-- | Deciding how to realise a derivation graph: what to substitute, what to
-- build, and in what order, root first.
--
-- Upstream tries a derivation's outputs before its inputs, so a package a
-- cache holds never pulls in what it was built from
-- (@DerivationGoal::haveDerivation@ at 2.24.9).  A substituted path needs its
-- references instead, and those arrive first, so the store stays closed under
-- references at every step (@PathSubstitutionGoal::init@).
--
-- Every effect is a record field, so the decisions run in tests over a fake
-- cache and store.  "Nix.Builder" supplies the real ones.
module Nix.Realise
  ( -- * Substituting a closure
    SubstEffects (..),
    ClosureOutcome (..),
    substituteClosure,

    -- * Realising a graph
    RealiseEffects (..),
    Realised (..),
    realise,
  )
where

import Control.Monad (filterM, void)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Nix.DependencyGraph (DepGraph (..), DepNode (..), directDeps)
import Nix.Derivation (Derivation (..), DerivationOutput (..))
import Nix.Store.Path (StorePath)

-- ---------------------------------------------------------------------------
-- Substituting a closure
-- ---------------------------------------------------------------------------

-- | What substitution needs from the store and the caches.
data SubstEffects m = SubstEffects
  { -- | Is this path already valid in the store?
    seValid :: !(StorePath -> m Bool),
    -- | The references a cache declares for a path, or 'Nothing' when no
    -- cache will serve it.
    seReferences :: !(StorePath -> m (Maybe [StorePath])),
    -- | Fetch and register these paths, all of them or none.
    seFetch :: !([StorePath] -> m Bool)
  }

-- | How an attempt to substitute a set of paths ended.
data ClosureOutcome
  = -- | The paths and everything they reference are valid.
    ClosureComplete
  | -- | No cache could supply the paths themselves.
    ClosureUnavailable
  | -- | A cache has the paths but not something they reference.  Building
    -- the inputs can supply the hole, so substitution is worth retrying
    -- afterwards.
    ClosureIncomplete
  deriving (Eq, Show)

-- | Substitute paths with their closure, references first.
--
-- The paths arrive together through 'seFetch', because the outputs of one
-- derivation may reference each other and a batch that cannot land whole
-- registers nothing.  Anything else they reference arrives first, each path
-- on its own, as upstream's does: a reference that lands before a later
-- failure stays, a valid path like any other, and so can leave its own
-- derivation with only some outputs valid.
substituteClosure :: (Monad m) => SubstEffects m -> [StorePath] -> m ClosureOutcome
substituteClosure effects paths =
  fst <$> closureOf effects (Set.fromList paths) paths

-- | 'substituteClosure', threading the paths already visited.  A cache's
-- declared references are not trusted to be acyclic: a cycle ends here at the
-- visited set, and the registration it reaches refuses the unregistered
-- referent.
closureOf ::
  (Monad m) =>
  SubstEffects m ->
  Set StorePath ->
  [StorePath] ->
  m (ClosureOutcome, Set StorePath)
closureOf effects visited paths = do
  missing <- filterM (fmap not . seValid effects) paths
  declared <- traverse (seReferences effects) missing
  case (missing, sequence declared) of
    ([], _) -> pure (ClosureComplete, visited)
    (_, Nothing) -> pure (ClosureUnavailable, visited)
    (_, Just references) -> do
      let outside = Set.fromList (concat references) `Set.difference` Set.fromList missing
      (held, deeper) <- referencesFirst effects visited (Set.toAscList outside)
      case held of
        ClosureComplete -> do
          fetched <- seFetch effects missing
          pure (if fetched then ClosureComplete else ClosureUnavailable, deeper)
        _ -> pure (ClosureIncomplete, deeper)

-- | Realise references one at a time, stopping at the first that fails.
referencesFirst ::
  (Monad m) =>
  SubstEffects m ->
  Set StorePath ->
  [StorePath] ->
  m (ClosureOutcome, Set StorePath)
referencesFirst _ visited [] = pure (ClosureComplete, visited)
referencesFirst effects visited (reference : rest)
  | reference `Set.member` visited = referencesFirst effects visited rest
  | otherwise = do
      (outcome, deeper) <- closureOf effects (Set.insert reference visited) [reference]
      case outcome of
        ClosureComplete -> referencesFirst effects deeper rest
        _ -> pure (ClosureIncomplete, deeper)

-- ---------------------------------------------------------------------------
-- Realising a graph
-- ---------------------------------------------------------------------------

-- | How one derivation came to be realised.
data Realised
  = -- | Every output was already valid.
    AlreadyValid
  | -- | Every output came from a cache.
    Substituted
  | -- | The derivation was built here.
    Built
  deriving (Eq, Show)

-- | What realising a graph needs from the outside.
data RealiseEffects m = RealiseEffects
  { reSubst :: !(SubstEffects m),
    -- | Are all of this derivation's outputs valid?
    reAllValid :: !(Derivation -> m Bool),
    -- | Build one derivation, whose inputs are realised.
    reBuild :: !(Derivation -> m (Either Text ())),
    -- | Report how a derivation was realised.
    reReport :: !(Realised -> Derivation -> m ())
  }

-- | Realise a graph from its root.  A derivation that is valid or substituted
-- takes its inputs out of the work, and what remains builds leaves first.
--
-- The graph has to be acyclic ('Nix.DependencyGraph.topoSort' decides that).
realise :: (Monad m) => RealiseEffects m -> DepGraph -> StorePath -> m (Either Text ())
realise effects graph root = void <$> realiseNode effects graph Set.empty root

-- | Realise one node, threading the nodes already visited, so a derivation
-- several others depend on is handled once.
realiseNode ::
  (Monad m) =>
  RealiseEffects m ->
  DepGraph ->
  Set StorePath ->
  StorePath ->
  m (Either Text (Set StorePath))
realiseNode effects graph visited node
  | node `Set.member` visited = pure (Right visited)
  | otherwise = case Map.lookup node (unDepGraph graph) of
      -- A node outside the graph has nothing to realise.
      Nothing -> pure (Right visited)
      Just depNode -> do
        let drv = dnDerivation depNode
            seen = Set.insert node visited
        allValid <- reAllValid effects drv
        if allValid
          then reported AlreadyValid drv seen
          else do
            firstTry <- substituteOutputs drv
            case firstTry of
              ClosureComplete -> reported Substituted drv seen
              _ -> buildFromInputs firstTry drv seen
  where
    reported status drv done = Right done <$ reReport effects status drv
    substituteOutputs drv = substituteClosure (reSubst effects) (map doPath (drvOutputs drv))
    buildFromInputs firstTry drv seen = do
      inputs <- realiseAll effects graph seen (directDeps graph node)
      case inputs of
        Left err -> pure (Left err)
        Right realised -> do
          -- The inputs may have supplied what the closure lacked, and then
          -- the substitute is still good (upstream's retrySubstitution).
          retried <- case firstTry of
            ClosureIncomplete -> substituteOutputs drv
            other -> pure other
          case retried of
            ClosureComplete -> reported Substituted drv realised
            _ -> do
              built <- reBuild effects drv
              case built of
                Left err -> pure (Left err)
                Right () -> reported Built drv realised

-- | Realise nodes in order, stopping at the first failure.
realiseAll ::
  (Monad m) =>
  RealiseEffects m ->
  DepGraph ->
  Set StorePath ->
  [StorePath] ->
  m (Either Text (Set StorePath))
realiseAll _ _ visited [] = pure (Right visited)
realiseAll effects graph visited (node : rest) = do
  realised <- realiseNode effects graph visited node
  case realised of
    Left err -> pure (Left err)
    Right deeper -> realiseAll effects graph deeper rest
