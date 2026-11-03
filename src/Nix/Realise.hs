-- | Deciding how to realise a derivation graph: what to substitute, what to
-- build, and in what order, root first.
--
-- Upstream tries a derivation's outputs before its inputs, so a package a
-- cache holds never pulls in what it was built from
-- (@DerivationGoal::haveDerivation@ at 2.24.9).  A substituted path needs its
-- references instead, and those arrive first, so the store stays closed under
-- references at every step (@PathSubstitutionGoal::init@).
--
-- A cache that lacks a path and one that fails are told apart, as upstream
-- tells them apart.  What no cache holds builds.  A substitute that a cache
-- offers and fails to deliver stops the build, unless 'BuildFromSource'
-- (upstream's @fallback@) says to build instead
-- (@DerivationGoal::outputsSubstitutionTried@).  A cache that cannot be asked
-- stops the build at once ('QueryFailed'); under 'BuildFromSource' the caller
-- asks the next cache instead, so that failure never arrives here.
--
-- Every effect is a record field, so the decisions run in tests over a fake
-- cache and store.  "Nix.Builder" supplies the real ones.
module Nix.Realise
  ( -- * Substituting a closure
    SubstEffects (..),
    Offer (..),
    FetchOutcome (..),
    ClosureOutcome (..),
    substituteClosure,

    -- * Realising a graph
    Fallback (..),
    RealiseEffects (..),
    Realised (..),
    RealiseFailure (..),
    realiseFailureMessage,
    realise,
  )
where

import Control.Monad (filterM, void)
import Control.Monad.Except (ExceptT (..), runExceptT)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Nix.DependencyGraph (DepGraph (..), DepNode (..), directDeps)
import Nix.Derivation (Derivation (..), DerivationOutput (..))
import Nix.Store.Path (StorePath, defaultStoreDir, storePathToText)

-- ---------------------------------------------------------------------------
-- Substituting a closure
-- ---------------------------------------------------------------------------

-- | What substitution needs from the store and the caches.
data SubstEffects m = SubstEffects
  { -- | Is this path already valid in the store?
    seValid :: !(StorePath -> m Bool),
    -- | What the caches say about a path before anything is fetched.
    seReferences :: !(StorePath -> m Offer),
    -- | Fetch and register these paths, all of them or none.
    seFetch :: !([StorePath] -> m FetchOutcome)
  }

-- | What the caches answer for one path, before anything is fetched.
data Offer
  = -- | A cache serves the path, with these references.
    Offered ![StorePath]
  | -- | No cache serves it.
    NotOffered
  | -- | A cache could not be asked, and why.  Upstream rethrows a failed
    -- narinfo query unless @fallback@ is set, which ends the whole build
    -- (@PathSubstitutionGoal::init@ at 2.24.9).
    QueryFailed !Text
  deriving (Eq, Show)

-- | How fetching a batch of paths ended.
data FetchOutcome
  = -- | Every path is valid.
    Fetched
  | -- | No cache supplied some path, and none failed to.
    FetchMissed
  | -- | A cache that offered a path failed to deliver it, and no other cache
    -- delivered it: upstream's failed substitution.
    FetchFailed
  deriving (Eq, Show)

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
  | -- | A substitute for the paths, or for something they reference, failed
    -- ('FetchFailed').
    ClosureFailed
  | -- | A cache could not be asked about one of them ('QueryFailed').
    ClosureQueryFailed !Text
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
  -- A failed query ends the build, so it is the last one asked.
  declared <- runExceptT (traverse (ExceptT . fmap offeredReferences . seReferences effects) missing)
  case (missing, sequence <$> declared) of
    ([], _) -> pure (ClosureComplete, visited)
    (_, Left failure) -> pure (ClosureQueryFailed failure, visited)
    (_, Right Nothing) -> pure (ClosureUnavailable, visited)
    (_, Right (Just references)) -> do
      let outside = Set.fromList (concat references) `Set.difference` Set.fromList missing
      (held, deeper) <- referencesFirst effects visited (Set.toAscList outside)
      case held of
        ClosureComplete -> do
          fetched <- seFetch effects missing
          pure (fetchedClosure fetched, deeper)
        _ -> pure (held, deeper)

-- | An offer's references, 'Nothing' when no cache serves the path, or the
-- failure of a query that could not be asked.
offeredReferences :: Offer -> Either Text (Maybe [StorePath])
offeredReferences (Offered references) = Right (Just references)
offeredReferences NotOffered = Right Nothing
offeredReferences (QueryFailed failure) = Left failure

-- | What a batch fetch makes of paths whose references are all valid.
fetchedClosure :: FetchOutcome -> ClosureOutcome
fetchedClosure Fetched = ClosureComplete
fetchedClosure FetchMissed = ClosureUnavailable
fetchedClosure FetchFailed = ClosureFailed

-- | Realise references one at a time, stopping at the first that fails.  A
-- reference no cache supplies is a hole the inputs may fill, so the paths
-- naming it are incomplete.  A reference whose substitute or query failed
-- passes that failure on, as upstream's goal for a path fails when a
-- reference's goal does (@PathSubstitutionGoal::tryToRun@ at 2.24.9).
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
        ClosureUnavailable -> pure (ClosureIncomplete, deeper)
        ClosureIncomplete -> pure (ClosureIncomplete, deeper)
        ClosureFailed -> pure (ClosureFailed, deeper)
        ClosureQueryFailed failure -> pure (ClosureQueryFailed failure, deeper)

-- ---------------------------------------------------------------------------
-- Realising a graph
-- ---------------------------------------------------------------------------

-- | What a failed substitute leads to: upstream's @fallback@ setting, which
-- its @--fallback@ flag turns on.
data Fallback
  = -- | The build stops and names @--fallback@, upstream's default.
    NoFallback
  | -- | The derivation builds from source instead.
    BuildFromSource
  deriving (Eq, Show)

-- | Why realising a graph stopped.
data RealiseFailure
  = -- | A derivation failed to build, with the builder's report.
    BuildFailed !Text
  | -- | A cache could not be asked about a path ('QueryFailed').
    CacheQueryFailed !Text
  | -- | A substitute for this derivation's outputs, or for something they
    -- reference, failed under 'NoFallback'.
    SubstitutesFailed !StorePath
  deriving (Eq, Show)

-- | A failure as the build reports it.  A failed substitute reads as
-- upstream's (@DerivationGoal::outputsSubstitutionTried@ at 2.24.9), less
-- the space its format string ends in.  The @.drv@ is named under the
-- logical store directory, wherever the store's files live, as upstream's
-- @printStorePath@ names it: a store kept elsewhere still has @/nix/store@
-- as its @storeDir@ and only its @realStoreDir@ moves.
realiseFailureMessage :: RealiseFailure -> Text
realiseFailureMessage (BuildFailed report) = report
realiseFailureMessage (CacheQueryFailed failure) = failure
realiseFailureMessage (SubstitutesFailed drvPath) =
  "some substitutes for the outputs of derivation '"
    <> storePathToText defaultStoreDir drvPath
    <> "' failed (usually happens due to networking issues); try '--fallback' to build derivation from source"

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
    -- | Build one derivation, whose inputs are realised, given its @.drv@
    -- path.
    reBuild :: !(Derivation -> StorePath -> m (Either Text ())),
    -- | Report how a derivation was realised.
    reReport :: !(Realised -> Derivation -> m ())
  }

-- | Realise a graph from its root.  A derivation that is valid or substituted
-- takes its inputs out of the work, and what remains builds leaves first.
--
-- The graph has to be acyclic ('Nix.DependencyGraph.topoSort' decides that).
realise :: (Monad m) => Fallback -> RealiseEffects m -> DepGraph -> StorePath -> m (Either RealiseFailure ())
realise fallback effects graph root = void <$> realiseNode fallback effects graph Set.empty root

-- | Realise one node, threading the nodes already visited, so a derivation
-- several others depend on is handled once.
realiseNode ::
  (Monad m) =>
  Fallback ->
  RealiseEffects m ->
  DepGraph ->
  Set StorePath ->
  StorePath ->
  m (Either RealiseFailure (Set StorePath))
realiseNode fallback effects graph visited node
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
            case (firstTry, stopFor firstTry) of
              (ClosureComplete, _) -> reported Substituted drv seen
              (_, Just failure) -> pure (Left failure)
              (_, Nothing) -> buildFromInputs firstTry drv seen
  where
    stopFor = substitutionStop fallback node
    reported status drv done = Right done <$ reReport effects status drv
    substituteOutputs drv = substituteClosure (reSubst effects) (map doPath (drvOutputs drv))
    buildFromInputs firstTry drv seen = do
      inputs <- realiseAll fallback effects graph seen (directDeps graph node)
      case inputs of
        Left err -> pure (Left err)
        Right realised -> do
          -- The inputs may have supplied what the closure lacked, and then
          -- the substitute is still good (upstream's retrySubstitution).
          retried <- case firstTry of
            ClosureIncomplete -> substituteOutputs drv
            other -> pure other
          case (retried, stopFor retried) of
            (ClosureComplete, _) -> reported Substituted drv realised
            (_, Just failure) -> pure (Left failure)
            (_, Nothing) -> do
              built <- reBuild effects drv node
              case built of
                Left err -> pure (Left (BuildFailed err))
                Right () -> reported Built drv realised

-- | The failure a substitution attempt for a derivation stops the build
-- with, if it stops it: a failed query always, and a failed substitute
-- unless the derivation may build from source.  Upstream retries a
-- substitution after the inputs only when every failure was a hole in the
-- closure, so a failed substitute that falls back is not retried.
substitutionStop :: Fallback -> StorePath -> ClosureOutcome -> Maybe RealiseFailure
substitutionStop fallback drvPath outcome = case outcome of
  ClosureQueryFailed failure -> Just (CacheQueryFailed failure)
  ClosureFailed -> case fallback of
    NoFallback -> Just (SubstitutesFailed drvPath)
    BuildFromSource -> Nothing
  ClosureComplete -> Nothing
  ClosureUnavailable -> Nothing
  ClosureIncomplete -> Nothing

-- | Realise nodes in order, stopping at the first failure.
realiseAll ::
  (Monad m) =>
  Fallback ->
  RealiseEffects m ->
  DepGraph ->
  Set StorePath ->
  [StorePath] ->
  m (Either RealiseFailure (Set StorePath))
realiseAll _ _ _ visited [] = pure (Right visited)
realiseAll fallback effects graph visited (node : rest) = do
  realised <- realiseNode fallback effects graph visited node
  case realised of
    Left err -> pure (Left err)
    Right deeper -> realiseAll fallback effects graph deeper rest
