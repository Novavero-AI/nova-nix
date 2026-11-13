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
-- A derivation whose @allowSubstitutes@ attribute is false builds here even
-- when a cache holds its outputs ('substitutesAllowed').  Its attributes are
-- read first, as upstream's @ParsedDerivation@ reads them
-- ('derivationAttrs').
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

    -- * Whether a derivation may be substituted
    DerivationAttrs (..),
    derivationAttrs,
    SubstitutesSetting (..),
    Substitutes (..),
    substitutesAllowed,

    -- * Realising a graph
    Fallback (..),
    RealiseSettings (..),
    RealiseEffects (..),
    Realised (..),
    RealiseFailure (..),
    realiseFailureMessage,
    realise,
  )
where

import Control.Monad (filterM, void)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Nix.DependencyGraph (DepGraph (..), DepNode (..), directDeps)
import Nix.Derivation (Derivation (..), DerivationOutput (..))
import Nix.Derivation.StructuredAttrs (structuredAttrsKey)
import Nix.Json (Json (..), parseJson)
import Nix.Store.Path (StorePath, defaultStoreDir, storePathBasename, storePathToText)

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
-- Whether a derivation may be substituted
-- ---------------------------------------------------------------------------

-- | Where a derivation's attributes are read from, as upstream's
-- @ParsedDerivation@ holds them.
data DerivationAttrs
  = -- | The structured attributes of a derivation that has them, parsed.
    AttrsInJson !Json
  | -- | The environment of a derivation without structured attributes.
    AttrsInEnvironment !(Map Text ByteString)
  deriving (Eq, Show)

-- | Upstream's @ParsedDerivation@ constructor at 2.24.9, for the derivation
-- at a @.drv@ path: structured attributes are parsed as JSON, and ones that
-- are not JSON are an error naming the derivation.  Upstream constructs one
-- for every derivation it has a goal for, before it looks at the outputs
-- (@DerivationGoal::haveDerivation@).
derivationAttrs :: StorePath -> Derivation -> Either Text DerivationAttrs
derivationAttrs drvPath drv = case Map.lookup structuredAttrsKey env of
  Nothing -> Right (AttrsInEnvironment env)
  Just encoded -> AttrsInJson <$> first unreadable (first (const "invalid UTF-8") (TE.decodeUtf8' encoded) >>= parseJson)
  where
    env = drvEnv drv
    unreadable reason =
      "cannot process " <> structuredAttrsKey <> " attribute of '" <> storePathBasename drvPath <> "': " <> reason

-- | Upstream's @always-allow-substitutes@ setting.
data SubstitutesSetting
  = -- | A derivation's @allowSubstitutes@ attribute decides, upstream's
    -- default.
    HeedAllowSubstitutes
  | -- | Every derivation may be substituted, whatever its attribute says.
    AlwaysAllowSubstitutes
  deriving (Eq, Show)

-- | Whether a derivation's own outputs may come from a cache.
data Substitutes
  = MaySubstitute
  | MustBuild
  deriving (Eq, Show)

-- | The attribute a derivation forbids substitutes with.
allowSubstitutesAttr :: Text
allowSubstitutesAttr = "allowSubstitutes"

-- | The one environment value upstream's @getBoolAttr@ reads as true, which
-- is how evaluation writes @true@; @false@ is written as the empty string.
environmentTrue :: ByteString
environmentTrue = "1"

-- | Upstream's @ParsedDerivation::substitutesAllowed@ at 2.24.9, for the
-- derivation at a @.drv@ path.  Under 'AlwaysAllowSubstitutes' every
-- derivation may be substituted and the attribute is not read.  Otherwise
-- @allowSubstitutes@ decides, and a derivation without it may be
-- substituted.  In structured attributes it is a JSON Boolean, and any
-- other value there is an error; in the environment only @1@ is true.
-- Structured attributes that are not a JSON object hold no attribute, as
-- nlohmann's @find@ finds nothing in such a value.
--
-- The rule covers a derivation's own outputs alone.  A path that another
-- substituted output references is substituted as a path, whichever
-- derivation built it: upstream's @PathSubstitutionGoal@ never reads the
-- attribute.
substitutesAllowed :: SubstitutesSetting -> StorePath -> DerivationAttrs -> Either Text Substitutes
substitutesAllowed AlwaysAllowSubstitutes _ _ = Right MaySubstitute
substitutesAllowed HeedAllowSubstitutes _ (AttrsInEnvironment env) =
  Right (maybe MaySubstitute fromEnvironment (Map.lookup allowSubstitutesAttr env))
  where
    fromEnvironment value
      | value == environmentTrue = MaySubstitute
      | otherwise = MustBuild
substitutesAllowed HeedAllowSubstitutes drvPath (AttrsInJson (JsonObject members)) =
  maybe (Right MaySubstitute) fromJson (Map.lookup allowSubstitutesAttr members)
  where
    fromJson (JsonBool True) = Right MaySubstitute
    fromJson (JsonBool False) = Right MustBuild
    fromJson _ =
      Left ("attribute '" <> allowSubstitutesAttr <> "' of derivation '" <> storePathBasename drvPath <> "' must be a Boolean")
substitutesAllowed HeedAllowSubstitutes _ (AttrsInJson _) = Right MaySubstitute

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

-- | The settings that decide how a graph is realised.
data RealiseSettings = RealiseSettings
  { rsFallback :: !Fallback,
    rsSubstitutes :: !SubstitutesSetting
  }
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
  | -- | A derivation's attributes could not be read ('derivationAttrs',
    -- 'substitutesAllowed').
    InvalidDerivation !Text
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
realiseFailureMessage (InvalidDerivation problem) = problem
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
realise :: (Monad m) => RealiseSettings -> RealiseEffects m -> DepGraph -> StorePath -> m (Either RealiseFailure ())
realise settings effects graph root = void <$> realiseNode settings effects graph Set.empty root

-- | Realise one node, threading the nodes already visited, so a derivation
-- several others depend on is handled once.
realiseNode ::
  (Monad m) =>
  RealiseSettings ->
  RealiseEffects m ->
  DepGraph ->
  Set StorePath ->
  StorePath ->
  m (Either RealiseFailure (Set StorePath))
realiseNode settings effects graph visited node
  | node `Set.member` visited = pure (Right visited)
  | otherwise = case Map.lookup node (unDepGraph graph) of
      -- A node outside the graph has nothing to realise.
      Nothing -> pure (Right visited)
      Just depNode ->
        let drv = dnDerivation depNode
         in either invalid (realiseParsed drv (Set.insert node visited)) (derivationAttrs node drv)
  where
    stopFor = substitutionStop (rsFallback settings) node
    reported status drv done = Right done <$ reReport effects status drv
    invalid = pure . Left . InvalidDerivation
    realiseParsed drv seen attrs = do
      allValid <- reAllValid effects drv
      if allValid
        then reported AlreadyValid drv seen
        else either invalid (substituteOrBuild drv seen) (substitutesAllowed (rsSubstitutes settings) node attrs)
    substituteOrBuild drv seen allowed = do
      let substituteOutputs = outputsFromCaches allowed drv
      firstTry <- substituteOutputs
      case (firstTry, stopFor firstTry) of
        (ClosureComplete, _) -> reported Substituted drv seen
        (_, Just failure) -> pure (Left failure)
        (_, Nothing) -> buildFromInputs substituteOutputs firstTry drv seen
    -- A derivation that must build asks no cache, which leaves it where one
    -- that no cache holds is: upstream starts no substitution for it and
    -- goes on to its inputs.
    outputsFromCaches MustBuild _ = pure ClosureUnavailable
    outputsFromCaches MaySubstitute drv = substituteClosure (reSubst effects) (map doPath (drvOutputs drv))
    buildFromInputs substituteOutputs firstTry drv seen = do
      inputs <- realiseAll settings effects graph seen (directDeps graph node)
      case inputs of
        Left err -> pure (Left err)
        Right realised -> do
          -- The inputs may have supplied what the closure lacked, and then
          -- the substitute is still good (upstream's retrySubstitution).
          retried <- case firstTry of
            ClosureIncomplete -> substituteOutputs
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
  RealiseSettings ->
  RealiseEffects m ->
  DepGraph ->
  Set StorePath ->
  [StorePath] ->
  m (Either RealiseFailure (Set StorePath))
realiseAll _ _ _ visited [] = pure (Right visited)
realiseAll settings effects graph visited (node : rest) = do
  realised <- realiseNode settings effects graph visited node
  case realised of
    Left err -> pure (Left err)
    Right deeper -> realiseAll settings effects graph deeper rest
