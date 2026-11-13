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
-- Under 'BuildCheck' the derivation asked for is not realised but checked:
-- built again beside its valid outputs, each compared with the one the
-- store holds ('checkRoot').  This is upstream's @bmCheck@, which
-- @nix-build --check@ selects, and it covers that derivation alone: its
-- inputs are realised as under 'BuildNormal'.
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
    BuildMode (..),
    RealiseSettings (..),
    RealiseEffects (..),
    CheckVerdict (..),
    Realised (..),
    RealiseFailure (..),
    realiseFailureMessage,
    realiseFailureStatus,
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
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Nix.DependencyGraph (DepGraph (..), DepNode (..), directDeps)
import Nix.Derivation (Derivation (..), DerivationOutput (..))
import Nix.Derivation.StructuredAttrs (structuredAttrsKey)
import Nix.Json (Json (..), parseJson)
import Nix.Store.Path (StoreDir, StorePath, defaultStoreDir, storePathBasename, storePathToFilePath, storePathToText)

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

-- | What a build does with the derivation it was asked for: upstream's
-- @BuildMode@, less @bmRepair@.
data BuildMode
  = -- | Make its outputs valid, upstream's @bmNormal@.
    BuildNormal
  | -- | Build it again beside its valid outputs and compare each with the
    -- one the store holds: upstream's @bmCheck@.
    BuildCheck
  deriving (Eq, Show)

-- | The settings that decide how a graph is realised.
data RealiseSettings = RealiseSettings
  { rsFallback :: !Fallback,
    rsSubstitutes :: !SubstitutesSetting,
    -- | What is done with the root.  Every other derivation in the graph
    -- is realised as under 'BuildNormal', whatever this says.
    rsMode :: !BuildMode
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
  | -- | Under 'BuildCheck', this derivation has an output that is not
    -- valid and that no cache supplied, so a rebuild has nothing to be
    -- compared with.
    OutputsNotValid !StorePath
  | -- | Under 'BuildCheck', rebuilding this derivation gave this output
    -- other contents than the store holds ('OutputDiffers').
    NotDeterministic !StorePath !StorePath
  | -- | Under 'BuildCheck', the graph holds no derivation at the root, so
    -- nothing was built again or compared.
    MissingDerivation !StorePath
  deriving (Eq, Show)

-- | A failure as the build reports it, given the directory the store's
-- files are in.  A failed substitute reads as upstream's
-- (@DerivationGoal::outputsSubstitutionTried@ at 2.24.9), less the space
-- its format string ends in, a check that cannot start as that function's
-- other error, and a check that found a difference as the error upstream's
-- @registerOutputs@ raises when neither @keep-failed@ nor @run-diff-hook@
-- is set (local-derivation-goal.cc:2713).  A root the graph does not hold
-- reads as upstream's goal for a derivation it cannot load
-- (@DerivationGoal::loadDerivation@).
--
-- A @.drv@ is named under the logical store directory, wherever the
-- store's files live, as upstream's @printStorePath@ names it: a store
-- kept elsewhere still has @/nix/store@ as its @storeDir@ and only its
-- @realStoreDir@ moves.  The output that differs is named where its files
-- are, as upstream's @toRealPath@ names it there, which is also how a
-- build prints the output it made.
realiseFailureMessage :: StoreDir -> RealiseFailure -> Text
realiseFailureMessage realDir failure = case failure of
  BuildFailed report -> report
  CacheQueryFailed queryFailure -> queryFailure
  InvalidDerivation problem -> problem
  SubstitutesFailed drvPath ->
    "some substitutes for the outputs of derivation '"
      <> logical drvPath
      <> "' failed (usually happens due to networking issues); try '--fallback' to build derivation from source"
  OutputsNotValid drvPath ->
    "some outputs of '" <> logical drvPath <> "' are not valid, so checking is not possible"
  NotDeterministic drvPath output ->
    "derivation '"
      <> logical drvPath
      <> "' may not be deterministic: output '"
      <> T.pack (storePathToFilePath realDir output)
      <> "' differs"
  MissingDerivation drvPath ->
    "cannot build missing derivation '" <> logical drvPath <> "'"
  where
    logical = storePathToText defaultStoreDir

-- | Upstream's exit status for a build whose one failure is a check that
-- found a difference: the check-mismatch bit and the two bits every status
-- @Worker::failingExitStatus@ composes carries, @0x08 | 0x60@ (worker.cc at
-- 2.24.9).
checkMismatchStatus :: Int
checkMismatchStatus = 104

-- | The exit status of a build that failed any other way.
failureStatus :: Int
failureStatus = 1

-- | The exit status of a build that stopped with this failure.  A check
-- that found a difference exits with upstream's status for one, so a
-- caller can tell it from a build that broke.  Every other failure exits
-- 1, as upstream's does except a failed build's ('BuildFailed'), whose
-- status there says more: 100 when the builder of a derivation that is not
-- fixed-output fails, 102 when a fixed output has the wrong hash.
realiseFailureStatus :: RealiseFailure -> Int
realiseFailureStatus failure = case failure of
  NotDeterministic _ _ -> checkMismatchStatus
  BuildFailed _ -> failureStatus
  CacheQueryFailed _ -> failureStatus
  SubstitutesFailed _ -> failureStatus
  InvalidDerivation _ -> failureStatus
  OutputsNotValid _ -> failureStatus
  MissingDerivation _ -> failureStatus

-- | How one derivation came to be realised.
data Realised
  = -- | Every output was already valid.
    AlreadyValid
  | -- | Every output came from a cache.
    Substituted
  | -- | The derivation was built here.
    Built
  | -- | The derivation was built again here, and every output came out as
    -- the store holds it ('BuildCheck').
    Checked
  deriving (Eq, Show)

-- | What building a derivation again beside its valid outputs found.
data CheckVerdict
  = -- | Every output came out as the store holds it.
    OutputsMatch
  | -- | This output came out with other contents.  It is the first that
    -- did, in the order the outputs are compared in.
    OutputDiffers !StorePath
  deriving (Eq, Show)

-- | What realising a graph needs from the outside.
data RealiseEffects m = RealiseEffects
  { reSubst :: !(SubstEffects m),
    -- | Are all of this derivation's outputs valid?
    reAllValid :: !(Derivation -> m Bool),
    -- | Build one derivation, whose inputs are realised, given its @.drv@
    -- path.
    reBuild :: !(Derivation -> StorePath -> m (Either Text ())),
    -- | Build one derivation, whose outputs are valid and whose inputs are
    -- realised, again beside those outputs, given its @.drv@ path, and
    -- compare what comes out with what the store holds.  It registers
    -- nothing and keeps nothing it built.
    reCheck :: !(Derivation -> StorePath -> m (Either Text CheckVerdict)),
    -- | Report how a derivation was realised.
    reReport :: !(Realised -> Derivation -> m ())
  }

-- | Realise a graph from its root, or under 'BuildCheck' check the root
-- ('checkRoot').  A derivation that is valid or substituted takes its
-- inputs out of the work, and what remains builds leaves first.
--
-- The graph has to be acyclic ('Nix.DependencyGraph.topoSort' decides that).
realise :: (Monad m) => RealiseSettings -> RealiseEffects m -> DepGraph -> StorePath -> m (Either RealiseFailure ())
realise settings effects graph root = case rsMode settings of
  BuildNormal -> void <$> realiseNode settings effects graph Set.empty root
  BuildCheck -> checkRoot settings effects graph root

-- | Check the root: upstream's @DerivationGoal@ under @bmCheck@ at 2.24.9.
--
-- An output that is not valid is substituted first, as under any mode
-- (@haveDerivation@ starts the substitution goals before it looks at the
-- mode again), and a derivation still short of an output cannot be checked
-- (@outputsSubstitutionTried@).  That error comes before any input is
-- looked at, so a substitute with a hole in its closure is not retried
-- here as 'realiseNode' retries it.
--
-- With every output valid the inputs are realised as under 'BuildNormal'
-- (@gaveUpOnSubstitution@ gives each input's goal @bmNormal@): the rebuild
-- needs them, and a root that was substituted never fetched them.  Then
-- the derivation is built again and compared ('reCheck').
--
-- @allowSubstitutes@ is read whether or not the outputs are valid, where
-- 'realiseNode' leaves it unread for a valid derivation: upstream reads it
-- before it starts a substitution goal for each output, a valid one
-- included, and such a goal ends at once.
--
-- A root the graph does not hold fails the check, where 'realiseNode' has
-- nothing to realise for such a node: a check that compared nothing has
-- not passed.
checkRoot :: (Monad m) => RealiseSettings -> RealiseEffects m -> DepGraph -> StorePath -> m (Either RealiseFailure ())
checkRoot settings effects graph root = case Map.lookup root (unDepGraph graph) of
  Nothing -> pure (Left (MissingDerivation root))
  Just depNode ->
    let drv = dnDerivation depNode
     in either (pure . Left . InvalidDerivation) (checkAllowed drv) $
          derivationAttrs root drv >>= substitutesAllowed (rsSubstitutes settings) root
  where
    checkAllowed drv allowed = do
      allValid <- reAllValid effects drv
      if allValid
        then rebuild drv
        else do
          substituted <- outputsFromCaches effects allowed drv
          case (substituted, substitutionStop (rsFallback settings) root substituted) of
            (ClosureComplete, _) -> reReport effects Substituted drv *> rebuild drv
            (_, Just failure) -> pure (Left failure)
            (_, Nothing) -> pure (Left (OutputsNotValid root))
    rebuild drv = do
      inputs <- realiseAll settings effects graph (Set.singleton root) (directDeps graph root)
      case inputs of
        Left err -> pure (Left err)
        Right _ -> reCheck effects drv root >>= either (pure . Left . BuildFailed) (checked drv)
    checked drv OutputsMatch = Right () <$ reReport effects Checked drv
    checked _ (OutputDiffers output) = pure (Left (NotDeterministic root output))

-- | Substitute a derivation's own outputs, if it may be substituted.  One
-- that must build asks no cache, which leaves it where one that no cache
-- holds is: upstream starts no substitution for it.
outputsFromCaches :: (Monad m) => RealiseEffects m -> Substitutes -> Derivation -> m ClosureOutcome
outputsFromCaches _ MustBuild _ = pure ClosureUnavailable
outputsFromCaches effects MaySubstitute drv = substituteClosure (reSubst effects) (map doPath (drvOutputs drv))

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
      let substituteOutputs = outputsFromCaches effects allowed drv
      firstTry <- substituteOutputs
      case (firstTry, stopFor firstTry) of
        (ClosureComplete, _) -> reported Substituted drv seen
        (_, Just failure) -> pure (Left failure)
        (_, Nothing) -> buildFromInputs substituteOutputs firstTry drv seen
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
