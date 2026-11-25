-- | Deciding how to realise a derivation graph: what to substitute, what to
-- build, and in what order, root first.
--
-- Upstream tries a derivation's outputs before its inputs, so a package a
-- cache holds never pulls in what it was built from
-- (@DerivationGoal::haveDerivation@ at 2.24.9).  A substituted path needs its
-- references instead, and those arrive first, so the store stays closed under
-- references at every step (@PathSubstitutionGoal::init@).  The references
-- realised are the ones the cache the path then comes from declares.
--
-- A cache that lacks a path and one that fails are told apart, as upstream
-- tells them apart.  What no cache holds builds.  A substitute that a cache
-- offers and fails to deliver passes the path to the caches after it, and
-- when none of them delivers it the build stops, unless 'BuildFromSource'
-- (upstream's @fallback@) says to build instead.  A cache that cannot be
-- asked stops the build at once ('QueryFailed'); under 'BuildFromSource' the
-- caller asks the next cache instead, so that failure arrives here only as
-- a refusal to substitute at all ('noWriteAccessMessage'), which stops the
-- build too.
--
-- A derivation whose @allowSubstitutes@ attribute is false builds here even
-- when a cache holds its outputs, unless @always-allow-substitutes@ is set
-- ('substitutesAllowed').  Its attributes are read first, as upstream Nix
-- reads them ('derivationAttrs').
--
-- Under 'BuildCheck' the derivation asked for is not realised but checked:
-- built again beside its valid outputs, each compared with the one the
-- store holds ('checkRoot').  This is upstream's @bmCheck@, which
-- @nix-build --check@ selects, and it covers that derivation alone: its
-- inputs are realised as under 'BuildNormal'.
--
-- Several derivations can be realised in one call, each for the outputs a
-- request asks of it, as upstream's @Store::buildPaths@ realises several
-- requests with one worker ('realiseRequests').
--
-- Every effect is a record field, so the decisions run in tests over a fake
-- cache and store.  "Nix.Builder" supplies the real ones.
module Nix.Realise
  ( -- * Substituting a closure
    SubstEffects (..),
    Offer (..),
    Registration (..),
    FetchOutcome (..),
    Undelivered (..),
    ClosureOutcome (..),
    substituteClosure,
    noWriteAccessMessage,

    -- * Whether a derivation may be substituted
    DerivationAttrs (..),
    derivationAttrs,
    derivationBoolAttr,
    derivationStringAttr,
    derivationStringsAttr,
    SubstitutesSetting (..),
    Substitutes (..),
    substitutesAllowed,

    -- * Realising a graph
    Fallback (..),
    BuildMode (..),
    RealiseSettings (..),
    RealiseEffects (..),
    CheckVerdict (..),
    DifferingRebuild (..),
    checkedOutputPath,
    Realised (..),
    RealiseFailure (..),
    realiseFailureMessage,
    realiseFailureStatus,
    realise,

    -- * Realising several requests
    RequestsFailure (..),
    requestsFailureMessage,
    requestsFailureStatus,
    realiseRequests,
  )
where

import Control.Monad (filterM, void)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Nix.DependencyGraph (DepGraph (..), DepNode (..))
import Nix.Derivation (Derivation (..), DerivationOutput (..))
import Nix.Derivation.StructuredAttrs (structuredAttrsKey)
import Nix.Json (Json (..), expectJson, parseJson)
import Nix.Store.Path (StoreDir, StorePath (spName), defaultStoreDir, storePathBasename, storePathToFilePath, storePathToText)

-- ---------------------------------------------------------------------------
-- Substituting a closure
-- ---------------------------------------------------------------------------

-- | What substitution needs from the store and the caches.  A cache that
-- offers a path hands over a substitute @s@ with the references it
-- declares, and the path is fetched from that substitute, so what arrives
-- first and the path that names it are the same cache's account.
-- Upstream's substitution goal copies a path from the substituter whose
-- path info it realised the references of (@PathSubstitutionGoal::init@
-- and @tryToRun@ at 2.24.9).
data SubstEffects s m = SubstEffects
  { -- | Is this path already valid in the store?
    seValid :: !(StorePath -> m Bool),
    -- | What the first cache that serves a path says about it, before
    -- anything is fetched.
    seReferences :: !(StorePath -> m (Offer s)),
    -- | What the caches after this substitute's say about the path, asked
    -- once the substitute did not arrive.  Only caches after the given
    -- one are asked, so a path runs out of caches.
    seNextOffer :: !(StorePath -> s -> m (Offer s)),
    -- | Fetch these paths, each from its substitute, and register them as
    -- the 'Registration' says.  The paths an outcome names are among these.
    seFetch :: !(Registration -> NonEmpty (StorePath, s) -> m FetchOutcome)
  }

-- | What a fetch registers of the paths that arrive.
data Registration
  = -- | Every one of them if every one arrives, and none otherwise.
    RegisterAll
  | -- | None: another path of the batch has no cache left to offer it, so
    -- the batch cannot land.  The fetch learns which of these a cache
    -- delivers, which decides whether the substitution failed.
    RegisterNone
  deriving (Eq, Show)

-- | What the caches answer for one path, before anything is fetched.
data Offer s
  = -- | A cache serves the path with these references, and this substitute
    -- fetches it from that cache.
    Offered !s ![StorePath]
  | -- | No cache serves it.
    NotOffered
  | -- | A cache could not be asked, and why.  Upstream rethrows a failed
    -- narinfo query unless @fallback@ is set, which ends the whole build
    -- (@PathSubstitutionGoal::init@ at 2.24.9).
    QueryFailed !Text
  deriving (Eq, Show)

-- | How fetching a batch of paths ended.
data FetchOutcome
  = -- | Every path arrived, and under 'RegisterAll' is valid.
    Fetched
  | -- | Nothing was registered.  These paths did not arrive from their
    -- substitutes, and the others of the batch did.
    NotFetched !(NonEmpty (StorePath, Undelivered))
  deriving (Eq, Show)

-- | Why a path did not arrive from the cache that offered it.
data Undelivered
  = -- | The cache no longer holds it.  Upstream treats such a substitute
    -- as one that never existed (@SubstituteGone@ in
    -- @PathSubstitutionGoal::tryToRun@ at 2.24.9).
    Gone
  | -- | The cache failed to deliver it, or its registration was refused:
    -- upstream's @substituterFailed@.
    DeliveryFailed
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
    -- ('DeliveryFailed'), and no later cache delivered it.
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
substituteClosure :: (Monad m) => SubstEffects s m -> [StorePath] -> m ClosureOutcome
substituteClosure effects paths =
  fst <$> closureOf effects (Set.fromList paths) paths

-- | 'substituteClosure', threading the paths already visited.  A cache's
-- declared references are not trusted to be acyclic: a cycle ends here at the
-- visited set, and the registration it reaches refuses the unregistered
-- referent.  A batch is asked about in 'substitutionOrder', so the first
-- cache that fails, or the first refusal, is the one upstream reports.
closureOf ::
  (Monad m) =>
  SubstEffects s m ->
  Set StorePath ->
  [StorePath] ->
  m (ClosureOutcome, Set StorePath)
closureOf effects visited paths = do
  missing <- filterM (fmap not . seValid effects) (sortOn substitutionOrder paths)
  case NE.nonEmpty missing of
    Nothing -> pure (ClosureComplete, visited)
    Just batch -> do
      offered <- offersFor (fmap (\path -> (path, seReferences effects path)) batch)
      case offered of
        Left failure -> pure (ClosureQueryFailed failure, visited)
        Right Nothing -> pure (ClosureUnavailable, visited)
        Right (Just pending) -> fetchRounds effects visited (fmap (Offering NoneFailed) pending)

-- | One path of a batch with the substitute a cache offered it as, and the
-- references that cache declares.
data Pending s = Pending
  { pendingPath :: !StorePath,
    pendingSubstitute :: !s,
    pendingReferences :: ![StorePath]
  }

-- | Whether a cache has failed to deliver a path, which upstream's goal for
-- the path records as @substituterFailed@.
data Attempts = NoneFailed | SomeFailed

-- | Where one path of a batch stands between fetches.
data Standing s
  = -- | A cache offers it, and none it was asked of before has delivered it.
    Offering !Attempts !(Pending s)
  | -- | It arrived from its substitute, and was not registered.
    Arrived !Attempts !(Pending s)
  | -- | No cache is left to offer it: 'DeliveryFailed' when one failed to
    -- deliver it, and 'Gone' when none did.
    RanOut !StorePath !Undelivered

-- | The path a standing is about.
standingPath :: Standing s -> StorePath
standingPath (Offering _ pending) = pendingPath pending
standingPath (Arrived _ pending) = pendingPath pending
standingPath (RanOut path _) = path

-- | Each path's offer, asked in order.  A failed query ends the build, so it
-- is the last one asked; a path no cache offers does not stop the others
-- being asked, and makes the answer 'Nothing'.
offersFor :: (Monad m) => NonEmpty (StorePath, m (Offer s)) -> m (Either Text (Maybe (NonEmpty (Pending s))))
offersFor asks = fmap sequence <$> runExceptT (traverse (\(path, ask) -> ExceptT (pendingFor path <$> ask)) asks)
  where
    pendingFor path (Offered substitute references) = Right (Just (Pending path substitute references))
    pendingFor _ NotOffered = Right Nothing
    pendingFor _ (QueryFailed failure) = Left failure

-- | Fetch a batch until it lands or can tell why not.  Each fetch realises
-- what the paths it takes reference first; a reference that cannot be
-- realised ends the batch, whatever later caches offer, as upstream's goal
-- for a path is done once a reference's goal fails
-- (@PathSubstitutionGoal::tryToRun@ at 2.24.9).
--
-- While every path has a cache to offer it, the whole batch is fetched and
-- registered if it all arrives.  A path that does not arrive is asked of
-- the caches after the one that offered it, that cache's references come
-- first, and the batch is fetched again, the paths that arrived included,
-- since nothing was registered: upstream's goal for each path moves on to
-- its next substituter (@PathSubstitutionGoal::init@).  Once a path has no
-- cache left the batch cannot land, and the paths still offered are
-- fetched, registering nothing, until each arrives or runs out too, as each
-- of upstream's goals runs its own course.  The batch then failed when a
-- path ran out after a cache failed to deliver it, and is otherwise
-- unavailable, as a path no cache delivers and none failed to counts as one
-- no cache offered: upstream's derivation fails only when a goal failed
-- other than for want of a substitute or a reference
-- (@DerivationGoal::outputsSubstitutionTried@ in derivation-goal.cc at
-- 2.24.9).
fetchRounds ::
  (Monad m) =>
  SubstEffects s m ->
  Set StorePath ->
  NonEmpty (Standing s) ->
  m (ClosureOutcome, Set StorePath)
fetchRounds effects visited standings =
  case NE.nonEmpty (concatMap fetching standings) of
    Nothing -> pure (exhaustedOutcome standings, visited)
    Just batch -> do
      let outside = Set.fromList (concatMap pendingReferences batch) `Set.difference` Set.fromList (map standingPath (NE.toList standings))
      (held, deeper) <- referencesFirst effects visited (Set.toAscList outside)
      case held of
        ClosureComplete -> do
          fetched <- seFetch effects registration (fmap (\each -> (pendingPath each, pendingSubstitute each)) batch)
          case (registration, fetched) of
            (RegisterAll, Fetched) -> pure (ClosureComplete, deeper)
            _ -> do
              next <- runExceptT (traverse (ExceptT . afterFetch effects (undeliveredIn fetched)) standings)
              either (\failure -> pure (ClosureQueryFailed failure, deeper)) (fetchRounds effects deeper) next
        _ -> pure (held, deeper)
  where
    whole = not (any ranOut standings)
    registration
      | whole = RegisterAll
      | otherwise = RegisterNone
    fetching (Offering _ pending) = [pending]
    fetching (Arrived _ pending) = [pending | whole]
    fetching (RanOut _ _) = []
    ranOut (RanOut _ _) = True
    ranOut _ = False

-- | The paths a fetch names as not delivered.
undeliveredIn :: FetchOutcome -> Map StorePath Undelivered
undeliveredIn Fetched = Map.empty
undeliveredIn (NotFetched undelivered) = Map.fromList (NE.toList undelivered)

-- | Where a path stands after a fetch that registered nothing.  One the fetch
-- took has arrived unless it is named, and one named is asked of the caches
-- after its substitute's.  A path the fetch did not take stands as it did.
afterFetch :: (Monad m) => SubstEffects s m -> Map StorePath Undelivered -> Standing s -> m (Either Text (Standing s))
afterFetch effects undelivered standing = case standing of
  Offering attempts pending -> fetched attempts pending
  Arrived attempts pending -> fetched attempts pending
  RanOut _ _ -> pure (Right standing)
  where
    fetched attempts pending@(Pending path substitute _) = case Map.lookup path undelivered of
      Nothing -> pure (Right (Arrived attempts pending))
      Just why -> fromNext path (attemptsAfter why attempts) <$> seNextOffer effects path substitute
    fromNext path attempts (Offered substitute references) = Right (Offering attempts (Pending path substitute references))
    fromNext path attempts NotOffered = Right (RanOut path (ranOutBecause attempts))
    fromNext _ _ (QueryFailed failure) = Left failure

-- | A path's attempts once a cache has not delivered it, for this reason.
attemptsAfter :: Undelivered -> Attempts -> Attempts
attemptsAfter DeliveryFailed _ = SomeFailed
attemptsAfter Gone attempts = attempts

-- | Why a path with no cache left did not arrive.
ranOutBecause :: Attempts -> Undelivered
ranOutBecause SomeFailed = DeliveryFailed
ranOutBecause NoneFailed = Gone

-- | What a batch comes to when it cannot land and nothing is left to fetch:
-- failed when some path ran out after a cache failed to deliver it, and
-- otherwise unavailable.
exhaustedOutcome :: NonEmpty (Standing s) -> ClosureOutcome
exhaustedOutcome standings
  | any failedPath standings = ClosureFailed
  | otherwise = ClosureUnavailable
  where
    failedPath (RanOut _ DeliveryFailed) = True
    failedPath _ = False

-- | Realise references one at a time, stopping at the first that fails.  A
-- reference no cache supplies is a hole the inputs may fill, so the paths
-- naming it are incomplete.  A reference whose substitute or query failed
-- passes that failure on, as upstream's goal for a path fails when a
-- reference's goal does (@PathSubstitutionGoal::tryToRun@ at 2.24.9).
referencesFirst ::
  (Monad m) =>
  SubstEffects s m ->
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

-- | The order upstream's worker runs the substitution goals of a batch in:
-- by key, @a$@ then the path's name, @$@ and the path
-- (@PathSubstitutionGoal::key@ in substitution-goal.hh, @Worker::run@ in
-- worker.cc).  A @$@ sorts before every character a store path name may
-- hold, so the keys sort as the names do and then as the paths do.
substitutionOrder :: StorePath -> (Text, StorePath)
substitutionOrder path = (spName path, path)

-- | Upstream's refusal to substitute a path where nothing may be written,
-- which its substitution goal throws in a read-only evaluation before it
-- asks any cache (@PathSubstitutionGoal::init@ in substitution-goal.cc).
-- The path is named under the logical store directory, as upstream's
-- @printStorePath@ names it.
noWriteAccessMessage :: StorePath -> Text
noWriteAccessMessage path = "cannot substitute path '" <> storePathToText defaultStoreDir path <> "' - no write access to the Nix store"

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

-- | Upstream's @ParsedDerivation::getBoolAttr@ in parsed-derivations.cc at
-- 2.24.9 (@getBoolAttr@ in derivation-options.cc at 2.36.0), for the
-- derivation at a @.drv@ path: the default when the derivation lacks the
-- attribute.  In structured attributes it is a JSON Boolean, and any other
-- value there is an error; in the environment only @1@ is true.  Structured
-- attributes that are not a JSON object hold no attribute, as nlohmann's
-- @find@ finds nothing in such a value.
derivationBoolAttr :: StorePath -> Text -> Bool -> DerivationAttrs -> Either Text Bool
derivationBoolAttr _ name def (AttrsInEnvironment env) =
  Right (maybe def (== environmentTrue) (Map.lookup name env))
derivationBoolAttr drvPath name def (AttrsInJson (JsonObject members)) =
  maybe (Right def) fromJson (Map.lookup name members)
  where
    fromJson (JsonBool flag) = Right flag
    fromJson _ =
      Left ("attribute '" <> name <> "' of derivation '" <> storePathBasename drvPath <> "' must be a Boolean")
derivationBoolAttr _ _ def (AttrsInJson _) = Right def

-- | Upstream's @getStringAttr@ (derivation-options.cc at 2.36.0), for
-- the derivation at a @.drv@ path: 'Nothing' when the derivation lacks
-- the attribute.  In the environment it is the variable's bytes; in
-- structured attributes it is a JSON string, its UTF-8 bytes, and any
-- other value there is upstream's refusal ('attributeRefusal').
derivationStringAttr :: StorePath -> Text -> DerivationAttrs -> Either Text (Maybe ByteString)
derivationStringAttr _ name (AttrsInEnvironment env) = Right (Map.lookup name env)
derivationStringAttr drvPath name (AttrsInJson (JsonObject members)) =
  traverse (first (attributeRefusal drvPath name) . fmap TE.encodeUtf8 . expectJson "string" jsonString) (Map.lookup name members)
derivationStringAttr _ _ (AttrsInJson _) = Right Nothing

-- | Upstream's @getStringSetAttr@ (derivation-options.cc at 2.36.0), for
-- the derivation at a @.drv@ path: 'Nothing' when the derivation lacks
-- the attribute.  In the environment it is the variable's bytes split at
-- spaces, tabs, carriage returns and newlines, upstream's
-- @tokenizeString@ separators, with no empty token; in structured
-- attributes it is a JSON array of strings, and anything else there is
-- upstream's refusal of the first value of the wrong type
-- ('attributeRefusal').
derivationStringsAttr :: StorePath -> Text -> DerivationAttrs -> Either Text (Maybe [ByteString])
derivationStringsAttr _ name (AttrsInEnvironment env) =
  Right (filter (not . BS.null) . BS.splitWith (`BS.elem` tokenSeparators) <$> Map.lookup name env)
  where
    tokenSeparators = " \t\n\r"
derivationStringsAttr drvPath name (AttrsInJson (JsonObject members)) =
  traverse (first (attributeRefusal drvPath name) . strings) (Map.lookup name members)
  where
    strings value = expectJson "array" jsonArray value >>= traverse (fmap TE.encodeUtf8 . expectJson "string" jsonString)
    jsonArray value = case value of
      JsonArray items -> Just items
      _ -> Nothing
derivationStringsAttr _ _ (AttrsInJson _) = Right Nothing

-- | A JSON value's text, when it is a string.
jsonString :: Json -> Maybe Text
jsonString value = case value of
  JsonString text -> Just text
  _ -> Nothing

-- | Upstream's refusal of an attribute of the wrong type in structured
-- attributes, with the two lines of context upstream puts before it, the
-- derivation's and the attribute's (@derivationOptionsFromStructuredAttrs@
-- and its caller in derivation-building-goal.cc at 2.36.0), on one line
-- as nova-nix reports such context.
attributeRefusal :: StorePath -> Text -> Text -> Text
attributeRefusal drvPath name refusal =
  "while parsing derivation '" <> storePathToText defaultStoreDir drvPath <> "': while parsing attribute \"" <> name <> "\": " <> refusal

-- | Upstream's @ParsedDerivation::substitutesAllowed@ at 2.24.9, for the
-- derivation at a @.drv@ path.  Under 'AlwaysAllowSubstitutes' every
-- derivation may be substituted and the attribute is not read.  Otherwise
-- @allowSubstitutes@ decides ('derivationBoolAttr'), and a derivation
-- without it may be substituted.
--
-- The rule covers a derivation's own outputs alone.  A path that another
-- substituted output references is substituted as a path, whichever
-- derivation built it: upstream's @PathSubstitutionGoal@ never reads the
-- attribute.
substitutesAllowed :: SubstitutesSetting -> StorePath -> DerivationAttrs -> Either Text Substitutes
substitutesAllowed AlwaysAllowSubstitutes _ _ = Right MaySubstitute
substitutesAllowed HeedAllowSubstitutes drvPath attrs =
  (\allowed -> if allowed then MaySubstitute else MustBuild) <$> derivationBoolAttr drvPath allowSubstitutesAttr True attrs

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

-- | The settings that decide how a graph is realised, as upstream's
-- @fallback@ and @always-allow-substitutes@ do.  What is done with the
-- derivation asked for is the 'BuildMode' a call to 'realise' is given,
-- as upstream's @Store::buildPaths@ is given its mode with each call.
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
  | -- | Under 'BuildCheck', this derivation has an output that is not
    -- valid and that no cache supplied, so a rebuild has nothing to be
    -- compared with.
    OutputsNotValid !StorePath
  | -- | Under 'BuildCheck', rebuilding this derivation gave this output
    -- other contents than the store holds, and the rebuild was kept or
    -- discarded ('OutputDiffers').
    NotDeterministic !StorePath !StorePath !DifferingRebuild
  | -- | Under 'BuildCheck', the graph holds no derivation at the root, so
    -- nothing was built again or compared.
    MissingDerivation !StorePath
  | -- | A dependent asked this derivation for outputs it does not have,
    -- named in order.
    MissingOutputs !StorePath !(NonEmpty Text)
  deriving (Eq, Show)

-- | A failure as the build reports it, given the directory the store's
-- files are in, each in upstream's words: a failed substitute less the
-- space upstream's message ends in, a check that cannot start, a check
-- that found a difference, with each path in double quotes as upstream
-- quotes a path and the rebuilt output named where it was kept, a root
-- the graph does not hold as upstream reports a derivation it cannot
-- load, and an output asked of a derivation that does not have it.
--
-- A @.drv@ is named under the logical store directory, wherever the
-- store's files live, as upstream names one: a store kept elsewhere
-- still has @/nix/store@ as its store directory, and only the directory
-- its files are in moves.  The output that differs is named where its
-- files are, as upstream names it, which is also how a build prints the
-- output it made.
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
  NotDeterministic drvPath output rebuild ->
    "derivation '"
      <> logical drvPath
      <> "' may not be deterministic: output "
      <> quotedPath (storePathToFilePath realDir output)
      <> " differs"
      <> case rebuild of
        RebuildDiscarded -> ""
        RebuildKept -> " from " <> quotedPath (checkedOutputPath realDir output)
  MissingDerivation drvPath ->
    "cannot build missing derivation '" <> logical drvPath <> "'"
  MissingOutputs drvPath names ->
    "derivation '" <> logical drvPath <> "' does not have wanted outputs " <> T.intercalate ", " ["'" <> name <> "'" | name <- NE.toList names]
  where
    logical = storePathToText defaultStoreDir
    quotedPath path = "\"" <> T.pack path <> "\""

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
  NotDeterministic {} -> checkMismatchStatus
  BuildFailed _ -> failureStatus
  CacheQueryFailed _ -> failureStatus
  SubstitutesFailed _ -> failureStatus
  InvalidDerivation _ -> failureStatus
  OutputsNotValid _ -> failureStatus
  MissingDerivation _ -> failureStatus
  MissingOutputs _ _ -> failureStatus

-- | How one derivation came to be realised.
data Realised
  = -- | Every output asked of it was already valid.
    AlreadyValid
  | -- | Every output asked of it came from a cache.
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
  | -- | This output came out with other contents, and its rebuild was
    -- kept or discarded.  It is the first that did, in the order the
    -- outputs are compared in.
    OutputDiffers !StorePath !DifferingRebuild
  deriving (Eq, Show)

-- | What became of a rebuilt output that differs from the valid one.
data DifferingRebuild
  = -- | It was discarded with everything else the rebuild wrote.
    RebuildDiscarded
  | -- | It was kept beside the valid output ('checkedOutputPath'), as
    -- upstream keeps it under @keep-failed@.
    RebuildKept
  deriving (Eq, Show)

-- | Where a check keeps the rebuild of an output that differs, given the
-- directory the store's files are in: the output's path with @.check@
-- after it, where upstream keeps it.  Nothing registers it, so it is not
-- a valid path.
checkedOutputPath :: StoreDir -> StorePath -> FilePath
checkedOutputPath realDir output = storePathToFilePath realDir output <> ".check"

-- | What realising a graph needs from the outside.
data RealiseEffects s m = RealiseEffects
  { -- | The store and caches, which also say whether an output is valid.
    reSubst :: !(SubstEffects s m),
    -- | Build one derivation, whose inputs are realised, given its @.drv@
    -- path and the outputs the request that led to the build asks of it.
    -- A build makes every output, so the store's builder leaves those
    -- unread; they say what the build was for.
    reBuild :: !(Derivation -> StorePath -> Set Text -> m (Either Text ())),
    -- | Build one derivation, whose outputs are valid and whose inputs are
    -- realised, again beside those outputs, given its @.drv@ path, and
    -- compare what comes out with what the store holds.  It registers
    -- nothing, and keeps nothing it built but a rebuild that differs,
    -- where the verdict says it was kept.
    reCheck :: !(Derivation -> StorePath -> m (Either Text CheckVerdict)),
    -- | Report how a derivation was realised.
    reReport :: !(Realised -> Derivation -> m ())
  }

-- | Realise a graph from its root, or under 'BuildCheck' check the root
-- ('checkRoot').  Every other derivation in the graph is realised as
-- under 'BuildNormal', whatever the mode.  A derivation that is valid or
-- substituted takes its inputs out of the work, and what remains builds
-- leaves first.
--
-- Each derivation is realised for the outputs asked of it, as upstream's
-- goal for it is (@DerivationGoal@'s @wantedOutputs@ at 2.24.9): the root
-- for every output, and each input for the outputs its dependent names
-- ('dnDeps').  Only those are checked for validity and substituted
-- (@checkPathValidity@, @haveDerivation@), so an output nothing asks for
-- is neither fetched nor a reason to build, and asking for one the
-- derivation does not have fails ('MissingOutputs').  A build makes
-- every output.
--
-- The graph has to be acyclic ('Nix.DependencyGraph.topoSort' decides that).
realise :: (Monad m) => RealiseSettings -> BuildMode -> RealiseEffects s m -> DepGraph -> StorePath -> m (Either RealiseFailure ())
realise settings mode effects graph root = case mode of
  BuildNormal -> void <$> realiseNode settings effects graph Map.empty (root, everyOutput)
  BuildCheck -> checkRoot settings effects graph root
  where
    everyOutput = maybe Set.empty (outputNames . dnDerivation) (Map.lookup root (unDepGraph graph))

-- | The outputs of each derivation realised so far: those a request for
-- them found valid or substituted, and every output of one that was built.
type RealisedOutputs = Map StorePath (Set Text)

-- | The names of a derivation's outputs.
outputNames :: Derivation -> Set Text
outputNames = Set.fromList . map doName . drvOutputs

-- | The paths of a derivation's outputs that have one of the names.
namedOutputPaths :: Set Text -> Derivation -> [StorePath]
namedOutputPaths names drv = [doPath output | output <- drvOutputs drv, doName output `Set.member` names]

-- | Whether every one of the paths is valid.
allValid :: (Monad m) => RealiseEffects s m -> [StorePath] -> m Bool
allValid effects = fmap and . traverse (seValid (reSubst effects))

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
checkRoot :: (Monad m) => RealiseSettings -> RealiseEffects s m -> DepGraph -> StorePath -> m (Either RealiseFailure ())
checkRoot settings effects graph root = case Map.lookup root (unDepGraph graph) of
  Nothing -> pure (Left (MissingDerivation root))
  Just depNode ->
    let drv = dnDerivation depNode
     in either (pure . Left . InvalidDerivation) (checkAllowed depNode) $
          derivationAttrs root drv >>= substitutesAllowed (rsSubstitutes settings) root
  where
    checkAllowed depNode allowed = do
      let drv = dnDerivation depNode
          outputs = map doPath (drvOutputs drv)
      valid <- allValid effects outputs
      if valid
        then rebuild depNode
        else do
          substituted <- outputsFromCaches effects allowed outputs
          case (substituted, substitutionStop (rsFallback settings) root substituted) of
            (ClosureComplete, _) -> reReport effects Substituted drv *> rebuild depNode
            (_, Just failure) -> pure (Left failure)
            (_, Nothing) -> pure (Left (OutputsNotValid root))
    rebuild depNode = do
      let drv = dnDerivation depNode
      inputs <- realiseAll settings effects graph (Map.singleton root (outputNames drv)) (Map.toList (dnDeps depNode))
      case inputs of
        Left err -> pure (Left err)
        Right _ -> reCheck effects drv root >>= either (pure . Left . BuildFailed) (checked drv)
    checked drv OutputsMatch = Right () <$ reReport effects Checked drv
    checked _ (OutputDiffers output differing) = pure (Left (NotDeterministic root output differing))

-- | Substitute outputs of a derivation, if it may be substituted.  One
-- that must build asks no cache, which leaves it where one that no cache
-- holds is: upstream starts no substitution for it.
outputsFromCaches :: (Monad m) => RealiseEffects s m -> Substitutes -> [StorePath] -> m ClosureOutcome
outputsFromCaches _ MustBuild _ = pure ClosureUnavailable
outputsFromCaches effects MaySubstitute outputs = substituteClosure (reSubst effects) outputs

-- | Where a request for some of a derivation's outputs stands before
-- anything is substituted or built for it.
data Assessment
  = -- | Nothing is left to realise: the graph does not hold the
    -- derivation, or the outputs asked of it are realised already.
    NothingToRealise
  | -- | Every output asked of the derivation is valid.
    OutputsValid !DepNode
  | -- | Some output asked of the derivation is not valid.  The paths are
    -- those of every output asked, and the derivation may or may not be
    -- substituted.
    OutputsWanted !DepNode ![StorePath] !Substitutes

-- | Assess a request against what has been realised so far, reading what
-- upstream's goal for it reads before it asks any cache
-- (@DerivationGoal::haveDerivation@ at 2.24.9): the derivation's
-- attributes, whether it has every output asked, whether those are valid,
-- and, when one is not, whether the derivation may be substituted.  A node
-- outside the graph has nothing to realise.
assess ::
  (Monad m) =>
  RealiseSettings ->
  RealiseEffects s m ->
  DepGraph ->
  RealisedOutputs ->
  (StorePath, Set Text) ->
  m (Either RealiseFailure Assessment)
assess settings effects graph realised (node, wanted) = case Map.lookup node (unDepGraph graph) of
  Nothing -> pure (Right NothingToRealise)
  Just depNode
    | wanted `Set.isSubsetOf` Map.findWithDefault Set.empty node realised -> pure (Right NothingToRealise)
    | otherwise -> either (pure . Left . InvalidDerivation) (parsed depNode) (derivationAttrs node (dnDerivation depNode))
  where
    parsed depNode attrs = case NE.nonEmpty (Set.toAscList (wanted `Set.difference` outputNames drv)) of
      Just absent -> pure (Left (MissingOutputs node absent))
      Nothing -> do
        valid <- allValid effects outputs
        pure $
          if valid
            then Right (OutputsValid depNode)
            else first InvalidDerivation (OutputsWanted depNode outputs <$> substitutesAllowed (rsSubstitutes settings) node attrs)
      where
        drv = dnDerivation depNode
        outputs = namedOutputPaths wanted drv

-- | Realise one node for the outputs asked of it, threading what has been
-- realised so far.  A request for outputs already realised does nothing,
-- so a derivation several others ask the same of is realised once.  One
-- asking for an output not yet realised realises the node again, for the
-- outputs it asks for: upstream's worker drops a goal once it is done
-- (@Goal::amDone@ at 2.24.9), so a later request for the derivation starts
-- a goal of its own (@Worker::makeDerivationGoalCommon@).  A request is
-- recorded before the inputs are realised, so a cycle, which
-- 'Nix.DependencyGraph.topoSort' refuses, still ends.
realiseNode ::
  (Monad m) =>
  RealiseSettings ->
  RealiseEffects s m ->
  DepGraph ->
  RealisedOutputs ->
  (StorePath, Set Text) ->
  m (Either RealiseFailure RealisedOutputs)
realiseNode settings effects graph realised request@(node, wanted) =
  assess settings effects graph realised request >>= either (pure . Left) realiseAssessed
  where
    seen = Map.insertWith Set.union node wanted realised
    stopFor = substitutionStop (rsFallback settings) node
    reported status drv done = Right done <$ reReport effects status drv
    realiseAssessed NothingToRealise = pure (Right realised)
    realiseAssessed (OutputsValid depNode) = reported AlreadyValid (dnDerivation depNode) seen
    realiseAssessed (OutputsWanted depNode outputs allowed) = substituteOrBuild depNode outputs allowed
    substituteOrBuild depNode outputs allowed = do
      let substituteOutputs = outputsFromCaches effects allowed outputs
      firstTry <- substituteOutputs
      case (firstTry, stopFor firstTry) of
        (ClosureComplete, _) -> reported Substituted (dnDerivation depNode) seen
        (_, Just failure) -> pure (Left failure)
        (_, Nothing) -> buildFromInputs substituteOutputs firstTry depNode
    buildFromInputs substituteOutputs firstTry depNode = do
      let drv = dnDerivation depNode
      inputs <- realiseAll settings effects graph seen (Map.toList (dnDeps depNode))
      case inputs of
        Left err -> pure (Left err)
        Right done -> do
          -- The inputs may have supplied what the closure lacked, and then
          -- the substitute is still good (upstream's retrySubstitution).
          retried <- case firstTry of
            ClosureIncomplete -> substituteOutputs
            other -> pure other
          case (retried, stopFor retried) of
            (ClosureComplete, _) -> reported Substituted drv done
            (_, Just failure) -> pure (Left failure)
            (_, Nothing) -> do
              built <- reBuild effects drv node wanted
              case built of
                Left err -> pure (Left (BuildFailed err))
                Right () -> reported Built drv (Map.insert node (outputNames drv) done)

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

-- | Realise nodes in order, each for the outputs asked of it, stopping at
-- the first failure.
realiseAll ::
  (Monad m) =>
  RealiseSettings ->
  RealiseEffects s m ->
  DepGraph ->
  RealisedOutputs ->
  [(StorePath, Set Text)] ->
  m (Either RealiseFailure RealisedOutputs)
realiseAll _ _ _ realised [] = pure (Right realised)
realiseAll settings effects graph realised (request : rest) = do
  outcome <- realiseNode settings effects graph realised request
  case outcome of
    Left err -> pure (Left err)
    Right deeper -> realiseAll settings effects graph deeper rest

-- ---------------------------------------------------------------------------
-- Realising several requests
-- ---------------------------------------------------------------------------

-- | Why realising several requests stopped, as upstream's
-- @Store::buildPaths@ reports it (entry-points.cc at 2.24.9,
-- @Worker::buildPaths@ there at 2.36.0).
data RequestsFailure
  = -- | The call failed with this, reported as it is: a failure that ends
    -- the whole call, as one upstream's goals throw does, or one that ended
    -- a request when every other request was realised.
    FailedWith !RealiseFailure
  | -- | The request for this derivation failed with this, and left the
    -- requests for these others unrealised.  Upstream logs the failure,
    -- then fails the call naming every request its worker did not finish.
    FailedLeaving !RealiseFailure !StorePath !(NonEmpty StorePath)
  deriving (Eq, Show)

-- | A failure as the call reports it, given the directory the store's
-- files are in: the failure's own message ('realiseFailureMessage'), or
-- upstream's @build of %s failed@, which names the request that failed
-- and the others it left unrealised in the order of their paths, as
-- upstream's sorted set of them lists them, each under the logical store
-- directory.  The failure that left them is what upstream logs before it.
requestsFailureMessage :: StoreDir -> RequestsFailure -> Text
requestsFailureMessage realDir (FailedWith failure) = realiseFailureMessage realDir failure
requestsFailureMessage _ (FailedLeaving _ failed others) =
  "build of " <> T.intercalate ", " (map quoted (Set.toAscList (Set.fromList (failed : NE.toList others)))) <> " failed"
  where
    quoted drvPath = "'" <> storePathToText defaultStoreDir drvPath <> "'"

-- | The exit status of a call that stopped with this failure: the
-- failure's own ('realiseFailureStatus'), however it is reported.
-- Upstream gives a failed request's report the status its worker composes
-- from what failed in the call (@Worker::failingExitStatus@ in worker.cc
-- at 2.24.9, @ExitStatusFlags::failingExitStatus@ in build-result.cc at
-- 2.36.0), and a call here stops at its first failure, so that is the one
-- failure's.  An error upstream's goals throw keeps its own status, 1
-- (@ErrorInfo@'s @status@ in error.hh), which 'realiseFailureStatus'
-- gives each of them.
requestsFailureStatus :: RequestsFailure -> Int
requestsFailureStatus (FailedWith failure) = realiseFailureStatus failure
requestsFailureStatus (FailedLeaving failure _ _) = realiseFailureStatus failure

-- | How far a failure reaches in upstream's worker.
data FailureReach
  = -- | It ends the request it arose in, and with it the call, as a failed
    -- goal does, and @Store::buildPaths@ reports it among the requests
    -- its worker left unrealised.
    EndsRequest
  | -- | It ends the call where it arises, as an error upstream's goals
    -- throw does: its worker does not catch one (@Worker::run@ in
    -- worker.cc), so it leaves @buildPaths@ as it is.
    EndsCall

-- | How far a failure reaches.  A cache that cannot be asked is rethrown
-- (@PathSubstitutionGoal::init@ in substitution-goal.cc at 2.24.9), and
-- attributes that cannot be read, an output a derivation does not have and
-- a check of outputs that are not valid are thrown (@haveDerivation@,
-- @checkPathValidity@ and @outputsSubstitutionTried@ of @DerivationGoal@
-- in derivation-goal.cc at 2.24.9).  A failed build or check, a failed
-- substitute and a derivation that cannot be loaded end their goal
-- (@buildDone@, @outputsSubstitutionTried@ and @loadDerivation@ there).
failureReach :: RealiseFailure -> FailureReach
failureReach failure = case failure of
  BuildFailed _ -> EndsRequest
  SubstitutesFailed _ -> EndsRequest
  NotDeterministic {} -> EndsRequest
  MissingDerivation _ -> EndsRequest
  CacheQueryFailed _ -> EndsCall
  InvalidDerivation _ -> EndsCall
  MissingOutputs _ _ -> EndsCall
  OutputsNotValid _ -> EndsCall

-- | Realise several derivations, each for the outputs a request asks of
-- it, with one record of what is realised, so that a derivation is not
-- realised again for outputs one request, or an input of one, has
-- already realised.  Upstream's @Store::buildPaths@ makes a goal for each
-- request and runs them with one worker (entry-points.cc at 2.24.9),
-- which keeps one goal for a derivation: requests that name the same
-- derivation share it, and it wants every output any of them asks for
-- (@Worker::makeDerivationGoalCommon@ in worker.cc at 2.24.9).  Such
-- requests are realised here as one request for all those outputs.
--
-- Every request is assessed before any is realised, as the worker's first
-- pass works every goal until it waits for a substitute, an input or a
-- build (@Worker::run@ in worker.cc): one whose outputs are valid is realised
-- there, and one whose derivation's attributes cannot be read, that asks for
-- an output the derivation does not have, or that has an output that is not
-- valid and an @allowSubstitutes@ that cannot be read stops the call before
-- any substitute or build has finished (@DerivationGoal::haveDerivation@ in
-- derivation-goal.cc at 2.24.9).  Both passes take the requests in the order
-- upstream's worker takes their goals, by the derivation's name and then its
-- path (@DerivationGoal::key@ in derivation-goal.cc), so a failure two
-- requests could report is the one upstream's reports.
--
-- The first failure stops the call, as a failed goal ends every other
-- unless @keep-going@ is set (@Worker::removeGoal@ in worker.cc),
-- which nova-nix does not offer.  One that upstream's goals throw, such
-- as a cache that cannot be asked, is reported as it is.  Any other is
-- reported as it is when every other request was realised, and otherwise
-- with the request that failed and each after it whose outputs are not
-- valid, which upstream's worker had not finished (@Store::buildPaths@ in
-- entry-points.cc at 2.24.9).  That worker runs the goals that wait for
-- substitutes and builds together, so which of them it has finished when one
-- fails depends on how their work interleaves; realising them in turn answers
-- as upstream does when each request's work ends before the next one's
-- begins.
--
-- The requests are realised and never checked, as import from derivation
-- builds its requests under @bmNormal@ (@EvalState::realiseContext@ in
-- primops.cc): a check is of one derivation, which 'realise' checks
-- under 'BuildCheck'.  A derivation the graph does not hold has nothing
-- to realise, as for 'realise', and the graph has to be acyclic.
realiseRequests ::
  (Monad m) =>
  RealiseSettings ->
  RealiseEffects s m ->
  DepGraph ->
  [(StorePath, Set Text)] ->
  m (Either RequestsFailure ())
realiseRequests settings effects graph requests = do
  firstPass <- runExceptT (traverse assessRequest (sortOn goalOrder (Map.toList (Map.fromListWith Set.union requests))))
  case firstPass of
    Left failure -> pure (Left (FailedWith failure))
    Right assessments -> do
      mapM_ (reReport effects AlreadyValid . dnDerivation) [depNode | (_, OutputsValid depNode) <- assessments]
      inTurn
        (Map.fromList [request | (request, OutputsValid _) <- assessments])
        [(request, outputs) | (request, OutputsWanted _ outputs _) <- assessments]
  where
    assessRequest request = (,) request <$> ExceptT (assess settings effects graph Map.empty request)
    goalOrder (drvPath, _) = (spName drvPath, drvPath)
    inTurn _ [] = pure (Right ())
    inTurn realised ((request, _) : rest) =
      realiseNode settings effects graph realised request >>= either (fmap Left . stoppedAt request rest) (`inTurn` rest)
    stoppedAt (node, _) rest failure = case failureReach failure of
      EndsCall -> pure (FailedWith failure)
      EndsRequest -> do
        unrealised <- filterM (fmap not . allValid effects . snd) rest
        pure (maybe (FailedWith failure) (FailedLeaving failure node) (NE.nonEmpty (map (fst . fst) unrealised)))
