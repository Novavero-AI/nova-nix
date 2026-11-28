{-# LANGUAGE LambdaCase #-}

-- | The operations a daemon serves, as upstream's daemon reads and judges
-- them: what each one reads before its work begins ('getRequest'), what a
-- client the daemon does not trust may not ask ('authorize'), what a
-- client's settings change for its connection ('applyClientSettings'),
-- and what the store must provide to serve them, a t'DaemonStore' in the
-- protocol's own terms.  "Nix.Daemon.Connection" runs them on a
-- connection.
--
-- The operations served are the ones upstream's clients send for
-- building, substituting, copying, querying and collecting with a
-- daemon as their store.  Any other operation code is refused as
-- upstream refuses one it does not know.
--
-- Upstream's messages are kept without the colours its formatter puts
-- around the values it interpolates.
module Nix.Daemon.Ops
  ( -- * Requests
    Request (..),
    getRequest,
    getUploadPathInfo,
    Substitute (..),
    Repair (..),
    CheckSigs (..),
    CheckContents (..),
    Liveness (..),
    RootCensoring (..),
    AddToStoreRequest (..),
    GCRequest (..),

    -- * Trust
    Authorization (..),
    authorize,
    rootCensoring,

    -- * Client settings
    ClientSettings (..),
    ConnectionSettings (..),
    applyClientSettings,

    -- * The store
    DaemonStore (..),
    Work (..),
    LogEvent (..),
    Intake (..),
    DumpUpload (..),
    Missing (..),
    GCResult (..),
    daemonError,

    -- * Archives in uploads
    NarBoundary (..),
    narSizeBoundary,
  )
where

import Control.Monad (replicateM, replicateM_, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word32, Word64)
import Nix.Config (ConfigAssignment (..), NixConfig (..), applyAssignment)
import Nix.Daemon.Protocol
  ( Activity,
    ActivityId,
    BuildMode (..),
    ContentAddressMethod,
    DaemonError (..),
    DerivedPath,
    Field,
    GCAction (..),
    GCPaths (..),
    KeyedBuildResult,
    OutputName,
    ResultType,
    Signature,
    Trust (..),
    UnkeyedValidPathInfo (..),
    ValidPathInfo (..),
    Verbosity (..),
    Version (..),
    VersionNumber (..),
    defaultExitStatus,
    errorVerbosity,
    featureDeleteDeadSpecificReferrers,
    getBuildMode,
    getDerivedPath,
    getGCAction,
    getGCPaths,
    getSignatureSet,
    getStorePath,
    getStorePathSet,
    getValidPathInfo,
  )
import Nix.Daemon.Wire (Decoder, getBool, getBytes, getList, getWord32, getWord64, malformed)
import Nix.Store.Path (StoreDir, StorePath)

-- ---------------------------------------------------------------------------
-- Requests
-- ---------------------------------------------------------------------------

-- | Whether a query first substitutes the paths it asks about.
data Substitute = NoSubstitute | Substitute
  deriving (Eq, Show)

-- | Whether an import replaces the contents of a path that is valid.
data Repair = NoRepair | Repair
  deriving (Eq, Show)

-- | Whether an import holds a path to the signatures the store trusts.
data CheckSigs = CheckSigs | NoCheckSigs
  deriving (Eq, Show)

-- | Whether a verification of the store reads each path's contents.
data CheckContents = SkipContents | CheckContents
  deriving (Eq, Show)

-- | Whether a collection takes live paths for dead ones, which upstream's
-- daemon refuses every client.
data Liveness = KeepLiveness | IgnoreLiveness
  deriving (Eq, Show)

-- | Whether the roots a client is shown name what holds them, or hide
-- the names upstream calls privacy-sensitive (a temporary root's owner,
-- a process's open file).
data RootCensoring = ShowRoots | CensorRoots
  deriving (Eq, Show)

-- | An upload of a dump to make a content-addressed path from, as far as
-- it is read before its work begins.  Its method is parsed once that work
-- has begun, as upstream parses it.
data AddToStoreRequest = AddToStoreRequest
  { -- | The name of the path to make, as the client sent it.
    atsName :: !ByteString,
    -- | The method and algorithm, spelled as the client sent them.
    atsMethod :: !ByteString,
    -- | The paths the dump refers to.
    atsReferences :: !(Set StorePath),
    -- | Whether a valid path's contents are replaced.
    atsRepair :: !Repair
  }
  deriving (Eq, Show)

-- | A collection, as far as the store performs it.
data GCRequest = GCRequest
  { -- | What it does.
    gcAction :: !GCAction,
    -- | Which paths it considers.
    gcPaths :: !GCPaths,
    -- | How many bytes it may free before it stops, 0 for no limit.
    gcMaxFreed :: !Word64
  }
  deriving (Eq, Show)

-- | An operation a client asks of the daemon, with what it sends before
-- the daemon begins its work.  An upload's bytes follow its request.
data Request
  = -- | Settings for the rest of the connection.
    SetOptions !ClientSettings
  | -- | Whether a path is valid.
    IsValidPath !StorePath
  | -- | Which of the paths are valid.
    QueryValidPaths !(Set StorePath) !Substitute
  | -- | Which of the paths a substituter offers.
    QuerySubstitutablePaths !(Set StorePath)
  | -- | The valid paths that refer to a path.
    QueryReferrers !StorePath
  | -- | The valid derivations that output a path.
    QueryValidDerivers !StorePath
  | -- | A derivation's outputs and the paths they have, where known.
    QueryDerivationOutputMap !StorePath
  | -- | Every valid path.
    QueryAllValidPaths
  | -- | What the store records of a path.
    QueryPathInfo !StorePath
  | -- | What realising the paths would build, substitute or not find.
    QueryMissing ![DerivedPath]
  | -- | Make a content-addressed path from a dump that follows.
    AddToStore !AddToStoreRequest
  | -- | Import a path from the NAR that follows.
    AddToStoreNar !ValidPathInfo !Repair !CheckSigs
  | -- | Import the paths that follow, each its info and its NAR.
    AddMultipleToStore !Repair !CheckSigs
  | -- | Realise the paths.
    BuildPaths ![DerivedPath] !BuildMode
  | -- | Realise the paths, with each one's result.
    BuildPathsWithResults ![DerivedPath] !BuildMode
  | -- | Make a path valid, substituting it if need be.
    EnsurePath !StorePath
  | -- | Keep a path from collection while the connection lasts.
    AddTempRoot !StorePath
  | -- | Keep what a link names from collection while the link exists.
    AddIndirectRoot !ByteString
  | -- | The roots that keep paths from collection.
    FindRoots
  | -- | Collect garbage.
    CollectGarbage !GCRequest !Liveness
  | -- | Share identical files in the store.
    OptimiseStore
  | -- | Check the store's consistency.
    VerifyStore !CheckContents !Repair
  | -- | Add signatures to a path's info.
    AddSignatures !StorePath !(Set Signature)
  | -- | A path's NAR serialisation.
    NarFromPath !StorePath
  deriving (Eq, Show)

-- | The operations served, each with its code on the wire.
data Op
  = OpIsValidPath
  | OpQueryReferrers
  | OpAddToStore
  | OpBuildPaths
  | OpEnsurePath
  | OpAddTempRoot
  | OpAddIndirectRoot
  | OpFindRoots
  | OpSetOptions
  | OpCollectGarbage
  | OpQueryAllValidPaths
  | OpQueryPathInfo
  | OpQueryValidPaths
  | OpQuerySubstitutablePaths
  | OpQueryValidDerivers
  | OpOptimiseStore
  | OpVerifyStore
  | OpAddSignatures
  | OpNarFromPath
  | OpAddToStoreNar
  | OpQueryMissing
  | OpQueryDerivationOutputMap
  | OpAddMultipleToStore
  | OpBuildPathsWithResults
  deriving (Eq, Show, Enum, Bounded)

-- | An operation's code.
opCode :: Op -> Word32
opCode = \case
  OpIsValidPath -> 1
  OpQueryReferrers -> 6
  OpAddToStore -> 7
  OpBuildPaths -> 9
  OpEnsurePath -> 10
  OpAddTempRoot -> 11
  OpAddIndirectRoot -> 12
  OpFindRoots -> 14
  OpSetOptions -> 19
  OpCollectGarbage -> 20
  OpQueryAllValidPaths -> 23
  OpQueryPathInfo -> 26
  OpQueryValidPaths -> 31
  OpQuerySubstitutablePaths -> 32
  OpQueryValidDerivers -> 33
  OpOptimiseStore -> 34
  OpVerifyStore -> 35
  OpAddSignatures -> 37
  OpNarFromPath -> 38
  OpAddToStoreNar -> 39
  OpQueryMissing -> 40
  OpQueryDerivationOutputMap -> 41
  OpAddMultipleToStore -> 44
  OpBuildPathsWithResults -> 46

-- | The operations served, by code.
opsByCode :: Map Word32 Op
opsByCode = Map.fromList [(opCode op, op) | op <- [minBound .. maxBound]]

-- | The info an upload sends for a path, which upstream reads as the
-- version that carries it whole sends it, whatever version was agreed.
getUploadPathInfo :: StoreDir -> Decoder ValidPathInfo
getUploadPathInfo storeDir = getValidPathInfo storeDir (Version (VersionNumber 1 16) Set.empty)

-- | A query's substitute flag is sent from this version.
substituteFlagSince :: VersionNumber
substituteFlagSince = VersionNumber 1 27

-- | AddToStore sends a method and a framed dump from this version, and an
-- unframed NAR before it.
framedAddToStoreSince :: VersionNumber
framedAddToStoreSince = VersionNumber 1 25

-- | AddToStoreNar sends its NAR framed from this version, and before it
-- either unframed or on the daemon's request.
framedAddToStoreNarSince :: VersionNumber
framedAddToStoreNarSince = VersionNumber 1 23

-- | The request an operation code names, read as upstream's daemon reads
-- it before the operation's work begins, or 'Nothing' for a code this
-- daemon does not serve.  A form older than this daemon reads, in which
-- an upload is not framed, is refused as the request begins.
getRequest :: StoreDir -> Version -> Word32 -> Maybe (Decoder Request)
getRequest storeDir version code = requestFor <$> Map.lookup code opsByCode
  where
    path = getStorePath storeDir
    paths = getStorePathSet storeDir
    derivedPaths = getList (getDerivedPath storeDir version)
    since gate = versionNumber version >= gate
    requestFor = \case
      OpSetOptions -> SetOptions <$> getClientSettings
      OpIsValidPath -> IsValidPath <$> path
      OpQueryValidPaths ->
        QueryValidPaths <$> paths <*> (if since substituteFlagSince then substituteFrom <$> intFlag else pure NoSubstitute)
      OpQuerySubstitutablePaths -> QuerySubstitutablePaths <$> paths
      OpQueryReferrers -> QueryReferrers <$> path
      OpQueryValidDerivers -> QueryValidDerivers <$> path
      OpQueryDerivationOutputMap -> QueryDerivationOutputMap <$> path
      OpQueryAllValidPaths -> pure QueryAllValidPaths
      OpQueryPathInfo -> QueryPathInfo <$> path
      OpQueryMissing -> QueryMissing <$> derivedPaths
      OpAddToStore
        | since framedAddToStoreSince ->
            fmap AddToStore $
              AddToStoreRequest <$> getBytes <*> getBytes <*> paths <*> (repairFrom <$> getBool)
        | otherwise -> unframedForm "AddToStore" framedAddToStoreSince
      OpAddToStoreNar
        | since framedAddToStoreNarSince ->
            AddToStoreNar <$> getUploadPathInfo storeDir <*> (repairFrom <$> getBool) <*> (checkSigsFrom <$> getBool)
        | otherwise -> unframedForm "AddToStoreNar" framedAddToStoreNarSince
      OpAddMultipleToStore -> AddMultipleToStore . repairFrom <$> getBool <*> (checkSigsFrom <$> getBool)
      OpBuildPaths -> BuildPaths <$> derivedPaths <*> getBuildMode
      OpBuildPathsWithResults -> BuildPathsWithResults <$> derivedPaths <*> getBuildMode
      OpEnsurePath -> EnsurePath <$> path
      OpAddTempRoot -> AddTempRoot <$> path
      OpAddIndirectRoot -> AddIndirectRoot . absoluteLink <$> getBytes
      OpFindRoots -> pure FindRoots
      OpCollectGarbage -> getCollectGarbage storeDir version
      OpOptimiseStore -> pure OptimiseStore
      OpVerifyStore -> VerifyStore . checkContentsFrom <$> getBool <*> (repairFrom <$> getBool)
      OpAddSignatures -> AddSignatures <$> path <*> getSignatureSet
      OpNarFromPath -> NarFromPath <$> path
    unframedForm name gate =
      malformed (name <> " before protocol 1." <> T.pack (show (vnMinor gate)) <> " is not supported by this daemon")

-- | A flag upstream reads as an @unsigned int@ and takes as set when it is
-- not 0.
intFlag :: Decoder Bool
intFlag = (/= 0) <$> getWord32

substituteFrom :: Bool -> Substitute
substituteFrom set = if set then Substitute else NoSubstitute

repairFrom :: Bool -> Repair
repairFrom set = if set then Repair else NoRepair

-- | A client's flag that the signatures need not be checked.
checkSigsFrom :: Bool -> CheckSigs
checkSigsFrom dontCheck = if dontCheck then NoCheckSigs else CheckSigs

checkContentsFrom :: Bool -> CheckContents
checkContentsFrom set = if set then CheckContents else SkipContents

-- | A collection's request.  Without 'featureDeleteDeadSpecificReferrers'
-- its paths are a bare set: none means the whole store, unless the action
-- is to delete the paths given, and a collection of the dead limited to
-- some paths is refused once everything is read.  The three numbers after
-- the limit are obsolete.
getCollectGarbage :: StoreDir -> Version -> Decoder Request
getCollectGarbage storeDir version = do
  action <- getGCAction
  considered <-
    if withFeature
      then getGCPaths storeDir
      else bareSet action <$> getStorePathSet storeDir
  liveness <- (\ignore -> if ignore then IgnoreLiveness else KeepLiveness) <$> getBool
  maxFreed <- getWord64
  replicateM_ obsoleteCollectionFields getWord32
  when (action == DeleteDead && specific considered && not withFeature) $
    malformed "Garbage collecting specific paths requested but it is not supported by the negotiated protocol"
  pure (CollectGarbage (GCRequest action considered maxFreed) liveness)
  where
    withFeature = Set.member featureDeleteDeadSpecificReferrers (versionFeatures version)
    bareSet action given
      | action /= DeleteSpecific && Set.null given = WholeStore
      | otherwise = SpecificPaths given False
    specific = \case
      SpecificPaths _ _ -> True
      WholeStore -> False

-- | How many obsolete numbers follow a collection's limit.
obsoleteCollectionFields :: Int
obsoleteCollectionFields = 3

-- | A link's path made absolute as upstream's daemon makes it, in the
-- directory its workers run in, @/@: a relative path is taken from @/@,
-- and the result is collapsed lexically, no link followed.
absoluteLink :: ByteString -> ByteString
absoluteLink link = BS.cons slash (BS.intercalate (BS.singleton slash) (reverse (foldl' step [] (BS.split slash link))))
  where
    slash = 0x2f
    step kept component
      | BS.null component || component == "." = kept
      | component == ".." = drop 1 kept
      | otherwise = component : kept

-- ---------------------------------------------------------------------------
-- Trust
-- ---------------------------------------------------------------------------

-- | What becomes of a request.
data Authorization
  = -- | The request, as the daemon performs it for this client.
    Permitted !Request
  | -- | Refused before the request's work begins, which ends the
    -- connection.
    RefusedBeforeWork !Text
  | -- | Refused once its work has begun: the client is told, and the
    -- connection goes on.
    RefusedInWork !Text
  deriving (Eq, Show)

-- | Upstream's rules for what a client may ask.  A client the daemon
-- does not trust may not repair (an import's or a build's repair is not
-- atomic), its imports are always held to the signatures the store
-- trusts and are never ultimate, and the roots it is shown are censored
-- ('rootCensoring').  No client may have a collection ignore liveness.
-- A refusal before work and one in work differ only in that the second
-- leaves the connection open, which is where upstream checks each.
authorize :: Trust -> Request -> Authorization
authorize trust request = case request of
  AddToStore upload
    | untrusted && atsRepair upload == Repair -> RefusedBeforeWork repairPathsRefusal
  AddToStoreNar (ValidPathInfo path info) repair _
    | untrusted && repair == Repair -> RefusedBeforeWork repairStorePathsRefusal
    | untrusted -> Permitted (AddToStoreNar (ValidPathInfo path info {upiUltimate = False}) repair CheckSigs)
  AddMultipleToStore repair _
    | untrusted && repair == Repair -> RefusedBeforeWork repairStorePathsRefusal
    | untrusted -> Permitted (AddMultipleToStore repair CheckSigs)
  BuildPaths _ BuildRepair
    | untrusted -> RefusedBeforeWork repairBuildRefusal
  BuildPathsWithResults _ BuildRepair
    | untrusted -> RefusedBeforeWork repairBuildRefusal
  VerifyStore _ Repair
    | untrusted -> RefusedInWork repairPathsRefusal
  CollectGarbage _ IgnoreLiveness -> RefusedInWork "you are not allowed to ignore liveness"
  _ -> Permitted request
  where
    untrusted = trust == NotTrusted

-- | Upstream's refusals of a repair to a client it does not trust, each
-- worded as the operations that raise it word it.
repairPathsRefusal, repairStorePathsRefusal, repairBuildRefusal :: Text
repairPathsRefusal = "you are not privileged to repair paths"
repairStorePathsRefusal = "you are not privileged to repair store paths"
repairBuildRefusal = "repairing is not allowed because you are not in 'trusted-users'"

-- | Whether a client is shown what holds each root.
rootCensoring :: Trust -> RootCensoring
rootCensoring = \case
  Trusted -> ShowRoots
  NotTrusted -> CensorRoots

-- ---------------------------------------------------------------------------
-- Client settings
-- ---------------------------------------------------------------------------

-- | The settings a client sends, as upstream's daemon reads them.
data ClientSettings = ClientSettings
  { csKeepFailed :: !Bool,
    csKeepGoing :: !Bool,
    csTryFallback :: !Bool,
    -- | How much the client is told, at most upstream's loudest level, 7:
    -- a louder one is taken as that.
    csVerbosity :: !Verbosity,
    csMaxBuildJobs :: !Word32,
    csMaxSilentTime :: !Word32,
    csVerboseBuild :: !Bool,
    csBuildCores :: !Word32,
    csUseSubstitutes :: !Bool,
    -- | Other settings by name, the first a name is sent with winning, as
    -- upstream keeps them.
    csOverrides :: !(Map ByteString ByteString)
  }
  deriving (Eq, Show)

-- | Upstream's loudest verbosity (@lvlVomit@).
loudestVerbosity :: Word32
loudestVerbosity = 7

-- | A client's settings, as upstream's daemon reads them: three flags,
-- its verbosity, its build limits, an obsolete build-hook flag, its
-- build-log verbosity (verbose when it is the error level), two obsolete
-- logging numbers, its cores and substitution flag, and the other
-- settings as name and value pairs.  Each number is an @unsigned int@.
getClientSettings :: Decoder ClientSettings
getClientSettings = do
  keepFailed <- intFlag
  keepGoing <- intFlag
  tryFallback <- intFlag
  verbosity <- Verbosity . min loudestVerbosity <$> getWord32
  maxBuildJobs <- getWord32
  maxSilentTime <- getWord32
  _useBuildHook <- getWord32
  verboseBuild <- (== errorVerbosity) . Verbosity <$> getWord32
  _logType <- getWord32
  _printBuildTrace <- getWord32
  buildCores <- getWord32
  useSubstitutes <- intFlag
  count <- getWord32
  overrides <- replicateM (fromIntegral count) ((,) <$> getBytes <*> getBytes)
  pure
    ClientSettings
      { csKeepFailed = keepFailed,
        csKeepGoing = keepGoing,
        csTryFallback = tryFallback,
        csVerbosity = verbosity,
        csMaxBuildJobs = maxBuildJobs,
        csMaxSilentTime = maxSilentTime,
        csVerboseBuild = verboseBuild,
        csBuildCores = buildCores,
        csUseSubstitutes = useSubstitutes,
        csOverrides = Map.fromListWith (\_later earlier -> earlier) overrides
      }

-- | What a connection's operations run under: the daemon's own until the
-- client sends settings, and then those its trust lets it change.
data ConnectionSettings = ConnectionSettings
  { -- | How much the client is told.
    connVerbosity :: !Verbosity,
    -- | The daemon's settings, as the client changed them.
    connConfig :: !NixConfig,
    -- | What the client last sent, where it has.
    connClient :: !(Maybe ClientSettings)
  }
  deriving (Eq, Show)

-- | A client's settings applied to its connection, as upstream's daemon
-- applies them, with the warnings it gives on the way, each the message a
-- warning-level line carries.
--
-- The client's verbosity, keep-failed, fallback and cores apply
-- whatever its trust.  Then each other setting, in name order: an
-- obsolete one is ignored, as are experimental features (nova-nix knows
-- none, so a client's list always reads as the daemon's own, and
-- upstream's note that it differs never fires), and @plugin-files@ is
-- ignored with upstream's warning.  A trusted client's setting applies as
-- a @nix.conf@ line would ('applyAssignment'), one nova-nix does not
-- model being ignored as upstream ignores an unknown one; so does an
-- untrusted client's @timeout@, @max-silent-time@, @build-poll-interval@,
-- @connect-timeout@ or empty @builders@.  An untrusted client's
-- substituters keep only those the daemon trusts (its trusted
-- substituters and those it already uses), each other one warned of, and
-- its other settings are refused with upstream's warning.  A value a
-- setting cannot take is warned of as upstream warns of a value it
-- refuses, and the setting keeps its value.
--
-- One divergence: upstream compares substituters as parsed store
-- references, and these are compared by their spelling, a URL that has
-- no slash ending what precedes its query also matching one that has, as
-- upstream's comparison allows.  Two spellings of one reference's
-- parameters in different orders match there and not here.
applyClientSettings :: Trust -> [Text] -> ClientSettings -> ConnectionSettings -> (ConnectionSettings, [ByteString])
applyClientSettings trust trustedSubstituters client current =
  ( current {connVerbosity = csVerbosity client, connConfig = configured, connClient = Just client},
    reverse warnings
  )
  where
    fixed =
      (connConfig current)
        { ncKeepFailed = csKeepFailed client,
          ncFallback = csTryFallback client,
          ncCores = Just (csBuildCores client)
        }
    (configured, warnings) = foldl' override (fixed, []) (Map.toAscList (csOverrides client))
    override (config, said) (name, value)
      | name `elem` ignoredSettings = (config, said)
      | name == pluginFilesSetting = (config, warningLine pluginFilesWarning : said)
      | trust == Trusted || name `elem` unrestrictedSettings || (name == buildersSetting && BS.null value) =
          either (\refusal -> (config, usageWarning refusal : said)) (\applied -> (applied, said)) (applyAssignment config (ConfigAssignment name value))
      | name `elem` substituterSettings =
          let (kept, refused) = trustedOnly (trustedSubstituters ++ ncSubstituters config) value
           in (config {ncSubstituters = kept}, reverse (map (warningLine . untrustedSubstituterWarning) refused) ++ said)
      | otherwise = (config, warningLine (restrictedWarning name) : said)

-- | Settings upstream's daemon ignores from a client: an obsolete one,
-- and the experimental features.
ignoredSettings :: [ByteString]
ignoredSettings = ["ssh-auth-sock", "experimental-features"]

-- | The setting a client may not set, with a warning of its own.
pluginFilesSetting :: ByteString
pluginFilesSetting = "plugin-files"

-- | The settings an untrusted client may set (each upstream setting's own
-- name, not an alias).
unrestrictedSettings :: [ByteString]
unrestrictedSettings = ["timeout", "max-silent-time", "build-poll-interval", "connect-timeout"]

-- | The setting an untrusted client may set only to nothing.
buildersSetting :: ByteString
buildersSetting = "builders"

-- | The names of the substituters setting.
substituterSettings :: [ByteString]
substituterSettings = ["substituters", "binary-caches"]

-- | The substituters a list names that are trusted, as the trusted
-- spelling, and the ones that are not, as sent.
trustedOnly :: [Text] -> ByteString -> ([Text], [ByteString])
trustedOnly trusted value = (mapMaybe snd matched, [token | (token, Nothing) <- matched])
  where
    known = Map.fromList [(TE.encodeUtf8 name, name) | name <- trusted]
    matched = [(token, trustedSpelling token) | token <- BS8.splitWith (`BS8.elem` " \t\n\r") value, not (BS.null token)]
    trustedSpelling token = case Map.lookup token known of
      Just name -> Just name
      Nothing
        | "://" `BS.isInfixOf` token,
          (base, query) <- BS8.break (== '?') token,
          not ("/" `BS.isSuffixOf` base) ->
            Map.lookup (base <> "/" <> query) known
        | otherwise -> Nothing

-- | A warning as upstream logs one it raises itself: its prefix, the
-- message, and the line feed every logged line ends with.
warningLine :: ByteString -> ByteString
warningLine message = warningPrefix <> message <> "\n"

-- | A refused value as upstream shows the error that refuses it, at the
-- warning level: its prefix, then the message with each line after the
-- first indented to the prefix's width and each line's trailing space
-- dropped.
usageWarning :: Text -> ByteString
usageWarning refusal = BS.intercalate "\n" (zipWith indented (warningPrefix : repeat indent) (BS8.lines (chomp (TE.encodeUtf8 refusal))))
  where
    indent = BS8.replicate (BS.length warningPrefix) ' '
    indented prefix line = chomp (prefix <> line)

-- | Bytes without their trailing whitespace, as upstream trims a line.
chomp :: ByteString -> ByteString
chomp = BS8.dropWhileEnd (`BS8.elem` " \n\r\t")

warningPrefix :: ByteString
warningPrefix = "warning: "

pluginFilesWarning :: ByteString
pluginFilesWarning =
  "Ignoring the client-specified plugin-files.\nThe client specifying plugins to the daemon never made sense, and was removed in Nix >=2.14."

untrustedSubstituterWarning :: ByteString -> ByteString
untrustedSubstituterWarning substituter =
  "ignoring untrusted substituter '"
    <> substituter
    <> "', you are not a trusted user.\nRun `man nix.conf` for more information on the `substituters` configuration option."

restrictedWarning :: ByteString -> ByteString
restrictedWarning name =
  "ignoring the client-specified setting '" <> name <> "', because it is a restricted setting and you are not a trusted user"

-- ---------------------------------------------------------------------------
-- The store
-- ---------------------------------------------------------------------------

-- | What an operation reports while it works, which the daemon forwards
-- to the client as upstream's daemon forwards its logger's calls.
data LogEvent
  = -- | A message at a level, without its line feed.  The client is sent
    -- it if its verbosity reaches that level.
    LogLine !Verbosity !ByteString
  | -- | An activity started.
    ActivityStarted !Activity
  | -- | An activity stopped.
    ActivityStopped !ActivityId
  | -- | An activity reported a result of a type, with fields: a build's
    -- log line, for one, is a result of its build's activity.
    ActivityResult !ActivityId !ResultType ![Field]
  deriving (Eq, Show)

-- | What the store is given for an operation's work: where it reports,
-- and the connection's settings.  The report is the operation's for as
-- long as the call runs.
data Work m = Work
  { workReport :: !(LogEvent -> m ()),
    workSettings :: !ConnectionSettings
  }

-- | The store taking in the bytes of an upload as the daemon receives
-- them.  It may stop before they end, having what it needs or having
-- failed; the daemon then reads past the rest itself.
data Intake m a
  = -- | It takes the next bytes, hears that they have ended, or hears
    -- that the upload broke off before its end could be read, the
    -- operation failing with the reason the daemon has, so it releases
    -- what it holds for the upload.
    Taking !(ByteString -> m (Intake m a)) !(m (Either DaemonError a)) !(m ())
  | -- | It needs nothing more.
    Taken !(Either DaemonError a)

-- | A dump to make a content-addressed path from: the name, the method
-- and the algorithm it is hashed with, the paths it refers to, and
-- whether a valid path's contents are replaced.  A flat dump is a file's
-- contents and a recursive one a NAR.
data DumpUpload = DumpUpload
  { duName :: !ByteString,
    duMethod :: !ContentAddressMethod,
    duAlgo :: !Text,
    duReferences :: !(Set StorePath),
    duRepair :: !Repair
  }
  deriving (Eq, Show)

-- | What realising some paths would take.
data Missing = Missing
  { missingWillBuild :: !(Set StorePath),
    missingWillSubstitute :: !(Set StorePath),
    missingUnknown :: !(Set StorePath),
    -- | The bytes substituting would download.
    missingDownloadSize :: !Word64,
    -- | The NAR bytes substituting would unpack.
    missingNarSize :: !Word64
  }
  deriving (Eq, Show)

-- | What a collection did.
data GCResult = GCResult
  { -- | The paths it found or deleted, as upstream names them.
    gcResultPaths :: !(Set ByteString),
    -- | The bytes it freed.
    gcResultBytesFreed :: !Word64
  }
  deriving (Eq, Show)

-- | The store a daemon serves, as one connection sees it: each of
-- upstream's store calls an operation makes, given the operation's
-- t'Work', in the protocol's own terms.  An expected failure is a
-- t'DaemonError', which the client is sent as upstream's daemon sends
-- the error its store throws.  A connection's temporary roots are the
-- ones its record adds, so each connection is given a record of its own.
data DaemonStore m = DaemonStore
  { dsIsValidPath :: !(Work m -> StorePath -> m (Either DaemonError Bool)),
    -- | The valid paths among those given, having first substituted them
    -- where asked to.
    dsQueryValidPaths :: !(Work m -> Substitute -> Set StorePath -> m (Either DaemonError (Set StorePath))),
    dsQuerySubstitutablePaths :: !(Work m -> Set StorePath -> m (Either DaemonError (Set StorePath))),
    dsQueryReferrers :: !(Work m -> StorePath -> m (Either DaemonError (Set StorePath))),
    dsQueryValidDerivers :: !(Work m -> StorePath -> m (Either DaemonError (Set StorePath))),
    -- | Each output of the derivation, and its path where it is known.
    dsQueryDerivationOutputMap :: !(Work m -> StorePath -> m (Either DaemonError (Map OutputName (Maybe StorePath)))),
    dsQueryAllValidPaths :: !(Work m -> m (Either DaemonError (Set StorePath))),
    -- | A valid path's info, or 'Nothing' for a path that is not valid.
    dsQueryPathInfo :: !(Work m -> StorePath -> m (Either DaemonError (Maybe UnkeyedValidPathInfo))),
    dsQueryMissing :: !(Work m -> [DerivedPath] -> m (Either DaemonError Missing)),
    -- | Make a path from a dump, and give its info.
    dsAddToStore :: !(Work m -> DumpUpload -> m (Intake m ValidPathInfo)),
    -- | Import a path from its NAR.
    dsAddToStoreNar :: !(Work m -> ValidPathInfo -> Repair -> CheckSigs -> m (Intake m ())),
    dsBuildPaths :: !(Work m -> [DerivedPath] -> BuildMode -> m (Either DaemonError ())),
    -- | Each path's result, keyed by the path as it was asked for.
    dsBuildPathsWithResults :: !(Work m -> [DerivedPath] -> BuildMode -> m (Either DaemonError [KeyedBuildResult])),
    dsEnsurePath :: !(Work m -> StorePath -> m (Either DaemonError ())),
    dsAddTempRoot :: !(Work m -> StorePath -> m (Either DaemonError ())),
    -- | Record a link, as the bytes of its absolute path, as a root.
    dsAddIndirectRoot :: !(Work m -> ByteString -> m (Either DaemonError ())),
    -- | Each rooted path and what holds it.
    dsFindRoots :: !(Work m -> RootCensoring -> m (Either DaemonError (Map StorePath (Set ByteString)))),
    dsCollectGarbage :: !(Work m -> GCRequest -> m (Either DaemonError GCResult)),
    dsOptimiseStore :: !(Work m -> m (Either DaemonError ())),
    -- | Whether the check found errors.
    dsVerifyStore :: !(Work m -> CheckContents -> Repair -> m (Either DaemonError Bool)),
    dsAddSignatures :: !(Work m -> StorePath -> Set Signature -> m (Either DaemonError ())),
    -- | Write a path's NAR, a chunk at a time, to what is given.  It runs
    -- after the operation's work is reported done, where upstream's
    -- daemon writes the NAR, so it has nothing to report through.
    dsNarFromPath :: !(StorePath -> (ByteString -> m ()) -> m (Either DaemonError ()))
  }

-- | An error with upstream's defaults for one its daemon raises itself:
-- the error level, no traces, and exit status 1.
daemonError :: Text -> DaemonError
daemonError message = DaemonError errorVerbosity (TE.encodeUtf8 message) [] defaultExitStatus

-- ---------------------------------------------------------------------------
-- Archives in uploads
-- ---------------------------------------------------------------------------

-- | Where an archive ends in the bytes of an upload, told a chunk at a
-- time from the archive's first byte.  The daemon gives the store an
-- archive's bytes up to its end and reads what follows itself, so the
-- store's reader sees exactly one archive.
data NarBoundary
  = -- | The archive goes on past what has been fed: every byte so far is
    -- its, and the next chunk is fed here.
    BoundaryAhead !(ByteString -> NarBoundary)
  | -- | The archive ended, followed by these bytes of the last chunk fed.
    BoundaryReached !ByteString

-- | An archive's end as the size its path's info declares: the boundary
-- an upload's NAR has here.  Upstream instead parses the NAR to its root
-- node's close, and then skips to the declared size where its store read
-- less, which words its refusal of a declared size that does not match
-- the archive differently; a store that holds the archive to its info's
-- size and hash accepts the same uploads either way.  A parsing boundary
-- needs nova-cache's parser of an archive that more input follows, which
-- the release nova-nix's bound admits does not have; once it does, that
-- chunk-fed parse is a 'BoundaryAhead' that reaches its end where the
-- root node closes, with a third case for bytes that are no archive.
narSizeBoundary :: Word64 -> NarBoundary
narSizeBoundary remaining
  | remaining == 0 = BoundaryReached BS.empty
  | otherwise = BoundaryAhead $ \chunk ->
      let size = fromIntegral (BS.length chunk)
       in if size <= remaining
            then narSizeBoundary (remaining - size)
            else BoundaryReached (BS.drop (fromIntegral remaining) chunk)
