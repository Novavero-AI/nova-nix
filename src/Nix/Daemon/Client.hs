{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

-- | A client of a daemon that speaks upstream's worker protocol, as
-- upstream's remote store is one: the handshake as the client, then each
-- operation it asks, its request, any upload after it, the daemon's
-- messages while it works and the reply.
--
-- == The session
--
-- A t'ClientT' runs over a t'Transport', the daemon's side being the
-- other end, after the handshake 'runClient' makes: the client offers the
-- newest version it speaks and its features, and the session runs at the
-- lower of the two numbers, with the features both list, refusing a
-- daemon older than upstream's oldest.  The core is pure: everything the
-- client does to the world goes through the transport and the report the
-- daemon's messages are given to ('loggerReport' reports them as
-- upstream's simple logger does).
--
-- == Failures
--
-- An operation the daemon fails is a 'DaemonFailed', which 'attempt'
-- turns into a value, the session going on as upstream's does after an
-- error its daemon sends; one the client will not send at the agreed
-- version is a 'NotSent', which leaves the session as it was; and a
-- connection that broke, or that the daemon sent bytes the protocol does
-- not allow, is a 'ConnectionLost', after which nothing more is read.
--
-- == Uploads
--
-- An upload is framed, as from the versions every daemon serving it
-- frames it.  The client sends the whole upload before it reads the
-- daemon's messages, which upstream's client reads on a thread of their
-- own while it sends; a transport that cannot hold what the daemon says
-- while an upload is under way is to read it concurrently.
--
-- == Which store
--
-- Whether a command goes through a daemon is what 'chooseStore' answers
-- for what @--store@ names ('parseStoreTarget'): by default the daemon
-- whenever its socket accepts a connection, root included, and the store
-- directly otherwise.
module Nix.Daemon.Client
  ( -- * The session
    ClientT,
    ClientFailure (..),
    runClient,
    attempt,
    agreedVersion,
    daemonInfo,
    offeredVersion,

    -- * Requests
    putRequest,
    putClientSettings,

    -- * Operations
    setOptions,
    isValidPath,
    queryValidPaths,
    querySubstitutablePaths,
    queryReferrers,
    queryValidDerivers,
    queryDerivationOutputMap,
    queryAllValidPaths,
    queryPathInfo,
    queryMissing,
    addToStore,
    addToStoreNar,
    buildPaths,
    buildPathsWithResults,
    ensurePath,
    addTempRoot,
    addIndirectRoot,

    -- * The daemon's messages
    resBuildLogLine,
    resPostBuildLogLine,
    loggerReport,
    disconnectedMessage,

    -- * Which store
    StoreTarget (..),
    parseStoreTarget,
    StoreChoice (..),
    chooseStore,
  )
where

import Control.Monad (void, when)
import Control.Monad.Except (ExceptT, MonadError, catchError, runExceptT, throwError)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.State.Strict (MonadState, StateT, evalStateT, gets, modify')
import Control.Monad.Trans (lift)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Char8 as BS8
import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word32)
import Nix.Daemon.Connection (Transport (..))
import Nix.Daemon.Ops (AddToStoreRequest (..), CheckSigs (..), ClientSettings (..), Missing (..), Repair (..), Request (..), Substitute (..))
import Nix.Daemon.Protocol
  ( Activity (..),
    BuildMode,
    DaemonError,
    DerivedPath,
    Field (..),
    HandshakeInfo,
    KeyedBuildResult,
    OutputName,
    ResultType (..),
    StderrMessage (..),
    UnkeyedValidPathInfo,
    ValidPathInfo,
    Verbosity (..),
    Version (..),
    VersionNumber (..),
    featureDeleteDeadSpecificReferrers,
    featureRealisationWithPath,
    getFeatures,
    getHandshakeInfo,
    getKeyedBuildResult,
    getOptionalStorePath,
    getOutputName,
    getServerMagic,
    getStderrMessage,
    getStorePathSet,
    getUnkeyedValidPathInfo,
    getValidPathInfo,
    getVersionNumber,
    latestVersionNumber,
    minimumVersionNumber,
    putBuildMode,
    putClientMagic,
    putClientObsoleteFields,
    putDerivedPath,
    putFeatures,
    putStorePath,
    putStorePathSet,
    putValidPathInfo,
    putVersionNumber,
  )
import Nix.Daemon.Wire (DecodeError (..), Decoder, Decoding (..), feedInput, getBool, getList, getWord64, putBool, putBytes, putFrame, putFrameEnd, putList, putNum, startDecoding)
import Nix.Logger (Logger (..))
import Nix.Store.Path (StoreDir, StorePath)

-- ---------------------------------------------------------------------------
-- The session
-- ---------------------------------------------------------------------------

-- | Why an operation, or the session, did not get its answer.
data ClientFailure
  = -- | The daemon failed the operation, with its error, which upstream's
    -- client throws as the daemon's.  The session goes on, unless the
    -- daemon closed the connection with it, which the next operation
    -- finds.
    DaemonFailed !DaemonError
  | -- | The client will not send the operation at the agreed version, for
    -- the reason given; nothing was sent.
    NotSent !Text
  | -- | The connection broke, or the daemon sent what the protocol does
    -- not allow, for the reason given.  Nothing more can be read.
    ConnectionLost !Text
  deriving (Eq, Show)

-- | What every step of a session reads.
data ClientEnv m = ClientEnv
  { ceStoreDir :: !StoreDir,
    ceTransport :: !(Transport m),
    ceReport :: !(StderrMessage -> m ())
  }

-- | What a session holds: the version agreed, what the daemon told the
-- client, and the bytes the daemon sent that have not been read.
data Session = Session
  { sessVersion :: !Version,
    sessInfo :: !(Maybe HandshakeInfo),
    sessInput :: !ByteString
  }

-- | A session's steps over the monad its transport runs in.  A failure
-- ends the steps that follow it unless 'attempt' catches it, and leaves
-- what the session has read read, so a caught failure leaves the session
-- where the daemon's bytes put it.
newtype ClientT m a = ClientT (ReaderT (ClientEnv m) (ExceptT ClientFailure (StateT Session m)) a)
  deriving (Functor, Applicative, Monad, MonadReader (ClientEnv m), MonadError ClientFailure, MonadState Session)

-- | Run an action in the transport's monad.
inner :: (Monad m) => m a -> ClientT m a
inner = ClientT . lift . lift . lift

-- | The version this client offers: the newest number upstream speaks,
-- with the features the codec carries.
offeredVersion :: Version
offeredVersion = Version latestVersionNumber (Set.fromList [featureRealisationWithPath, featureDeleteDeadSpecificReferrers])

-- | Make the handshake with a daemon over a transport, offering the
-- version given ('offeredVersion' as upstream's client offers its own),
-- naming paths under the store directory given, and giving what the
-- daemon says as it works to the report; then run the session's steps.
-- The session's input is the daemon's; nothing is closed here.
runClient :: (Monad m) => StoreDir -> Version -> Transport m -> (StderrMessage -> m ()) -> ClientT m a -> m (Either ClientFailure a)
runClient storeDir offered transport report (ClientT steps) =
  evalStateT (runExceptT (runReaderT (unwrap (handshake offered) *> steps) (ClientEnv storeDir transport report))) (Session (Version minimumVersionNumber Set.empty) Nothing BS.empty)
  where
    unwrap (ClientT step) = step

-- | Run steps, a failure of theirs answered as a value: what was read
-- stays read, and a 'ConnectionLost' leaves the session unusable all the
-- same.
attempt :: (Monad m) => ClientT m a -> ClientT m (Either ClientFailure a)
attempt steps = (Right <$> steps) `catchError` (pure . Left)

-- | The version the session agreed.
agreedVersion :: (Monad m) => ClientT m Version
agreedVersion = gets sessVersion

-- | What the daemon told the client at the handshake.
daemonInfo :: (Monad m) => ClientT m (Maybe HandshakeInfo)
daemonInfo = gets sessInfo

-- | The version a daemon's features are exchanged from.
featureExchangeSince :: VersionNumber
featureExchangeSince = VersionNumber 1 38

-- | Agree a version with the daemon, as upstream's client does: send the
-- opening word and the version offered together, read the daemon's word
-- and number, refuse another major version or one older than upstream's
-- oldest, and from 1.38 send the features offered and read the daemon's.
-- Then send the obsolete fields, read what the daemon tells its client,
-- and read its startup work to its end.
handshake :: (Monad m) => Version -> ClientT m ()
handshake offered = do
  send (putClientMagic <> putVersionNumber (versionNumber offered))
  receive getServerMagic
  daemonNumber <- receive getVersionNumber
  when (vnMajor daemonNumber /= vnMajor latestVersionNumber) (throwError (ConnectionLost "Nix daemon protocol version not supported"))
  let agreed = min daemonNumber (versionNumber offered)
  when (agreed < minimumVersionNumber) (throwError (ConnectionLost "the Nix daemon version is too old"))
  daemonFeatures <-
    if agreed >= featureExchangeSince
      then send (putFeatures (versionFeatures offered)) *> receive getFeatures
      else pure Set.empty
  let version = Version agreed (Set.intersection daemonFeatures (versionFeatures offered))
  modify' (\session -> session {sessVersion = version})
  send (putClientObsoleteFields version)
  info <- receive (getHandshakeInfo version)
  modify' (\session -> session {sessInfo = Just info})
  awaitWork

-- | Upstream's words for a daemon that went away mid-session.
disconnectedMessage :: Text
disconnectedMessage = "Nix daemon disconnected unexpectedly (maybe it crashed?)"

-- | Read a value the daemon sends, the input after it kept.
receive :: (Monad m) => Decoder a -> ClientT m a
receive decoder = do
  held <- gets sessInput
  modify' (\session -> session {sessInput = BS.empty})
  settle (if BS.null held then startDecoding decoder else feedInput (Just held) (startDecoding decoder))
  where
    settle = \case
      Decoded value rest -> value <$ modify' (\session -> session {sessInput = rest})
      DecodeFailed failure rest -> do
        modify' (\session -> session {sessInput = rest})
        throwError (ConnectionLost (decodeFailure failure))
      NeedInput more -> do
        transport <- asks ceTransport
        inner (transportReceive transport) >>= settle . more
    decodeFailure = \case
      InputEnded -> disconnectedMessage
      Malformed message -> message

-- | Send bytes to the daemon.
send :: (Monad m) => Builder -> ClientT m ()
send bytes = do
  transport <- asks ceTransport
  inner (transportSend transport bytes)

-- | Read the daemon's messages until its work on an operation is done,
-- each given to the report, as upstream's client reads them: the end of
-- the work ends the reading, and the daemon's error fails the operation.
-- The client sends no upload unframed and has no sink, so a request for
-- either is the daemon's breach of the session, in upstream's words.
awaitWork :: (Monad m) => ClientT m ()
awaitWork = do
  version <- gets sessVersion
  receive (getStderrMessage version) >>= \case
    StderrLast -> pure ()
    StderrError failure -> throwError (DaemonFailed failure)
    StderrRead _ -> throwError (ConnectionLost "no source")
    StderrWrite _ -> throwError (ConnectionLost "no sink")
    message -> do
      report <- asks ceReport
      inner (report message)
      awaitWork

-- ---------------------------------------------------------------------------
-- Requests
-- ---------------------------------------------------------------------------

-- | A request as the client sends it at a version: its operation's code
-- and what follows it, as upstream's daemon reads it, so the daemon's
-- reader takes back what this writes.  'Left' is why the client does not
-- send it at that version: an upload before it was framed, an operation
-- the version does not carry, or one this client never sends.  An
-- upload's bytes follow the request.
putRequest :: StoreDir -> Version -> Request -> Either Text Builder
putRequest storeDir version = \case
  SetOptions settings -> Right (code 19 <> putClientSettings settings)
  IsValidPath path -> Right (code 1 <> putStorePath storeDir path)
  QueryValidPaths paths substitute
    | since substituteFlagSince -> Right (code 31 <> putStorePathSet storeDir paths <> putBool (substitute == Substitute))
    | substitute == Substitute -> Left (tooOld "substituting in QueryValidPaths" substituteFlagSince)
    | otherwise -> Right (code 31 <> putStorePathSet storeDir paths)
  QuerySubstitutablePaths paths -> Right (code 32 <> putStorePathSet storeDir paths)
  QueryReferrers path -> Right (code 6 <> putStorePath storeDir path)
  QueryValidDerivers path -> Right (code 33 <> putStorePath storeDir path)
  QueryDerivationOutputMap path
    | since outputMapSince -> Right (code 41 <> putStorePath storeDir path)
    | otherwise -> Left (tooOld "QueryDerivationOutputMap" outputMapSince)
  QueryAllValidPaths -> Right (code 23)
  QueryPathInfo path -> Right (code 26 <> putStorePath storeDir path)
  QueryMissing targets
    | since queryMissingSince -> (code 40 <>) <$> derivedPaths targets
    | otherwise -> Left (tooOld "QueryMissing" queryMissingSince)
  AddToStore upload
    | since framedAddToStoreSince ->
        Right (code 7 <> putBytes (atsName upload) <> putBytes (atsMethod upload) <> putStorePathSet storeDir (atsReferences upload) <> putBool (atsRepair upload == Repair))
    | otherwise -> Left (tooOld "AddToStore" framedAddToStoreSince)
  AddToStoreNar info repair checkSigs
    | since framedAddToStoreNarSince ->
        Right (code 39 <> putValidPathInfo storeDir uploadInfoVersion info <> putBool (repair == Repair) <> putBool (checkSigs == NoCheckSigs))
    | otherwise -> Left (tooOld "AddToStoreNar" framedAddToStoreNarSince)
  BuildPaths targets mode -> (\paths -> code 9 <> paths <> putBuildMode mode) <$> derivedPaths targets
  BuildPathsWithResults targets mode
    | since buildResultsSince -> (\paths -> code 46 <> paths <> putBuildMode mode) <$> derivedPaths targets
    | otherwise -> Left (tooOld "BuildPathsWithResults" buildResultsSince)
  EnsurePath path -> Right (code 10 <> putStorePath storeDir path)
  AddTempRoot path -> Right (code 11 <> putStorePath storeDir path)
  AddIndirectRoot link -> Right (code 12 <> putBytes link)
  AddMultipleToStore {} -> Left (notSent "AddMultipleToStore")
  FindRoots -> Left (notSent "FindRoots")
  CollectGarbage {} -> Left (notSent "CollectGarbage")
  OptimiseStore -> Left (notSent "OptimiseStore")
  VerifyStore {} -> Left (notSent "VerifyStore")
  AddSignatures {} -> Left (notSent "AddSignatures")
  NarFromPath _ -> Left (notSent "NarFromPath")
  where
    code :: Word32 -> Builder
    code = putNum
    since gate = versionNumber version >= gate
    derivedPaths targets = (\written -> putNum (length targets) <> mconcat written) <$> traverse (putDerivedPath storeDir version) targets
    tooOld operation gate = operation <> " needs protocol 1." <> T.pack (show (vnMinor gate)) <> ", which the daemon does not speak"
    notSent operation = operation <> " is not sent by this client"

-- | A client's settings as upstream's client sends them: three flags, its
-- verbosity, its build limits, the obsolete build-hook flag set, its
-- build-log verbosity (the error level when the build is verbose, the
-- loudest otherwise), two obsolete logging numbers, its cores and
-- substitution flag, and the other settings as name and value pairs in
-- name order.
putClientSettings :: ClientSettings -> Builder
putClientSettings settings =
  flag (csKeepFailed settings)
    <> flag (csKeepGoing settings)
    <> flag (csTryFallback settings)
    <> putNum (verbosityWord (csVerbosity settings))
    <> putNum (csMaxBuildJobs settings)
    <> putNum (csMaxSilentTime settings)
    <> flag True
    <> putNum (if csVerboseBuild settings then errorLevel else loudestLevel)
    <> putNum (0 :: Word32)
    <> putNum (0 :: Word32)
    <> putNum (csBuildCores settings)
    <> flag (csUseSubstitutes settings)
    <> putList (\(name, value) -> putBytes name <> putBytes value) (Map.toAscList (csOverrides settings))
  where
    flag set = putNum (if set then 1 else 0 :: Word32)
    verbosityWord (Verbosity level) = level
    errorLevel = 0 :: Word32
    loudestLevel = 7 :: Word32

-- | The version upstream's client writes an upload's path info as,
-- whatever version was agreed: the one that carries it whole.
uploadInfoVersion :: Version
uploadInfoVersion = Version (VersionNumber 1 16) Set.empty

-- | The versions each gated request is sent from, as upstream's daemon
-- reads them.
substituteFlagSince, outputMapSince, queryMissingSince, framedAddToStoreSince, framedAddToStoreNarSince, buildResultsSince :: VersionNumber
substituteFlagSince = VersionNumber 1 27
outputMapSince = VersionNumber 1 22
queryMissingSince = VersionNumber 1 19
framedAddToStoreSince = VersionNumber 1 25
framedAddToStoreNarSince = VersionNumber 1 23
buildResultsSince = VersionNumber 1 34

-- ---------------------------------------------------------------------------
-- Operations
-- ---------------------------------------------------------------------------

-- | Send a request, then any upload after it, framed from a chunk source
-- that gives an empty chunk at its end, and read the daemon's work to its
-- end.
perform :: (Monad m) => Request -> Maybe (m ByteString) -> ClientT m ()
perform request upload = do
  storeDir <- asks ceStoreDir
  version <- gets sessVersion
  written <- either (throwError . NotSent) pure (putRequest storeDir version request)
  send written
  mapM_ sendFrames upload
  awaitWork
  where
    sendFrames source = do
      chunk <- inner source
      if BS.null chunk
        then send putFrameEnd
        else send (putFrame chunk) *> sendFrames source

-- | Ask, then read the reply.
ask :: (Monad m) => Request -> Decoder a -> ClientT m a
ask request reply = perform request Nothing *> receive reply

-- | Ask, then read the word an operation with nothing else to say replies
-- with.
done :: (Monad m) => Request -> ClientT m ()
done request = void (ask request getWord64)

-- | The store directory paths are named under.
storeDirOf :: (Monad m) => ClientT m StoreDir
storeDirOf = asks ceStoreDir

-- | Send the client's settings for the rest of the session.
setOptions :: (Monad m) => ClientSettings -> ClientT m ()
setOptions settings = perform (SetOptions settings) Nothing

-- | Whether a path is valid.
isValidPath :: (Monad m) => StorePath -> ClientT m Bool
isValidPath path = ask (IsValidPath path) getBool

-- | Which of the paths are valid, having first substituted them where
-- asked to.
queryValidPaths :: (Monad m) => Set StorePath -> Substitute -> ClientT m (Set StorePath)
queryValidPaths paths substitute = storeDirOf >>= ask (QueryValidPaths paths substitute) . getStorePathSet

-- | Which of the paths a substituter offers.
querySubstitutablePaths :: (Monad m) => Set StorePath -> ClientT m (Set StorePath)
querySubstitutablePaths paths = storeDirOf >>= ask (QuerySubstitutablePaths paths) . getStorePathSet

-- | The valid paths that refer to a path.
queryReferrers :: (Monad m) => StorePath -> ClientT m (Set StorePath)
queryReferrers path = storeDirOf >>= ask (QueryReferrers path) . getStorePathSet

-- | The valid derivations that output a path.
queryValidDerivers :: (Monad m) => StorePath -> ClientT m (Set StorePath)
queryValidDerivers path = storeDirOf >>= ask (QueryValidDerivers path) . getStorePathSet

-- | A derivation's outputs and the paths they have, where known.
queryDerivationOutputMap :: (Monad m) => StorePath -> ClientT m (Map OutputName (Maybe StorePath))
queryDerivationOutputMap path = do
  storeDir <- storeDirOf
  Map.fromList <$> ask (QueryDerivationOutputMap path) (getList ((,) <$> getOutputName <*> getOptionalStorePath storeDir))

-- | Every valid path.
queryAllValidPaths :: (Monad m) => ClientT m (Set StorePath)
queryAllValidPaths = storeDirOf >>= ask QueryAllValidPaths . getStorePathSet

-- | What the store records of a path, or 'Nothing' for a path that is not
-- valid.
queryPathInfo :: (Monad m) => StorePath -> ClientT m (Maybe UnkeyedValidPathInfo)
queryPathInfo path = do
  storeDir <- storeDirOf
  version <- gets sessVersion
  ask (QueryPathInfo path) $ do
    valid <- getBool
    if valid then Just <$> getUnkeyedValidPathInfo storeDir version else pure Nothing

-- | What realising the paths would build, substitute or not find.
queryMissing :: (Monad m) => [DerivedPath] -> ClientT m Missing
queryMissing targets = do
  storeDir <- storeDirOf
  ask (QueryMissing targets) (Missing <$> getStorePathSet storeDir <*> getStorePathSet storeDir <*> getStorePathSet storeDir <*> getWord64 <*> getWord64)

-- | Make a content-addressed path from a dump, given the request and the
-- dump as a chunk source that gives an empty chunk at its end, and answer
-- the path's info.
addToStore :: (Monad m) => AddToStoreRequest -> m ByteString -> ClientT m ValidPathInfo
addToStore upload dump = do
  storeDir <- storeDirOf
  version <- gets sessVersion
  perform (AddToStore upload) (Just dump)
  receive (getValidPathInfo storeDir version)

-- | Import a path from its NAR, given its info, whether a valid path's
-- contents are replaced, whether its signatures are checked, and the NAR
-- as a chunk source that gives an empty chunk where the archive ends.
addToStoreNar :: (Monad m) => ValidPathInfo -> Repair -> CheckSigs -> m ByteString -> ClientT m ()
addToStoreNar info repair checkSigs nar = perform (AddToStoreNar info repair checkSigs) (Just nar)

-- | Realise the paths.
buildPaths :: (Monad m) => [DerivedPath] -> BuildMode -> ClientT m ()
buildPaths targets mode = done (BuildPaths targets mode)

-- | Realise the paths, with each one's result, keyed by the path as it was
-- asked for.
buildPathsWithResults :: (Monad m) => [DerivedPath] -> BuildMode -> ClientT m [KeyedBuildResult]
buildPathsWithResults targets mode = do
  storeDir <- storeDirOf
  version <- gets sessVersion
  ask (BuildPathsWithResults targets mode) (getList (getKeyedBuildResult storeDir version))

-- | Make a path valid, substituting it if need be.
ensurePath :: (Monad m) => StorePath -> ClientT m ()
ensurePath = done . EnsurePath

-- | Keep a path from collection while the session lasts.
addTempRoot :: (Monad m) => StorePath -> ClientT m ()
addTempRoot = done . AddTempRoot

-- | Keep what a link names from collection while the link exists, given
-- the bytes of the link's absolute path.
addIndirectRoot :: (Monad m) => ByteString -> ClientT m ()
addIndirectRoot = done . AddIndirectRoot

-- ---------------------------------------------------------------------------
-- The daemon's messages
-- ---------------------------------------------------------------------------

-- | The result a build's activity reports a line its builder wrote as,
-- and one its post-build hook wrote as: upstream's @resBuildLogLine@ and
-- @resPostBuildLogLine@.
resBuildLogLine, resPostBuildLogLine :: ResultType
resBuildLogLine = ResultType 101
resPostBuildLogLine = ResultType 107

-- | Give what the daemon says as it works to a logger, as upstream's
-- simple logger reports what its client reads, at the verbosity given: a
-- line as a message, its trailing whitespace taken off; an activity's
-- start as its text and an ellipsis, when it has text and its level is
-- within the verbosity; a build's line as the build line it is, and a
-- post-build hook's as a message after @post-build-hook: @.  Nothing else
-- is reported.  The logger takes text, so a message is shown with any
-- byte that is not UTF-8 replaced.
loggerReport :: Verbosity -> Logger -> StderrMessage -> IO ()
loggerReport (Verbosity verbosity) logger = \case
  StderrNext line -> logMessage logger (shown (BS8.dropWhileEnd isTrailingSpace line))
  StderrStartActivity activity
    | let Verbosity level = actLevel activity,
      level <= verbosity,
      not (BS.null (actText activity)) ->
        logMessage logger (shown (actText activity) <> "...")
  StderrResult _ kind (FieldString line : _)
    | kind == resBuildLogLine -> logBuildLine logger line
    | kind == resPostBuildLogLine -> logMessage logger ("post-build-hook: " <> shown line)
  _ -> pure ()
  where
    shown = TE.decodeUtf8Lenient
    isTrailingSpace c = c `elem` [' ', '\n', '\r', '\t']

-- ---------------------------------------------------------------------------
-- Which store
-- ---------------------------------------------------------------------------

-- | What @--store@ names, as nova-nix reads it.
data StoreTarget
  = -- | Nothing, or @auto@: the daemon when it runs, the store otherwise.
    AutoStore
  | -- | @daemon@, the daemon at its socket, or @unix:\/\/PATH@, the daemon
    -- at that socket.
    DaemonSocket !(Maybe FilePath)
  | -- | @local@, the store directly, or a directory, the store there,
    -- directly.
    LocalStore !(Maybe FilePath)
  deriving (Eq, Show)

-- | What @--store@'s value names, as upstream reads @auto@, @daemon@,
-- @unix:\/\/PATH@ and @local@; anything else is a store directory, as
-- nova-nix has always read one.
parseStoreTarget :: Maybe String -> StoreTarget
parseStoreTarget = \case
  Nothing -> AutoStore
  Just "auto" -> AutoStore
  Just "daemon" -> DaemonSocket Nothing
  Just "local" -> LocalStore Nothing
  Just value
    | unixScheme `isPrefixOf` value -> DaemonSocket (Just (drop (length unixScheme) value))
    | otherwise -> LocalStore (Just value)
  where
    unixScheme = "unix://"

-- | Where a command's store operations go.
data StoreChoice connection
  = -- | Through the daemon, over the connection made.
    ThroughDaemon !connection
  | -- | To the store directly, in the directory named or the default.
    DirectStore !(Maybe FilePath)
  deriving (Eq, Show)

-- | Where a command's store operations go, given how to connect to a
-- daemon's socket, the default socket and what @--store@ names: for
-- 'AutoStore', through the daemon whenever its socket accepts a
-- connection, root included, and directly otherwise; for a daemon named,
-- through it, a connection that fails being the command's failure, in
-- upstream's words; for a store named, directly.
chooseStore :: (Monad m) => (FilePath -> m (Either Text connection)) -> FilePath -> StoreTarget -> m (Either Text (StoreChoice connection))
chooseStore connect defaultSocket = \case
  AutoStore -> Right . either (const (DirectStore Nothing)) ThroughDaemon <$> connect defaultSocket
  DaemonSocket named -> do
    let socket = fromMaybe defaultSocket named
    either (Left . refused socket) (Right . ThroughDaemon) <$> connect socket
  LocalStore named -> pure (Right (DirectStore named))
  where
    refused socket reason = "cannot connect to socket at '" <> T.pack socket <> "': " <> reason
