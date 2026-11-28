{-# LANGUAGE LambdaCase #-}

-- | A daemon's side of one connection, as upstream's daemon serves one:
-- the handshake, then each operation the client asks, run over the store
-- the connection is given ("Nix.Daemon.Ops"), until the client's input
-- ends or an error closes the connection.
--
-- An operation's request is read, and judged by the client's trust,
-- before its work begins.  A failure there is sent to the client and
-- closes the connection, the error going out twice, as upstream's
-- connection loop sends it once for the operation and again as it gives
-- up.  Once the work has begun, what the store reports goes to the client
-- as it happens (t'LogEvent'), and a failure is sent once and the
-- connection goes on with the next operation.  The end of the work is
-- @STDERR_LAST@, then the reply.
--
-- An upload follows its request as a framed stream.  The store is fed its
-- bytes, an archive's only up to the archive's end ('NarBoundary'), and
-- whatever of the stream it does not take is read past before the
-- operation's result is sent, as upstream drains a framed upload, so the
-- next operation starts at its first byte.  A refused frame leaves the
-- stream where upstream's does, and the draining goes on from there.
--
-- The core is pure: everything a connection does to the world goes
-- through its t'Transport' and its t'DaemonStore'.
module Nix.Daemon.Connection
  ( Transport (..),
    ServerConfig (..),
    ConnectionEnd (..),
    serveConnection,
  )
where

import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.State.Strict (StateT, evalStateT, get, gets, modify', put, runStateT)
import Control.Monad.Trans (lift)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BB
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import Nix.Config (NixConfig)
import Nix.Daemon.Ops
  ( AddToStoreRequest (..),
    Authorization (..),
    CheckSigs,
    ConnectionSettings (..),
    DaemonStore (..),
    DumpUpload (..),
    GCResult (..),
    Intake (..),
    LogEvent (..),
    Missing (..),
    NarBoundary (..),
    Repair,
    Request (..),
    Work (..),
    applyClientSettings,
    authorize,
    daemonError,
    getRequest,
    getUploadPathInfo,
    narSizeBoundary,
    rootCensoring,
  )
import Nix.Daemon.Protocol
  ( Activity (..),
    DaemonError,
    KeyedBuildResult,
    OutputName,
    StderrMessage (..),
    Trust,
    UnkeyedValidPathInfo (..),
    ValidPathInfo (..),
    Verbosity (..),
    Version (..),
    VersionNumber (..),
    getClientMagic,
    getClientObsoleteFields,
    getFeatures,
    getVersionNumber,
    minimumVersionNumber,
    parseContentAddressMethod,
    putFeatures,
    putHandshakeInfo,
    putKeyedBuildResult,
    putOptionalStorePath,
    putOutputName,
    putServerMagic,
    putStderrMessage,
    putStorePath,
    putStorePathSet,
    putUnkeyedValidPathInfo,
    putValidPathInfo,
    putVersionNumber,
  )
import Nix.Daemon.Wire (DecodeError (..), Decoder, Decoding (..), feedInput, getFrame, getWord32, getWord64, putBool, putBytes, putList, putNum, startDecoding)
import Nix.Store.Path (StoreDir, StorePath)

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | How a connection reaches its client.
data Transport m = Transport
  { -- | The next bytes the client sent, never empty, or 'Nothing' once its
    -- input has ended, and at every call after.
    transportReceive :: !(m (Maybe ByteString)),
    -- | Send bytes to the client, all of them, before returning.
    transportSend :: !(Builder -> m ())
  }

-- | What a daemon tells and gives every connection.
data ServerConfig = ServerConfig
  { -- | The store directory paths are named under on the wire.
    scStoreDir :: !StoreDir,
    -- | The protocol version this daemon offers: its number, and the
    -- features it lists for a client to share.
    scVersion :: !Version,
    -- | The Nix version this daemon names itself by, from 1.33.
    scNixVersion :: !ByteString,
    -- | How much a client is told until it sets its own verbosity.
    scVerbosity :: !Verbosity,
    -- | The daemon's settings, which each connection starts from.
    scConfig :: !NixConfig,
    -- | The substituters a client the daemon does not trust may use,
    -- besides those the daemon uses.
    scTrustedSubstituters :: ![Text]
  }

-- | Why a connection ended.
data ConnectionEnd
  = -- | The client's input ended between operations.
    ClientClosed
  | -- | The handshake failed, for this reason, and the client was sent
    -- nothing about it.
    HandshakeFailed !Text
  | -- | An error closed the connection, and the client was sent it.
    ClosedOnError !DaemonError
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- The connection
-- ---------------------------------------------------------------------------

-- | What a connection holds while it runs: the input that has arrived and
-- not been read, and its settings.
data Session = Session
  { sessInput :: !ByteString,
    sessSettings :: !ConnectionSettings
  }

-- | A connection's steps, over the monad its transport and store run in.
type Conn m = StateT Session m

-- | What every operation of an agreed connection reads.
data Env m = Env
  { envConfig :: !ServerConfig,
    envTrust :: !Trust,
    envStore :: !(DaemonStore m),
    envTransport :: !(Transport m),
    envVersion :: !Version
  }

-- | How an operation left its connection.
data OpEnd
  = -- | It ended, served or failed, and the next operation may follow.
    OpServed
  | -- | It failed where the connection cannot go on, and the client was
    -- sent the error.
    OpClosed !DaemonError

-- | Serve a client, the daemon trusting it or not, over a store and a
-- transport of the connection's own, until the connection ends.
serveConnection :: (Monad m) => ServerConfig -> Trust -> DaemonStore m -> Transport m -> m ConnectionEnd
serveConnection config trust store transport = evalStateT connection (Session BS.empty initialSettings)
  where
    initialSettings = ConnectionSettings (scVerbosity config) (scConfig config) Nothing
    connection =
      handshake config trust transport >>= \case
        Left reason -> pure (HandshakeFailed reason)
        Right version -> serveOperations (Env config trust store transport version)

-- | The version a client's features are exchanged from.
featureExchangeSince :: VersionNumber
featureExchangeSince = VersionNumber 1 38

-- | Agree a version with a client, as upstream's daemon does: read its
-- opening word, offer this daemon's version number, read the client's,
-- and from 1.38 read its features and offer this daemon's.  The version
-- is the lower number and the features both list.  A client below the
-- oldest version served is refused, and so is one whose handshake cannot
-- be read; neither is sent a reason.  Then the client's obsolete fields
-- are read, the daemon's Nix version and its trust in the client are
-- sent where the version carries them, and its startup work is reported
-- done.
handshake :: (Monad m) => ServerConfig -> Trust -> Transport m -> Conn m (Either Text Version)
handshake config trust transport = runExceptT $ do
  receive getClientMagic
  lift (send transport (putServerMagic <> putVersionNumber offered))
  clientNumber <- receive getVersionNumber
  let agreed = min clientNumber offered
  clientFeatures <-
    if agreed >= featureExchangeSince
      then receive getFeatures <* lift (send transport (putFeatures offeredFeatures))
      else pure Set.empty
  when (agreed < minimumVersionNumber) (ExceptT (pure (Left "the Nix client version is too old")))
  let version = Version agreed (Set.intersection clientFeatures offeredFeatures)
  receive (getClientObsoleteFields version)
  lift (send transport (putHandshakeInfo version (scNixVersion config) (Just trust) <> putStderrMessage version StderrLast))
  pure version
  where
    offered = versionNumber (scVersion config)
    offeredFeatures = versionFeatures (scVersion config)
    receive decoder = ExceptT (either (Left . decodeErrorText) Right <$> receiveValue transport decoder)

-- | Serve operations until the client's input ends or an error closes the
-- connection.  The input ending before an operation's code has been read
-- whole is the client closing it; a code too large to read is an error
-- sent once, as upstream sends it.
serveOperations :: (Monad m) => Env m -> Conn m ConnectionEnd
serveOperations env =
  receiveValue (envTransport env) getWord32 >>= \case
    Left InputEnded -> pure ClientClosed
    Left (Malformed message) -> ClosedOnError (daemonError message) <$ sendError env (daemonError message)
    Right code -> case getRequest (scStoreDir (envConfig env)) (envVersion env) code of
      Nothing -> refusedBeforeWork (daemonError ("invalid operation " <> T.pack (show code)))
      Just decoder ->
        receiveValue (envTransport env) decoder >>= \case
          Left err -> refusedBeforeWork (daemonError (decodeErrorText err))
          Right request -> case authorize (envTrust env) request of
            RefusedBeforeWork message -> refusedBeforeWork (daemonError message)
            RefusedInWork message -> sendError env (daemonError message) >> serveOperations env
            Permitted permitted ->
              perform env permitted >>= \case
                OpServed -> serveOperations env
                OpClosed err -> pure (ClosedOnError err)
  where
    refusedBeforeWork err = ClosedOnError err <$ closingError env err

-- | Perform a permitted request, its work begun.
perform :: (Monad m) => Env m -> Request -> Conn m OpEnd
perform env = \case
  SetOptions client -> do
    settings <- gets sessSettings
    let (updated, warnings) = applyClientSettings (envTrust env) (scTrustedSubstituters config) client settings
    modify' (\session -> session {sessSettings = updated})
    traverse_ (warn (connVerbosity updated)) warnings
    finish mempty
  IsValidPath path -> answered (\w -> dsIsValidPath store w path) putBool
  QueryValidPaths paths substitute -> answered (\w -> dsQueryValidPaths store w substitute paths) pathSet
  QuerySubstitutablePaths paths -> answered (\w -> dsQuerySubstitutablePaths store w paths) pathSet
  QueryReferrers path -> answered (\w -> dsQueryReferrers store w path) pathSet
  QueryValidDerivers path -> answered (\w -> dsQueryValidDerivers store w path) pathSet
  QueryDerivationOutputMap path -> answered (\w -> dsQueryDerivationOutputMap store w path) (putOutputMap storeDir)
  QueryAllValidPaths -> answered (dsQueryAllValidPaths store) pathSet
  QueryPathInfo path ->
    answered (\w -> dsQueryPathInfo store w path) (maybe (putNum invalidFlag) (\info -> putNum validFlag <> putUnkeyedValidPathInfo storeDir version info))
  QueryMissing targets -> answered (\w -> dsQueryMissing store w targets) (putMissing storeDir)
  AddToStore upload ->
    -- The method is parsed before the dump is read, so a method refused
    -- leaves the dump unread, as upstream leaves it.
    case parseContentAddressMethod (atsMethod upload) of
      Left refusal -> failed (daemonError refusal)
      Right (method, algo) -> do
        w <- work env
        intake <- lift (dsAddToStore store w (DumpUpload (atsName upload) method algo (atsReferences upload) (atsRepair upload)))
        result <- framed transport (feedToEnd transport intake)
        either failed (finish . putValidPathInfo storeDir version) result
  AddToStoreNar info repair checkSigs -> do
    w <- work env
    intake <- lift (dsAddToStoreNar store w info repair checkSigs)
    result <- framed transport (fst <$> feedArchive transport (narSizeBoundary (upiNarSize (vpiInfo info))) intake)
    either failed (const (finish mempty)) result
  AddMultipleToStore repair checkSigs ->
    framed transport (importPaths env repair checkSigs) >>= either failed (const (finish mempty))
  BuildPaths targets mode -> answered (\w -> dsBuildPaths store w targets mode) (const done)
  BuildPathsWithResults targets mode -> do
    w <- work env
    lift (dsBuildPathsWithResults store w targets mode) >>= \case
      Left err -> failed err
      Right results -> case putKeyedBuildResults storeDir version results of
        (reply, Nothing) -> finish reply
        -- Upstream writes each result as it goes, so a client is sent
        -- the ones before the result it cannot write, then the error.
        (written, Just refusal) -> do
          send transport (putStderrMessage version StderrLast <> written)
          OpClosed (daemonError refusal) <$ closingError env (daemonError refusal)
  EnsurePath path -> answered (\w -> dsEnsurePath store w path) (const done)
  AddTempRoot path -> answered (\w -> dsAddTempRoot store w path) (const done)
  AddIndirectRoot link -> answered (\w -> dsAddIndirectRoot store w link) (const done)
  FindRoots -> answered (\w -> dsFindRoots store w (rootCensoring (envTrust env))) (putRoots storeDir)
  -- 'authorize' has refused a collection that ignores liveness.
  CollectGarbage request _ -> answered (\w -> dsCollectGarbage store w request) putGCResult
  OptimiseStore -> answered (dsOptimiseStore store) (const done)
  VerifyStore contents repair -> answered (\w -> dsVerifyStore store w contents repair) putBool
  AddSignatures path sigs -> answered (\w -> dsAddSignatures store w path sigs) (const done)
  -- The NAR follows the end of the work, so a failure writing it can no
  -- longer be told apart from the NAR and closes the connection.
  NarFromPath path -> do
    send transport (putStderrMessage version StderrLast)
    lift (dsNarFromPath store path (transportSend transport . BB.byteString)) >>= \case
      Left err -> OpClosed err <$ closingError env err
      Right () -> pure OpServed
  where
    config = envConfig env
    storeDir = scStoreDir config
    version = envVersion env
    store = envStore env
    transport = envTransport env
    pathSet = putStorePathSet storeDir
    done = putNum doneWord
    finish reply = OpServed <$ send transport (putStderrMessage version StderrLast <> reply)
    failed err = OpServed <$ sendError env err
    answered call reply = do
      w <- work env
      lift (call w) >>= either failed (finish . reply)
    warn verbosity message =
      when (heard warningVerbosity verbosity) (send transport (putStderrMessage version (StderrNext message)))

-- | The word an operation with nothing else to say replies with.
doneWord :: Word64
doneWord = 1

-- | The flags a path's info is sent after, or in place of.
validFlag, invalidFlag :: Word64
validFlag = 1
invalidFlag = 0

-- | The word that follows a collection's result, which no client reads.
obsoleteCollectionWord :: Word64
obsoleteCollectionWord = 0

-- | The level of a warning (@lvlWarn@).
warningVerbosity :: Verbosity
warningVerbosity = Verbosity 1

-- | Whether a client at a verbosity hears a message at a level.
heard :: Verbosity -> Verbosity -> Bool
heard (Verbosity level) (Verbosity verbosity) = level <= verbosity

-- | The store's view of an operation's work: its report goes to the
-- client, and it runs under the connection's settings.
work :: (Monad m) => Env m -> Conn m (Work m)
work env = do
  settings <- gets sessSettings
  pure (Work (report (envTransport env) (envVersion env) (connVerbosity settings)) settings)

-- | Send what the store reports to the client, as upstream's daemon
-- forwards its logger's calls.
report :: (Monad m) => Transport m -> Version -> Verbosity -> LogEvent -> m ()
report transport version verbosity event =
  maybe (pure ()) (transportSend transport . putStderrMessage version) (forwarded version verbosity event)

-- | Activities go to the client as their own messages from this version.
activitiesSince :: VersionNumber
activitiesSince = VersionNumber 1 20

-- | The message a report goes to the client as, if any.  A line goes if
-- the client's verbosity reaches its level, with a line feed after it.
-- Before 1.20 an activity's start goes as its text, if it has any, with
-- an ellipsis, a line at the activity's level, and the rest of what an
-- activity reports does not go at all.
forwarded :: Version -> Verbosity -> LogEvent -> Maybe StderrMessage
forwarded version verbosity = \case
  LogLine level line -> logged level line
  ActivityStarted activity
    | activities -> Just (StderrStartActivity activity)
    | BS.null (actText activity) -> Nothing
    | otherwise -> logged (actLevel activity) (actText activity <> "...")
  ActivityStopped act -> onlyWithActivities (StderrStopActivity act)
  ActivityResult act kind fields -> onlyWithActivities (StderrResult act kind fields)
  where
    activities = versionNumber version >= activitiesSince
    onlyWithActivities message = if activities then Just message else Nothing
    logged level line
      | heard level verbosity = Just (StderrNext (line <> "\n"))
      | otherwise = Nothing

-- ---------------------------------------------------------------------------
-- Uploads
-- ---------------------------------------------------------------------------

-- | Where a framed upload's reading stands: the bytes of a chunk received
-- and not yet taken, and whether the chunk that ends the stream has been
-- read.
data Frames = Frames
  { framesHeld :: !ByteString,
    framesEnded :: !Bool
  }

-- | An operation's steps while it reads a framed upload.
type Upload m = StateT Frames (Conn m)

-- | Read an upload, and then read past what of it was left, as upstream's
-- framed source does as it goes out of scope, a failure to read past it
-- going unreported as it goes there.
framed :: (Monad m) => Transport m -> Upload m a -> Conn m a
framed transport upload = do
  (result, frames) <- runStateT upload (Frames BS.empty False)
  unless (framesEnded frames) drain
  pure result
  where
    drain =
      receiveValue transport getFrame >>= \case
        Right (Just _) -> drain
        _ -> pure ()

-- | The next bytes of an upload, or 'Nothing' at its end.
nextChunk :: (Monad m) => Transport m -> Upload m (Either DaemonError (Maybe ByteString))
nextChunk transport =
  get >>= \case
    Frames held ended
      | not (BS.null held) -> Right (Just held) <$ put (Frames BS.empty ended)
      | ended -> pure (Right Nothing)
      | otherwise ->
          lift (receiveValue transport getFrame) >>= \case
            Left err -> pure (Left (daemonError (decodeErrorText err)))
            Right Nothing -> Right Nothing <$ put (Frames BS.empty True)
            Right (Just chunk) -> pure (Right (Just chunk))

-- | Give back bytes an upload's reader took and did not use.
unread :: (Monad m) => ByteString -> Upload m ()
unread bytes = modify' (\frames -> frames {framesHeld = bytes <> framesHeld frames})

-- | Upstream's refusal of a read past the end of an upload.
uploadEnded :: DaemonError
uploadEnded = daemonError "reached end of FramedSource"

-- | A value read from an upload.
uploadValue :: (Monad m) => Transport m -> Decoder a -> Upload m (Either DaemonError a)
uploadValue transport decoder = settle (startDecoding decoder)
  where
    settle = \case
      Decoded value rest -> Right value <$ unread rest
      DecodeFailed InputEnded _ -> pure (Left uploadEnded)
      DecodeFailed (Malformed message) rest -> Left (daemonError message) <$ unread rest
      NeedInput more -> nextChunk transport >>= either (pure . Left) (settle . more)

-- | Feed the store the rest of an upload.
feedToEnd :: (Monad m) => Transport m -> Intake m a -> Upload m (Either DaemonError a)
feedToEnd transport = \case
  Taken result -> pure result
  Taking onChunk onEnd onBroken ->
    nextChunk transport >>= \case
      Left err -> Left err <$ lift (lift onBroken)
      Right Nothing -> lift (lift onEnd)
      Right (Just chunk) -> lift (lift (onChunk chunk)) >>= feedToEnd transport

-- | How much of an archive the store left.
data ArchiveLeft
  = -- | None: the upload stands at the archive's end.
    ArchiveRead
  | -- | Some, which this boundary still finds the end of.
    ArchiveUnread !NarBoundary

-- | Feed the store an archive from an upload, up to the archive's end,
-- and say where that leaves the upload.  The store hears that its bytes
-- have ended where the archive ends, or where the upload does before it,
-- and that they broke off where the upload cannot be read.
feedArchive :: (Monad m) => Transport m -> NarBoundary -> Intake m a -> Upload m (Either DaemonError a, ArchiveLeft)
feedArchive transport boundary intake = case boundary of
  BoundaryReached rest -> do
    unread rest
    result <- lift (lift (intakeEnd intake))
    pure (result, ArchiveRead)
  BoundaryAhead step -> case intake of
    Taken result -> pure (result, ArchiveUnread boundary)
    Taking onChunk onEnd onBroken ->
      nextChunk transport >>= \case
        Left err -> (Left err, ArchiveUnread boundary) <$ lift (lift onBroken)
        Right Nothing -> (\result -> (result, ArchiveUnread boundary)) <$> lift (lift onEnd)
        Right (Just chunk) -> case step chunk of
          BoundaryReached rest -> do
            let archived = BS.take (BS.length chunk - BS.length rest) chunk
            fed <- if BS.null archived then pure intake else lift (lift (onChunk archived))
            feedArchive transport (BoundaryReached rest) fed
          ahead -> lift (lift (onChunk chunk)) >>= feedArchive transport ahead

-- | What the store makes of its bytes ending, if it still takes them.
intakeEnd :: (Applicative m) => Intake m a -> m (Either DaemonError a)
intakeEnd = \case
  Taken result -> pure result
  Taking _ onEnd _ -> onEnd

-- | Read past the rest of an archive the store did not take, which must
-- be there, as upstream reads its declared size whatever its store read.
skipArchive :: (Monad m) => Transport m -> NarBoundary -> Upload m (Either DaemonError ())
skipArchive transport = \case
  BoundaryReached rest -> Right () <$ unread rest
  BoundaryAhead step ->
    nextChunk transport >>= \case
      Left err -> pure (Left err)
      Right Nothing -> pure (Left uploadEnded)
      Right (Just chunk) -> skipArchive transport (step chunk)

-- | Import the paths an upload holds: their count, then each path's info
-- and its NAR.  No path imported this way is ultimate, whoever sends it.
importPaths :: (Monad m) => Env m -> Repair -> CheckSigs -> Upload m (Either DaemonError ())
importPaths env repair checkSigs = runExceptT $ do
  count <- ExceptT (uploadValue transport getWord64)
  traverse_ (const importOne) [1 .. count]
  where
    transport = envTransport env
    importOne = do
      uploaded <- ExceptT (uploadValue transport (getUploadPathInfo (scStoreDir (envConfig env))))
      let info = (vpiInfo uploaded) {upiUltimate = False}
      w <- lift (lift (work env))
      intake <- lift (lift (lift (dsAddToStoreNar (envStore env) w uploaded {vpiInfo = info} repair checkSigs)))
      (result, left) <- lift (feedArchive transport (narSizeBoundary (upiNarSize info)) intake)
      ExceptT (pure result)
      case left of
        ArchiveRead -> pure ()
        ArchiveUnread boundary -> ExceptT (skipArchive transport boundary)

-- ---------------------------------------------------------------------------
-- Replies
-- ---------------------------------------------------------------------------

-- | A derivation's outputs and their paths.
putOutputMap :: StoreDir -> Map OutputName (Maybe StorePath) -> Builder
putOutputMap storeDir outputs = putList (\(name, path) -> putOutputName name <> putOptionalStorePath storeDir path) (Map.toAscList outputs)

-- | What realising some paths would take.
putMissing :: StoreDir -> Missing -> Builder
putMissing storeDir missing =
  putStorePathSet storeDir (missingWillBuild missing)
    <> putStorePathSet storeDir (missingWillSubstitute missing)
    <> putStorePathSet storeDir (missingUnknown missing)
    <> putNum (missingDownloadSize missing)
    <> putNum (missingNarSize missing)

-- | The roots: how many there are, then each one's link and the path it
-- keeps, in the order of the paths and then of the links.
putRoots :: StoreDir -> Map StorePath (Set.Set ByteString) -> Builder
putRoots storeDir roots =
  putNum (sum (map Set.size (Map.elems roots)))
    <> foldMap (\(target, links) -> foldMap (\link -> putBytes link <> putStorePath storeDir target) (Set.toAscList links)) (Map.toAscList roots)

-- | What a collection did.
putGCResult :: GCResult -> Builder
putGCResult result =
  putList putBytes (Set.toAscList (gcResultPaths result))
    <> putNum (gcResultBytesFreed result)
    <> putNum obsoleteCollectionWord

-- | Build results with the paths they answer, as far as they can be
-- written, and the refusal of the first that cannot.
putKeyedBuildResults :: StoreDir -> Version -> [KeyedBuildResult] -> (Builder, Maybe Text)
putKeyedBuildResults storeDir version results = go (putNum (length results)) results
  where
    go written = \case
      [] -> (written, Nothing)
      result : rest -> either (\refusal -> (written, Just refusal)) (\bytes -> go (written <> bytes) rest) (putKeyedBuildResult storeDir version result)

-- ---------------------------------------------------------------------------
-- Input and output
-- ---------------------------------------------------------------------------

-- | Read a value from the client, the input after it kept for what is
-- read next.  A refused value keeps the input after the bytes it took.
receiveValue :: (Monad m) => Transport m -> Decoder a -> Conn m (Either DecodeError a)
receiveValue transport decoder = do
  held <- gets sessInput
  keepInput BS.empty
  settle (if BS.null held then startDecoding decoder else feedInput (Just held) (startDecoding decoder))
  where
    settle = \case
      Decoded value rest -> Right value <$ keepInput rest
      DecodeFailed err rest -> Left err <$ keepInput rest
      NeedInput more -> lift (transportReceive transport) >>= settle . more

-- | Keep input for what is read next.
keepInput :: (Monad m) => ByteString -> Conn m ()
keepInput rest = modify' (\session -> session {sessInput = rest})

-- | Upstream's message for a value that could not be read: its refusal,
-- or the end of the input before the value's end.
decodeErrorText :: DecodeError -> Text
decodeErrorText = \case
  InputEnded -> "unexpected end-of-file"
  Malformed message -> message

-- | Send bytes to the client.
send :: (Monad m) => Transport m -> Builder -> Conn m ()
send transport = lift . transportSend transport

-- | Send the client an error.
sendError :: (Monad m) => Env m -> DaemonError -> Conn m ()
sendError env err = send (envTransport env) (putStderrMessage (envVersion env) (StderrError err))

-- | Send the client an error that closes the connection: once for the
-- operation and once more as the connection ends, as upstream sends it.
closingError :: (Monad m) => Env m -> DaemonError -> Conn m ()
closingError env err = sendError env err >> sendError env err
