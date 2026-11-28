{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Where a daemon listens for clients, as upstream's daemon does on a
-- POSIX system: a Unix socket at upstream's path, created with the
-- directories above it and made writable by every user, so the
-- directory's own mode decides who reaches it.  Each connection is
-- served on a thread of its own, its peer's identity read from the
-- socket and judged by "Nix.Daemon.Auth" unless the daemon was told to
-- trust or not trust every client.  The listener stops the connections it
-- serves as it stops, and waits for them.
--
-- Windows has no listener yet: a named pipe and identity by account
-- would make the same t'Listener', and until then 'openListener' refuses
-- with the reason 'daemonUnavailable' gives.
module Nix.Daemon.Listen
  ( TrustOverride (..),
    DaemonOptions (..),
    daemonUnavailable,
    daemonSocketPath,
    authSettingsFor,
    Listener,
    openListener,
    closeListener,
    serveListener,
  )
where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Nix.Config (NixConfig (..))
import Nix.Daemon.Auth (AuthSettings (..))
import Nix.Daemon.Connection (ConnectionEnd (..), Transport (..))
import Nix.Daemon.Protocol (Trust (..))
import Nix.Environment (EnvLookup (..), lookupEnvBytes)
import Nix.Logger (Logger (..))
#if !defined(mingw32_HOST_OS)
import Control.Concurrent (ThreadId, forkIOWithUnmask, killThread, myThreadId)
import Control.Concurrent.MVar (MVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar)
import Control.Exception (IOException, catch, displayException, finally, mask_, onException, try)
import Control.Monad (forever, unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BS8
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word32)
import Network.Socket (Family (AF_UNIX), SockAddr (SockAddrUnix), Socket, SocketType (Stream), accept, bind, close, defaultProtocol, getPeerCredential, listen, socket)
import Network.Socket.ByteString (recv)
import qualified Network.Socket.ByteString.Lazy as SocketLazy
import qualified Nix.Daemon.Auth as Auth
import Nix.HostPath (hostPathFromBytes)
import qualified System.Directory.OsPath as OsDir
import System.IO.Error (isDoesNotExistError)
import qualified System.OsPath as OP
import System.Posix.Files.ByteString (removeLink, setFileMode)
import qualified System.Posix.User.ByteString as Posix
#endif
#if defined(mingw32_HOST_OS)
import Data.Void (Void, absurd)
#endif
#if defined(darwin_HOST_OS)
import Foreign.C.Types (CInt (..), CUInt (..))
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import Data.Word (Word8)
#endif

-- ---------------------------------------------------------------------------
-- Options
-- ---------------------------------------------------------------------------

-- | Whether the daemon trusts its clients by who they are or by what it
-- was told.
data TrustOverride
  = -- | Trust every client, without reading who it is.
    ForceTrusted
  | -- | Trust no client, without reading who it is.
    ForceUntrusted
  | -- | Admit and trust each client as @trusted-users@ and
    -- @allowed-users@ say.
    DefaultTrust
  deriving (Eq, Show)

-- | How a daemon admits each client and what it does with one it admits.
data DaemonOptions = DaemonOptions
  { doTrust :: !TrustOverride,
    doAuth :: !AuthSettings,
    -- | Serve one admitted client, trusted or not, over its connection.
    doServe :: !(Trust -> Transport IO -> IO ConnectionEnd)
  }

-- | The socket a daemon listens on unless it is told another, as
-- upstream's names it for the local store: @NIX_DAEMON_SOCKET_PATH@ when
-- it is set and not empty, and otherwise @daemon-socket\/socket@ in the
-- state directory, @NIX_STATE_DIR@ when that is set and not empty and
-- @\/nix\/var\/nix@ otherwise.
daemonSocketPath :: IO ByteString
daemonSocketPath =
  lookupEnvBytes "NIX_DAEMON_SOCKET_PATH" >>= \case
    EnvValue path | path /= "" -> pure path
    _ ->
      lookupEnvBytes "NIX_STATE_DIR" >>= \case
        EnvValue dir | dir /= "" -> pure (dir <> "/" <> socketBelowStateDir)
        _ -> pure (defaultStateDir <> "/" <> socketBelowStateDir)

-- | The admission settings a daemon's configuration gives, the build
-- users' group, where none is set, @nixbld@ for a daemon run as root and
-- none otherwise, as upstream's defaults it.
authSettingsFor :: NixConfig -> IO AuthSettings
authSettingsFor config =
  AuthSettings (ncTrustedUsers config) (ncAllowedUsers config)
    <$> maybe defaultBuildUsersGroup pure (ncBuildUsersGroup config)

-- | The build users' group a daemon has where its configuration sets
-- none.
defaultBuildUsersGroup :: IO ByteString
#if defined(mingw32_HOST_OS)
defaultBuildUsersGroup = pure ""
#else
defaultBuildUsersGroup = (\uid -> if uid == rootUser then rootBuildUsersGroup else "") <$> Posix.getRealUserID
  where
    rootUser = 0
    rootBuildUsersGroup = "nixbld"
#endif

-- | Upstream's state directory, and where the socket is beneath it.
defaultStateDir, socketBelowStateDir :: ByteString
defaultStateDir = "/nix/var/nix"
socketBelowStateDir = "daemon-socket/socket"

-- ---------------------------------------------------------------------------
-- Serving
-- ---------------------------------------------------------------------------

-- | Why this system has no daemon, or 'Nothing' where it has one.
daemonUnavailable :: Maybe Text
#if defined(mingw32_HOST_OS)
daemonUnavailable = Just windowsRefusal

-- | Why Windows has no daemon.
windowsRefusal :: Text
windowsRefusal = "the daemon is not available on Windows yet"
#else
daemonUnavailable = Nothing
#endif

-- | A socket a daemon listens on.
#if defined(mingw32_HOST_OS)
newtype Listener = Listener Void
#else
newtype Listener = Listener Socket
#endif

-- | Make the socket at a path and listen on it, or say why it cannot be
-- made.
openListener :: ByteString -> IO (Either Text Listener)
#if defined(mingw32_HOST_OS)
openListener _ = pure (Left windowsRefusal)
#else
openListener path = fmap Listener <$> listenAt path
#endif

-- | Stop listening.  The socket's file stays, as upstream's daemon leaves
-- it.
closeListener :: Listener -> IO ()
#if defined(mingw32_HOST_OS)
closeListener (Listener none) = absurd none
#else
closeListener (Listener sock) = close sock
#endif

-- | Serve each client that connects, until the thread running this is
-- stopped, which stops every connection it is serving and waits for each
-- to end.  The line each connection is accepted with, and the reason one
-- is refused or fails its handshake, go to the logger.
serveListener :: Logger -> DaemonOptions -> Listener -> IO a
#if defined(mingw32_HOST_OS)
serveListener _ _ (Listener none) = absurd none
#else
serveListener logger options (Listener sock) = withConnections (\spawn -> forever (acceptOne spawn))
  where
    -- Masked from the accept to the thread's start, so a connection the
    -- loop accepts as it is stopped is still closed; the wait in accept
    -- stays interruptible.
    acceptOne spawn =
      mask_ (accept sock >>= \(conn, _) -> spawn (serveClient logger options conn `finally` close conn) `onException` close conn)
        `catch` \(err :: IOException) -> logMessage logger ("error: while processing connection: accepting connection: " <> T.pack (displayException err))
#endif

#if !defined(mingw32_HOST_OS)
-- | The size of a socket address's path on this system, its terminating
-- NUL included: 104 bytes on macOS, 108 on Linux.
sunPathSize :: Int
#if defined(darwin_HOST_OS)
sunPathSize = 104
#else
sunPathSize = 108
#endif

-- | How many connections may wait to be accepted, as upstream's daemon
-- lets them.
listenBacklog :: Int
listenBacklog = 100

-- | The mode the socket gets, every user's read and write, so the
-- directory around it decides who can reach it.
socketMode :: Word32
socketMode = 0o666

-- | Make the socket as upstream's daemon makes it: the directories above
-- it, any file in its place removed, bound, given 'socketMode', and
-- listening.  A path longer than a socket address holds is refused,
-- where upstream binds it from its directory.
listenAt :: ByteString -> IO (Either Text Socket)
listenAt path
  | BS.length path >= sunPathSize = pure (Left ("cannot bind to socket at '" <> shown <> "': path is too long"))
  | otherwise = case hostPathFromBytes path of
      Nothing -> pure (Left ("cannot bind to socket at '" <> shown <> "': not a path"))
      Just osPath ->
        stepping ("creating directory '" <> shown <> "'") (OsDir.createDirectoryIfMissing True (OP.takeDirectory osPath)) `andThen` \() ->
          stepping ("removing '" <> shown <> "'") (removeLink path `catch` \(err :: IOException) -> unless (isDoesNotExistError err) (ioError err)) `andThen` \() ->
            stepping "creating a Unix socket" (socket AF_UNIX Stream defaultProtocol) `andThen` \sock ->
              let bound = do
                    bind sock (SockAddrUnix (BS8.unpack path))
                    setFileMode path (fromIntegral socketMode)
                    listen sock listenBacklog
               in stepping ("cannot bind to socket at '" <> shown <> "'") (bound `onException` close sock) `andThen` \() ->
                    pure (Right sock)
  where
    shown = TE.decodeUtf8Lenient path
    andThen step next = step >>= either (pure . Left) next
    stepping what action = either (\(err :: IOException) -> Left (what <> ": " <> T.pack (displayException err))) Right <$> try action

-- | Run the accept loop with what it spawns each connection's thread by,
-- and on the way out stop every connection still being served and wait
-- for each to finish.
withConnections :: ((IO () -> IO ()) -> IO a) -> IO a
withConnections body = do
  live <- newMVar Map.empty
  body (spawn live) `finally` stopAll live
  where
    spawn :: MVar (Map ThreadId (MVar ())) -> IO () -> IO ()
    spawn live action = mask_ $ modifyMVar_ live $ \running -> do
      done <- newEmptyMVar
      thread <- forkIOWithUnmask (\unmask -> unmask action `finally` finished live done)
      pure (Map.insert thread done running)
    finished live done = do
      me <- myThreadId
      modifyMVar_ live (pure . Map.delete me)
      putMVar done ()
    stopAll live = do
      running <- readMVar live
      mapM_ killThread (Map.keys running)
      mapM_ takeMVar (Map.elems running)

-- | Serve one client: admit it, say so, and serve it, or say why not.
serveClient :: Logger -> DaemonOptions -> Socket -> IO ()
serveClient logger options conn = do
  (peer, admission) <- case doTrust options of
    ForceTrusted -> pure (Auth.unknownPeer, Auth.Admitted Trusted Nothing)
    ForceUntrusted -> pure (Auth.unknownPeer, Auth.Admitted NotTrusted Nothing)
    DefaultTrust -> do
      peer <- peerOf conn
      (,) peer <$> Auth.admitPeer systemUsers (doAuth options) peer
  case admission of
    Auth.Refused reason -> logMessage logger ("error: while processing connection: " <> reason)
    Auth.Admitted trust _ -> do
      logMessage logger (Auth.acceptedMessage peer admission)
      ended <- try (doServe options trust (socketTransport conn))
      case ended of
        Left (err :: IOException) -> logMessage logger ("error: " <> T.pack (displayException err))
        Right (HandshakeFailed reason) -> logMessage logger ("error: " <> reason)
        Right _ -> pure ()

-- | What the socket says of its peer.
peerOf :: Socket -> IO Auth.Peer
peerOf conn = do
  (pid, uid, gid) <- getPeerCredential conn
  pure (Auth.Peer (fromIntegral <$> pid) (fromIntegral <$> uid) (fromIntegral <$> gid))

-- | How much a read from a client asks for.
receiveChunkSize :: Int
receiveChunkSize = 65536

-- | A connection's transport over its socket.  A read of nothing is the
-- end of the client's input, which every read after it gives too.
socketTransport :: Socket -> Transport IO
socketTransport conn =
  Transport
    { transportReceive = (\chunk -> if BS.null chunk then Nothing else Just chunk) <$> recv conn receiveChunkSize,
      transportSend = SocketLazy.sendAll conn . BB.toLazyByteString
    }

-- | The system's users and groups, an entry that is not there and one
-- that cannot be read alike missing.
systemUsers :: Auth.UserDatabase IO
systemUsers =
  Auth.UserDatabase
    { Auth.udUserName = \uid -> entry (Posix.userName <$> Posix.getUserEntryForID (fromIntegral uid)),
      Auth.udGroupName = \gid -> entry (Posix.groupName <$> Posix.getGroupEntryForID (fromIntegral gid)),
      Auth.udGroup = \name -> entry ((\group -> Auth.GroupEntry (fromIntegral (Posix.groupID group)) (Posix.groupMembers group)) <$> Posix.getGroupEntryForName name),
      Auth.udMembership = membership
    }
  where
    entry query = either (\(_ :: IOException) -> Nothing) Just <$> try query

-- | Whether the directory counts a user a member of a group, as macOS
-- answers apart from a group's member list; elsewhere there is no such
-- answer.
membership :: Word32 -> Word32 -> IO (Maybe Bool)
#if defined(darwin_HOST_OS)
membership uid gid =
  allocaBytes uuidSize $ \userUuid ->
    allocaBytes uuidSize $ \groupUuid ->
      alloca $ \isMember -> do
        userMapped <- c_mbr_uid_to_uuid (fromIntegral uid) userUuid
        groupMapped <- c_mbr_gid_to_uuid (fromIntegral gid) groupUuid
        if userMapped /= 0 || groupMapped /= 0
          then pure Nothing
          else do
            checked <- c_mbr_check_membership userUuid groupUuid isMember
            if checked /= 0 then pure Nothing else Just . (/= 0) <$> peek isMember

-- | The size of a @uuid_t@.
uuidSize :: Int
uuidSize = 16

foreign import ccall safe "membership.h mbr_uid_to_uuid" c_mbr_uid_to_uuid :: CUInt -> Ptr Word8 -> IO CInt

foreign import ccall safe "membership.h mbr_gid_to_uuid" c_mbr_gid_to_uuid :: CUInt -> Ptr Word8 -> IO CInt

foreign import ccall safe "membership.h mbr_check_membership" c_mbr_check_membership :: Ptr Word8 -> Ptr Word8 -> Ptr CInt -> IO CInt
#else
membership _ _ = pure Nothing
#endif
#endif
