{-# LANGUAGE CPP #-}

-- | Resources of the invoking user's that the macOS sandbox tests reach
-- for from a build: a session of its own, which takes a process out of
-- the builder's group; a pseudo-terminal, as a terminal the user types
-- in; a Unix socket listening where the user's live ones are;
-- @SIGHUP@ ignored, as a program started under @nohup@ has it; and
-- @SIGCONT@, which continues a stopped process.  And what the tests of a
-- builder's start read: a process's session, and a soft limit on core
-- dumps a builder would inherit.  Windows has no sessions,
-- pseudo-terminals, signals or such limits, so those answer that there is
-- none there, or run the action as it is.
module UserResources
  ( newSession,
    processSession,
    withCoreDumpsAllowed,
    withPseudoTerminal,
    withUnixListener,
    withHangupIgnored,
    continueProcess,
  )
where

import Control.Concurrent.Async (withAsync)
import Control.Exception (bracket, bracketOnError)
import Control.Monad (forever)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import qualified Network.Socket as Socket
import qualified System.Process as Proc

#if !defined(mingw32_HOST_OS)
import qualified Control.Exception as Exception
import Data.Foldable (traverse_)
import System.Posix.IO (closeFd)
import System.Posix.Process (createSession, getProcessID)
import System.Posix.Resource (Resource (ResourceCoreFileSize), ResourceLimit (..), ResourceLimits (..), getResourceLimit, setResourceLimit)
import System.Posix.Signals (Handler (Ignore), installHandler, sigCONT, sigHUP, signalProcess)
import System.Posix.Terminal (getSlaveTerminalName, openPseudoTerminal)
import System.Posix.Types (CPid (..))
#endif

-- | Start a session of the calling process's own, which takes it out of
-- its process group, answering whether it did.
newSession :: IO Bool
#if defined(mingw32_HOST_OS)
newSession = pure False
#else
newSession = True <$ createSession
#endif

-- | This process's id and its session's, where there are sessions.
processSession :: IO (Maybe (Integer, Integer))
#if defined(mingw32_HOST_OS)
processSession = pure Nothing
#else
processSession = do
  pid <- getProcessID
  session <- c_getsid 0
  pure (Just (fromIntegral pid, fromIntegral session))

foreign import ccall unsafe "unistd.h getsid"
  c_getsid :: CPid -> IO CPid
#endif

-- | Run an action with this process's soft limit on core dumps raised to
-- its hard limit, which a process it starts would inherit, and put the
-- limit back afterwards, answering what the action did; or run nothing
-- and answer 'Nothing' where the hard limit allows no core, or is not
-- known.
withCoreDumpsAllowed :: IO a -> IO (Maybe a)
#if defined(mingw32_HOST_OS)
withCoreDumpsAllowed _ = pure Nothing
#else
withCoreDumpsAllowed action = do
  limits <- getResourceLimit ResourceCoreFileSize
  case hardLimit limits of
    ResourceLimit 0 -> pure Nothing
    ResourceLimitUnknown -> pure Nothing
    hard -> Just <$> Exception.bracket_ (setResourceLimit ResourceCoreFileSize limits {softLimit = hard}) (setResourceLimit ResourceCoreFileSize limits) action
#endif

-- | Run an action given the name of a pseudo-terminal's device, which is
-- open while it runs, or answer 'Nothing' where there is none.
withPseudoTerminal :: (FilePath -> IO a) -> IO (Maybe a)
#if defined(mingw32_HOST_OS)
withPseudoTerminal _ = pure Nothing
#else
withPseudoTerminal action =
  bracket openPseudoTerminal (\(controller, terminal) -> closeFd controller *> closeFd terminal) $ \(controller, _) ->
    getSlaveTerminalName controller >>= fmap Just . action
#endif

-- | Run an action while a Unix socket listens at the given path, given how
-- many connections it has accepted so far; each is closed once accepted.
withUnixListener :: FilePath -> (IO Int -> IO a) -> IO a
withUnixListener path action =
  bracket open Socket.close $ \listener -> do
    accepted <- newIORef (0 :: Int)
    let serve = forever $ do
          (connection, _) <- Socket.accept listener
          Socket.close connection
          atomicModifyIORef' accepted (\count -> (count + 1, ()))
    withAsync serve $ \_ -> action (readIORef accepted)
  where
    open =
      bracketOnError (Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol) Socket.close $ \listener ->
        listener <$ (Socket.bind listener (Socket.SockAddrUnix path) *> Socket.listen listener Socket.maxListenQueue)

-- | Run an action with this process ignoring @SIGHUP@, which a process it
-- starts then inherits, and restore what it did before afterwards.
withHangupIgnored :: IO a -> IO a
#if defined(mingw32_HOST_OS)
withHangupIgnored = id
#else
withHangupIgnored action =
  bracket (installHandler sigHUP Ignore Nothing) (\previous -> installHandler sigHUP previous Nothing) (const action)
#endif

-- | Send @SIGCONT@ to a process this one started, which continues it if it
-- is stopped.
continueProcess :: Proc.ProcessHandle -> IO ()
#if defined(mingw32_HOST_OS)
continueProcess _ = pure ()
#else
continueProcess handle = Proc.getPid handle >>= traverse_ (signalProcess sigCONT)
#endif
