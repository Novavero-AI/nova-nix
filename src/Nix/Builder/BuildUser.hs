{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Build users: the accounts a build runs as when nova-nix runs as root,
-- as upstream's do, so that a builder can change neither the store nor
-- another build, and every process it leaves can be found by its user.
--
-- == The setting
--
-- @build-users-group@ names the group whose members are the build users.
-- Upstream defaults it to @nixbld@ for root and to nothing for anyone
-- else, and builds as its members only when it runs as root on Linux or
-- macOS ('buildUsersFor'); a build run otherwise runs as the user nova-nix
-- runs as.  A daemon is to refuse a connection from a peer whose primary
-- group is the one the setting names ('peerInBuildUsersGroup').
--
-- == The pool
--
-- Each build takes one member for itself, as upstream's does: a member is
-- free when no process holds the lock on its file in the user pool,
-- @userpool\/UID@ in upstream's state directory, which every process that
-- builds as these users shares, nova-nix's and upstream's alike, so that
-- no two builds run as one user at once.  The choice is made over a
-- t'UserDatabase', a capability record, so the rules run over a model of
-- the user database in tests ('acquireBuildUser'), and a build waits,
-- polling as upstream does, while every member is taken ('withBuildUser').
--
-- == The processes
--
-- Every process of the build user is killed before its build starts and
-- after it ends ('killBuildUser'), as upstream kills them, so a process a
-- builder left behind cannot outlive its build or reach the next one.  A
-- builder outside the sandbox is started as the build user by a small C
-- shim that drops to the user between the fork and the exec
-- ('spawnAsBuildUser'), with the groups looked up beforehand, since the
-- process library can drop to another user only by looking the user up in
-- the forked child of this threaded process.  On Linux and macOS the shim
-- starts every builder outside the sandbox, a build that runs as the user
-- nova-nix runs as too, with no drop ('runAsInvoker'), so each starts as
-- upstream's child starts one and ends as upstream ends one.
module Nix.Builder.BuildUser
  ( -- * The setting
    ProcessPrivilege (..),
    processPrivilege,
    defaultBuildUsersGroup,
    BuildUsers (NoBuildUsers),
    buildUsersFor,
    userPoolDir,

    -- * Who may not connect
    peerInBuildUsersGroup,

    -- * Choosing a build user
    BuildUser,
    buUid,
    buGid,
    buSupplementaryGids,
    Member (..),
    UserDatabase (..),
    acquireBuildUser,
    withBuildUser,
    waitingForBuildUserMessage,

    -- * A build user's processes and files
    killBuildUser,
    giveToBuildUser,
    runAsBuildUser,
    runAsInvoker,
    openStoreToBuildUsers,
    worldWritableAncestor,
    worldWritableMessage,
    invokingUserIds,
  )
where

import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word32)
import Nix.Logger (Logger (..))
import System.Exit (ExitCode (..))
import qualified System.FilePath.Posix as PosixPath

#if !defined(mingw32_HOST_OS)

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, bracket, displayException, finally, mask, onException, try, uninterruptibleMask_)
import Control.Monad (unless, void)
import Data.Bits (complement, (.&.))
import Foreign.C.Error (Errno (..), eINVAL, ePERM)
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CInt (..), CUInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray0, withArrayLen)
import Foreign.Marshal.Utils (withMany)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import qualified GHC.Foreign as GHC
import GHC.IO.Encoding (getFileSystemEncoding)
import GHC.IO.FD (fdFD)
import GHC.IO.Handle.FD (handleToFd)
import Nix.Builder.Log (openLogPipe, streamBuildLog)
import Nix.Builder.Sandbox (errnoSuffix, setupContext)
import System.Directory (createDirectoryIfMissing)
import System.FileLock (FileLock, SharedExclusive (Exclusive), tryLockFile, unlockFile)
import System.FilePath (dropTrailingPathSeparator, takeDirectory, (</>))
import System.IO (hClose)
import System.Posix.Files (fileGroup, fileMode, fileOwner, fileTypeModes, getFileStatus, getSymbolicLinkStatus, otherWriteMode, setFileMode, setOwnerAndGroup)
import System.Posix.IO (OpenFileFlags (..), OpenMode (ReadOnly, WriteOnly), closeFd, defaultFileFlags, openFd)
import System.Posix.Process (ProcessStatus (..), getProcessStatus)
import System.IO.Error (catchIOError, isPermissionError)
import System.Posix.Signals (sigKILL, signalProcessGroup)
import System.Posix.Types (Fd (..), ProcessID)
import qualified System.Posix.User.ByteString as User

#endif

#if defined(linux_HOST_OS)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Storable (poke)
#endif

-- ---------------------------------------------------------------------------
-- The setting
-- ---------------------------------------------------------------------------

-- | Whether nova-nix runs as root, which upstream's build users need.
data ProcessPrivilege = RunsAsRoot | RunsUnprivileged
  deriving (Eq, Show)

-- | Whether this process runs as root: its real user is uid 0, upstream's
-- test.  Never on Windows, which has no uid.
processPrivilege :: IO ProcessPrivilege
#if defined(mingw32_HOST_OS)
processPrivilege = pure RunsUnprivileged
#else
processPrivilege = (\uid -> if uid == 0 then RunsAsRoot else RunsUnprivileged) <$> User.getRealUserID
#endif

-- | @build-users-group@ where no source sets it: upstream's @nixbld@ for
-- root, and for anyone else nothing, which builds as the user nova-nix
-- runs as.
defaultBuildUsersGroup :: ProcessPrivilege -> ByteString
defaultBuildUsersGroup privilege = case privilege of
  RunsAsRoot -> "nixbld"
  RunsUnprivileged -> ""

-- | Whom a spawned builder runs as.  Only 'buildUsersFor' makes the
-- first, for root, so a build user is taken only where builds are to run
-- as one ('withBuildUser').
data BuildUsers
  = -- | A member of this group, taken from the pool in this directory.
    BuildUsersOf !ByteString !FilePath
  | -- | The user nova-nix runs as.
    NoBuildUsers
  deriving (Eq, Show)

-- | Whom builds run as, given whether nova-nix runs as root, the user pool's
-- directory ('userPoolDir') and @build-users-group@: its members, for root
-- on Linux and macOS with the group named, as upstream decides; otherwise
-- the user nova-nix runs as, whose builds cannot be another user's.
buildUsersFor :: ProcessPrivilege -> FilePath -> ByteString -> BuildUsers
buildUsersFor privilege pool group
  | buildUsersSupported, privilege == RunsAsRoot, not (BS.null group) = BuildUsersOf group pool
  | otherwise = NoBuildUsers

-- | Whether this system has build users: Linux and macOS, as upstream's.
buildUsersSupported :: Bool
#if defined(linux_HOST_OS) || defined(darwin_HOST_OS)
buildUsersSupported = True
#else
buildUsersSupported = False
#endif

-- | The user pool's directory, given @NIX_STATE_DIR@ where it is set and
-- not empty: @userpool@ in upstream's state directory, which upstream's
-- daemon shares, so that its builds and nova-nix's never take one user at
-- once.  It is a POSIX path on every system, since build users exist only
-- on Linux and macOS ('buildUsersFor'), and a Windows build of nova-nix
-- names it the same.
userPoolDir :: Maybe FilePath -> FilePath
userPoolDir stateDir = fromMaybe defaultStateDir stateDir PosixPath.</> "userpool"
  where
    defaultStateDir = "/nix/var/nix"

-- ---------------------------------------------------------------------------
-- Who may not connect
-- ---------------------------------------------------------------------------

-- | Whether a daemon is to refuse a peer for its primary group, given
-- @build-users-group@ and the name of the peer's primary group, where the
-- system gives one: a peer whose primary group is the build users' is
-- refused, whatever @trusted-users@ says, as upstream's daemon refuses
-- one, since a build user is whom a build runs as.  macOS gives a peer no
-- group, so there the answer is never.
peerInBuildUsersGroup :: ByteString -> Maybe ByteString -> Bool
peerInBuildUsersGroup group peerGroup = not (BS.null group) && peerGroup == Just group

-- ---------------------------------------------------------------------------
-- Choosing a build user
-- ---------------------------------------------------------------------------

-- | A member of the build users' group a build may run as: its uid, the
-- build users' group, and its supplementary groups but that one.
data Member = Member
  { memberUid :: !Word32,
    memberGid :: !Word32,
    memberGroups :: ![Word32]
  }
  deriving (Eq, Show)

-- | A member a build holds and runs as.  Only 'withBuildUser' makes one,
-- for a process that runs as root with build users ('buildUsersFor'), its
-- lock held and its uid neither root's nor this process's, so a build
-- user's processes, which 'killBuildUser' ends and 'runAsBuildUser'
-- starts, are never the caller's own.
newtype BuildUser = BuildUser Member
  deriving (Eq, Show)

-- | A build user's uid.
buUid :: BuildUser -> Word32
buUid (BuildUser member) = memberUid member

-- | The build users' group, which a build user builds as.
buGid :: BuildUser -> Word32
buGid (BuildUser member) = memberGid member

-- | A build user's supplementary groups but the build users' own.
buSupplementaryGids :: BuildUser -> [Word32]
buSupplementaryGids (BuildUser member) = memberGroups member

-- | What choosing a build user reads and locks, over a monad, holding a
-- lock of type @lock@.
data UserDatabase m lock = UserDatabase
  { -- | A group's id and its members' names, by its name, or nothing when
    -- there is no such group.
    udGroup :: ByteString -> m (Maybe (Word32, [ByteString])),
    -- | A user's uid and primary group, by name, or nothing when there is
    -- no such user.
    udUser :: ByteString -> m (Maybe (Word32, Word32)),
    -- | Every group a user is in, given its name and primary group: the
    -- groups a build as it keeps.
    udGroups :: ByteString -> Word32 -> m [Word32],
    -- | Take the lock on a uid's file in the pool, or nothing when another
    -- holder has it; 'Left' is why the file cannot be opened.
    udTryLock :: Word32 -> m (Either Text (Maybe lock)),
    -- | Give a lock back.
    udRelease :: lock -> m ()
  }

-- | The first member of the build users' group whose lock is free, with
-- the lock, given the uids this process runs as and the group's name;
-- nothing while every member is taken; or, in upstream's words, why there
-- is none to take: the group is missing or has no members, a member names
-- no user, or the member a build would take is the user nova-nix runs as.
-- Members are tried in the order the group lists them, and each is looked
-- up only as far as the first free one, as upstream looks them up.  On
-- Linux the user keeps its supplementary groups, as upstream lets an
-- administrator give a build one such as @kvm@.
acquireBuildUser :: (Monad m) => UserDatabase m lock -> [Word32] -> ByteString -> m (Either Text (Maybe (Member, lock)))
acquireBuildUser database ownUids group = do
  found <- udGroup database group
  case found of
    Nothing -> pure (Left ("the group '" <> shown group <> "' specified in 'build-users-group' does not exist"))
    Just (_, []) -> pure (Left ("the build users group '" <> shown group <> "' has no members"))
    Just (gid, members) -> runExceptT (firstFree gid members)
  where
    inner action = ExceptT (Right <$> action)
    firstFree _ [] = pure Nothing
    firstFree gid (member : rest) = do
      entry <- inner (udUser database member)
      case entry of
        Nothing -> throwError ("the user '" <> shown member <> "' in the group '" <> shown group <> "' does not exist")
        Just (uid, primary) -> do
          locked <- ExceptT (udTryLock database uid)
          case locked of
            Nothing -> firstFree gid rest
            Just lock
              | uid `elem` ownUids -> do
                  inner (udRelease database lock)
                  throwError ("the Nix user should not be a member of '" <> shown group <> "'")
              | otherwise -> do
                  groups <- inner (udGroups database member primary)
                  pure (Just (Member uid gid (filter (/= gid) groups), lock))
    -- A name in a message, shown with any byte that is not UTF-8 replaced.
    shown = TE.decodeUtf8Lenient

-- | The line a build waiting for a free build user says once, in
-- upstream's words, given the @.drv@ path.
waitingForBuildUserMessage :: Text -> Text
waitingForBuildUserMessage drvPath = "waiting for a free build user ID for '" <> drvPath <> "'"

-- | Run an action as whom builds run as, given the @.drv@ path the build
-- is of: with no build users, as the user nova-nix runs as ('Nothing');
-- otherwise as a free member of the build users' group, taken
-- ('acquireBuildUser') and given back when the action ends, however it
-- ends.  While every member is taken the build says so once
-- ('waitingForBuildUserMessage') and looks again at upstream's default
-- @poll-interval@, since a member another process gives back announces
-- nothing.  'Left' is why there is no member to take, a process that is
-- not root among the reasons, whatever privilege 'buildUsersFor' was
-- told of.
withBuildUser :: Logger -> BuildUsers -> Text -> (Maybe BuildUser -> IO a) -> IO (Either Text a)

-- | Kill every process of a build user, as upstream kills them before and
-- after a build, from a child of this process that is that user, so the
-- kill reaches the user's processes alone.  'Left' is why it could not, in
-- upstream's words.
killBuildUser :: BuildUser -> IO (Either Text ())

-- | Give a file or directory nova-nix made for a build to its build user,
-- as upstream gives a build its directory and the files it writes there.
giveToBuildUser :: BuildUser -> FilePath -> IO ()

-- | Run a builder as a build user, outside any sandbox, given the logger
-- its lines go to ('streamBuildLog'), the program, its arguments, its
-- environment as @NAME=VALUE@ entries, and the directory it starts in.
-- It is started and ended as 'runAsInvoker' starts and ends one, with the
-- user's uid, the build users' group and the user's supplementary groups,
-- set between the fork and the exec.  The processes it leaves outside its
-- process group are the caller's to kill ('killBuildUser').
runAsBuildUser :: Logger -> BuildUser -> FilePath -> [String] -> [ByteString] -> FilePath -> IO (Either Text ExitCode)

-- | Run a builder outside any sandbox as the user nova-nix runs as, given
-- what 'runAsBuildUser' is given but the user, as upstream runs one.  The
-- program is executed by its path, with no search of @PATH@.  It leads a
-- session of its own, so it has no controlling terminal and its process
-- group holds what it starts; it reads @\/dev\/null@ and writes its
-- standard output and error to one pipe, the build log, with every other
-- descriptor of this process closed; and it dumps no core, its soft limit
-- for one being 0.  Once the log ends, or when an interrupt comes, every
-- process left in its group is sent SIGKILL and the builder is reaped, as
-- upstream takes a builder's status, so a builder that closed its log and
-- ran on ends on SIGKILL.  Answers how it exited, a signal as the process
-- library reports one (the signal's number, negated), or why it could not
-- start, in upstream's words for each step.  Linux and macOS only.
runAsInvoker :: Logger -> FilePath -> [String] -> [ByteString] -> FilePath -> IO (Either Text ExitCode)

-- | Open a store's directory to the build users, as upstream opens it
-- when it runs as root with build users: owned by root and their group,
-- with mode 1775, so a builder can write its outputs there and remove none
-- but its own.  A directory already so is left as it is.
openStoreToBuildUsers :: FilePath -> BuildUser -> IO ()

-- | The first of a directory and its ancestors, read without following a
-- symlink, whose mode lets every user write it, which on Linux every
-- symlink's does; nothing when there is none.  Upstream refuses to build
-- as a build user in a directory below one, since any user could replace
-- the build's directory there.
worldWritableAncestor :: FilePath -> IO (Maybe FilePath)

-- | Upstream's refusal of a build directory below a directory every user
-- may write.
worldWritableMessage :: FilePath -> Text
worldWritableMessage path = "Path \"" <> T.pack path <> "\" is world-writable or a symlink. That's not allowed for security."

-- | The ids of the user nova-nix runs as: its real uid and gid.  Windows
-- has none, and answers zero for each.
invokingUserIds :: IO (Word32, Word32)

#if defined(mingw32_HOST_OS)

withBuildUser _ users _ action = case users of
  NoBuildUsers -> Right <$> action Nothing
  BuildUsersOf _ _ -> pure (Left unsupported)

killBuildUser _ = pure (Left unsupported)

giveToBuildUser _ _ = pure ()

runAsBuildUser _ _ _ _ _ _ = pure (Left unsupported)

runAsInvoker _ _ _ _ _ = pure (Left unsupported)

openStoreToBuildUsers _ _ = pure ()

worldWritableAncestor _ = pure Nothing

invokingUserIds = pure (0, 0)

-- | Why a build user is refused where there are none.
unsupported :: Text
unsupported = "build users need Linux or macOS"

#else

withBuildUser logger users drvPath action = case users of
  NoBuildUsers -> Right <$> action Nothing
  BuildUsersOf group pool -> do
    ownUids <- traverse (fmap fromIntegral) [User.getRealUserID, User.getEffectiveUserID]
    if any (/= 0) ownUids
      then pure (Left ("builds run as the members of '" <> TE.decodeUtf8Lenient group <> "' only for root, and this process is not root"))
      else do
        createDirectoryIfMissing True pool
        let attempt announced = mask $ \restore -> do
              found <- acquireBuildUser (systemUserDatabase pool) ownUids group
              case found of
                Left reason -> pure (Left reason)
                Right (Just (member, lock)) -> Right <$> (restore (action (Just (BuildUser member))) `finally` unlockFile lock)
                Right Nothing -> restore $ do
                  unless announced (logMessage logger (waitingForBuildUserMessage drvPath))
                  threadDelay freeUserPollMicros
                  attempt True
        attempt False

-- | How long a build waiting for a free build user waits before it looks
-- again: upstream's default @poll-interval@, five seconds.
freeUserPollMicros :: Int
freeUserPollMicros = 5 * 1000000

-- | The system's user database and the pool's locks.  A group or user
-- the system cannot look up is one it does not have, as upstream reads a
-- failed lookup.  A member's lock file is made with mode 0600, as
-- upstream makes it, and locked with @flock@, as upstream's is, so a
-- holder in either excludes the other.
systemUserDatabase :: FilePath -> UserDatabase IO FileLock
systemUserDatabase pool =
  UserDatabase
    { udGroup = \name -> either (\(_ :: IOException) -> Nothing) (\entry -> Just (fromIntegral (User.groupID entry), User.groupMembers entry)) <$> try (User.getGroupEntryForName name),
      udUser = \name -> either (\(_ :: IOException) -> Nothing) (\entry -> Just (fromIntegral (User.userID entry), fromIntegral (User.userGroupID entry))) <$> try (User.getUserEntryForName name),
      udGroups = groupList,
      udTryLock = \uid -> do
        let file = pool </> show uid
        opened <- try (openFd file WriteOnly defaultFileFlags {creat = Just lockFileMode, cloexec = True} >>= closeFd)
        case opened of
          Left (failure :: IOException) -> pure (Left ("opening user lock \"" <> T.pack file <> "\": " <> T.pack (displayException failure)))
          Right () -> Right <$> tryLockFile file Exclusive,
      udRelease = unlockFile
    }
  where
    lockFileMode = 0o600

killBuildUser user = alloca $ \statusPtr -> do
  failure <- c_nn_kill_user (fromIntegral (buUid user)) statusPtr
  status <- peek statusPtr
  if failure /= 0
    then pure (Left (refused (Errno failure)))
    else killOutcome status
  where
    -- The shim refuses uid 0 and a caller that is not root before it
    -- forks, which a build user never is; any other errno is the fork's
    -- or the wait's.
    refused errno@(Errno number)
      | errno == eINVAL = cannot "uid 0 is root's, whose kill would reach every process"
      | errno == ePERM = cannot "only root may kill a build user's processes"
      | otherwise = "unable to fork" <> errnoSuffix (fromIntegral number)
    killOutcome status
      | status == 0 = pure (Right ())
      | status > signalledBase = do
          let signal = status - signalledBase
          description <- c_nn_signal_text signal >>= peekCString
          pure (Left (cannot ("failed due to signal " <> T.pack (show signal) <> " (" <> T.pack description <> ")")))
      | otherwise = pure (Left (cannot ("failed with exit code " <> T.pack (show status))))
    cannot reason = "cannot kill processes for uid '" <> T.pack (show (buUid user)) <> "': " <> reason
    -- What nn_kill_user adds to the number of a signal that ended its child.
    signalledBase = 256

giveToBuildUser user path = setOwnerAndGroup path (fromIntegral (buUid user)) (fromIntegral (buGid user))

runAsBuildUser logger user program arguments environment workDir =
  runStarted logger (spawnAsBuildUser user program arguments environment workDir)

runAsInvoker logger program arguments environment workDir =
  runStarted logger (spawnAsInvoker program arguments environment workDir)

-- | Run the builder a start begins, given its standard input and its log,
-- to its end, as 'runAsInvoker' says.
runStarted :: Logger -> (Fd -> Fd -> IO (Either Text ProcessID)) -> IO (Either Text ExitCode)
runStarted logger start =
  bracket openLogPipe closePipe $ \(fromBuilder, toLog) ->
    bracket (openFd nullDevice ReadOnly defaultFileFlags {cloexec = True}) closeFd $ \input -> do
      output <- Fd . fdFD <$> handleToFd toLog
      -- The builder is reaped only once nothing can interrupt the reap:
      -- until then its process id names it alone, and names its process
      -- group, so a kill of the group can reach no other process.
      mask $ \restore -> do
        spawned <- start input output
        case spawned of
          Left reason -> pure (Left reason)
          Right pid -> do
            let reap = getProcessStatus True False pid
                -- An interrupt may come once the group has ended, which
                -- the kill then fails on.
                quietly action = void (try action :: IO (Either IOException ()))
                stop = uninterruptibleMask_ (quietly (signalProcessGroup sigKILL pid) *> quietly (void reap))
                -- The builder's tree holds the only write ends once this
                -- process closes its own, so the tree's end ends the log.
                follow = hClose toLog *> streamBuildLog fromBuilder logger
            (restore follow *> endGroup pid) `onException` stop
            Right . exitOf <$> reap
  where
    closePipe (fromBuilder, toLog) = hClose fromBuilder *> hClose toLog
    nullDevice = "/dev/null"
    exitOf ended = case ended of
      Just (Exited code) -> code
      Just (Terminated signal _) -> ExitFailure (negate (fromIntegral signal))
      Just (Stopped signal) -> ExitFailure (negate (fromIntegral signal))
      Nothing -> ExitFailure 1

-- | Send SIGKILL to what is left of a builder's process group once its log
-- has ended.  macOS refuses the signal to a group whose only process is
-- the builder, ended and not yet reaped (EPERM), which upstream passes
-- over too.
endGroup :: ProcessID -> IO ()
endGroup pid = signalProcessGroup sigKILL pid `catchIOError` \err -> unless (isPermissionError err) (ioError err)

openStoreToBuildUsers storeDir user = do
  status <- getFileStatus storeDir
  let group = fromIntegral (buGid user)
  unless (fileOwner status == 0 && fileGroup status == group && fileMode status .&. complement fileTypeModes == storeMode) $ do
    setOwnerAndGroup storeDir 0 group
    setFileMode storeDir storeMode
  where
    storeMode = 0o1775

worldWritableAncestor start = go (dropTrailingPathSeparator start)
  where
    go path = do
      status <- getSymbolicLinkStatus path
      let parent = takeDirectory path
      if fileMode status .&. otherWriteMode /= 0
        then pure (Just path)
        else if parent == path then pure Nothing else go parent

invokingUserIds = (\uid gid -> (fromIntegral uid, fromIntegral gid)) <$> User.getRealUserID <*> User.getRealGroupID

-- | Start a builder as a build user, outside any sandbox: the program, its
-- arguments, its environment as @NAME=VALUE@ entries, the directory it
-- starts in, and the descriptors its standard input and its standard
-- output and error are, every other descriptor closed.  Answers its
-- process id once the program has replaced the child, or why it could
-- not, in upstream's words for each step.
spawnAsBuildUser :: BuildUser -> FilePath -> [String] -> [ByteString] -> FilePath -> Fd -> Fd -> IO (Either Text ProcessID)
spawnAsBuildUser user = spawnWith $ \cProgram argv envp cWorkDir input output pidPtr stepPtr ->
  withArrayLen (map fromIntegral (buSupplementaryGids user)) $ \groupCount groups ->
    c_nn_spawn_as_user cProgram argv envp cWorkDir input output (fromIntegral (buUid user)) (fromIntegral (buGid user)) (fromIntegral groupCount) groups pidPtr stepPtr

-- | Start a builder as the user nova-nix runs as, given what
-- 'spawnAsBuildUser' is given but the user.
spawnAsInvoker :: FilePath -> [String] -> [ByteString] -> FilePath -> Fd -> Fd -> IO (Either Text ProcessID)
spawnAsInvoker = spawnWith c_nn_spawn_as_invoker

-- | Start a builder through the shim's call given, which takes the
-- program, its argument and environment vectors, its directory, its
-- input and output descriptors, and where to put its process id and the
-- step that failed.
spawnWith ::
  (CString -> Ptr CString -> Ptr CString -> CString -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> IO CInt) ->
  FilePath ->
  [String] ->
  [ByteString] ->
  FilePath ->
  Fd ->
  Fd ->
  IO (Either Text ProcessID)
spawnWith spawn program arguments environment workDir (Fd input) (Fd output) = do
  encoding <- getFileSystemEncoding
  let withPath = GHC.withCString encoding
  withPath program $ \cProgram ->
    withMany withPath (program : arguments) $ \cArguments ->
      withArray0 nullPtr cArguments $ \argv ->
        withMany BS.useAsCString environment $ \cEnvironment ->
          withArray0 nullPtr cEnvironment $ \envp ->
            withPath workDir $ \cWorkDir ->
              alloca $ \pidPtr ->
                alloca $ \stepPtr -> do
                  failure <- spawn cProgram argv envp cWorkDir input output pidPtr stepPtr
                  if failure == 0
                    then Right . fromIntegral <$> peek pidPtr
                    else Left . stepMessage failure <$> peek stepPtr
  where
    stepMessage failure step = setupContext <> stepText step <> errnoSuffix (fromIntegral failure)
    -- The values of nn_spawn_step in nn_buildusers.h.
    stepText :: CInt -> Text
    stepText step = case step of
      1 -> "creating pipe"
      2 -> "unable to fork"
      3 -> "creating a new session"
      4 -> "cannot dup null device into stdin"
      5 -> "cannot dup output pipe into stdout"
      6 -> "changing into \"" <> T.pack workDir <> "\""
      7 -> "cannot set supplementary groups of build user"
      8 -> "setgid failed"
      9 -> "setuid failed"
      _ -> "executing '" <> T.pack program <> "'"

-- | Every group a user is in, given its name and primary group, as the
-- system's @getgrouplist@ answers on Linux, where upstream keeps a build
-- user's supplementary groups; none elsewhere, where upstream keeps none.
groupList :: ByteString -> Word32 -> IO [Word32]
#if defined(linux_HOST_OS)
groupList name primary = BS.useAsCString name (grow initialGroups)
  where
    -- getgrouplist answers -1, with the count it needs in place of the
    -- count it was given, only when the list does not fit.
    grow capacity cName = allocaArray capacity $ \groups ->
      alloca $ \countPtr -> do
        poke countPtr (fromIntegral capacity :: CInt)
        answer <- c_getgrouplist cName (fromIntegral primary) groups countPtr
        count <- fromIntegral <$> peek countPtr
        if answer < 0
          then grow (max count (2 * capacity)) cName
          else map fromIntegral <$> peekArray count groups
    initialGroups = 32

foreign import ccall safe "grp.h getgrouplist"
  c_getgrouplist :: CString -> CUInt -> Ptr CUInt -> Ptr CInt -> IO CInt
#else
groupList _ _ = pure []
#endif

foreign import ccall safe "nn_buildusers.h nn_kill_user"
  c_nn_kill_user :: CUInt -> Ptr CInt -> IO CInt

foreign import ccall unsafe "nn_buildusers.h nn_signal_text"
  c_nn_signal_text :: CInt -> IO CString

foreign import ccall safe "nn_buildusers.h nn_spawn_as_user"
  c_nn_spawn_as_user :: CString -> Ptr CString -> Ptr CString -> CString -> CInt -> CInt -> CUInt -> CUInt -> CInt -> Ptr CUInt -> Ptr CInt -> Ptr CInt -> IO CInt

foreign import ccall safe "nn_buildusers.h nn_spawn_as_invoker"
  c_nn_spawn_as_invoker :: CString -> Ptr CString -> Ptr CString -> CString -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> IO CInt

#endif
