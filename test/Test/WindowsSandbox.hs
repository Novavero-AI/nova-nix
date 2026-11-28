{-# LANGUAGE ScopedTypeVariables #-}

-- | The sandbox helper's Windows backend (@sandbox\/windows.c@), run as
-- the service that logs each build slot on runs it.
--
-- Most of its checks need that service's account: a virtual account
-- holding the privileges that start a process as another account.  CI's
-- Windows build runs the suite as an administrator
-- ('windowsSandboxInstall'), given the runner's user, whose files, hive
-- and processes a build must not reach.  That run makes two slot
-- accounts with the batch logon right, and a test service that runs as
-- its virtual account with those privileges; lays out a directory that
-- only the service, SYSTEM and administrators may use, and writes there
-- what the checks are given, the slots' passwords among it; starts the
-- service, waits for it to stop, and prints the checks' log.  It removes
-- the service and the accounts, each one's rights first, however the
-- checks end.  The service ('windowsSandboxService') runs the checks as a
-- process of its own ('windowsSandboxAccounts'), which logs each slot on
-- as the service that runs builds does and runs the helper over the plans
-- 'Sandbox.renderWindowsPlan' writes, with the suite itself as the
-- builder ('windowsSandboxBuilder').  The suite's own run, as a user
-- without those privileges, checks that the probe and a run report the
-- step that needs them ('testWindowsSandboxIO').
module Test.WindowsSandbox
  ( testWindowsSandboxIO,
    windowsSandboxInstallFlag,
    windowsSandboxInstall,
    windowsSandboxServiceFlag,
    windowsSandboxService,
    windowsSandboxAccountsFlag,
    windowsSandboxAccounts,
    windowsSandboxBuilderFlag,
    windowsSandboxBuilder,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (poll, wait, withAsync)
import Control.Exception (IOException, bracket, finally, try)
import Control.Monad (unless, void, when, (<=<))
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Foldable (traverse_)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (intercalate, isPrefixOf, sort)
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Word (Word32, Word64)
import qualified DirDacl
import Foreign.C.Error (Errno (..), eINVAL)
import Foreign.C.String (CString, CWString, peekCString, peekCWString, withCWString)
import Foreign.C.Types (CInt (..), CULLong (..), CULong (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (Ptr, nullPtr, ptrToWordPtr)
import Foreign.Storable (peek)
import GHC.IO.Handle (hDuplicateTo)
import qualified Nix.Builder.Sandbox as Sandbox
import qualified Nix.Builder.Sandbox.Spawn as Spawn
import Nix.Logger (Logger (..))
import Nix.Store (canonicalizeMetadata)
import Nix.Store.Access (Sid, createStoreDirectory, sidFromText, sidText)
import Nix.Store.CaseSensitive (processCaseHack)
import qualified Nix.Store.ExecBit as ExecBit
import qualified NovaCache.Base64 as B64
import Numeric (showHex)
import qualified System.Directory as Dir
import System.Environment (getExecutablePath, lookupEnv)
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.FilePath (takeDirectory, takeDrive, takeFileName, (</>))
import System.IO (BufferMode (..), Handle, IOMode (..), hClose, hFlush, hSetBuffering, stderr, stdout, withBinaryFile)
import System.IO.Error (ioeGetFileName)
import qualified System.Info as SI
import qualified System.Process as Proc
import System.Timeout (timeout)
import Test.Builder (processWaitMicros, sandboxHelperVar, startWaitMicros, waitUntil, waitUntilWithin)
import Test.Harness (TestResult (..), andThen, assertEqual, runTest, runTestM)
import Text.Read (readMaybe)

-- ---------------------------------------------------------------------------
-- Windows' calls (test/cbits/nn_test_sandbox_windows.c)
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "nova_test_account_make"
  c_account_make :: CWString -> Ptr CWString -> Ptr CString -> IO CULong

foreign import ccall unsafe "nova_test_group_make"
  c_group_make :: CWString -> IO CULong

foreign import ccall unsafe "nova_test_group_add"
  c_group_add :: CWString -> CWString -> IO CULong

foreign import ccall unsafe "nova_test_group_remove"
  c_group_remove :: CWString -> IO CULong

foreign import ccall unsafe "nova_test_account_sid"
  c_account_sid :: CWString -> Ptr CWString -> IO CULong

foreign import ccall unsafe "nova_test_mkdir"
  c_mkdir :: CWString -> IO CULong

foreign import ccall unsafe "nova_test_lock"
  c_lock :: CWString -> IO CULong

foreign import ccall unsafe "nova_test_account_remove"
  c_account_remove :: CWString -> Ptr CString -> IO CULong

foreign import ccall unsafe "nova_test_service_install"
  c_service_install :: CWString -> CWString -> Ptr CWString -> Ptr CString -> IO CULong

foreign import ccall unsafe "nova_test_service_start"
  c_service_start :: CWString -> IO CULong

foreign import ccall unsafe "nova_test_service_state"
  c_service_state :: CWString -> Ptr CULong -> Ptr CULong -> IO CULong

foreign import ccall unsafe "nova_test_service_remove"
  c_service_remove :: CWString -> Ptr CString -> IO CULong

-- Safe, as it returns only once the service has stopped.
foreign import ccall safe "nova_test_service_dispatch"
  c_service_dispatch :: CWString -> CWString -> IO CULong

foreign import ccall unsafe "nova_test_account_logon"
  c_account_logon :: CWString -> CWString -> Ptr CWString -> Ptr (Ptr ()) -> IO CULong

foreign import ccall unsafe "nova_test_close_handle"
  c_close_handle :: Ptr () -> IO CULong

foreign import ccall unsafe "nova_test_local_free"
  c_local_free :: CWString -> IO ()

foreign import ccall unsafe "nova_test_directory_with_sddl"
  c_directory_with_sddl :: CWString -> CWString -> IO CULong

foreign import ccall unsafe "nova_test_set_sddl"
  c_set_sddl :: CWString -> CWString -> IO CULong

foreign import ccall unsafe "nova_test_start_as"
  c_start_as :: Ptr () -> CWString -> Ptr CULong -> IO CULong

foreign import ccall unsafe "nova_test_process_running"
  c_process_running :: CULong -> IO CInt

foreign import ccall unsafe "nova_test_token_user"
  c_token_user :: Ptr CWString -> IO CULong

foreign import ccall unsafe "nova_test_user_object_name"
  c_user_object_name :: CInt -> CWString -> CULong -> IO CULong

foreign import ccall unsafe "nova_test_logon_id"
  c_logon_id :: Ptr CULong -> Ptr CULong -> IO CULong

foreign import ccall unsafe "nova_test_open_station"
  c_open_station :: CWString -> IO CULong

foreign import ccall unsafe "nova_test_station_dacl"
  c_station_dacl :: CWString -> Ptr CWString -> IO CULong

foreign import ccall unsafe "nova_test_account_processes"
  c_account_processes :: CWString -> CWString -> CULong -> Ptr CULong -> IO CULong

foreign import ccall unsafe "nova_test_stdout_value"
  c_stdout_value :: IO CULong

foreign import ccall unsafe "nova_test_hold_handle"
  c_hold_handle :: CULong -> CULong -> IO CULong

foreign import ccall unsafe "nova_test_in_job"
  c_in_job :: IO CInt

foreign import ccall unsafe "nova_test_console"
  c_console :: IO CULong

foreign import ccall unsafe "nova_test_commit"
  c_commit :: CULLong -> IO CULong

foreign import ccall unsafe "nova_test_start"
  c_start :: CWString -> CULong -> Ptr CULong -> Ptr (Ptr ()) -> IO CULong

-- Safe, as it blocks until the process it waits for ends.
foreign import ccall safe "nova_test_wait"
  c_wait :: Ptr () -> Ptr CULong -> IO CULong

foreign import ccall unsafe "nova_test_crash"
  c_crash :: IO ()

foreign import ccall unsafe "nova_test_open"
  c_open :: CWString -> CULong -> IO CULong

foreign import ccall unsafe "nova_test_create"
  c_create :: CWString -> IO CULong

foreign import ccall unsafe "nova_test_delete"
  c_delete :: CWString -> IO CULong

foreign import ccall unsafe "nova_test_list"
  c_list :: CWString -> IO CULong

foreign import ccall unsafe "nova_test_open_key"
  c_open_key :: CWString -> IO CULong

foreign import ccall unsafe "nova_test_open_process"
  c_open_process :: CULong -> IO CULong

-- | A wide string the Win32 API allocated, read and then released.
takeWide :: CWString -> IO Text
takeWide wide = (T.pack <$> peekCWString wide) `finally` c_local_free wide

-- | Throw unless a call answered 0, naming the call and its error.
succeeded :: String -> CULong -> IO ()
succeeded call code =
  unless (code == 0) (ioError (userError (call <> " failed with Win32 error " <> show code)))

-- | Throw unless a call that names what failed in it answered 0, naming
-- that, or the call where it names nothing.
succeededNaming :: String -> String -> (Ptr CString -> IO CULong) -> IO ()
succeededNaming call subject run =
  alloca $ \failedOut -> do
    code <- run failedOut
    unless (code == 0) $ do
      failed <- peek failedOut
      named <- if failed == nullPtr then pure call else peekCString failed
      succeeded (named <> " for " <> subject) code

-- | @GENERIC_READ@ (winnt.h).
genericRead :: CULong
genericRead = 0x80000000

-- | @CREATE_BREAKAWAY_FROM_JOB@ (winbase.h).
breakawayFromJob :: CULong
breakawayFromJob = 0x01000000

-- | @ERROR_ACCESS_DENIED@ (winerror.h).
accessDenied :: CULong
accessDenied = 5

-- | @ERROR_FILE_NOT_FOUND@ (winerror.h).
fileNotFound :: Int
fileNotFound = 2

-- | @ERROR_NOT_ALL_ASSIGNED@ (winerror.h): what enabling a privilege the
-- token does not hold answers.
notAllAssigned :: Int
notAllAssigned = 1300

-- | @SERVICE_STOPPED@ (winsvc.h).
serviceStopped :: CULong
serviceStopped = 1

-- | The code a builder's crash raises (@nova_test_crash@).
crashExceptionCode :: Word32
crashExceptionCode = 0xE04E4E01

-- | The exit code the helper ends a build's processes with, the process
-- library's.
endedBuildExitCode :: Int
endedBuildExitCode = 1

-- ---------------------------------------------------------------------------
-- The suite's own run
-- ---------------------------------------------------------------------------

-- | The helper as the suite's own user runs it, which holds neither
-- privilege that starts a process as another account: the probe and a run
-- each fail at enabling them, and a plan the helper refuses is reported
-- as the plan's.  Windows only, with the helper 'sandboxHelperVar' names.
testWindowsSandboxIO :: IO [Bool]
testWindowsSandboxIO = do
  putStrLn "builder/sandbox-windows"
  named <- lookupEnv sandboxHelperVar
  case (SI.os, named) of
    ("mingw32", Just helper) -> do
      present <- Dir.doesFileExist helper
      if present
        then unprivilegedChecks helper
        else (: []) <$> runTest (T.pack sandboxHelperVar <> " names a file") (Fail (T.pack helper))
    ("mingw32", Nothing) -> do
      putStrLn ("  SKIP  needs " ++ sandboxHelperVar)
      pure []
    _ -> do
      putStrLn "  SKIP  Windows only"
      pure []

unprivilegedChecks :: FilePath -> IO [Bool]
unprivilegedChecks helper = case sidFromText localSystemSid of
  Nothing -> (: []) <$> runTest "SYSTEM's SID reads as one" (Fail localSystemSid)
  Just system -> unprivilegedChecksAs helper system

-- | SYSTEM's SID, which a plan whose run never reaches its token names.
localSystemSid :: Text
localSystemSid = "S-1-5-18"

-- | A plan's token value for a run that never reaches its token.
unreachedToken :: Word32
unreachedToken = 4

unprivilegedChecksAs :: FilePath -> Sid -> IO [Bool]
unprivilegedChecksAs helper system = do
  scratch <- Dir.getTemporaryDirectory
  let plan = Sandbox.WindowsPlan (pathBytes scratch) unreachedToken system ordinaryMemoryCap []
      runPlan rendered = fst <$> capturingBuildLog (\logger -> Spawn.runHelper logger helper rendered scratch ["cmd.exe"])
  sequence
    [ runTestM "without the privileges that start a build as another account, the probe fails at enabling them, as a run does" $ do
        (code, out, _) <- Proc.readCreateProcessWithExitCode ((Proc.proc helper ["probe"]) {Proc.env = Just []}) ""
        run <- runPlan (Sandbox.renderWindowsPlan plan)
        pure $
          assertEqual "probe" (ExitFailure 1, Right (Just (Sandbox.StatusRecord Sandbox.ProbePrivileges notAllAssigned Nothing))) (code, Sandbox.parseStatus Sandbox.probeStageName (BS8.pack out))
            `andThen` assertEqual "run" (ExitFailure 1, Right (Just (Sandbox.StatusRecord Sandbox.WindowsStagePrivileges notAllAssigned Nothing))) (Spawn.hrExit run, windowsRecord (Spawn.hrStatus run)),
      runTestM "a plan the helper refuses is reported as the plan's" $ do
        run <- runPlan (Sandbox.renderWindowsPlan plan {Sandbox.windowsPlanMemoryCap = 0})
        pure (assertEqual "run" (ExitFailure 1, Right (Just (Sandbox.StatusRecord Sandbox.WindowsStagePlan einval Nothing))) (Spawn.hrExit run, windowsRecord (Spawn.hrStatus run)))
    ]

-- | What the helper wrote on its status channel, read with the Windows
-- stages' names.
windowsRecord :: BS.ByteString -> Either Text (Maybe (Sandbox.StatusRecord Sandbox.WindowsStage))
windowsRecord = Sandbox.parseStatus Sandbox.windowsStageName

-- | The errno a plan the helper refuses is reported with.
einval :: Int
einval = case eINVAL of Errno number -> fromIntegral number

-- ---------------------------------------------------------------------------
-- What the checks are given
-- ---------------------------------------------------------------------------

-- | The runner's user, whose own a build must not reach: its SID, its
-- profile, TEMP and APPDATA directories, one of its processes, a file
-- only it may read and a file in its profile.
data Victim = Victim
  { victimSid :: !Text,
    victimProfile :: !FilePath,
    victimTemp :: !FilePath,
    victimAppData :: !FilePath,
    victimPid :: !Word32,
    victimOnlyFile :: !FilePath,
    victimProfileFile :: !FilePath
  }
  deriving (Show, Read)

-- | A slot's account as the service holds it: its name and its password,
-- which the service logs it on with.
data SlotAccount = SlotAccount
  { saName :: !Text,
    saPassword :: !Text
  }
  deriving (Show, Read)

-- | What the checks the test service runs are given: the directory they
-- lay out, which holds their log; the helper; MSYS2's bash; the runner's
-- user; and the two slots' accounts.  The administrator's run writes it
-- in that directory, where the service, SYSTEM and administrators alone
-- may read it.
data ServiceChecks = ServiceChecks
  { scRoot :: !FilePath,
    scHelper :: !FilePath,
    scBash :: !FilePath,
    scVictim :: !Victim,
    scSlotOne :: !SlotAccount,
    scSlotTwo :: !SlotAccount
  }
  deriving (Show, Read)

-- | The file the checks are given in, in their directory.
checksConfig :: FilePath -> FilePath
checksConfig root = root </> "checks"

-- | The checks' log, in their directory.
checksLog :: FilePath -> FilePath
checksLog root = root </> "checks.log"

-- | The name every account the checks use starts with, which CI's step
-- that removes what a run left behind matches.
accountPrefix :: Text
accountPrefix = "nnslot"

-- | The name the test service starts with, which CI's step that removes
-- what a run left behind matches.
servicePrefix :: Text
servicePrefix = "nnsandbox"

-- ---------------------------------------------------------------------------
-- The administrator's run
-- ---------------------------------------------------------------------------

-- | The flag CI's step runs the suite with, as an administrator: the
-- directory the checks' directory is made in, the helper, MSYS2's bash,
-- and the runner's user ('Victim').
windowsSandboxInstallFlag :: String
windowsSandboxInstallFlag = "--windows-sandbox-install"

-- | Make the slot accounts and the test service, run the checks through
-- the service, print their log, and remove the service and the accounts
-- however the checks end; exit 0 only when every check passed.
windowsSandboxInstall :: [String] -> IO ()
windowsSandboxInstall args = case args of
  [parent, helper, bash, sid, profile, temp, appData, pidText, onlyFile, profileFile]
    | Just pid <- readMaybe pidText -> do
        hSetBuffering stdout LineBuffering
        tag <- T.pack . show <$> Proc.getCurrentPid
        suite <- getExecutablePath
        let root = parent </> ("nova-nix-sandbox-windows-" <> T.unpack tag)
            service = servicePrefix <> tag
            command = unwords [quoted suite, windowsSandboxServiceFlag, T.unpack service, quoted (checksConfig root)]
            victim = Victim (T.pack sid) profile temp appData pid onlyFile profileFile
        code <-
          withBuildersGroup $
            withAccount (accountPrefix <> tag <> "a") $ \one ->
              withAccount (accountPrefix <> tag <> "b") $ \two ->
                withService service command $ \serviceSid ->
                  withChecksRoot root serviceSid $ do
                    BS.writeFile (checksConfig root) (TE.encodeUtf8 (T.pack (show (ServiceChecks root helper bash victim one two))))
                    runChecks service (checksLog root)
        exitWith code
  _ -> do
    putStrLn ("usage: " <> windowsSandboxInstallFlag <> " PARENT HELPER BASH SID PROFILE TEMP APPDATA PID USER-ONLY-FILE PROFILE-FILE")
    exitFailure

-- | Run an action with a slot's account made for it, with the batch
-- logon right and a member of the build accounts' group, and removed
-- after it, its right first, however the action ends, a failure to make
-- it included.
withAccount :: Text -> (SlotAccount -> IO a) -> IO a
withAccount name use = (makeAccount name >>= joined >>= use) `finally` removeAccount name
  where
    joined account = do
      withCWString buildersGroupName $ \group -> withCWString (T.unpack name) $ \member ->
        succeeded ("adding " <> T.unpack name <> " to the group " <> buildersGroupName) =<< c_group_add group member
      pure account

-- | The build accounts' local group, the one the store names
-- ("Nix.Store.Access"), which the checks make and every slot's account
-- is a member of.
buildersGroupName :: String
buildersGroupName = "nixbld"

-- | Run an action with the build accounts' group made for it and removed
-- after it however the action ends.  A group of that name already on the
-- machine fails the checks rather than being taken for theirs.
withBuildersGroup :: IO a -> IO a
withBuildersGroup action =
  withCWString buildersGroupName $ \wide -> do
    succeeded ("making the group " <> buildersGroupName) =<< c_group_make wide
    action `finally` (c_group_remove wide >>= reportRemoval)
  where
    reportRemoval code =
      unless (code == 0) (putStrLn ("  removing the group " <> buildersGroupName <> " failed with Win32 error " <> show code))

-- | The SID of the build accounts' group, which the administrator's run
-- made.
buildersGroupSid :: IO Sid
buildersGroupSid = do
  text <- withCWString buildersGroupName $ \wide -> wideResult (c_account_sid wide)
  maybe (ioError (userError ("the group " <> buildersGroupName <> " has no SID: " <> T.unpack text))) pure (sidFromText text)

makeAccount :: Text -> IO SlotAccount
makeAccount name =
  withCWString (T.unpack name) $ \wide -> alloca $ \passwordOut -> do
    succeededNaming "nova_test_account_make" (T.unpack name) (c_account_make wide passwordOut)
    SlotAccount name <$> (takeWide =<< peek passwordOut)

-- | Remove an account the checks used, saying why it could not be, where
-- it could not: CI's step after removes it by its name.
removeAccount :: Text -> IO ()
removeAccount name =
  withCWString (T.unpack name) $ \wide -> alloca $ \failedOut -> do
    code <- c_account_remove wide failedOut
    unless (code == 0) $ do
      failed <- peek failedOut
      call <- if failed == nullPtr then pure "nova_test_account_remove" else peekCString failed
      putStrLn ("  removing the account " <> T.unpack name <> ": " <> call <> " failed with Win32 error " <> show code)

-- | Run an action with the test service made for it, started by the
-- command line given and running as its virtual account, whose SID the
-- action is given, and removed after it, its account's rights first,
-- however the action ends, a failure to make it included.
withService :: Text -> String -> (Sid -> IO a) -> IO a
withService name command use = (installService name command >>= use) `finally` removeService name

installService :: Text -> String -> IO Sid
installService name command =
  withCWString (T.unpack name) $ \wideName -> withCWString command $ \wideCommand -> alloca $ \sidOut -> do
    succeededNaming "nova_test_service_install" (T.unpack name) (c_service_install wideName wideCommand sidOut)
    sidString <- takeWide =<< peek sidOut
    maybe (ioError (userError ("not a SID: " <> T.unpack sidString))) pure (sidFromText sidString)

-- | Remove the test service, saying why it could not be, where it could
-- not: CI's step after removes it by its name.
removeService :: Text -> IO ()
removeService name =
  withCWString (T.unpack name) $ \wide -> alloca $ \failedOut -> do
    code <- c_service_remove wide failedOut
    unless (code == 0) $ do
      failed <- peek failedOut
      call <- if failed == nullPtr then pure "nova_test_service_remove" else peekCString failed
      putStrLn ("  removing the service " <> T.unpack name <> ": " <> call <> " failed with Win32 error " <> show code)

-- | Run an action with the checks' directory made for it and removed
-- after it, saying why it could not be removed, where it could not.
withChecksRoot :: FilePath -> Sid -> IO a -> IO a
withChecksRoot root service action =
  (makeDirectory root (rootSddl service) *> action) `finally` removeRoot
  where
    removeRoot = do
      removed <- try (Dir.removePathForcibly root)
      case removed of
        Left (failure :: IOException) -> putStrLn ("  removing " <> root <> ": " <> show failure)
        Right () -> pure ()

-- | Start the test service, wait for it to stop, print the checks' log,
-- and answer whether every check passed: the service stopped within the
-- wait, with the checks' exit code 0, and the log ends with their count.
runChecks :: Text -> FilePath -> IO ExitCode
runChecks service logPath = do
  withCWString (T.unpack service) (succeeded "starting the test service" <=< c_service_start)
  stopped <- waitUntilWithin checksWaitMicros ((== serviceStopped) . fst <$> serviceState service)
  (_, code) <- serviceState service
  written <- try (BS.readFile logPath)
  let contents = either (\(_ :: IOException) -> "") id written
      counted = any isCount (BS8.lines contents)
  BS8.putStr contents
  unless stopped (putStrLn ("the test service had not stopped within " <> show checksWaitMinutes <> " minutes"))
  putStrLn ("the account checks ended with " <> show code)
  unless counted (putStrLn "the account checks did not finish: their log has no count")
  pure (if stopped && counted && code == 0 then ExitSuccess else ExitFailure 1)
  where
    isCount text = case BS8.words text of
      [flag, _, "passed"] -> flag == BS8.pack (windowsSandboxAccountsFlag <> ":")
      _ -> False

-- | How long the checks may take, in all.
checksWaitMinutes, checksWaitMicros :: Int
checksWaitMinutes = 20
checksWaitMicros = checksWaitMinutes * 60 * 1000000

-- | The test service's state and the code it stopped with.
serviceState :: Text -> IO (CULong, CULong)
serviceState service =
  withCWString (T.unpack service) $ \wide -> alloca $ \stateOut -> alloca $ \codeOut -> do
    succeeded "querying the test service" =<< c_service_state wide stateOut codeOut
    (,) <$> peek stateOut <*> peek codeOut

-- ---------------------------------------------------------------------------
-- The test service
-- ---------------------------------------------------------------------------

-- | The flag the test service is started with: its name, and the file
-- its checks are given in.
windowsSandboxServiceFlag :: String
windowsSandboxServiceFlag = "--windows-sandbox-service"

-- | Run as the test service until it stops, which runs the checks as a
-- process of its own and stops when they end.
windowsSandboxService :: [String] -> IO ()
windowsSandboxService args = case args of
  [name, config] -> do
    self <- getExecutablePath
    let checks = unwords [quoted self, windowsSandboxAccountsFlag, quoted config]
    code <- withCWString name $ \wideName -> withCWString checks (c_service_dispatch wideName)
    unless (code == 0) (exitWith (ExitFailure (fromIntegral code)))
  _ -> exitFailure

-- ---------------------------------------------------------------------------
-- The checks, as the test service
-- ---------------------------------------------------------------------------

-- | The flag the test service starts the checks with: the file they are
-- given in ('ServiceChecks').
windowsSandboxAccountsFlag :: String
windowsSandboxAccountsFlag = "--windows-sandbox-accounts"

-- | Run the checks as the test service, writing what the suite prints to
-- their log, and exit 0 only when every check passed.
windowsSandboxAccounts :: [String] -> IO ()
windowsSandboxAccounts args = case args of
  [config] -> do
    bytes <- BS.readFile config
    case either (const Nothing) (readMaybe . T.unpack) (TE.decodeUtf8' bytes) of
      Just checks ->
        withBinaryFile (checksLog (scRoot checks)) WriteMode $ \logHandle -> do
          redirectOutput logHandle
          results <- accountChecks checks
          let passed = length (filter id results)
          putStrLn (windowsSandboxAccountsFlag <> ": " <> show passed <> "/" <> show (length results) <> " passed")
          hFlush stdout
          when (passed /= length results) exitFailure
      Nothing -> exitFailure
  _ -> exitFailure

-- | Send this process's standard output and error to the log, line by
-- line, since no one reads the service's own.
redirectOutput :: Handle -> IO ()
redirectOutput logHandle = do
  hDuplicateTo logHandle stdout
  hDuplicateTo logHandle stderr
  hSetBuffering stdout LineBuffering

-- | A build slot: its account, the account's SID, its primary token,
-- which this process holds inheritable for the helpers it starts, and the
-- directory its builds start in.
data Slot = Slot
  { slotAccount :: !SlotAccount,
    slotSid :: !Sid,
    slotToken :: !(Ptr ()),
    slotBuildDir :: !FilePath
  }

-- | What every check runs with: the helper, the suite's copy every slot
-- may run as the builder, the directory the runs lay out, the service's
-- SID, Windows' directory, and how many runs have started, which names
-- the files some of them hold.
data Fixture = Fixture
  { fxHelper :: !FilePath,
    fxBuilder :: !FilePath,
    fxRoot :: !FilePath,
    fxService :: !Sid,
    fxSystemRoot :: !FilePath,
    fxRuns :: !(IORef Int)
  }

-- | The memory cap of a run whose builder needs room and no more.
ordinaryMemoryCap :: Word64
ordinaryMemoryCap = 1024 * mebibyte

-- | The memory check's cap, and a commit beneath it and past it, in
-- mebibytes.
cappedMemory :: Word64
cappedMemory = 256 * mebibyte

underCapMebibytes, overCapMebibytes :: Word64
underCapMebibytes = 64
overCapMebibytes = 1024

mebibyte :: Word64
mebibyte = 1024 * 1024

-- | The DACLs the checks' files are made with.  Each grants SYSTEM and
-- administrators full control, so the administrator's run can remove what
-- the checks leave.  The checks' directory grants the service full
-- control too, passing all three down, and Users read and execute on it
-- alone, so the slots reach their directories and the tools but not what
-- the checks are given; the tools grant the service full control and
-- Users read and execute, passing both down; and a slot's build directory
-- grants the service and that slot's account full control, passing both
-- down.
rootSddl, toolsSddl :: Sid -> Text
rootSddl service = "D:P(A;OICI;FA;;;SY)(A;OICI;FA;;;BA)(A;OICI;FA;;;" <> sidText service <> ")(A;;0x1200a9;;;BU)"
toolsSddl service = "D:P(A;OICI;FA;;;SY)(A;OICI;FA;;;BA)(A;OICI;FA;;;" <> sidText service <> ")(A;OICI;0x1200a9;;;BU)"

slotSddl :: Sid -> Sid -> Text
slotSddl service sid = "D:P(A;OICI;FA;;;SY)(A;OICI;FA;;;BA)(A;OICI;FA;;;" <> sidText service <> ")(A;OICI;FA;;;" <> sidText sid <> ")"

-- | The DACL of a builder's image that one account alone, beside SYSTEM
-- and administrators, may read and run.
readableBySddl :: Sid -> Text
readableBySddl sid = "D:P(A;;FA;;;SY)(A;;FA;;;BA)(A;;0x1200a9;;;" <> sidText sid <> ")"

accountChecks :: ServiceChecks -> IO [Bool]
accountChecks checks = do
  putStrLn "builder/sandbox-windows-accounts"
  systemRoot <- fromMaybe defaultSystemRoot <$> lookupEnv "SystemRoot"
  self <- getExecutablePath
  service <- ownSid
  runs <- newIORef 0
  let root = scRoot checks
      tools = root </> "tools"
      builder = tools </> takeFileName self
      fixture = Fixture (scHelper checks) builder root service systemRoot runs
  makeDirectory tools (toolsSddl service)
  Dir.copyFile self builder
  builders <- buildersGroupSid
  withSlot fixture (scSlotOne checks) $ \one ->
    withSlot fixture (scSlotTwo checks) $ \two ->
      (<>) <$> accountCheckList fixture checks one two <*> storeAccountChecks fixture builders one two

-- | Windows' directory where the environment does not say.
defaultSystemRoot :: FilePath
defaultSystemRoot = "C:\\Windows"

-- | The SID of the account this process runs as.
ownSid :: IO Sid
ownSid = do
  text <- wideResult c_token_user
  maybe (ioError (userError ("not a SID: " <> T.unpack text))) pure (sidFromText text)

-- | Make a directory with the DACL the SDDL describes.
makeDirectory :: FilePath -> Text -> IO ()
makeDirectory path sddl =
  withCWString path $ \wide -> withCWString (T.unpack sddl) (succeeded ("making " <> path) <=< c_directory_with_sddl wide)

-- | Give a file the DACL the SDDL describes, protected from its
-- directory's.
setSddl :: FilePath -> Text -> IO ()
setSddl path sddl =
  withCWString path $ \wide -> withCWString (T.unpack sddl) (succeeded ("setting the DACL of " <> path) <=< c_set_sddl wide)

-- | Run an action with a slot logged on, with a build directory of its
-- own; its token is closed after it.
withSlot :: Fixture -> SlotAccount -> (Slot -> IO a) -> IO a
withSlot fixture account use =
  withLogon account $ \(sid, token) -> do
    let buildDir = fxRoot fixture </> T.unpack (saName account)
    makeDirectory buildDir (slotSddl (fxService fixture) sid)
    use (Slot account sid token buildDir)

-- | Run an action with a slot's account logged on as the service logs it
-- on for a build, given its SID and its token, which is closed after it.
withLogon :: SlotAccount -> ((Sid, Ptr ()) -> IO a) -> IO a
withLogon account = bracket (logOn account) (void . c_close_handle . snd)

logOn :: SlotAccount -> IO (Sid, Ptr ())
logOn account =
  withCWString (T.unpack (saName account)) $ \wideName -> withCWString (T.unpack (saPassword account)) $ \widePassword ->
    alloca $ \sidOut -> alloca $ \tokenOut -> do
      succeeded ("logging " <> T.unpack (saName account) <> " on") =<< c_account_logon wideName widePassword sidOut tokenOut
      sidString <- takeWide =<< peek sidOut
      token <- peek tokenOut
      case sidFromText sidString of
        Just sid -> pure (sid, token)
        Nothing -> c_close_handle token *> ioError (userError ("not a SID: " <> T.unpack sidString))

-- | What a run of the helper came to: how it exited, what it wrote on its
-- status channel, and the build log.
data Run = Run
  { runExit :: !ExitCode,
    runStatus :: !BS.ByteString,
    runLog :: !BS.ByteString
  }

-- | A run's number, which names the files some runs hold.
nextRun :: Fixture -> IO Int
nextRun fixture = atomicModifyIORef' (fxRuns fixture) (\count -> (count + 1, count + 1))

-- | The plan of a run in a slot: the slot's build directory, token and
-- SID, and the environment a builder here needs, whose @PATH@ is
-- System32.
planFor :: Fixture -> Slot -> Sandbox.WindowsPlan
planFor fixture slot =
  Sandbox.WindowsPlan
    { Sandbox.windowsPlanBuildDir = pathBytes (slotBuildDir slot),
      Sandbox.windowsPlanToken = tokenValue (slotToken slot),
      Sandbox.windowsPlanSlot = slotSid slot,
      Sandbox.windowsPlanMemoryCap = ordinaryMemoryCap,
      Sandbox.windowsPlanEnvironment = environmentWith fixture slot [system32 fixture]
    }

-- | A token's handle as a plan names it.
tokenValue :: Ptr () -> Word32
tokenValue = fromIntegral . ptrToWordPtr

-- | A builder's environment: Windows' directory, a @PATH@ of the
-- directories given, and the slot's build directory as its temporary
-- directory.
environmentWith :: Fixture -> Slot -> [FilePath] -> [BS.ByteString]
environmentWith fixture slot path =
  map
    pathBytes
    [ "SystemRoot=" <> fxSystemRoot fixture,
      "PATH=" <> intercalate ";" path,
      "TEMP=" <> slotBuildDir slot,
      "TMP=" <> slotBuildDir slot
    ]

system32 :: Fixture -> FilePath
system32 fixture = fxSystemRoot fixture </> "System32"

-- | Windows PowerShell, which a builder starts a process through WMI
-- with.
powerShell :: FilePath -> FilePath
powerShell systemRoot = systemRoot </> "System32" </> "WindowsPowerShell" </> "v1.0" </> "powershell.exe"

-- | A path's or an entry's bytes, as a Windows plan holds them.
pathBytes :: String -> BS.ByteString
pathBytes = TE.encodeUtf8 . T.pack

-- | Run the helper over a command in a slot, the plan changed as given.
runCommand :: Fixture -> Slot -> (Sandbox.WindowsPlan -> Sandbox.WindowsPlan) -> [String] -> IO Run
runCommand fixture slot change command = do
  let plan = change (planFor fixture slot)
  (run, logBytes) <- capturingBuildLog (\logger -> Spawn.runHelper logger (fxHelper fixture) (Sandbox.renderWindowsPlan plan) (slotBuildDir slot) command)
  pure (Run (Spawn.hrExit run) (Spawn.hrStatus run) logBytes)

-- | Run an action with a logger that keeps each build line it is given,
-- and answer what the action answers with the build log those lines make,
-- each ended by its line feed again.  Messages go to this run's output.
capturingBuildLog :: (Logger -> IO a) -> IO (a, BS.ByteString)
capturingBuildLog action = do
  kept <- newIORef []
  let logger =
        Logger
          { logMessage = TIO.putStrLn,
            logBuildLine = \line -> atomicModifyIORef' kept (\earlier -> (line : earlier, ()))
          }
  result <- action logger
  delivered <- readIORef kept
  pure (result, BS.concat [line <> "\n" | line <- reverse delivered])

-- | Run the helper over the suite's builder, with an operation and its
-- arguments.
runBuilder :: Fixture -> Slot -> (Sandbox.WindowsPlan -> Sandbox.WindowsPlan) -> [String] -> IO Run
runBuilder fixture slot change operation = runCommand fixture slot change (builderCommand (fxBuilder fixture) operation)

builderCommand :: FilePath -> [String] -> [String]
builderCommand builder operation = builder : windowsSandboxBuilderFlag : operation

-- | The value a line of a run's log gives a name: the line @NAME VALUE@.
logged :: BS.ByteString -> Run -> Maybe BS.ByteString
logged name run = case mapMaybe (BS.stripPrefix (name <> " ")) (BS8.lines (runLog run)) of
  [value] -> Just value
  _ -> Nothing

-- | The result line a builder's attempt to reach something writes.
attempt :: Run -> Maybe CULong
attempt run = logged "result" run >>= readMaybe . BS8.unpack

-- | That a run's builder ran and ended as expected, its status channel
-- empty, with its log shown should it not have.
ranAs :: ExitCode -> Run -> TestResult
ranAs expected run =
  assertEqual ("exit and status, log " <> TE.decodeUtf8Lenient (runLog run)) (expected, "") (runExit run, runStatus run)

-- | A run's record on its status channel, and how it exited.
reported :: Run -> (ExitCode, Either Text (Maybe (Sandbox.StatusRecord Sandbox.WindowsStage)))
reported run = (runExit run, windowsRecord (runStatus run))

-- | That a run's builder ran on the window station of its slot's logon,
-- which the logon's ID names, and its default desktop, as the slot's
-- account and with a console.
ranOnLogonStation :: Slot -> Run -> TestResult
ranOnLogonStation slot run =
  ranAs ExitSuccess run
    `andThen` assertEqual "user" (Just (TE.encodeUtf8 (sidText (slotSid slot)))) (logged "user" run)
    `andThen` assertEqual "the logon's station" (True, logged "logon-station" run) (isJust (logged "station" run), logged "station" run)
    `andThen` assertEqual "desktop" (Just "Default") (logged "desktop" run)
    `andThen` assertEqual "console" (Just "0") (logged "console" run)

accountCheckList :: Fixture -> ServiceChecks -> Slot -> Slot -> IO [Bool]
accountCheckList fixture checks one two =
  sequence
    [ runTestM "the probe finds the privileges that start a build as another account, which the service holds" $ do
        (code, out, err) <- Proc.readCreateProcessWithExitCode ((Proc.proc (fxHelper fixture) ["probe"]) {Proc.env = Just []}) ""
        pure (assertEqual "probe" (ExitSuccess, "", "") (code, out, err)),
      runTestM "a build runs as its slot's account, on the window station of its logon, with a console, and each logon's station is its own" $ do
        first <- runBuilder fixture one id ["identity"]
        other <- runBuilder fixture two id ["identity"]
        again <- withLogon (slotAccount one) $ \(_, token) -> runBuilder fixture one (\plan -> plan {Sandbox.windowsPlanToken = tokenValue token}) ["identity"]
        let stations = traverse (logged "station") [first, other, again]
        pure $
          ranOnLogonStation one first
            `andThen` ranOnLogonStation two other
            `andThen` ranOnLogonStation one again
            `andThen` assertEqual "three logons, three stations" (Just 3) (Set.size . Set.fromList <$> stations),
      runTestM "a running build's window station opens by name for the build, and for no other slot's build" $ do
        number <- nextRun fixture
        let held = slotBuildDir one </> ("station-" <> show number)
            release = held <> ".release"
        withAsync (runBuilder fixture one id ["station-hold", held, release]) $ \build -> do
          started <- waitUntil (Dir.doesFileExist held)
          fields <- readFields held
          let field name = fields >>= lookup name
              dacl = maybe "unread" TE.decodeLatin1 (field "dacl")
          other <- traverse (\name -> runBuilder fixture two id ["open-station", BS8.unpack name]) (field "name")
          BS.writeFile release ""
          finished <- timeout startWaitMicros (wait build)
          pure $ case other of
            Just opened ->
              assertEqual ("opened by its own build, the station's DACL " <> dacl) (Just "0") (field "open")
                `andThen` ranAs ExitSuccess opened
                `andThen` assertEqual ("opened by another slot's build, which answered " <> T.pack (show (attempt opened)) <> ", the station's DACL " <> dacl) True (maybe False (/= 0) (attempt opened))
                `andThen` assertEqual "the build that held it" (Just (ExitSuccess, "")) (fmap (\run -> (runExit run, runStatus run)) finished)
            Nothing -> Fail ("the build wrote no station, started " <> T.pack (show started)),
      runTestM "a build's standard output and error are its log, and the helper exits as its builder does" $ do
        run <- runBuilder fixture one id ["echo", "7"]
        pure (ranAs (ExitFailure 7) run `andThen` assertEqual "log" "out\nerr\n" (runLog run)),
      runTestM "each process of a build may commit memory up to the plan's cap and no more, and the helper says so in the log" $ do
        let capped plan = plan {Sandbox.windowsPlanMemoryCap = cappedMemory}
        under <- runBuilder fixture one capped ["commit", show underCapMebibytes]
        over <- runBuilder fixture one capped ["commit", show overCapMebibytes]
        let limitLine = "reached its memory limit of " <> BS8.pack (show cappedMemory) <> " bytes"
        pure $
          ranAs ExitSuccess under
            `andThen` assertEqual "under the cap" (Just 0) (attempt under)
            `andThen` ranAs ExitSuccess over
            `andThen` assertEqual "past the cap, refused" True (maybe False (/= 0) (attempt over))
            `andThen` assertEqual "the helper's line" True (limitLine `BS.isInfixOf` runLog over),
      runTestM "a process a build starts shares its console, and none breaks away from the build's job" $ do
        shared <- runBuilder fixture one id ["spawn", "0"]
        breaking <- runBuilder fixture one id ["spawn", show breakawayFromJob]
        pure $
          ranAs ExitSuccess shared
            `andThen` assertEqual "sharing the console" (Just "0 0") (logged "spawn" shared)
            `andThen` ranAs ExitSuccess breaking
            `andThen` assertEqual "breaking away" (Just (BS8.pack (show accessDenied))) (BS8.takeWhile (/= ' ') <$> logged "spawn" breaking),
      runTestM "a crash ends its build at once with the crash's code" $ do
        run <- runBuilder fixture one id ["crash"]
        pure $ case runExit run of
          ExitFailure code -> assertEqual "code and status" (crashExceptionCode, "") (fromIntegral code, runStatus run)
          ExitSuccess -> Fail "the build ended with 0",
      runTestM "the helper waits for every process of its build, not its builder alone" $ do
        let release = slotBuildDir one </> "release"
            out = slotBuildDir one </> "lingered"
            pids = slotBuildDir one </> "pids"
        withAsync (runBuilder fixture one id ["leave", release, out, pids]) $ \build -> do
          started <- waitUntil (Dir.doesFileExist pids)
          written <- readPids pids
          case written of
            Just (builderPid, lingering) -> do
              builderGone <- waitUntil ((== 0) <$> c_process_running (fromIntegral builderPid))
              lingeringRuns <- (== 1) <$> c_process_running (fromIntegral lingering)
              stillWaiting <- isNothing <$> poll build
              BS.writeFile release ""
              finished <- timeout startWaitMicros (wait build)
              left <- BS.readFile out
              pure $
                assertEqual "the builder ended, and what it left runs while the helper waits" (True, True, True) (builderGone, lingeringRuns, stillWaiting)
                  `andThen` assertEqual "the helper's run" (Just (ExitSuccess, "")) (fmap (\run -> (runExit run, runStatus run)) finished)
                  `andThen` assertEqual "what the process it left wrote" "lingered\n" left
            Nothing -> pure (Fail ("the builder wrote no process IDs, started " <> T.pack (show started))),
      runTestM "the lifeline's end, or the helper's, ends every process of the build" $ do
        lifeline <- heldBuild fixture one (\planIn _ -> hClose planIn)
        terminated <- heldBuild fixture one (\_ process -> Proc.terminateProcess process)
        pure $
          assertEqual "the lifeline: started, helper ended, processes gone, exit" (True, True, True, Just (ExitFailure endedBuildExitCode)) lifeline
            `andThen` assertEqual "the helper terminated: started, helper ended, processes gone" (True, True, True) (dropExit terminated),
      runTestM "a step that fails is the one record on the status channel" $ do
        missing <- runCommand fixture one id [slotBuildDir one </> "absent.exe"]
        otherToken <- runBuilder fixture one (\plan -> plan {Sandbox.windowsPlanToken = tokenValue (slotToken two)}) ["echo", "0"]
        refused <- runBuilder fixture one (\plan -> plan {Sandbox.windowsPlanMemoryCap = 0}) ["echo", "0"]
        pure $
          assertEqual "a program that does not exist" (ExitFailure 1, Right (Just (Sandbox.StatusRecord Sandbox.WindowsStageExec fileNotFound Nothing))) (reported missing)
            `andThen` assertEqual "another slot's token" (ExitFailure 1, Right (Just (Sandbox.StatusRecord Sandbox.WindowsStageTokenUser 0 Nothing))) (reported otherToken)
            `andThen` assertEqual "a plan the helper refuses" (ExitFailure 1, Right (Just (Sandbox.StatusRecord Sandbox.WindowsStagePlan einval Nothing))) (reported refused),
      runTestM "the builder's image is opened as its slot: one the slot may not read is refused, and one only the slot may read runs" $ do
        let tools = takeDirectory (fxBuilder fixture)
            serviceOnly = tools </> "service-only.exe"
            slotOnly = tools </> "slot-only.exe"
        Dir.copyFile (fxBuilder fixture) serviceOnly
        setSddl serviceOnly (readableBySddl (fxService fixture))
        Dir.copyFile (fxBuilder fixture) slotOnly
        setSddl slotOnly (readableBySddl (slotSid one))
        refused <- runCommand fixture one id (builderCommand serviceOnly ["echo", "0"])
        ran <- runCommand fixture one id (builderCommand slotOnly ["echo", "0"])
        pure $
          assertEqual "an image the service alone may read" (ExitFailure 1, Right (Just (Sandbox.StatusRecord Sandbox.WindowsStageExec (fromIntegral accessDenied) Nothing))) (reported refused)
            `andThen` ranAs ExitSuccess ran
            `andThen` assertEqual "the log of the image the slot alone may read" "out\nerr\n" (runLog ran),
      runTestM "a process a build starts through WMI runs outside its job, the service may end every process of the slot's, and the build ends with them, its log with it" $ do
        number <- nextRun fixture
        let marker = slotBuildDir one </> ("escaped-" <> show number)
            release = marker <> ".release"
            withPowerShell plan = plan {Sandbox.windowsPlanEnvironment = powerShellEnvironment fixture one}
        withAsync (runBuilder fixture one withPowerShell ["escape", marker, release]) $ \build -> do
          appeared <- waitUntilWithin startWaitMicros (Dir.doesFileExist marker)
          reach <- accountProcesses (slotSid one)
          BS.writeFile release ""
          finished <- timeout startWaitMicros (wait build)
          written <- readEscaped marker
          case (finished, written) of
            (Just run, Just (pid, inJob, held)) -> do
              running <- c_process_running (fromIntegral pid)
              pure $
                ranAs ExitSuccess run
                  `andThen` assertEqual "WMI's start" (Just "0") (logged "wmi" run)
                  `andThen` assertEqual "outside the job, holding the build log" (0, 0) (inJob, held)
                  `andThen` assertEqual ("the slot's processes, each its program, ending it, waiting on it: " <> reach) True (all endable (T.splitOn ";" reach))
                  `andThen` assertEqual "running once the build ended" 0 running
            (Nothing, _) -> pure (Fail ("the build, or its log, had not ended within the wait; the slot's processes " <> reach))
            (Just run, Nothing) -> pure (Fail ("the process started through WMI wrote nothing, appeared " <> T.pack (show appeared) <> "; log " <> TE.decodeUtf8Lenient (runLog run))),
      runTestM "a process left running as a slot's account ends when the slot's next build starts" $ do
        pid <- startAs (slotToken two) (unwords (map quoted (builderCommand (fxBuilder fixture) ["wait"])))
        before <- c_process_running pid
        run <- runBuilder fixture two id ["echo", "0"]
        after <- c_process_running pid
        pure (assertEqual "running before the build" 1 before `andThen` ranAs ExitSuccess run `andThen` assertEqual "running after it" 0 after),
      runTestM "a build cannot reach the user's own: its TEMP, profile and APPDATA, a file only it may read, a profile file, its hive or its process" $ do
        let victim = scVictim checks
            attempts =
              [ ("create in TEMP", ["create", victimTemp victim </> buildMade]),
                ("list the profile", ["list", victimProfile victim </> "*"]),
                ("create in the profile", ["create", victimProfile victim </> buildMade]),
                ("create in APPDATA", ["create", victimAppData victim </> buildMade]),
                ("read a file only the user may", ["open", victimOnlyFile victim]),
                ("read a profile file", ["open", victimProfileFile victim]),
                ("delete a profile file", ["delete", victimProfileFile victim]),
                ("read the hive", ["key", T.unpack (victimSid victim) <> "\\Software"]),
                ("open the process", ["process", show (victimPid victim)])
              ]
        outcomes <- traverse (\(label, operation) -> (,) label <$> runBuilder fixture one id operation) attempts
        pure (foldr (andThen . refusedAttempt) Pass outcomes),
      runTestM "a build cannot reach another slot's build directory" $ do
        let made = slotBuildDir one </> "made"
        own <- runBuilder fixture one id ["create", made]
        outcomes <-
          traverse
            (\(label, operation) -> (,) label <$> runBuilder fixture two id operation)
            [ ("read the other slot's file", ["open", made]),
              ("create in the other slot's directory", ["create", slotBuildDir one </> "other"]),
              ("delete the other slot's file", ["delete", made])
            ]
        pure (ranAs ExitSuccess own `andThen` assertEqual "its own" (Just 0) (attempt own) `andThen` foldr (andThen . refusedAttempt) Pass outcomes),
      runTestM "MSYS2's bash runs a command under the pseudoconsole, and starts an MSYS2 program" $ do
        let bash = scBash checks
            withBash plan = plan {Sandbox.windowsPlanEnvironment = environmentWith fixture one [takeDirectory bash, system32 fixture]}
        run <- runCommand fixture one withBash [bash, "-c", "/usr/bin/true && echo ok"]
        pure (assertEqual "exit and status" (ExitSuccess, "") (runExit run, runStatus run) `andThen` assertEqual "ok in the log" True ("ok\n" `BS.isInfixOf` runLog run))
    ]
  where
    dropExit (started, ended, gone, _) = (started, ended, gone)
    -- The name of a file a build tries to make where it may not.
    buildMade = "nova-nix-sandbox-build"

-- | A builder's environment for Windows PowerShell: the ordinary one,
-- PowerShell's directory on its @PATH@ too, and the slot's build
-- directory as its profile and application data.
powerShellEnvironment :: Fixture -> Slot -> [BS.ByteString]
powerShellEnvironment fixture slot =
  environmentWith fixture slot [system32 fixture, takeDirectory (powerShell (fxSystemRoot fixture))]
    <> map
      pathBytes
      [ "USERPROFILE=" <> slotBuildDir slot,
        "APPDATA=" <> slotBuildDir slot,
        "LOCALAPPDATA=" <> slotBuildDir slot
      ]

-- ---------------------------------------------------------------------------
-- The store, as build accounts meet it
-- ---------------------------------------------------------------------------

-- | The store's layout as build accounts meet it, this process being the
-- service: a store directory made where the machine has the build
-- accounts' group, an output a slot makes in it, which this process
-- registers, and what each slot may do to it before and after.  The
-- store is made at the system drive's root, since the store's open
-- checks every directory on the way to it, and the runner's own are not
-- the store's to vouch for.
storeAccountChecks :: Fixture -> Sid -> Slot -> Slot -> IO [Bool]
storeAccountChecks fixture builders one two = do
  putStrLn "store/build-accounts"
  tag <- Proc.getCurrentPid
  let base = takeDrive (fxSystemRoot fixture) </> ("nova-nix-store-accounts-" <> show tag)
      store = base </> "store"
      output = store </> "output"
      file = output </> "file"
      locked = store </> "locked"
      lockedFile = locked </> "file"
      service = sidText (fxService fixture)
      sidOf slot = sidText (slotSid slot)
      -- Each entry of a DACL as DirDacl reads it, the SIDs given as text.
      allowing flags rights sid = DirDacl.DaclEntry 0 flags rights (Just sid)
      itself = 0x0
      -- OBJECT_INHERIT_ACE | CONTAINER_INHERIT_ACE.
      passedDown = 0x3
      -- INHERITED_ACE.
      inheritedFlag = 0x10
      carries label protected expected path = do
        dacl <- try (DirDacl.directoryDacl path)
        pure $ case dacl of
          Left (failure :: IOException) -> Fail (label <> ": " <> T.pack (show failure))
          Right found -> assertEqual label (Just (DirDacl.Dacl protected (sort expected))) (fmap (\reading -> reading {DirDacl.daclEntries = sort (DirDacl.daclEntries reading)}) found)
      ownerOf path = do
        owner <- try (DirDacl.entryOwner path)
        pure (either (\(failure :: IOException) -> Left (T.pack (show failure))) Right owner)
      -- The rights a build might use to change a node.
      changes =
        [ ("write its data", fileWriteData),
          ("append to it", fileAppendData),
          ("write its attributes", fileWriteAttributes),
          ("delete it", deleteRight),
          ("change its DACL", writeDac),
          ("change its owner", writeOwner)
        ]
      refusedOpens slot path = traverse (\(label, access) -> (,) (label <> " as " <> sidOf slot) <$> runBuilder fixture slot id ["open-for", show access, path])
      refusedChanges slot path = refusedOpens slot path changes
      -- What the group's grant leaves out on the store's directory: the
      -- two rights it holds, FILE_ADD_FILE and FILE_ADD_SUBDIRECTORY, are
      -- the first two the list names on a file.
      storeChanges = drop 2 changes
      allRefused = foldr (andThen . refusedAttempt) Pass
  results <-
    sequence
      [ runTestM "where the machine has the build accounts' group, a store's directory grants its entries to itself alone, and the group the right to create entries alone" $ do
          createStoreDirectory store
          carries
            "the store's DACL"
            True
            [ allowing itself fileAllAccess service,
              allowing itself fileAllAccess localSystemSid,
              allowing itself fileAllAccess administratorsSid,
              allowing itself readAndExecute usersSid,
              allowing itself createOnly (sidText builders)
            ]
            store,
        -- The group's entry is the only one that lets a slot create in the
        -- store's directory, so a slot making its output there is the group
        -- entry standing for the account's.
        runTestM "a build account makes its output in the store's directory through the group's entry, and what it makes takes its token's own DACL" $ do
          made <- runBuilder fixture one id ["mkdir", output]
          filled <- runBuilder fixture one id ["create", file]
          aside <- refusedOpens one store storeChanges
          owner <- ownerOf output
          dacl <- try (DirDacl.directoryDacl output)
          let entries = either (\(_ :: IOException) -> []) (maybe [] DirDacl.daclEntries) dacl
          pure $
            ranAs ExitSuccess made
              `andThen` assertEqual "the output made" (Just 0) (attempt made)
              `andThen` assertEqual "a file made in it" (Just 0) (attempt filled)
              `andThen` assertEqual "its owner" (Right (Just (sidOf one))) owner
              `andThen` assertEqual "an entry it took from the store's directory" [] (filter ((/= 0) . (.&. inheritedFlag) . DirDacl.deFlags) entries)
              `andThen` assertEqual "an entry for the group" [] (filter ((== Just (sidText builders)) . DirDacl.deSid) entries)
              `andThen` allRefused aside,
        runTestM "another build account cannot reach an output while it is built" $ do
          outcomes <-
            traverse
              (\(label, operation) -> (,) label <$> runBuilder fixture two id operation)
              [ ("read the other slot's output", ["open", file]),
                ("create in the other slot's output", ["create", output </> "added"]),
                ("delete the other slot's file", ["delete", file])
              ]
          pure (allRefused outcomes),
        runTestM "registering an output makes each node this process's, as the service's, with a valid path's DACL, and keeps its NAR hash" $ do
          before <- ExecBit.narHashOfPath processCaseHack output
          canonicalizeMetadata output
          after <- ExecBit.narHashOfPath processCaseHack output
          owners <- traverse ownerOf [output, file]
          -- A directory's entries are passed down; a file's carry no
          -- inheritance flags.
          let valid flags =
                [ allowing flags fileAllAccess service,
                  allowing flags fileAllAccess localSystemSid,
                  allowing flags fileAllAccess administratorsSid,
                  allowing flags readAndExecute usersSid,
                  allowing flags readAndExecute (sidText builders),
                  allowing flags readControl ownerRightsSid
                ]
          outputDacl <- carries "the output's DACL" True (valid passedDown) output
          fileDacl <- carries "its file's DACL" True (valid itself) file
          pure $
            assertEqual "the NAR hash" before after
              `andThen` assertEqual "the owners" [Right (Just service), Right (Just service)] owners
              `andThen` outputDacl
              `andThen` fileDacl,
        runTestM "a registered output is kept from every build, the one that made it included, and each may read it" $ do
          changed <- concat <$> traverse (\slot -> (<>) <$> refusedChanges slot file <*> refusedChanges slot output) [one, two]
          others <-
            traverse
              (\(label, slot, operation) -> (,) label <$> runBuilder fixture slot id operation)
              [ ("create in it as the slot that made it", one, ["create", output </> "added"]),
                ("create in it as another slot", two, ["create", output </> "added"]),
                ("delete its file as the slot that made it", one, ["delete", file])
              ]
          readings <- traverse (\slot -> runBuilder fixture slot id ["open", file]) [one, two]
          pure (allRefused (changed <> others) `andThen` assertEqual "reading it" [Just 0, Just 0] (map attempt readings)),
        -- Whether the service can take such an output, holding the
        -- privileges it is given and no others, is what this measures, so
        -- the log says which it did: made the output its own, or failed,
        -- naming the node it could not reach.
        runTestM "registering an output whose DACL a build set to grant it alone makes it this process's, or fails naming a node of it" $ do
          steps <- traverse (runBuilder fixture one id) [["mkdir", locked], ["create", lockedFile], ["lock", lockedFile], ["lock", locked]]
          registered <- try (canonicalizeMetadata locked)
          outcome <- case registered of
            Right () -> do
              putStrLn "  NOTE  registering an output that grants this process nothing made it this process's"
              owner <- ownerOf locked
              pure (assertEqual "its owner" (Right (Just service)) owner)
            Left err -> do
              putStrLn ("  NOTE  registering an output that grants this process nothing failed: " <> show (err :: IOException))
              pure (assertEqual "the failure names a node of the output" True (maybe False (locked `isPrefixOf`) (ioeGetFileName err)))
          pure (assertEqual "the build's steps" (replicate 4 (Just 0)) (map attempt steps) `andThen` outcome)
      ]
  -- What the slot locked is the slot's to remove, where registering it did
  -- not make it this process's.
  _ <- runBuilder fixture one id ["remove-tree", locked]
  removed <- try (Dir.removePathForcibly base)
  either (\err -> putStrLn ("  removing " <> base <> ": " <> show (err :: IOException))) pure removed
  pure results

-- | The rights a node's DACL grants (winnt.h): @FILE_ALL_ACCESS@,
-- @FILE_GENERIC_READ | FILE_GENERIC_EXECUTE@, the build accounts' group's
-- create-only grant on the store's directory, and @READ_CONTROL@.
fileAllAccess, readAndExecute, createOnly, readControl :: Word32
fileAllAccess = 0x1F01FF
readAndExecute = 0x1200A9
createOnly = 0x1000A6
readControl = 0x20000

-- | The rights a build could change a node through (winnt.h):
-- @FILE_WRITE_DATA@, @FILE_APPEND_DATA@, @FILE_WRITE_ATTRIBUTES@,
-- @DELETE@, @WRITE_DAC@ and @WRITE_OWNER@.
fileWriteData, fileAppendData, fileWriteAttributes, deleteRight, writeDac, writeOwner :: CULong
fileWriteData = 0x2
fileAppendData = 0x4
fileWriteAttributes = 0x100
deleteRight = 0x10000
writeDac = 0x40000
writeOwner = 0x80000

-- | The well-known SIDs a store's DACLs name beside SYSTEM's:
-- Administrators, Users and OWNER RIGHTS.
administratorsSid, usersSid, ownerRightsSid :: Text
administratorsSid = "S-1-5-32-544"
usersSid = "S-1-5-32-545"
ownerRightsSid = "S-1-3-4"

-- | That a builder's attempt was refused access, labelled.
refusedAttempt :: (Text, Run) -> TestResult
refusedAttempt (label, run) =
  ranAs ExitSuccess run `andThen` assertEqual label (Just accessDenied) (attempt run)

-- | Start a command line as the account whose token is given, outside
-- any job, as a process a build left running is: its process ID.
startAs :: Ptr () -> String -> IO CULong
startAs token command =
  withCWString command $ \wide -> alloca $ \pidOut -> do
    succeeded "starting a process as a slot's account" =<< c_start_as token wide pidOut
    peek pidOut

-- | The words a file holds, once it appears whole.
readWords :: FilePath -> IO (Maybe [BS.ByteString])
readWords path = do
  contents <- try (BS.readFile path)
  pure $ case contents of
    Left (_ :: IOException) -> Nothing
    Right bytes -> Just (BS8.words bytes)

-- | The process IDs a builder wrote: its own and the one it started.
readPids :: FilePath -> IO (Maybe (Word32, Word32))
readPids path = do
  written <- readWords path
  pure $ case traverse (readMaybe . BS8.unpack) =<< written of
    Just [builderPid, started] -> Just (builderPid, started)
    _ -> Nothing

-- | The lines of a file a builder wrote whole, each a name and a value.
readFields :: FilePath -> IO (Maybe [(BS.ByteString, BS.ByteString)])
readFields path = do
  contents <- try (BS.readFile path)
  pure $ case contents of
    Left (_ :: IOException) -> Nothing
    Right bytes -> traverse field (BS8.lines bytes)
  where
    field text = case BS8.words text of
      [name, value] -> Just (name, value)
      _ -> Nothing

-- | What the test service may do to each process running as an account,
-- as @nova_test_account_processes@ lists it.
accountProcesses :: Sid -> IO Text
accountProcesses sid =
  withCWString (T.unpack (sidText sid)) $ \wide -> allocaArray processListChars $ \text -> alloca $ \countOut -> do
    succeeded "listing an account's processes" =<< c_account_processes wide text (fromIntegral processListChars) countOut
    T.pack <$> peekCWString text

-- | The room a listing of an account's processes is given.
processListChars :: Int
processListChars = 4096

-- | Whether an entry of 'accountProcesses' says the process may be ended;
-- the empty text after the last entry's semicolon is no entry.
endable :: Text -> Bool
endable entry = case T.splitOn ":" entry of
  [_, terminate, _] -> terminate == "0"
  [""] -> True
  _ -> False

-- | What a process a build started through WMI wrote: its ID, whether it
-- is in a job, and what copying the build log answered.
readEscaped :: FilePath -> IO (Maybe (Word32, CInt, CULong))
readEscaped path = do
  written <- readWords path
  pure $ case written of
    Just [pidText, inJobText, heldText] -> (,,) <$> readMaybe (BS8.unpack pidText) <*> readMaybe (BS8.unpack inJobText) <*> readMaybe (BS8.unpack heldText)
    _ -> Nothing

-- | Start the helper over a builder that starts a process and holds the
-- lifeline, end the build as told once both run, and answer whether they
-- started, the helper ended within the wait, both processes are gone,
-- and how the helper exited.
heldBuild :: Fixture -> Slot -> (Handle -> Proc.ProcessHandle -> IO ()) -> IO (Bool, Bool, Bool, Maybe ExitCode)
heldBuild fixture slot stop = do
  number <- nextRun fixture
  let pids = slotBuildDir slot </> ("held-" <> show number)
      logPath = fxRoot fixture </> ("run-" <> show number <> ".log")
      plan = planFor fixture slot
      spec =
        (Proc.proc (fxHelper fixture) ("run" : builderCommand (fxBuilder fixture) ["hold", pids]))
          { Proc.cwd = Just (slotBuildDir slot),
            Proc.env = Just [],
            Proc.std_in = Proc.CreatePipe,
            Proc.std_out = Proc.CreatePipe
          }
  withBinaryFile logPath WriteMode $ \logHandle -> do
    spawned <- Proc.createProcess spec {Proc.std_err = Proc.UseHandle logHandle}
    case spawned of
      (Just planIn, Just statusOut, _, process) -> do
        BS.hPut planIn (Sandbox.renderWindowsPlan plan) *> hFlush planIn
        started <- waitUntil (Dir.doesFileExist pids)
        written <- readPids pids
        let running = maybe [] (\(builderPid, held) -> [builderPid, held]) written
        stop planIn process
        ended <- timeout processWaitMicros (Proc.waitForProcess process)
        when (isNothing ended) (Proc.terminateProcess process *> void (Proc.waitForProcess process))
        gone <- waitUntil (all (== 0) <$> traverse (c_process_running . fromIntegral) running)
        -- A pipe whose reader or writer is gone can fail its close.
        traverse_ (\handle -> void (try (hClose handle) :: IO (Either IOException ()))) [planIn, statusOut]
        pure (started && isJust written, isJust ended, gone && not (null running), ended)
      _ -> pure (False, False, False, Nothing)

-- ---------------------------------------------------------------------------
-- The builder
-- ---------------------------------------------------------------------------

-- | The flag that makes the suite a builder the helper runs as a slot's
-- account, given an operation and its arguments.
windowsSandboxBuilderFlag :: String
windowsSandboxBuilderFlag = "--windows-sandbox-builder"

-- | How long a process a build starts through WMI holds what it copied
-- from the build: past the check's wait for the build to end, so that the
-- build ends within it only when the process is ended.
escapedHoldMicros :: Int
escapedHoldMicros = 2 * startWaitMicros

-- | Carry out one operation as a builder, writing what it finds to
-- standard output, the build log, as lines of a name and a value.
-- Output is written as bytes, which no newline translation touches.
windowsSandboxBuilder :: [String] -> IO ()
windowsSandboxBuilder operation = case operation of
  ["echo", codeText] | Just code <- readMaybe codeText -> do
    BS8.hPutStr stdout "out\n" *> hFlush stdout
    BS8.hPutStr stderr "err\n"
    exitWith (if code == 0 then ExitSuccess else ExitFailure code)
  ["identity"] -> do
    user <- wideResult c_token_user
    station <- userObjectName 0
    desktop <- userObjectName 1
    console <- c_console
    logonStation <- logonStationName
    traverse_ logLine [("user", user), ("station", station), ("desktop", desktop), ("console", T.pack (show console)), ("logon-station", logonStation)]
  ["station-hold", held, release] -> do
    station <- userObjectName 0
    own <- withCWString (T.unpack station) c_open_station
    dacl <- withCWString (T.unpack station) (wideResult . c_station_dacl)
    writeWhole held (BS8.unlines [field "name" station, field "open" (T.pack (show own)), field "dacl" dacl])
    released <- waitUntilWithin startWaitMicros (Dir.doesFileExist release)
    unless released exitFailure
  ["open-station", name] -> attemptOn name c_open_station
  ["commit", mebibytesText] | Just mebibytes <- readMaybe mebibytesText -> do
    code <- c_commit (fromIntegral (mebibytes * mebibyte))
    logLine ("result", T.pack (show code))
  ["spawn", flagsText] | Just flags <- readMaybe flagsText -> do
    self <- getExecutablePath
    (code, process) <- start (quoted self <> " " <> windowsSandboxBuilderFlag <> " console") flags
    exitCode <- if code == 0 then waitFor process else pure 0
    logLine ("spawn", T.pack (show code <> " " <> show exitCode))
  ["console"] -> do
    code <- c_console
    exitWith (if code == 0 then ExitSuccess else ExitFailure (fromIntegral code))
  ["crash"] -> c_crash
  ["open", path] -> attemptOn path (`c_open` genericRead)
  ["open-for", accessText, path] | Just access <- readMaybe accessText -> attemptOn path (`c_open` access)
  ["create", path] -> attemptOn path c_create
  ["mkdir", path] -> attemptOn path c_mkdir
  ["lock", path] -> attemptOn path c_lock
  ["remove-tree", path] -> do
    removed <- try (Dir.removePathForcibly path)
    logLine ("result", either (\(failure :: IOException) -> T.pack (show failure)) (const "0") removed)
  ["delete", path] -> attemptOn path c_delete
  ["list", wildcard] -> attemptOn wildcard c_list
  ["key", path] -> attemptOn path c_open_key
  ["process", pidText] | Just pid <- readMaybe pidText -> c_open_process pid >>= \code -> logLine ("result", T.pack (show code))
  ["escape", marker, release] -> escapeThroughWmi marker release
  ["escaped", marker, builderText, valueText]
    | Just builderPid <- readMaybe builderText,
      Just value <- readMaybe valueText -> do
        held <- c_hold_handle builderPid value
        inJob <- c_in_job
        own <- Proc.getCurrentPid
        writeWhole marker (BS8.pack (unwords [show own, show inJob, show held]))
        threadDelay escapedHoldMicros
  ["hold", pids] -> do
    held <- startSelf ["wait"]
    writePids pids held
    threadDelay startWaitMicros
  ["leave", release, out, pids] -> do
    held <- startSelf ["linger", release, out]
    writePids pids held
  ["linger", release, out] -> do
    released <- waitUntilWithin startWaitMicros (Dir.doesFileExist release)
    when released (BS.writeFile out "lingered\n")
  ["wait"] -> threadDelay startWaitMicros
  _ -> do
    BS8.hPutStr stderr ("unknown builder operation: " <> BS8.pack (show operation) <> "\n")
    exitFailure
  where
    attemptOn path call = withCWString path call >>= \code -> logLine ("result", T.pack (show code))
    -- A line of a name and a value, the value's spaces made dashes so
    -- that it stays one word.
    field name value = name <> " " <> TE.encodeUtf8 (T.replace " " "-" value)

-- | Write a line of a name and a value to the build log.
logLine :: (BS.ByteString, Text) -> IO ()
logLine (name, value) = BS8.hPutStr stdout (name <> " " <> TE.encodeUtf8 value <> "\n")

-- | The name of this process's window station, for 0, or of its thread's
-- desktop, or the error that kept it from being read.
userObjectName :: CInt -> IO Text
userObjectName which =
  allocaArray objectNameChars $ \name -> do
    code <- c_user_object_name which name (fromIntegral objectNameChars)
    if code == 0 then T.pack <$> peekCWString name else pure ("error " <> T.pack (show code))

-- | The name of the window station Windows makes for this process's
-- logon: @Service-0x@, the logon's ID's high and low parts in hex, and a
-- dollar sign.
logonStationName :: IO Text
logonStationName =
  alloca $ \highOut -> alloca $ \lowOut -> do
    code <- c_logon_id highOut lowOut
    high <- peek highOut
    low <- peek lowOut
    pure $
      if code == 0
        then T.pack ("Service-0x" <> showHex high "" <> "-" <> showHex low "" <> "$")
        else "error " <> T.pack (show code)

-- | Have WMI start this program for the build, as a process that copies
-- the build log from this one and says in MARKER whether it is in a job,
-- then wait for that, write what WMI's start answered, and wait for
-- RELEASE before ending.
escapeThroughWmi :: FilePath -> FilePath -> IO ()
escapeThroughWmi marker release = do
  self <- getExecutablePath
  builderPid <- Proc.getCurrentPid
  value <- c_stdout_value
  systemRoot <- fromMaybe defaultSystemRoot <$> lookupEnv "SystemRoot"
  let escaped = unwords [quoted self, windowsSandboxBuilderFlag, "escaped", quoted marker, show builderPid, show value]
      script = "$r = Invoke-CimMethod -ClassName Win32_Process -MethodName Create -Arguments @{CommandLine='" <> T.pack escaped <> "'}; exit [int]$r.ReturnValue"
      encoded = T.unpack (B64.encode (TE.encodeUtf16LE script))
  (code, _, err) <- Proc.readCreateProcessWithExitCode (Proc.proc (powerShell systemRoot) ["-NoProfile", "-NonInteractive", "-EncodedCommand", encoded]) ""
  logLine ("wmi", T.pack (exitNumber code))
  unless (code == ExitSuccess) (putStr err)
  written <- waitUntilWithin startWaitMicros (Dir.doesFileExist marker)
  released <- waitUntilWithin startWaitMicros (Dir.doesFileExist release)
  unless (written && released) exitFailure
  where
    exitNumber code = case code of
      ExitSuccess -> "0"
      ExitFailure number -> show number

-- | A word of a command line, quoted whole.
quoted :: String -> String
quoted word = "\"" <> word <> "\""

-- | The room a window station's or desktop's name is given.
objectNameChars :: Int
objectNameChars = 256

-- | A wide string a call writes for LocalFree, or the call's error.
wideResult :: (Ptr CWString -> IO CULong) -> IO Text
wideResult call =
  alloca $ \out -> do
    code <- call out
    if code == 0 then takeWide =<< peek out else pure ("error " <> T.pack (show code))

-- | Start a command line with CreateProcessW's flags, inheriting no
-- handle: the call's error, and the process, for 'waitFor'.
start :: String -> CULong -> IO (CULong, (CULong, Ptr ()))
start command flags =
  withCWString command $ \wide -> alloca $ \pidOut -> alloca $ \processOut -> do
    code <- c_start wide flags pidOut processOut
    pid <- peek pidOut
    process <- peek processOut
    pure (code, (pid, process))

-- | Wait for a process 'start' started, answering its exit code.
waitFor :: (CULong, Ptr ()) -> IO CULong
waitFor (_, process) =
  alloca $ \codeOut -> do
    succeeded "waiting for a process" =<< c_wait process codeOut
    peek codeOut

-- | Start this program as a builder with an operation, not waiting for it,
-- answering its process ID.  Each word is quoted whole, which holds for
-- the program's path, the flag and the paths an operation takes, none
-- holding a quote or ending in a backslash.
startSelf :: [String] -> IO CULong
startSelf operation = do
  self <- getExecutablePath
  (code, (pid, process)) <- start (unwords (map quoted (builderCommand self operation))) 0
  succeeded "starting a process" code
  succeeded "closing a process's handle" =<< c_close_handle process
  pure pid

-- | Write this process's ID and another's to a file, whole.
writePids :: FilePath -> CULong -> IO ()
writePids path other = do
  own <- Proc.getCurrentPid
  writeWhole path (BS8.pack (show own <> " " <> show other))

-- | Write a file whole: it appears by a rename once it holds every byte.
writeWhole :: FilePath -> BS.ByteString -> IO ()
writeWhole path bytes = do
  let partial = path <> ".partial"
  BS.writeFile partial bytes
  Dir.renameFile partial path
