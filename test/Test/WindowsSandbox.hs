{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | The sandbox helper's Windows backend (@sandbox\/windows.c@), run as
-- the service that holds each build slot's token runs it.
--
-- Most of its checks need SYSTEM, which holds the privileges that start
-- a process as another account.  CI's Windows build runs them in a child
-- of the suite that a scheduled task starts as SYSTEM
-- ('windowsSandboxAccounts'), given the runner's user, whose files, hive
-- and processes a build must not reach.  That child makes two slot
-- accounts, logs each on as a batch job for its token, lays a directory
-- out for each slot's builds, and runs the helper over the plans
-- 'Sandbox.renderWindowsPlan' writes, with the suite itself as the
-- builder ('windowsSandboxBuilder'); it removes the accounts, each one's
-- logon right first, however the checks end.  The suite's own run, as a
-- user without those privileges, checks that the probe and a run report
-- the step that needs them ('testWindowsSandboxIO').
module Test.WindowsSandbox
  ( testWindowsSandboxIO,
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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Either (fromRight)
import Data.Foldable (traverse_)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (intercalate, sort)
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Word (Word32, Word64)
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
import Nix.Store.Access (Sid, sidFromText, sidText)
import qualified System.Directory as Dir
import System.Environment (getExecutablePath, lookupEnv)
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO (BufferMode (..), Handle, IOMode (..), hClose, hFlush, hSetBuffering, stderr, stdout, withBinaryFile)
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
  c_account_make :: CWString -> Ptr CWString -> Ptr (Ptr ()) -> Ptr CString -> IO CULong

foreign import ccall unsafe "nova_test_account_remove"
  c_account_remove :: CWString -> Ptr CString -> IO CULong

foreign import ccall unsafe "nova_test_close_handle"
  c_close_handle :: Ptr () -> IO CULong

foreign import ccall unsafe "nova_test_local_free"
  c_local_free :: CWString -> IO ()

foreign import ccall unsafe "nova_test_station_hold"
  c_station_hold :: CWString -> Ptr (Ptr ()) -> IO CULong

foreign import ccall unsafe "nova_test_station_release"
  c_station_release :: Ptr () -> IO CULong

foreign import ccall unsafe "nova_test_directory_with_sddl"
  c_directory_with_sddl :: CWString -> CWString -> IO CULong

foreign import ccall unsafe "nova_test_process_running"
  c_process_running :: CULong -> IO CInt

foreign import ccall unsafe "nova_test_token_user"
  c_token_user :: Ptr CWString -> IO CULong

foreign import ccall unsafe "nova_test_user_object_name"
  c_user_object_name :: CInt -> CWString -> CULong -> IO CULong

foreign import ccall unsafe "nova_test_user_object_dacl"
  c_user_object_dacl :: CInt -> Ptr CWString -> IO CULong

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
  let plan = Sandbox.WindowsPlan (pathBytes scratch) unreachedToken system "nn-unprivileged\\build" ordinaryMemoryCap []
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
-- The run as SYSTEM
-- ---------------------------------------------------------------------------

-- | The flag that makes the suite the child CI's scheduled task starts as
-- SYSTEM: the log it writes, the helper, MSYS2's bash, and the runner's
-- user, whose own a build must not reach: its SID, its profile, TEMP and
-- APPDATA directories, one of its processes, a file only it may read and
-- a file in its profile.
windowsSandboxAccountsFlag :: String
windowsSandboxAccountsFlag = "--windows-sandbox-accounts"

-- | The runner's user, whose own a build must not reach.
data Victim = Victim
  { victimSid :: !Text,
    victimProfile :: !FilePath,
    victimTemp :: !FilePath,
    victimAppData :: !FilePath,
    victimPid :: !Word32,
    victimOnlyFile :: !FilePath,
    victimProfileFile :: !FilePath
  }

-- | Run the checks as SYSTEM, writing what the suite prints to the log,
-- and exit 0 only when every check passed.
windowsSandboxAccounts :: [String] -> IO ()
windowsSandboxAccounts args = case args of
  [logPath, helper, bash, sid, profile, temp, appData, pidText, onlyFile, profileFile]
    | Just pid <- readMaybe pidText ->
        withBinaryFile logPath WriteMode $ \logHandle -> do
          redirectOutput logHandle
          results <- accountChecks helper bash (Victim (T.pack sid) profile temp appData pid onlyFile profileFile)
          let passed = length (filter id results)
          putStrLn (windowsSandboxAccountsFlag <> ": " <> show passed <> "/" <> show (length results) <> " passed")
          hFlush stdout
          when (passed /= length results) exitFailure
  _ -> do
    putStrLn ("usage: " <> windowsSandboxAccountsFlag <> " LOG HELPER BASH SID PROFILE TEMP APPDATA PID USER-ONLY-FILE PROFILE-FILE")
    exitFailure

-- | Send this process's standard output and error to the log, line by
-- line, since no one reads the task's own.
redirectOutput :: Handle -> IO ()
redirectOutput logHandle = do
  hDuplicateTo logHandle stdout
  hDuplicateTo logHandle stderr
  hSetBuffering stdout LineBuffering

-- | A build slot: its account's SID, the account's primary token, which
-- this process holds inheritable for the helpers it starts, and the
-- directory its builds start in.
data Slot = Slot
  { slotSid :: !Sid,
    slotToken :: !(Ptr ()),
    slotBuildDir :: !FilePath
  }

-- | What every check runs with: the helper, the suite's copy every slot
-- may run as the builder, the directory the runs lay out, Windows'
-- directory, and how many runs have started, which names each one's
-- window station and log.
data Fixture = Fixture
  { fxHelper :: !FilePath,
    fxBuilder :: !FilePath,
    fxRoot :: !FilePath,
    -- | This process's ID, which names its window stations.
    fxTag :: !String,
    fxSystemRoot :: !FilePath,
    fxRuns :: !(IORef Int)
  }

-- | The name every account the checks make starts with, which CI's step
-- that removes what a run left behind matches.
accountPrefix :: Text
accountPrefix = "nnslot"

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

-- | The DACLs the layout's directories are made with: the root, which
-- SYSTEM may change and Users read, each passing nothing down; the tools,
-- which Users may read and run; and a slot's build directory, which
-- SYSTEM and that slot's account alone may use, both passing it down.
rootSddl, toolsSddl :: Text
rootSddl = "D:P(A;;FA;;;SY)(A;;0x1200a9;;;BU)"
toolsSddl = "D:P(A;OICI;FA;;;SY)(A;OICI;0x1200a9;;;BU)"

slotSddl :: Sid -> Text
slotSddl sid = "D:P(A;OICI;FA;;;SY)(A;OICI;FA;;;" <> sidText sid <> ")"

accountChecks :: FilePath -> FilePath -> Victim -> IO [Bool]
accountChecks helper bash victim = do
  putStrLn "builder/sandbox-windows-accounts"
  tmpBase <- Dir.getTemporaryDirectory
  pid <- Proc.getCurrentPid
  systemRoot <- fromMaybe defaultSystemRoot <$> lookupEnv "SystemRoot"
  self <- getExecutablePath
  runs <- newIORef 0
  let tag = show pid
      root = tmpBase </> ("nova-nix-sandbox-windows-" <> tag)
      tools = root </> "tools"
      builder = tools </> takeFileName self
      fixture = Fixture helper builder root tag systemRoot runs
  ( do
      makeDirectory root rootSddl
      makeDirectory tools toolsSddl
      Dir.copyFile self builder
      withSlot root (accountPrefix <> T.pack tag <> "a") $ \one ->
        withSlot root (accountPrefix <> T.pack tag <> "b") $ \two ->
          accountCheckList fixture bash victim one two
    )
    `finally` Dir.removePathForcibly root

-- | Windows' directory where the environment does not say.
defaultSystemRoot :: FilePath
defaultSystemRoot = "C:\\Windows"

-- | Make a directory with the DACL the SDDL describes.
makeDirectory :: FilePath -> Text -> IO ()
makeDirectory path sddl =
  withCWString path $ \wide -> withCWString (T.unpack sddl) (succeeded ("making " <> path) <=< c_directory_with_sddl wide)

-- | Run an action with a slot whose account is made for it, with a build
-- directory of its own, and removed after it, its logon right first,
-- however the action ends, a failure to make it included; its token is
-- closed before.
withSlot :: FilePath -> Text -> (Slot -> IO a) -> IO a
withSlot root name use =
  bracket (makeAccount name) (void . c_close_handle . snd) inSlot `finally` removeAccount name
  where
    inSlot (sid, token) = do
      let buildDir = root </> T.unpack name
      makeDirectory buildDir (slotSddl sid)
      use (Slot sid token buildDir)

-- | Make a slot's account and log it on: its SID and its token.
makeAccount :: Text -> IO (Sid, Ptr ())
makeAccount name =
  withCWString (T.unpack name) $ \wide -> alloca $ \sidOut -> alloca $ \tokenOut -> alloca $ \failedOut -> do
    code <- c_account_make wide sidOut tokenOut failedOut
    failed <- peek failedOut
    call <- if failed == nullPtr then pure "nova_test_account_make" else peekCString failed
    succeeded (call <> " for " <> T.unpack name) code
    sidString <- takeWide =<< peek sidOut
    token <- peek tokenOut
    case sidFromText sidString of
      Just sid -> pure (sid, token)
      Nothing -> c_close_handle token *> ioError (userError ("not a SID: " <> T.unpack sidString))

-- | Remove an account the checks made, saying in the log why it could not
-- be, where it could not: CI's step after removes it by its name.
removeAccount :: Text -> IO ()
removeAccount name =
  withCWString (T.unpack name) $ \wide -> alloca $ \failedOut -> do
    code <- c_account_remove wide failedOut
    unless (code == 0) $ do
      failed <- peek failedOut
      call <- if failed == nullPtr then pure "nova_test_account_remove" else peekCString failed
      putStrLn ("  removing the account " <> T.unpack name <> ": " <> call <> " failed with Win32 error " <> show code)

-- | What a run of the helper came to: how it exited, what it wrote on its
-- status channel, and the build log.
data Run = Run
  { runExit :: !ExitCode,
    runStatus :: !BS.ByteString,
    runLog :: !BS.ByteString
  }

-- | A run's number, which names its window station and its log.
nextRun :: Fixture -> IO Int
nextRun fixture = atomicModifyIORef' (fxRuns fixture) (\count -> (count + 1, count + 1))

-- | The plan of a run in a slot, given its number: the slot's build
-- directory, token and SID, a window station named for the run, and the
-- environment a builder here needs, whose @PATH@ is System32.
planFor :: Fixture -> Slot -> Int -> Sandbox.WindowsPlan
planFor fixture slot number =
  Sandbox.WindowsPlan
    { Sandbox.windowsPlanBuildDir = pathBytes (slotBuildDir slot),
      Sandbox.windowsPlanToken = fromIntegral (ptrToWordPtr (slotToken slot)),
      Sandbox.windowsPlanSlot = slotSid slot,
      Sandbox.windowsPlanDesktop = BS8.pack (stationName fixture number) <> "\\build",
      Sandbox.windowsPlanMemoryCap = ordinaryMemoryCap,
      Sandbox.windowsPlanEnvironment = environmentWith fixture slot [system32 fixture]
    }

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

-- | The window station a run makes, named for this process and the run.
stationName :: Fixture -> Int -> String
stationName fixture number = "nn-sandbox-" <> fxTag fixture <> "-" <> show number

-- | A path's or an entry's bytes, as a Windows plan holds them.
pathBytes :: String -> BS.ByteString
pathBytes = TE.encodeUtf8 . T.pack

-- | Run the helper over a command in a slot, the plan changed as given.
runCommand :: Fixture -> Slot -> (Sandbox.WindowsPlan -> Sandbox.WindowsPlan) -> [String] -> IO Run
runCommand fixture slot change command = do
  number <- nextRun fixture
  let plan = change (planFor fixture slot number)
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
runBuilder fixture slot change operation = runCommand fixture slot change (builderCommand fixture operation)

builderCommand :: Fixture -> [String] -> [String]
builderCommand fixture operation = fxBuilder fixture : windowsSandboxBuilderFlag : operation

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

-- | The trustee of each entry of a DACL's SDDL, with its type, or nothing
-- for text that is not one.
daclEntries :: BS.ByteString -> Maybe [(BS.ByteString, BS.ByteString)]
daclEntries sddl = do
  body <- BS.stripPrefix "D:" sddl
  entries <- traverse entry (filter (not . BS.null) (BS8.split ')' body))
  pure (sort entries)
  where
    entry text = case BS8.split ';' text of
      [kind, _, _, _, _, trustee] -> (,trustee) <$> BS.stripPrefix "(" kind
      _ -> Nothing

accountCheckList :: Fixture -> FilePath -> Victim -> Slot -> Slot -> IO [Bool]
accountCheckList fixture bash victim one two =
  sequence
    [ runTestM "the probe finds the privileges that start a build as another account, which SYSTEM holds" $ do
        (code, out, err) <- Proc.readCreateProcessWithExitCode ((Proc.proc (fxHelper fixture) ["probe"]) {Proc.env = Just []}) ""
        pure (assertEqual "probe" (ExitSuccess, "", "") (code, out, err)),
      runTestM "a build runs as its slot's account, on a window station and desktop of its own that grant SYSTEM and the slot alone, attached to a console" $ do
        number <- (+ 1) <$> peekRuns fixture
        run <- runBuilder fixture one id ["identity"]
        let grantees = Just (sort [("A", "SY"), ("A", TE.encodeUtf8 (sidText (slotSid one)))])
        pure $
          ranAs ExitSuccess run
            `andThen` assertEqual "user" (Just (TE.encodeUtf8 (sidText (slotSid one)))) (logged "user" run)
            `andThen` assertEqual "station" (Just (BS8.pack (stationName fixture number))) (logged "station" run)
            `andThen` assertEqual "desktop" (Just "build") (logged "desktop" run)
            `andThen` assertEqual "console" (Just "0") (logged "console" run)
            `andThen` assertEqual "station's DACL" grantees (logged "station-dacl" run >>= daclEntries)
            `andThen` assertEqual "desktop's DACL" grantees (logged "desktop-dacl" run >>= daclEntries),
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
        otherToken <- runBuilder fixture one (\plan -> plan {Sandbox.windowsPlanToken = fromIntegral (ptrToWordPtr (slotToken two))}) ["echo", "0"]
        number <- (+ 1) <$> peekRuns fixture
        taken <- withHeldStation (stationName fixture number) (runBuilder fixture one id ["echo", "0"])
        refused <- runBuilder fixture one (\plan -> plan {Sandbox.windowsPlanDesktop = "no-desktop"}) ["echo", "0"]
        pure $
          assertEqual "a program that does not exist" (ExitFailure 1, Right (Just (Sandbox.StatusRecord Sandbox.WindowsStageExec fileNotFound Nothing))) (reported missing)
            `andThen` assertEqual "another slot's token" (ExitFailure 1, Right (Just (Sandbox.StatusRecord Sandbox.WindowsStageTokenUser 0 Nothing))) (reported otherToken)
            `andThen` assertEqual "a window station already made: exit, stage, an error" (ExitFailure 1, Just Sandbox.WindowsStageStation, True) (stationRecord taken)
            `andThen` assertEqual "a plan the helper refuses" (ExitFailure 1, Right (Just (Sandbox.StatusRecord Sandbox.WindowsStagePlan einval Nothing))) (reported refused),
      runTestM "a build cannot reach the user's own: its TEMP, profile and APPDATA, a file only it may read, a profile file, its hive or its process" $ do
        let attempts =
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
        let withBash plan = plan {Sandbox.windowsPlanEnvironment = environmentWith fixture one [takeDirectory bash, system32 fixture]}
        run <- runCommand fixture one withBash [bash, "-c", "/usr/bin/true && echo ok"]
        pure (assertEqual "exit and status" (ExitSuccess, "") (runExit run, runStatus run) `andThen` assertEqual "ok in the log" True ("ok\n" `BS.isInfixOf` runLog run))
    ]
  where
    dropExit (started, ended, gone, _) = (started, ended, gone)
    stationRecord run =
      let record = fromRight Nothing (windowsRecord (runStatus run))
       in (runExit run, Sandbox.recordStage <$> record, maybe False ((/= 0) . Sandbox.recordErrno) record)
    -- The name of a file a build tries to make where it may not.
    buildMade = "nova-nix-sandbox-build"

-- | That a builder's attempt was refused access, labelled.
refusedAttempt :: (Text, Run) -> TestResult
refusedAttempt (label, run) =
  ranAs ExitSuccess run `andThen` assertEqual label (Just accessDenied) (attempt run)

-- | How many runs have started.
peekRuns :: Fixture -> IO Int
peekRuns = readIORef . fxRuns

-- | Run an action while this process holds a window station of the name
-- given.
withHeldStation :: String -> IO a -> IO a
withHeldStation name action =
  withCWString name $ \wide -> alloca $ \stationOut -> do
    succeeded ("holding the window station " <> name) =<< c_station_hold wide stationOut
    station <- peek stationOut
    action `finally` c_station_release station

-- | The process IDs a builder wrote: its own and the one it started.
readPids :: FilePath -> IO (Maybe (Word32, Word32))
readPids path = do
  contents <- try (BS.readFile path)
  pure $ case contents of
    Left (_ :: IOException) -> Nothing
    Right bytes -> case traverse (readMaybe . BS8.unpack) (BS8.words bytes) of
      Just [builderPid, started] -> Just (builderPid, started)
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
      plan = planFor fixture slot number
      spec =
        (Proc.proc (fxHelper fixture) ("run" : builderCommand fixture ["hold", pids]))
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
    stationDacl <- wideResult (c_user_object_dacl 0)
    desktopDacl <- wideResult (c_user_object_dacl 1)
    traverse_ line [("user", user), ("station", station), ("desktop", desktop), ("console", T.pack (show console)), ("station-dacl", stationDacl), ("desktop-dacl", desktopDacl)]
  ["commit", mebibytesText] | Just mebibytes <- readMaybe mebibytesText -> do
    code <- c_commit (fromIntegral (mebibytes * mebibyte))
    line ("result", T.pack (show code))
  ["spawn", flagsText] | Just flags <- readMaybe flagsText -> do
    self <- getExecutablePath
    (code, process) <- start (quoted self <> " " <> windowsSandboxBuilderFlag <> " console") flags
    exitCode <- if code == 0 then waitFor process else pure 0
    line ("spawn", T.pack (show code <> " " <> show exitCode))
  ["console"] -> do
    code <- c_console
    exitWith (if code == 0 then ExitSuccess else ExitFailure (fromIntegral code))
  ["crash"] -> c_crash
  ["open", path] -> attemptOn path (`c_open` genericRead)
  ["create", path] -> attemptOn path c_create
  ["delete", path] -> attemptOn path c_delete
  ["list", wildcard] -> attemptOn wildcard c_list
  ["key", path] -> attemptOn path c_open_key
  ["process", pidText] | Just pid <- readMaybe pidText -> c_open_process pid >>= \code -> line ("result", T.pack (show code))
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
    line (name, value) = BS8.hPutStr stdout (name <> " " <> TE.encodeUtf8 value <> "\n")
    attemptOn path call = withCWString path call >>= \code -> line ("result", T.pack (show code))
    userObjectName which =
      allocaArray objectNameChars $ \name -> do
        code <- c_user_object_name which name (fromIntegral objectNameChars)
        if code == 0 then T.pack <$> peekCWString name else pure ("error " <> T.pack (show code))

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
  (code, (pid, process)) <- start (unwords (map quoted (self : windowsSandboxBuilderFlag : operation))) 0
  succeeded "starting a process" code
  succeeded "closing a process's handle" =<< c_close_handle process
  pure pid

-- | Write this process's ID and another's to a file, whole: the file
-- appears by a rename once it holds both.
writePids :: FilePath -> CULong -> IO ()
writePids path other = do
  own <- Proc.getCurrentPid
  let partial = path <> ".partial"
  BS.writeFile partial (BS8.pack (show own <> " " <> show other))
  Dir.renameFile partial path
