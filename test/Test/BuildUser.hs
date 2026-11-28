{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Build users ("Nix.Builder.BuildUser").
--
-- 'testBuildUsersPure' holds the setting, the refusal of a build user's
-- connection and the choice of a member to upstream's rules, the choice
-- over a model of the user database.  The rest needs root and a group of
-- build users: CI's Linux build makes @nixbld@ and two members, then runs
-- 'buildUsersRootChild' under sudo, which builds as them, unsandboxed and
-- in the sandbox, and checks whom a build ran as, what its output is, that
-- a node another user owns fails the build, that a process a build left
-- is killed, and that a build waits for a free member.
module Test.BuildUser
  ( testBuildUsersPure,
    buildUsersRootFlag,
    buildUsersRootChild,
  )
where

import Control.Monad.State.Strict (State, evalState, gets, modify', runState)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Word (Word32)
import Nix.Builder.BuildUser (BuildUser (..), BuildUsers (..), ProcessPrivilege (..), UserDatabase (..), acquireBuildUser, buildUsersFor, defaultBuildUsersGroup, peerInBuildUsersGroup, userPoolDir, waitingForBuildUserMessage, worldWritableMessage)
import System.FilePath ((</>))
import qualified System.Info
import qualified Test.QuickCheck as QC

#if defined(mingw32_HOST_OS)

import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Test.Harness (TestResult (..), andThen, assertEqual, runTest, runTestM)

#else

import Control.Concurrent.Async (race, wait, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, tryPutMVar)
import Control.Exception (bracket, finally)
import Control.Monad (unless, void, when)
import Data.Bits ((.&.))
import Data.Foldable (traverse_)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Maybe (catMaybes)
import Foreign.C.Types (CInt (..))
import qualified Nix.Builder as Builder
import qualified Nix.Builder.Sandbox.Spawn as Spawn
import Nix.Logger (Logger (..))
import Nix.Store (Store (..), closeStore, openStore)
import Nix.Store.Path (StoreDir (..), defaultStoreDir, storePathToFilePath, storePathToText)
import Nix.Store.Path.Internal (StorePath (..))
import System.Directory (createDirectoryIfMissing, doesFileExist, removePathForcibly)
import System.Exit (exitFailure)
import System.FileLock (SharedExclusive (Exclusive), tryLockFile, unlockFile)
import System.IO (hPutStrLn, stderr)
import System.Posix.Files (fileMode, fileOwner, getSymbolicLinkStatus, setFileMode, setOwnerAndGroup)
import qualified System.Posix.User.ByteString as User
import Test.Builder (linuxSandboxConfig)
import Test.Harness (TestResult (..), andThen, assertEqual, mkTestBuildDrv, runTest, runTestM, testDrvPath, testLogger)
import Text.Read (readMaybe)

#endif

-- ---------------------------------------------------------------------------
-- Without root
-- ---------------------------------------------------------------------------

-- | The setting, the refusal of a peer, and the choice of a member.
testBuildUsersPure :: IO [Bool]
testBuildUsersPure = do
  putStrLn "builder/build-users"
  sequence
    [ runTest "build-users-group defaults to nixbld for root and to nothing for anyone else, and builds run as its members only for root with it set" $
        assertEqual "root's default" "nixbld" (defaultBuildUsersGroup RunsAsRoot)
          `andThen` assertEqual "another's default" "" (defaultBuildUsersGroup RunsUnprivileged)
          `andThen` assertEqual "root with a group" (if supported then BuildUsersOf "nixbld" pool else NoBuildUsers) (buildUsersFor RunsAsRoot pool "nixbld")
          `andThen` assertEqual "root with none" NoBuildUsers (buildUsersFor RunsAsRoot pool "")
          `andThen` assertEqual "another with a group" NoBuildUsers (buildUsersFor RunsUnprivileged pool "nixbld"),
      runTest "the user pool is in upstream's state directory, or the one NIX_STATE_DIR names" $
        assertEqual "default" "/nix/var/nix/userpool" (userPoolDir Nothing)
          `andThen` assertEqual "named" ("/s" </> "userpool") (userPoolDir (Just "/s")),
      runTest "a peer whose primary group is the build users' is refused, and no other" $
        assertEqual "build user" True (peerInBuildUsersGroup "nixbld" (Just "nixbld"))
          `andThen` assertEqual "another group" False (peerInBuildUsersGroup "nixbld" (Just "users"))
          `andThen` assertEqual "no group" False (peerInBuildUsersGroup "nixbld" Nothing)
          `andThen` assertEqual "no setting" False (peerInBuildUsersGroup "" (Just "")),
      runTest "a group that is missing, has no members or names a missing user is refused in upstream's words" $
        assertEqual "missing" (Left "the group 'nixbld' specified in 'build-users-group' does not exist") (fst (acquired Map.empty Set.empty []))
          `andThen` assertEqual "empty" (Left "the build users group 'nixbld' has no members") (fst (acquired (Map.singleton "nixbld" (buildGid, [])) Set.empty []))
          `andThen` assertEqual "a missing member" (Left "the user 'ghost' in the group 'nixbld' does not exist") (fst (acquired (groupOf ["ghost"]) Set.empty [])),
      runTest "the first member whose lock is free is taken, with its supplementary groups but the build users' own, and those after it are not looked up" $
        let (outcome, model) = acquired (groupOf (memberNames 3)) (Set.singleton (uidOf 1)) []
         in assertEqual "taken" (Right (Just (BuildUser (uidOf 2) buildGid [kvmGid], uidOf 2))) outcome
              `andThen` assertEqual "locks" (Set.fromList [uidOf 1, uidOf 2]) (modelLocked model)
              `andThen` assertEqual "looked up" ["nixbld2", "nixbld1"] (modelLookedUp model),
      runTest "with every member taken there is none to take, and no lock is held" $
        let locked = Set.fromList (map uidOf [1, 2])
            (outcome, model) = acquired (groupOf (memberNames 2)) locked []
         in assertEqual "none" (Right Nothing) outcome `andThen` assertEqual "locks" locked (modelLocked model),
      runTest "a member that is the user nova-nix runs as is refused in upstream's words, and its lock given back" $
        let (outcome, model) = acquired (groupOf (memberNames 2)) Set.empty [uidOf 1]
         in assertEqual "refused" (Left "the Nix user should not be a member of 'nixbld'") outcome
              `andThen` assertEqual "locks" Set.empty (modelLocked model),
      runTest "a lock file that cannot be opened refuses the build with the reason" $
        assertEqual "refused" (Left unopenableReason) (evalState (acquireBuildUser (modelDatabase (groupOf (memberNames 2)) (Set.singleton (uidOf 1))) [] "nixbld") (Model Set.empty [])),
      runTestM "the member taken is the first in the group's order whose lock is free, and the only lock taken is its own" $ do
        result <-
          QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} $
            QC.forAll ((,) <$> QC.choose (1, 6) <*> QC.sublistOf (map uidOf [1 .. 6])) $ \(count, lockedList) ->
              let locked = Set.fromList lockedList
                  (outcome, model) = acquired (groupOf (memberNames count)) locked []
                  firstFree = take 1 [uid | n <- [1 .. count], let uid = uidOf n, Set.notMember uid locked]
               in case (outcome, firstFree) of
                    (Right (Just (user, lock)), [uid]) -> buUid user QC.=== uid QC..&&. lock QC.=== uid QC..&&. modelLocked model QC.=== Set.insert uid locked
                    (Right Nothing, []) -> modelLocked model QC.=== locked
                    _ -> QC.counterexample (show outcome) False
        pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result))),
      runTest "the waiting line and the refusal of a directory others may write are upstream's" $
        assertEqual "waiting" "waiting for a free build user ID for '/nix/store/x.drv'" (waitingForBuildUserMessage "/nix/store/x.drv")
          `andThen` assertEqual "world-writable" "Path \"/tmp\" is world-writable or a symlink. That's not allowed for security." (worldWritableMessage "/tmp")
    ]
  where
    pool = "/nix/var/nix/userpool"
    supported = System.Info.os `elem` ["linux", "darwin"]
    acquired groups locked own = runState (acquireBuildUser (modelDatabase groups Set.empty) own "nixbld") (Model locked [])

-- | What the model of the user database holds of the pool: the uids whose
-- locks are held, and the users looked up, the latest first.
data Model = Model
  { modelLocked :: !(Set Word32),
    modelLookedUp :: ![BS.ByteString]
  }

-- | A user database holding the groups given, whose every member
-- @nixbldN@ is a user with uid 'uidOf' N and the build users' group as its
-- primary group, and is also in @kvm@; the lock file of each uid in the set
-- given cannot be opened.
modelDatabase :: Map.Map BS.ByteString (Word32, [BS.ByteString]) -> Set Word32 -> UserDatabase (State Model) Word32
modelDatabase groups unopenable =
  UserDatabase
    { udGroup = \name -> pure (Map.lookup name groups),
      udUser = \name -> do
        modify' (\model -> model {modelLookedUp = name : modelLookedUp model})
        pure (Map.lookup name users),
      udGroups = \_ primary -> pure (nub [primary, kvmGid, buildGid]),
      udTryLock = \uid ->
        if Set.member uid unopenable
          then pure (Left unopenableReason)
          else do
            taken <- gets (Set.member uid . modelLocked)
            if taken
              then pure (Right Nothing)
              else Right (Just uid) <$ modify' (\model -> model {modelLocked = Set.insert uid (modelLocked model)}),
      udRelease = \uid -> modify' (\model -> model {modelLocked = Set.delete uid (modelLocked model)})
    }
  where
    users = Map.fromList [(name, (uid, buildGid)) | n <- [1 .. 6], let name = memberName n, let uid = uidOf n]

-- | The model's reason a lock file cannot be opened.
unopenableReason :: T.Text
unopenableReason = "opening user lock \"/nix/var/nix/userpool/30001\": permission denied"

-- | The build users' group of the model, holding these members.
groupOf :: [BS.ByteString] -> Map.Map BS.ByteString (Word32, [BS.ByteString])
groupOf members = Map.singleton "nixbld" (buildGid, members)

-- | The model's first members, in order.
memberNames :: Int -> [BS.ByteString]
memberNames count = map memberName [1 .. count]

-- | The model's member N.
memberName :: Int -> BS.ByteString
memberName n = "nixbld" <> BS8.pack (show n)

-- | The model's uid of member N.
uidOf :: Int -> Word32
uidOf n = 30000 + fromIntegral n

-- | The model's build users' group and the other group its members are in.
buildGid, kvmGid :: Word32
buildGid = 30000
kvmGid = 78

-- ---------------------------------------------------------------------------
-- As root
-- ---------------------------------------------------------------------------

-- | The flag that makes the suite the root child that builds as build
-- users, given the sandbox helper, the static busybox, a directory to make
-- and remove that no ancestor of every user may write and every user may
-- search, and the group of build users.
buildUsersRootFlag :: String
buildUsersRootFlag = "--build-users-root"

-- | Run the checks that build as build users, printing each, and exit with
-- a failure when one fails.
buildUsersRootChild :: [String] -> IO ()

#if defined(mingw32_HOST_OS)

buildUsersRootChild _ = hPutStrLn stderr "build users need Linux or macOS" *> exitFailure

#else

buildUsersRootChild arguments = case arguments of
  [helper, busybox, root, group] -> do
    results <- rootChecks helper busybox root (BS8.pack group) `finally` removePathForcibly root
    let failed = length (filter not results)
    putStrLn (show (length results) <> " build-user checks, " <> show failed <> " failed")
    when (failed > 0) exitFailure
  _ -> hPutStrLn stderr ("usage: " <> buildUsersRootFlag <> " HELPER BUSYBOX ROOT GROUP") *> exitFailure

-- | The checks, in a store under the directory given.
rootChecks :: FilePath -> FilePath -> FilePath -> BS.ByteString -> IO [Bool]
rootChecks helper busybox root group = do
  putStrLn "builder/build-users-as-root"
  removePathForcibly root
  createDirectoryIfMissing True root
  setFileMode root 0o755
  members <- memberUids group
  groupId <- User.groupID <$> User.getGroupEntryForName group
  busyboxBytes <- Spawn.filePathBytes busybox
  helperThere <- doesFileExist helper
  nobody <- User.getUserEntryForName "nobody"
  let storeDir = StoreDir (root </> "store")
      pool = root </> "state" </> "userpool"
      unsandboxed store = (Builder.defaultBuildConfig (stDir store)) {Builder.bcBuildUsers = BuildUsersOf group pool}
      sandboxed store = (unsandboxed store) {Builder.bcSandbox = linuxSandboxConfig helper busyboxBytes}
      sandboxAvailable = System.Info.os == "linux" && helperThere
      outPath letter = StorePath (T.replicate 32 (T.singleton letter))
      onDisk = storePathToFilePath storeDir
      build config store sp script = Builder.buildDerivation config store (mkTestBuildDrv "/bin/sh" sp script) testDrvPath
      rootOwnedReadOnly path = do
        status <- getSymbolicLinkStatus path
        pure (assertEqual "owner" 0 (fileOwner status) `andThen` assertEqual "write bits" 0 (fileMode status .&. 0o222))
  bracket (openStore testLogger storeDir) closeStore $ \store ->
    sequence
      [ runTestM "an unsandboxed build runs as a member of the build users' group, with the group alone, leading a session of its own, and its output is root's and read-only" $ do
          let sp = outPath 'a' "unsandboxed-uid"
          built <- build (unsandboxed store) store sp "{ id -u; id -G; echo $$ $(cut -d ' ' -f 6 /proc/$$/stat); } > $out"
          succeeded built $ do
            written <- BS8.lines <$> BS.readFile (onDisk sp)
            owner <- rootOwnedReadOnly (onDisk sp)
            pure $ case map BS8.words written of
              [[uid], groups, [pid, session]] ->
                assertEqual "a build user's uid" True (maybe False (`elem` members) (readMaybe (BS8.unpack uid)))
                  `andThen` assertEqual "groups" [BS8.pack (show groupId)] (nub groups)
                  `andThen` assertEqual "its own session" pid session
                  `andThen` owner
              _ -> Fail ("the build wrote " <> T.pack (show written)),
        if sandboxAvailable
          then runTestM "a sandboxed build runs as the sandbox's user, which maps to a member of the build users' group, and its output is root's and read-only" $ do
            let sp = outPath 'b' "sandboxed-uid"
            built <- build (sandboxed store) store sp "/bin/busybox cat /proc/self/uid_map > $out"
            succeeded built $ do
              mapping <- BS8.words <$> BS.readFile (onDisk sp)
              owner <- rootOwnedReadOnly (onDisk sp)
              pure $ case mapping of
                [inside, outside, count] ->
                  assertEqual "inside" "1000" inside
                    `andThen` assertEqual "a build user's uid" True (maybe False (`elem` members) (readMaybe (BS8.unpack outside)))
                    `andThen` assertEqual "count" "1" count
                    `andThen` owner
                _ -> Fail ("the map was " <> T.pack (show mapping))
          else runTest "a sandboxed build as a build user (skipped: no sandbox helper on Linux here)" Pass,
        if sandboxAvailable
          then runTestM "a sandboxed build as root with no build users is refused" $ do
            built <- build ((sandboxed store) {Builder.bcBuildUsers = NoBuildUsers}) store (outPath 'c' "refused") "echo ran > $out"
            pure $ case built of
              Builder.BuildFailure message _ -> assertEqual "refusal" True ("needs build users" `T.isInfixOf` message)
              Builder.BuildSuccess _ -> Fail "a sandboxed build ran as root"
          else runTest "a sandboxed build as root with no build users (skipped: no sandbox helper on Linux here)" Pass,
        runTestM "an output holding a node another user owns fails the build, naming the node, which is left as it was" $ do
          let foreignDir = root </> "foreign"
              foreignFile = foreignDir </> "file"
              sp = outPath 'd' "foreign"
          createDirectoryIfMissing True foreignDir
          setFileMode foreignDir 0o755
          BS.writeFile foreignFile "another user's\n"
          setOwnerAndGroup foreignFile (User.userID nobody) (User.userGroupID nobody)
          -- So a build user may link it, as Linux's protected_hardlinks
          -- lets a user link a file it may read and write.
          setFileMode foreignFile 0o666
          built <- build (unsandboxed store) store sp ("mkdir $out && ln " <> T.pack foreignFile <> " $out/linked")
          after <- getSymbolicLinkStatus foreignFile
          pure $ case built of
            Builder.BuildFailure message _ ->
              assertEqual "named" True (("invalid ownership on file '" <> T.pack (onDisk sp </> "linked") <> "'") `T.isInfixOf` message)
                `andThen` assertEqual "still another's" (User.userID nobody) (fileOwner after)
                `andThen` assertEqual "its mode" 0o666 (fileMode after .&. 0o777)
            Builder.BuildSuccess _ -> Fail "an output holding another user's file was registered",
        runTestM "a build below a directory every user may write is refused in upstream's words" $ do
          let open = root </> "open"
          createDirectoryIfMissing True open
          setFileMode open 0o777
          built <- bracket (openStore testLogger (StoreDir (open </> "store"))) closeStore $ \openedStore ->
            build (unsandboxed openedStore) openedStore (outPath 'h' "under-open") "echo ran > $out"
          pure $ case built of
            Builder.BuildFailure message _ -> assertEqual "refusal" True (worldWritableMessage open `T.isInfixOf` message)
            Builder.BuildSuccess _ -> Fail "a build below a directory every user may write ran",
        runTestM "a process a build leaves behind is killed once the build ends" $ do
          let sp = outPath 'f' "leftover"
          built <- build (unsandboxed store) store sp "sleep 600 </dev/null >/dev/null 2>&1 & echo $! > $out"
          succeeded built $ do
            pid <- readMaybe . BS8.unpack . BS8.strip <$> BS.readFile (onDisk sp)
            case pid of
              Nothing -> pure (Fail "the build wrote no process id")
              Just number -> assertEqual "ended" 0 <$> c_nova_test_await_exit number leftoverWaitMillis,
        runTestM "a build waits while every build user is taken, says so once, and runs once one is free" $ do
          createDirectoryIfMissing True pool
          held <- catMaybes <$> traverse (\uid -> tryLockFile (pool </> show uid) Exclusive) members
          waiting <- newEmptyMVar
          logged <- newIORef []
          let drvText = storePathToText defaultStoreDir testDrvPath
              logger =
                Logger
                  { logMessage = \message -> do
                      modifyIORef' logged (message :)
                      when (message == waitingForBuildUserMessage drvText) (void (tryPutMVar waiting ())),
                    logBuildLine = const (pure ())
                  }
              sp = outPath 'g' "waited"
          -- A build that ends before it waits ends the check, rather than
          -- leaving it waiting for a line that never comes.
          built <-
            withAsync
              (build (unsandboxed store) store {stLogger = logger} sp "echo ran > $out")
              ( \running -> do
                  first <- race (takeMVar waiting) (wait running)
                  either (\() -> traverse_ unlockFile (take 1 held) *> wait running) pure first
              )
              `finally` traverse_ unlockFile (drop 1 held)
          messages <- readIORef logged
          pure $
            assertEqual "every member held" (length members) (length held)
              `andThen` succeededWith built
              `andThen` assertEqual "said once" 1 (length (filter (== waitingForBuildUserMessage drvText) messages))
      ]
  where
    succeeded built check = case built of
      Builder.BuildSuccess _ -> check
      Builder.BuildFailure message code -> pure (Fail ("the build failed (" <> T.pack (show code) <> "): " <> message))
    succeededWith built = case built of
      Builder.BuildSuccess _ -> Pass
      Builder.BuildFailure message code -> Fail ("the build failed (" <> T.pack (show code) <> "): " <> message)

-- | The uids of a group's members.
memberUids :: BS.ByteString -> IO [Word32]
memberUids group = do
  entry <- User.getGroupEntryForName group
  users <- traverse User.getUserEntryForName (User.groupMembers entry)
  unless (length users >= 2) (hPutStrLn stderr "the group of build users needs two members for these checks")
  pure (map (fromIntegral . User.userID) users)

-- | How long a process a build left behind is given to end once the build
-- has, which a kill ends at once and a sleep of ten minutes does not.
leftoverWaitMillis :: CInt
leftoverWaitMillis = 10000

foreign import ccall safe "nova_test_await_exit"
  c_nova_test_await_exit :: CInt -> CInt -> IO CInt

#endif
