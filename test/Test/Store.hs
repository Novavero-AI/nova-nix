{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests of the store: paths, hashes, archives, the database, garbage
-- collection and the metadata and access of what it holds.
module Test.Store
  ( testStorePaths,
    testSerialiseIfReadableIO,
    testExecBit,
    testPathLocks,
    testCaseSensitiveVolumeIO,
    testFoldingVolumeIO,
    testStoreDB,
    testParseStorePath,
    testStoreAccessIO,
    testStoreOpsIO,
    testHashHelpers,
    testNarKnownAnswer,
    testStoreDelete,
    testGcPure,
    testStoreGC,
    testStoreGCUpstreamManaged,
    testStoreWritesUpstreamManaged,
    testStoreMetadata,
    testStoreOps,
    testSymlinkWalksIO,
    testLinkOrdering,
    testStoreNameSinks,
    testNarNameSafety,
    testStoreNameSinksIO,
  )
where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Exception (IOException, SomeException, bracket, bracket_, evaluate, finally, throwIO, try)
import Control.Monad (filterM, forM, join, when)
import Data.Bits (complement, popCount, shiftR, xor, (.&.), (.|.))
import qualified Data.ByteString as BS
import Data.Char (isDigit)
import Data.Either (isLeft, isRight)
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isPrefixOf, nub, sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Word (Word16, Word32, Word8)
import qualified Database.SQLite.Simple as SQL
import DirDacl (Dacl (..), DaclEntry (..), directoryDacl, processUserSid)
import GHC.IO.Exception (IOErrorType (..), IOException (..))
import HttpFixture (Reply (..), httpResponse, withHttpServer)
import Nix.Builder (BuildConfig (..), BuildResult (..), BuildStop (..), buildDerivation, buildWithDeps, checkDerivation, defaultBuildConfig, verifyFetchHash)
import Nix.Builder.BuildDir (createBuildDir)
import Nix.Derivation (Derivation (..), DerivationOutput (..), currentPlatform, toATerm)
import Nix.Eval (NixValue (..))
import Nix.Eval.CanonPath (canonPath, canonPathValue)
import Nix.Eval.IO (EvalState (..), sourceCopies)
import Nix.Hash (FileIngestionMethod (..), compressHash, makeFixedOutputPath, makeTextPath, sha256Digest)
import qualified Nix.Hash as Hash
import Nix.Http (defaultFetchRetryPolicy, ioRetryEffects)
import Nix.Push (computeClosure)
import qualified Nix.Realise as Realise
import Nix.Store (DeleteOutcome (..), GcResults (..), GcRoot (..), Store (..), SweepPlan (..), acquirePathLock, addOutLinkRoot, addToStore, autoRootsDir, canonicalStoreDir, canonicalizeMetadata, closeStore, collectGarbage, copyPathInto, deleteStorePathRaw, deleteStorePaths, findRoots, gcLockFilePath, gcRootsDir, gcSummaryLine, indirectRootRecordName, isValid, materializeEvalSources, materializeEvalStoreWrites, openStore, orderLinks, pathExists, reachableFrom, referencesMap, registrationFor, releasePathLock, resolveDeleteTarget, rewriteBytes, rewriteStoreHashes, scanReferences, showFreedBytes, storeEntryName, storeEntryNameText, sweepPlan, tryAcquirePathLock, unpackNarEntry, upstreamDatabasePath, withCollectorLock, writeDrv)
import Nix.Store.Access (AclEntry (..), DirAccess (..), DirSecurity (..), Distrust (..), accessSddl, descriptorDistrust, distrustMessage, parseSecurityDescriptor, securityDistrust, sidFromText, stateDirectories, storeDirectories)
import Nix.Store.CaseSensitive (CaseSensitivity (..), probeCaseSensitivity, processCaseHack)
import Nix.Store.DB (PathInfo (..), PathRegistration (..), closeStoreDB, dbFileName, isValidPath, metaDirName, openStoreDB, queryAllValidPaths, queryDeriver, queryPathInfo, queryReferences, registerPath, registerPaths)
import Nix.Store.EntryName (NameRules (..), checkEntryName, checkLinkTarget)
import qualified Nix.Store.ExecBit as ExecBit
import Nix.Store.Metadata (MetadataChange (..), NodeKind (..), NodeMetadata (..), Ownership (..), canonicalPermissions, executablePermissions, metadataChanges, readOnlyPermissions, storeModificationTime)
import Nix.Store.Ops (EvalObjects (..), StoreOps (..), StoreQueries (..), localQueries, withLocalStore, withOpenedOnFirstUse)
import Nix.Store.Path (StoreDir (..), StorePath, StoreWriteMode (..), defaultStoreDir, defaultStoreDirText, isCanonicalStoreText, parseStorePath, parseStorePathBaseName, parseStorePathPrefix, platformStoreDir, qualifyPathText, storePathHashLen, storePathToFilePath, storePathToText, storeTextToFilePath, windowsStoreDir)
import Nix.Store.Path.Internal (StorePath (..))
import Nix.Store.Upstream (UpstreamHazard (..), upstreamManagedRefusal)
import qualified Nix.Substituter as Subst
import NodeStatus (NodeStatus (..), nodeStatus)
import qualified NovaCache.Hash as CHash
import qualified NovaCache.NAR as NAR
import qualified NovaCache.NarInfo as NarInfo
import qualified NovaCache.Signing as Signing
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getPermissions, getTemporaryDirectory, removeDirectoryRecursive, writable)
import qualified System.Directory as Dir
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import qualified System.FileLock as FileLock
import System.FilePath (dropDrive, joinPath, pathSeparator, splitDirectories, takeDirectory, takeDrive, takeFileName, (</>))
import System.IO.Error (ioeGetErrorString, ioeGetFileName, isAlreadyExistsError, isUserError)
import qualified System.Info as SI
import qualified System.OsPath as OP
import qualified System.Process as Proc
import System.Timeout (timeout)
import Test.Harness (TestResult (..), andThen, assertEqual, assertEvalErrorIO, bytesText, canonicalAs, chunkReader, deleteNamed, evalAndBuild, evalNix, evalNixIOIn, evalNixIOStore, findTestShell, forceRemoveIfExists, genDrvName, genFixturePath, mkTestBuildDrv, narInfoRequest, narSpecFileVector, narSpecVector, offeredThenFetched, parseErrorTag, posixOnlyEntries, quotedAsCxx, removeIfExists, runTest, runTestM, servedPaths, sigTestKeyText, simpleTestDrv, symlinksAvailable, testDrvPath, testEvalState, testLogger, walkWatchdogMicros, withTempStore)
import qualified Test.QuickCheck as QC
import Text.Read (readMaybe)

-- ---------------------------------------------------------------------------
-- Tests: Store paths (existing)
-- ---------------------------------------------------------------------------

testStorePaths :: IO [Bool]
testStorePaths = do
  putStrLn "store/path"
  let sp = StorePath {spHash = "s66mzxpvicwk07gjbjfw9izjfa797vsw", spName = "hello-2.12.1"}
  sequence
    [ runTest "default store dir" $
        assertEqual "defaultStoreDir" "/nix/store" (unStoreDir defaultStoreDir),
      runTest "windows store dir" $
        assertEqual "windowsStoreDir" "C:\\nix\\store" (unStoreDir windowsStoreDir),
      runTest "store path to text" $
        assertEqual
          "storePathToText"
          "/nix/store/s66mzxpvicwk07gjbjfw9izjfa797vsw-hello-2.12.1"
          (T.unpack (storePathToText defaultStoreDir sp)),
      runTest "store path ordering" $
        let sp2 = StorePath {spHash = "zzz", spName = "later"}
         in assertEqual "Ord" True (sp < sp2),
      -- The reader-side mapping: canonical store text resolves into the
      -- store dir it is given; everything else is the path as written.
      runTest "store text maps into the platform store dir" $
        assertEqual
          "mapped"
          (unStoreDir platformStoreDir <> "/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-x/sub/f")
          (storeTextToFilePath platformStoreDir "/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-x/sub/f"),
      runTest "bare store dir text maps to the platform store root" $
        assertEqual "mapped-root" (unStoreDir platformStoreDir) (storeTextToFilePath platformStoreDir "/nix/store"),
      runTest "store text with a backslash subpath maps" $
        assertEqual
          "mapped-backslash"
          (unStoreDir platformStoreDir <> "\\bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb-y")
          (storeTextToFilePath platformStoreDir "/nix/store\\bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb-y"),
      runTest "non-store text under a store with no drive is the path as written" $
        assertEqual "unmapped" "/etc/hosts" (storeTextToFilePath defaultStoreDir "/etc/hosts"),
      runTest "a store-prefix-like name does not map" $
        assertEqual "unmapped-like" "/nix/storefoo" (storeTextToFilePath defaultStoreDir "/nix/storefoo")
          `andThen` assertEqual "on-the-store-drive" "C:/nix/storefoo" (storeTextToFilePath windowsStoreDir "/nix/storefoo"),
      -- A rooted path with no drive names the store's drive, fixed by
      -- configuration rather than by the drive that is current.
      runTest "a rooted path with no drive opens on the store's drive" $
        assertEqual "default" "C:/probe/f.txt" (storeTextToFilePath windowsStoreDir "/probe/f.txt")
          `andThen` assertEqual "redirected" "D:/probe/f.txt" (storeTextToFilePath (StoreDir "D:\\s") "/probe/f.txt")
          `andThen` assertEqual "root" "C:/" (storeTextToFilePath windowsStoreDir "/"),
      -- A backslash roots a path only where it is a separator.
      runTest "a backslash-rooted path gains the drive on Windows and is a name on POSIX" $
        assertEqual
          "backslash"
          (if SI.os == "mingw32" then "C:\\probe\\f.txt" else "\\probe\\f.txt")
          (storeTextToFilePath windowsStoreDir "\\probe\\f.txt"),
      runTest "a drive-qualified, relative or UNC path opens as written" $
        assertEqual "other-drive" "C:/probe/f.txt" (storeTextToFilePath (StoreDir "D:\\s") "C:/probe/f.txt")
          `andThen` assertEqual "relative" "probe/f.txt" (storeTextToFilePath windowsStoreDir "probe/f.txt")
          `andThen` assertEqual "unc" "//server/share/f.txt" (storeTextToFilePath windowsStoreDir "//server/share/f.txt")
          `andThen` assertEqual "unc-backslash" "\\\\server\\share\\f.txt" (storeTextToFilePath windowsStoreDir "\\\\server\\share\\f.txt"),
      -- Store text names the store directory, not the store drive's
      -- \nix\store, and the logical store's parent is a plain path there.
      runTest "store text under a redirected Windows store resolves through the store dir" $
        assertEqual "store" "D:\\s/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-x/f" (storeTextToFilePath (StoreDir "D:\\s") "/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-x/f")
          `andThen` assertEqual "store-root" "D:\\s" (storeTextToFilePath (StoreDir "D:\\s") "/nix/store")
          `andThen` assertEqual "parent" "D:/nix" (storeTextToFilePath (StoreDir "D:\\s") "/nix")
          `andThen` assertEqual "value" "/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-x" (qualifyPathText (StoreDir "D:\\s") "/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-x"),
      runTest "on this platform's store a rooted path with no drive gains C: on Windows only" $
        assertEqual
          "platform"
          (if SI.os == "mingw32" then "C:/probe/f.txt" else "/probe/f.txt")
          (storeTextToFilePath platformStoreDir "/probe/f.txt"),
      runTestM "a drive-qualified path opens as written under every store dir" $
        property $
          QC.forAll ((,) <$> genStoreDir <*> genDriveQualified) $ \(sd, path) ->
            storeTextToFilePath sd path QC.=== T.unpack path,
      runTestM "a rooted path outside the store gains the store's drive" $
        property $
          QC.forAll ((,,) <$> genDrive <*> genDriveDirTail <*> genOutsideStore) $ \(drive, dirTail, path) ->
            storeTextToFilePath (StoreDir (T.unpack (drive <> dirTail))) path QC.=== T.unpack (drive <> path),
      runTestM "under a store with no drive every path outside the store opens as written" $
        property $
          QC.forAll ((,) <$> genPosixStoreDir <*> QC.oneof [genOutsideStore, genDriveQualified]) $ \(sd, path) ->
            storeTextToFilePath sd path QC.=== T.unpack path,
      runTestM "store text resolves through the store dir under every store dir" $
        property $
          QC.forAll ((,) <$> genStoreDir <*> genStoreTail) $ \(sd, rest) ->
            storeTextToFilePath sd (defaultStoreDirText <> rest) QC.=== (unStoreDir sd <> T.unpack rest),
      runTestM "a path outside the store opens as the text the policy checks" $
        property $
          QC.forAll ((,) <$> genStoreDir <*> QC.oneof [genOutsideStore, genDriveQualified]) $ \(sd, path) ->
            storeTextToFilePath sd path QC.=== T.unpack (qualifyPathText sd path),
      runTestM "qualifying a path twice is qualifying it once" $
        property $
          QC.forAll ((,) <$> genStoreDir <*> QC.oneof [genOutsideStore, genDriveQualified, (defaultStoreDirText <>) <$> genStoreTail]) $ \(sd, path) ->
            qualifyPathText sd (qualifyPathText sd path) QC.=== qualifyPathText sd path,
      -- The reason this takes a store dir at all: a caller given one must
      -- read from it, not from the platform default.
      runTest "store text maps into a redirected store dir" $
        assertEqual
          "redirected"
          "/tmp/scratch-store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-x"
          (storeTextToFilePath (StoreDir "/tmp/scratch-store") "/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-x"),
      -- Both sides must land in the SAME store dir.  Not the same string:
      -- storePathToFilePath joins with the native separator while
      -- storeTextToFilePath passes the canonical text's tail through, so
      -- the two spellings differ on Windows and open the same file.
      runTest "a redirected read lands in the same store as the write" $
        let redirected = StoreDir "/tmp/scratch-store"
            redirectedPath = StorePath {spHash = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", spName = "x"}
            readSide = storeTextToFilePath redirected (storePathToText defaultStoreDir redirectedPath)
            writeSide = storePathToFilePath redirected redirectedPath
            underRedirected p = unStoreDir redirected `isPrefixOf` p
         in assertEqual
              "both under the redirected store"
              (True, True, False)
              (underRedirected readSide, underRedirected writeSide, unStoreDir platformStoreDir `isPrefixOf` readSide),
      runTest "isCanonicalStoreText accepts both separators and rejects lookalikes" $
        assertEqual
          "predicate"
          [True, True, True, False, False]
          ( map
              isCanonicalStoreText
              ["/nix/store", "/nix/store/x", "/nix/store\\x", "/nix/storefoo", "C:\\nix\\store\\x"]
          ),
      runTestM "compressHash XORs byte i into position i mod the length" $ do
        result <-
          QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} $
            QC.forAll genCompressCase $ \(len, bytes) ->
              BS.unpack (compressHash len (BS.pack bytes)) QC.=== compressHashModel len bytes
        pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result))),
      -- Upstream's own fixture (makeNarInfo in libstore-tests/nar-info.cc
      -- and data/nar-info/json-1/impure.json at 2.35.4): foo, the NAR
      -- SHA-256 of "(...)", referencing bar and itself, is n5wkd9...-foo,
      -- so the type upstream hashes is source, then each reference after
      -- a colon.
      runTest "a source path's type is upstream's: source, then each reference after a colon" $
        assertEqual
          "upstream's fixture"
          (Right "n5wkd9frr45pa74if5gpz9j7mifg27fh")
          (spHash <$> Hash.makeStorePath defaultStoreDir "source:/nix/store/g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-bar:self" (sha256Digest "(...)") "foo"),
      runTestM "a source path appends its references in path order, each once" $
        property $
          QC.forAll ((,) <$> QC.resize 4 (QC.listOf (genFixturePath "")) <*> genSourceDigest) $ \(refs, digest) ->
            Hash.makeSourcePath "profile" digest (Set.fromList refs)
              QC.=== Hash.makeStorePath defaultStoreDir ("source" <> T.concat [":" <> storePathToText defaultStoreDir r | r <- sort (nub refs)]) digest "profile",
      runTestM "a NAR SHA-256 fixed output is a source path with no references, its type source alone" $
        property $
          QC.forAll ((,) <$> genDrvName <*> genSourceDigest) $ \(name, digest) ->
            makeFixedOutputPath name "sha256" NarIngestion digest
              QC.=== Hash.makeSourcePath name digest Set.empty
              QC..&&. Hash.makeSourcePath name digest Set.empty QC.=== Hash.makeStorePath defaultStoreDir "source" digest name
    ]
  where
    genSourceDigest = sha256Digest . BS.pack <$> QC.resize 16 (QC.listOf QC.arbitrary)
    -- Lengths on both sides of the input's, so the last chunk is short,
    -- whole or absent, and below one, where there is nothing to fold into.
    genCompressCase = (,) <$> QC.choose (-1, 40) <*> QC.resize 70 (QC.listOf QC.arbitrary)
    property :: (QC.Testable prop) => prop -> IO TestResult
    property prop = do
      result <- QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} prop
      pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))
    genDrive = QC.elements ["C:", "D:", "z:"]
    -- What follows the drive in a Windows store directory, either spelling.
    genDriveDirTail = QC.elements ["\\nix\\store", "\\s", "/s/store"]
    genPosixStoreDir = StoreDir <$> QC.elements ["/nix/store", "/tmp/scratch-store", "/s"]
    genStoreDir = QC.oneof [genPosixStoreDir, (\drive dirTail -> StoreDir (T.unpack (drive <> dirTail))) <$> genDrive <*> genDriveDirTail]
    -- Segments that spell the store, and its lookalikes, when they lead.
    genSegments = QC.listOf (QC.elements ["a", "src", "f.txt", "nix", "store", "storefoo", "x y"])
    genDriveQualified = (\drive segments -> drive <> "/" <> T.intercalate "/" segments) <$> genDrive <*> genSegments
    -- Rooted paths with no drive that are not store text, @/@ and @/nix@
    -- among them.
    genOutsideStore = ((\segments -> "/" <> T.intercalate "/" segments) <$> genSegments) `QC.suchThat` (not . isCanonicalStoreText)
    -- What follows @/nix/store@ in store text.
    genStoreTail = QC.oneof [pure "", (\sep segments -> sep <> T.intercalate "/" segments) <$> QC.elements ["/", "\\"] <*> genSegments]

-- | Upstream's @compressHash@ loop (hash.cc), one byte at a time.
compressHashModel :: Int -> [Word8] -> [Word8]
compressHashModel len bytes =
  [foldl' xor 0 [b | (i, b) <- zip [0 :: Int ..] bytes, i `mod` len == j] | j <- [0 .. len - 1]]

-- ---------------------------------------------------------------------------
-- Tests: import and IO builtins (IO)
-- ---------------------------------------------------------------------------

-- | A tree holding a directory the walk cannot list.  The walk fails as a
-- 'NAR.NarWalkError', not an 'IOException', so a caller that reads an
-- unreadable tree as no tree must catch both, which
-- 'ExecBit.serialiseIfReadable' does.  Skipped where a directory without
-- read permission can still be listed (as root, or on Windows).
testSerialiseIfReadableIO :: IO [Bool]
testSerialiseIfReadableIO = do
  putStrLn "store/serialise-if-readable-io"
  tmpBase <- getTemporaryDirectory
  let root = tmpBase </> "nova-nix-test-serialise-if-readable"
      locked = root </> "locked"
      listable = Dir.setOwnerReadable True (Dir.setOwnerSearchable True (Dir.setOwnerWritable True Dir.emptyPermissions))
      unlistable = Dir.setOwnerSearchable True Dir.emptyPermissions
  -- A run that died while the directory was unlistable left it so, and it
  -- cannot be emptied until it is listable again.
  leftLocked <- Dir.doesDirectoryExist locked
  when leftLocked (Dir.setPermissions locked listable)
  Dir.removePathForcibly root
  createDirectoryIfMissing True locked
  BS.writeFile (locked </> "file") "f"
  readable <- ExecBit.serialiseIfReadable processCaseHack root
  let checks =
        [ runTestM "serialiseIfReadable archives a tree it can read" $
            pure (if isJust readable then Pass else Fail "a readable tree read as no tree"),
          runTestM "a directory the walk cannot list fails the walk as a NarWalkError" $ do
            walked <- try (ExecBit.serialiseFromPath processCaseHack root) :: IO (Either NAR.NarWalkError NAR.NarEntry)
            pure $ case walked of
              Left failure -> assertEqual "walk-access" NAR.WalkListDirectory (NAR.nweAccess failure)
              Right _ -> Fail "the walk archived a directory it cannot list",
          runTestM "serialiseIfReadable reads a tree it cannot list as no tree" $ do
            unread <- ExecBit.serialiseIfReadable processCaseHack root
            pure (if isNothing unread then Pass else Fail "an unlistable tree was archived")
        ]
      probeThenCheck = do
        probe <- try (Dir.listDirectory locked) :: IO (Either IOException [FilePath])
        case probe of
          Right _ -> [] <$ putStrLn "  SKIP  a directory without read permission can be listed here"
          Left _ -> sequence checks
  -- Read permission is restored however the checks end, and the tree
  -- removed after it.
  bracket_ (Dir.setPermissions locked unlistable) (Dir.setPermissions locked listable) probeThenCheck
    `finally` Dir.removePathForcibly root

-- ---------------------------------------------------------------------------
-- Tests: per-store-path locks (the substitution race)
-- ---------------------------------------------------------------------------

-- | Watchdog for the concurrent-substitution race: generous, because a
-- lock-protocol regression shows up as a hang, never as a fast wrong
-- answer.
raceWatchdogMicros :: Int
raceWatchdogMicros = 30 * 1000000

-- | How long a delete gets to (wrongly) finish while the path lock is
-- held elsewhere: long enough that an unlocked delete of one tiny tree
-- always completes inside it, while a correctly blocked delete waits
-- until the release that follows the probe.
deleteLockProbeMicros :: Int
deleteLockProbeMicros = 500 * 1000

-- | The executable bit's own representation, independent of any store.
--
-- Every assertion here is platform-agnostic on purpose: the module's whole
-- point is that a caller asks 'ExecBit' rather than the filesystem, so the
-- same questions must have the same answers on Windows and on Unix even
-- though the bit is kept in different places.
testExecBit :: IO [Bool]
testExecBit = do
  putStrLn "store/exec-bit"
  sequence
    [ -- The round-trip the representation exists for: what was marked
      -- reads back marked, and an unmarked sibling stays unmarked.
      runTestM "exec mark round-trips through isExecutable" $ do
        dir <- freshExecBitDir "roundtrip"
        let marked = dir </> "tool"
            plain = dir </> "data"
        BS.writeFile marked "#!x"
        BS.writeFile plain "hello"
        ExecBit.markExecutable marked
        isMarked <- ExecBit.isExecutable marked
        isPlain <- ExecBit.isExecutable plain
        forceRemoveIfExists dir
        pure $ case (isMarked, isPlain) of
          (True, False) -> Pass
          (False, _) -> Fail "a marked file did not read back executable"
          (_, True) -> Fail "an unmarked file read back executable",
      -- On Unix the flag is the owner-execute bit of the file's own mode,
      -- as upstream's dump reads it, whatever the group's and others' bits.
      runTestM "on Unix the flag is the owner-execute bit of the file's mode" $
        if SI.os == "mingw32"
          then pure Pass
          else do
            dir <- freshExecBitDir "owner-bit"
            let file = dir </> "f"
            BS.writeFile file "x"
            flags <- forM ["644", "744", "611", "100", "755"] $ \mode -> do
              Proc.callProcess "/bin/chmod" [mode, file]
              (,) mode <$> ExecBit.isExecutable file
            Proc.callProcess "/bin/chmod" ["644", file]
            forceRemoveIfExists dir
            pure (assertEqual "flags" [("644", False), ("744", True), ("611", False), ("100", True), ("755", True)] flags),
      -- An access control entry on macOS can refuse the owner execution of
      -- a file whose mode allows it: access(2) then says no, and the flag
      -- is still the mode's.
      runTestM "on macOS a file whose access control refuses execution keeps its mode's flag" $
        if SI.os /= "darwin"
          then pure Pass
          else do
            dir <- freshExecBitDir "acl"
            let file = dir </> "f"
            BS.writeFile file "#!x"
            Proc.callProcess "/bin/chmod" ["755", file]
            Proc.callProcess "/bin/chmod" ["+a", "everyone deny execute", file]
            refused <- not . Dir.executable <$> Dir.getPermissions file
            flag <- ExecBit.isExecutable file
            Proc.callProcess "/bin/chmod" ["-N", file]
            forceRemoveIfExists dir
            pure (assertEqual "access refuses, the flag stays" (True, True) (refused, flag)),
      -- copyExecMark out of a sealed store path.  canonicalizeMetadata is
      -- what placeInStore does, and on Windows copyFile propagates that
      -- attribute to the copy, where a mark cannot be written until it is
      -- cleared again.  The source must also survive read-only.
      runTestM "exec mark survives a copy out of a read-only source" $ do
        dir <- freshExecBitDir "copy"
        let src = dir </> "tool"
            dest = dir </> "tool-copy"
        BS.writeFile src "#!x"
        ExecBit.markExecutable src
        canonicalizeMetadata src
        Dir.copyFile src dest
        ExecBit.copyExecMark src dest
        copied <- ExecBit.isExecutable dest
        srcStillExec <- ExecBit.isExecutable src
        -- Unseal before cleanup: a read-only file resists removal on
        -- Windows the same way it resists a stream write.
        Dir.getPermissions src >>= Dir.setPermissions src . Dir.setOwnerWritable True
        forceRemoveIfExists dir
        pure $ case (copied, srcStillExec) of
          (True, True) -> Pass
          (False, _) -> Fail "the copy lost its exec mark"
          (_, False) -> Fail "sealing the source dropped its exec mark",
      -- The serialiser reads the mark, not the file's permissions: this is
      -- the flag a NAR carries, and on Windows getPermissions would answer
      -- from the extension instead.
      runTestM "serialiseFromPath reports the marked flag" $ do
        dir <- freshExecBitDir "serialise"
        let marked = dir </> "tool"
        BS.writeFile marked "#!x"
        BS.writeFile (dir </> "data") "hello"
        ExecBit.markExecutable marked
        entry <- ExecBit.serialiseFromPath processCaseHack dir
        forceRemoveIfExists dir
        pure $ case entry of
          NAR.NarDirectory entries ->
            assertEqual
              "flags of (data, tool)"
              [("data", False), ("tool", True)]
              [(n, e) | (n, NAR.NarRegular e _) <- entries]
          other -> Fail ("expected a directory entry, got: " <> T.pack (show other)),
      -- The sink and the verifier must read one source of truth.  A
      -- substituted tree is written through markExecutable and its hash
      -- rechecked on disk, so if narHashOfPath consulted the filesystem's
      -- own idea of executability instead, every path holding a marked
      -- file would be downloaded, rejected and deleted.
      runTestM "narHashOfPath agrees with the serialiser it verifies" $ do
        dir <- freshExecBitDir "hash"
        let marked = dir </> "tool"
        BS.writeFile marked "#!x"
        BS.writeFile (dir </> "data") "hello"
        ExecBit.markExecutable marked
        streamed <- ExecBit.narHashOfPath processCaseHack dir
        fromEntry <- NAR.narHash <$> ExecBit.serialiseFromPath processCaseHack dir
        -- A control on the same axis: an unmarked tree must hash
        -- differently, or agreement above would prove nothing.
        unmarkedDir <- freshExecBitDir "hash-plain"
        BS.writeFile (unmarkedDir </> "tool") "#!x"
        BS.writeFile (unmarkedDir </> "data") "hello"
        unmarked <- ExecBit.narHashOfPath processCaseHack unmarkedDir
        forceRemoveIfExists dir
        forceRemoveIfExists unmarkedDir
        pure $
          if streamed /= fromEntry
            then Fail "the verifier's hash disagrees with the serialiser's"
            else
              if streamed == unmarked
                then Fail "the exec mark did not reach the hash at all"
                else Pass,
      -- correct/ recurses by name, and a non-ASCII name must survive that
      -- recursion: decoding it a byte at a time names a file that is not
      -- there and the mark silently reads as absent.
      runTestM "a non-ASCII directory name keeps its child's mark" $ do
        dir <- freshExecBitDir "utf8"
        let sub = dir </> "\955-dir"
            marked = sub </> "\955-tool"
        createDirectoryIfMissing True sub
        BS.writeFile marked "#!x"
        ExecBit.markExecutable marked
        entry <- ExecBit.serialiseFromPath processCaseHack dir
        forceRemoveIfExists dir
        pure $ case entry of
          NAR.NarDirectory [(_, NAR.NarDirectory [(_, NAR.NarRegular True _)])] -> Pass
          other -> Fail ("expected one marked child, got: " <> T.pack (show other))
    ]

-- | An empty scratch directory for one 'testExecBit' case.
freshExecBitDir :: String -> IO FilePath
freshExecBitDir name = do
  tmpBase <- getTemporaryDirectory
  let dir = tmpBase </> ("nova-nix-test-execbit-" ++ name)
  forceRemoveIfExists dir
  createDirectoryIfMissing True dir
  pure dir

testPathLocks :: IO [Bool]
testPathLocks = do
  putStrLn "store/path-locks"
  sequence
    [ -- Two independent handles on one path lock exclude each other -
      -- the guarantee flock and LockFileEx share, and exactly the shape
      -- of two processes contending for one store path.
      runTestM "path lock excludes a second handle" $ do
        tmpBase <- getTemporaryDirectory
        let lockRoot = tmpBase </> "nova-nix-test-lock-excl"
        forceRemoveIfExists lockRoot
        createDirectoryIfMissing True lockRoot
        let dir = StoreDir lockRoot
            sp = StorePath (T.replicate 32 "a") "lockee"
        held <- acquirePathLock testLogger dir sp
        second <- tryAcquirePathLock dir sp
        releasePathLock held
        third <- tryAcquirePathLock dir sp
        mapM_ releasePathLock second
        mapM_ releasePathLock third
        forceRemoveIfExists lockRoot
        pure $ case (second, third) of
          (Nothing, Just _) -> Pass
          (Just _, _) -> Fail "a second handle acquired a held lock"
          (Nothing, Nothing) -> Fail "release did not free the lock",
      -- The already-valid short-circuit: a path registered by the time
      -- its substitute is fetched is adopted as SubstAlreadyValid with no
      -- NAR request and no disk writes, and the lock is released.
      runTestM "fetching a substitute adopts an already-valid path untouched, asking for no NAR" $ do
        let sp = StorePath (T.replicate 32 "c") "validpath"
        case servedPaths [(sp, [])] of
          Left err -> pure (Fail (T.pack err))
          Right (key, serve) ->
            withHttpServer (\target _ -> serve target) $ \base requests _ -> do
              tmpBase <- getTemporaryDirectory
              let tmpStore = tmpBase </> "nova-nix-test-lock-valid"
                  destPath = storePathToFilePath (StoreDir tmpStore) sp
              forceRemoveIfExists tmpStore
              store <- openStore testLogger (StoreDir tmpStore)
              createDirectoryIfMissing True destPath
              BS.writeFile (destPath </> "payload") "registered bytes"
              reg <- registrationFor store sp Nothing []
              registerPath (stDB store) reg
              result <- offeredThenFetched defaultFetchRetryPolicy (ioRetryEffects testLogger) store [Subst.CacheConfig base [key] 40] sp
              survivor <- BS.readFile (destPath </> "payload")
              -- The short-circuit released the lock, so the path must be
              -- lockable again at once.
              reLock <- tryAcquirePathLock (StoreDir tmpStore) sp
              mapM_ releasePathLock reLock
              closeStore store
              forceRemoveIfExists tmpStore
              seen <- requests
              pure $ case (result, reLock) of
                (Subst.SubstAlreadyValid, Just _)
                  | survivor /= "registered bytes" -> Fail "tree touched during already-valid adoption"
                  | seen /= [narInfoRequest sp] -> Fail ("expected the narinfo request alone, got: " <> T.pack (show seen))
                  | otherwise -> Pass
                (Subst.SubstAlreadyValid, Nothing) -> Fail "lock still held after already-valid return"
                (other, _) -> Fail ("expected SubstAlreadyValid, got: " <> T.pack (show other)),
      -- The strict pipeline honors the same short-circuit: fed a
      -- DIFFERENT (self-consistent) NAR for an already-valid path, it
      -- must adopt the registered tree rather than overwrite it.
      runTestM "unpackAndVerify adopts an already-valid path untouched" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-lock-valid-strict"
        forceRemoveIfExists tmpStore
        store <- openStore testLogger (StoreDir tmpStore)
        let sp = StorePath (T.replicate 32 "d") "strictvalid"
            destPath = storePathToFilePath (StoreDir tmpStore) sp
        createDirectoryIfMissing True destPath
        BS.writeFile (destPath </> "payload") "registered bytes"
        reg <- registrationFor store sp Nothing []
        registerPath (stDB store) reg
        let differentNar = NAR.serialise (NAR.NarDirectory [("other", NAR.NarRegular False "different bytes")])
        result <- Subst.unpackAndVerify store sp (lockTestNarInfo sp differentNar) differentNar
        survivor <- BS.readFile (destPath </> "payload")
        closeStore store
        forceRemoveIfExists tmpStore
        pure $ case result of
          Subst.SubstAlreadyValid
            | survivor == "registered bytes" -> Pass
            | otherwise -> Fail "tree touched during already-valid adoption"
          other -> Fail ("expected SubstAlreadyValid, got: " <> T.pack (show other)),
      -- The race the locks exist for: two concurrent substitutions of
      -- one path, independent lock handles.  Exactly one materializes;
      -- the other waits at the lock and adopts the winner's registered
      -- path; the surviving tree is intact and registered once.
      runTestM "concurrent substitutions: one materializes, one adopts" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-lock-race"
        forceRemoveIfExists tmpStore
        store <- openStore testLogger (StoreDir tmpStore)
        let sp = StorePath (T.replicate 32 "e") "racepath"
            destPath = storePathToFilePath (StoreDir tmpStore) sp
            rawNar = NAR.serialise (NAR.NarDirectory [("data", NAR.NarRegular False "race payload")])
            -- The caller's contract per attempt: register under the
            -- still-held lock, then release it.
            attempt = do
              result <- Subst.unpackAndVerify store sp (lockTestNarInfo sp rawNar) rawNar
              case result of
                Subst.SubstSuccess winnerReg lock -> do
                  registerPath (stDB store) winnerReg
                  releasePathLock lock
                _ -> pure ()
              pure result
        firstDone <- newEmptyMVar
        secondDone <- newEmptyMVar
        _ <- forkIO ((try attempt :: IO (Either SomeException Subst.SubstResult)) >>= putMVar firstDone)
        _ <- forkIO ((try attempt :: IO (Either SomeException Subst.SubstResult)) >>= putMVar secondDone)
        outcomeA <- timeout raceWatchdogMicros (takeMVar firstDone)
        outcomeB <- timeout raceWatchdogMicros (takeMVar secondDone)
        rowCount <- length <$> queryAllValidPaths (stDB store)
        onDisk <- NAR.serialiseFromPath destPath
        closeStore store
        forceRemoveIfExists tmpStore
        pure $ case (outcomeA, outcomeB) of
          (Just (Right resultA), Just (Right resultB)) ->
            let outcomes = [resultA, resultB]
                materialized = length [() | Subst.SubstSuccess _ _ <- outcomes]
                adopted = length [() | Subst.SubstAlreadyValid <- outcomes]
             in if materialized == 1 && adopted == 1 && rowCount == 1 && NAR.serialise onDisk == rawNar
                  then Pass
                  else
                    Fail
                      ( "unexpected race outcome: "
                          <> T.pack (show outcomes)
                          <> ", registered rows: "
                          <> T.pack (show rowCount)
                      )
          other -> Fail ("a race attempt hung or threw: " <> T.pack (show other))
    ]
  where
    lockTestNarInfo sp rawNar =
      NarInfo.NarInfo
        { NarInfo.niStorePath = storePathToText defaultStoreDir sp,
          NarInfo.niUrl = "nar/lock-test.nar",
          NarInfo.niCompression = "none",
          NarInfo.niFileHash = Nothing,
          NarInfo.niFileSize = Nothing,
          NarInfo.niNarHash = CHash.formatNixHash (CHash.hashBytes rawNar),
          NarInfo.niNarSize = fromIntegral (BS.length rawNar),
          NarInfo.niReferences = [],
          NarInfo.niDeriver = Nothing,
          NarInfo.niSigs = [],
          NarInfo.niCA = Nothing
        }

-- ---------------------------------------------------------------------------
-- Tests: Store.DB (Phase 2, Batch 1)
-- ---------------------------------------------------------------------------

-- | Helper: run a test with a temporary store DB, cleaning up after.
withTempStoreDB :: (StoreDir -> IO [Bool]) -> IO [Bool]
withTempStoreDB action = do
  tmpBase <- getTemporaryDirectory
  let tmpStore = tmpBase </> "nova-nix-test-store-db"
  removeIfExists tmpStore
  createDirectoryIfMissing True tmpStore
  results <- action (StoreDir tmpStore)
  removeIfExists tmpStore
  pure results

-- | Names a directory on a case-sensitive volume for
-- 'testCaseSensitiveVolumeIO'.  A Linux temp directory qualifies on
-- its own; on macOS a "Case-sensitive APFS" image attached with
-- hdiutil does, and the default volume does not.
caseSensitiveDirVar :: String
caseSensitiveDirVar = "NOVA_NIX_TEST_CASE_SENSITIVE_DIR"

-- | True-name materialization on a volume that does not fold: a store
-- opened there probes sensitive, and a NAR carrying @Makefile@ and
-- @makefile@ unpacks to two files under their own names through the
-- strict and the streaming substituter paths alike.  A directory named
-- by 'caseSensitiveDirVar' is held to the claim: one that is missing
-- or probes folding FAILS, since the variable is how a run asserts a
-- volume (macOS CI attaches an image for it) and a typo must not read
-- as "no volume available".  With the variable unset the temp
-- directory is tried, and skips where it folds.
testCaseSensitiveVolumeIO :: IO [Bool]
testCaseSensitiveVolumeIO = do
  putStrLn "store/case-sensitive-volume"
  named <- lookupEnv caseSensitiveDirVar
  case named of
    Just base -> do
      verdict <- namedDirVerdict base
      case verdict of
        Right () -> testCaseSensitiveVolumeBody base
        Left reason ->
          (: []) <$> runTest (T.pack caseSensitiveDirVar <> " names a case-sensitive directory") (Fail reason)
    Nothing -> do
      tmpBase <- getTemporaryDirectory
      sensitivity <- probeCaseSensitivity tmpBase
      case sensitivity of
        CaseInsensitive -> do
          putStrLn ("  SKIP  needs a case-sensitive volume (" ++ caseSensitiveDirVar ++ " names a directory on one)")
          pure []
        CaseSensitive -> testCaseSensitiveVolumeBody tmpBase
  where
    namedDirVerdict base = do
      present <- doesDirectoryExist base
      if not present
        then pure (Left (T.pack base <> " does not exist"))
        else do
          sensitivity <- probeCaseSensitivity base
          pure $ case sensitivity of
            CaseSensitive -> Right ()
            CaseInsensitive -> Left (T.pack base <> " probes " <> T.pack (show CaseInsensitive))

testCaseSensitiveVolumeBody :: FilePath -> IO [Bool]
testCaseSensitiveVolumeBody base = do
  let storeRoot = base </> "nova-nix-test-cs-store"
      storeDir = StoreDir storeRoot
      tree =
        NAR.NarDirectory
          [ ("Makefile", NAR.NarRegular False "upper"),
            ("makefile", NAR.NarRegular False "lower")
          ]
      rawNar = NAR.serialise tree
      digest = CHash.hashBytes rawNar
      narInfoOf nar sp =
        NarInfo.NarInfo
          { NarInfo.niStorePath = storePathToText defaultStoreDir sp,
            NarInfo.niUrl = "nar/case.nar",
            NarInfo.niCompression = "none",
            NarInfo.niFileHash = Nothing,
            NarInfo.niFileSize = Nothing,
            NarInfo.niNarHash = CHash.formatNixHash (CHash.hashBytes nar),
            NarInfo.niNarSize = fromIntegral (BS.length nar),
            NarInfo.niReferences = [],
            NarInfo.niDeriver = Nothing,
            NarInfo.niSigs = [],
            NarInfo.niCA = Nothing
          }
      narInfoFor = narInfoOf rawNar
      -- A name carrying the case-hack suffix beside the name it would
      -- strip to: read with the suffix stripped, the two collide.
      suffixTree =
        NAR.NarDirectory
          [ ("Makefile", NAR.NarRegular False "upper"),
            ("Makefile~nix~case~hack~1", NAR.NarRegular False "literal")
          ]
      suffixNar = NAR.serialise suffixTree
      suffixNames sp = do
        names <- sort <$> Dir.listDirectory (storePathToFilePath storeDir sp)
        pure (assertEqual "on-disk names" ["Makefile", "Makefile~nix~case~hack~1"] names)
      -- Both spellings present with their own contents, and nothing
      -- carrying the case-hack suffix.
      trueNames sp = do
        let dest = storePathToFilePath storeDir sp
        names <- Dir.listDirectory dest
        upper <- BS.readFile (dest </> "Makefile")
        lower <- BS.readFile (dest </> "makefile")
        pure $
          if sort names == ["Makefile", "makefile"] && upper == "upper" && lower == "lower"
            then Pass
            else Fail ("on disk: " <> T.pack (show names))
  forceRemoveIfExists storeRoot
  store <- openStore testLogger storeDir
  results <-
    sequence
      [ runTest "the store probes its volume as case-sensitive" $
          assertEqual "stCaseSensitivity" CaseSensitive (stCaseSensitivity store),
        runTestM "strict substitution materializes true names" $ do
          let sp = StorePath (T.replicate 32 "c") "case-strict"
          result <- Subst.unpackAndVerify store sp (narInfoFor sp) rawNar
          case result of
            Subst.SubstSuccess _ lock -> do
              releasePathLock lock
              trueNames sp
            other -> pure (Fail ("substitution failed: " <> T.pack (show other))),
        runTestM "streaming substitution materializes true names" $ do
          let sp = StorePath (T.replicate 32 "d") "case-stream"
          source <- chunkReader [rawNar]
          result <- Subst.materializeNarFromSource store sp (narInfoFor sp) digest [] Nothing source
          case result of
            Right _ -> trueNames sp
            Left err -> pure (Fail ("streaming substitution failed: " <> Subst.attemptFailureMessage err)),
        -- Nothing on a volume that keeps names apart is ever hacked, so
        -- a name carrying the suffix is a name like any other: both
        -- pipelines accept it, and their on-disk rechecks read it back
        -- as spelled.
        runTestM "a name carrying the case-hack suffix substitutes verbatim, strict" $ do
          let sp = StorePath (T.replicate 32 "e") "suffix-strict"
          result <- Subst.unpackAndVerify store sp (narInfoOf suffixNar sp) suffixNar
          case result of
            Subst.SubstSuccess _ lock -> do
              releasePathLock lock
              suffixNames sp
            other -> pure (Fail ("substitution failed: " <> T.pack (show other))),
        runTestM "a name carrying the case-hack suffix substitutes verbatim, streaming" $ do
          let sp = StorePath (T.replicate 32 "f") "suffix-stream"
          source <- chunkReader [suffixNar]
          result <- Subst.materializeNarFromSource store sp (narInfoOf suffixNar sp) (CHash.hashBytes suffixNar) [] Nothing source
          case result of
            Right _ -> suffixNames sp
            Left err -> pure (Fail ("streaming substitution failed: " <> Subst.attemptFailureMessage err)),
        -- A store path already on this volume is read back as spelled
        -- too: an evaluation write holding the name reproduces its
        -- address and registers the NAR upstream dumps for it with
        -- use-case-hack off ('suffixPairNarHash').
        runTestM "a store path naming a file with the case-hack suffix registers under that name" $
          case makeFixedOutputPath "suffix-write" "sha256" NarIngestion (sha256Digest suffixNar) of
            Left err -> pure (Fail ("test store path rejected: " <> T.pack (show err)))
            Right sp -> do
              let dest = storePathToFilePath storeDir sp
              createDirectoryIfMissing True dest
              BS.writeFile (dest </> "Makefile") "upper"
              BS.writeFile (dest </> "Makefile~nix~case~hack~1") "literal"
              materializeEvalStoreWrites store (Map.singleton (storePathToText defaultStoreDir sp) ([], WriteRecursive))
              info <- queryPathInfo (stDB store) sp
              pure $ case info of
                Just row -> assertEqual "registered NAR hash" suffixPairNarHash (piNarHash row)
                Nothing -> Fail "not registered",
        -- Evaluation reads a store path as the store does: copying one
        -- holding the name hashes it as spelled, where a source read
        -- would follow the process and, on macOS, strip it.
        runTestM "builtins.path reads a store path under its volume's case-hack mode" $ do
          let held = StorePath (T.replicate 32 "g") "suffix-read"
              heldPath = storePathToFilePath storeDir held
          createDirectoryIfMissing True heldPath
          BS.writeFile (heldPath </> "Makefile") "upper"
          BS.writeFile (heldPath </> "Makefile~nix~case~hack~1") "literal"
          copied <-
            evalNixIOStore
              storeDir
              "."
              ("builtins.path { path = " <> storePathToText defaultStoreDir held <> "; name = \"suffix-copy\"; }")
          let expected = storePathToText defaultStoreDir <$> makeFixedOutputPath "suffix-copy" "sha256" NarIngestion (sha256Digest suffixNar)
          case (copied, expected) of
            (Right (VStr pathBytes _), Right expectedPath)
              | bytesText pathBytes /= expectedPath -> pure (Fail ("copied to " <> bytesText pathBytes <> ", expected " <> expectedPath))
              | otherwise -> maybe (pure (Fail "unparseable store path")) suffixNames (parseStorePath defaultStoreDir expectedPath)
            (Left err, _) -> pure (Fail ("eval failed: " <> err))
            other -> pure (Fail ("unexpected result: " <> T.pack (show other))),
        -- A source tree copied off this volume into one that folds the
        -- sharp-s pair: the two names are distinct here and one name
        -- there, so the copy refuses the second at the write rather
        -- than landing it on the first.
        runTestM "a copy into a volume that folds a pair refuses the later name" $ do
          probed <- tempFoldsSharpS
          case probed of
            Left reason -> Pass <$ putStrLn ("  SKIP  " ++ reason)
            Right tmpBase -> do
              let src = base </> "nova-nix-test-cs-copy-src"
                  dest = tmpBase </> "nova-nix-test-cs-copy-dest"
                  contents = Map.fromList [(T.unpack sharpSUpper, "upper"), (T.unpack sharpSLower, "lower")]
              forceRemoveIfExists src
              Subst.clearStaleDestination dest
              createDirectoryIfMissing True src
              mapM_ (\(name, bytes) -> BS.writeFile (src </> name) bytes) (Map.toList contents)
              -- The copy walks the listing's order, which the volume
              -- chooses, so the refused name is whichever comes second.
              listed <- Dir.listDirectory src
              outcome <- try (copyPathInto src dest)
              landed <- traverse (\name -> (,) name <$> BS.readFile (dest </> name)) (take 1 listed)
              forceRemoveIfExists src
              Subst.clearStaleDestination dest
              pure $ case (listed, outcome) of
                ([earlier, later], Left (e :: IOException))
                  | T.pack (ioeGetErrorString e) /= "creating file '" <> quotedAsCxx (dest </> later) <> "': File exists" ->
                      Fail ("unexpected refusal: " <> T.pack (show e))
                  | landed /= [(earlier, Map.findWithDefault "" earlier contents)] ->
                      Fail ("the earlier entry was overwritten: " <> T.pack (show landed))
                  | otherwise -> Pass
                (_, Right ()) -> Fail ("the copy succeeded; on disk: " <> T.pack (show landed))
                other -> Fail ("unexpected listing or outcome: " <> T.pack (show other)),
        -- A source holding a case-only pair, built against a store
        -- whose volume folds: evaluation names the two-file tree, and
        -- the store holds it as upstream's restore does, the later
        -- name case-hacked, registered under that address.
        runTestM "a coerced source's case-only pair is stored case-hacked on a folding store" $
          caseOnlyPairBuild base canonPathValue,
        runTestM "builtins.path stores a case-only pair case-hacked on a folding store" $
          caseOnlyPairBuild base (\srcPath -> "builtins.path { path = " <> canonPathValue srcPath <> "; }")
      ]
  closeStore store
  forceRemoveIfExists storeRoot
  pure results

-- | What upstream records for 'caseOnlyPairBuild''s source on a
-- folding volume: nix-instantiate 2.33.2 with
-- @--store 'local?root=...'@ adds it at this path, @nix-store
-- --query --hash@ and @--size@ report this NAR hash and size, and the
-- tree holds @Makefile@ and @makefile~nix~case~hack~1@.
caseOnlyPairPath :: StorePath
caseOnlyPairPath = StorePath "1rn1vqr2y9f65qg5vflbfr5gkwllmp22" "src"

caseOnlyPairNarHash :: Text
caseOnlyPairNarHash = "sha256:04z1mpa501269w2aqjr3kq5k4y85hjzbisvfc3jgmw3dnd7va9hl"

caseOnlyPairNarSize :: Int
caseOnlyPairNarSize = 480

-- | Build a derivation whose @src@ is the given expression over a
-- source directory named @src@ holding @Makefile@ and @makefile@, on a
-- case-sensitive base, against a store in the temp directory, and
-- hold the result to what upstream stores ('caseOnlyPairPath').  Skips
-- where the temp directory's volume does not fold.
caseOnlyPairBuild :: FilePath -> (Text -> Text) -> IO TestResult
caseOnlyPairBuild base srcExpr = do
  tmpBase <- getTemporaryDirectory
  sensitivity <- probeCaseSensitivity tmpBase
  case sensitivity of
    CaseSensitive -> Pass <$ putStrLn "  SKIP  the temp directory's volume does not fold case"
    CaseInsensitive -> do
      shell <- findTestShell
      let srcRoot = base </> "nova-nix-test-cs-pair"
          srcDir = srcRoot </> "src"
          tmpStore = tmpBase </> "nova-nix-test-cs-pair-store"
          nixEscape = T.concatMap (\c -> if c == '\\' then "\\\\" else if c == '"' then "\\\"" else T.singleton c)
          source =
            T.concat
              [ "derivation { name = \"pair\"; system = builtins.currentSystem; ",
                "builder = \"" <> nixEscape shell <> "\"; ",
                "args = [\"-c\" \"cd $src && for f in *; do echo \\\"$f\\\"; done > $out\"]; ",
                "src = " <> srcExpr (T.pack srcDir) <> "; }"
              ]
      forceRemoveIfExists srcRoot
      forceRemoveIfExists tmpStore
      createDirectoryIfMissing True srcDir
      BS.writeFile (srcDir </> "Makefile") "upper\n"
      BS.writeFile (srcDir </> "makefile") "lower\n"
      built <- evalAndBuild (StoreDir tmpStore) source
      verdict <- case built of
        Left err -> pure (Fail err)
        Right (BuildFailure msg _, store) -> Fail msg <$ closeStore store
        Right (BuildSuccess outSP, store) -> do
          let stored = storePathToFilePath (stDir store) caseOnlyPairPath
          names <- sort <$> Dir.listDirectory stored
          contents <- traverse (BS.readFile . (stored </>)) names
          info <- queryPathInfo (stDB store) caseOnlyPairPath
          seen <- BS.readFile (storePathToFilePath (stDir store) outSP)
          closeStore store
          let hacked = ["Makefile", "makefile~nix~case~hack~1"]
          pure $ case info of
            Nothing -> Fail "the source path is not registered"
            Just row
              | names /= hacked || contents /= ["upper\n", "lower\n"] ->
                  Fail ("stored as " <> T.pack (show (zip names contents)))
              | piNarHash row /= caseOnlyPairNarHash || piNarSize row /= caseOnlyPairNarSize ->
                  Fail ("registered " <> piNarHash row <> " / " <> T.pack (show (piNarSize row)))
              | seen /= "Makefile\nmakefile~nix~case~hack~1\n" ->
                  Fail ("the builder saw " <> T.pack (show seen))
              | otherwise -> Pass
      forceRemoveIfExists srcRoot
      forceRemoveIfExists tmpStore
      pure verdict

-- | The NAR hash of a directory holding @Makefile@ (@upper@) and
-- @Makefile~nix~case~hack~1@ (@literal@): @nix-store --option
-- use-case-hack false --dump@ 2.33.2 piped to @nix-hash --flat@.  With
-- the setting at its macOS default the dump strips the suffix and
-- fails on the collision.
suffixPairNarHash :: Text
suffixPairNarHash = "sha256:1x31cqjvkihkh3wbzq00z8xplwbf7i0bpk3g9vqs403lh3f5b4gs"

-- | U+1E9E (capital sharp s) and U+00DF (small sharp s): a pair APFS
-- folds to one name and 'onDiskNameKey' keeps apart, since 'toUpper'
-- leaves U+00DF as it is.  Escaped, as the source is ASCII.
sharpSUpper, sharpSLower :: Text
sharpSUpper = "STRA\x1E9E\&E"
sharpSLower = "stra\xDF\&e"

-- | Whether the volume holding a directory folds the sharp-s pair,
-- answered by the volume itself: one spelling is created and the other
-- looked up.  The store's probe says whether a volume folds case at
-- all, not which pairs its table folds.
sharpSFolds :: FilePath -> IO Bool
sharpSFolds dir = do
  let probeDir = dir </> "nova-nix-test-sharp-s-probe"
  Subst.clearStaleDestination probeDir
  createDirectoryIfMissing True probeDir
  BS.writeFile (probeDir </> T.unpack sharpSUpper) "probe"
  folds <- Dir.doesFileExist (probeDir </> T.unpack sharpSLower)
  Subst.clearStaleDestination probeDir
  pure folds

-- | Whether the suite's temp directory folds the sharp-s pair: the
-- store's own probe first, then the volume for the pair itself.
tempFoldsSharpS :: IO (Either String FilePath)
tempFoldsSharpS = do
  tmpBase <- getTemporaryDirectory
  sensitivity <- probeCaseSensitivity tmpBase
  case sensitivity of
    CaseSensitive -> pure (Left "the temp directory's volume does not fold case")
    CaseInsensitive -> do
      folds <- sharpSFolds tmpBase
      pure $
        if folds
          then Right tmpBase
          else Left "the temp directory's volume folds case but keeps U+1E9E and U+00DF apart"

-- | A sibling pair the volume folds but 'onDiskNameKey' keeps apart.
-- The exclusive create refuses the later entry at the write, in
-- upstream's wording and naming it, through both unpack paths, and
-- the earlier entry keeps its own content: nothing lands on it, and
-- two directories do not merge.  A link needed no new exclusivity
-- (link creation never replaces a name); its refusal is pinned
-- alongside.  Skips with the reason where the temp directory's volume
-- does not fold the pair.
testFoldingVolumeIO :: IO [Bool]
testFoldingVolumeIO = do
  putStrLn "store/folding-volume"
  probed <- tempFoldsSharpS
  case probed of
    Left reason -> do
      putStrLn ("  SKIP  " ++ reason)
      pure []
    Right tmpBase -> testFoldingVolumeBody (tmpBase </> "nova-nix-test-folding")

testFoldingVolumeBody :: FilePath -> IO [Bool]
testFoldingVolumeBody root = do
  let upperName = T.unpack sharpSUpper
      lowerName = T.unpack sharpSLower
      pair earlier later =
        NAR.NarDirectory [(TE.encodeUtf8 sharpSUpper, earlier), (TE.encodeUtf8 sharpSLower, later)]
      filePair = pair (NAR.NarRegular False "upper") (NAR.NarRegular False "lower")
      linkPair = pair (NAR.NarRegular False "upper") (NAR.NarSymlink "a")
      dirPair =
        pair
          (NAR.NarDirectory [("a", NAR.NarRegular False "upper")])
          (NAR.NarDirectory [("b", NAR.NarRegular False "lower")])
      dirOverFilePair = pair (NAR.NarRegular False "upper") (NAR.NarDirectory [("b", NAR.NarRegular False "lower")])
      fileRefusal dest = "creating file '" <> quotedAsCxx (dest </> lowerName) <> "': File exists"
      dirRefusal dest = "path '" <> T.pack (dest </> lowerName) <> "' already exists"
      dirOverFileRefusal dest = "creating directory '" <> T.pack (dest </> lowerName) <> "': File exists"
      linkRefusal dest = "creating symlink from '" <> T.pack (dest </> lowerName) <> "' -> 'a': File exists"
      upperFileKept dest = (== "upper") <$> BS.readFile (dest </> upperName)
      upperDirKept dest = (== ["a"]) <$> Dir.listDirectory (dest </> upperName)
      narInfoOf raw =
        NarInfo.NarInfo
          { NarInfo.niStorePath = "/nix/store/" <> T.replicate 32 "f" <> "-folding",
            NarInfo.niUrl = "nar/folding.nar",
            NarInfo.niCompression = "none",
            NarInfo.niFileHash = Nothing,
            NarInfo.niFileSize = Nothing,
            NarInfo.niNarHash = CHash.formatNixHash (CHash.hashBytes raw),
            NarInfo.niNarSize = fromIntegral (BS.length raw),
            NarInfo.niReferences = [],
            NarInfo.niDeriver = Nothing,
            NarInfo.niSigs = [],
            NarInfo.niCA = Nothing
          }
      strictly = unpackNarEntry CaseInsensitive
      streamed dest tree = do
        let raw = NAR.serialise tree
        source <- chunkReader [raw]
        outcome <- Subst.consumeNarStream CaseInsensitive dest (narInfoOf raw) (CHash.hashBytes raw) source
        pure (either (Left . Subst.attemptFailureMessage) (const (Right ())) outcome)
      refusedAtWrite label unpack tree refusal kept = runTestM label $ do
        let dest = root </> "out"
        Subst.clearStaleDestination dest
        result <- unpack dest tree
        earlierKept <- kept dest
        Subst.clearStaleDestination dest
        pure $
          if earlierKept
            then assertEqual "unpack outcome" (Left (refusal dest)) result
            else Fail ("the earlier entry was overwritten or merged into; unpack returned " <> T.pack (show result))
  forceRemoveIfExists root
  createDirectoryIfMissing True root
  results <-
    sequence
      [ refusedAtWrite "strict unpack refuses the later file of a folded pair" strictly filePair fileRefusal upperFileKept,
        refusedAtWrite "streaming unpack refuses the later file of a folded pair" streamed filePair fileRefusal upperFileKept,
        refusedAtWrite "strict unpack refuses the later directory of a folded pair" strictly dirPair dirRefusal upperDirKept,
        refusedAtWrite "streaming unpack refuses the later directory of a folded pair" streamed dirPair dirRefusal upperDirKept,
        refusedAtWrite "strict unpack refuses a directory folding onto a file" strictly dirOverFilePair dirOverFileRefusal upperFileKept,
        refusedAtWrite "streaming unpack refuses a directory folding onto a file" streamed dirOverFilePair dirOverFileRefusal upperFileKept,
        refusedAtWrite "a link folding onto an earlier file is refused at the link" strictly linkPair linkRefusal upperFileKept
      ]
  forceRemoveIfExists root
  pure results

testStoreDB :: IO [Bool]
testStoreDB = do
  putStrLn "store/db"
  withTempStoreDB $ \storeDir ->
    sequence
      [ -- open and close without error
        runTestM "db open/close" $ do
          db <- openStoreDB storeDir
          closeStoreDB db
          pure Pass,
        -- isValidPath returns False for unknown path
        runTestM "db isValidPath false for unknown" $ do
          db <- openStoreDB storeDir
          result <- isValidPath db (StorePath "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1" "unknown")
          closeStoreDB db
          pure (assertEqual "unknown" False result),
        -- register + isValidPath returns True
        runTestM "db register + isValid" $ do
          db <- openStoreDB storeDir
          let sp = StorePath "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa2" "hello"
              reg = PathRegistration sp "sha256:abc" 100 Nothing []
          registerPath db reg
          result <- isValidPath db sp
          closeStoreDB db
          pure (assertEqual "registered" True result),
        -- A peer's held write transaction must be waited out, not died
        -- on: SQLite's default busy timeout is zero, so registration
        -- would crash with an uncaught ErrorBusy SQLError while a
        -- second process is mid-commit.  Upstream waits
        -- (sqlite3_busy_timeout, one hour); this pins that we do too.
        runTestM "db registration waits out a busy peer" $ do
          db <- openStoreDB storeDir
          let dbPath = unStoreDir storeDir </> metaDirName </> dbFileName
          peer <- SQL.open dbPath
          SQL.execute_ peer "BEGIN IMMEDIATE"
          released <- newEmptyMVar
          _ <- forkIO $ do
            threadDelay 300000
            SQL.execute_ peer "COMMIT"
            SQL.close peer
            putMVar released ()
          let sp = StorePath "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa3" "busywait"
          registerPath db (PathRegistration sp "sha256:busy" 10 Nothing [])
          takeMVar released
          result <- isValidPath db sp
          closeStoreDB db
          pure (assertEqual "registered after the peer released" True result),
        -- register with refs + query
        runTestM "db register with refs + query" $ do
          db <- openStoreDB storeDir
          let ref1 = StorePath "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" "dep1"
              ref2 = StorePath "cccccccccccccccccccccccccccccccc" "dep2"
              mainSp = StorePath "dddddddddddddddddddddddddddddddd" "mainpkg"
          registerPath db (PathRegistration ref1 "sha256:r1" 50 Nothing [])
          registerPath db (PathRegistration ref2 "sha256:r2" 60 Nothing [])
          registerPath db (PathRegistration mainSp "sha256:m1" 200 Nothing [ref1, ref2])
          refs <- queryReferences db mainSp
          closeStoreDB db
          let ref1Path = T.pack (storePathToFilePath storeDir ref1)
              ref2Path = T.pack (storePathToFilePath storeDir ref2)
              hasRef1 = ref1Path `elem` refs
              hasRef2 = ref2Path `elem` refs
          pure $
            if hasRef1 && hasRef2
              then Pass
              else Fail ("expected refs to contain both deps, got: " <> T.pack (show refs)),
        -- Re-registration replaces the edge set: the refresh contract
        -- covers references too, and a union would over-report into
        -- pushed narinfos.
        runTestM "db re-register replaces refs" $ do
          db <- openStoreDB storeDir
          let refA = StorePath "gggggggggggggggggggggggggggggggg" "refresh-a"
              refB = StorePath "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh" "refresh-b"
              sp = StorePath "iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii" "refresh-main"
          registerPaths
            db
            [ PathRegistration refA "sha256:ra" 10 Nothing [],
              PathRegistration refB "sha256:rb" 11 Nothing [],
              PathRegistration sp "sha256:rm" 12 Nothing [refA, refB]
            ]
          registerPath db (PathRegistration sp "sha256:rm" 12 Nothing [refA])
          refs <- queryReferences db sp
          closeStoreDB db
          let refAPath = T.pack (storePathToFilePath storeDir refA)
              refBPath = T.pack (storePathToFilePath storeDir refB)
          pure $
            if refAPath `elem` refs && refBPath `notElem` refs
              then Pass
              else Fail ("expected only refresh-a after re-register, got: " <> T.pack (show refs)),
        -- A reference to an unregistered path is a loud error: a silently
        -- dropped edge under-reports narinfos and hands a future GC
        -- permission to delete a live dependency.  The refusal is
        -- upstream's queryValidPathId message, naming the reference under
        -- /nix/store though this store is kept elsewhere, and the batch
        -- records nothing.
        runTestM "db register with an unregistered ref is refused in upstream's words" $ do
          db <- openStoreDB storeDir
          let ghost = StorePath "jjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjj" "ghost"
              sp = StorePath "kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk" "orphan-edge"
          outcome <- try (registerPath db (PathRegistration sp "sha256:oe" 13 Nothing [ghost]))
          recorded <- isValidPath db sp
          closeStoreDB db
          pure $ case outcome of
            Left (err :: IOException) ->
              assertEqual
                "refusal"
                ("path '/nix/store/jjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjj-ghost' is not valid", False)
                (ioeGetErrorString err, recorded)
            Right () -> Fail "expected registration to throw on an unregistered reference",
        -- Of several unregistered references the refusal names the one
        -- upstream's does: the batch in path order, then each path's
        -- references in path order.  The list order would name ghost-a,
        -- and so would the least unregistered path overall.
        runTestM "db refusal names the first unregistered ref in upstream's order" $ do
          db <- openStoreDB storeDir
          let ghostA = StorePath "22222222222222222222222222222222" "ghost-a"
              ghostB = StorePath "33333333333333333333333333333333" "ghost-b"
              ghostC = StorePath "44444444444444444444444444444444" "ghost-c"
              early = StorePath "mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm" "early"
              late = StorePath "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz" "late"
          outcome <-
            try
              ( registerPaths
                  db
                  [ PathRegistration late "sha256:lt" 14 Nothing [ghostA],
                    PathRegistration early "sha256:er" 15 Nothing [ghostC, ghostB]
                  ]
              )
          closeStoreDB db
          pure $ case outcome of
            Left (err :: IOException) ->
              assertEqual
                "refusal"
                "path '/nix/store/33333333333333333333333333333333-ghost-b' is not valid"
                (ioeGetErrorString err)
            Right () -> Fail "expected registration to throw on an unregistered reference",
        -- queryDeriver nothing
        runTestM "db queryDeriver nothing" $ do
          db <- openStoreDB storeDir
          let sp = StorePath "cccccccccccccccccccccccccccccccc" "noderiver"
          registerPath db (PathRegistration sp "sha256:nd" 80 Nothing [])
          result <- queryDeriver db sp
          closeStoreDB db
          pure (assertEqual "no deriver" Nothing result),
        -- queryDeriver just
        runTestM "db queryDeriver just" $ do
          db <- openStoreDB storeDir
          let sp = StorePath "ffffffffffffffffffffffffffffffff" "hasdrv"
          registerPath db (PathRegistration sp "sha256:hd" 90 (Just "/nix/store/xxx.drv") [])
          result <- queryDeriver db sp
          closeStoreDB db
          pure (assertEqual "has deriver" (Just "/nix/store/xxx.drv") result),
        -- queryPathInfo
        runTestM "db queryPathInfo" $ do
          db <- openStoreDB storeDir
          let sp = StorePath "gggggggggggggggggggggggggggggggg" "infotest"
          registerPath db (PathRegistration sp "sha256:info" 150 (Just "/drv") [])
          minfo <- queryPathInfo db sp
          closeStoreDB db
          pure $ case minfo of
            Nothing -> Fail "expected PathInfo but got Nothing"
            Just info ->
              if piNarHash info == "sha256:info"
                && piNarSize info == 150
                && piDeriver info == Just "/drv"
                then Pass
                else Fail ("bad PathInfo: " <> T.pack (show info)),
        -- queryPathInfo for unknown returns Nothing
        runTestM "db queryPathInfo unknown" $ do
          db <- openStoreDB storeDir
          minfo <- queryPathInfo db (StorePath "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh" "nope")
          closeStoreDB db
          pure (assertEqual "no info" Nothing minfo),
        -- double register is idempotent
        runTestM "db double register idempotent" $ do
          db <- openStoreDB storeDir
          let sp = StorePath "iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii" "double"
              reg = PathRegistration sp "sha256:dup" 120 Nothing []
          registerPath db reg
          registerPath db reg
          result <- isValidPath db sp
          closeStoreDB db
          pure (assertEqual "still valid" True result),
        -- multi-path reference graph
        runTestM "db multi-path reference graph" $ do
          db <- openStoreDB storeDir
          let spA = StorePath "jjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjj" "a"
              spB = StorePath "kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk" "b"
              spC = StorePath "llllllllllllllllllllllllllllllll" "c"
          registerPath db (PathRegistration spA "sha256:a" 10 Nothing [])
          registerPath db (PathRegistration spB "sha256:b" 20 Nothing [spA])
          registerPath db (PathRegistration spC "sha256:c" 30 Nothing [spA, spB])
          refsC <- queryReferences db spC
          refsA <- queryReferences db spA
          closeStoreDB db
          let aPath = T.pack (storePathToFilePath storeDir spA)
              bPath = T.pack (storePathToFilePath storeDir spB)
          pure $
            if length refsC == 2 && aPath `elem` refsC && bPath `elem` refsC && null refsA
              then Pass
              else Fail ("bad ref graph: c refs=" <> T.pack (show refsC) <> " a refs=" <> T.pack (show refsA))
      ]

-- ---------------------------------------------------------------------------
-- Tests: Store Operations + parseStorePath (Phase 2, Batch 2)
-- ---------------------------------------------------------------------------

testParseStorePath :: IO [Bool]
testParseStorePath = do
  putStrLn "store/parseStorePath"
  let sd = defaultStoreDir
  sequence
    [ runTest "parse valid store path" $
        assertEqual
          "valid"
          (Just (StorePath "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "hello-2.12"))
          (parseStorePath sd "/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-hello-2.12"),
      runTest "parse missing prefix" $
        assertEqual "no prefix" Nothing (parseStorePath sd "/tmp/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-hello"),
      runTest "parse too short hash" $
        assertEqual "short hash" Nothing (parseStorePath sd "/nix/store/aaa-hello"),
      runTest "parse missing dash after hash" $
        assertEqual "no dash" Nothing (parseStorePath sd "/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaahello"),
      runTest "parse empty name" $
        assertEqual "empty name" Nothing (parseStorePath sd "/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-"),
      runTest "parse windows store path" $
        assertEqual
          "windows"
          (Just (StorePath "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" "pkg"))
          (parseStorePath windowsStoreDir "C:\\nix\\store/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb-pkg"),
      -- The backslash separator is the form storePathToFilePath actually
      -- produces on Windows (what %out% and DB rows look like there).
      runTest "parse windows store path with backslash separator" $
        assertEqual
          "windows-backslash"
          (Just (StorePath "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" "pkg"))
          (parseStorePath windowsStoreDir "C:\\nix\\store\\bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb-pkg"),
      -- Charset gates (upstream's parse-boundary checks): the hash slot
      -- accepts only nix-base32 (no e o u t), and the name only
      -- [A-Za-z0-9+._?=-] - parsed text can come from a cache, and an
      -- unchecked component would later become a filesystem path.
      runTest "parse rejects non-base32 hash" $
        assertEqual "e-hash" Nothing (parseStorePath sd ("/nix/store/" <> T.replicate 32 "e" <> "-hello")),
      runTest "parse rejects traversal text in hash slot" $
        assertEqual "traversal-hash" Nothing (parseStorePath sd ("/nix/store/" <> T.replicate 16 ".\\" <> "-x")),
      runTest "parse rejects separator in name" $
        assertEqual "sep-name" Nothing (parseStorePath sd "/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-he/llo"),
      runTest "parse rejects dot-dot name" $
        assertEqual "dotdot-name" Nothing (parseStorePath sd "/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-.."),
      runTest "parse rejects overlong name" $
        assertEqual "overlong-name" Nothing (parseStorePath sd ("/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-" <> T.replicate 212 "x")),
      runTest "parse accepts the full name charset" $
        assertEqual
          "name-specials"
          (Just (StorePath "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "gcc-13.2.0_pre+x?="))
          (parseStorePath sd "/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-gcc-13.2.0_pre+x?="),
      -- The first dash-separated component of a name may not be . or ..
      -- (upstream checkName): the traversal names' .- / ..- prefixed forms
      -- are rejected while other dot-leading names stay valid.
      runTest "parse rejects a .- first component" $
        assertEqual "dot-dash" Nothing (parseStorePath sd "/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-.-cfg"),
      runTest "parse rejects a ..- first component" $
        assertEqual "dotdot-dash" Nothing (parseStorePath sd "/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-..-cfg"),
      runTest "parse accepts a dot-leading name" $
        assertEqual
          "dot-leading"
          (Just (StorePath "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ".config-1.0"))
          (parseStorePath sd "/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-.config-1.0")
    ]

-- ---------------------------------------------------------------------------
-- Tests: Push (pure narinfo construction + closure computation)
-- ---------------------------------------------------------------------------

-- | Who a directory the store creates is open to, and when one already
-- there may hold a store or lead to one ("Nix.Store.Access").  The SDDL
-- each is created with on Windows is checked on every platform, and so
-- are the directories a store's open checks, install-nova-nix's rule
-- over an owner and DACL, the security descriptor that rule reads, and
-- the refusal's words.  On Windows a new store's directories, the
-- parents made for them and a build directory are read back through the
-- API: protected, with nothing inherited, and granting what
-- install-nova-nix grants on @C:\\nix@, that less Users' entry, or the
-- user alone, so no entry names Authenticated Users or Everyone.  A
-- store nova-nix made opens again, and one whose parent another account
-- owns, or whose parent's DACL lets another account change it, is
-- refused naming that parent.
testStoreAccessIO :: IO [Bool]
testStoreAccessIO = do
  putStrLn "store/access"
  tmpBase <- getTemporaryDirectory
  let scratch label = tmpBase </> ("nova-nix-test-store-access-" <> label)
      fresh label = bracket_ (forceRemoveIfExists (scratch label)) (forceRemoveIfExists (scratch label))
      someUser = "S-1-5-21-1-2-3-1001"
      -- The SDDL a directory of the access gets for the user, or the
      -- refusal, which no expected SDDL equals.
      sddlFor user access = maybe ("refused: " <> user) (`accessSddl` access) (sidFromText user)
      -- An SDDL's entries, each as its fields, when it is a protected
      -- DACL of entries and nothing else.
      sddlEntries sddl = map (T.splitOn ";") . T.splitOn ")(" <$> (T.stripPrefix "D:P(" sddl >>= T.stripSuffix ")")
      sddlEntry rights sid = ["A", "OICI", rights, "", "", sid]
      trustedEntries user =
        [sddlEntry "0x1f01ff" user | user /= localSystem]
          <> [sddlEntry "0x1f01ff" localSystem, sddlEntry "0x1f01ff" "S-1-5-32-544"]
      intendedEntries user access = case access of
        StoreAccess -> trustedEntries user <> [sddlEntry "0x1200a9" "S-1-5-32-545"]
        ParentAccess -> trustedEntries user
        OwnerAccess -> [sddlEntry "0x1f01ff" user]
      -- Every access renders the accepted SID as the trustee of exactly
      -- the entries it intends, and nothing else.
      rendersIntended user sid =
        QC.conjoin [sddlEntries (accessSddl sid access) QC.=== Just (intendedEntries user access) | access <- [StoreAccess, ParentAccess, OwnerAccess]]
      decimalField = T.pack . show <$> (QC.arbitrary :: QC.Gen Word32)
      -- An authority of 2^32 or more, as ConvertSidToStringSidW writes
      -- one: its top digit is not zero.
      wideAuthority = do
        top <- QC.elements "123456789abcdefABCDEF"
        rest <- QC.vectorOf 11 (QC.elements "0123456789abcdefABCDEF")
        pure (T.pack ("0x" <> (top : rest)))
      genSid = do
        authority <- QC.frequency [(8, decimalField), (1, wideAuthority), (1, pure "5")]
        count <- QC.choose (1, 15)
        subauthorities <- QC.vectorOf count decimalField
        QC.frequency [(9, pure ("S-1-" <> authority <> foldMap ("-" <>) subauthorities)), (1, pure localSystem)]
      -- A SID with something put into it: an SDDL entry, a separator,
      -- a digit or a letter, anywhere in it.
      genAltered = do
        sid <- genSid
        position <- QC.choose (0, T.length sid)
        inserted <- QC.elements [")(A;OICI;FA;;;WD", ")", "(", ";", "-", "0", "7", "x", "S", " ", "a"]
        pure (T.take position sid <> inserted <> T.drop position sid)
      property prop = do
        result <- QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} prop
        pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))
      -- FILE_ALL_ACCESS, and FILE_GENERIC_READ | FILE_GENERIC_EXECUTE.
      fullControl = 0x1F01FF
      readAndExecute = 0x1200A9
      localSystem = "S-1-5-18"
      -- OBJECT_INHERIT_ACE | CONTAINER_INHERIT_ACE, without
      -- INHERITED_ACE: passed down to what is made below, and not taken
      -- from the parent.
      passedDown = 0x3
      allow sid mask = DaclEntry 0 passedDown mask (Just sid)
      parentEntries user =
        [allow user fullControl | user /= localSystem]
          <> [allow localSystem fullControl, allow "S-1-5-32-544" fullControl]
      storeEntries user = parentEntries user <> [allow "S-1-5-32-545" readAndExecute]
      -- Entries compare as a set: their order grants nothing.
      carries label expected path = do
        dacl <- directoryDacl path
        pure $ case dacl of
          Nothing -> Fail (label <> ": no DACL was read")
          Just found -> assertEqual label (Dacl True (sort expected)) (Dacl (daclProtected found) (sort (daclEntries found)))
      onWindows name check
        | SI.os /= "mingw32" = True <$ putStrLn ("  SKIP  " <> T.unpack name <> ": a POSIX directory has no DACL")
        | otherwise = runTestM name $ do
            user <- processUserSid
            maybe (pure (Fail "no user SID was read")) check user
      otherUser = "S-1-5-21-1-2-3-1002"
      administrators = "S-1-5-32-544"
      users = "S-1-5-32-545"
      authenticatedUsers = "S-1-5-11"
      creatorOwner = "S-1-3-0"
      everyone = "S-1-1-0"
      localService = "S-1-5-19"
      -- Modify, FILE_GENERIC_READ | FILE_GENERIC_WRITE |
      -- FILE_GENERIC_EXECUTE | DELETE, which a client edition's drive
      -- root passes down to Authenticated Users.
      modify = 0x1301BF
      -- GENERIC_READ | GENERIC_EXECUTE, which C:\Users passes down to
      -- Everyone.
      genericReadExecute = 0xA0000000
      -- Each right that lets an account change a directory, by its
      -- winnt.h name.
      changingRights =
        [ ("FILE_WRITE_DATA", 0x2),
          ("FILE_APPEND_DATA", 0x4),
          ("FILE_WRITE_EA", 0x10),
          ("FILE_DELETE_CHILD", 0x40),
          ("FILE_WRITE_ATTRIBUTES", 0x100),
          ("DELETE", 0x10000),
          ("WRITE_DAC", 0x40000),
          ("WRITE_OWNER", 0x80000),
          ("GENERIC_ALL", 0x10000000),
          ("GENERIC_WRITE", 0x40000000)
        ]
      allowing rights sid = AllowEntry rights <$> sidFromText sid
      installerEntries = [allowing fullControl someUser, allowing fullControl localSystem, allowing fullControl administrators, allowing readAndExecute users]
      -- The decision for someUser over an owner and a DACL, their SIDs
      -- given as text, which is Nothing when one is not a SID.
      decision owner entries = do
        user <- sidFromText someUser
        security <- DirSecurity <$> traverse sidFromText owner <*> traverse sequenceA entries
        pure (securityDistrust user security)
      decides label owner entries expected =
        runTest label (fromMaybe (Fail "a SID in the case is not one") (assertEqual label <$> expected <*> decision owner entries))
      accepted = Just Nothing
      refusedAs reason = Just <$> reason
      writableBy sid = refusedAs (WritableBy <$> sidFromText sid)
      -- A SID, a DACL and a self-relative security descriptor laid out
      -- as MS-DTYP gives them.  An entry is its type, flags and size,
      -- then its access mask and SID where its type puts them.
      le16 :: Word16 -> BS.ByteString
      le16 value = BS.pack [fromIntegral value, fromIntegral (value `shiftR` 8)]
      le32 :: Word32 -> BS.ByteString
      le32 value = BS.pack [fromIntegral (value `shiftR` shift) | shift <- [0, 8, 16, 24]]
      sidBytes text = do
        rest <- T.stripPrefix "S-1-" text
        (authority, subauthorities) <- case T.splitOn "-" rest of
          first : more -> Just (first, more)
          [] -> Nothing
        value <- readMaybe (T.unpack authority) :: Maybe Integer
        subs <- traverse (readMaybe . T.unpack) subauthorities
        pure (BS.pack (1 : fromIntegral (length subs) : [fromIntegral (value `shiftR` shift) | shift <- [40, 32 .. 0]]) <> foldMap le32 subs)
      entryOf kind flags body = BS.pack [kind, flags] <> le16 (fromIntegral (4 + BS.length body)) <> body
      common kind flags mask sid trailing = entryOf kind flags (le32 mask <> sid <> trailing)
      compound flags mask sid = entryOf 0x4 flags (le32 mask <> le16 1 <> le16 0 <> sid)
      object kind flags mask objectFlags sid = entryOf kind flags (le32 mask <> le32 objectFlags <> BS.replicate (16 * popCount objectFlags) 0xAB <> sid)
      aclOf entries = BS.pack [2, 0] <> le16 (fromIntegral (8 + sum (map BS.length entries))) <> le16 (fromIntegral (length entries)) <> le16 0 <> mconcat entries
      -- The DACL is Nothing for a descriptor that says it has none, Just
      -- Nothing for a null one, and otherwise the DACL's bytes.
      descriptorOf owner dacl =
        let ownerBytes = fromMaybe BS.empty owner
            daclBytes = fromMaybe BS.empty (join dacl)
            offsetOf part at = if BS.null part then 0 else at
         in BS.pack [1, 0]
              <> le16 (0x8000 .|. maybe 0 (const 0x4) dacl)
              <> le32 (offsetOf ownerBytes 20)
              <> le32 0
              <> le32 0
              <> le32 (offsetOf daclBytes (20 + fromIntegral (BS.length ownerBytes)))
              <> ownerBytes
              <> daclBytes
      -- SIDs spelled byte by byte, apart from sidBytes.
      systemBytes = BS.pack [1, 1, 0, 0, 0, 0, 0, 5, 18, 0, 0, 0]
      administratorsBytes = BS.pack [1, 2, 0, 0, 0, 0, 0, 5, 32, 0, 0, 0, 0x20, 0x02, 0, 0]
      authenticatedBytes = BS.pack [1, 1, 0, 0, 0, 0, 0, 5, 11, 0, 0, 0]
      creatorOwnerBytes = BS.pack [1, 1, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0]
      userBytes = BS.pack [1, 5, 0, 0, 0, 0, 0, 5, 21, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 0xE9, 0x03, 0, 0]
      -- Owned by Administrators, with a DACL of one entry, full control
      -- for SYSTEM passed down (OICI), spelled byte by byte.
      fixture =
        BS.pack
          ( [1, 0, 0x04, 0x80, 20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 0, 0, 0]
              <> [1, 2, 0, 0, 0, 0, 0, 5, 32, 0, 0, 0, 0x20, 0x02, 0, 0]
              <> [2, 0, 28, 0, 1, 0, 0, 0]
              <> [0, 0x03, 20, 0, 0xFF, 0x01, 0x1F, 0, 1, 1, 0, 0, 0, 0, 0, 5, 18, 0, 0, 0]
          )
      setByte index value bytes = BS.take index bytes <> BS.singleton value <> BS.drop (index + 1) bytes
      ownedByUser dacl = descriptorOf (Just userBytes) (Just (Just (aclOf dacl)))
      -- The kinds of entry each layout holds, spelled apart from the
      -- library's tables.
      commonKinds = [0x0, 0x1, 0x2, 0x3, 0x9, 0xA, 0xD, 0xE]
      objectKinds = [0x5, 0x6, 0x7, 0x8, 0xB, 0xC, 0xF, 0x10]
      allowKinds = [0x0, 0x4, 0x5, 0x9, 0xB]
      -- An entry's bytes and what it is read as, for a generated type,
      -- flags, mask, object flags and SID.
      encodedEntry (kind, flags, mask, objectFlags, sidText) = do
        sid <- sidBytes sidText
        named <- sidFromText sidText
        let meaning
              | kind `elem` allowKinds = AllowEntry mask named
              | kind `elem` (0x4 : commonKinds <> objectKinds) = OtherEntry
              | otherwise = UnknownEntry kind
            bytes
              | kind == 0x4 = compound flags mask sid
              | kind `elem` objectKinds = object kind flags mask objectFlags sid
              | otherwise = common kind flags mask sid BS.empty
        pure (bytes, meaning)
      genEntry = (,,,,) <$> QC.choose (0, 0x14) <*> QC.arbitrary <*> QC.arbitrary <*> QC.choose (0, 3) <*> genSid
      genDacl = QC.oneof [pure Nothing, pure (Just Nothing), Just . Just <$> QC.resize 6 (QC.listOf genEntry)]
      -- A generated descriptor's bytes and what they are read as.
      encodedDescriptor (ownerText, daclSpec) = do
        owner <- traverse sidBytes ownerText
        ownerSid <- traverse sidFromText ownerText
        entries <- traverse (traverse (traverse encodedEntry)) daclSpec
        let dacl = fmap (fmap (aclOf . map fst)) entries
        pure (descriptorOf owner dacl, DirSecurity ownerSid (fmap (map snd) =<< entries))
      -- Open the store again, and expect it refused for the directory,
      -- for the reason given.
      refusedFor root dir reason = do
        store <- unStoreDir <$> canonicalStoreDir (StoreDir root)
        outcome <- try (openStore testLogger (StoreDir root) >>= closeStore)
        pure $ case (reason, outcome) of
          (Nothing, _) -> Fail "the reason names no SID"
          (Just why, Left err)
            | isUserError err -> assertEqual "the refusal" (distrustMessage dir store why) (ioeGetErrorString err)
            | otherwise -> Fail ("expected a refusal, got: " <> T.pack (show (err :: IOException)))
          (Just _, Right ()) -> Fail "the store was opened"
      -- icacls changes a DACL or an owner, as another account's tools
      -- would; its exit code says whether it could.
      icacls args = do
        (code, out, err) <- Proc.readProcessWithExitCode "icacls" args ""
        pure (code, out <> err)
      -- nova-nix makes the store, and icacls changes its parent.
      madeThenChanged label args = do
        let root = scratch label </> "nix" </> "store"
        openStore testLogger (StoreDir root) >>= closeStore
        parent <- takeDirectory . unStoreDir <$> canonicalStoreDir (StoreDir root)
        (code, said) <- icacls (parent : args)
        pure (root, parent, code, said)
  sequence
    [ runTest "a store directory's DACL is protected, with full control for the user, SYSTEM and Administrators and read and execute for Users, each passed down" $
        assertEqual
          "SDDL"
          "D:P(A;OICI;0x1f01ff;;;S-1-5-21-1-2-3-1001)(A;OICI;0x1f01ff;;;S-1-5-18)(A;OICI;0x1f01ff;;;S-1-5-32-544)(A;OICI;0x1200a9;;;S-1-5-32-545)"
          (sddlFor someUser StoreAccess),
      runTest "a parent made for a store has a protected DACL with full control for the user, SYSTEM and Administrators alone, passed down" $
        assertEqual
          "SDDL"
          "D:P(A;OICI;0x1f01ff;;;S-1-5-21-1-2-3-1001)(A;OICI;0x1f01ff;;;S-1-5-18)(A;OICI;0x1f01ff;;;S-1-5-32-544)"
          (sddlFor someUser ParentAccess),
      runTest "a store directory's DACL names SYSTEM once when nova-nix runs as SYSTEM" $
        assertEqual
          "SDDL"
          "D:P(A;OICI;0x1f01ff;;;S-1-5-18)(A;OICI;0x1f01ff;;;S-1-5-32-544)(A;OICI;0x1200a9;;;S-1-5-32-545)"
          (sddlFor localSystem StoreAccess),
      runTest "a build directory's DACL is protected, with full control for the user alone, passed down" $
        assertEqual "SDDL" "D:P(A;OICI;0x1f01ff;;;S-1-5-21-1-2-3-1001)" (sddlFor someUser OwnerAccess),
      runTest "a SID of the form S-1-I-S... is accepted, its authority in decimal or wide hexadecimal" $
        assertEqual
          "refused"
          []
          ( filter
              (isNothing . sidFromText)
              [ "S-1-5-18",
                "S-1-5-32-544",
                "S-1-5-21-1-2-3-1001",
                "S-1-0-0",
                "S-1-5-4294967295",
                "S-1-4294967295-1",
                "S-1-0x123456789abc-1",
                "S-1-0x123456789ABC-1",
                "S-1-5" <> T.replicate 15 "-1"
              ]
          ),
      -- The first is how a trustee would smuggle an Everyone entry into
      -- the DACL.
      runTest "anything else is refused as a SID, an SDDL entry put after one among them" $
        assertEqual
          "accepted"
          []
          ( filter
              (isJust . sidFromText)
              [ "S-1-5-21-1-2-3-1001)(A;OICI;FA;;;WD",
                "x)(A;OICI;FA;;;WD",
                "",
                "S-1-5",
                "S-1-5-",
                "S-1-5--18",
                "S-1-5-018",
                "S-1-05-18",
                "S-1-5-+18",
                "s-1-5-18",
                "S-2-5-18",
                " S-1-5-18",
                "S-1-5-18 ",
                "S-1-4294967296-1",
                "S-1-5-4294967296",
                "S-1-0x000000000005-18",
                "S-1-0x12345678abc-1",
                "S-1-0x12345678abcg-1",
                "S-1-5" <> T.replicate 16 "-1"
              ]
          ),
      runTestM "every SID of the form S-1-I-S... is accepted and is the trustee of exactly the entries each access intends" $
        property $
          QC.forAll genSid $ \user ->
            maybe (QC.counterexample ("refused " <> T.unpack user) False) (rendersIntended user) (sidFromText user),
      runTestM "whatever is accepted as a SID, altered or arbitrary, is the trustee of exactly the entries each access intends" $
        property $
          QC.forAll (QC.oneof [genAltered, T.pack <$> QC.arbitrary]) $ \text ->
            maybe (QC.property True) (rendersIntended text) (sidFromText text),
      -- Two parents are missing, as C:\nix is at a drive root without
      -- the installer, and one is made below the other.
      onWindows "a new store's directories and the parents made for them carry a protected DACL with no entry for Authenticated Users or Everyone" $ \user ->
        fresh "new" $
          bracket (openStore testLogger (StoreDir (scratch "new" </> "nix" </> "store"))) closeStore $ \store -> do
            let root = unStoreDir (stDir store)
            outer <- carries "the outer parent" (parentEntries user) (scratch "new")
            inner <- carries "the inner parent" (parentEntries user) (scratch "new" </> "nix")
            storeDir <- carries "the store directory" (storeEntries user) root
            stateDir <- carries "the state directory" (storeEntries user) (root </> metaDirName)
            pure (outer `andThen` inner `andThen` storeDir `andThen` stateDir),
      onWindows "a build directory carries a protected DACL granting the user alone" $ \user ->
        fresh "build" $ do
          createDirectoryIfMissing True (scratch "build")
          let dir = scratch "build" </> "dir"
          createBuildDir dir
          carries "the build directory" [allow user fullControl] dir,
      -- The open creates the store directory over the file and stops
      -- there, the error naming the store directory.
      onWindows "a file where the store directory would be fails the open with an already-exists error naming it" $ \_ ->
        fresh "file" $ do
          createDirectoryIfMissing True (scratch "file")
          let root = scratch "file" </> "store"
          BS.writeFile root "not a store"
          outcome <- try (openStore testLogger (StoreDir root) >>= closeStore)
          expected <- unStoreDir <$> canonicalStoreDir (StoreDir root)
          pure $ case outcome of
            Left err
              | isAlreadyExistsError err -> assertEqual "the path the error names" (Just expected) (ioeGetFileName err)
              | otherwise -> Fail ("expected an already-exists error, got: " <> T.pack (show (err :: IOException)))
            Right () -> Fail "opened a store over a file",
      -- The directory is made as the test's temporary directories are,
      -- so it takes what its parent passes down; opening a store there
      -- leaves that as it is.
      onWindows "a store directory that already exists keeps its access control list" $ \_ ->
        fresh "existing" $ do
          let root = scratch "existing" </> "store"
          createDirectoryIfMissing True root
          before <- directoryDacl root
          openStore testLogger (StoreDir root) >>= closeStore
          after <- directoryDacl root
          pure $
            assertEqual "protected before the open" (Just False) (daclProtected <$> before)
              `andThen` assertEqual "the DACL after the open" before after,
      runTest "the directories down to a store are those below its volume root, outermost first, the store's made with StoreAccess and its parents with ParentAccess" $
        assertEqual "the default store" [("C:\\nix", ParentAccess), ("C:\\nix\\store", StoreAccess)] (storeDirectories "C:\\nix\\store")
          `andThen` assertEqual
            "a store further down"
            [("D:\\a", ParentAccess), ("D:\\a\\_temp", ParentAccess), ("D:\\a\\_temp\\store", StoreAccess)]
            (storeDirectories "D:\\a\\_temp\\store")
          `andThen` assertEqual "a store at a drive root" [] (storeDirectories "E:\\")
          `andThen` assertEqual "a trailing separator" [("C:\\nix", ParentAccess), ("C:\\nix\\store", StoreAccess)] (storeDirectories "C:\\nix\\store\\"),
      runTest "the directories a store's open checks are those down to the store, then its state directory, made with StoreAccess" $
        assertEqual
          "the default store"
          [("C:\\nix", ParentAccess), ("C:\\nix\\store", StoreAccess), ("C:\\nix\\store\\.nova-nix", StoreAccess)]
          (stateDirectories "C:\\nix\\store\\.nova-nix")
          `andThen` assertEqual "a store at a drive root" [("E:\\.nova-nix", StoreAccess)] (stateDirectories "E:\\.nova-nix"),
      decides "install-nova-nix's DACL, on a directory the user owns, is trusted with a store" (Just someUser) (Just installerEntries) accepted,
      decides "a directory Administrators own, as one an elevated shell makes is, is trusted" (Just administrators) (Just installerEntries) accepted,
      decides "a directory SYSTEM owns is trusted" (Just localSystem) (Just installerEntries) accepted,
      decides "a directory another account owns is refused, naming its owner" (Just otherUser) (Just installerEntries) (refusedAs (OwnedBy . Just <$> sidFromText otherUser)),
      decides "a directory with no owner is refused" Nothing (Just installerEntries) (refusedAs (Just (OwnedBy Nothing))),
      decides "a directory with no DACL, or a null one, is refused" (Just someUser) Nothing (refusedAs (Just NoDacl)),
      decides "an empty DACL, which grants no account anything, is trusted" (Just someUser) (Just []) accepted,
      decides
        "the DACL a client edition's drive root passes down is refused for its modify for Authenticated Users"
        (Just someUser)
        (Just [allowing fullControl administrators, allowing fullControl localSystem, allowing readAndExecute users, allowing modify authenticatedUsers])
        (writableBy authenticatedUsers),
      decides
        "the DACL a server edition's drive root passes down is refused for its first grant to Users, to add a subdirectory"
        (Just administrators)
        (Just [allowing fullControl localSystem, allowing fullControl administrators, allowing readAndExecute users, allowing 0x4 users, allowing 0x2 users, allowing 0x10000000 creatorOwner])
        (writableBy users),
      decides
        "read and execute for Users and Everyone, and the generic read and execute C:\\Users passes down, change nothing and are trusted"
        (Just someUser)
        (Just (installerEntries <> [allowing readAndExecute everyone, allowing genericReadExecute everyone]))
        accepted,
      decides "a deny, audit or alarm entry, whatever it names, is trusted" (Just someUser) (Just (Just OtherEntry : installerEntries)) accepted,
      decides "an entry of a type nova-nix cannot read is refused" (Just someUser) (Just (installerEntries <> [Just (UnknownEntry 0x11)])) (refusedAs (Just UnreadableEntry)),
      decides
        "of two entries another account could change the directory through, the first is named"
        (Just someUser)
        (Just [allowing 0x40 authenticatedUsers, allowing fullControl users])
        (writableBy authenticatedUsers),
      runTest "each right that changes a directory, granted alone to another account, is refused, and every other right together is trusted" $
        foldr
          (\(name, rights) rest -> assertEqual name (writableBy authenticatedUsers) (decision (Just someUser) (Just [allowing rights authenticatedUsers])) `andThen` rest)
          Pass
          changingRights
          `andThen` assertEqual
            "every other right"
            accepted
            (decision (Just someUser) (Just [allowing (complement (foldr ((.|.) . snd) 0 changingRights)) authenticatedUsers])),
      runTest "one SID is trusted whatever the case of its wide authority's hexadecimal digits" $
        fromMaybe (Fail "not a SID") $ do
          user <- sidFromText "S-1-0x123456789abc-1"
          spelled <- sidFromText "S-1-0x123456789ABC-1"
          pure (assertEqual "the decision" Nothing (securityDistrust user (DirSecurity (Just spelled) (Just [AllowEntry fullControl spelled])))),
      -- What nova-nix creates a directory with, read back from the SDDL
      -- it writes.
      runTestM "a directory nova-nix creates, owned by the user, is trusted with a store, for every user and access" $
        let readBack user sddl = do
              entries <- sddlEntries sddl
              DirSecurity (Just user) . Just <$> traverse allowEntryOf entries
            allowEntryOf fields = case fields of
              ["A", "OICI", rights, "", "", trustee] -> AllowEntry <$> readMaybe (T.unpack rights) <*> sidFromText trustee
              _ -> Nothing
         in property $
              QC.forAll genSid $ \text -> case sidFromText text of
                Nothing -> QC.counterexample ("refused " <> T.unpack text) False
                Just user ->
                  QC.conjoin
                    [ QC.counterexample (show access) (fmap (securityDistrust user) (readBack user (accessSddl user access)) QC.=== Just Nothing)
                    | access <- [StoreAccess, ParentAccess, OwnerAccess]
                    ],
      runTest "a security descriptor is read as MS-DTYP lays it out" $
        fromMaybe (Fail "not a SID") $ do
          owner <- sidFromText administrators
          system <- sidFromText localSystem
          pure $
            assertEqual "the fixture" (Just (DirSecurity (Just owner) (Just [AllowEntry fullControl system]))) (parseSecurityDescriptor fixture)
              `andThen` assertEqual "the fixture as built" fixture (descriptorOf (Just administratorsBytes) (Just (Just (aclOf [common 0x0 0x03 fullControl systemBytes BS.empty])))),
      runTest "an inherit-only entry granting another account a change is refused" $
        fromMaybe (Fail "not a SID") $ do
          user <- sidFromText someUser
          grantee <- sidFromText creatorOwner
          -- OBJECT_INHERIT_ACE | CONTAINER_INHERIT_ACE | INHERIT_ONLY_ACE
          pure (assertEqual "the decision" (Just (WritableBy grantee)) (descriptorDistrust user (ownedByUser [common 0x0 0x0B 0x10000000 creatorOwnerBytes BS.empty]))),
      runTest "a null DACL, a descriptor that says it has no DACL, and one with no owner are refused" $
        fromMaybe (Fail "not a SID") $ do
          user <- sidFromText someUser
          pure $
            assertEqual "a null DACL" (Just NoDacl) (descriptorDistrust user (descriptorOf (Just userBytes) (Just Nothing)))
              `andThen` assertEqual "no DACL" (Just NoDacl) (descriptorDistrust user (descriptorOf (Just userBytes) Nothing))
              `andThen` assertEqual "no owner" (Just (OwnedBy Nothing)) (descriptorDistrust user (descriptorOf Nothing (Just (Just (aclOf []))))),
      runTest "an allow entry of every kind is read, past a callback's data and an object entry's GUIDs" $
        fromMaybe (Fail "not a SID") $ do
          user <- sidFromText someUser
          grantee <- sidFromText authenticatedUsers
          pure $
            assertEqual
              "the entries"
              (Just (DirSecurity (Just user) (Just (replicate 5 (AllowEntry 0x2 grantee)))))
              ( parseSecurityDescriptor
                  ( ownedByUser
                      [ common 0x0 0x0 0x2 authenticatedBytes BS.empty,
                        common 0x9 0x0 0x2 authenticatedBytes "callback data",
                        compound 0x0 0x2 authenticatedBytes,
                        object 0x5 0x0 0x2 3 authenticatedBytes,
                        object 0xB 0x0 0x2 1 authenticatedBytes
                      ]
                  )
              ),
      runTest "a deny, audit or alarm entry of every kind grants nothing, and an entry of a later type, such as a mandatory label, is unreadable" $
        fromMaybe (Fail "not a SID") $ do
          user <- sidFromText someUser
          let granting kind
                | kind `elem` objectKinds = object kind 0x0 0xFFFFFFFF 2 authenticatedBytes
                | otherwise = common kind 0x0 0xFFFFFFFF authenticatedBytes BS.empty
              restricting = [0x1, 0x2, 0x3, 0xA, 0xD, 0xE, 0x6, 0x7, 0x8, 0xC, 0xF, 0x10]
          pure $
            assertEqual "the entries" (Just (DirSecurity (Just user) (Just (map (const OtherEntry) restricting)))) (parseSecurityDescriptor (ownedByUser (map granting restricting)))
              `andThen` assertEqual "a mandatory label" (Just UnreadableEntry) (descriptorDistrust user (ownedByUser [common 0x11 0x0 0x1 authenticatedBytes BS.empty])),
      runTest "bytes that are not a security descriptor are refused as unreadable" $
        fromMaybe (Fail "not a SID") $ do
          user <- sidFromText someUser
          let malformed =
                [ ("empty", BS.empty),
                  ("a header cut short", BS.take 19 fixture),
                  ("another revision", setByte 0 2 fixture),
                  ("not self-relative", setByte 3 0x00 fixture),
                  ("an owner past the end", setByte 4 200 fixture),
                  ("an ACL past the end", setByte 38 60 fixture),
                  ("an entry past its ACL", setByte 46 40 fixture),
                  ("an entry smaller than its header", setByte 46 2 fixture),
                  ("more entries than the ACL holds", setByte 40 2 fixture),
                  ("a SID of another revision", setByte 20 2 fixture),
                  ("a SID of sixteen subauthorities", descriptorOf (Just (BS.pack [1, 16, 0, 0, 0, 0, 0, 5] <> foldMap le32 (replicate 16 1))) Nothing)
                ]
          pure (foldr (\(name, bytes) rest -> assertEqual name (Just UnreadableDescriptor) (descriptorDistrust user bytes) `andThen` rest) Pass malformed),
      runTestM "a descriptor is read back as it was laid out, and no part of it cut short is read" $
        property $
          QC.forAll ((,) <$> QC.oneof [pure Nothing, Just <$> genSid] <*> genDacl) $ \spec -> case encodedDescriptor spec of
            Nothing -> QC.counterexample "a generated SID does not encode" False
            Just (bytes, expected) ->
              parseSecurityDescriptor bytes QC.=== Just expected
                QC..&&. QC.conjoin [QC.counterexample ("cut to " <> show size) (parseSecurityDescriptor (BS.take size bytes) QC.=== Nothing) | size <- [0 .. BS.length bytes - 1]],
      runTest "a refusal names the directory, what is wrong with it and the store, and says to move the directory aside" $
        fromMaybe (Fail "not a SID") $ do
          owner <- sidFromText otherUser
          grantee <- sidFromText authenticatedUsers
          let refusal = distrustMessage "C:\\nix" "C:\\nix\\store"
              advice = ", so the store in C:\\nix\\store cannot be trusted; move C:\\nix aside and let nova-nix or install-nova-nix create it"
          pure $
            assertEqual "another owner" ("C:\\nix belongs to S-1-5-21-1-2-3-1002, not to you, SYSTEM or Administrators" <> advice) (refusal (OwnedBy (Just owner)))
              `andThen` assertEqual "no owner" ("C:\\nix belongs to no one, not to you, SYSTEM or Administrators" <> advice) (refusal (OwnedBy Nothing))
              `andThen` assertEqual "no DACL" ("C:\\nix has no DACL, which lets every account change it" <> advice) (refusal NoDacl)
              `andThen` assertEqual "an unreadable entry" ("C:\\nix carries an access entry of a kind nova-nix cannot read" <> advice) (refusal UnreadableEntry)
              `andThen` assertEqual "a grant" ("C:\\nix lets S-1-5-11 change it" <> advice) (refusal (WritableBy grantee))
              `andThen` assertEqual "an unreadable descriptor" ("C:\\nix has a security descriptor nova-nix cannot read" <> advice) (refusal UnreadableDescriptor),
      -- The first open checks every directory above the store's parents
      -- in the temporary directory, which nova-nix did not make; the
      -- second checks those it made.
      onWindows "a store nova-nix made, and the parents it made for it, are trusted when it is opened again" $ \_ ->
        fresh "reopened" $ do
          let root = scratch "reopened" </> "nix" </> "store"
          openStore testLogger (StoreDir root) >>= closeStore
          outcome <- try (openStore testLogger (StoreDir root) >>= closeStore)
          pure (either (\err -> Fail ("the second open failed: " <> T.pack (show (err :: IOException)))) (const Pass) outcome),
      -- An evaluation's write can come before the store is opened: its
      -- directories are made as the open makes them, and the state
      -- directory, which only the open makes, is not.
      onWindows "a store an evaluation writes to first is made closed to other users, its parents too" $ \user ->
        fresh "eval-first" $ do
          let root = scratch "eval-first" </> "nix" </> "store"
          written <- evalNixIOStore (StoreDir root) tmpBase "builtins.toFile \"written\" \"by an evaluation\""
          outer <- carries "the outer parent" (parentEntries user) (scratch "eval-first")
          inner <- carries "the inner parent" (parentEntries user) (scratch "eval-first" </> "nix")
          storeDir <- carries "the store directory" (storeEntries user) root
          stateMade <- doesDirectoryExist (root </> metaDirName)
          pure $
            either (\err -> Fail ("the evaluation failed: " <> err)) (const Pass) written
              `andThen` outer
              `andThen` inner
              `andThen` storeDir
              `andThen` assertEqual "the state directory made" False stateMade,
      onWindows "an evaluation's write into a store whose parent lets Authenticated Users change it is refused in the open's words" $ \_ ->
        fresh "eval-refused" $ do
          (root, parent, code, said) <- madeThenChanged "eval-refused" ["/grant", "*" <> T.unpack authenticatedUsers <> ":(OI)(CI)(M)"]
          store <- unStoreDir <$> canonicalStoreDir (StoreDir root)
          written <- evalNixIOStore (StoreDir root) tmpBase "builtins.toFile \"written\" \"by an evaluation\""
          pure $ case (code, written, sidFromText authenticatedUsers) of
            (ExitFailure _, _, _) -> Fail ("icacls could not grant the right: " <> T.pack said)
            (ExitSuccess, Left err, Just grantee)
              | T.pack (distrustMessage parent store (WritableBy grantee)) `T.isInfixOf` err -> Pass
            (ExitSuccess, outcome, _) -> Fail ("expected the write refused, got: " <> T.pack (show outcome)),
      -- LOCAL SERVICE stands for another user: giving a directory away
      -- takes the restore privilege an elevated shell holds.
      onWindows "a store whose parent another account owns is refused, naming the parent and its owner" $ \_ ->
        fresh "owned" $ do
          (root, parent, code, said) <- madeThenChanged "owned" ["/setowner", "*" <> T.unpack localService]
          case code of
            ExitFailure _ -> Pass <$ putStrLn ("  SKIP  giving a directory another owner needs an elevated shell: " <> said)
            ExitSuccess -> refusedFor root parent (OwnedBy . Just <$> sidFromText localService),
      onWindows "a store whose parent lets Authenticated Users change it is refused, by the read-only queries too, naming the parent and them" $ \_ ->
        fresh "granted" $ do
          (root, parent, code, said) <- madeThenChanged "granted" ["/grant", "*" <> T.unpack authenticatedUsers <> ":(OI)(CI)(M)"]
          case code of
            ExitFailure _ -> pure (Fail ("icacls could not grant the right: " <> T.pack said))
            ExitSuccess -> do
              opened <- refusedFor root parent (WritableBy <$> sidFromText authenticatedUsers)
              store <- unStoreDir <$> canonicalStoreDir (StoreDir root)
              queried <- try (sqValidPaths (localQueries (StoreDir root)) Set.empty)
              pure $
                opened `andThen` case (queried, sidFromText authenticatedUsers) of
                  (Left err, Just grantee)
                    | isUserError err -> assertEqual "the read-only refusal" (distrustMessage parent store (WritableBy grantee)) (ioeGetErrorString err)
                  (outcome, _) -> Fail ("expected the read-only queries refused, got: " <> T.pack (show (outcome :: Either IOException (Set.Set StorePath)))),
      -- Upstream's database beside the store makes the store's tree
      -- answer a read-only query in place of nova-nix's database.
      onWindows "a store upstream's database also manages, whose parent lets Authenticated Users change it, is refused before its tree answers a query" $ \_ ->
        fresh "upstream" $ do
          (root, parent, code, said) <- madeThenChanged "upstream" ["/grant", "*" <> T.unpack authenticatedUsers <> ":(OI)(CI)(M)"]
          let upstreamDatabase = parent </> "var" </> "nix" </> "db" </> "db.sqlite"
          createDirectoryIfMissing True (takeDirectory upstreamDatabase)
          BS.writeFile upstreamDatabase BS.empty
          store <- unStoreDir <$> canonicalStoreDir (StoreDir root)
          queried <- try (sqValidPaths (localQueries (StoreDir root)) Set.empty)
          pure $ case (code, queried, sidFromText authenticatedUsers) of
            (ExitFailure _, _, _) -> Fail ("icacls could not grant the right: " <> T.pack said)
            (ExitSuccess, Left err, Just grantee)
              | isUserError err -> assertEqual "the refusal" (distrustMessage parent store (WritableBy grantee)) (ioeGetErrorString err)
            (ExitSuccess, outcome, _) -> Fail ("expected the query refused, got: " <> T.pack (show (outcome :: Either IOException (Set.Set StorePath)))),
      onWindows "a store whose parent passes down full control for CREATOR OWNER, an inherit-only entry, is refused" $ \_ ->
        fresh "inherit-only" $ do
          (root, parent, code, said) <- madeThenChanged "inherit-only" ["/grant", "*" <> T.unpack creatorOwner <> ":(OI)(CI)(IO)(F)"]
          case code of
            ExitFailure _ -> pure (Fail ("icacls could not grant the right: " <> T.pack said))
            ExitSuccess -> refusedFor root parent (WritableBy <$> sidFromText creatorOwner),
      onWindows "a store whose parent grants Users read and execute and denies Guests a write is trusted" $ \_ ->
        fresh "read-only" $ do
          (root, parent, granted, said) <- madeThenChanged "read-only" ["/grant", "*" <> T.unpack users <> ":(OI)(CI)(RX)"]
          (denied, deniedSaid) <- icacls [parent, "/deny", "*S-1-5-32-546:(W)"]
          case (granted, denied) of
            (ExitSuccess, ExitSuccess) -> do
              outcome <- try (openStore testLogger (StoreDir root) >>= closeStore)
              pure (either (\err -> Fail ("the open failed: " <> T.pack (show (err :: IOException)))) (const Pass) outcome)
            _ -> pure (Fail ("icacls could not change the DACL: " <> T.pack (said <> deniedSaid)))
    ]

-- ---------------------------------------------------------------------------
-- Tests: store operations
-- ---------------------------------------------------------------------------

-- | Evaluate an expression against a store directory, its relative paths
-- under a base directory, and answer what the session recorded for a
-- store to add, as the build command reads it from the session.
evalObjectsOf :: StoreDir -> FilePath -> Text -> IO (Either Text EvalObjects)
evalObjectsOf storeDir baseDir source = do
  st <- testEvalState storeDir baseDir
  evaluatedTo <- evalNixIOIn st baseDir source
  recorded <- EvalObjects . sourceCopies <$> readIORef (esSourcePathCache st) <*> readIORef (esStoreWriteCache st) <*> readIORef (esDrvClosure st)
  pure (recorded <$ evaluatedTo)

-- | The store paths a database registered, in the order their rows
-- landed.
registrationOrder :: StoreDir -> IO [Text]
registrationOrder dir =
  bracket (SQL.open (unStoreDir dir </> metaDirName </> dbFileName)) SQL.close $ \conn ->
    map SQL.fromOnly <$> SQL.query_ conn "SELECT path FROM ValidPaths ORDER BY id"

testStoreOpsIO :: IO [Bool]
testStoreOpsIO = do
  putStrLn "store/operations-record"
  shell <- findTestShell
  tmpBase <- getTemporaryDirectory
  let root = tmpBase </> "nova-nix-test-store-ops"
      fresh = forceRemoveIfExists root >> createDirectoryIfMissing True root
      metaDir dir = unStoreDir dir </> metaDirName
      noObjects = EvalObjects Map.empty Map.empty Map.empty
      -- Whether a handle holds the store's collector lock: an exclusive
      -- hold of it is refused while one does, the handle's own included.
      leaseHeld dir = do
        probe <- FileLock.tryLockFile (gcLockFilePath dir) FileLock.Exclusive
        maybe (pure True) (\lock -> False <$ FileLock.unlockFile lock) probe
  storeDir <- canonicalStoreDir (StoreDir (root </> "store"))
  let config = defaultBuildConfig storeDir
      inStore = T.pack . storePathToFilePath storeDir
  results <-
    sequence
      [ runTestM "a resource opened on first use is opened once, answered the same after, and closed once when the action ends" $ do
          counts <- newIORef (0 :: Int, 0 :: Int)
          let open = atomicModifyIORef' counts (\(opened, closed) -> ((opened + 1, closed), opened + 1))
              close _ = modifyIORef' counts (\(opened, closed) -> (opened, closed + 1))
          unused <- withOpenedOnFirstUse open close (const (readIORef counts))
          afterUnused <- readIORef counts
          used <- withOpenedOnFirstUse open close (\use -> (,,) <$> use <*> use <*> readIORef counts)
          afterUsed <- readIORef counts
          pure $
            assertEqual "unused" ((0, 0), (0, 0)) (unused, afterUnused)
              `andThen` assertEqual "used" ((1, 1, (1, 0)), (1, 1)) (used, afterUsed),
        runTestM "a resource opened on first use is closed once when the action fails, and a use after that is refused" $ do
          closes <- newIORef (0 :: Int)
          escaped <- newIORef Nothing
          failed <-
            try
              ( withOpenedOnFirstUse (pure ()) (\() -> modifyIORef' closes (+ 1)) $ \use -> do
                  use
                  writeIORef escaped (Just use)
                  throwIO (userError "the action failed") :: IO ()
              )
          closed <- readIORef closes
          later <- readIORef escaped >>= traverse try
          pure $ case (failed, closed, later) of
            (Left (thrown :: IOException), 1, Just (Left refused))
              | ioeGetErrorString thrown == "the action failed", ioe_type refused == IllegalOperation -> Pass
            (thrown, count, refusal) -> Fail ("expected the failure, one close and a refusal, got: " <> T.pack (show (thrown, count, refusal))),
        runTestM "the local store opens at its first operation, holds its lease while open, and closes when the action ends" $ do
          fresh
          untouched <- withLocalStore testLogger config (const (Dir.doesDirectoryExist (metaDir storeDir)))
          -- An add of nothing asks nothing of the store.
          emptyAdd <- withLocalStore testLogger config $ \ops -> (,) <$> soAddEvalObjects ops noObjects <*> Dir.doesDirectoryExist (metaDir storeDir)
          during <- withLocalStore testLogger config $ \ops -> do
            asked <- sqValidPaths (soQueries ops) (Set.singleton (StorePath (T.replicate 32 "q") "queried"))
            (,) asked <$> leaseHeld storeDir
          after <- leaseHeld storeDir
          pure $
            assertEqual "unopened" False untouched
              `andThen` assertEqual "empty add" (Right (), False) emptyAdd
              `andThen` assertEqual "open" (Set.empty, True) during
              `andThen` assertEqual "closed" False after,
        -- A tree evaluation wrote references the source it names, and the
        -- derivation both: each is registered after what it references.
        runTestM "an add registers an evaluation's sources, then its trees, then its derivations, each with its references" $ do
          fresh
          createDirectoryIfMissing True (root </> "src")
          TIO.writeFile (root </> "src" </> "hello.txt") "hello"
          recorded <-
            evalObjectsOf
              storeDir
              root
              "(let src = ./src; note = builtins.toFile \"note\" \"see ${src}\"; in derivation { name = \"ops\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; inherit src note; }).drvPath"
          let named = mapMaybe (parseStorePath defaultStoreDir)
          case (Map.keys . eoSources <$> recorded, named . Map.keys . eoStoreWrites <$> recorded, named . Map.keys . eoDerivations <$> recorded, recorded) of
            (Right [src], Right [note], Right [drv], Right objects) -> do
              added <- withLocalStore testLogger config (`soAddEvalObjects` objects)
              (valid, references) <-
                bracket (openStore testLogger storeDir) closeStore $ \store ->
                  (,) <$> filterM (isValid store) [src, note, drv] <*> traverse (fmap Set.fromList . queryReferences (stDB store)) [src, note, drv]
              landed <- registrationOrder storeDir
              pure $
                assertEqual "added" (Right ()) added
                  `andThen` assertEqual "valid" [src, note, drv] valid
                  `andThen` assertEqual "references" [Set.empty, Set.singleton (inStore src), Set.fromList [inStore src, inStore note]] references
                  `andThen` assertEqual "order" (map inStore [src, note, drv]) landed
            (sources, trees, derivations, _) -> pure (Fail ("expected one source, one tree and one derivation, got: " <> T.pack (show (sources, trees, derivations)))),
        runTestM "an add, a build or a check in a store upstream's database manages is refused before the store is opened" $ do
          fresh
          let planted = StoreDir (root </> "nix" </> "store")
              upstreamDb = root </> "nix" </> "var" </> "nix" </> "db" </> "db.sqlite"
          createDirectoryIfMissing True (takeDirectory upstreamDb)
          BS.writeFile upstreamDb ""
          createDirectoryIfMissing True (unStoreDir planted)
          plantedDir <- canonicalStoreDir planted
          refusal <- upstreamManagedRefusal ReplacesUpstreamPaths plantedDir
          (nothingAdded, added, built, checked) <-
            withLocalStore testLogger (defaultBuildConfig plantedDir) $ \ops ->
              (,,,)
                <$> soAddEvalObjects ops noObjects
                <*> soAddEvalObjects ops (EvalObjects Map.empty (Map.singleton (storePathToText defaultStoreDir (StorePath (T.replicate 32 "t") "tree")) ([], WriteRecursive)) Map.empty)
                <*> soBuild ops []
                <*> soCheck ops (StorePath (T.replicate 32 "c") "checked.drv")
          opened <- Dir.doesDirectoryExist (metaDir plantedDir)
          pure $ case refusal of
            Just reason ->
              assertEqual "nothing to add" (Right ()) nothingAdded
                `andThen` assertEqual "add" (Left reason) added
                `andThen` assertEqual "build" (Left (BuildStop reason 1)) built
                `andThen` assertEqual "check" (Left (BuildStop reason 1)) checked
                `andThen` assertEqual "opened" False opened
            Nothing -> Fail "the planted store was not refused",
        runTestM "a build or a check through the store's operations stops as buildWithDeps stops, and builds and checks what it does" $ do
          fresh
          let failingOut = StorePath (T.replicate 32 "f") "ops-failing"
              failingDrv = StorePath (T.replicate 32 "g") "ops-failing.drv"
              buildingOut = StorePath (T.replicate 32 "h") "ops-building"
              buildingDrv = StorePath (T.replicate 32 "i") "ops-building.drv"
              failing = mkTestBuildDrv shell failingOut "exit 3"
              building = mkTestBuildDrv shell buildingOut "mkdir -p $out && echo built > $out/value"
              recipes = noObjects {eoDerivations = Map.fromList [(storePathToText defaultStoreDir sp, toATerm drv) | (sp, drv) <- [(failingDrv, failing), (buildingDrv, building)]]}
          (directBuild, directCheck) <-
            bracket (openStore testLogger storeDir) closeStore $ \store ->
              (,) <$> buildWithDeps config store failing failingDrv <*> buildWithDeps config {bcMode = Realise.BuildCheck} store failing failingDrv
          (added, stopped, unchecked, built, checked) <- withLocalStore testLogger config $ \ops ->
            (,,,,)
              <$> soAddEvalObjects ops recipes
              <*> soBuild ops [(failingDrv, Set.singleton "out")]
              <*> soCheck ops failingDrv
              <*> soBuild ops [(buildingDrv, Set.singleton "out")]
              <*> soCheck ops buildingDrv
          value <- try (T.strip <$> TIO.readFile (storePathToFilePath storeDir buildingOut </> "value"))
          pure $ case (directBuild, directCheck) of
            (BuildFailure buildMsg buildCode, BuildFailure checkMsg checkCode) ->
              assertEqual "added" (Right ()) added
                `andThen` assertEqual "stopped" (Left (BuildStop buildMsg buildCode)) stopped
                `andThen` assertEqual "unchecked" (Left (BuildStop checkMsg checkCode)) unchecked
                `andThen` assertEqual "built" (Right ()) built
                `andThen` assertEqual "checked" (Right ()) checked
                `andThen` assertEqual "value" (Right "built" :: Either IOException Text) value
            other -> Fail ("expected buildWithDeps to fail to build and to check, got: " <> T.pack (show other))
      ]
  forceRemoveIfExists root
  pure results

-- | Pure hash decode/digest helpers shared by eval (fixed-output paths) and
-- the builder (builtin:fetchurl verification).
testHashHelpers :: IO [Bool]
testHashHelpers = do
  putStrLn "hash/decode-helpers"
  sequence
    [ -- The incremental digest is the streaming twin of the one-shot:
      -- chunked input must produce identical bytes for every algorithm.
      runTest "incremental digest matches one-shot across algorithms" $
        let chunks = ["nova", "-", "nix", " streams", BS.replicate 1000 55]
            agrees algo = case (Hash.hashInitWithAlgo algo, Hash.rawHashWithAlgo algo (BS.concat chunks)) of
              (Just ctx0, Just expected) ->
                Hash.hashFinalizeBytes (foldl' Hash.hashUpdateChunk ctx0 chunks) == expected
              _ -> False
         in if all agrees ["sha256", "sha512", "sha1", "md5"]
              then Pass
              else Fail "incremental and one-shot digests disagree",
      runTest "incremental digest rejects an unknown algorithm" $
        case Hash.hashInitWithAlgo "blake3" of
          Nothing -> Pass
          Just _ -> Fail "unknown algorithm accepted",
      runTest "hexToBytes empty" $ assertEqual "empty" (Just BS.empty) (Hash.hexToBytes ""),
      runTest "hexToBytes deadbeef" $
        assertEqual "deadbeef" (Just (BS.pack [0xde, 0xad, 0xbe, 0xef])) (Hash.hexToBytes "deadbeef"),
      runTest "hexToBytes uppercase" $
        assertEqual "DEADBEEF" (Just (BS.pack [0xde, 0xad, 0xbe, 0xef])) (Hash.hexToBytes "DEADBEEF"),
      runTest "hexToBytes odd length rejected" $ assertEqual "odd" Nothing (Hash.hexToBytes "abc"),
      runTest "hexToBytes non-hex rejected" $ assertEqual "nonhex" Nothing (Hash.hexToBytes "zz"),
      runTest "rawHashWithAlgo sha256 empty == known vector" $
        assertEqual
          "sha256-empty"
          (Hash.hexToBytes "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
          (Hash.rawHashWithAlgo "sha256" BS.empty),
      runTest "rawHashWithAlgo unknown algo rejected" $
        assertEqual "unknown" Nothing (Hash.rawHashWithAlgo "sha3-256" (BS.pack [1, 2, 3])),
      -- verifyFetchHash: a buffered body checked as the flat output file
      -- it makes, through the comparison builtin:fetchurl makes
      runTest "verifyFetchHash accepts a matching flat hash" $
        assertEqual
          "fetch-hash-ok"
          (Right ())
          (verifyFetchHash "https://x/f.tar.gz" (fetchOut "sha256" sha256Hello) "hello"),
      runTest "verifyFetchHash rejects mismatched bytes" $
        case verifyFetchHash "https://x/f.tar.gz" (fetchOut "sha256" sha256Hello) "world" of
          Left (_, msg) | "hash mismatch" `T.isInfixOf` msg -> Pass
          other -> Fail ("expected mismatch rejection, got: " <> T.pack (show other)),
      runTest "verifyFetchHash rejects a malformed expected hash" $
        case verifyFetchHash "https://x/f" (fetchOut "sha256" "zz-not-hex") "hello" of
          Left (_, msg) | "malformed expected hash" `T.isInfixOf` msg -> Pass
          other -> Fail ("expected malformed-hash rejection, got: " <> T.pack (show other)),
      runTest "verifyFetchHash rejects an unsupported algorithm" $
        case verifyFetchHash "https://x/f" (fetchOut "sha3-256" "00") "hello" of
          Left (_, msg) | "unsupported hash algorithm" `T.isInfixOf` msg -> Pass
          other -> Fail ("expected unsupported-algo rejection, got: " <> T.pack (show other)),
      runTest "verifyFetchHash rejects recursive mode rather than comparing flat" $
        case verifyFetchHash "https://x/f" (fetchOut "r:sha256" sha256Hello) "hello" of
          Left (_, msg) | "recursive" `T.isInfixOf` msg -> Pass
          other -> Fail ("expected recursive-mode rejection, got: " <> T.pack (show other))
    ]
  where
    -- sha256 of the ASCII bytes "hello".
    sha256Hello = "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824"
    fetchOut algo hash =
      DerivationOutput
        { doName = "out",
          doPath = StorePath (T.replicate 32 "f") "fetched",
          doHashAlgo = algo,
          doHash = hash
        }

-- ---------------------------------------------------------------------------
-- Tests: NAR and hash known-answer vectors
-- ---------------------------------------------------------------------------

-- | The in-memory tree 'narSpecVector' encodes.
narSpecTree :: NAR.NarEntry
narSpecTree =
  NAR.NarDirectory
    [ ("bin", NAR.NarDirectory [("tool", NAR.NarRegular True "#!x")]),
      ("data.txt", NAR.NarRegular False "hello\n"),
      ("link", NAR.NarSymlink "data.txt")
    ]

-- ---------------------------------------------------------------------------
-- builtins.fetchGit against a real repository
-- ---------------------------------------------------------------------------

testNarKnownAnswer :: IO [Bool]
testNarKnownAnswer = do
  putStrLn "nar/known-answer"
  sequence
    [ runTest "serialise matches the hand-written spec vector" $
        if NAR.serialise narSpecTree == narSpecVector
          then Pass
          else Fail "NAR encoder no longer matches the spec byte layout",
      runTest "deserialise reads the spec vector back" $
        case NAR.deserialise narSpecVector of
          Right entry
            | NAR.serialise entry == narSpecVector -> Pass
            | otherwise -> Fail "spec vector round-trips to different bytes"
          Left err -> Fail ("spec vector rejected: " <> T.pack err),
      -- The empty-input SHA-256 in nix-base32: pins digest, alphabet, and
      -- bit order against the value real Nix computes.
      runTest "sha256 empty formats to the known nix-base32 vector" $
        assertEqual
          "nix32-empty"
          "sha256:0mdqa9w1p6cmli6976v4wi0sw9r4p5prkj7lzfd1877wk11c9c73"
          (CHash.formatNixHash (CHash.hashBytes BS.empty)),
      -- addToStore must record exactly the hash of the spec bytes for the
      -- same tree, or interop with every real cache silently breaks.
      runTestM "addToStore records the externally-computed NAR hash" $ do
        tmpBase <- getTemporaryDirectory
        let srcDir = tmpBase </> "nova-nix-test-nar-vector-src"
            tmpStore = tmpBase </> "nova-nix-test-nar-vector-store"
        forceRemoveIfExists srcDir
        forceRemoveIfExists tmpStore
        createDirectoryIfMissing True srcDir
        createDirectoryIfMissing True tmpStore
        BS.writeFile (srcDir </> "data.txt") "hello\n"
        store <- openStore testLogger (StoreDir tmpStore)
        let sp = StorePath (T.replicate 32 "e") "nar-vector"
        addToStore store srcDir sp Nothing []
        recorded <- queryPathInfo (stDB store) sp
        closeStore store
        forceRemoveIfExists tmpStore
        let expected = CHash.formatNixHash (CHash.hashBytes narSpecFileVector)
        pure $ case recorded of
          Just info -> assertEqual "recorded NarHash" expected (piNarHash info)
          Nothing -> Fail "path not registered"
    ]

-- | store delete: row-and-tree removal with referrer refusal.  The raw
-- basename channel exists for rows the current name rules reject; the
-- legacy-row case plants one over SQL exactly as a pre-name-rules store
-- carries it.
testStoreDelete :: IO [Bool]
testStoreDelete = do
  putStrLn "store/delete"
  withTempStore $ \store -> do
    let sd = stDir store
        basenameOf sp = spHash sp <> "-" <> spName sp
        dep = StorePath (T.replicate 32 "a") "del-dep"
        user = StorePath (T.replicate 32 "b") "del-user"
        quickCheckPasses property = do
          result <- QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} property
          pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))
        -- Store directories in either host's spelling, the root among
        -- them, and no other with a trailing separator, as an opened
        -- store's has none.
        genEntryDir = QC.elements ["/nix/store", "/tmp/scratch-store", "/", "C:\\nix\\store", "D:\\s", "C:/s/store"]
        -- Short names, so a generated name is accepted about as often as
        -- one holds a separator, a colon, a NUL or a leading dot.
        genEntryText =
          T.pack <$> QC.resize 12 (QC.listOf (QC.frequency [(6, QC.elements "az09-+_?=~ .\233"), (1, QC.elements "./\\:\NUL")]))
    sequence
      [ runTestM "referenced path is refused with its referrers listed" $ do
          registerPaths
            (stDB store)
            [ PathRegistration dep "sha256:d" 1 Nothing [],
              PathRegistration user "sha256:u" 1 Nothing [dep]
            ]
          outcome <- deleteNamed store (basenameOf dep)
          stillValid <- isValid store dep
          pure $ case outcome of
            Left err
              | "referenced by" `T.isInfixOf` err,
                T.pack (storePathToFilePath sd user) `T.isInfixOf` err,
                stillValid ->
                  Pass
              | otherwise -> Fail ("wrong refusal: " <> err)
            Right removed -> Fail ("deleted a referenced path: " <> T.pack (show removed)),
        runTestM "a reference chain deletes leaf-first" $ do
          userGone <- deleteNamed store (basenameOf user)
          depGone <- deleteNamed store (basenameOf dep)
          depValid <- isValid store dep
          userValid <- isValid store user
          pure $ case (userGone, depGone) of
            (Right _, Right _)
              | not depValid && not userValid -> Pass
              | otherwise -> Fail "rows survived deletion"
            other -> Fail ("chain deletion failed: " <> T.pack (show other)),
        -- One call deletes in the order given under one live set: a
        -- referrer named first clears the way for the path it references,
        -- each deletion is reported before the next begins, and the first
        -- entry that cannot be deleted ends the call with the entries
        -- after it untouched.  The referrer's hash sorts after its
        -- reference's, so a call that reordered its arguments would fail
        -- on the reference.
        runTestM "a batch delete reports each deletion in argument order and stops at the first failure" $ do
          let batchDep = StorePath (T.replicate 32 "l") "del-batch-dep"
              batchUser = StorePath (T.replicate 32 "m") "del-batch-user"
              batchAfter = StorePath (T.replicate 32 "p") "del-batch-after"
              missing = T.replicate 32 "n" <> "-del-batch-missing"
              batchPaths = [batchUser, batchDep, batchAfter]
              plant sp = do
                createDirectoryIfMissing True (storePathToFilePath sd sp)
                TIO.writeFile (storePathToFilePath sd sp </> "data.txt") "batch"
          mapM_ plant batchPaths
          registerPaths
            (stDB store)
            [ PathRegistration batchDep "sha256:bd" 1 Nothing [],
              PathRegistration batchUser "sha256:bu" 1 Nothing [batchDep],
              PathRegistration batchAfter "sha256:ba" 1 Nothing []
            ]
          reports <- newIORef []
          let report name removed = do
                stillValid <- mapM (isValid store) batchPaths
                modifyIORef' reports ((storeEntryNameText name, removed, stillValid) :)
          result <-
            either
              (pure . Left)
              (\names -> deleteStorePaths store names report)
              (traverse storeEntryName [basenameOf batchUser, basenameOf batchDep, missing, basenameOf batchAfter])
          reported <- reverse <$> readIORef reports
          afterKept <- (&&) <$> isValid store batchAfter <*> Dir.doesPathExist (storePathToFilePath sd batchAfter </> "data.txt")
          let expected =
                [ (basenameOf batchUser, DeleteOutcome True True, [False, True, True]),
                  (basenameOf batchDep, DeleteOutcome True True, [False, False, True])
                ]
          pure $ case result of
            Left err
              | missing `T.isPrefixOf` err,
                "not in this store" `T.isInfixOf` err,
                reported == expected,
                afterKept ->
                  Pass
            other -> Fail ("wrong batch: " <> T.pack (show (other, reported, afterKept))),
        runTestM "a registered row without a tree deletes (repair case)" $ do
          let ghost = StorePath (T.replicate 32 "c") "del-ghost"
          registerPath (stDB store) (PathRegistration ghost "sha256:g" 1 Nothing [])
          outcome <- deleteNamed store (basenameOf ghost)
          pure $ case outcome of
            Right removed
              | doRowRemoved removed && not (doTreeRemoved removed) -> Pass
              | otherwise -> Fail ("wrong outcome: " <> T.pack (show removed))
            Left err -> Fail err,
        runTestM "an unregistered tree deletes (repair case)" $ do
          let strayBase = T.replicate 32 "d" <> "-del-stray"
              treePath = unStoreDir sd </> T.unpack strayBase
          createDirectoryIfMissing True treePath
          TIO.writeFile (treePath </> "junk.txt") "stray"
          outcome <- deleteNamed store strayBase
          gone <- not <$> Dir.doesPathExist treePath
          pure $ case outcome of
            Right removed
              | not (doRowRemoved removed), doTreeRemoved removed, gone -> Pass
              | otherwise -> Fail ("wrong outcome: " <> T.pack (show removed))
            Left err -> Fail err,
        runTestM "a read-only registered tree deletes fully" $ do
          let solid = StorePath (T.replicate 32 "e") "del-solid"
              treePath = storePathToFilePath sd solid
          registerPath (stDB store) (PathRegistration solid "sha256:s" 1 Nothing [])
          createDirectoryIfMissing True treePath
          TIO.writeFile (treePath </> "data.txt") "content"
          canonicalizeMetadata treePath
          outcome <- deleteNamed store (basenameOf solid)
          gone <- not <$> Dir.doesPathExist treePath
          stillValid <- isValid store solid
          pure $ case outcome of
            Right removed
              | doRowRemoved removed, doTreeRemoved removed, gone, not stillValid -> Pass
              | otherwise -> Fail ("wrong outcome: " <> T.pack (show removed))
            Left err -> Fail err,
        runTestM "a self-referencing path deletes" $ do
          let selfp = StorePath (T.replicate 32 "f") "del-self"
          registerPath (stDB store) (PathRegistration selfp "sha256:f" 1 Nothing [selfp])
          outcome <- deleteNamed store (basenameOf selfp)
          pure (assertEqual "self delete" (Right (DeleteOutcome True False)) outcome),
        runTestM "a target with neither row nor tree is an error" $ do
          outcome <- deleteNamed store (T.replicate 32 "9" <> "-del-nothing")
          pure $ case outcome of
            Left err | "not in this store" `T.isInfixOf` err -> Pass
            other -> Fail ("expected not-in-store, got: " <> T.pack (show other)),
        runTestM "a legacy row the validator rejects deletes by raw text" $ do
          let legacyBase = T.replicate 32 "g" <> "-old~marker"
              treePath = unStoreDir sd </> T.unpack legacyBase
              legacyPathText = T.pack treePath
          conn <- SQL.open (unStoreDir sd </> metaDirName </> dbFileName)
          SQL.execute
            conn
            "INSERT INTO ValidPaths (path, hash, registrationTime, deriver, narSize) VALUES (?, ?, 0, NULL, 0)"
            (legacyPathText, "sha256:legacy" :: T.Text)
          SQL.close conn
          createDirectoryIfMissing True treePath
          TIO.writeFile (treePath </> "seed.txt") "epoch"
          let resolved = resolveDeleteTarget sd legacyPathText
          outcome <- either (pure . Left) (deleteStorePathRaw store) resolved
          remaining <- queryAllValidPaths (stDB store)
          gone <- not <$> Dir.doesPathExist treePath
          pure $ case outcome of
            Right removed
              | doRowRemoved removed, doTreeRemoved removed, gone, legacyPathText `notElem` remaining -> Pass
              | otherwise -> Fail ("wrong outcome: " <> T.pack (show removed))
            Left err -> Fail err,
        runTest "resolveDeleteTarget accepts bare, store-dir, platform, and canonical spellings" $
          let nameBase = T.replicate 32 "h" <> "-name"
              spellings =
                [ nameBase,
                  T.pack (unStoreDir sd </> T.unpack nameBase),
                  T.pack (unStoreDir platformStoreDir) <> "\\" <> nameBase,
                  defaultStoreDirText <> "/" <> nameBase
                ]
           in assertEqual "all resolve" (replicate 4 (Right nameBase)) (map (fmap storeEntryNameText . resolveDeleteTarget sd) spellings),
        runTest "resolveDeleteTarget refuses traversal shapes" $
          let refused =
                [ ".nova-nix",
                  "..",
                  ".",
                  "",
                  T.pack (unStoreDir sd) <> "/",
                  "/somewhere/else/" <> T.replicate 32 "h" <> "-name",
                  T.replicate 32 "h" <> "-na:me",
                  T.replicate 32 "h" <> "-na\NULme"
                ]
           in case [t | t <- refused, either (const False) (const True) (resolveDeleteTarget sd t)] of
                [] -> Pass
                accepted -> Fail ("accepted: " <> T.pack (show accepted)),
        -- A name that is not one entry of the store directory is refused
        -- before a delete takes its path lock or looks for a tree.  Joined
        -- to the directory, an absolute path or one climbing out names a
        -- file outside the store, the empty name names the store directory
        -- itself, "." everything in it, ".." the directory above it, and
        -- the metadata directory's name the store's database and roots.  A
        -- delete that got that far would leave its lock file beside what
        -- it named: in the store, beside it, or beside the file outside.
        -- The store sits two levels into a directory of its own, so none
        -- of these forms reaches outside that directory if it is not
        -- refused.
        runTestM "a delete of a name that is not one store entry is refused and creates nothing" $ do
          sandbox <- (</> "nova-nix-test-delete-escape") <$> getTemporaryDirectory
          let root = sandbox </> "root"
              outsideDir = root </> "outside"
              planted = outsideDir </> "planted"
              forms =
                [ T.pack planted,
                  T.pack (".." </> takeFileName outsideDir </> takeFileName planted),
                  "a/b",
                  "a\\b",
                  ".",
                  "..",
                  "",
                  T.pack metaDirName,
                  "C:x",
                  T.replicate 32 "q" <> "-del\NULname"
                ]
              deleteOne escapeStore form =
                either (pure . Left) (\name -> deleteStorePaths escapeStore [name] (\_ _ -> pure ())) (storeEntryName form)
              -- A delete's refusal, or what it did instead, so every form
              -- runs and the listings below see all that reached the disk.
              refusalOf delete = do
                result <- try delete
                pure $ case result of
                  Left (e :: IOException) -> Left ("raised " <> T.pack (show e))
                  Right (Left refusal) -> Right refusal
                  Right (Right _) -> Left "deleted"
              -- A directory's entries, or Nothing once a delete removed it.
              listing dir = do
                found <- try (Dir.listDirectory dir)
                pure (either (\(_ :: IOException) -> Nothing) (Just . sort) found)
          forceRemoveIfExists sandbox
          createDirectoryIfMissing True (root </> "store")
          createDirectoryIfMissing True outsideDir
          TIO.writeFile planted "outside the store"
          flip finally (forceRemoveIfExists sandbox) $
            bracket (openStore testLogger (StoreDir (root </> "store"))) closeStore $ \escapeStore -> do
              let storeRoot = unStoreDir (stDir escapeStore)
              rootBefore <- listing root
              storeBefore <- listing storeRoot
              single <- mapM (refusalOf . deleteNamed escapeStore) forms
              batch <- mapM (refusalOf . deleteOne escapeStore) forms
              rootAfter <- listing root
              storeAfter <- listing storeRoot
              outsideAfter <- listing outsideDir
              pure $
                if all isRight single
                  && batch == single
                  && rootAfter == rootBefore
                  && storeAfter == storeBefore
                  && outsideAfter == Just [takeFileName planted]
                  then Pass
                  else Fail ("delete reached the filesystem: " <> T.pack (show (zip forms single, batch, rootAfter, storeBefore, storeAfter, outsideAfter))),
        runTestM "a name storeEntryName accepts joins a store directory as one entry in it" $
          quickCheckPasses . QC.checkCoverage $
            QC.forAll ((,) <$> genEntryDir <*> genEntryText) $ \(dir, text) ->
              let accepted = storeEntryName text
               in QC.cover 20 (isRight accepted) "accepted" . QC.cover 20 (isLeft accepted) "refused" $ case accepted of
                    Left _ -> QC.property True
                    Right name ->
                      let joined = dir </> T.unpack (storeEntryNameText name)
                       in (takeFileName joined, takeDirectory joined) QC.=== (T.unpack text, dir),
        runTestM "resolveDeleteTarget gives a bare name storeEntryName's answer" $
          quickCheckPasses $
            QC.forAll (genEntryText `QC.suchThat` (not . T.any (`elem` ['/', '\\']))) $ \text ->
              resolveDeleteTarget sd text QC.=== storeEntryName text,
        -- Lock files are never deleted (the Nix.Store.Lock design), but
        -- names like flake.lock are legal store-path names, so only the
        -- registration rows can tell a lock file from a store object:
        -- an unregistered lock-shaped file refuses, a registered object
        -- of the same shape deletes, and lock-shaped junk whose
        -- stripped prefix is not a store basename deletes as junk.
        runTestM "delete refuses an unregistered lock file by name" $ do
          let guarded = StorePath (T.replicate 32 "j") "del-guard"
              lockBase = basenameOf guarded <> ".lock"
              lockPath = storePathToFilePath sd guarded <> ".lock"
          TIO.writeFile lockPath ""
          outcome <- deleteNamed store lockBase
          survived <- Dir.doesPathExist lockPath
          pure $ case outcome of
            Left err
              | "never deleted" `T.isInfixOf` err ->
                  if survived then Pass else Fail "lock file removed despite refusal"
              | otherwise -> Fail ("wrong refusal: " <> err)
            Right _ -> Fail "unregistered lock file was deleted",
        runTestM "delete removes a registered store object named like a lock file" $ do
          let object = StorePath (T.replicate 32 "k") "flake.lock"
              objectPath = storePathToFilePath sd object
          registerPath (stDB store) (PathRegistration object "sha256:m" 1 Nothing [])
          TIO.writeFile objectPath "pinned inputs"
          outcome <- deleteNamed store (basenameOf object)
          gone <- not <$> Dir.doesPathExist objectPath
          pure $ case outcome of
            Right removed
              | doRowRemoved removed, doTreeRemoved removed, gone -> Pass
              | otherwise -> Fail ("wrong outcome: " <> T.pack (show removed))
            Left err -> Fail ("registered .lock-named object refused: " <> err),
        runTestM "delete removes lock-shaped junk with no store-shaped prefix" $ do
          let junkName = "not-a-store-path.lock"
              junkPath = unStoreDir sd </> T.unpack junkName
          TIO.writeFile junkPath "debris"
          outcome <- deleteNamed store junkName
          gone <- not <$> Dir.doesPathExist junkPath
          pure $ case outcome of
            Right removed
              | doTreeRemoved removed, gone -> Pass
              | otherwise -> Fail ("wrong outcome: " <> T.pack (show removed))
            Left err -> Fail ("lock-shaped junk refused: " <> err),
        -- Deletion and substitution contend on one per-path lock: a
        -- delete must wait while another handle holds the path's lock -
        -- otherwise it can tear a tree out between a substituter's
        -- on-disk recheck and its registration commit - and proceed
        -- once the holder releases.
        runTestM "delete waits for a held path lock and proceeds on release" $ do
          let guarded = StorePath (T.replicate 32 "i") "del-locked"
              treePath = storePathToFilePath sd guarded
          registerPath (stDB store) (PathRegistration guarded "sha256:l" 1 Nothing [])
          createDirectoryIfMissing True treePath
          TIO.writeFile (treePath </> "data.txt") "guarded"
          holder <- acquirePathLock testLogger sd guarded
          done <- newEmptyMVar
          _ <- forkIO (deleteNamed store (basenameOf guarded) >>= putMVar done)
          early <- timeout deleteLockProbeMicros (takeMVar done)
          releasePathLock holder
          outcome <- timeout raceWatchdogMicros (takeMVar done)
          gone <- not <$> Dir.doesPathExist treePath
          pure $ case (early, outcome) of
            (Just finished, _) ->
              Fail ("delete ignored the held lock: " <> T.pack (show finished))
            (Nothing, Just (Right removed))
              | doRowRemoved removed, doTreeRemoved removed, gone -> Pass
              | otherwise -> Fail ("wrong outcome after release: " <> T.pack (show removed))
            (Nothing, other) ->
              Fail ("delete never completed after release: " <> T.pack (show other)),
        -- The eval-side materializer joins the same protocol: its
        -- check-then-act must wait while another handle holds the
        -- path's lock - unlocked, the loser's removePathForcibly could
        -- delete the tree the winner had just registered - and proceed
        -- once the holder releases.
        runTestM "materializeEvalSources waits for a held path lock" $ do
          srcDir <- (</> "nova-nix-test-mat-lock-src") <$> getTemporaryDirectory
          forceRemoveIfExists srcDir
          createDirectoryIfMissing True srcDir
          TIO.writeFile (srcDir </> "f.txt") "locked source"
          -- The address the source's content derives, as evaluation
          -- would name it: a restore refuses any other.
          entry <- NAR.serialiseFromPath srcDir
          case makeFixedOutputPath "mat-locked" "sha256" NarIngestion (sha256Digest (NAR.serialise entry)) of
            Left err -> pure (Fail ("test store path rejected: " <> T.pack (show err)))
            Right guarded -> do
              holder <- acquirePathLock testLogger sd guarded
              done <- newEmptyMVar
              _ <- forkIO (materializeEvalSources store (Map.singleton guarded (T.pack srcDir)) >>= putMVar done)
              early <- timeout deleteLockProbeMicros (takeMVar done)
              releasePathLock holder
              outcome <- timeout raceWatchdogMicros (takeMVar done)
              registered <- isValid store guarded
              forceRemoveIfExists srcDir
              pure $ case (early, outcome) of
                (Just _, _) -> Fail "materialize ignored the held lock"
                (Nothing, Just ())
                  | registered -> Pass
                  | otherwise -> Fail "proceeded after release but never registered"
                (Nothing, Nothing) -> Fail "materialize never completed after release"
      ]

-- | The collector's pure core: the reachability walk over the references
-- map, and the sweep plan over the valid set, the live set, and the store
-- directory's entries.  No store is opened.
testGcPure :: IO [Bool]
testGcPure = do
  putStrLn "store/gc-pure"
  let sd = StoreDir "/s"
      at name = T.pack (unStoreDir sd </> name)
      refs = referencesMap [(at "a", at "b"), (at "b", at "c"), (at "d", at "c"), (at "e", at "e")]
      hashOf c = T.unpack (T.replicate 32 c)
      liveName = hashOf "a" <> "-gc-live"
      deadOnDisk = hashOf "b" <> "-gc-dead"
      deadNoTree = hashOf "c" <> "-gc-ghost"
      stale = hashOf "d" <> "-gc-stale"
      nix32Alphabet = "0123456789abcdfghijklmnpqrsvwxyz" :: String
  sequence
    [ runTest "reachableFrom closes over references transitively" $
        assertEqual "closure" (Set.fromList [at "a", at "b", at "c"]) (reachableFrom refs [at "a"]),
      runTest "reachableFrom leaves paths the roots do not reach out" $
        assertEqual "d alone" (Set.fromList [at "c", at "d"]) (reachableFrom refs [at "d"]),
      runTest "reachableFrom terminates on a cycle and a self-reference" $
        assertEqual
          "cycle, self"
          (Set.fromList [at "x", at "y"], Set.singleton (at "e"))
          (reachableFrom (referencesMap [(at "x", at "y"), (at "y", at "x")]) [at "x"], reachableFrom refs [at "e"]),
      runTest "reachableFrom keeps a root the map does not know as a dead end" $
        assertEqual "unknown root" (Set.singleton (at "z")) (reachableFrom refs [at "z"]),
      runTest "reachableFrom of no roots is empty" $
        assertEqual "no roots" Set.empty (reachableFrom refs []),
      -- Dead rows go whether or not a tree exists; every directory entry
      -- that is not live goes too (a dead tree, a stale build, junk, a
      -- dead path's lock file), while the live path, its lock file and
      -- the metadata directory stay.
      runTest "sweepPlan unregisters dead rows and removes every entry that is not live, keeping a live path's lock file" $
        let valid = Set.fromList [at liveName, at deadOnDisk, at deadNoTree]
            live = Set.fromList [at liveName]
            entries = map T.pack [liveName, liveName <> ".lock", deadOnDisk, deadOnDisk <> ".lock", metaDirName, stale, stale <> ".lock", "junk-file"]
         in assertEqual
              "plan"
              SweepPlan
                { spDeadRows = sort [at deadOnDisk, at deadNoTree],
                  spTrees = sort [at deadOnDisk, at (deadOnDisk <> ".lock"), at stale, at (stale <> ".lock"), at "junk-file"]
                }
              (sweepPlan sd valid live entries),
      runTest "sweepPlan removes a dead registered object named like a live path's lock file" $
        let lockNamed = liveName <> ".lock"
            valid = Set.fromList [at liveName, at lockNamed]
            live = Set.fromList [at liveName]
         in assertEqual
              "plan"
              SweepPlan {spDeadRows = [at lockNamed], spTrees = [at lockNamed]}
              (sweepPlan sd valid live (map T.pack [liveName, lockNamed])),
      -- Nothing registers the rebuild a check keeps under keep-failed, so
      -- a collection removes it even beside a live path, as upstream's
      -- removes it unless the path is a temporary root at the time.
      runTest "sweepPlan removes the rebuild a check kept beside a live path" $
        let live = Set.fromList [at liveName]
         in assertEqual
              "plan"
              SweepPlan {spDeadRows = [], spTrees = [at (liveName <> ".check")]}
              (sweepPlan sd live live (map T.pack [liveName, liveName <> ".check"])),
      runTest "parseStorePathPrefix names the path for itself, a trailing separator and anything beneath, and nothing outside" $
        let sp = StorePath (T.replicate 32 "a") "gc-live"
            dirText = T.pack (unStoreDir sd)
            named = map (parseStorePathPrefix sd) [at liveName, at liveName <> "/", at liveName <> "/bin/tool", at liveName <> "\\bin"]
            unnamed = map (parseStorePathPrefix sd) [dirText, dirText <> "/", "/elsewhere/" <> T.pack liveName, dirText <> "/not-a-store-path"]
         in assertEqual "prefixes" (replicate 4 (Just sp), replicate 4 Nothing) (named, unnamed),
      runTest "showFreedBytes renders upstream's two-place MiB" $
        assertEqual "MiB" ["0.00 MiB", "1.00 MiB", "1.50 MiB"] (map showFreedBytes [0, 1048576, 1572864]),
      runTest "gcSummaryLine is upstream's PrintFreed line" $
        assertEqual "summary" "2 store paths deleted, 1.00 MiB freed" (gcSummaryLine (GcResults [at "a", at "b"] 1048576)),
      runTest "indirectRootRecordName is a nix-base32 digest keyed by the link path" $
        let name = indirectRootRecordName "/home/u/result"
         in if length name == 52 && all (`elem` nix32Alphabet) name && name /= indirectRootRecordName "/home/u/result-2"
              then Pass
              else Fail ("unexpected record name: " <> T.pack name),
      runTest "upstreamDatabasePath is upstream's database for a nix/store directory, and nothing for another" $
        assertEqual
          "databases"
          (map (fmap splitDirectories) [Just "/nix/var/nix/db/db.sqlite", Just "/home/u/nix/var/nix/db/db.sqlite", Nothing, Nothing])
          (map (fmap splitDirectories . upstreamDatabasePath) ["/nix/store", "/home/u/nix/store/", "/nix/nova-store", "/home/u/store"])
    ]

-- | The descriptors this process holds open, where the system lists them
-- under @/dev/fd@, or 'Nothing' where it does not (Windows).
openDescriptors :: IO (Maybe [FilePath])
openDescriptors = do
  listed <- Dir.doesDirectoryExist "/dev/fd"
  if listed then Just . sort <$> Dir.listDirectory "/dev/fd" else pure Nothing

-- | Garbage collection end to end against a temp store: a rooted closure
-- survives and the orphan goes, the rooted path and what it reaches
-- cannot be deleted, one directory keys one store however it is
-- spelled while a foreign spelling is refused, a removed root lets the
-- closure go, a dead cycle sweeps, a dead path's lock file goes with it,
-- a second open handle holds the collector off, a failed lease renewal
-- leaves a closable handle, an open whose database fails keeps nothing
-- open, and an out-link root behaves as nix-build's result link does.
testStoreGC :: IO [Bool]
testStoreGC = do
  putStrLn "store/gc"
  canLink <- symlinksAvailable
  tmpBase <- getTemporaryDirectory
  let linkDir = tmpBase </> "nova-nix-test-gc-links"
      outLink = linkDir </> "result"
  Dir.removePathForcibly linkDir
  createDirectoryIfMissing True linkDir
  withTempStore $ \store -> do
    let sd = stDir store
        basenameOf sp = spHash sp <> "-" <> spName sp
        pathText sp = T.pack (storePathToFilePath sd sp)
        lockOf sp = storePathToFilePath sd sp <> ".lock"
        plant sp bytes = do
          let tree = storePathToFilePath sd sp
          createDirectoryIfMissing True tree
          BS.writeFile (tree </> "data") bytes
        dep = StorePath (T.replicate 32 "a") "gc-dep"
        top = StorePath (T.replicate 32 "b") "gc-top"
        orphan = StorePath (T.replicate 32 "c") "gc-orphan"
        rootFile = gcRootsDir sd </> T.unpack (basenameOf top)
        registerTrio = do
          mapM_ (uncurry plant) [(dep, "dep"), (top, "top!"), (orphan, "orphan bytes")]
          registerPaths
            (stDB store)
            [ PathRegistration dep "sha256:d" 1 Nothing [],
              PathRegistration top "sha256:t" 1 Nothing [dep],
              PathRegistration orphan "sha256:o" 1 Nothing []
            ]
        -- A refused collection is a failure of the test, not a result.
        collect = collectGarbage store >>= either (throwIO . userError . T.unpack) pure
    permanent <-
      sequence
        [ -- A regular file named like the path, under gcroots: upstream's
          -- own permanent-root form, the one that needs no symlink.
          runTestM "a permanent root keeps its closure and the orphan is swept" $ do
            registerTrio
            createDirectoryIfMissing True (gcRootsDir sd)
            BS.writeFile rootFile ""
            results <- collect
            validity <- mapM (isValid store) [dep, top, orphan]
            orphanGone <- not <$> Dir.doesPathExist (storePathToFilePath sd orphan)
            topKept <- Dir.doesPathExist (storePathToFilePath sd top </> "data")
            pure $
              if results == GcResults [pathText orphan] 12 && validity == [True, True, False] && orphanGone && topKept
                then Pass
                else Fail ("wrong sweep: " <> T.pack (show (results, validity, orphanGone, topKept))),
          runTestM "delete refuses the rooted path with upstream's words" $ do
            outcome <- deleteNamed store (basenameOf top)
            stillValid <- isValid store top
            pure $ case outcome of
              Left err
                | "since it is still alive" `T.isInfixOf` err, pathText top `T.isInfixOf` err, stillValid -> Pass
                | otherwise -> Fail ("wrong refusal: " <> err)
              Right removed -> Fail ("deleted a rooted path: " <> T.pack (show removed)),
          runTestM "delete refuses a path the root reaches through references" $ do
            outcome <- deleteNamed store (basenameOf dep)
            pure $ case outcome of
              Left err | "since it is still alive" `T.isInfixOf` err -> Pass
              other -> Fail ("expected the liveness refusal, got: " <> T.pack (show other)),
          runTestM "findRoots reports the permanent root under its own path" $ do
            roots <- findRoots store
            pure (assertEqual "roots" [GcRoot rootFile (pathText top)] roots),
          -- One directory keys one set of rows however it is spelled: the
          -- handle canonicalises the spelling at open, so a second handle
          -- opened through '.', '..' and a trailing separator (and, when
          -- the temp store shares the working directory's drive, through
          -- a relative path) is the same store with the same root.
          runTestM "openStore testLogger keys the store by one spelling of its directory" $ do
            cwd <- Dir.getCurrentDirectory
            let dotted = unStoreDir sd </> "." </> "zig" </> ".." <> [pathSeparator]
                relative =
                  [ joinPath (replicate (length (splitDirectories cwd) - 1) "..") </> dropDrive (unStoreDir sd)
                  | takeDrive cwd == takeDrive (unStoreDir sd)
                  ]
                spellings = dotted : relative
                inspect spelling = do
                  other <- openStore testLogger (StoreDir spelling)
                  roots <- findRoots other
                  closeStore other
                  pure (stDir other, roots)
            outcomes <- mapM inspect spellings
            pure (assertEqual "spellings" (map (const (sd, [GcRoot rootFile (pathText top)])) spellings) outcomes),
          -- A row keyed under another spelling of the directory would
          -- compare as dead; the collector and the checked delete refuse
          -- instead of sweeping, and nothing changes.
          runTestM "a collection and a checked delete refuse a database keyed under another spelling" $ do
            let foreignRow = "/elsewhere/" <> T.replicate 32 "z" <> "-gc-foreign"
            conn <- SQL.open (unStoreDir sd </> metaDirName </> dbFileName)
            SQL.execute
              conn
              "INSERT INTO ValidPaths (path, hash, registrationTime, deriver, narSize) VALUES (?, ?, 0, NULL, 0)"
              (foreignRow, "sha256:z" :: T.Text)
            collected <- collectGarbage store
            deleted <- deleteNamed store (basenameOf dep)
            SQL.execute conn "DELETE FROM ValidPaths WHERE path = ?" (SQL.Only foreignRow)
            SQL.close conn
            validity <- mapM (isValid store) [dep, top, orphan]
            topKept <- Dir.doesPathExist (storePathToFilePath sd top </> "data")
            pure $ case (collected, deleted) of
              (Left gcErr, Left delErr)
                | "another spelling" `T.isInfixOf` gcErr,
                  "another spelling" `T.isInfixOf` delErr,
                  validity == [True, True, False],
                  topKept ->
                    Pass
              other -> Fail ("expected both refusals, got: " <> T.pack (show (other, validity, topKept))),
          -- Upstream reports an entry it cannot read and carries on; a
          -- directory with no permissions is the reproducible case, and
          -- one that lists anyway (root, or Windows, where permissions
          -- do not forbid a listing) proves nothing and is skipped.
          runTestM "an unreadable entry under gcroots is reported and skipped" $ do
            let locked = gcRootsDir sd </> "locked"
                unlock = do
                  Dir.setPermissions locked (Dir.setOwnerReadable True (Dir.setOwnerSearchable True (Dir.setOwnerWritable True Dir.emptyPermissions)))
                  Dir.removePathForcibly locked
            createDirectoryIfMissing True locked
            Dir.setPermissions locked Dir.emptyPermissions
            readable <- try (Dir.listDirectory locked) :: IO (Either SomeException [FilePath])
            -- Restored on every exit: a walk that throws here would
            -- otherwise leave the directory unreadable for every later
            -- root walk in the group.
            roots <- findRoots store `finally` unlock
            case readable of
              Right _ -> Pass <$ putStrLn "  SKIP  an empty-permission directory is readable here"
              Left _ -> pure (assertEqual "roots" [GcRoot rootFile (pathText top)] roots),
          runTestM "removing the root lets the closure go in one sweep" $ do
            Dir.removeFile rootFile
            results <- collect
            remaining <- queryAllValidPaths (stDB store)
            depGone <- not <$> Dir.doesPathExist (storePathToFilePath sd dep)
            pure $
              if gcrDeleted results == [pathText dep, pathText top] && null remaining && depGone
                then Pass
                else Fail ("closure survived: " <> T.pack (show (results, remaining, depGone))),
          -- Cross-output references run both ways between the outputs of
          -- one derivation, and no order of one-row deletes gets through
          -- that; the batch delete does.
          runTestM "a dead reference cycle is swept" $ do
            let out = StorePath (T.replicate 32 "d") "gc-cycle-out"
                dev = StorePath (T.replicate 32 "e") "gc-cycle-dev"
            mapM_ (uncurry plant) [(out, "o"), (dev, "d")]
            registerPaths
              (stDB store)
              [ PathRegistration out "sha256:x" 1 Nothing [dev],
                PathRegistration dev "sha256:y" 1 Nothing [out]
              ]
            results <- collect
            remaining <- queryAllValidPaths (stDB store)
            pure (assertEqual "swept" ([pathText out, pathText dev], []) (gcrDeleted results, remaining)),
          -- Every entry that is not live goes, a dead path's lock file
          -- with it; a live path's lock file is the one lock file the
          -- protocol keeps, and it goes once the path does.
          runTestM "a dead path's lock file is swept with it and a live path's stays until the path goes" $ do
            registerTrio
            BS.writeFile rootFile ""
            let stale = StorePath (T.replicate 32 "f") "gc-stale"
            plant stale "stale"
            BS.writeFile (lockOf stale) ""
            BS.writeFile (lockOf top) ""
            first <- collect
            staleGone <- not <$> Dir.doesPathExist (storePathToFilePath sd stale)
            staleLockGone <- not <$> Dir.doesPathExist (lockOf stale)
            topLockKept <- Dir.doesPathExist (lockOf top)
            Dir.removeFile rootFile
            second <- collect
            topLockGone <- not <$> Dir.doesPathExist (lockOf top)
            remaining <- queryAllValidPaths (stDB store)
            pure $
              if gcrDeleted first == sort [pathText orphan, pathText stale, T.pack (lockOf stale)]
                && staleGone
                && staleLockGone
                && topLockKept
                && gcrDeleted second == sort [pathText dep, pathText top, T.pack (lockOf top)]
                && topLockGone
                && null remaining
                then Pass
                else Fail ("wrong sweep: " <> T.pack (show (first, staleGone, staleLockGone, topLockKept, second, topLockGone, remaining))),
          -- Every open handle holds the collector lock shared; the
          -- collector needs it exclusive, so it waits for the other
          -- handle to close and runs the moment it does.
          runTestM "collectGarbage waits for another open handle to close" $ do
            other <- openStore testLogger sd
            done <- newEmptyMVar
            _ <- forkIO (collect >>= putMVar done)
            early <- timeout deleteLockProbeMicros (takeMVar done)
            closeStore other
            outcome <- timeout raceWatchdogMicros (takeMVar done)
            pure $ case (early, outcome) of
              (Just _, _) -> Fail "collector ignored the other handle's lease"
              (Nothing, Just _) -> Pass
              (Nothing, Nothing) -> Fail "collector never ran after the other handle closed",
          -- A renewal that fails must not leave the handle without a
          -- lease slot, or closeStore waits forever.  The lock file is
          -- replaced by a directory while the exclusive lock is held, so
          -- the shared re-acquire cannot open it; Windows forbids
          -- replacing a held file, so the case is POSIX-only.
          runTestM "withCollectorLock leaves a closable handle when the lease cannot be renewed" $
            if SI.os == "mingw32"
              then Pass <$ putStrLn "  SKIP  a held lock file cannot be replaced on Windows"
              else do
                let renewalStore = tmpBase </> "nova-nix-test-gc-renewal"
                Dir.removePathForcibly renewalStore
                other <- openStore testLogger (StoreDir renewalStore)
                let lockPath = gcLockFilePath (stDir other)
                swapped <- try (withCollectorLock other (Dir.removeFile lockPath >> Dir.createDirectory lockPath))
                closed <- timeout raceWatchdogMicros (closeStore other)
                Dir.removePathForcibly renewalStore
                pure $ case (swapped :: Either SomeException (), closed) of
                  (Left _, Just ()) -> Pass
                  (Right (), _) -> Fail "renewal succeeded against a directory"
                  (Left _, Nothing) -> Fail "closeStore hung after a failed renewal",
          -- An open that fails after taking the lease must give it back,
          -- or every later collection waits on it for as long as the
          -- process runs.  A file that is no database fails at the
          -- first statement, once SQLite holds it open.
          runTestM "a store whose database fails to open keeps neither its lease nor the database open" $ do
            let failingStore = tmpBase </> "nova-nix-test-open-release"
                metaPath = failingStore </> metaDirName
            Dir.removePathForcibly failingStore
            createDirectoryIfMissing True metaPath
            BS.writeFile (metaPath </> dbFileName) "not a database"
            before <- openDescriptors
            opened <- try (openStore testLogger (StoreDir failingStore))
            after <- openDescriptors
            probe <- FileLock.tryLockFile (gcLockFilePath (StoreDir failingStore)) FileLock.Exclusive
            mapM_ FileLock.unlockFile probe
            outcome <- case opened of
              Left (_ :: SQL.SQLError) ->
                pure (assertEqual "lease released" True (isJust probe) `andThen` assertEqual "descriptors" before after)
              Right opener -> Fail "the database opened" <$ closeStore opener
            Dir.removePathForcibly failingStore
            pure outcome
        ]
    linked <-
      if not canLink
        then do
          putStrLn "  SKIP  out-link roots need symlink privilege"
          pure []
        else
          sequence
            [ runTestM "an out-link root keeps the closure until the link is removed" $ do
                registerTrio
                rooted <- addOutLinkRoot store outLink top
                case rooted of
                  Left err -> pure (Fail err)
                  Right link -> do
                    let record = autoRootsDir sd </> indirectRootRecordName link
                    recordThere <- Dir.doesFileExist record
                    roots <- findRoots store
                    first <- collect
                    Dir.removePathForcibly link
                    second <- collect
                    recordGone <- not <$> Dir.doesFileExist record
                    remaining <- queryAllValidPaths (stDB store)
                    pure $
                      if roots == [GcRoot link (pathText top)]
                        && recordThere
                        && gcrDeleted first == [pathText orphan]
                        && gcrDeleted second == [pathText dep, pathText top]
                        && recordGone
                        && null remaining
                        then Pass
                        else Fail ("out-link root misbehaved: " <> T.pack (show (roots, recordThere, first, second, recordGone, remaining))),
              -- upstream's findRoots takes toStorePath of the target, so a
              -- link into a store path's subtree, or to the path with a
              -- trailing separator, roots the path.
              runTestM "a symlink root into a store path's subtree or with a trailing separator roots the path" $ do
                plant top "top"
                registerPath (stDB store) (PathRegistration top "sha256:t" 1 Nothing [])
                let subLink = gcRootsDir sd </> "sub"
                    slashLink = gcRootsDir sd </> "slash"
                Dir.createFileLink (storePathToFilePath sd top </> "data") subLink
                Dir.createDirectoryLink (storePathToFilePath sd top <> [pathSeparator]) slashLink
                roots <- findRoots store
                mapM_ Dir.removePathForcibly [subLink, slashLink]
                pure (assertEqual "roots" [GcRoot slashLink (pathText top), GcRoot subLink (pathText top)] roots),
              -- upstream dedups through a set keyed by (link, path); an
              -- operator's symlink to the out-link and the out-link's own
              -- record name the same root.
              runTestM "findRoots lists one link once when two records name it" $ do
                rooted <- addOutLinkRoot store outLink top
                case rooted of
                  Left err -> pure (Fail err)
                  Right link -> do
                    let mine = gcRootsDir sd </> "mine"
                    Dir.createFileLink link mine
                    roots <- findRoots store
                    mapM_ Dir.removePathForcibly [mine, link]
                    pure (assertEqual "roots" [GcRoot link (pathText top)] roots),
              -- upstream's addPermRoot canonPaths the link and makeSymlink
              -- creates its parents; a '..' that lands inside the store is
              -- refused as inside.
              runTestM "addOutLinkRoot collapses .. and creates missing parents, and .. cannot spell a link into the store" $ do
                let expectedLink = T.unpack (canonPath (T.pack (linkDir </> "deep" </> "result")))
                    dotted = linkDir </> "deep" </> "down" </> ".." </> "result"
                    insideViaDotDot = takeDirectory (unStoreDir sd) </> takeFileName linkDir </> ".." </> takeFileName (unStoreDir sd) </> "result"
                collapsed <- addOutLinkRoot store dotted top
                target <- Dir.getSymbolicLinkTarget expectedLink
                recorded <- BS.readFile (autoRootsDir sd </> indirectRootRecordName expectedLink)
                inside <- addOutLinkRoot store insideViaDotDot top
                pure $ case (collapsed, inside) of
                  (Right link, Left insideErr)
                    | link == expectedLink,
                      target == storePathToFilePath sd top,
                      recorded == TE.encodeUtf8 (T.pack link),
                      "in the Nix store is forbidden" `T.isInfixOf` insideErr ->
                        Pass
                  other -> Fail ("wrong outcomes: " <> T.pack (show (other, target, recorded))),
              -- upstream's addPermRoot: an existing store link is replaced,
              -- anything else at the name is refused, and a link inside
              -- the store is refused.
              runTestM "addOutLinkRoot replaces a store link and refuses a file or a link inside the store" $ do
                let replaceLink = linkDir </> "replace"
                    plainFile = linkDir </> "plain"
                Dir.createFileLink (storePathToFilePath sd orphan) replaceLink
                replaced <- addOutLinkRoot store replaceLink top
                target <- Dir.getSymbolicLinkTarget replaceLink
                BS.writeFile plainFile "not a link"
                clobber <- addOutLinkRoot store plainFile top
                inside <- addOutLinkRoot store (unStoreDir sd </> "result") top
                pure $ case (replaced, clobber, inside) of
                  (Right _, Left clobberErr, Left insideErr)
                    | target == storePathToFilePath sd top,
                      "already exists" `T.isInfixOf` clobberErr,
                      "in the Nix store is forbidden" `T.isInfixOf` insideErr ->
                        Pass
                  other -> Fail ("wrong outcomes: " <> T.pack (show (other, target)))
            ]
    Dir.removePathForcibly linkDir
    pure (permanent ++ linked)

-- | A store whose directory upstream Nix's database also manages, laid
-- out as upstream lays out a store under a root: the collector and the
-- checked delete refuse it and leave an unregistered tree alone, a
-- sibling store under the same directory is collected, a link to the
-- store is refused as the store is, the collector refuses while the
-- database's directory cannot be searched, and with the database gone
-- the same tree is swept.
testStoreGCUpstreamManaged :: IO [Bool]
testStoreGCUpstreamManaged = do
  putStrLn "store/gc-upstream-managed"
  canLink <- symlinksAvailable
  tmpBase <- getTemporaryDirectory
  let root = tmpBase </> "nova-nix-test-upstream-root"
      sd = StoreDir (root </> "nix" </> "store")
      upstreamDb = root </> "nix" </> "var" </> "nix" </> "db" </> "db.sqlite"
      upstreamPath = StorePath (T.replicate 32 "u") "upstream-hello"
      tree = storePathToFilePath sd upstreamPath
      plant dir = do
        let planted = storePathToFilePath dir upstreamPath
        createDirectoryIfMissing True planted
        BS.writeFile (planted </> "data") "upstream's"
  Dir.removePathForcibly root
  createDirectoryIfMissing True (takeDirectory upstreamDb)
  BS.writeFile upstreamDb ""
  plant sd
  store <- openStore testLogger sd
  -- The handle's spelling of the directory, which the refusal names and
  -- the sweep reports in.
  let opened = stDir store
  results <-
    sequence
      [ runTestM "a collection and a checked delete refuse a store upstream's database manages" $ do
          collected <- collectGarbage store
          deleted <- deleteNamed store (spHash upstreamPath <> "-" <> spName upstreamPath)
          treeKept <- Dir.doesPathExist (tree </> "data")
          let names err = all (`T.isInfixOf` err) ["upstream Nix's database", "also manages the store", "db.sqlite", T.pack (unStoreDir opened)]
          pure $ case (collected, deleted) of
            (Left gcErr, Left delErr) | names gcErr, names delErr, treeKept -> Pass
            other -> Fail ("expected both refusals, got: " <> T.pack (show (other, treeKept))),
        -- Upstream keeps a store only in a directory named nix/store, so
        -- a store beside it is nova-nix's alone.
        runTestM "a sibling store under the same directory is collected" $ do
          let sibling = StoreDir (root </> "nix" </> "nova-store")
          createDirectoryIfMissing True (unStoreDir sibling)
          plant sibling
          other <- openStore testLogger sibling
          collected <- collectGarbage other
          closeStore other
          treeGone <- not <$> Dir.doesPathExist (storePathToFilePath sibling upstreamPath)
          pure $ case collected of
            Right _ | treeGone -> Pass
            unexpected -> Fail ("expected the sibling collected, got: " <> T.pack (show (unexpected, treeGone))),
        -- The link's own name is no upstream store directory; the
        -- directory it resolves to is.
        runTestM "a store opened through a link to upstream's store is refused" $
          if not canLink
            then Pass <$ putStrLn "  SKIP  symlinks are unavailable here"
            else do
              let linkRoot = tmpBase </> "nova-nix-test-upstream-link"
                  linkedStore = StoreDir (linkRoot </> "nix" </> "store")
                  link = linkRoot </> "s"
                  linkedDb = linkRoot </> "nix" </> "var" </> "nix" </> "db" </> "db.sqlite"
              Dir.removePathForcibly linkRoot
              createDirectoryIfMissing True (takeDirectory linkedDb)
              BS.writeFile linkedDb ""
              createDirectoryIfMissing True (unStoreDir linkedStore)
              plant linkedStore
              Dir.createDirectoryLink (unStoreDir linkedStore) link
              viaLink <- openStore testLogger (StoreDir link)
              collected <- collectGarbage viaLink
              closeStore viaLink
              treeKept <- Dir.doesPathExist (storePathToFilePath linkedStore upstreamPath </> "data")
              Dir.removePathForcibly linkRoot
              pure $ case collected of
                Left err | "also manages the store" `T.isInfixOf` err, treeKept -> Pass
                other -> Fail ("expected the refusal, got: " <> T.pack (show (other, treeKept))),
        -- A directory with no permissions is the reproducible case; one
        -- that can be searched anyway (root, or Windows, where
        -- permissions do not forbid it) proves nothing and is skipped.
        runTestM "a collection refuses while upstream's database cannot be examined" $ do
          let dbDir = takeDirectory upstreamDb
              unlock = Dir.setPermissions dbDir (Dir.setOwnerReadable True (Dir.setOwnerSearchable True (Dir.setOwnerWritable True Dir.emptyPermissions)))
          Dir.setPermissions dbDir Dir.emptyPermissions
          examinable <- try (Dir.getPermissions upstreamDb) :: IO (Either IOException Dir.Permissions)
          collected <- collectGarbage store `finally` unlock
          treeKept <- Dir.doesPathExist (tree </> "data")
          case examinable of
            Right _ -> Pass <$ putStrLn "  SKIP  an empty-permission directory can be searched here"
            Left _ -> pure $ case collected of
              Left err | "cannot tell whether upstream Nix's database" `T.isInfixOf` err, treeKept -> Pass
              other -> Fail ("expected the refusal, got: " <> T.pack (show (other, treeKept))),
        runTestM "without upstream's database the same tree is swept" $ do
          Dir.removeFile upstreamDb
          collected <- collectGarbage store
          treeGone <- not <$> Dir.doesPathExist tree
          pure $ case collected of
            Right results | gcrDeleted results == [T.pack (storePathToFilePath opened upstreamPath)], treeGone -> Pass
            other -> Fail ("expected the tree swept, got: " <> T.pack (show (other, treeGone)))
      ]
  closeStore store
  Dir.removePathForcibly root
  pure results

-- | Builds, substitutions, the restore of evaluation's sources and an
-- evaluation's own store writes in a store whose directory upstream
-- Nix's database also manages, laid out as upstream lays out a store
-- under a root: each refuses before it writes, the tree upstream left
-- at the path it would write stays, the substitution asks no cache, and
-- an evaluation that writes nothing still evaluates.  A sibling store
-- under the same directory is written as before, from the same cache,
-- over the same leftover trees and by the same expressions, which is
-- what makes the cache's silence and the unchanged trees mean something.
testStoreWritesUpstreamManaged :: IO [Bool]
testStoreWritesUpstreamManaged = do
  putStrLn "store/writes-upstream-managed"
  case signedNarInfo of
    Left err -> (: []) <$> runTest "upstream-managed writes setup" (Fail (T.pack err))
    Right (publicKey, narInfo) -> do
      tmpBase <- getTemporaryDirectory
      let root = tmpBase </> "nova-nix-test-upstream-writes"
          upstreamDb = root </> "nix" </> "var" </> "nix" </> "db" </> "db.sqlite"
          cache base = Subst.CacheConfig base [publicKey] 40
          route path _
            | path == narInfoTarget = Send (httpResponse "200 OK" (TE.encodeUtf8 (NarInfo.renderNarInfo narInfo)))
            | path == narTarget = Send (httpResponse "200 OK" nar)
            | otherwise = Send (httpResponse "500 Internal Server Error" "unexpected request")
          -- A leftover tree at each path a store's operations write, as
          -- upstream would have registered it, in place of whatever an
          -- earlier case left there (a substitute or a build output,
          -- sealed read-only).
          planted sd = do
            mapM_
              ( \sp -> do
                  Dir.removePathForcibly (storePathToFilePath sd sp)
                  createDirectoryIfMissing True (storePathToFilePath sd sp)
                  BS.writeFile (storePathToFilePath sd sp </> "data") upstreamBytes
              )
              [substPath, builtPath]
            pure sd
          withPlantedStore dir action = do
            sd <- planted (StoreDir (root </> "nix" </> dir))
            bracket (openStore testLogger sd) closeStore $ \store ->
              action store (defaultBuildConfig (stDir store))
          kept store sp = (== Right upstreamBytes) <$> (try (BS.readFile (storePathToFilePath (stDir store) sp </> "data")) :: IO (Either IOException BS.ByteString))
          -- The refusal names upstream's database and this store's
          -- directory as the handle spells it, and what a write would do.
          refuses store msg = all (`T.isInfixOf` msg) ["upstream Nix's database", "db.sqlite", "' also manages the store '" <> T.pack (unStoreDir (stDir store)) <> "'", "nova-nix would replace those paths as unregistered"]
          releaseHeld result = case result of
            Subst.SubstSuccess _ lock -> releasePathLock lock
            _ -> pure ()
          -- Each expression's relative path names the source directory
          -- under the root.
          evalIn store = evalNixIOStore (stDir store) root
          landsIn store value = case value of
            Right (VStr bytes _) | Right text <- TE.decodeUtf8' bytes -> Dir.doesPathExist (storeTextToFilePath (stDir store) text)
            _ -> pure False
      forceRemoveIfExists root
      createDirectoryIfMissing True (takeDirectory upstreamDb)
      BS.writeFile upstreamDb ""
      createDirectoryIfMissing True (root </> "source")
      BS.writeFile (root </> "source" </> "hello.txt") "hello"
      results <-
        sequence
          [ -- The realisation is given the cache: refused only where it
            -- builds, it would ask the cache about the output first.
            runTestM "a build, a check and a realisation refuse a store upstream's database manages before they ask a cache, and leave its tree" $
              withHttpServer route $ \base requests _ ->
                withPlantedStore "store" $ \store config -> do
                  built <- buildDerivation config store buildDrv testDrvPath
                  checked <- checkDerivation config store buildDrv testDrvPath
                  realised <- buildWithDeps config {bcCaches = [cache base]} store buildDrv testDrvPath
                  seen <- requests
                  treeKept <- kept store builtPath
                  pure $ case (built, checked, realised) of
                    (BuildFailure b 1, Left c, BuildFailure r 1) | all (refuses store) [b, c, r], null seen, treeKept -> Pass
                    other -> Fail ("expected three refusals and no request, got: " <> T.pack (show (other, seen, treeKept))),
            -- A reference query touches no store, so the narinfo is read;
            -- the fetch is refused before its request.
            runTestM "fetching a substitute refuses a store upstream's database manages before it asks for the NAR, and leaves its tree" $
              withHttpServer route $ \base requests _ ->
                withPlantedStore "store" $ \store _ -> do
                  -- A substitution that went ahead holds the path's lock,
                  -- which the next one would wait on forever.
                  streamed <- offeredThenFetched defaultFetchRetryPolicy (ioRetryEffects testLogger) store [cache base] substPath
                  releaseHeld streamed
                  unpacked <- Subst.unpackAndVerify store substPath narInfo nar
                  releaseHeld unpacked
                  seen <- requests
                  treeKept <- kept store substPath
                  pure $ case (streamed, unpacked) of
                    (Subst.SubstError s, Subst.SubstError u) | refuses store s, refuses store u, seen == [narInfoTarget], treeKept -> Pass
                    other -> Fail ("expected two refusals and the narinfo request alone, got: " <> T.pack (show (other, seen, treeKept))),
            runTestM "restoring evaluation's sources refuses a store upstream's database manages" $
              withPlantedStore "store" $ \store _ -> do
                restored <- try (materializeEvalSources store Map.empty)
                pure $ case restored of
                  Left (err :: IOException) | refuses store (T.pack (ioeGetErrorString err)) -> Pass
                  other -> Fail ("expected the refusal, got: " <> T.pack (show other)),
            runTestM "an evaluation's store writes refuse a store upstream's database manages and write nothing, and one that writes nothing evaluates" $
              withPlantedStore "store" $ \store _ -> do
                before <- Dir.listDirectory (unStoreDir (stDir store))
                written <- mapM (evalIn store) evalWrites
                computed <- evalIn store writesNothing
                after <- Dir.listDirectory (unStoreDir (stDir store))
                pure $ case (written, computed) of
                  ([Left toFile, Left copied], Right (VStr _ _))
                    | refuses store toFile, refuses store copied, sort before == sort after -> Pass
                  other -> Fail ("expected two refusals and the derivation path, got: " <> T.pack (show (other, before, after))),
            -- Upstream keeps a store only in a directory named nix/store,
            -- so a store beside it is nova-nix's alone.
            runTestM "a sibling store under the same directory substitutes and builds over its leftovers" $
              withHttpServer route $ \base requests _ ->
                withPlantedStore "nova-store" $ \store config -> do
                  restored <- try (materializeEvalSources store Map.empty) :: IO (Either IOException ())
                  substituted <- offeredThenFetched defaultFetchRetryPolicy (ioRetryEffects testLogger) store [cache base] substPath
                  fetched <- case substituted of
                    Subst.SubstSuccess _ lock -> do
                      releasePathLock lock
                      Right <$> BS.readFile (storePathToFilePath (stDir store) substPath </> "data")
                    other -> pure (Left other)
                  built <- buildDerivation config store buildDrv testDrvPath
                  builtFile <- Dir.doesFileExist (storePathToFilePath (stDir store) builtPath)
                  seen <- requests
                  pure $ case (restored, fetched, built) of
                    (Right (), Right bytes, BuildSuccess _)
                      | bytes == cacheBytes, builtFile, seen == [narInfoTarget, narTarget] -> Pass
                    other -> Fail ("expected the sibling written, got: " <> T.pack (show (other, builtFile, seen))),
            -- An add is refused before the store is opened, so this store's
            -- database is never made where upstream's manages the paths.
            runTestM "adding a source tree refuses a store upstream's database manages before it opens the store" $ do
              let fresh = root </> "fresh"
                  freshDir = StoreDir (fresh </> "nix" </> "store")
              createDirectoryIfMissing True (fresh </> "nix" </> "var" </> "nix" </> "db")
              BS.writeFile (fresh </> "nix" </> "var" </> "nix" </> "db" </> "db.sqlite") ""
              added <- withLocalStore testLogger (defaultBuildConfig freshDir) $ \ops -> soAddSourceTree ops "tree" Set.empty (NAR.NarRegular False "x")
              madeDatabase <- Dir.doesPathExist (unStoreDir freshDir </> metaDirName)
              pure $ case added of
                Left reason | "upstream Nix's database" `T.isInfixOf` reason, not madeDatabase -> Pass
                other -> Fail ("expected the refusal and no database, got: " <> T.pack (show (other, madeDatabase))),
            runTestM "a sibling store under the same directory takes an evaluation's store writes" $
              withPlantedStore "nova-store" $ \store _ -> do
                written <- mapM (evalIn store) evalWrites
                landed <- mapM (landsIn store) written
                pure $ if and landed then Pass else Fail ("expected both writes in the sibling, got: " <> T.pack (show written))
          ]
      forceRemoveIfExists root
      pure results
  where
    upstreamBytes = "upstream's" :: BS.ByteString
    cacheBytes = "the cache's" :: BS.ByteString
    -- The two kinds of eval-time write: text the expression holds, and a
    -- tree copied from disk.
    evalWrites = ["builtins.toFile \"greeting\" \"hello\"", "builtins.path { path = ./source; name = \"source\"; }"]
    -- A derivation's store path, with a source coerced to its own store
    -- path on the way, which evaluation computes without writing either.
    writesNothing = "(derivation { name = \"greeting\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; src = ./source; }).drvPath"
    substHash = T.replicate 32 "w"
    substPath = StorePath substHash "upstream-hello"
    builtPath = StorePath (T.replicate 32 "x") "upstream-greeting"
    narInfoTarget = "/" <> TE.encodeUtf8 substHash <> ".narinfo"
    narTarget = "/nar/upstream-hello.nar"
    nar = NAR.serialise (NAR.NarDirectory [("data", NAR.NarRegular False cacheBytes)])
    signedNarInfo = do
      secretKey <- Signing.parseSecretKey sigTestKeyText
      publicKey <- Signing.toPublicKey secretKey
      let unsigned =
            NarInfo.NarInfo
              { NarInfo.niStorePath = "/nix/store/" <> substHash <> "-upstream-hello",
                NarInfo.niUrl = "nar/upstream-hello.nar",
                NarInfo.niCompression = "none",
                NarInfo.niFileHash = Nothing,
                NarInfo.niFileSize = Nothing,
                NarInfo.niNarHash = CHash.formatNixHash (CHash.hashBytes nar),
                NarInfo.niNarSize = fromIntegral (BS.length nar),
                NarInfo.niReferences = [],
                NarInfo.niDeriver = Nothing,
                NarInfo.niSigs = [],
                NarInfo.niCA = Nothing
              }
      sig <- Signing.sign secretKey unsigned
      pure (Signing.renderPublicKey publicKey, unsigned {NarInfo.niSigs = [sig]})
    (builder, builderArgs)
      | SI.os == "mingw32" = ("cmd.exe", ["/c", "echo built>%out%"])
      | otherwise = ("/bin/sh", ["-c", "echo built > $out"])
    buildDrv =
      Derivation
        { drvOutputs = [DerivationOutput "out" builtPath "" ""],
          drvInputDrvs = Map.empty,
          drvInputSrcs = [],
          drvPlatform = currentPlatform,
          drvBuilder = builder,
          drvArgs = builderArgs,
          drvEnv = Map.singleton "name" "upstream-greeting"
        }

-- | The canonical form a registered path's metadata takes
-- ("Nix.Store.Metadata"): what one node needs, decided without the
-- filesystem, and the walk that makes the changes on disk.
testStoreMetadata :: IO [Bool]
testStoreMetadata = do
  putStrLn "store/metadata"
  sequence
    [ runTest "a file other users may read and write becomes 0444, and one its owner may execute 0555" $
        assertEqual "file" [SetPermissions readOnlyPermissions] (metadataChanges (node RegularNode 0o644))
          `andThen` assertEqual "executable" [SetPermissions executablePermissions] (metadataChanges (node RegularNode 0o744))
          `andThen` assertEqual "directory" [SetPermissions executablePermissions] (metadataChanges (node DirectoryNode 0o700)),
      runTest "the setuid, setgid and sticky bits are dropped" $
        assertEqual "setuid" [SetPermissions executablePermissions] (metadataChanges (node RegularNode 0o4755))
          `andThen` assertEqual "sticky" [SetPermissions executablePermissions] (metadataChanges (node DirectoryNode 0o1777)),
      runTest "a canonical node needs nothing, and a symlink's mode is never changed" $
        assertEqual "file" [] (metadataChanges (node RegularNode 0o444))
          `andThen` assertEqual "executable" [] (metadataChanges (node RegularNode 0o555))
          `andThen` assertEqual "symlink" [] (metadataChanges (node SymlinkNode 0o777)),
      runTest "a node with another time is given the store's, a symlink included" $
        assertEqual "file" [SetModificationTime] (metadataChanges (node RegularNode 0o444) {nmModificationTime = 0})
          `andThen` assertEqual "symlink" [SetModificationTime] (metadataChanges (node SymlinkNode 0o777) {nmModificationTime = 1700000000}),
      runTest "a node another user owns is taken over, after its mode and time" $
        assertEqual
          "owned by other"
          [SetPermissions readOnlyPermissions, SetModificationTime, TakeOwnership]
          (metadataChanges (node RegularNode 0o644) {nmModificationTime = 0, nmOwnership = OwnedByOther}),
      -- The law: canonicalizing a canonical node changes nothing, so the
      -- walk is idempotent and a second registration of a path leaves it
      -- as the first did.
      runTestM "once a node's changes are made it needs no more" $
        property $
          QC.forAll genNode $ \before ->
            metadataChanges (foldl' (flip applyChange) before (metadataChanges before)) QC.=== [],
      -- A NAR records a file as executable by its owner's execute bit, so
      -- keeping that bit keeps the path's NAR hash.
      runTestM "a canonical mode is 0444 or 0555 and keeps the owner's execute bit" $
        property $
          QC.forAll (QC.choose (0, 0o7777)) $ \perms ->
            let canonical = canonicalPermissions perms
             in QC.counterexample (show canonical) (canonical `elem` [readOnlyPermissions, executablePermissions])
                  QC..&&. (ownerExecutes canonical QC.=== ownerExecutes perms),
      runTestM "canonicalizeMetadata gives each file and directory upstream's mode and the store's time" $ do
        tmpBase <- getTemporaryDirectory
        let tree = tmpBase </> "nova-nix-test-canonical-tree"
        Dir.removePathForcibly tree
        createDirectoryIfMissing True (tree </> "sub")
        BS.writeFile (tree </> "data") "data"
        BS.writeFile (tree </> "run") "#!/bin/sh\n"
        ExecBit.markExecutable (tree </> "run")
        BS.writeFile (tree </> "sub" </> "inner") "inner"
        canonicalizeMetadata tree
        results <-
          traverse
            (uncurry canonicalAs)
            [ (executablePermissions, tree),
              (readOnlyPermissions, tree </> "data"),
              (executablePermissions, tree </> "run"),
              (executablePermissions, tree </> "sub"),
              (readOnlyPermissions, tree </> "sub" </> "inner")
            ]
        Dir.removePathForcibly tree
        pure (foldr andThen Pass results)
    ]
  where
    node kind perms = NodeMetadata kind perms storeModificationTime OwnedByProcess
    genNode =
      NodeMetadata
        <$> QC.elements [RegularNode, DirectoryNode, SymlinkNode]
        <*> QC.choose (0, 0o7777)
        <*> QC.oneof [pure storeModificationTime, QC.arbitrary]
        <*> QC.elements [OwnedByProcess, OwnedByOther]
    -- What each change does to the node it is made to.
    applyChange change meta = case change of
      SetPermissions perms -> meta {nmPermissions = perms}
      SetModificationTime -> meta {nmModificationTime = storeModificationTime}
      TakeOwnership -> meta {nmOwnership = OwnedByProcess}
    ownerExecutes perms = perms .&. 0o100 /= 0
    property :: (QC.Testable prop) => prop -> IO TestResult
    property prop = do
      result <- QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} prop
      pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))

testStoreOps :: IO [Bool]
testStoreOps = do
  putStrLn "store/ops"
  withTempStore $ \store -> do
    let sd = stDir store
    sequence
      [ -- computeClosure emits references before referrers (deps-first):
        -- phase-2 narinfo publication then never announces a path whose
        -- references are not yet visible.  The dep is shared by both
        -- roots and must precede both.
        runTestM "computeClosure orders references first" $ do
          let dep = StorePath "llllllllllllllllllllllllllllllll" "cl-dep"
              p1 = StorePath "mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm" "cl-p1"
              p2 = StorePath "nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn" "cl-p2"
          registerPaths
            (stDB store)
            [ PathRegistration dep "sha256:cd" 1 Nothing [],
              PathRegistration p1 "sha256:c1" 2 Nothing [dep],
              PathRegistration p2 "sha256:c2" 3 Nothing [dep]
            ]
          result <- computeClosure store [p1, p2]
          pure (assertEqual "closure order" (Right [dep, p1, p2]) result),
        -- materializeEvalSources adoption is verified: a partial tree left
        -- at the destination by an interrupted restore must be cleared
        -- and restored again, never registered as-is.
        runTestM "materializeEvalSources restores over a mismatched tree" $ do
          tmpBase <- getTemporaryDirectory
          let srcDir = tmpBase </> "nova-nix-test-adopt-src"
          removeIfExists srcDir
          createDirectoryIfMissing True srcDir
          TIO.writeFile (srcDir </> "data.txt") "real content"
          entry <- NAR.serialiseFromPath srcDir
          case makeFixedOutputPath "nova-nix-test-adopt-src" "sha256" NarIngestion (sha256Digest (NAR.serialise entry)) of
            Left err -> pure (Fail ("test store path rejected: " <> T.pack (show err)))
            Right sp -> do
              let dest = storePathToFilePath (stDir store) sp
              -- Fake an interrupted earlier copy: wrong partial content.
              removeIfExists dest
              createDirectoryIfMissing True dest
              TIO.writeFile (dest </> "data.txt") "partial garbage"
              materializeEvalSources store (Map.fromList [(sp, T.pack srcDir)])
              adopted <- TIO.readFile (dest </> "data.txt")
              registered <- isValid store sp
              removeIfExists srcDir
              pure $
                if adopted == "real content" && registered
                  then Pass
                  else Fail ("expected re-copied content, got: " <> adopted),
        -- A tree that reproduces its address is adopted as it is on disk,
        -- which may be as a restore or an evaluation left it, before its
        -- metadata was made canonical.
        runTestM "materializeEvalSources makes an adopted tree's metadata canonical" $ do
          tmpBase <- getTemporaryDirectory
          let srcDir = tmpBase </> "nova-nix-test-adopt-canonical"
          Dir.removePathForcibly srcDir
          createDirectoryIfMissing True srcDir
          TIO.writeFile (srcDir </> "data.txt") "adopted content"
          entry <- NAR.serialiseFromPath srcDir
          case makeFixedOutputPath "nova-nix-test-adopt-canonical" "sha256" NarIngestion (sha256Digest (NAR.serialise entry)) of
            Left err -> pure (Fail ("test store path rejected: " <> T.pack (show err)))
            Right sp -> do
              let dest = storePathToFilePath (stDir store) sp
              forceRemoveIfExists dest
              createDirectoryIfMissing True dest
              TIO.writeFile (dest </> "data.txt") "adopted content"
              materializeEvalSources store (Map.fromList [(sp, T.pack srcDir)])
              registered <- isValid store sp
              results <- traverse (uncurry canonicalAs) [(executablePermissions, dest), (readOnlyPermissions, dest </> "data.txt")]
              Dir.removePathForcibly srcDir
              pure (assertEqual "registered" True registered `andThen` foldr andThen Pass results),
        -- Evaluation hashes a source well before the build driver
        -- restores it.  A source edited in between no longer reproduces
        -- the address evaluation gave it, so it is refused and nothing
        -- is left at the path, rather than its new bytes being sealed
        -- and registered under the old address.
        runTestM "materializeEvalSources refuses a source changed since evaluation" $ do
          tmpBase <- getTemporaryDirectory
          let srcDir = tmpBase </> "nova-nix-test-changed-src"
          removeIfExists srcDir
          createDirectoryIfMissing True srcDir
          TIO.writeFile (srcDir </> "data.txt") "evaluated content"
          entry <- NAR.serialiseFromPath srcDir
          case makeFixedOutputPath "nova-nix-test-changed-src" "sha256" NarIngestion (sha256Digest (NAR.serialise entry)) of
            Left err -> pure (Fail ("test store path rejected: " <> T.pack (show err)))
            Right sp -> do
              let dest = storePathToFilePath (stDir store) sp
              forceRemoveIfExists dest
              TIO.writeFile (srcDir </> "data.txt") "edited after evaluation"
              outcome <- try (materializeEvalSources store (Map.fromList [(sp, T.pack srcDir)]))
              registered <- isValid store sp
              leftBehind <- Dir.doesPathExist dest
              removeIfExists srcDir
              pure $ case outcome of
                Right () -> Fail "registered a source that changed after evaluation"
                Left (e :: IOException)
                  | not ("changed after evaluation hashed it" `T.isInfixOf` T.pack (ioeGetErrorString e)) ->
                      Fail ("unexpected refusal: " <> T.pack (show e))
                  | registered || leftBehind ->
                      Fail ("refused, but registered " <> T.pack (show registered) <> ", tree left " <> T.pack (show leftBehind))
                  | otherwise -> Pass,
        -- The registrar's half of the eval-write pair: content that does
        -- not re-derive its store path under the recorded scheme must be
        -- refused loudly, never registered valid under a hash its bytes
        -- do not have and then sealed read-only.
        runTestM "materializeEvalStoreWrites refuses content that does not reproduce its path" $ do
          let goodBytes = TE.encodeUtf8 "the full flat content"
          case makeFixedOutputPath "flat-verify" "sha256" FlatIngestion (sha256Digest goodBytes) of
            Left err -> pure (Fail ("test store path rejected: " <> T.pack (show err)))
            Right sp -> do
              let spText = storePathToText defaultStoreDir sp
                  dest = storePathToFilePath (stDir store) sp
              removeIfExists dest
              BS.writeFile dest (TE.encodeUtf8 "the full")
              outcome <- try (materializeEvalStoreWrites store (Map.fromList [(spText, ([], WriteFlat))]))
              stillInvalid <- not <$> isValid store sp
              removeIfExists dest
              pure $ case (outcome :: Either SomeException ()) of
                Left _ | stillInvalid -> Pass
                Left _ -> Fail "refused but registered anyway"
                Right () -> Fail "registered a truncated write as valid",
        runTestM "materializeEvalStoreWrites registers content that reproduces its path" $ do
          let goodBytes = TE.encodeUtf8 "registered text content"
              refs = []
          case makeTextPath "text-verify" (sha256Digest goodBytes) refs of
            Left err -> pure (Fail ("test store path rejected: " <> T.pack (show err)))
            Right sp -> do
              let spText = storePathToText defaultStoreDir sp
                  dest = storePathToFilePath (stDir store) sp
              removeIfExists dest
              BS.writeFile dest goodBytes
              materializeEvalStoreWrites store (Map.fromList [(spText, (refs, WriteText))])
              registered <- isValid store sp
              pure (assertEqual "registered" True registered),
        -- A tree that DOES reproduce its store path is adopted with its
        -- bytes as they are.  The source is deleted before the call:
        -- adoption never reads it, so a wrongful re-copy attempt throws and
        -- fails the test.
        runTestM "materializeEvalSources adopts a matching tree without copying it again" $ do
          tmpBase <- getTemporaryDirectory
          let srcDir = tmpBase </> "nova-nix-test-adopt-ok"
          removeIfExists srcDir
          createDirectoryIfMissing True srcDir
          TIO.writeFile (srcDir </> "data.txt") "same bytes"
          entry <- NAR.serialiseFromPath srcDir
          case makeFixedOutputPath "nova-nix-test-adopt-ok" "sha256" NarIngestion (sha256Digest (NAR.serialise entry)) of
            Left err -> pure (Fail ("test store path rejected: " <> T.pack (show err)))
            Right sp -> do
              let dest = storePathToFilePath (stDir store) sp
              removeIfExists dest
              createDirectoryIfMissing True dest
              TIO.writeFile (dest </> "data.txt") "same bytes"
              removeIfExists srcDir
              materializeEvalSources store (Map.fromList [(sp, T.pack srcDir)])
              registered <- isValid store sp
              pure (if registered then Pass else Fail "matching tree was not adopted and registered"),
        -- scanReferences finds the canonical /nix/store text eval injects
        -- into builder envs, independent of the platform store dir (the
        -- bare hash is the needle, matching upstream Nix).
        runTestM "scanReferences finds canonical ref" $ do
          tmpBase <- getTemporaryDirectory
          let scanDir = tmpBase </> "nova-nix-test-scan"
          removeIfExists scanDir
          createDirectoryIfMissing True scanDir
          let candidate = StorePath "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "dep1"
              refString = "/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-dep1"
          BS.writeFile (scanDir </> "output.txt") (TE.encodeUtf8 (T.pack ("hello " <> refString <> " world")))
          refs <- scanReferences [candidate] scanDir
          removeIfExists scanDir
          pure $
            if candidate `elem` refs
              then Pass
              else Fail ("expected to find ref, got: " <> T.pack (show refs)),
        -- scanReferences finds a Windows-form embedding too
        runTestM "scanReferences finds windows-form ref" $ do
          tmpBase <- getTemporaryDirectory
          let scanDir = tmpBase </> "nova-nix-test-scan-win"
          removeIfExists scanDir
          createDirectoryIfMissing True scanDir
          let candidate = StorePath "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "dep1"
              refString = "C:\\nix\\store\\aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-dep1"
          BS.writeFile (scanDir </> "output.txt") (TE.encodeUtf8 (T.pack ("exec " <> refString)))
          refs <- scanReferences [candidate] scanDir
          removeIfExists scanDir
          pure $
            if candidate `elem` refs
              then Pass
              else Fail ("expected to find ref, got: " <> T.pack (show refs)),
        -- scanReferences misses non-matching
        runTestM "scanReferences misses non-match" $ do
          tmpBase <- getTemporaryDirectory
          let scanDir = tmpBase </> "nova-nix-test-scan2"
          removeIfExists scanDir
          createDirectoryIfMissing True scanDir
          let candidate = StorePath "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "dep1"
          BS.writeFile (scanDir </> "output.txt") "no store paths here"
          refs <- scanReferences [candidate] scanDir
          removeIfExists scanDir
          pure (assertEqual "no refs" [] refs),
        -- scanReferences ignores partial match
        runTestM "scanReferences ignores partial" $ do
          tmpBase <- getTemporaryDirectory
          let scanDir = tmpBase </> "nova-nix-test-scan3"
          removeIfExists scanDir
          createDirectoryIfMissing True scanDir
          let candidate = StorePath "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "dep1"
              -- Only 20 chars of hash - should not match the 32-char needle
              partialRef = "/nix/store/aaaaaaaaaaaaaaaaaaaa"
          BS.writeFile (scanDir </> "output.txt") (TE.encodeUtf8 (T.pack partialRef))
          refs <- scanReferences [candidate] scanDir
          removeIfExists scanDir
          pure (assertEqual "no partial refs" [] refs),
        runTestM "rewriteBytes agrees with a substring replacement model" $ do
          result <-
            QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} $
              QC.forAll (QC.listOf (QC.elements [97, 98, 99])) $ \bytes ->
                let input = BS.pack bytes
                    model = T.replace "bc" "aa" (T.replace "ab" "cc" (TE.decodeUtf8 input))
                 in TE.decodeUtf8 (rewriteBytes [("ab", "cc"), ("bc", "aa")] input) QC.=== model
          pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result))),
        runTest "rewriteBytes leaves bytes alone when nothing matches" $
          assertEqual "no match" "clean bytes" (rewriteBytes [("zz", "yy")] "clean bytes"),
        runTest "rewriteBytes ignores an empty needle" $
          assertEqual "empty needle" "bytes" (rewriteBytes [("", "x")] "bytes"),
        runTestM "rewriteStoreHashes reaches contents, entry names and read-only files" $ do
          tmpBase <- getTemporaryDirectory
          let dir = tmpBase </> "nova-nix-test-rewrite-tree"
          forceRemoveIfExists dir
          createDirectoryIfMissing True (dir </> "sub")
          BS.writeFile (dir </> "sub" </> "file") "path /nix/store/aaaa-dep/bin"
          BS.writeFile (dir </> "clean") "nothing here"
          BS.writeFile (dir </> "aaaa-named") "x"
          readOnly <- Dir.getPermissions (dir </> "sub" </> "file")
          Dir.setPermissions (dir </> "sub" </> "file") (Dir.setOwnerWritable False readOnly)
          rewriteStoreHashes [("aaaa", "bbbb")] dir
          contents <- BS.readFile (dir </> "sub" </> "file")
          stillReadOnly <- not . Dir.writable <$> Dir.getPermissions (dir </> "sub" </> "file")
          renamed <- Dir.doesFileExist (dir </> "bbbb-named")
          oldName <- Dir.doesFileExist (dir </> "aaaa-named")
          forceRemoveIfExists dir
          pure (assertEqual "rewrite" ("path /nix/store/bbbb-dep/bin", True, True, False) (contents, stillReadOnly, renamed, oldName)),
        runTestM "rewriteStoreHashes reaches a directory named for a hash" $ do
          tmpBase <- getTemporaryDirectory
          let dir = tmpBase </> "nova-nix-test-rewrite-named-dir"
          forceRemoveIfExists dir
          createDirectoryIfMissing True (dir </> "aaaa-sub")
          BS.writeFile (dir </> "aaaa-sub" </> "file") "see aaaa"
          rewriteStoreHashes [("aaaa", "bbbb")] dir
          contents <- BS.readFile (dir </> "bbbb-sub" </> "file")
          forceRemoveIfExists dir
          pure (assertEqual "nested contents" "see bbbb" contents),
        runTestM "rewriteStoreHashes refuses to overwrite an entry" $ do
          tmpBase <- getTemporaryDirectory
          let dir = tmpBase </> "nova-nix-test-rewrite-collision"
          forceRemoveIfExists dir
          createDirectoryIfMissing True dir
          BS.writeFile (dir </> "aaaa-x") "scratch"
          BS.writeFile (dir </> "bbbb-x") "final"
          outcome <- try (rewriteStoreHashes [("aaaa", "bbbb")] dir)
          survivor <- BS.readFile (dir </> "bbbb-x")
          forceRemoveIfExists dir
          pure $ case (outcome :: Either IOException (), survivor) of
            (Left _, "final") -> Pass
            other -> Fail (T.pack (show other)),
        runTestM "scanReferences finds a hash held only in an entry name" $ do
          tmpBase <- getTemporaryDirectory
          let dir = tmpBase </> "nova-nix-test-scan-entry-name"
              candidate = StorePath "cccccccccccccccccccccccccccccccc" "dep"
          forceRemoveIfExists dir
          createDirectoryIfMissing True (dir </> "share")
          BS.writeFile (dir </> "share" </> "cccccccccccccccccccccccccccccccc-dep") ""
          refs <- scanReferences [candidate] dir
          forceRemoveIfExists dir
          pure (assertEqual "entry name" [candidate] refs),
        -- A relative target: Windows normalises the separators of a rooted one.
        runTestM "rewriteStoreHashes rewrites a symlink target, in a read-only directory too" $ do
          canLink <- symlinksAvailable
          if not canLink
            then pure Pass
            else do
              tmpBase <- getTemporaryDirectory
              let dir = tmpBase </> "nova-nix-test-rewrite-link"
              forceRemoveIfExists dir
              createDirectoryIfMissing True (dir </> "sub")
              Dir.createFileLink "aaaa-dep" (dir </> "sub" </> "link")
              frozen <- Dir.getPermissions (dir </> "sub")
              Dir.setPermissions (dir </> "sub") (Dir.setOwnerWritable False frozen)
              outcome <- try (rewriteStoreHashes [("aaaa", "bbbb")] dir)
              target <- Dir.getSymbolicLinkTarget (dir </> "sub" </> "link")
              Dir.setPermissions (dir </> "sub") (Dir.setOwnerWritable True frozen)
              forceRemoveIfExists dir
              pure $ case outcome :: Either IOException () of
                Left err -> Fail (T.pack (show err))
                Right () -> assertEqual "link target" "bbbb-dep" target,
        -- addToStore moves dir + registers in DB
        runTestM "addToStore moves + registers" $ do
          tmpBase <- getTemporaryDirectory
          let srcDir = tmpBase </> "nova-nix-test-add-src"
          removeIfExists srcDir
          createDirectoryIfMissing True srcDir
          writeFile (srcDir </> "hello.txt") "hello world"
          let sp = StorePath "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz" "addtest"
          addToStore store srcDir sp Nothing []
          valid <- isValid store sp
          exists <- pathExists store sp
          pure $
            if valid && exists
              then Pass
              else Fail ("valid=" <> T.pack (show valid) <> " exists=" <> T.pack (show exists)),
        -- addToStore sets read-only
        runTestM "addToStore sets read-only" $ do
          tmpBase <- getTemporaryDirectory
          let srcDir = tmpBase </> "nova-nix-test-add-ro"
          removeIfExists srcDir
          createDirectoryIfMissing True srcDir
          writeFile (srcDir </> "data.txt") "data"
          let sp = StorePath "yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy" "rotest"
          addToStore store srcDir sp Nothing []
          let destFile = storePathToFilePath sd sp </> "data.txt"
          perms <- getPermissions destFile
          pure $
            if not (writable perms)
              then Pass
              else Fail "expected read-only but file is writable",
        -- canonicalizeMetadata works on a plain directory
        runTestM "canonicalizeMetadata makes a directory and its file read-only" $ do
          tmpBase <- getTemporaryDirectory
          let roDir = tmpBase </> "nova-nix-test-readonly"
          removeIfExists roDir
          createDirectoryIfMissing True roDir
          writeFile (roDir </> "f.txt") "content"
          canonicalizeMetadata roDir
          dirPerms <- getPermissions roDir
          filePerms <- getPermissions (roDir </> "f.txt")
          -- Cleanup: restore writable so removeIfExists works
          Dir.setPermissions roDir (Dir.setOwnerWritable True dirPerms)
          Dir.setPermissions (roDir </> "f.txt") (Dir.setOwnerWritable True filePerms)
          removeIfExists roDir
          pure $
            if not (writable dirPerms) && not (writable filePerms)
              then Pass
              else
                Fail
                  ( "dir writable="
                      <> T.pack (show (writable dirPerms))
                      <> " file writable="
                      <> T.pack (show (writable filePerms))
                  ),
        runTestM "a registered path's files take upstream's modes and the store's time" $ do
          tmpBase <- getTemporaryDirectory
          let srcDir = tmpBase </> "nova-nix-test-add-canonical"
              sp = StorePath (T.replicate 32 "v") "canonical"
              dest = storePathToFilePath sd sp
          Dir.removePathForcibly srcDir
          createDirectoryIfMissing True srcDir
          BS.writeFile (srcDir </> "data") "data"
          BS.writeFile (srcDir </> "run") "#!/bin/sh\n"
          ExecBit.markExecutable (srcDir </> "run")
          addToStore store srcDir sp Nothing []
          results <-
            traverse
              (uncurry canonicalAs)
              [(executablePermissions, dest), (readOnlyPermissions, dest </> "data"), (executablePermissions, dest </> "run")]
          pure (foldr andThen Pass results),
        -- A .drv is a store object like any other, and the build driver
        -- writes the root's after the closure that already holds it.
        runTestM "a written .drv is read-only with the store's time, and writing it again keeps it" $ do
          let sp = StorePath (T.replicate 32 "u") "canonical.drv"
          writeDrv store simpleTestDrv sp
          writeDrv store simpleTestDrv sp
          contents <- BS.readFile (storePathToFilePath sd sp)
          sealed <- canonicalAs readOnlyPermissions (storePathToFilePath sd sp)
          pure (assertEqual "contents" (toATerm simpleTestDrv) contents `andThen` sealed),
        -- pathExists true after addToStore
        runTestM "pathExists after addToStore" $ do
          tmpBase <- getTemporaryDirectory
          let srcDir = tmpBase </> "nova-nix-test-exists"
          removeIfExists srcDir
          createDirectoryIfMissing True srcDir
          writeFile (srcDir </> "x.txt") "x"
          let sp = StorePath "wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww" "existstest"
          addToStore store srcDir sp Nothing []
          exists <- pathExists store sp
          pure (assertEqual "exists" True exists)
      ]

-- | Store walks treat a symlink as a leaf.  Each fixture plants a link
-- the walk must not follow - out of the tree or back into it (a cycle) -
-- and asserts the walk neither reads nor mutates through the link, and
-- still terminates.
testSymlinkWalksIO :: IO [Bool]
testSymlinkWalksIO = do
  putStrLn "store/symlink-walks"
  canLink <- symlinksAvailable
  if not canLink
    then do
      putStrLn "  SKIP  cannot create symlinks here (Windows needs Developer Mode or elevation)"
      pure []
    else
      sequence
        [ -- A link out of the tree must not have the read-only bit or the
          -- store's time pushed through to its target.  The link's own
          -- time is the store's, which only lstat reads.
          runTestM "canonicalizeMetadata does not change link targets outside the tree" $ do
            tmpBase <- getTemporaryDirectory
            let base = tmpBase </> "nova-nix-test-ro-links"
                outside = base </> "outside"
                tree = base </> "tree"
                targets = [outside, outside </> "victim.txt"]
            Dir.removePathForcibly base
            createDirectoryIfMissing True outside
            createDirectoryIfMissing True tree
            BS.writeFile (outside </> "victim.txt") "victim"
            BS.writeFile (tree </> "inside.txt") "inside"
            Dir.createFileLink (outside </> "victim.txt") (tree </> "file-link")
            Dir.createDirectoryLink outside (tree </> "dir-link")
            targetsBefore <- traverse nodeStatus targets
            canonicalizeMetadata tree
            targetsAfter <- traverse nodeStatus targets
            linkTimes <- traverse (fmap (fmap nsModificationTime) . nodeStatus) [tree </> "file-link", tree </> "dir-link"]
            victimWritable <- writable <$> getPermissions (outside </> "victim.txt")
            insideWritable <- writable <$> getPermissions (tree </> "inside.txt")
            Dir.removePathForcibly base
            pure $
              assertEqual "targets' modes and times" targetsBefore targetsAfter
                `andThen` assertEqual "links' own times" (map (storeModificationTime <$) linkTimes) linkTimes
                `andThen` if victimWritable && not insideWritable
                  then Pass
                  else
                    Fail
                      ( "victim writable="
                          <> T.pack (show victimWritable)
                          <> " inside writable="
                          <> T.pack (show insideWritable)
                      ),
          -- A link cycle must not recurse the walk forever.
          runTestM "canonicalizeMetadata terminates on a link cycle" $ do
            tmpBase <- getTemporaryDirectory
            let tree = tmpBase </> "nova-nix-test-ro-cycle"
            Dir.removePathForcibly tree
            createDirectoryIfMissing True (tree </> "sub")
            BS.writeFile (tree </> "sub" </> "f.txt") "f"
            Dir.createDirectoryLink tree (tree </> "sub" </> "loop")
            outcome <- timeout walkWatchdogMicros (canonicalizeMetadata tree)
            marked <- writable <$> getPermissions (tree </> "sub" </> "f.txt")
            Dir.removePathForcibly tree
            pure $ case outcome of
              Nothing -> Fail "walk did not terminate on a link cycle"
              Just ()
                | marked -> Fail "regular file next to the cycle link was not marked read-only"
                | otherwise -> Pass,
          -- The reference scan reads a link's TARGET STRING (the NAR
          -- carries it) and never the bytes behind the link.
          runTestM "scanReferences scans link targets, not linked bytes" $ do
            tmpBase <- getTemporaryDirectory
            let base = tmpBase </> "nova-nix-test-scan-links"
                outside = base </> "outside"
                scanDir = base </> "tree"
                inTargetHash = T.replicate 32 "a"
                outsideHash = T.replicate 32 "b"
                inTarget = StorePath inTargetHash "dep1"
                outsideOnly = StorePath outsideHash "dep2"
            Dir.removePathForcibly base
            createDirectoryIfMissing True outside
            createDirectoryIfMissing True scanDir
            -- outsideOnly's hash exists only in file bytes OUTSIDE the
            -- tree, reachable through a link; inTarget's exists only in
            -- a link's target string.
            BS.writeFile
              (outside </> "secret.txt")
              (TE.encodeUtf8 ("/nix/store/" <> outsideHash <> "-dep2"))
            Dir.createFileLink (outside </> "secret.txt") (scanDir </> "spy-link")
            Dir.createFileLink
              ("/nix/store/" <> T.unpack inTargetHash <> "-dep1/bin/tool")
              (scanDir </> "dep-link")
            Dir.createDirectoryLink scanDir (scanDir </> "loop")
            found <- timeout walkWatchdogMicros (scanReferences [inTarget, outsideOnly] scanDir)
            Dir.removePathForcibly base
            pure $ case found of
              Nothing -> Fail "scan did not terminate on a link cycle"
              Just refs -> assertEqual "scanned refs" [inTarget] refs,
          -- copyPathInto replicates every link kind, so the NAR bytes -
          -- and the hash a cache signs - are identical whether an output
          -- was renamed or copied across volumes.
          runTestM "copyPathInto replicates links and preserves NAR bytes" $ do
            tmpBase <- getTemporaryDirectory
            let base = tmpBase </> "nova-nix-test-copy-links"
                src = base </> "src"
                dest = base </> "dest"
            Dir.removePathForcibly base
            createDirectoryIfMissing True (src </> "subdir")
            BS.writeFile (src </> "file.txt") "payload"
            BS.writeFile (src </> "subdir" </> "inner.txt") "inner"
            Dir.createFileLink "file.txt" (src </> "rel-link")
            Dir.createDirectoryLink "subdir" (src </> "dir-link")
            Dir.createFileLink "missing.txt" (src </> "dangling")
            copyPathInto src dest
            relIsLink <- Dir.pathIsSymbolicLink (dest </> "rel-link")
            relTarget <- Dir.getSymbolicLinkTarget (dest </> "rel-link")
            dirIsLink <- Dir.pathIsSymbolicLink (dest </> "dir-link")
            danglingIsLink <- Dir.pathIsSymbolicLink (dest </> "dangling")
            srcNarHash <- CHash.formatNixHash . CHash.hashBytes . NAR.serialise <$> NAR.serialiseFromPath src
            destNarHash <- CHash.formatNixHash . CHash.hashBytes . NAR.serialise <$> NAR.serialiseFromPath dest
            Dir.removePathForcibly base
            pure $
              if relIsLink && dirIsLink && danglingIsLink && relTarget == "file.txt"
                then assertEqual "copied NAR hash" srcNarHash destNarHash
                else
                  Fail
                    ( "rel-link/dir-link/dangling as links: "
                        <> T.pack (show (relIsLink, dirIsLink, danglingIsLink))
                        <> ", rel target: "
                        <> T.pack (show relTarget)
                    )
        ]

-- | Pure ordering for the store's symlink second pass: each link
-- follows the pending links its target resolves at or through, and
-- cycle members fall out at the end in input order.
testLinkOrdering :: IO [Bool]
testLinkOrdering = do
  putStrLn "store/link-ordering"
  let root = "out"
      -- The fixtures are ASCII: one path unit per character, both ways,
      -- on every platform.
      osPath = OP.pack . map OP.unsafeFromChar
      fromOsPath = map OP.toChar . OP.unpack
      orderedLinks pending = map (fromOsPath . fst) (orderLinks [(osPath link, osPath target) | (link, target) <- pending])
  sequence
    [ runTest "chain orders targets first" $
        let pending = [(root </> "l1", "l2"), (root </> "l2", "l3"), (root </> "l3", "real")]
         in assertEqual
              "chain"
              [root </> "l3", root </> "l2", root </> "l1"]
              (orderedLinks pending),
      runTest "already-ordered chain is stable" $
        let pending = [(root </> "l3", "real"), (root </> "l2", "l3"), (root </> "l1", "l2")]
         in assertEqual
              "stable"
              [root </> "l3", root </> "l2", root </> "l1"]
              (orderedLinks pending),
      runTest "link through a linked directory follows it" $
        let pending = [(root </> "a", "dirlink/x"), (root </> "dirlink", "realdir")]
         in assertEqual
              "through-dir"
              [root </> "dirlink", root </> "a"]
              (orderedLinks pending),
      runTest "parent-relative target orders after its link" $
        let pending = [(root </> "sub" </> "l", "../other"), (root </> "other", "real")]
         in assertEqual
              "dotdot"
              [root </> "other", root </> "sub" </> "l"]
              (orderedLinks pending),
      runTest "cycle members keep input order at the end" $
        let pending = [(root </> "a", "b"), (root </> "b", "a"), (root </> "c", "real")]
         in assertEqual
              "cycle"
              [root </> "c", root </> "a", root </> "b"]
              (orderedLinks pending),
      runTest "self-target link survives to the fallback" $
        assertEqual
          "self"
          [root </> "x"]
          (orderedLinks [(root </> "x", "x")]),
      runTestM "long chain orders in linear time" $ do
        -- 20000 links each targeting the next: the ready-set rounds
        -- this replaced were quadratic here.
        let chain =
              [ (root </> ("l" <> show i), "l" <> show (i + 1))
              | i <- [1 :: Int .. 20000]
              ]
                ++ [(root </> "l20001", "real")]
            complete = length (orderedLinks chain) == length chain
        outcome <- timeout walkWatchdogMicros (evaluate complete)
        pure $ case outcome of
          Just True -> Pass
          Just False -> Fail "ordering dropped links"
          Nothing -> Fail "ordering did not return promptly"
    ]

-- ---------------------------------------------------------------------------
-- Tests: store-path name validation at write sinks
-- ---------------------------------------------------------------------------

-- | Name validation at write sinks: the store-path name rules hold
-- at path CONSTRUCTION, not only at parse.  Derivation names, output names, and
-- fetchurl basenames reject exactly what the parse boundary rejects,
-- before any write path is built from them - so a name with an interior
-- @..@, which the parse boundary accepts, is valid at a sink too.
testStoreNameSinks :: IO [Bool]
testStoreNameSinks = do
  putStrLn "store/name-sinks"
  let drvWith nameLit =
        "(derivation { name = " <> nameLit <> "; system = \"x86_64-linux\"; builder = \"/bin/sh\"; }).drvPath"
      drvOutPath nameLit =
        "(derivation { name = " <> nameLit <> "; system = \"x86_64-linux\"; builder = \"/bin/sh\"; }).outPath"
      drvWithOutputs outsLit =
        "(derivation { name = \"p\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; outputs = " <> outsLit <> "; }).drvPath"
      failsWith label source needle = case evalNix source of
        Left err
          | parseErrorTag `T.isPrefixOf` err -> Fail (label <> ": did not parse: " <> err)
          | needle `T.isInfixOf` err -> Pass
          | otherwise -> Fail (label <> ": wrong error: " <> err)
        Right val -> Fail (label <> ": expected an eval error, got " <> T.pack (show val))
      succeedsWithSuffix label source suffix = case evalNix source of
        Left err -> Fail (label <> ": unexpected error: " <> err)
        Right (VStr s _) ->
          if TE.encodeUtf8 suffix `BS.isSuffixOf` s
            then Pass
            else Fail (label <> ": expected suffix " <> suffix <> ", got " <> bytesText s)
        Right other -> Fail (label <> ": expected VStr, got " <> T.pack (show other))
      -- Upstream's BadStorePath text in full for a name refused where its
      -- path is built from a derivation's hashes, the hash part held only
      -- to the nix-base32 shape.
      refusedConstructing label source name reason = case evalNix source of
        Left err
          | Just rest <- T.stripPrefix "path '" err,
            (hashPart, afterHash) <- T.splitAt storePathHashLen rest,
            Just _ <- parseStorePathBaseName (hashPart <> "-x"),
            afterHash == "-" <> name <> "' is not a valid store path: name '" <> name <> "' " <> reason ->
              Pass
          | otherwise -> Fail (label <> ": wrong error: " <> err)
        Right val -> Fail (label <> ": expected an eval error, got " <> T.pack (show val))
      drvNamedWithOutputs nameText outsLit =
        "(derivation { name = \"" <> nameText <> "\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; outputs = " <> outsLit <> "; }).drvPath"
      longest = T.replicate 211 "a"
      nearlyLongest = T.replicate 208 "a"
  sequence
    [ runTest "derivation name with a space is rejected" $
        failsWith "name-space" (drvWith "\"a b\"") "invalid derivation name",
      runTest "derivation name with a slash is rejected" $
        failsWith "name-slash" (drvWith "\"a/b\"") "invalid derivation name",
      runTest "derivation name with a backslash is rejected" $
        failsWith "name-backslash" (drvWith "\"a\\\\b\"") "invalid derivation name",
      runTest "derivation name with a drive colon is rejected" $
        failsWith "name-colon" (drvWith "\"C:x\"") "invalid derivation name",
      runTest "empty derivation name is rejected" $
        failsWith "name-empty" (drvWith "\"\"") "name must not be empty",
      runTest "212-character derivation name is rejected" $
        failsWith "name-long" (drvWith ("\"" <> T.replicate 212 "a" <> "\"")) "must be no longer than 211 characters",
      runTest "derivation name '..' is rejected" $
        failsWith "name-dotdot" (drvWith "\"..\"") "name '..' is not valid",
      runTest "derivation name '.-x' is rejected" $
        failsWith "name-dotdash" (drvWith "\".-x\"") "dash-separated component",
      runTest "dotfile derivation name stays valid" $
        succeedsWithSuffix "name-dotfile" (drvWith "\".foo-1.0\"") "-.foo-1.0.drv",
      runTest "interior '..' in a derivation name stays valid" $
        succeedsWithSuffix "name-interior-dots" (drvOutPath "\"x..y\"") "-x..y",
      runTest "derivation output name with a traversal is rejected" $
        failsWith "out-traversal" (drvWithOutputs "[ \"out\" \"../x\" ]") "invalid derivation output name",
      runTest "derivation output name with a colon is rejected" $
        failsWith "out-colon" (drvWithOutputs "[ \"a:b\" ]") "invalid derivation output name",
      -- Upstream's derivationStrict refuses these in its outputs loop: the
      -- result set already has a drvPath attribute, and a repeat would
      -- collide there too.
      runTest "derivation output named drvPath is rejected" $
        failsWith "out-drvpath" (drvWithOutputs "[ \"drvPath\" ]") "invalid derivation output name 'drvPath'",
      runTest "repeated derivation output name is rejected" $
        failsWith "out-duplicate" (drvWithOutputs "[ \"out\" \"out\" ]") "duplicate derivation output 'out'",
      -- The wrapper never calls derivationStrict with an empty list (its
      -- builtins.head fails on the empty list of outputs first), so the
      -- primop is called directly.
      runTest "empty derivation output list is rejected" $
        failsWith
          "out-empty"
          "derivationStrict { name = \"p\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; outputs = [ ]; }"
          "derivation cannot have an empty set of outputs",
      -- Both fields clean on their own, only the COMPOSED drvName-output
      -- crosses the length cap: the construction-side check catches what
      -- the field-level checks cannot.
      runTest "composed name-output past the length cap is rejected" $
        refusedConstructing
          "composed-long"
          (drvNamedWithOutputs nearlyLongest "[ \"dev\" ]")
          (nearlyLongest <> "-dev")
          "must be no longer than 211 characters",
      -- Upstream builds the output paths over its sorted output set, so
      -- of two names past the cap the first by name is the one refused.
      runTest "of two outputs past the length cap the first by name is refused" $
        refusedConstructing
          "composed-order"
          (drvNamedWithOutputs nearlyLongest "[ \"out\" \"zzz\" \"dev\" ]")
          (nearlyLongest <> "-dev")
          "must be no longer than 211 characters",
      runTest "a name whose .drv name is past the length cap is refused" $
        refusedConstructing
          "drv-long"
          (drvNamedWithOutputs longest "[ \"out\" ]")
          (longest <> ".drv")
          "must be no longer than 211 characters",
      -- fetchurl derives the store name from the URL alone, so a bad
      -- basename fails BEFORE the download: under PureEval a reachable
      -- download would surface as "runProcess: not available" instead.
      runTest "fetchurl rejects a backslash basename before fetching" $
        failsWith "fetchurl-backslash" "builtins.fetchurl \"http://e/a\\\\b\"" "invalid store path name",
      runTest "fetchurl rejects a dot-segment basename before fetching" $
        failsWith "fetchurl-dotdot" "builtins.fetchurl \"http://e/..\"" "invalid store path name"
    ]

-- | Which entry names and link targets each host's rules admit
-- ("Nix.Store.EntryName").  The decision is pure, so every host's answer
-- is checked on any host: the grammar's refusals hold everywhere, POSIX
-- holds every other name as bytes, macOS needs UTF-8 without a
-- noncharacter, and Windows refuses what Win32 reserves or rewrites.
testNarNameSafety :: IO [Bool]
testNarNameSafety = do
  putStrLn "store/entry-name-rules"
  let admitted rules names = [(name, admits rules name) | name <- names]
      admits rules name = either (const False) (const True) (checkEntryName rules name)
      allAdmit rules names = assertEqual (T.pack (show rules) <> " verdicts") [(name, True) | name <- names] (admitted rules names)
      allRefuse rules names = assertEqual (T.pack (show rules) <> " verdicts") [(name, False) | name <- names] (admitted rules names)
      everyHost = [PosixNames, DarwinNames, WindowsNames]
      grammarRefused = ["", ".", "..", "a/b", "/", "a\NULb"]
      posixOnlyNames = [name | (name, _) <- posixOnlyEntries]
      notUtf8 = ["caf\xE9", "\xFF", "\xC0\xAF", "\xED\xA0\x80", "\xF4\x90\x80\x80"]
      noncharacters = ["n\xEF\xB7\x90", "n\xEF\xB7\xAF", "n\xEF\xBF\xBE", "n\xEF\xBF\xBF", "n\xF0\x9F\xBF\xBE", "n\xF4\x8F\xBF\xBF"]
  sequence
    [ runTest "the grammar's refusals hold on every host" $
        foldr (keepFirstFail . (`allRefuse` grammarRefused)) Pass everyHost,
      runTest "POSIX holds every other name as its bytes" $
        allAdmit PosixNames (posixOnlyNames <> notUtf8 <> noncharacters <> ["caf\xC3\xA9", "a\x7F"]),
      runTest "macOS holds a name upstream restores there" $
        allAdmit DarwinNames (posixOnlyNames <> ["caf\xC3\xA9", "n\xEE\x80\x80", "n\xEF\xBF\xBD", "n\xEF\xBB\xBF"]),
      runTest "macOS refuses a name that is not UTF-8 or holds a noncharacter" $
        allRefuse DarwinNames (notUtf8 <> noncharacters),
      runTest "Windows refuses device names, trailing dots and spaces, and reserved characters" $
        allRefuse
          WindowsNames
          ( filter (not . BS.isPrefixOf "link-") posixOnlyNames
              <> ["nul.txt", "Nul .txt", "COM0.log", "com3", "lpt9", "COM\xC2\xB2", "COM\xC2\xB3", "C:evil", "a*b", "a?b", "a<b", "a>b", "a|b", "ctl\x1F"]
          ),
      runTest "Windows refuses a name with no UTF-16 form" $
        allRefuse WindowsNames notUtf8,
      runTest "Windows holds ordinary and near-miss names" $
        allAdmit
          WindowsNames
          ["a", ".hidden", "a.b", "a b", " a", "com10", "COM", "NULx", "xNUL.txt", "caf\xC3\xA9", "LPT\xE2\x81\xB9", "a~b", "a\x7F", "n\xEF\xBF\xBE"],
      runTest "a refusal names the entry and the reason" $
        assertEqual
          "refusals"
          [ Left "NAR directory entry name 'aux.c' is a Windows device name",
            Left "NAR directory entry name 'x.' ends in a dot or a space, which Windows strips",
            Left "NAR directory entry name 'a:b' holds a character Windows does not allow in a name",
            Left "NAR directory entry name \"caf\\233\" is not valid UTF-8, which a macOS filesystem cannot hold",
            Left "unsafe NAR directory entry name: \"a/b\""
          ]
          [ checkEntryName WindowsNames "aux.c",
            checkEntryName WindowsNames "x.",
            checkEntryName WindowsNames "a:b",
            checkEntryName DarwinNames "caf\xE9",
            checkEntryName PosixNames "a/b"
          ],
      runTest "a link target is bytes on POSIX and macOS and UTF-16 on Windows" $
        assertEqual
          "target verdicts"
          [Right (), Right (), Left "NAR symlink target \"caf\\233\" is not valid UTF-8, so it has no UTF-16 form for Windows", Right ()]
          [ checkLinkTarget PosixNames "caf\xE9",
            checkLinkTarget DarwinNames "caf\xE9",
            checkLinkTarget WindowsNames "caf\xE9",
            checkLinkTarget WindowsNames "../a b/c:d"
          ],
      -- The Windows rules check a name's bytes, where
      -- 'legacyTextNameSafety' reads it as text; on every name both can
      -- judge (UTF-8 without a character the text check lets through and
      -- Windows refuses at the create) they must agree.
      runTestM "the Windows rules refuse what the text check they replace refused" $ do
        result <-
          QC.quickCheckWithResult QC.stdArgs {QC.chatty = False, QC.maxSuccess = 5000} $
            QC.forAll genDeviceLikeName $ \name ->
              either (const False) (const True) (checkEntryName WindowsNames (TE.encodeUtf8 name))
                QC.=== legacyTextNameSafety name
        pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))
    ]
  where
    keepFirstFail Pass acc = acc
    keepFirstFail failure _ = failure

-- | The text-form name check, verbatim, as the oracle the Windows rules'
-- byte-form port is tested against.
legacyTextNameSafety :: Text -> Bool
legacyTextNameSafety name =
  not (T.null name)
    && name /= ".."
    && name /= "."
    && not (T.any escapesTree name)
    && not (trailingRewritten name)
    && not (reservedDeviceStem name)
  where
    escapesTree c = c == '/' || c == '\\' || c == ':' || c == '\0'
    trailingRewritten n = case T.unsnoc n of
      Just (_, end) -> end == '.' || end == ' '
      Nothing -> False
    reservedDeviceStem n =
      let stem = T.toUpper (T.dropWhileEnd (== ' ') (T.takeWhile (/= '.') n))
       in stem == "CON"
            || stem == "PRN"
            || stem == "AUX"
            || stem == "NUL"
            || numberedDeviceStem stem
    numberedDeviceStem stem = case T.unpack stem of
      [a, b, c, digit] -> ([a, b, c] == "COM" || [a, b, c] == "LPT") && deviceDigit digit
      _ -> False
    deviceDigit c = isDigit c || c == '\185' || c == '\178' || c == '\179'

-- | Names shaped like a Windows device: a stem in mixed case or a near
-- miss, then a digit (plain, superscript, or a superscript nine that is
-- not a device digit), spaces, and nothing, an extension, or more
-- pieces, among them separators, dots and letters whose uppercase is
-- longer than themselves.  About one name in twenty is refused by the
-- device rule alone.
genDeviceLikeName :: QC.Gen Text
genDeviceLikeName = do
  stem <- QC.elements ["con", "CON", "Prn", "aux", "AUX", "nul", "NuL", "com", "COM", "lpt", "LpT", "x", "\305", "\329"]
  digit <- QC.elements ["", "0", "1", "9", "10", "\185", "\178", "\179", "\8313", "\233"]
  spaces <- QC.elements ["", " ", "  "]
  rest <- QC.frequency [(3, pure ""), (3, ("." <>) <$> pieces), (2, pieces)]
  pure (stem <> digit <> spaces <> rest)
  where
    pieces =
      T.concat
        <$> QC.resize 4 (QC.listOf (QC.elements [".", "..", " ", "x", "txt", "\233", "\223", "\64256", ":", "\\", "/", "\NUL", "con", "1"]))

-- | IO-evaluator write sinks reject an invalid name BEFORE any write:
-- every case here errors out against a scratch dir with no store traffic,
-- in upstream's BadStorePath words, naming the path the write would have
-- had.  The hash parts are computed apart from nova-nix, from the NAR and
-- store-path formats, and the first two are what nix-instantiate 2.24.9
-- prints.  The fixtures are written as bytes, so their NARs, and so the
-- hash parts, are the same on every host.
testStoreNameSinksIO :: IO [Bool]
testStoreNameSinksIO = do
  putStrLn "store/name-sinks-io"
  tmpBase <- getTemporaryDirectory
  let testDir = tmpBase </> "nova-nix-test-name-sinks"
      refusedAs label source expected = runTestM label (assertEvalErrorIO testDir label source expected)
  bracket_
    ( do
        createDirectoryIfMissing True (testDir </> "tree")
        BS.writeFile (testDir </> "tree" </> "f.txt") "payload\n"
        BS.writeFile (testDir </> "sp ace") "payload\n"
    )
    ( do
        exists <- doesDirectoryExist testDir
        when exists (removeDirectoryRecursive testDir)
    )
    $ sequence
      [ refusedAs
          "toFile rejects a name with a space"
          "builtins.toFile \"a b\" \"x\""
          "path 'dm85kbl5cy82j2dzvy1874s14y3salv3-a b' is not a valid store path: name 'a b' contains illegal character ' '",
        refusedAs
          "toFile rejects an empty name"
          "builtins.toFile \"\" \"x\""
          "path '9r1mh2rc3s3i33vpdw86kqqyvy9i1y4w-' is not a valid store path: name must not be empty",
        refusedAs
          "toFile rejects a stream-colon name"
          "builtins.toFile \"a:b\" \"x\""
          "path '2gs912wpmbp0agwviqp9ymw7ljxlsla0-a:b' is not a valid store path: name 'a:b' contains illegal character ':'",
        refusedAs
          "toFile rejects a dot-segment name"
          "builtins.toFile \"..\" \"x\""
          "path 'hv29bhyiqgqrrvfwnrc9gg9g9cw83k4b-..' is not a valid store path: name '..' is not valid",
        -- A copy's name is refused once the tree is hashed, so its hash
        -- part is that of the tree's NAR.
        refusedAs
          "path rejects a traversal name override"
          "builtins.path { path = ./tree; name = \"../../evil\"; }"
          "path '7gzygjfd61wpybnrahsdpkpa1qhcgx55-../../evil' is not a valid store path: name '../../evil' contains illegal character '/'",
        refusedAs
          "path with a filter rejects a drive-prefixed name"
          "builtins.path { path = ./tree; name = \"C:evil\"; filter = (_: _: true); }"
          "path 'la26d723gkznrdzhaxknqp0mi025ibi7-C:evil' is not a valid store path: name 'C:evil' contains illegal character ':'",
        -- A pinned copy's name is refused under the pin, before the pin
        -- is checked against the tree (which it does not match).
        refusedAs
          "a pinned path's name is refused under the pinned hash"
          ("builtins.path { path = ./tree; name = \"a b\"; sha256 = \"" <> T.replicate 64 "0" <> "\"; }")
          "path 'mrlc43by4xlm6cr8163bcqrr712bh2k2-a b' is not a valid store path: name 'a b' contains illegal character ' '",
        -- Every argument is forced before the pinned path is built.
        refusedAs
          "a pinned path forces its filter before refusing its name"
          ("builtins.path { path = ./tree; name = \"a b\"; sha256 = \"" <> T.replicate 64 "0" <> "\"; filter = throw \"filter forced\"; }")
          "filter forced",
        refusedAs
          "source coercion rejects a basename outside the name rules"
          "\"${./. + \"/sp ace\"}\""
          "path 'pj9xmn69xsrk8nlx6xsljb0bmsydj4dr-sp ace' is not a valid store path: name 'sp ace' contains illegal character ' '"
      ]
