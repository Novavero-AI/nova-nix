{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Tests of the builder: builds, their sandbox and logs, the builtin
-- builders and the profiles buildenv makes.
module Test.Builder
  ( testBuildOrchestrator,
    testPassAsFile,
    testBuilderLayout,
    testSandboxPure,
    testSandboxSpawnIO,
    sandboxBuildChildFlag,
    sandboxBuildChild,
    sandboxFallbackChildFlag,
    sandboxFallbackChild,
    testSandboxFallbackIO,
    testLinuxSandboxIO,
    testDarwinSandboxIO,
    sandboxEscapeChildFlag,
    sandboxEscapeChild,
    sandboxForkerChildFlag,
    sandboxForkerChild,
    sandboxThawChildFlag,
    sandboxThawChild,
    testTrivialBuildIO,
    testBuildDirIO,
    testBuildLogIO,
    testSourceDateEpochIO,
    testBuildCoresIO,
    testBuildCores,
    testScrubbedBuildEnvIO,
    testUnpackBuildIO,
    testProfileTree,
    testBuildenvDerivations,
    testBuildenvBuildIO,
    testProfileManifest,
    testProfileGenerations,
    testProfileUpdateIO,
    testDependentBuildIO,
    testBuildCheckIO,
    testReferenceClosureIO,
    testOutputOrderIO,
    testOutputCycleBuildIO,
    testExecWrapper,
    testFetchMirrors,
    testFetchurlUnpack,
    testSubstituteFromCaches,
    testWantedOutputsIO,
    testBuilder,
    testFlatOutputFormIO,

    -- * What the Windows sandbox tests share
    sandboxHelperVar,
    processWaitMicros,
    startWaitMicros,
    waitUntil,
    waitUntilWithin,
  )
where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as TarEntry
import qualified Codec.Compression.GZip as GZip
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel, poll, wait, waitCatch, withAsync, withAsyncBound)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, tryPutMVar)
import Control.Exception (AsyncException (..), IOException, bracket, bracket_, catches, finally, fromException, throwIO, toException, try)
import Control.Monad (guard, replicateM, replicateM_, unless, void, when, (>=>))
import Control.Monad.Except (runExceptT)
import Control.Monad.State.Strict (evalStateT)
import Data.Bits (shiftR, (.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.Char (intToDigit, isDigit)
import Data.Either (fromLeft, fromRight, isLeft, isRight)
import Data.Foldable (for_, toList, traverse_)
import Data.Functor.Identity (Identity (..))
import qualified Data.Graph as Graph
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Int (Int32)
import Data.List (inits, mapAccumL, partition, sort, stripPrefix)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybeToList)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Tuple (swap)
import Data.Word (Word32, Word64, Word8)
import Foreign.C.Error (Errno (..), eACCES, eAGAIN, eINVAL, eIO, eMFILE, eNOENT, eNOTDIR, getErrno)
import Foreign.C.String (withCString)
import Foreign.C.Types (CChar, CInt (..), CLong (..), CSize (..))
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import HttpFixture (Reply (..), httpResponse, withFetchurlServer, withHttpServer)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Status as HTTP
import Nix.Builder (AttrsDelivery (..), BuildConfig (..), BuildLayout (..), BuildPlace (..), BuildResult (..), BuilderInvocation (..), BuilderSpawn (..), ReferenceCycle (..), attrsDelivery, buildDerivation, buildPath, buildWithDeps, builderInvocation, checkDerivation, closureUnder, defaultBuildConfig, execWrapperConfig, execWrapperFor, fetchUrlsFromEnv, orderByReferences, rewriteEnv, rewritePlaceholders, scrubAmbient, spawnFor, substituteFromCaches, substitutePaths, tryFetchUrlsWith, unionEnvs)
import Nix.Builder.BuildDir (BuildEnding (..), FailedBuilds (..), WorkDir (..), buildDirFor, createBuildDir, withBuildDir)
import Nix.Builder.Buildenv (Activity (..), DerivationsError (..), Element (..), builtinBuildenvBuilder, elementPackages, parseDerivations)
import qualified Nix.Builder.Cores as Cores
import Nix.Builder.Log (emptyPendingLine, endBuildLog, feedBuildLog)
import qualified Nix.Builder.Sandbox as Sandbox
import qualified Nix.Builder.Sandbox.Spawn as Spawn
import Nix.Builder.Unpack (UnpackLimits (..), builtinUnpackBuilder, entryComponents, envSrcs, resolveLinkTarget)
import qualified Nix.Cli.Args as Cli
import Nix.Config (NixConfig (..))
import qualified Nix.Config as Config
import qualified Nix.Daemon.Protocol as Proto
import qualified Nix.DependencyGraph as DepGraph
import Nix.Derivation (Derivation (..), DerivationOutput (..), Platform (..), currentPlatform, platformToText)
import Nix.Eval (NixValue (..))
import Nix.Hash (FileIngestionMethod (..), hashPlaceholder, makeFixedOutputPath, sha256Digest)
import qualified Nix.Hash as Hash
import Nix.HostPath (hostPathFromBytes, hostPathText)
import Nix.Http (FetchRetryPolicy (..), RetryEffects (..), TransferError (..), defaultFetchRetryPolicy, fetchExceptionFailure, fetchStatusFailure, ioRetryEffects, retryDelayMs, retryTransient, statusError, transferBodyReader, transferFailureHandlers, userAgent, withUserAgent)
import Nix.Logger (EscapeFilter (..), Logger (..), filterLogLine, handleLogger)
import qualified Nix.Profile.Generation as Gen
import qualified Nix.Profile.Manifest as Manifest
import Nix.Profile.Tree (BuildEnvFailure (..), Built (..), EntryKind (..), Listing (..), Node (..), Package (..), ReadFailure (..), Tree, TreeReader (..), Warning (..), buildEnvFailureText, buildProfile, propagatedTokens, warningText)
import Nix.Push (PushCompression (..), preparePath, storePathBasename)
import qualified Nix.Realise as Realise
import Nix.Store (DeleteOutcome (..), Store (..), acquirePathLock, addToStore, canonicalizeMetadata, closeStore, collectGarbage, freshCopyPath, gcRootsDir, isValid, openStore, pathLockFilePath, releasePathLock, replaceWithCopy, rewriteRegularFile, tryAcquirePathLock, waitingForLockMessage, writeDrv)
import qualified Nix.Store.Access as Access
import Nix.Store.CaseSensitive (volumeCaseHack)
import Nix.Store.DB (PathInfo (..), PathRegistration (..), Ultimate (..), metaDirName, queryDeriver, queryPathInfo, queryReferences, registerPaths)
import qualified Nix.Store.ExecBit as ExecBit
import Nix.Store.Ops (StoreOps (..), StoreQueries (..), withLocalStore)
import Nix.Store.Path (StoreDir (..), StorePath, defaultStoreDir, parseStorePath, storePathToFilePath, storePathToText)
import Nix.Store.Path.Internal (StorePath (..))
import qualified Nix.Substituter as Subst
import NodeStatus (NodeStatus (..), nodeStatus)
import qualified NovaCache.Base64 as B64
import qualified NovaCache.Hash as CHash
import qualified NovaCache.NAR as NAR
import qualified NovaCache.NarInfo as NarInfo
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getPermissions, getTemporaryDirectory)
import qualified System.Directory as Dir
import qualified System.Directory.OsPath as OsDir
import System.Environment (getExecutablePath, lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO (Handle, IOMode (..), hClose, hFlush, hGetLine, hPutStrLn, stderr, stdout, withBinaryFile)
import System.IO.Error (isAlreadyExistsError, mkIOError, resourceVanishedErrorType)
import qualified System.Info as SI
import System.OsPath (OsPath)
import qualified System.OsPath as OP
import qualified System.Process as Proc
import System.Timeout (timeout)
import Test.Harness (TestResult (..), andThen, asciiOsPath, assertEqual, assertLeft, assertRight, bytesText, canonicalAs, deleteNamed, evalDerivationIO, evalNixIO, findTestShell, forceRemoveIfExists, genFixturePath, genTextOf, hostToolsEnv, mkTestBuildDrv, narInfoRequest, narRequest, narSpecFileVector, narSpecStr, narSpecVector, offeredSubstitute, profileFixture, pureDepGraph, runTest, runTestM, servedPaths, signedTestNarInfo, symlinksAvailable, tarDir, tarFile, tarPathOrDie, testDrvPath, testLogger, withEnvVar, withTempStore)
import qualified Test.QuickCheck as QC
import Text.Read (readMaybe)
import UserResources (continueProcess, newSession, withHangupIgnored, withPseudoTerminal, withUnixListener)

-- ---------------------------------------------------------------------------
-- Tests: Build orchestrator (Phase 3, Batch 7)
-- ---------------------------------------------------------------------------

testBuildOrchestrator :: IO [Bool]
testBuildOrchestrator = do
  putStrLn "build/orchestrator"
  sequence
    [ -- BuildConfig has caches field
      runTest "defaultBuildConfig has empty caches" $
        assertEqual "empty-caches" [] (bcCaches (defaultBuildConfig defaultStoreDir)),
      -- Upstream's shape is a product token with the version; the comment
      -- after it is ours.  What bot mitigation keys on is that there is one.
      runTest "the User-Agent is a nova-nix product token with a version" $
        assertEqual "ua" True ("nova-nix/" `BS.isPrefixOf` userAgent && BS.length userAgent > BS.length "nova-nix/ (+https://github.com/Novavero-AI/nova-nix)"),
      runTestM "withUserAgent sets exactly one User-Agent, replacing any present" $ do
        request0 <- HTTP.parseRequest "http://127.0.0.1/narinfo-hashes"
        let stale = request0 {HTTP.requestHeaders = [("User-Agent", "curl/0"), ("Accept", "*/*")]}
        pure (assertEqual "headers" [("User-Agent", userAgent), ("Accept", "*/*")] (HTTP.requestHeaders (withUserAgent stale))),
      -- On Unix the build's PATH is upstream's, naming no directory
      -- whatever the builder is.
      runTest "a Unix build's PATH is upstream's /path-not-set" $
        if SI.os == "mingw32"
          then Pass
          else
            foldr
              andThen
              Pass
              [assertEqual (T.pack builder) "/path-not-set" (buildPath builder) | builder <- ["bash", "./bash", "./tools/bash", "/store/aaa-bootstrap/bin/bash"]],
      -- A Windows build's PATH must never name the build working
      -- directory: a builder whose directory depends on it, a bare name or
      -- a relative path, adds none.
      runTest "a builder named relative to the working directory adds no PATH entry" $
        foldr
          andThen
          Pass
          [ assertEqual (T.pack builder) False (any (\e -> e == "." || "./" `T.isPrefixOf` e || ".\\" `T.isPrefixOf` e || "tools" `T.isPrefixOf` e) (T.splitOn ";" (buildPath builder)))
          | builder <- ["bash", "./bash", "./tools/bash", "tools/bash"]
          ],
      runTest "an absolute builder opens a Windows build's PATH with its own directory" $
        if SI.os == "mingw32"
          then assertEqual "builder-dir-first" (Just "/store/aaa-bootstrap/bin") (listToMaybe (T.splitOn ";" (buildPath ("/store/aaa-bootstrap/bin" </> "bash"))))
          else Pass,
      -- unionEnvs: earlier maps win; on Windows displacement is
      -- case-insensitive and keeps the winner's spelling.
      runTest "unionEnvs left map wins" $
        assertEqual
          "left-bias"
          (Just "build")
          (Map.lookup "A" (unionEnvs [Map.fromList [("A", "build")], Map.fromList [("A", "host")]])),
      runTest "unionEnvs displaces a case variant on Windows" $
        let merged = unionEnvs [Map.fromList [("PATH", "build")], Map.fromList [("Path", "host")]]
         in if SI.os == "mingw32"
              then assertEqual "displaced" (Map.fromList [("PATH", "build")]) merged
              else assertEqual "distinct" (Map.fromList [("PATH", "build"), ("Path", "host")]) merged,
      -- scrubAmbient: nothing ambient survives on Unix; on Windows only
      -- the allowlist passes (keeping its ambient spelling), COMSPEC is
      -- synthesized from SystemRoot, and PATHEXT is pinned.
      runTest "scrubAmbient drops undeclared ambient variables" $
        let ambient =
              Map.fromList
                [ ("SystemRoot", "C:\\WINDOWS"),
                  ("APPDATA", "C:\\Users\\host\\AppData\\Roaming"),
                  ("LANG", "en_US.UTF-8"),
                  ("Path", "C:\\host\\bin")
                ]
            scrubbed = scrubAmbient ambient
         in if SI.os == "mingw32"
              then
                assertEqual
                  "allowlist-only"
                  ( Map.fromList
                      [ ("SystemRoot", "C:\\WINDOWS"),
                        ("COMSPEC", "C:\\WINDOWS\\System32\\cmd.exe"),
                        ("PATHEXT", ".COM;.EXE;.BAT;.CMD")
                      ]
                  )
                  scrubbed
              else assertEqual "empty" Map.empty scrubbed,
      runTest "scrubAmbient pins PATHEXT even without SystemRoot" $
        let scrubbed = scrubAmbient Map.empty
         in if SI.os == "mingw32"
              then assertEqual "pinned" (Map.fromList [("PATHEXT", ".COM;.EXE;.BAT;.CMD")]) scrubbed
              else assertEqual "empty" Map.empty scrubbed,
      -- Build-time placeholder substitution.  Upstream rewrites the whole
      -- @name=value@ string (@runChild@ in local-derivation-goal.cc at
      -- 2.24.9, unix-derivation-builder.cc at 2.36.0), so a sentinel in a
      -- dynamic attribute name is substituted along with the values.
      runTest "rewritePlaceholders substitutes an output sentinel" $
        assertEqual
          "substituted"
          "/store/aaa-p/lib"
          (rewritePlaceholders [("out", "/store/aaa-p")] (hashPlaceholder "out" <> "/lib")),
      runTest "rewritePlaceholders leaves a foreign output's sentinel alone" $
        assertEqual
          "untouched"
          (hashPlaceholder "dev")
          (rewritePlaceholders [("out", "/store/aaa-p")] (hashPlaceholder "dev")),
      runTest "rewriteEnv rewrites names as well as values" $
        let rewrite = rewritePlaceholders [("out", "/store/aaa-p")]
            env = Map.fromList [(hashPlaceholder "out" <> "-flag", hashPlaceholder "out" <> "/lib")]
         in assertEqual
              "name-and-value"
              (Map.fromList [("/store/aaa-p-flag", "/store/aaa-p/lib")])
              (rewriteEnv rewrite env),
      -- BuildConfig with caches
      runTest "BuildConfig accepts caches" $
        let cache = Subst.CacheConfig "https://cache.example.com" ["key"] 10
            config = (defaultBuildConfig defaultStoreDir) {bcCaches = [cache]}
         in assertEqual "one-cache" 1 (length (bcCaches config)),
      -- buildWithDeps on a simple derivation (no deps, builder fails but graph resolves)
      runTestM "buildWithDeps single drv" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-orch"
        createDirectoryIfMissing True tmpStore
        store <- openStore testLogger (StoreDir tmpStore)
        let drv =
              Derivation
                { drvOutputs =
                    [ DerivationOutput
                        { doName = "out",
                          doPath = StorePath "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz" "test-out",
                          doHashAlgo = "",
                          doHash = ""
                        }
                    ],
                  drvInputDrvs = Map.empty,
                  drvInputSrcs = [],
                  drvPlatform = currentPlatform,
                  drvBuilder = "/nonexistent-builder",
                  drvArgs = [],
                  drvEnv = Map.singleton "name" "test"
                }
            drvSP = StorePath "yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy" "test.drv"
        -- Write .drv to store so buildWithDeps can read it
        writeDrv store drv drvSP
        let config = defaultBuildConfig (StoreDir tmpStore)
        result <- buildWithDeps config store drv drvSP
        closeStore store
        forceRemoveIfExists tmpStore
        -- Builder will fail (nonexistent) but the graph should resolve
        -- correctly: the failure must come from SPAWNING THE BUILDER, not
        -- from graph resolution or drv reading upstream of it.
        pure $ case result of
          BuildFailure msg _
            | "nonexistent-builder" `T.isInfixOf` msg -> Pass
            | otherwise -> Fail ("failed before reaching the builder: " <> msg)
          BuildSuccess _ -> Fail "expected build failure for nonexistent builder",
      -- buildWithDeps with cycle detection (mocked through malformed graph)
      runTest "cycle detection returns failure" $
        let spA = StorePath "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "a.drv"
            spB = StorePath "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" "b.drv"
            drvACyc =
              Derivation
                { drvOutputs = [],
                  drvInputDrvs = Map.singleton spB ["out"],
                  drvInputSrcs = [],
                  drvPlatform = X86_64_Linux,
                  drvBuilder = "/bin/sh",
                  drvArgs = [],
                  drvEnv = Map.empty
                }
            drvBCyc =
              Derivation
                { drvOutputs = [],
                  drvInputDrvs = Map.singleton spA ["out"],
                  drvInputSrcs = [],
                  drvPlatform = X86_64_Linux,
                  drvBuilder = "/bin/sh",
                  drvArgs = [],
                  drvEnv = Map.empty
                }
            readFn sp
              | sp == spB = Right drvBCyc
              | sp == spA = Right drvACyc
              | otherwise = Left "unknown"
         in case pureDepGraph readFn drvACyc spA of
              Right graph -> case DepGraph.topoSort graph of
                DepGraph.TopoCycle _ -> Pass
                DepGraph.TopoSorted order -> Fail ("expected cycle, got sorted: " <> T.pack (show order))
              Left err -> Fail ("expected graph to build, got: " <> err),
      -- missing .drv causes failure in dep graph
      runTest "missing drv in dep graph" $
        let sp = StorePath "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "missing.drv"
            drv =
              Derivation
                { drvOutputs = [],
                  drvInputDrvs = Map.singleton sp ["out"],
                  drvInputSrcs = [],
                  drvPlatform = X86_64_Linux,
                  drvBuilder = "/bin/sh",
                  drvArgs = [],
                  drvEnv = Map.empty
                }
            readFn _ = Left "not found"
         in case pureDepGraph readFn drv (StorePath "rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr" "root.drv") of
              Left _ -> Pass
              Right _ -> Fail "expected failure for missing .drv",
      -- derivation with context creates populated inputDrvs.  Hashing a
      -- dependent derivation reads input modulo hashes from the drv-hash
      -- cache, which only the IO evaluator maintains, so this runs under
      -- evalNixIO (an in-session dependency hits the cache bottom-up).
      runTestM "derivation context populates inputDrvs" $ do
        tmpBase <- getTemporaryDirectory
        result <-
          evalDerivationIO tmpBase $
            T.concat
              [ "let dep = derivation { name = \"dep\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; }; ",
                "main = derivation { name = \"main\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; src = dep.outPath; }; ",
                "in main"
              ]
        pure $ assertRight "drv-ctx-inputs" result $ \drv ->
          if Map.null (drvInputDrvs drv)
            then Fail "expected non-empty drvInputDrvs"
            else Pass,
      -- drv3: a dependent derivation's drvPath is stable across evaluations
      -- (the input modulo substitution is deterministic).
      runTestM "dependent derivation drvPath is deterministic (IO eval)" $ do
        tmpBase <- getTemporaryDirectory
        let expr =
              T.concat
                [ "let dep = derivation { name = \"dep\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; }; ",
                  "main = derivation { name = \"main\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; src = dep.outPath; }; ",
                  "in main.drvPath"
                ]
        r1 <- evalNixIO tmpBase expr
        r2 <- evalNixIO tmpBase expr
        pure $ case (r1, r2) of
          (Right (VStr a _), Right (VStr b _))
            | a == b -> Pass
            | otherwise -> Fail ("drvPath not deterministic: " <> bytesText a <> " vs " <> bytesText b)
          _ -> Fail "expected main.drvPath to evaluate to a string under IO eval",
      -- drv1: embedding another derivation's drvPath (an all-outputs ref) adds
      -- it to inputDrvs carrying the referenced derivation's FULL
      -- output-name set; the IO evaluator recovers the names in-session.
      runTestM "derivation embedding a drvPath lists all its outputs in inputDrvs (IO eval)" $ do
        tmpBase <- getTemporaryDirectory
        let depSrc = "derivation { name = \"dep\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; outputs = [ \"out\" \"dev\" ]; }"
        depPathR <- evalNixIO tmpBase ("(" <> depSrc <> ").drvPath")
        result <-
          evalDerivationIO tmpBase $
            T.concat
              [ "let dep = ",
                depSrc,
                "; main = derivation { name = \"main\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; ref = dep.drvPath; }; ",
                "in main"
              ]
        pure $ case (depPathR, result) of
          (Right (VStr depPathBytes _), Right drv) ->
            case parseStorePath defaultStoreDir (bytesText depPathBytes) of
              Nothing -> Fail ("unparseable dep drvPath: " <> bytesText depPathBytes)
              Just depSP -> case Map.lookup depSP (drvInputDrvs drv) of
                Just outs
                  | Set.fromList outs == Set.fromList ["dev", "out"] -> Pass
                  | otherwise -> Fail ("expected the full output set [dev, out], got " <> T.pack (show outs))
                Nothing -> Fail "dep .drv missing from inputDrvs despite the deep drvPath ref"
          other -> Fail ("expected dep drvPath and main's derivation, got " <> T.pack (show other))
    ]

-- | @passAsFile@ on the value a build writes and spawns from, upstream's
-- @initTmpDir@ (local-derivation-goal.cc at 2.24.9).  The file name for
-- @foo@ is the one upstream's own test pins
-- (@tests\/functional\/pass-as-file.sh@ at 2.24.9).
testPassAsFile :: IO [Bool]
testPassAsFile = do
  putStrLn "builder/passAsFile"
  sequence
    [ runTest "an attribute passAsFile names becomes upstream's file and a Path variable" $
        assertRight "delivery" (deliver [("foo", "xyzzy"), ("passAsFile", "foo")]) $ \delivery ->
          assertEqual "files" [(fooName, "xyzzy")] (adFiles delivery)
            `andThen` assertEqual "fooPath" (Just (T.pack fooFile)) (Map.lookup "fooPath" (adEnv delivery))
            `andThen` assertEqual "foo" Nothing (Map.lookup "foo" (adEnv delivery))
            `andThen` assertEqual "passAsFile" (Just "foo") (Map.lookup "passAsFile" (adEnv delivery)),
      -- Upstream's tokenizer splits at space, tab, newline and carriage
      -- return only, so the vertical tab leaves one name, "e\vf", that no
      -- attribute has.
      runTest "passAsFile splits at upstream's separators and skips absent names" $
        let attrs = [(name, "v") | name <- ["a", "b", "c", "d", "e", "f"]]
         in assertRight "delivery" (deliver (("passAsFile", "a\tb\nc\rd  e\vf missing") : attrs)) $ \delivery ->
              assertEqual "path variables" ["aPath", "bPath", "cPath", "dPath"] (filter ("Path" `T.isSuffixOf`) (Map.keys (adEnv delivery)))
                `andThen` assertEqual "files" 4 (length (adFiles delivery))
                `andThen` assertEqual "kept" [Just "v", Just "v"] (map (`Map.lookup` adEnv delivery) ["e", "f"]),
      runTest "an attribute called <name>Path keeps its own value" $
        assertRight "delivery" (deliver [("foo", "x"), ("fooPath", "literal"), ("passAsFile", "foo")]) $ \delivery ->
          assertEqual "fooPath" (Just "literal") (Map.lookup "fooPath" (adEnv delivery))
            `andThen` assertEqual "files" [(fooName, "x")] (adFiles delivery),
      -- The derivation's out is the final path; the build writes elsewhere
      -- when that path is already valid, and the file says where.
      runTest "an output passed as a file leaves the environment for the path it is written to" $
        assertRight "delivery" (deliver [("out", "/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-p"), ("passAsFile", "out")]) $ \delivery ->
          assertEqual "out" Nothing (Map.lookup "out" (adEnv delivery))
            `andThen` case Map.lookup "outPath" (adEnv delivery) of
              Just path -> assertEqual "files" [(takeFileName (T.unpack path), TE.encodeUtf8 (T.pack scratchOut))] (adFiles delivery)
              Nothing -> Fail "no outPath variable",
      runTest "a value passed as a file stays bytes" $
        assertRight "as a file" (deliver [("foo", "\xff\xfe"), ("passAsFile", "foo")]) (assertEqual "files" [(fooName, "\xff\xfe")] . adFiles)
          `andThen` assertLeft "as a variable" (deliver [("foo", "\xff\xfe")]),
      runTest "structured attributes ignore passAsFile" $
        let json = "{\"foo\":\"xyzzy\",\"passAsFile\":[\"foo\"]}"
         in assertRight "delivery" (deliver [("__json", json), ("foo", "xyzzy"), ("passAsFile", "foo")]) $ \delivery ->
              assertEqual "files" [".attrs.json", ".attrs.sh"] (map fst (adFiles delivery))
                `andThen` assertEqual "variables" ["NIX_ATTRS_JSON_FILE", "NIX_ATTRS_SH_FILE"] (Map.keys (adEnv delivery)),
      -- The law passAsFile exists for: moving an attribute from the
      -- environment to a file changes where the builder reads it, not what
      -- it reads.  The store directories include one ending in /nix/store,
      -- which a rewrite applied twice would mangle.
      runTestM "an attribute's file holds the bytes its variable would have held" $ do
        result <-
          QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} $
            QC.forAll ((,) <$> QC.elements storeDirs <*> genRewritable) $ \(dir, value) ->
              let config = defaultBuildConfig (StoreDir dir)
                  bytes = TE.encodeUtf8 value
               in case (deliverWith config [("foo", bytes)], deliverWith config [("foo", bytes), ("passAsFile", "foo")]) of
                    (Right variable, Right file) ->
                      let found = lookup fooName (adFiles file)
                       in QC.counterexample "no file" (isJust found)
                            QC..&&. ((TE.encodeUtf8 <$> Map.lookup "foo" (adEnv variable)) QC.=== found)
                    _ -> QC.counterexample "delivery failed" False
        pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))
    ]
  where
    buildDir = "/build"
    fooName = ".attr-1bp7cri8hplaz6hbz0v4f0nl44rl84q1sg25kgwqzipzd1mv89ic"
    fooFile = buildDir </> fooName
    scratchOut = "/scratch/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb-p"
    outputDirs = [("out", scratchOut), ("dev", "/scratch/cccccccccccccccccccccccccccccccc-p-dev")]
    deliverWith config env =
      attrsDelivery config (mkTestBuildDrv "sh" (StorePath "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "p") "") {drvEnv = Map.fromList env} buildDir outputDirs
    deliver = deliverWith (defaultBuildConfig defaultStoreDir)
    storeDirs = ["/nix/store", "/home/user/nix/store", "C:\\nix\\store"]
    genRewritable =
      T.concat
        <$> QC.listOf
          ( QC.elements
              [ "/nix/store/",
                "/nix/store/dddddddddddddddddddddddddddddddd-dep",
                hashPlaceholder "out",
                hashPlaceholder "dev",
                hashPlaceholder "lib",
                "/",
                " ",
                "\\",
                "a",
                "\233",
                "\128512"
              ]
          )

-- | A spawned builder is told of its build directory and outputs where it
-- sees them, and is started, with its files written, where they are on
-- disk ('builderInvocation').  Here the two differ, as they do for a
-- builder whose view of the filesystem is not this process's.
testBuilderLayout :: IO [Bool]
testBuilderLayout = do
  putStrLn "builder/layout"
  sequence
    [ runTest "the environment names the build directory where the builder sees it, and it starts where the directory is on disk" $
        assertRight "invocation" (invoke [] []) $ \invocation ->
          assertEqual "the build directory's variables" (map (const (Just (T.pack seenTop))) topVars) (map (`Map.lookup` invEnv invocation) topVars)
            `andThen` assertEqual "the working directory" diskTop (invWorkDir invocation),
      runTest "outputs and their placeholders name each output where the builder sees it" $
        assertRight "invocation" (invoke [("lib", hashPlaceholder "out" <> "/lib")] ["-c", hashPlaceholder "dev"]) $ \invocation ->
          assertEqual "the outputs" [Just (T.pack seenOut), Just (T.pack seenDev)] (map (`Map.lookup` invEnv invocation) ["out", "dev"])
            `andThen` assertEqual "a placeholder in a variable" (Just (T.pack seenOut <> "/lib")) (Map.lookup "lib" (invEnv invocation))
            `andThen` assertEqual "the arguments" ["-c", seenDev] (invArgs invocation),
      runTest "a file passed is named where the builder sees it and written where the directory is on disk" $
        assertRight "invocation" (invoke [("foo", hashPlaceholder "out"), ("passAsFile", "foo")] []) $ \invocation ->
          case invFiles invocation of
            [(written, bytes)] ->
              assertEqual "written in" diskTop (takeDirectory written)
                `andThen` assertEqual "fooPath" (Just (T.pack (seenTop </> takeFileName written))) (Map.lookup "fooPath" (invEnv invocation))
                `andThen` assertEqual "what it holds" (TE.encodeUtf8 (T.pack seenOut)) bytes
            other -> Fail ("expected one file, got " <> T.pack (show other)),
      runTest "structured attributes are named where the builder sees them and hold the outputs it sees" $
        assertRight "invocation" (invoke [("__json", "{\"ref\":\"" <> hashPlaceholder "dev" <> "\"}")] []) $ \invocation ->
          assertEqual
            "the variables"
            [Just (T.pack (seenTop </> ".attrs.json")), Just (T.pack (seenTop </> ".attrs.sh"))]
            (map (`Map.lookup` invEnv invocation) ["NIX_ATTRS_JSON_FILE", "NIX_ATTRS_SH_FILE"])
            `andThen` assertEqual "the files" [diskTop </> ".attrs.json", diskTop </> ".attrs.sh"] (map fst (invFiles invocation))
            `andThen` assertEqual
              "the outputs they name"
              [(True, True, False) | _ <- invFiles invocation]
              [(BS.isInfixOf (encoded seenOut) bytes, BS.isInfixOf (encoded seenDev) bytes, BS.isInfixOf (TE.encodeUtf8 diskRoot) bytes) | (_, bytes) <- invFiles invocation],
      runTestM "nothing a builder is told of names a place on disk, and every file it finds is written in the directory on disk" $ do
        result <-
          QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} $
            QC.forAll genInvocation $ \(env, args) ->
              case invoke env args of
                Left err -> QC.counterexample (T.unpack err) False
                Right invocation ->
                  let told = Map.keys (invEnv invocation) <> Map.elems (invEnv invocation) <> map T.pack (invArgs invocation)
                      onDisk (path, bytes) = takeDirectory path == diskTop && not (BS.isInfixOf (TE.encodeUtf8 diskRoot) bytes)
                   in QC.counterexample (show invocation) (not (any (diskRoot `T.isInfixOf`) told) && all onDisk (invFiles invocation))
        pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))
    ]
  where
    seenTop = "/build"
    diskRoot = "/disk"
    diskTop = T.unpack diskRoot </> "builds" </> "0123"
    seenOut = "/nix/store/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb-p"
    seenDev = "/nix/store/cccccccccccccccccccccccccccccccc-p-dev"
    onDiskAt seen = BuildPlace seen (T.unpack diskRoot <> seen)
    layout = BuildLayout (BuildPlace seenTop diskTop) [("out", onDiskAt seenOut), ("dev", onDiskAt seenDev)] seenTop
    topVars = ["NIX_BUILD_TOP", "TMPDIR", "TEMPDIR", "TMP", "TEMP", if SI.os == "mingw32" then "USERPROFILE" else "HOME"]
    encoded = TE.encodeUtf8 . T.pack
    invoke :: [(Text, Text)] -> [Text] -> Either Text BuilderInvocation
    invoke env args =
      builderInvocation
        (defaultBuildConfig defaultStoreDir)
        (mkTestBuildDrv "sh" (StorePath "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" "p") "")
          { drvArgs = map TE.encodeUtf8 args,
            drvEnv = Map.fromList [(name, TE.encodeUtf8 value) | (name, value) <- env]
          }
        layout
    piece = QC.elements ["/nix/store/", hashPlaceholder "out", hashPlaceholder "dev", hashPlaceholder "lib", "/", " ", "x"]
    genValue = T.concat <$> QC.listOf piece
    genInvocation = do
      names <- QC.sublistOf ["a", "b", "c"]
      values <- traverse (const genValue) names
      passed <- QC.sublistOf names
      args <- QC.listOf genValue
      pure (("passAsFile", T.unwords passed) : zip names values, args)

-- | The sandbox's settings, decision, plan, chroot, macOS profile and
-- reports, which "Nix.Builder.Sandbox" works out without IO, held to
-- upstream's (local-derivation-goal.cc and globals.cc at 2.24.9, and
-- darwin-derivation-builder.cc and unix-derivation-builder.cc at 2.36.0).
testSandboxPure :: IO [Bool]
testSandboxPure = do
  putStrLn "builder/sandbox"
  sequence
    [ runTest "sandbox takes true, relaxed and false, under its own name and both of upstream's older ones" $
        foldr
          andThen
          Pass
          [ assertEqual (TE.decodeLatin1 (name <> " = " <> value)) (Right (Just mode)) (ncSandbox <$> Config.resolveConfig [Config.ConfigAssignment name value])
          | name <- ["sandbox", "build-use-chroot", "build-use-sandbox"],
            (value, mode) <- [("true", Sandbox.SandboxEnabled), ("relaxed", Sandbox.SandboxRelaxed), ("false", Sandbox.SandboxDisabled)]
          ],
      runTest "sandbox refuses a value upstream refuses, naming the setting whatever name set it" $
        assertEqual "refusal" (Left "option 'sandbox' has invalid value 'yes'") (ncSandbox <$> Config.resolveConfig [Config.ConfigAssignment "build-use-chroot" "yes"]),
      runTest "sandbox-paths replaces its default where an extra- assignment adds to it, keeping each path's bytes" $
        let resolved assignments = ncSandboxPaths <$> Config.resolveConfig assignments
         in assertEqual "plain, through an alias" (Right (Config.PathSetValue Config.ReplacesDefault (Set.fromList ["/a", "/b=\xff"]))) (resolved [Config.ConfigAssignment "build-chroot-dirs" "/b=\xff /a"])
              `andThen` assertEqual "extra" (Right (Config.PathSetValue Config.KeepsDefault (Set.fromList ["/c"]))) (resolved [Config.ConfigAssignment "extra-sandbox-paths" "/c"])
              `andThen` assertEqual "extra after plain, through an alias" (Right (Config.PathSetValue Config.ReplacesDefault (Set.fromList ["/a", "/c"]))) (resolved [Config.ConfigAssignment "sandbox-paths" "/a", Config.ConfigAssignment "extra-build-sandbox-paths" "/c"]),
      runTest "the other sandbox settings read upstream's values, and those whose defaults take IO stay unset" $
        let set =
              Config.resolveConfig
                [ Config.ConfigAssignment "sandbox-fallback" "false",
                  Config.ConfigAssignment "sandbox-build-dir" "/tmp/b",
                  Config.ConfigAssignment "sandbox-dev-shm-size" "10%",
                  Config.ConfigAssignment "require-drop-supplementary-groups" "yes",
                  Config.ConfigAssignment "ssl-cert-file" "/certs.pem"
                ]
            fields config = (ncSandboxFallback config, ncSandboxBuildDir config, ncSandboxDevShmSize config, ncRequireDropSupplementaryGroups config, ncSslCertFile config)
         in assertEqual "set" (Right (False, "/tmp/b", "10%", Just True, Just "/certs.pem")) (fields <$> set)
              `andThen` assertEqual "defaults" (True, "/build", "50%", Nothing, Nothing) (fields Config.defaultNixConfig)
              `andThen` assertEqual "sandbox unset" Nothing (ncSandbox Config.defaultNixConfig)
              `andThen` assertEqual "extra- reaches no scalar" (Right Config.defaultNixConfig) (Config.resolveConfig [Config.ConfigAssignment "extra-sandbox-build-dir" "/x"]),
      runTest "build, eval and trust take upstream's sandbox flags, its older names meaning --relaxed-sandbox as upstream's do, the later flag winning" $
        let mode args = Cli.optSandbox <$> Cli.parseArgs args
         in foldr
              andThen
              Pass
              [ assertEqual (T.pack (unwords arguments)) (Right (Just expected)) (mode arguments)
              | (flag, expected) <-
                  [ ("--sandbox", Sandbox.SandboxEnabled),
                    ("--no-sandbox", Sandbox.SandboxDisabled),
                    ("--relaxed-sandbox", Sandbox.SandboxRelaxed),
                    ("--build-use-chroot", Sandbox.SandboxRelaxed),
                    ("--build-use-sandbox", Sandbox.SandboxRelaxed)
                  ],
                command <- ["build", "eval", "trust"],
                arguments <- [[flag, command, "--expr", "1"], [command, flag, "--expr", "1"], [command, "--expr", "1", flag]]
              ]
              `andThen` assertEqual "the later" (Right (Just Sandbox.SandboxEnabled)) (mode ["build", "--no-sandbox", "--expr", "1", "--sandbox"])
              `andThen` assertEqual "none" (Right Nothing) (mode ["build", "--expr", "1"]),
      runTest "sandbox-paths entries are upstream's: TARGET=SOURCE split at the first =, a bare path naming itself, ? after an optional source" $
        assertEqual
          "entries"
          [ Sandbox.SandboxPath "/a" "/b=c" Sandbox.OptionalPath,
            Sandbox.SandboxPath "/bin/sh" "/host/sh" Sandbox.RequiredPath,
            Sandbox.SandboxPath "/dev/nvidiactl" "/dev/nvidiactl" Sandbox.OptionalPath,
            Sandbox.SandboxPath "/usr" "/usr" Sandbox.RequiredPath
          ]
          (Sandbox.parseSandboxPaths (Set.fromList ["/usr", "/bin/sh=/host/sh", "/dev/nvidiactl?", "/a=/b=c?", ""])),
      runTest "a build is sandboxed as upstream's mode, derivation type and __noChroot decide" $
        let plain = mkTestBuildDrv "/bin/sh" sandboxOutPath ""
            fixed = mkFixedOutputDrv "/bin/sh" sandboxOutPath "" "sha256" (T.replicate 64 "0")
            floating = plain {drvOutputs = [DerivationOutput "out" sandboxOutPath "r:sha256" ""]}
            noChroot drv = drv {drvEnv = Map.insert "__noChroot" "1" (drvEnv drv)}
            refusal = Left "derivation '/nix/store/dddddddddddddddddddddddddddddddd-x.drv' has '__noChroot' set, but that's not allowed when 'sandbox' is 'true'"
            cases =
              [ ("true, input-addressed", Sandbox.SandboxEnabled, plain, Right (Sandbox.Sandboxed Sandbox.PrivateNetwork)),
                ("true, fixed-output", Sandbox.SandboxEnabled, fixed, Right (Sandbox.Sandboxed Sandbox.HostNetwork)),
                ("true, floating", Sandbox.SandboxEnabled, floating, Right (Sandbox.Sandboxed Sandbox.PrivateNetwork)),
                ("true, __noChroot", Sandbox.SandboxEnabled, noChroot plain, refusal),
                ("relaxed, input-addressed", Sandbox.SandboxRelaxed, plain, Right (Sandbox.Sandboxed Sandbox.PrivateNetwork)),
                ("relaxed, fixed-output", Sandbox.SandboxRelaxed, fixed, Right Sandbox.Unsandboxed),
                ("relaxed, floating", Sandbox.SandboxRelaxed, floating, Right (Sandbox.Sandboxed Sandbox.PrivateNetwork)),
                ("relaxed, __noChroot", Sandbox.SandboxRelaxed, noChroot plain, Right Sandbox.Unsandboxed),
                ("false", Sandbox.SandboxDisabled, plain, Right Sandbox.Unsandboxed),
                ("false, __noChroot", Sandbox.SandboxDisabled, noChroot plain, Right Sandbox.Unsandboxed)
              ]
         in foldr andThen Pass [assertEqual label expected (Sandbox.sandboxUse (Just Sandbox.LinuxBackend) mode sandboxDrvPath drv) | (label, mode, drv, expected) <- cases],
      runTest "__noChroot among structured attributes is a JSON Boolean, and anything else is upstream's error" $
        let structured json = (mkTestBuildDrv "/bin/sh" sandboxOutPath "") {drvEnv = Map.fromList [("__json", json)]}
         in assertEqual "true" (Right Sandbox.Unsandboxed) (Sandbox.sandboxUse (Just Sandbox.LinuxBackend) Sandbox.SandboxRelaxed sandboxDrvPath (structured "{\"__noChroot\":true}"))
              `andThen` assertEqual "false" (Right (Sandbox.Sandboxed Sandbox.PrivateNetwork)) (Sandbox.sandboxUse (Just Sandbox.LinuxBackend) Sandbox.SandboxRelaxed sandboxDrvPath (structured "{\"__noChroot\":false}"))
              `andThen` assertEqual "not a Boolean" (Left "attribute '__noChroot' of derivation 'dddddddddddddddddddddddddddddddd-x.drv' must be a Boolean") (Sandbox.sandboxUse (Just Sandbox.LinuxBackend) Sandbox.SandboxEnabled sandboxDrvPath (structured "{\"__noChroot\":1}")),
      runTest "a plan binds each input and device node read-only, the build directory writable where the builder sees it, and no output" $
        let inputs = sandboxPlanInputs {Sandbox.piInputs = [storeIn "aaaa", storeIn "bbbb"], Sandbox.piOutputs = [storeIn "oooo"], Sandbox.piSandboxPaths = [Sandbox.SandboxPath (storeIn "oooo") "/host/out" Sandbox.RequiredPath]}
         in assertRight "plan" (Sandbox.sandboxPlan inputs) $ \plan ->
              assertEqual
                "binds"
                ( [Sandbox.Bind "/build" "/state/builds/h" Sandbox.ReadWriteBind Sandbox.RequiredPath Sandbox.SourceAsGiven]
                    ++ [Sandbox.Bind node node Sandbox.ReadOnlyBind Sandbox.RequiredPath Sandbox.SourceResolved | node <- sandboxDeviceNodes]
                    ++ [Sandbox.Bind path path Sandbox.ReadOnlyBind Sandbox.RequiredPath Sandbox.SourceAsGiven | path <- [storeIn "aaaa", storeIn "bbbb"]]
                )
                (Sandbox.planBinds plan),
      runTest "a fixed-output build's plan adds the host's network files and CA bundle, optional and read-only, after what is already there" $
        let inputs =
              sandboxPlanInputs
                { Sandbox.piNetwork = Sandbox.HostNetwork,
                  Sandbox.piCaFile = Just "/host/ca.pem",
                  Sandbox.piSandboxPaths = [Sandbox.SandboxPath "/dev/null" "/host/null" Sandbox.RequiredPath, Sandbox.SandboxPath "/etc/hosts" "/host/hosts" Sandbox.OptionalPath]
                }
            optional target source = Sandbox.Bind target source Sandbox.ReadOnlyBind Sandbox.OptionalPath Sandbox.SourceResolved
         in assertRight "plan" (Sandbox.sandboxPlan inputs) $ \plan ->
              assertEqual "/dev/null" [Sandbox.Bind "/dev/null" "/host/null" Sandbox.ReadOnlyBind Sandbox.RequiredPath Sandbox.SourceAsGiven] (filter ((== "/dev/null") . Sandbox.bindTarget) (Sandbox.planBinds plan))
                `andThen` assertEqual
                  "/etc"
                  [ Sandbox.Bind "/etc/hosts" "/host/hosts" Sandbox.ReadOnlyBind Sandbox.OptionalPath Sandbox.SourceAsGiven,
                    optional "/etc/resolv.conf" "/etc/resolv.conf",
                    optional "/etc/services" "/etc/services",
                    optional "/etc/ssl/certs/ca-certificates.crt" "/host/ca.pem"
                  ]
                  (filter (("/etc/" `BS.isPrefixOf`) . Sandbox.bindTarget) (Sandbox.planBinds plan)),
      runTest "a plan binds no device node of its own when /dev is bound whole" $
        assertRight "plan" (Sandbox.sandboxPlan sandboxPlanInputs {Sandbox.piSandboxPaths = [Sandbox.SandboxPath "/dev" "/dev" Sandbox.RequiredPath]}) $ \plan ->
          assertEqual "/dev binds" ["/dev"] (filter ("/dev" `BS.isPrefixOf`) (map Sandbox.bindTarget (Sandbox.planBinds plan))),
      runTest "a sandbox-build-dir that holds the store directory is upstream's error" $
        assertEqual "refusal" (Left "`sandbox-build-dir` must not contain the storeDir") (Sandbox.planBinds <$> Sandbox.sandboxPlan sandboxPlanInputs {Sandbox.piBuildDirSeen = "/nix"}),
      runTestM "every plan binds each input once, no output, the build directory alone writable, and its targets in sorted order" $ do
        result <-
          QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} $
            QC.forAll genSandboxPlanInputs $ \inputs ->
              case Sandbox.sandboxPlan inputs of
                Left reason -> QC.counterexample (T.unpack reason) False
                Right plan ->
                  let binds = Sandbox.planBinds plan
                      targets = map Sandbox.bindTarget binds
                      kept = filter (`notElem` Sandbox.piOutputs inputs) (Sandbox.piInputs inputs)
                      readOnlyOnce path = [Sandbox.bindAccess bind | bind <- binds, Sandbox.bindTarget bind == path] == [Sandbox.ReadOnlyBind]
                      writableTargets = [Sandbox.bindTarget bind | bind <- binds, Sandbox.bindAccess bind == Sandbox.ReadWriteBind]
                   in QC.counterexample (show binds) $
                        and [readOnlyOnce path | path <- kept]
                          && not (any (`elem` Sandbox.piOutputs inputs) targets)
                          && writableTargets == [Sandbox.piBuildDirSeen inputs]
                          && and (zipWith (<) targets (drop 1 targets))
        pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result))),
      runTest "a plan renders as the helper reads it: the version line, the length of the rest, then the backend, keys and values, each its length, a colon, its bytes and a NUL" $
        let plan = Sandbox.SandboxPlan "/r" "/build" "50%" Sandbox.PrivateNetwork Sandbox.TolerateKeptGroups [Sandbox.Bind "/b" "/s" Sandbox.ReadOnlyBind Sandbox.OptionalPath Sandbox.SourceResolved, Sandbox.Bind "/t" "/u" Sandbox.ReadWriteBind Sandbox.RequiredPath Sandbox.SourceAsGiven] ["LD_PRELOAD=/x.so", "out=/nix/store/o"]
            body =
              BS.concat
                [ "7:backend\NUL5:linux\NUL",
                  "4:root\NUL2:/r\NUL9:build-dir\NUL6:/build\NUL12:dev-shm-size\NUL3:50%\NUL",
                  "11:sandbox-uid\NUL4:1000\NUL11:sandbox-gid\NUL3:100\NUL7:network\NUL7:private\NUL11:drop-groups\NUL8:tolerate\NUL",
                  "4:bind\NUL2:/b\NUL2:/s\NUL3:roc\NUL4:bind\NUL2:/t\NUL2:/u\NUL1:-\NUL",
                  "3:env\NUL16:LD_PRELOAD=/x.so\NUL3:env\NUL16:out=/nix/store/o\NUL"
                ]
         in assertEqual "bytes" ("nova-nix-sandbox-plan 4\n" <> BS8.pack (show (BS.length body)) <> "\n" <> body) (Sandbox.renderPlan plan),
      runTestM "a rendered plan reads back into exactly its fields, whatever bytes they hold" $ do
        result <-
          QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} $
            QC.forAll genPlanAnyBytes $
              \plan -> readRenderedPlan (Sandbox.renderPlan plan) QC.=== Just plan
        pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result))),
      runTestM "the helper's parser reads a rendered plan back into exactly its fields, and refuses one it cannot use as C strings" $ do
        result <-
          QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} $
            QC.checkCoverage $
              QC.forAll genPlanAnyBytes $ \plan ->
                QC.cover 30 (helperCanUse plan) "the helper can use it" $
                  QC.cover 5 (not (helperCanUse plan) && helperCanUse (withoutNuls plan)) "a NUL alone keeps the helper from using it" $
                    QC.ioProperty $ do
                      let tokens = renderedPlanTokens plan
                      echoed <- helperPlanEcho tokens
                      pure (echoed QC.=== if helperCanUse plan then Right tokens else Left invalidPlan)
        pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result))),
      runTestM "the helper's parser takes the backend, each key once and in order, then binds, then entries, and nothing else" $ do
        let framed = BS.concat . map planToken
            singletons = ["backend", "linux", "root", "/r", "build-dir", "/build", "dev-shm-size", "50%", "sandbox-uid", "1000", "sandbox-gid", "100", "network", "private", "drop-groups", "tolerate"]
            bind = ["bind", "/t", "/s", "r"]
            entry value = ["env", value]
            -- Entries upstream's execve passes as they are: an empty name,
            -- no '=' at all, and nothing.
            whole = singletons ++ bind ++ entry "=x" ++ entry "no-equals" ++ entry ""
        accepted <- helperPlanEcho (framed whole)
        refused <-
          traverse
            (helperPlanEcho . framed)
            [ singletons ++ ["root", "/again"],
              singletons ++ entry "a=b" ++ bind,
              drop 2 singletons,
              take 2 singletons ++ take 2 (drop 4 singletons) ++ take 2 (drop 2 singletons) ++ drop 6 singletons,
              ["backend", "freebsd"] ++ drop 2 singletons,
              ["backend", "darwin"] ++ drop 2 singletons,
              singletons ++ ["mystery", "x"],
              singletons ++ ["bind", "/t", "/s"],
              singletons ++ ["bind", "t", "/s", "-"],
              singletons ++ ["bind", "/t", "/s", ""],
              singletons ++ ["env"]
            ]
        rawRefused <-
          traverse
            (helperPlanEcho . (framed singletons <>))
            ["x", "02:ab\NUL", "3:env\NUL3:ab\NUL", "3:env\NUL2:ab", "3:env\NUL3:a\NULb\NUL", "99999999999999999999999:x\NUL"]
        pure $
          assertEqual "accepted" (Right (framed whole)) accepted
            `andThen` assertEqual "refused" (replicate 11 (Left invalidPlan)) refused
            `andThen` assertEqual "refused framing" (replicate 6 (Left invalidPlan)) rawRefused,
      runTestM "the helper's walk of a bind's mounts finds the mount points the library reads, whatever the text" $ do
        result <-
          QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} $
            QC.checkCoverage $
              QC.forAll genMountinfo $ \(mountinfo, dir) ->
                let expected = Spawn.mountPointsWithin mountinfo dir
                 in QC.cover 10 (not (null expected)) "a mount point is found" $
                      QC.ioProperty $ do
                        found <- helperMountsWithin mountinfo dir
                        pure (found QC.=== expected)
        pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result))),
      runTest "the build's /etc/passwd and /etc/group are upstream's" $
        assertEqual "passwd" "root:x:0:0:Nix build user:/build:/noshell\nnixbld:x:1000:100:Nix build user:/build:/noshell\nnobody:x:65534:65534:Nobody:/:/noshell\n" (Sandbox.passwdFile "/build")
          `andThen` assertEqual "group" "root:x:0:\nnixbld:!:100:\nnogroup:x:65534:\n" Sandbox.groupFile,
      runTest "the chroot holds /etc/hosts for a build with a network of its own, nsswitch.conf for one with the host's, and no /dev of its own under a bound one" $
        let files network paths = [path | Sandbox.SkeletonFile path _ <- Sandbox.chrootSkeleton "/nix/store" "/build" network paths]
            devEntries paths = [entry | entry <- Sandbox.chrootSkeleton "/nix/store" "/build" Sandbox.PrivateNetwork paths, skeletonPath entry == "/dev" || "/dev/" `BS.isPrefixOf` skeletonPath entry]
         in assertEqual "private" ["/etc/passwd", "/etc/group", "/etc/hosts"] (files Sandbox.PrivateNetwork [])
              `andThen` assertEqual "host" ["/etc/passwd", "/etc/group", "/etc/nsswitch.conf"] (files Sandbox.HostNetwork [])
              `andThen` assertEqual "/dev bound" [] (devEntries [Sandbox.SandboxPath "/dev" "/dev" Sandbox.RequiredPath])
              `andThen` assertEqual "/dev entries" 7 (length (devEntries [])),
      runTest "each stage reads back from the record the helper writes for it"
        $ foldr andThen Pass
        $ [assertEqual (TE.decodeLatin1 (Sandbox.setupStageName stage)) (Right (Just (Sandbox.StatusRecord stage 13 (Just 4)))) (Sandbox.parseStatus Sandbox.setupStageName (Sandbox.setupStageName stage <> " 13 4\n")) | stage <- [minBound .. maxBound]]
          ++ [assertEqual (TE.decodeLatin1 (Sandbox.probeStageName stage)) (Right (Just (Sandbox.StatusRecord stage 0 Nothing))) (Sandbox.parseStatus Sandbox.probeStageName (Sandbox.probeStageName stage <> " 0 -1\n")) | stage <- [minBound .. maxBound]],
      runTest "a status is nothing, which is no failure, or one whole record, and anything else is refused" $
        let parse = Sandbox.parseStatus Sandbox.setupStageName
         in assertEqual "nothing" (Right Nothing) (parse "")
              `andThen` assertLeft "no newline" (parse "bind 2 0")
              `andThen` assertLeft "two values" (parse "bind 2\n")
              `andThen` assertLeft "two records" (parse "bind 2 0\nexec 2 -1\n")
              `andThen` assertLeft "unknown stage" (parse "mystery 2 0\n")
              `andThen` assertLeft "signed errno" (parse "bind -2 0\n"),
      runTestM "reading a status is total over any bytes" $ do
        result <-
          QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} $
            QC.forAll genStatusBytes $ \bytes ->
              case Sandbox.parseStatus Sandbox.setupStageName bytes of
                Left reason -> QC.property (T.length reason > 0)
                Right Nothing -> QC.property (BS.null bytes)
                Right (Just record) -> QC.property (Sandbox.recordErrno record >= 0)
        pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result))),
      runTest "a failed step is upstream's words, naming the paths a bind's index points at" $
        let plan = Sandbox.SandboxPlan "/r" "/build" "50%" Sandbox.PrivateNetwork Sandbox.TolerateKeptGroups [Sandbox.Bind "/b" "/s" Sandbox.ReadOnlyBind Sandbox.RequiredPath Sandbox.SourceAsGiven, Sandbox.Bind "/build" "/state/builds/h" Sandbox.ReadWriteBind Sandbox.RequiredPath Sandbox.SourceAsGiven, Sandbox.Bind "/t" "/u" Sandbox.ReadWriteBind Sandbox.RequiredPath Sandbox.SourceAsGiven] []
            message stage number index = Sandbox.setupFailureMessage plan "/bin/sh" (Sandbox.StatusRecord stage number index)
         in assertEqual "bind" "while setting up the build environment: bind mount from '/u' to '/r/t' failed: No such file or directory" (message Sandbox.StageBind 2 (Just 2))
              `andThen` assertEqual "build directory" "while setting up the build environment: changing into '/state/builds/h': No such file or directory" (message Sandbox.StageChdir 2 Nothing)
              `andThen` assertEqual "pipe" "while setting up the build environment: creating pipe: Too many open files" (message Sandbox.StageSync 24 Nothing)
              `andThen` assertEqual "source" "while setting up the build environment: getting attributes of path '/s': No such file or directory" (message Sandbox.StageBindSource 2 (Just 0))
              `andThen` assertEqual "setgroups refused" "while setting up the build environment: setgroups failed. Set the require-drop-supplementary-groups option to false to skip this step." (message Sandbox.StageSetgroups 1 Nothing)
              `andThen` assertEqual "clone" "while setting up the build environment: unable to start build process" (message Sandbox.StageClone 1 Nothing)
              `andThen` assertEqual "exec" "while setting up the build environment: executing '/bin/sh': No such file or directory" (message Sandbox.StageExec 2 Nothing),
      runTest "a run of the helper is a failed step when it reports one, else the builder's exit, unless the plan never reached it" $
        let plan = Sandbox.SandboxPlan "/r" "/build" "50%" Sandbox.PrivateNetwork Sandbox.TolerateKeptGroups [] []
            outcome = Sandbox.helperOutcome Sandbox.setupStageName (Sandbox.setupFailureMessage plan "/bin/sh")
         in assertEqual "record" (Sandbox.SetupIncomplete "while setting up the build environment: cannot set host name: Operation not permitted") (outcome (Right ()) (ExitFailure 1) "hostname 1 -1\n")
              `andThen` assertEqual "builder" (Sandbox.BuilderRan (ExitFailure 3)) (outcome (Right ()) (ExitFailure 3) "")
              `andThen` assertEqual "undelivered" (Sandbox.SetupIncomplete "the sandbox helper ended before it read its plan: broken pipe") (outcome (Left "broken pipe") (ExitFailure 1) ""),
      runTest "a system without the namespaces is refused in upstream's words, and a fallback warns naming the cause" $
        let apparmor = Sandbox.ProbeFailed (Sandbox.StatusRecord Sandbox.ProbeAppArmor 0 Nothing)
         in assertEqual "refusal" "this system does not support the kernel namespaces that are required for sandboxing; use '--no-sandbox' to disable sandboxing" (Sandbox.unavailableMessage (Just Sandbox.LinuxBackend) apparmor)
              `andThen` assertEqual "warning" "warning: building without a sandbox, since AppArmor restricts unprivileged user namespaces ('/proc/sys/kernel/apparmor_restrict_unprivileged_userns' is 1), and no AppArmor profile lets nova-nix-sandbox use them" (Sandbox.fallbackWarning apparmor)
              `andThen` assertEqual "no helper" "the sandbox helper 'nova-nix-sandbox' was not found beside nova-nix; use '--no-sandbox' to disable sandboxing" (Sandbox.unavailableMessage (Just Sandbox.LinuxBackend) Sandbox.HelperNotFound),
      runTest "a sandbox path that is not absolute, or leads out of the sandbox, is refused before the plan is made" $
        let refusal target = Left ("the sandbox path '" <> target <> "' is not an absolute path without a '..' component, so it names no path in the sandbox")
            planWith paths = Sandbox.sandboxPlan sandboxPlanInputs {Sandbox.piSandboxPaths = paths}
            entry target = [Sandbox.SandboxPath target "/host" Sandbox.RequiredPath]
         in assertEqual "up and out" (refusal "/a/../../etc") (planWith (entry "/a/../../etc"))
              `andThen` assertEqual "relative" (refusal "etc") (planWith (entry "etc"))
              `andThen` assertEqual "the build directory" (refusal "/build/..") (Sandbox.sandboxPlan sandboxPlanInputs {Sandbox.piBuildDirSeen = "/build/.."})
              `andThen` assertRight "a name that starts with two dots" (planWith (entry "/a/..b/c")) (const Pass),
      runTest "a sandboxed build is told upstream's HOME, and as root is refused for want of build users" $
        assertEqual "home" "/homeless-shelter" Sandbox.sandboxHome
          `andThen` assertEqual "root" "a sandboxed build as root needs build users, which nova-nix does not have yet; run nova-nix as a user other than root, or pass '--no-sandbox' to build without the sandbox" Sandbox.rootRefusal,
      runTest "allowed-impure-host-deps keeps its default under extra- and replaces it otherwise, and darwin-log-sandbox-violations is a Boolean that defaults off" $
        assertEqual "plain" (Right (Config.PathSetValue Config.ReplacesDefault (Set.fromList ["/a", "/b"]))) (ncAllowedImpureHostDeps <$> Config.resolveConfig [Config.ConfigAssignment "allowed-impure-host-deps" "/b /a"])
          `andThen` assertEqual "extra" (Right (Config.PathSetValue Config.KeepsDefault (Set.fromList ["/c"]))) (ncAllowedImpureHostDeps <$> Config.resolveConfig [Config.ConfigAssignment "extra-allowed-impure-host-deps" "/c"])
          `andThen` assertEqual "default" (Config.PathSetValue Config.KeepsDefault Set.empty) (ncAllowedImpureHostDeps Config.defaultNixConfig)
          `andThen` assertEqual "log" (Right True) (ncDarwinLogSandboxViolations <$> Config.resolveConfig [Config.ConfigAssignment "darwin-log-sandbox-violations" "yes"])
          `andThen` assertEqual "log default" False (ncDarwinLogSandboxViolations Config.defaultNixConfig)
          `andThen` assertEqual "log refusal" (Left "Boolean setting 'darwin-log-sandbox-violations' has invalid value 'sometimes'") (ncDarwinLogSandboxViolations <$> Config.resolveConfig [Config.ConfigAssignment "darwin-log-sandbox-violations" "sometimes"]),
      runTest "on macOS sandbox = true refuses a derivation that sets __sandboxProfile, which relaxed sandboxes, and Linux reads no profile" $
        let profiled = (mkTestBuildDrv "/bin/sh" sandboxOutPath "") {drvEnv = Map.fromList [("__sandboxProfile", "(allow default)")]}
            emptyProfile = (mkTestBuildDrv "/bin/sh" sandboxOutPath "") {drvEnv = Map.fromList [("__sandboxProfile", "")]}
            structured json = (mkTestBuildDrv "/bin/sh" sandboxOutPath "") {drvEnv = Map.fromList [("__json", json)]}
            darwin = Sandbox.sandboxUse (Just Sandbox.DarwinBackend)
         in assertEqual "true" (Left "derivation '/nix/store/dddddddddddddddddddddddddddddddd-x.drv' specifies a sandbox profile, but this is only allowed when 'sandbox' is 'relaxed'") (darwin Sandbox.SandboxEnabled sandboxDrvPath profiled)
              `andThen` assertEqual "true, structured" (Left "derivation '/nix/store/dddddddddddddddddddddddddddddddd-x.drv' specifies a sandbox profile, but this is only allowed when 'sandbox' is 'relaxed'") (darwin Sandbox.SandboxEnabled sandboxDrvPath (structured "{\"__sandboxProfile\":\"(allow default)\"}"))
              `andThen` assertEqual "true, not a string" (Left "while parsing derivation '/nix/store/dddddddddddddddddddddddddddddddd-x.drv': while parsing attribute \"__sandboxProfile\": Expected JSON value to be of type 'string' but it is of type 'number': 1") (darwin Sandbox.SandboxEnabled sandboxDrvPath (structured "{\"__sandboxProfile\":1}"))
              `andThen` assertEqual "true, empty" (Right (Sandbox.Sandboxed Sandbox.PrivateNetwork)) (darwin Sandbox.SandboxEnabled sandboxDrvPath emptyProfile)
              `andThen` assertEqual "true, __noChroot first" (Left "derivation '/nix/store/dddddddddddddddddddddddddddddddd-x.drv' has '__noChroot' set, but that's not allowed when 'sandbox' is 'true'") (darwin Sandbox.SandboxEnabled sandboxDrvPath profiled {drvEnv = Map.insert "__noChroot" "1" (drvEnv profiled)})
              `andThen` assertEqual "relaxed" (Right (Sandbox.Sandboxed Sandbox.PrivateNetwork)) (darwin Sandbox.SandboxRelaxed sandboxDrvPath profiled)
              `andThen` assertEqual "Linux" (Right (Sandbox.Sandboxed Sandbox.PrivateNetwork)) (Sandbox.sandboxUse (Just Sandbox.LinuxBackend) Sandbox.SandboxEnabled sandboxDrvPath profiled),
      runTest "a derivation's macOS options are upstream's attributes, from its environment or its structured attributes, a mistyped one refused in 2.36.0's words" $
        let withEnv entries = (mkTestBuildDrv "/bin/sh" sandboxOutPath "") {drvEnv = Map.fromList entries}
            options = Sandbox.darwinOptions sandboxDrvPath
         in assertEqual "none" (Right (Sandbox.DarwinOptions "" Set.empty Sandbox.LocalNetworkingDenied)) (options (withEnv []))
              `andThen` assertEqual "environment" (Right (Sandbox.DarwinOptions "(allow x)" (Set.fromList ["/usr/lib/a", "/bin/sh"]) Sandbox.LocalNetworkingAllowed)) (options (withEnv [("__sandboxProfile", "(allow x)"), ("__impureHostDeps", " /usr/lib/a\t/bin/sh\n/usr/lib/a "), ("__darwinAllowLocalNetworking", "1")]))
              `andThen` assertEqual "structured" (Right (Sandbox.DarwinOptions "(allow y)" (Set.fromList ["/a", "/b"]) Sandbox.LocalNetworkingAllowed)) (options (withEnv [("__json", "{\"__sandboxProfile\":\"(allow y)\",\"__impureHostDeps\":[\"/b\",\"/a\"],\"__darwinAllowLocalNetworking\":true}")]))
              `andThen` assertEqual "an element not a string" (Left "while parsing derivation '/nix/store/dddddddddddddddddddddddddddddddd-x.drv': while parsing attribute \"__impureHostDeps\": Expected JSON value to be of type 'string' but it is of type 'number': 1") (options (withEnv [("__json", "{\"__impureHostDeps\":[\"/a\",1]}")]))
              `andThen` assertEqual "not a list" (Left "while parsing derivation '/nix/store/dddddddddddddddddddddddddddddddd-x.drv': while parsing attribute \"__impureHostDeps\": Expected JSON value to be of type 'array' but it is of type 'string': \"/a\"") (options (withEnv [("__json", "{\"__impureHostDeps\":\"/a\"}")])),
      runTest "a macOS build's paths are upstream's pathsInChroot, an input replacing an entry for its target, and their ancestry" $
        assertRight "paths" (Sandbox.darwinPathsInSandbox darwinPathInputs {Sandbox.dpiSandboxPaths = [Sandbox.SandboxPath (storeIn "aaaa") (storeIn "aaaa") Sandbox.OptionalPath, Sandbox.SandboxPath "/bin/sh" "/bin/sh" Sandbox.RequiredPath, Sandbox.SandboxPath "/opt/x" "/opt/x" Sandbox.OptionalPath], Sandbox.dpiSandboxClosure = [storeIn "cccc"], Sandbox.dpiImpureHostDeps = Set.fromList ["/usr/lib/libz.dylib"], Sandbox.dpiAllowedImpureHostDeps = Set.fromList ["/usr/lib"], Sandbox.dpiInputs = [storeIn "aaaa", storeIn "bbbb"]}) $ \paths ->
          assertEqual
            "entries"
            [ Sandbox.DarwinPath "/bin/sh" "/bin/sh" Sandbox.RequiredPath,
              Sandbox.DarwinPath (storeIn "aaaa") (storeIn "aaaa") Sandbox.RequiredPath,
              Sandbox.DarwinPath (storeIn "bbbb") (storeIn "bbbb") Sandbox.RequiredPath,
              Sandbox.DarwinPath (storeIn "cccc") (storeIn "cccc") Sandbox.RequiredPath,
              Sandbox.DarwinPath "/opt/x" "/opt/x" Sandbox.OptionalPath,
              Sandbox.DarwinPath "/private/state/builds/h" "/private/state/builds/h" Sandbox.RequiredPath,
              Sandbox.DarwinPath "/usr/lib/libz.dylib" "/usr/lib/libz.dylib" Sandbox.OptionalPath
            ]
            (Sandbox.dpPaths paths)
            `andThen` assertEqual
              "ancestry"
              ["/", "/bin", "/nix", "/nix/store", "/opt", "/private", "/private/state", "/private/state/builds", "/state", "/state/builds", "/usr", "/usr/lib"]
              (Sandbox.dpAncestry paths),
      runTest "a macOS build's impure host path is upstream's to refuse unless an allowed-impure-host-deps entry holds it, compared by components" $
        let refused dep = Left ("derivation '/nix/store/dddddddddddddddddddddddddddddddd-x.drv' requested impure path '" <> dep <> "', but it was not in allowed-impure-host-deps")
            planned deps allowed = Sandbox.dpPaths <$> Sandbox.darwinPathsInSandbox darwinPathInputs {Sandbox.dpiImpureHostDeps = Set.fromList deps, Sandbox.dpiAllowedImpureHostDeps = Set.fromList allowed}
            optional path = Sandbox.DarwinPath path path Sandbox.OptionalPath
         in assertEqual "a sibling sharing a prefix" (refused "/usr/libexec/x") (planned ["/usr/libexec/x"] ["/usr/lib"])
              `andThen` assertEqual "the entry itself, given loosely" (Right [Sandbox.DarwinPath "/private/state/builds/h" "/private/state/builds/h" Sandbox.RequiredPath, optional "/usr/lib/./a/../"]) (planned ["/usr/lib/./a/../"] ["/usr//lib/"])
              `andThen` assertEqual "beneath the root" (Right [Sandbox.DarwinPath "/private/state/builds/h" "/private/state/builds/h" Sandbox.RequiredPath, optional "/x"]) (planned ["/x"] ["/"])
              `andThen` assertEqual "none allowed" (refused "/bin/sh") (planned ["/bin/sh"] [])
              `andThen` assertEqual "relative" (Left "not an absolute path: \"lib\"") (planned ["lib"] ["/usr/lib"])
              `andThen` assertEqual "a sandbox path that is relative" (Left "the sandbox path 'bin/sh' is not an absolute path without a '..' component, so it names no path in the sandbox") (Sandbox.dpPaths <$> Sandbox.darwinPathsInSandbox darwinPathInputs {Sandbox.dpiSandboxPaths = [Sandbox.SandboxPath "bin/sh" "bin/sh" Sandbox.RequiredPath]})
              `andThen` assertEqual "a sandbox path with a '..' component, as upstream's" (Right [Sandbox.DarwinPath "/private/state/builds/h" "/private/state/builds/h" Sandbox.RequiredPath, Sandbox.DarwinPath "/usr/lib/../lib" "/usr/lib/../lib" Sandbox.RequiredPath]) (Sandbox.dpPaths <$> Sandbox.darwinPathsInSandbox darwinPathInputs {Sandbox.dpiSandboxPaths = [Sandbox.SandboxPath "/usr/lib/../lib" "/usr/lib/../lib" Sandbox.RequiredPath]}),
      runTest "a macOS build's path adds to its profile what lstat found there, in upstream's words when it may not" $
        let required = Sandbox.DarwinPath "/a" "/a" Sandbox.RequiredPath
            optional = Sandbox.DarwinPath "/a" "/a" Sandbox.OptionalPath
            errno (Errno number) = fromIntegral number
         in assertEqual "directory" (Right (Just (Sandbox.Subpath "/a"))) (Sandbox.profileFilter required Sandbox.FoundDirectory)
              `andThen` assertEqual "other" (Right (Just (Sandbox.Literal "/a"))) (Sandbox.profileFilter required Sandbox.FoundOther)
              `andThen` assertEqual "missing, optional" (Right Nothing) (Sandbox.profileFilter optional (Sandbox.LstatFailed (errno eNOTDIR)))
              `andThen` assertEqual "missing, required" (Left "while setting up the build environment: getting attributes of required path '/a: No such file or directory") (Sandbox.profileFilter required (Sandbox.LstatFailed (errno eNOENT)))
              `andThen` assertEqual "unreadable" (Left "while setting up the build environment: getting status of \"/a\": Permission denied") (Sandbox.profileFilter optional (Sandbox.LstatFailed (errno eACCES)))
              `andThen` assertEqual "mapped elsewhere" (Left "while setting up the build environment: can't map \"/bin/sh\" to \"/host/sh\": mismatched impure paths not supported on Darwin") (Sandbox.darwinPathTarget (Sandbox.DarwinPath "/bin/sh" "/host/sh" Sandbox.RequiredPath)),
      runTestM "a macOS build's profile is the checked-in one for the same build" $ do
        cwd <- Dir.getCurrentDirectory
        expected <- BS.readFile (cwd </> "test" </> "data" </> "sandbox-darwin.sb")
        pure (assertEqual "profile" expected (Sandbox.darwinProfile sampleDarwinProfile)),
      runTest "a macOS profile has the network rules for a fixed-output build alone, and logs refusals under darwin-log-sandbox-violations" $
        let private = Sandbox.darwinProfile sampleDarwinProfile {Sandbox.dprNetwork = Sandbox.PrivateNetwork}
            logged = Sandbox.darwinProfile sampleDarwinProfile {Sandbox.dprViolationLog = Sandbox.ViolationsLogged}
            hostRules = Sandbox.darwinProfile sampleDarwinProfile
         in assertEqual "host network" True ("(allow network* (local ip) (remote ip))" `BS.isInfixOf` hostRules)
              `andThen` assertEqual "private network" False ("(allow network* (local ip) (remote ip))" `BS.isInfixOf` private)
              `andThen` assertEqual "trustd, private" False ("com.apple.trustd" `BS.isInfixOf` private)
              `andThen` assertEqual "unlogged" True ("(version 1)\n(deny default (with no-log))\n" `BS.isPrefixOf` hostRules)
              `andThen` assertEqual "logged" True ("(version 1)\n(deny default)\n" `BS.isPrefixOf` logged),
      runTestM "a macOS profile names each path once, escaped, a store path in the group its hash's first character guards, and writes no form without a filter" $ do
        result <-
          QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} $
            QC.checkCoverage $
              QC.forAll genDarwinProfile $ \profile ->
                let renderedLines = BS8.lines (Sandbox.darwinProfile profile)
                    storePrefix = Sandbox.dprStoreDir profile <> "/"
                    inStore pathFilter = maybe False (not . BS.null) (BS.stripPrefix storePrefix (profileFilterPath pathFilter))
                    (storePaths, otherPaths) = partition inStore (Sandbox.dprPaths profile)
                    forms = profileForms "(allow file-read* process-exec" renderedLines
                    (bucketForms, otherForms) = partition (("\t(require-all" `elem`) . take 1) forms
                    -- A bucket's lines: the guard, its prefix, the
                    -- alternatives, its filters, and the closing line.
                    bucketFilters form = [line | line <- form, "\t\t\t(" `BS.isPrefixOf` line]
                    bucketPrefix form = listToMaybe (mapMaybe (BS.stripPrefix "\t\t(prefix \"" >=> BS.stripSuffix "\")") form)
                    -- A filter line's quoted path, past its kind.
                    quotedPath = BS.drop 1 . BS8.dropWhile (/= ' ') . BS.drop 3
                    guarded form = case bucketPrefix form of
                      Just prefix -> not (null (bucketFilters form)) && all ((("\"" <> prefix) `BS.isPrefixOf`) . quotedPath) (bucketFilters form)
                      Nothing -> False
                    expectedStore = sort (map (("\t\t" <>) . profileFilterLine) storePaths)
                    -- A form opened on a line of its own and closed on
                    -- the next holds no filter.
                    openedEmpty = [line | (line, next) <- zip renderedLines (drop 1 renderedLines), "(allow " `BS.isPrefixOf` line, next == ")"]
                 in QC.cover 5 (length bucketForms > 1) "the store paths take more than one group"
                      $ QC.cover 5 (not (null otherForms)) "other paths take a form of their own"
                      $ QC.counterexample (BS8.unpack (BS8.unlines renderedLines))
                      $ concat otherForms == map profileFilterLine otherPaths
                        && length otherForms <= 1
                        && sort (concatMap bucketFilters bucketForms) == expectedStore
                        && all guarded bucketForms
                        && null openedEmpty
        pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result))),
      runTest "a macOS profile groups the store paths after every other host path, a group to each first character, in byte order" $
        let profile =
              sampleDarwinProfile
                { Sandbox.dprPaths =
                    [ Sandbox.Subpath "/nix/store/bbbb-two",
                      Sandbox.Literal "/bin/sh",
                      Sandbox.Subpath "/nix/store/aaaa-one",
                      Sandbox.Subpath "/nix/store/bbbb-three"
                    ]
                }
            renderedLines = BS8.lines (Sandbox.darwinProfile profile)
            pathLines = filter (\line -> "(" `BS.isPrefixOf` BS8.dropWhile (== '\t') line && "\t" `BS.isPrefixOf` line && any (`BS.isInfixOf` line) ["/bin/sh", "/nix/store/aaaa-", "/nix/store/bbbb-"]) renderedLines
            prefixes = [line | line <- renderedLines, "\t\t(prefix " `BS.isPrefixOf` line]
         in assertEqual "paths" ["\t(literal \"/bin/sh\")", "\t\t\t(subpath \"/nix/store/aaaa-one\")", "\t\t\t(subpath \"/nix/store/bbbb-two\")", "\t\t\t(subpath \"/nix/store/bbbb-three\")"] pathLines
              `andThen` assertEqual "guards" ["\t\t(prefix \"/nix/store/a\")", "\t\t(prefix \"/nix/store/b\")"] prefixes,
      runTest "a macOS build's profile parameters are upstream's names, the temporary directory its own without trailing slashes" $
        assertEqual "local networking" [("_NIX_BUILD_TOP", "/b//"), ("_GLOBAL_TMP_DIR", "/b"), ("_ALLOW_LOCAL_NETWORKING", "1")] (Sandbox.darwinParameters "/b//" Sandbox.LocalNetworkingAllowed)
          `andThen` assertEqual "none" [("_NIX_BUILD_TOP", "/b"), ("_GLOBAL_TMP_DIR", "/b")] (Sandbox.darwinParameters "/b" Sandbox.LocalNetworkingDenied),
      runTest "a macOS plan renders as the helper reads it: the backend, the build directory and the profile, each parameter's name and value, then entries" $
        let plan = Sandbox.DarwinPlan "/b" "(version 1)" [("_NIX_BUILD_TOP", "/b")] ["out=/nix/store/o"]
            body =
              BS.concat
                [ "7:backend\NUL6:darwin\NUL9:build-dir\NUL2:/b\NUL7:profile\NUL11:(version 1)\NUL",
                  "5:param\NUL14:_NIX_BUILD_TOP\NUL2:/b\NUL",
                  "3:env\NUL16:out=/nix/store/o\NUL"
                ]
         in assertEqual "bytes" ("nova-nix-sandbox-plan 4\n" <> BS8.pack (show (BS.length body)) <> "\n" <> body) (Sandbox.renderDarwinPlan plan),
      runTestM "the helper's parser reads a rendered macOS plan back into exactly its fields, and refuses one it cannot use" $ do
        result <-
          QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} $
            QC.checkCoverage $
              QC.forAll genDarwinPlan $ \plan ->
                let usable = darwinPlanUsable plan
                 in QC.cover 30 usable "the helper can use it" $
                      QC.cover 5 (not usable) "the helper cannot use it" $
                        QC.ioProperty $ do
                          let tokens = BS.drop 1 (BS8.dropWhile (/= '\n') (BS.drop (BS.length Sandbox.planVersionLine) (Sandbox.renderDarwinPlan plan)))
                          echoed <- helperPlanEcho tokens
                          pure (echoed QC.=== if usable then Right tokens else Left invalidPlan)
        pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result))),
      runTestM "the helper's parser takes a macOS plan's keys once and in order, then params, then entries, and nothing else" $ do
        let framed = BS.concat . map planToken
            singletons = ["backend", "darwin", "build-dir", "/b", "profile", "(version 1)"]
            whole = singletons ++ ["param", "_A", "1", "param", "_B", ""] ++ ["env", "a=b"]
        accepted <- helperPlanEcho (framed whole)
        refused <-
          traverse
            (helperPlanEcho . framed)
            [ take 4 singletons,
              singletons ++ ["bind", "/t", "/s", "r"],
              singletons ++ ["param", "", "1"],
              singletons ++ ["param", "_A"],
              singletons ++ ["env", "a=b", "param", "_A", "1"],
              ["backend", "darwin", "profile", "(version 1)", "build-dir", "/b"],
              ["backend", "darwin", "build-dir", "b", "profile", "(version 1)"]
            ]
        pure $
          assertEqual "accepted" (Right (framed whole)) accepted
            `andThen` assertEqual "refused" (replicate 7 (Left invalidPlan)) refused,
      runTest "the macOS helper's records parse, and each failed step reads in upstream's words" $
        let plan = Sandbox.DarwinPlan "/b" "" [] []
            message stage number = Sandbox.darwinSetupFailureMessage plan "/bin/sh" (Sandbox.StatusRecord stage number Nothing)
            errno (Errno number) = fromIntegral number
            outcome = Sandbox.helperOutcome Sandbox.darwinStageName (Sandbox.darwinSetupFailureMessage plan "/bin/sh")
         in foldr andThen Pass [assertEqual (TE.decodeLatin1 (Sandbox.darwinStageName stage)) (Right (Just (Sandbox.StatusRecord stage 13 Nothing))) (Sandbox.parseStatus Sandbox.darwinStageName (Sandbox.darwinStageName stage <> " 13 -1\n")) | stage <- [minBound .. maxBound]]
              `andThen` assertEqual "sandbox" "while setting up the build environment: failed to configure sandbox" (message Sandbox.DarwinStageSandbox 0)
              `andThen` assertEqual "chdir" "while setting up the build environment: changing into \"/b\": No such file or directory" (message Sandbox.DarwinStageChdir 2)
              `andThen` assertEqual "fork" "while setting up the build environment: unable to fork: Resource temporarily unavailable" (message Sandbox.DarwinStageFork (errno eAGAIN))
              `andThen` assertEqual "exec" "while setting up the build environment: executing '/bin/sh': Operation not permitted" (message Sandbox.DarwinStageExec 1)
              `andThen` assertEqual "the build log, unwatched" "the sandbox helper cannot watch the build log, which must be pollable (a pipe or a regular file, not /dev/null): Bad file descriptor" (message Sandbox.DarwinStageLogWatch 9)
              `andThen` assertEqual "the report's pipe" "while setting up the build environment: creating pipe: Too many open files" (message Sandbox.DarwinStageReport (errno eMFILE))
              `andThen` assertEqual "the report, unread" "the sandbox helper cannot read the builder's report of its setup: Input/output error" (message Sandbox.DarwinStageRelay (errno eIO))
              `andThen` assertEqual "a Linux stage" (Sandbox.SetupIncomplete "the sandbox helper wrote a status that cannot be read: 'hostname 1 -1\n'") (outcome (Right ()) (ExitFailure 1) "hostname 1 -1\n")
              `andThen` assertEqual "builder" (Sandbox.BuilderRan (ExitFailure 3)) (outcome (Right ()) (ExitFailure 3) ""),
      runTest "a Windows plan renders as the helper reads it: the backend, the build directory, the token, the slot and the memory cap, then entries" $
        case Access.sidFromText "S-1-5-21-1-2-3-1001" of
          Nothing -> Fail "the slot's SID does not read as one"
          Just slot ->
            let plan = Sandbox.WindowsPlan "C:\\b" 404 slot 268435456 ["PATH=C:\\Windows"]
                body =
                  BS.concat
                    [ "7:backend\NUL7:windows\NUL9:build-dir\NUL4:C:\\b\NUL",
                      "5:token\NUL3:404\NUL8:slot-sid\NUL19:S-1-5-21-1-2-3-1001\NUL",
                      "10:memory-cap\NUL9:268435456\NUL",
                      "3:env\NUL15:PATH=C:\\Windows\NUL"
                    ]
             in assertEqual "bytes" ("nova-nix-sandbox-plan 4\n" <> BS8.pack (show (BS.length body)) <> "\n" <> body) (Sandbox.renderWindowsPlan plan),
      runTestM "the helper's parser reads a rendered Windows plan back into exactly its fields, and refuses one it cannot use" $ do
        result <-
          QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} $
            QC.checkCoverage $
              QC.forAll genWindowsPlan $ \plan ->
                let usable = windowsPlanUsable plan
                 in QC.cover 30 usable "the helper can use it" $
                      QC.cover 5 (not usable) "the helper cannot use it" $
                        QC.ioProperty $ do
                          let tokens = BS.drop 1 (BS8.dropWhile (/= '\n') (BS.drop (BS.length Sandbox.planVersionLine) (Sandbox.renderWindowsPlan plan)))
                          echoed <- helperPlanEcho tokens
                          pure (echoed QC.=== if usable then Right tokens else Left invalidPlan)
        pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result))),
      runTestM "the helper's parser takes a Windows plan's keys once and in order, then entries, and nothing else" $ do
        let framed = BS.concat . map planToken
            singletons = ["backend", "windows", "build-dir", "C:\\b", "token", "404", "slot-sid", "S-1-5-18", "memory-cap", "1"]
            whole = singletons ++ ["env", "a=b"]
            withValue key value = go singletons
              where
                go (name : old : rest)
                  | name == key = name : value : rest
                  | otherwise = name : old : go rest
                go other = other
            refusals =
              [ take 8 singletons,
                singletons ++ ["bind", "/t", "/s", "r"],
                singletons ++ ["param", "_A", "1"],
                singletons ++ ["env", "a=b", "memory-cap", "1"],
                ["backend", "windows", "token", "404", "build-dir", "C:\\b", "slot-sid", "S-1-5-18", "memory-cap", "1"],
                singletons ++ ["desktop", "s\\d"],
                withValue "build-dir" "b",
                withValue "build-dir" "/b",
                withValue "token" "0",
                withValue "token" "2147483648",
                withValue "slot-sid" "SY",
                withValue "memory-cap" "0"
              ]
            acceptances = [whole, withValue "build-dir" "\\\\server\\share", withValue "build-dir" "c:/b"]
        accepted <- traverse (helperPlanEcho . framed) acceptances
        refused <- traverse (helperPlanEcho . framed) refusals
        pure $
          assertEqual "accepted" (map (Right . framed) acceptances) accepted
            `andThen` assertEqual "refused" (map (const (Left invalidPlan)) refusals) refused,
      runTest "the Windows helper's records parse, each stage under a name of its own, and the probe's check of the privileges is one of its checks" $
        let names = map Sandbox.windowsStageName [minBound .. maxBound]
         in foldr andThen Pass [assertEqual (TE.decodeLatin1 (Sandbox.windowsStageName stage)) (Right (Just (Sandbox.StatusRecord stage 1300 Nothing))) (Sandbox.parseStatus Sandbox.windowsStageName (Sandbox.windowsStageName stage <> " 1300 -1\n")) | stage <- [minBound .. maxBound]]
              `andThen` assertEqual "distinct names" (length names) (Set.size (Set.fromList names))
              `andThen` assertEqual "the probe's" (Right (Just (Sandbox.StatusRecord Sandbox.ProbePrivileges 1300 Nothing))) (Sandbox.parseStatus Sandbox.probeStageName "privileges 1300 -1\n")
              `andThen` assertEqual "a Linux stage" (Left "the sandbox helper wrote a status that cannot be read: 'pivot 1 -1\n'") (Sandbox.parseStatus Sandbox.windowsStageName "pivot 1 -1\n"),
      runTest "on macOS a sandbox that cannot be made fails the build in upstream's words, whatever sandbox-fallback says, which Linux heeds" $
        let refused = Sandbox.ProbeFailed (Sandbox.StatusRecord Sandbox.ProbeSandbox 0 Nothing)
            darwin = Just Sandbox.DarwinBackend
         in assertEqual "refusal" "while setting up the build environment: failed to configure sandbox" (Sandbox.unavailableMessage darwin refused)
              `andThen` assertEqual "unreadable" "while setting up the build environment: failed to configure sandbox" (Sandbox.unavailableMessage darwin (Sandbox.ProbeUnreadable "x"))
              `andThen` assertEqual "no helper" "the sandbox helper 'nova-nix-sandbox' was not found beside nova-nix; use '--no-sandbox' to disable sandboxing" (Sandbox.unavailableMessage darwin Sandbox.HelperNotFound)
              `andThen` assertEqual "record" (Right (Just (Sandbox.StatusRecord Sandbox.ProbeSandbox 0 Nothing))) (Sandbox.parseStatus Sandbox.probeStageName "sandbox 0 -1\n")
              `andThen` assertEqual "macOS" [Sandbox.RefuseUnsandboxed, Sandbox.RefuseUnsandboxed] (map (Sandbox.backendFallback darwin) [Sandbox.FallBackUnsandboxed, Sandbox.RefuseUnsandboxed])
              `andThen` assertEqual "Linux" [Sandbox.FallBackUnsandboxed, Sandbox.RefuseUnsandboxed] (map (Sandbox.backendFallback (Just Sandbox.LinuxBackend)) [Sandbox.FallBackUnsandboxed, Sandbox.RefuseUnsandboxed]),
      runTest "a macOS profile grants the build none of the user's own: no temporary directory but its own, no terminal, no shared memory, and its defaults no temporary directory" $
        let rendered = Sandbox.darwinProfile sampleDarwinProfile {Sandbox.dprPaths = Sandbox.Subpath "/dev" : Sandbox.dprPaths sampleDarwinProfile}
            renderedLines = BS8.lines rendered
            position needle = listToMaybe [index | (index, line) <- zip [0 :: Int ..] renderedLines, needle `BS.isInfixOf` line]
         in assertEqual "the temporary directory is the build's" [("_NIX_BUILD_TOP", "/b"), ("_GLOBAL_TMP_DIR", "/b")] (Sandbox.darwinParameters "/b" Sandbox.LocalNetworkingDenied)
              `andThen` assertEqual "no rule on TMPDIR's subpath" False ("(subpath TMPDIR)" `BS.isInfixOf` rendered)
              `andThen` assertEqual "no terminal allowed" False (any (`BS.isInfixOf` rendered) ["(allow file* (literal \"/dev/ptmx\")", "#\"^/dev/ttys[0-9]+\""])
              `andThen` assertEqual "terminals denied after /dev is allowed" (Just True) ((<) <$> position "(subpath \"/dev\")" <*> position "(deny file-read* file-write*")
              `andThen` assertEqual "POSIX semaphores alone" (True, False) ("(allow ipc-posix-sem)" `BS.isInfixOf` rendered, any (`BS.isInfixOf` rendered) ["ipc-posix*", "ipc-sysv", "ipc-posix-shm"])
              `andThen` assertEqual "no temporary directory by default" [] (filter (`Set.member` Sandbox.darwinSandboxPaths) ["/private/tmp", "/private/var/tmp", "/tmp"])
              `andThen` assertEqual "the console denied" True ("\t(literal \"/dev/console\"))" `BS.isInfixOf` rendered),
      runTest "a macOS profile ends with the build's mark, after a derivation's own text, on a line of its own" $
        let rendered = Sandbox.darwinProfile sampleDarwinProfile {Sandbox.dprExtra = "; a comment with no newline"}
            mark = ["(allow mach-lookup (global-name (param \"_NOVA_NIX_BUILD_TOKEN\")))", "(deny mach-lookup (global-name (param \"_NOVA_NIX_BUILD_DECOY\")))"]
            renderedLines = BS8.lines rendered
         in assertEqual "the last lines" mark (drop (length renderedLines - length mark) renderedLines)
              `andThen` assertEqual "the derivation's text, ended" True ("\n; a comment with no newline\n" `BS.isInfixOf` rendered)
    ]
  where
    sandboxDrvPath = StorePath "dddddddddddddddddddddddddddddddd" "x.drv"
    sandboxOutPath = StorePath "eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee" "x"
    storeIn hash = "/nix/store/" <> BS.concat (replicate 8 hash) <> "-p"
    skeletonPath entry = case entry of
      Sandbox.SkeletonDir path _ -> path
      Sandbox.SkeletonFile path _ -> path
      Sandbox.SkeletonLink path _ -> path
    genStatusBytes =
      BS.concat
        <$> QC.listOf
          ( QC.oneof
              [ QC.elements (map Sandbox.setupStageName [minBound .. maxBound]),
                QC.elements [" ", "\n", "13", "-1", "0", "x", "-", "99999999999999999999"],
                BS.pack <$> QC.listOf QC.arbitrary
              ]
          )

-- | The device nodes a plan binds when nothing binds @\/dev@ whole.
sandboxDeviceNodes :: [BS.ByteString]
sandboxDeviceNodes = ["/dev/full", "/dev/null", "/dev/random", "/dev/tty", "/dev/urandom", "/dev/zero"]

-- | A macOS build's path inputs with nothing in them but its store and
-- build directories, the build directory behind a link as macOS's
-- temporary directories are.
darwinPathInputs :: Sandbox.DarwinPathInputs
darwinPathInputs =
  Sandbox.DarwinPathInputs
    { Sandbox.dpiDrvPath = StorePath "dddddddddddddddddddddddddddddddd" "x.drv",
      Sandbox.dpiStoreDir = "/nix/store",
      Sandbox.dpiRealStoreDir = "/nix/store",
      Sandbox.dpiBuildDir = "/state/builds/h",
      Sandbox.dpiRealBuildDir = "/private/state/builds/h",
      Sandbox.dpiSandboxPaths = [],
      Sandbox.dpiSandboxClosure = [],
      Sandbox.dpiImpureHostDeps = Set.empty,
      Sandbox.dpiAllowedImpureHostDeps = Set.empty,
      Sandbox.dpiInputs = []
    }

-- | The macOS profile test/data/sandbox-darwin.sb holds: a fixed-output
-- build's, with a path of each kind, store paths in two groups, one that
-- needs escaping, and a derivation's own text.
sampleDarwinProfile :: Sandbox.DarwinProfile
sampleDarwinProfile =
  Sandbox.DarwinProfile
    { Sandbox.dprViolationLog = Sandbox.ViolationsUnlogged,
      Sandbox.dprNetwork = Sandbox.HostNetwork,
      Sandbox.dprStoreDir = "/nix/store",
      Sandbox.dprOutputs = ["/nix/store/oooooooooooooooooooooooooooooooo-x"],
      Sandbox.dprPaths =
        [ Sandbox.Literal "/bin/sh",
          Sandbox.Subpath "/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-dep",
          Sandbox.Literal "/nix/store/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb-file",
          Sandbox.Subpath "/private/state/builds/h",
          Sandbox.Literal "/srv/a \"quoted\" \\ name",
          Sandbox.Subpath "/usr/lib"
        ],
      Sandbox.dprAncestry = ["/", "/bin", "/nix", "/nix/store", "/private", "/private/state", "/private/state/builds", "/srv", "/usr"],
      Sandbox.dprExtra = "(allow file-read* (literal \"/etc/extra\"))\n"
    }

-- | The path a profile filter names.
profileFilterPath :: Sandbox.ProfileFilter -> BS.ByteString
profileFilterPath pathFilter = case pathFilter of
  Sandbox.Subpath path -> path
  Sandbox.Literal path -> path

-- | A profile's line for one filter, its path escaped as the profile's
-- reader takes it.
profileFilterLine :: Sandbox.ProfileFilter -> BS.ByteString
profileFilterLine pathFilter = case pathFilter of
  Sandbox.Subpath path -> "\t(subpath \"" <> escaped path <> "\")"
  Sandbox.Literal path -> "\t(literal \"" <> escaped path <> "\")"
  where
    escaped = BS.concatMap (\byte -> if byte == 0x5c || byte == 0x22 then BS.pack [0x5c, byte] else BS.singleton byte)

-- | The lines of each form in a profile's lines that opens with the given
-- line, up to its closing one: a line of its own for a plain form, or the
-- end of a group's guard.
profileForms :: BS.ByteString -> [BS.ByteString] -> [[BS.ByteString]]
profileForms header profileLines = case break (== header) profileLines of
  (_, []) -> []
  (_, _ : rest) -> let (form, after) = break (`elem` [")", "\t\t)))"]) rest in form : profileForms header after

-- | A macOS profile's inputs drawn to stress its forms: a store directory
-- that needs escaping now and then, store paths whose names start with a
-- few characters, a quote among them, other paths holding quotes and
-- backslashes, and now and then no output, path or ancestor at all.
genDarwinProfile :: QC.Gen Sandbox.DarwinProfile
genDarwinProfile = do
  storeDir <- QC.elements ["/nix/store", "/s \"q\"/store"]
  let genParts = QC.choose (1, 6) >>= \count -> QC.vectorOf count (QC.elements ["a", "b/", "\"", "\\", " ", "\xc3\xa0", "store/"])
      genOther = (\parts -> "/" <> BS.concat parts) <$> genParts
      genStorePath = (\first parts -> storeDir <> "/" <> first <> BS.concat parts) <$> QC.elements ["0", "1", "a", "z", "\""] <*> genParts
      genPath = QC.oneof [genOther, genStorePath]
      genFilter = QC.oneof [Sandbox.Subpath <$> genPath, Sandbox.Literal <$> genPath]
  Sandbox.DarwinProfile
    <$> QC.elements [Sandbox.ViolationsUnlogged, Sandbox.ViolationsLogged]
    <*> QC.elements [Sandbox.PrivateNetwork, Sandbox.HostNetwork]
    <*> pure storeDir
    <*> QC.listOf genPath
    <*> (QC.choose (0, 30) >>= \count -> QC.vectorOf count genFilter)
    <*> QC.listOf genPath
    <*> QC.elements ["", "(allow file-read* (literal \"/x\"))\n"]

-- | A macOS plan with any bytes in every field, most often bytes the
-- helper can use.
genDarwinPlan :: QC.Gen Sandbox.DarwinPlan
genDarwinPlan =
  Sandbox.DarwinPlan
    <$> genPath
    <*> genText
    <*> few ((,) <$> genName <*> genText)
    <*> few genText
  where
    few gen = QC.choose (0, 3) >>= \count -> QC.vectorOf count gen
    usable = BS.filter (/= 0) <$> genPlanBytes
    genPath = QC.frequency [(12, ("/" <>) <$> usable), (1, genPlanBytes)]
    genName = QC.frequency [(12, ("_" <>) <$> usable), (1, genPlanBytes)]
    genText = QC.frequency [(12, usable), (1, genPlanBytes)]

-- | Whether the helper can use a macOS plan: no field holds a NUL, its
-- build directory is absolute, and every parameter has a name.
darwinPlanUsable :: Sandbox.DarwinPlan -> Bool
darwinPlanUsable plan =
  all (BS.notElem 0) (Sandbox.darwinPlanBuildDir plan : Sandbox.darwinPlanProfile plan : concat [[name, value] | (name, value) <- Sandbox.darwinPlanParameters plan] ++ Sandbox.darwinPlanEnvironment plan)
    && "/" `BS.isPrefixOf` Sandbox.darwinPlanBuildDir plan
    && not (any (BS.null . fst) (Sandbox.darwinPlanParameters plan))

-- | A Windows plan with any bytes in its byte fields, most often bytes the
-- helper can use, and now and then a token or a memory cap it refuses.
genWindowsPlan :: QC.Gen Sandbox.WindowsPlan
genWindowsPlan =
  Sandbox.WindowsPlan
    <$> QC.frequency [(12, ("C:\\" <>) <$> usable), (1, genPlanBytes)]
    <*> QC.frequency [(12, QC.choose (1, handleValueMax)), (1, QC.elements [0, handleValueMax + 1, maxBound])]
    <*> QC.suchThatMap genSidText Access.sidFromText
    <*> QC.frequency [(12, QC.choose (1, maxBound)), (1, pure 0)]
    <*> few (QC.frequency [(12, usable), (1, genPlanBytes)])
  where
    few gen = QC.choose (0, 3) >>= \count -> QC.vectorOf count gen
    usable = BS.filter (/= 0) <$> genPlanBytes
    genSidText = do
      subauthorities <- QC.choose (1, 15) >>= \count -> QC.vectorOf count (QC.choose (0, 4294967295 :: Integer))
      authority <- QC.elements [1, 5 :: Integer]
      pure (T.intercalate "-" ("S-1" : map (T.pack . show) (authority : subauthorities)))

-- | The largest value of a handle the helper takes, the largest an int
-- holds.
handleValueMax :: Word32
handleValueMax = 2147483647

-- | Whether the helper can use a Windows plan: no field holds a NUL, its
-- build directory is absolute, its token a value a handle has, and its
-- memory cap above 0.
windowsPlanUsable :: Sandbox.WindowsPlan -> Bool
windowsPlanUsable plan =
  all (BS.notElem 0) (Sandbox.windowsPlanBuildDir plan : Sandbox.windowsPlanEnvironment plan)
    && windowsAbsolute (Sandbox.windowsPlanBuildDir plan)
    && Sandbox.windowsPlanToken plan >= 1
    && Sandbox.windowsPlanToken plan <= handleValueMax
    && Sandbox.windowsPlanMemoryCap plan > 0
  where
    windowsAbsolute path = case BS8.unpack (BS.take 3 path) of
      [letter, ':', separator] -> isAsciiLetter letter && separator `elem` ['\\', '/']
      _ -> "\\\\" `BS.isPrefixOf` path
    isAsciiLetter letter = letter `elem` (['A' .. 'Z'] ++ ['a' .. 'z'])

-- | A plan's inputs with nothing in them but a root and a build directory.
sandboxPlanInputs :: Sandbox.PlanInputs
sandboxPlanInputs =
  Sandbox.PlanInputs
    { Sandbox.piRoot = "/chroot/root",
      Sandbox.piStoreDir = "/nix/store",
      Sandbox.piBuildDirSeen = "/build",
      Sandbox.piBuildDirOnDisk = "/state/builds/h",
      Sandbox.piInputs = [],
      Sandbox.piSandboxPaths = [],
      Sandbox.piSandboxClosure = [],
      Sandbox.piOutputs = [],
      Sandbox.piEnvironment = [],
      Sandbox.piNetwork = Sandbox.PrivateNetwork,
      Sandbox.piCaFile = Nothing,
      Sandbox.piDevShmSize = "50%",
      Sandbox.piDropGroups = Sandbox.TolerateKeptGroups
    }

-- | Plan inputs drawn from a small pool of store paths, so inputs,
-- outputs, closures and sandbox-paths targets collide.
genSandboxPlanInputs :: QC.Gen Sandbox.PlanInputs
genSandboxPlanInputs = do
  let pool = ["/nix/store/" <> BS.concat (replicate 32 (BS.singleton letter)) <> "-p" | letter <- [97 .. 104]]
      targets = pool ++ ["/dev", "/dev/null", "/bin/sh", "/etc/hosts", "/build"]
  inputs <- QC.sublistOf pool
  outputs <- QC.sublistOf pool
  closure <- QC.sublistOf pool
  paths <- QC.listOf (Sandbox.SandboxPath <$> QC.elements targets <*> QC.elements ("/host" : pool) <*> QC.elements [Sandbox.RequiredPath, Sandbox.OptionalPath])
  network <- QC.elements [Sandbox.PrivateNetwork, Sandbox.HostNetwork]
  caFile <- QC.elements [Nothing, Just "/ca.pem", Just ""]
  pure sandboxPlanInputs {Sandbox.piInputs = inputs, Sandbox.piOutputs = outputs, Sandbox.piSandboxClosure = closure, Sandbox.piSandboxPaths = paths, Sandbox.piNetwork = network, Sandbox.piCaFile = caFile}

-- | One token of a plan as the helper reads it: its length in decimal, a
-- colon, its bytes and a NUL.
planToken :: BS.ByteString -> BS.ByteString
planToken bytes = BS8.pack (show (BS.length bytes)) <> ":" <> bytes <> "\NUL"

-- | A rendered plan's tokens, past its version and length lines.
renderedPlanTokens :: Sandbox.SandboxPlan -> BS.ByteString
renderedPlanTokens plan = BS.drop 1 (BS8.dropWhile (/= '\n') (BS.drop (BS.length Sandbox.planVersionLine) (Sandbox.renderPlan plan)))

-- | A plan read back from what 'Sandbox.renderPlan' writes, by the
-- format's own rules, or nothing when the bytes are not one.
readRenderedPlan :: BS.ByteString -> Maybe Sandbox.SandboxPlan
readRenderedPlan bytes = do
  rest <- BS.stripPrefix Sandbox.planVersionLine bytes
  let (lengthText, afterLength) = BS8.break (== '\n') rest
  body <- BS.stripPrefix "\n" afterLength
  declared <- readMaybe (BS8.unpack lengthText)
  guard (declared == BS.length body)
  tokens <- splitTokens body
  case tokens of
    "backend" : "linux" : "root" : root : "build-dir" : buildDir : "dev-shm-size" : shm : "sandbox-uid" : uid : "sandbox-gid" : gid : "network" : network : "drop-groups" : groups : sections -> do
      guard (uid == BS8.pack (show Sandbox.sandboxUid) && gid == BS8.pack (show Sandbox.sandboxGid))
      networkRead <- lookup network [("private", Sandbox.PrivateNetwork), ("host", Sandbox.HostNetwork)]
      groupsRead <- lookup groups [("require", Sandbox.RequireDropGroups), ("tolerate", Sandbox.TolerateKeptGroups)]
      (binds, entries) <- readSections sections
      pure (Sandbox.SandboxPlan root buildDir shm networkRead groupsRead binds entries)
    _ -> Nothing
  where
    splitTokens remaining
      | BS.null remaining = Just []
      | otherwise = do
          let (digits, afterDigits) = BS8.span isDigit remaining
          size <- readMaybe (BS8.unpack digits)
          afterColon <- BS.stripPrefix ":" afterDigits
          let (token, afterToken) = BS.splitAt size afterColon
          guard (BS.length token == size)
          next <- BS.stripPrefix "\NUL" afterToken
          (token :) <$> splitTokens next
    readSections sections = case sections of
      "bind" : target : source : flags : rest -> do
        bind <- readBind target source flags
        (binds, entries) <- readSections rest
        Just (bind : binds, entries)
      _ -> (,) [] <$> readEntries sections
    readEntries sections = case sections of
      [] -> Just []
      "env" : entry : rest -> (entry :) <$> readEntries rest
      _ -> Nothing
    readBind target source flags = do
      let letters = if flags == "-" then "" else BS8.unpack flags
          access = if 'r' `elem` letters then Sandbox.ReadOnlyBind else Sandbox.ReadWriteBind
          presence = if 'o' `elem` letters then Sandbox.OptionalPath else Sandbox.RequiredPath
          resolution = if 'c' `elem` letters then Sandbox.SourceResolved else Sandbox.SourceAsGiven
      guard (all (`elem` ("roc" :: String)) letters && flags /= "")
      Just (Sandbox.Bind target source access presence resolution)

-- | Bytes for a plan's fields, drawn to collide with the format itself:
-- NULs, colons, digits, keys, paths and anything at all.
genPlanBytes :: QC.Gen BS.ByteString
genPlanBytes =
  BS.concat
    <$> QC.listOf
      ( QC.oneof
          [ QC.elements ["/", "/a", "\NUL", ":", "1:", "0", "12", "bind", "env", "root", "=", "-", " ", "\n"],
            BS.pack <$> QC.listOf QC.arbitrary
          ]
      )

-- | A plan with any bytes in every field: most often bytes the helper
-- can use, an absolute path or text holding no NUL, else anything, with
-- few enough binds and entries that a plan it can use, and one it cannot
-- for a NUL alone, are both common ('withoutNuls').
genPlanAnyBytes :: QC.Gen Sandbox.SandboxPlan
genPlanAnyBytes =
  Sandbox.SandboxPlan
    <$> genPath
    <*> genPath
    <*> genText
    <*> QC.elements [Sandbox.PrivateNetwork, Sandbox.HostNetwork]
    <*> QC.elements [Sandbox.RequireDropGroups, Sandbox.TolerateKeptGroups]
    <*> few
      ( Sandbox.Bind
          <$> genPath
          <*> genPath
          <*> QC.elements [Sandbox.ReadOnlyBind, Sandbox.ReadWriteBind]
          <*> QC.elements [Sandbox.RequiredPath, Sandbox.OptionalPath]
          <*> QC.elements [Sandbox.SourceAsGiven, Sandbox.SourceResolved]
      )
    <*> few genText
  where
    few gen = QC.choose (0, 3) >>= \count -> QC.vectorOf count gen
    usable = BS.filter (/= 0) <$> genPlanBytes
    genPath = QC.frequency [(12, ("/" <>) <$> usable), (1, genPlanBytes)]
    genText = QC.frequency [(12, usable), (1, genPlanBytes)]

-- | A plan with every NUL taken out of its fields.
withoutNuls :: Sandbox.SandboxPlan -> Sandbox.SandboxPlan
withoutNuls plan =
  plan
    { Sandbox.planRoot = strip (Sandbox.planRoot plan),
      Sandbox.planBuildDir = strip (Sandbox.planBuildDir plan),
      Sandbox.planDevShmSize = strip (Sandbox.planDevShmSize plan),
      Sandbox.planBinds = [bind {Sandbox.bindTarget = strip (Sandbox.bindTarget bind), Sandbox.bindSource = strip (Sandbox.bindSource bind)} | bind <- Sandbox.planBinds plan],
      Sandbox.planEnvironment = map strip (Sandbox.planEnvironment plan)
    }
  where
    strip = BS.filter (/= 0)

-- | Whether the helper can use a plan: no field holds a NUL, since it
-- takes each as a C string, and its root, build directory and every
-- bind's paths are absolute.
helperCanUse :: Sandbox.SandboxPlan -> Bool
helperCanUse plan =
  all (BS.notElem 0) (fields ++ Sandbox.planEnvironment plan ++ [Sandbox.planDevShmSize plan])
    && all ("/" `BS.isPrefixOf`) fields
  where
    fields = [Sandbox.planRoot plan, Sandbox.planBuildDir plan] ++ concat [[Sandbox.bindTarget bind, Sandbox.bindSource bind] | bind <- Sandbox.planBinds plan]

-- | The errno the helper's parser refuses a plan with.
invalidPlan :: CInt
invalidPlan = case eINVAL of Errno number -> number

foreign import ccall unsafe "nova_test_plan_echo"
  c_nova_test_plan_echo :: Ptr CChar -> CSize -> Ptr (Ptr CChar) -> Ptr CSize -> IO CInt

foreign import ccall unsafe "nova_test_mountinfo_within"
  c_nova_test_mountinfo_within :: Ptr CChar -> CSize -> Ptr CChar -> CSize -> Ptr (Ptr CChar) -> Ptr CSize -> IO CInt

foreign import ccall unsafe "nova_test_plant_possessed_key"
  c_nova_test_plant_possessed_key :: Ptr CChar -> IO CLong

-- | Take the bytes a test entry point wrote into a buffer of its own,
-- freeing the buffer.
takeCBuffer :: Ptr (Ptr CChar) -> Ptr CSize -> IO BS.ByteString
takeCBuffer outPtr outLength = do
  out <- peek outPtr
  size <- peek outLength
  bytes <- if size == 0 then pure BS.empty else BS.packCStringLen (out, fromIntegral size)
  free out
  pure bytes

-- | The helper's plan parser (sandbox/plan.c) run on a plan's tokens: the
-- plan it read, written back as tokens, or the errno it refused them with.
helperPlanEcho :: BS.ByteString -> IO (Either CInt BS.ByteString)
helperPlanEcho tokens =
  BS.useAsCStringLen tokens $ \(bytes, size) ->
    alloca $ \outPtr -> alloca $ \outLength -> do
      code <- c_nova_test_plan_echo bytes (fromIntegral size) outPtr outLength
      if code /= 0 then pure (Left code) else Right <$> takeCBuffer outPtr outLength

-- | The mount points at or beneath a directory that the helper's walk
-- (sandbox/mountinfo.c) finds in a mountinfo text.
helperMountsWithin :: BS.ByteString -> BS.ByteString -> IO [BS.ByteString]
helperMountsWithin mountinfo dir =
  BS.useAsCStringLen mountinfo $ \(text, textSize) ->
    BS.useAsCStringLen dir $ \(dirBytes, dirSize) ->
      alloca $ \outPtr -> alloca $ \outLength -> do
        code <- c_nova_test_mountinfo_within text (fromIntegral textSize) dirBytes (fromIntegral dirSize) outPtr outLength
        if code /= 0 then throwIO (userError ("the mountinfo walk failed: " <> show code)) else items <$> takeCBuffer outPtr outLength
  where
    items bytes
      | BS.null bytes = []
      | otherwise =
          let (digits, afterDigits) = BS8.span isDigit bytes
              size = fromMaybe 0 (readMaybe (BS8.unpack digits))
              (item, rest) = BS.splitAt size (BS.drop 1 afterDigits)
           in item : items rest

-- | A mountinfo text and a directory to look beneath: lines of fields
-- drawn from paths, escapes, bytes other splitters take for spaces, and
-- anything at all, and a directory that is often one of the paths.
genMountinfo :: QC.Gen (BS.ByteString, BS.ByteString)
genMountinfo = do
  let pieces =
        [ "/a",
          "/a/b",
          "/ab",
          "\\040",
          "\\134",
          "\\0",
          "\\777",
          "\\",
          "\xc3\xa0",
          "\xa0",
          "\t",
          "\NUL",
          "-",
          "rw"
        ]
      genField = BS.concat <$> QC.listOf (QC.oneof [QC.elements pieces, BS.pack <$> QC.listOf QC.arbitrary])
      genLine = do
        fields <- QC.listOf genField
        separator <- QC.elements [" ", "  "]
        pure (BS.intercalate separator fields)
  mountinfo <- BS.intercalate "\n" <$> QC.listOf genLine
  dir <- QC.oneof [QC.elements ["", "/", "/a", "/a/b", "/a b", "/a\xc3\xa0"], genField]
  pure (mountinfo, dir)

-- | The sandbox's IO that runs anywhere POSIX ("Nix.Builder.Sandbox.Spawn"):
-- the chroot and its removal, and the helper's protocol, run against
-- helpers written here that speak it, since the real one runs only on
-- Linux.
testSandboxSpawnIO :: IO [Bool]
testSandboxSpawnIO = do
  putStrLn "builder/sandbox-spawn"
  if SI.os == "mingw32"
    then do
      putStrLn "  SKIP  POSIX only"
      pure []
    else do
      tmpBase <- getTemporaryDirectory
      let root = tmpBase </> "nova-nix-test-sandbox-spawn"
          buildDir = root </> "builds" </> "h"
          storeDirPath = root </> "store"
          helperAt name = root </> name
          writeHelper name body = do
            BS.writeFile (helperAt name) ("#!/bin/sh\n" <> body)
            Dir.setPermissions (helperAt name) (Dir.setOwnerExecutable True (Dir.setOwnerReadable True Dir.emptyPermissions))
          -- A fake helper reads the plan as the real one does, by the
          -- length its second line gives, and leaves the pipe open.
          readPlanInto target = "IFS= read -r version\nIFS= read -r length\nhead -c \"$length\" > " <> target <> "\n"
          runFake name plan command = do
            let logPath = root </> (name <> ".log")
            run <- withBinaryFile logPath WriteMode $ \logHandle ->
              Spawn.runHelper (handleLogger DropEscapes logHandle) (helperAt name) plan root command
            logged <- BS.readFile logPath
            pure (run, logged)
          framedPlan bytes = Sandbox.planVersionLine <> BS8.pack (show (BS.length bytes)) <> "\n" <> bytes
      bracket_
        (forceRemoveIfExists root >> createDirectoryIfMissing True (root </> "builds"))
        (forceRemoveIfExists root)
        ( do
            storeBytes <- Spawn.filePathBytes storeDirPath
            sequence
              [ runTestM "a chroot holds its skeleton while the build runs, and is gone after" $ do
                  createDirectoryIfMissing True buildDir
                  inside <-
                    Spawn.withChroot testLogger buildDir (Sandbox.chrootSkeleton storeBytes "/build" Sandbox.PrivateNetwork []) $ \chrootRoot -> do
                      passwd <- BS.readFile (chrootRoot </> "etc" </> "passwd")
                      tmpMode <- fmap nsPermissions <$> nodeStatus (chrootRoot </> "tmp")
                      storeMode <- fmap nsPermissions <$> nodeStatus (chrootRoot <> storeDirPath)
                      fdLink <- Dir.getSymbolicLinkTarget (chrootRoot </> "dev" </> "fd")
                      pure (passwd, tmpMode, storeMode, fdLink)
                  left <- Dir.doesPathExist (Spawn.chrootDirFor buildDir)
                  pure $
                    assertRight "chroot" inside $ \(passwd, tmpMode, storeMode, fdLink) ->
                      assertEqual "passwd" (Sandbox.passwdFile "/build") passwd
                        `andThen` assertEqual "/tmp" (Just 0o1777) tmpMode
                        `andThen` assertEqual "store" (Just 0o1775) storeMode
                        `andThen` assertEqual "/dev/fd" "/proc/self/fd" fdLink
                        `andThen` assertEqual "left after" False left,
                runTestM "a chroot an earlier build left, read-only inside and linking outside, is removed before the next one is made, and what it links to stays" $ do
                  let leftover = Spawn.chrootDirFor buildDir </> "root" </> "etc"
                      outside = root </> "outside"
                  createDirectoryIfMissing True leftover
                  createDirectoryIfMissing True outside
                  BS.writeFile (outside </> "kept") "host"
                  Dir.createDirectoryLink outside (Spawn.chrootDirFor buildDir </> "root" </> "linked")
                  BS.writeFile (leftover </> "stale") "x"
                  Dir.getPermissions leftover >>= Dir.setPermissions leftover . Dir.setOwnerWritable False
                  made <- Spawn.withChroot testLogger buildDir (Sandbox.chrootSkeleton storeBytes "/build" Sandbox.HostNetwork []) (\chrootRoot -> Dir.doesPathExist (chrootRoot </> "etc" </> "stale"))
                  kept <- BS.readFile (outside </> "kept")
                  pure (assertEqual "stale file" (Right False) made `andThen` assertEqual "what the link named" "host" kept),
                -- A real chroot's /dev/fd names /proc/self/fd; a read-only
                -- directory of the test's own stands in for it, so a
                -- removal that followed the link would show in it rather
                -- than act beyond it.
                runTestM "a chroot an earlier build left, read-only and linking to a read-only directory outside it, is removed without anything done through the link" $ do
                  let leftover = Spawn.chrootDirFor buildDir </> "root" </> "dev"
                      sentinel = root </> "dev-fd-sentinel"
                  createDirectoryIfMissing True leftover
                  createDirectoryIfMissing True sentinel
                  BS.writeFile (sentinel </> "kept") "sentinel"
                  Proc.callProcess "chmod" ["0555", sentinel]
                  Dir.createDirectoryLink sentinel (leftover </> "fd")
                  Dir.getPermissions leftover >>= Dir.setPermissions leftover . Dir.setOwnerWritable False
                  made <- Spawn.withChroot testLogger buildDir (Sandbox.chrootSkeleton storeBytes "/build" Sandbox.PrivateNetwork []) (\chrootRoot -> Dir.getSymbolicLinkTarget (chrootRoot </> "dev" </> "fd"))
                  sentinelMode <- fmap nsPermissions <$> nodeStatus sentinel
                  kept <- BS.readFile (sentinel </> "kept")
                  pure $
                    assertEqual "the chroot made in its place" (Right "/proc/self/fd") made
                      `andThen` assertEqual "the sentinel's mode" (Just 0o555) sentinelMode
                      `andThen` assertEqual "the sentinel's file" "sentinel" kept,
                runTest "the mount points in a chroot are read from mountinfo, its escapes undone, its fields split at spaces alone, and no mount beside it is one" $
                  let mountinfo =
                        BS.intercalate
                          "\n"
                          [ "22 1 8:1 / / rw,relatime shared:1 - ext4 /dev/sda1 rw",
                            "40 22 0:35 / /tmp/c/root/x rw - tmpfs tmpfs rw",
                            "41 22 0:36 / /tmp/c\\040d/root rw - tmpfs tmpfs rw",
                            "42 22 0:37 / /tmp/cc rw - tmpfs tmpfs rw",
                            -- "voil\xc3\xa0", UTF-8 for a name ending in a-grave,
                            -- whose second byte other splitters take for a space.
                            "43 22 0:38 / /tmp/c/voil\xc3\xa0 rw - tmpfs tmpfs rw"
                          ]
                   in assertEqual "within /tmp/c" ["/tmp/c/root/x", "/tmp/c/voil\xc3\xa0"] (Spawn.mountPointsWithin mountinfo "/tmp/c")
                        `andThen` assertEqual "within /tmp/c d" ["/tmp/c d/root"] (Spawn.mountPointsWithin mountinfo "/tmp/c d")
                        `andThen` assertEqual "within /srv" [] (Spawn.mountPointsWithin mountinfo "/srv"),
                runTestM "a chroot whose build has ended is closed to other users: its way to the store private, every directory in the store shut to them, parent first, what the builder left open recorded, nothing outside the store touched, no link followed" $ do
                  let chroot = root </> "seal"
                      store = chroot </> "nix" </> "store"
                      out = store </> "out"
                      shut = store </> "shut"
                      elsewhere = root </> "seal-elsewhere"
                      beneath = root </> "seal-beneath"
                      closed = out </> "closed"
                      opened = [chroot, chroot </> "nix", store, out, out </> "sub", closed </> "inside", chroot </> "tmp", chroot </> "tmp" </> "junk", elsewhere, beneath]
                  traverse_ (createDirectoryIfMissing True) [out </> "sub", closed </> "inside", shut, chroot </> "tmp" </> "junk", elsewhere, beneath]
                  Dir.createDirectoryLink elsewhere (out </> "linked")
                  -- A link planted below an output, where a walk that
                  -- followed links would reach the directory it names.
                  Dir.createDirectoryLink beneath (out </> "sub" </> "planted")
                  traverse_ (\dir -> Proc.callProcess "chmod" ["0777", dir]) opened
                  Proc.callProcess "chmod" ["0000", closed]
                  sealed <- Spawn.sealChroot chroot "/nix/store"
                  modes <- traverse (fmap (fmap nsPermissions) . nodeStatus) (opened ++ [closed, shut])
                  pure $
                    assertEqual "what the builder left open" (Right (Set.fromList ["out"])) sealed
                      `andThen` assertEqual "modes" (map Just [0o700, 0o700, 0o700, 0o700, 0o700, 0o700, 0o777, 0o777, 0o777, 0o777, 0o700, 0o700]) modes,
                -- A file its owner may not remove stands in for a directory
                -- another user made, which no test can make; macOS lets an
                -- owner set one.
                runTestM "a chroot an earlier build left that cannot be removed is moved aside, and the next is made" $
                  if SI.os /= "darwin"
                    then pure Pass
                    else do
                      let stuck = Spawn.chrootDirFor buildDir </> "root" </> "stuck"
                          aside = Spawn.chrootDirFor buildDir <> ".leftover.1"
                      createDirectoryIfMissing True stuck
                      BS.writeFile (stuck </> "f") "x"
                      Proc.callProcess "chflags" ["uchg", stuck </> "f"]
                      -- The flag comes off whatever happens, so what is
                      -- left can be removed.
                      let release = for_ [Spawn.chrootDirFor buildDir, aside] $ \dir -> Proc.readProcessWithExitCode "chflags" ["-R", "nouchg", dir] ""
                      ( do
                          made <- Spawn.withChroot testLogger buildDir (Sandbox.chrootSkeleton storeBytes "/build" Sandbox.PrivateNetwork []) (\chrootRoot -> Dir.doesDirectoryExist (chrootRoot </> "etc"))
                          movedAside <- Dir.doesPathExist (aside </> "root" </> "stuck" </> "f")
                          pure (assertEqual "the chroot made" (Right True) made `andThen` assertEqual "moved aside" True movedAside)
                        )
                        `finally` (release *> Dir.removePathForcibly aside),
                runTestM "a chroot whose way to its store is not directories of its own is refused before anything is read there" $ do
                  let chroot = root </> "seal-linked"
                  createDirectoryIfMissing True (chroot </> "nix")
                  createDirectoryIfMissing True (root </> "seal-target" </> "store")
                  Dir.createDirectoryLink (root </> "seal-target" </> "store") (chroot </> "nix" </> "store")
                  linked <- Spawn.sealChroot chroot "/nix/store"
                  missing <- Spawn.sealChroot chroot "/nix/gone"
                  let replaced path = Left ("the build replaced '" <> T.pack path <> "' in its sandbox, which holds its outputs, with something other than a directory of its own")
                  pure $
                    assertEqual "link" (replaced (chroot </> "nix" </> "store")) linked
                      `andThen` assertEqual "missing" (replaced (chroot </> "nix" </> "gone")) missing,
                runTestM "the helper gets the plan on standard input and no environment, and a builder it starts logs and exits as itself" $ do
                  writeHelper "relay" ("[ \"$1\" = run ] || exit 64\nshift\n" <> readPlanInto "\"$(dirname \"$0\")/plan-copy\"" <> "env > \"$(dirname \"$0\")/environment\"\nexec 1>&2\nexec \"$@\"\n")
                  let sent = framedPlan "root\0/r\0"
                  setEnv "NOVA_NIX_TEST_AMBIENT" "inherited"
                  (run, logged) <- runFake "relay" sent ["/bin/sh", "-c", "echo built; exit 3"]
                  unsetEnv "NOVA_NIX_TEST_AMBIENT"
                  delivered <- BS.readFile (root </> "plan-copy")
                  environment <- BS.readFile (root </> "environment")
                  pure $
                    assertEqual "run" (Spawn.HelperRun (Right ()) (ExitFailure 3) "") run
                      `andThen` assertEqual "plan" "root\0/r\0" delivered
                      `andThen` assertEqual "log" "built\n" logged
                      `andThen` assertEqual "ambient in the helper" [] (filter ("NOVA_NIX_TEST_AMBIENT=" `BS.isPrefixOf`) (BS8.lines environment)),
                runTestM "a step the helper reports failing comes back as its record" $ do
                  writeHelper "failing" (readPlanInto "/dev/null" <> "printf 'bind 2 0\\n'\nexit 1\n")
                  (run, _) <- runFake "failing" (framedPlan "") ["/bin/sh"]
                  pure (assertEqual "run" (Spawn.HelperRun (Right ()) (ExitFailure 1) "bind 2 0\n") run),
                runTestM "a helper that ends before it reads the plan leaves the plan undelivered" $ do
                  writeHelper "early" "exit 1\n"
                  (run, _) <- runFake "early" (framedPlan (BS.replicate (4 * 1024 * 1024) 0x61)) ["/bin/sh"]
                  pure $ case run of
                    Spawn.HelperRun (Left _) (ExitFailure 1) "" -> Pass
                    other -> Fail ("expected an undelivered plan and exit 1, got " <> T.pack (show other)),
                runTestM "a run that is interrupted returns only once the helper has ended" $ do
                  let started = root </> "started"
                      stopped = root </> "stopped"
                  writeHelper "stopping" (TE.encodeUtf8 ("trap 'sleep 1; echo stopped > " <> T.pack stopped <> "; exit 0' TERM\n" <> "IFS= read -r version\nIFS= read -r length\nhead -c \"$length\" > /dev/null\n" <> "echo started > " <> T.pack started <> "\nwhile :; do sleep 0.1; done\n"))
                  withAsync (runFake "stopping" (framedPlan "") ["/bin/sh"]) $ \running -> do
                    up <- waitForFile started
                    cancel running
                    ended <- Dir.doesPathExist stopped
                    pure (assertEqual "started" True up `andThen` assertEqual "stopped before the run returned" True ended)
              ]
        )
  where
    -- Whether a file appears within the wait a helper's start takes.
    waitForFile path = go (processWaitMicros `div` filePollMicros)
      where
        go :: Int -> IO Bool
        go 0 = Dir.doesPathExist path
        go remaining = do
          present <- Dir.doesPathExist path
          if present then pure True else threadDelay filePollMicros >> go (remaining - 1)
        filePollMicros = 50000

-- | The variable naming the sandbox helper this run's Linux and macOS
-- sandbox tests use: on Linux one AppArmor lets make namespaces, as CI's
-- Linux build loads a profile for.
sandboxHelperVar :: String
sandboxHelperVar = "NOVA_NIX_TEST_SANDBOX_HELPER"

-- | The variable naming a static busybox, which the Linux sandbox tests
-- bind as the builder's /bin/sh and /bin/busybox.
sandboxBusyboxVar :: String
sandboxBusyboxVar = "NOVA_NIX_TEST_BUSYBOX"

-- | The variable naming a directory with a mount beneath it, at @inner@,
-- holding a file @inner/file@ the test's user may write: CI's Linux jobs
-- mount a tmpfs there for the test of a bind's submounts, which runs only
-- where the variable is set.
sandboxSubmountVar :: String
sandboxSubmountVar = "NOVA_NIX_TEST_SANDBOX_SUBMOUNT"

-- | The variable naming a file another user owns, root, in a directory
-- the test's user may write: CI's Linux jobs make one for the test of an
-- output holding such a node, which runs only where the variable is set
-- and moves the file away.
foreignFileVar :: String
foreignFileVar = "NOVA_NIX_TEST_FOREIGN_FILE"

-- | The variable naming a directory root owns at 0711, holding a
-- directory @inner@: CI's Linux jobs make one for the test of a bind
-- beneath a directory its user may search but not read, which runs only
-- where the variable is set.
searchOnlyDirVar :: String
searchOnlyDirVar = "NOVA_NIX_TEST_SEARCH_ONLY_DIR"

-- | The flag that makes the test binary a process that runs one sandboxed
-- build, for a test to kill: the helper, the busybox, the store root and
-- a marker the build's sleeps carry.
sandboxBuildChildFlag :: String
sandboxBuildChildFlag = "--sandbox-build-child"

-- | Run, in a child process, a sandboxed build that sleeps until killed.
sandboxBuildChild :: FilePath -> FilePath -> FilePath -> String -> IO ()
sandboxBuildChild helper busybox storeRoot marker = do
  busyboxBytes <- Spawn.filePathBytes busybox
  let outPath = StorePath (T.replicate 31 "s" <> "e") "sandboxed"
      config store = (defaultBuildConfig (stDir store)) {bcSandbox = linuxSandboxConfig helper busyboxBytes}
      script = "/bin/busybox sleep " <> T.pack marker <> "; echo ran > $out"
  bracket (openStore testLogger (StoreDir storeRoot)) closeStore $ \store ->
    void (buildDerivation (config store) store (mkTestBuildDrv "/bin/sh" outPath script) testDrvPath)

-- | The sandbox the Linux sandbox tests build in: on, with no fallback,
-- and the static busybox as @/bin/sh@ and @/bin/busybox@.
linuxSandboxConfig :: FilePath -> BS.ByteString -> Sandbox.SandboxConfig
linuxSandboxConfig helper busyboxBytes =
  Sandbox.defaultSandboxConfig
    { Sandbox.scMode = Sandbox.SandboxEnabled,
      Sandbox.scFallback = Sandbox.RefuseUnsandboxed,
      Sandbox.scPaths = [Sandbox.SandboxPath "/bin/sh" busyboxBytes Sandbox.RequiredPath, Sandbox.SandboxPath "/bin/busybox" busyboxBytes Sandbox.RequiredPath],
      Sandbox.scHelper = Just helper
    }

-- | The flag that makes the test binary a process that runs two builds
-- under @sandbox = true@ with no helper, so each falls back.
sandboxFallbackChildFlag :: String
sandboxFallbackChildFlag = "--sandbox-fallback-child"

-- | Run two builds that fall back to running unsandboxed, exiting 0 when
-- both succeed.  The warnings go to standard error, which the parent
-- reads, and the builders' lines to standard output.
sandboxFallbackChild :: FilePath -> IO ()
sandboxFallbackChild storeRoot = do
  shell <- findTestShell
  let config store = (defaultBuildConfig (stDir store)) {bcSandbox = Sandbox.defaultSandboxConfig {Sandbox.scMode = Sandbox.SandboxEnabled}}
      outPath letter = StorePath (T.replicate 31 "f" <> T.singleton letter) "fallback"
  results <- bracket (openStore testLogger (StoreDir storeRoot)) closeStore $ \store ->
    traverse (\letter -> buildDerivation (config store) (buildLogTo stdout store) (mkTestBuildDrv shell (outPath letter) "echo ran > $out") testDrvPath) ['1', '2']
  traverse_ (TIO.hPutStrLn stderr) [msg | BuildFailure msg _ <- results]
  case [() | BuildSuccess _ <- results] of
    [_, _] -> pure ()
    _ -> exitFailure

-- | A process falls back to running its builds unsandboxed after one
-- warning, however many builds fall back; on macOS, where upstream reads
-- no @sandbox-fallback@, each fails instead.
testSandboxFallbackIO :: IO [Bool]
testSandboxFallbackIO = do
  putStrLn "builder/sandbox-fallback"
  tmpBase <- getTemporaryDirectory
  self <- getExecutablePath
  let storeRoot = tmpBase </> "nova-nix-test-sandbox-fallback"
  bracket_ (forceRemoveIfExists storeRoot) (forceRemoveIfExists storeRoot) $ do
    (code, _, err) <- Proc.readProcessWithExitCode self [sandboxFallbackChildFlag, storeRoot] ""
    let warnings = filter ("warning: building without a sandbox, since " `T.isPrefixOf`) (T.lines (T.pack err))
        refusals = filter (== "the sandbox helper 'nova-nix-sandbox' was not found beside nova-nix; use '--no-sandbox' to disable sandboxing") (T.lines (T.pack err))
    sequence
      [ case SI.os of
          "darwin" ->
            runTest "on macOS two builds that cannot be sandboxed fail, though sandbox-fallback is on" $
              assertEqual "exit" (ExitFailure 1) code `andThen` assertEqual "warnings" 0 (length warnings) `andThen` assertEqual "refusals" 2 (length refusals)
          _ ->
            runTest "two builds that fall back print one warning" $
              assertEqual "exit" ExitSuccess code `andThen` assertEqual "warnings" 1 (length warnings)
      ]

-- | The sandbox the macOS sandbox tests build in: on, with no fallback,
-- the shipped @sandbox-paths@ and @allowed-impure-host-deps@ defaults,
-- and beside them the programs the tests run and the OpenSSL
-- configuration the system's @curl@ reads before it connects anywhere.
darwinSandboxConfig :: FilePath -> Sandbox.SandboxConfig
darwinSandboxConfig helper =
  Sandbox.defaultSandboxConfig
    { Sandbox.scMode = Sandbox.SandboxEnabled,
      Sandbox.scFallback = Sandbox.RefuseUnsandboxed,
      Sandbox.scPaths =
        Sandbox.parseSandboxPaths Sandbox.darwinSandboxPaths
          ++ [ Sandbox.SandboxPath path path Sandbox.RequiredPath
             | path <- ["/bin/cat", "/bin/chmod", "/bin/dd", "/bin/mkdir", "/bin/sleep", "/usr/bin/curl", "/usr/bin/nc", "/usr/bin/yes", "/private/etc/ssl"]
             ],
      Sandbox.scAllowedImpureHostDeps = Sandbox.darwinImpureHostPrefixes,
      Sandbox.scHelper = Just helper
    }

-- | The flag that makes the test binary a process a sandboxed build
-- starts, which leaves the builder's process group for a session of its
-- own and waits to change the build's output once it is registered.
sandboxEscapeChildFlag :: String
sandboxEscapeChildFlag = "--sandbox-escape-child"

-- | How long an escaped process waits for the output it changes to be
-- registered: longer than a test waits for it to be gone, so one the
-- sandbox helper left running is still there to be seen.
escapeWaitMicros :: Int
escapeWaitMicros = 2 * processWaitMicros

-- | The flag that makes the test binary a process a sandboxed build
-- starts, which leaves the builder's process group and starts sleepers as
-- fast as it can.
sandboxForkerChildFlag :: String
sandboxForkerChildFlag = "--sandbox-forker-child"

-- | How many sleepers a forker starts: enough that the sweep meets it still
-- starting them, and few enough to do no harm if they were left.
forkerSleepers :: Int
forkerSleepers = 200

-- | Leave the builder's process group and say so by making the file
-- @ready@, then start 'forkerSleepers' processes sleeping for @marker@
-- seconds, which the tests find them by.
sandboxForkerChild :: String -> FilePath -> IO ()
sandboxForkerChild marker ready = do
  left <- newSession
  when left $ do
    BS.writeFile ready ""
    replicateM_ forkerSleepers (void (Proc.spawnProcess "/bin/sleep" [marker]))

-- | The flag that makes the test binary a process a sandboxed build
-- starts, which leaves the builder's process group, starts a shell that
-- starts sleepers, and continues that shell for as long as it runs, as a
-- process of a build may to undo the sweep's stopping its sibling.
sandboxThawChildFlag :: String
sandboxThawChildFlag = "--sandbox-thaw-child"

-- | Leave the builder's process group, start a shell that starts
-- 'forkerSleepers' processes sleeping for @marker@ seconds, say so by
-- making the file @ready@, then send the shell @SIGCONT@ until it ends.
sandboxThawChild :: String -> FilePath -> IO ()
sandboxThawChild marker ready = do
  left <- newSession
  when left $ do
    let forkLoop = "i=0; while [ $i -lt " <> show forkerSleepers <> " ]; do /bin/sleep " <> marker <> " & i=$((i + 1)); done"
    (_, _, _, forker) <- Proc.createProcess (Proc.proc "/bin/sh" ["-c", forkLoop])
    BS.writeFile ready ""
    let keepContinuing = do
          ended <- Proc.getProcessExitCode forker
          when (isNothing ended) (continueProcess forker *> keepContinuing)
    keepContinuing

-- | How long a test gives the sandbox helper, its log unread, to end once
-- told: far less than 'processWaitMicros', so a helper that waits on the
-- log for any length of time fails the test rather than slipping under it.
helperEndMicros :: Int
helperEndMicros = processWaitMicros `div` 5

-- | How long a test gives the sandbox helper to act on a signal before it
-- takes the build to have carried on through it.
signalSettleMicros :: Int
signalSettleMicros = processWaitMicros `div` 10

-- | Leave the builder's process group and say so by making the file
-- @ready@, then wait for the output at @out@ to be registered, which
-- makes it read-only, and make it writable and append to it.
sandboxEscapeChild :: FilePath -> FilePath -> IO ()
sandboxEscapeChild out ready = do
  left <- newSession
  when left $ do
    BS.writeFile ready ""
    registered <- waitUntilWithin escapeWaitMicros readOnly
    when registered $ do
      Dir.getPermissions out >>= Dir.setPermissions out . Dir.setOwnerWritable True
      BS.appendFile out "poisoned\n"
  where
    readOnly = do
      present <- Dir.doesFileExist out
      if present then not . Dir.writable <$> Dir.getPermissions out else pure False

-- | How many store paths the macOS test of a large profile names: more
-- than macOS compiles into one table for an operation when they share
-- one form, which it refuses near 8000, and fewer than it takes in all,
-- near 12000.
darwinProfileStorePaths :: Int
darwinProfileStorePaths = 8000

-- | The command line of every process running, one to an entry.
runningCommands :: IO [BS.ByteString]
runningCommands = map BS8.pack . lines <$> Proc.readProcess "ps" ["-axo", "command="] ""

-- | macOS builds under the sandbox's profile, with @sandbox = true@ and
-- @sandbox-fallback = false@ and the system's @\/bin\/sh@ as the builder:
-- what a build cannot read, write or reach, what it can, and how a failed
-- setup and an interrupt end.  The helper is the one 'sandboxHelperVar'
-- names, which this run can use; with it unset the group skips, and a
-- name that is set but missing fails.  The group runs with @TMPDIR@
-- naming a directory of its own beside the store and the host files the
-- builds reach for, so a build that reached it would reach only that.
testDarwinSandboxIO :: IO [Bool]
testDarwinSandboxIO = do
  putStrLn "builder/sandbox-darwin"
  named <- lookupEnv sandboxHelperVar
  case (SI.os, named) of
    ("darwin", Just helper) -> do
      present <- Dir.doesFileExist helper
      if present
        then testDarwinSandboxBody helper
        else (: []) <$> runTest (T.pack sandboxHelperVar <> " names a file") (Fail (T.pack helper))
    ("darwin", Nothing) -> do
      putStrLn ("  SKIP  needs " ++ sandboxHelperVar)
      pure []
    _ -> do
      putStrLn "  SKIP  macOS only"
      pure []

testDarwinSandboxBody :: FilePath -> IO [Bool]
testDarwinSandboxBody helper = do
  tmpBase <- getTemporaryDirectory
  self <- getExecutablePath >>= Spawn.filePathBytes >>= Spawn.resolvedPath
  let root = tmpBase </> "nova-nix-test-sandbox-darwin"
      storeRoot = root </> "store"
      outside = root </> "outside"
      scratch = root </> "tmp"
      sandbox = darwinSandboxConfig helper
      outputAt letter = StorePath (T.replicate 31 "m" <> T.singleton letter) "sandboxed"
      drvFor letter = mkTestBuildDrv "/bin/sh" (outputAt letter)
      configFor store = (defaultBuildConfig (stDir store)) {bcSandbox = sandbox}
      outputFile store letter = storePathToFilePath (stDir store) (outputAt letter)
      withSandboxStore = bracket (openStore testLogger (StoreDir storeRoot)) closeStore
      readOutput store letter = BS.readFile (outputFile store letter)
      unsandboxedConfig store = (configFor store) {bcSandbox = sandbox {Sandbox.scMode = Sandbox.SandboxDisabled}}
      -- A build that runs the test binary as a child the flag names,
      -- which leaves the builder's group and says so, its standard
      -- streams on /dev/null so it writes nothing into the build log; the
      -- build goes on once it has.
      escapeConfig store = (configFor store) {bcSandbox = sandbox {Sandbox.scPaths = Sandbox.scPaths sandbox ++ [Sandbox.SandboxPath self self Sandbox.RequiredPath]}}
      escapeReady = "\"$NIX_BUILD_TOP/escaped\""
      childScript flag argument =
        T.concat
          [ TE.decodeUtf8 self <> " " <> T.pack flag <> " " <> argument <> " " <> escapeReady <> " < /dev/null > /dev/null 2>&1 & ",
            awaitPathScript escapeReady <> "; ",
            "[ -e " <> escapeReady <> " ] || exit 3; "
          ]
      -- One that waits to change the output once it is registered.
      escapeScriptFor = childScript sandboxEscapeChildFlag
      escapeScript = escapeScriptFor "\"$out\""
      escapees = length . filter ((self <> " " <> BS8.pack sandboxEscapeChildFlag) `BS.isPrefixOf`) <$> runningCommands
      sleepersWith marker = length . filter (("/bin/sleep " <> TE.encodeUtf8 marker) `BS.isPrefixOf`) <$> runningCommands
      helperProcesses = do
        listing <- Proc.readProcess "ps" ["-axo", "pid=,command="] ""
        pure [pid | (pid : command : _) <- map words (lines listing), command == helper]
  bracket_
    (forceRemoveIfExists root >> traverse_ (createDirectoryIfMissing True) [outside, scratch] >> BS.writeFile (outside </> "secret") "host secret\n")
    (forceRemoveIfExists root)
    $ withEnvVar "TMPDIR" scratch
    $ sequence
      [ runTestM "a sandboxed build runs in its build directory, which it writes, told upstream's HOME" $
          withSandboxStore $ \store -> do
            result <- buildDerivation (configFor store) store (drvFor '1' "echo \"$HOME\" > $out; cd \"$NIX_BUILD_TOP\" && echo scratch > \"$TMPDIR/f\" && echo written >> $out") testDrvPath
            case result of
              BuildSuccess _ -> assertEqual "seen" "/homeless-shelter\nwritten\n" <$> readOutput store '1'
              BuildFailure msg _ -> pure (Fail msg),
        runTestM "a sandboxed build cannot read a host path outside its inputs, which an unsandboxed one can" $
          withSandboxStore $ \store -> do
            let script = "/bin/cat " <> T.pack (outside </> "secret") <> " > $out 2>/dev/null || echo refused > $out"
            sandboxed <- buildDerivation (configFor store) store (drvFor '2' script) testDrvPath
            sandboxedSaw <- readOutput store '2'
            unsandboxed <- buildDerivation ((configFor store) {bcSandbox = sandbox {Sandbox.scMode = Sandbox.SandboxDisabled}}) store (drvFor '3' script) testDrvPath
            unsandboxedSaw <- readOutput store '3'
            pure $
              assertEqual "sandboxed" (BuildSuccess (outputAt '2')) sandboxed
                `andThen` assertEqual "what the sandboxed build read" "refused\n" sandboxedSaw
                `andThen` assertEqual "unsandboxed" (BuildSuccess (outputAt '3')) unsandboxed
                `andThen` assertEqual "what the unsandboxed build read" "host secret\n" unsandboxedSaw,
        runTestM "a sandboxed build cannot write a host path outside its outputs and its build directory, the temporary directory nova-nix runs with included, nor the store" $
          withSandboxStore $ \store -> do
            let script =
                  T.concat
                    [ "echo escaped 2>/dev/null > " <> T.pack (outside </> "escaped") <> "; ",
                      "echo escaped 2>/dev/null > " <> T.pack (storeRoot </> "escaped") <> "; ",
                      "echo kept > " <> T.pack (scratch </> "kept") <> "; ",
                      "echo ran > $out"
                    ]
            result <- buildDerivation (configFor store) store (drvFor '4' script) testDrvPath
            escaped <- traverse Dir.doesPathExist [outside </> "escaped", storeRoot </> "escaped"]
            kept <- Dir.doesPathExist (scratch </> "kept")
            pure $
              assertEqual "build" (BuildSuccess (outputAt '4')) result
                `andThen` assertEqual "escaped" [False, False] escaped
                `andThen` assertEqual "the temporary directory, written" False kept,
        runTestM "an input a sandboxed builder tries to make writable and change keeps its bytes and mode" $
          withSandboxStore $ \store -> do
            dep <- buildDerivation (configFor store) store (drvFor '5' "/bin/mkdir $out; echo original > $out/file") testDrvPath
            case dep of
              BuildFailure msg _ -> pure (Fail ("dependency: " <> msg))
              BuildSuccess depPath -> do
                let depFile = storePathToFilePath (stDir store) depPath </> "file"
                modeBefore <- fmap nsPermissions <$> nodeStatus depFile
                let script = "/bin/chmod u+w " <> T.pack depFile <> " 2>/dev/null; echo changed 2>/dev/null >> " <> T.pack depFile <> "; /bin/cat " <> T.pack depFile <> " > $out"
                result <- buildDerivation (configFor store) store ((drvFor '6' script) {drvInputSrcs = [depPath]}) testDrvPath
                contents <- BS.readFile depFile
                seen <- readOutput store '6'
                modeAfter <- fmap nsPermissions <$> nodeStatus depFile
                pure $
                  assertEqual "build" (BuildSuccess (outputAt '6')) result
                    `andThen` assertEqual "what the build read" "original\n" seen
                    `andThen` assertEqual "contents" "original\n" contents
                    `andThen` assertEqual "mode" modeBefore modeAfter,
        runTestM "a sandboxed build cannot change a host file a sandbox-paths entry names" $
          withSandboxStore $ \store -> do
            ownedBytes <- Spawn.filePathBytes (outside </> "owned")
            BS.writeFile (outside </> "owned") "the user's\n"
            realOwned <- Spawn.resolvedPath ownedBytes
            let bound = sandbox {Sandbox.scPaths = Sandbox.scPaths sandbox ++ [Sandbox.SandboxPath realOwned realOwned Sandbox.RequiredPath]}
                script = "echo changed 2>/dev/null > " <> TE.decodeUtf8 realOwned <> "; /bin/cat " <> TE.decodeUtf8 realOwned <> " > $out"
            result <- buildDerivation ((configFor store) {bcSandbox = bound}) store (drvFor '7' script) testDrvPath
            seen <- readOutput store '7'
            contents <- BS.readFile (outside </> "owned")
            pure $
              assertEqual "build" (BuildSuccess (outputAt '7')) result
                `andThen` assertEqual "what the build read" "the user's\n" seen
                `andThen` assertEqual "the host file" "the user's\n" contents,
        runTestM "a sandboxed build that is not fixed-output reaches no host server, a fixed-output one does, and so does one asking for local networking" $
          withHttpServer (\_ _ -> Send (httpResponse "200 OK" "payload")) $ \base requests _ ->
            withSandboxStore $ \store -> do
              let fetch target = "/usr/bin/curl -s -f -o $out " <> base <> target
              private <- buildDerivation (configFor store) store (drvFor '8' (fetch "/private" <> " || echo unreachable > $out")) testDrvPath
              privateSeen <- readOutput store '8'
              beforeFixed <- requests
              let digest = maybe "" Hash.bytesToHexText (Hash.rawHashWithAlgo "sha256" "payload")
                  fixedPath = StorePath (T.replicate 31 "m" <> "9") "fixed"
              fixed <- buildDerivation (configFor store) store (mkFixedOutputDrv "/bin/sh" fixedPath (fetch "/fixed") "sha256" digest) testDrvPath
              afterFixed <- requests
              let base9 = drvFor 'a' (fetch "/local")
                  local = base9 {drvEnv = Map.insert "__darwinAllowLocalNetworking" "1" (drvEnv base9)}
              localResult <- buildDerivation (configFor store) store local testDrvPath
              afterLocal <- requests
              pure $
                assertEqual "private build" (BuildSuccess (outputAt '8')) private
                  `andThen` assertEqual "private output" "unreachable\n" privateSeen
                  `andThen` assertEqual "requests from the private build" [] beforeFixed
                  `andThen` assertEqual "fixed build" (BuildSuccess fixedPath) fixed
                  `andThen` assertEqual "requests from the fixed build" ["/fixed"] afterFixed
                  `andThen` assertEqual "local build" (BuildSuccess (outputAt 'a')) localResult
                  `andThen` assertEqual "requests from the local build" ["/fixed", "/local"] afterLocal,
        runTestM "a setup step that fails is reported in upstream's words" $
          withSandboxStore $ \store -> do
            let with paths = (configFor store) {bcSandbox = sandbox {Sandbox.scPaths = Sandbox.scPaths sandbox ++ paths}}
            missing <- buildDerivation (with [Sandbox.SandboxPath "/nonexistent-nova-nix-test" "/nonexistent-nova-nix-test" Sandbox.RequiredPath]) store (drvFor 'b' "echo ran > $out") testDrvPath
            mapped <- buildDerivation (with [Sandbox.SandboxPath "/bin/sh" "/bin/bash" Sandbox.RequiredPath]) store (drvFor 'c' "echo ran > $out") testDrvPath
            pure $
              assertEqual "missing" (BuildFailure "while setting up the build environment: getting attributes of required path '/nonexistent-nova-nix-test: No such file or directory" 1) missing
                `andThen` assertEqual "mapped" (BuildFailure "while setting up the build environment: can't map \"/bin/sh\" to \"/bin/bash\": mismatched impure paths not supported on Darwin" 1) mapped,
        runTestM "under relaxed a derivation's own profile comes last, and one the sandbox refuses fails the build, upstream's line and the sandbox's reason in the log" $
          withSandboxStore $ \store -> do
            secretBytes <- Spawn.filePathBytes (outside </> "secret")
            realSecret <- Spawn.resolvedPath secretBytes
            let relaxed = (configFor store) {bcSandbox = sandbox {Sandbox.scMode = Sandbox.SandboxRelaxed}}
                withProfile letter profile = let drv = drvFor letter ("/bin/cat " <> TE.decodeUtf8 realSecret <> " > $out") in drv {drvEnv = Map.insert "__sandboxProfile" profile (drvEnv drv)}
                logPath = root </> "profile.log"
            allowed <- buildDerivation relaxed store (withProfile 'd' ("(allow file-read* (literal \"" <> realSecret <> "\"))")) testDrvPath
            seen <- readOutput store 'd'
            refused <- withBinaryFile logPath WriteMode $ \logHandle -> buildDerivation relaxed (buildLogTo logHandle store) (withProfile 'e' "(allow nonsense-operation)") testDrvPath
            logged <- BS.readFile logPath
            pure $
              assertEqual "allowed" (BuildSuccess (outputAt 'd')) allowed
                `andThen` assertEqual "what it read" "host secret\n" seen
                `andThen` assertEqual "refused" (BuildFailure "while setting up the build environment: failed to configure sandbox" 1) refused
                `andThen` assertEqual "the sandbox's words, logged" True ("failed to configure sandbox: " `BS.isInfixOf` logged),
        runTestM "the helper applies a profile naming thousands of store paths, more than macOS compiles into one table for an operation" $ do
          createDirectoryIfMissing True storeRoot
          realStore <- Spawn.filePathBytes storeRoot >>= Spawn.resolvedPath
          realScratch <- Spawn.filePathBytes scratch >>= Spawn.resolvedPath
          let nix32 = "0123456789abcdfghijklmnpqrsvwxyz" :: String
              storePath index = realStore <> "/" <> BS8.pack (take 32 (drop (index `mod` 32) (cycle nix32))) <> "-p" <> BS8.pack (show index)
              system = [Sandbox.Literal "/bin/sh", Sandbox.Literal "/bin/bash", Sandbox.Subpath "/usr/lib", Sandbox.Subpath "/System/Library/Frameworks", Sandbox.Subpath "/System/Library/PrivateFrameworks"]
              ancestors path = "/" : [BS.intercalate "/" ("" : parts) | parts <- drop 1 (inits (filter (not . BS.null) (BS8.split '/' path)))]
              profile =
                Sandbox.darwinProfile
                  Sandbox.DarwinProfile
                    { Sandbox.dprViolationLog = Sandbox.ViolationsUnlogged,
                      Sandbox.dprNetwork = Sandbox.PrivateNetwork,
                      Sandbox.dprStoreDir = realStore,
                      Sandbox.dprOutputs = [],
                      Sandbox.dprPaths = system ++ [Sandbox.Subpath (storePath index) | index <- [1 .. darwinProfileStorePaths]],
                      Sandbox.dprAncestry = Set.toAscList (Set.fromList (concatMap ancestors ["/bin", "/usr", "/System/Library", realScratch])),
                      Sandbox.dprExtra = ""
                    }
              plan = Sandbox.DarwinPlan realScratch profile (Sandbox.darwinParameters realScratch Sandbox.LocalNetworkingDenied) []
          run <- withBinaryFile (root </> "large-profile.log") WriteMode $ \logHandle ->
            Spawn.runHelper (handleLogger DropEscapes logHandle) helper (Sandbox.renderDarwinPlan plan) scratch ["/bin/sh", "-c", "exit 7"]
          pure $
            assertEqual "the profile's size" True (BS.length profile > 65536)
              `andThen` assertEqual
                "outcome"
                (Sandbox.BuilderRan (ExitFailure 7))
                (Sandbox.helperOutcome Sandbox.darwinStageName (Sandbox.darwinSetupFailureMessage plan "/bin/sh") (Spawn.hrDelivered run) (Spawn.hrExit run) (Spawn.hrStatus run)),
        runTestM "an interrupt ends every process of the build, and leaves no build directory" $
          withSandboxStore $ \store -> do
            let marker = "987655"
                sleeper = (("/bin/sleep " <> TE.encodeUtf8 marker) `BS.isPrefixOf`)
                sleepers = length . filter sleeper <$> runningCommands
                script = startedScript <> "/bin/sleep " <> marker <> "1 & /bin/sleep " <> marker <> "2; echo ran > $out"
                buildDir = buildDirFor (stDir store) (outputAt 'f')
            withAsync (buildDerivation (configFor store) store (drvFor 'f' script) testDrvPath) $ \build -> do
              started <- awaitStart buildDir (isJust <$> poll build)
              appeared <- (started &&) <$> waitUntilWithin startWaitMicros ((== 2) <$> sleepers)
              cancel build
              gone <- waitUntil ((== 0) <$> sleepers)
              buildDirLeft <- Dir.doesPathExist buildDir
              pure $
                assertEqual "started" True appeared
                  `andThen` assertEqual "processes gone" True gone
                  `andThen` assertEqual "build directory left" False buildDirLeft,
        runTestM "a process of the build that leaves the builder's group ends with the build, before its output is registered" $
          withSandboxStore $ \store -> do
            result <- buildDerivation (escapeConfig store) store (drvFor 'n' (escapeScript <> "echo built > $out")) testDrvPath
            gone <- waitUntil ((== 0) <$> escapees)
            contents <- readOutput store 'n'
            pure $
              assertEqual "build" (BuildSuccess (outputAt 'n')) result
                `andThen` assertEqual "the escaped process, gone" True gone
                `andThen` assertEqual "the output" "built\n" contents,
        runTestM "USR1 sent to the helper ends every process of the build, one that left the builder's group included" $
          withSandboxStore $ \store -> do
            let marker = "987656"
                script = startedScript <> escapeScript <> "/bin/sleep " <> marker <> "; echo ran > $out"
            withAsync (buildDerivation (escapeConfig store) store (drvFor 'p' script) testDrvPath) $ \build -> do
              started <- awaitStart (buildDirFor (stDir store) (outputAt 'p')) (isJust <$> poll build)
              appeared <- (started &&) <$> waitUntilWithin startWaitMicros ((== (1, 1)) <$> ((,) <$> sleepersWith marker <*> escapees))
              helpers <- helperProcesses
              traverse_ (\pid -> Proc.callProcess "/bin/kill" ["-USR1", pid]) helpers
              result <- timeout startWaitMicros (wait build)
              gone <- waitUntil ((== (0, 0)) <$> ((,) <$> sleepersWith marker <*> escapees))
              pure $
                assertEqual "started" True appeared
                  `andThen` assertEqual "helpers signalled" 1 (length helpers)
                  `andThen` assertEqual "build failed" True (case result of Just (BuildFailure _ _) -> True; _ -> False)
                  `andThen` assertEqual "processes gone" True gone,
        runTestM "a build that ends leaves another build's processes in the same store running, and a sandboxed process of the user's" $
          withSandboxStore $ \store -> withSandboxStore $ \other -> do
            let marker = "987657"
                unrelatedMarker = "987659"
                script = startedScript <> "/bin/sleep " <> marker <> "1 & /bin/sleep " <> marker <> "2; echo ran > $out"
                unrelatedProfile =
                  unlines
                    [ "(version 1)",
                      "(deny default (with no-log))",
                      "(allow process-exec file-read* (literal \"/bin/sleep\") (subpath \"/usr/lib\") (subpath \"/System/Library\"))",
                      "(allow file-read* (literal \"/\") (subpath \"/dev\"))"
                    ]
                unrelatedSpec = Proc.proc "/usr/bin/sandbox-exec" ["-p", unrelatedProfile, "/bin/sleep", T.unpack unrelatedMarker]
            bracket (Proc.createProcess unrelatedSpec) (\(_, _, _, handle) -> Proc.terminateProcess handle *> void (Proc.waitForProcess handle)) $ \_ ->
              withAsync (buildDerivation (configFor store) store (drvFor 'q' script) testDrvPath) $ \running -> do
                started <- awaitStart (buildDirFor (stDir store) (outputAt 'q')) (isJust <$> poll running)
                appeared <- (started &&) <$> waitUntilWithin startWaitMicros ((== (2, 1)) <$> ((,) <$> sleepersWith marker <*> sleepersWith unrelatedMarker))
                ended <- buildDerivation (configFor other) other (drvFor 'r' "echo ran > $out") testDrvPath
                left <- (,) <$> sleepersWith marker <*> sleepersWith unrelatedMarker
                cancel running
                gone <- waitUntil ((== 0) <$> sleepersWith marker)
                pure $
                  assertEqual "started" True appeared
                    `andThen` assertEqual "the build that ended" (BuildSuccess (outputAt 'r')) ended
                    `andThen` assertEqual "the other build's processes and the user's, once it ended" (2, 1) left
                    `andThen` assertEqual "the other build's, gone once it is cancelled" True gone,
        runTestM "a sandboxed build reaches no Unix socket in the temporary directory nova-nix runs with, which an unsandboxed build does" $ do
          -- A temporary directory of its own, beside the group's, keeps the
          -- socket's path within the 104 bytes a Unix socket address holds.
          let socketDir = tmpBase </> "nn-sock"
              socketPath = socketDir </> "s"
              script = "/usr/bin/nc -U " <> T.pack socketPath <> " < /dev/null > /dev/null 2>&1; echo $? > $out"
          bracket_ (createDirectoryIfMissing True socketDir) (forceRemoveIfExists socketDir) . withEnvVar "TMPDIR" socketDir . withUnixListener socketPath $ \accepted -> withSandboxStore $ \store -> do
            sandboxed <- buildDerivation (configFor store) store (drvFor 's' script) testDrvPath
            sandboxedExit <- readOutput store 's'
            afterSandboxed <- accepted
            unsandboxed <- buildDerivation (unsandboxedConfig store) store (drvFor 'v' script) testDrvPath
            unsandboxedExit <- readOutput store 'v'
            reached <- waitUntilWithin startWaitMicros ((== 1) <$> accepted)
            pure $
              assertEqual "sandboxed" (BuildSuccess (outputAt 's')) sandboxed
                `andThen` assertEqual "nc's exit, sandboxed" "1\n" sandboxedExit
                `andThen` assertEqual "connections from the sandboxed build" 0 afterSandboxed
                `andThen` assertEqual "unsandboxed" (BuildSuccess (outputAt 'v')) unsandboxed
                `andThen` assertEqual "nc's exit, unsandboxed" "0\n" unsandboxedExit
                `andThen` assertEqual "the unsandboxed build connected" True reached,
        runTestM "a sandboxed build cannot open a terminal of the user's to read what is typed there, even with /dev among its impure host paths, which an unsandboxed build can" $
          fmap (fromMaybe (Fail "no pseudo-terminal")) . withPseudoTerminal $ \terminal ->
            withSandboxStore $ \store -> do
              let script = "( : < " <> T.pack terminal <> " ) 2>/dev/null && echo opened > $out || echo refused > $out"
                  withDev drv = drv {drvEnv = Map.insert "__impureHostDeps" "/dev" (drvEnv drv)}
              plain <- buildDerivation (configFor store) store (drvFor 'w' script) testDrvPath
              plainSeen <- readOutput store 'w'
              viaDev <- buildDerivation (configFor store) store (withDev (drvFor 'x' script)) testDrvPath
              viaDevSeen <- readOutput store 'x'
              unsandboxed <- buildDerivation (unsandboxedConfig store) store (drvFor 'y' script) testDrvPath
              unsandboxedSeen <- readOutput store 'y'
              pure $
                assertEqual "builds" [BuildSuccess (outputAt 'w'), BuildSuccess (outputAt 'x'), BuildSuccess (outputAt 'y')] [plain, viaDev, unsandboxed]
                  `andThen` assertEqual "sandboxed" "refused\n" plainSeen
                  `andThen` assertEqual "with /dev" "refused\n" viaDevSeen
                  `andThen` assertEqual "unsandboxed" "opened\n" unsandboxedSeen,
        runTestM "a process of the build that keeps starting others outside the builder's group is stopped and ended with all it started" $
          withSandboxStore $ \store -> do
            let marker = "987660"
            result <- buildDerivation (escapeConfig store) store (drvFor 'k' (childScript sandboxForkerChildFlag marker <> "echo ran > $out")) testDrvPath
            left <- sleepersWith marker
            forkers <- length . filter ((self <> " " <> BS8.pack sandboxForkerChildFlag) `BS.isPrefixOf`) <$> runningCommands
            pure $
              assertEqual "build" (BuildSuccess (outputAt 'k')) result
                `andThen` assertEqual "sleepers left once the build returned" 0 left
                `andThen` assertEqual "forkers left" 0 forkers,
        runTestM "a process of the build that keeps continuing another the sweep stops is ended with it and all it started, build after build" $
          withSandboxStore $ \store -> do
            let marker = "987661"
                leftovers = length . filter (BS.isInfixOf (TE.encodeUtf8 marker)) <$> runningCommands
                thawBuild letter = do
                  result <- buildDerivation (escapeConfig store) store (drvFor letter (childScript sandboxThawChildFlag marker <> "echo ran > $out")) testDrvPath
                  left <- leftovers
                  pure (result, left)
            runs <- traverse thawBuild "ghi"
            pure $ assertEqual "builds and what each left" [(BuildSuccess (outputAt letter), 0) | letter <- "ghi"] runs,
        runTestM "a build's output reaches the build log whole through the helper's pipe, a megabyte of it included" $
          withSandboxStore $ \store -> do
            let logPath = root </> "large-output.log"
                script = "/bin/dd if=/dev/zero bs=1000 count=1000 2>/dev/null; echo end; echo ran > $out"
            result <- withBinaryFile logPath WriteMode $ \logHandle -> buildDerivation (configFor store) (buildLogTo logHandle store) (drvFor 'j' script) testDrvPath
            logged <- BS.readFile logPath
            pure $
              assertEqual "build" (BuildSuccess (outputAt 'j')) result
                `andThen` assertEqual "zero bytes" 1000000 (BS.count 0 logged)
                `andThen` assertEqual "the last line" True ("end\n" `BS.isSuffixOf` logged),
        runTestM "cancelling a build whose builder writes lines without pause ends it within the wait, its processes gone" $
          withSandboxStore $ \store -> withBinaryFile "/dev/null" WriteMode $ \discard -> do
            let marker = "987662"
                writer = "/usr/bin/yes " <> marker
                writers = length . filter (TE.encodeUtf8 writer `BS.isPrefixOf`) <$> runningCommands
            -- Not withAsync, which would wait for a build that cannot end,
            -- so a cancel that never returns fails the test at the timeout.
            build <- async (buildDerivation (configFor store) (buildLogTo discard store) (drvFor '0' (startedScript <> writer <> "; echo ran > $out")) testDrvPath)
            running <- awaitStart (buildDirFor (stDir store) (outputAt '0')) (isJust <$> poll build)
            started <- (running &&) <$> waitUntilWithin startWaitMicros ((== 1) <$> writers)
            cancelled <- timeout startWaitMicros (cancel build)
            gone <- waitUntil ((== 0) <$> writers)
            pure $
              assertEqual "started" True started
                `andThen` assertEqual "cancelled within the wait" (Just ()) cancelled
                `andThen` assertEqual "the writer, gone" True gone,
        runTestM "the helper ends a build whose log is open but unread within the wait, on SIGTERM and on the lifeline's end" $ do
          createDirectoryIfMissing True storeRoot
          realStore <- Spawn.filePathBytes storeRoot >>= Spawn.resolvedPath
          realScratch <- Spawn.filePathBytes scratch >>= Spawn.resolvedPath
          -- This run's own, so a suite running beside it cannot be taken
          -- for it.
          pid <- Proc.getCurrentPid
          nonce <- QC.generate (QC.choose (0, maxBound :: Int))
          let marker = "nova-nix-unread-" <> T.pack (show pid) <> "-" <> T.pack (show nonce)
              writers = length . filter (("/usr/bin/yes " <> TE.encodeUtf8 marker) `BS.isPrefixOf`) <$> runningCommands
              ancestors path = "/" : [BS.intercalate "/" ("" : parts) | parts <- drop 1 (inits (filter (not . BS.null) (BS8.split '/' path)))]
              profile =
                Sandbox.darwinProfile
                  Sandbox.DarwinProfile
                    { Sandbox.dprViolationLog = Sandbox.ViolationsUnlogged,
                      Sandbox.dprNetwork = Sandbox.PrivateNetwork,
                      Sandbox.dprStoreDir = realStore,
                      Sandbox.dprOutputs = [],
                      Sandbox.dprPaths = [Sandbox.Literal "/bin/sh", Sandbox.Literal "/bin/bash", Sandbox.Literal "/usr/bin/yes", Sandbox.Subpath "/usr/lib", Sandbox.Subpath "/System/Library/Frameworks"],
                      Sandbox.dprAncestry = Set.toAscList (Set.fromList (concatMap ancestors ["/bin", "/usr/bin", "/System/Library", realScratch])),
                      Sandbox.dprExtra = ""
                    }
              plan = Sandbox.DarwinPlan realScratch profile (Sandbox.darwinParameters realScratch Sandbox.LocalNetworkingDenied) []
              -- The builder says it runs, in the build directory it starts
              -- in, then becomes the writer.
              builder = ["/bin/sh", "-c", ": > " <> buildStartedName <> "; exec /usr/bin/yes " <> T.unpack marker]
              spec = (Proc.proc helper ("run" : builder)) {Proc.cwd = Just scratch, Proc.env = Just [], Proc.std_in = Proc.CreatePipe, Proc.std_out = Proc.CreatePipe, Proc.std_err = Proc.CreatePipe}
              -- Kill what a helper that never ended leaves, all of it
              -- started here, so a failure does not outlive the test.
              -- A process that has ended by now makes kill fail, which
              -- the cleanup takes as done.
              killQuietly target = void (Proc.readProcessWithExitCode "/bin/kill" ["-KILL", target] "")
              killLeft process = do
                Proc.getPid process >>= traverse_ (killQuietly . show)
                left <- Proc.readProcess "ps" ["-axo", "pid=,command="] ""
                traverse_ killQuietly [writer | (writer : command) <- map words (lines left), command == ["/usr/bin/yes", T.unpack marker]]
                void (Proc.waitForProcess process)
              -- Run the helper with its log never read, end the build as
              -- told once its builder runs, and answer whether it started,
              -- the helper ended within the wait, and its builder is gone.
              unread stop = do
                spawned <- Proc.createProcess spec
                case spawned of
                  (Just planIn, Just statusOut, Just logOut, process) -> do
                    BS.hPut planIn (Sandbox.renderDarwinPlan plan) *> hFlush planIn
                    running <- awaitStart scratch (isJust <$> Proc.getProcessExitCode process)
                    started <- (running &&) <$> waitUntilWithin startWaitMicros ((== 1) <$> writers)
                    stop planIn process :: IO ()
                    ended <- timeout helperEndMicros (Proc.waitForProcess process)
                    when (isNothing ended) (killLeft process)
                    gone <- waitUntil ((== 0) <$> writers)
                    -- A pipe whose reader has gone can fail its close.
                    traverse_ (\handle -> void (try (hClose handle) :: IO (Either IOException ()))) [planIn, statusOut, logOut]
                    pure (started, isJust ended, gone)
                  _ -> pure (False, False, False)
          let fresh = Dir.removePathForcibly (scratch </> buildStartedName)
          terminated <- fresh *> unread (\_ process -> Proc.terminateProcess process)
          cutOff <- fresh *> unread (\planIn _ -> hClose planIn)
          pure $
            assertEqual "SIGTERM: started, ended, builder gone" (True, True, True) terminated
              `andThen` assertEqual "lifeline: started, ended, builder gone" (True, True, True) cutOff,
        runTestM "a build cannot execute a setuid program, which would take its process out of the user's listing" $
          withSandboxStore $ \store -> do
            let withPrograms = (configFor store) {bcSandbox = sandbox {Sandbox.scPaths = Sandbox.scPaths sandbox ++ [Sandbox.SandboxPath path path Sandbox.RequiredPath | path <- ["/bin/ls", "/bin/ps"]]}}
                script = "/bin/ls -d / > /dev/null 2>&1; echo \"ls $?\" > $out; /bin/ps -p $$ > /dev/null 2>&1; echo \"ps $?\" >> $out"
            result <- buildDerivation withPrograms store (drvFor 'm' script) testDrvPath
            case result of
              BuildSuccess _ -> assertEqual "exit statuses" "ls 0\nps 126\n" <$> readOutput store 'm'
              BuildFailure msg _ -> pure (Fail msg),
        runTestM "a SIGHUP the helper started out ignoring, as under nohup, leaves the build running" $
          withSandboxStore $ \store -> withHangupIgnored $ do
            let goFile = "\"$NIX_BUILD_TOP/go\""
                script = startedScript <> awaitPathScript goFile <> "; [ -e " <> goFile <> " ] || exit 3; echo ran > $out"
                buildDir = buildDirFor (stDir store) (outputAt 'l')
                -- The builder's own shell, which the helper's command line,
                -- holding the same script, is not.
                builderRunning = any (\command -> "/bin/sh -c " `BS.isPrefixOf` command && "NIX_BUILD_TOP/go" `BS.isInfixOf` command) <$> runningCommands
            withAsync (buildDerivation (configFor store) store (drvFor 'l' script) testDrvPath) $ \build -> do
              started <- awaitStart buildDir (isJust <$> poll build)
              helpers <- helperProcesses
              traverse_ (\pid -> Proc.callProcess "/bin/kill" ["-HUP", pid]) helpers
              ended <- waitUntilWithin signalSettleMicros (not <$> builderRunning)
              unless ended $ BS.writeFile (buildDir </> "go") ""
              result <- timeout startWaitMicros (wait build)
              pure $
                assertEqual "started" True started
                  `andThen` assertEqual "helpers signalled" 1 (length helpers)
                  `andThen` assertEqual "the builder, ended by the signal" False ended
                  `andThen` assertEqual "build" (Just (BuildSuccess (outputAt 'l'))) result,
        runTestM "a build under the shipped macOS sandbox-paths alone runs the system's shells" $
          withSandboxStore $ \store -> do
            let shipped = sandbox {Sandbox.scPaths = Sandbox.parseSandboxPaths Sandbox.darwinSandboxPaths}
            result <- buildDerivation ((configFor store) {bcSandbox = shipped}) store (drvFor 'z' "echo sh > $out; /bin/bash -c 'echo bash' >> $out") testDrvPath
            case result of
              BuildSuccess _ -> assertEqual "seen" "sh\nbash\n" <$> readOutput store 'z'
              BuildFailure msg _ -> pure (Fail msg)
      ]

-- | Linux builds in the sandbox, with @sandbox = true@ and
-- @sandbox-fallback = false@ and a static busybox as the builder: what a
-- build cannot read, write or reach, what it is, and how a failed setup
-- and an interrupt end.  The helper is the one 'sandboxHelperVar' names,
-- which this run can use; with it unset the group skips, and a name that
-- is set but missing fails, since the variable is how a run asserts it.
testLinuxSandboxIO :: IO [Bool]
testLinuxSandboxIO = do
  putStrLn "builder/sandbox-linux"
  named <- (,) <$> lookupEnv sandboxHelperVar <*> lookupEnv sandboxBusyboxVar
  case named of
    (Just helper, Just busybox) -> do
      present <- (&&) <$> Dir.doesFileExist helper <*> Dir.doesFileExist busybox
      if present
        then testLinuxSandboxBody helper busybox
        else (: []) <$> runTest (T.pack sandboxHelperVar <> " and " <> T.pack sandboxBusyboxVar <> " name files") (Fail (T.pack (helper <> ", " <> busybox)))
    _ -> do
      putStrLn ("  SKIP  needs " ++ sandboxHelperVar ++ " and " ++ sandboxBusyboxVar)
      pure []

testLinuxSandboxBody :: FilePath -> FilePath -> IO [Bool]
testLinuxSandboxBody helper busybox = do
  tmpBase <- getTemporaryDirectory
  busyboxBytes <- Spawn.filePathBytes busybox
  submounted <- lookupEnv sandboxSubmountVar
  foreignFile <- lookupEnv foreignFileVar
  searchOnly <- lookupEnv searchOnlyDirVar
  let root = tmpBase </> "nova-nix-test-sandbox-linux"
      storeRoot = root </> "store"
      outside = root </> "outside"
      sandbox = linuxSandboxConfig helper busyboxBytes
      outputAt letter = StorePath (T.replicate 31 "s" <> T.singleton letter) "sandboxed"
      drvFor letter = mkTestBuildDrv "/bin/sh" (outputAt letter)
      configFor store = (defaultBuildConfig (stDir store)) {bcSandbox = sandbox}
      outputFile store letter = storePathToFilePath (stDir store) (outputAt letter)
      withSandboxStore = bracket (openStore testLogger (StoreDir storeRoot)) closeStore
  bracket_
    (forceRemoveIfExists root >> createDirectoryIfMissing True outside >> BS.writeFile (outside </> "secret") "host secret\n")
    (forceRemoveIfExists root)
    ( sequence $
        [ runTestM "a sandboxed build runs as uid 1000 and gid 100, on host localhost, as PID 1, in /build, with upstream's HOME" $
            withSandboxStore $ \store -> do
              result <- buildDerivation (configFor store) store (drvFor '1' "echo \"$(/bin/busybox id -u) $(/bin/busybox id -g) $(/bin/busybox hostname) $$ $(/bin/busybox pwd) $NIX_BUILD_TOP $TMPDIR $HOME\" > $out") testDrvPath
              case result of
                BuildSuccess _ -> assertEqual "seen" "1000 100 localhost 1 /build /build /build /homeless-shelter\n" <$> BS.readFile (outputFile store '1')
                BuildFailure msg _ -> pure (Fail msg),
          runTestM "a sandboxed build cannot read a host path outside its inputs, which an unsandboxed one can" $
            withSandboxStore $ \store -> do
              let script = "/bin/busybox cat " <> T.pack (outside </> "secret") <> " > $out 2>/dev/null || echo refused > $out"
              sandboxed <- buildDerivation (configFor store) store (drvFor '2' script) testDrvPath
              sandboxedSaw <- BS.readFile (outputFile store '2')
              unsandboxed <- buildDerivation ((configFor store) {bcSandbox = sandbox {Sandbox.scMode = Sandbox.SandboxDisabled}}) store (drvFor '3' script) testDrvPath
              unsandboxedSaw <- BS.readFile (outputFile store '3')
              pure $
                assertEqual "sandboxed" (BuildSuccess (outputAt '2')) sandboxed
                  `andThen` assertEqual "what the sandboxed build read" "refused\n" sandboxedSaw
                  `andThen` assertEqual "unsandboxed" (BuildSuccess (outputAt '3')) unsandboxed
                  `andThen` assertEqual "what the unsandboxed build read" "host secret\n" unsandboxedSaw,
          runTestM "the helper runs with no environment, so its loader reads no variable a derivation sets, which reach the builder" $
            withSandboxStore $ \store -> do
              helperBytes <- Spawn.filePathBytes helper
              let logPath = root </> "loader.log"
                  helperLine = helperBytes <> "\0run\0"
                  -- The builder waits for a file in its own directory, so
                  -- nothing it does writes to the log: a child it saw
                  -- killed, its shell would report there.
                  release = buildDirFor (stDir store) (outputAt 'b') </> "build" </> "release"
                  base = drvFor 'b' "while [ ! -e /build/release ]; do /bin/busybox sleep 1; done; echo \"$LD_DEBUG $LD_PRELOAD\" > $out"
                  drv = base {drvEnv = Map.union (Map.fromList [("LD_DEBUG", "libs"), ("LD_PRELOAD", "/nonexistent-nova-nix-preload.so")]) (drvEnv base)}
              (result, environs) <- withBinaryFile logPath WriteMode $ \logHandle ->
                withAsync (buildDerivation (configFor store) (buildLogTo logHandle store) drv testDrvPath) $ \build -> do
                  _ <- waitForProcesses helperLine (not . null)
                  helpers <- processesWith helperLine
                  -- Read while the helper runs, then let the build go on.
                  environs <- traverse (\pid -> BS.readFile ("/proc" </> pid </> "environ")) helpers
                  BS.writeFile release ""
                  (,environs) <$> timeout processWaitMicros (wait build)
              logged <- BS.readFile logPath
              case result of
                Nothing -> pure (Fail "the build did not finish once it was released")
                Just (BuildSuccess _) -> do
                  given <- BS.readFile (outputFile store 'b')
                  pure $
                    assertEqual "the helper's environment" [""] environs
                      `andThen` assertEqual "the loader's output in the log" "" logged
                      `andThen` assertEqual "what the builder was given" "libs /nonexistent-nova-nix-preload.so\n" given
                Just (BuildFailure msg _) -> pure (Fail (msg <> "; log: " <> TE.decodeUtf8Lenient logged)),
          runTestM "a NUL in a derivation's variable ends the entry its builder gets, and the bytes after it bind nothing" $
            withSandboxStore $ \store -> do
              outsideBytes <- Spawn.filePathBytes outside
              -- The shape of a bind of the outside directory at /escape, as
              -- tokens that end at a NUL would read it.
              let smuggled = "kept\0bind\0/escape\0" <> outsideBytes <> "\0-"
                  base = drvFor 'k' "/bin/busybox cat /escape/secret > $out 2>/dev/null || echo \"refused $smuggled\" > $out"
                  drv = base {drvEnv = Map.insert "smuggled" smuggled (drvEnv base)}
              result <- buildDerivation (configFor store) store drv testDrvPath
              case result of
                BuildSuccess _ -> assertEqual "what the builder saw" "refused kept\n" <$> BS.readFile (outputFile store 'k')
                BuildFailure msg _ -> pure (Fail msg),
          runTestM "a sandboxed builder that opens its build directory to everyone leaves the directory around it closed" $
            withSandboxStore $ \store -> do
              let marker = "987648"
                  sleepLine = "sleep\0" <> TE.encodeUtf8 marker
                  outer = buildDirFor (stDir store) (outputAt 'l')
                  script = "/bin/busybox chmod 0755 /build; /bin/busybox sleep " <> marker <> "; echo ran > $out"
              withAsync (buildDerivation (configFor store) store (drvFor 'l' script) testDrvPath) $ \build -> do
                appeared <- waitForProcesses sleepLine (not . null)
                outerMode <- fmap nsPermissions <$> nodeStatus outer
                innerMode <- fmap nsPermissions <$> nodeStatus (outer </> "build")
                sleeps <- processesWith sleepLine
                for_ sleeps $ \pid -> Proc.readProcessWithExitCode "kill" ["-TERM", pid] ""
                finished <- timeout processWaitMicros (wait build)
                pure $
                  assertEqual "started" True appeared
                    `andThen` assertEqual "the build directory" (Just 0o700) outerMode
                    `andThen` assertEqual "the directory the builder sees" (Just 0o755) innerMode
                    `andThen` assertEqual "build" (Just (BuildSuccess (outputAt 'l'))) finished,
          runTestM "a sandboxed output others could write is refused in the chroot, before it reaches the store" $
            withSandboxStore $ \store -> do
              result <- buildDerivation (configFor store) store (drvFor 'm' "/bin/busybox mkdir $out; /bin/busybox chmod 0777 $out") testDrvPath
              let outPath = outputFile store 'm'
                  inChroot = (Spawn.chrootDirFor (buildDirFor (stDir store) (outputAt 'm')) </> "root") <> outPath
              left <- Dir.doesPathExist outPath
              pure $
                assertEqual "result" (BuildFailure ("suspicious ownership or permission on '" <> T.pack inChroot <> "' for output 'out'; rejecting this build output") 1) result
                  `andThen` assertEqual "in the store" False left,
          runTestM "a sandboxed directory output keeps its hard links through its move into the store" $
            withSandboxStore $ \store -> do
              result <- buildDerivation (configFor store) store (drvFor 'n' "/bin/busybox mkdir $out; echo linked > $out/a; /bin/busybox ln $out/a $out/b") testDrvPath
              (_, links, _) <- Proc.readProcessWithExitCode "stat" ["-c", "%h", outputFile store 'n' </> "a"] ""
              pure (assertEqual "build" (BuildSuccess (outputAt 'n')) result `andThen` assertEqual "links" "2\n" links),
          runTestM "a sandboxed output that is a link to a sibling output is an output, though the sibling is still in the chroot" $
            withSandboxStore $ \store -> do
              let base = drvFor 'p' "/bin/busybox mkdir $out; /bin/busybox ln -s $out $dev"
                  devPath = StorePath (T.replicate 31 "s" <> "q") "sandboxed-dev"
                  drv = base {drvOutputs = drvOutputs base ++ [DerivationOutput "dev" devPath "" ""]}
              result <- buildDerivation (configFor store) store drv testDrvPath
              target <- Dir.getSymbolicLinkTarget (storePathToFilePath (stDir store) devPath)
              pure (assertEqual "build" (BuildSuccess (outputAt 'p')) result `andThen` assertEqual "dev" (outputFile store 'p') target),
          runTestM "a sandboxed check reads its rebuild in the chroot, and refuses one others could write there" $
            withSandboxStore $ \store -> do
              let markerDir = root </> "check-marker"
              createDirectoryIfMissing True markerDir
              BS.writeFile (markerDir </> "check") ""
              markerBytes <- Spawn.filePathBytes markerDir
              let drv = drvFor 'r' "/bin/busybox mkdir $out; if [ -e /marker/check ]; then /bin/busybox chmod 0777 $out; fi"
                  checking = sandbox {Sandbox.scPaths = Sandbox.scPaths sandbox ++ [Sandbox.SandboxPath "/marker" markerBytes Sandbox.RequiredPath]}
                  chrootRoot = Spawn.chrootDirFor (buildDirFor (stDir store) (outputAt 'r')) </> "root"
              built <- buildDerivation (configFor store) store drv testDrvPath
              checked <- checkDerivation (configFor store) {bcSandbox = checking} store drv testDrvPath
              let reason = fromLeft "the check passed" checked
              pure $
                assertEqual "build" (BuildSuccess (outputAt 'r')) built
                  `andThen` assertEqual ("refused in the chroot: " <> reason) True (T.pack chrootRoot `T.isInfixOf` reason && "suspicious ownership or permission" `T.isInfixOf` reason),
          -- Upstream keeps the build directory and names the one inside
          -- it that the sandbox shows at /build; the chroot goes either
          -- way.
          runTestM "a sandboxed build that fails under keep-failed keeps its build directory, what the builder left in /build inside it" $
            withSandboxStore $ \store -> do
              let outer = buildDirFor (stDir store) (outputAt 'e')
              result <- buildDerivation (configFor store) {bcFailedBuilds = KeepFailed} store (drvFor 'e' "echo left > /build/left; exit 1") testDrvPath
              left <- BS.readFile (outer </> "build" </> "left")
              chrootLeft <- Dir.doesPathExist (Spawn.chrootDirFor outer)
              pure $
                assertEqual "build" (BuildFailure "builder failed" 1) result
                  `andThen` assertEqual "left" "left\n" left
                  `andThen` assertEqual "the chroot" False chrootLeft,
          -- The rebuild leaves the chroot by a rename of a canonical, so
          -- read-only, directory into the store.
          runTestM "a sandboxed check under keep-failed moves a rebuild that differs out of the chroot to PATH.check" $
            withSandboxStore $ \store -> do
              let writing word = drvFor 'o' ("/bin/busybox mkdir $out; echo " <> word <> " > $out/value")
                  kept = outputFile store 'o' <> ".check"
              built <- buildDerivation (configFor store) store (writing "first") testDrvPath
              checked <- checkDerivation (configFor store) {bcFailedBuilds = KeepFailed} store (writing "second") testDrvPath
              keptValue <- BS.readFile (kept </> "value")
              ownerWrite <- fmap ((.&. 0o200) . nsPermissions) <$> nodeStatus kept
              pure $
                assertEqual "build" (BuildSuccess (outputAt 'o')) built
                  `andThen` assertEqual "check" (Right (Realise.OutputDiffers (outputAt 'o') Realise.RebuildKept)) checked
                  `andThen` assertEqual "the kept rebuild" "second\n" keptValue
                  `andThen` assertEqual "its owner's write bit, cleared as the rebuild was made canonical" (Just 0) ownerWrite,
          runTestM "a sandbox path whose target crosses a link in the chroot is refused, and nothing outside the chroot is made" $
            withSandboxStore $ \store -> do
              let linkSource = root </> "escape-link"
                  escaped = root </> "escape-target"
              createDirectoryIfMissing True escaped
              Dir.removePathForcibly linkSource
              Dir.createDirectoryLink escaped linkSource
              linkBytes <- Spawn.filePathBytes linkSource
              hostBytes <- Spawn.filePathBytes outside
              let crossing = sandbox {Sandbox.scPaths = Sandbox.scPaths sandbox ++ [Sandbox.SandboxPath "/escape" linkBytes Sandbox.RequiredPath, Sandbox.SandboxPath "/escape/inner" hostBytes Sandbox.RequiredPath]}
              result <- buildDerivation ((configFor store) {bcSandbox = crossing}) store (drvFor 's' "echo ran > $out") testDrvPath
              made <- Dir.doesPathExist (escaped </> "inner")
              let refused = case result of
                    BuildFailure msg _ -> "Too many levels of symbolic links" `T.isInfixOf` msg
                    BuildSuccess _ -> False
              pure (assertEqual ("refused: " <> T.pack (show result)) True refused `andThen` assertEqual "made outside the chroot" False made),
          runTestM "an output's extended attributes are removed as it is made canonical" $
            withSandboxStore $ \store -> do
              let unsandboxed = (configFor store) {bcSandbox = sandbox {Sandbox.scMode = Sandbox.SandboxDisabled}}
                  script = "mkdir $out && echo x > $out/f && /usr/bin/python3 -c 'import os, sys; os.setxattr(sys.argv[1], \"user.nova-nix-test\", b\"1\")' $out/f"
              result <- buildDerivation unsandboxed store (drvFor 'v' script) testDrvPath
              (_, listed, _) <- Proc.readProcessWithExitCode "/usr/bin/python3" ["-c", "import os, sys; print(os.listxattr(sys.argv[1]))", outputFile store 'v' </> "f"] ""
              pure (assertEqual "build" (BuildSuccess (outputAt 'v')) result `andThen` assertEqual "attributes" "[]\n" listed),
          runTestM "a sandboxed builder gains no privileges through exec, where the process that started it could" $
            withSandboxStore $ \store -> do
              own <- filter ("NoNewPrivs:" `BS.isPrefixOf`) . BS8.lines <$> BS.readFile "/proc/self/status"
              result <- buildDerivation (configFor store) store (drvFor 'f' "/bin/busybox grep NoNewPrivs: /proc/self/status > $out") testDrvPath
              case result of
                BuildSuccess _ -> do
                  seen <- BS.readFile (outputFile store 'f')
                  pure (assertEqual "this process" ["NoNewPrivs:\t0"] own `andThen` assertEqual "the builder" "NoNewPrivs:\t1\n" seen)
                BuildFailure msg _ -> pure (Fail msg),
          runTestM "a sandboxed build does not inherit the session keyring of the process that starts it, which an unsandboxed one does" $
            -- A keyring belongs to an OS thread's credentials, so the key is
            -- planted, and both builds started, on a bound thread of the
            -- test's own.
            flip withAsyncBound wait $
              withSandboxStore $ \store -> do
                serial <- withCString keyDescription c_nova_test_plant_possessed_key
                if serial < 0
                  then (\(Errno number) -> Fail ("planting the key: errno " <> T.pack (show number))) <$> getErrno
                  else do
                    let unsandboxed = (configFor store) {bcSandbox = sandbox {Sandbox.scMode = Sandbox.SandboxDisabled}}
                        holdsKey = BS.isInfixOf (BS8.pack keyDescription)
                    sandboxedResult <- buildDerivation (configFor store) store (drvFor 'h' "/bin/busybox cat /proc/keys > $out") testDrvPath
                    unsandboxedResult <- buildDerivation unsandboxed store (drvFor 'i' (T.pack busybox <> " cat /proc/keys > $out")) testDrvPath
                    sandboxedKeys <- BS.readFile (outputFile store 'h')
                    unsandboxedKeys <- BS.readFile (outputFile store 'i')
                    pure $
                      assertEqual "sandboxed build" (BuildSuccess (outputAt 'h')) sandboxedResult
                        `andThen` assertEqual "unsandboxed build" (BuildSuccess (outputAt 'i')) unsandboxedResult
                        `andThen` assertEqual "the unsandboxed build sees the key" True (holdsKey unsandboxedKeys)
                        `andThen` assertEqual "the sandboxed build sees the key" False (holdsKey sandboxedKeys),
          runTestM "a helper started with SIGCHLD ignored still waits for its builder and relays its exit" $
            withSandboxStore $ \store -> do
              helperBytes <- Spawn.filePathBytes helper
              let wrapper = root </> "ignoring-sigchld"
              -- trap '' sets SIGCHLD ignored, which exec keeps.
              BS.writeFile wrapper ("#!/bin/sh\ntrap '' CHLD\nexec '" <> helperBytes <> "' \"$@\"\n")
              Dir.getPermissions wrapper >>= Dir.setPermissions wrapper . Dir.setOwnerExecutable True
              let ignoring = sandbox {Sandbox.scHelper = Just wrapper}
              result <- timeout processWaitMicros (buildDerivation (configFor store) {bcSandbox = ignoring} store (drvFor 'g' "echo ran > $out") testDrvPath)
              pure (assertEqual "build" (Just (BuildSuccess (outputAt 'g'))) result),
          runTestM "a sandboxed build cannot change a host file a sandbox-paths entry binds" $
            withSandboxStore $ \store -> do
              ownedBytes <- Spawn.filePathBytes (outside </> "owned")
              BS.writeFile (outside </> "owned") "the user's\n"
              let bound = sandbox {Sandbox.scPaths = Sandbox.scPaths sandbox ++ [Sandbox.SandboxPath "/owned" ownedBytes Sandbox.RequiredPath]}
              result <- buildDerivation ((configFor store) {bcSandbox = bound}) store (drvFor 'c' "echo changed > /owned 2>/dev/null; echo ran > $out") testDrvPath
              contents <- BS.readFile (outside </> "owned")
              pure (assertEqual "build" (BuildSuccess (outputAt 'c')) result `andThen` assertEqual "the host file" "the user's\n" contents),
          runTestM "a sandboxed build cannot write a host path outside its outputs and build directory" $
            withSandboxStore $ \store -> do
              result <- buildDerivation (configFor store) store (drvFor '4' ("echo escaped > " <> T.pack (outside </> "escaped") <> "; echo ran > $out")) testDrvPath
              escaped <- Dir.doesPathExist (outside </> "escaped")
              pure (assertEqual "build" (BuildSuccess (outputAt '4')) result `andThen` assertEqual "escaped" False escaped),
          runTestM "an input a sandboxed builder tries to make writable and change keeps its bytes and mode" $
            withSandboxStore $ \store -> do
              dep <- buildDerivation (configFor store) store (drvFor '5' "/bin/busybox mkdir $out; echo original > $out/file") testDrvPath
              case dep of
                BuildFailure msg _ -> pure (Fail ("dependency: " <> msg))
                BuildSuccess depPath -> do
                  let depFile = storePathToFilePath (stDir store) depPath </> "file"
                  modeBefore <- fmap nsPermissions <$> nodeStatus depFile
                  let script = "/bin/busybox chmod u+w " <> T.pack depFile <> " 2>/dev/null; echo changed >> " <> T.pack depFile <> " 2>/dev/null; echo ran > $out"
                  result <- buildDerivation (configFor store) store ((drvFor '6' script) {drvInputSrcs = [depPath]}) testDrvPath
                  contents <- BS.readFile depFile
                  modeAfter <- fmap nsPermissions <$> nodeStatus depFile
                  pure $
                    assertEqual "build" (BuildSuccess (outputAt '6')) result
                      `andThen` assertEqual "contents" "original\n" contents
                      `andThen` assertEqual "mode" modeBefore modeAfter,
          runTestM "a sandboxed build that is not fixed-output reaches no host server, and a fixed-output one does" $
            withHttpServer (\_ _ -> Send (httpResponse "200 OK" "payload")) $ \base requests _ ->
              withSandboxStore $ \store -> do
                private <- buildDerivation (configFor store) store (drvFor '7' ("/bin/busybox wget -q -O - " <> base <> "/private > $out 2>/dev/null || echo unreachable > $out")) testDrvPath
                privateSeen <- BS.readFile (outputFile store '7')
                beforeFixed <- requests
                let digest = maybe "" Hash.bytesToHexText (Hash.rawHashWithAlgo "sha256" "payload")
                    fixedPath = StorePath (T.replicate 31 "s" <> "8") "fixed"
                fixed <- buildDerivation (configFor store) store (mkFixedOutputDrv "/bin/sh" fixedPath ("/bin/busybox wget -q -O $out " <> base <> "/fixed") "sha256" digest) testDrvPath
                afterFixed <- requests
                pure $
                  assertEqual "private build" (BuildSuccess (outputAt '7')) private
                    `andThen` assertEqual "private output" "unreachable\n" privateSeen
                    `andThen` assertEqual "requests from the private build" [] beforeFixed
                    `andThen` assertEqual "fixed build" (BuildSuccess fixedPath) fixed
                    `andThen` assertEqual "requests from the fixed build" ["/fixed"] afterFixed,
          runTestM "a setup step that fails is reported in upstream's words, and no chroot is left" $
            withSandboxStore $ \store -> do
              let missing = sandbox {Sandbox.scPaths = Sandbox.scPaths sandbox ++ [Sandbox.SandboxPath "/missing" "/nonexistent-nova-nix-test" Sandbox.RequiredPath]}
              result <- buildDerivation ((configFor store) {bcSandbox = missing}) store (drvFor '9' "echo ran > $out") testDrvPath
              left <- Dir.doesPathExist (Spawn.chrootDirFor (buildDirFor (stDir store) (outputAt '9')))
              pure $
                assertEqual "result" (BuildFailure "while setting up the build environment: getting attributes of path '/nonexistent-nova-nix-test': No such file or directory" 1) result
                  `andThen` assertEqual "chroot left" False left,
          runTestM "an interrupt returns once no process of the build, and neither its chroot nor its build directory, is left" $
            withSandboxStore $ \store -> do
              let marker = "987654"
                  -- The sleep's own command line, which only the sleep
                  -- has: the helper's holds the script, which spaces it.
                  sleepLine = "sleep\0" <> TE.encodeUtf8 marker
                  buildDir = buildDirFor (stDir store) (outputAt 'a')
              withAsync (buildDerivation (configFor store) store (drvFor 'a' (sleepingScript marker)) testDrvPath) $ \build -> do
                appeared <- waitForProcesses sleepLine (not . null)
                cancel build
                left <- processesWith sleepLine
                chrootLeft <- Dir.doesPathExist (Spawn.chrootDirFor buildDir)
                buildDirLeft <- Dir.doesPathExist buildDir
                pure $
                  assertEqual "started" True appeared
                    `andThen` assertEqual "processes left" [] left
                    `andThen` assertEqual "chroot left" False chrootLeft
                    `andThen` assertEqual "build directory left" False buildDirLeft,
          runTestM "killing the helper ends a build whose builder keeps its parent-death signal" $
            withSandboxStore $ \store -> do
              helperBytes <- Spawn.filePathBytes helper
              let marker = "987653"
                  sleepLine = "sleep\0" <> TE.encodeUtf8 marker
              withAsync (buildDerivation (configFor store) store (drvFor 'd' (sleepingScript marker)) testDrvPath) $ \build -> do
                appeared <- waitForProcesses sleepLine (not . null)
                helpers <- processesWith (helperBytes <> "\0run\0")
                for_ helpers $ \pid -> Proc.readProcessWithExitCode "kill" ["-KILL", pid] ""
                ended <- waitCatch build
                gone <- waitForProcesses sleepLine null
                pure $
                  assertEqual "started" True appeared
                    `andThen` assertEqual "helpers found" 1 (length helpers)
                    `andThen` assertEqual "the build failed" True (either (const False) (\case BuildFailure _ _ -> True; BuildSuccess _ -> False) ended)
                    `andThen` assertEqual "the build's processes gone" True gone,
          runTestM "killing nova-nix ends the build, whose helper sees its lifeline close" $ do
            self <- getExecutablePath
            let marker = "987652"
                sleepLine = "sleep\0" <> TE.encodeUtf8 marker
            Proc.withCreateProcess (Proc.proc self [sandboxBuildChildFlag, helper, busybox, root </> "child-store", T.unpack marker]) $ \_ _ _ child -> do
              appeared <- waitForProcesses sleepLine (not . null)
              childPid <- Proc.getPid child
              for_ childPid $ \pid -> Proc.readProcessWithExitCode "kill" ["-KILL", show pid] ""
              _ <- Proc.waitForProcess child
              gone <- waitForProcesses sleepLine null
              pure (assertEqual "started" True appeared `andThen` assertEqual "the build's processes gone" True gone)
        ]
          ++ [ runTestM "a sandboxed build cannot write through a mount beneath a host path a sandbox-paths entry binds" $
                 withSandboxStore $ \store -> do
                   dirBytes <- Spawn.filePathBytes dir
                   let bound = sandbox {Sandbox.scPaths = Sandbox.scPaths sandbox ++ [Sandbox.SandboxPath "/bound" dirBytes Sandbox.RequiredPath]}
                       beneath = dir </> "inner" </> "file"
                   before <- BS.readFile beneath
                   result <- buildDerivation ((configFor store) {bcSandbox = bound}) store (drvFor 'j' "echo changed > /bound/inner/file 2>/dev/null; echo ran > $out") testDrvPath
                   after <- BS.readFile beneath
                   pure (assertEqual "build" (BuildSuccess (outputAt 'j')) result `andThen` assertEqual "the file beneath the mount" before after)
             | Just dir <- [submounted]
             ]
          ++ [ runTestM "a sandbox path is bound beneath an earlier bind of a directory its user may search but not read" $
                 withSandboxStore $ \store -> do
                   searchBytes <- Spawn.filePathBytes searchable
                   hostBytes <- Spawn.filePathBytes outside
                   let beneath = sandbox {Sandbox.scPaths = Sandbox.scPaths sandbox ++ [Sandbox.SandboxPath "/searchable" searchBytes Sandbox.RequiredPath, Sandbox.SandboxPath "/searchable/inner" hostBytes Sandbox.RequiredPath]}
                   result <- buildDerivation ((configFor store) {bcSandbox = beneath}) store (drvFor 'w' "/bin/busybox cat /searchable/inner/secret > $out") testDrvPath
                   case result of
                     BuildSuccess _ -> assertEqual "what the builder read" "host secret\n" <$> BS.readFile (outputFile store 'w')
                     BuildFailure msg _ -> pure (Fail msg)
             | Just searchable <- [searchOnly]
             ]
          ++ [ runTestM "an output holding a node another user owns is refused before it is made canonical" $
                 withSandboxStore $ \store -> do
                   let unsandboxed = (configFor store) {bcSandbox = sandbox {Sandbox.scMode = Sandbox.SandboxDisabled}}
                       outPath = outputFile store 't'
                   result <- buildDerivation unsandboxed store (drvFor 't' ("mkdir $out && mv '" <> T.pack rootOwned <> "' $out/")) testDrvPath
                   pure (assertEqual "result" (BuildFailure ("invalid ownership on file '" <> T.pack (outPath </> takeFileName rootOwned) <> "'") 1) result)
             | Just rootOwned <- [foreignFile]
             ]
    )
  where
    keyDescription = "nova-nix-test-keyring"
    sleepingScript marker = "/bin/busybox sleep " <> marker <> " & /bin/busybox sleep " <> marker <> "; echo ran > $out"
    -- Whether, within the wait, the processes whose command line holds
    -- the marker came to satisfy the condition.
    waitForProcesses :: BS.ByteString -> ([FilePath] -> Bool) -> IO Bool
    waitForProcesses marker satisfied = waitUntil (satisfied <$> processesWith marker)
    processesWith marker = do
      pids <- filter (all isDigit) <$> Dir.listDirectory "/proc"
      fmap catMaybes . traverse (holds marker) $ pids
    holds marker pid = do
      line <- try (BS.readFile ("/proc" </> pid </> "cmdline")) :: IO (Either IOException BS.ByteString)
      pure $ case line of
        Right bytes | marker `BS.isInfixOf` bytes -> Just pid
        _ -> Nothing

-- ---------------------------------------------------------------------------
-- Tests: Push (pure narinfo construction + closure computation)
-- ---------------------------------------------------------------------------

-- | How long a test waits for a process it started, or one in a build's
-- PID namespace, to appear, to act or to be gone.
processWaitMicros :: Int
processWaitMicros = 10000000

-- | How often a wait for a condition checks it.
pollMicros :: Int
pollMicros = 100000

-- | Whether a condition came to hold within 'processWaitMicros', checked
-- every 'pollMicros'.
waitUntil :: IO Bool -> IO Bool
waitUntil = waitUntilWithin processWaitMicros

-- | Whether a condition came to hold within the given microseconds,
-- checked every 'pollMicros'.
waitUntilWithin :: Int -> IO Bool -> IO Bool
waitUntilWithin micros condition = go (micros `div` pollMicros)
  where
    go :: Int -> IO Bool
    go 0 = condition
    go remaining = do
      done <- condition
      if done then pure True else threadDelay pollMicros >> go (remaining - 1)

-- | How long a test waits for a build, or a process it starts, to get
-- going or to finish before taking it as never doing so: long enough for a
-- loaded CI runner.  A wait ends the moment what it waits for holds, so
-- the bound costs time only when a test fails.
startWaitMicros :: Int
startWaitMicros = 12 * processWaitMicros

-- | A shell command that waits, checking every 'pollMicros', for a path to
-- exist, for at most 'startWaitMicros'.
awaitPathScript :: T.Text -> T.Text
awaitPathScript path =
  T.concat
    [ "polls=0; while [ ! -e " <> path <> " ] && [ $polls -lt " <> T.pack (show (startWaitMicros `div` pollMicros)) <> " ]; ",
      "do /bin/sleep " <> T.pack (show (fromIntegral pollMicros / 1000000 :: Double)) <> "; polls=$((polls + 1)); done"
    ]

-- | The file a test's build makes in its directory once it runs.
buildStartedName :: FilePath
buildStartedName = "started"

-- | The shell command a test's build starts with, which says it runs
-- ('awaitStart').
startedScript :: T.Text
startedScript = ": > \"$NIX_BUILD_TOP/" <> T.pack buildStartedName <> "\"; "

-- | Whether a build said it runs, by making 'buildStartedName' in the
-- given directory, before the given check found it ended or
-- 'startWaitMicros' ran out.
awaitStart :: FilePath -> IO Bool -> IO Bool
awaitStart directory ended = go (startWaitMicros `div` pollMicros)
  where
    go :: Int -> IO Bool
    go remaining = do
      started <- Dir.doesFileExist (directory </> buildStartedName)
      over <- ended
      if started || over || remaining <= 0 then pure started else threadDelay pollMicros *> go (remaining - 1)

-- | Step 1 of the Windows-stdenv ladder: prove the Builder can run a
-- trivial derivation end-to-end natively - a recipe that writes to @$out@,
-- with the output landing in the store.  No stdenv, no dependencies: just
-- the raw build path (process spawn, output capture, addToStore),
-- exercised on the host platform (@cmd.exe@ on Windows, @\/bin\/sh@ elsewhere).
testTrivialBuildIO :: IO [Bool]
testTrivialBuildIO = do
  putStrLn "builder/trivial-native-build"
  withTempStore $ \store -> do
    let storeDir = stDir store
        outPath = StorePath (T.replicate 31 "0" <> "1") "trivial"
        (builder, args)
          | SI.os == "mingw32" = ("cmd.exe", ["/c", "echo hi>%out%"])
          | otherwise = ("/bin/sh", ["-c", "echo hi > $out"])
        drv =
          Derivation
            { drvOutputs =
                [ DerivationOutput
                    { doName = "out",
                      doPath = outPath,
                      doHashAlgo = "",
                      doHash = ""
                    }
                ],
              drvInputDrvs = Map.empty,
              drvInputSrcs = [],
              drvPlatform = currentPlatform,
              drvBuilder = builder,
              drvArgs = args,
              drvEnv = Map.empty
            }
    let config = defaultBuildConfig storeDir
    result <- buildDerivation config store drv testDrvPath
    case result of
      BuildSuccess sp -> do
        let outFile = storePathToFilePath storeDir sp
        landed <- Dir.doesFileExist outFile
        content <- if landed then TIO.readFile outFile else pure ""
        narInfo <- queryPathInfo (stDB store) sp
        let realNarHash = case narInfo of
              Just info ->
                piNarSize info > 0
                  && T.isPrefixOf "sha256:" (piNarHash info)
                  && T.any (/= '0') (T.drop 7 (piNarHash info))
              Nothing -> False
        sequence
          [ runTest "trivial build succeeds" Pass,
            runTest "trivial build output landed in store" $
              if landed then Pass else Fail ("missing output file: " <> T.pack outFile),
            runTest "trivial build actually ran the command" $
              if "hi" `T.isInfixOf` content
                then Pass
                else Fail ("unexpected output content: " <> T.pack (show content)),
            runTest "trivial build records a real NAR hash + size" $
              if realNarHash
                then Pass
                else Fail ("NAR hash/size not real: " <> T.pack (show narInfo)),
            -- Upstream marks an output it built ultimately trusted; one
            -- that is not fixed has no content address.
            runTest "trivial build's output is ultimately trusted, unsigned and with no content address" $
              assertEqual "trust" (Just (Ultimate, Set.empty, Nothing)) ((\info -> (piUltimate info, piSignatures info, piContentAddress info)) <$> narInfo)
          ]
      BuildFailure msg code ->
        sequence
          [ runTest "trivial build succeeds" $
              Fail ("build failed (exit " <> T.pack (show code) <> "): " <> msg)
          ]

-- | Where a build runs ("Nix.Builder.BuildDir"): in a directory under the
-- store's state directory, named by the first output, that the build
-- created itself, private to the user on POSIX, and removed when the
-- build ends, unless it failed under keep-failed and its builder is not a
-- builtin.  Builds of one derivation into two stores run in two
-- directories.
testBuildDirIO :: IO [Bool]
testBuildDirIO = do
  putStrLn "builder/build-dir"
  shell <- findTestShell
  tmpBase <- getTemporaryDirectory
  let scratch label = tmpBase </> ("nova-nix-test-build-dir-" <> label)
      -- A fresh store, removed when the action ends, however it ends.
      inStore label action =
        bracket_ (forceRemoveIfExists (scratch label)) (forceRemoveIfExists (scratch label)) $
          bracket (openStore testLogger (StoreDir (scratch label))) closeStore action
      outSP = StorePath (T.replicate 31 "b" <> "d") "build-dir"
      -- Spelled out rather than taken from 'buildDirFor', so the test
      -- pins the location instead of echoing it.
      expectedDir store = unStoreDir (stDir store) </> metaDirName </> "builds" </> T.unpack (spHash outSP)
      -- Records where the builder ran, what its directory held when it
      -- started, and the directory's own line of a long listing.
      probe =
        mkTestBuildDrv shell outSP $
          T.unwords
            [ "mkdir \"$out\" &&",
              "printf '%s' \"$NIX_BUILD_TOP\" > \"$out/top\" &&",
              "ls -A . > \"$out/entries\" &&",
              "ls -ld . > \"$out/mode\""
            ]
      recorded store name = BS.readFile (storePathToFilePath (stDir store) outSP </> name)
      built result check = case result of
        BuildSuccess _ -> check
        BuildFailure msg code -> pure (Fail ("build failed (" <> T.pack (show code) <> "): " <> msg))
      -- msys reports a mode Windows does not have, so only POSIX checks it.
      privateOnPosix listing
        | SI.os == "mingw32" = Pass
        | otherwise = assertEqual "mode" "drwx------" (BS.take 10 listing)
      refused :: Text -> Either IOException () -> TestResult
      refused label outcome = case outcome of
        Left err
          | isAlreadyExistsError err -> Pass
          | otherwise -> Fail (label <> ": expected an already-exists error, got: " <> T.pack (show err))
        Right () -> Fail (label <> ": created a directory where one exists")
      keeping store = (defaultBuildConfig (stDir store)) {bcFailedBuilds = KeepFailed}
      -- Leaves a file where it works, then fails.
      failing = mkTestBuildDrv shell outSP "echo left > left; exit 1"
      leftIn dir = do
        present <- Dir.doesFileExist (dir </> "left")
        if present then Just <$> BS.readFile (dir </> "left") else pure Nothing
      modeOnPosix expected dir
        | SI.os == "mingw32" = pure Pass
        | otherwise = assertEqual "mode" (Just expected) . fmap nsPermissions <$> nodeStatus dir
      isFailure result = case result of
        BuildFailure _ _ -> True
        BuildSuccess _ -> False
  sequence
    [ -- Upstream keeps the directory and names it; the mode stays the
      -- build's, the builder having run as the user who asked.
      runTestM "a build that fails under keep-failed keeps its directory, with what its builder left there, mode 0700 on POSIX" $
        inStore "keep" $ \store -> do
          result <- buildDerivation (keeping store) store failing testDrvPath
          left <- leftIn (expectedDir store)
          mode <- modeOnPosix 0o700 (expectedDir store)
          pure (assertEqual "build" (BuildFailure "builder failed" 1) result `andThen` assertEqual "left" (Just "left\n") left `andThen` mode),
      runTestM "a build that succeeds under keep-failed removes its directory" $
        inStore "keep-success" $ \store -> do
          result <- buildDerivation (keeping store) store probe testDrvPath
          built result (assertEqual "the directory after the build" False <$> Dir.doesPathExist (expectedDir store)),
      -- A builtin may hold privileged files, so upstream keeps nothing of
      -- one; a builder named builtin: that nova-nix does not run in
      -- process is one too.
      runTestM "a builtin's build that fails under keep-failed keeps nothing" $
        inStore "keep-builtin" $ \store -> do
          let named builder = failing {drvBuilder = builder, drvArgs = []}
          inProcess <- buildDerivation (keeping store) store (named "builtin:buildenv") testDrvPath
          inProcessLeft <- Dir.doesPathExist (expectedDir store)
          unknown <- buildDerivation (keeping store) store (named "builtin:nova-nix-test-unknown") testDrvPath
          unknownLeft <- Dir.doesPathExist (expectedDir store)
          pure $
            assertEqual "the builtin failed" True (isFailure inProcess)
              `andThen` assertEqual "its directory" False inProcessLeft
              `andThen` assertEqual "the unknown builtin failed" True (isFailure unknown)
              `andThen` assertEqual "its directory" False unknownLeft,
      -- The name is the derivation's, so the next build of it starts where
      -- the kept directory is.
      runTestM "a directory a failed build kept is removed by the next build of the derivation" $
        inStore "keep-next" $ \store -> do
          _ <- buildDerivation (keeping store) store failing testDrvPath
          kept <- leftIn (expectedDir store)
          result <- buildDerivation (defaultBuildConfig (stDir store)) store probe testDrvPath
          built result $ do
            entries <- recorded store "entries"
            pure (assertEqual "kept" (Just "left\n") kept `andThen` assertEqual "entries when the next builder started" "" entries),
      runTestM "a build runs in a directory under the store's state directory, removed when it ends" $
        inStore "location" $ \store -> do
          result <- buildDerivation (defaultBuildConfig (stDir store)) store probe testDrvPath
          built result $ do
            top <- recorded store "top"
            remains <- Dir.doesPathExist (expectedDir store)
            pure $
              assertEqual "NIX_BUILD_TOP" (BS8.pack (expectedDir store)) top
                `andThen` assertEqual "the directory after the build" False remains,
      -- What a killed build leaves: a directory of the default mode,
      -- holding a tree sealed read-only, as one copied out of the store
      -- is.  The build removes it and runs in a directory of its own.
      runTestM "a build removes what an interrupted build left and runs in a directory it made, mode 0700 on POSIX" $
        inStore "fresh" $ \store -> do
          let sealed = expectedDir store </> "sealed"
          createDirectoryIfMissing True sealed
          BS.writeFile (sealed </> "file") "stale"
          canonicalizeMetadata sealed
          result <- buildDerivation (defaultBuildConfig (stDir store)) store probe testDrvPath
          built result $ do
            entries <- recorded store "entries"
            listing <- recorded store "mode"
            pure (assertEqual "entries when the builder started" "" entries `andThen` privateOnPosix listing),
      -- The window between removing a leftover and creating the directory
      -- is where another writer could put one there first; the creation
      -- fails then rather than taking it over.  A dangling symlink is
      -- refused without the directory being made where it points.
      runTestM "a build directory is never created where something already answers to the name" $ do
        let dir = scratch "exclusive"
            occupied = dir </> "occupied"
            target = dir </> "elsewhere"
        bracket_ (forceRemoveIfExists dir) (forceRemoveIfExists dir) $ do
          createDirectoryIfMissing True occupied
          BS.writeFile (occupied </> "theirs") "theirs"
          overDirectory <- try (createBuildDir occupied)
          theirs <- BS.readFile (occupied </> "theirs")
          overLink <-
            if SI.os == "mingw32"
              then pure Pass
              else do
                Dir.createFileLink target (dir </> "link")
                outcome <- try (createBuildDir (dir </> "link"))
                followed <- Dir.doesPathExist target
                pure (refused "over a dangling symlink" outcome `andThen` assertEqual "the link's target" False followed)
          pure $
            refused "over a directory" overDirectory
              `andThen` assertEqual "the directory's contents" "theirs" theirs
              `andThen` overLink,
      runTestM "builds of one derivation into two stores run in two directories" $
        inStore "store-a" $ \storeA ->
          inStore "store-b" $ \storeB -> do
            resultA <- buildDerivation (defaultBuildConfig (stDir storeA)) storeA probe testDrvPath
            resultB <- buildDerivation (defaultBuildConfig (stDir storeB)) storeB probe testDrvPath
            built resultA . built resultB $ do
              topA <- recorded storeA "top"
              topB <- recorded storeB "top"
              pure $
                assertEqual "store a's" (BS8.pack (expectedDir storeA)) topA
                  `andThen` assertEqual "store b's" (BS8.pack (expectedDir storeB)) topB
                  `andThen` assertEqual "one directory for both" False (topA == topB),
      -- The builder's output is where its log goes, so an empty log is a
      -- builder that never ran.
      runTestM "a derivation with no output fails before a build directory is made or its builder runs" $
        inStore "no-output" $ \store -> do
          let logPath = unStoreDir (stDir store) </> "build.log"
              drv = (mkTestBuildDrv shell outSP "echo ran") {drvOutputs = []}
          result <-
            withBinaryFile logPath WriteMode $ \logHandle ->
              buildDerivation (defaultBuildConfig (stDir store)) (buildLogTo logHandle store) drv testDrvPath
          logged <- BS.readFile logPath
          made <- Dir.doesPathExist (unStoreDir (stDir store) </> metaDirName </> "builds")
          pure $
            assertEqual "result" (BuildFailure "no outputs defined" 1) result
              `andThen` assertEqual "the builder's log" "" logged
              `andThen` assertEqual "a directory for builds" False made,
      runTestM "a build directory holding a read-only tree is removed when the build ends" $
        inStore "read-only" $ \store -> do
          let drv = mkTestBuildDrv shell outSP "mkdir \"$out\" sealed && echo x > sealed/file && chmod 555 sealed"
          result <- buildDerivation (defaultBuildConfig (stDir store)) store drv testDrvPath
          built result $ do
            remains <- Dir.doesPathExist (expectedDir store)
            pure (assertEqual "the directory after the build" False remains)
    ]

-- | A builder's standard output and standard error reach the build log
-- together, a line at a time, as upstream's do: one channel for both
-- (local-derivation-goal.cc and child.cc at 2.24.9) and upstream's line
-- discipline over it (@DerivationGoal::handleChildOutput@,
-- derivation-goal.cc).
testBuildLogIO :: IO [Bool]
testBuildLogIO = do
  putStrLn "builder/build-log"
  shell <- findTestShell
  tmpBase <- getTemporaryDirectory
  let outPath = StorePath (T.replicate 31 "0" <> "2") "build-log"
      tmpStore = tmpBase </> "nova-nix-test-build-log-store"
      logFile = tmpBase </> "nova-nix-test-build-log.txt"
      daemonPidFile = tmpBase </> "nova-nix-test-build-log-daemon.pid"
      -- Build in a fresh store with the log a file opened in the given
      -- mode, and return the result with the file's bytes.
      buildLogged mode drv = do
        forceRemoveIfExists tmpStore
        BS.writeFile logFile ""
        ( do
            result <-
              bracket (openStore testLogger (StoreDir tmpStore)) closeStore $ \store ->
                withBinaryFile logFile mode $ \logHandle ->
                  buildDerivation (defaultBuildConfig (stDir store)) (buildLogTo logHandle store) drv testDrvPath
            logged <- BS.readFile logFile
            pure (result, logged)
          )
          `finally` (forceRemoveIfExists tmpStore *> Dir.removePathForcibly logFile)
      -- Whether or not the build waited for it.
      stopDaemon = do
        recorded <- Dir.doesFileExist daemonPidFile
        when recorded $ do
          pid <- BS.readFile daemonPidFile
          void (Proc.readProcessWithExitCode "kill" [BS8.unpack (BS8.strip pid)] "")
          Dir.removeFile daemonPidFile
      -- The host's own shell, so on Windows the lines come from a native
      -- program, which ends each one in CRLF.
      (nativeBuilder, nativeArgs)
        | SI.os == "mingw32" = ("cmd.exe", ["/c", "echo one&echo two>&2&echo three&echo done>%out%"])
        | otherwise = ("/bin/sh", ["-c", "echo one; echo two >&2; echo three; echo done > $out"])
      nativeDrv = (mkTestBuildDrv shell outPath "") {drvBuilder = nativeBuilder, drvArgs = nativeArgs}
      failingDrv =
        mkTestBuildDrv
          shell
          outPath
          "printf 'out-line\\n'; printf 'err-line\\r\\n' >&2; printf '10%%\\r20%%\\n'; printf 'tail'; exit 3"
      -- Records the daemon's PID, so the test can stop it.
      daemonDrv =
        mkTestBuildDrv
          shell
          outPath
          ( "sleep "
              <> T.pack (show daemonLifetimeSeconds)
              <> " >/dev/null 2>&1 & echo $! > '"
              <> T.pack daemonPidFile
              <> "'; mkdir -p $out"
          )
      -- Colour, a tab, a screen clear and a bell.
      escapesDrv =
        mkTestBuildDrv
          shell
          outPath
          "printf 'a\\033[31mred\\033[0m\\tb\\033[2Jc\\a\\n'; mkdir -p $out"
  sequence
    [ runTest "a line feed ends a build-log line" $
        assertEqual "lines" ["one", "", "two"] (logLinesOf ["one\n\ntwo\n"]),
      runTest "a CRLF ends a build-log line without its CR" $
        assertEqual "lines" ["one", "two"] (logLinesOf ["one\r\ntwo\r\n"]),
      runTest "a carriage return redraws the build-log line from its start" $
        assertEqual "lines" ["30%", "xycdef"] (logLinesOf ["10%\r20%\r30%\n", "abcdef\rxy\n"]),
      runTest "an unterminated last build-log line is flushed when output ends" $
        assertEqual "lines" ["done now"] (logLinesOf ["done", " now"]),
      runTest "output ending in a bare carriage return adds no build-log line" $
        assertEqual "lines" [] (logLinesOf ["\r"]),
      runTestM "build-log lines are upstream's, however the output is split into reads" $ do
        result <-
          QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} $
            QC.forAll genLogChunks $ \chunks ->
              map BS.unpack (logLinesOf chunks) QC.=== upstreamLogLines (concatMap BS.unpack chunks)
        pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result))),
      runTest "a terminal's build log keeps colour escapes and drops the rest" $
        assertEqual "line" "\ESC[1;31mred\ESC[0mx0;t" (filterLogLine KeepColour "\ESC[1;31mred\ESC[0m\ESC[2J\ESC[Hx\ESC]0;t\a"),
      runTest "a build log that is not a terminal drops every escape" $
        assertEqual "line" "redx0;t" (filterLogLine DropEscapes "\ESC[1;31mred\ESC[0m\ESC[2J\ESC[Hx\ESC]0;t\a"),
      runTest "a tab in a build-log line expands to the next tab stop, a UTF-8 character one column" $
        assertEqual
          "line"
          ("a" <> BS8.replicate 7 ' ' <> "b" <> BS8.replicate 15 ' ' <> "c\xc3\xa9" <> BS8.replicate 6 ' ' <> "d")
          (filterLogLine DropEscapes "a\tb\t\tc\xc3\xa9\td"),
      runTestM "build-log lines are filtered as upstream's logger filters them" $ do
        result <-
          QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} $
            QC.forAll genEscapeFilter $ \escapes ->
              QC.forAll (QC.listOf genEscapeByte) $ \bytes ->
                BS.unpack (filterLogLine escapes (BS.pack bytes)) QC.=== upstreamFilterEscapes escapes bytes
        pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result))),
      runTestM "a builder's escapes reach a build log that is not a terminal filtered" $ do
        (result, logged) <- buildLogged WriteMode escapesDrv
        pure $ case result of
          BuildSuccess _ -> assertEqual "log" "ared    bc\n" logged
          BuildFailure msg code -> Fail ("build failed (exit " <> T.pack (show code) <> "): " <> msg),
      runTestM "a builder's stdout and stderr reach the build log in the order written" $ do
        (result, logged) <- buildLogged WriteMode nativeDrv
        pure $ case result of
          BuildSuccess _ -> assertEqual "log" "one\ntwo\nthree\n" logged
          BuildFailure msg code -> Fail ("build failed (exit " <> T.pack (show code) <> "): " <> msg),
      runTestM "a failed builder's output is in the build log, not its failure" $ do
        (result, logged) <- buildLogged WriteMode failingDrv
        pure $ case result of
          BuildFailure msg code ->
            assertEqual "failure and log" ("builder failed", 3, "out-line\nerr-line\n20%\ntail\n") (msg, code, logged)
          BuildSuccess _ -> Fail "expected the builder's exit 3 to fail the build",
      -- The test holds the output's lock, lets the build go once the
      -- store's logger has its wait, and then holds each line the builder
      -- wrote, in order, with nothing else.
      runTestM "a build's wait on a held lock and its lines reach the store's logger" $ do
        forceRemoveIfExists tmpStore
        reports <- newIORef []
        firstMessage <- newEmptyMVar
        let record report = atomicModifyIORef' reports (\earlier -> (report : earlier, ()))
            capturing =
              Logger
                { logMessage = \message -> record (ReportedMessage message) *> void (tryPutMVar firstMessage ()),
                  logBuildLine = record . ReportedLine
                }
            contend store =
              bracket (acquirePathLock testLogger (stDir store) outPath) releasePathLock $ \holder ->
                withAsync (buildDerivation (defaultBuildConfig (stDir store)) store nativeDrv testDrvPath) $ \build -> do
                  announced <- timeout processWaitMicros (takeMVar firstMessage)
                  releasePathLock holder
                  result <- wait build
                  pure (announced, result, pathLockFilePath (stDir store) outPath)
        (announced, result, lockFile) <-
          bracket (openStore capturing (StoreDir tmpStore)) closeStore contend `finally` forceRemoveIfExists tmpStore
        reported <- reverse <$> readIORef reports
        pure $ case result of
          BuildSuccess _ ->
            assertEqual "announced" (Just ()) announced
              `andThen` assertEqual
                "reports"
                [ReportedMessage (waitingForLockMessage lockFile), ReportedLine "one", ReportedLine "two", ReportedLine "three"]
                reported
          BuildFailure msg code -> Fail ("build failed (exit " <> T.pack (show code) <> "): " <> msg),
      -- A handle open only for reading refuses every write, as a standard
      -- error whose reader has gone does.
      runTestM "a build log that refuses its lines does not fail the build" $ do
        (result, _) <- buildLogged ReadMode nativeDrv
        pure $ case result of
          BuildSuccess _ -> Pass
          BuildFailure msg code -> Fail ("build failed (exit " <> T.pack (show code) <> "): " <> msg),
      -- The daemon outlives the deadline, so the build times out if it
      -- waits for the daemon, which it would if the builder inherited the
      -- pipe beside its standard output and standard error.
      runTestM "a daemon that closes stdout and stderr lets the build finish" $
        if SI.os == "mingw32"
          then Pass <$ putStrLn "  SKIP  a Windows build waits for its whole job, a daemon included"
          else do
            outcome <- timeout daemonDeadlineMicros (buildLogged WriteMode daemonDrv) `finally` stopDaemon
            pure $ case outcome of
              Nothing -> Fail "the build waited for the daemon to exit"
              Just (BuildSuccess _, _) -> Pass
              Just (BuildFailure msg code, _) -> Fail ("build failed (exit " <> T.pack (show code) <> "): " <> msg)
    ]
  where
    daemonLifetimeSeconds = 60 :: Int
    daemonDeadlineMicros = 10 * 1000 * 1000
    genLogChunks = QC.listOf (BS.pack <$> QC.listOf genLogByte)
    genLogByte = QC.frequency [(3, QC.elements [logLineFeed, logCarriageReturn]), (5, QC.elements [0x61, 0x62, 0x63]), (1, QC.arbitrary)]
    genEscapeFilter = QC.elements [KeepColour, DropEscapes]
    -- Weighted to the bytes escape sequences, tabs and UTF-8 are read by.
    genEscapeByte =
      QC.frequency
        [ (4, QC.elements [logEscape, logCsi, logSgrFinal, 0x3B, 0x31, 0x4A, 0x5D, 0x20, 0x2F]),
          (2, QC.elements [logTab, logBell, logCarriageReturn]),
          (2, QC.elements [0xC3, 0xE2, 0xF0, 0x80, 0xA9, 0xFF]),
          (3, QC.elements [0x61, 0x62]),
          (1, QC.arbitrary)
        ]

-- | The store, its builds writing their lines to a handle, as a build log
-- with every escape dropped, and its other reports going where they went.
buildLogTo :: Handle -> Store -> Store
buildLogTo handle store = store {stLogger = (stLogger store) {logBuildLine = logBuildLine (handleLogger DropEscapes handle)}}

-- | What a build told its store's logger.
data Reported
  = -- | A message.
    ReportedMessage !Text
  | -- | A line its builder wrote.
    ReportedLine !BS.ByteString
  deriving (Eq, Show)

-- | The lines a builder's output makes when it is read in the chunks
-- given.
logLinesOf :: [BS.ByteString] -> [BS.ByteString]
logLinesOf chunks =
  let (pending, completed) = mapAccumL (\partial chunk -> swap (feedBuildLog partial chunk)) emptyPendingLine chunks
   in concat completed <> maybeToList (endBuildLog pending)

-- | Upstream's build-log line discipline, transcribed byte for byte from
-- @DerivationGoal::handleChildOutput@ and @handleEOF@ (derivation-goal.cc
-- at 2.24.9): the oracle the builder's line splitting is held to.
upstreamLogLines :: [Word8] -> [[Word8]]
upstreamLogLines = go [] 0
  where
    go line _ [] = [line | not (null line)]
    go line pos (byte : rest)
      | byte == logCarriageReturn = go line 0 rest
      | byte == logLineFeed = line : go [] 0 rest
      | otherwise = go (take pos line <> [byte] <> drop (pos + 1) line) (pos + 1) rest

-- | Upstream's terminal filter, transcribed from @filterANSIEscapes@
-- (terminal.cc at 2.24.9) at the unlimited width its logger passes: the
-- oracle the build log's filter is held to.
upstreamFilterEscapes :: EscapeFilter -> [Word8] -> [Word8]
upstreamFilterEscapes escapes = go 0
  where
    filterAll = escapes == DropEscapes
    go :: Int -> [Word8] -> [Word8]
    go _ [] = []
    go w (byte : rest)
      | byte == logEscape =
          let (escapeBytes, final, after) = escape rest
           in (if not filterAll && final == Just logSgrFinal then byte : escapeBytes else []) <> go w after
      | byte == logTab =
          let spaces = 1 + (8 - (w + 1) `mod` 8) `mod` 8
           in replicate spaces 0x20 <> go (w + spaces) rest
      | byte == logCarriageReturn || byte == logBell = go w rest
      | otherwise =
          let (character, after) = utf8Character byte rest
           in character <> go (w + 1) after
    escape (next : rest)
      | next == logCsi =
          let (parameters, afterParameters) = span (\b -> b >= 0x30 && b <= 0x3F) rest
              (intermediates, afterIntermediates) = span (\b -> b >= 0x20 && b <= 0x2F) afterParameters
           in case afterIntermediates of
                final : after
                  | final >= 0x40 && final <= 0x7E -> (next : parameters <> intermediates <> [final], Just final, after)
                _ -> (next : parameters <> intermediates, Nothing, afterIntermediates)
      | next >= 0x40 && next <= 0x5F = ([next], Nothing, rest)
    escape rest = ([], Nothing, rest)
    utf8Character lead rest
      | lead .&. 0xE0 == 0xC0 = continued 1
      | lead .&. 0xF0 == 0xE0 = continued 2
      | lead .&. 0xF8 == 0xF0 = continued 3
      | otherwise = ([lead], rest)
      where
        continued count =
          let taken = takeWhile (\b -> b .&. 0xC0 == 0x80) (take count rest)
           in (lead : taken, drop (length taken) rest)

-- | The control bytes the build-log tests write and read: line feed,
-- carriage return, escape, tab, bell, and the @[@ and @m@ a colour
-- sequence opens and ends with.
logLineFeed, logCarriageReturn, logEscape, logTab, logBell, logCsi, logSgrFinal :: Word8
logLineFeed = 0x0A
logCarriageReturn = 0x0D
logEscape = 0x1B
logTab = 0x09
logBell = 0x07
logCsi = 0x5B
logSgrFinal = 0x6D

-- | The builder hands every build a fixed SOURCE_DATE_EPOCH (1980-01-01),
-- the reproducible-builds.org convention that determinism-aware tools
-- (ld's PE timestamp, gcc's __DATE__) honor.  Proven behaviorally: a build
-- that echoes the variable into $out must see the pinned value.
testSourceDateEpochIO :: IO [Bool]
testSourceDateEpochIO = do
  putStrLn "builder/source-date-epoch"
  withTempStore $ \store -> do
    let storeDir = stDir store
        outPath = StorePath (T.replicate 31 "0" <> "5") "sde-probe"
        (builder, args)
          | SI.os == "mingw32" = ("cmd.exe", ["/c", "echo %SOURCE_DATE_EPOCH%>%out%"])
          | otherwise = ("/bin/sh", ["-c", "echo $SOURCE_DATE_EPOCH > $out"])
        drv =
          Derivation
            { drvOutputs =
                [ DerivationOutput
                    { doName = "out",
                      doPath = outPath,
                      doHashAlgo = "",
                      doHash = ""
                    }
                ],
              drvInputDrvs = Map.empty,
              drvInputSrcs = [],
              drvPlatform = currentPlatform,
              drvBuilder = builder,
              drvArgs = args,
              drvEnv = Map.empty
            }
    let config = defaultBuildConfig storeDir
    result <- buildDerivation config store drv testDrvPath
    case result of
      BuildFailure msg code ->
        sequence
          [ runTest "SOURCE_DATE_EPOCH build succeeds" $
              Fail ("build failed (exit " <> T.pack (show code) <> "): " <> msg)
          ]
      BuildSuccess sp -> do
        content <- TIO.readFile (storePathToFilePath storeDir sp)
        sequence
          [ runTest "builder pins SOURCE_DATE_EPOCH to 1980-01-01" $
              if "315532800" `T.isInfixOf` content
                then Pass
                else Fail ("unexpected content: " <> T.pack (show content))
          ]

-- | A builder is told the configured cores in NIX_BUILD_CORES, and a
-- derivation attribute of that name wins over the setting, as upstream's
-- initEnv sets the variable before it copies the derivation's environment.
-- Proven behaviorally: each build echoes the variable into $out.
testBuildCoresIO :: IO [Bool]
testBuildCoresIO = do
  putStrLn "builder/nix-build-cores"
  withTempStore $ \store -> do
    let storeDir = stDir store
        -- cmd reads a digit just before ">" as a handle to redirect, so
        -- the redirection comes first.
        (builder, args)
          | SI.os == "mingw32" = ("cmd.exe", ["/c", ">%out% echo %NIX_BUILD_CORES%"])
          | otherwise = ("/bin/sh", ["-c", "echo $NIX_BUILD_CORES > $out"])
        probe hashTail name env =
          Derivation
            { drvOutputs =
                [ DerivationOutput
                    { doName = "out",
                      doPath = StorePath (T.replicate 31 "0" <> hashTail) name,
                      doHashAlgo = "",
                      doHash = ""
                    }
                ],
              drvInputDrvs = Map.empty,
              drvInputSrcs = [],
              drvPlatform = currentPlatform,
              drvBuilder = builder,
              drvArgs = args,
              drvEnv = env
            }
    let config = (defaultBuildConfig storeDir) {bcCores = 7}
        echoed drv = do
          result <- buildDerivation config store drv testDrvPath
          case result of
            BuildFailure msg code -> pure (Left ("build failed (exit " <> T.pack (show code) <> "): " <> msg))
            BuildSuccess sp -> Right . T.strip <$> TIO.readFile (storePathToFilePath storeDir sp)
    configured <- echoed (probe "6" "cores-probe" Map.empty)
    attribute <- echoed (probe "7" "cores-attr-probe" (Map.singleton "NIX_BUILD_CORES" "3"))
    sequence
      [ runTest "a builder's NIX_BUILD_CORES is the configured cores" $
          assertEqual "configured" (Right "7") configured,
        runTest "a derivation's own NIX_BUILD_CORES wins over the setting" $
          assertEqual "attribute" (Right "3") attribute
      ]

-- | Upstream's default for cores, piece by piece: the three kernel files
-- as getMaxCPU and glibc's getmntent read them, the reads chained as
-- getMaxCPU chains them over an in-memory map, and getDefaultCores's
-- choice.
testBuildCores :: IO [Bool]
testBuildCores = do
  putStrLn "builder/cores"
  let mountLine dir = "cgroup2 " <> dir <> " cgroup2 rw,nosuid,nodev,noexec,relatime 0 0\n"
      -- How the kernel writes a mount point (seq_path_root in
      -- show_vfsmnt, fs/proc_namespace.c): each of space, tab, newline
      -- and backslash as a backslash and three octal digits.
      kernelEscape = BS.concatMap (\byte -> if byte `BS.elem` " \t\n\\" then octalEscape byte else BS.singleton byte)
      octalEscape byte = BS8.pack ('\\' : [intToDigit (fromIntegral ((byte `shiftR` bits) .&. 7)) | bits <- [6, 3, 0]])
      -- Mount points weighted toward the bytes an escape is made of, so
      -- a literal backslash before octal digits turns up.
      genMountPoint = BS.pack <$> QC.resize 48 (QC.listOf1 (QC.frequency [(3, QC.choose (1, 255)), (2, QC.elements (BS.unpack " \t\n\\0123467#"))]))
      property prop = do
        result <- QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} prop
        pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))
      -- A long device name pushes the type toward the end of getmntent's
      -- 4095-byte line.
      mountAfter deviceLength = Cores.cgroup2MountPoint (BS.replicate deviceLength 0x61 <> " /m cgroup2 rw 0 0\n")
      unifiedGroup = "0::/user.slice/build.scope\n"
      cpuMaxPath = "/sys/fs/cgroup//user.slice/build.scope/cpu.max"
      machine =
        Map.fromList
          [ ("/proc/mounts", "proc /proc proc rw 0 0\n" <> mountLine "/sys/fs/cgroup"),
            ("/proc/self/cgroup", unifiedGroup),
            (cpuMaxPath, "300000 100000\n")
          ]
      limitOver files = runIdentity (Cores.cgroupCoreLimit (\path -> Identity (Map.lookup path (Map.mapKeys asciiOsPath files))))
  sequence
    [ -- /proc/mounts, as getmntent reads it.
      runTest "the cgroup2 mount point is found among /proc/mounts' lines" $
        assertEqual "mount" (Just "/sys/fs/cgroup") (Cores.cgroup2MountPoint ("proc /proc proc rw 0 0\ncgroup /sys/fs/cgroup/cpu cgroup rw,cpu 0 0\n" <> mountLine "/sys/fs/cgroup")),
      runTest "the first of two cgroup2 mounts is the one" $
        assertEqual "first" (Just "/a") (Cores.cgroup2MountPoint (mountLine "/a" <> mountLine "/b")),
      runTest "no cgroup2 mount is none, a cgroup v1 mount included" $
        assertEqual "none" Nothing (Cores.cgroup2MountPoint "cgroup /sys/fs/cgroup/cpu cgroup rw,cpu 0 0\nproc /proc proc rw 0 0\n"),
      runTest "a mount point's escapes are decoded as glibc decodes them" $
        assertEqual "escapes" (Just "/a b\tc\nd\\e\\f") (Cores.cgroup2MountPoint (mountLine "/a\\040b\\011c\\012d\\134e\\\\f")),
      runTest "an escape glibc does not decode stays as written" $
        assertEqual "unknown-escape" (Just "/a\\043b\\0") (Cores.cgroup2MountPoint (mountLine "/a\\043b\\0")),
      runTest "comment and blank lines are skipped, and fields split at runs of spaces and tabs" $
        assertEqual "lines" (Just "/real") (Cores.cgroup2MountPoint " \t# c /commented cgroup2 rw 0 0\n\n \t\n\t cgroup2 \t /real\t\tcgroup2  rw 0 0\n"),
      runTest "a line is read only as far as the 4095 bytes getmntent holds" $
        assertEqual "line-limit" (Just "/m", Nothing) (mountAfter 4084, mountAfter 4085),
      runTestM "decoding inverts the kernel's escaping of a mount point" $
        property $
          QC.forAll genMountPoint $
            \dir -> Cores.cgroup2MountPoint (mountLine (kernelEscape dir)) QC.=== Just dir,
      -- /proc/self/cgroup, as getCgroups reads it.
      runTest "a cgroup v2 line is the hierarchy with no name" $
        assertEqual "v2" (Just (Map.singleton "" "/user.slice/build.scope")) (Cores.parseCgroups unifiedGroup),
      runTest "each line is keyed by its controllers, a name= prefix dropped" $
        assertEqual
          "hybrid"
          (Just (Map.fromList [("cpu,cpuacct", "/a"), ("systemd", "/b"), ("", "/c")]))
          (Cores.parseCgroups "4:cpu,cpuacct:/a\n1:name=systemd:/b\n0::/c\n"),
      runTest "a later line replaces an earlier one of the same name" $
        assertEqual "later" (Just (Map.singleton "" "/second")) (Cores.parseCgroups "0::/first\n1:name=:/second\n"),
      runTest "blank lines are skipped, and a path may hold a colon" $
        assertEqual "colon" (Just (Map.singleton "" "/a:b")) (Cores.parseCgroups "\n0::/a:b\n\n"),
      runTest "a line the regex does not match makes the whole file unread" $
        assertEqual
          "malformed"
          (replicate 5 Nothing)
          (map Cores.parseCgroups ["0::/ok\n0:/one-colon\n", "x::/letter\n", "::/no-id\n", "1a::/mixed\n", "0::/carriage\r\n"]),
      -- cpu.max, as getMaxCPU reads it.
      runTest "a cpu.max quota of max is no limit" $
        assertEqual "max" Nothing (Cores.cpuMaxCores "max 100000\n"),
      runTest "a quota is its periods rounded up" $
        assertEqual "quota" [Just 2, Just 2, Just 1, Just 0, Just 2148] (map Cores.cpuMaxCores ["200000 100000\n", "150000 100000\n", "50000 100000\n", "0 100000\n", "2147483647 1000000"]),
      runTest "the division is upstream's single-precision one" $
        assertEqual "float" (Just 20) (Cores.cpuMaxCores "20000001 1000000\n"),
      runTest "anything but two decimal tokens, a quota past int and a period of 0 are no limit" $
        assertEqual
          "malformed"
          (replicate 7 Nothing)
          (map Cores.cpuMaxCores ["", "100000\n", "1 2 3\n", "abc 100000\n", "+5 100000\n", "2147483648 100000\n", "100000 0\n"]),
      -- The reads, in upstream's order.
      runTest "the limit is the cpu.max of the unnamed group under the first cgroup2 mount, joined as upstream joins them" $
        assertEqual "limit" (Just 3) (limitOver machine),
      runTest "a missing file, an empty group path, no unnamed group, a malformed line and max are each no limit" $
        assertEqual
          "steps"
          (replicate 7 Nothing)
          [ limitOver (Map.delete "/proc/mounts" machine),
            limitOver (Map.delete "/proc/self/cgroup" machine),
            limitOver (Map.delete cpuMaxPath machine),
            limitOver (Map.insert "/proc/self/cgroup" "0::\n" (Map.insert "/sys/fs/cgroup//cpu.max" "500000 100000\n" machine)),
            limitOver (Map.insert "/proc/self/cgroup" "1:name=systemd:/user.slice/build.scope\n" machine),
            limitOver (Map.insert "/proc/self/cgroup" (unifiedGroup <> "garbage\n") machine),
            limitOver (Map.insert cpuMaxPath "max 100000\n" machine)
          ],
      -- getDefaultCores.
      runTest "the default is a positive limit, else the hardware count raised to 1" $
        assertEqual "default" [3, 8, 8, 1] [Cores.defaultCores (Just 3) 8, Cores.defaultCores Nothing 8, Cores.defaultCores (Just 0) 8, Cores.defaultCores Nothing 0],
      hardwareCase
    ]
  where
    -- getconf answers _NPROCESSORS_ONLN by calling sysconf with that name,
    -- in glibc and on macOS alike.  Windows has neither.
    hardwareCase
      | SI.os == "mingw32" = True <$ putStrLn "  SKIP  the hardware count is sysconf's: Windows has no sysconf"
      | otherwise =
          runTestM "the hardware count is sysconf's _NPROCESSORS_ONLN, as getconf reports it" $ do
            reported <- Proc.readProcess "getconf" ["_NPROCESSORS_ONLN"] ""
            assertEqual "getconf" (readMaybe (filter (/= '\n') reported)) . Just <$> Cores.hardwareConcurrency

-- | The child's environment is the build environment plus the ambient
-- allowlist, nothing else.  Proven behaviorally at both ends: an ambient
-- canary set in the test process must not reach a build, and the four
-- temp-directory names must all point at the same build-owned directory.
testScrubbedBuildEnvIO :: IO [Bool]
testScrubbedBuildEnvIO = do
  putStrLn "builder/scrubbed-env"
  withTempStore $ \store -> do
    let storeDir = stDir store
        probeDrv name suffix (builder, args) =
          Derivation
            { drvOutputs =
                [ DerivationOutput
                    { doName = "out",
                      doPath = StorePath (T.replicate 31 "0" <> suffix) name,
                      doHashAlgo = "",
                      doHash = ""
                    }
                ],
              drvInputDrvs = Map.empty,
              drvInputSrcs = [],
              drvPlatform = currentPlatform,
              drvBuilder = builder,
              drvArgs = args,
              drvEnv = Map.empty
            }
        canaryDrv =
          probeDrv "env-canary" "6" $
            if SI.os == "mingw32"
              then ("cmd.exe", ["/c", "echo %NOVA_NIX_TEST_CANARY%>%out%"])
              else ("/bin/sh", ["-c", "echo \"${NOVA_NIX_TEST_CANARY-scrubbed}\" > $out"])
        quartetDrv =
          probeDrv "temp-quartet" "7" $
            if SI.os == "mingw32"
              then ("cmd.exe", ["/c", "if \"%TMPDIR%\"==\"%TEMPDIR%\" if \"%TMPDIR%\"==\"%TMP%\" if \"%TMPDIR%\"==\"%TEMP%\" if not \"%TMPDIR%\"==\"\" echo quartet-ok>%out%"])
              else ("/bin/sh", ["-c", "[ -n \"$TMPDIR\" ] && [ \"$TMPDIR\" = \"$TEMPDIR\" ] && [ \"$TMPDIR\" = \"$TMP\" ] && [ \"$TMPDIR\" = \"$TEMP\" ] && echo quartet-ok > $out"])
    let config = defaultBuildConfig storeDir
    setEnv "NOVA_NIX_TEST_CANARY" "ambient-leak"
    canaryResult <- buildDerivation config store canaryDrv testDrvPath
    unsetEnv "NOVA_NIX_TEST_CANARY"
    quartetResult <- buildDerivation config store quartetDrv testDrvPath
    sequence
      [ runTestM "an undeclared ambient variable does not reach a build" $
          case canaryResult of
            BuildFailure msg code -> pure (Fail ("build failed (exit " <> T.pack (show code) <> "): " <> msg))
            BuildSuccess sp -> do
              content <- TIO.readFile (storePathToFilePath storeDir sp)
              pure $
                if "ambient-leak" `T.isInfixOf` content
                  then Fail ("ambient canary leaked into the build: " <> T.pack (show content))
                  else Pass,
        runTestM "all four temp names point at the build directory" $
          case quartetResult of
            BuildFailure msg code -> pure (Fail ("quartet disagreed or build failed (exit " <> T.pack (show code) <> "): " <> msg))
            BuildSuccess sp -> do
              content <- TIO.readFile (storePathToFilePath storeDir sp)
              pure $
                if "quartet-ok" `T.isInfixOf` content
                  then Pass
                  else Fail ("unexpected content: " <> T.pack (show content))
      ]

-- | A gzip-compressed tar archive of these entries.
gzipArchive :: [TarEntry.Entry] -> BL.ByteString
gzipArchive = GZip.compress . Tar.write

-- | A test archive's link target.  The targets the tests pass are short
-- literals, so encoding one cannot fail; 'error' marks a setup bug, not a
-- test failure.
linkOrDie :: FilePath -> TarEntry.LinkTarget
linkOrDie t =
  fromMaybe (error ("test link target: " ++ t)) (TarEntry.toLinkTarget t)

tarExecFile :: FilePath -> BL.ByteString -> TarEntry.Entry
tarExecFile p content = (tarFile p content) {TarEntry.entryPermissions = 0o755}

tarHardLink :: FilePath -> FilePath -> TarEntry.Entry
tarHardLink p target =
  TarEntry.simpleEntry (tarPathOrDie False p) (Tar.HardLink (linkOrDie target))

tarSymLink :: FilePath -> FilePath -> TarEntry.Entry
tarSymLink p target =
  TarEntry.simpleEntry (tarPathOrDie False p) (Tar.SymbolicLink (linkOrDie target))

-- | A pax global header (type @g@) holding @records@, the entry
-- @git archive@ opens GitHub's archives with to carry the commit id.
tarPaxGlobalHeader :: BL.ByteString -> TarEntry.Entry
tarPaxGlobalHeader records =
  TarEntry.simpleEntry (tarPathOrDie False "pax_global_header") (Tar.OtherEntryType 'g' records (BL.length records))

-- | Step 3 of the ladder: @builtin:unpack@, the stage-0 seed extractor.
-- Two gzip-compressed tar archives sharing a top-level directory are
-- extracted into ONE output: regular files, an executable, a hardlink and
-- a relative symlink (both materialized as copies), and a file at the
-- archive root whose name begins with a dot.  A second build proves
-- cross-archive file collisions fail loudly, and further builds that a
-- parent-climbing (..) entry and drive-rooted file, symlink, and hardlink
-- paths are all rejected.
testUnpackBuildIO :: IO [Bool]
testUnpackBuildIO = do
  putStrLn "builder/unpack-seed-archives"
  tmpBase0 <- getTemporaryDirectory
  if ' ' `elem` tmpBase0
    then do
      -- The srcs env is space-separated by the derivation contract (store
      -- paths never contain spaces), but these test archives live under
      -- TEMP: a spaced TEMP would fragment the paths and fail spuriously.
      putStrLn "  SKIP  unpack archive tests need a space-free TEMP dir"
      pure []
    else withTempStore $ \store -> do
      let storeDir = stDir store
      -- Archives live in the temp store under store-path names: the srcs
      -- env is store paths by contract, and runBuiltinUnpack insists on
      -- that - each entry is parsed and rendered through the
      -- build's store dir, which for these builds is the temp store.
      let archiveFile name = storePathToFilePath storeDir (StorePath (T.replicate 32 "0") name)
          archiveTools = archiveFile "pkg-tools.tar.gz"
          archiveData = archiveFile "pkg-data.tar.gz"
          archiveCollide = archiveFile "pkg-collide.tar.gz"
          archiveEscape = archiveFile "pkg-escape.tar.gz"
          archiveRootFile = archiveFile "pkg-root-file.tar.gz"
          archiveEscapeSymlink = archiveFile "pkg-escape-symlink.tar.gz"
          archiveEscapeHardlink = archiveFile "pkg-escape-hardlink.tar.gz"
          archiveDirLink = archiveFile "pkg-dirlink.tar.gz"
          archiveDirLinkBase = archiveFile "pkg-dirlink-base.tar.gz"
          archiveDirLinkCollide = archiveFile "pkg-dirlink-collide.tar.gz"
          archiveHardLinkCollide = archiveFile "pkg-hardlink-collide.tar.gz"
          archiveManyEntries = archiveFile "pkg-many-entries.tar.gz"
          archiveBigFile = archiveFile "pkg-big-file.tar.gz"
          archiveAmplify = archiveFile "pkg-amplify.tar.gz"
          archiveGitHub = archiveFile "repo-0123456.tar.gz"
      BL.writeFile archiveTools $
        gzipArchive
          [ tarDir "pkg",
            tarDir "pkg/bin",
            tarExecFile "pkg/bin/tool.exe" "tool-payload",
            tarHardLink "pkg/bin/tool-link.exe" "pkg/bin/tool.exe",
            tarSymLink "pkg/bin/sh.exe" "tool.exe"
          ]
      BL.writeFile archiveData $
        gzipArchive
          [ tarDir "pkg",
            tarDir "pkg/share",
            tarFile "pkg/share/data.txt" "shared-data",
            tarFile ".hidden" "dot-file"
          ]
      BL.writeFile archiveCollide $
        gzipArchive [tarFile "pkg/bin/tool.exe" "conflicting"]
      BL.writeFile archiveEscape $
        gzipArchive [tarFile "../evil.txt" "escape"]
      -- A leading '/' entry: rooted at the current drive.  tar stores paths with
      -- '/', so this is the on-disk form even of a Windows-native "\..." name.
      BL.writeFile archiveRootFile $
        gzipArchive [tarFile "/planted-abs.txt" "rooted-file"]
      -- Escaping link targets: '..' climbs above the archive root.  Unlike a
      -- rooted ('/') target, this builds portably - tar's toLinkTarget accepts a
      -- relative path on every host - so the real unpacker is exercised end to end.
      BL.writeFile archiveEscapeSymlink $
        gzipArchive [tarSymLink "sneaky-link.txt" "../escape-link-target.txt"]
      BL.writeFile archiveEscapeHardlink $
        gzipArchive [tarHardLink "sneaky-hardlink.txt" "../escape-hardlink-target.txt"]
      -- A directory symlink entry with a fresh destination: materialized
      -- by copying its target tree.
      BL.writeFile archiveDirLink $
        gzipArchive
          [ tarDir "realdir",
            tarFile "realdir/g.txt" "dir-link-payload",
            tarSymLink "dirlink" "realdir"
          ]
      -- Collisions: the first archive already provides pkg/, and a later
      -- archive's directory symlink/hardlink also lands at pkg.  The
      -- tree copy must hit the same collision guard as a regular entry,
      -- not silently merge - the merged result would depend on entry
      -- order.
      BL.writeFile archiveDirLinkBase $
        gzipArchive [tarDir "pkg", tarFile "pkg/original.txt" "original"]
      BL.writeFile archiveDirLinkCollide $
        gzipArchive
          [ tarDir "smuggle",
            tarFile "smuggle/extra.txt" "smuggled",
            tarSymLink "pkg" "smuggle"
          ]
      BL.writeFile archiveHardLinkCollide $
        gzipArchive
          [ tarDir "smuggle",
            tarFile "smuggle/extra.txt" "smuggled",
            tarHardLink "pkg" "smuggle"
          ]
      -- Budget probes, built against tiny caps injected via BuildConfig.
      BL.writeFile archiveManyEntries $
        gzipArchive [tarFile ("e" <> show i <> ".txt") "x" | i <- [1 :: Int .. 6]]
      BL.writeFile archiveBigFile $
        gzipArchive [tarFile "big.bin" (BL.replicate 100 55)]
      -- Two directory symlinks each re-copy the 40-byte payload: the
      -- copies must charge the budget like first-class content.
      BL.writeFile archiveAmplify $
        gzipArchive
          [ tarDir "base",
            tarFile "base/blob.bin" (BL.replicate 40 55),
            tarSymLink "dup1" "base",
            tarSymLink "dup2" "base"
          ]
      -- GitHub's archive of a commit: gzip over a tar that opens with a pax
      -- global header naming the commit, then one directory named for the
      -- repository and the commit.
      BL.writeFile archiveGitHub $
        gzipArchive
          [ tarPaxGlobalHeader "52 comment=0123456789abcdef0123456789abcdef01234567\n",
            tarDir "repo-0123456",
            tarDir "repo-0123456/src",
            tarFile "repo-0123456/src/main.c" "int main;"
          ]
      let -- srcs carries the canonical /nix/store spelling, as eval writes
          -- it; the physical archives live under the temp store dir.
          canonicalSrc file = case parseStorePath storeDir (T.pack file) of
            Just sp -> storePathToText defaultStoreDir sp
            Nothing -> T.pack file
          mkUnpackDrv outP srcFiles =
            Derivation
              { drvOutputs =
                  [ DerivationOutput
                      { doName = "out",
                        doPath = outP,
                        doHashAlgo = "",
                        doHash = ""
                      }
                  ],
                drvInputDrvs = Map.empty,
                drvInputSrcs = [],
                drvPlatform = currentPlatform,
                drvBuilder = builtinUnpackBuilder,
                drvArgs = [],
                drvEnv =
                  Map.fromList
                    [(envSrcs, TE.encodeUtf8 (T.intercalate " " (map canonicalSrc srcFiles)))]
              }
          seedOut = StorePath (T.replicate 31 "0" <> "2") "unpack-seed"
          collideOut = StorePath (T.replicate 31 "0" <> "3") "unpack-collide"
          escapeOut = StorePath (T.replicate 31 "0" <> "4") "unpack-escape"
          rootFileOut = StorePath (T.replicate 31 "0" <> "5") "unpack-root-file"
          escapeSymlinkOut = StorePath (T.replicate 31 "0" <> "6") "unpack-escape-symlink"
          escapeHardlinkOut = StorePath (T.replicate 31 "0" <> "7") "unpack-escape-hardlink"
          dirLinkOut = StorePath (T.replicate 31 "0" <> "8") "unpack-dirlink"
          dirLinkCollideOut = StorePath (T.replicate 31 "0" <> "9") "unpack-dirlink-collide"
          hardLinkCollideOut = StorePath (T.replicate 30 "0" <> "10") "unpack-hardlink-collide"
          manyEntriesOut = StorePath (T.replicate 30 "0" <> "11") "unpack-entry-budget"
          bigFileOut = StorePath (T.replicate 30 "0" <> "12") "unpack-size-budget"
          amplifyOut = StorePath (T.replicate 30 "0" <> "13") "unpack-amplify-budget"
          gitHubOut = StorePath (T.replicate 30 "0" <> "14") "unpack-github-archive"
      let config = defaultBuildConfig storeDir
      seedResult <- buildDerivation config store (mkUnpackDrv seedOut [archiveTools, archiveData]) testDrvPath
      collideResult <- buildDerivation config store (mkUnpackDrv collideOut [archiveTools, archiveCollide]) testDrvPath
      escapeResult <- buildDerivation config store (mkUnpackDrv escapeOut [archiveEscape]) testDrvPath
      rootFileResult <- buildDerivation config store (mkUnpackDrv rootFileOut [archiveRootFile]) testDrvPath
      escapeSymlinkResult <- buildDerivation config store (mkUnpackDrv escapeSymlinkOut [archiveEscapeSymlink]) testDrvPath
      escapeHardlinkResult <- buildDerivation config store (mkUnpackDrv escapeHardlinkOut [archiveEscapeHardlink]) testDrvPath
      dirLinkResult <- buildDerivation config store (mkUnpackDrv dirLinkOut [archiveDirLink]) testDrvPath
      dirLinkCollideResult <- buildDerivation config store (mkUnpackDrv dirLinkCollideOut [archiveDirLinkBase, archiveDirLinkCollide]) testDrvPath
      hardLinkCollideResult <- buildDerivation config store (mkUnpackDrv hardLinkCollideOut [archiveDirLinkBase, archiveHardLinkCollide]) testDrvPath
      let tinyBudget bytes entries =
            config {bcUnpackLimits = UnpackLimits {ulMaxBytes = bytes, ulMaxEntries = entries}}
      manyEntriesResult <- buildDerivation (tinyBudget 1000000 4) store (mkUnpackDrv manyEntriesOut [archiveManyEntries]) testDrvPath
      bigFileResult <- buildDerivation (tinyBudget 64 1000) store (mkUnpackDrv bigFileOut [archiveBigFile]) testDrvPath
      amplifyResult <- buildDerivation (tinyBudget 100 1000) store (mkUnpackDrv amplifyOut [archiveAmplify]) testDrvPath
      -- This process's own read of the archive, were it still open, would
      -- lock it against a write; on Windows it keeps the file from being
      -- deleted too.  Opened at once, before a collection could finalize
      -- such a read.
      amplifyClosed <- try (withBinaryFile archiveAmplify ReadWriteMode (const (pure ())))
      gitHubResult <- buildDerivation config store (mkUnpackDrv gitHubOut [archiveGitHub]) testDrvPath
      seedChecks <- case seedResult of
        BuildFailure msg code ->
          sequence
            [ runTest "unpack seed build succeeds" $
                Fail ("build failed (exit " <> T.pack (show code) <> "): " <> msg)
            ]
        BuildSuccess sp -> do
          let outRoot = storePathToFilePath storeDir sp
              readOut rel = do
                let path = outRoot </> rel
                exists <- Dir.doesFileExist path
                if exists then Just <$> TIO.readFile path else pure Nothing
          tool <- readOut ("pkg" </> "bin" </> "tool.exe")
          toolLink <- readOut ("pkg" </> "bin" </> "tool-link.exe")
          shLink <- readOut ("pkg" </> "bin" </> "sh.exe")
          dataFile <- readOut ("pkg" </> "share" </> "data.txt")
          dotFile <- readOut ".hidden"
          execBitOk <-
            if SI.os == "mingw32"
              then pure True -- NTFS has no exec bit; PATHEXT decides
              else Dir.executable <$> getPermissions (outRoot </> "pkg" </> "bin" </> "tool.exe")
          narInfo <- queryPathInfo (stDB store) sp
          let realNarHash = case narInfo of
                Just info ->
                  piNarSize info > 0 && T.isPrefixOf "sha256:" (piNarHash info)
                Nothing -> False
          sequence
            [ runTest "unpack seed build succeeds" Pass,
              runTest "unpack: file extracted with content" $
                assertEqual "tool.exe" (Just "tool-payload") tool,
              runTest "unpack: hardlink materialized as copy" $
                assertEqual "tool-link.exe" (Just "tool-payload") toolLink,
              runTest "unpack: relative symlink materialized as copy" $
                assertEqual "sh.exe" (Just "tool-payload") shLink,
              runTest "unpack: second archive merged into shared prefix" $
                assertEqual "data.txt" (Just "shared-data") dataFile,
              runTest "unpack: a dot file at the archive root extracted" $
                assertEqual ".hidden" (Just "dot-file") dotFile,
              runTest "unpack: executable bit materialized (unix)" $
                if execBitOk then Pass else Fail "tool.exe not executable",
              runTest "unpack: real NAR hash registered" $
                if realNarHash then Pass else Fail (T.pack (show narInfo))
            ]
      collideChecks <-
        sequence
          [ runTest "unpack: cross-archive file collision fails loudly" $
              case collideResult of
                BuildFailure msg _
                  | "file collision" `T.isInfixOf` msg -> Pass
                  | otherwise -> Fail ("wrong failure: " <> msg)
                BuildSuccess _ -> Fail "collision build unexpectedly succeeded"
          ]
      escapeChecks <-
        sequence
          [ runTest "unpack: path traversal rejected" $
              case escapeResult of
                BuildFailure msg _
                  | "escapes archive root" `T.isInfixOf` msg -> Pass
                  | otherwise -> Fail ("wrong failure: " <> msg)
                BuildSuccess _ -> Fail "escape build unexpectedly succeeded"
          ]
      let rejectedWith needle res = case res of
            BuildFailure msg _
              | needle `T.isInfixOf` msg -> Pass
              | otherwise -> Fail ("wrong failure: " <> msg)
            BuildSuccess _ -> Fail "malicious entry unexpectedly built"
      -- Integration: the real unpacker rejects a rooted entry path and an
      -- escaping ('..') symlink/hardlink target, end to end.
      maliciousChecks <-
        sequence
          [ runTest "unpack: rooted file entry rejected" $
              rejectedWith "planted-abs.txt" rootFileResult,
            runTest "unpack: escaping symlink target rejected" $
              rejectedWith "escape-link-target.txt" escapeSymlinkResult,
            runTest "unpack: escaping hardlink target rejected" $
              rejectedWith "escape-hardlink-target.txt" escapeHardlinkResult
          ]
      dirLinkChecks <- case dirLinkResult of
        BuildFailure msg code ->
          sequence
            [ runTest "unpack: directory symlink materialized as copied tree" $
                Fail ("build failed (exit " <> T.pack (show code) <> "): " <> msg)
            ]
        BuildSuccess sp -> do
          let outRoot = storePathToFilePath storeDir sp
              linkedFile = outRoot </> "dirlink" </> "g.txt"
          linkedExists <- Dir.doesFileExist linkedFile
          outcome <-
            if not linkedExists
              then pure (Fail "dirlink/g.txt missing from the output")
              else do
                linked <- TIO.readFile linkedFile
                -- The copy contract: a real directory, never a link (a
                -- store link would need privilege and be machine-dependent).
                isLink <- Dir.pathIsSymbolicLink (outRoot </> "dirlink")
                pure $
                  if linked == "dir-link-payload" && not isLink
                    then Pass
                    else Fail ("content=" <> linked <> " isLink=" <> T.pack (show isLink))
          sequence [runTest "unpack: directory symlink materialized as copied tree" outcome]
      collisionGuardChecks <-
        sequence
          [ runTest "unpack: directory symlink onto existing content fails loudly" $
              rejectedWith "file collision" dirLinkCollideResult,
            runTest "unpack: directory hardlink onto existing content fails loudly" $
              rejectedWith "file collision" hardLinkCollideResult
          ]
      budgetChecks <-
        sequence
          [ runTest "unpack: entry budget breach fails loudly" $
              rejectedWith "entry budget" manyEntriesResult,
            runTest "unpack: size budget breach fails loudly" $
              rejectedWith "size budget" bigFileResult,
            runTest "unpack: directory link copies charge the budget" $
              rejectedWith "size budget" amplifyResult,
            runTest "unpack: an archive the unpack stopped partway through is closed, so it can be written at once" $
              either (\err -> Fail ("the archive is still open: " <> T.pack (show (err :: IOException)))) (const Pass) amplifyClosed
          ]
      -- Unit: the rooted (drive-relative) case is the Blocker-4 Windows vuln.
      -- tar's toLinkTarget refuses an absolute payload on POSIX, so exercise the
      -- guard predicate directly - portable and exact on every host.
      guardChecks <-
        sequence
          [ runTest "unpack guard: rooted entry path rejected" $
              assertLeft "rooted entry" (entryComponents "/planted-abs.txt"),
            runTest "unpack guard: rooted symlink target rejected" $
              assertLeft "rooted symlink" (resolveLinkTarget [] "/planted-link-target.txt"),
            runTest "unpack guard: rooted hardlink target rejected" $
              assertLeft "rooted hardlink" (entryComponents "/planted-hardlink-target.txt")
          ]
      gitHubChecks <- case gitHubResult of
        BuildFailure msg code ->
          sequence
            [ runTest "unpack: a gzip archive in GitHub's shape extracts" $
                Fail ("build failed (exit " <> T.pack (show code) <> "): " <> msg)
            ]
        BuildSuccess sp -> do
          let outRoot = storePathToFilePath storeDir sp
              source = outRoot </> "repo-0123456" </> "src" </> "main.c"
          sourceExists <- Dir.doesFileExist source
          content <- if sourceExists then Just <$> TIO.readFile source else pure Nothing
          topLevel <- Dir.listDirectory outRoot
          sequence
            [ runTest "unpack: a gzip archive in GitHub's shape extracts" $
                assertEqual "main.c" (Just "int main;") content,
              runTest "unpack: the pax global header is not extracted" $
                assertEqual "top level" ["repo-0123456"] topLevel
            ]
      pure (seedChecks ++ collideChecks ++ escapeChecks ++ maliciousChecks ++ dirLinkChecks ++ collisionGuardChecks ++ budgetChecks ++ guardChecks ++ gitHubChecks)

-- ---------------------------------------------------------------------------
-- Profile trees and builtin:buildenv
-- ---------------------------------------------------------------------------

-- | A node of the in-memory store the tree tests read.
data FakeNode = FakeDirectory | FakeFile !BS.ByteString | FakeLink !OsPath
  deriving (Show)

-- | An in-memory store: every node by its path's components.
newtype FakeStore = FakeStore (Map.Map [OsPath] FakeNode)
  deriving (Show)

-- | A store holding these nodes, with every ancestor of each a directory.
fakeStore :: [(OsPath, FakeNode)] -> FakeStore
fakeStore entries = FakeStore (Map.union (Map.fromList nodes) ancestors)
  where
    nodes = [(OP.splitDirectories path, node) | (path, node) <- entries]
    ancestors = Map.fromList [(prefix, FakeDirectory) | (key, _) <- nodes, prefix <- drop 1 (inits key), prefix /= key]

-- | How many links resolving one path follows before it reads a loop, as
-- a POSIX system's @SYMLOOP_MAX@ bounds it.
fakeLinkBudget :: Int
fakeLinkBudget = 40

-- | The components a path names in the store with every link on it
-- followed, or 'Nothing' for a loop.  The store's link targets are
-- absolute.
fakeResolve :: FakeStore -> OsPath -> Maybe [OsPath]
fakeResolve (FakeStore nodes) = go fakeLinkBudget [] . OP.splitDirectories
  where
    go budget walked remaining = case remaining of
      [] -> Just walked
      component : rest ->
        let here = walked ++ [component]
         in case Map.lookup here nodes of
              Just (FakeLink target)
                | budget > 0 -> go (budget - 1) [] (OP.splitDirectories target ++ rest)
                | otherwise -> Nothing
              _ -> go budget here rest

-- | The names directly in a directory of the store.
fakeChildren :: FakeStore -> [OsPath] -> [OsPath]
fakeChildren (FakeStore nodes) key = [name | (other, _) <- Map.toList nodes, Just [name] <- [stripPrefix key other]]

-- | The path of a package's propagated-user-env-packages file.
propagatedFileOf :: OsPath -> OsPath
propagatedFileOf pkg = pkg OP.</> asciiOsPath "nix-support" OP.</> asciiOsPath "propagated-user-env-packages"

-- | A t'TreeReader' over an in-memory store, following links as @stat@
-- does, and naming a propagated package by its bytes.
fakeReader :: FakeStore -> TreeReader Identity
fakeReader store@(FakeStore nodes) =
  TreeReader
    { trEntries = \path -> pure $ do
        key <- resolved path
        case Map.lookup key nodes of
          Just FakeDirectory -> Right (Entries (fakeChildren store key))
          Just _ -> Right NotADirectory
          Nothing -> Left (failure "cannot read directory" path),
      trKind = \path -> pure (fmap kindOf . (`Map.lookup` nodes) <$> resolved path),
      trResolve = \path -> pure (OP.joinPath <$> resolved path),
      trPropagated = \pkg -> pure $ case fakeResolve store (propagatedFileOf pkg) >>= (`Map.lookup` nodes) of
        Just (FakeFile bytes) -> traverse (maybe (Left (failure "no path in" pkg)) Right . hostPathFromBytes) (propagatedTokens bytes)
        _ -> Right []
    }
  where
    resolved path = maybe (Left (failure "too many links in" path)) Right (fakeResolve store path)
    failure doing path = ReadFailure (doing <> " " <> hostPathText path)
    kindOf node = case node of
      FakeDirectory -> DirectoryEntry
      _ -> NonDirectoryEntry

-- | A path in the in-memory store, from ASCII components under its root.
fakeTreePath :: [String] -> OsPath
fakeTreePath = foldl' (\path component -> path OP.</> asciiOsPath component) (asciiOsPath "/s")

-- | A file in the in-memory store.
fakeFile :: [String] -> (OsPath, FakeNode)
fakeFile components = (fakeTreePath components, FakeFile "contents")

-- | The directory the tree tests name the tree's own paths under.
fakeOut :: OsPath
fakeOut = asciiOsPath "/out"

-- | A path's bytes as a propagated-user-env-packages file spells it.
-- Test paths are ASCII, so its text's UTF-8 is exact.
propagatedBytes :: OsPath -> BS.ByteString
propagatedBytes = TE.encodeUtf8 . hostPathText

-- | A propagated-user-env-packages file naming these packages.
propagating :: [OsPath] -> FakeNode
propagating = FakeFile . BS.intercalate "\n" . map propagatedBytes

-- | What 'buildProfile' makes of packages in an in-memory store.
fakeBuild :: FakeStore -> [Package] -> Built
fakeBuild store = runIdentity . buildProfile (fakeReader store) fakeOut

-- | A directory of the tree, from ASCII names.
namedTree :: [(String, Node)] -> Tree
namedTree entries = Map.fromList [(asciiOsPath name, node) | (name, node) <- entries]

-- | The tree two packages sharing @bin@ make at priority 5, the first
-- with a program @hello@ and a @share@ directory and the second with a
-- program @world@: @bin@ is a directory of the tree's own, which holds
-- the first package's program under what its @bin@ resolves to, and
-- @share@ is linked whole.  Both the golden case and the build through
-- the builder are held to it.
sharedBinTree :: OsPath -> OsPath -> OsPath -> Tree
sharedBinTree first firstBinResolved second =
  namedTree
    [ ( "bin",
        Directory
          ( namedTree
              [ ("hello", Link (firstBinResolved OP.</> asciiOsPath "hello") 5),
                ("world", Link (second OP.</> asciiOsPath "bin" OP.</> asciiOsPath "world") 5)
              ]
          )
      ),
      ("share", Link (first OP.</> asciiOsPath "share") 5)
    ]

-- | Every link target in a tree.
treeLinkTargets :: Tree -> [OsPath]
treeLinkTargets = concatMap targets . Map.elems
  where
    targets node = case node of
      Link target _ -> [target]
      Directory sub -> treeLinkTargets sub

-- | Packages and every package they propagate, transitively.
fakePropagationClosure :: FakeStore -> [OsPath] -> [OsPath]
fakePropagationClosure store = go Set.empty []
  where
    go _ found [] = found
    go seen found (path : rest)
      | Set.member key seen = go seen found rest
      | otherwise = go (Set.insert key seen) (path : found) (propagated ++ rest)
      where
        key = OP.splitDirectories path
        propagated = fromRight [] (runIdentity (trPropagated (fakeReader store) path))

-- | Every node of the store reachable from these paths, each by the
-- components it resolves to: the paths themselves and everything below
-- them, through links.
fakeReachable :: FakeStore -> [OsPath] -> Set.Set [OsPath]
fakeReachable store@(FakeStore nodes) roots = go Set.empty (mapMaybe (fakeResolve store) roots)
  where
    go seen pending = case pending of
      [] -> seen
      key : rest
        | Set.member key seen -> go seen rest
        | otherwise -> go (Set.insert key seen) (children key ++ rest)
    children key = case Map.lookup key nodes of
      Just FakeDirectory -> mapMaybe (\name -> fakeResolve store (OP.joinPath (key ++ [name]))) (fakeChildren store key)
      _ -> []

-- | Whether a tree holds a directory of its own at its top, which only
-- expanding a link to a directory makes.
madeDirectory :: Tree -> Bool
madeDirectory = any isDirectory . Map.elems
  where
    isDirectory node = case node of
      Directory _ -> True
      Link _ _ -> False

-- | The law that a tree stays inside its packages: every link in it names
-- a node that is there, which a package given or one propagated reaches.
-- Each class of case is held to about a third of its share of 20000
-- generated at QuickCheck's sizes: a tree built in 69% of them, and one
-- with a directory of its own in 25%.
linksResolveIntoPackages :: (FakeStore, [Package]) -> QC.Property
linksResolveIntoPackages (store@(FakeStore nodes), packages) =
  QC.cover 20 (isRight tree) "the tree was built"
    $ QC.cover 8 (either (const False) madeDirectory tree) "a link to a directory was expanded"
    $ QC.conjoin [QC.counterexample (show target) (reaches target) | target <- either (const []) treeLinkTargets tree]
  where
    tree = builtTree (fakeBuild store packages)
    reachable = fakeReachable store (fakePropagationClosure store (map pkgPath packages))
    reaches target = case fakeResolve store target of
      Just key -> Map.member key nodes && Set.member key reachable
      Nothing -> False

-- | How deep a generated package's directories go.
fakeTreeDepth :: Int
fakeTreeDepth = 2

-- | An in-memory store of one to four packages, and packages to build
-- from it.  Each name keeps one kind wherever it is, so two packages meet
-- at a name only with two of a kind: @bin@, @share@, @lib@ and @info@
-- are directories; @x@, @dir@, @log@ and @.hid@ files; @lnd@ a link to
-- some package's @bin@ or @share@, @lnf@ a link to some package's @x@,
-- and @gone@ a link to nothing.  A package is sometimes a file, and
-- sometimes propagates others.  Priorities are few, so files meet at one
-- priority as well as at two.
genFakeProfile :: QC.Gen (FakeStore, [Package])
genFakeProfile = do
  count <- QC.choose (1, 4 :: Int)
  let roots = [fakeTreePath ["p" <> show index] | index <- [1 .. count]]
  contents <- traverse (genFakePackage roots) roots
  packages <- QC.listOf1 (Package <$> QC.elements roots <*> QC.elements ([0 .. 9] ++ [1000, 1001]))
  pure (fakeStore (concat contents), packages)

-- | One generated package of 'genFakeProfile', at @root@.
genFakePackage :: [OsPath] -> OsPath -> QC.Gen [(OsPath, FakeNode)]
genFakePackage roots root =
  QC.frequency
    [ (1, pure [(root, FakeFile "not a directory")]),
      (8, (++) <$> genFakeDirectory roots fakeTreeDepth root <*> propagation)
    ]
  where
    propagation =
      QC.frequency
        [ (2, pure []),
          (1, (\named -> [(propagatedFileOf root, propagating named)]) <$> QC.sublistOf roots)
        ]

-- | A generated directory of 'genFakeProfile' at @dir@, @depth@ levels of
-- directories deep at most.
genFakeDirectory :: [OsPath] -> Int -> OsPath -> QC.Gen [(OsPath, FakeNode)]
genFakeDirectory roots depth dir = do
  names <- QC.sublistOf (if depth > 0 then directoryNames ++ leafNames else leafNames)
  ((dir, FakeDirectory) :) . concat <$> traverse entry names
  where
    directoryNames = ["bin", "share", "lib", "info"]
    leafNames = ["x", "dir", "log", ".hid", "lnd", "lnf", "gone"]
    entry name
      | name `elem` directoryNames = genFakeDirectory roots (depth - 1) path
      | name == "lnd" = linkTo <$> QC.elements [root OP.</> asciiOsPath sub | root <- roots, sub <- ["bin", "share"]]
      | name == "lnf" = linkTo <$> QC.elements [root OP.</> asciiOsPath "x" | root <- roots]
      | name == "gone" = pure (linkTo (fakeTreePath ["nowhere"]))
      | otherwise = pure [(path, FakeFile (BS8.pack name))]
      where
        path = dir OP.</> asciiOsPath name
        linkTo target = [(path, FakeLink target)]

-- | 'buildProfile' (upstream's buildenv.cc at 2.35.4) over an in-memory
-- store: one golden case per rule, then the laws that the tree does not
-- depend on the order the packages are given in and that it stays inside
-- them.
testProfileTree :: IO [Bool]
testProfileTree = do
  putStrLn "profile/tree"
  sequence
    [ runTest "one package's entries are linked whole" $
        assertEqual
          "built"
          (Built [] (Right (namedTree [("bin", Link (pkg "a" ["bin"]) 5), ("share", Link (pkg "a" ["share"]) 5)])))
          (fakeBuild (fakeStore [fakeFile ["a", "bin", "hello"], fakeFile ["a", "share", "doc", "a.txt"]]) [Package (pkg "a" []) 5]),
      runTest "two packages sharing bin make it a directory of the tree's own, given in either order" $
        let store = fakeStore [fakeFile ["a", "bin", "hello"], fakeFile ["a", "share", "man", "a.1"], fakeFile ["b", "bin", "world"]]
            expected = Built [] (Right (sharedBinTree (pkg "a" []) (pkg "a" ["bin"]) (pkg "b" [])))
         in assertEqual "b then a" expected (fakeBuild store [Package (pkg "b" []) 5, Package (pkg "a" []) 5])
              `andThen` assertEqual "a then b" expected (fakeBuild store [Package (pkg "a" []) 5, Package (pkg "b" []) 5]),
      runTest "a link to a directory is expanded at the priority it was linked at, so its files keep their place" $
        assertEqual
          "built"
          (Built [] (Right (namedTree [("lib", Directory (namedTree [("liba", Link (pkg "a" ["lib", "liba"]) 3), ("libb", Link (pkg "b" ["lib", "libb"]) 7)]))])))
          (fakeBuild (fakeStore [fakeFile ["a", "lib", "liba"], fakeFile ["b", "lib", "liba"], fakeFile ["b", "lib", "libb"]]) [Package (pkg "a" []) 3, Package (pkg "b" []) 7]),
      runTest "a link to a directory is expanded from what it resolves to" $
        assertEqual
          "built"
          (Built [] (Right (namedTree [("lib", Directory (namedTree [("x", Link (fakeTreePath ["real", "lib", "x"]) 5), ("y", Link (pkg "b" ["lib", "y"]) 6)]))])))
          (fakeBuild (fakeStore [(pkg "a" ["lib"], FakeLink (fakeTreePath ["real", "lib"])), fakeFile ["real", "lib", "x"], fakeFile ["b", "lib", "y"]]) [Package (pkg "a" []) 5, Package (pkg "b" []) 6]),
      runTest "of two files at one path the lower priority number stays, whichever is linked first" $
        assertEqual
          "lower linked first"
          (Built [] (Right (namedTree [("bin", Directory (namedTree [("tool", Link (pkg "b" ["bin", "tool"]) 4)]))])))
          (fakeBuild (fakeStore [fakeFile ["a", "bin", "tool"], fakeFile ["b", "bin", "tool"]]) [Package (pkg "a" []) 6, Package (pkg "b" []) 4])
          `andThen` assertEqual
            "lower linked second, by propagation"
            (Built [] (Right (namedTree [("bin", Directory (namedTree [("tool", Link (pkg "p" ["bin", "tool"]) 1000)]))])))
            (fakeBuild (fakeStore [fakeFile ["e", "bin", "tool"], (propagatedFileOf (pkg "e" []), propagating [pkg "p" []]), fakeFile ["p", "bin", "tool"]]) [Package (pkg "e" []) 2000]),
      runTest "two files at one path with one priority conflict, in upstream's words" $
        let conflict = FileConflict (pkg "a" ["bin", "tool"]) (pkg "b" ["bin", "tool"]) 5
         in assertEqual "built" (Built [] (Left conflict)) (fakeBuild (fakeStore [fakeFile ["a", "bin", "tool"], fakeFile ["b", "bin", "tool"]]) [Package (pkg "b" []) 5, Package (pkg "a" []) 5])
              `andThen` assertEqual
                "message"
                ("Unable to build profile. There is a conflict for the following files:\n\n  \"" <> hostPathText (pkg "a" ["bin", "tool"]) <> "\"\n  \"" <> hostPathText (pkg "b" ["bin", "tool"]) <> "\"")
                (buildEnvFailureText conflict),
      runTest "propagated packages follow at priority 1000 and up, one number per queued path in path order" $
        let store =
              fakeStore
                [ fakeFile ["a", "bin", "a"],
                  (propagatedFileOf (pkg "a" []), FakeFile (propagatedBytes (pkg "q" []) <> "\n" <> propagatedBytes (pkg "p" []) <> "  " <> propagatedBytes (pkg "q" []) <> " " <> propagatedBytes (pkg "a" []) <> "\n")),
                  fakeFile ["p", "share", "p"],
                  (propagatedFileOf (pkg "p" []), propagating [pkg "q" [], pkg "r" []]),
                  fakeFile ["q", "share", "q"],
                  fakeFile ["r", "etc", "r"]
                ]
         in -- a names itself, which is added already and is not queued.
            -- p takes 1000 and q 1001; q, queued again by p before q was
            -- added, takes 1002 unused, and r 1003.
            assertEqual
              "built"
              ( Built
                  []
                  ( Right
                      ( namedTree
                          [ ("bin", Link (pkg "a" ["bin"]) 5),
                            ("etc", Link (pkg "r" ["etc"]) 1003),
                            ("share", Directory (namedTree [("p", Link (pkg "p" ["share", "p"]) 1000), ("q", Link (pkg "q" ["share", "q"]) 1001)]))
                          ]
                      )
                  )
              )
              (fakeBuild store [Package (pkg "a" []) 5]),
      runTest "the files upstream keeps out of profiles are skipped wherever they are, and dir only in info" $
        let store =
              fakeStore
                ( map (\name -> fakeFile ["a", name]) ["propagated-build-inputs", "perllocal.pod", "log", "manifest.nix", "manifest.json"]
                    ++ [ fakeFile ["a", "nix-support", "setup-hook"],
                         fakeFile ["a", "bin", "x"],
                         fakeFile ["a", "lib", "perl5", "perllocal.pod"],
                         fakeFile ["a", "share", "info", "dir"],
                         fakeFile ["a", "share", "info", "a.info"],
                         fakeFile ["a", "share", "doc", "dir"],
                         fakeFile ["b", "log", "b.log"],
                         fakeFile ["b", "lib", "perl5", "perllocal.pod"],
                         fakeFile ["b", "lib", "perl5", "B.pm"],
                         fakeFile ["b", "share", "info", "dir"],
                         fakeFile ["b", "share", "info", "b.info"],
                         fakeFile ["b", "share", "doc", "other"]
                       ]
                )
            share =
              namedTree
                [ ("doc", Directory (namedTree [("dir", Link (pkg "a" ["share", "doc", "dir"]) 5), ("other", Link (pkg "b" ["share", "doc", "other"]) 5)])),
                  ("info", Directory (namedTree [("a.info", Link (pkg "a" ["share", "info", "a.info"]) 5), ("b.info", Link (pkg "b" ["share", "info", "b.info"]) 5)]))
                ]
         in assertEqual
              "built"
              ( Built
                  []
                  ( Right
                      ( namedTree
                          [ ("bin", Link (pkg "a" ["bin"]) 5),
                            ("lib", Directory (namedTree [("perl5", Directory (namedTree [("B.pm", Link (pkg "b" ["lib", "perl5", "B.pm"]) 5)]))])),
                            ("share", Directory share)
                          ]
                      )
                  )
              )
              (fakeBuild store [Package (pkg "a" []) 5, Package (pkg "b" []) 5]),
      runTest "names starting with a dot are skipped" $
        assertEqual
          "built"
          (Built [] (Right (namedTree [("bin", Directory (namedTree [("other", Link (pkg "b" ["bin", "other"]) 5), ("tool", Link (pkg "a" ["bin", "tool"]) 5)]))])))
          (fakeBuild (fakeStore [fakeFile ["a", ".hidden"], fakeFile ["a", "bin", ".x"], fakeFile ["a", "bin", "tool"], fakeFile ["b", "bin", ".x"], fakeFile ["b", "bin", "other"]]) [Package (pkg "a" []) 5, Package (pkg "b" []) 5]),
      runTest "an entry whose link leads nowhere is skipped with upstream's warning, by its path in the tree" $
        let dangling = fakeOut OP.</> asciiOsPath "dangling"
            store = fakeStore [(pkg "a" ["dangling"], FakeLink (fakeTreePath ["missing"])), fakeFile ["a", "bin", "tool"], (pkg "b" ["bin", "broken"], FakeLink (fakeTreePath ["gone"])), fakeFile ["b", "bin", "other"]]
         in assertEqual
              "built"
              ( Built
                  [DanglingLink dangling, DanglingLink (fakeOut OP.</> asciiOsPath "bin" OP.</> asciiOsPath "broken")]
                  (Right (namedTree [("bin", Directory (namedTree [("other", Link (pkg "b" ["bin", "other"]) 5), ("tool", Link (pkg "a" ["bin", "tool"]) 5)]))]))
              )
              (fakeBuild store [Package (pkg "a" []) 5, Package (pkg "b" []) 5])
              `andThen` assertEqual "message" ("skipping dangling symlink '" <> hostPathText dangling <> "'") (warningText (DanglingLink dangling)),
      runTest "a package that is not a directory is left out with upstream's warning" $
        let file = pkg "file" []
         in assertEqual "built" (Built [NotIncluded file] (Right (namedTree [("bin", Link (pkg "a" ["bin"]) 6)]))) (fakeBuild (fakeStore [(file, FakeFile "a file"), fakeFile ["a", "bin", "tool"]]) [Package file 5, Package (pkg "a" []) 6])
              `andThen` assertEqual "message" ("not including \"" <> hostPathText file <> "\" in the user environment because it's not a directory") (warningText (NotIncluded file)),
      runTest "a path given twice is added once, at its lower priority number" $
        assertEqual "built" (Built [] (Right (namedTree [("bin", Link (pkg "a" ["bin"]) 3)]))) (fakeBuild (fakeStore [fakeFile ["a", "bin", "tool"]]) [Package (pkg "a" []) 5, Package (pkg "a" []) 3]),
      runTest "a directory where a link to a file stands fails whatever the priorities, in upstream's words" $
        let collision = LinkToNonDirectory (pkg "b" ["share"]) (pkg "a" ["share"])
         in assertEqual "built" (Built [] (Left collision)) (fakeBuild (fakeStore [fakeFile ["a", "share"], fakeFile ["b", "share", "x"]]) [Package (pkg "a" []) 3, Package (pkg "b" []) 5])
              `andThen` assertEqual "message" ("collision between \"" <> hostPathText (pkg "b" ["share"]) <> "\" and non-directory \"" <> hostPathText (pkg "a" ["share"]) <> "\"") (buildEnvFailureText collision),
      runTest "a file where a directory of the tree's own stands fails, in upstream's words" $
        let collision = FileOverDirectory (pkg "c" ["bin"]) (fakeOut OP.</> asciiOsPath "bin")
         in assertEqual "built" (Built [] (Left collision)) (fakeBuild (fakeStore [fakeFile ["a", "bin", "x"], fakeFile ["b", "bin", "y"], fakeFile ["c", "bin"]]) [Package (pkg "a" []) 1, Package (pkg "b" []) 2, Package (pkg "c" []) 3])
              `andThen` assertEqual "message" ("collision between non-directory '" <> hostPathText (pkg "c" ["bin"]) <> "' and directory '" <> hostPathText (fakeOut OP.</> asciiOsPath "bin") <> "'") (buildEnvFailureText collision),
      runTest "a package that is not there fails the build" $
        assertEqual
          "built"
          (Built [] (Left (ReadFailed (ReadFailure ("cannot read directory " <> hostPathText (pkg "missing" []))))))
          (fakeBuild (fakeStore [fakeFile ["a", "bin", "tool"]]) [Package (pkg "missing" []) 5]),
      runTest "a propagated-user-env-packages file splits on spaces and newlines alone" $
        assertEqual "tokens" ["a", "b", "c\td\r"] (propagatedTokens " a\n\nb  c\td\r\n"),
      -- A failure counts too: which pair a conflict names does not
      -- depend on the order either.  Each class is held to about a third
      -- of its share of 20000 cases at QuickCheck's sizes: a tree built in
      -- 69% of them, and two files meeting at one priority in 27%.
      runTestM "the tree does not depend on the order the packages are given in" $
        property . QC.checkCoverage . QC.forAll genFakeProfile $ \(store, packages) ->
          let built = builtTree (fakeBuild store packages)
              conflicted = case built of
                Left (FileConflict {}) -> True
                _ -> False
           in QC.cover 20 (isRight built) "the tree was built" $
                QC.cover 8 conflicted "two files met at one priority" $
                  QC.forAll (QC.shuffle packages) $
                    \shuffled -> fakeBuild store shuffled QC.=== fakeBuild store packages,
      runTestM "every link in the tree names a node that a package given or one propagated reaches" $
        property (QC.checkCoverage (QC.forAll genFakeProfile linksResolveIntoPackages))
    ]
  where
    pkg name rest = fakeTreePath (name : rest)
    property :: (QC.Testable prop) => prop -> IO TestResult
    property prop = do
      result <- QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} prop
      pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))

-- | Elements, and the @derivations@ attribute @buildenv.nix@ makes of
-- them, with each separator a run of tokenizeString's separators.
genDerivationsCase :: QC.Gen ([Element], BS.ByteString)
genDerivationsCase = do
  elements <- QC.listOf genElement
  let tokens = concatMap elementTokens elements
  lead <- separators QC.listOf
  following <- traverse (const (separators QC.listOf1)) tokens
  pure (elements, BS.concat (lead : zipWith (<>) tokens following))
  where
    genElement = Element <$> QC.elements [Active, Inactive] <*> (fromIntegral <$> (QC.arbitrary :: QC.Gen Int32)) <*> QC.listOf genPath
    genPath = ("/nix/store/" <>) . BS8.pack <$> QC.listOf1 (QC.elements ['a' .. 'z'])
    separators runOf = BS8.pack <$> runOf (QC.elements " \t\n\r")
    elementTokens (Element activity priority paths) =
      [if activity == Active then "true" else "false", BS8.pack (show priority), BS8.pack (show (length paths))] ++ paths

-- | Reading the @derivations@ attribute of a @builtin:buildenv@
-- derivation (@builtinBuildenv@ in buildenv.cc).
testBuildenvDerivations :: IO [Bool]
testBuildenvDerivations = do
  putStrLn "builder/buildenv-derivations"
  sequence
    [ runTest "an empty attribute, or one of separators alone, names no elements" $
        assertEqual "empty" (Right []) (parseDerivations "")
          `andThen` assertEqual "separators" (Right []) (parseDerivations " \t\n\r  \n"),
      runTest "tokens are split on spaces, tabs, carriage returns and newlines, with the empty ones dropped" $
        assertEqual
          "elements"
          (Right [Element Active 5 ["/nix/store/a", "/nix/store/b"], Element Inactive 3 ["/nix/store/c"], Element Active 0 []])
          (parseDerivations "\n true \t5\r\n2  /nix/store/a\t\t/nix/store/b\nfalse 3 1 /nix/store/c\r\n\r\ntrue 0 0 \n"),
      runTest "only false makes an element inactive" $
        assertEqual "activities" (Right [Inactive, Active, Active, Active]) (map elementActivity <$> parseDerivations "false 1 0 False 1 0 0 1 0 true 1 0"),
      runTest "a priority is read as stoi reads it: space, a sign and digits, with whatever follows ignored" $
        assertEqual "priorities" (Right [5, -3, 7, 9, -2147483648, 2147483647]) (map elementPriority <$> parseDerivations "true 5.500000 0 true -3 0 true +7x 0 true \v9 0 true -2147483648 0 true 2147483647 0"),
      runTest "a priority no digit leads, or one out of an int's range, is refused before the count is read" $
        assertEqual "letters" (Left (BadPriority "high")) (parseDerivations "true high 0")
          `andThen` assertEqual "too large" (Left (BadPriority "2147483648")) (parseDerivations "true 2147483648 0")
          `andThen` assertEqual "sign alone" (Left (BadPriority "-")) (parseDerivations "true - 0")
          `andThen` assertEqual "no count after it" (Left (BadPriority "x")) (parseDerivations "true x"),
      runTest "an output count of -0 is zero, and a negative or non-numeric one is refused" $
        assertEqual "minus zero" (Right [Element Active 5 []]) (parseDerivations "true 5 -0")
          `andThen` assertEqual "negative" (Left (BadOutputCount "-1")) (parseDerivations "true 5 -1 /nix/store/a")
          `andThen` assertEqual "letters" (Left (BadOutputCount "one")) (parseDerivations "true 5 one /nix/store/a"),
      runTest "an attribute that ends inside an element is refused" $
        assertEqual "no priority" (Left EndsInsideElement) (parseDerivations "true")
          `andThen` assertEqual "no count" (Left EndsInsideElement) (parseDerivations "true 5")
          `andThen` assertEqual "too few paths" (Left EndsInsideElement) (parseDerivations "true 5 2 /nix/store/a"),
      runTest "an inactive element's paths are not packages" $
        assertEqual
          "packages"
          [("/nix/store/a", 5), ("/nix/store/b", 5), ("/nix/store/d", 2)]
          (elementPackages [Element Active 5 ["/nix/store/a", "/nix/store/b"], Element Inactive 1 ["/nix/store/c"], Element Active 2 ["/nix/store/d"]]),
      runTestM "elements written as buildenv.nix writes them, with any separators between their tokens, read back" $ do
        result <- QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} (QC.forAll genDerivationsCase (\(elements, rendered) -> parseDerivations rendered QC.=== Right elements))
        pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))
    ]

-- | A tree on disk as read back: each link by its target, each directory
-- by its entries.
data Shape = ShapeLink !OsPath | ShapeDirectory !(Map.Map OsPath Shape)
  deriving (Eq, Show)

-- | A tree's shape, without the priorities its links were made at.
shapeOf :: Tree -> Map.Map OsPath Shape
shapeOf = Map.map nodeShape
  where
    nodeShape node = case node of
      Link target _ -> ShapeLink target
      Directory sub -> ShapeDirectory (shapeOf sub)

-- | The shape of the tree in a directory on disk, links not followed.
readShape :: OsPath -> IO (Map.Map OsPath Shape)
readShape dir = do
  names <- OsDir.listDirectory dir
  Map.fromList <$> traverse (\name -> (name,) <$> entryShape (dir OP.</> name)) names
  where
    entryShape path = do
      isLink <- OsDir.pathIsSymbolicLink path
      if isLink then ShapeLink <$> OsDir.getSymbolicLinkTarget path else ShapeDirectory <$> readShape path

-- | @builtin:buildenv@ through the builder, in a derivation of
-- buildenv.nix's shape: two packages sharing @bin@, and an inactive
-- element naming a path that is not there, make the tree the golden case
-- of two packages sharing @bin@ describes, under the store's spelling of
-- their paths, with @manifest.nix@ linked to the manifest.  The manifest
-- is the one input, as nix-env's is, and the packages reach the closure
-- as its references.
testBuildenvBuildIO :: IO [Bool]
testBuildenvBuildIO = do
  putStrLn "builder/buildenv"
  canLink <- symlinksAvailable
  if not canLink
    then do
      putStrLn "  SKIP  cannot create symlinks here (Windows needs Developer Mode or elevation)"
      pure []
    else withTempStore $ \store -> do
      tmpBase <- getTemporaryDirectory
      let storeDir = stDir store
          stage name = tmpBase </> ("nova-nix-test-buildenv-" <> name)
          hello = StorePath (T.replicate 31 "0" <> "a") "hello"
          world = StorePath (T.replicate 31 "0" <> "b") "world"
          absent = StorePath (T.replicate 31 "0" <> "c") "absent"
          manifest = StorePath (T.replicate 31 "0" <> "d") "env-manifest.nix"
          environment = StorePath (T.replicate 31 "0" <> "f") "user-environment"
          canonical = storePathToText defaultStoreDir
          writeStaged path bytes = createDirectoryIfMissing True (takeDirectory path) *> BS.writeFile path bytes
          onDisk = OP.encodeFS . storePathToFilePath storeDir
      writeStaged (stage "hello" </> "bin" </> "hello") "hello"
      writeStaged (stage "hello" </> "share" </> "man" </> "hello.1") "manual"
      writeStaged (stage "world" </> "bin" </> "world") "world"
      writeStaged (stage "manifest") (TE.encodeUtf8 (T.unwords [canonical hello, canonical world]))
      addToStore store (stage "hello") hello Nothing []
      addToStore store (stage "world") world Nothing []
      addToStore store (stage "manifest") manifest Nothing [hello, world]
      let drv =
            Derivation
              { drvOutputs = [DerivationOutput {doName = "out", doPath = environment, doHashAlgo = "", doHash = ""}],
                drvInputDrvs = Map.empty,
                drvInputSrcs = [manifest],
                drvPlatform = OtherPlatform "builtin",
                drvBuilder = builtinBuildenvBuilder,
                drvArgs = [],
                drvEnv =
                  Map.fromList
                    [ ("derivations", TE.encodeUtf8 (T.unwords ["true", "5", "1", canonical world, "false", "5", "1", canonical absent, "true", "5", "1", canonical hello])),
                      ("manifest", TE.encodeUtf8 (canonical manifest))
                    ]
              }
      result <- buildDerivation (defaultBuildConfig storeDir) store drv testDrvPath
      case result of
        BuildFailure message code ->
          sequence [runTest "a buildenv derivation builds" (Fail ("build failed (exit " <> T.pack (show code) <> "): " <> message))]
        BuildSuccess built ->
          sequence
            [ runTest "a buildenv derivation builds" Pass,
              runTestM "its output is the tree of the golden case, with manifest.nix linked to the manifest" $ do
                helloRoot <- onDisk hello
                worldRoot <- onDisk world
                manifestPath <- onDisk manifest
                helloBin <- OsDir.canonicalizePath (helloRoot OP.</> asciiOsPath "bin")
                made <- onDisk built >>= readShape
                let expected = Map.insert (asciiOsPath "manifest.nix") (ShapeLink manifestPath) (shapeOf (sharedBinTree helloRoot helloBin worldRoot))
                pure (assertEqual "tree" expected made),
              runTestM "its output references the packages and the manifest" $ do
                references <- queryReferences (stDB store) built
                pure (assertEqual "references" (Set.fromList (map (T.pack . storePathToFilePath storeDir) [hello, world, manifest])) (Set.fromList references))
            ]

-- ---------------------------------------------------------------------------
-- Tests: profiles
-- ---------------------------------------------------------------------------

-- | An active element holding paths, at the default priority.
manifestElement :: [StorePath] -> Manifest.Element
manifestElement paths = Manifest.Element (Set.fromList paths) Active Manifest.defaultPriority Nothing

-- | A manifest of elements, each added under its name.
manifestOf :: [(Text, Manifest.Element)] -> Manifest.Manifest
manifestOf = foldl' (\manifest (name, element) -> Manifest.addElement name element manifest) Manifest.emptyManifest

-- | Whether a QuickCheck property holds, as a test's result.
propertyHolds :: (QC.Testable prop) => prop -> IO TestResult
propertyHolds prop = do
  result <- QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} prop
  pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))

-- | A manifest's elements: names, paths, activity, priority, and for
-- some a flake source with each kind of outputs.
genManifest :: QC.Gen Manifest.Manifest
genManifest = manifestOf <$> QC.resize 4 (QC.listOf ((,) <$> genElementName <*> genElement))
  where
    genElementName = genTextOf 1 "abc" >>= \first -> (first <>) <$> genTextOf 2 "xyz-1"
    genElement =
      Manifest.Element . Set.fromList
        <$> QC.resize 3 (QC.listOf (genFixturePath ""))
        <*> QC.elements [Active, Inactive]
        <*> QC.chooseInt (-10, 2000)
        <*> QC.oneof [pure Nothing, Just <$> genSource]
    genSource =
      Manifest.FlakeSource
        <$> QC.elements ["github:o/r", "path:/x"]
        <*> QC.elements ["github:o/r/0123abc", "path:/x?narHash=sha256-x"]
        <*> QC.elements ["packages.x86_64-linux.hello", "hello"]
        <*> QC.elements [Nothing, Just Proto.AllOutputs, Proto.namedOutputs (Set.fromList [Proto.OutputName "out", Proto.OutputName "dev"])]

-- | A profile's generations in order of number, distinct, each made at a
-- moment among a few, with a current generation that is one of them, one
-- that is not listed, or none, and a cutoff among those moments.
genDatedListing :: QC.Gen ([Gen.DatedGeneration], Maybe Gen.GenerationNumber, Gen.EpochSeconds)
genDatedListing = do
  gaps <- QC.resize 8 (QC.listOf (QC.chooseInt (1, 3)))
  let numbers = drop 1 (scanl (+) 0 gaps)
  times <- traverse (const (QC.chooseInt (0, 20))) numbers
  current <- QC.oneof [pure Nothing, Just <$> QC.elements (0 : numbers)]
  cutoff <- QC.chooseInt (0, 21)
  let entry n t = Gen.DatedGeneration (Gen.Generation (numbered n) (asciiOsPath ("profile-" <> show n <> "-link"))) (Gen.EpochSeconds (fromIntegral t))
      numbered = Gen.GenerationNumber . fromIntegral
  pure (zipWith entry numbers times, numbered <$> current, Gen.EpochSeconds (fromIntegral cutoff))

-- | The manifest: upstream's names, its version 3 bytes, the versions it
-- reads, and what selects its elements.
testProfileManifest :: IO [Bool]
testProfileManifest = do
  putStrLn "profile/manifest"
  let hello = profileFixture 'a' "hello-2.12.1"
      helloMan = profileFixture 'b' "hello-2.12.1-man"
      world = profileFixture 'c' "world-1.0"
      canonical = storePathToText defaultStoreDir
      helloWorld = manifestOf [("hello", manifestElement [hello]), ("world", manifestElement [world])]
      flake =
        Manifest.Element
          (Set.singleton hello)
          Active
          Manifest.defaultPriority
          (Just (Manifest.FlakeSource "github:o/r" "github:o/r/0123abc" "packages.x86_64-linux.hello" (Just Proto.AllOutputs)))
      names manifest = Map.keys (Manifest.manifestElements manifest)
      parsed text = Manifest.parseManifest (TE.encodeUtf8 text)
      selectedBy selector manifest = case Manifest.selectElements selector manifest of
        Manifest.Selection picked warnings -> (Set.toAscList picked, warnings)
      -- Nothing when a pattern does not compile, which no case expects.
      selected matchers manifest = (`selectedBy` manifest) . Manifest.SelectSome <$> NE.nonEmpty matchers
      regex = maybeToList . Manifest.regexMatcher
      removal args = fmap Cli.optCommand (Cli.parseArgs ("profile" : "remove" : args))
  sequence
    [ runTest "the name part of a derivation name ends at the first dash a non-letter follows, as upstream's DrvName reads one" $
        assertEqual
          "names"
          ["hello", "apache-httpd", "foo", "foo", "foo-", "", "foo-bar", "hello"]
          (map Manifest.drvNameName ["hello-2.12.1", "apache-httpd-2.0.48", "foo-.bar", "foo--bar", "foo-", "-1", "foo-bar", "hello"]),
      runTest "an element is identified by its paths' names, each once, in byte order, an empty one left out" $
        assertEqual "one name" "hello" (Manifest.elementIdentifier (manifestElement [hello, helloMan]))
          `andThen` assertEqual "two names" "hello, world" (Manifest.elementIdentifier (manifestElement [world, hello]))
          `andThen` assertEqual "an empty name" "hello" (Manifest.elementIdentifier (manifestElement [hello, profileFixture 'd' "-1"])),
      runTest "an element a flake made is identified by its reference, attribute and outputs" $
        assertEqual "every output" "github:o/r#packages.x86_64-linux.hello^*" (Manifest.elementIdentifier flake)
          `andThen` assertEqual
            "named outputs"
            "github:o/r#packages.x86_64-linux.hello^dev,out"
            (Manifest.elementIdentifier flake {Manifest.elementSource = (\source -> source {Manifest.sourceOutputs = Proto.namedOutputs (Set.fromList [Proto.OutputName "out", Proto.OutputName "dev"])}) <$> Manifest.elementSource flake}),
      runTest "a name already taken gets -1, -2 and on, as upstream's addElement names it" $
        assertEqual "names" ["hello", "hello-1", "hello-2"] (names (manifestOf (replicate 3 ("hello", manifestElement [hello])))),
      runTest "manifest.json is version 3 as nlohmann's dump writes it: members in name order, paths canonical" $
        assertEqual
          "bytes"
          ( TE.encodeUtf8
              ( "{\"elements\":{\"hello\":{\"active\":true,\"priority\":5,\"storePaths\":[\""
                  <> canonical hello
                  <> "\"]},\"world\":{\"active\":true,\"priority\":5,\"storePaths\":[\""
                  <> canonical world
                  <> "\"]}},\"version\":3}"
              )
          )
          (Manifest.renderManifest helloWorld)
          `andThen` assertEqual
            "a flake element"
            ( TE.encodeUtf8
                ( "{\"elements\":{\"hello\":{\"active\":true,\"attrPath\":\"packages.x86_64-linux.hello\",\"originalUrl\":\"github:o/r\",\"outputs\":[\"*\"],\"priority\":5,\"storePaths\":[\""
                    <> canonical hello
                    <> "\"],\"url\":\"github:o/r/0123abc\"}},\"version\":3}"
                )
            )
            (Manifest.renderManifest (manifestOf [("hello", flake)])),
      runTestM "a version 3 manifest reads back as itself" $
        propertyHolds $
          QC.forAll genManifest $ \manifest ->
            Manifest.parseManifest (Manifest.renderManifest manifest) QC.=== Right manifest,
      runTest "versions 1 and 2 are read, their elements named by their paths" $
        assertEqual
          "version 1"
          (Right (manifestOf [("hello", (manifestElement [hello]) {Manifest.elementPriority = 4})]))
          (parsed ("{\"version\":1,\"elements\":[{\"active\":true,\"priority\":4,\"storePaths\":[\"" <> canonical hello <> "\"],\"uri\":\"\"}]}"))
          `andThen` assertEqual
            "version 2, priority defaulted"
            (Right (manifestOf [("hello", manifestElement [hello]), ("hello-1", (manifestElement [helloMan]) {Manifest.elementActivity = Inactive})]))
            (parsed ("{\"version\":2,\"elements\":[{\"active\":true,\"storePaths\":[\"" <> canonical hello <> "\"]},{\"active\":false,\"storePaths\":[\"" <> canonical helloMan <> "\"]}]}"))
          `andThen` assertEqual
            "version 1's flake element, named from its URL upstream"
            (Left (Manifest.UnnamedFlakeElement "0"))
            (parsed "{\"version\":1,\"elements\":[{\"active\":true,\"storePaths\":[],\"uri\":\"github:o/r\",\"originalUri\":\"github:o/r\",\"attrPath\":\"x\",\"outputs\":null}]}"),
      runTest "a version other than 1 to 3, a missing one included, is refused in upstream's words" $
        assertEqual "4" (Left (Manifest.UnsupportedVersion 4)) (parsed "{\"version\":4,\"elements\":{}}")
          `andThen` assertEqual "missing" (Left (Manifest.UnsupportedVersion 0)) (parsed "{\"elements\":{}}")
          `andThen` assertEqual
            "words"
            "profile manifest \"p/manifest.json\" has unsupported version 4"
            (Manifest.manifestErrorText (asciiOsPath "p/manifest.json") (Manifest.UnsupportedVersion 4)),
      runTest "no elements is an empty profile, and an element must say whether it is active and hold store paths" $
        assertEqual "no elements" (Right Manifest.emptyManifest) (parsed "{\"version\":3}")
          `andThen` assertEqual "no active" (Left (Manifest.MalformedElement "a" "its active is not a Boolean")) (parsed "{\"version\":3,\"elements\":{\"a\":{\"storePaths\":[]}}}")
          `andThen` assertEqual
            "a path outside the store"
            (Left (Manifest.MalformedElement "a" "path '/elsewhere/x' is not in the Nix store"))
            (parsed "{\"version\":3,\"elements\":{\"a\":{\"active\":true,\"storePaths\":[\"/elsewhere/x\"]}}}"),
      runTest "the tree takes the active elements' paths, and the generation references every element's" $
        let manifest = manifestOf [("hello", manifestElement [hello]), ("world", (manifestElement [world]) {Manifest.elementActivity = Inactive, Manifest.elementPriority = 3})]
         in assertEqual "packages" [(hello, Manifest.defaultPriority)] (Manifest.manifestPackages manifest)
              `andThen` assertEqual "references" (Set.fromList [hello, world]) (Manifest.manifestReferences manifest),
      runTest "a name, a store path, a pattern matching the whole identifier ignoring case, or --all selects, as upstream's matchers do" $
        assertEqual "a name" (Just (["world"], [])) (selected [Manifest.MatchName "world"] helloWorld)
          `andThen` assertEqual "a store path" (Just (["hello"], [])) (selected [Manifest.MatchStorePath hello] helloWorld)
          `andThen` assertEqual "a pattern" (Just (["hello"], [])) (selected (regex "HEL+O") helloWorld)
          `andThen` assertEqual "--all" (["hello", "world"], []) (selectedBy Manifest.SelectAll helloWorld)
          `andThen` assertEqual
            "part of an identifier"
            (Just ([], ["Regex 'ell' does not match any packages in the profile.", "Package name 'nope' does not match any packages in the profile.", "Store path '" <> storePathBasename helloMan <> "' does not match any packages in the profile."]))
            (selected (regex "ell" ++ [Manifest.MatchName "nope", Manifest.MatchStorePath helloMan]) helloWorld)
          `andThen` assertEqual "a pattern that does not compile" [] (map Manifest.matcherTitle (regex "(")),
      runTest "an empty profile selects nothing, with upstream's warning" $
        assertEqual "empty" ([], ["There are no packages in the profile."]) (selectedBy Manifest.SelectAll Manifest.emptyManifest),
      runTest "profile remove refuses a line that selects nothing, and --all beside a selector, as usage errors" $
        assertEqual "nothing" (Left "profile remove: no packages specified") (removal [])
          `andThen` assertEqual "--all and a name" (Left "profile remove: --all cannot be used with package names or regular expressions") (removal ["--all", "hello"])
          `andThen` assertEqual "--all twice" (Left "profile remove: --all cannot be used with package names or regular expressions") (removal ["--all", "--all"])
          `andThen` assertEqual "--all" (Right (Cli.CmdProfileRemove Cli.DefaultProfile Cli.RemoveAll)) (removal ["--all"])
          `andThen` assertEqual
            "flags' selectors first, each in order"
            (Right (Cli.CmdProfileRemove Cli.DefaultProfile (Cli.RemoveSome (Cli.RemoveRegex "b" :| [Cli.RemoveRegex "c", Cli.RemoveNamed "a"]))))
            (removal ["a", "--regex", "b", "--regex", "c"]),
      runTest "profile list --json is indented on a terminal unless --pretty or --no-pretty says otherwise, the later winning" $
        let listing args = fmap Cli.optCommand (Cli.parseArgs ("profile" : "list" : args))
            listed format = Right (Cli.CmdProfileList Cli.DefaultProfile format)
         in assertEqual "default" (listed (Cli.ListJson Cli.IndentedOnTerminal)) (listing ["--json"])
              `andThen` assertEqual "--pretty" (listed (Cli.ListJson Cli.Indented)) (listing ["--no-pretty", "--json", "--pretty"])
              `andThen` assertEqual "--no-pretty" (listed (Cli.ListJson Cli.Compact)) (listing ["--pretty", "--no-pretty", "--json"])
              `andThen` assertEqual "without --json" (listed Cli.ListText) (listing ["--pretty"]),
      runTest "a conflict names the first element in name order holding either file as the existing one, and the last as the new one" $
        let other = profileFixture 'e' "other-1"
            storeDir = StoreDir "/s"
            manifest = manifestOf [("hello", manifestElement [hello]), ("other", manifestElement [other])]
            inStore path rest = asciiOsPath (storePathToFilePath storeDir path <> rest)
            fileA = inStore hello "/bin/x"
            fileB = inStore other "/bin/x"
            spelled = Set.singleton . T.pack . storePathToFilePath storeDir
            resolvedDir = StoreDir "/r"
            resolvedA = asciiOsPath (storePathToFilePath resolvedDir hello <> "/bin/x")
         in assertEqual
              "conflict"
              (Just (Manifest.Conflict (Manifest.ConflictSide fileA "hello" (spelled hello)) (Manifest.ConflictSide fileB "other" (spelled other)) 5))
              (Manifest.attributeConflict storeDir storeDir manifest fileA fileB 5)
              `andThen` assertEqual "neither held" Nothing (Manifest.attributeConflict storeDir storeDir manifest (asciiOsPath "/t/x") (asciiOsPath "/t/y") 5)
              `andThen` assertEqual
                "a file in a directory the tree expanded, under the store resolved through its links"
                (Just (Manifest.Conflict (Manifest.ConflictSide resolvedA "hello" (spelled hello)) (Manifest.ConflictSide fileB "other" (spelled other)) 5))
                (Manifest.attributeConflict storeDir resolvedDir manifest resolvedA fileB 5),
      runTest "an element's versions are its paths' version parts, each once, in byte order, an empty one an epsilon and none the empty-set sign" $
        let bare = profileFixture 'g' "tool"
         in assertEqual "two" "2.12.1, 2.12.1-man" (Manifest.elementVersions (manifestElement [helloMan, hello]))
              `andThen` assertEqual "each once" "2.12.1" (Manifest.elementVersions (manifestElement [hello, profileFixture 'h' "hello-2.12.1"]))
              `andThen` assertEqual "an empty version" "\x03B5" (Manifest.elementVersions (manifestElement [bare]))
              `andThen` assertEqual "after every other" "2.12.1, \x03B5" (Manifest.elementVersions (manifestElement [bare, hello]))
              `andThen` assertEqual "no paths" "\x2205" (Manifest.elementVersions (manifestElement [])),
      runTest "a history line names each element added, removed or at other versions, in name order, by its earlier identifier, as upstream's does" $
        let helloTwo = profileFixture 'f' "hello-2.0"
            bare = profileFixture 'g' "tool"
            renamed = profileFixture 'i' "renamed-3"
            earlier = manifestOf [("hello", manifestElement [hello]), ("tool", manifestElement [bare]), ("world", manifestElement [world]), ("x", manifestElement [helloTwo])]
            later = manifestOf [("hello", manifestElement [helloTwo, helloMan]), ("tool", manifestElement [bare]), ("x", manifestElement [renamed]), ("zeta", manifestElement [bare])]
         in assertEqual
              "lines"
              ["hello: 2.12.1 -> 2.0, 2.12.1-man", "world: 1.0 -> \x2205", "hello: 2.0 -> 3", "tool: \x2205 -> \x03B5"]
              (map Manifest.changeLine (Manifest.manifestChanges earlier later))
              `andThen` assertEqual "no changes" [] (Manifest.manifestChanges earlier earlier),
      runTestM "a manifest has no changes from itself, and from or to an empty one each element is added or removed, in name order" $
        propertyHolds $
          QC.forAll genManifest $ \manifest ->
            let elements = Map.elems (Manifest.manifestElements manifest)
                described element = (Manifest.elementIdentifier element, Manifest.elementVersions element)
             in Manifest.manifestChanges manifest manifest QC.=== []
                  QC..&&. Manifest.manifestChanges Manifest.emptyManifest manifest QC.=== map (uncurry Manifest.ElementAdded . described) elements
                  QC..&&. Manifest.manifestChanges manifest Manifest.emptyManifest QC.=== map (uncurry Manifest.ElementRemoved . described) elements
    ]

-- | Generations and where profiles are: names, numbering, the location
-- from the environment, and a generation's NAR.
testProfileGenerations :: IO [Bool]
testProfileGenerations = do
  putStrLn "profile/generations"
  base <- Dir.getTemporaryDirectory >>= OP.encodeFS
  let profileName = asciiOsPath "profile"
      profile = base OP.</> profileName
      number = Gen.GenerationNumber
      at = (base OP.</>) . asciiOsPath
      home = at "home"
      environment stateHome = Gen.ProfileEnvironment stateHome home
      generation n = Gen.Generation (number n) (at ("profile-" <> show n <> "-link"))
      target = at "store-path"
  sequence
    [ runTestM "a generation's name reads back as its number" $
        propertyHolds $
          QC.forAll (QC.arbitrary :: QC.Gen Word64) $ \n ->
            Gen.parseGenerationName profileName (OP.takeFileName (Gen.generationName profile (number n))) QC.=== Just (number n),
      runTest "a name is the profile's, a dash, and a number before the first -link, as upstream's parseName reads one" $
        assertEqual
          "names"
          [Just (number 3), Just (number 3), Just (number 3), Just (number 3), Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
          ( map
              (Gen.parseGenerationName profileName . asciiOsPath)
              ["profile-3-link", "profile-3-link.tmp", "profile-+3-link", "profile-03-link", "profile--3-link", "profile-3", "profile-x-link", "profiles-3-link", "profile-18446744073709551616-link", "profile--link"]
          ),
      runTest "a profile's generations are the names of its directory it parses, in order of number" $
        assertEqual
          "generations"
          [generation 1, generation 2, generation 10]
          (Gen.generationsIn profile (map asciiOsPath ["profile-10-link", "profile", "profile.lock", "profile-2-link", "other-1-link", "profile-1-link"])),
      runTest "the next generation is one past the newest, or the newest when it already names the path" $
        assertEqual "first" (Gen.NewGeneration (number 1)) (Gen.nextGeneration Nothing target)
          `andThen` assertEqual "the same path" (Gen.ReuseGeneration (generation 3)) (Gen.nextGeneration (Just (generation 3, target)) target)
          `andThen` assertEqual "another path" (Gen.NewGeneration (number 4)) (Gen.nextGeneration (Just (generation 3, at "other")) target),
      runTest "the state directory is NIX_STATE_HOME's, else XDG_STATE_HOME's nix, else the home's .local/state/nix, and on Windows Local AppData's nix\\state" $
        assertEqual "NIX_STATE_HOME" (at "s") (Gen.stateDirectory (environment (Gen.NixStateHome (at "s"))))
          `andThen` assertEqual "XDG_STATE_HOME" (at "x" OP.</> asciiOsPath "nix") (Gen.stateDirectory (environment (Gen.XdgStateHome (Just (at "x")))))
          `andThen` assertEqual "the home" (home OP.</> asciiOsPath ".local" OP.</> asciiOsPath "state" OP.</> asciiOsPath "nix") (Gen.stateDirectory (environment (Gen.XdgStateHome Nothing)))
          `andThen` assertEqual "Local AppData" (at "l" OP.</> asciiOsPath "nix" OP.</> asciiOsPath "state") (Gen.stateDirectory (environment (Gen.LocalAppDataHome (at "l")))),
      runTest "profiles are in the state directory's profiles, the default one profile, linked from ~/.nix-profile or with XDG directories <state>/profile" $
        let env = environment (Gen.NixStateHome (at "s"))
         in assertEqual "profiles" (at "s" OP.</> asciiOsPath "profiles") (Gen.profilesDirectory env)
              `andThen` assertEqual "default" (at "s" OP.</> asciiOsPath "profiles" OP.</> asciiOsPath "profile") (Gen.defaultProfilePath env)
              `andThen` assertEqual "home link" (home OP.</> asciiOsPath ".nix-profile") (Gen.profileLinkPath Gen.HomeProfileLink env)
              `andThen` assertEqual "state link" (at "s" OP.</> asciiOsPath "profile") (Gen.profileLinkPath Gen.StateProfileLink env),
      runTest "a path is made absolute and collapsed without reading a link, as upstream's absPath makes one" $
        assertEqual "relative" (at "d") (Gen.absolutePath base (asciiOsPath "c" OP.</> asciiOsPath ".." OP.</> asciiOsPath "d"))
          `andThen` assertEqual "a dot" (at "c") (Gen.absolutePath base (asciiOsPath "." OP.</> asciiOsPath "c"))
          `andThen` assertEqual "absolute" (at "x") (Gen.absolutePath (at "elsewhere") (at "x" OP.</> asciiOsPath "y" OP.</> asciiOsPath ".."))
          -- Windows' temporary directory ends in a separator, which
          -- takeDirectory alone would drop instead of the last component.
          `andThen` assertEqual "up" (OP.takeDirectory (OP.dropTrailingPathSeparator base)) (Gen.absolutePath base (asciiOsPath "..")),
      runTest "a generation's NAR is its tree and manifest.json, each link's target as the NAR spells it" $
        let tree =
              Map.fromList
                [ (asciiOsPath "bin", Directory (Map.fromList [(asciiOsPath "hello", Link (asciiOsPath "/s/a/bin/hello") 5)])),
                  (asciiOsPath "share", Link (asciiOsPath "/s/b/share") 5)
                ]
            expected =
              NAR.NarDirectory
                [ ("bin", NAR.NarDirectory [("hello", NAR.NarSymlink "/s/a/bin/hello")]),
                  ("manifest.json", NAR.NarRegular False "{}"),
                  ("share", NAR.NarSymlink "/s/b/share")
                ]
         in assertEqual "NAR" (Right (NAR.serialise expected)) (NAR.serialise <$> Gen.generationNar Gen.TargetBytes tree "{}"),
      runTest "on Windows a link target's backslashes are written as slashes, as nova-cache's serialiser writes one, and elsewhere its bytes are kept" $
        let windowsTree = Map.fromList [(asciiOsPath "bin", Link (asciiOsPath "C:\\nix\\store\\a\\bin") 5)]
            windowsNar = NAR.NarDirectory [("bin", NAR.NarSymlink "C:/nix/store/a/bin"), ("manifest.json", NAR.NarRegular False "{}")]
         in assertEqual "slashed" "C:/nix/store/a/bin" (Gen.narLinkTarget Gen.SlashedTargets "C:\\nix\\store\\a\\bin")
              `andThen` assertEqual "bytes" "a\\b" (Gen.narLinkTarget Gen.TargetBytes "a\\b")
              `andThen` assertEqual "a Windows generation" (Right (NAR.serialise windowsNar)) (NAR.serialise <$> Gen.generationNar Gen.SlashedTargets windowsTree "{}")
              `andThen` assertEqual "this host" (if SI.os == "mingw32" then Gen.SlashedTargets else Gen.TargetBytes) Gen.hostNarTargets,
      runTest "profile history, rollback and wipe-history take their flags, every --to kept in order and the last --older-than" $
        let parsed args = fmap Cli.optCommand (Cli.parseArgs ("profile" : args))
         in assertEqual "history" (Right (Cli.CmdProfileHistory (Cli.ProfileAt "p"))) (parsed ["history", "--profile", "p"])
              `andThen` assertEqual "rollback" (Right (Cli.CmdProfileRollback Cli.DefaultProfile [] Gen.MakeChanges)) (parsed ["rollback"])
              `andThen` assertEqual "every --to, and --dry-run" (Right (Cli.CmdProfileRollback Cli.DefaultProfile ["2", "3"] Gen.DryRun)) (parsed ["rollback", "--to", "2", "--dry-run", "--to", "3"])
              `andThen` assertEqual "the last --older-than" (Right (Cli.CmdProfileWipeHistory Cli.DefaultProfile (Just "14d") Gen.MakeChanges)) (parsed ["wipe-history", "--older-than", "x", "--older-than", "14d"])
              `andThen` assertEqual "the other's flag" (Left "unknown profile rollback argument: --older-than") (parsed ["rollback", "--older-than", "1d"])
              `andThen` assertEqual "history changes nothing to dry-run" (Left "unknown profile history argument: --dry-run") (parsed ["history", "--dry-run"])
              `andThen` assertEqual "--to with no value" (Left "--to requires a value") (parsed ["rollback", "--to"]),
      runTest "the current generation is the one the profile's link names by its bare name, and none for another target or no link, as upstream reads it" $
        assertEqual "bare" (Just (number 3)) (Gen.currentGeneration profile (Just (asciiOsPath "profile-3-link")))
          `andThen` assertEqual "the -link quirk" (Just (number 3)) (Gen.currentGeneration profile (Just (asciiOsPath "profile-3-link.tmp")))
          `andThen` assertEqual "absolute" Nothing (Gen.currentGeneration profile (Just (Gen.generationName profile (number 3))))
          `andThen` assertEqual "another profile's" Nothing (Gen.currentGeneration profile (Just (asciiOsPath "other-3-link")))
          `andThen` assertEqual "no link" Nothing (Gen.currentGeneration profile Nothing),
      runTest "a rollback switches to the newest generation below the current one, or the one --to names, and is refused in upstream's terms otherwise" $
        let listed = map generation [1, 2, 3, 5]
            plan wanted current = Gen.rollbackPlan wanted (number <$> current) listed
            twoNames = Gen.generationsIn profile (map asciiOsPath ["profile-3-link.tmp", "profile-3-link"])
         in assertEqual "below 5" (Right (generation 3)) (plan Gen.PreviousGeneration (Just 5))
              `andThen` assertEqual "below an unlisted 4" (Right (generation 3)) (plan Gen.PreviousGeneration (Just 4))
              `andThen` assertEqual "below the oldest" (Left (Gen.NoOlderGeneration (Just (number 1)))) (plan Gen.PreviousGeneration (Just 1))
              `andThen` assertEqual "no current generation" (Left (Gen.NoOlderGeneration Nothing)) (plan Gen.PreviousGeneration Nothing)
              `andThen` assertEqual "--to 2" (Right (generation 2)) (plan (Gen.GenerationAt (number 2)) (Just 5))
              `andThen` assertEqual "--to the current one" (Right (generation 5)) (plan (Gen.GenerationAt (number 5)) (Just 5))
              `andThen` assertEqual "--to an unlisted 4" (Left (Gen.NoSuchGeneration (number 4))) (plan (Gen.GenerationAt (number 4)) (Just 5))
              `andThen` assertEqual "of two names for one number, the later" (Right (Gen.Generation (number 3) (at "profile-3-link.tmp"))) (Gen.rollbackPlan (Gen.GenerationAt (number 3)) Nothing twoNames),
      runTestM "a rollback picks a listed generation below the current one with none between them, or one of the number asked for, and is refused only when there is none" $
        propertyHolds $
          QC.forAll genDatedListing $ \(listing, current, _) ->
            let listed = map Gen.datedGeneration listing
                numbers = map Gen.generationNumber listed
                between low high n = low < n && n < high
                previous = case (Gen.rollbackPlan Gen.PreviousGeneration current listed, current) of
                  (Right chosen, Just now) ->
                    let n = Gen.generationNumber chosen
                     in QC.counterexample "the generation before" (chosen `elem` listed && n < now && not (any (between n now) numbers))
                  (Left (Gen.NoOlderGeneration refused), _) -> QC.counterexample "refused" (refused == current && maybe True (\now -> all (>= now) numbers) current)
                  (other, _) -> QC.counterexample (show other) False
                asked = QC.conjoin [QC.counterexample (show n) (either (const (n `notElem` numbers)) ((== n) . Gen.generationNumber) (Gen.rollbackPlan (Gen.GenerationAt n) current listed)) | n <- map number [0 .. 30]]
             in previous QC..&&. asked,
      runTest "--to reads an unsigned number with an optional binary unit, refused in upstream's words otherwise" $
        assertEqual
          "values"
          [Right 3, Right 3, Right 2048, Right 1048576, Right 0, Left "'-1' is not an integer", Left "invalid unit specifier 'X'", Left "'1.5' is not an integer", Left "'' is not an integer", Left "'' is not an integer", Left "'18446744073709551616' is not an integer", Left "' 3' is not an integer"]
          (map (fmap (\(Gen.GenerationNumber n) -> n) . Gen.readGenerationNumber) ["3", "+3", "2k", "1M", "16777216T", "-1", "5x", "1.5K", "", "K", "18446744073709551616", " 3"]),
      runTest "--older-than reads a number of days from 1 followed by d, refused in upstream's two messages otherwise" $
        let unsuffixed spec = Left ("invalid number of days specifier '" <> spec <> "', expected something like '14d'")
            unread spec = Left ("invalid number of days specifier '" <> spec <> "'")
         in assertEqual
              "values"
              [Right (Gen.Days 14), Right (Gen.Days 3), Right (Gen.Days maxBound), unsuffixed "14", unsuffixed "", unsuffixed "14D", unread "d", unread "0d", unread "-1d", unread "1.5d", unread "2147483648d", unread " 1d"]
              (map Gen.parseOlderThan ["14d", "+3d", "2147483647d", "14", "", "14D", "d", "0d", "-1d", "1.5d", "2147483648d", " 1d"]),
      runTest "the cutoff is that many days of 86400 seconds before now, with no overflow at the largest count" $
        assertEqual "two days" (Gen.EpochSeconds (1000000 - 2 * 86400)) (Gen.cutoffBefore (Gen.EpochSeconds 1000000) (Gen.Days 2))
          `andThen` assertEqual "the largest" (Gen.EpochSeconds (negate (fromIntegral (maxBound :: Int32) * 86400))) (Gen.cutoffBefore (Gen.EpochSeconds 0) (Gen.Days maxBound)),
      runTest "a generation's date is its day in UTC" $
        assertEqual
          "dates"
          ["1970-01-01", "1970-01-01", "1970-01-02", "2020-04-10", "1969-12-31"]
          (map (Gen.generationDate . Gen.EpochSeconds) [0, 86399, 86400, 1586520000, -1]),
      runTest "wiping the history deletes every generation but the current one, in order of number, and all of them when none is current" $
        let listed = [dated 1 0, dated 2 0, dated 3 0]
         in assertEqual "current 2" (map Gen.RemoveGeneration [number 1, number 3]) (Gen.wipePlan Gen.WipeEveryOld (Just (number 2)) listed)
              `andThen` assertEqual "none current" (map (Gen.RemoveGeneration . number) [1, 2, 3]) (Gen.wipePlan Gen.WipeEveryOld Nothing listed)
              `andThen` assertEqual "an unlisted current one" (map (Gen.RemoveGeneration . number) [1, 2, 3]) (Gen.wipePlan Gen.WipeEveryOld (Just (number 9)) listed),
      runTest "under a cutoff the newest generation made before it stays, those before it go newest first, and one made after it stays with a warning" $
        let listed = [dated 1 100, dated 2 200, dated 3 300, dated 4 400]
            older cutoff current = Gen.wipePlan (Gen.WipeOlderThan (Gen.EpochSeconds cutoff)) (number <$> current) listed
         in assertEqual "the newest before 250 is 2" [Gen.RemoveGeneration (number 1)] (older 250 (Just 4))
              `andThen` assertEqual "the current one stays" [] (older 250 (Just 1))
              `andThen` assertEqual "none before the cutoff" [] (older 50 (Just 4))
              `andThen` assertEqual "every one before it, the newest kept" (map (Gen.RemoveGeneration . number) [3, 2, 1]) (older 1000 (Just 4))
              `andThen` assertEqual "a generation as old as the cutoff is not before it" [Gen.RemoveGeneration (number 1)] (older 300 (Just 4))
              `andThen` assertEqual
                "one made after the cutoff, below one made before it"
                [Gen.KeepNewerGeneration (number 1)]
                (Gen.wipePlan (Gen.WipeOlderThan (Gen.EpochSeconds 250)) (Just (number 3)) [dated 1 500, dated 2 200, dated 3 300]),
      runTestM "wiping the history never deletes the current generation; under a cutoff it deletes exactly the ones made before it below the newest of those, and warns of the others below it" $
        propertyHolds . QC.checkCoverage $
          QC.forAll genDatedListing $ \(listing, current, cutoff) ->
            let numbers = map (Gen.generationNumber . Gen.datedGeneration) listing
                everyOld = Gen.wipePlan Gen.WipeEveryOld current listing
                steps = Gen.wipePlan (Gen.WipeOlderThan cutoff) current listing
                removed = [n | Gen.RemoveGeneration n <- steps]
                warned = [n | Gen.KeepNewerGeneration n <- steps]
                madeBefore entry = Gen.datedTime entry < cutoff
                newestBefore = listToMaybe (reverse [Gen.generationNumber (Gen.datedGeneration entry) | entry <- listing, madeBefore entry])
                below = [entry | Just newest <- [newestBefore], entry <- reverse listing, Gen.generationNumber (Gen.datedGeneration entry) < newest]
                numberOf = Gen.generationNumber . Gen.datedGeneration
             in QC.cover 20 (not (null removed)) "some generation is deleted"
                  $ QC.cover 5 (not (null warned)) "a generation is kept with a warning"
                  $ QC.cover 5 (any ((== current) . Just . numberOf) (filter madeBefore below)) "the current generation is below the newest made before the cutoff"
                  $ QC.counterexample "every old one" (everyOld == [Gen.RemoveGeneration n | n <- numbers, Just n /= current])
                    QC..&&. QC.counterexample "the current one" (all ((/= current) . Just) removed)
                    QC..&&. QC.counterexample "deleted" (removed == [numberOf entry | entry <- below, madeBefore entry, Just (numberOf entry) /= current])
                    QC..&&. QC.counterexample "warned" (warned == [numberOf entry | entry <- below, not (madeBefore entry)])
    ]
  where
    dated n seconds = Gen.DatedGeneration (Gen.Generation (Gen.GenerationNumber n) (asciiOsPath ("profile-" <> show n <> "-link"))) (Gen.EpochSeconds seconds)

-- | A profile changed through the library against a store on disk: the
-- default profile's directory and link made, generations made and
-- switched to and rooted, an add that finds the profile changed started
-- again, and a profile of another store, or one only nix-env's manifest
-- describes, refused.
testProfileUpdateIO :: IO [Bool]
testProfileUpdateIO = do
  putStrLn "profile/update"
  canLink <- symlinksAvailable
  tmpBase <- getTemporaryDirectory
  let root = tmpBase </> "nova-nix-test-profile-update"
  if not canLink
    then [] <$ putStrLn "  SKIP  a profile's links need symlink privilege (Windows needs Developer Mode or elevation)"
    else bracket_ (forceRemoveIfExists root >> createDirectoryIfMissing True root) (forceRemoveIfExists root) $ do
      rootPath <- OP.encodeFS root
      store <- openStore testLogger (StoreDir (root </> "store"))
      let storeDir = stDir store
          stage name = root </> "stage" </> name
          hello = profileFixture 'a' "hello-1.0"
          world = profileFixture 'b' "world-1.0"
          writeStaged path bytes = createDirectoryIfMissing True (takeDirectory path) *> BS.writeFile path bytes
          env = Gen.ProfileEnvironment (Gen.NixStateHome (rootPath OP.</> asciiOsPath "state")) (rootPath OP.</> asciiOsPath "home")
          adding name path manifest = pure (Gen.Changed (Manifest.addElement name (manifestElement [path]) manifest))
          names = fmap (Map.keys . Manifest.manifestElements)
      writeStaged (stage "hello" </> "bin" </> "hello") "hello"
      writeStaged (stage "world" </> "bin" </> "world") "world"
      addToStore store (stage "hello") hello Nothing []
      addToStore store (stage "world") world Nothing []
      closeStore store
      createDirectoryIfMissing True (root </> "home")
      defaulted <- Gen.resolveDefaultProfile Gen.HomeProfileLink env
      firstChanged <- newIORef True
      let osLinkIfThere path = either (\(_ :: IOException) -> Nothing) Just <$> try (OsDir.getSymbolicLinkTarget path)
          profile = Gen.defaultProfilePath env
          otherProfile = rootPath OP.</> asciiOsPath "other" OP.</> asciiOsPath "profile"
          nixEnvProfile = rootPath OP.</> asciiOsPath "nix-env-profile"
          sourceNar = fromRight (NAR.NarDirectory []) (Gen.generationNar Gen.hostNarTargets Map.empty "{}")
          generations = fmap (map (\(Gen.Generation (Gen.GenerationNumber n) _) -> n))
      linkTarget <- osLinkIfThere (rootPath OP.</> asciiOsPath "home" OP.</> asciiOsPath ".nix-profile")
      (added, restarted, reused, firstManifest, lastManifest, sourceAdds, sourceRefs) <- withLocalStore testLogger (defaultBuildConfig storeDir) $ \ops -> do
        let -- The first add the store is asked for runs another change of
            -- the profile to completion before it answers, as a command
            -- running beside this one would.
            interrupted =
              ops
                { soAddSourceTree = \name refs entry -> do
                    first <- atomicModifyIORef' firstChanged (False,)
                    when first (void (Gen.updateProfile testLogger ops storeDir profile (adding "world" world)))
                    soAddSourceTree ops name refs entry
                }
        added <- Gen.updateProfile testLogger ops storeDir profile (adding "hello" hello)
        restarted <- Gen.updateProfile testLogger interrupted storeDir profile (adding "again" hello)
        -- The same manifest again, under a spelling of the store directory
        -- that collapses to the store's own.  A spelling through a link is
        -- another store to the database, which keys rows by the spelling
        -- ('Nix.Store.Handle.canonicalStoreDir').
        reused <- Gen.updateProfile testLogger ops (StoreDir (unStoreDir storeDir </> ".")) profile (pure . Gen.Changed)
        firstManifest <- Gen.readManifest (Gen.generationName profile (Gen.GenerationNumber 1))
        lastManifest <- Gen.readManifest profile
        sourceAdds <- replicateM 2 (soAddSourceTree ops Gen.generationStoreName (Set.fromList [hello, world]) sourceNar)
        sourceRefs <- either (pure . Left) (sqReferences (soQueries ops)) (fromMaybe (Left "no add") (listToMaybe sourceAdds))
        pure (added, restarted, reused, firstManifest, lastManifest, sourceAdds, sourceRefs)
      createDirectoryIfMissing True (root </> "other")
      refusedOther <- withLocalStore testLogger (defaultBuildConfig (StoreDir (root </> "other-store"))) $ \otherOps -> do
        made <- Gen.updateProfile testLogger otherOps (StoreDir (root </> "other-store")) otherProfile (\_ -> pure (Gen.Changed Manifest.emptyManifest))
        withLocalStore testLogger (defaultBuildConfig storeDir) $ \ops -> (,) made <$> Gen.updateProfile testLogger ops storeDir otherProfile (adding "hello" hello)
      missingDirectory <- withLocalStore testLogger (defaultBuildConfig storeDir) $ \ops ->
        Gen.updateProfile testLogger ops storeDir (rootPath OP.</> asciiOsPath "missing" OP.</> asciiOsPath "profile") (\_ -> pure (Gen.Changed Manifest.emptyManifest))
      createDirectoryIfMissing True (root </> "nix-env-tree")
      BS.writeFile (root </> "nix-env-tree" </> "manifest.nix") "[ ]"
      Dir.createDirectoryLink (root </> "nix-env-tree") (root </> "nix-env-profile")
      nixEnv <- Gen.readManifest nixEnvProfile
      Dir.createDirectoryLink (root </> "store") (root </> "store-link")
      throughLink <- Gen.checkProfileStore (StoreDir (root </> "store-link")) profile
      elsewhere <- Gen.checkProfileStore (StoreDir (root </> "other-store")) profile
      -- A store opened through a linked directory: the tree links a
      -- directory it expands at its resolved spelling and everything else
      -- at the store's, so a conflict's files are under both.
      createDirectoryIfMissing True (root </> "real")
      Dir.createDirectoryLink (root </> "real") (root </> "via")
      viaStore <- openStore testLogger (StoreDir (root </> "via" </> "store"))
      let viaDir = stDir viaStore
          alpha = profileFixture 'c' "alpha-1.0"
          beta = profileFixture 'd' "beta-1.0"
          viaProfile = rootPath OP.</> asciiOsPath "via-profiles" OP.</> asciiOsPath "profile"
      writeStaged (stage "alpha" </> "bin" </> "hello") "alpha"
      writeStaged (stage "alpha" </> "bin" </> "a") "a"
      writeStaged (stage "beta" </> "bin" </> "hello") "beta"
      addToStore viaStore (stage "alpha") alpha Nothing []
      addToStore viaStore (stage "beta") beta Nothing []
      closeStore viaStore
      createDirectoryIfMissing True (root </> "via-profiles")
      (viaAdded, viaAgain, viaConflict) <- withLocalStore testLogger (defaultBuildConfig viaDir) $ \ops -> do
        viaAdded <- Gen.updateProfile testLogger ops viaDir viaProfile (adding "alpha" alpha)
        viaAgain <- Gen.updateProfile testLogger ops viaDir viaProfile (pure . Gen.Changed)
        viaConflict <- Gen.updateProfile testLogger ops viaDir viaProfile (adding "beta" beta)
        pure (viaAdded, viaAgain, viaConflict)
      -- A directory standing at a generation's name, with a file in it, in
      -- a profile whose link names generation 2.
      let wiped = rootPath OP.</> asciiOsPath "wiped" OP.</> asciiOsPath "profile"
      createDirectoryIfMissing True (root </> "wiped" </> "profile-1-link")
      BS.writeFile (root </> "wiped" </> "profile-1-link" </> "kept") "kept"
      Dir.createDirectoryLink "profile-2-link" (root </> "wiped" </> "profile")
      wipedDirectory <- Gen.wipeHistory testLogger wiped Gen.WipeEveryOld Gen.MakeChanges
      directoryKept <- Dir.doesFileExist (root </> "wiped" </> "profile-1-link" </> "kept")
      linked <- osLinkIfThere profile
      helloRun <- either (\(_ :: IOException) -> Nothing) Just <$> try (BS.readFile (root </> "home" </> ".nix-profile" </> "bin" </> "hello"))
      gens <- Gen.listGenerations profile
      sequence
        [ runTest "the default profile's directory and link are made, the link naming profiles/profile" $
            assertEqual "default" (Right profile) defaulted
              `andThen` assertEqual "link" (Just profile) linkTarget,
          runTest "a change makes generation 1 and links the profile to it by its bare name" $
            assertEqual "switched" (Right (Gen.SwitchedTo (Gen.Generation (Gen.GenerationNumber 1) (Gen.generationName profile (Gen.GenerationNumber 1))))) added
              `andThen` assertEqual "generation 1 holds hello" (Right ["hello"]) (names firstManifest),
          runTest "an add that finds the profile changed while it worked starts again, keeping the other change" $
            case restarted of
              Right (Gen.SwitchedTo (Gen.Generation (Gen.GenerationNumber 3) _)) ->
                assertEqual "link" (Just (asciiOsPath "profile-3-link")) linked
                  `andThen` assertEqual "generations" (Right [1, 2, 3]) (generations gens)
                  `andThen` assertEqual "elements" (Right ["again", "hello", "world"]) (names lastManifest)
              other -> Fail (T.pack (show other)),
          runTest "the same manifest again keeps the newest generation, under a spelling of the store directory that collapses to its own" $
            case reused of
              Right (Gen.SwitchedTo (Gen.Generation (Gen.GenerationNumber 3) _)) -> assertEqual "generations" (Right [1, 2, 3]) (generations gens)
              other -> Fail (T.pack (show other)),
          runTest "a store opened through a linked directory keeps the newest generation for the same manifest" $
            case (viaAdded, viaAgain) of
              (Right (Gen.SwitchedTo first), Right (Gen.SwitchedTo again)) -> assertEqual "generation" first again
              other -> Fail (T.pack (show other)),
          runTest "a conflict in a store opened through a linked directory names the package already there as the existing one" $
            case viaConflict of
              Left (Gen.TreeConflict (Manifest.Conflict (Manifest.ConflictSide _ existing _) (Manifest.ConflictSide _ new _) _)) ->
                assertEqual "existing, new" ("alpha", "beta") (existing, new)
              other -> Fail (T.pack (show other)),
          runTest "the profile's tree is in ~/.nix-profile" $ assertEqual "bin/hello" (Just "hello") helloRun,
          runTest "a tree added by its NAR is the source path its references make, added again as itself, its references recorded" $
            case sourceAdds of
              [Right first, Right second] ->
                assertEqual "path" (Hash.makeSourcePath "profile" (sha256Digest (NAR.serialise sourceNar)) (Set.fromList [hello, world])) (Right first)
                  `andThen` assertEqual "again" first second
                  `andThen` assertEqual "references" (Right (Set.fromList [hello, world])) sourceRefs
              other -> Fail (T.pack (show other)),
          runTest "a profile whose current generation is in another store is refused" $
            case refusedOther of
              (Right (Gen.SwitchedTo _), Left (Gen.ForeignGeneration refused _)) -> assertEqual "profile" otherProfile refused
              other -> Fail (T.pack (show other)),
          runTest "a store reached through a linked directory is the profile's own, and another store is not" $
            assertEqual "through a link" (Right ()) throughLink
              `andThen` case elsewhere of
                Left (Gen.ForeignGeneration refused _) -> assertEqual "profile" profile refused
                other -> Fail (T.pack (show other)),
          runTest "a profile only nix-env's manifest.nix describes is refused" $
            assertEqual "nix-env" (Left (Gen.NixEnvProfile nixEnvProfile)) nixEnv,
          runTest "a profile whose directory is not there fails as a value, at its lock file" $
            case missingDirectory of
              Left (Gen.ProfileIOFailed reason) | "opening lock file" `T.isPrefixOf` reason -> Pass
              other -> Fail (T.pack (show other)),
          runTest "wiping the history unlinks a generation's name and never removes a directory standing there" $
            case wipedDirectory of
              Left (Gen.ProfileIOFailed reason) | "removing \"" `T.isPrefixOf` reason -> assertEqual "the directory's file" True directoryKept
              other -> Fail (T.pack (show other))
        ]

-- | Step 2 of the ladder: a dependency-aware build end-to-end.  A root
-- derivation depends on a leaf; both @.drv@ files are pre-written to the store
-- (as the build driver's closure-writing does), and 'buildWithDeps' must read
-- the closure, topologically order it, build the leaf first, then the root -
-- whose 'validateInputs' requires the leaf's realized output to be valid.
--
-- This is the regression guard for the input-@.drv@-closure fix: before it, no
-- derivation with a non-empty @drvInputDrvs@ could be realized (the closure was
-- never on disk, so the dependency graph could not be read).
testDependentBuildIO :: IO [Bool]
testDependentBuildIO = do
  putStrLn "builder/dependent-native-build"
  withTempStore $ \store -> do
    let storeDir = stDir store
        depOut = StorePath (T.replicate 31 "c" <> "0") "dep"
        depDrvSP = StorePath (T.replicate 31 "c" <> "1") "dep.drv"
        rootOut = StorePath (T.replicate 31 "a" <> "0") "root"
        rootDrvSP = StorePath (T.replicate 31 "a" <> "1") "root.drv"
        mkBuilder word
          | SI.os == "mingw32" = ("cmd.exe", ["/c", "echo " <> word <> ">%out%"])
          | otherwise = ("/bin/sh", ["-c", "echo " <> word <> " > $out"])
        (depBuilder, depArgs) = mkBuilder "leaf"
        (rootBuilder, rootArgs) = mkBuilder "root"
        depDrv =
          Derivation
            { drvOutputs = [DerivationOutput {doName = "out", doPath = depOut, doHashAlgo = "", doHash = ""}],
              drvInputDrvs = Map.empty,
              drvInputSrcs = [],
              drvPlatform = currentPlatform,
              drvBuilder = depBuilder,
              drvArgs = depArgs,
              drvEnv = Map.singleton "name" "dep"
            }
        rootDrv =
          Derivation
            { drvOutputs = [DerivationOutput {doName = "out", doPath = rootOut, doHashAlgo = "", doHash = ""}],
              drvInputDrvs = Map.singleton depDrvSP ["out"],
              drvInputSrcs = [],
              drvPlatform = currentPlatform,
              drvBuilder = rootBuilder,
              drvArgs = rootArgs,
              drvEnv = Map.singleton "name" "root"
            }
    -- The build driver writes the full .drv closure before building; emulate
    -- that here by writing both recipes to the store.
    writeDrv store depDrv depDrvSP
    writeDrv store rootDrv rootDrvSP
    let config = defaultBuildConfig storeDir
    result <- buildWithDeps config store rootDrv rootDrvSP
    depValid <- isValid store depOut
    rootValid <- isValid store rootOut
    case result of
      BuildSuccess sp ->
        sequence
          [ runTest "dependent build succeeds with root output" $
              assertEqual "root output path" rootOut sp,
            runTest "leaf dependency built and registered first" $
              if depValid then Pass else Fail "leaf output not valid after build",
            runTest "root output built and registered" $
              if rootValid then Pass else Fail "root output not valid after build"
          ]
      BuildFailure msg code ->
        sequence
          [ runTest "dependent build succeeds with root output" $
              Fail ("dependency-aware build failed (exit " <> T.pack (show code) <> "): " <> msg)
          ]

-- ---------------------------------------------------------------------------
-- Tests: store operations
-- ---------------------------------------------------------------------------

-- | 'buildWithDeps' under 'Realise.BuildCheck', and 'checkDerivation',
-- against a real store: the builder runs again beside the valid outputs,
-- what it writes is compared with them, and the rebuild leaves the store
-- as it was whatever the comparison finds, but under keep-failed the
-- rebuild of an output that differs, kept beside it.  A rebuild that
-- differs is a derivation with the same output paths and another script,
-- so no case depends on a builder that is itself nondeterministic.
testBuildCheckIO :: IO [Bool]
testBuildCheckIO = do
  putStrLn "builder/check"
  shell <- findTestShell
  tmpBase <- getTemporaryDirectory
  let inStore :: String -> (Store -> BuildConfig -> IO TestResult) -> IO TestResult
      inStore label action = do
        let root = tmpBase </> ("nova-nix-test-check-" <> label)
        forceRemoveIfExists root
        createDirectoryIfMissing True root
        store <- openStore testLogger (StoreDir (root </> "store"))
        action store (defaultBuildConfig (stDir store))
          `finally` (closeStore store >> forceRemoveIfExists root)
      checking config = config {bcMode = Realise.BuildCheck}
      keeping config = config {bcFailedBuilds = KeepFailed}
      -- What a check has to leave as it found it: the store directory's
      -- entries, where a scratch tree would show, and what the database
      -- records of the outputs.
      snapshot store outputs = do
        entries <- sort <$> Dir.listDirectory (unStoreDir (stDir store))
        infos <- mapM (queryPathInfo (stDB store)) outputs
        pure (entries, infos)
      fixture tag suffix = StorePath (T.replicate 31 tag <> suffix)
      -- A derivation with two outputs, each under its name.
      twoOutputs :: BS.ByteString -> (Text, StorePath) -> (Text, StorePath) -> BS.ByteString -> Derivation
      twoOutputs name (firstName, firstSP) (secondName, secondSP) script =
        Derivation
          { drvOutputs = [DerivationOutput firstName firstSP "" "", DerivationOutput secondName secondSP "" ""],
            drvInputDrvs = Map.empty,
            drvInputSrcs = [],
            drvPlatform = currentPlatform,
            drvBuilder = TE.encodeUtf8 shell,
            drvArgs = ["-c", script],
            drvEnv = Map.union (Map.fromList [("name", name)]) hostToolsEnv
          }
      -- Output a records output b's path, and each holds the word.
      bothWriting word = "mkdir -p $a $b && echo $b > $a/sibling && echo " <> word <> " > $a/value && echo " <> word <> " > $b/value"
      differs store drvSP output =
        BuildFailure
          ( "derivation '"
              <> storePathToText defaultStoreDir drvSP
              <> "' may not be deterministic: output \""
              <> T.pack (storePathToFilePath (stDir store) output)
              <> "\" differs"
          )
          104
      -- Upstream's message under keep-failed, naming the rebuild it kept.
      differsFromKept store drvSP output =
        let outPath = T.pack (storePathToFilePath (stDir store) output)
         in BuildFailure ("derivation '" <> storePathToText defaultStoreDir drvSP <> "' may not be deterministic: output \"" <> outPath <> "\" differs from \"" <> outPath <> ".check\"") 104
      keptPath store output = storePathToFilePath (stDir store) output <> ".check"
      -- The name a kept rebuild has in the store directory, as a store
      -- path, to show the database holds no row for it.
      keptName output = StorePath (spHash output) (spName output <> ".check")
  sequence
    [ -- The rebuild writes the scratch path it was handed into the output,
      -- so it matches only once that path's hash is rewritten to the
      -- output's own, as upstream rewrites it before it hashes the tree.
      runTestM "a check passes for a derivation that builds the same again, its own path in the output included" $
        inStore "same" $ \store config -> do
          let outSP = fixture "k" "1" "check-same"
              drvSP = fixture "k" "2" "check-same.drv"
              drv = mkTestBuildDrv shell outSP "mkdir -p $out && echo fixed > $out/value && echo $out > $out/self"
              outPath = storePathToFilePath (stDir store) outSP
          built <- buildWithDeps config store drv drvSP
          before <- snapshot store [outSP]
          checked <- buildWithDeps (checking config) store drv drvSP
          after <- snapshot store [outSP]
          self <- T.strip <$> TIO.readFile (outPath </> "self")
          pure $
            assertEqual "build" (BuildSuccess outSP) built
              `andThen` assertEqual "check" (BuildSuccess outSP) checked
              `andThen` assertEqual "store" before after
              `andThen` assertEqual "self" (T.pack outPath) self,
      runTestM "a check fails with upstream's message and status for a derivation that builds differently, and the store keeps what it had" $
        inStore "differs" $ \store config -> do
          let outSP = fixture "k" "3" "check-differs"
              drvSP = fixture "k" "4" "check-differs.drv"
              writing word = mkTestBuildDrv shell outSP ("mkdir -p $out && echo " <> word <> " > $out/value")
          built <- buildWithDeps config store (writing "first") drvSP
          before <- snapshot store [outSP]
          checked <- buildWithDeps (checking config) store (writing "second") drvSP
          after <- snapshot store [outSP]
          value <- T.strip <$> TIO.readFile (storePathToFilePath (stDir store) outSP </> "value")
          buildDirLeft <- Dir.doesPathExist (buildDirFor (stDir store) outSP)
          pure $
            assertEqual "build" (BuildSuccess outSP) built
              `andThen` assertEqual "check" (differs store drvSP outSP) checked
              `andThen` assertEqual "store" before after
              `andThen` assertEqual "value" "first" value
              `andThen` assertEqual "the build directory" False buildDirLeft,
      -- Upstream under keep-failed moves the rebuild, as it was compared,
      -- to the output's path with .check after it, registers nothing, and
      -- names it; the failure keeps the build directory as any does.
      runTestM "a check under keep-failed that finds a difference keeps the rebuild as PATH.check, unregistered, and names it" $
        inStore "keep" $ \store config -> do
          let outSP = fixture "k" "x" "check-keep"
              drvSP = fixture "k" "y" "check-keep.drv"
              outPath = storePathToFilePath (stDir store) outSP
              writing word = mkTestBuildDrv shell outSP ("mkdir -p $out && echo " <> word <> " > $out/value && echo $out > $out/self")
          built <- buildWithDeps config store (writing "first") drvSP
          before <- queryPathInfo (stDB store) outSP
          checked <- buildWithDeps (keeping (checking config)) store (writing "second") drvSP
          after <- queryPathInfo (stDB store) outSP
          value <- T.strip <$> TIO.readFile (outPath </> "value")
          keptValue <- T.strip <$> TIO.readFile (keptPath store outSP </> "value")
          keptSelf <- T.strip <$> TIO.readFile (keptPath store outSP </> "self")
          keptRow <- queryPathInfo (stDB store) (keptName outSP)
          keptWritable <- Dir.writable <$> getPermissions (keptPath store outSP </> "value")
          buildDirKept <- Dir.doesDirectoryExist (buildDirFor (stDir store) outSP)
          pure $
            assertEqual "build" (BuildSuccess outSP) built
              `andThen` assertEqual "check" (differsFromKept store drvSP outSP) checked
              `andThen` assertEqual "the valid output's row" before after
              `andThen` assertEqual "the valid output" "first" value
              `andThen` assertEqual "the kept rebuild" "second" keptValue
              `andThen` assertEqual "its own path, rewritten from the scratch path's" (T.pack outPath) keptSelf
              `andThen` assertEqual "a row for the kept rebuild" Nothing keptRow
              `andThen` assertEqual "the kept rebuild's file writable" False keptWritable
              `andThen` assertEqual "the build directory" True buildDirKept,
      -- Upstream removes what is at the .check path before it moves the
      -- rebuild there.
      runTestM "a check under keep-failed replaces the rebuild an earlier check kept" $
        inStore "keep-again" $ \store config -> do
          let outSP = fixture "k" "z" "check-keep-again"
              drvSP = fixture "k" "0" "check-keep-again.drv"
              writing word = mkTestBuildDrv shell outSP ("mkdir -p $out && echo " <> word <> " > $out/value")
          _ <- buildWithDeps config store (writing "first") drvSP
          _ <- buildWithDeps (keeping (checking config)) store (writing "second") drvSP
          checked <- buildWithDeps (keeping (checking config)) store (writing "third") drvSP
          keptValue <- T.strip <$> TIO.readFile (keptPath store outSP </> "value")
          pure (assertEqual "check" (differsFromKept store drvSP outSP) checked `andThen` assertEqual "the kept rebuild" "third" keptValue),
      runTestM "a check under keep-failed that passes keeps nothing" $
        inStore "keep-same" $ \store config -> do
          let outSP = fixture "k" "t" "check-keep-same"
              drvSP = fixture "k" "u" "check-keep-same.drv"
              drv = mkTestBuildDrv shell outSP "mkdir -p $out && echo fixed > $out/value"
          built <- buildWithDeps config store drv drvSP
          before <- snapshot store [outSP]
          checked <- buildWithDeps (keeping (checking config)) store drv drvSP
          after <- snapshot store [outSP]
          buildDirLeft <- Dir.doesPathExist (buildDirFor (stDir store) outSP)
          pure $
            assertEqual "build" (BuildSuccess outSP) built
              `andThen` assertEqual "check" (BuildSuccess outSP) checked
              `andThen` assertEqual "store" before after
              `andThen` assertEqual "the build directory" False buildDirLeft,
      -- The builder's output is where its log goes, so an empty log is a
      -- builder that never ran.
      runTestM "a check of a derivation with no valid output fails as upstream's does, and runs no builder" $
        inStore "not-valid" $ \store config -> do
          let outSP = fixture "k" "5" "check-not-valid"
              drvSP = fixture "k" "6" "check-not-valid.drv"
              drv = mkTestBuildDrv shell outSP "echo ran && mkdir -p $out"
              logPath = unStoreDir (stDir store) <> "-log"
          before <- snapshot store [outSP]
          checked <-
            withBinaryFile logPath WriteMode $ \logHandle ->
              buildWithDeps (checking config) (buildLogTo logHandle store) drv drvSP
          after <- snapshot store [outSP]
          logged <- BS.readFile logPath
          pure $
            assertEqual
              "check"
              (BuildFailure ("some outputs of '" <> storePathToText defaultStoreDir drvSP <> "' are not valid, so checking is not possible") 1)
              checked
              `andThen` assertEqual "store" before after
              `andThen` assertEqual "log" "" logged,
      runTestM "a check whose rebuild fails reports the builder as a failed build does, and the store keeps what it had" $
        inStore "fails" $ \store config -> do
          let outSP = fixture "k" "7" "check-fails"
              drvSP = fixture "k" "8" "check-fails.drv"
          built <- buildWithDeps config store (mkTestBuildDrv shell outSP "echo first > $out") drvSP
          before <- snapshot store [outSP]
          checked <- buildWithDeps (checking config) store (mkTestBuildDrv shell outSP "exit 3") drvSP
          after <- snapshot store [outSP]
          pure $
            assertEqual "build" (BuildSuccess outSP) built
              `andThen` assertEqual "check" (BuildFailure "building test-build failed: exit 3: builder failed" 1) checked
              `andThen` assertEqual "store" before after,
      -- out records dev's path, which under a check is dev's scratch path
      -- until it is rewritten, and dev is compared first because out
      -- references it.
      runTestM "a check compares every output, one that records its sibling included, and names the one that differs" $
        inStore "siblings" $ \store config -> do
          let outSP = fixture "k" "9" "check-main"
              devSP = fixture "k" "a" "check-dev"
              drvSP = fixture "k" "b" "check-siblings.drv"
              writing word =
                Derivation
                  { drvOutputs = [DerivationOutput "out" outSP "" "", DerivationOutput "dev" devSP "" ""],
                    drvInputDrvs = Map.empty,
                    drvInputSrcs = [],
                    drvPlatform = currentPlatform,
                    drvBuilder = TE.encodeUtf8 shell,
                    drvArgs = ["-c", "mkdir -p $out $dev && echo $dev > $out/sibling && echo " <> word <> " > $dev/value"],
                    drvEnv = Map.union (Map.fromList [("name", "check-siblings")]) hostToolsEnv
                  }
          built <- buildWithDeps config store (writing "first") drvSP
          before <- snapshot store [outSP, devSP]
          same <- buildWithDeps (checking config) store (writing "first") drvSP
          other <- buildWithDeps (checking config) store (writing "second") drvSP
          after <- snapshot store [outSP, devSP]
          pure $
            assertEqual "build" (BuildSuccess outSP) built
              `andThen` assertEqual "same" (BuildSuccess outSP) same
              `andThen` assertEqual "other" (differs store drvSP devSP) other
              `andThen` assertEqual "store" before after,
      -- Upstream raises the fixed-output mismatch for a checked fixed
      -- output, with the status of a failed build, not the check's.
      runTestM "a checked fixed output is held to its declared hash" $
        inStore "fixed" $ \store config -> do
          let outSP = fixture "k" "c" "check-fixed"
              drvSP = fixture "k" "d" "check-fixed.drv"
              digest = maybe "" Hash.bytesToHexText (Hash.rawHashWithAlgo "sha256" "fixed output payload\n")
              printing text = mkFixedOutputDrv shell outSP ("printf '" <> text <> "\\n' > $out") "sha256" digest
          built <- buildWithDeps config store (printing "fixed output payload") drvSP
          before <- snapshot store [outSP]
          same <- buildWithDeps (checking config) store (printing "fixed output payload") drvSP
          other <- buildWithDeps (checking config) store (printing "tampered content") drvSP
          after <- snapshot store [outSP]
          pure $
            assertEqual "build" (BuildSuccess outSP) built
              `andThen` assertEqual "same" (BuildSuccess outSP) same
              `andThen` ( case other of
                            BuildFailure message 1 | "hash mismatch" `T.isInfixOf` message -> Pass
                            _ -> Fail ("expected the fixed-output mismatch, got " <> T.pack (show other))
                        )
              `andThen` assertEqual "store" before after,
      -- A root that came from a cache has inputs nothing fetched; here
      -- the input's output is deleted to the same effect.
      runTestM "a check builds an input that is not valid, then checks the root alone" $
        inStore "input" $ \store config -> do
          let depOut = fixture "c" "0" "check-dep"
              depDrvSP = fixture "c" "1" "check-dep.drv"
              rootOut = fixture "c" "2" "check-root"
              rootDrvSP = fixture "c" "3" "check-root.drv"
              depDrv = mkTestBuildDrv shell depOut "echo leaf > $out"
              rootDrv = (mkTestBuildDrv shell rootOut "echo root > $out") {drvInputDrvs = Map.singleton depDrvSP ["out"]}
          writeDrv store depDrv depDrvSP
          writeDrv store rootDrv rootDrvSP
          built <- buildWithDeps config store rootDrv rootDrvSP
          rootBefore <- queryPathInfo (stDB store) rootOut
          deleted <- deleteNamed store (storePathBasename depOut)
          depGone <- not <$> isValid store depOut
          checked <- buildWithDeps (checking config) store rootDrv rootDrvSP
          depBack <- isValid store depOut
          rootAfter <- queryPathInfo (stDB store) rootOut
          pure $
            assertEqual "build" (BuildSuccess rootOut) built
              `andThen` assertEqual "deleted" (True, True) (either (const False) doRowRemoved deleted, depGone)
              `andThen` assertEqual "check" (BuildSuccess rootOut) checked
              `andThen` assertEqual "input valid again" True depBack
              `andThen` assertEqual "root" rootBefore rootAfter,
      -- Upstream compares the outputs in the order its topological sort
      -- of their references gives, an output after the ones it references
      -- (registerOutputs in local-derivation-goal.cc at 2.24.9,
      -- derivation-builder-impl.cc at 2.36.0), and throws at the first
      -- that differs.  Output a records b's path, so b is compared first
      -- though a sorts first by name and comes first in the derivation,
      -- and both differ.
      runTestM "of two outputs that differ the one the other references is named, though the other sorts first" $
        inStore "order" $ \store config -> do
          let aSP = fixture "k" "f" "check-order-a"
              bSP = fixture "k" "g" "check-order-b"
              drvSP = fixture "k" "h" "check-order.drv"
          built <- buildWithDeps config store (twoOutputs "check-order" ("a", aSP) ("b", bSP) (bothWriting "first")) drvSP
          before <- snapshot store [aSP, bSP]
          checked <- buildWithDeps (checking config) store (twoOutputs "check-order" ("a", aSP) ("b", bSP) (bothWriting "second")) drvSP
          after <- snapshot store [aSP, bSP]
          pure $
            assertEqual "build" (BuildSuccess aSP) built
              `andThen` assertEqual "check" (differs store drvSP bSP) checked
              `andThen` assertEqual "store" before after,
      -- Upstream stops at the first output that differs, having moved
      -- that one's rebuild alone.
      runTestM "of two outputs that differ under keep-failed only the rebuild of the one named is kept" $
        inStore "keep-order" $ \store config -> do
          let aSP = fixture "m" "1" "check-keep-order-a"
              bSP = fixture "m" "2" "check-keep-order-b"
              drvSP = fixture "m" "3" "check-keep-order.drv"
          _ <- buildWithDeps config store (twoOutputs "check-keep-order" ("a", aSP) ("b", bSP) (bothWriting "first")) drvSP
          checked <- buildWithDeps (keeping (checking config)) store (twoOutputs "check-keep-order" ("a", aSP) ("b", bSP) (bothWriting "second")) drvSP
          aKept <- Dir.doesPathExist (keptPath store aSP)
          bKept <- Dir.doesPathExist (keptPath store bSP)
          pure $
            assertEqual "check" (differsFromKept store drvSP bSP) checked
              `andThen` assertEqual "a's rebuild kept" False aKept
              `andThen` assertEqual "b's rebuild kept" True bKept,
      -- The rebuilt outputs are ordered before any is compared, so two
      -- that record each other fail the check as they would fail a build,
      -- with upstream's message for the cycle and not for a difference.
      runTestM "rebuilt outputs that reference each other in a cycle fail the check as they fail a build" $
        inStore "cycle" $ \store config -> do
          let devSP = fixture "k" "i" "check-cycle-dev"
              outSP = fixture "k" "j" "check-cycle"
              drvSP = fixture "k" "l" "check-cycle.drv"
              apart = "mkdir -p $out $dev && echo first > $out/value && echo first > $dev/value"
              entwined = "mkdir -p $out $dev && echo $dev > $out/sibling && echo $out > $dev/sibling"
          built <- buildWithDeps config store (twoOutputs "check-cycle" ("dev", devSP) ("out", outSP) apart) drvSP
          before <- snapshot store [devSP, outSP]
          checked <- buildWithDeps (checking config) store (twoOutputs "check-cycle" ("dev", devSP) ("out", outSP) entwined) drvSP
          after <- snapshot store [devSP, outSP]
          pure $
            assertEqual "build" (BuildSuccess devSP) built
              `andThen` assertEqual
                "check"
                ( BuildFailure
                    ( "building check-cycle failed: exit 1: cycle detected in build of '"
                        <> storePathToText defaultStoreDir drvSP
                        <> "' in the references of output 'dev' from output 'out'"
                    )
                    1
                )
                checked
              `andThen` assertEqual "store" before after,
      -- 'buildWithDeps' reaches the rebuild only once the outputs are
      -- valid, so the rebuild's own answer for one that is not shows only
      -- when it is asked directly: upstream's assertPathValidity message,
      -- with no builder run for it.  It asks under the output's lock, so
      -- the lock's file is the one thing it leaves in the store.
      runTestM "checkDerivation fails for an output that is not valid as upstream's does, and runs no builder" $
        inStore "direct" $ \store config -> do
          let outSP = fixture "k" "m" "check-direct"
              drvSP = fixture "k" "n" "check-direct.drv"
              drv = mkTestBuildDrv shell outSP "echo ran && mkdir -p $out"
              logPath = unStoreDir (stDir store) <> "-log"
              withoutLocks (entries, infos) = (filter (not . T.isSuffixOf ".lock" . T.pack) entries, infos)
          before <- snapshot store [outSP]
          checked <-
            withBinaryFile logPath WriteMode $ \logHandle ->
              checkDerivation config (buildLogTo logHandle store) drv drvSP
          after <- snapshot store [outSP]
          logged <- BS.readFile logPath
          pure $
            assertEqual "check" (Left "building test-build failed: exit 1: some outputs are unexpectedly invalid") checked
              `andThen` assertEqual "store" before (withoutLocks after)
              `andThen` assertEqual "log" "" logged,
      runTestM "checkDerivation answers what the rebuild found for a derivation whose outputs are valid" $
        inStore "direct-valid" $ \store config -> do
          let outSP = fixture "k" "p" "check-direct-valid"
              drvSP = fixture "k" "q" "check-direct-valid.drv"
              writing word = mkTestBuildDrv shell outSP ("echo " <> word <> " > $out")
          built <- buildDerivation config store (writing "first") drvSP
          same <- checkDerivation config store (writing "first") drvSP
          other <- checkDerivation config store (writing "second") drvSP
          kept <- checkDerivation (keeping config) store (writing "second") drvSP
          pure $
            assertEqual "build" (BuildSuccess outSP) built
              `andThen` assertEqual "same" (Right Realise.OutputsMatch) same
              `andThen` assertEqual "other" (Right (Realise.OutputDiffers outSP Realise.RebuildDiscarded)) other
              `andThen` assertEqual "kept" (Right (Realise.OutputDiffers outSP Realise.RebuildKept)) kept,
      -- A derivation with no output has nothing to compare, and a check
      -- that compared nothing has not passed.
      runTestM "checkDerivation fails for a derivation with no outputs" $
        inStore "no-outputs" $ \store config -> do
          let drvSP = fixture "k" "r" "check-empty.drv"
              drv = (mkTestBuildDrv shell (fixture "k" "s" "unused") "exit 0") {drvOutputs = []}
          checked <- checkDerivation config store drv drvSP
          pure (assertEqual "check" (Left "building test-build failed: exit 1: no outputs defined") checked),
      -- A check hashes the rebuilt tree as a stream, and registration
      -- hashed the valid one from its whole archive.  The two have to
      -- agree on everything a NAR holds, or a tree that builds the same
      -- every time would be found different: here a nested directory, an
      -- executable and a symlink.
      runTestM "a check hashes a rebuilt tree to what registration recorded for the same tree" $
        inStore "hash" $ \store config -> do
          let outSP = fixture "k" "v" "check-hash"
              drvSP = fixture "k" "w" "check-hash.drv"
              script =
                "mkdir -p $out/bin $out/share/deep && echo data > $out/share/deep/file"
                  <> " && printf '#!/bin/sh\\necho tool\\n' > $out/bin/tool && chmod +x $out/bin/tool"
                  <> " && ln -s ../share/deep/file $out/bin/link"
              drv = mkTestBuildDrv shell outSP script
          built <- buildWithDeps config store drv drvSP
          recorded <- fmap piNarHash <$> queryPathInfo (stDB store) outSP
          streamed <- CHash.formatNixHash <$> ExecBit.narHashOfPath (volumeCaseHack (stCaseSensitivity store)) (storePathToFilePath (stDir store) outSP)
          checked <- buildWithDeps (checking config) store drv drvSP
          pure $
            assertEqual "build" (BuildSuccess outSP) built
              `andThen` assertEqual "hashes" (Just streamed) recorded
              `andThen` assertEqual "check" (BuildSuccess outSP) checked
    ]

-- | A random reference graph over a few nodes, cycles and self-references
-- included, with roots drawn from it.
genReferenceGraph :: QC.Gen (Map.Map Int [Int], [Int])
genReferenceGraph = do
  size <- QC.chooseInt (1, 12)
  references <- traverse (\node -> (,) node <$> QC.sublistOf [0 .. size - 1]) [0 .. size - 1]
  roots <- QC.sublistOf [0 .. size - 1]
  pure (Map.fromList references, roots)

-- | Reference scanning covers the closure of a build's inputs, upstream's
-- @inputPaths@: an output records a path its inputs reach only through
-- their references, from an input derivation's output or from an input
-- source, and a collection keeps that path while the output is rooted.  A
-- valid path outside the closure is not recorded, and neither is the input
-- derivation's @.drv@.
testReferenceClosureIO :: IO [Bool]
testReferenceClosureIO = do
  putStrLn "builder/reference-closure"
  shell <- findTestShell
  closureLaw <-
    runTestM "closureUnder is the least set holding the roots and closed under references" $ do
      result <-
        QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} $
          QC.forAll genReferenceGraph $ \(references, roots) ->
            let referencesOf node = Map.findWithDefault [] node references
                fixpoint reached
                  | next == reached = reached
                  | otherwise = fixpoint next
                  where
                    next = reached <> Set.fromList (concatMap referencesOf (Set.toList reached))
             in runIdentity (closureUnder (Identity . referencesOf) roots) QC.=== fixpoint (Set.fromList roots)
      pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))
  built <- withTempStore $ \store -> do
    let sd = stDir store
        onDisk = storePathToFilePath sd
        pathText = T.pack . onDisk
        plant sp = do
          createDirectoryIfMissing True (onDisk sp)
          BS.writeFile (onDisk sp </> "data") (TE.encodeUtf8 (spName sp))
        registration sp refs = PathRegistration sp ("sha256:" <> T.replicate 52 "0") 1 Nothing refs NotUltimate Set.empty Nothing
        -- The input derivation's output records libRuntime, and the input
        -- source records srcRuntime.
        lib = StorePath (T.replicate 32 "k") "closure-lib"
        libRuntime = StorePath (T.replicate 32 "l") "closure-lib-runtime"
        libDrvPath = StorePath (T.replicate 32 "m") "closure-lib.drv"
        src = StorePath (T.replicate 32 "n") "closure-src"
        srcRuntime = StorePath (T.replicate 32 "p") "closure-src-runtime"
        stray = StorePath (T.replicate 32 "q") "closure-stray"
        outSP = StorePath (T.replicate 32 "r") "closure-out"
        libDrv = mkTestBuildDrv shell lib "mkdir -p \"$out\""
        script =
          "mkdir -p \"$out\" && printf '%s\\n' "
            <> T.unwords ["'" <> pathText sp <> "'" | sp <- [libRuntime, srcRuntime, stray, libDrvPath]]
            <> " > \"$out/paths\""
        drv = (mkTestBuildDrv shell outSP script) {drvInputDrvs = Map.singleton libDrvPath ["out"], drvInputSrcs = [src]}
        config = defaultBuildConfig sd
        rootFile = gcRootsDir sd </> T.unpack (spHash outSP <> "-" <> spName outSP)
    mapM_ plant [lib, libRuntime, src, srcRuntime, stray]
    registerPaths
      (stDB store)
      [ registration libRuntime [],
        registration lib [libRuntime],
        registration srcRuntime [],
        registration src [srcRuntime],
        registration stray []
      ]
    writeDrv store libDrv libDrvPath
    result <- buildDerivation config store drv testDrvPath
    recorded <- Set.fromList <$> queryReferences (stDB store) outSP
    createDirectoryIfMissing True (gcRootsDir sd)
    BS.writeFile rootFile ""
    collected <- collectGarbage store
    validity <- mapM (isValid store) [outSP, libRuntime, srcRuntime, stray]
    runtimesOnDisk <- mapM (Dir.doesPathExist . (</> "data") . onDisk) [libRuntime, srcRuntime]
    let recordedAmong = filter (\sp -> Set.member (pathText sp) recorded)
    case result of
      BuildFailure msg code ->
        sequence
          [ runTest "an output records a path its inputs reach through their references" $
              Fail ("build failed (exit " <> T.pack (show code) <> "): " <> msg)
          ]
      BuildSuccess _ ->
        sequence
          [ runTest "an output records a path its inputs reach through their references" $
              assertEqual "recorded" [libRuntime, srcRuntime] (recordedAmong [libRuntime, srcRuntime]),
            runTest "a collection keeps a dependency of a dependency while the output is rooted" $
              assertRight "collectGarbage" collected $ \_ ->
                assertEqual "out, libRuntime, srcRuntime, stray valid; runtimes on disk" ([True, True, True, False], [True, True]) (validity, runtimesOnDisk),
            runTest "a path outside the input closure is not recorded" $
              assertEqual "recorded" [] (recordedAmong [stray, libDrvPath])
          ]
  pure (closureLaw : built)

-- | A random acyclic reference graph over a few nodes, labelled in random
-- order so that ascending order is not a topological one.  A node
-- references only nodes before it in that order, and may also reference
-- itself or a node outside the graph, which 'orderByReferences' ignores.
genAcyclicReferences :: QC.Gen (Map.Map Int (Set.Set Int))
genAcyclicReferences = do
  size <- QC.chooseInt (1, 12)
  labels <- QC.shuffle [0 .. size - 1]
  let referencesOf (label, before) = do
        earlier <- QC.sublistOf before
        itself <- QC.elements [[], [label]]
        outside <- QC.sublistOf [size, size + 1]
        pure (label, Set.fromList (earlier ++ itself ++ outside))
  Map.fromList <$> traverse referencesOf (zip labels (inits labels))

-- | 'orderByReferences' is upstream's @topoSort@ (topo-sort.hh at 2.24.9)
-- followed by the reversal @registerOutputs@ applies.  Over generated
-- graphs it finds a cycle exactly when the references hold one, an order
-- it returns lists every node once and after each node it references, and
-- a cycle it reports is closed by a real reference.  The fixed cases are
-- what upstream's @topoSort@ and that reversal give for the same graphs.
testOutputOrderIO :: IO [Bool]
testOutputOrderIO = do
  putStrLn "builder/output-order"
  let quickCheckPasses property = do
        result <- QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} property
        pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))
      -- A generated graph as the map the walk takes: its roots are the
      -- nodes, and a reference may name a node outside them.
      asNodes (references, roots) =
        Map.fromList [(node, Set.fromList (Map.findWithDefault [] node references)) | node <- roots]
      -- The references the walk follows: between two distinct nodes.
      edgesWithin nodes =
        [(from, to) | (from, references) <- Map.toList nodes, to <- Set.toList references, to /= from, Map.member to nodes]
      successors nodes node = [to | (from, to) <- edgesWithin nodes, from == node]
      hasCycle nodes =
        any cyclic (Graph.stronglyConnComp [(node, node, successors nodes node) | node <- Map.keys nodes])
      cyclic (Graph.AcyclicSCC _) = False
      cyclic _ = True
      orderOf :: Map.Map Int (Set.Set Int) -> Either (ReferenceCycle Int) [(Int, Set.Set Int)]
      orderOf = orderByReferences id
      namedOrder :: [(Text, [Text])] -> Either (ReferenceCycle Text) [Text]
      namedOrder graph = map fst <$> orderByReferences Set.fromList (Map.fromList graph)
  sequence
    [ runTestM "orderByReferences finds a cycle exactly when the references hold one" $
        quickCheckPasses . QC.checkCoverage $
          QC.forAll (QC.frequency [(2, asNodes <$> genReferenceGraph), (1, genAcyclicReferences)]) $ \nodes ->
            QC.cover 25 (hasCycle nodes) "with a cycle" . QC.cover 25 (not (hasCycle nodes)) "without one" $
              either (const True) (const False) (orderOf nodes) QC.=== hasCycle nodes,
      runTestM "orderByReferences lists every node once, after each node it references" $
        quickCheckPasses $
          QC.forAll genAcyclicReferences $ \nodes -> case orderOf nodes of
            Left found -> QC.counterexample ("reported a cycle in an acyclic graph: " <> show found) False
            Right order ->
              let position = Map.fromList (zip (map fst order) [0 :: Int ..])
                  before from to = (<) <$> Map.lookup to position <*> Map.lookup from position
               in QC.conjoin
                    [ sort (map fst order) QC.=== Map.keys nodes,
                      Map.fromList order QC.=== nodes,
                      QC.counterexample "a node is listed before a node it references" (all (\(from, to) -> before from to == Just True) (edgesWithin nodes))
                    ],
      runTestM "a cycle orderByReferences reports is closed by a real reference" $
        quickCheckPasses . QC.checkCoverage $
          QC.forAll (asNodes <$> genReferenceGraph) $ \nodes -> QC.cover 40 (hasCycle nodes) "with a cycle" $ case orderOf nodes of
            Right _ -> QC.property True
            Left (ReferenceCycle node referrer) ->
              let reachable = runIdentity (closureUnder (Identity . successors nodes) [node])
               in QC.counterexample (show (node, referrer)) $
                    (referrer, node) `elem` edgesWithin nodes && Set.member referrer reachable,
      runTest "orderByReferences gives upstream's output order and cycle for fixed graphs" $
        assertEqual
          "orders"
          [ Left (ReferenceCycle "dev" "out"),
            Left (ReferenceCycle "b" "c"),
            Right ["c", "a", "b"],
            Right ["bin", "dev", "out"]
          ]
          [ namedOrder [("dev", ["out"]), ("out", ["dev"])],
            namedOrder [("a", ["b"]), ("b", ["c"]), ("c", ["b"])],
            namedOrder [("a", ["c"]), ("b", []), ("c", [])],
            namedOrder [("bin", ["gone"]), ("dev", ["bin"]), ("out", ["dev", "out"])]
          ]
    ]

-- | Outputs that reference each other in a cycle fail the build with
-- upstream's message (@registerOutputs@ in local-derivation-goal.cc at
-- 2.24.9, derivation-builder-impl.cc at 2.36.0) and leave nothing
-- registered, while a one-way reference between outputs and a
-- self-reference build and are recorded.  A rebuilt output referencing a
-- sibling registered before the build is no cycle even when the sibling's
-- discarded copy references it back, since upstream treats a registered
-- output as a leaf.
testOutputCycleBuildIO :: IO [Bool]
testOutputCycleBuildIO = do
  putStrLn "builder/output-cycle"
  shell <- findTestShell
  withTempStore $ \store -> do
    let sd = stDir store
        config = defaultBuildConfig sd
        pathText = T.pack . storePathToFilePath sd
        basename sp = spHash sp <> "-" <> spName sp
        hashOf = T.replicate 32
        -- A derivation with outputs dev and out, written to the store as
        -- the build driver writes what it builds, then built.
        buildTwo drvSP name devSP outSP script = do
          let drv =
                Derivation
                  { drvOutputs = [DerivationOutput "dev" devSP "" "", DerivationOutput "out" outSP "" ""],
                    drvInputDrvs = Map.empty,
                    drvInputSrcs = [],
                    drvPlatform = currentPlatform,
                    drvBuilder = TE.encodeUtf8 shell,
                    drvArgs = ["-c", script],
                    drvEnv = Map.union (Map.singleton "name" name) hostToolsEnv
                  }
          writeDrv store drv drvSP
          buildWithDeps config store drv drvSP
        referencesOf sp = Set.fromList <$> queryReferences (stDB store) sp
        cycleDrv = StorePath (hashOf "s") "cycle.drv"
        cycleDev = StorePath (hashOf "v") "cycle-dev"
        cycleOut = StorePath (hashOf "w") "cycle"
        onewayDrv = StorePath (hashOf "x") "oneway.drv"
        onewayDev = StorePath (hashOf "y") "oneway-dev"
        onewayOut = StorePath (hashOf "z") "oneway"
        leafFirstDrv = StorePath (hashOf "g") "leaf.drv"
        leafSecondDrv = StorePath (hashOf "h") "leaf.drv"
        leafDev = StorePath (hashOf "j") "leaf-dev"
        leafOut = StorePath (hashOf "k") "leaf"
    cycleResult <- buildTwo cycleDrv "cycle" cycleDev cycleOut "mkdir -p \"$out\" \"$dev\" && echo \"$dev\" > \"$out/dev\" && echo \"$out\" > \"$dev/out\""
    cycleValid <- mapM (isValid store) [cycleDev, cycleOut]
    cycleOnDisk <- mapM (Dir.doesPathExist . storePathToFilePath sd) [cycleDev, cycleOut]
    onewayResult <- buildTwo onewayDrv "oneway" onewayDev onewayOut "mkdir -p \"$out\" \"$dev\" && echo \"$dev\" > \"$out/dev\" && echo \"$out\" > \"$out/self\" && echo \"$dev\" > \"$dev/self\""
    onewayRefs <- mapM referencesOf [onewayDev, onewayOut]
    leafFirst <- buildTwo leafFirstDrv "leaf" leafDev leafOut "mkdir -p \"$out\" \"$dev\" && echo first > \"$out/v\" && echo first > \"$dev/v\""
    deleted <- deleteNamed store (basename leafOut)
    leafSecond <- buildTwo leafSecondDrv "leaf" leafDev leafOut "mkdir -p \"$out\" \"$dev\" && echo \"$dev\" > \"$out/dev\" && echo \"$out\" > \"$dev/out\""
    leafValid <- mapM (isValid store) [leafDev, leafOut]
    leafRefs <- referencesOf leafOut
    let succeeded result = case result of
          BuildSuccess _ -> Pass
          BuildFailure msg code -> Fail ("build failed (exit " <> T.pack (show code) <> "): " <> msg)
    sequence
      [ runTest "outputs referencing each other fail the build with upstream's message" $
          assertEqual
            "result"
            (BuildFailure ("building cycle failed: exit 1: cycle detected in build of '/nix/store/" <> hashOf "s" <> "-cycle.drv' in the references of output 'dev' from output 'out'") 1)
            cycleResult,
        runTest "outputs referencing each other leave nothing registered or in the store" $
          assertEqual "valid, on disk" ([False, False], [False, False]) (cycleValid, cycleOnDisk),
        runTest "a one-way reference between outputs and self-references build" $ succeeded onewayResult,
        runTest "a one-way reference between outputs and self-references are recorded" $
          assertEqual
            "dev's and out's references"
            [Set.fromList [pathText onewayDev], Set.fromList [pathText onewayDev, pathText onewayOut]]
            onewayRefs,
        runTest "a rebuilt output referencing a registered sibling that references it back builds" $
          case (leafFirst, deleted, leafSecond) of
            (BuildSuccess _, Right outcome, BuildSuccess _)
              | not (doRowRemoved outcome) -> Fail "the setup did not unregister out"
              | otherwise -> assertEqual "valid, out's references" ([True, True], Set.fromList [pathText leafDev]) (leafValid, leafRefs)
            (_, _, BuildFailure msg code) -> Fail ("the rebuild failed (exit " <> T.pack (show code) <> "): " <> msg)
            _ -> Fail "the setup did not reach a registered dev and an unregistered out"
      ]

-- ---------------------------------------------------------------------------
-- Tests: NAR and hash known-answer vectors
-- ---------------------------------------------------------------------------

-- | Which derivations this machine will spawn, and how.
--
-- The distinction that matters is between two answers: a derivation for
-- this platform needs no launcher, and a derivation for another platform
-- with no launcher configured is not the same thing.
testExecWrapper :: IO [Bool]
testExecWrapper = do
  putStrLn "builder/exec-wrapper"
  sequence
    [ runTest "a derivation for this platform is spawned directly" $
        assertEqual
          "native"
          SpawnNative
          (execWrapperFor (wrapperConfig [(platformToText foreignPlatform, "/usr/bin/wine")]) (drvFor currentPlatform)),
      runTest "a configured foreign system spawns through its launcher" $
        assertEqual
          "wrapped"
          (SpawnThrough "/usr/bin/wine")
          (execWrapperFor (wrapperConfig [(platformToText foreignPlatform, "/usr/bin/wine")]) (drvFor foreignPlatform)),
      -- The finding this exists for: without a distinct answer here, a
      -- foreign builder was spawned natively and failed in the loader,
      -- after the whole closure had already been realized.
      runTest "an unconfigured foreign system is refused, not spawned" $
        assertEqual
          "unsupported"
          (SpawnUnsupported (platformToText foreignPlatform))
          (execWrapperFor (wrapperConfig []) (drvFor foreignPlatform)),
      -- A 32-bit derivation on a 64-bit host of the same OS is native, as
      -- upstream's extra-platforms makes it, launcher or no launcher; on
      -- any other host it is as foreign as anything else.
      runTest "an i686 derivation spawns natively on its x86_64 host, whatever is configured" $
        assertEqual
          "wow64"
          [SpawnNative, SpawnNative, SpawnNative, SpawnNative]
          [ spawnFor X86_64_Windows (wrapperConfig []) (drvFor I686_Windows),
            spawnFor X86_64_Windows (wrapperConfig [("i686-windows", "/usr/bin/wine")]) (drvFor I686_Windows),
            spawnFor X86_64_Linux (wrapperConfig []) (drvFor I686_Linux),
            spawnFor X86_64_Linux (wrapperConfig [("i686-linux", "/opt/qemu")]) (drvFor I686_Linux)
          ],
      runTest "an i686 derivation elsewhere is refused or launched like any foreign system" $
        assertEqual
          "elsewhere"
          [SpawnUnsupported "i686-windows", SpawnThrough "/usr/bin/wine", SpawnUnsupported "i686-linux"]
          [ spawnFor X86_64_Linux (wrapperConfig []) (drvFor I686_Windows),
            spawnFor X86_64_Linux (wrapperConfig [("i686-windows", "/usr/bin/wine")]) (drvFor I686_Windows),
            spawnFor Aarch64_Darwin (wrapperConfig []) (drvFor I686_Linux)
          ],
      -- The system string is the derivation's own spelling, matched
      -- exactly.  The documentation once named a different one, which
      -- parsed, stored, and then never matched anything.
      runTest "SYSTEM=PATH splits at the first separator" $
        assertEqual
          "specs"
          (Right (Map.fromList [("x86_64-windows", "/usr/bin/wine"), ("aarch64-linux", "/opt/qemu=static/qemu")]))
          (execWrapperConfig ["x86_64-windows=/usr/bin/wine", "aarch64-linux=/opt/qemu=static/qemu"]),
      runTest "no specs is an empty map, not an error" $
        assertEqual "empty" (Right Map.empty) (execWrapperConfig []),
      runTest "a spec missing either half is rejected" $
        assertEqual
          "malformed"
          (replicate 4 True)
          (map (either (const True) (const False) . execWrapperConfig . pure) ["wine", "=wine", "x86_64-windows=", ""]),
      -- Last-one-wins would run a build through a launcher the operator
      -- did not think they had asked for.
      runTest "naming one system twice is rejected" $
        assertEqual
          "duplicate"
          (Left "--exec-wrapper names x86_64-windows twice")
          (execWrapperConfig ["x86_64-windows=/usr/bin/wine", "x86_64-windows=/usr/bin/wine64"]),
      runTest "a launcher keyed on any other spelling does not match" $
        assertEqual
          "near-miss keys"
          (replicate 3 (SpawnUnsupported (platformToText foreignPlatform)))
          ( [ execWrapperFor (wrapperConfig [(key, "/usr/bin/wine")]) (drvFor foreignPlatform)
            | key <- ["x86-windows", "windows", T.toUpper (platformToText foreignPlatform)]
            ]
          )
    ]
  where
    -- Some platform this test is not running on, so "foreign" is foreign
    -- whichever host runs the suite.
    foreignPlatform =
      if currentPlatform == X86_64_Windows then X86_64_Linux else X86_64_Windows
    wrapperConfig entries =
      (defaultBuildConfig (StoreDir "/nix/store")) {bcExecWrappers = Map.fromList entries}
    drvFor platform =
      Derivation
        { drvOutputs = [DerivationOutput "out" (StorePath (T.replicate 32 "a") "spawn") "" ""],
          drvInputDrvs = Map.empty,
          drvInputSrcs = [],
          drvPlatform = platform,
          drvBuilder = "/bin/sh",
          drvArgs = [],
          drvEnv = Map.empty
        }

testFetchMirrors :: IO [Bool]
testFetchMirrors = do
  putStrLn "builder/fetch-mirrors"
  sequence
    [ runTest "fetch URLs preserve the legacy single URL" $
        assertEqual
          "url"
          (Right ("https://a/file%20name" :| []))
          (fetchUrlsFromEnv (Map.singleton "url" "https://a/file%20name")),
      runTest "fetch URLs prefer the explicit ordered list" $
        assertEqual
          "urls"
          (Right ("https://a" :| ["https://b"]))
          (fetchUrlsFromEnv (Map.fromList [("url", "https://ignored"), ("urls", "  https://a\t https://b\n")])),
      runTest "fetch URLs preserve non-ASCII whitespace within a URL" $
        let url = "https://a/has\xA0space"
         in assertEqual
              "non-ASCII URL"
              (Right (url :| ["https://b"]))
              (fetchUrlsFromEnv (Map.singleton "urls" (TE.encodeUtf8 (url <> " https://b")))),
      runTest "fetch URLs reject missing, empty and invalid UTF-8 inputs" $
        assertEqual
          "invalid"
          (replicate 5 True)
          ( map
              (either (const True) (const False) . fetchUrlsFromEnv)
              [Map.empty, Map.singleton "url" "", Map.fromList [("urls", " "), ("url", "https://ignored")], Map.singleton "url" (BS.pack [255]), Map.singleton "urls" (BS.pack [255])]
          ),
      runTestM "mirror policy retries timeout and hash failures in order" $ do
        calls <- newIORef []
        let attempt url = do
              atomicModifyIORef' calls (\seen -> (seen ++ [url], ()))
              pure $ case url of
                "slow" -> Left "download timeout"
                "corrupt" -> Left "hash mismatch"
                _ -> Right ("verified" :: Text)
        result <- tryFetchUrlsWith attempt ("slow" :| ["corrupt", "good", "unused"])
        seen <- readIORef calls
        pure (assertEqual "ordered attempts" (Right "verified", ["slow", "corrupt", "good"]) (result, seen)),
      runTestM "mirror policy returns every failure in order" $ do
        result <- tryFetchUrlsWith (pure . Left) ("first failure" :| ["second failure"])
        pure (assertEqual "failures" (Left "first failure\nsecond failure" :: Either Text ()) result),
      runTestM "mirror policy does not evaluate candidates after success" $ do
        let attempt "good" = pure (Right "good")
            attempt _ = fail "unused mirror attempted"
        result <- tryFetchUrlsWith attempt ("good" :| ["unused"])
        pure (assertEqual "first success" (Right "good" :: Either Text Text) result),
      -- The per-URL retry policy, with its effects injected: upstream's five
      -- attempts and doubling delay from 250 ms, spent on transient failures
      -- only.
      runTest "retry delay is upstream's 250 ms doubling, with jitter on the exponent" $
        assertEqual
          "delays"
          ([250, 500, 1000, 2000], 353, 2828)
          ( map (\retry -> retryDelayMs defaultFetchRetryPolicy retry 0) [1 .. 4],
            retryDelayMs defaultFetchRetryPolicy 1 0.5,
            retryDelayMs defaultFetchRetryPolicy 4 0.5
          ),
      runTestM "retry policy retries a transient failure with backoff until it succeeds" $ do
        calls <- newIORef (0 :: Int)
        sleeps <- newIORef []
        warnings <- newIORef []
        let effects =
              RetryEffects
                { reSleepMs = \ms -> atomicModifyIORef' sleeps (\seen -> (seen ++ [ms], ())),
                  reJitter = pure 0,
                  reWarn = \msg -> atomicModifyIORef' warnings (\seen -> (seen ++ [msg], ()))
                }
            attempt = do
              n <- atomicModifyIORef' calls (\c -> (c + 1, c + 1))
              pure (if n < 3 then Left (Subst.TransientFailure "HTTP 503 fetching u") else Right ("verified" :: Text))
        result <- retryTransient defaultFetchRetryPolicy effects attempt
        seenSleeps <- readIORef sleeps
        seenWarnings <- readIORef warnings
        pure
          ( assertEqual
              "two retries"
              (Right "verified", [250, 500], ["HTTP 503 fetching u; retrying in 250 ms", "HTTP 503 fetching u; retrying in 500 ms"])
              (result, seenSleeps, seenWarnings)
          ),
      runTestM "retry policy gives up after five attempts" $ do
        calls <- newIORef (0 :: Int)
        sleeps <- newIORef []
        let effects = RetryEffects {reSleepMs = \ms -> atomicModifyIORef' sleeps (\seen -> (seen ++ [ms], ())), reJitter = pure 0, reWarn = \_ -> pure ()}
            attempt = atomicModifyIORef' calls (\c -> (c + 1, ())) >> pure (Left (Subst.TransientFailure "timeout") :: Either Subst.AttemptFailure ())
        result <- retryTransient defaultFetchRetryPolicy effects attempt
        seenCalls <- readIORef calls
        seenSleeps <- readIORef sleeps
        pure (assertEqual "spent" (Left (Subst.TransientFailure "timeout"), 5, [250, 500, 1000, 2000]) (result, seenCalls, seenSleeps)),
      runTestM "retry policy does not retry a fatal failure" $ do
        calls <- newIORef (0 :: Int)
        let effects = RetryEffects {reSleepMs = \_ -> fail "slept on a fatal failure", reJitter = pure 0, reWarn = \_ -> fail "warned on a fatal failure"}
            attempt = atomicModifyIORef' calls (\c -> (c + 1, ())) >> pure (Left (Subst.FatalFailure "HTTP 404") :: Either Subst.AttemptFailure ())
        result <- retryTransient defaultFetchRetryPolicy effects attempt
        seenCalls <- readIORef calls
        pure (assertEqual "once" (Left (Subst.FatalFailure "HTTP 404"), 1) (result, seenCalls)),
      -- The classification tables, against upstream's (filetransfer.cc at
      -- 2.24.9): which statuses and which client failures a retry can outlive.
      runTest "status classification matches upstream's transfer layer" $
        let transient status = case fetchStatusFailure "u" status of
              Subst.TransientFailure _ -> True
              Subst.FatalFailure _ -> False
         in assertEqual
              "classes"
              (replicate 6 True, replicate 10 False)
              ( map transient [HTTP.status408, HTTP.status429, HTTP.status500, HTTP.status502, HTTP.status503, HTTP.status504],
                map transient [HTTP.status404, HTTP.status410, HTTP.status401, HTTP.status403, HTTP.status407, HTTP.status400, HTTP.status418, HTTP.status501, HTTP.status505, HTTP.status511]
              ),
      runTest "status verdicts follow upstream's FileTransfer::Error, in its order" $
        assertEqual
          "verdicts"
          (replicate 2 NotFound ++ replicate 3 Forbidden ++ replicate 5 Misc ++ replicate 6 Transient)
          ( map
              statusError
              [ HTTP.status404,
                HTTP.status410,
                HTTP.status401,
                HTTP.status403,
                HTTP.status407,
                HTTP.status400,
                HTTP.status418,
                HTTP.status501,
                HTTP.status505,
                HTTP.status511,
                HTTP.status408,
                HTTP.status429,
                HTTP.status500,
                HTTP.status502,
                HTTP.status503,
                HTTP.status504
              ]
          ),
      runTest "exception classification retries transport failures only" $
        let transient err = case fetchExceptionFailure "u" err of
              Subst.TransientFailure _ -> True
              Subst.FatalFailure _ -> False
            request = HTTP.defaultRequest
         in assertEqual
              "classes"
              (replicate 4 True, replicate 4 False)
              ( map
                  transient
                  [ HTTP.HttpExceptionRequest request HTTP.ConnectionTimeout,
                    HTTP.HttpExceptionRequest request (HTTP.ResponseBodyTooShort 100 5),
                    HTTP.HttpExceptionRequest request HTTP.ResponseTimeout,
                    HTTP.HttpExceptionRequest request (HTTP.InternalException (toException connectionReset))
                  ],
                map
                  transient
                  [ HTTP.InvalidUrlException "ftp://x" "Invalid scheme",
                    HTTP.HttpExceptionRequest request (HTTP.InvalidDestinationHost ""),
                    HTTP.HttpExceptionRequest request HTTP.TlsNotSupported,
                    HTTP.HttpExceptionRequest request (HTTP.TooManyRedirects [])
                  ]
              ),
      -- The attempt boundary: an IOException there is a failure writing
      -- the output, and an asynchronous exception is never caught.
      runTestM "a failure writing the download is fatal" $ do
        outcome <- throwIO (userError "disk full") `catches` transferFailureHandlers "u" :: IO (Either Subst.AttemptFailure ())
        pure $ case outcome of
          Left (Subst.FatalFailure _) -> Pass
          other -> Fail ("expected a fatal failure, got: " <> T.pack (show other)),
      runTestM "an interrupt passes through the attempt boundary untouched" $ do
        outcome <- try (throwIO ThreadKilled `catches` transferFailureHandlers "u" :: IO (Either Subst.AttemptFailure ()))
        pure $ case outcome of
          Left err | fromException err == Just ThreadKilled -> Pass
          other -> Fail ("expected ThreadKilled to propagate, got: " <> T.pack (show other)),
      -- The body reader: the HTTP client wraps a failure opening the
      -- response but not one reading its body, where a reset mid-body
      -- arrives as a bare IOException.  Read through transferBodyReader it
      -- is a transport failure, retried like any other.
      runTestM "a connection reset mid-body is a transport failure, not a failed write" $ do
        let reader = transferBodyReader HTTP.defaultRequest (throwIO connectionReset)
        outcome <- (Right <$> reader) `catches` transferFailureHandlers "u"
        pure $ case outcome of
          Left (Subst.TransientFailure _) -> Pass
          other -> Fail ("expected a transient failure, got: " <> T.pack (show other)),
      runTestM "the body reader passes the client's own failures and an interrupt through unchanged" $ do
        let tooShort = HTTP.HttpExceptionRequest HTTP.defaultRequest (HTTP.ResponseBodyTooShort 100 5)
        clientFailure <- try (transferBodyReader HTTP.defaultRequest (throwIO tooShort))
        interrupt <- try (transferBodyReader HTTP.defaultRequest (throwIO ThreadKilled))
        chunk <- transferBodyReader HTTP.defaultRequest (pure "bytes")
        pure $ case (clientFailure, interrupt) of
          (Left (HTTP.HttpExceptionRequest _ (HTTP.ResponseBodyTooShort 100 5)), Left ThreadKilled)
            | chunk == "bytes" -> Pass
          other -> Fail ("unexpected body reader outcomes: " <> T.pack (show (other, chunk))),
      -- End to end against the loopback fixture, under a policy with a 1 ms
      -- base delay: a torn transfer and a server error spend all five
      -- attempts before the next mirror; a 404 and a hash mismatch spend one.
      runTestM "HTTP mirrors retry a torn transfer, skip a missing or corrupt one, and stop on success" $
        httpCase "fallback" False ["/missing", "/bad", "/partial", "/good", "/unused"] True (["/missing", "/bad"] ++ replicate 5 "/partial" ++ ["/good"]),
      runTestM "HTTP fetch retains single-url compatibility" $
        httpCase "legacy" True ["/good"] True ["/good"],
      runTestM "a server error is retried five times before the next mirror" $
        httpCase "retried" False ["/error", "/good"] True (replicate 5 "/error" ++ ["/good"]),
      runTestM "HTTP failure leaves no output or registration" $
        httpCase "errors" False ["/error", "/missing"] False (replicate 5 "/error" ++ ["/missing"]),
      runTestM "hash failure leaves no corrupt output or registration" $
        httpCase "corrupt" False ["/bad", "/partial"] False ("/bad" : replicate 5 "/partial"),
      runTestM "cancelling a download releases its lock and does not try another mirror" $
        withFetchurlServer $ \base requests requested ->
          withStore "cancel" $ \store config sp -> do
            let drv = fixtureDrv sp (Map.singleton "urls" (TE.encodeUtf8 (base <> "/slow " <> base <> "/good")))
            outcome <- timeout 10000000 $
              withAsync (buildDerivation config store drv testDrvPath) $ \worker -> do
                requested
                cancel worker
                waitCatch worker
            seen <- requests
            valid <- isValid store sp
            present <- Dir.doesPathExist (storePathToFilePath (stDir store) sp)
            lock <- tryAcquirePathLock (stDir store) sp
            mapM_ releasePathLock lock
            pure $ case outcome of
              Just (Left _) | seen == ["/slow"] && not valid && not present && isJust lock -> Pass
              _ -> Fail ("cancellation failed: " <> T.pack (show (outcome, seen, valid, present, isJust lock)))
    ]
  where
    hash = "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824"
    -- What http-client 0.7's body reader raises when the server resets
    -- the connection mid-body.
    connectionReset = mkIOError resourceVanishedErrorType "Network.Socket.recvBuf" Nothing Nothing
    withStore label action = do
      tmpBase <- getTemporaryDirectory
      let root = tmpBase </> ("nova-nix-test-mirrors-" ++ label)
          dir = StoreDir (root </> "store")
          config = (defaultBuildConfig dir) {bcFetchRetry = defaultFetchRetryPolicy {frpBaseDelayMs = 1}}
      sp <- either (fail . show) pure (makeFixedOutputPath "mirrors" "sha256" FlatIngestion (sha256Digest "hello"))
      bracket_ (forceRemoveIfExists root) (forceRemoveIfExists root) $
        bracket (openStore testLogger dir) closeStore $
          \store -> action store config sp
    fixtureDrv sp env =
      Derivation
        { drvOutputs = [DerivationOutput "out" sp "sha256" hash],
          drvInputDrvs = Map.empty,
          drvInputSrcs = [],
          drvPlatform = currentPlatform,
          drvBuilder = "builtin:fetchurl",
          drvArgs = [],
          drvEnv = env
        }
    httpCase label legacy paths succeeds expectedRequests =
      withFetchurlServer $ \base requests _ ->
        withStore label $ \store config sp -> do
          let urls = T.unwords (map (base <>) paths)
              env = Map.singleton (if legacy then "url" else "urls") (TE.encodeUtf8 urls)
          result <- buildDerivation config store (fixtureDrv sp env) testDrvPath
          seen <- requests
          valid <- isValid store sp
          present <- Dir.doesPathExist (storePathToFilePath (stDir store) sp)
          case result of
            BuildSuccess _
              | succeeds && valid && present && seen == expectedRequests ->
                  assertEqual "verified output" "hello" <$> BS.readFile (storePathToFilePath (stDir store) sp)
            BuildFailure msg _ | not succeeds && not valid && not present && seen == expectedRequests && not (T.null msg) -> pure Pass
            _ -> pure (Fail (T.pack (show (result, seen, valid, present))))

-- | @builtin:fetchurl@ with @unpack@ and @executable@, against a loopback
-- server: the output each makes, its modes once registered, and the
-- recursive hash upstream's builtin gives it (@builtinFetchurl@ in
-- fetchurl.cc).  Each expected NAR is encoded here from the format
-- ('narSpecStr'), the bytes upstream's restore and dump reproduce, so the
-- restore and the hash are held to bytes nova-cache did not write.
testFetchurlUnpack :: IO [Bool]
testFetchurlUnpack = do
  putStrLn "builder/fetchurl-unpack"
  case B64.decode treeXzB64 of
    Left err -> (: []) <$> runTest "fetchurl unpack setup" (Fail (T.pack err))
    Right treeXz -> do
      canLink <- symlinksAvailable
      let served =
            Map.fromList
              [ ("/tree.nar", narSpecVector),
                ("/tree.nar.xz", treeXz),
                ("/trailing.nar", narSpecVector <> "bytes after the archive"),
                ("/other.nar", narSpecFileVector),
                ("/plain.nar", fileNar "hello"),
                ("/link.nar", symlinkNar victimName),
                ("/dangling.nar", symlinkNar "absent"),
                ("/tool", "hello")
              ]
          route path _ = Send (maybe (httpResponse "404 Not Found" "missing") (httpResponse "200 OK") (Map.lookup path served))
          -- Run a builtin:fetchurl derivation of the paths and attributes
          -- given, its output fixed by the sha256 of the declared bytes in
          -- the mode given, in a store of its own.  The check gets the
          -- store's directory and output path before the build, and the
          -- build, which answers its result, the requests the server saw
          -- and the NAR hash the store registered.
          fetchIn label paths@(firstPath :| _) attrs (mode, algo) declared check =
            withHttpServer route $ \base requests _ -> do
              tmpBase <- getTemporaryDirectory
              let root = tmpBase </> ("nova-nix-test-fetchurl-" ++ label)
                  dir = StoreDir (root </> "store")
                  config = (defaultBuildConfig dir) {bcFetchRetry = defaultFetchRetryPolicy {frpBaseDelayMs = 1}}
                  digest = sha256Digest declared
                  env =
                    Map.fromList
                      ( [ ("url", TE.encodeUtf8 (base <> firstPath)),
                          ("urls", TE.encodeUtf8 (T.unwords (map (base <>) (toList paths))))
                        ]
                          ++ attrs
                      )
              case makeFixedOutputPath (T.pack label) "sha256" mode digest of
                Left err -> pure (Fail (T.pack (show err)))
                Right sp ->
                  bracket_ (forceRemoveIfExists root) (forceRemoveIfExists root) $
                    bracket (openStore testLogger dir) closeStore $ \store -> do
                      let drv =
                            Derivation
                              { drvOutputs = [DerivationOutput "out" sp algo (Hash.bytesToHexText digest)],
                                drvInputDrvs = Map.empty,
                                drvInputSrcs = [],
                                drvPlatform = currentPlatform,
                                drvBuilder = "builtin:fetchurl",
                                drvArgs = [],
                                drvEnv = env
                              }
                          build = do
                            result <- buildDerivation config store drv testDrvPath
                            seen <- requests
                            info <- queryPathInfo (stDB store) sp
                            pure (result, seen, piNarHash <$> info)
                      check dir sp build
          -- 'fetchIn' for a fetch that builds: check the requests, the
          -- registered NAR hash and the tree on disk.
          fetchCase label paths attrs hashMode declared expectedNar checkTree =
            fetchIn label paths attrs hashMode declared $ \dir sp build -> do
              (result, seen, narHash) <- build
              case result of
                BuildSuccess built | built == sp -> do
                  tree <- checkTree (storePathToFilePath dir sp)
                  pure
                    ( assertEqual "requests" (map TE.encodeUtf8 (toList paths)) seen
                        `andThen` assertEqual "NAR hash" (Just (CHash.formatNixHash (CHash.hashBytes expectedNar))) narHash
                        `andThen` tree
                    )
                other -> pure (Fail ("build: " <> T.pack (show other)))
          -- 'fetchIn' for a fetch upstream refuses with the error given,
          -- once the store's directory is prepared: nothing is
          -- registered, and nothing is left at the output path.
          refusedCase label paths attrs hashMode declared prepare expected =
            fetchIn label paths attrs hashMode declared $ \dir sp build -> do
              () <- prepare dir
              (result, _, narHash) <- build
              present <- Dir.doesPathExist (storePathToFilePath dir sp)
              pure $ case result of
                BuildFailure msg _
                  | expected `T.isInfixOf` msg -> assertEqual "registered, present" (Nothing, False) (narHash, present)
                other -> Fail ("expected a refusal naming " <> expected <> ", got: " <> T.pack (show other))
          -- An executable unpack fetch whose first URL serves a NAR whose
          -- root is a symlink beside the output: the file the link names
          -- is left as it was, and the next URL's plain file is made the
          -- executable the hash names.
          executableAfterLink label linkPath =
            fetchIn label (linkPath :| ["/plain.nar"]) (("executable", "1") : unpackAttrs) recursiveHash toolNar $ \dir sp build -> do
              let victim = victimIn dir
              BS.writeFile victim "a file outside the output"
              (result, seen, narHash) <- build
              victimExecutable <- ExecBit.isExecutable victim
              pure
                ( assertEqual "build" (BuildSuccess sp) result
                    `andThen` assertEqual "requests" (map TE.encodeUtf8 [linkPath, "/plain.nar"]) seen
                    `andThen` assertEqual "NAR hash" (Just (CHash.formatNixHash (CHash.hashBytes toolNar))) narHash
                    `andThen` assertEqual "the linked file made executable" False victimExecutable
                )
      unpacked <-
        if canLink
          then
            sequence
              [ runTestM "an unpack fetch restores the NAR it downloads, under that NAR's hash" $
                  fetchCase "unpack" ("/tree.nar" :| []) unpackAttrs recursiveHash narSpecVector narSpecVector restoredTree,
                -- Upstream's restore stops at the root node's close, so
                -- what a body holds after the archive is never read.
                runTestM "an unpack fetch stops at the archive's end and leaves the bytes after it unread" $
                  fetchCase "unpack-trailing" ("/trailing.nar" :| []) unpackAttrs recursiveHash narSpecVector narSpecVector restoredTree,
                runTestM "an unpack fetch of a .xz URL restores the NAR it decompresses" $
                  fetchCase "unpack-xz" ("/tree.nar.xz" :| []) unpackAttrs recursiveHash narSpecVector narSpecVector restoredTree,
                runTestM "an unpack fetch whose tree has another hash moves to the next URL" $
                  fetchCase "unpack-mirror" ("/other.nar" :| ["/tree.nar"]) unpackAttrs recursiveHash narSpecVector narSpecVector restoredTree,
                runTestM "an executable unpack fetch leaves alone the file a symlink root names, and moves to the next URL" $
                  executableAfterLink "exec-link" "/link.nar",
                runTestM "an executable unpack fetch of a dangling symlink root moves to the next URL" $
                  executableAfterLink "exec-dangling" "/dangling.nar",
                runTestM "a flat unpack fetch of a symlink root is refused with upstream's error, not hashed as the file it names" $
                  refusedCase "flat-link" ("/link.nar" :| []) unpackAttrs flatHash "hello" (\dir -> BS.writeFile (victimIn dir) "hello") flatOutputRefusal
              ]
          else [] <$ putStrLn "  SKIP  the unpacked trees hold symlinks, which need symlink privilege"
      others <-
        sequence
          [ runTestM "an executable fetch makes an executable file, under the hash of its NAR" $
              fetchCase "executable" ("/tool" :| []) [("executable", "1")] recursiveHash toolNar toolNar $ \outPath -> do
                mode <- canonicalAs 0o555 outPath
                executable <- ExecBit.isExecutable outPath
                contents <- BS.readFile outPath
                pure (mode `andThen` assertEqual "file" ("hello", True) (contents, executable)),
            runTestM "a fetch of a .xz URL without unpack keeps the bytes it downloads" $
              fetchCase "xz-kept" ("/tree.nar.xz" :| []) [] flatHash treeXz (fileNar treeXz) $ \outPath -> do
                mode <- canonicalAs 0o444 outPath
                contents <- BS.readFile outPath
                pure (mode `andThen` assertEqual "contents" treeXz contents),
            runTestM "an executable fetch under a flat hash is refused with upstream's error" $
              refusedCase "flat-executable" ("/tool" :| []) [("executable", "1")] flatHash "hello" (const (pure ())) flatOutputRefusal
          ]
      pure (unpacked ++ others)
  where
    unpackAttrs = [("unpack", "1")]
    recursiveHash = (NarIngestion, "r:sha256")
    flatHash = (FlatIngestion, "sha256")
    -- The NAR of a regular file holding the given bytes, and of the
    -- executable file the executable case fetches.
    fileNar contents = BS.concat (map narSpecStr ["nix-archive-1", "(", "type", "regular", "contents", contents, ")"])
    toolNar = BS.concat (map narSpecStr ["nix-archive-1", "(", "type", "regular", "executable", "", "contents", "hello", ")"])
    -- The NAR of a symlink to the given target.  A relative target
    -- resolves beside the output, in the store's directory, where a test
    -- plants the file 'victimIn' names, outside every output.
    symlinkNar target = BS.concat (map narSpecStr ["nix-archive-1", "(", "type", "symlink", "target", target, ")"])
    victimName = "victim"
    victimIn dir = unStoreDir dir </> BS8.unpack victimName
    -- 'narSpecVector' restored and registered: every directory and the
    -- executable 0555, the plain file 0444, and the symlink as it was.
    restoredTree outPath = do
      let tool = outPath </> "bin" </> "tool"
          plain = outPath </> "data.txt"
      modes <- traverse (uncurry canonicalAs) [(0o555, outPath), (0o555, outPath </> "bin"), (0o555, tool), (0o444, plain)]
      flags <- traverse ExecBit.isExecutable [tool, plain]
      contents <- traverse BS.readFile [tool, plain]
      target <- Dir.getSymbolicLinkTarget (outPath </> "link")
      pure (foldr andThen Pass modes `andThen` assertEqual "tree" ([True, False], ["#!x", "hello\n"], "data.txt") (flags, contents, target))
    -- 'narSpecVector' compressed with xz 5.8.4 at -9 (244 bytes),
    -- embedded as base64 so the test source stays ASCII.
    treeXzB64 =
      "/Td6WFoAAATm1rRGBMC0AegGIQEcAAAAAAAAAECWtWrgA2cArF0ABoA2H++mzr92uhp6TyvqpFMk7aEjBHH/W7+XabrzKb4lenEr6bTaDX6aj51eJG3iFAbZDd8GA2XaYJrrkAHWgeGGgmFdILNyZZxSeSX3iaFCjpuwFBhssFJc2PhwJiJi0g8PV0XUxS/A206A1s9Roe9EEHR69sTLQiQYEFLSL17xYPIJsVKtCwROwCBJbpmdSTKWs6h14bbSSD5mKx99dPudGvwbMcpAf7NyGABJYU2iEzNN3AAB0AHoBgAAJ/RN+7HEZ/sCAAAAAARZWg==" :: Text

-- | 'substituteFromCaches' against loopback caches.  A path's reference
-- arrives first, so the path's registration finds it valid; 'substitutePaths'
-- fetches only the paths it is given, and the store refuses to register a
-- path whose reference is not valid.  A run asks each cache about each path
-- once, and a path's NAR comes from the cache whose narinfo gave the
-- references realised before it, as upstream's substitution goal takes
-- both from one substituter (@PathSubstitutionGoal@ at 2.24.9).
testSubstituteFromCaches :: IO [Bool]
testSubstituteFromCaches = do
  putStrLn "builder/substitute-from-caches"
  tmpBase <- getTemporaryDirectory
  shell <- findTestShell
  let dep = StorePath (T.replicate 32 "c") "dep"
      top = StorePath (T.replicate 32 "d") "top"
      other = StorePath (T.replicate 32 "r") "other"
      -- Each link references the next.
      chain = [StorePath (T.replicate 32 (T.singleton c)) ("link-" <> T.singleton c) | c <- "fghj"]
      chainEntries = zip chain (map (: []) (drop 1 chain) ++ [[]])
      notFound = Send (httpResponse "404 Not Found" "")
      unavailable = Send (httpResponse "503 Service Unavailable" "")
      multiOut = StorePath (T.replicate 32 "p") "multi"
      multiDev = StorePath (T.replicate 32 "q") "multi-dev"
      multiDrvSP = StorePath (T.replicate 31 "p" <> "1") "multi.drv"
      multiDrv =
        Derivation
          { drvOutputs = [DerivationOutput "out" multiOut "" "", DerivationOutput "dev" multiDev "" ""],
            drvInputDrvs = Map.empty,
            drvInputSrcs = [],
            drvPlatform = currentPlatform,
            drvBuilder = TE.encodeUtf8 shell,
            drvArgs = ["-c", "echo built > $out && echo built > $dev"],
            drvEnv = Map.singleton "name" "multi"
          }
      -- Realise the derivation with two outputs: the result, then what its
      -- out holds, which the builder writes and a cache serves otherwise.
      builtMulti config store = do
        writeDrv store multiDrv multiDrvSP
        result <- buildWithDeps config store multiDrv multiDrvSP
        let outFile = storePathToFilePath (stDir store) multiOut
        present <- Dir.doesFileExist outFile
        held <- if present then Just <$> BS.readFile outFile else pure Nothing
        pure (result, held)
      -- Run a substitution in a store of its own against caches answering
      -- as given, in priority order, all trusting the fixture's key and
      -- retrying without a real wait: what it returns, then the requests
      -- each cache saw.
      substitutedFrom :: String -> Text -> [BS.ByteString -> Int -> Reply] -> (BuildConfig -> Store -> IO outcome) -> IO (outcome, [[BS.ByteString]])
      substitutedFrom label key routes substitute =
        withCaches routes $ \served -> do
          let root = tmpBase </> ("nova-nix-test-substitute-from-caches-" ++ label)
              caches = zipWith (\priority (base, _) -> Subst.CacheConfig base [key] priority) [10, 20 ..] served
              config = (defaultBuildConfig (StoreDir root)) {bcCaches = caches, bcFetchRetry = defaultFetchRetryPolicy {frpBaseDelayMs = 1}}
          outcome <-
            bracket_ (forceRemoveIfExists root) (forceRemoveIfExists root) $
              bracket (openStore testLogger (StoreDir root)) closeStore (substitute config)
          (,) outcome <$> traverse snd served
      -- Substitute paths with their closure: the outcome, then whether each
      -- observed path is valid and the base names of what the store records
      -- it referencing.
      substitutedClosure paths observed config store = do
        outcome <- substituteFromCaches config store paths
        (,) outcome <$> traverse (registered store) observed
      registered store path = do
        valid <- isValid store path
        references <- queryReferences (stDB store) path
        pure (valid, map (T.pack . takeFileName . T.unpack) references)
      baseName path = spHash path <> "-" <> spName path
      -- The top path's own offer fetched without what it references, then
      -- whether each path is valid.
      fetchedAlone config store = do
        offer <- evalStateT (Subst.queryReferencesWith testLogger defaultFetchRetryPolicy (ioRetryEffects testLogger) Realise.NoFallback (bcCaches config) top) Subst.emptyCacheMemo
        outcome <- traverse (substitutePaths config store Realise.RegisterAll . (:| [])) (offeredSubstitute offer)
        (,) outcome <$> mapM (isValid store) [dep, top]
      -- The dep path's own substitute: the path it names, then what fetching
      -- it registering nothing and then registering it gives, each with
      -- whether the path is valid after.
      probedThenFetched config store = do
        offer <- evalStateT (Subst.queryReferencesWith testLogger defaultFetchRetryPolicy (ioRetryEffects testLogger) Realise.NoFallback (bcCaches config) dep) Subst.emptyCacheMemo
        let substitute = offeredSubstitute offer
            fetchAs registration = do
              outcome <- traverse (substitutePaths config store registration . (:| [])) substitute
              (,) outcome <$> isValid store dep
        probed <- fetchAs Realise.RegisterNone
        fetched <- fetchAs Realise.RegisterAll
        pure (map Subst.substitutePath (maybeToList substitute), probed, fetched)
  case (,,,,,) <$> servedPaths [(dep, []), (top, [dep])] <*> servedPaths chainEntries <*> servedPaths [(top, [])] <*> servedPaths [(top, [other]), (other, [])] <*> servedPaths [(multiOut, []), (multiDev, [])] <*> servedPaths [(multiOut, [])] of
    Left err -> (: []) <$> runTest "substitute-from-caches setup" (Fail (T.pack err))
    Right ((key, withDep), (_, links), (_, bare), (_, naming), (_, bothOutputs), (_, outOnly)) ->
      sequence
        [ runTestM "substituting a path from the caches fetches what it references first" $
            assertEqual "outcome and validity" (Realise.ClosureComplete, [True, True])
              . (\((outcome, paths), _) -> (outcome, map fst paths))
              <$> substitutedFrom "closure" key [const . withDep] (substitutedClosure [top] [dep, top]),
          runTestM "substituting the path alone is refused, since what it references is not valid" $
            assertEqual "outcome and validity" (Just (Realise.NotFetched ((top, Realise.DeliveryFailed) :| [])), [False, False])
              . fst
              <$> substitutedFrom "alone" key [const . withDep] fetchedAlone,
          runTestM "a substitute fetches the path its narinfo was checked for, and registers it only when asked to" $
            assertEqual "path, then outcome and validity unregistered and registered" ([dep], (Just Realise.Fetched, False), (Just Realise.Fetched, True))
              . fst
              <$> substitutedFrom "own-path" key [const . withDep] probedThenFetched,
          runTestM "a path whose NAR one cache fails to deliver comes from the next cache" $ do
            let failing target _
                  | target == narRequest dep = Send (httpResponse "501 Not Implemented" "")
                  | otherwise = withDep target
            result <- substitutedFrom "nar-failed" key [failing, const . withDep] (substitutedClosure [dep] [dep])
            pure $
              assertEqual
                "outcome, registration and requests"
                ((Realise.ClosureComplete, [(True, [])]), [[narInfoRequest dep, narRequest dep], [narInfoRequest dep, narRequest dep]])
                result,
          runTestM "a closure of four paths requests each narinfo once" $ do
            ((outcome, paths), seen) <- substitutedFrom "narinfo-once" key [const . links] (substitutedClosure (take 1 chain) chain)
            pure $
              assertEqual
                "outcome, validity and narinfo requests"
                (Realise.ClosureComplete, map (const True) chain, sort (map narInfoRequest chain))
                (outcome, map fst paths, sort (concatMap (filter (".narinfo" `BS.isSuffixOf`)) seen)),
          -- The first cache stops holding the narinfo once it has served
          -- it, and the next cache says the path references another.  The
          -- path comes from the first, as the narinfo its references were
          -- read from describes it, and the next cache is never asked.
          runTestM "a path is fetched with the narinfo its references were read from" $ do
            let once target earlier
                  | target == narInfoRequest top && earlier > 0 = notFound
                  | otherwise = bare target
            result <- substitutedFrom "same-narinfo" key [once, const . naming] (substitutedClosure [top] [top])
            pure (assertEqual "outcome, registration and requests" ((Realise.ClosureComplete, [(True, [])]), [[narInfoRequest top, narRequest top], []]) result),
          -- Upstream's goal moves to its next substituter when the NAR is
          -- gone, and realises what that one's path info references first.
          runTestM "a path whose NAR is gone from one cache comes from the next, after what that cache says it references" $ do
            let gone target _
                  | target == narRequest top = notFound
                  | otherwise = bare target
            ((outcome, paths), seen) <- substitutedFrom "nar-gone" key [gone, const . naming] (substitutedClosure [top] [other, top])
            pure $
              assertEqual
                "outcome, registrations and requests"
                ( Realise.ClosureComplete,
                  [(True, []), (True, [baseName other])],
                  [ [narInfoRequest top, narRequest top, narInfoRequest other],
                    [narInfoRequest top, narInfoRequest other, narRequest other, narRequest top]
                  ]
                )
                (outcome, paths, seen),
          -- The hole sends each derivation to its inputs and back to its
          -- outputs, which reference the hole again.
          runTestM "a path no cache holds is asked about once in a run" $ do
            let hole = StorePath (T.replicate 32 "z") "hole"
                midOut = StorePath (T.replicate 32 "m") "mid"
                topOut = StorePath (T.replicate 32 "k") "top-out"
                midDrvSP = StorePath (T.replicate 31 "m" <> "1") "mid.drv"
                topDrvSP = StorePath (T.replicate 31 "k" <> "1") "top.drv"
                (builder, builderArgs)
                  | SI.os == "mingw32" = ("cmd.exe", ["/c", "echo built>%out%"])
                  | otherwise = ("/bin/sh", ["-c", "echo built > $out"])
                hollowDrv name out inputs =
                  Derivation
                    { drvOutputs = [DerivationOutput "out" out "" ""],
                      drvInputDrvs = inputs,
                      drvInputSrcs = [],
                      drvPlatform = currentPlatform,
                      drvBuilder = builder,
                      drvArgs = builderArgs,
                      drvEnv = Map.singleton "name" name
                    }
                midDrv = hollowDrv "mid" midOut Map.empty
                topDrv = hollowDrv "top" topOut (Map.singleton midDrvSP ["out"])
            case servedPaths [(topOut, [hole]), (midOut, [hole])] of
              Left err -> pure (Fail (T.pack err))
              Right (_, holed) -> do
                result <- substitutedFrom "hole" key [const . holed] $ \config store -> do
                  writeDrv store midDrv midDrvSP
                  writeDrv store topDrv topDrvSP
                  buildWithDeps config store topDrv topDrvSP
                pure (assertEqual "result and narinfo requests" (BuildSuccess topOut, [map narInfoRequest [topOut, hole, midOut]]) result),
          -- The first cache no longer holds dev's NAR and fails to deliver
          -- out's, and the next holds out alone.  Upstream's goal for out
          -- arrives from the next cache and dev's ends for want of a
          -- substitute, so no goal failed and the derivation builds
          -- (DerivationGoal::outputsSubstitutionTried in derivation-goal.cc
          -- at 2.24.9, DerivationGoal::haveDerivation at 2.36.0).  Out is
          -- fetched from the next cache to learn that, and not registered,
          -- since dev cannot arrive.
          runTestM "an output that fails at one cache and arrives from the next builds its derivation when a sibling has no cache left" $ do
            let first target _
                  | target == narRequest multiDev = notFound
                  | target == narRequest multiOut = unavailable
                  | otherwise = bothOutputs target
            result <- substitutedFrom "sibling-out" key [first, const . outOnly] builtMulti
            pure $
              assertEqual
                "result, out's contents and requests"
                ( (BuildSuccess multiOut, Just "built\n"),
                  [ map narInfoRequest [multiOut, multiDev] ++ replicate 5 (narRequest multiOut) ++ [narRequest multiDev],
                    [narInfoRequest multiOut, narInfoRequest multiDev, narRequest multiOut]
                  ]
                )
                result,
          -- Out fails at the first cache and arrives from the next, and the
          -- first cache's dev NAR, delivered once, is gone when the batch is
          -- fetched again.  Out's failure is behind it, so the batch is only
          -- unavailable, and the derivation builds.
          runTestM "an output that failed at one cache and arrived from the next fails nothing when its sibling is gone" $ do
            let first target earlier
                  | target == narRequest multiOut = unavailable
                  | target == narRequest multiDev && earlier > 0 = notFound
                  | otherwise = bothOutputs target
            result <- substitutedFrom "sibling-gone" key [first, const . outOnly] builtMulti
            pure $
              assertEqual
                "result, out's contents and requests"
                ( (BuildSuccess multiOut, Just "built\n"),
                  [ map narInfoRequest [multiOut, multiDev] ++ replicate 5 (narRequest multiOut) ++ [narRequest multiDev, narRequest multiDev],
                    [narInfoRequest multiOut, narRequest multiOut, narInfoRequest multiDev]
                  ]
                )
                result
        ]

-- | 'buildWithDeps' against a loopback cache, for inputs asked for some
-- of their outputs: each is realised for the outputs its dependent's
-- @.drv@ names, as upstream's goal for it is (@checkPathValidity@ and
-- @haveDerivation@, derivation-goal.cc at 2.24.9).  lib has the outputs
-- out, dev and doc, and the cache never serves doc.
testWantedOutputsIO :: IO [Bool]
testWantedOutputsIO = do
  putStrLn "builder/wanted-outputs"
  shell <- findTestShell
  tmpBase <- getTemporaryDirectory
  let libOut = StorePath (T.replicate 32 "1") "lib"
      libDev = StorePath (T.replicate 32 "2") "lib-dev"
      libDoc = StorePath (T.replicate 32 "3") "lib-doc"
      midOut = StorePath (T.replicate 32 "4") "mid"
      appOut = StorePath (T.replicate 32 "5") "app"
      -- lib.drv sorts before mid.drv, so app's request for lib comes
      -- before mid's.
      libDrvSP = StorePath (T.replicate 32 "f") "lib.drv"
      midDrvSP = StorePath (T.replicate 32 "g") "mid.drv"
      appDrvSP = StorePath (T.replicate 32 "6") "app.drv"
      outNar = NAR.serialise (NAR.NarRegular False "cache out\n")
      devNar = NAR.serialise (NAR.NarRegular False "cache dev\n")
      narInfoTarget sp = "/" <> TE.encodeUtf8 (spHash sp) <> ".narinfo"
      derivationOf name outputs inputs script =
        Derivation
          { drvOutputs = [DerivationOutput outputName sp "" "" | (outputName, sp) <- outputs],
            drvInputDrvs = Map.fromList inputs,
            drvInputSrcs = [],
            drvPlatform = currentPlatform,
            drvBuilder = TE.encodeUtf8 shell,
            drvArgs = ["-c", script],
            drvEnv = Map.singleton "name" name
          }
      libDrv = derivationOf "lib" [("out", libOut), ("dev", libDev), ("doc", libDoc)] []
      midDrv = derivationOf "mid" [("out", midOut)] [(libDrvSP, ["out"])] "echo built > $out"
      appDrv inputs = derivationOf "app" [("out", appOut)] inputs "echo built > $out"
      contents store sp = either (const Nothing) Just <$> (try (BS.readFile (storePathToFilePath (stDir store) sp)) :: IO (Either IOException BS.ByteString))
      -- Build app, over lib and mid as it asks for them, against a cache
      -- serving these files under a trusted key: the result, the requests
      -- the cache saw, what lib's three outputs hold, and whether mid's
      -- output is valid.
      realised key label served libScript inputs =
        withHttpServer (\target _ -> maybe (Send (httpResponse "404 Not Found" "")) (Send . httpResponse "200 OK") (lookup target served)) $ \base requests _ -> do
          let root = tmpBase </> ("nova-nix-test-wanted-outputs-" ++ label)
              config = (defaultBuildConfig (StoreDir root)) {bcCaches = [Subst.CacheConfig base [key] 40]}
          bracket_ (forceRemoveIfExists root) (forceRemoveIfExists root) $
            bracket (openStore testLogger (StoreDir root)) closeStore $ \store -> do
              writeDrv store (libDrv libScript) libDrvSP
              writeDrv store midDrv midDrvSP
              writeDrv store (appDrv inputs) appDrvSP
              result <- buildWithDeps config store (appDrv inputs) appDrvSP
              seen <- requests
              held <- mapM (contents store) [libOut, libDev, libDoc]
              midValid <- isValid store midOut
              pure (result, seen, held, midValid)
      signed = do
        (key, outNarInfo) <- signedTestNarInfo libOut [] "nar/lib.nar" outNar
        (_, devReferencingOut) <- signedTestNarInfo libDev [libOut] "nar/lib-dev.nar" devNar
        (_, devAlone) <- signedTestNarInfo libDev [] "nar/lib-dev.nar" devNar
        pure (key, outNarInfo, devReferencingOut, devAlone)
  case signed of
    Left err -> (: []) <$> runTest "wanted-outputs setup" (Fail (T.pack err))
    Right (key, outNarInfo, devReferencingOut, devAlone) -> do
      let served narInfos nars = [(narInfoTarget sp, TE.encodeUtf8 narInfo) | (sp, narInfo) <- narInfos] ++ nars
      sequence
        [ -- lib's builder fails if it runs.
          runTestM "a dependency on one output fetches that output and its closure only, and builds nothing for the rest" $ do
            (result, seen, held, _) <-
              realised
                key
                "one-output"
                (served [(libDev, devReferencingOut), (libOut, outNarInfo)] [("/nar/lib-dev.nar", devNar), ("/nar/lib.nar", outNar)])
                "exit 1"
                [(libDrvSP, ["dev"])]
            pure $
              assertEqual
                "result, lib's outputs, and whether doc was asked for"
                (BuildSuccess appOut, [Just "cache out\n", Just "cache dev\n", Nothing], False)
                (result, held, narInfoTarget libDoc `elem` seen),
          -- lib's dev comes from the cache for app.  mid's request for out,
          -- which no cache serves, then builds lib beside the dev it has.
          runTestM "a later request for another output of the same derivation builds it" $ do
            (result, _, held, midValid) <-
              realised
                key
                "later-request"
                (served [(libDev, devAlone)] [("/nar/lib-dev.nar", devNar)])
                "echo built > $out && echo built > $dev && echo built > $doc"
                [(libDrvSP, ["dev"]), (midDrvSP, ["out"])]
            pure $
              assertEqual
                "result, lib's outputs, and whether mid's is valid"
                (BuildSuccess appOut, [Just "built\n", Just "cache dev\n", Just "built\n"], True)
                (result, held, midValid)
        ]

-- | Run an action against loopback servers, one for each route, with each
-- one's base URL and the requests it saw.
withCaches :: [BS.ByteString -> Int -> Reply] -> ([(Text, IO [BS.ByteString])] -> IO a) -> IO a
withCaches [] action = action []
withCaches (route : rest) action =
  withHttpServer route $ \base requests _ ->
    withCaches rest (\others -> action ((base, requests) : others))

-- ---------------------------------------------------------------------------
-- Tests: Builder (Phase 2, Batch 4)
-- ---------------------------------------------------------------------------

-- | 'mkTestBuildDrv' with a fixed-output spec: registration must
-- re-verify the placed bytes against the declared digest.
mkFixedOutputDrv :: Text -> StorePath -> Text -> Text -> Text -> Derivation
mkFixedOutputDrv shell outSP script algoField hexDigest =
  (mkTestBuildDrv shell outSP script)
    { drvOutputs =
        [ DerivationOutput
            { doName = "out",
              doPath = outSP,
              doHashAlgo = algoField,
              doHash = hexDigest
            }
        ]
    }

testBuilder :: IO [Bool]
testBuilder = do
  putStrLn "builder"
  shell <- findTestShell
  sequence
    [ -- Build simple script that writes to $out
      runTestM "build simple script" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-builder1"
        forceRemoveIfExists tmpStore
        store <- openStore testLogger (StoreDir tmpStore)
        let outSP = StorePath "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1" "simple-test"
            drv = mkTestBuildDrv shell outSP "mkdir -p $out && echo hello > $out/result.txt"
            config = defaultBuildConfig (stDir store)
        result <- buildDerivation config store drv testDrvPath
        closeStore store
        let ret = case result of
              BuildSuccess sp -> assertEqual "success path" outSP sp
              BuildFailure msg code -> Fail ("build failed (" <> T.pack (show code) <> "): " <> msg)
        forceRemoveIfExists tmpStore
        pure ret,
      -- Build with specific file content
      runTestM "build writes file content" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-builder2"
        forceRemoveIfExists tmpStore
        store <- openStore testLogger (StoreDir tmpStore)
        let outSP = StorePath "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" "content-test"
            drv = mkTestBuildDrv shell outSP "mkdir -p $out && echo 'test content 42' > $out/data.txt"
            config = defaultBuildConfig (stDir store)
        result <- buildDerivation config store drv testDrvPath
        ret <- case result of
          BuildSuccess _ -> do
            let dataFile = storePathToFilePath (stDir store) outSP </> "data.txt"
            content <- TIO.readFile dataFile
            pure $
              if T.strip content == "test content 42"
                then Pass
                else Fail ("unexpected content: " <> content)
          BuildFailure msg code -> pure (Fail ("build failed (" <> T.pack (show code) <> "): " <> msg))
        closeStore store
        forceRemoveIfExists tmpStore
        pure ret,
      -- Missing builder fails
      runTestM "missing builder fails" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-builder3"
        forceRemoveIfExists tmpStore
        store <- openStore testLogger (StoreDir tmpStore)
        let outSP = StorePath "cccccccccccccccccccccccccccccccc" "fail-test"
            drv =
              Derivation
                { drvOutputs = [DerivationOutput "out" outSP "" ""],
                  drvInputDrvs = Map.empty,
                  drvInputSrcs = [],
                  drvPlatform = currentPlatform,
                  drvBuilder = "/nonexistent/builder",
                  drvArgs = [],
                  drvEnv = Map.empty
                }
            config = defaultBuildConfig (stDir store)
        result <- buildDerivation config store drv testDrvPath
        closeStore store
        forceRemoveIfExists tmpStore
        pure $ case result of
          BuildFailure _ _ -> Pass
          BuildSuccess _ -> Fail "expected failure for missing builder",
      -- Exit failure returns error code
      runTestM "exit failure returns error code" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-builder4"
        forceRemoveIfExists tmpStore
        store <- openStore testLogger (StoreDir tmpStore)
        let outSP = StorePath "dddddddddddddddddddddddddddddddd" "exitfail"
            drv = mkTestBuildDrv shell outSP "exit 42"
            config = defaultBuildConfig (stDir store)
        result <- buildDerivation config store drv testDrvPath
        closeStore store
        forceRemoveIfExists tmpStore
        pure $ case result of
          BuildFailure _ code -> if code == 42 then Pass else Fail ("expected code 42, got " <> T.pack (show code))
          BuildSuccess _ -> Fail "expected failure",
      -- A builder that throws (a nonexistent executable) leaves nothing
      -- in the directory that holds the store's build directories.
      runTestM "crashed build removes its build dir" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-builder-leak"
        forceRemoveIfExists tmpStore
        store <- openStore testLogger (StoreDir tmpStore)
        let outSP = StorePath "pppppppppppppppppppppppppppppppp" "leaktest"
            drv = mkTestBuildDrv (T.pack (tmpBase </> "no-such-builder.exe")) outSP "unused"
            config = defaultBuildConfig (stDir store)
            buildsRoot = takeDirectory (buildDirFor (stDir store) outSP)
        result <- buildDerivation config store drv testDrvPath
        leftovers <- do
          exists <- doesDirectoryExist buildsRoot
          if exists then Dir.listDirectory buildsRoot else pure []
        closeStore store
        forceRemoveIfExists tmpStore
        pure $ case result of
          BuildFailure _ _
            | null leftovers -> Pass
            | otherwise -> Fail ("build dir leaked: " <> T.pack (show leftovers))
          BuildSuccess _ -> Fail "expected failure for a nonexistent builder",
      -- A derivation carrying __json is built as upstream builds one: its
      -- attributes, outputs included, arrive as .attrs.json and .attrs.sh
      -- and none of them as a variable.  The builder copies both files
      -- out, finding $out where a structured builder must, in the files.
      runTestM "structured attributes reach the builder as .attrs.json and .attrs.sh" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-builder-structured"
        forceRemoveIfExists tmpStore
        store <- openStore testLogger (StoreDir tmpStore)
        let outSP = StorePath "ssssssssssssssssssssssssssssssss" "structured"
            outText = T.pack (storePathToFilePath (stDir store) outSP)
            attrs =
              "{\"greeting\":\"it's\",\"list\":[\"a\",1,true,null],\"nested\":{\"k\":\"v\"},\"outputs\":[\"out\"],\"self\":\""
                <> hashPlaceholder "out"
                <> "/bin\"}"
            script =
              T.unlines
                [ "probe=\"${out-unset}|${__json-unset}|${greeting-unset}\"",
                  -- Under structured attributes no variable the derivation
                  -- sets reaches the builder, so the tools are named whole.
                  "out=$(/usr/bin/sed -n \"/^declare -A outputs=/s/.*\\['out'\\]='\\([^']*\\)'.*/\\1/p\" \"$NIX_ATTRS_SH_FILE\")",
                  "/bin/mkdir \"$out\" && /bin/cp \"$NIX_ATTRS_JSON_FILE\" \"$out/attrs.json\" && /bin/cp \"$NIX_ATTRS_SH_FILE\" \"$out/attrs.sh\" && printf '%s' \"$probe\" > \"$out/env\""
                ]
            drv =
              (mkTestBuildDrv shell outSP script)
                { drvEnv = Map.fromList [("__json", TE.encodeUtf8 attrs), ("out", "/nix/store/ssssssssssssssssssssssssssssssss-structured")]
                }
            config = defaultBuildConfig (stDir store)
            jsonPath = "\"" <> T.replace "\\" "\\\\" outText <> "\""
            expectedJson =
              "{\"greeting\":\"it's\",\"list\":[\"a\",1,true,null],\"nested\":{\"k\":\"v\"},\"outputs\":{\"out\":"
                <> jsonPath
                <> "},\"self\":"
                <> T.dropEnd 1 jsonPath
                <> "/bin\"}"
            expectedShell =
              T.concat
                [ "declare greeting='it'\\''s'\n",
                  "declare -a list=('a' 1 1 '' )\n",
                  "declare -A nested=(['k']='v' )\n",
                  "declare -A outputs=(['out']='" <> outText <> "' )\n",
                  "declare self='" <> outText <> "/bin'\n"
                ]
        result <- buildDerivation config store drv testDrvPath
        ret <- case result of
          BuildSuccess _ -> do
            let outDir = storePathToFilePath (stDir store) outSP
            json <- BS.readFile (outDir </> "attrs.json")
            shellText <- BS.readFile (outDir </> "attrs.sh")
            probe <- BS.readFile (outDir </> "env")
            pure $
              assertEqual "attrs.json" (TE.encodeUtf8 expectedJson) json
                `andThen` assertEqual "attrs.sh" (TE.encodeUtf8 expectedShell) shellText
                `andThen` assertEqual "no attribute is a variable" "unset|unset|unset" probe
          BuildFailure msg code -> pure (Fail ("build failed (" <> T.pack (show code) <> "): " <> msg))
        closeStore store
        forceRemoveIfExists tmpStore
        pure ret,
      -- An attribute passAsFile names arrives as upstream's file in the
      -- build directory, named by fooPath: its store path and placeholder
      -- rewritten as a variable's would be, its other bytes as they are,
      -- invalid UTF-8 included.  Neither it nor a name with no attribute
      -- behind it is a variable.
      runTestM "passAsFile attributes reach the builder as files in the build directory" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-builder-passasfile"
        forceRemoveIfExists tmpStore
        store <- openStore testLogger (StoreDir tmpStore)
        let outSP = StorePath "gggggggggggggggggggggggggggggggg" "passasfile"
            outDir = storePathToFilePath (stDir store) outSP
            dep = "/dddddddddddddddddddddddddddddddd-dep"
            value = "\xff " <> TE.encodeUtf8 ("/nix/store" <> dep <> " " <> hashPlaceholder "out")
            expected = "\xff " <> TE.encodeUtf8 (T.pack (unStoreDir (stDir store)) <> dep <> " " <> T.pack outDir)
            script = "mkdir \"$out\" && cp \"$fooPath\" \"$out/foo\" && printf '%s|%s|%s' \"${foo-unset}\" \"${missingPath-unset}\" \"$fooPath\" > \"$out/env\""
            base = mkTestBuildDrv shell outSP script
            drv = base {drvEnv = Map.union (Map.fromList [("foo", value), ("passAsFile", "foo missing")]) (drvEnv base)}
            config = defaultBuildConfig (stDir store)
            fooFile = buildDirFor (stDir store) outSP </> ".attr-1bp7cri8hplaz6hbz0v4f0nl44rl84q1sg25kgwqzipzd1mv89ic"
        result <- buildDerivation config store drv testDrvPath
        ret <- case result of
          BuildSuccess _ -> do
            contents <- BS.readFile (outDir </> "foo")
            probe <- BS.readFile (outDir </> "env")
            pure $
              assertEqual "file contents" expected contents
                `andThen` assertEqual "variables" (TE.encodeUtf8 ("unset|unset|" <> T.pack fooFile)) probe
          BuildFailure msg code -> pure (Fail ("build failed (" <> T.pack (show code) <> "): " <> msg))
        closeStore store
        forceRemoveIfExists tmpStore
        pure ret,
      -- Output at expected path
      runTestM "output at expected store path" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-builder5"
        forceRemoveIfExists tmpStore
        store <- openStore testLogger (StoreDir tmpStore)
        let outSP = StorePath "cccccccccccccccccccccccccccccccc" "pathtest"
            drv = mkTestBuildDrv shell outSP "mkdir -p $out && touch $out/marker"
            config = defaultBuildConfig (stDir store)
        result <- buildDerivation config store drv testDrvPath
        ret <- case result of
          BuildSuccess _ -> do
            let expectedDir = storePathToFilePath (stDir store) outSP
            exists <- doesDirectoryExist expectedDir
            pure $ if exists then Pass else Fail "output dir doesn't exist at expected path"
          BuildFailure msg code -> pure (Fail ("build failed (" <> T.pack (show code) <> "): " <> msg))
        closeStore store
        forceRemoveIfExists tmpStore
        pure ret,
      -- A leftover tree at the output path that is NOT valid in the DB has
      -- unknowable integrity (interrupted earlier run); the build replaces
      -- it with the fresh output - upstream's delete-then-move - clearing
      -- read-only marks rather than adopting stale bytes.
      runTestM "stale leftover output is replaced by the fresh build" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-builder-stale"
        forceRemoveIfExists tmpStore
        store <- openStore testLogger (StoreDir tmpStore)
        let outSP = StorePath "qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq" "staletest"
            drv = mkTestBuildDrv shell outSP "mkdir -p $out && echo fresh > $out/data.txt"
            config = defaultBuildConfig (stDir store)
            targetPath = storePathToFilePath (stDir store) outSP
        -- Fake the interrupted run: stale read-only content at the output's
        -- store location, never registered.
        createDirectoryIfMissing True targetPath
        TIO.writeFile (targetPath </> "data.txt") "stale"
        canonicalizeMetadata targetPath
        result <- buildDerivation config store drv testDrvPath
        content <- TIO.readFile (targetPath </> "data.txt")
        registered <- isValid store outSP
        closeStore store
        forceRemoveIfExists tmpStore
        pure $ case result of
          BuildSuccess _
            | T.strip content == "fresh" && registered -> Pass
            | otherwise -> Fail ("expected fresh registered content, got: " <> content)
          BuildFailure msg code -> Fail ("build failed (" <> T.pack (show code) <> "): " <> msg),
      -- Path registered in DB after build
      runTestM "path registered in DB" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-builder6"
        forceRemoveIfExists tmpStore
        store <- openStore testLogger (StoreDir tmpStore)
        let outSP = StorePath "ffffffffffffffffffffffffffffffff" "dbtest"
            drv = mkTestBuildDrv shell outSP "mkdir -p $out && touch $out/file"
            config = defaultBuildConfig (stDir store)
        result <- buildDerivation config store drv testDrvPath
        ret <- case result of
          BuildSuccess _ -> do
            valid <- isValid store outSP
            pure $ if valid then Pass else Fail "path not valid in DB after build"
          BuildFailure msg code -> pure (Fail ("build failed (" <> T.pack (show code) <> "): " <> msg))
        closeStore store
        forceRemoveIfExists tmpStore
        pure ret,
      -- Multiple outputs
      runTestM "multiple outputs" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-builder7"
        forceRemoveIfExists tmpStore
        store <- openStore testLogger (StoreDir tmpStore)
        let outSP = StorePath "ggggggggggggggggggggggggggggggg1" "multi"
            devSP = StorePath "ggggggggggggggggggggggggggggggg2" "multi-dev"
            drv =
              Derivation
                { drvOutputs =
                    [ DerivationOutput "out" outSP "" "",
                      DerivationOutput "dev" devSP "" ""
                    ],
                  drvInputDrvs = Map.empty,
                  drvInputSrcs = [],
                  drvPlatform = currentPlatform,
                  drvBuilder = TE.encodeUtf8 shell,
                  drvArgs = ["-c", "mkdir -p $out && echo lib > $out/lib.txt && mkdir -p $dev && echo headers > $dev/include.h"],
                  drvEnv = Map.union (Map.fromList [("name", "multi")]) hostToolsEnv
                }
            config = defaultBuildConfig (stDir store)
        result <- buildDerivation config store drv testDrvPath
        ret <- case result of
          BuildSuccess _ -> do
            outExists <- doesDirectoryExist (storePathToFilePath (stDir store) outSP)
            devExists <- doesDirectoryExist (storePathToFilePath (stDir store) devSP)
            pure $
              if outExists && devExists
                then Pass
                else Fail ("out exists=" <> T.pack (show outExists) <> " dev exists=" <> T.pack (show devExists))
          BuildFailure msg code -> pure (Fail ("build failed (" <> T.pack (show code) <> "): " <> msg))
        closeStore store
        forceRemoveIfExists tmpStore
        pure ret,
      -- Upstream records the .drv as every output's deriver
      -- (registerOutputs in local-derivation-goal.cc at 2.24.9,
      -- derivation-builder-impl.cc at 2.36.0), and a push names it in the
      -- narinfo's Deriver field, a basename as on the wire.
      runTestM "every built output names its .drv as its deriver, and a push's narinfo carries it" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-builder-deriver"
        forceRemoveIfExists tmpStore
        store <- openStore testLogger (StoreDir tmpStore)
        let outSP = StorePath "ggggggggggggggggggggggggggggggg9" "deriver"
            devSP = StorePath "gggggggggggggggggggggggggggggggb" "deriver-dev"
            drv =
              (mkTestBuildDrv shell outSP "mkdir -p $out $dev")
                { drvOutputs = [DerivationOutput "out" outSP "" "", DerivationOutput "dev" devSP "" ""]
                }
            drvText = T.pack (storePathToFilePath (stDir store) testDrvPath)
        result <- buildDerivation (defaultBuildConfig (stDir store)) store drv testDrvPath
        ret <- case result of
          BuildSuccess _ -> do
            derivers <- traverse (queryDeriver (stDB store)) [outSP, devSP]
            prepared <- runExceptT (preparePath PushNone store outSP)
            pure $
              assertEqual "derivers" [Just drvText, Just drvText] derivers
                `andThen` assertRight
                  "push"
                  prepared
                  ( \(_, narInfo) ->
                      assertEqual "Deriver" (Just (storePathBasename testDrvPath)) (NarInfo.niDeriver narInfo)
                        `andThen` assertEqual
                          "rendered"
                          True
                          (("\nDeriver: " <> storePathBasename testDrvPath <> "\n") `T.isInfixOf` NarInfo.renderNarInfo narInfo)
                  )
          BuildFailure msg code -> pure (Fail ("build failed (" <> T.pack (show code) <> "): " <> msg))
        closeStore store
        forceRemoveIfExists tmpStore
        pure ret,
      runTestM "partial rebuild failure preserves a valid sibling output" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-builder-partial-valid"
        forceRemoveIfExists tmpStore
        store <- openStore testLogger (StoreDir tmpStore)
        let outSP = StorePath "ggggggggggggggggggggggggggggggg3" "partial-main"
            devSP = StorePath "ggggggggggggggggggggggggggggggg4" "partial-dev"
            mkDrv script =
              Derivation
                { drvOutputs =
                    [ DerivationOutput "out" outSP "" "",
                      DerivationOutput "dev" devSP "" ""
                    ],
                  drvInputDrvs = Map.empty,
                  drvInputSrcs = [],
                  drvPlatform = currentPlatform,
                  drvBuilder = TE.encodeUtf8 shell,
                  drvArgs = ["-c", script],
                  drvEnv = Map.union (Map.fromList [("name", "partial-valid")]) hostToolsEnv
                }
            config = defaultBuildConfig (stDir store)
            devPath = storePathToFilePath (stDir store) devSP
        initial <- buildDerivation config store (mkDrv "mkdir -p $out $dev && echo main > $out/value && echo keep > $dev/value") testDrvPath
        deleted <- deleteNamed store (spHash outSP <> "-" <> spName outSP)
        rebuilt <- buildDerivation config store (mkDrv "mkdir -p $out && echo partial > $out/value && exit 1") testDrvPath
        devOnDisk <- Dir.doesPathExist devPath
        devValid <- isValid store devSP
        devContents <- if devOnDisk then T.strip <$> TIO.readFile (devPath </> "value") else pure ""
        closeStore store
        forceRemoveIfExists tmpStore
        let deletedFirst = either (const False) (\outcome -> doRowRemoved outcome && doTreeRemoved outcome) deleted
        pure $ case (initial, rebuilt) of
          (BuildSuccess _, BuildFailure _ _)
            | deletedFirst && devOnDisk && devValid && devContents == "keep" -> Pass
            | otherwise -> Fail "failed partial rebuild removed or changed its valid sibling output"
          _ -> Fail "partial rebuild test setup did not reach the expected states",
      -- The shape the reviewer's blocker had: a package with a dev output
      -- has ONE builder writing both, so a partial rebuild hands that
      -- builder a $dev that is valid, registered and sealed read-only.
      -- Writing there has to work and must not disturb what is registered.
      runTestM "partial rebuild writes both outputs and leaves the valid one alone" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-builder-partial-both"
        forceRemoveIfExists tmpStore
        store <- openStore testLogger (StoreDir tmpStore)
        let outSP = StorePath "ggggggggggggggggggggggggggggggg5" "both-main"
            devSP = StorePath "ggggggggggggggggggggggggggggggg6" "both-dev"
            mkDrv script =
              Derivation
                { drvOutputs =
                    [ DerivationOutput "out" outSP "" "",
                      DerivationOutput "dev" devSP "" ""
                    ],
                  drvInputDrvs = Map.empty,
                  drvInputSrcs = [],
                  drvPlatform = currentPlatform,
                  drvBuilder = TE.encodeUtf8 shell,
                  drvArgs = ["-c", script],
                  drvEnv = Map.union (Map.fromList [("name", "partial-both")]) hostToolsEnv
                }
            config = defaultBuildConfig (stDir store)
            devPath = storePathToFilePath (stDir store) devSP
            outPath = storePathToFilePath (stDir store) outSP
            -- Both outputs, every time, as a real multi-output builder does.
            writeBoth v = "mkdir -p $out $dev && echo " <> v <> " > $out/value && echo " <> v <> " > $dev/value"
        initial <- buildDerivation config store (mkDrv (writeBoth "first")) testDrvPath
        deleted <- deleteNamed store (spHash outSP <> "-" <> spName outSP)
        rebuilt <- buildDerivation config store (mkDrv (writeBoth "second")) testDrvPath
        outContents <- T.strip <$> TIO.readFile (outPath </> "value")
        devContents <- T.strip <$> TIO.readFile (devPath </> "value")
        devValid <- isValid store devSP
        outValid <- isValid store outSP
        closeStore store
        forceRemoveIfExists tmpStore
        let deletedFirst = either (const False) (\outcome -> doRowRemoved outcome && doTreeRemoved outcome) deleted
        pure $ case (initial, rebuilt) of
          (BuildSuccess _, BuildSuccess _)
            | not deletedFirst -> Fail "partial rebuild setup did not delete the first output"
            -- The valid sibling keeps the bytes its NarHash describes: the
            -- builder wrote to a scratch path, not to it.
            | devContents /= "first" -> Fail ("valid sibling was overwritten: " <> devContents)
            | outContents /= "second" -> Fail ("rebuilt output has the wrong contents: " <> outContents)
            | not (outValid && devValid) -> Fail "both outputs should be valid after the rebuild"
            | otherwise -> Pass
          (_, BuildFailure msg _) -> Fail ("a builder writing both outputs failed the rebuild: " <> msg)
          _ -> Fail "partial rebuild test setup did not reach the expected states",
      -- The scratch path is discarded after the build, so an output that
      -- records it would be left naming nothing.  The hash is rewritten to
      -- the sibling's final one, as upstream rewrites it.
      runTestM "an output recording a valid sibling's scratch path names the sibling" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-builder-scratch-leak"
        forceRemoveIfExists tmpStore
        store <- openStore testLogger (StoreDir tmpStore)
        let outSP = StorePath "ggggggggggggggggggggggggggggggg7" "leak-main"
            devSP = StorePath "ggggggggggggggggggggggggggggggg8" "leak-dev"
            mkDrv script =
              Derivation
                { drvOutputs =
                    [ DerivationOutput "out" outSP "" "",
                      DerivationOutput "dev" devSP "" ""
                    ],
                  drvInputDrvs = Map.empty,
                  drvInputSrcs = [],
                  drvPlatform = currentPlatform,
                  drvBuilder = TE.encodeUtf8 shell,
                  drvArgs = ["-c", script],
                  drvEnv = Map.union (Map.fromList [("name", "scratch-leak")]) hostToolsEnv
                }
            config = defaultBuildConfig (stDir store)
            devPath = storePathToFilePath (stDir store) devSP
            outPath = storePathToFilePath (stDir store) outSP
        initial <- buildDerivation config store (mkDrv "mkdir -p $out $dev && echo first > $out/value && echo first > $dev/value") testDrvPath
        deleted <- deleteNamed store (spHash outSP <> "-" <> spName outSP)
        -- \$dev now names a scratch path, and the builder bakes it into $out
        -- the way a compiler driver or a libtool archive would.
        rebuilt <- buildDerivation config store (mkDrv "mkdir -p $out $dev && echo $dev > $out/recorded && echo first > $dev/value") testDrvPath
        devContents <- T.strip <$> TIO.readFile (devPath </> "value")
        recorded <- T.strip <$> TIO.readFile (outPath </> "recorded")
        devValid <- isValid store devSP
        outValid <- isValid store outSP
        references <- queryReferences (stDB store) outSP
        closeStore store
        forceRemoveIfExists tmpStore
        let deletedFirst = either (const False) (\outcome -> doRowRemoved outcome && doTreeRemoved outcome) deleted
        pure $ case (initial, rebuilt) of
          (BuildSuccess _, BuildSuccess _)
            | not deletedFirst -> Fail "scratch-leak setup did not delete the first output"
            | not (outValid && devValid) -> Fail "both outputs should be valid after the rebuild"
            | devContents /= "first" -> Fail ("valid sibling was overwritten: " <> devContents)
            | recorded /= T.pack devPath -> Fail ("the output did not name the sibling: " <> recorded)
            | map (T.takeEnd (T.length (spName devSP))) references /= [spName devSP] -> Fail ("expected only the sibling as a reference: " <> T.pack (show references))
            | otherwise -> Pass
          (_, BuildFailure msg _) -> Fail ("the rebuild failed: " <> msg)
          _ -> Fail "scratch-leak test setup did not reach the expected states",
      -- Builder succeeds but doesn't create $out, so the build fails
      runTestM "missing output fails build" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-builder-noout"
        forceRemoveIfExists tmpStore
        store <- openStore testLogger (StoreDir tmpStore)
        let outSP = StorePath "iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii" "nooutput"
            drv = mkTestBuildDrv shell outSP "echo 'forgot to create output'"
            config = defaultBuildConfig (stDir store)
        result <- buildDerivation config store drv testDrvPath
        closeStore store
        forceRemoveIfExists tmpStore
        pure $ case result of
          BuildFailure msg _ ->
            if T.isInfixOf "outputs missing" msg
              then Pass
              else Fail ("expected 'outputs missing' error, got: " <> msg)
          BuildSuccess _ -> Fail "expected failure when builder doesn't create $out",
      runTestM "missing sibling output cleans produced orphan" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-builder-partial-missing"
        forceRemoveIfExists tmpStore
        store <- openStore testLogger (StoreDir tmpStore)
        let outSP = StorePath "iiiiiiiiiiiiiiiiiiiiiiiiiiiiiii1" "partial-output"
            devSP = StorePath "iiiiiiiiiiiiiiiiiiiiiiiiiiiiiii2" "missing-output"
            drv =
              Derivation
                { drvOutputs =
                    [ DerivationOutput "out" outSP "" "",
                      DerivationOutput "dev" devSP "" ""
                    ],
                  drvInputDrvs = Map.empty,
                  drvInputSrcs = [],
                  drvPlatform = currentPlatform,
                  drvBuilder = TE.encodeUtf8 shell,
                  drvArgs = ["-c", "mkdir -p $out && echo orphan > $out/value"],
                  drvEnv = Map.union (Map.fromList [("name", "partial-missing")]) hostToolsEnv
                }
            config = defaultBuildConfig (stDir store)
            outPath = storePathToFilePath (stDir store) outSP
            devPath = storePathToFilePath (stDir store) devSP
        result <- buildDerivation config store drv testDrvPath
        outOnDisk <- Dir.doesPathExist outPath
        devOnDisk <- Dir.doesPathExist devPath
        outValid <- isValid store outSP
        devValid <- isValid store devSP
        closeStore store
        forceRemoveIfExists tmpStore
        pure $ case result of
          BuildFailure msg _
            | "outputs missing" `T.isInfixOf` msg && not outOnDisk && not devOnDisk && not outValid && not devValid -> Pass
            | otherwise -> Fail ("missing-output failure left store state behind: " <> msg)
          BuildSuccess _ -> Fail "partial-output builder unexpectedly succeeded",
      -- File output (not directory) should succeed
      runTestM "file output succeeds" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-builder-fileout"
        forceRemoveIfExists tmpStore
        store <- openStore testLogger (StoreDir tmpStore)
        let outSP = StorePath "jjjjjjjjjjjjjjjjjjjjjjjjjjjjjj" "fileout"
            drv = mkTestBuildDrv shell outSP "echo 'I am a file output' > $out"
            config = defaultBuildConfig (stDir store)
        result <- buildDerivation config store drv testDrvPath
        closeStore store
        forceRemoveIfExists tmpStore
        pure $ case result of
          BuildSuccess sp -> assertEqual "file output path" outSP sp
          BuildFailure msg code -> Fail ("file output build failed (" <> T.pack (show code) <> "): " <> msg),
      -- Cleanup after failure
      runTestM "cleanup after failure" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-builder8"
        forceRemoveIfExists tmpStore
        store <- openStore testLogger (StoreDir tmpStore)
        let outSP = StorePath "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh" "cleantest"
            drv = mkTestBuildDrv shell outSP "exit 1"
            config = defaultBuildConfig (stDir store)
        _ <- buildDerivation config store drv testDrvPath
        buildDirExists <- doesDirectoryExist (buildDirFor (stDir store) outSP)
        closeStore store
        forceRemoveIfExists tmpStore
        pure $ if not buildDirExists then Pass else Fail "build dir not cleaned up after failure",
      -- Fixed-output: registration re-checks the placed bytes
      runTestM "fixed-output flat output verifies and registers" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-fo1"
        forceRemoveIfExists tmpStore
        store <- openStore testLogger (StoreDir tmpStore)
        let payload = "fixed output payload\n"
            digest = maybe "" Hash.bytesToHexText (Hash.rawHashWithAlgo "sha256" payload)
            outSP = StorePath "ffffffffffffffffffffffffffffff01" "fo-flat-good"
            drv = mkFixedOutputDrv shell outSP "printf 'fixed output payload\\n' > $out" "sha256" digest
            config = defaultBuildConfig (stDir store)
        result <- buildDerivation config store drv testDrvPath
        registered <- isValid store outSP
        info <- queryPathInfo (stDB store) outSP
        closeStore store
        forceRemoveIfExists tmpStore
        -- The address the output was checked against, as upstream records
        -- a fixed output's.
        let address = Hash.rawHashWithAlgo "sha256" payload >>= Proto.contentAddressFromParts (Proto.FixedMethod FlatIngestion) "sha256"
        pure $ case result of
          BuildSuccess _
            | not registered -> Fail "built but not registered valid"
            | otherwise -> assertEqual "trust" (Just (Ultimate, address)) ((\recorded -> (piUltimate recorded, piContentAddress recorded)) <$> info)
          BuildFailure msg code -> Fail ("expected success (" <> T.pack (show code) <> "): " <> msg),
      -- The impureEnvVars carve-out: a fixed-output derivation's
      -- listed ambient variables reach the scrubbed build, an absent
      -- listed name arrives as the EMPTY STRING (present, not omitted,
      -- upstream's value_or("")), and everything else stays scrubbed.
      runTestM "impureEnvVars reach a fixed-output build, absent names as empty" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-impure1"
        forceRemoveIfExists tmpStore
        store <- openStore testLogger (StoreDir tmpStore)
        setEnv "NOVA_TEST_IMPURE" "proxy-ok"
        unsetEnv "NOVA_TEST_ABSENT"
        let payload = "fixed output payload\n"
            digest = maybe "" Hash.bytesToHexText (Hash.rawHashWithAlgo "sha256" payload)
            outSP = StorePath "ffffffffffffffffffffffffffffff03" "fo-impure-env"
            script = "[ \"$NOVA_TEST_IMPURE\" = \"proxy-ok\" ] && [ \"${NOVA_TEST_ABSENT+set}\" = \"set\" ] && [ -z \"$NOVA_TEST_ABSENT\" ] && printf 'fixed output payload\\n' > $out"
            baseDrv = mkFixedOutputDrv shell outSP script "sha256" digest
            drv = baseDrv {drvEnv = Map.insert "impureEnvVars" "NOVA_TEST_IMPURE NOVA_TEST_ABSENT" (drvEnv baseDrv)}
            config = defaultBuildConfig (stDir store)
        result <- buildDerivation config store drv testDrvPath
        unsetEnv "NOVA_TEST_IMPURE"
        closeStore store
        forceRemoveIfExists tmpStore
        pure $ case result of
          BuildSuccess _ -> Pass
          BuildFailure msg code -> Fail ("carve-out did not reach the build (" <> T.pack (show code) <> "): " <> msg),
      runTestM "impureEnvVars are ignored for a non-fixed-output build" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-impure2"
        forceRemoveIfExists tmpStore
        store <- openStore testLogger (StoreDir tmpStore)
        setEnv "NOVA_TEST_IMPURE" "must-not-leak"
        let outSP = StorePath "ffffffffffffffffffffffffffffff04" "impure-scrubbed"
            script = "[ -z \"${NOVA_TEST_IMPURE-}\" ] && echo ok > $out"
            baseDrv = mkTestBuildDrv shell outSP script
            drv = baseDrv {drvEnv = Map.insert "impureEnvVars" "NOVA_TEST_IMPURE" (drvEnv baseDrv)}
            config = defaultBuildConfig (stDir store)
        result <- buildDerivation config store drv testDrvPath
        unsetEnv "NOVA_TEST_IMPURE"
        closeStore store
        forceRemoveIfExists tmpStore
        pure $ case result of
          BuildSuccess _ -> Pass
          BuildFailure msg code -> Fail ("ambient leaked into a non-fixed-output build (" <> T.pack (show code) <> "): " <> msg),
      -- Exec takes each argument and NAME=VALUE entry as a C string, so
      -- upstream's builder gets each up to its first NUL.  Uncut, a NUL in
      -- an entry is one the process library refuses to spawn with, and on
      -- Windows one that starts another variable or ends the command line.
      runTestM "a NUL ends each argument and environment entry a builder gets, as exec ends them" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-builder-nul"
        forceRemoveIfExists tmpStore
        store <- openStore testLogger (StoreDir tmpStore)
        let outSP = StorePath "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaz" "nul-ends"
            script = "printf '%s|%s|%s|%s\\n' \"$0\" \"$1\" \"$smuggled\" \"${INJECTED-unset}\" > $out"
            base = mkTestBuildDrv shell outSP script
            drv =
              base
                { drvArgs = drvArgs base ++ ["zero\0dropped", "one\0dropped"],
                  drvEnv = Map.insert "smuggled" "kept\0INJECTED=1" (drvEnv base)
                }
        result <- buildDerivation (defaultBuildConfig (stDir store)) store drv testDrvPath
        seen <- case result of
          BuildSuccess _ -> BS.readFile (storePathToFilePath (stDir store) outSP)
          BuildFailure _ _ -> pure ""
        closeStore store
        forceRemoveIfExists tmpStore
        pure (assertEqual "build" (BuildSuccess outSP) result `andThen` assertEqual "what the builder was given" "zero|one|kept|unset\n" seen),
      -- Upstream refuses an output its group or anyone else may write
      -- before it reads or changes the output, since someone other than
      -- the build may have changed it; on Windows there is no such mode.
      runTestM "an output others could write is refused before it is made canonical, and none is registered" $
        if SI.os == "mingw32"
          then pure Pass
          else do
            tmpBase <- getTemporaryDirectory
            let tmpStore = tmpBase </> "nova-nix-test-builder-open-output"
            forceRemoveIfExists tmpStore
            store <- openStore testLogger (StoreDir tmpStore)
            let outSP = StorePath "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaay" "open-output"
                outPath = storePathToFilePath (stDir store) outSP
            result <- buildDerivation (defaultBuildConfig (stDir store)) store (mkTestBuildDrv shell outSP "mkdir $out && chmod 0777 $out") testDrvPath
            registered <- isValid store outSP
            left <- Dir.doesPathExist outPath
            closeStore store
            forceRemoveIfExists tmpStore
            pure $
              assertEqual "result" (BuildFailure ("suspicious ownership or permission on '" <> T.pack outPath <> "' for output 'out'; rejecting this build output") 1) result
                `andThen` assertEqual "registered" False registered
                `andThen` assertEqual "left in the store" False left,
      -- A descriptor the builder passes out names the inode it wrote,
      -- which upstream replaces with a fresh copy before it hashes and
      -- registers a fixed output.  A process of the test's holds one open
      -- across the build and writes through it once the output is
      -- registered.
      runTestM "a descriptor held on a fixed output's file while it is built cannot change what is registered" $
        if SI.os == "mingw32"
          then pure Pass
          else do
            tmpBase <- getTemporaryDirectory
            let tmpStore = tmpBase </> "nova-nix-test-builder-fixed-fd"
                release = tmpBase </> "nova-nix-test-builder-fixed-fd.release"
            forceRemoveIfExists tmpStore
            Dir.removePathForcibly release
            store <- openStore testLogger (StoreDir tmpStore)
            let payload = "payload"
                digest = maybe "" Hash.bytesToHexText (Hash.rawHashWithAlgo "sha256" payload)
                outSP = StorePath "ffffffffffffffffffffffffffffff0y" "fixed-fd"
                outPath = storePathToFilePath (stDir store) outSP
                script = "printf payload > $out; while [ ! -e \"$release\" ]; do /bin/sleep 0.1; done"
                base = mkFixedOutputDrv shell outSP script "sha256" digest
                drv = base {drvEnv = Map.insert "release" (TE.encodeUtf8 (T.pack release)) (drvEnv base)}
                holder = (Proc.proc "/bin/sh" ["-c", "exec 3<>\"$1\"; echo ready; read -r _; printf tampered >&3", "sh", outPath]) {Proc.std_in = Proc.CreatePipe, Proc.std_out = Proc.CreatePipe}
            (result, seen) <- withAsync (buildDerivation (defaultBuildConfig (stDir store)) store drv testDrvPath) $ \build -> do
              written <- waitUntil (Dir.doesPathExist outPath)
              Proc.withCreateProcess holder $ \holderIn holderOut _ holding -> case (written, holderIn, holderOut) of
                (True, Just toHolder, Just fromHolder) -> do
                  _ <- hGetLine fromHolder
                  BS.writeFile release ""
                  finished <- timeout processWaitMicros (wait build)
                  hPutStrLn toHolder "" >> hClose toHolder
                  _ <- Proc.waitForProcess holding
                  registered <- BS.readFile outPath
                  pure (finished, registered)
                _ -> pure (Nothing, "")
            closeStore store
            forceRemoveIfExists tmpStore
            Dir.removePathForcibly release
            pure (assertEqual "build" (Just (BuildSuccess outSP)) result `andThen` assertEqual "what is registered" payload seen),
      -- Upstream reads an output with lstat, so one that is a symlink is
      -- there whatever it names, nothing at all included.
      runTestM "an output that is a symlink naming nothing is an output, as upstream reads one" $
        if SI.os == "mingw32"
          then pure Pass
          else do
            tmpBase <- getTemporaryDirectory
            let tmpStore = tmpBase </> "nova-nix-test-builder-dangling"
            forceRemoveIfExists tmpStore
            store <- openStore testLogger (StoreDir tmpStore)
            let outSP = StorePath "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaax" "dangling"
            result <- buildDerivation (defaultBuildConfig (stDir store)) store (mkTestBuildDrv shell outSP "ln -s /nonexistent-nova-nix-target $out") testDrvPath
            target <- Dir.getSymbolicLinkTarget (storePathToFilePath (stDir store) outSP)
            closeStore store
            forceRemoveIfExists tmpStore
            pure (assertEqual "build" (BuildSuccess outSP) result `andThen` assertEqual "what it names" "/nonexistent-nova-nix-target" target),
      runTestM "a fixed output's copy an earlier run left half made is removed by the next build of it" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-builder-fresh-copy"
        forceRemoveIfExists tmpStore
        store <- openStore testLogger (StoreDir tmpStore)
        let digest = maybe "" Hash.bytesToHexText (Hash.rawHashWithAlgo "sha256" "payload")
            outSP = StorePath "ffffffffffffffffffffffffffffff0x" "fresh-copy"
            leftover = storePathToFilePath (stDir store) outSP <> ".tmp"
        createDirectoryIfMissing True leftover
        BS.writeFile (leftover </> "half") "made"
        result <- buildDerivation (defaultBuildConfig (stDir store)) store (mkFixedOutputDrv shell outSP "exit 1" "sha256" digest) testDrvPath
        left <- Dir.doesPathExist leftover
        closeStore store
        forceRemoveIfExists tmpStore
        pure (assertEqual "build" (BuildFailure "builder failed" 1) result `andThen` assertEqual "the copy left" False left),
      -- Upstream makes an output canonical before it reads it, so a file
      -- its builder left unreadable is read once it is 0444.
      runTestM "an output is made canonical before it is scanned, so a file its builder left unreadable is read" $
        if SI.os == "mingw32"
          then pure Pass
          else do
            tmpBase <- getTemporaryDirectory
            let tmpStore = tmpBase </> "nova-nix-test-builder-canonical-first"
            forceRemoveIfExists tmpStore
            store <- openStore testLogger (StoreDir tmpStore)
            let outSP = StorePath "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaw" "canonical-first"
                outPath = storePathToFilePath (stDir store) outSP
            result <- buildDerivation (defaultBuildConfig (stDir store)) store (mkTestBuildDrv shell outSP "mkdir $out && echo hidden > $out/file && chmod 0200 $out/file") testDrvPath
            mode <- fmap nsPermissions <$> nodeStatus (outPath </> "file")
            closeStore store
            forceRemoveIfExists tmpStore
            pure (assertEqual "build" (BuildSuccess outSP) result `andThen` assertEqual "the file's mode" (Just 0o444) mode),
      -- Upstream changes each node before it reads a directory, so an
      -- output directory its builder left unreadable is read, not refused.
      -- A divergence: upstream would make the directory 0444 and, run as a
      -- user other than root, fail to read beneath it.
      runTestM "an output directory its builder left readable but not searchable is read, and made 0555" $
        if SI.os == "mingw32"
          then pure Pass
          else do
            tmpBase <- getTemporaryDirectory
            let tmpStore = tmpBase </> "nova-nix-test-builder-unreadable-dir"
            forceRemoveIfExists tmpStore
            store <- openStore testLogger (StoreDir tmpStore)
            let outSP = StorePath "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaav" "unreadable-dir"
                outPath = storePathToFilePath (stDir store) outSP
            result <- buildDerivation (defaultBuildConfig (stDir store)) store (mkTestBuildDrv shell outSP "mkdir -p $out/d && touch $out/d/f && chmod 0644 $out/d") testDrvPath
            mode <- fmap nsPermissions <$> nodeStatus (outPath </> "d")
            closeStore store
            forceRemoveIfExists tmpStore
            pure (assertEqual "build" (BuildSuccess outSP) result `andThen` assertEqual "the directory's mode" (Just 0o555) mode),
      runTestM "a fresh copy that fails part way is removed, and what it was copying stays as it was" $
        if SI.os == "mingw32"
          then pure Pass
          else do
            tmpBase <- getTemporaryDirectory
            let tree = tmpBase </> "nova-nix-test-fresh-copy-fails"
            forceRemoveIfExists tree
            Dir.removePathForcibly (freshCopyPath tree)
            createDirectoryIfMissing True tree
            BS.writeFile (tree </> "a") "a"
            BS.writeFile (tree </> "b") "b"
            -- A file the copy cannot read fails it after it has begun.
            Proc.callProcess "chmod" ["0000", tree </> "b"]
            copied <- try (replaceWithCopy tree) :: IO (Either IOException ())
            leftCopy <- Dir.doesPathExist (freshCopyPath tree)
            Proc.callProcess "chmod" ["0644", tree </> "b"]
            kept <- BS.readFile (tree </> "b")
            forceRemoveIfExists tree
            pure $
              assertEqual "failed" True (isLeft copied)
                `andThen` assertEqual "the copy left" False leftCopy
                `andThen` assertEqual "the original" "b" kept,
      -- A fixed output's path comes from its content alone, which a
      -- reference would leave incomplete.
      runTestM "a fixed output that references a store path is refused, as upstream refuses it" $
        if SI.os == "mingw32"
          then pure Pass
          else do
            tmpBase <- getTemporaryDirectory
            let tmpStore = tmpBase </> "nova-nix-test-builder-fixed-references"
            forceRemoveIfExists tmpStore
            store <- openStore testLogger (StoreDir tmpStore)
            let outSP = StorePath "ffffffffffffffffffffffffffffff0w" "fixed-references"
                outPath = storePathToFilePath (stDir store) outSP
                digest = maybe "" Hash.bytesToHexText (Hash.rawHashWithAlgo "sha256" (TE.encodeUtf8 (T.pack outPath)))
            result <- buildDerivation (defaultBuildConfig (stDir store)) store (mkFixedOutputDrv shell outSP "printf '%s' \"$out\" > $out" "sha256" digest) testDrvPath
            registered <- isValid store outSP
            closeStore store
            forceRemoveIfExists tmpStore
            pure $
              assertEqual "result" (BuildFailure ("illegal path references in fixed-output derivation '" <> storePathToText defaultStoreDir testDrvPath <> "'") 1) result
                `andThen` assertEqual "registered" False registered,
      -- The original is made canonical before it is copied, as upstream
      -- makes it, so a file its builder left unreadable is copied.
      runTestM "a fixed recursive output holding a file its builder left unreadable is copied, hashed and registered" $
        if SI.os == "mingw32"
          then pure Pass
          else do
            tmpBase <- getTemporaryDirectory
            let tmpStore = tmpBase </> "nova-nix-test-builder-fixed-unreadable"
            forceRemoveIfExists tmpStore
            store <- openStore testLogger (StoreDir tmpStore)
            let outSP = StorePath "ffffffffffffffffffffffffffffff0v" "fixed-unreadable"
                tree = NAR.NarDirectory [("f", NAR.NarRegular False "x\n")]
                digest = maybe "" Hash.bytesToHexText (Hash.rawHashWithAlgo "sha256" (NAR.serialise tree))
            result <- buildDerivation (defaultBuildConfig (stDir store)) store (mkFixedOutputDrv shell outSP "mkdir $out && printf 'x\\n' > $out/f && chmod 0200 $out/f" "r:sha256" digest) testDrvPath
            closeStore store
            forceRemoveIfExists tmpStore
            pure (assertEqual "build" (BuildSuccess outSP) result),
      -- A file its owner may not remove stands in for a directory another
      -- user made, which no test can make; macOS lets an owner set one.
      runTestM "a build directory an earlier build left that cannot be removed is moved aside, and the build goes on" $
        if SI.os /= "darwin"
          then pure Pass
          else do
            tmpBase <- getTemporaryDirectory
            let storeRoot = tmpBase </> "nova-nix-test-builder-leftover"
                storeDir = StoreDir storeRoot
                firstOutput = StorePath (T.replicate 31 "a" <> "s") "leftover"
                dir = buildDirFor storeDir firstOutput
                aside = dir <> ".leftover.1"
            _ <- Proc.readProcessWithExitCode "chflags" ["-R", "nouchg", storeRoot] ""
            forceRemoveIfExists storeRoot
            createDirectoryIfMissing True (dir </> "stuck")
            BS.writeFile (dir </> "stuck" </> "f") "x"
            Proc.callProcess "chflags" ["uchg", dir </> "stuck" </> "f"]
            -- The flag comes off whatever happens, so the store can go.
            ( do
                ran <- withBuildDir testLogger storeDir firstOutput WorksInBuildDir RemoveFailed (const BuildSucceeded) Dir.doesDirectoryExist
                movedAside <- Dir.doesPathExist (aside </> "stuck" </> "f")
                pure (assertEqual "the build" True ran `andThen` assertEqual "moved aside" True movedAside)
              )
              `finally` (Proc.readProcessWithExitCode "chflags" ["-R", "nouchg", storeRoot] "" *> forceRemoveIfExists storeRoot),
      -- The rewrite of a store hash in a file opens it without following
      -- a link, so one put where the file was is refused, not followed.
      runTestM "a file's rewrite in place does not follow a link put where the file was" $
        if SI.os == "mingw32"
          then pure Pass
          else do
            tmpBase <- getTemporaryDirectory
            let dir = tmpBase </> "nova-nix-test-rewrite-link"
                outside = tmpBase </> "nova-nix-test-rewrite-link-outside"
            forceRemoveIfExists dir
            Dir.removePathForcibly outside
            createDirectoryIfMissing True dir
            BS.writeFile outside "the user's"
            Dir.createFileLink outside (dir </> "file")
            rewritten <- try (rewriteRegularFile (const "changed") (dir </> "file")) :: IO (Either IOException ())
            kept <- BS.readFile outside
            forceRemoveIfExists dir
            Dir.removePathForcibly outside
            pure (assertEqual "refused" True (isLeft rewritten) `andThen` assertEqual "what the link names" "the user's" kept),
      -- The carve-out is for upstream's fixed-output type alone: one
      -- output, out, with a hash that fixes it.  Each builder only logs
      -- what it saw, so every build fails for want of its outputs, which
      -- is not what is tested.
      runTestM "impureEnvVars reach a fixed-output build and no other that names a hash algorithm" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-impure3"
            logPath = tmpBase </> "nova-nix-test-impure3.log"
            pin = T.replicate 64 "0"
            pathOf index = StorePath ("ffffffffffffffffffffffffffffff1" <> T.pack (show (index :: Int))) "impure-gate"
            output name = DerivationOutput name . pathOf
            kinds =
              [ ("a fixed-output build", [output "out" 0 "sha256" pin], "seen: passed\n"),
                ("a floating content-addressed build", [output "out" 1 "r:sha256" ""], "seen: unset\n"),
                ("an impure build", [output "out" 2 "r:sha256" "impure"], "seen: unset\n"),
                ("a fixed output beside another", [output "out" 3 "sha256" pin, output "dev" 4 "" ""], "seen: unset\n"),
                ("a fixed output not named out", [output "data" 5 "sha256" pin], "seen: unset\n")
              ]
            seenBy store outputs = do
              let baseDrv = mkTestBuildDrv shell (pathOf 0) "echo \"seen: ${NOVA_TEST_IMPURE-unset}\""
                  drv = baseDrv {drvOutputs = outputs, drvEnv = Map.insert "impureEnvVars" "NOVA_TEST_IMPURE" (drvEnv baseDrv)}
              _ <-
                withBinaryFile logPath WriteMode $ \logHandle ->
                  buildDerivation (defaultBuildConfig (stDir store)) (buildLogTo logHandle store) drv testDrvPath
              BS.readFile logPath
        forceRemoveIfExists tmpStore
        results <-
          bracket_ (setEnv "NOVA_TEST_IMPURE" "passed") (unsetEnv "NOVA_TEST_IMPURE" *> forceRemoveIfExists tmpStore *> Dir.removePathForcibly logPath) $
            bracket (openStore testLogger (StoreDir tmpStore)) closeStore $ \store ->
              traverse (\(label, outputs, expected) -> assertEqual ("what " <> label <> " saw") expected <$> seenBy store outputs) kinds
        pure (foldr andThen Pass results),
      runTestM "fixed-output flat mismatch fails and removes the output" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-fo2"
        forceRemoveIfExists tmpStore
        store <- openStore testLogger (StoreDir tmpStore)
        let digest = maybe "" Hash.bytesToHexText (Hash.rawHashWithAlgo "sha256" "expected content\n")
            outSP = StorePath "ffffffffffffffffffffffffffffff02" "fo-flat-bad"
            drv = mkFixedOutputDrv shell outSP "printf 'tampered content\\n' > $out" "sha256" digest
            config = defaultBuildConfig (stDir store)
        result <- buildDerivation config store drv testDrvPath
        registered <- isValid store outSP
        onDisk <- Dir.doesPathExist (storePathToFilePath (stDir store) outSP)
        closeStore store
        forceRemoveIfExists tmpStore
        pure $ case result of
          BuildFailure msg _
            | "hash mismatch" `T.isInfixOf` msg && not registered && not onDisk -> Pass
            | otherwise ->
                Fail
                  ( "wrong failure shape (valid="
                      <> T.pack (show registered)
                      <> ", onDisk="
                      <> T.pack (show onDisk)
                      <> "): "
                      <> msg
                  )
          BuildSuccess _ -> Fail "mismatched fixed output registered",
      runTestM "fixed-output recursive tree verifies against its NAR hash" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-fo3"
        forceRemoveIfExists tmpStore
        store <- openStore testLogger (StoreDir tmpStore)
        let tree = NAR.NarDirectory [("data.txt", NAR.NarRegular False "inner bytes\n")]
            digest = maybe "" Hash.bytesToHexText (Hash.rawHashWithAlgo "sha256" (NAR.serialise tree))
            outSP = StorePath "ffffffffffffffffffffffffffffff03" "fo-rec-good"
            drv = mkFixedOutputDrv shell outSP "mkdir -p $out && printf 'inner bytes\\n' > $out/data.txt" "r:sha256" digest
            config = defaultBuildConfig (stDir store)
        result <- buildDerivation config store drv testDrvPath
        registered <- isValid store outSP
        closeStore store
        forceRemoveIfExists tmpStore
        pure $ case result of
          BuildSuccess _
            | registered -> Pass
            | otherwise -> Fail "built but not registered valid"
          BuildFailure msg code -> Fail ("expected success (" <> T.pack (show code) <> "): " <> msg),
      runTestM "fixed-output recursive mismatch fails the build" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-fo4"
        forceRemoveIfExists tmpStore
        store <- openStore testLogger (StoreDir tmpStore)
        let tree = NAR.NarDirectory [("data.txt", NAR.NarRegular False "declared bytes\n")]
            digest = maybe "" Hash.bytesToHexText (Hash.rawHashWithAlgo "sha256" (NAR.serialise tree))
            outSP = StorePath "ffffffffffffffffffffffffffffff04" "fo-rec-bad"
            drv = mkFixedOutputDrv shell outSP "mkdir -p $out && printf 'other bytes\\n' > $out/data.txt" "r:sha256" digest
            config = defaultBuildConfig (stDir store)
        result <- buildDerivation config store drv testDrvPath
        registered <- isValid store outSP
        closeStore store
        forceRemoveIfExists tmpStore
        pure $ case result of
          BuildFailure msg _
            | "hash mismatch" `T.isInfixOf` msg && not registered -> Pass
            | otherwise -> Fail ("wrong failure shape: " <> msg)
          BuildSuccess _ -> Fail "mismatched fixed output registered",
      runTestM "fixed-output flat mode rejects a directory output" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-fo5"
        forceRemoveIfExists tmpStore
        store <- openStore testLogger (StoreDir tmpStore)
        let digest = maybe "" Hash.bytesToHexText (Hash.rawHashWithAlgo "sha256" "whatever\n")
            outSP = StorePath "ffffffffffffffffffffffffffffff05" "fo-flat-dir"
            drv = mkFixedOutputDrv shell outSP "mkdir -p $out && printf 'x' > $out/f" "sha256" digest
            config = defaultBuildConfig (stDir store)
        result <- buildDerivation config store drv testDrvPath
        closeStore store
        forceRemoveIfExists tmpStore
        pure $ case result of
          BuildFailure msg _
            | flatOutputRefusal `T.isInfixOf` msg -> Pass
            | otherwise -> Fail ("wrong failure: " <> msg)
          BuildSuccess _ -> Fail "directory output passed a flat fixed-output check"
    ]

-- | The tail of upstream's refusal of an output under a flat hash that is
-- not a regular file its owner may not execute (@registerOutputs@ in
-- local-derivation-goal.cc at 2.24.9, derivation-builder-impl.cc at
-- 2.36.0); the message opens with the output's path.
flatOutputRefusal :: Text
flatOutputRefusal = "should be a non-executable regular file since recursive hashing is not enabled (one of outputHashMode={flat,text} is true)"

-- | A builder's output under a flat hash is held to upstream's form, read
-- without following a symlink: each output here carries the bytes the
-- hash names, so only its form refuses it.  POSIX only: the Windows
-- shell's @chmod@ sets no exec stream, and its @ln -s@ copies.
testFlatOutputFormIO :: IO [Bool]
testFlatOutputFormIO = do
  putStrLn "builder/flat-output-form"
  if SI.os == "mingw32"
    then [] <$ putStrLn "  SKIP  these outputs are made with a POSIX shell's chmod and ln -s"
    else do
      shell <- findTestShell
      sequence
        [ runTestM "a builder's executable output under a flat hash is refused with upstream's error" $
            refused shell "fo-flat-exec" "printf hello > $out && chmod +x $out",
          runTestM "a builder's symlink output under a flat hash is refused, not hashed as the file it names" $
            refused shell "fo-flat-link" "printf hello > $out.target && ln -s $out.target $out"
        ]
  where
    refused shell name script = do
      tmpBase <- getTemporaryDirectory
      let tmpStore = tmpBase </> ("nova-nix-test-" ++ T.unpack name)
          digest = sha256Digest "hello"
      case makeFixedOutputPath name "sha256" FlatIngestion digest of
        Left err -> pure (Fail (T.pack (show err)))
        Right outSP ->
          bracket_ (forceRemoveIfExists tmpStore) (forceRemoveIfExists tmpStore) $
            bracket (openStore testLogger (StoreDir tmpStore)) closeStore $ \store -> do
              let drv = mkFixedOutputDrv shell outSP script "sha256" (Hash.bytesToHexText digest)
              result <- buildDerivation (defaultBuildConfig (stDir store)) store drv testDrvPath
              registered <- isValid store outSP
              pure $ case result of
                BuildFailure msg _ | flatOutputRefusal `T.isInfixOf` msg && not registered -> Pass
                other -> Fail ("expected upstream's refusal, got: " <> T.pack (show other))
