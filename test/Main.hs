{-# LANGUAGE LambdaCase #-}

-- | The nova-nix test suite.  One executable runs every group, prints each
-- failing case, and exits non-zero unless every case passes.
module Main (main) where

import Control.Exception (finally)
import qualified Data.ByteString as BS
import qualified Nix.Cli.Reference as Ref
import Nix.Store (useStoreFileMask)
import System.Directory (getTemporaryDirectory)
import qualified System.Directory as Dir
import System.Environment (getArgs, setEnv)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.IO (BufferMode (..), hSetBuffering, stdout)
import qualified System.Process as Proc
import Test.Builder (sandboxBuildChild, sandboxBuildChildFlag, sandboxEscapeChild, sandboxEscapeChildFlag, sandboxFallbackChild, sandboxFallbackChildFlag, sandboxForkerChild, sandboxForkerChildFlag, sandboxThawChild, sandboxThawChildFlag, testBuildCheckIO, testBuildCores, testBuildCoresIO, testBuildDirIO, testBuildLogIO, testBuildOrchestrator, testBuildenvBuildIO, testBuildenvDerivations, testBuilder, testBuilderLayout, testDarwinSandboxIO, testDependentBuildIO, testExecWrapper, testFetchMirrors, testFetchurlUnpack, testFlatOutputFormIO, testLinuxSandboxIO, testOutputCycleBuildIO, testOutputOrderIO, testPassAsFile, testProfileGenerations, testProfileManifest, testProfileTree, testProfileUpdateIO, testReferenceClosureIO, testSandboxFallbackIO, testSandboxPure, testSandboxSpawnIO, testScrubbedBuildEnvIO, testSourceDateEpochIO, testSubstituteFromCaches, testTrivialBuildIO, testUnpackBuildIO, testWantedOutputsIO)
import Test.CData (arenaGuardChild, arenaGuardChildFlag, testArenaGuard, testBytecodeCompile, testBytecodeCorrupt, testBytecodeCountSpill, testBytecodePerElement, testBytecodeTags, testCAttrSet, testCThunk, testLambdaShape, testSymbol, testUnknownTags, testValueCountWidths)
import Test.Cli (cliJsonPath, testBuildCoresCLI, testBuildUmaskCLI, testCheckCLI, testCliReference, testClosedStdoutCLI, testConfigBytesCLI, testDevelopCLI, testE2E, testEvalPolicyCLI, testExecWrapperCLI, testImportFromDerivationCLI, testNixConfig, testOutLinkCLI, testProfileCLI, testResultPrinterCLI, testSandboxSessionsCLI, testTargetArgsCLI, testTrustCLI, testUncaughtIOFailureCLI, writeCliJsonFlag)
import Test.Daemon (testDaemonAuth, testDaemonFixtures, testDaemonProtocol, testDaemonServer, testDaemonSocketIO, testDaemonWire)
import Test.Derivation (testBatchG, testDerivation, testDerivationWrapper, testDevelopCapture, testDevelopEnvironment, testDevelopShell, testFromATerm, testInputAddressed, testStructuredAttrs)
import Test.Eval (arenaCycleChild, arenaCycleChildFlag, failureChild, failureChildFlag, internOrderChild, internOrderChildFlag, namesOnlyChild, namesOnlyChildFlag, startupSymbolsChild, startupSymbolsChildFlag, testAbsolutePathStrings, testAllocSites, testAttrNameContext, testAttrPath, testBatch1, testBatch2, testBatch3, testBatch4, testBatch5, testBatch6, testBatch7, testBatchA, testBatchAIO, testBatchB, testBatchC, testBatchCIO, testBatchD, testBatchE, testBatchEIO, testBatchF, testBatchH, testBlackhole, testBlackholeRecoveryIO, testBuilderInterningIO, testBuiltinArgumentOrder, testByteStringSemantics, testByteStringSemanticsIO, testCallDepth, testCallErrorsIO, testCensus, testClassIFollowups, testClassIFollowupsIO, testCoercionErrors, testContextHelpers, testContextPropagation, testDeepContextClosureIO, testDeepForceOrderIO, testDerivationFields, testDerivationPathFieldsIO, testDerivationTraceIO, testDrivelessPathsIO, testDrvContext, testDynamicAttrInterningIO, testEmptyHashIO, testEnvLayout, testEnvironmentBytes, testErrorContext, testErrorContextIO, testEvalArithmetic, testEvalAttrs, testEvalBuiltins, testEvalComparison, testEvalErrors, testEvalFidelity, testEvalHigherOrder, testEvalIfAssert, testEvalLambda, testEvalLet, testEvalLists, testEvalLiterals, testEvalLogic, testEvalPolicyIO, testEvalPolicyRules, testEvalRecAttrs, testEvalStrings, testEvalVariables, testEvalWith, testFetchCache, testFetchGitPins, testFetchGitShallow, testFetchGitTransport, testFirstArgumentOrderIO, testFixedOutputHash, testFrameKnots, testFromTOML, testImportIO, testImportPure, testInStoreCopyIO, testInterpolatedSymlinkIO, testListCoercion, testListToAttrsTraceIO, testOutPathCoercion, testOutputHashMode, testParseOrderInterningIO, testPathArguments, testPathArgumentsIO, testPathFilterIO, testPathInterp, testPathMethodIO, testPathSymlinkIO, testPhase4, testPhase4IO, testPlainStringContext, testPlainStringContextIO, testReadDirUntypedEntriesIO, testRealiseContext, testRealiseContextIO, testRegionCensus, testRegionCollector, testRepeatedValues, testResultPrinter, testScratchDirs, testSelectInterningIO, testSharedEmptySet, testSharedVariableCellsIO, testSourceErrorMessage, testSourceReadErrorsIO, testStartupSymbolsIO, testStorePathLinksIO, testStringContext, testStringSharing, testStringsOffSymbolsIO, testTargetArgs, testToJSONPathIO, testTraceVerboseOff, testUnsafeGetAttrPos, testUpdateChains, testUpstreamConformance, testValuePrinter, testWalkPathRules)
import Test.Exceptions (testInterruptsPropagate)
import Test.Harness (evalChild, evalChildFlag, inTestSession)
import Test.Parser (testCurPos, testExprTypes, testLexer, testLineEndings, testParserErrors, testParserExprs, testParserIntegration, testSourceBytes, testSourceBytesIO, testStaticGlobalsSync, testStringAttrKeys, testUndefinedVariables, testUndefinedVariablesIO)
import Test.Pkgs (testBinutilsNativeX8664Windows, testBinutilsWindows, testBinutilsX8664Windows, testBusyboxWindows, testChecksForbidSubstitutes, testGcc10Windows, testGcc46CxxWindows, testGcc46Windows, testGccCoreWindows, testGccCoreX8664Windows, testGccLatestWindows, testGccNativeX8664Windows, testGccX8664Windows, testGnumakeWindows, testMesWindows, testMingwW64CrtX8664Windows, testMingwW64LatestWindows, testPkgsFetchurl, testSeedStdenvWindows, testStage0Pe32Chain, testStdenvWindows, testTinyccWindows, testWindowsRungs, testWindowsSources, testWorkflowSpellings)
import Test.Push (testPushClosureIO, testPushDirectory, testPushMultipart, testPushPure)
import Test.Realise (testDepGraph, testRealise)
import Test.Store (testCaseSensitiveVolumeIO, testExecBit, testFoldingVolumeIO, testGcPure, testHashHelpers, testLinkOrdering, testNarKnownAnswer, testNarNameSafety, testParseStorePath, testPathInfo, testPathLocks, testSerialiseIfReadableIO, testStoreAccessIO, testStoreDB, testStoreDelete, testStoreGC, testStoreGCUpstreamManaged, testStoreMetadata, testStoreNameSinks, testStoreNameSinksIO, testStoreOps, testStoreOpsIO, testStorePaths, testStoreWritesUpstreamManaged, testSymlinkWalksIO)
import Test.Substituter (testNarInfoValidation, testStatedContentAddress, testSubstituter, testSubstituterRetry, testVerifySigs)
import Test.Trust (testTrust, testTrustScanIO, testTrustWindowsGraphs)
import Test.WindowsSandbox (testWindowsSandboxIO, windowsSandboxAccounts, windowsSandboxAccountsFlag, windowsSandboxBuilder, windowsSandboxBuilderFlag)

main :: IO ()
main = do
  -- The builds here run with upstream's mask, as the CLI's do, whatever
  -- the one the suite was started with.
  useStoreFileMask
  getArgs >>= \case
    [flag, name] | flag == arenaGuardChildFlag -> arenaGuardChild name
    [flag, source] | flag == evalChildFlag -> inTestSession (evalChild source)
    [flag, source] | flag == internOrderChildFlag -> inTestSession (internOrderChild source)
    [flag] | flag == startupSymbolsChildFlag -> inTestSession startupSymbolsChild
    [flag, source] | flag == failureChildFlag -> inTestSession (failureChild source)
    [flag, helper, busybox, storeRoot, marker] | flag == sandboxBuildChildFlag -> sandboxBuildChild helper busybox storeRoot marker
    [flag, storeRoot] | flag == sandboxFallbackChildFlag -> sandboxFallbackChild storeRoot
    [flag, out, ready] | flag == sandboxEscapeChildFlag -> sandboxEscapeChild out ready
    [flag, marker, ready] | flag == sandboxForkerChildFlag -> sandboxForkerChild marker ready
    [flag, marker, ready] | flag == sandboxThawChildFlag -> sandboxThawChild marker ready
    [flag, source] | flag == namesOnlyChildFlag -> inTestSession (namesOnlyChild source)
    [flag] | flag == arenaCycleChildFlag -> arenaCycleChild
    [flag] | flag == writeCliJsonFlag -> BS.writeFile cliJsonPath Ref.referenceJson
    flag : rest | flag == windowsSandboxAccountsFlag -> windowsSandboxAccounts rest
    flag : rest | flag == windowsSandboxBuilderFlag -> windowsSandboxBuilder rest
    _ -> withRunTempRoot (inTestSession runSuite)

-- | Give the run a temporary root of its own and point the process's
-- temporary directory at it, so every fixture the suite derives from
-- 'getTemporaryDirectory' (each under a fixed name) is this run's alone
-- and two suites running at once cannot delete each other's.
-- @TMPDIR@ is what POSIX reads, @TMP@ and @TEMP@ what Windows'
-- @GetTempPath@ reads; a child process inherits them.  The root is named
-- for this process, so no live run shares it, and one a crashed run left
-- under the same PID is cleared first.
withRunTempRoot :: IO a -> IO a
withRunTempRoot action = do
  base <- getTemporaryDirectory
  pid <- Proc.getCurrentPid
  let root = base </> ("nova-nix-test-" <> show pid)
  Dir.removePathForcibly root
  Dir.createDirectory root
  mapM_ (`setEnv` root) ["TMPDIR", "TMP", "TEMP"]
  action `finally` Dir.removePathForcibly root

-- | Every group, under the one arena bracket.
runSuite :: IO ()
runSuite = do
  hSetBuffering stdout LineBuffering
  putStrLn "nova-nix test suite"
  putStrLn "==================="
  results <-
    concat
      <$> sequence
        [ testExprTypes,
          testStorePaths,
          testDerivation,
          testTrivialBuildIO,
          testBuildDirIO,
          testStoreAccessIO,
          testBuildLogIO,
          testSourceDateEpochIO,
          testBuildCoresIO,
          testScrubbedBuildEnvIO,
          testUnpackBuildIO,
          testBuildenvDerivations,
          testProfileTree,
          testBuildenvBuildIO,
          testProfileManifest,
          testProfileGenerations,
          testProfileUpdateIO,
          testDependentBuildIO,
          testBuildCheckIO,
          testStoreOpsIO,
          testReferenceClosureIO,
          testOutputOrderIO,
          testOutputCycleBuildIO,
          testEvalFidelity,
          testUpstreamConformance,
          testHashHelpers,
          testFetchMirrors,
          testFetchurlUnpack,
          testSubstituterRetry,
          testSubstituteFromCaches,
          testWantedOutputsIO,
          testPkgsFetchurl,
          testStage0Pe32Chain,
          testTrustWindowsGraphs,
          testMesWindows,
          testTinyccWindows,
          testGnumakeWindows,
          testBusyboxWindows,
          testSeedStdenvWindows,
          testBinutilsWindows,
          testGccCoreWindows,
          testGcc46Windows,
          testGcc46CxxWindows,
          testGcc10Windows,
          testMingwW64LatestWindows,
          testGccLatestWindows,
          testBinutilsX8664Windows,
          testGccCoreX8664Windows,
          testMingwW64CrtX8664Windows,
          testGccX8664Windows,
          testBinutilsNativeX8664Windows,
          testGccNativeX8664Windows,
          testStdenvWindows,
          testChecksForbidSubstitutes,
          testWindowsSources,
          testWindowsRungs,
          testWorkflowSpellings,
          testNarKnownAnswer,
          testFetchGitTransport,
          testFetchGitShallow,
          testFetchGitPins,
          testPathInterp,
          testOutPathCoercion,
          testFetchCache,
          testExecWrapper,
          testAttrPath,
          testScratchDirs,
          testEvalLiterals,
          testEvalVariables,
          testEvalArithmetic,
          testEvalComparison,
          testEvalLogic,
          testEvalStrings,
          testEvalIfAssert,
          testEvalLet,
          testEvalAttrs,
          testSharedEmptySet,
          testValuePrinter,
          testCoercionErrors,
          testResultPrinter,
          testRepeatedValues,
          testEvalRecAttrs,
          testFrameKnots,
          testCensus,
          testRegionCensus,
          testAllocSites,
          testUpdateChains,
          testRegionCollector,
          testEnvLayout,
          testEvalLists,
          testEvalLambda,
          testEvalWith,
          testEvalBuiltins,
          testFromTOML,
          testEvalErrors,
          testListCoercion,
          testEvalHigherOrder,
          testBuiltinArgumentOrder,
          testLexer,
          testLineEndings,
          testParserExprs,
          testParserErrors,
          testSourceBytes,
          testParserIntegration,
          testStringAttrKeys,
          testStaticGlobalsSync,
          testUndefinedVariables,
          testUndefinedVariablesIO,
          testBatch1,
          testBatch2,
          testBatch3,
          testBatch4,
          testBatch5,
          testBatch6,
          testBatch7,
          testImportPure,
          testImportIO,
          testUnsafeGetAttrPos,
          testCurPos,
          testAttrNameContext,
          testPlainStringContext,
          testPlainStringContextIO,
          testBlackholeRecoveryIO,
          testDerivationTraceIO,
          testListToAttrsTraceIO,
          testErrorContext,
          testErrorContextIO,
          testFirstArgumentOrderIO,
          testParseOrderInterningIO,
          testStartupSymbolsIO,
          testCallErrorsIO,
          testStringsOffSymbolsIO,
          testBuilderInterningIO,
          testSelectInterningIO,
          testDynamicAttrInterningIO,
          testDeepForceOrderIO,
          testSharedVariableCellsIO,
          testStringSharing,
          testTraceVerboseOff,
          testStorePathLinksIO,
          testDeepContextClosureIO,
          testDerivationFields,
          testDerivationPathFieldsIO,
          testFixedOutputHash,
          testOutputHashMode,
          testEmptyHashIO,
          testPathArguments,
          testPathArgumentsIO,
          testPathFilterIO,
          testPathSymlinkIO,
          testPathMethodIO,
          testInterpolatedSymlinkIO,
          testSourceErrorMessage,
          testSourceReadErrorsIO,
          testReadDirUntypedEntriesIO,
          testSerialiseIfReadableIO,
          testBatchA,
          testBatchAIO,
          testEnvironmentBytes,
          testBatchB,
          testBatchC,
          testBatchCIO,
          testBlackhole,
          testCallDepth,
          testBatchD,
          testBatchE,
          testBatchEIO,
          testBatchF,
          testBatchG,
          testBatchH,
          testDerivationWrapper,
          testStringContext,
          testContextHelpers,
          testContextPropagation,
          testDrvContext,
          testStructuredAttrs,
          testDepGraph,
          testTrust,
          testTrustScanIO,
          testRealise,
          testRealiseContext,
          testRealiseContextIO,
          testSubstituter,
          testStatedContentAddress,
          testCaseSensitiveVolumeIO,
          testFoldingVolumeIO,
          testPathLocks,
          testExecBit,
          testVerifySigs,
          testNarInfoValidation,
          testNixConfig,
          testBuildCores,
          testPushPure,
          testPushClosureIO,
          testPushMultipart,
          testPushDirectory,
          testBuildOrchestrator,
          testPassAsFile,
          testBuilderLayout,
          testSandboxPure,
          testSandboxSpawnIO,
          testSandboxFallbackIO,
          testLinuxSandboxIO,
          testDarwinSandboxIO,
          testWindowsSandboxIO,
          testStoreDB,
          testPathInfo,
          testParseStorePath,
          testDaemonWire,
          testDaemonProtocol,
          testDaemonFixtures,
          testDaemonServer,
          testDaemonAuth,
          testDaemonSocketIO,
          testStoreMetadata,
          testStoreOps,
          testStoreDelete,
          testGcPure,
          testStoreGC,
          testStoreGCUpstreamManaged,
          testStoreWritesUpstreamManaged,
          testSymlinkWalksIO,
          testLinkOrdering,
          testFromATerm,
          testInputAddressed,
          testDevelopCapture,
          testDevelopEnvironment,
          testDevelopShell,
          testBuilder,
          testFlatOutputFormIO,
          testE2E,
          testExecWrapperCLI,
          testCheckCLI,
          testDevelopCLI,
          testImportFromDerivationCLI,
          testBuildCoresCLI,
          testBuildUmaskCLI,
          testSandboxSessionsCLI,
          testOutLinkCLI,
          testProfileCLI,
          testTargetArgs,
          testTargetArgsCLI,
          testUncaughtIOFailureCLI,
          testClosedStdoutCLI,
          testCliReference,
          testTrustCLI,
          testPhase4,
          testPhase4IO,
          testToJSONPathIO,
          testByteStringSemantics,
          testByteStringSemanticsIO,
          testSourceBytesIO,
          testSymbol,
          testCAttrSet,
          testCThunk,
          testArenaGuard,
          testBytecodeCompile,
          testBytecodeTags,
          testLambdaShape,
          testUnknownTags,
          testBytecodeCorrupt,
          testBytecodeCountSpill,
          testBytecodePerElement,
          testValueCountWidths,
          testStoreNameSinks,
          testNarNameSafety,
          testStoreNameSinksIO,
          testClassIFollowups,
          testClassIFollowupsIO,
          testInStoreCopyIO,
          testWalkPathRules,
          testAbsolutePathStrings,
          testEvalPolicyRules,
          testEvalPolicyIO,
          testDrivelessPathsIO,
          testEvalPolicyCLI,
          testResultPrinterCLI,
          testConfigBytesCLI,
          testInterruptsPropagate
        ]
  let total = length results
      passed = length (filter id results)
      failed = total - passed
  putStrLn $ "\n" ++ show passed ++ "/" ++ show total ++ " passed"
  if failed > 0
    then do
      putStrLn $ show failed ++ " FAILED"
      exitFailure
    else do
      putStrLn "All tests passed."
      exitSuccess
