{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Tests of the evaluator: values and operators, builtins and their
-- errors, evaluation policies, search paths and attribute paths.
module Test.Eval
  ( testEvalLiterals,
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
    testCensus,
    testRegionCollector,
    testRegionCensus,
    testUpdateChains,
    testAllocSites,
    testEnvLayout,
    testFrameKnots,
    testEvalLists,
    testEvalLambda,
    testEvalWith,
    testEvalBuiltins,
    testListCoercion,
    testEvalErrors,
    testEvalHigherOrder,
    testBuiltinArgumentOrder,
    testBatch1,
    testBatch2,
    testBatch3,
    testBatch4,
    testBatch5,
    testBatch6,
    testBatch7,
    testImportPure,
    testDerivationFields,
    testDerivationPathFieldsIO,
    testFixedOutputHash,
    testOutputHashMode,
    testPathArguments,
    testPathArgumentsIO,
    testPathMethodIO,
    testDerivationTraceIO,
    testErrorContext,
    testErrorContextIO,
    testListToAttrsTraceIO,
    testBlackholeRecoveryIO,
    testPathFilterIO,
    testInterpolatedSymlinkIO,
    testSourceErrorMessage,
    testSourceReadErrorsIO,
    testReadDirUntypedEntriesIO,
    testPathSymlinkIO,
    testImportIO,
    testAttrNameContext,
    testPlainStringContext,
    testPlainStringContextIO,
    testUnsafeGetAttrPos,
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
    testBatchH,
    testStringContext,
    testContextPropagation,
    testContextHelpers,
    testDrvContext,
    testUpstreamConformance,
    testEvalFidelity,
    testFromTOML,
    testAttrPath,
    testFetchCache,
    testFetchGitTransport,
    testFetchGitShallow,
    testFetchGitPins,
    testPathInterp,
    testOutPathCoercion,
    testScratchDirs,
    testRealiseContext,
    testRealiseContextIO,
    testTargetArgs,
    testPhase4,
    testPhase4IO,
    testToJSONPathIO,
    testStorePathLinksIO,
    testDeepContextClosureIO,
    internOrderChildFlag,
    internOrderChild,
    namesOnlyChildFlag,
    arenaCycleChildFlag,
    arenaCycleChild,
    namesOnlyChild,
    testStringsOffSymbolsIO,
    testBuilderInterningIO,
    testSelectInterningIO,
    testDynamicAttrInterningIO,
    testDeepForceOrderIO,
    testSharedVariableCellsIO,
    testStringSharing,
    testParseOrderInterningIO,
    startupSymbolsChildFlag,
    startupSymbolsChild,
    failureChildFlag,
    failureChild,
    testStartupSymbolsIO,
    testCallErrorsIO,
    testFirstArgumentOrderIO,
    testEmptyHashIO,
    testTraceVerboseOff,
    testClassIFollowups,
    testClassIFollowupsIO,
    testInStoreCopyIO,
    testWalkPathRules,
    testByteStringSemantics,
    testByteStringSemanticsIO,
    testAbsolutePathStrings,
    testEvalPolicyRules,
    testEvalPolicyIO,
    testDrivelessPathsIO,
  )
where

import Control.Exception (IOException, bracket, bracket_, displayException, evaluate, finally, try)
import Control.Monad (filterM, foldM, forM_, replicateM, replicateM_, void, when, zipWithM)
import Data.Bits (shiftL, shiftR, (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.Char (ord)
import Data.Foldable (toList, traverse_)
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity (..))
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.List (genericLength, inits, isPrefixOf, nub, sortOn, transpose, unfoldr)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybeToList)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Time.Clock as Clock
import Data.Void (absurd)
import Data.Word (Word32, Word64, Word8)
import qualified Database.SQLite.Simple as SQL
import Foreign.C.Error (Errno (..), eACCES, eISDIR, eNOENT, eNOTDIR)
import Foreign.Ptr (Ptr, WordPtr, castPtr, nullPtr, ptrToWordPtr)
import Foreign.Storable (peekElemOff)
import GHC.IO.Exception (IOErrorType (..), IOException (..))
import Nix.Builder (BuildResult (..), BuildStop (..))
import Nix.Builtins (builtinEnv, isNixPathPseudoUrl, parseNixPath, rootScopeNames, splitNixPath)
import qualified Nix.Cli.Args as Cli
import Nix.Config (NixConfig (..))
import qualified Nix.Config as Config
import Nix.Derivation (Derivation (..), DerivationOutput (..))
import Nix.Eval (BuiltinArg (..), EvalPolicy (..), FetchCache (..), FetchGitArgs (..), MonadEval (..), NixValue (..), StringContext (..), StringContextElement (..), Thunk (..), attrSetFromMap, attrSetLookup, attrSetNull, attrSetSize, checkGitRef, checkGitRev, checkGitUrl, decodeFetchCache, deepForce, emptyContext, emptyEnv, encodeFetchCache, eval, evalParsed, evaluated, fetchCacheKey, force, gitInputUrl, gitRemote, listedBuiltins, localFileUrl, mkStr, readThunkValue, runPureEval, typeName, unrestrictedPolicy)
import Nix.Eval.AllocSite (EnvSite (..), SetSite (..), envSiteName, setSiteName)
import Nix.Eval.AttrPath (parseAttrPath, selectAttrPath)
import Nix.Eval.AutoCall (autoCallFunction)
import Nix.Eval.Builtin (builtinIndex, builtinName, builtinNamed)
import qualified Nix.Eval.Builtin as Builtin
import Nix.Eval.CAttrSet (cattrsetFreeze, cattrsetInsert, cattrsetNew, cattrsetSize, cattrsetUnion, cattrsetUpdate)
import Nix.Eval.CBytecode (cbcArg1, cbcArg2, cbcCountedPayload, cbcData, cbcFlags, cbcString)
import Nix.Eval.CData (CData, MonadCData (..), runCDataPure)
import Nix.Eval.CEnv (cenvParent)
import Nix.Eval.CList (CList (..), clistDrop, clistFromThunks, clistIndex, clistLen, clistThunks, thunkListToCList)
import Nix.Eval.CRoots (crootsAddThunk)
import Nix.Eval.CStats (CRoot (..), Census (..), EnvSiteFigures (..), EnvSize (..), Figures (..), PerKind (..), SetSiteFigures (..), SiteCounts (..), countSites, envSitesCounted, renderCensus, setSitesCounted, takeCensus, valueRoots)
import Nix.Eval.CStatus (CStatusError (..))
import Nix.Eval.CThunk (CThunkPtr, cthunkMarkBlackhole, cthunkNewComputedAttrs, cthunkNewComputedInt, cthunkNewComputedList, cthunkNull, cthunkPayload, cthunkPrimop, cthunkState)
import Nix.Eval.CallDepth (topLevelCallDepth)
import Nix.Eval.CanonPath (canonPath, canonPathValue)
import Nix.Eval.Compile (BcBindings (..), BcShares (..), SiblingVar (..), compileExpr, compileParsed, foldBcShares, siblingShares)
import Nix.Eval.Context (contextRequest)
import qualified Nix.Eval.Context as Context
import Nix.Eval.EvalFormals (EvalFormal (..), EvalFormals (..), FormalDefault (..))
import Nix.Eval.IO (EvalFailure (..), EvalState (..), SessionStore (..), allowEvalPath, evalBaseEnv, newEvalStateWith, nixPathSearchPath, renderEvalFailure, runEvalIO, runEvalIOTraced, sourceCopies)
import Nix.Eval.Operator (evalUpdate)
import Nix.Eval.Policy (ImportFromDerivation (..), LinkTarget (..), PathRules (..), SymlinkResolution (..), WalkStop (..), allowPathIn, forbiddenPathMessage, isAllowedPath, isAllowedUri, joinComponents, noAllowedPaths, pathComponents, storeRootPrefix, uriAccess, walkPath, windowsLinkTargetJoins)
import qualified Nix.Eval.Policy as Policy
import Nix.Eval.Print (PrintOptions (..), printAmbiguous, printSymbol, printValue)
import Nix.Eval.RealiseContext (ContextRefusal (..), ContextRequest (..), PendingOrigin (..), Plan (..), SessionMode (..), contextRefusalMessage, findFileRefusalMessage, planRealisation, referenceClosure)
import Nix.Eval.SourceError (SourceAccess (..), sourceErrorMessage)
import Nix.Eval.SourcePos (noPos)
import Nix.Eval.StartupSymbols (startupSymbols)
import Nix.Eval.StringInterp (joinCoercedList)
import Nix.Eval.Symbol (Symbol (..), symbolCount, symbolIntern, symbolText)
import Nix.Eval.Types (AttrSet (..), Env (..), Pending (..), attrSetFromEntries, attrSetFromSymbolEntriesSizedFor, attrSetKeys, attrSetRemoveKeys, attrSetToMap, buildCaptureEnv, cheapThunkBc, emptyAttrSet, emptyCList, envLookupResolved, envWithScopesRaw, mkApply2Thunk, mkApplyThunk, mkThunkBc, newEnv, newScopeEnv, pushLazyWithScope, readComputed, readPending, tieFrame, pattern ThunkBlackhole, pattern ThunkComputed)
import Nix.Expr.Types
import Nix.Hash (FileIngestionMethod (..), makeFixedOutputPath, sha256Digest)
import qualified Nix.Hash as Hash
import Nix.Parser (parseNix, parseNixWithScope, parsedExpr)
import qualified Nix.Realise as Realise
import Nix.Store (Store (..), closeStore, isValid, materializeEvalSources, openStore, registrationFor)
import Nix.Store.CaseSensitive (CaseSensitivity (..), pathCaseHack, probeCaseSensitivity, processCaseHack, volumeCaseHack)
import Nix.Store.DB (dbFileName, metaDirName, registerPath)
import qualified Nix.Store.ExecBit as ExecBit
import Nix.Store.Metadata (executablePermissions, readOnlyPermissions, storeModificationTime)
import Nix.Store.Ops (EvalObjects (..), StoreOps (..), StoreQueries (..), localQueries)
import Nix.Store.Path (StoreDir (..), StorePath, defaultStoreDir, defaultStoreDirText, isCanonicalStoreText, isInStoreText, parseStorePath, platformStoreDir, qualifyPathText, storePathToFilePath, storePathToText, windowsStoreDir)
import Nix.Store.Path.Internal (StorePath (..))
import NodeStatus (NodeStatus (..), nodeStatus)
import qualified NovaCache.Base32 as Nix32
import qualified NovaCache.Base64 as B64
import qualified NovaCache.NAR as NAR
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getTemporaryDirectory, removeDirectoryRecursive)
import qualified System.Directory as Dir
import System.Environment (getEnvironment, getExecutablePath, lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import System.FilePath (dropDrive, makeRelative, takeDirectory, takeFileName, (</>))
import qualified System.Info as SI
import qualified System.Process as Proc
import System.Timeout (timeout)
import Test.Harness (TestResult (..), andThen, asciiOsPath, assertEqual, assertEval, assertEvalError, assertEvalErrorIO, assertEvalFail, assertLeft, assertParse, assertParseFail, assertRight, bytesText, canonicalAs, censusStoreHash, compiledBindings, decodeFormals, emits, envSpelling, evalAndBuild, evalDerivationIO, evalDerivationIOStore, evalNix, evalNixIO, evalNixIOIn, evalNixIOStore, evalNixIOStoreDir, evalNixInFile, evalNixWith, fetchedStorePath, findTestShell, fixtureGit, forceRemoveIfExists, inTestSession, jsonInFile, makeGitFixture, nixQuotedPath, positionJson, positionsFile, purePolicy, removeIfExists, repeatedMarker, restrictedPolicy, runTest, runTestIO, runTestM, selfHoldingDerivation, selfHoldingPrinted, selfHoldingPrintedStrict, setLayerRhsMax, setMaxLayers, sitesAdded, storePathStringValue, symlinksAvailable, testBaseDir, testEvalState, testOrigin, testPos, testSessionStore, unexpectedOps, upstreamBaseEnvNames, walkWatchdogMicros, withEnvVar, withGlobals)
import qualified Test.QuickCheck as QC

-- ---------------------------------------------------------------------------
-- Test harness
-- ---------------------------------------------------------------------------

-- | 'assertRight' with a check that reads the C data layer.
assertRightM :: (Show e) => Text -> Either e a -> (a -> IO TestResult) -> IO TestResult
assertRightM label result check = case result of
  Left err -> pure (Fail (label <> ": got error: " <> T.pack (show err)))
  Right val -> check val

-- | Each entry of a parsed @NIX_PATH@ as its prefix and path values, each
-- 'Nothing' when absent or not computed.
searchPathEntries :: BS.ByteString -> IO [(Maybe NixValue, Maybe NixValue)]
searchPathEntries raw = parseNixPath raw >>= mapM entry
  where
    entry thunk =
      readThunkValue thunk >>= \case
        Just (VAttrs m) -> (,) <$> computed "prefix" m <*> computed "path" m
        _ -> pure (Nothing, Nothing)
    computed key m = attrSetLookup key m >>= maybe (pure Nothing) readThunkValue

-- ---------------------------------------------------------------------------
-- Tests: Eval - Literals
-- ---------------------------------------------------------------------------

testEvalLiterals :: IO [Bool]
testEvalLiterals = do
  putStrLn "eval/literals"
  sequence
    [ runTest "empty env" $
        assertEqual "emptyEnv" emptyEnv emptyEnv,
      runTest "int" $
        assertEval "int" "42" (VInt 42),
      runTest "float" $
        assertEval "float" "3.14" (VFloat 3.14),
      runTest "bool true" $
        assertEval "true" "true" (VBool True),
      runTest "null" $
        assertEval "null" "null" VNull,
      runTest "string" $
        assertEval "string" "\"hello\"" (mkStr "hello")
    ]

-- ---------------------------------------------------------------------------
-- Tests: Eval - Variables
-- ---------------------------------------------------------------------------

testEvalVariables :: IO [Bool]
testEvalVariables = do
  putStrLn "eval/variables"
  sequence
    [ runTest "let variable" $
        assertEval "let-var" "let x = 1; in x" (VInt 1),
      runTest "undefined variable" $
        assertEvalFail "undef" "x",
      runTest "builtin true" $
        assertEval "builtin-true" "true" (VBool True)
    ]

-- ---------------------------------------------------------------------------
-- Tests: Eval - Arithmetic
-- ---------------------------------------------------------------------------

testEvalArithmetic :: IO [Bool]
testEvalArithmetic = do
  putStrLn "eval/arithmetic"
  sequence
    [ runTest "int add" $
        assertEval "add" "1 + 2" (VInt 3),
      runTest "int sub" $
        assertEval "sub" "10 - 3" (VInt 7),
      runTest "int mul" $
        assertEval "mul" "4 * 5" (VInt 20),
      runTest "int div" $
        assertEval "div" "10 / 3" (VInt 3),
      runTest "float add" $
        assertEval "float-add" "1.5 + 2.5" (VFloat 4.0),
      -- Integer overflow is an eval error, never a two's-complement wrap;
      -- the boundary itself still computes.  The messages are upstream's
      -- from 2.25 on, as nix-instantiate 2.33.2 prints them (2.24.9 leaves
      -- overflow to undefined C++ signed arithmetic), except division's,
      -- which 2.24.9 checks itself and 2.25 rewords.
      runTest "add overflow fails" $
        assertEvalError "add-overflow" "9223372036854775807 + 1" "integer overflow in adding 9223372036854775807 + 1",
      runTest "sub overflow fails" $
        assertEvalError "sub-overflow" "(-9223372036854775807) - 2" "integer overflow in subtracting -9223372036854775807 - 2",
      runTest "mul overflow fails" $
        assertEvalError "mul-overflow" "builtins.mul 9223372036854775807 2" "integer overflow in multiplying 9223372036854775807 * 2",
      runTest "div minBound by -1 overflows" $
        assertEvalError "div-overflow" "((-9223372036854775807) - 1) / (-1)" "overflow in integer division",
      runTest "builtins.div minBound by -1 overflows" $
        assertEvalError "builtin-div-overflow" "builtins.div ((-9223372036854775807) - 1) (-1)" "overflow in integer division",
      runTest "negate minBound overflows as 0 - n" $
        assertEvalError "neg-overflow" "-((-9223372036854775807) - 1)" "integer overflow in subtracting 0 - -9223372036854775808",
      -- Unary minus is __sub 0 e upstream (parser.y at 2.24.9).
      runTest "negating a float zero gives positive zero" $
        assertEval "neg-zero" "builtins.toJSON [ (-(0.0)) (-(-0.0)) ]" (mkStr "[0.0,0.0]"),
      runTest "negating a non-number fails as builtins.sub does" $
        assertEvalError "neg-set" "-{ }" "expected an integer but found a set: { }",
      -- -, *, / and the arithmetic builtins are upstream's prim_sub,
      -- prim_mul, prim_div and prim_add (primops.cc at 2.24.9): forceInt
      -- on both operands, or forceFloat on both once either is a float.
      runTest "subtracting a set from an integer fails its forceInt" $
        assertEvalError "sub-set" "1 - {}" "expected an integer but found a set: { }",
      runTest "subtracting from a set fails on the left operand" $
        assertEvalError "set-sub" "{} - 1" "expected an integer but found a set: { }",
      runTest "subtracting a set from a float fails its forceFloat" $
        assertEvalError "float-sub-set" "1.0 - {}" "expected a float but found a set: { }",
      runTest "two non-numbers fail on the left one" $
        assertEvalError "set-sub-str" "{} - \"a\"" "expected an integer but found a set: { }",
      runTest "multiplying a float by a string fails its forceFloat" $
        assertEvalError "float-mul-str" "2.5 * \"a\"" "expected a float but found a string: \"a\"",
      runTest "dividing by a set fails the divisor's forceFloat" $
        assertEvalError "div-set" "1 / {}" "expected a float but found a set: { }",
      runTest "dividing a set by an integer fails its forceInt" $
        assertEvalError "set-div" "{} / 1" "expected an integer but found a set: { }",
      runTest "a zero divisor is refused before the dividend is checked" $
        assertEvalError "set-div-zero" "{} / 0" "division by zero",
      runTest "a non-number divisor is refused before the dividend" $
        assertEvalError "set-div-str" "{} / \"a\"" "expected a float but found a string: \"a\"",
      runTest "dividing by negative zero is a division by zero" $
        assertEvalError "div-neg-zero" "1 / (-0.0)" "division by zero",
      runTest "builtins.div by zero says only division by zero" $
        assertEvalError "builtin-div-zero" "builtins.div 1 0" "division by zero",
      runTest "builtins.add refuses a string through forceInt" $
        assertEvalError "builtin-add-str" "builtins.add \"a\" 1" "expected an integer but found a string: \"a\"",
      runTest "builtins.add with a float refuses a string through forceFloat" $
        assertEvalError "builtin-add-float-str" "builtins.add 1.5 \"a\"" "expected a float but found a string: \"a\"",
      runTest "builtins.sub refuses a set" $
        assertEvalError "builtin-sub-set" "builtins.sub 1 {}" "expected an integer but found a set: { }",
      runTest "builtins.bitXor refuses a float" $
        assertEvalError "bitxor-float" "builtins.bitXor 1 1.0" "expected an integer but found a float: 1",
      runTest "builtins.bitAnd fails on its left operand first" $
        assertEvalError "bitand-set-str" "builtins.bitAnd {} \"a\"" "expected an integer but found a set: { }",
      runTest "builtins.bitOr refuses a set" $
        assertEvalError "bitor-set" "builtins.bitOr {} 1" "expected an integer but found a set: { }",
      runTest "float division still divides" $
        assertEval "float-div" "1 / 4.0" (VFloat 0.25),
      runTest "add at the boundary still works" $
        assertEval "add-boundary" "9223372036854775806 + 1 == 9223372036854775807" (VBool True),
      runTest "int-float promotion" $
        assertEval "promote" "1 + 2.0" (VFloat 3.0),
      runTest "trailing-dot float" $
        assertEval "trailing-dot" "12. == 12.0" (VBool True),
      runTest "trailing-dot float with exponent" $
        assertEval "trailing-dot-exp" "12.e2 == 1200.0" (VBool True),
      runTest "trailing-dot float typeOf" $
        assertEval "trailing-dot-type" "builtins.typeOf 12." (mkStr "float"),
      runTest "negate int" $
        assertEval "negate" "- 5" (VInt (-5)),
      runTest "division by zero" $
        assertEvalFail "div0" "1 / 0",
      runTest "absolute path lexes as path, not division" $
        assertEval "path-abs" "builtins.typeOf /abs/path" (mkStr "path"),
      runTest "bareword relative path lexes as path" $
        assertEval "path-rel" "builtins.typeOf foo/bar" (mkStr "path"),
      runTest "function applied to an absolute path argument" $
        assertEval "app-abs-path" "(p: builtins.typeOf p) /abs/path" (mkStr "path")
    ]

-- ---------------------------------------------------------------------------
-- Tests: Eval - Comparison
-- ---------------------------------------------------------------------------

testEvalComparison :: IO [Bool]
testEvalComparison = do
  putStrLn "eval/comparison"
  sequence
    [ runTest "int eq" $
        assertEval "eq" "1 == 1" (VBool True),
      runTest "int neq" $
        assertEval "neq" "1 != 2" (VBool True),
      runTest "int lt" $
        assertEval "lt" "1 < 2" (VBool True),
      runTest "int gte" $
        assertEval "gte" "3 >= 3" (VBool True),
      runTest "string compare" $
        assertEval "str-lt" "\"abc\" < \"def\"" (VBool True),
      -- <= and >= are negated swapped <, so NaN compares as upstream:
      -- nan <= x and nan >= x are true while nan < x and nan == nan are
      -- false ((1.0e308 * 10) * 0.0 is inf * 0.0, i.e. nan).
      runTest "nan lte is true" $
        assertEval "nan-lte" "let nan = (1.0e308 * 10) * 0.0; in nan <= 1.0" (VBool True),
      runTest "nan gte is true" $
        assertEval "nan-gte" "let nan = (1.0e308 * 10) * 0.0; in nan >= 1.0" (VBool True),
      runTest "nan lt is false" $
        assertEval "nan-lt" "let nan = (1.0e308 * 10) * 0.0; in nan < 1.0" (VBool False),
      runTest "nan self-equality is false" $
        assertEval "nan-eq" "let nan = (1.0e308 * 10) * 0.0; in nan == nan" (VBool False),
      runTest "lte on equal lists" $
        assertEval "list-lte-eq" "[ 1 2 ] <= [ 1 2 ]" (VBool True),
      runTest "lte incomparable types fails" $
        assertEvalFail "lte-err" "1 <= \"a\"",
      -- Upstream's CompareValues (primops.cc at 2.24.9); > and <= swap
      -- the operands as the parser's desugaring to __lessThan does.
      runTest "comparing different types names both" $
        assertEvalError "lt-int-set" "1 < {}" "cannot compare an integer with a set",
      runTest "gt compares the swapped operands" $
        assertEvalError "gt-int-set" "1 > {}" "cannot compare a set with an integer",
      runTest "lte compares the swapped operands" $
        assertEvalError "lte-int-set" "1 <= {}" "cannot compare a set with an integer",
      runTest "gte compares the operands in order" $
        assertEvalError "gte-int-set" "1 >= {}" "cannot compare an integer with a set",
      runTest "Booleans are incomparable" $
        assertEvalError "lt-bool" "true < false" "cannot compare a Boolean with a Boolean; values of that type are incomparable",
      runTest "a lambda and a primop are both functions, so incomparable" $
        assertEvalError "lt-fn" "(x: x) < builtins.map" "cannot compare a function with the built-in function 'map'; values of that type are incomparable",
      runTest "builtins.lessThan on nulls is incomparable" $
        assertEvalError "lessthan-null" "builtins.lessThan null null" "cannot compare null with null; values of that type are incomparable",
      runTest "list comparison reports the first unequal pair" $
        assertEvalError "lt-list" "[ 1 ] < [ \"a\" ]" "cannot compare an integer with a string",
      runTest "equal incomparable elements do not stop a list comparison" $
        assertEval "lt-list-sets" "[ {} ] < [ {} ]" (VBool False),
      -- Upstream's eqValues forces both sides before it takes a value as
      -- equal to itself (eval.cc at 2.24.9), and a list literal holds a
      -- variable's own value, so both lists share x.
      runTest "== forces a list element both sides share" $
        assertEvalError "eq-shared-elem" "let x = throw \"x\"; in [ x ] == [ x ]" "x",
      runTest "== forces an attribute both sides share" $
        assertEvalError "eq-shared-attr" "let x = throw \"x\"; in { a = x; } == { a = x; }" "x",
      runTest "< forces a list element both sides share" $
        assertEvalError "lt-shared-elem" "let x = throw \"x\"; in [ x ] < [ x ]" "x",
      -- Under the IO evaluator, which keeps the output set once it is
      -- forced, so d and d.out are one set holding one outPath; the pure
      -- one builds the set again at each use.
      runTestM "== forces the outPath two derivations share" $ do
        tmpBase <- getTemporaryDirectory
        assertEvalErrorIO
          tmpBase
          "eq-shared-outpath"
          "let d = derivation { name = \"w\"; system = \"x\"; builder = \"/bin/sh\"; x = throw \"x\"; }; in d == d.out"
          "x",
      runTest "a function both sides share is equal to itself" $
        assertEval "eq-shared-fn" "let f = y: y; in [ f ] == [ f ]" (VBool True),
      runTest "two functions are never equal" $
        assertEval "eq-fns" "[ (y: y) ] == [ (y: y) ]" (VBool False),
      runTest "a NaN both sides share is equal to itself" $
        assertEval "eq-shared-nan" "let nan = (1.0e308 * 10) * 0.0; in [ nan ] == [ nan ]" (VBool True),
      -- Upstream's eqValues compares two derivations by their outPaths,
      -- takes sets of different sizes as unequal, and walks two sets of
      -- one size pair by pair in symbol order, each pair's names and then
      -- its values, stopping at the first difference (eval.cc).  A
      -- symbol's order is the order the table interned the name in, so
      -- the walk's order is pinned in a child process, whose table is
      -- new: there, as under upstream, the names below are interned in
      -- the order the expression gives them, since neither evaluator
      -- interns them at startup.
      emits
        "== compares a pair's values before a later pair's names"
        "builtins.trace ({ zq1 = throw \"x\"; zq2 = 1; } == { zq1 = 1; zq3 = 1; }) null"
        ["error: x"],
      emits
        "== decides at the pair whose name was interned first"
        "builtins.trace ({ zb = 1; za = throw \"a\"; } == { zb = 2; za = 1; }) null"
        ["trace: false"],
      emits
        "== forces each pair's values, left then right, up to the pair whose values differ, and no value past it"
        "builtins.trace ({ za = builtins.trace \"a1\" 1; zb = builtins.trace \"b1\" 2; zc = builtins.trace \"c1\" 3; } == { za = builtins.trace \"a2\" 1; zb = builtins.trace \"b2\" 5; zc = builtins.trace \"c2\" 3; }) null"
        ["trace: a1", "trace: a2", "trace: b1", "trace: b2", "trace: false"],
      -- zc is interned before zb, so the second pair is zc and zb.
      emits
        "== forces no value past the pair whose names differ"
        "builtins.trace ({ za = builtins.trace \"a1\" 1; zc = builtins.trace \"c1\" 1; } == { za = builtins.trace \"a2\" 1; zb = builtins.trace \"b2\" 1; }) null"
        ["trace: a1", "trace: a2", "trace: false"],
      runTest "== takes a smaller set as unequal without forcing a value" $
        assertEval "eq-set-smaller" "{ zs1 = throw \"a\"; } == { zs1 = throw \"a\"; zs2 = 1; }" (VBool False),
      runTest "== takes a larger set as unequal without forcing a value" $
        assertEval "eq-set-larger" "{ zs1 = throw \"a\"; zs2 = 1; } == { zs1 = throw \"a\"; }" (VBool False),
      runTest "== compares two derivations by outPath before their sizes or other values" $
        assertEval
          "eq-drv-outpath-first"
          "{ type = \"derivation\"; outPath = \"/a\"; x = throw \"x\"; } == { type = \"derivation\"; outPath = \"/a\"; }"
          (VBool True),
      runTestM "== on two sets answers as a comparison of their names, then their values by name, whenever both answer" $ do
        result <-
          QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} $
            QC.forAll genSetPair setEqualityAgrees
        pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))
    ]
  where
    -- Two sets over a pool of names new to each case, so the left set's
    -- order of them, shuffled, is their interning order, which is not
    -- their name order.  The right set has the left's names in another
    -- order or any others from the pool, so the two can differ in size or
    -- in a name; a value is 0, 1 or a throw.
    genSetPair = do
      tag <- QC.choose (0, maxBound :: Int)
      let pool = [T.pack ("eqOrder" <> show tag <> "n" <> show i) | i <- [0 .. 5 :: Int]]
      leftNames <- QC.shuffle =<< QC.sublistOf pool
      rightNames <- QC.oneof [QC.shuffle leftNames, QC.shuffle =<< QC.sublistOf pool]
      (,) <$> bindingsOf leftNames <*> bindingsOf rightNames
    bindingsOf names = zip names <$> QC.vectorOf (length names) genSetValue
    genSetValue = QC.frequency [(4, QC.elements ["0", "1"]), (1, pure throwingValue)]
    throwingValue = "throw \"t\""
    setSource bindings = "{ " <> T.concat [name <> " = " <> value <> "; " | (name, value) <- bindings] <> "}"
    -- The walk's order decides which failure wins and nothing else: where
    -- both answer, it answers as a comparison of the names whole and then
    -- the values in name order does.  With no throwing value, both answer.
    setEqualityAgrees (left, right) =
      let walked = evalNix (setSource left <> " == " <> setSource right)
          byName =
            evalNix
              ( "let a = "
                  <> setSource left
                  <> "; b = "
                  <> setSource right
                  <> "; names = builtins.attrNames a; in names == builtins.attrNames b && builtins.all (n: a.${n} == b.${n}) names"
              )
          throws = any ((== throwingValue) . snd) (left ++ right)
       in QC.counterexample (show (walked, byName)) $ case (walked, byName) of
            (Right answer, Right expected) -> answer == expected
            _ -> throws

-- ---------------------------------------------------------------------------
-- Tests: Eval - Logic
-- ---------------------------------------------------------------------------

testEvalLogic :: IO [Bool]
testEvalLogic = do
  putStrLn "eval/logic"
  sequence
    [ runTest "and true" $
        assertEval "and-true" "true && true" (VBool True),
      runTest "and short-circuit" $
        assertEval "and-short" "false && true" (VBool False),
      runTest "or true" $
        assertEval "or-true" "true || false" (VBool True),
      runTest "or short-circuit" $
        assertEval "or-short" "false || true" (VBool True),
      runTest "not" $
        assertEval "not" "!false" (VBool True),
      runTest "implication false->x" $
        assertEval "impl-false" "false -> false" (VBool True),
      runTest "implication true->true" $
        assertEval "impl-true" "true -> true" (VBool True)
    ]

-- ---------------------------------------------------------------------------
-- Tests: Eval - Strings
-- ---------------------------------------------------------------------------

testEvalStrings :: IO [Bool]
testEvalStrings = do
  putStrLn "eval/strings"
  sequence
    [ runTest "string concat" $
        assertEval "concat" "\"hello\" + \" world\"" (mkStr "hello world"),
      runTest "string interpolation" $
        assertEval "interp" "let x = \"world\"; in \"hello ${x}\"" (mkStr "hello world"),
      runTest "interpolation coerce int" $
        assertEval "coerce-int" "\"val=${builtins.toString 42}\"" (mkStr "val=42"),
      runTest "empty string" $
        assertEval "empty" "\"\"" (mkStr "")
    ]

-- ---------------------------------------------------------------------------
-- Tests: Eval - If/Assert
-- ---------------------------------------------------------------------------

testEvalIfAssert :: IO [Bool]
testEvalIfAssert = do
  putStrLn "eval/if-assert"
  sequence
    [ runTest "if true" $
        assertEval "if-true" "if true then 1 else 2" (VInt 1),
      runTest "if false" $
        assertEval "if-false" "if false then 1 else 2" (VInt 2),
      runTest "assert pass" $
        assertEval "assert-pass" "assert true; 42" (VInt 42),
      runTest "assert fail" $
        assertEvalFail "assert-fail" "assert false; 42"
    ]

-- ---------------------------------------------------------------------------
-- Tests: Eval - Let
-- ---------------------------------------------------------------------------

testEvalLet :: IO [Bool]
testEvalLet = do
  putStrLn "eval/let"
  sequence
    [ runTest "simple let" $
        assertEval "let" "let x = 1; in x" (VInt 1),
      runTest "multi let" $
        assertEval "multi" "let x = 1; y = 2; in x + y" (VInt 3),
      runTest "recursive let" $
        assertEval "rec-let" "let x = 1; y = x + 1; in y" (VInt 2)
    ]

-- ---------------------------------------------------------------------------
-- Tests: Eval - Attribute sets
-- ---------------------------------------------------------------------------

testEvalAttrs :: IO [Bool]
testEvalAttrs = do
  putStrLn "eval/attrs"
  sequence
    [ runTest "simple select" $
        assertEval "select" "{ a = 1; }.a" (VInt 1),
      runTest "nested select" $
        assertEval "nested" "{ a = { b = 2; }; }.a.b" (VInt 2),
      runTest "select or default" $
        assertEval "default" "{ a = 1; }.b or 42" (VInt 42),
      runTest "has-attr true" $
        assertEval "has-true" "{ a = 1; } ? a" (VBool True),
      runTest "has-attr false" $
        assertEval "has-false" "{ a = 1; } ? b" (VBool False),
      runTest "nested attr path" $
        assertEval "dot-path" "{ a.b.c = 1; }.a.b.c" (VInt 1),
      -- A non-set operand of // fails with upstream's evalAttrs message
      -- (eval.cc at 2.24.9), checked against nix-instantiate 2.33.2's
      -- final error line.  tryEval does not catch it.
      runTest "update with a null left operand" $
        assertEvalError "update-null-left" "null // {}" "expected a set but found null: null",
      runTest "update with a null right operand" $
        assertEvalError "update-null-right" "{} // null" "expected a set but found null: null",
      runTest "update with an integer left operand" $
        assertEvalError "update-int-left" "1 // {}" "expected a set but found an integer: 1",
      runTest "update with a string right operand" $
        assertEvalError "update-string-right" "{} // \"x\"" "expected a set but found a string: \"x\"",
      -- The update queue of 2.34 and later evaluates and checks the right
      -- operand before it evaluates the left (ExprOpUpdate::evalForUpdate
      -- in eval.cc); 2.24.9 does the left first.
      runTest "update with two non-set operands reports the right" $
        assertEvalError "update-both" "1 // \"x\"" "expected a set but found a string: \"x\"",
      runTest "update evaluates a throwing right before checking the left" $
        assertEval "update-right-first" "(builtins.tryEval (null // throw \"b\")).success" (VBool False),
      runTest "update with a set left evaluates a throwing right" $
        assertEval "update-right-throws" "(builtins.tryEval ({} // throw \"b\")).success" (VBool False),
      runTest "update refuses a non-set right before evaluating a throwing left" $
        assertEvalError "update-left-throws" "throw \"a\" // null" "expected a set but found null: null",
      runTest "a chain of updates fails at the last operand that fails" $
        assertEvalError "update-chain-last" "throw \"first\" // { a = 1; } // throw \"last\"" "last",
      -- builtins.warn forces its message with forceString (prim_warn,
      -- primops.cc at 2.24.9); final lines as nix-instantiate 2.33.2
      -- prints them, and tryEval does not catch the refusal.
      runTest "warn refuses an integer message" $
        assertEvalError "warn-int" "builtins.warn 1 2" "expected a string but found an integer: 1",
      runTest "warn refuses a Boolean message" $
        assertEvalError "warn-bool" "builtins.warn true 2" "expected a string but found a Boolean: true",
      runTest "warn refuses a null message" $
        assertEvalError "warn-null" "builtins.warn null 2" "expected a string but found null: null",
      runTest "warn refuses a float message" $
        assertEvalError "warn-float" "builtins.warn 1.5 2" "expected a string but found a float: 1.5",
      runTest "warn's refusal is not caught by tryEval" $
        assertEvalError "warn-tryeval" "builtins.tryEval (builtins.warn 1 2)" "expected a string but found an integer: 1",
      runTest "warn with a string message returns its value" $
        assertEval "warn-string" "builtins.warn \"m\" 2" (VInt 2),
      runTest "tryEval does not catch the update error" $
        assertEvalError "update-tryEval" "builtins.tryEval (null // {})" "expected a set but found null: null"
    ]

-- | One operand of an update: the shared empty set, an empty set of its
-- own, or a set holding these names.
data UpdateOperand = SharedEmpty | OwnEmpty | Holding ![Text]
  deriving (Show)

genUpdateOperand :: QC.Gen UpdateOperand
genUpdateOperand =
  QC.oneof
    [ pure SharedEmpty,
      pure OwnEmpty,
      Holding <$> QC.listOf1 (QC.elements ["a", "b", "c", "d"])
    ]

-- | The shared empty set (cbits/nn_attrset.h) and the identity of the
-- sets @//@ gives, which upstream's printers show by entering a set by
-- its bindings.
testSharedEmptySet :: IO [Bool]
testSharedEmptySet = do
  putStrLn "eval/shared-empty-set"
  sequence
    [ -- ExprOpUpdate::eval (eval.cc): the left operand's emptiness is
      -- checked first, so two empty operands give the right one.
      runTestM "an update with an empty operand is the other operand's set, the left checked first, and otherwise a new right-biased set" $
        quickCheckPasses $
          QC.forAll ((,) <$> genUpdateOperand <*> genUpdateOperand) $ \(leftShape, rightShape) -> QC.ioProperty $ do
            left <- operandSet 0 leftShape
            right <- operandSet 1 rightShape
            leftEntries <- entryCells left
            rightEntries <- entryCells right
            outcome <- evaluate (runPureEval (evalUpdate (pure (VAttrs left)) (pure (VAttrs right))))
            case outcome of
              Right (VAttrs merged)
                | Map.null leftEntries -> pure (unAttrSet merged QC.=== unAttrSet right)
                | Map.null rightEntries -> pure (unAttrSet merged QC.=== unAttrSet left)
                | otherwise -> do
                    mergedEntries <- entryCells merged
                    pure $
                      (mergedEntries, unAttrSet merged `elem` map unAttrSet [left, right])
                        QC.=== (Map.union rightEntries leftEntries, False)
              other -> pure (QC.counterexample ("update gave " <> show other) False),
      runTestM "the shared empty set holds nothing" $ do
        size <- attrSetSize emptyAttrSet
        keys <- attrSetKeys emptyAttrSet
        pure (assertEqual "size and keys" (0, []) (size, keys))
    ]
  where
    quickCheckPasses property = do
      result <- QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} property
      pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))
    -- Each side's values are its own integer, so a merged entry shows
    -- which side it came from.
    operandSet :: Int64 -> UpdateOperand -> IO AttrSet
    operandSet _ SharedEmpty = pure emptyAttrSet
    operandSet _ OwnEmpty = attrSetFromSymbolEntriesSizedFor SetLiteral 1 Map.empty
    operandSet side (Holding names) = do
      cells <- mapM (\name -> (name,) <$> evaluated (VInt side)) names
      attrSetFromMap SetLiteral (Map.fromList cells)
    entryCells set = Map.map unThunk <$> attrSetToMap set

-- ---------------------------------------------------------------------------
-- Tests: Eval - Value printer
-- ---------------------------------------------------------------------------

testValuePrinter :: IO [Bool]
testValuePrinter = do
  putStrLn "eval/print"
  tmpBase <- getTemporaryDirectory
  let stringFound body = "expected a set but found a string: " <> body
      setFound body = "cannot coerce a set to a string: " <> body
      listFound body = "cannot coerce a list to a string: " <> body
      runOfA n = "builtins.concatStringsSep \"\" (builtins.genList (x: \"a\") " <> T.pack (show (n :: Int)) <> ")"
      elided what = " \x00AB" <> what <> " elided\x00BB"
      marker inner = "\x00AB" <> inner <> "\x00BB"
      interpolated source = "\"${" <> source <> "}\""
      deeplyForced source = "let x = " <> source <> "; in builtins.seq (builtins.deepSeq x null) \"${x}\""
      ints n = map (T.pack . show) [1 .. n :: Int]
      listOfInts n = "[ " <> T.concat (map (<> " ") (ints n)) <> "]"
      attrsOfInts n = "{ " <> T.concat [name <> " = " <> value <> "; " | (name, value) <- zip letters (ints n)] <> "}"
      letters = map T.singleton ['a' ..]
      shownInts = T.unwords [name <> " = " <> value <> ";" | (name, value) <- zip letters (ints 10)]
      paddedName i = T.pack ('a' : if i < 10 then '0' : show i else show i)
      paddedAttrs n = "{ " <> T.concat [paddedName i <> " = " <> T.pack (show i) <> "; " | i <- [1 .. n]] <> "}"
      boundedList n = bounded "[" "]" (ints (min n 10)) (n - 10) "item" "items"
      boundedAttrs n =
        bounded "{" "}" [paddedName i <> " = " <> T.pack (show i) <> ";" | i <- [1 .. min n 10]] (n - 10) "attribute" "attributes"
      bounded open close shown left single plural =
        open <> " " <> T.concat (map (<> " ") shown) <> leftOut left single plural <> close
      leftOut left single plural
        | left <= 0 = ""
        | left == 1 = marker ("1 " <> single <> " elided") <> " "
        | otherwise = marker (T.pack (show left) <> " " <> plural <> " elided") <> " "
      quickCheckPasses property = do
        result <- QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} property
        pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))
  sequence
    [ -- Values print as upstream's printValue prints them under
      -- errorPrintOptions (print.cc and print-options.hh at 2.24.9), each
      -- checked against nix-instantiate 2.33.2's final error line.
      runTest "a path prints bare" $
        assertEvalError "print-path" "{} // /foo/bar" "expected a set but found a path: /foo/bar",
      runTest "a float prints with six significant digits" $
        assertEvalError "print-float" "{} // 1234567.0" "expected a set but found a float: 1.23457e+06",
      runTest "a string prints quoted with the lexer's escapes" $
        assertEvalError
          "print-escapes"
          "{} // \"a\\nb\\\"c$d\\${e}\\t\\r\""
          (stringFound "\"a\\nb\\\"c$d\\${e}\\t\\r\""),
      runTest "a string of 1024 bytes prints whole" $
        assertEvalError
          "print-1024"
          ("{} // " <> runOfA 1024)
          (stringFound ("\"" <> T.replicate 1024 "a" <> "\"")),
      runTest "a longer string is cut after 1024 bytes" $
        assertEvalError
          "print-1025"
          ("{} // " <> runOfA 1025)
          (stringFound ("\"" <> T.replicate 1024 "a" <> "\"" <> elided "1 byte")),
      runTest "the bytes cut are counted" $
        assertEvalError
          "print-1100"
          ("{} // " <> runOfA 1100)
          (stringFound ("\"" <> T.replicate 1024 "a" <> "\"" <> elided "76 bytes")),
      runTest "a dollar shown last is escaped when the brace after it was cut" $
        assertEvalError
          "print-cut-dollar"
          ("{} // (" <> runOfA 1023 <> " + \"\\${\")")
          (stringFound ("\"" <> T.replicate 1023 "a" <> "\\$\"" <> elided "1 byte")),
      -- A set or list prints its entries, nothing forced, and is cut
      -- once the print has shown ten attributes or ten items in all.
      runTest "a set prints its attributes by name" $
        assertEvalError "print-set" "\"${{ b = \"s\"; a = 1; }}\"" (setFound "{ a = 1; b = \"s\"; }"),
      runTest "an empty set prints as a pair of braces" $
        assertEvalError "print-empty-set" "\"${{}}\"" (setFound "{ }"),
      runTest "_type and type print ahead of the other attributes" $
        assertEvalError
          "print-important-first"
          "\"${{ z = 1; type = 2; _type = 3; a = 4; }}\""
          (setFound "{ _type = 3; type = 2; a = 4; z = 1; }"),
      runTest "an attribute name the lexer cannot read bare prints quoted" $
        assertEvalError
          "print-quoted-names"
          "\"${{ \"a b\" = 1; \"if\" = 2; \"\" = 3; }}\""
          (setFound "{ \"\" = 3; \"a b\" = 1; \"if\" = 2; }"),
      runTest "an attribute never forced prints as upstream's thunk marker" $
        assertEvalError "print-thunk" "\"${{ a = 1 + 1; }}\"" (setFound ("{ a = " <> marker "thunk" <> "; }")),
      runTest "an eleventh attribute is counted, not shown" $
        assertEvalError
          "print-11-attrs"
          (interpolated (attrsOfInts 11))
          (setFound ("{ " <> shownInts <> " " <> marker "1 attribute elided" <> " }")),
      runTest "attributes past the tenth are counted together" $
        assertEvalError
          "print-12-attrs"
          (interpolated (attrsOfInts 12))
          (setFound ("{ " <> shownInts <> " " <> marker "2 attributes elided" <> " }")),
      runTest "an empty list prints as a pair of brackets" $
        assertEvalError "print-empty-list" "\"${[ ]}\"" (listFound "[ ]"),
      runTest "an eleventh item is counted, not shown" $
        assertEvalError
          "print-11-items"
          (interpolated (listOfInts 11))
          (listFound ("[ 1 2 3 4 5 6 7 8 9 10 " <> marker "1 item elided" <> " ]")),
      runTest "items past the tenth are counted together" $
        assertEvalError
          "print-12-items"
          (interpolated (listOfInts 12))
          (listFound ("[ 1 2 3 4 5 6 7 8 9 10 " <> marker "2 items elided" <> " ]")),
      runTestM "a list shows ten items and counts the rest" $
        quickCheckPasses $
          QC.forAll (QC.chooseInt (0, 25)) $ \n ->
            evalNix (interpolated (listOfInts n)) QC.=== Left (listFound (boundedList n)),
      runTestM "a set shows ten attributes and counts the rest" $
        quickCheckPasses $
          QC.forAll (QC.chooseInt (0, 25)) $ \n ->
            evalNix (interpolated (paddedAttrs n)) QC.=== Left (setFound (boundedAttrs n)),
      -- Forced values need the IO evaluator, which keeps them.
      runTestM "a nested set's attributes count before the attribute holding it" $
        assertEvalErrorIO
          tmpBase
          "print-nested-count"
          (deeplyForced "{ a = { b = 1; c = 2; d = 3; e = 4; f = 5; g = 6; }; h = 7; i = 8; j = 9; k = 10; l = 11; }")
          (setFound ("{ a = { b = 1; c = 2; d = 3; e = 4; f = 5; g = 6; }; h = 7; i = 8; j = 9; " <> marker "2 attributes elided" <> " }")),
      runTestM "list items count over the whole print" $
        assertEvalErrorIO
          tmpBase
          "print-items-across-lists"
          (deeplyForced "{ a = [ 1 2 3 4 5 6 7 8 9 ]; b = [ 10 11 12 ]; }")
          (setFound ("{ a = [ 1 2 3 4 5 6 7 8 9 ]; b = [ 10 " <> marker "2 items elided" <> " ]; }")),
      runTestM "a set nested ten deep prints as an ellipsis" $
        assertEvalErrorIO
          tmpBase
          "print-deep-set"
          (deeplyForced (T.replicate 12 "{ a = " <> "1" <> T.replicate 12 "; }"))
          (setFound (T.replicate 10 "{ a = " <> "{ ... }" <> T.replicate 10 "; }")),
      runTestM "a list nested ten deep prints as an ellipsis" $
        assertEvalErrorIO
          tmpBase
          "print-deep-list"
          (deeplyForced (T.replicate 12 "[ " <> "1" <> T.replicate 12 " ]"))
          (listFound (T.replicate 10 "[ " <> "[ ... ]" <> T.replicate 10 " ]")),
      runTestM "an attribute being forced prints as upstream's recursion marker" $
        assertEvalErrorIO
          tmpBase
          "print-blackhole"
          "let x = { a = \"${x}\"; }; in x.a"
          (setFound ("{ a = " <> marker "potential infinite recursion" <> "; }")),
      -- Upstream renders a type error's value as it builds the error
      -- (HintFmt formats its arguments as it is constructed, fmt.hh at
      -- 2.24.9), while the attribute is still being forced.
      runTestM "a type error prints an attribute being forced as upstream's recursion marker" $
        assertEvalErrorIO
          tmpBase
          "print-blackhole-type-error"
          "let x = { a = builtins.length x; }; in x.a"
          ("expected a list but found a set: { a = " <> marker "potential infinite recursion" <> "; }"),
      -- A set or list the print reaches again prints as upstream's
      -- repeated marker: a set by its bindings, any empty one among them,
      -- and a list with elements by the cell holding it (printAttrs and
      -- printList in print.cc).  The first two expect nix-instantiate
      -- 2.24.9's final error lines for the same expressions.
      runTestM "a set reached twice prints as repeated the second time" $
        assertEvalErrorIO
          tmpBase
          "print-repeated-set"
          "let x = { a = 1; }; in builtins.seq x \"${{ p = x; q = x; }}\""
          (setFound ("{ p = { a = 1; }; q = " <> marker "repeated" <> "; }")),
      runTestM "two equal sets made apart both print in full" $
        assertEvalErrorIO
          tmpBase
          "print-equal-sets"
          "let a = { x = 1; }; b = { x = 1; }; in builtins.seq a (builtins.seq b \"${{ p = a; q = b; }}\")"
          (setFound "{ p = { x = 1; }; q = { x = 1; }; }"),
      runTestM "a second empty literal prints as repeated, as upstream's empty literals are one set" $
        assertEvalErrorIO
          tmpBase
          "print-empty-sets"
          "let a = { }; b = { }; in builtins.seq a (builtins.seq b \"${{ p = a; q = b; }}\")"
          (setFound ("{ p = { }; q = " <> marker "repeated" <> "; }")),
      -- Upstream gives bindings of their own to removeAttrs of every
      -- attribute, intersectAttrs of sets with no name in common, and a
      -- literal whose every name is a null dynamic one (prim_removeAttrs
      -- and prim_intersectAttrs in primops.cc, ExprAttrs::eval in
      -- eval.cc), and shares the rest, a removal from an empty set among
      -- them.
      runTestM "an empty set upstream gives bindings of its own prints in full after the shared one" $
        assertEvalErrorIO
          tmpBase
          "print-own-empty-sets"
          ( "let e = { }; f = removeAttrs { } [ \"x\" ]; i = builtins.intersectAttrs { x = 1; } { y = 2; }; "
              <> "n = { ${null} = 1; }; r = removeAttrs { x = 1; } [ \"x\" ]; "
              <> "in builtins.deepSeq [ e f i n r ] \"${{ inherit e f i n r; }}\""
          )
          (setFound ("{ e = { }; f = " <> marker "repeated" <> "; i = { }; n = { }; r = { }; }")),
      -- ExprOpUpdate::eval copies the other operand's Value when one is
      -- empty (eval.cc), bindings and all.
      runTestM "an update with an empty operand prints as repeated after the other operand" $
        assertEvalErrorIO
          tmpBase
          "print-update-empty-operand"
          "let a = { x = 1; }; l = a // { }; r = { } // a; in builtins.deepSeq [ l r ] \"${{ inherit a l r; }}\""
          (setFound ("{ a = { x = 1; }; l = " <> marker "repeated" <> "; r = " <> marker "repeated" <> "; }")),
      runTestM "an empty list reached twice prints in full both times" $
        assertEvalErrorIO
          tmpBase
          "print-empty-lists"
          "let a = [ ]; in builtins.seq a \"${{ p = a; q = a; }}\""
          (setFound "{ p = [ ]; q = [ ]; }"),
      runTestM "a list reached twice through one cell prints as repeated the second time" $
        assertEvalErrorIO
          tmpBase
          "print-repeated-list"
          "let l = [ 1 2 3 ]; in builtins.seq l \"${{ p = l; q = l; }}\""
          (setFound ("{ p = [ 1 2 3 ]; q = " <> marker "repeated" <> "; }")),
      runTestM "a list a function returns twice is in two cells and prints in full both times" $
        assertEvalErrorIO
          tmpBase
          "print-list-two-cells"
          (deeplyForced "let l = [ 1 2 3 ]; f = y: y; in { p = f l; q = f l; }")
          (setFound "{ p = [ 1 2 3 ]; q = [ 1 2 3 ]; }"),
      -- Upstream enters a set before it checks the depth, so one first
      -- reached past the limit is entered there, and repeated after.
      runTestM "a set first reached past the depth limit prints as repeated after" $
        assertEvalErrorIO
          tmpBase
          "print-repeated-past-depth"
          (deeplyForced ("let s = { z = 1; }; in { a = " <> T.replicate 9 "[ " <> "s" <> T.replicate 9 " ]" <> "; b = s; }"))
          (setFound ("{ a = " <> T.replicate 9 "[ " <> "{ ... }" <> T.replicate 9 " ]" <> "; b = " <> marker "repeated" <> "; }")),
      runTest "a primop prints as upstream's primop marker" $
        assertEvalError
          "print-primop"
          "{} // builtins.map"
          ("expected a set but found the built-in function 'map': " <> marker "primop map"),
      runTest "a partially applied primop prints as upstream's marker" $
        assertEvalError
          "print-primop-app"
          "{} // builtins.map (x: x)"
          ("expected a set but found the partially applied built-in function 'map': " <> marker "partially applied primop map"),
      -- builtins.trace's defaults bound nothing and sort only by name.
      runTestM "a set prints in full by name as trace prints it"
        $ assertRightM "print-full-set" (evalNix "{ type = 1; a = 2; }")
        $ fmap (assertEqual "print-full-set" "{ a = 2; type = 1; }") . printValue PrintInFull,
      runTestM "a long list prints in full as trace prints it"
        $ assertRightM "print-full-list" (evalNix (listOfInts 12))
        $ fmap (assertEqual "print-full-list" "[ 1 2 3 4 5 6 7 8 9 10 11 12 ]") . printValue PrintInFull,
      -- builtins.trace prints a non-string through the same printer with
      -- upstream's default options.
      runTestM "a float prints in full as trace prints it" $
        assertEqual "print-full-float" "1.23457e+06" <$> printValue PrintInFull (VFloat 1234567.0),
      runTestM "a string printed in full reads back as itself" $ do
        result <-
          QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} $
            QC.forAll genEscapable $ \text -> QC.ioProperty $ do
              printed <- printValue PrintInFull (mkStr text)
              pure (evalNix printed QC.=== Right (mkStr text))
        pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))
    ]

-- | Text built from what the string printer escapes (quotes, backslashes,
-- the escaped control characters, a dollar before a brace) among plain and
-- multi-byte characters.  @${@ is a piece of its own so the escape that
-- needs lookahead comes up often.
genEscapable :: QC.Gen Text
genEscapable =
  T.concat <$> QC.listOf (QC.elements ["\"", "\\", "\n", "\r", "\t", "$", "{", "}", "${", "a", " ", "\233", "\128512"])

-- ---------------------------------------------------------------------------
-- Tests: Eval - + and string coercion errors
-- ---------------------------------------------------------------------------

-- | @+@ and string coercion fail with upstream's messages, those of
-- @ExprConcatStrings::eval@ and @coerceToString@ (eval.cc at 2.24.9): the
-- type as @showType@ names it, and after a failed coercion the value as
-- @errorPrintOptions@ prints it.  Each expected message is nix-instantiate
-- 2.33.2's final error line for the same expression.
testCoercionErrors :: IO [Bool]
testCoercionErrors = do
  putStrLn "eval/coercion"
  tmpBase <- getTemporaryDirectory
  let testDir = tmpBase </> "nova-nix-test-coercion"
      setup = createDirectoryIfMissing True (testDir </> "d")
      cleanup = do
        exists <- doesDirectoryExist testDir
        when exists (removeDirectoryRecursive testDir)
      withContext = "\"${builtins.toFile \"a\" \"b\"}\""
      primopMarker = "\x00ABprimop map\x00BB"
      failsIO label source expected = runTestM label (assertEvalErrorIO testDir label source expected)
  bracket_ setup cleanup . sequence $
    [ -- An integer or float on the left makes + arithmetic, and a right
      -- operand that is no number is named.
      runTest "an integer plus a string cannot be added" $
        assertEvalError "add-int-string" "1 + \"a\"" "cannot add a string to an integer",
      runTest "a float plus a string cannot be added" $
        assertEvalError "add-float-string" "1.0 + \"a\"" "cannot add a string to a float",
      runTest "an integer plus null cannot be added" $
        assertEvalError "add-int-null" "1 + null" "cannot add null to an integer",
      failsIO "an integer plus a string with context names the context" ("1 + " <> withContext) "cannot add a string with context to an integer",
      runTest "an integer plus a primop names the primop" $
        assertEvalError "add-int-primop" "1 + builtins.map" "cannot add the built-in function 'map' to an integer",
      runTest "an integer plus a partial application names its primop" $
        assertEvalError
          "add-int-primop-app"
          "1 + builtins.map (x: x)"
          "cannot add the partially applied built-in function 'map' to an integer",
      -- Anything else on the left makes + concatenation, each operand
      -- coerced as interpolation coerces it.
      runTest "a primop on the left does not coerce" $
        assertEvalError "add-primop-int" "builtins.map + 1" ("cannot coerce the built-in function 'map' to a string: " <> primopMarker),
      failsIO "a string with context plus an integer refuses the integer" (withContext <> " + 1") "cannot coerce an integer to a string: 1",
      runTest "a Boolean on the left fails to coerce rather than to add" $
        assertEvalError "add-bool-int" "true + 1" "cannot coerce a Boolean to a string: true",
      runTest "a string plus a float refuses the float" $
        assertEvalError "add-string-float" "\"a\" + 1.5" "cannot coerce a float to a string: 1.5",
      runTest "a string plus a list refuses the list" $
        assertEvalError "add-string-list" "\"a\" + [ ]" "cannot coerce a list to a string: [ ]",
      runTest "a path plus an integer refuses the integer" $
        assertEvalError "add-path-int" "./d + 1" "cannot coerce an integer to a string: 1",
      failsIO
        "a path plus a string with context is refused"
        ("./d + " <> withContext)
        "a string that refers to a store path cannot be appended to a path",
      -- The left operand is dealt with before the right is evaluated.
      runTest "a set on the left fails before the right is evaluated" $
        assertEvalError "add-left-first" "{} + throw \"b\"" "cannot coerce a set to a string: { }",
      runTest "tryEval does not catch a failed coercion" $
        assertEvalError "add-tryEval" "builtins.tryEval ({} + throw \"b\")" "cannot coerce a set to a string: { }",
      -- Upstream copies a path into the store only when the left operand
      -- is a string, so a set's outPath on the left is its own text.
      runTestM "a set on the left contributes its path without a store copy" $ do
        result <- evalNixIO testDir "let s = { outPath = ./d; } + \"x\"; in builtins.getContext s == {} && s == toString ./d + \"x\""
        pure (assertRight "add-set-path" result (assertEqual "add-set-path" (VBool True))),
      -- Interpolation, toString and concatStringsSep coerce the same way.
      runTest "an interpolated set without outPath prints the set" $
        assertEvalError "interp-set" "\"${{ a = 1; b = \"s\"; }}\"" "cannot coerce a set to a string: { a = 1; b = \"s\"; }",
      runTest "an interpolated integer is refused" $
        assertEvalError "interp-int" "\"v${1}\"" "cannot coerce an integer to a string: 1",
      runTest "toString of a primop is refused" $
        assertEvalError "tostring-primop" "builtins.toString builtins.map" ("cannot coerce the built-in function 'map' to a string: " <> primopMarker),
      runTest "toString of a partial application is refused" $
        assertEvalError
          "tostring-primop-app"
          "builtins.toString (builtins.map (x: x))"
          "cannot coerce the partially applied built-in function 'map' to a string: \x00ABpartially applied primop map\x00BB",
      -- Upstream prints a lambda's source position after the word, which
      -- a nova-nix function value does not carry.
      runTest "toString of a lambda is refused" $
        assertEvalError "tostring-lambda" "builtins.toString (x: x)" "cannot coerce a function to a string: \x00ABlambda\x00BB",
      runTest "toString of a set without outPath is refused" $
        assertEvalError "tostring-set" "builtins.toString { }" "cannot coerce a set to a string: { }",
      runTest "concatStringsSep refuses an integer element" $
        assertEvalError "concatsep-int" "builtins.concatStringsSep \",\" [ 1 ]" "cannot coerce an integer to a string: 1",
      -- An interpolated path literal concatenates as + does with a path
      -- first, and refuses a context only after every piece coerced.
      runTest "a path segment refuses a set" $
        assertEvalError "path-segment-set" "./a${{}}" "cannot coerce a set to a string: { }",
      failsIO
        "a later path segment that fails to coerce is reported over an earlier one's context"
        ("./a${" <> withContext <> "}${1}")
        "cannot coerce an integer to a string: 1",
      -- A selection path names the type it cannot index as showType does
      -- (attr-path.cc at 2.24.9).
      runTest "a selection path through a function names it" $
        assertRight "attrpath-functions" (evalNix "{ a = builtins.map; b = builtins.map (x: x); c = x: x; }") $ \root ->
          assertEqual
            "attrpath-functions"
            ( Right
                [ Left "the expression selected by the selection path 'a.x' should be a set but is the built-in function 'map'",
                  Left "the expression selected by the selection path 'b.x' should be a set but is the partially applied built-in function 'map'",
                  Left "the expression selected by the selection path 'c.x' should be a set but is a function"
                ]
            )
            (runPureEval (mapM (\path -> selectAttrPath pure path root) ["a.x", "b.x", "c.x"]))
    ]

-- ---------------------------------------------------------------------------
-- Tests: Eval - Result printer
-- ---------------------------------------------------------------------------

-- | What @nova-nix eval@ prints for a value.  Each expected value is the
-- output of @nix-instantiate --eval --readonly-mode -E@ (Nix 2.33.2) for
-- the same expression, less the newline the command writes after it.
testResultPrinter :: IO [Bool]
testResultPrinter = do
  putStrLn "eval/print-result"
  tmpBase <- getTemporaryDirectory
  let testDir = tmpBase </> "nova-nix-test-print-result"
      notUtf8 = "a" <> BS.singleton 0xfc <> "b"
      setup = do
        createDirectoryIfMissing True testDir
        BS.writeFile (testDir </> "latin1") notUtf8
      cleanup = do
        exists <- doesDirectoryExist testDir
        when exists (removeDirectoryRecursive testDir)
      printsIO label source expected = runTestM label $ do
        result <- evalNixIO testDir source
        assertRightM label result (fmap (assertEqual label expected) . renderResult)
      prints label source expected =
        runTestM label (assertRightM label (evalNix source) (fmap (assertEqual label expected) . renderResult))
      floatPrints (source, expected) =
        prints ("a float prints in ostream form: " <> source) source expected
  bracket_ setup cleanup . sequence $
    [ printsIO
        "a string prints its bytes, not U+FFFD for one that is not UTF-8"
        "builtins.readFile ./latin1"
        ("\"" <> notUtf8 <> "\""),
      prints
        "a string escapes each of upstream's characters and a dollar before a brace"
        "\"\\\" \\\\ \\n \\r \\t \\${ $\""
        "\"\\\" \\\\ \\n \\r \\t \\${ $\"",
      prints "a lone dollar prints as it is" "\"$\"" "\"$\"",
      prints
        "an attribute name past ASCII prints quoted, as its bytes"
        "{ \"\233\" = 1; }"
        ("{ \"" <> BS.pack [0xc3, 0xa9] <> "\" = 1; }"),
      prints
        "an attribute name the lexer cannot read bare prints quoted"
        "{ \"a b\" = 1; \"if\" = 2; \"\" = 3; \"1x\" = 4; or = 5; \"a-b'\" = 6; _x = 7; \"-a\" = 8; \"'a\" = 9; \"x\\\"y\" = 10; }"
        "{ \"\" = 3; \"'a\" = 9; \"-a\" = 8; \"1x\" = 4; _x = 7; \"a b\" = 1; a-b' = 6; \"if\" = 2; or = 5; \"x\\\"y\" = 10; }",
      printsIO
        "nested lists and sets print their strings and names the same way"
        "let v = [ (builtins.readFile ./latin1) { \"b c\" = [ \"$\" \"\\${\" ]; } ]; in builtins.deepSeq v v"
        ("[ \"" <> notUtf8 <> "\" { \"b c\" = [ \"$\" \"\\${\" ]; } ]"),
      prints "an element never forced prints as upstream's thunk marker" "[ (1 + 1) ]" "[ <CODE> ]",
      prints "a lambda prints as upstream's marker" "x: x" "<LAMBDA>",
      prints "a primop prints as upstream's marker" "builtins.map" "<PRIMOP>",
      prints "a partially applied primop prints as upstream's marker" "builtins.map (x: x)" "<PRIMOP-APP>",
      prints "a regex builtin given its pattern is a partial application" "builtins.match \"a\"" "<PRIMOP-APP>",
      runTestM "a printed string reads back as itself" $
        quickCheckPasses $
          QC.forAll genEscapable $ \text -> QC.ioProperty $ do
            printed <- renderResult (mkStr text)
            pure (readsBack printed (mkStr text)),
      runTestM "a printed attribute name reads back as itself" $
        quickCheckPasses $
          QC.forAll genAttrName $ \name -> QC.ioProperty $ do
            one <- evaluated (VInt 1)
            attrs <- VAttrs <$> attrSetFromMap SetLiteral (Map.singleton name one)
            printed <- renderResult attrs
            pure (readsBack ("builtins.head (builtins.attrNames " <> printed <> ")") (mkStr name)),
      runTestM "a name printed as a symbol reads back as itself" $
        quickCheckPasses $
          QC.forAll genAttrName $ \name ->
            readsBack (TE.encodeUtf8 ("builtins.head (builtins.attrNames { " <> printSymbol name <> " = 1; })")) (mkStr name)
    ]
      <> map floatPrints floatCases
  where
    renderResult val = BL.toStrict . BB.toLazyByteString <$> printAmbiguous val
    readsBack printed expected = case TE.decodeUtf8' printed of
      Left _ -> QC.counterexample "the printed bytes are not UTF-8" False
      Right source -> evalNix source QC.=== Right expected
    quickCheckPasses property = do
      result <- QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} property
      pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))
    -- Upstream unary minus is 0 - x, so -0.0 written as a literal is
    -- positive zero there; fromJSON yields a negative one.
    floatCases =
      [ ("1.0", "1"),
        ("1.5", "1.5"),
        ("0.1", "0.1"),
        ("1.0e20", "1e+20"),
        ("1.23456789", "1.23457"),
        ("builtins.fromJSON \"-0.0\"", "-0"),
        ("1234567.0", "1.23457e+06")
      ]

-- | Attribute names built from what decides whether one prints bare: the
-- identifier characters in and out of first place, the reserved keywords
-- and @or@, which is not one, and characters only a quoted name holds.
genAttrName :: QC.Gen Text
genAttrName =
  T.concat <$> QC.listOf (QC.elements ["a", "Z", "_", "0", "'", "-", " ", "\"", "\\", "${", ".", "\233", "if", "in", "inherit", "rec", "or"])

-- ---------------------------------------------------------------------------
-- Tests: Eval - Values reached twice
-- ---------------------------------------------------------------------------

-- | A value reached twice is entered once: upstream's deep force skips a
-- value it has forced (@forceValueDeep@ in eval.cc), and its printers
-- print a set or list they have printed as repeated (@printAmbiguous@ in
-- print-ambiguous.cc, @printAttrs@ and @printList@ in print.cc).  A
-- derivation holds itself through its @all@ and output attributes, so
-- neither ends on one without that.  Each case runs under
-- 'repeatedWatchdogMicros', and a print is read only one byte past what
-- is expected.
testRepeatedValues :: IO [Bool]
testRepeatedValues = do
  putStrLn "eval/repeated"
  tmpBase <- getTemporaryDirectory
  let cutAfter expected val = BL.toStrict . BL.take (fromIntegral (BS.length expected) + 1) . BB.toLazyByteString <$> printAmbiguous val
      printsWith evaluator label source expectedText = runTestM label $ finishesInTime label $ do
        let expected = TE.encodeUtf8 expectedText
        result <- evaluator tmpBase source
        case result of
          Left err -> pure (Fail (label <> ": got error: " <> err))
          Right val -> assertEqual label expected <$> (cutAfter expected val >>= evaluate)
      -- What nova-nix eval prints, and what it prints with --strict.
      prints = printsWith evalNixIO
      printsStrict = printsWith evalStrictIO
      returns label source expected = runTestM label $ finishesInTime label $ do
        result <- evalNixIO tmpBase source
        pure (assertRight label result (assertEqual label expected))
  sequence
    [ prints "nova-nix eval prints a derivation once and its output as repeated" selfHoldingDerivation selfHoldingPrinted,
      printsStrict "nova-nix eval --strict prints a derivation, its outputs in all and out as repeated" selfHoldingDerivation selfHoldingPrintedStrict,
      returns "deepSeq on a derivation returns" ("builtins.deepSeq (" <> selfHoldingDerivation <> ") 1") (VInt 1),
      runTestM "deepSeq on a set that holds itself returns under the pure evaluator" $
        finishesInTime "deepSeq-self-pure" $ do
          result <- evaluate (evalNix "let x = { a = x; }; in builtins.deepSeq x 1")
          pure (assertRight "deepSeq-self-pure" result (assertEqual "deepSeq-self-pure" (VInt 1))),
      -- Identity, not equality, and an empty set or list is never
      -- repeated (printAmbiguous in print-ambiguous.cc).
      printsStrict "a set reached twice prints as repeated the second time" "let a = { x = 1; }; in [ a a ]" ("[ { x = 1; } " <> repeatedMarker <> " ]"),
      -- An update with an empty operand is the other operand's Value,
      -- bindings and all (ExprOpUpdate::eval in eval.cc).
      printsStrict
        "an update with an empty operand prints as repeated after the other operand"
        "let a = { x = 1; }; in [ a (a // { }) ({ } // a) (a // { y = 2; }) ]"
        ("[ { x = 1; } " <> repeatedMarker <> " " <> repeatedMarker <> " { x = 1; y = 2; } ]"),
      -- Upstream's prim_tryEval builds the result's bindings afresh on
      -- each call (primops.cc), a failed one included.
      printsStrict
        "two failed tryEvals are two sets"
        "[ (builtins.tryEval (throw \"a\")) (builtins.tryEval (throw \"b\")) ]"
        "[ { success = false; value = false; } { success = false; value = false; } ]",
      printsStrict "two equal sets made apart both print in full" "let a = { x = 1; }; b = { x = 1; }; in [ a b ]" "[ { x = 1; } { x = 1; } ]",
      printsStrict "an empty set or list reached twice prints in full both times" "let e = { }; l = [ ]; in [ e e l l ]" "[ { } { } [ ] [ ] ]",
      -- Upstream keeps the elements of a list of one or two in the Value
      -- that holds it, and a longer list's in an array its copies share
      -- (listElems in value.hh at 2.24.9), so a function's two results
      -- are two short lists but one long one.
      printsStrict
        "a short list is one per cell and a long one is one however many cells hold it"
        "let f = y: y; s = [ 1 2 ]; l = [ 1 2 3 ]; in [ (f s) (f s) s s (f l) (f l) ]"
        ("[ [ 1 2 ] [ 1 2 ] [ 1 2 ] " <> repeatedMarker <> " [ 1 2 3 ] " <> repeatedMarker <> " ]"),
      -- The result itself is nix-instantiate's own copy: a short list
      -- there is no cell's, and a long one is its array's.
      printsStrict "a short list holding itself prints one level of itself" "let l = [ l ]; in l" ("[ [ " <> repeatedMarker <> " ] ]"),
      printsStrict "a long list holding itself prints as repeated inside" "let l = [ 1 2 l ]; in l" ("[ 1 2 " <> repeatedMarker <> " ]"),
      -- builtins.trace prints a set or list that holds itself to an end.
      runTestM "the trace printer prints a set that holds itself as repeated inside" $
        finishesInTime "trace-self" $ do
          result <- evalNixIO tmpBase "let x = { a = x; }; in builtins.seq x x"
          case result of
            Left err -> pure (Fail ("trace-self: got error: " <> err))
            Right val -> assertEqual "trace-self" ("{ a = " <> repeatedMarker <> "; }") <$> (printValue PrintInFull val >>= evaluate)
    ]

-- ---------------------------------------------------------------------------
-- Tests: Eval - Recursive attribute sets
-- ---------------------------------------------------------------------------

testEvalRecAttrs :: IO [Bool]
testEvalRecAttrs = do
  putStrLn "eval/rec-attrs"
  sequence
    [ runTest "rec self-reference" $
        assertEval "rec-self" "rec { a = 1; b = a + 1; }.b" (VInt 2),
      runTest "rec mutual reference" $
        assertEval "rec-mutual" "rec { a = 1; b = a; }.b" (VInt 1),
      -- A plain inherit inside a fallback (nested-path) rec/let must reference
      -- the OUTER binding, not self-reference into infinite recursion.
      runTest "inherit in fallback rec set references outer" $
        assertEval "inherit-fallback-rec" "let n = 7; in (rec { inherit n; a.b = 1; }).n" (VInt 7),
      runTest "inherit in fallback let references outer" $
        assertEval "inherit-fallback-let" "let x = 5; in let a.b = 1; inherit x; in x" (VInt 5),
      -- Guard the fix: a genuine sibling reference in a fallback rec still resolves.
      runTest "sibling reference survives in fallback rec" $
        assertEval "sibling-fallback-rec" "(rec { a = 1; b = a + 10; c.d = 2; }).b" (VInt 11),
      -- Dynamic rec-set keys evaluate in the rec env (C++ Nix env2): a key may
      -- reference an enclosing var or a static sibling, and must not abort.
      runTest "dynamic key references enclosing var" $
        assertEval "dyn-key-enclosing" "let k = \"x\"; in (rec { p.q = 1; ${k} = 2; }).x" (VInt 2),
      runTest "dynamic key resolves the correct enclosing slot" $
        assertEval "dyn-key-slot" "let a = \"A\"; b = \"B\"; in let c = \"C\"; in (rec { p.q = 1; ${b} = 2; }).B" (VInt 2),
      runTest "dynamic key references a static rec sibling" $
        assertEval "dyn-key-sibling" "(rec { a = \"b\"; ${a} = 1; }).b" (VInt 1)
    ]

-- ---------------------------------------------------------------------------
-- Tests: Eval - frames tied in C
-- ---------------------------------------------------------------------------

-- | A binding of a generated frame: its declaration position and value.
data FrameBinding = FrameBinding
  { fbPos :: !Int,
    fbValue :: !FrameValue,
    -- | The argument a formals set passes for it instead, if any.
    fbPassed :: !(Maybe Int)
  }
  deriving (Show)

-- | A frame binding's value: a constant, another binding of the frame
-- (by declaration position), or that binding plus a constant.
data FrameValue = FrameConstant !Int | FrameAlias !Int | FrameOffset !Int !Int
  deriving (Show)

-- | A graph of C integers and lists for the census to count: a list's
-- child either appears once or fills two slots with one cell.
data CensusTree = CensusLeaf !Int64 | CensusNode ![CensusChild]
  deriving (Show)

data CensusChild = CensusOnce !CensusTree | CensusTwice !CensusTree
  deriving (Show)

-- | Bytes of a thunk cell, a list header and a pointer on a 64-bit
-- target (cbits/nn_thunk.h, cbits/nn_list.h).
censusCellBytes, censusHeaderBytes, censusPointerBytes :: Word64
censusCellBytes = 16
censusHeaderBytes = 16
censusPointerBytes = 8

-- | The bytes of a partial application's header before its argument
-- cells: @sizeof (nn_primop_app_t)@ of @cbits/nn_primop.h@.
primopAppHeaderBytes :: Word64
primopAppHeaderBytes = 8

-- | The bytes of an env's word and of the scopes an env with a name table
-- or with-scopes has: @NN_ENV_WORD_BYTES@ and @NN_ENV_SCOPES_BYTES@ of
-- @cbits/nn_env.h@.  Its slots, and a parent after a header, take a
-- pointer each.
envWordBytes, envScopesBytes :: Word64
envWordBytes = 8
envScopesBytes = 24

-- | The most slots a short env, whose word holds its parent, has:
-- @NN_ENV_SHORT_MAX@ of @cbits/nn_env.h@.
envShortMax :: Int
envShortMax = 7

-- | The bytes of an env of this many slots under a parent, with no
-- scopes: a short one's word holds the parent, and a header is followed
-- by it.
childEnvBytes :: Int -> Word64
childEnvBytes n
  | n >= 1 && n <= envShortMax = envWordBytes + censusPointerBytes * fromIntegral n
  | otherwise = envWordBytes + censusPointerBytes * fromIntegral (n + 1)

-- | Bytes a string of these bytes takes in its block: a 4-byte length,
-- then the bytes, padded to the next multiple of 4 (cbits/nn_str.h).
censusStringBytes :: BS.ByteString -> Word64
censusStringBytes bytes = (4 + fromIntegral (BS.length bytes) + 3) `div` 4 * 4

genCensusTree :: QC.Gen CensusTree
genCensusTree = QC.sized grow
  where
    grow 0 = CensusLeaf <$> QC.arbitrary
    grow n =
      QC.frequency
        [ (1, CensusLeaf <$> QC.arbitrary),
          (3, CensusNode <$> QC.resize 4 (QC.listOf (genChild (n `div` 2))))
        ]
    genChild n = QC.elements [CensusOnce, CensusTwice] <*> grow n

-- | The cell of a fresh graph: every integer and list a cell of its own.
buildCensusTree :: CensusTree -> IO CThunkPtr
buildCensusTree (CensusLeaf n) = cthunkNewComputedInt n
buildCensusTree (CensusNode children) = do
  slots <- concat <$> mapM buildChild children
  list <- thunkListToCList slots
  cthunkNewComputedList (castPtr list)
  where
    buildChild (CensusOnce tree) = pure <$> buildCensusTree tree
    buildChild (CensusTwice tree) = (\cell -> [cell, cell]) <$> buildCensusTree tree

-- | What a census reaches from a graph's cell: a thunk per node, and per
-- list with elements a header and an items array of a slot per element.
censusTreeReach :: CensusTree -> PerKind Figures
censusTreeReach tree =
  (pure (Figures (Just 0) 0))
    { kindThunks = Figures (Just nodes) (nodes * censusCellBytes),
      kindLists = Figures (Just lists) (lists * censusHeaderBytes + slots * censusPointerBytes)
    }
  where
    (nodes, lists, slots) = shape tree
    shape (CensusLeaf _) = (1, 0, 0)
    shape (CensusNode []) = (1, 0, 0)
    shape (CensusNode children) =
      let below = map (shape . childTree) children
          (n, l, s) = foldl' (\(a, b, c) (x, y, z) -> (a + x, b + y, c + z)) (0, 0, 0) below
       in (n + 1, l + 1, s + sum (map childSlots children))
    childTree (CensusOnce t) = t
    childTree (CensusTwice t) = t
    childSlots (CensusOnce _) = 1
    childSlots (CensusTwice _) = 2

-- | A census, or the failure the C side reported as text.
censusOf :: [CRoot] -> IO (Either Text Census)
censusOf roots = either (Left . T.pack . displayException) Right <$> takeCensus roots

-- | A check of a census, or a failure naming why there is none.
withCensus :: [CRoot] -> (Census -> TestResult) -> IO TestResult
withCensus roots check = either (Fail . ("no census: " <>)) check <$> censusOf roots

testCensus :: IO [Bool]
testCensus = do
  putStrLn "eval/census"
  tmpBase <- getTemporaryDirectory
  ambient <- getEnvironment
  sequence
    [ runTestM "a census counts each object a graph reaches once, through every slot that holds it" $
        quickCheckPasses $
          QC.forAll genCensusTree $ \tree -> QC.ioProperty $ do
            root <- buildCensusTree tree
            taken <- censusOf [RootThunk (Thunk root)]
            pure $ case taken of
              Left failure -> QC.counterexample (T.unpack failure) False
              Right census ->
                (censusReached census, censusUnplaced census, censusVisitedObjects census, censusVisitedBytes census)
                  QC.=== let reach = censusTreeReach tree
                          in (reach, 0, sum (fmap (fromMaybe 0 . figuresCount) reach) + lists reach, sum (fmap figuresBytes reach)),
      -- Layers share a base or sit one on another, and a list holds every
      -- layer: the census reaches each set once, sizes a layer by the
      -- entries its own arrays hold, and reaches each value once.
      runTestM "a census counts a base its layers share once, and a layer by its own entries" $ do
        pool <- mapM (\i -> symbolIntern ("census_layer_" <> T.pack (show (i :: Int)))) [1 .. 30]
        let setOf names = do
              set <- cattrsetNew SetLiteral (length names)
              forM_ names $ \name -> cthunkNewComputedInt 0 >>= cattrsetInsert set name noPos
              cattrsetFreeze set
              pure set
            -- A right operand's names start halfway into the base's, so
            -- some of them shadow it, and 16 of them are left in the pool.
            rightNames baseSize size = take size (drop (min (baseSize `div` 2) (length pool - setLayerRhsMax)) pool)
            gen = (,) <$> QC.chooseInt (1, length pool) <*> (QC.chooseInt (1, setMaxLayers - 2) >>= \n -> QC.vectorOf n ((,) <$> QC.chooseInt (1, setLayerRhsMax) <*> QC.arbitrary))
        quickCheckPasses $
          QC.forAll gen $ \(baseSize, shapes) -> QC.ioProperty $ do
            base <- setOf (take baseSize pool)
            (_, layers) <-
              foldM
                ( \(below, made) (size, onBelow) -> do
                    right <- setOf (rightNames baseSize size)
                    layer <- cattrsetUpdate SetUpdate (if onBelow then below else base) right
                    pure (layer, layer : made)
                )
                (base, [])
                shapes
            cells <- mapM (cthunkNewComputedAttrs . castPtr) layers
            list <- thunkListToCList cells
            root <- cthunkNewComputedList (castPtr list)
            taken <- censusOf [RootThunk (Thunk root)]
            let sizes = map fst shapes
                setCount = 1 + length layers
                cellCount = 1 + length layers + baseSize + sum sizes
            pure $ case taken of
              Left failure -> QC.counterexample (T.unpack failure) False
              Right census ->
                let reached = censusReached census
                 in (kindAttrSets reached, kindThunks reached, kindLists reached, censusSetSizes census)
                      QC.=== ( Figures (Just (fromIntegral setCount)) (setBytes baseSize + sum (map setBytes sizes)),
                               Figures (Just (fromIntegral cellCount)) (fromIntegral cellCount * censusCellBytes),
                               Figures (Just 1) (censusHeaderBytes + fromIntegral (length layers) * censusPointerBytes),
                               Map.fromListWith (+) [(fromIntegral n, 1) | n <- baseSize : sizes]
                             ),
      runTestM "a census taken twice over the same roots counts the same, so the mark leaves the data layer as it found it" $
        quickCheckPasses $
          QC.forAll genCensusTree $ \tree -> QC.ioProperty $ do
            root <- buildCensusTree tree
            first <- censusOf [RootThunk (Thunk root)]
            second <- censusOf [RootThunk (Thunk root)]
            let figures c = (censusAllocated c, censusReached c, censusVisitedObjects c, censusSetSizes c)
            pure (fmap figures first QC.=== fmap figures second),
      -- The registry only grows, so what it reaches before and after one
      -- registration differs by what that root alone holds.
      -- Under the sanitizers' build a read of memory the first teardown
      -- freed fails the child, and without them it reads what the second
      -- window put there, so the two results differ.
      runTestM "an evaluation after the arena is torn down and set up again reads nothing the teardown freed" $ do
        self <- getExecutablePath
        (_, out, err) <- Proc.readCreateProcessWithExitCode (Proc.proc self [arenaCycleChildFlag]) ""
        pure $ case lines out of
          [first, second]
            | not ("error: " `isPrefixOf` first) -> assertEqual "second window's result" first second
          _ -> Fail ("child printed " <> T.pack (show out) <> " and " <> T.pack err),
      runTestM "a census from the registered roots reaches a cell once it is registered" $ do
        cell <- cthunkNewComputedInt 7
        before <- censusOf [RootRegistered]
        crootsAddThunk cell
        after <- censusOf [RootRegistered]
        let thunks = fmap (figuresCount . kindThunks . censusReached)
        pure (assertEqual "thunks reached" (fmap (fmap (+ 1)) (thunks before)) (thunks after)),
      runTestM "a nested session forgets the roots it registered as it ends" $ do
        before <- censusOf [RootRegistered]
        inTestSession (pure ())
        after <- censusOf [RootRegistered]
        pure (assertEqual "reached from the registered roots" (fmap censusReached before) (fmap censusReached after)),
      runTestM "a session registers the value an import caches, which a census from the registered roots reaches" $ do
        storeDir <- evalNixIOStoreDir
        st <- testEvalState storeDir tmpBase
        before <- censusOf [RootRegistered]
        imported <- evalNixIOIn st tmpBase "import (builtins.toFile \"cached.nix\" \"{ a = 1; b = 2; }\")"
        after <- censusOf [RootRegistered]
        let sets = fmap (figuresCount . kindAttrSets . censusReached)
        pure $ case imported of
          Left failure -> Fail failure
          Right _ -> assertEqual "sets reached" (fmap (fmap (+ 1)) (sets before)) (sets after),
      runTestM "a census follows a partial application in C to the cells it holds" $ do
        inner <- cthunkNewComputedInt 7
        held <- evaluated (VBuiltin Builtin.Map [ArgDeferred (Thunk inner)])
        withCensus [RootThunk held] $ \census ->
          assertEqual "thunks" (Figures (Just 2) (2 * censusCellBytes)) (kindThunks (censusReached census))
            `andThen` assertEqual "partial applications" (Figures (Just 1) (primopAppHeaderBytes + censusPointerBytes)) (kindPrimopApps (censusReached census))
            `andThen` assertEqual "unplaced" 0 (censusUnplaced census),
      runTestM "a census reaches a builtin's own cell without counting it, as it does the null cell" $ do
        own <- cthunkPrimop (builtinIndex Builtin.Map)
        withCensus [RootThunk (Thunk own)] $ \census ->
          assertEqual "thunks" (Figures (Just 0) 0) (kindThunks (censusReached census))
            `andThen` assertEqual "unplaced" 0 (censusUnplaced census),
      runTestM "a partial application holds its deferred argument as a cell, which a census follows without making one" $ do
        result <- evalNixIO tmpBase "let f = map (x: x); in builtins.seq f { inherit f; }"
        case result of
          Left failure -> pure (Fail failure)
          Right value -> withCensus (valueRoots value) $ \census ->
            assertEqual "unplaced" 0 (censusUnplaced census)
              `andThen` assertEqual "partial applications reached" True (figuresCount (kindPrimopApps (censusReached census)) >= Just 1),
      runTestM "a census follows a pending thunk to its env and the env's slots" $ do
        cells <- mapM cthunkNewComputedInt [1, 2, 3]
        env <- newEnv EnvClosure Nothing (map Thunk cells)
        code <- compileExpr (ELit (NixInt 0))
        pending <- mkThunkBc env code
        withCensus [RootThunk pending] $ \census ->
          assertEqual "thunks" (Figures (Just 4) (4 * censusCellBytes)) (kindThunks (censusReached census))
            `andThen` assertEqual "envs" (Figures (Just 1) (envWordBytes + 3 * censusPointerBytes)) (kindEnvs (censusReached census))
            `andThen` assertEqual "unplaced" 0 (censusUnplaced census),
      -- A deferred application's cells are a pointer each beside no env,
      -- and a blackholed one keeps them for the force that restores it.
      runTestM "a census follows a deferred application to its cells, a pointer each, pending or blackholed" $ do
        fn <- Thunk <$> cthunkNewComputedInt 1
        first <- Thunk <$> cthunkNewComputedInt 2
        second <- Thunk <$> cthunkNewComputedInt 3
        one <- mkApplyThunk fn first
        two <- mkApply2Thunk fn first second
        blackholed <- mkApplyThunk second first
        _ <- cthunkMarkBlackhole (unThunk blackholed)
        withCensus [RootThunk one, RootThunk two, RootThunk blackholed] $ \census ->
          assertEqual "thunks" (Figures (Just 6) (6 * censusCellBytes)) (kindThunks (censusReached census))
            `andThen` assertEqual "applications" (Figures (Just 3) ((2 + 3 + 2) * censusPointerBytes)) (kindApps (censusReached census))
            `andThen` assertEqual "envs" (Figures (Just 0) 0) (kindEnvs (censusReached census))
            `andThen` assertEqual "unplaced" 0 (censusUnplaced census),
      runTestM "a deferred application reads back as the cells it was made of, and forcing it applies the function to them" $ do
        fn <- evaluated (VBuiltin Builtin.Add [])
        first <- Thunk <$> cthunkNewComputedInt 2
        second <- Thunk <$> cthunkNewComputedInt 3
        one <- mkApplyThunk fn first
        two <- mkApply2Thunk fn first second
        readOne <- readPending (unThunk one)
        readTwo <- readPending (unThunk two)
        let cellsOf pending = case pending of
              PendingApply f a -> map unThunk [f, a]
              PendingApply2 f a b -> map unThunk [f, a, b]
              PendingCode _ _ -> []
        pure $
          assertEqual "cells" (map unThunk [fn, first], map unThunk [fn, first, second]) (cellsOf readOne, cellsOf readTwo)
            `andThen` assertEqual "applied" (Right (VInt 5)) (runPureEval (force two)),
      runTestM "a list and its suffix count one items array, whichever is reached first" $ do
        cells <- mapM cthunkNewComputedInt [1, 2, 3, 4]
        whole <- CList <$> thunkListToCList cells
        suffix <- clistDrop 2 whole
        let expected = (Figures (Just 2) (2 * censusHeaderBytes + 4 * censusPointerBytes), Figures (Just 4) (4 * censusCellBytes), 0)
            listsAndCells c = (kindLists (censusReached c), kindThunks (censusReached c), censusUnplaced c)
        suffixFirst <- censusOf [RootList suffix, RootList whole]
        wholeFirst <- censusOf [RootList whole, RootList suffix]
        pure (assertEqual "suffix first" (Right expected) (listsAndCells <$> suffixFirst) `andThen` assertEqual "whole first" (Right expected) (listsAndCells <$> wholeFirst)),
      runTestM "a census reaches a set's names as symbols and its string values as strings" $ do
        value <- evaluated (mkStr "nova-nix census value")
        set <- attrSetFromMap SetLiteral (Map.fromList [("novaNixCensusName", value)])
        withCensus [RootAttrSet set] $ \census ->
          assertEqual
            "sets, thunks, strings and symbols"
            (Just 1, Just 1, Figures (Just 1) (censusStringBytes "nova-nix census value"), Figures (Just 1) (fromIntegral (T.length "novaNixCensusName" + 1)))
            (figuresCount (kindAttrSets (censusReached census)), figuresCount (kindThunks (censusReached census)), kindStrings (censusReached census), kindSymbols (censusReached census))
            `andThen` assertEqual "set sizes" (Map.fromList [(1, 1)]) (censusSetSizes census)
            `andThen` assertEqual "unplaced" 0 (censusUnplaced census),
      runTestM "a census places the shared empty set as a static, counting it nowhere" $ do
        cell <- evaluated (VAttrs emptyAttrSet)
        withCensus [RootThunk cell, RootAttrSet emptyAttrSet] $ \census ->
          assertEqual "sets and cells" (Just 0, Just 1) (figuresCount (kindAttrSets (censusReached census)), figuresCount (kindThunks (censusReached census)))
            `andThen` assertEqual "set sizes" Map.empty (censusSetSizes census)
            `andThen` assertEqual "unplaced" 0 (censusUnplaced census),
      runTestM "a census reaches a context string's text, store path and output as strings" $ do
        let element = SCDrvOutput (StorePath censusStoreHash "zname") "zout"
        cell <- evaluated (VStr "ztext" (StringContext (Set.singleton element)))
        withCensus [RootThunk cell] $ \census ->
          assertEqual "context strings" (Just 1) (figuresCount (kindCtxStrs (censusReached census)))
            `andThen` assertEqual
              "strings"
              (Figures (Just 4) (sum (map censusStringBytes ["ztext", TE.encodeUtf8 censusStoreHash, "zname", "zout"])))
              (kindStrings (censusReached census))
            `andThen` assertEqual "symbols" (Figures (Just 0) 0) (kindSymbols (censusReached census))
            `andThen` assertEqual "unplaced" 0 (censusUnplaced census),
      runTestM "a census places a literal's bytes in the code and the empty string, counting neither" $ do
        literal <- compileExpr (EStr [StrLit "zcensus literal"]) >>= cheapThunkBc emptyEnv
        empty <- evaluated (mkStr "")
        withCensus [RootThunk literal, RootThunk empty] $ \census ->
          assertEqual "thunks" (Just 2) (figuresCount (kindThunks (censusReached census)))
            `andThen` assertEqual "strings" (Figures (Just 0) 0) (kindStrings (censusReached census))
            `andThen` assertEqual "unplaced" 0 (censusUnplaced census),
      runTestM "a census of the builtins reaches no more than is allocated and places every pointer it meets" $ do
        env <- builtinEnv unrestrictedPolicy 0 []
        withCensus [RootEnv env] $ \census ->
          let within (Figures allocatedCount allocatedBytes) (Figures reachedCount reachedBytes) =
                reachedBytes <= allocatedBytes && maybe True (>= fromMaybe 0 reachedCount) allocatedCount
           in assertEqual "every kind within" (pure True) (within <$> censusAllocated census <*> censusReached census)
                `andThen` assertEqual "unplaced" 0 (censusUnplaced census)
                `andThen` assertEqual "some builtins reached" True (fromMaybe 0 (figuresCount (kindThunks (censusReached census))) > 0),
      runTest "a census renders as a name and a value per line, with no line for a count no allocator keeps" $
        assertEqual "report" renderedSample (renderCensus sampleCensus),
      runTest "a census counting by site renders the envs' slots and sizes, then three figures for each site, then the entries // copied, after the rest" $
        let rendered = BS8.lines (renderCensus sampleCensus {censusSites = Just sampleSites})
            before = BS8.lines renderedSample
            sites = drop (length before) rendered
            siteCount = length [minBound .. maxBound :: EnvSite] + length [minBound .. maxBound :: SetSite]
         in assertEqual "the lines without sites" before (take (length before) rendered)
              `andThen` assertEqual "slots and sizes" ["envs.allocated.slots 6", "envs.allocated.size 2 1", "envs.allocated.size 32+ 1"] (take 3 sites)
              `andThen` assertEqual "figures" (3 * siteCount) (length sites - 4)
              `andThen` assertEqual "the entries // copied, last" ["attrsets.update.copied 5"] (drop (length sites - 1) sites)
              `andThen` assertEqual
                "a site's figures"
                []
                ( filter
                    (`notElem` sites)
                    [ "envs.site.let.count 2",
                      "envs.site.let.slots 6",
                      "envs.site.let.bytes 192",
                      "attrsets.site.update.count 3",
                      "attrsets.site.update.shareable 2",
                      "attrsets.site.update.bytes 184"
                    ]
                ),
      runTestM "eval writes a census to the file NOVA_NIX_C_STATS names, and none when it is empty" $ do
        let root = tmpBase </> "nova-nix-test-census"
            target = root </> "census.txt"
            run value =
              Proc.readCreateProcessWithExitCode
                (Proc.proc "cabal" ["run", "-v0", "exe:nova-nix", "--", "eval", "--strict", "--expr", "builtins.genList (i: { inherit i; }) 3"])
                  { Proc.env = Just (("NOVA_NIX_C_STATS", value) : filter ((/= "NOVA_NIX_C_STATS") . fst) ambient)
                  }
                ""
        bracket_ (forceRemoveIfExists root >> createDirectoryIfMissing True root) (forceRemoveIfExists root) $ do
          (code, out, _) <- run target
          written <- BS8.lines <$> BS.readFile target
          Dir.removeFile target
          (emptyCode, emptyOut, _) <- run ""
          leftOver <- Dir.doesFileExist target
          -- The result's three sets of one attribute are reached; the
          -- builtins' sets are bigger.
          let oneEntrySets = [count | line <- written, ["attrsets.reached.size", "1", count] <- [BS8.words line]]
              pairs = [(key, n) | [key, value] <- map BS8.words written, Just n <- [wholeNumber value]]
              named name = [n | (key, n) <- pairs, key == name]
              siteSum kind figure = [sum [n | (key, n) <- pairs, [k, "site", _, f] <- [BS8.split '.' key], k == kind, f == figure]]
              sizeSum = [sum [n | ["envs.allocated.size", _, value] <- map BS8.words written, Just n <- [wholeNumber value]]]
          pure $
            assertEqual "printed" (ExitSuccess, "[ { i = 0; } { i = 1; } { i = 2; } ]\n") (code, out)
              `andThen` assertEqual "one-attribute sets reached" ["3"] oneEntrySets
              `andThen` assertEqual "unplaced" True ("mark.unplaced 0" `elem` written)
              `andThen` assertEqual "with an empty value" (ExitSuccess, out, False) (emptyCode, emptyOut, leftOver)
              -- Counting began with the arena, so the envs have a count,
              -- and the sites account for every env and set the
              -- allocators hold: the env pages' bytes, less the lists'
              -- items, and the tracked sets.
              `andThen` assertEqual "envs counted by site" (siteSum "envs" "count") (named "envs.allocated.count")
              `andThen` assertEqual "envs counted by size" sizeSum (named "envs.allocated.count")
              `andThen` assertEqual "env bytes by site" (siteSum "envs" "bytes") (named "envs.allocated.bytes")
              `andThen` assertEqual "env slots by site" (siteSum "envs" "slots") (named "envs.allocated.slots")
              `andThen` assertEqual "sets counted by site" (siteSum "attrsets" "count") (named "attrsets.allocated.count")
              `andThen` assertEqual "set bytes by site" (siteSum "attrsets" "bytes") (named "attrsets.allocated.bytes")
              -- genList defers one application per element, cells of
              -- its own and no env, each of which calls the lambda,
              -- which makes one set.
              `andThen` assertEqual "genList's applications" [3] (named "apps.allocated.count")
              `andThen` assertEqual "the lambda's calls" [3] (named "envs.site.call.count")
              `andThen` assertEqual "the literal sets" [3] (named "attrsets.site.literal.count")
    ]
  where
    wholeNumber value = case BS8.readInteger value of
      Just (n, rest) | BS.null rest -> Just n
      _ -> Nothing
    lists reach = fromMaybe 0 (figuresCount (kindLists reach))
    quickCheckPasses property = do
      result <- QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} property
      pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))
    sampleCensus =
      Census
        { censusAllocated = (pure (Figures (Just 3) 48)) {kindEnvs = Figures Nothing 96},
          censusReached = pure (Figures (Just 1) 16),
          censusOverheadBytes = 5,
          censusCodeBytes = 6,
          censusVisitedObjects = 9,
          censusVisitedBytes = 10,
          censusUnplaced = 0,
          censusIndexSeconds = 0.25,
          censusMarkSeconds = 1.5,
          censusSetSizes = Map.fromList [(0, 2), (12, 1)],
          censusSites = Nothing
        }
    sampleSites =
      SiteCounts
        { sitesEnvs = Map.insert EnvLet (EnvSiteFigures 2 6 192) (Map.fromList [(site, EnvSiteFigures 0 0 0) | site <- [minBound .. maxBound]]),
          sitesEnvSizes = Map.fromList [(EnvSlots 2, 1), (EnvSlotsAtLeast 32, 1)],
          sitesSets = Map.insert SetUpdate (SetSiteFigures 3 2 184) (Map.fromList [(site, SetSiteFigures 0 0 0) | site <- [minBound .. maxBound]]),
          sitesUpdateCopied = 5
        }
    renderedSample =
      BS8.unlines
        ( concat
            [ [name <> ".allocated.count 3" | name /= "envs"]
                ++ [name <> ".allocated.bytes " <> (if name == "envs" then "96" else "48"), name <> ".reached.count 1", name <> ".reached.bytes 16"]
            | name <- ["thunks", "envs", "attrsets", "lists", "lambdas", "primopapps", "apps", "ctxstrs", "strings", "symbols"]
            ]
            ++ [ "overhead.bytes 5",
                 "code.bytes 6",
                 "mark.objects 9",
                 "mark.bytes 10",
                 "mark.unplaced 0",
                 "mark.index-seconds 0.250000",
                 "mark.seconds 1.500000",
                 "attrsets.reached.size 0 2",
                 "attrsets.reached.size 12 1"
               ]
        )

-- | An env at a site with a number of slots, as a site allocates one.
data EnvAllocation = EnvAllocation !EnvSite !Int
  deriving (Show)

-- | A set at a site with a number of entries.
data SetAllocation = SetAllocation !SetSite !Int
  deriving (Show)

-- | Bytes of a set header and a set entry (its key, position and value)
-- on a 64-bit target (cbits/nn_attrset.c).  An env's are given by
-- 'childEnvBytes'.
setHeaderSize, setEntrySize :: Word64
setHeaderSize = 40
setEntrySize = 16

-- | Bytes of a set with room for this many entries.
setBytes :: Int -> Word64
setBytes room = setHeaderSize + fromIntegral room * setEntrySize

-- | The room @nn_attrset_new@ gives a set asked for none
-- (cbits/nn_attrset.c).
setDefaultRoom :: Int
setDefaultRoom = 8

-- | A source that forces a chain of updates, each on the one before it:
-- the left operand, then each right operand in turn.
updateSource :: Text -> [Text] -> Text
updateSource left rights = "builtins.deepSeq (" <> foldl' (\chain right -> "(" <> chain <> " // " <> right <> ")") left rights <> ") null"

-- | A set literal binding @b1@ to @bN@.
rightOf :: Int -> Text
rightOf n = "{ " <> T.concat ["b" <> T.pack (show i) <> " = 1; " | i <- [1 .. n]] <> "}"

-- | Sets of one attribute each, every one binding a name of its own.
singleRights :: Int -> [Text]
singleRights n = ["{ c" <> T.pack (show i) <> " = 1; }" | i <- [1 .. n]]

-- | What evaluating a source adds to the counts by site.  The IO
-- evaluator runs it, as @nova-nix eval@ does: the pure one evaluates a
-- thunk again each time it is forced, and so allocates again too.
countsOfEval :: FilePath -> Text -> IO (Either Text SiteCounts)
countsOfEval baseDir source = do
  outcome <- newIORef (Right ())
  added <- sitesAdded (evalNixIO baseDir source >>= writeIORef outcome . void)
  failure <- readIORef outcome
  pure (failure >> added)

-- | What evaluating a source adds at each site but the root's, whose
-- environment and sets each evaluation builds, under the IO evaluator as
-- 'countsOfEval' runs it.
sitesOfEval :: FilePath -> Text -> IO (Either Text (Map.Map EnvSite EnvSiteFigures, Map.Map SetSite SetSiteFigures))
sitesOfEval baseDir source =
  fmap (\counts -> (Map.delete EnvRoot (sitesEnvs counts), Map.delete SetRoot (sitesSets counts))) <$> countsOfEval baseDir source

-- | The deferred applications evaluating a source allocates, by the
-- census's count before and after, under the IO evaluator as
-- 'countsOfEval' runs it.
appsOfEval :: FilePath -> Text -> IO (Either Text Word64)
appsOfEval baseDir source = do
  before <- censusOf []
  outcome <- evalNixIO baseDir source
  after <- censusOf []
  let count census = fromMaybe 0 (figuresCount (kindApps (censusAllocated census)))
  pure (outcome >> ((-) <$> fmap count after <*> fmap count before))

-- | The region collector ("Nix.Eval.CRegion"), which the suite's own
-- session runs.
testRegionCollector :: IO [Bool]
testRegionCollector = do
  putStrLn "eval/region-collector"
  sequence
    [ leavesNoMore "the collector frees the sets a force made once nothing reaches them, so forcing more of them leaves no more behind" id,
      -- Each force makes a set of a thousand names and a layer on it,
      -- whose own force ends with the layer alone holding the set, and
      -- reads the set's names through the layer.
      leavesNoMoreOf "the collector frees a set and the layer on it once nothing reaches them, so forcing more of them leaves no more behind" layeredDropped (\n -> n * (n - 1) + 1001 * n),
      -- One force around them all: the forces inside it close inside a
      -- region, tenuring what they mark, and it marks its region whole as
      -- they pass the budget.
      leavesNoMore "and so does a force whose region holds every one of them" nested,
      takesNoMore "the collector hands out again the lines a force's cells, envs and strings took once nothing reaches them, so most of what forcing more of them allocates comes from lines it freed" id,
      takesNoMore "and so does a force whose region holds every one of them" nested,
      losesNoLine "a line that sibling forces share is freed once both have freed their part of it" id,
      losesNoLine "and so is one inside a force whose region holds every one of them" nested,
      holdsFewerSpans "under one force around them all, the span table drops the spans that hold no line, so it holds fewer at once than the forces begin"
    ]
  where
    leavesNoMore name wrap = leavesNoMoreOf name (wrap . dropped) (* 1000)
    leavesNoMoreOf name sourceOf answerOf =
      runTestM name $ do
        tmpBase <- getTemporaryDirectory
        few <- setsOfEval tmpBase (sourceOf 20)
        many <- setsOfEval tmpBase (sourceOf 200)
        pure $ case (few, many) of
          (Right (VInt answerFew, madeFew, leftFew), Right (VInt answerMany, madeMany, leftMany)) ->
            assertEqual "each force read its thousand names" (answerOf 20, answerOf 200) (answerFew, answerMany)
              `andThen` assertEqual "the forces made a thousand sets each" True (madeMany - madeFew >= 180 * 1000)
              `andThen` assertEqual "as many left after 200 forces as after 20" leftFew leftMany
          _ -> Fail ("no census or no count: " <> T.pack (show (fmap third few, fmap third many)))
    third (_, _, left) = left
    -- The blocks grow by less than half of what 200 forces allocate, where
    -- without reuse they grow by nearly all of it.  They need not stay
    -- exactly as they were: a refill that finds no run long enough among
    -- the blocks it looks at makes a new one, and which runs are free
    -- depends on what the suite's earlier evaluations left.  The first 20
    -- forces take their lines before the measure starts.
    takesNoMore name wrap =
      runTestM name $ do
        tmpBase <- getTemporaryDirectory
        warmed <- evalNixIO tmpBase (wrap (dropped 20))
        (reservedBefore, allocatedBefore) <- bumpBytes
        answer <- evalNixIO tmpBase (wrap (dropped 200))
        (reservedAfter, allocatedAfter) <- bumpBytes
        let grown = reservedAfter - reservedBefore
            allocated = allocatedAfter - allocatedBefore
        pure $
          assertEqual "each force counted its thousand names" (Right (Just 20000), Right (Just 200000)) (countOf warmed, countOf answer)
            `andThen` if 2 * grown < allocated
              then Pass
              else Fail ("the blocks grew by " <> T.pack (show grown) <> " bytes while the forces allocated " <> T.pack (show allocated))
    losesNoLine name wrap =
      runTestM name $ do
        tmpBase <- getTemporaryDirectory
        answer <- evalNixIO tmpBase (wrap (siblings 200))
        lost <- c_nn_region_lines_lost
        pure $
          assertEqual "each force counted its cells" (Right (Just (200 * siblingCells))) (countOf answer)
            `andThen` assertEqual "lines whose every unit is freed but that are not free" 0 lost
    -- Without the drop, the table holds every span begun since the last
    -- close outside every region, so the peak the force's own close ends
    -- (nn_region_spans_most) nears what the forces began.
    holdsFewerSpans name =
      runTestM name $ do
        tmpBase <- getTemporaryDirectory
        before <- c_nn_region_spans_begun
        answer <- evalNixIO tmpBase (nested (siblings manySiblings))
        begun <- subtract before <$> c_nn_region_spans_begun
        most <- c_nn_region_spans_most
        pure $
          assertEqual "each force counted its cells" (Right (Just (fromIntegral manySiblings * siblingCells))) (countOf answer)
            `andThen` if 2 * most < begun
              then Pass
              else Fail ("the table held " <> T.pack (show most) <> " spans at once, and the forces began " <> T.pack (show begun))
    -- Enough forces to begin spans many times the table's floor
    -- (NN_REGION_SPAN_FLOOR in cbits/nn_region.c).
    manySiblings :: Int
    manySiblings = 20000
    countOf = fmap (\case VInt n -> Just n; _ -> Nothing)
    nested source = "let total = " <> source <> "; in total"
    -- Forces one after another, each of a list of siblingCells cells it
    -- drops but for their count: past the collector's threshold, and an
    -- odd count of cells, so that most forces start partway into a line
    -- the one before them freed its part of.
    siblings :: Int -> Text
    siblings n =
      "builtins.foldl' (a: b: a + b) 0 (builtins.genList (i: builtins.length (builtins.genList (j: j + i) "
        <> T.pack (show siblingCells)
        <> ")) "
        <> T.pack (show n)
        <> ")"
    siblingCells :: Int64
    siblingCells = 1025
    -- Each element of the map is a force that makes a set of a thousand
    -- names, each from a set of its own, and returns only a count.
    layeredDropped :: Int64 -> Text
    layeredDropped n =
      "let mk = i: let layered = (let base = builtins.listToAttrs "
        <> "(builtins.genList (j: { name = \"a${toString j}\"; value = i; }) 1000); in base // { extra = i; }); "
        <> "in layered.a7 + layered.extra + builtins.length (builtins.attrNames layered); "
        <> "in builtins.foldl' (a: b: a + b) 0 (map mk (builtins.genList (i: i) "
        <> T.pack (show n)
        <> "))"
    dropped :: Int64 -> Text
    dropped n =
      "let mk = i: builtins.length (builtins.attrNames (builtins.listToAttrs "
        <> "(builtins.genList (j: { name = \"a${toString j}\"; value = i; }) 1000))); "
        <> "in builtins.foldl' (a: b: a + b) 0 (map mk (builtins.genList (i: i) "
        <> T.pack (show n)
        <> "))"

-- | The bytes the thunk arena's blocks, the env pages and the value
-- strings' blocks hold from the allocator, and the bytes their objects
-- took as they were made, which reused lines put where others were.
bumpBytes :: IO (Word64, Word64)
bumpBytes = do
  values <- c_nn_str_values
  reserved <-
    (\thunks envs strings -> thunks + envs + strings)
      <$> c_nn_thunk_reserved_bytes
      <*> c_nn_env_reserved_bytes
      <*> c_nn_str_arena_reserved_bytes values
  allocated <-
    (\cells envs strings -> cells * censusCellBytes + envs + strings)
      <$> c_nn_thunk_cells_allocated
      <*> c_nn_env_bytes_allocated
      <*> c_nn_str_arena_bytes values
  pure (reserved, allocated)

-- | What the bump allocators hold and have handed out, for the collector's
-- test of reuse: no census reads it, as a census counts objects, which
-- reuse puts where others were.
foreign import ccall unsafe "nn_thunk_reserved_bytes"
  c_nn_thunk_reserved_bytes :: IO Word64

foreign import ccall unsafe "nn_thunk_cells_allocated"
  c_nn_thunk_cells_allocated :: IO Word64

foreign import ccall unsafe "nn_env_reserved_bytes"
  c_nn_env_reserved_bytes :: IO Word64

foreign import ccall unsafe "nn_env_bytes_allocated"
  c_nn_env_bytes_allocated :: IO Word64

foreign import ccall unsafe "nn_str_values"
  c_nn_str_values :: IO (Ptr ())

foreign import ccall unsafe "nn_str_arena_reserved_bytes"
  c_nn_str_arena_reserved_bytes :: Ptr () -> IO Word64

foreign import ccall unsafe "nn_str_arena_bytes"
  c_nn_str_arena_bytes :: Ptr () -> IO Word64

-- | The collector's lines left all freed but not free (nn_region.h).
foreign import ccall unsafe "nn_region_lines_lost"
  c_nn_region_lines_lost :: IO Word64

-- | The spans begun since the regions began, and the most the span table
-- held at once between the last two closes outside every region
-- (nn_region.h).
foreign import ccall unsafe "nn_region_spans_begun"
  c_nn_region_spans_begun :: IO Word32

foreign import ccall unsafe "nn_region_spans_most"
  c_nn_region_spans_most :: IO Word32

-- | What evaluating a source answered, the sets it made, by the sites'
-- counts, and the sets it left allocated, by the census's before and
-- after.
setsOfEval :: FilePath -> Text -> IO (Either Text (NixValue, Integer, Integer))
setsOfEval baseDir source = do
  before <- censusOf []
  outcome <- newIORef (Left "the evaluation did not run")
  added <- sitesAdded (evalNixIO baseDir source >>= writeIORef outcome)
  after <- censusOf []
  answer <- readIORef outcome
  let count census = toInteger (fromMaybe 0 (figuresCount (kindAttrSets (censusAllocated census))))
      made counts = sum (map (toInteger . setSiteSets) (Map.elems (sitesSets counts)))
  pure ((,,) <$> answer <*> fmap made added <*> ((-) <$> fmap count after <*> fmap count before))

-- | The census of regions ("Nix.Eval.CRegion"), run by @nova-nix eval@ in
-- a process of its own: the census starts before its arena does.
testRegionCensus :: IO [Bool]
testRegionCensus = do
  putStrLn "eval/region-census"
  tmpBase <- getTemporaryDirectory
  ambient <- getEnvironment
  let root = tmpBase </> "nova-nix-test-regions"
      target = root </> "regions.txt"
      inRoot = bracket_ (forceRemoveIfExists root >> createDirectoryIfMissing True root) (forceRemoveIfExists root)
      evalWith value args =
        Proc.readCreateProcessWithExitCode
          (Proc.proc "cabal" (["run", "-v0", "exe:nova-nix", "--", "eval"] <> args))
            { Proc.env = Just ((regionVar, value) : filter ((/= regionVar) . fst) ambient)
            }
          ""
      -- The figures the census writes over an evaluation, or why there
      -- are none.
      figuresOf args = do
        (code, _, err) <- evalWith target args
        written <- Dir.doesFileExist target
        if code /= ExitSuccess || not written
          then pure (Left (T.pack ("eval " <> unwords args <> " ended " <> show code <> ": " <> err)))
          else do
            figures <- figureLines <$> BS.readFile target
            Dir.removeFile target
            pure (Right figures)
      checked args check = either Fail check <$> figuresOf args
  sequence
    [ runTestM "eval writes a census of regions to the file NOVA_NIX_REGION_STATS names, and none when it is empty" $ inRoot $ do
        let expr = ["--expr", "let a = 1 + 1; b = a + 1; in b"]
        (code, out, _) <- evalWith target expr
        figures <- figureLines <$> BS.readFile target
        Dir.removeFile target
        (emptyCode, emptyOut, _) <- evalWith "" expr
        leftOver <- Dir.doesFileExist target
        pure $
          assertEqual "printed" (ExitSuccess, "3\n") (code, out)
            `andThen` assertEqual "with an empty value" (ExitSuccess, out, False) (emptyCode, emptyOut, leftOver)
            -- Forcing b is a region, and forcing a inside it one more.
            `andThen` assertEqual "closes" [2] (total "regions.closes" figures)
            `andThen` assertEqual "depth" [2] (total "regions.depth" figures),
      runTestM "a loop that forces and drops a value each step holds what one step does, however many steps it runs" $ inRoot $ do
        short <- figuresOf ["--expr", forcedLoop 50]
        long <- figuresOf ["--expr", forcedLoop 200]
        pure $ case (short, long) of
          (Left why, _) -> Fail why
          (_, Left why) -> Fail why
          (Right few, Right many) -> case (heldAtZero few, heldAtZero many, total allocatedName few, total allocatedName many) of
            ([fewHeld], [manyHeld], [fewAllocated], [manyAllocated]) ->
              assertEqual "held grows by under a hundredth of what is allocated" True ((manyHeld - fewHeld) * 100 < manyAllocated - fewAllocated)
                -- Each step's region is past the threshold and inside the
                -- loop's, so only the tenure collector's closes below the
                -- outermost free it.
                `andThen` case (tenuredHeld few, tenuredHeld many) of
                  ([fewTenured], [manyTenured]) ->
                    assertEqual "so does what the tenure collector with no budget holds" True ((manyTenured - fewTenured) * 100 < manyAllocated - fewAllocated)
                  missing -> Fail ("tenure figures missing: " <> T.pack (show missing))
                `andThen` orderings few
                `andThen` orderings many
            missing -> Fail ("figures missing: " <> T.pack (show missing)),
      runTestM "a list made by dropping elements keeps the whole items array it is the tail of, which an allocator frees whole" $ inRoot $ do
        -- r is the last of 999 tails of a list of 1000 elements, and its
        -- region, the largest, keeps it and so the whole array.
        let elements = 1000 :: Integer
        figures <- figuresOf ["--expr", "let t = l: n: if n == 0 then l else t (builtins.tail l) (n - 1); r = t (builtins.genList (x: x) " <> show elements <> ") " <> show (elements - 1) <> "; in builtins.length r"]
        pure $ case figures of
          Left why -> Fail why
          Right found -> case reverse [values | ("regions.class", values) <- found] of
            (_ : closes : _ : kept : _) : _ ->
              assertEqual "the largest region is r's alone" 1 closes
                `andThen` assertEqual "r's region keeps the array's bytes" True (kept >= elements * 8)
                `andThen` orderings found
            _ -> Fail "no size class",
      runTestM "the census places every pointer, resurrects nothing, and its peaks keep their order, over sets, strings, with-scopes and an import" $ inRoot $ do
        BS.writeFile (root </> "lib.nix") "{ n }: builtins.genList (x: { inherit x; name = \"x${toString x}\"; }) n\n"
        BS.writeFile (root </> "main.nix") "let lib = import ./lib.nix; in builtins.length (lib { n = 20; }) + (let ys = lib { n = 30; }; in builtins.length ys)\n"
        results <-
          traverse
            (`checked` orderings)
            [ ["--expr", withScopes],
              ["--strict", "--expr", setsAndStrings],
              ["--expr", listAndTail],
              [root </> "main.nix"]
            ]
        pure (foldr andThen Pass results),
      runTestM "a tenure collector enters a survivor at the close that tenures it and where a close marks its region whole, where its threshold's collector enters it at every close around" $ inRoot $ do
        -- Each of 20 nested forces returns the list the innermost made,
        -- so every close around that one finds the list still reached.
        figures <- figuresOf ["--expr", "let f = n: if n == 0 then builtins.genList (x: x) 5000 else let xs = f (n - 1); in builtins.seq xs xs; in builtins.length (f 20)"]
        pure $ case figures of
          Left why -> Fail why
          Right found ->
            let fewer (bytes, entered) = case [rowEntered | ("regions.threshold", t : _ : rowEntered : _) <- found, t == bytes] of
                  [rowEntered] -> assertEqual "fewer than a quarter of the objects entered" True (entered * 4 < rowEntered)
                  _ -> Fail "no threshold of the tenure collector's bytes"
             in case [(bytes, entered) | ("regions.tenure", _ : bytes : _ : _ : entered : _) <- found] of
                  [] -> Fail "no tenure collector"
                  tenured -> foldr (andThen . fewer) (orderings found) tenured,
      runTestM "the tenure collector remembers what a survivor holds of a region around that the region has not tenured, and frees nothing reached" $ inRoot $ do
        -- Each level's set holds thunks the level around made before it
        -- forced the level, and each level's junk dies at its own close.
        figures <- figuresOf ["--expr", "let f = n: acc: let mine = builtins.genList (x: x + n) 2000; junk = builtins.length (builtins.genList (x: x * n) 3000); next = acc ++ [ mine ]; c = if n == 0 then { inherit acc; } else f (n - 1) next; r = builtins.seq junk (builtins.seq c { inherit c next; }); in r; top = f 10 []; in builtins.length top.next"]
        pure $ case figures of
          Left why -> Fail why
          Right found -> case [made | ("regions.tenure.remembered", [_, made, _]) <- found] of
            [] -> Fail "no count of what was remembered"
            made -> assertEqual "pointers remembered by each" True (all (> 0) made) `andThen` orderings found,
      runTestM "a tenure collector with a budget marks a region whole once its closes have tenured that much, and frees what died since" $ inRoot $ do
        -- Each level's list survives the close that makes it, stored in
        -- the level's cell, and dies once the level has its length.
        figures <- figuresOf ["--expr", "let f = n: if n == 0 then 0 else let xs = builtins.genList (x: x + n) 30000; ys = builtins.seq (f (n - 1)) xs; in builtins.length (builtins.seq ys ys); in f 12"]
        pure $ case figures of
          Left why -> Fail why
          Right found -> case ([rest | ("regions.tenure", 0 : rest) <- found], [rest | ("regions.tenure", b : rest) <- found, b == 2 ^ (20 :: Int)]) of
            ([_ : _ : _ : _ : _ : _ : unbudgeted : _], [_ : _ : full : _ : _ : freed : budgeted : _]) ->
              assertEqual "it marks inner regions whole" True (full > 1)
                `andThen` assertEqual "they free what died since" True (freed > 0)
                `andThen` assertEqual "it holds less than the collector with no budget" True (budgeted < unbudgeted)
                `andThen` orderings found
            _ -> Fail "no tenure collectors with no budget and with 1 MiB"
    ]
  where
    regionVar = "NOVA_NIX_REGION_STATS"
    allocatedName = "regions.allocated.bytes"
    figureLines written =
      [(name, values) | name : rest <- map BS8.words (BS8.lines written), Just values <- [traverse wholeNumber rest]]
    wholeNumber value = case BS8.readInteger value of
      Just (n, rest) | BS.null rest -> Just n
      _ -> Nothing
    total name figures = [value | (key, [value]) <- figures, key == name]
    -- Each threshold's row past its bytes and three counts: its peaks,
    -- single objects first, then each block size, largest first, then
    -- each line length, longest first.
    peaks figures = [drop 3 rest | ("regions.threshold", _ : rest) <- figures]
    heldAtZero figures = [held | ("regions.threshold", 0 : _ : _ : _ : held : _) <- figures]
    -- The single objects the tenure collector with no budget holds at
    -- most, past its budget, threshold, three counts and what its marks
    -- of the whole freed.
    tenuredHeld figures = [held | ("regions.tenure", 0 : _ : _ : _ : _ : _ : _ : held : _) <- figures]
    -- Each step's value is a binding of its own, so it is forced, and
    -- what the step allocates dies with its region.
    forcedLoop :: Int -> String
    forcedLoop steps =
      "let r = builtins.foldl' (acc: i: acc + (let n = builtins.length (builtins.genList (x: x * i) 1000); in n)) 0 (builtins.genList (x: x) "
        <> show steps
        <> "); in r"
    withScopes =
      "let f = n: with rec { xs = builtins.genList (x: x) n; ys = builtins.map (x: x * 2) xs; }; builtins.length ys + builtins.length xs; "
        <> "in builtins.foldl' (acc: i: acc + (let v = f i; in v)) 0 (builtins.genList (x: x) 30)"
    -- At c's close the records of l and t push their lists in that order,
    -- so t's is entered first and keeps the head of l's items array, which
    -- l's list must still follow: d's close, which reaches l alone, would
    -- otherwise reach what c's found unreached.
    listAndTail =
      "let f = _: let l = builtins.genList (x: x) 1000; t = builtins.tail l; c = builtins.seq t (builtins.length t); in builtins.seq c l; "
        <> "d = f null; in builtins.foldl' (a: x: a + x) 0 d"
    setsAndStrings =
      "let mk = i: { name = \"n${toString i}\"; value = builtins.genList (x: { inherit x; }) 10; }; "
        <> "s = builtins.listToAttrs (builtins.genList mk 100); "
        <> "in builtins.attrNames s ++ [ (builtins.length (builtins.concatLists (builtins.attrValues s))) ]"
    -- What every census satisfies: a collector that marks only past a
    -- higher threshold frees later and holds no less; freeing whole
    -- blocks or lines frees less than freeing every object, and a smaller
    -- block or line frees at least what a larger one does, as each nests
    -- in the one before.
    orderings figures =
      let rows = peaks figures
          rising values = and (zipWith (<=) values (drop 1 values))
          falling values = and (zipWith (>=) values (drop 1 values))
          finerHoldsLess row = case row of
            objects : grains -> falling grains && all (objects <=) grains
            [] -> False
          classes = [values | ("regions.class", values) <- figures]
       in assertEqual "unplaced" [0] (total "regions.unplaced" figures)
            `andThen` assertEqual "resurrected" [0] (total "regions.resurrected.count" figures)
            `andThen` assertEqual "nine thresholds of eight peaks" (replicate 9 8) (map length rows)
            `andThen` assertEqual "lines of 256 and 128 bytes after the blocks" [[6, 256], [7, 128]] [values | ("regions.line", values) <- figures]
            `andThen` assertEqual "a higher threshold holds no less" True (all rising (transpose rows))
            `andThen` assertEqual "a smaller block or line holds no more, and single objects least" True (all finerHoldsLess rows)
            `andThen` assertEqual "no class keeps more than it allocated" True (and [kept <= allocated | _ : _ : allocated : kept : _ <- classes])
            `andThen` assertEqual "every close is in a class" (total "regions.closes" figures) [sum [closes | _ : closes : _ <- classes]]
            `andThen` tenureOrderings finerHoldsLess figures
    -- What every census satisfies of each tenure collector: it marks at
    -- the closes its threshold's collector does, frees nothing sooner,
    -- and frees nothing anything reaches.
    tenureOrderings finerHoldsLess figures =
      let tenures = [values | ("regions.tenure", values) <- figures]
          ofBudget name budget = [rest | (key, b : rest) <- figures, key == name, b == budget]
          check values = case values of
            budget : bytes : closes : full : _ : _ : _ : held ->
              case [rest | ("regions.threshold", t : rest) <- figures, t == bytes] of
                [rowCloses : _ : _ : rowHeld] ->
                  assertEqual "it marks where its threshold's collector does" rowCloses closes
                    `andThen` assertEqual "of them, no more marks of the whole than closes that mark" True (full <= closes)
                    `andThen` assertEqual "eight peaks" 8 (length held)
                    `andThen` assertEqual "tenuring holds no less" True (and (zipWith (>=) held rowHeld))
                    `andThen` assertEqual "a smaller block or line holds no more under tenure, and single objects least" True (finerHoldsLess held)
                    `andThen` assertEqual "freed nothing the census's mark at the close reached" [[0]] (ofBudget "regions.tenure.reached" budget)
                    `andThen` assertEqual "freed nothing a later close reached" [[0, 0]] (ofBudget "regions.tenure.resurrected" budget)
                _ -> Fail "no threshold of the tenure collector's bytes"
            _ -> Fail "a tenure collector's figures are missing"
       in assertEqual "tenure collectors with no budget and with budgets of 1, 16 and 256 MiB" [0, 2 ^ (20 :: Int), 2 ^ (24 :: Int), 2 ^ (28 :: Int)] [budget | budget : _ <- tenures]
            `andThen` foldr (andThen . check) Pass tenures

-- | A chain of @//@: its operands evaluated last to first and merged from
-- the first, as upstream 2.34's update queue gathers and folds them
-- (@ExprOpUpdate::evalForUpdate@ and @ExprOpUpdate::eval@ in eval.cc).
testUpdateChains :: IO [Bool]
testUpdateChains = do
  putStrLn "eval/update-chains"
  tmpBase <- getTemporaryDirectory
  sequence
    [ emits
        "a chain of updates evaluates its operands last to first"
        "builtins.trace (builtins.attrNames (builtins.trace \"a\" { za = 1; } // builtins.trace \"b\" { zb = 1; } // builtins.trace \"c\" { zc = 1; })) null"
        ["trace: c", "trace: b", "trace: a", "trace: [ \"za\" \"zb\" \"zc\" ]"],
      runTest "a chain of updates refuses its last operand that is not a set, and evaluates none before it" $
        assertEvalError "update-chain-last-refusal" "throw \"earlier\" // 1 // { }" "expected a set but found an integer: 1",
      -- The flat copy is removeAttrs of nothing, which makes a set of its
      -- own; the model merges the operands' attributes, the later winning.
      -- Operands of up to 20 attributes cross the 16 a layer takes, and
      -- chains of up to 12 the eight sets a chain of layers holds.
      runTestM "a chain of updates reads as a flat copy of it and as its operands merged, its positions and printed form too" . quickCheckPasses $
        QC.forAll genChain $ \operands -> QC.ioProperty $ do
          let model = foldl' (flip Map.union) Map.empty operands
              chain = T.intercalate " // " (map setSource operands)
              withChain body = "let s = " <> chain <> "; f = builtins.removeAttrs s [ ]; names = builtins.attrNames s; in " <> body
              agreements =
                withChain $
                  "builtins.all (x: x) [ (s == f) (names == builtins.attrNames f) (builtins.attrValues s == builtins.attrValues f) "
                    <> "(builtins.toJSON s == builtins.toJSON f) (builtins.all (n: builtins.unsafeGetAttrPos n s == builtins.unsafeGetAttrPos n f) names) "
                    <> "(s == "
                    <> setSource model
                    <> ") ]"
              printedAs value = evalNixIO tmpBase (withChain ("builtins.deepSeq " <> value <> " " <> value)) >>= traverse (printValue PrintInFull)
          printedChain <- printedAs "s"
          printedCopy <- printedAs "f"
          printedModel <- printedAs (setSource model)
          pure
            $ QC.counterexample (T.unpack chain)
            $ (evalNixInFile agreements, printedChain, printedCopy)
              QC.=== (Right (VBool True), printedModel, printedModel)
    ]
  where
    quickCheckPasses property = do
      result <- QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} property
      pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))
    genChain = QC.chooseInt (1, 12) >>= \n -> QC.vectorOf n genOperand
    genOperand = QC.chooseInt (0, 20) >>= \size -> Map.fromList <$> QC.vectorOf size ((,) <$> QC.elements chainNames <*> QC.chooseInt (0, 9))
    chainNames = [T.pack ("p" <> show i) | i <- [0 .. 23 :: Int]]
    setSource attrs = "{ " <> T.concat [name <> " = " <> T.pack (show value) <> "; " | (name, value) <- Map.toList attrs] <> "}"

testAllocSites :: IO [Bool]
testAllocSites = do
  putStrLn "eval/alloc-sites"
  countSites
  sequence
    [ runTestM "the evaluator names every site the C side counts, each by a name of its own" $ do
        envSites <- envSitesCounted
        setSites <- setSitesCounted
        let envNames = map envSiteName [minBound .. maxBound]
            setNames = map setSiteName [minBound .. maxBound]
        pure $
          assertEqual "env sites" (fromIntegral envSites) (length envNames)
            `andThen` assertEqual "set sites" (fromIntegral setSites) (length setNames)
            `andThen` assertEqual "distinct env names" envNames (nub envNames)
            `andThen` assertEqual "distinct set names" setNames (nub setNames),
      runTestM "counting by site counts each env at its site, by size, with its slots and its bytes" $
        quickCheckPasses $
          QC.forAll (QC.listOf genEnvAllocation) $ \allocations -> QC.ioProperty $ do
            added <- sitesAdded (mapM_ allocateEnv allocations)
            pure (fmap (\counts -> (sitesEnvs counts, sitesEnvSizes counts)) added QC.=== Right (envsExpected allocations)),
      runTestM "counting by site counts each set at its site with its bytes, and an empty one nowhere, as the shared empty set" $
        quickCheckPasses $
          QC.forAll (QC.listOf genSetAllocation) $ \allocations -> QC.ioProperty $ do
            added <- sitesAdded (mapM_ allocateSet allocations)
            pure (fmap sitesSets added QC.=== Right (setsExpected allocations)),
      -- Upstream's allocBindings answers a size of 0 with its one
      -- emptyBindings (attr-set.cc).
      runTestM "every set built with no attributes is the shared empty set, at every site" $
        quickCheckPasses $
          QC.forAll ((,) <$> QC.arbitraryBoundedEnum <*> QC.listOf (QC.elements ["a", "b", "c"])) $ \(site, removed) -> QC.ioProperty $ do
            built <- newIORef []
            let keep make = make >>= \(AttrSet cset) -> modifyIORef' built (cset :)
            added <-
              sitesAdded $ do
                keep (attrSetFromMap site Map.empty)
                keep (attrSetFromEntries site Map.empty)
                keep (attrSetFromSymbolEntriesSizedFor site 0 Map.empty)
                keep (attrSetRemoveKeys site removed emptyAttrSet)
            sets <- readIORef built
            pure ((sets, fmap sitesSets added) QC.=== (replicate 4 (unAttrSet emptyAttrSet), Right Map.empty)),
      -- Upstream sizes a literal and intersectAttrs' result before it knows
      -- what they hold (ExprAttrs::eval in eval.cc, prim_intersectAttrs in
      -- primops.cc), and gives bindings of their own to a size above 0.
      runTestM "a set sized for attributes that holds none is an empty set of its own, counted and not shareable" $
        quickCheckPasses $
          QC.forAll ((,) <$> QC.arbitraryBoundedEnum <*> QC.chooseInt (1, 4)) $ \(site, size) -> QC.ioProperty $ do
            sets <- newIORef []
            let keep make = make >>= \set -> modifyIORef' sets (set :)
            added <- sitesAdded (replicateM_ 2 (keep (attrSetFromSymbolEntriesSizedFor site size Map.empty)))
            made <- readIORef sets
            sizes <- mapM attrSetSize made
            let pointers = map unAttrSet made
            pure $
              (sizes, length (nub pointers), unAttrSet emptyAttrSet `elem` pointers, fmap sitesSets added)
                QC.=== ([0, 0], 2, False, Right (Map.singleton site (SetSiteFigures 2 0 (2 * (setHeaderSize + setEntrySize))))),
      runTestM "a union with an empty operand counts as shareable, and one of two non-empty sets does not" $ do
        nullCell <- evaluated VNull
        one <- attrSetFromMap SetLiteral (Map.singleton "a" nullCell)
        none <- attrSetFromMap SetLiteral Map.empty
        added <-
          sitesAdded $
            mapM_ (\(AttrSet left, AttrSet right) -> void (cattrsetUnion SetUpdate left right)) [(one, none), (none, one), (one, one)]
        pure (assertEqual "update" (Right (Map.singleton SetUpdate (SetSiteFigures 3 2 (3 * setHeaderSize + 4 * setEntrySize)))) (fmap sitesSets added)),
      -- Upstream layers a right operand of at most 16 attributes on a left
      -- whose chain is not full, adding the right's size to
      -- nrOpUpdateValuesCopied, and else merges, adding the result's
      -- (ExprOpUpdate::eval in eval.cc at 2.34.9).  A set built here is
      -- flat, so its chain is not full.  An empty operand, which the
      -- evaluator never passes, makes a union, which counts as shareable,
      -- and two make one with the default room.
      runTestM "an update layers a right operand of at most 16 entries and counts its entries as copied, else merges and counts the result's, and a union counts none" $ do
        pool <- mapM (\i -> symbolIntern ("copied_" <> T.pack (show (i :: Int)))) [1 .. 30]
        let build keys = do
              set <- cattrsetNew SetLiteral (length keys)
              mapM_ (\key -> cattrsetInsert set key noPos cthunkNull) keys
              cattrsetFreeze set
              pure set
        quickCheckPasses $
          QC.forAll ((,) <$> QC.sublistOf pool <*> QC.sublistOf pool) $ \(left, right) -> QC.ioProperty $ do
            leftSet <- build left
            rightSet <- build right
            made <- newIORef 0
            updated <- sitesAdded (cattrsetUpdate SetUpdate leftSet rightSet >>= cattrsetSize >>= writeIORef made)
            size <- readIORef made
            merged <- sitesAdded (void (cattrsetUnion SetUpdate leftSet rightSet))
            let union = Set.size (Set.fromList (left ++ right))
                layered = not (null left) && not (null right) && length right <= setLayerRhsMax
                (copied, room)
                  | layered = (length right, length right)
                  | null left && null right = (union, setDefaultRoom)
                  | otherwise = (union, length left + length right)
                shareable = if null left || null right then 1 else 0
            pure $
              (fromIntegral size, fmap sitesUpdateCopied updated, fmap (Map.lookup SetUpdate . sitesSets) updated, fmap sitesUpdateCopied merged)
                QC.=== (union, Right (fromIntegral copied), Right (Just (SetSiteFigures 1 shareable (setBytes room))), Right 0),
      -- Each source's figures are worked out by hand from the evaluator:
      -- an env is an 8-byte word with 8 bytes a slot, which holds the
      -- parent of an env of one to seven slots and no scopes and is
      -- otherwise a header followed by 8 bytes for a parent, a with's env
      -- 24 more for its scopes and 8 for each entry of its with-scope
      -- array, and a set a 40-byte header with 16 bytes an entry, with
      -- room for one when it is an empty set of its own; the shared empty
      -- set is no allocation.  The root env is each evaluation's, and
      -- every other env made in it has a parent; a trimmed closure's and a
      -- capture's have none.  A builtin's deferred application is its
      -- cells and no env.
      sitesTest
        "a let makes one env with a slot a binding"
        "let a = 1; b = 2; in a + b"
        [(EnvLet, EnvSiteFigures 1 2 24)]
        [],
      sitesTest
        "a call of a lambda with an argument name makes one env of one slot"
        "(x: x) 1"
        [(EnvCall, EnvSiteFigures 1 1 16)]
        [],
      sitesTest
        "a curried call makes an env per call, and the inner lambda a closure of what it uses"
        "(x: y: x) 1 2"
        [(EnvCall, EnvSiteFigures 2 2 32), (EnvClosure, EnvSiteFigures 1 1 16)]
        [],
      sitesTest
        "a call of a lambda with formals makes one env of a slot a formal"
        "({ a, b ? 2 }: a + b) { a = 1; }"
        [(EnvCallFormals, EnvSiteFigures 1 2 24)]
        [(SetLiteral, SetSiteFigures 1 0 56)],
      sitesTest
        "a with makes one env, a copy of its scope's slots, with an array of its with-scopes"
        "with { a = 1; }; a"
        [(EnvWith, EnvSiteFigures 1 0 40)]
        [(SetLiteral, SetSiteFigures 1 0 56)],
      -- The with's variable makes closure trimming give the let a capture
      -- holding the with-scopes the root env sees, which are none, so the
      -- capture is a bare header.  The let's frame sits under that
      -- capture, and the with's env copies the frame's slot and parent,
      -- with an array of its own scope.
      sitesTest
        "a with's env holds and counts the slots it copies from the env it is made in"
        "let b = 2; in with { a = 1; }; a + b"
        [(EnvLet, EnvSiteFigures 1 1 16), (EnvLetCapture, EnvSiteFigures 1 0 8), (EnvWith, EnvSiteFigures 1 1 56)]
        [(SetLiteral, SetSiteFigures 1 0 56)],
      sitesTest
        "an update with an empty operand makes no set, and the empty literal is the shared empty set"
        "builtins.deepSeq [ ({ } // { a = 1; }) ({ b = 2; } // { }) ] null"
        []
        [(SetLiteral, SetSiteFigures 2 0 112)],
      sitesTest
        "an update of two sets with attributes makes a layer, with room for the right operand's alone"
        "builtins.deepSeq ({ a = 1; } // { b = 2; }) null"
        []
        [(SetLiteral, SetSiteFigures 2 0 112), (SetUpdate, SetSiteFigures 1 0 56)],
      sitesTest
        "an update layers a right operand of 16 attributes"
        (updateSource "{ a = 1; }" [rightOf setLayerRhsMax])
        []
        [(SetLiteral, SetSiteFigures 2 0 (setBytes 1 + setBytes setLayerRhsMax)), (SetUpdate, SetSiteFigures 1 0 (setBytes setLayerRhsMax))],
      sitesTest
        "an update merges a right operand of 17 attributes, into a set with room for both"
        (updateSource "{ a = 1; }" [rightOf (setLayerRhsMax + 1)])
        []
        [(SetLiteral, SetSiteFigures 2 0 (setBytes 1 + setBytes (setLayerRhsMax + 1))), (SetUpdate, SetSiteFigures 1 0 (setBytes (setLayerRhsMax + 2)))],
      -- Seven updates layer, the chain growing to eight sets, and the
      -- eighth merges the chain's eight names with its own.
      sitesTest
        "an update on a chain of eight sets merges"
        (updateSource "{ a = 1; }" (singleRights setMaxLayers))
        []
        [(SetLiteral, SetSiteFigures 9 0 (9 * setBytes 1)), (SetUpdate, SetSiteFigures 8 0 (7 * setBytes 1 + setBytes 9))],
      copiesTest
        "an update with an empty operand copies nothing"
        "builtins.deepSeq [ ({ } // { a = 1; }) ({ b = 2; } // { }) ] null"
        0,
      copiesTest
        "an update that layers copies the right operand's entries alone"
        "builtins.deepSeq ({ a = 1; b = 2; } // { b = 3; c = 4; }) null"
        2,
      copiesTest
        "an update that merges copies every entry of the set it makes, a name both operands hold once"
        (updateSource "{ a = 1; b1 = 2; }" [rightOf (setLayerRhsMax + 1)])
        (fromIntegral setLayerRhsMax + 2),
      copiesTest
        "an update on a chain of eight sets copies every entry of the set it merges"
        (updateSource "{ a = 1; }" (singleRights setMaxLayers))
        (fromIntegral setMaxLayers - 1 + 9),
      -- A chain folds from its first operand, as upstream 2.34's update
      -- queue does (ExprOpUpdate::eval in eval.cc): { c } layers on
      -- { a, b }, and { d } on that, one entry each, as 2.34.9's
      -- nrOpUpdateValuesCopied counts them.
      copiesTest
        "a chain of updates copies what each update layers"
        "builtins.deepSeq ({ a = 1; b = 2; } // { c = 3; } // { d = 4; }) null"
        2,
      -- Written without parentheses the chain nests to the right, and it
      -- folds from its first operand as the chain of eight above does.
      copiesTest
        "a chain of updates copies as one folded from its first operand, however it nests"
        ("builtins.deepSeq (" <> T.intercalate " // " ("{ a = 1; }" : singleRights setMaxLayers) <> ") null")
        (fromIntegral setMaxLayers - 1 + 9),
      sitesTest
        "a chain of updates makes the sets of one folded from its first operand, however it nests"
        ("builtins.deepSeq (" <> T.intercalate " // " ("{ a = 1; }" : singleRights setMaxLayers) <> ") null")
        []
        [(SetLiteral, SetSiteFigures 9 0 (9 * setBytes 1)), (SetUpdate, SetSiteFigures 8 0 (7 * setBytes 1 + setBytes 9))],
      -- 2.34's update queue evaluates every operand before it merges any
      -- (ExprOpUpdate::eval in eval.cc), so a chain one of whose
      -- operands fails merges nothing, whichever operand it is.
      copiesTest
        "a chain of updates one of whose operands fails copies nothing"
        "builtins.deepSeq [ (builtins.tryEval ({ a = 1; } // { b = 2; } // throw \"x\")) (builtins.tryEval ({ a = 1; } // throw \"x\" // { b = 2; })) ] null"
        0,
      -- A let is not an update, so r is one operand: { c } layers on { b }
      -- inside it, copying one entry, and then r, layered itself, is copied
      -- flat into a layer on { a }, its two entries.
      copiesTest
        "an operand that is not an update is one operand of the chain, and a layered one is copied flat"
        "builtins.deepSeq ({ a = 1; } // (let r = { b = 2; } // { c = 3; }; in r)) null"
        3,
      sitesTest
        "removeAttrs of every attribute makes an empty set of its own, and of an empty set none"
        "builtins.deepSeq [ (builtins.removeAttrs { a = 1; } [ \"a\" ]) (builtins.removeAttrs { } [ \"a\" ]) ] null"
        []
        [(SetLiteral, SetSiteFigures 1 0 56), (SetRemoveAttrs, SetSiteFigures 1 0 56)],
      sitesTest
        "intersectAttrs of two sets with no name in common makes an empty set of its own, and with an empty one none"
        "builtins.deepSeq [ (builtins.intersectAttrs { a = 1; } { b = 2; }) (builtins.intersectAttrs { } { b = 2; }) ] null"
        []
        [(SetLiteral, SetSiteFigures 3 0 168), (SetIntersectAttrs, SetSiteFigures 1 0 56)],
      sitesTest
        "a literal whose every name is a null dynamic one makes an empty set of its own"
        "builtins.deepSeq { ${null} = 1; } null"
        []
        [(SetLiteral, SetSiteFigures 1 0 56)],
      -- The frame binds its names through a name table: an env of a
      -- header, a parent and its scopes, 8, 8 and 24 bytes.  Its name
      -- table holds no static name, so it is the shared empty set and no
      -- scope set is made.
      sitesTest
        "a rec set whose every name is a null dynamic one makes an empty set of its own"
        "builtins.deepSeq (rec { ${null} = 1; }) null"
        [(EnvRec, EnvSiteFigures 1 0 40)]
        [(SetRec, SetSiteFigures 1 0 56)],
      sitesTest
        "map defers an application per element, which makes no env, and each calls the function"
        "builtins.deepSeq (builtins.map (x: x) [ 1 2 3 ]) null"
        [(EnvCall, EnvSiteFigures 3 3 48)]
        [],
      sitesTest
        "concatMap calls the function on each element, making no env but the call's"
        "builtins.deepSeq (builtins.concatMap (x: [ x ]) [ 1 2 ]) null"
        [(EnvCall, EnvSiteFigures 2 2 32)]
        [],
      sitesTest
        "each inherit (from) clause makes an env of one slot"
        "let s = { a = 1; b = 2; }; in builtins.deepSeq { inherit (s) a; inherit (s) b; } null"
        [(EnvLet, EnvSiteFigures 1 1 16), (EnvInheritFrom, EnvSiteFigures 2 2 32)]
        [(SetLiteral, SetSiteFigures 2 0 144)],
      sitesTest
        "the names of one inherit (from) clause share its env"
        "let s = { a = 1; b = 2; c = 3; }; in builtins.deepSeq { inherit (s) a b c; } null"
        [(EnvLet, EnvSiteFigures 1 1 16), (EnvInheritFrom, EnvSiteFigures 1 1 16)]
        [(SetLiteral, SetSiteFigures 2 0 176)],
      sitesTest
        "the names of an inherit (from) clause in a let share its env"
        "let s = { a = 1; b = 2; }; in let inherit (s) a b; in a + b"
        [(EnvLet, EnvSiteFigures 2 3 40), (EnvLetCapture, EnvSiteFigures 1 1 16), (EnvInheritFrom, EnvSiteFigures 1 1 16)]
        [(SetLiteral, SetSiteFigures 1 0 72)],
      sitesTest
        "a let using a variable from outside it makes its frame and a capture of that variable"
        "(x: let y = x; in y) 1"
        [(EnvCall, EnvSiteFigures 1 1 16), (EnvLet, EnvSiteFigures 1 1 16), (EnvLetCapture, EnvSiteFigures 1 1 16)]
        [],
      sitesTest
        "a rec set makes one env with a slot a binding and its set"
        "builtins.deepSeq (rec { a = 1; b = a; }) null"
        [(EnvRec, EnvSiteFigures 1 2 24)]
        [(SetRec, SetSiteFigures 1 0 72)],
      sitesTest
        "mapAttrs defers an application per attribute, which makes no env, and each makes two calls"
        "builtins.deepSeq (builtins.mapAttrs (n: v: v) { a = 1; b = 2; }) null"
        [(EnvCall, EnvSiteFigures 4 4 64)]
        [(SetLiteral, SetSiteFigures 1 0 72), (SetMapAttrs, SetSiteFigures 1 0 72)],
      appsTest "map defers one application per element" "builtins.deepSeq (builtins.map (x: x) [ 1 2 3 ]) null" 3,
      appsTest "genList defers one application per index" "builtins.deepSeq (builtins.genList (i: i) 4) null" 4,
      appsTest "mapAttrs defers one application per attribute" "builtins.deepSeq (builtins.mapAttrs (n: v: v) { a = 1; b = 2; }) null" 2,
      appsTest "zipAttrsWith defers one application per name" "builtins.deepSeq (builtins.zipAttrsWith (n: vs: vs) [ { a = 1; } { b = 2; } ]) null" 2,
      appsTest "concatMap calls its function and defers no application" "builtins.deepSeq (builtins.concatMap (x: [ x ]) [ 1 2 ]) null" 0,
      sitesTest
        "functionArgs of a lambda without formals or with no formal is the shared empty set"
        "builtins.deepSeq [ (builtins.functionArgs (x: x)) (builtins.functionArgs ({ }: 1)) ] null"
        []
        []
    ]
  where
    quickCheckPasses property = do
      result <- QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} property
      pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))
    sitesTest name source envs sets =
      runTestM name $ do
        tmpBase <- getTemporaryDirectory
        added <- sitesOfEval tmpBase source
        pure (assertEqual "envs and sets" (Right (Map.fromList envs, Map.fromList sets)) added)
    appsTest name source expected =
      runTestM name $ do
        tmpBase <- getTemporaryDirectory
        added <- appsOfEval tmpBase source
        pure (assertEqual "applications" (Right expected) added)
    copiesTest name source expected =
      runTestM name $ do
        tmpBase <- getTemporaryDirectory
        added <- countsOfEval tmpBase source
        pure (assertEqual "entries copied" (Right expected) (fmap sitesUpdateCopied added))
    genEnvAllocation = EnvAllocation <$> QC.arbitraryBoundedEnum <*> QC.chooseInt (0, 5)
    genSetAllocation = SetAllocation <$> QC.arbitraryBoundedEnum <*> QC.chooseInt (0, 4)
    allocateEnv (EnvAllocation site n) = do
      nullCell <- evaluated VNull
      void (newEnv site Nothing (replicate n nullCell))
    allocateSet (SetAllocation site n) = do
      nullCell <- evaluated VNull
      void (attrSetFromMap site (Map.fromList [(T.pack ("k" ++ show i), nullCell) | i <- [1 .. n]]))
    envsExpected allocations =
      ( Map.fromListWith
          addEnv
          [(site, EnvSiteFigures 1 slots (envWordBytes + censusPointerBytes * slots)) | EnvAllocation site n <- allocations, let slots = fromIntegral n],
        Map.fromListWith (+) [(EnvSlots (fromIntegral n), 1) | EnvAllocation _ n <- allocations]
      )
    setsExpected allocations =
      Map.fromListWith
        addSet
        [ (site, SetSiteFigures 1 0 (setHeaderSize + setEntrySize * fromIntegral n))
        | SetAllocation site n <- allocations,
          n > 0
        ]
    addEnv (EnvSiteFigures a b c) (EnvSiteFigures x y z) = EnvSiteFigures (a + x) (b + y) (c + z)
    addSet (SetSiteFigures a b c) (SetSiteFigures x y z) = SetSiteFigures (a + x) (b + y) (c + z)

-- | One step of a chain of envs ('testEnvLayout'): how the next env is
-- made from the one before it.
data EnvStep
  = -- | An env under the previous one holding this many cells, as a
    -- call's.
    StepChild !Int
  | -- | An env with no parent holding this many cells, as a trimmed
    -- closure's.
    StepParentless !Int
  | -- | An env under the previous one that binds a name table, as a
    -- frame whose names are not all static.
    StepNameTable
  | -- | The previous env under a with whose set is not evaluated yet.
    StepWith
  deriving (Show)

genEnvStep :: QC.Gen EnvStep
genEnvStep =
  QC.oneof
    [ StepChild <$> QC.chooseInt (0, envShortMax + 2),
      StepParentless <$> QC.chooseInt (0, envShortMax + 2),
      pure StepNameTable,
      pure StepWith
    ]

-- | An env a chain of steps made, and what @cbits/nn_env.h@ says it
-- holds.
data EnvModel = EnvModel
  { modelEnv :: !Env,
    -- | Its slots' cells, in order.
    modelSlots :: ![CThunkPtr],
    -- | Its parent, when it has one.
    modelParent :: !(Maybe EnvModel),
    -- | Its name table, when it has one: the set and the cell of its one
    -- value.
    modelNameTable :: !(Maybe (Ptr (), CThunkPtr)),
    -- | The cells behind the with-scope entries it sees, innermost first.
    modelWithCells :: ![CThunkPtr],
    -- | The cells behind the entries of its own with-scope array, none
    -- when it has none.
    modelOwnWithCells :: ![CThunkPtr],
    -- | The bytes it and its own with-scope array take.
    modelBytes :: !Word64
  }

-- | The env a step makes from the previous one, with its model: a child
-- sees its parent's with-scopes, and a with's env is a copy of the env it
-- is made in with one more with-scope of its own.
envStep :: EnvModel -> EnvStep -> IO EnvModel
envStep prev step = case step of
  StepChild n -> do
    cells <- newCells n
    env <- newEnv EnvCall (Just (modelEnv prev)) (map Thunk cells)
    pure (EnvModel env cells (Just prev) Nothing (modelWithCells prev) [] (childEnvBytes n))
  StepParentless n -> parentlessModel n
  StepNameTable -> do
    (set@(AttrSet cset), value) <- oneNameTable
    env <- newScopeEnv EnvLet (Just (modelEnv prev)) set
    pure (EnvModel env [] (Just prev) (Just (castPtr cset, value)) (modelWithCells prev) [] (envWordBytes + censusPointerBytes + envScopesBytes))
  StepWith -> do
    -- Nothing forces the with's set here, so any cell stands for the
    -- thunk that evaluates to it.
    cell <- cthunkNewComputedInt 0
    env <- pushLazyWithScope (Thunk cell) (modelEnv prev)
    let withs = cell : modelWithCells prev
        parentBytes = maybe 0 (const censusPointerBytes) (modelParent prev)
        slotBytes = censusPointerBytes * genericLength (modelSlots prev)
    pure (EnvModel env (modelSlots prev) (modelParent prev) (modelNameTable prev) withs withs (envWordBytes + slotBytes + parentBytes + envScopesBytes + censusPointerBytes * genericLength withs))

-- | A model of an env with no parent holding this many fresh cells.
parentlessModel :: Int -> IO EnvModel
parentlessModel n = do
  cells <- newCells n
  env <- newEnv EnvClosure Nothing (map Thunk cells)
  pure (EnvModel env cells Nothing Nothing [] [] (envWordBytes + censusPointerBytes * genericLength cells))

-- | This many fresh integer cells.
newCells :: Int -> IO [CThunkPtr]
newCells n = mapM cthunkNewComputedInt (take n [1 ..])

-- | A set binding one name, to stand for a name table, and the cell of
-- its value.
oneNameTable :: IO (AttrSet, CThunkPtr)
oneNameTable = do
  value <- cthunkNewComputedInt 0
  set <- attrSetFromMap SetScope (Map.singleton "x" (Thunk value))
  pure (set, value)

-- | What the C side holds for a modelled env: its slots' cells, its
-- parent and the with-scope entries it sees.
observeEnv :: EnvModel -> IO ([CThunkPtr], Maybe Env, [WordPtr])
observeEnv model = observe (modelEnv model)
  where
    observe env@(Env ptr) = do
      slots <- mapM (\i -> unThunk <$> envLookupResolved 0 (fromIntegral i) env) [0 .. length (modelSlots model) - 1]
      parent <- cenvParent ptr
      (arr, count) <- envWithScopesRaw env
      withs <- mapM (fmap ptrToWordPtr . peekElemOff arr) [0 .. fromIntegral count - 1]
      pure (slots, if parent == nullPtr then Nothing else Just (Env parent), withs)

-- | What the model says 'observeEnv' finds: each with-scope entry is its
-- cell tagged lazy (NN_ENV_LAZY_WITH), since nothing forced it.
expectedEnv :: EnvModel -> ([CThunkPtr], Maybe Env, [WordPtr])
expectedEnv model =
  ( modelSlots model,
    modelEnv <$> modelParent model,
    map (\cell -> ptrToWordPtr (castPtr cell) .|. lazyWithTag) (modelWithCells model)
  )
  where
    lazyWithTag = 1

-- | What a census from an env reaches by the model: the envs on its
-- parent chain and their bytes, the distinct cells in their slots, own
-- with-scope arrays and name tables, and the distinct name tables.
expectedReach :: EnvModel -> (Figures, Figures, Maybe Word64)
expectedReach final = (envs, thunks, Just (fromIntegral (Set.size sets)))
  where
    reachable = final : unfoldr (fmap (\parent -> (parent, parent)) . modelParent) final
    envs = Figures (Just (genericLength reachable)) (sum (map modelBytes reachable))
    cells = Set.fromList (concatMap (\m -> modelSlots m ++ modelOwnWithCells m ++ maybe [] (pure . snd) (modelNameTable m)) reachable)
    thunks = Figures (Just (fromIntegral (Set.size cells))) (fromIntegral (Set.size cells) * censusCellBytes)
    sets = Set.fromList (mapMaybe (fmap fst . modelNameTable) reachable)

-- | The layout of @cbits/nn_env.h@: what each kind of env holds, the
-- with-scopes it sees through its parents, and the bytes it takes, which
-- the census counts.
testEnvLayout :: IO [Bool]
testEnvLayout = do
  putStrLn "eval/env-layout"
  sequence
    [ runTestM "an env of each kind takes the bytes nn_env.h gives it" $ do
        -- A trimmed closure's: three slots and no parent.
        trimmed <- parentlessModel 3
        -- A call's: one slot under a parent, here one of two slots.
        callParent <- parentlessModel 2
        call <- envStep callParent (StepChild 1)
        -- A root's: a name table and no parent.
        root <- oneNameTable >>= newScopeEnv EnvRoot Nothing . fst
        -- A with's: a copy of the two slots of the env it is made in and
        -- a with-scope array of one entry, its own.
        withEnv <- envStep callParent StepWith
        -- A frame of more slots than a short env holds, under a parent.
        wide <- envStep callParent (StepChild (envShortMax + 1))
        reached <- mapM (\env -> fmap (kindEnvs . censusReached) <$> censusOf [RootEnv env]) [modelEnv trimmed, modelEnv call, root, modelEnv withEnv, modelEnv wide]
        let parentBytes = envWordBytes + 2 * censusPointerBytes
        pure $
          assertEqual
            "envs reached"
            [ Right (Figures (Just 1) (envWordBytes + 3 * censusPointerBytes)),
              -- The call's word, which holds its parent, and its slot, and
              -- its parent's word and two slots.
              Right (Figures (Just 2) ((envWordBytes + censusPointerBytes) + parentBytes)),
              Right (Figures (Just 1) (envWordBytes + envScopesBytes)),
              -- Its header, two slots, scopes and one-entry array.
              Right (Figures (Just 1) (envWordBytes + 2 * censusPointerBytes + envScopesBytes + censusPointerBytes)),
              -- Its header, eight slots and parent, and its parent's.
              Right (Figures (Just 2) ((envWordBytes + 8 * censusPointerBytes + censusPointerBytes) + parentBytes))
            ]
            reached,
      runTestM "an env holds the slots, parent and with-scopes its making gives it, and a census reaches what they hold and counts the bytes its shape takes" $
        quickCheckPasses $
          QC.forAll ((,) <$> QC.chooseInt (0, 4) <*> QC.resize 12 (QC.listOf genEnvStep)) $ \(firstSlots, steps) ->
            QC.ioProperty $ do
              first <- parentlessModel firstSlots
              -- Newest first.
              chain <- foldM (\built step -> (:| toList built) <$> envStep (NE.head built) step) (first :| []) steps
              observed <- mapM observeEnv (toList chain)
              taken <- censusOf [RootEnv (modelEnv (NE.head chain))]
              let reach census = ((kindEnvs (censusReached census), kindThunks (censusReached census), figuresCount (kindAttrSets (censusReached census))), censusUnplaced census)
              pure $
                (observed, reach <$> taken)
                  QC.=== (map expectedEnv (toList chain), Right (expectedReach (NE.head chain), 0)),
      -- The closure's env holds the array itself, and a lazy entry one
      -- env replaces by its set is replaced for the other.
      runTestM "a trimmed closure under a with holds the with-scopes array the env it is made in sees" $ do
        cell <- cthunkNewComputedInt 0
        base <- modelEnv <$> parentlessModel 1
        withEnv <- pushLazyWithScope (Thunk cell) base
        closure <- buildCaptureEnv EnvClosure withEnv (CapturesWithScopes [(0, 0)])
        seen <- envWithScopesRaw withEnv
        held <- envWithScopesRaw closure
        pure (assertEqual "with-scopes" seen held)
    ]
  where
    quickCheckPasses property = do
      result <- QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} property
      pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))

-- | A pending thunk holds the C pointer of the env its code runs in, and a
-- recursive frame (a @let@, a @rec@ set, a formals set) is built before
-- the thunks that point at it and filled after.  A generated binding may
-- name one declared after it, whose slot is still empty while the
-- earlier binding's thunk is made, so a frame that read its own slots
-- while being built would read that hole.
testFrameKnots :: IO [Bool]
testFrameKnots = do
  putStrLn "eval/frame-knots"
  tmpBase <- getTemporaryDirectory
  sequence
    [ runTestM "a pending thunk's payload is its env's pointer, and forcing it reads that env" $
        quickCheckPasses $
          QC.forAll genSlotRead $ \(before, value, after) ->
            QC.ioProperty $ do
              env@(Env envPtr) <- mapM (evaluated . VInt) (before ++ value : after) >>= newEnv EnvClosure Nothing
              bcIdx <- compileExpr (EResolvedVar 0 (length before))
              Thunk cell <- mkThunkBc env bcIdx
              payload <- cthunkPayload cell
              stored <- readPending cell
              let code = case stored of
                    PendingCode (Env storedEnv) storedIdx -> Just (storedEnv, storedIdx)
                    _ -> Nothing
              pure $
                (payload, code, runPureEval (force (Thunk cell)))
                  QC.=== (castPtr envPtr, Just (envPtr, bcIdx), Right (VInt value)),
      runTestM "a frame binding may name one declared after it, in a let, a rec set and a formals set" $
        quickCheckPasses $
          QC.forAll genFrame $ \frame ->
            QC.ioProperty $ do
              let declared = sortOn fbPos frame
                  cases =
                    [ (letSource declared, frameText (const Nothing) frame),
                      (recSource declared, frameText (const Nothing) frame),
                      (formalsSource declared, frameText fbPassed frame)
                    ]
              viaIO <- mapM (evalNixIO tmpBase . fst) cases
              pure $
                QC.conjoin
                  [ QC.counterexample (T.unpack source) ((evalNix source, io) QC.=== (want, want))
                  | ((source, expected), io) <- zip cases viaIO,
                    let want = Right (mkStr expected)
                  ],
      runTest "a default may name the @-pattern" $
        assertEval "at-pattern" "(args@{ a ? args.b, b ? 2 }: a) { b = 5; }" (VInt 5),
      -- A frame whose @-pattern slot and formal slots disagree with the
      -- resolver's layout reads b's value for a, giving 5.
      runTest "a default reading the @-pattern finds the argument set in its own slot" $
        assertEval "at-pattern-slot" "(args@{ a ? args.b + 1, b ? 2 }: a) { b = 5; }" (VInt 6),
      runTest "a rec set or let binding may name an inherit declared after it" $
        assertEval "rec-inherit-later" "let x = 1; in (rec { y = x; inherit x; }).y" (VInt 1)
          `andThen` assertEval "let-inherit-later" "let x = 1; in let y = x; inherit x; in y" (VInt 1),
      runTest "inherit (from) in a rec set or let may name a binding declared after it" $
        assertEval "rec-inherit-from-later" "(rec { inherit (s) a; s = { a = 1; }; }).a" (VInt 1)
          `andThen` assertEval "let-inherit-from-later" "let inherit (s) a; s = { a = 1; }; in a" (VInt 1),
      runTest "a rec set with a dynamic key may name bindings declared after them" $
        assertEval
          "dynamic-rec-later"
          "let k = \"d\"; s = rec { a = b; ${k} = a; inherit (t) c; b = 1; t = { c = 2; }; }; in toString [ s.a s.d s.c ]"
          (mkStr "1 1 2"),
      -- The parser refuses a dynamic key in a let, so only an AST built by
      -- hand takes the let's dynamic-key path.
      runTest "a let with a dynamic key may name bindings declared after them" $
        assertEqual "dynamic-let-later" (Right (mkStr "1 2")) (runPureEval (builtinEnv unrestrictedPolicy 0 [] >>= (`eval` dynamicLetLater))),
      -- Functions compare equal only as the same cell, so this holds only
      -- when the set's attribute and the frame's slot are one cell.  The
      -- IO evaluator keeps s once forced; the pure one rebuilds it at each
      -- use, in a new frame.
      -- One thunk of an inherit's from-expression serves every name it
      -- gives, as upstream's ExprAttrs::buildInheritFromEnv makes one
      -- (eval.cc), so the expression runs once.
      emits
        "an inherit's from-expression is evaluated once for all the names it gives"
        "builtins.deepSeq { inherit (builtins.trace \"e\" { a = 1; b = 2; }) a b; } null"
        ["trace: e"],
      runTestIO
        "a rec set's attributes are the cells in its frame's slots"
        tmpBase
        "let s = rec { f = x: x; l = [ f ]; }; in builtins.attrValues (builtins.removeAttrs s [ \"l\" ]) == s.l"
        (VBool True),
      runTestM "tieFrame answers the cells it filled the slots with" $ do
        codes <- mapM (compileExpr . ELit . NixInt) [1, 2, 3]
        (frame, thunks) <- tieFrame EnvLet Nothing (length codes) (\env -> mapM (mkThunkBc env) codes)
        slotted <- mapM (\i -> unThunk <$> envLookupResolved 0 (fromIntegral i) frame) [0 .. length codes - 1]
        pure (assertEqual "cells" slotted (map unThunk thunks)),
      -- A frame's slots are empty until tieFrame fills them, and C answers
      -- NULL for an empty one; a thunk made by reading one would fault at
      -- its first force, so the read itself refuses.
      runTestM "reading a slot its frame has not filled raises CInvariantViolated" $ do
        outcome <- try (tieFrame EnvLet Nothing 1 (fmap pure . envLookupResolved 0 0))
        pure $ case outcome of
          Left (CInvariantViolated site _) -> assertEqual "site" "nn_env_lookup_resolved" site
          Left other -> Fail ("expected CInvariantViolated, got " <> T.pack (show other))
          Right _ -> Fail "the unfilled slot was read as a thunk",
      -- A level past the parent chain and an index past the slots hold
      -- nothing either, and refuse the same way.
      runTestM "a resolved lookup past the env's parents or slots raises CInvariantViolated" $ do
        cell <- evaluated VNull
        env <- newEnv EnvClosure Nothing [cell]
        outcomes <- mapM (\(level, idx) -> try (envLookupResolved level idx env)) [(1, 0), (0, 1), (maxBound, 0)]
        let site = \case
              Left (CInvariantViolated at _) -> Just at
              _ -> Nothing
        pure (assertEqual "sites" (replicate 3 (Just "nn_env_lookup_resolved")) (map site outcomes))
    ]
  where
    dynamicLetLater =
      let at = testPos 1 1
       in ELet
            [ NamedBinding at (StaticKey "a") (EVar "b"),
              NamedBinding at (DynamicKey (EStr [StrLit "d"])) (ELit (NixInt 3)),
              InheritFrom (EVar "t") [InheritedName at "c"],
              NamedBinding at (StaticKey "b") (ELit (NixInt 1)),
              NamedBinding at (StaticKey "t") (EAttrs False [NamedBinding at (StaticKey "c") (ELit (NixInt 2))] NoCaptureInfo)
            ]
            (EApp (EVar "toString") (EList [EVar "a", EVar "c"]))
            NoCaptureInfo
    quickCheckPasses property = do
      result <- QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} property
      pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))
    genSlotRead = QC.resize 6 ((,,) <$> QC.listOf QC.arbitrary <*> QC.arbitrary <*> QC.listOf QC.arbitrary)
    -- Bindings come out in dependency order, each naming only bindings
    -- before it there, so no binding depends on itself; their declaration
    -- positions are shuffled, so a reference often points forward.
    genFrame = do
      count <- QC.chooseInt (1, 8)
      positions <- QC.shuffle [0 .. count - 1]
      zipWithM genBinding (inits positions) positions
    genBinding earlier pos = do
      value <- case earlier of
        [] -> FrameConstant <$> genConstant
        _ ->
          QC.oneof
            [ FrameConstant <$> genConstant,
              FrameAlias <$> QC.elements earlier,
              FrameOffset <$> QC.elements earlier <*> genConstant
            ]
      passed <- QC.frequency [(3, pure Nothing), (1, Just <$> QC.chooseInt (100, 199))]
      pure (FrameBinding pos value passed)
    genConstant = QC.chooseInt (0, 99)
    -- The value each binding takes, keyed by declaration position, with
    -- the argument @override@ answers in place of its own value.
    frameValues override = foldl' (settle override) Map.empty
    settle override known binding =
      let own = case fbValue binding of
            FrameConstant k -> k
            FrameAlias ref -> Map.findWithDefault 0 ref known
            FrameOffset ref k -> Map.findWithDefault 0 ref known + k
       in Map.insert (fbPos binding) (fromMaybe own (override binding)) known
    -- What @toString@ makes of the frame's values listed in declaration
    -- order.
    frameText override frame = T.unwords (map (T.pack . show) (Map.elems (frameValues override frame)))
    bindingName pos = "x" <> T.pack (show pos)
    valueSource value = case value of
      FrameConstant k -> T.pack (show k)
      FrameAlias ref -> bindingName ref
      FrameOffset ref k -> bindingName ref <> " + " <> T.pack (show k)
    definitions = T.concat . map (\b -> bindingName (fbPos b) <> " = " <> valueSource (fbValue b) <> "; ")
    listed qualify declared = "toString [ " <> T.unwords (map (qualify . bindingName . fbPos) declared) <> " ]"
    letSource declared = "let " <> definitions declared <> "in " <> listed id declared
    recSource declared = "let s = rec { " <> definitions declared <> "}; in " <> listed ("s." <>) declared
    formalsSource declared =
      "(args@{ "
        <> T.intercalate ", " (map (\b -> bindingName (fbPos b) <> " ? " <> valueSource (fbValue b)) declared)
        <> " }: "
        <> listed id declared
        <> ") { "
        <> T.concat (mapMaybe (\b -> (\v -> bindingName (fbPos b) <> " = " <> T.pack (show v) <> "; ") <$> fbPassed b) declared)
        <> "}"

-- ---------------------------------------------------------------------------
-- Tests: Eval - Lists
-- ---------------------------------------------------------------------------

testEvalLists :: IO [Bool]
testEvalLists = do
  putStrLn "eval/lists"
  sequence
    [ runTest "list head" $
        assertEval "head" "builtins.head [ 1 2 3 ]" (VInt 1),
      runTest "list length" $
        assertEval "length" "builtins.length [ 1 2 3 ]" (VInt 3),
      runTest "list concat" $
        assertEval "concat" "builtins.length ([ 1 ] ++ [ 2 3 ])" (VInt 3),
      runTest "head forces only the first element" $
        assertEval "head-lazy" "builtins.head [ 1 (throw \"x\") ]" (VInt 1),
      runTest "tail keeps every element after the first" $
        assertEval "tail" "builtins.tail [ 1 2 3 ] == [ 2 3 ]" (VBool True),
      runTest "tail of a tail" $
        assertEval "tail-tail" "builtins.tail (builtins.tail [ 1 2 3 ]) == [ 3 ]" (VBool True),
      runTest "tail of a one-element list is empty" $
        assertEval "tail-one" "builtins.tail [ 1 ] == [ ]" (VBool True),
      runTest "tail forces none of the elements" $
        assertEval "tail-lazy" "builtins.length (builtins.tail [ (throw \"a\") (throw \"b\") ])" (VInt 1),
      runTest "head of a tail" $
        assertEval "head-tail" "builtins.head (builtins.tail [ 1 2 3 ])" (VInt 2),
      -- The messages 2.24.9 raises: head is its elemAt at index 0 and
      -- tail has its own (primops.cc), a non-list fails in forceList
      -- (eval-inline.hh), and tryEval catches neither.  2.26.0 reworded
      -- both empty-list messages, so nix-instantiate 2.33.2 prints
      -- "'builtins.head' called on an empty list" and the same for tail.
      runTest "head of an empty list" $
        assertEvalError "head-empty" "builtins.head [ ]" "list index 0 is out of bounds",
      runTest "tail of an empty list" $
        assertEvalError "tail-empty" "builtins.tail [ ]" "'tail' called on an empty list",
      runTest "head of the empty list tail returns" $
        assertEvalError "head-tail-empty" "builtins.head (builtins.tail [ 1 ])" "list index 0 is out of bounds",
      runTest "tail of the empty list tail returns" $
        assertEvalError "tail-tail-empty" "builtins.tail (builtins.tail [ 1 ])" "'tail' called on an empty list",
      runTest "head of an integer" $
        assertEvalError "head-int" "builtins.head 1" "expected a list but found an integer: 1",
      runTest "head of a string" $
        assertEvalError "head-string" "builtins.head \"x\"" "expected a list but found a string: \"x\"",
      runTest "tail of null" $
        assertEvalError "tail-null" "builtins.tail null" "expected a list but found null: null",
      runTest "tail of a Boolean" $
        assertEvalError "tail-bool" "builtins.tail true" "expected a list but found a Boolean: true",
      runTest "tryEval does not catch head of an empty list" $
        assertEvalError "head-empty-tryeval" "builtins.tryEval (builtins.head [ ])" "list index 0 is out of bounds",
      runTest "tryEval does not catch tail of a non-list" $
        assertEvalError "tail-int-tryeval" "builtins.tryEval (builtins.tail 1)" "expected a list but found an integer: 1",
      runTest "length of an integer" $
        assertEvalError "length-int" "builtins.length 1" "expected a list but found an integer: 1",
      runTest "length of null" $
        assertEvalError "length-null" "builtins.length null" "expected a list but found null: null",
      runTestM "drop and index on a C list agree with the materialized list" $ do
        result <-
          QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} $
            QC.forAll genCase $ \(values, drops, index) ->
              QC.ioProperty $ do
                ptrs <- mapM cthunkNewComputedInt values
                initial <- clistFromThunks ptrs
                dropped <- foldM (flip clistDrop) initial drops
                observed <- (,,) <$> clistThunks dropped <*> clistLen dropped <*> clistIndex dropped index
                let model = foldl' (flip drop) ptrs drops
                    modelAt i
                      | i < 0 = Nothing
                      | otherwise = listToMaybe (drop i model)
                pure (observed QC.=== (model, length model, modelAt index))
        pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))
    ]
  where
    -- Short lists and a few drops, so the drops and the index land on
    -- both sides of the bounds, negatives included.
    genCount = QC.choose (-2, 12)
    genCase = (,,) <$> QC.resize 10 (QC.listOf QC.arbitrary) <*> QC.resize 4 (QC.listOf genCount) <*> genCount

-- ---------------------------------------------------------------------------
-- Tests: Eval - Lambda
-- ---------------------------------------------------------------------------

testEvalLambda :: IO [Bool]
testEvalLambda = do
  putStrLn "eval/lambda"
  sequence
    [ runTest "identity" $
        assertEval "id" "(x: x) 42" (VInt 42),
      runTest "closure" $
        assertEval "closure" "let f = x: x + 1; in f 5" (VInt 6),
      runTest "set pattern" $
        assertEval "set-pat" "({ a, b }: a + b) { a = 1; b = 2; }" (VInt 3),
      runTest "default param" $
        assertEval "default" "({ a ? 10 }: a) { }" (VInt 10),
      -- Zero-formal set patterns marshal count 0 / entries NULL; the
      -- second force of the same lambda thunk re-reads the entries and
      -- must not underflow the unsigned count.
      runTest "empty formals forced twice" $
        assertEval "empty-formals" "let f = {}: 1; in f {} + f {}" (VInt 2),
      runTest "ellipsis-only formals forced twice" $
        assertEval "ellipsis-only" "let f = { ... }: 1; in f {} + f {}" (VInt 2),
      runTest "named empty formals forced twice" $
        assertEval "named-empty" "let f = args@{ ... }: 1; in f {} + f {}" (VInt 2)
    ]

-- ---------------------------------------------------------------------------
-- Tests: Eval - With
-- ---------------------------------------------------------------------------

testEvalWith :: IO [Bool]
testEvalWith = do
  putStrLn "eval/with"
  sequence
    [ runTest "with basic" $
        assertEval "with" "with { a = 1; }; a" (VInt 1),
      runTest "with lexical wins" $
        assertEval "lexical" "let a = 1; in with { a = 2; }; a" (VInt 1),
      -- EWithVar: with-scoped variable inside lambda (closure trimming must preserve with-scopes)
      runTest "with inside lambda" $
        assertEval
          "with-lambda"
          "with { a = 1; }; let f = x: a + x; in f 2"
          (VInt 3),
      -- Nested with: inner with wins
      runTest "nested with inner wins" $
        assertEval
          "nested-with"
          "with { a = 1; }; with { a = 2; }; a"
          (VInt 2),
      -- Let shadows with
      runTest "let shadows with" $
        assertEval
          "let-shadows-with"
          "with { a = 1; }; let a = 2; in a"
          (VInt 2),
      -- A fallback (nested-path) let/rec binding shadows with too: the
      -- NameBarrier must not be upgraded to a with-variable.
      runTest "barrier let shadows with" $
        assertEval
          "barrier-shadows-with"
          "let a.b = 1; x = 42; in with { x = 99; }; x"
          (VInt 42),
      runTest "barrier rec binding shadows with" $
        assertEval
          "barrier-rec-shadows-with"
          "with { q = 99; }; (rec { c.d = 1; q = 5; b = q; }).b"
          (VInt 5),
      -- Static globals bind at parse time; with never shadows them.
      runTest "with cannot shadow global map" $
        assertEval
          "with-global-map"
          "builtins.typeOf (with { map = 42; }; map)"
          (mkStr "lambda"),
      -- fetchurl is NOT an upstream global: the with-scope must win, or
      -- nixpkgs-style 'with pkgs; fetchurl' would bind the builtin.
      runTest "with shadows non-global fetchurl" $
        assertEval
          "with-fetchurl"
          "with { fetchurl = 42; }; fetchurl"
          (VInt 42),
      -- Builtin fallback: builtins still accessible inside with
      runTest "with builtin fallback" $
        assertEval
          "with-builtin"
          "with { a = 1; }; true"
          (VBool True),
      -- Builtin attr fallback: builtins.length still works inside with
      runTest "with builtin attr fallback" $
        assertEval
          "with-builtin-attr"
          "with { a = 1; }; builtins.length [1 2 3]"
          (VInt 3),
      -- With in rec attrs
      runTest "with in rec attrs" $
        assertEval
          "with-rec"
          "with { a = 1; }; rec { b = a + 1; c = b + 1; }.c"
          (VInt 3),
      -- A rec binds its static names alone, as upstream's ExprAttrs::bindVars
      -- does (nixexpr.cc at 2.24.9): a name only a dynamic key gives is a
      -- with-variable, and no with has it.  The key interpolates more than
      -- a plain string, which upstream's parser and this one read as a
      -- static name.
      runTest "a with-variable named only by a rec's dynamic key is undefined" $
        assertEvalError
          "with-rec-dynamic"
          "rec { ${\"x\" + \"\"} = 1; y = with {}; x; }.y"
          "undefined variable 'x'",
      -- A with-variable in a trimmed closure is looked up in the withs
      -- inside it and the withs around it, and in nothing else.
      runTest "a with-variable no with has is undefined in a trimmed closure" $
        assertEvalError
          "with-trimmed-inner-miss"
          "let b = 1; f = a: with {}; [ a b zzz ]; in builtins.elemAt (f 1) 2"
          "undefined variable 'zzz'"
          `andThen` assertEvalError
            "with-trimmed-outer-miss"
            "with { y = 2; }; let b = 1; f = a: [ a b y zzz ]; r = f 1; in builtins.seq (builtins.elemAt r 2) (builtins.elemAt r 3)"
            "undefined variable 'zzz'",
      -- AST test: parse "with a; b" produces EWithVar
      runTest "parse with produces EWithVar" $
        assertRight "with-ast" (parsedExpr <$> parseNixWithScope (withGlobals ["a"]) testBaseDir testOrigin "with a; b") $ \case
          EWith (EVar "a") (EWithVar "b") -> Pass
          other -> Fail ("expected EWith (EVar a) (EWithVar b), got: " <> T.pack (show other)),
      -- AST test: formal wins over with
      runTest "parse lambda formal wins over with" $
        assertRight "formal-wins" (parsedExpr <$> parseNixWithScope (withGlobals ["a"]) testBaseDir testOrigin "x: with a; x") $ \case
          ELambda _ (EWith _ (EResolvedVar 0 0)) _ -> Pass
          other -> Fail ("expected formal to win, got: " <> T.pack (show other)),
      -- Trimming test: lambda inside with gets CapturesWithScopes
      runTest "with lambda trimmed with CapturesWithScopes" $
        assertRight "with-trim" (parsedExpr <$> parseNixWithScope (withGlobals ["a"]) testBaseDir testOrigin "with a; x: b + x") $ \case
          EWith _ (ELambda _ _ (CapturesWithScopes _)) -> Pass
          other -> Fail ("expected CapturesWithScopes, got: " <> T.pack (show other))
    ]

-- ---------------------------------------------------------------------------
-- Tests: Eval - Builtins
-- ---------------------------------------------------------------------------

testEvalBuiltins :: IO [Bool]
testEvalBuiltins = do
  putStrLn "eval/builtins"
  sequence
    [ runTest "typeOf int" $
        assertEval "typeOf-int" "builtins.typeOf 42" (mkStr "int"),
      runTest "typeOf string" $
        assertEval "typeOf-str" "builtins.typeOf \"hi\"" (mkStr "string"),
      runTest "isNull true" $
        assertEval "isNull-t" "builtins.isNull null" (VBool True),
      runTest "isNull false" $
        assertEval "isNull-f" "builtins.isNull 1" (VBool False),
      runTest "stringLength" $
        assertEval "strlen" "builtins.stringLength \"hello\"" (VInt 5),
      runTest "a builtin taking a string coerces a set as upstream's coerceToString" $
        assertEval "strlen-outpath" "builtins.stringLength { outPath = \"abc\"; }" (VInt 3)
          `andThen` assertEval "strlen-tostring" "builtins.stringLength { __toString = s: \"ab\"; }" (VInt 2)
          `andThen` assertEval "substring-outpath" "builtins.substring 1 1 { outPath = \"abc\"; }" (mkStr "b")
          `andThen` assertEval "discard-outpath" "builtins.unsafeDiscardStringContext { outPath = \"abc\"; }" (mkStr "abc")
          `andThen` assertEval "discard-output-outpath" "builtins.unsafeDiscardOutputDependency { outPath = \"abc\"; }" (mkStr "abc"),
      runTest "a builtin taking a string refuses what coerceToString refuses" $
        assertEvalError "strlen-int" "builtins.stringLength 1" "cannot coerce an integer to a string: 1"
          `andThen` assertEvalError "substring-int" "builtins.substring 0 1 1" "cannot coerce an integer to a string: 1"
          `andThen` assertEvalError "discard-int" "builtins.unsafeDiscardStringContext 1" "cannot coerce an integer to a string: 1"
          `andThen` assertEvalError "discard-output-int" "builtins.unsafeDiscardOutputDependency 1" "cannot coerce an integer to a string: 1"
          `andThen` assertEvalError "add-output-int" "builtins.addDrvOutputDependencies 1" "cannot coerce an integer to a string: 1",
      -- Lexer edge cases pinned to upstream: $${ is literal, ''' escapes to '', \q -> q.
      runTest "$${ does not interpolate (double dollar is literal)" $
        assertEval "dollar-dollar-str" "builtins.stringLength \"a$${b}c\"" (VInt 7),
      runTest "$${ literal in indented string" $
        assertEval "dollar-dollar-ind" "builtins.stringLength ''a$${b}c''" (VInt 7),
      runTest "''' escapes two quotes in indented string" $
        assertEval "triple-quote" "''a'''b'' == \"a''b\"" (VBool True),
      runTest "unknown escape drops backslash (regular string)" $
        assertEval "esc-drop-str" "\"a\\qb\" == \"aqb\"" (VBool True),
      runTest "unknown escape drops backslash (indented string)" $
        assertEval "esc-drop-ind" "''a''\\qb'' == \"aqb\"" (VBool True),
      -- Indentation stripping is upstream's stripIndentation rule for
      -- rule (parser-state.hh at 2.24.9), every case checked against
      -- nix-instantiate 2.33.2.  The scan skips an escape, so the spaces
      -- after an escaped newline are never measured; the strip runs over
      -- escapes like any literal, so those spaces are stripped by the
      -- minimum measured elsewhere.  An interpolation is opaque to both.
      runTest "escaped newline: following spaces are stripped, not measured" $
        assertEval "ind-esc-nl-strip" "''  a''\\n  b''" (mkStr "a\nb"),
      runTest "escaped newline: a shallower line after it is not the minimum" $
        assertEval "ind-esc-nl-indent" "''\n    x''\\n y\n    z''" (mkStr "x\ny\nz"),
      runTest "escaped newline at line start keeps the next line stripped" $
        assertEval "ind-esc-nl-start" "''\n  ''\\n  b''" (mkStr "\nb"),
      runTest "interpolated newline leaves the next chunk mid-line" $
        assertEval "ind-interp-nl" "''\n  ${\"\\n\"}  b''" (mkStr "\n  b"),
      -- The minimum starts at a sentinel, so a string with no measured
      -- line loses every leading space.
      runTest "blank line then a spaces-only line strips to the newline" $
        assertEval "ind-blank-spaces" "''\n\n        ''" (mkStr "\n"),
      runTest "spaces-only line strips to nothing" $
        assertEval "ind-spaces-only" "''\n   ''" (mkStr ""),
      runTest "spaces between opener and closer strip to nothing" $
        assertEval "ind-spaces-same-line" "''   ''" (mkStr ""),
      -- Only a space is indentation: a tab fixes the minimum at its
      -- column and survives.
      runTest "tabs are content, not indentation" $
        assertEval "ind-tabs" "''\n\t\ta\n\t\tb\n''" (mkStr "\t\ta\n\t\tb\n"),
      runTest "a tab after the opener is content" $
        assertEval "ind-opener-tab" "''\t\n  a\n''" (mkStr "\t\n  a\n"),
      -- The last part sheds a final line of spaces after stripping; the
      -- rule runs on that part alone.
      runTest "closing indentation after a blank line is dropped" $
        assertEval "ind-closer-spaces" "''\n  a\n\n    ''" (mkStr "a\n\n"),
      runTest "closing indentation after an interpolation is dropped" $
        assertEval "ind-closer-after-interp" "''\n  ${\"v\"}\n   ''" (mkStr "v\n"),
      runTest "spaces after an interpolation on the last line survive" $
        assertEval "ind-interp-last-line" "''\n  ${\"v\"}   ''" (mkStr "v   "),
      -- The opener consumes its own line's trailing spaces and newline.
      runTest "spaces before the opener's newline are not a line" $
        assertEval "ind-opener-spaces" "''   \n  a\n''" (mkStr "a\n"),
      runTest "an opener line of spaces then the closer is empty" $
        assertEval "ind-opener-only" "''  \n''" (mkStr ""),
      runTest "toString int" $
        assertEval "toStr" "builtins.toString 42" (mkStr "42")
    ]

-- | A list coerced to a string, as upstream's @coerceToString@ joins one
-- under @coerceMore@ (eval.cc at 2.24.9).  Each expected string is
-- nix-instantiate 2.24.9's.
testListCoercion :: IO [Bool]
testListCoercion = do
  putStrLn "eval/list-coercion"
  let joins label source expected = runTest label (assertEval label ("builtins.toString " <> source) (mkStr expected))
      bytes = BS.pack <$> QC.listOf QC.arbitrary
      quickCheckPasses property = do
        result <- QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} property
        pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))
  sequence
    [ joins "elements are joined with spaces" "[ 1 \"a\" 2 ]" "1 a 2",
      joins "an empty list element takes no space after it" "[ [ ] 1 ]" "1",
      joins "an empty list between two elements leaves one space" "[ 1 [ ] 2 ]" "1 2",
      joins "an element before an empty list keeps its space" "[ 1 [ ] ]" "1 ",
      joins "a list holding an empty list is not empty" "[ [ [ ] ] 1 ]" " 1",
      joins "an empty string element keeps its space" "[ \"\" 1 ]" " 1",
      joins "nested lists flatten through the same join" "[ [ 1 [ 2 3 ] ] 4 ]" "1 2 3 4",
      joins "a __toString may return a list" "{ __toString = s: [ 1 2 ]; }" "1 2",
      joins "an outPath may be a list" "{ outPath = [ 1 2 ]; }" "1 2",
      joins "an element whose __toString is an empty list keeps its space" "[ { __toString = s: [ ]; } 1 ]" " 1",
      runTest "interpolation still refuses a list" $
        assertEvalError "interp-list" "\"${[ 1 2 ]}\"" "cannot coerce a list to a string: [ 1 2 ]",
      runTestM "without empty lists the join is intercalate" $
        quickCheckPasses $
          QC.forAll (QC.listOf bytes) $ \parts ->
            joinCoercedList [(part, False) | part <- parts] QC.=== BS.intercalate " " parts,
      runTestM "an empty list before any element changes nothing" $
        quickCheckPasses $
          QC.forAll (QC.listOf bytes) $ \before ->
            QC.forAll (QC.listOf1 bytes) $ \after ->
              let marked = map (,False)
               in joinCoercedList (marked before <> [("", True)] <> marked after) QC.=== joinCoercedList (marked (before <> after))
    ]

-- ---------------------------------------------------------------------------
-- Tests: Eval - Errors
-- ---------------------------------------------------------------------------

testEvalErrors :: IO [Bool]
testEvalErrors = do
  putStrLn "eval/errors"
  sequence
    [ runTest "type error in add (set + list)" $
        assertEvalFail "type-add" "{} + []",
      runTest "call non-function" $
        assertEvalFail "call-non" "42 1",
      runTest "builtins.throw" $
        assertEvalFail "throw" "builtins.throw \"boom\""
    ]

-- ---------------------------------------------------------------------------
-- Tests: Eval - Higher-order builtins
-- ---------------------------------------------------------------------------

testEvalHigherOrder :: IO [Bool]
testEvalHigherOrder = do
  putStrLn "eval/higher-order"
  sequence
    [ -- map
      runTest "map basic" $
        assertEval "map" "builtins.map (x: x + 1) [ 1 2 3 ] == [ 2 3 4 ]" (VBool True),
      runTest "map identity" $
        assertEval "map-id" "builtins.map (x: x) [ 1 2 ] == [ 1 2 ]" (VBool True),
      runTest "map empty" $
        assertEval "map-empty" "builtins.map (x: x) [ ]" (VList emptyCList),
      runTest "map lazy" $
        assertEval "map-lazy" "let xs = builtins.map (x: x * 2) [ 1 (throw \"boom\") 3 ]; in builtins.elemAt xs 0" (VInt 2),
      -- filter
      runTest "filter match" $
        assertEval "filter" "builtins.filter (x: x == 2) [ 1 2 3 ] == [ 2 ]" (VBool True),
      runTest "filter none" $
        assertEval "filter-none" "builtins.filter (x: false) [ 1 2 ] == [ ]" (VBool True),
      -- foldl'
      runTest "foldl' sum" $
        assertEval "foldl-sum" "builtins.foldl' (a: b: a + b) 0 [ 1 2 3 ]" (VInt 6),
      runTest "foldl' string concat" $
        assertEval "foldl-str" "builtins.foldl' (a: b: a + b) \"\" [ \"x\" \"y\" \"z\" ]" (mkStr "xyz"),
      runTest "foldl' empty" $
        assertEval "foldl-empty" "builtins.foldl' (a: b: a + b) 0 [ ]" (VInt 0),
      -- genList
      runTest "genList basic" $
        assertEval "genList" "builtins.genList (i: i * 2) 4 == [ 0 2 4 6 ]" (VBool True),
      runTest "genList zero" $
        assertEval "genList-0" "builtins.genList (i: i) 0" (VList emptyCList),
      runTest "genList lazy" $
        assertEval "genList-lazy" "let xs = builtins.genList (i: if i == 0 then 42 else throw \"boom\") 5; in builtins.elemAt xs 0" (VInt 42),
      -- sort
      runTest "sort ints" $
        assertEval "sort" "builtins.sort (a: b: a < b) [ 3 1 2 ] == [ 1 2 3 ]" (VBool True),
      runTest "sort already sorted" $
        assertEval "sort-sorted" "builtins.sort (a: b: a < b) [ 1 2 3 ] == [ 1 2 3 ]" (VBool True),
      -- Stable: comparator-equal elements keep their input order (std::stable_sort).
      runTest "sort is stable across ties" $
        assertEval
          "sort-stable"
          "builtins.sort (a: b: a.k < b.k) [ { k = 1; v = 1; } { k = 0; v = 2; } { k = 1; v = 3; } { k = 0; v = 4; } ] == [ { k = 0; v = 2; } { k = 0; v = 4; } { k = 1; v = 1; } { k = 1; v = 3; } ]"
          (VBool True),
      runTest "sort preserves order of all-equal elements" $
        assertEval
          "sort-stable-all"
          "builtins.sort (a: b: a.k < b.k) [ { k = 0; v = 1; } { k = 0; v = 2; } { k = 0; v = 3; } ] == [ { k = 0; v = 1; } { k = 0; v = 2; } { k = 0; v = 3; } ]"
          (VBool True),
      -- concatMap
      runTest "concatMap" $
        assertEval "concatMap" "builtins.concatMap (x: [ x (x * 2) ]) [ 1 2 ] == [ 1 2 2 4 ]" (VBool True),
      -- any
      runTest "any true" $
        assertEval "any-t" "builtins.any (x: x == 2) [ 1 2 3 ]" (VBool True),
      runTest "any false" $
        assertEval "any-f" "builtins.any (x: x == 5) [ 1 2 3 ]" (VBool False),
      -- all
      runTest "all true" $
        assertEval "all-t" "builtins.all (x: x > 0) [ 1 2 3 ]" (VBool True),
      runTest "all false" $
        assertEval "all-f" "builtins.all (x: x > 1) [ 1 2 3 ]" (VBool False),
      -- elem
      runTest "elem found" $
        assertEval "elem-t" "builtins.elem 2 [ 1 2 3 ]" (VBool True),
      runTest "elem not found" $
        assertEval "elem-f" "builtins.elem 5 [ 1 2 3 ]" (VBool False),
      -- elemAt
      runTest "elemAt valid" $
        assertEval "elemAt" "builtins.elemAt [ 10 20 30 ] 1" (VInt 20),
      -- The messages 2.24.9 raises (elemAt in primops.cc, forceInt in
      -- eval.cc, forceList in eval-inline.hh).  The index is forced and
      -- checked before the list.  2.26.0 reworded the bounds message;
      -- 2.33.2 prints "'builtins.elemAt' called with index 5 on a list of
      -- size 2".
      runTest "elemAt past the end" $
        assertEvalError "elemAt-oob" "builtins.elemAt [ 1 2 ] 5" "list index 5 is out of bounds",
      runTest "elemAt at the length" $
        assertEvalError "elemAt-length" "builtins.elemAt [ 1 2 ] 2" "list index 2 is out of bounds",
      runTest "elemAt negative index" $
        assertEvalError "elemAt-negative" "builtins.elemAt [ 1 2 ] (-1)" "list index -1 is out of bounds",
      runTest "elemAt on an empty list" $
        assertEvalError "elemAt-empty" "builtins.elemAt [ ] 0" "list index 0 is out of bounds",
      runTest "elemAt of a non-list" $
        assertEvalError "elemAt-int" "builtins.elemAt 1 0" "expected a list but found an integer: 1",
      runTest "elemAt with a string index" $
        assertEvalError "elemAt-string-index" "builtins.elemAt [ 1 2 ] \"x\"" "expected an integer but found a string: \"x\"",
      runTest "elemAt checks the index before the list" $
        assertEvalError "elemAt-index-first" "builtins.elemAt 1 \"x\"" "expected an integer but found a string: \"x\"",
      -- partition
      runTest "partition right" $
        assertEval "partition-right" "(builtins.partition (x: x > 2) [ 1 2 3 4 ]).right == [ 3 4 ]" (VBool True),
      runTest "partition wrong" $
        assertEval "partition-wrong" "(builtins.partition (x: x > 2) [ 1 2 3 4 ]).wrong == [ 1 2 ]" (VBool True),
      -- groupBy
      runTest "groupBy pos" $
        assertEval "groupBy-pos" "(builtins.groupBy (x: if x > 0 then \"pos\" else \"neg\") [ 1 (- 2) 3 ]).pos == [ 1 3 ]" (VBool True),
      runTest "groupBy neg" $
        assertEval "groupBy-neg" "(builtins.groupBy (x: if x > 0 then \"pos\" else \"neg\") [ 1 (- 2) 3 ]).neg == [ (- 2) ]" (VBool True),
      -- attrNames
      runTest "attrNames sorted" $
        assertEval "attrNames" "builtins.attrNames { b = 2; a = 1; c = 3; } == [ \"a\" \"b\" \"c\" ]" (VBool True),
      -- attrValues
      runTest "attrValues count" $
        assertEval "attrValues" "builtins.length (builtins.attrValues { a = 1; b = 2; })" (VInt 2),
      -- hasAttr
      runTest "hasAttr true" $
        assertEval "hasAttr-t" "builtins.hasAttr \"a\" { a = 1; }" (VBool True),
      runTest "hasAttr false" $
        assertEval "hasAttr-f" "builtins.hasAttr \"z\" { a = 1; }" (VBool False),
      -- getAttr
      runTest "getAttr" $
        assertEval "getAttr" "builtins.getAttr \"a\" { a = 42; }" (VInt 42),
      runTest "getAttr missing" $
        assertEvalFail "getAttr-miss" "builtins.getAttr \"z\" { a = 1; }",
      -- removeAttrs
      runTest "removeAttrs" $
        assertEval "removeAttrs" "builtins.attrNames (builtins.removeAttrs { a = 1; b = 2; c = 3; } [ \"b\" ]) == [ \"a\" \"c\" ]" (VBool True),
      -- intersectAttrs
      runTest "intersectAttrs" $
        assertEval "intersectAttrs" "(builtins.intersectAttrs { a = 1; b = 2; } { b = 20; c = 30; }).b" (VInt 20),
      runTest "intersectAttrs keys" $
        assertEval "intersectAttrs-keys" "builtins.attrNames (builtins.intersectAttrs { a = 1; b = 2; } { b = 20; c = 30; }) == [ \"b\" ]" (VBool True),
      -- catAttrs
      runTest "catAttrs" $
        assertEval "catAttrs" "builtins.catAttrs \"a\" [ { a = 1; } { b = 2; } { a = 3; } ] == [ 1 3 ]" (VBool True),
      -- listToAttrs
      runTest "listToAttrs" $
        assertEval "listToAttrs" "(builtins.listToAttrs [ { name = \"x\"; value = 1; } { name = \"y\"; value = 2; } ]).x" (VInt 1),
      -- prim_listToAttrs (primops.cc) looks value up only for a name it
      -- has not seen, and getAttr (primops.cc at 2.24.9, EvalState::getAttr
      -- in eval.cc at 2.36.0) words a missing attribute.
      runTest "listToAttrs reads no value for a name already seen" $
        assertEval "listToAttrs-repeat" "builtins.listToAttrs [ { name = \"a\"; value = 1; } { name = \"a\"; } ] == { a = 1; }" (VBool True),
      runTest "listToAttrs reads every element's name" $
        assertEvalError "listToAttrs-repeat-name" "builtins.listToAttrs [ { name = \"a\"; value = 1; } { name = throw \"n\"; } ]" "n",
      runTest "listToAttrs refuses an element without a name" $
        assertEvalError "listToAttrs-no-name" "builtins.listToAttrs [ { name = \"a\"; value = 1; } { value = 2; } ]" "attribute 'name' missing",
      runTest "listToAttrs refuses a first element of a name without a value" $
        assertEvalError "listToAttrs-no-value" "builtins.listToAttrs [ { name = \"a\"; } { name = \"a\"; value = 1; } ]" "attribute 'value' missing",
      runTest "listToAttrs refuses an element that is not a set" $
        assertEvalError "listToAttrs-element" "builtins.listToAttrs [ 1 ]" "expected a set but found an integer: 1",
      runTest "listToAttrs refuses an argument that is not a list" $
        assertEvalError "listToAttrs-argument" "builtins.listToAttrs 1" "expected a list but found an integer: 1",
      -- substring
      runTest "substring basic" $
        assertEval "substr" "builtins.substring 1 2 \"hello\"" (mkStr "el"),
      runTest "substring clamped" $
        assertEval "substr-clamp" "builtins.substring 3 100 \"hello\"" (mkStr "lo"),
      -- concatStringsSep
      runTest "concatStringsSep" $
        assertEval "concatSep" "builtins.concatStringsSep \", \" [ \"a\" \"b\" \"c\" ]" (mkStr "a, b, c"),
      -- Partial application
      runTest "partial application" $
        assertEval "partial" "let f = builtins.map (x: x + 1); in f [ 1 2 3 ] == [ 2 3 4 ]" (VBool True)
    ]

-- ---------------------------------------------------------------------------
-- Tests: Eval - Builtin argument order
-- ---------------------------------------------------------------------------

-- | A primop that forces an argument later than it is applied takes it
-- unforced and forces it where upstream does.  Each expectation follows
-- upstream's primops.cc and eval.cc at 2.24.9: with only the first
-- argument throwing, a collection that is empty never forces it, and
-- with both throwing, the argument upstream forces first is the error.
testBuiltinArgumentOrder :: IO [Bool]
testBuiltinArgumentOrder = do
  putStrLn "eval/builtin-argument-order"
  let yields source expected = runTest source (assertEval source source expected)
      fails source expected = runTest (source <> " fails with " <> expected) (assertEvalError source source expected)
  sequence $
    [ yields "map (throw \"a\") [ ]" (VList emptyCList),
      yields "builtins.filter (throw \"a\") [ ]" (VList emptyCList),
      yields "builtins.sort (throw \"a\") [ ]" (VList emptyCList),
      yields "builtins.mapAttrs (throw \"a\") { } == { }" (VBool True),
      yields "builtins.elem (throw \"a\") [ ]" (VBool False)
    ]
      <> [ fails source "b"
         | source <-
             [ "map (throw \"a\") (throw \"b\")",
               "builtins.filter (throw \"a\") (throw \"b\")",
               "builtins.genList (throw \"a\") (throw \"b\")",
               "builtins.sort (throw \"a\") (throw \"b\")",
               "builtins.mapAttrs (throw \"a\") (throw \"b\")",
               "builtins.elem (throw \"a\") (throw \"b\")",
               "builtins.elemAt (throw \"a\") (throw \"b\")",
               "builtins.filterSource (throw \"a\") (throw \"b\")",
               "builtins.scopedImport (throw \"a\") (throw \"b\")"
             ]
         ]
      <> [ -- genList forces its function even for no elements, and elem
           -- forces the needle before the element it compares.
           fails "builtins.genList (throw \"a\") 0" "a",
           fails "builtins.elem (throw \"a\") [ (throw \"b\") ]" "a",
           -- mapAttrs never forces its function itself.
           yields "builtins.attrNames (builtins.mapAttrs (throw \"a\") { x = 1; }) == [ \"x\" ]" (VBool True),
           -- Forced where upstream forces it, a function argument is
           -- checked there as forceFunction checks it, functors included.
           fails "builtins.length (map 1 [ 1 ])" "expected a function but found an integer: 1",
           fails "builtins.length (builtins.genList 1 1)" "expected a function but found an integer: 1",
           yields "map { __functor = self: x: x + 1; } [ 1 ] == [ 2 ]" (VBool True),
           -- zipAttrsWith forces and checks its function before it forces
           -- the list, so the function fails first whatever the list holds,
           -- and a partial application checks nothing.
           fails "builtins.zipAttrsWith 1 [ ]" "expected a function but found an integer: 1",
           fails "builtins.attrNames (builtins.zipAttrsWith 1 [ { a = 1; } ])" "expected a function but found an integer: 1",
           fails "builtins.zipAttrsWith 1 [ 1 ]" "expected a function but found an integer: 1",
           fails "builtins.zipAttrsWith 1 (throw \"b\")" "expected a function but found an integer: 1",
           fails "builtins.zipAttrsWith (throw \"a\") (throw \"b\")" "a",
           fails "builtins.zipAttrsWith (throw \"a\") 1" "a",
           fails "builtins.zipAttrsWith (throw \"a\") [ ]" "a",
           yields "builtins.zipAttrsWith (n: vs: vs) [ ] == { }" (VBool True),
           yields "builtins.zipAttrsWith { __functor = self: n: vs: n; } [ { a = 1; } ] == { a = \"a\"; }" (VBool True),
           yields "builtins.typeOf (builtins.zipAttrsWith 1)" (mkStr "lambda"),
           -- foldl' hands its initial accumulator to the first step
           -- unforced, and forces it itself only for an empty list.
           yields "builtins.foldl' (acc: x: x) (throw \"a\") [ 1 ]" (VInt 1),
           fails "builtins.foldl' (acc: x: x) (throw \"a\") (throw \"c\")" "c",
           fails "builtins.foldl' (acc: x: x) (throw \"a\") [ ]" "a",
           -- A partial application holds the argument unforced however it
           -- is applied: from source, by a higher-order builtin with an
           -- unforced element, or with a value already forced.
           yields "builtins.typeOf (map (throw \"a\"))" (mkStr "lambda"),
           yields "builtins.elemAt (map (map (throw \"a\")) [ [ ] ]) 0 == [ ]" (VBool True),
           yields "builtins.all (builtins.elem (throw \"a\")) [ [ ] ]" (VBool False),
           yields "builtins.foldl' builtins.elem 1 [ [ 1 ] [ true ] ]" (VBool True)
         ]
      -- A set with __functor is a function to every builtin that calls
      -- one, as upstream Nix calls one.
      <> [ yields "builtins.any { __functor = self: x: x; } [ true ]" (VBool True),
           yields "builtins.filter { __functor = self: x: x > 1; } [ 1 2 ] == [ 2 ]" (VBool True),
           yields "builtins.sort { __functor = self: a: b: a < b; } [ 2 1 ] == [ 1 2 ]" (VBool True),
           yields "builtins.foldl' { __functor = self: acc: x: acc + x; } 0 [ 1 2 ] == 3" (VBool True),
           yields "builtins.length (builtins.genericClosure { startSet = [ { key = 1; } ]; operator = { __functor = self: item: [ ]; }; }) == 1" (VBool True)
         ]
      -- partition forces each element before the function sees it.
      <> [ fails "builtins.partition (x: true) [ (throw \"element\") ]" "element",
           yields "builtins.any (x: true) [ (throw \"element\") ]" (VBool True)
         ]

-- ---------------------------------------------------------------------------
-- Tests: Batch 1 - Trivial pure builtins + constants
-- ---------------------------------------------------------------------------

testBatch1 :: IO [Bool]
testBatch1 = do
  putStrLn "eval/builtins-batch1"
  sequence
    [ -- isPath
      runTest "isPath true" $
        assertEval "isPath-t" "builtins.isPath ./foo" (VBool True),
      runTest "isPath false" $
        assertEval "isPath-f" "builtins.isPath \"foo\"" (VBool False),
      -- ceil
      runTest "ceil float" $
        assertEval "ceil" "builtins.ceil 1.2" (VInt 2),
      runTest "ceil int passthrough" $
        assertEval "ceil-int" "builtins.ceil 5" (VInt 5),
      runTest "ceil negative" $
        assertEval "ceil-neg" "builtins.ceil (- 1.7)" (VInt (-1)),
      runTest "ceil type error" $
        assertEvalFail "ceil-err" "builtins.ceil \"hi\"",
      -- floor
      runTest "floor float" $
        assertEval "floor" "builtins.floor 1.7" (VInt 1),
      runTest "floor int passthrough" $
        assertEval "floor-int" "builtins.floor 5" (VInt 5),
      runTest "floor negative" $
        assertEval "floor-neg" "builtins.floor (- 1.2)" (VInt (-2)),
      -- NaN, infinity, and out-of-range floats are eval errors (Nix 2.24),
      -- not whatever Int64 Haskell's unchecked conversion produces.
      runTest "ceil NaN fails" $
        assertEvalFail "ceil-nan" "builtins.ceil ((1.0e308 * 10) * 0.0)",
      runTest "floor infinity fails" $
        assertEvalFail "floor-inf" "builtins.floor (1.0e308 * 10)",
      runTest "ceil out of integer range fails" $
        assertEvalFail "ceil-range" "builtins.ceil 1.0e300",
      -- seq
      runTest "seq returns second" $
        assertEval "seq" "builtins.seq 1 42" (VInt 42),
      -- trace
      runTest "trace returns second" $
        assertEval "trace" "builtins.trace \"msg\" 42" (VInt 42),
      -- unsafeDiscardStringContext
      runTest "discardContext" $
        assertEval "discard" "builtins.unsafeDiscardStringContext \"hello\"" (mkStr "hello"),
      -- unsafeDiscardOutputDependency
      runTest "discardOutputDep" $
        assertEval "discardOut" "builtins.unsafeDiscardOutputDependency \"hello\"" (mkStr "hello"),
      -- baseNameOf
      runTest "baseNameOf string" $
        assertEval "baseName-str" "builtins.baseNameOf \"/foo/bar/baz\"" (mkStr "baz"),
      runTest "baseNameOf path" $
        assertEval "baseName-path" "builtins.baseNameOf ./foo/bar" (mkStr "bar"),
      runTest "baseNameOf no slash" $
        assertEval "baseName-flat" "builtins.baseNameOf \"filename\"" (mkStr "filename"),
      -- Upstream drops one trailing slash, not every one.
      runTest "baseNameOf drops a single trailing slash" $
        foldr
          andThen
          Pass
          [ assertEval (T.pack (show subject)) ("builtins.baseNameOf \"" <> subject <> "\"") (mkStr expected)
          | (subject, expected) <- [("a/", "a"), ("a//", ""), ("a/b//", ""), ("/a/", "a"), ("/", ""), ("//", ""), ("", "")]
          ],
      runTest "baseNameOf type error" $
        assertEvalFail "baseName-err" "builtins.baseNameOf 42",
      -- dirOf
      runTest "dirOf string" $
        assertEval "dirOf-str" "builtins.dirOf \"/foo/bar/baz\"" (mkStr "/foo/bar"),
      runTest "dirOf no slash" $
        assertEval "dirOf-flat" "builtins.dirOf \"filename\"" (mkStr "."),
      runTest "dirOf root-level path" $
        assertEval "dirOf-root" "builtins.dirOf \"/foo\"" (mkStr "/"),
      -- Path VALUES arrive native-spelled when eval's base dir is a
      -- native path (the CLI case); the path-operand splits must be
      -- separator-aware.  The forward-slash tests above use a '/'
      -- base and cannot see this.
      runTestM "baseNameOf on a native-based path value" $ do
        cwd <- Dir.getCurrentDirectory
        result <- evalNixIO cwd "builtins.baseNameOf ./regression-name.nix"
        pure $ case result of
          Right v
            | v == mkStr "regression-name.nix" -> Pass
            | otherwise -> Fail ("wrong basename: " <> T.pack (show v))
          Left err -> Fail ("eval failed: " <> T.pack (show err)),
      -- The path-value spec: absolute, lexically canonical, and
      -- slash-spelled regardless of the base dir's native spelling.
      runTestM "path values are slash-canonical from a native base" $ do
        cwd <- Dir.getCurrentDirectory
        result <- evalNixIO cwd "toString ./spec-name.nix"
        let expected = mkStr (T.replace "\\" "/" (T.pack cwd) <> "/spec-name.nix")
        pure $ case result of
          Right v
            | v == expected -> Pass
            | otherwise -> Fail ("wrong spelling: " <> T.pack (show v))
          Left err -> Fail ("eval failed: " <> T.pack (show err)),
      runTest "canonPathValue folds platform separators only" $
        let folded = canonPathValue "C:\\a\\.\\b"
            expectedByPlatform =
              if SI.os == "mingw32"
                then "C:/a/b" -- '\\' is a separator here and folds
                else "C:\\a\\.\\b" -- '\\' is a file-name character, preserved
         in assertEqual "platform fold" expectedByPlatform folded,
      runTestM "dirOf on a native-based path value" $ do
        cwd <- Dir.getCurrentDirectory
        dirResult <- evalNixIO cwd "builtins.dirOf ./sub/regression-name.nix"
        parentResult <- evalNixIO cwd "./sub"
        pure $
          if dirResult == parentResult
            then Pass
            else
              Fail
                ( "dirOf mismatch: "
                    <> T.pack (show dirResult)
                    <> " vs "
                    <> T.pack (show parentResult)
                ),
      -- appendContext: a key that is not a store path must refuse, as
      -- upstream does - a fabricated identity would flow into
      -- derivation inputs.
      runTest "appendContext rejects a non-store-path key" $
        assertEvalFail
          "appendContext-badkey"
          "builtins.appendContext \"x\" { \"not-a-store-path\" = { path = true; }; }",
      runTest "appendContext accepts a store-path key" $
        assertEval
          "appendContext-ok"
          "if (builtins.appendContext \"x\" { \"/nix/store/00000000000000000000000000000000-y\" = { path = true; }; }) == \"x\" then \"ok\" else \"no\""
          (mkStr "ok"),
      -- storePath: the hash component is charset-validated like every
      -- other store-path parse boundary ('E' is outside nix-base32), and
      -- refused with upstream's StorePath constructor text (path.cc at
      -- 2.24.9).
      runTest "storePath rejects a non-base32 hash" $
        assertEvalError
          "storePath-badhash"
          "builtins.storePath \"/nix/store/EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE-x\""
          "store path 'EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE-x' contains illegal base-32 character 'E'",
      runTest "storePath refuses a path outside the store" $
        assertEvalError "storePath-outside" "builtins.storePath \"/x\"" "path '/x' is not in the Nix store",
      runTest "storePath refuses the store directory itself" $
        assertEvalError "storePath-storedir" "builtins.storePath \"/nix/store/\"" "path '/nix/store' is not in the Nix store",
      runTest "storePath refuses a component too short for a hash" $
        assertEvalError "storePath-short" "builtins.storePath \"/nix/store/x\"" "'x' is too short to be a valid store path",
      runTest "storePath refuses an empty name" $
        assertEvalError "storePath-empty-name" "builtins.storePath \"/nix/store/00000000000000000000000000000000-\"" "path '00000000000000000000000000000000-' is not a valid store path: name must not be empty",
      runTest "storePath refuses an illegal name" $
        assertEvalError "storePath-bad-name" "builtins.storePath \"/nix/store/00000000000000000000000000000000-a b\"" "path '00000000000000000000000000000000-a b' is not a valid store path: name 'a b' contains illegal character ' '",
      runTest "storePath accepts a valid store path" $
        assertEval
          "storePath-ok"
          "if (builtins.storePath \"/nix/store/00000000000000000000000000000000-x\") == \"/nix/store/00000000000000000000000000000000-x\" then \"ok\" else \"no\""
          (mkStr "ok"),
      runTest "dirOf root" $
        assertEval "dirOf-slash" "builtins.dirOf \"/\"" (mkStr "/"),
      -- concatLists
      runTest "concatLists basic" $
        assertEval "concatLists" "builtins.concatLists [ [ 1 2 ] [ 3 ] [ 4 5 ] ] == [ 1 2 3 4 5 ]" (VBool True),
      runTest "concatLists empty" $
        assertEval "concatLists-empty" "builtins.concatLists [ ]" (VList emptyCList),
      runTest "concatLists type error" $
        assertEvalFail "concatLists-err" "builtins.concatLists [ 1 2 ]",
      -- lessThan
      runTest "lessThan true" $
        assertEval "lt-t" "builtins.lessThan 1 2" (VBool True),
      runTest "lessThan false" $
        assertEval "lt-f" "builtins.lessThan 2 1" (VBool False),
      runTest "lessThan strings" $
        assertEval "lt-str" "builtins.lessThan \"a\" \"b\"" (VBool True),
      -- List < decides at the first UNEQUAL pair, as upstream: a pair
      -- where < holds in neither direction (NaN) decides False rather
      -- than being skipped as equal.
      -- Distinct NaN values: a shared binding would be skipped as equal
      -- via reference identity, which upstream's eqValues also does.
      runTest "list compare decides at the first unequal pair" $
        assertEval "lt-list-nan" "let inf = 1.0e308 * 10; in [ (inf - inf) 1 ] < [ (inf - inf) 2 ]" (VBool False),
      runTest "list compare skips equal prefixes" $
        assertEval "lt-list-prefix" "[ 1 2 ] < [ 1 3 ]" (VBool True),
      -- Constants
      runTest "storeDir" $
        assertEval "storeDir" "builtins.storeDir" (mkStr defaultStoreDirText),
      runTest "nixVersion" $
        assertEval "nixVersion" "builtins.nixVersion" (mkStr "2.24.9"),
      runTest "langVersion" $
        assertEval "langVersion" "builtins.langVersion" (VInt 6),
      runTest "nixPath" $
        assertEval "nixPath" "builtins.nixPath" (VList emptyCList)
    ]

-- ---------------------------------------------------------------------------
-- Tests: Batch 2 - Arithmetic + bitwise builtins
-- ---------------------------------------------------------------------------

testBatch2 :: IO [Bool]
testBatch2 = do
  putStrLn "eval/builtins-batch2"
  sequence
    [ -- add
      runTest "add ints" $
        assertEval "add-int" "builtins.add 3 4" (VInt 7),
      runTest "add int+float" $
        assertEval "add-mixed" "builtins.add 1 2.5" (VFloat 3.5),
      runTest "add type error" $
        assertEvalFail "add-err" "builtins.add \"a\" 1",
      -- sub
      runTest "sub ints" $
        assertEval "sub-int" "builtins.sub 10 3" (VInt 7),
      runTest "sub float" $
        assertEval "sub-float" "builtins.sub 5.5 2.0" (VFloat 3.5),
      -- mul
      runTest "mul ints" $
        assertEval "mul-int" "builtins.mul 3 4" (VInt 12),
      runTest "mul float" $
        assertEval "mul-float" "builtins.mul 2 3.0" (VFloat 6.0),
      -- div
      runTest "div ints" $
        assertEval "div-int" "builtins.div 10 3" (VInt 3),
      runTest "div float" $
        assertEval "div-float" "builtins.div 7.0 2.0" (VFloat 3.5),
      runTest "div by zero" $
        assertEvalFail "div-zero" "builtins.div 1 0",
      -- bitAnd
      runTest "bitAnd" $
        assertEval "bitAnd" "builtins.bitAnd 12 10" (VInt 8),
      runTest "bitAnd type error" $
        assertEvalFail "bitAnd-err" "builtins.bitAnd 1.0 2",
      -- bitOr
      runTest "bitOr" $
        assertEval "bitOr" "builtins.bitOr 12 10" (VInt 14),
      -- bitXor
      runTest "bitXor" $
        assertEval "bitXor" "builtins.bitXor 12 10" (VInt 6)
    ]

-- ---------------------------------------------------------------------------
-- Tests: Batch 3 - Attrset higher-order builtins
-- ---------------------------------------------------------------------------

testBatch3 :: IO [Bool]
testBatch3 = do
  putStrLn "eval/builtins-batch3"
  let functionArgsError = "'functionArgs' requires a function"
  sequence
    [ -- mapAttrs
      runTest "mapAttrs basic" $
        assertEval "mapAttrs" "(builtins.mapAttrs (name: val: val + 1) { a = 1; b = 2; }).a" (VInt 2),
      runTest "mapAttrs name usage" $
        assertEval "mapAttrs-name" "(builtins.mapAttrs (name: val: name) { a = 1; }).a" (mkStr "a"),
      runTest "mapAttrs type error" $
        assertEvalFail "mapAttrs-err" "builtins.mapAttrs (n: v: v) [ 1 ]",
      runTest "mapAttrs lazy" $
        assertEval "mapAttrs-lazy" "let s = builtins.mapAttrs (k: v: if k == \"a\" then v else throw \"boom\") { a = 1; b = 2; }; in s.a" (VInt 1),
      -- functionArgs
      runTest "functionArgs set pattern" $
        assertEval "funcArgs" "(builtins.functionArgs ({ a, b ? 1 }: a)).b" (VBool True),
      runTest "functionArgs no default" $
        assertEval "funcArgs-nodef" "(builtins.functionArgs ({ a, b ? 1 }: a)).a" (VBool False),
      runTest "functionArgs simple lambda" $
        assertEqual "funcArgs-simple" (evalNix "{ }") (evalNix "builtins.functionArgs (x: x)"),
      -- Upstream's prim_functionArgs message (primops.cc at 2.24.9), the
      -- same whatever the argument, and tryEval does not catch it.
      runTest "functionArgs of a set is upstream's error" $
        assertEvalError "funcArgs-set" "builtins.functionArgs {}" functionArgsError,
      runTest "functionArgs of a path is upstream's error" $
        assertEvalError "funcArgs-path" "builtins.functionArgs ./foo" functionArgsError,
      runTest "functionArgs of an integer is upstream's error" $
        assertEvalError "funcArgs-int" "builtins.functionArgs 42" functionArgsError,
      runTest "functionArgs of a string is upstream's error" $
        assertEvalError "funcArgs-string" "builtins.functionArgs \"s\"" functionArgsError,
      runTest "tryEval does not catch the functionArgs error" $
        assertEvalError "funcArgs-tryEval" "builtins.tryEval (builtins.functionArgs {})" functionArgsError,
      -- The builtins set is observable (hasAttr/attrNames), so it must
      -- match upstream's primop set exactly: no mod/min/max/
      -- setFunctionArgs extensions (nixpkgs lib defines those in Nix),
      -- and functionArgs never consults __functionArgs - a functor set
      -- is an error, as upstream throws (lib.functionArgs unwraps
      -- functor sets itself before reaching the builtin).
      runTest "no invented arithmetic builtins" $
        assertEval
          "no-fake-builtins"
          "!(builtins ? mod) && !(builtins ? min) && !(builtins ? max) && !(builtins ? setFunctionArgs)"
          (VBool True),
      runTest "functionArgs rejects functor sets" $
        assertEvalFail
          "funcArgs-functor-err"
          "builtins.functionArgs { __functor = self: x: x; __functionArgs = { a = false; }; }",
      -- zipAttrsWith
      runTest "zipAttrsWith basic" $
        assertEval
          "zipAttrs"
          "(builtins.zipAttrsWith (name: vals: builtins.head vals) [ { a = 1; } { a = 2; b = 3; } ]).b"
          (VInt 3),
      runTest "zipAttrsWith collect" $
        assertEval
          "zipAttrs-collect"
          "builtins.length (builtins.zipAttrsWith (name: vals: vals) [ { a = 1; } { a = 2; } ]).a"
          (VInt 2),
      -- Upstream fills each name's list from the list's sets first to
      -- last (prim_zipAttrsWith in primops.cc).
      runTest "zipAttrsWith gathers each name's values in list order" $
        assertEval
          "zipAttrs-order"
          "builtins.zipAttrsWith (name: vals: [ name ] ++ vals) [ { zb = 1; a = 2; } { a = 3; } { c = 4; zb = 5; a = 6; } ] == { a = [ \"a\" 2 3 6 ]; c = [ \"c\" 4 ]; zb = [ \"zb\" 1 5 ]; }"
          (VBool True),
      runTest "mapAttrs passes each name with its own value" $
        assertEval
          "mapAttrs-pairs"
          "builtins.mapAttrs (name: val: [ name val ]) { zb = 1; a = 2; c = 3; } == { a = [ \"a\" 2 ]; c = [ \"c\" 3 ]; zb = [ \"zb\" 1 ]; }"
          (VBool True)
    ]

-- ---------------------------------------------------------------------------
-- Tests: Batch 4 - String operations
-- ---------------------------------------------------------------------------

testBatch4 :: IO [Bool]
testBatch4 = do
  putStrLn "eval/builtins-batch4"
  sequence
    [ -- replaceStrings
      runTest "replaceStrings basic" $
        assertEval "replace" "builtins.replaceStrings [ \"o\" ] [ \"0\" ] \"foobar\"" (mkStr "f00bar"),
      runTest "replaceStrings multi" $
        assertEval "replace-multi" "builtins.replaceStrings [ \"a\" \"b\" ] [ \"A\" \"B\" ] \"abc\"" (mkStr "ABc"),
      runTest "replaceStrings empty from" $
        assertEval "replace-empty" "builtins.replaceStrings [ \"\" ] [ \"x\" ] \"ab\"" (mkStr "xaxbx"),
      runTest "replaceStrings no match" $
        assertEval "replace-nomatch" "builtins.replaceStrings [ \"z\" ] [ \"Z\" ] \"abc\"" (mkStr "abc"),
      -- Match-gated forcing: an unmatched replacement is never evaluated.
      runTest "replaceStrings never forces an unmatched replacement" $
        assertEval "replace-lazy" "builtins.replaceStrings [ \"z\" ] [ (builtins.throw \"unused\") ] \"abc\"" (mkStr "abc"),
      -- compareVersions
      runTest "compareVersions equal" $
        assertEval "cmpVer-eq" "builtins.compareVersions \"1.2.3\" \"1.2.3\"" (VInt 0),
      runTest "compareVersions less" $
        assertEval "cmpVer-lt" "builtins.compareVersions \"1.2\" \"1.3\"" (VInt (-1)),
      runTest "compareVersions greater" $
        assertEval "cmpVer-gt" "builtins.compareVersions \"2.0\" \"1.9\"" (VInt 1),
      runTest "compareVersions type error" $
        assertEvalFail "cmpVer-err" "builtins.compareVersions 1 2",
      runTest "compareVersions treats - as a separator" $
        assertEval "cmpVer-dash" "builtins.compareVersions \"1.0-2\" \"1.0.2\"" (VInt 0),
      -- splitVersion
      runTest "splitVersion basic" $
        assertEval "splitVer" "builtins.splitVersion \"1.2.3\" == [ \"1\" \"2\" \"3\" ]" (VBool True),
      runTest "splitVersion pre" $
        assertEval "splitVer-pre" "builtins.splitVersion \"1.2pre\" == [ \"1\" \"2\" \"pre\" ]" (VBool True),
      runTest "splitVersion type error" $
        assertEvalFail "splitVer-err" "builtins.splitVersion 42",
      -- parseDrvName
      runTest "parseDrvName basic" $
        assertEval "parseDrv" "(builtins.parseDrvName \"hello-1.2.3\").name" (mkStr "hello"),
      runTest "parseDrvName version" $
        assertEval "parseDrv-ver" "(builtins.parseDrvName \"hello-1.2.3\").version" (mkStr "1.2.3"),
      runTest "parseDrvName no version" $
        assertEval "parseDrv-nover" "(builtins.parseDrvName \"hello\").version" (mkStr ""),
      runTest "parseDrvName type error" $
        assertEvalFail "parseDrv-err" "builtins.parseDrvName 42"
    ]

-- ---------------------------------------------------------------------------
-- Tests: Batch 5 - Serialization + hashing
-- ---------------------------------------------------------------------------

testBatch5 :: IO [Bool]
testBatch5 = do
  putStrLn "eval/builtins-batch5"
  sequence
    [ -- toJSON
      runTest "toJSON int" $
        assertEval "toJSON-int" "builtins.toJSON 42" (mkStr "42"),
      runTest "toJSON string" $
        assertEval "toJSON-str" "builtins.toJSON \"hello\"" (mkStr "\"hello\""),
      runTest "toJSON null" $
        assertEval "toJSON-null" "builtins.toJSON null" (mkStr "null"),
      runTest "toJSON bool" $
        assertEval "toJSON-bool" "builtins.toJSON true" (mkStr "true"),
      runTest "toJSON list" $
        assertEval "toJSON-list" "builtins.toJSON [ 1 2 3 ]" (mkStr "[1,2,3]"),
      runTest "toJSON attrs" $
        assertEval "toJSON-attrs" "builtins.toJSON { a = 1; }" (mkStr "{\"a\":1}"),
      runTest "toJSON lambda error" $
        assertEvalFail "toJSON-fn" "builtins.toJSON (x: x)",
      -- fromJSON
      runTest "fromJSON int" $
        assertEval "fromJSON-int" "builtins.fromJSON \"42\"" (VInt 42),
      runTest "fromJSON string" $
        assertEval "fromJSON-str" "builtins.fromJSON \"\\\"hello\\\"\"" (mkStr "hello"),
      runTest "fromJSON null" $
        assertEval "fromJSON-null" "builtins.fromJSON \"null\"" VNull,
      runTest "fromJSON bool" $
        assertEval "fromJSON-bool" "builtins.fromJSON \"true\"" (VBool True),
      runTest "fromJSON array" $
        assertEval "fromJSON-arr" "builtins.length (builtins.fromJSON \"[1,2,3]\")" (VInt 3),
      runTest "fromJSON object" $
        assertEval "fromJSON-obj" "(builtins.fromJSON \"{\\\"a\\\": 1}\").a" (VInt 1),
      runTest "fromJSON roundtrip" $
        assertEval "fromJSON-rt" "let x = builtins.fromJSON (builtins.toJSON { a = 1; b = [ 2 3 ]; }); in x.a == 1 && x.b == [ 2 3 ]" (VBool True),
      runTest "fromJSON invalid" $
        assertEvalFail "fromJSON-bad" "builtins.fromJSON \"not json\"",
      -- The integer range as json-to-value.cc has it at 2.25 and after.
      runTest "fromJSON reads the largest int" $
        assertEval "fromJSON-int-max" "builtins.fromJSON \"9223372036854775807\"" (VInt 9223372036854775807),
      runTest "fromJSON reads the smallest int" $
        assertEval "fromJSON-int-min" "builtins.fromJSON \"-9223372036854775808\"" (VInt (-9223372036854775808)),
      runTest "fromJSON refuses one past the largest int" $
        assertEvalError "fromJSON-past-int" "builtins.fromJSON \"9223372036854775808\"" "unsigned json number 9223372036854775808 outside of Nix integer range",
      runTest "fromJSON refuses the largest unsigned" $
        assertEvalError "fromJSON-uint-max" "builtins.fromJSON \"18446744073709551615\"" "unsigned json number 18446744073709551615 outside of Nix integer range",
      runTest "fromJSON refuses an unsigned number inside a list" $
        assertEvalError "fromJSON-uint-list" "builtins.fromJSON \"[1,[18446744073709551615]]\"" "unsigned json number 18446744073709551615 outside of Nix integer range",
      runTest "fromJSON refuses an unsigned number inside a set" $
        assertEvalError "fromJSON-uint-set" "builtins.fromJSON ''{\"a\":18446744073709551615}''" "unsigned json number 18446744073709551615 outside of Nix integer range",
      runTest "fromJSON names the first such number in the document, not in key order" $
        assertEvalError "fromJSON-uint-order" "builtins.fromJSON ''{\"b\":18446744073709551615,\"a\":9223372036854775808}''" "unsigned json number 18446744073709551615 outside of Nix integer range",
      runTest "fromJSON refuses such a number ahead of a later syntax error" $
        assertEvalError "fromJSON-uint-syntax" "builtins.fromJSON \"[18446744073709551615,nonsense\"" "unsigned json number 18446744073709551615 outside of Nix integer range",
      -- nlohmann's grammar (json-to-value.cc at 2.24.9), strict RFC 8259:
      -- each case below is refused or read as nix-instantiate reads it.
      runTest "fromJSON reads the backspace and form feed escapes" $
        assertEval "fromJSON-bf" "builtins.fromJSON ''\"a\\bb\\fc\"''" (mkStr "a\bb\fc"),
      runTest "fromJSON reads a surrogate pair" $
        assertEval "fromJSON-pair" "builtins.fromJSON ''\"\\ud83d\\ude00\"''" (mkStr "\128512"),
      runTest "fromJSON skips a leading byte order mark" $
        assertEval "fromJSON-bom" "builtins.fromJSON \"\65279 1\"" (VInt 1),
      runTest "fromJSON refuses an unterminated string" $
        assertEvalError "fromJSON-unterminated" "builtins.fromJSON ''\"abc''" "unterminated string",
      runTest "fromJSON refuses a trailing comma in an array" $
        assertEvalError "fromJSON-array-comma" "builtins.fromJSON \"[1,]\"" "unexpected ']', expected a JSON value",
      runTest "fromJSON refuses a trailing comma in an object" $
        assertEvalError "fromJSON-object-comma" "builtins.fromJSON ''{\"a\":1,}''" "unexpected '}' in an object, expected a member name",
      runTest "fromJSON refuses a leading zero" $
        assertEvalError "fromJSON-leading-zero" "builtins.fromJSON \"01\"" "unexpected content after the JSON value: '1'",
      runTest "fromJSON refuses a raw control character in a string" $
        assertEvalError "fromJSON-raw-tab" "builtins.fromJSON \"\\\"a\\tb\\\"\"" "control character U+0009 must be escaped",
      runTest "fromJSON refuses a \\u escape short of four hex digits" $
        assertEvalError "fromJSON-short-u" "builtins.fromJSON ''\"\\u12\"''" "invalid \\u escape '12\"'",
      runTest "fromJSON refuses a lone high surrogate" $
        assertEvalError "fromJSON-lone-high" "builtins.fromJSON ''\"\\ud800\"''" "a high surrogate must be followed by a low surrogate",
      runTest "fromJSON refuses a lone low surrogate" $
        assertEvalError "fromJSON-lone-low" "builtins.fromJSON ''\"\\udc00\"''" "a low surrogate must follow a high surrogate",
      runTest "fromJSON refuses an unknown escape" $
        assertEvalError "fromJSON-bad-escape" "builtins.fromJSON ''\"a\\x\"''" "invalid escape \\x",
      runTest "fromJSON refuses a number past the double range" $
        assertEvalError "fromJSON-overflow" "builtins.fromJSON \"1e400\"" "number overflow parsing '1e400'",
      runTest "fromJSON refuses whitespace JSON does not define" $
        assertEvalError "fromJSON-nbsp" "builtins.fromJSON \"\160 1\"" "unexpected '\160 1', expected a JSON value",
      runTest "fromJSON refuses a vertical tab around a value" $
        assertEvalError "fromJSON-vt" "builtins.fromJSON \"\v1\"" "unexpected '\v1', expected a JSON value",
      -- hashString
      runTest "hashString sha256" $
        assertEval "hash-sha256" "builtins.hashString \"sha256\" \"hello\"" (mkStr "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824"),
      runTest "hashString md5" $
        assertEval "hash-md5" "builtins.hashString \"md5\" \"hello\"" (mkStr "5d41402abc4b2a76b9719d911017c592"),
      runTest "hashString sha1" $
        assertEval "hash-sha1" "builtins.hashString \"sha1\" \"hello\"" (mkStr "aaf4c61ddcc5e8a2dabede0f3b482cd9aea9434d"),
      runTest "hashString unknown algo" $
        assertEvalFail "hash-bad" "builtins.hashString \"sha999\" \"hello\"",
      runTest "hashString type error" $
        assertEvalFail "hash-err" "builtins.hashString \"sha256\" 42"
    ]

-- ---------------------------------------------------------------------------
-- Tests: Batch 6 - tryEval + deepSeq
-- ---------------------------------------------------------------------------

testBatch6 :: IO [Bool]
testBatch6 = do
  putStrLn "eval/builtins-batch6"
  sequence
    [ -- tryEval success
      runTest "tryEval success" $
        assertEval "tryEval-ok" "(builtins.tryEval 42).value" (VInt 42),
      runTest "tryEval success flag" $
        assertEval "tryEval-flag" "(builtins.tryEval 42).success" (VBool True),
      -- tryEval failure
      runTest "tryEval catches throw" $
        assertEval "tryEval-throw" "(builtins.tryEval (builtins.throw \"boom\")).success" (VBool False),
      runTest "tryEval failure value" $
        assertEval "tryEval-fval" "(builtins.tryEval (builtins.throw \"boom\")).value" (VBool False),
      -- tryEval catches ONLY throw/assert (upstream ThrownError/
      -- AssertionError); type errors and missing attrs escape it.
      runTest "tryEval catches failed assert" $
        assertEval "tryEval-assert" "(builtins.tryEval (assert false; 1)).success" (VBool False),
      runTest "tryEval does not catch a type error" $
        assertEvalFail "tryEval-tyerr" "(builtins.tryEval ({} + [])).success",
      runTest "tryEval does not catch a missing attribute" $
        assertEvalFail "tryEval-noattr" "(builtins.tryEval ({ a = 1; }.b)).success",
      runTest "tryEval catches a throw nested under forcing" $
        assertEval "tryEval-deep-throw" "(builtins.tryEval (builtins.deepSeq [ (builtins.throw \"x\") ] 1)).success" (VBool False),
      -- tryEval passed as a VALUE (comparator/element positions) instead
      -- of applied syntactically: every application path must keep the
      -- catch around the argument's evaluation, and none may leak the
      -- old internal 'unreachable' dispatch error.
      runTest "tryEval as sort comparator fails like upstream" $
        case evalNix "builtins.sort builtins.tryEval [ 1 2 ]" of
          Left err
            | "unreachable" `T.isInfixOf` err -> Fail ("internal error leaked: " <> err)
            | otherwise -> Pass
          Right v -> Fail ("expected a boolean-coercion failure, got " <> T.pack (show v)),
      runTest "tryEval via map catches a throwing element" $
        assertEval
          "tryEval-map"
          "(builtins.elemAt (builtins.map builtins.tryEval [ (builtins.throw \"m\") ]) 0).success"
          (VBool False),
      runTest "tryEval via map success-wraps a clean element" $
        assertEval
          "tryEval-map-ok"
          "(builtins.elemAt (builtins.map builtins.tryEval [ 7 ]) 0).value"
          (VInt 7),
      runTest "tryEval through filter catches the element throw" $
        case evalNix "builtins.filter builtins.tryEval [ (builtins.throw \"leaked\") ]" of
          Left err
            | "leaked" `T.isInfixOf` err -> Fail ("throw escaped tryEval: " <> err)
            | otherwise -> Pass
          Right v -> Fail ("expected a boolean-coercion failure, got " <> T.pack (show v)),
      runTest "functor returning tryEval keeps the catch" $
        assertEval
          "tryEval-functor"
          "(({ __functor = self: builtins.tryEval; }) (builtins.throw \"f\")).success"
          (VBool False),
      -- deepSeq
      runTest "deepSeq returns second" $
        assertEval "deepSeq" "builtins.deepSeq [ 1 2 3 ] 42" (VInt 42),
      runTest "deepSeq forces nested" $
        assertEvalFail "deepSeq-err" "builtins.deepSeq [ (builtins.throw \"boom\") ] 42",
      runTest "deepSeq forces attrs" $
        assertEvalFail "deepSeq-attr" "builtins.deepSeq { a = builtins.throw \"boom\"; } 42"
    ]

-- ---------------------------------------------------------------------------
-- Tests: Batch 7 - genericClosure
-- ---------------------------------------------------------------------------

testBatch7 :: IO [Bool]
testBatch7 = do
  putStrLn "eval/builtins-batch7"
  sequence
    [ runTest "genericClosure basic" $
        assertEval
          "closure-basic"
          "builtins.length (builtins.genericClosure { startSet = [ { key = 1; } ]; operator = item: [ ]; })"
          (VInt 1),
      runTest "genericClosure expansion" $
        assertEval
          "closure-expand"
          "builtins.length (builtins.genericClosure { startSet = [ { key = 1; next = 2; } ]; operator = item: if item.next == 0 then [ ] else [ { key = item.next; next = 0; } ]; })"
          (VInt 2),
      runTest "genericClosure dedup" $
        assertEval
          "closure-dedup"
          "builtins.length (builtins.genericClosure { startSet = [ { key = 1; } { key = 1; } ]; operator = item: [ ]; })"
          (VInt 1),
      runTest "genericClosure missing startSet" $
        assertEvalFail "closure-nostart" "builtins.genericClosure { operator = x: [ ]; }",
      runTest "genericClosure type error" $
        assertEvalFail "closure-tyerr" "builtins.genericClosure 42"
    ]

-- ---------------------------------------------------------------------------
-- Tests: import and IO builtins (pure)
-- ---------------------------------------------------------------------------

testImportPure :: IO [Bool]
testImportPure = do
  putStrLn "eval/import-pure"
  sequence
    [ runTest "import errors in pure mode" $
        assertEvalFail "import-pure" "import ./foo.nix",
      runTest "builtins.typeOf import is lambda" $
        assertEval "typeof-import" "builtins.typeOf import" (mkStr "lambda"),
      runTest "pathExists returns false in pure mode" $
        assertEval "pathExists-pure" "builtins.pathExists ./nonexistent" (VBool False),
      runTest "readFile errors in pure mode" $
        assertEvalFail "readFile-pure" "builtins.readFile ./foo.nix",
      runTest "readDir errors in pure mode" $
        assertEvalFail "readDir-pure" "builtins.readDir ./some-dir"
    ]

-- ---------------------------------------------------------------------------
-- Tests: import and IO builtins (IO)
-- ---------------------------------------------------------------------------

-- | 'evalNixIO' with the search path @NIX_PATH@ gives ('nixPathSearchPath').
evalNixIOWithNixPath :: FilePath -> Text -> IO (Either Text NixValue)
evalNixIOWithNixPath baseDir source = do
  storeDir <- evalNixIOStoreDir
  searchPaths <- nixPathSearchPath
  st <- newEvalStateWith unrestrictedPolicy searchPaths (testSessionStore storeDir) storeDir baseDir
  evalNixIOIn st baseDir source

-- | 'evalNixIO' keeping a failure whole, its lines of context included.
evalNixIOTraced :: FilePath -> Text -> IO (Either EvalFailure NixValue)
evalNixIOTraced baseDir source = case parseNix baseDir testOrigin (TE.encodeUtf8 source) of
  Left err -> pure (Left (EvalFailure [] (T.pack (show err))))
  Right parsed -> do
    storeDir <- evalNixIOStoreDir
    st <- testEvalState storeDir baseDir
    runEvalIOTraced st (evalParsed (pure (evalBaseEnv st)) parsed)

-- | 'evalNixIO' as @nova-nix eval --strict@ evaluates: the result, once
-- everything inside it is forced in place.
evalStrictIO :: FilePath -> Text -> IO (Either Text NixValue)
evalStrictIO baseDir source = case parseNix baseDir testOrigin (TE.encodeUtf8 source) of
  Left err -> pure (Left (T.pack (show err)))
  Right parsed -> do
    storeDir <- evalNixIOStoreDir
    st <- testEvalState storeDir baseDir
    runEvalIO st $ do
      val <- evalParsed (pure (evalBaseEnv st)) parsed
      val <$ deepForce val

-- | Run a named IO eval test that should fail.
runTestIOFail :: Text -> FilePath -> Text -> IO Bool
runTestIOFail label baseDir source = do
  result <- evalNixIO baseDir source
  runTest label $ assertLeft label result

-- | The name, builder and system of a derivation, read as upstream's
-- derivationStrict reads them (primops.cc at 2.24.9): the name with
-- forceStringNoCtx, the builder and system as their environment entries
-- coerce, each checked for presence once all are read.  Every expected
-- message is nix-instantiate 2.24.9's final error line.
testDerivationFields :: IO [Bool]
testDerivationFields = do
  putStrLn "eval/derivation-fields"
  let drvWith fields = "(derivation { " <> fields <> " }).drvPath"
      plain = "system = \"x\"; builder = \"/bin/sh\";"
      withDep body = "let d = derivation { name = \"d\"; " <> plain <> " }; in " <> body
      fails label source expected = runTest label (assertEvalError label source expected)
      depOut = "/nix/store/simq0c07kvm2wyvasl0k1f619wishj4y-d"
      depDrv = "wwk71dy0w17kn5spjd8hbsrk6520xs11-d.drv"
  sequence
    [ fails "a name that is not a string is refused" (drvWith ("name = 1; " <> plain)) "expected a string but found an integer: 1",
      fails "a name with an output's context names it" (withDep (drvWith ("name = \"x${d}\"; " <> plain))) ("the string 'x" <> depOut <> "' is not allowed to refer to a store path (such as '!out!" <> depDrv <> "')"),
      fails "a name with a drvPath's context names it" (withDep (drvWith ("name = \"x${d.drvPath}\"; " <> plain))) ("the string 'x/nix/store/" <> depDrv <> "' is not allowed to refer to a store path (such as '=" <> depDrv <> "')"),
      fails "a plain path comes first among a name's context" (withDep (drvWith ("name = builtins.appendContext \"x${d}\" { \"/nix/store/00000000000000000000000000000000-a\" = { path = true; }; }; " <> plain))) ("the string 'x" <> depOut <> "' is not allowed to refer to a store path (such as '00000000000000000000000000000000-a')"),
      fails "all outputs come before one among a name's context" (withDep (drvWith ("name = \"x${d}${d.drvPath}\"; " <> plain))) ("the string 'x" <> depOut <> "/nix/store/" <> depDrv <> "' is not allowed to refer to a store path (such as '=" <> depDrv <> "')"),
      fails "an absent name is getAttr's error" (drvWith plain) "attribute 'name' missing",
      fails "an absent builder is refused" (drvWith "name = \"a\"; system = \"x\";") "required attribute 'builder' missing",
      fails "an empty builder is refused as absent" (drvWith "name = \"a\"; system = \"x\"; builder = \"\";") "required attribute 'builder' missing",
      fails "an empty system is refused as absent" (drvWith "name = \"a\"; system = \"\"; builder = \"/bin/sh\";") "required attribute 'system' missing",
      fails "the builder is checked before the system" (drvWith "name = \"a\";") "required attribute 'builder' missing",
      -- Upstream forces every attribute in name order, reading args, the
      -- builder, the system and the outputs at their keys, and looks for
      -- the builder and the system only after that pass
      -- (derivationStrictInternal, primops.cc at 2.24.9), so the first
      -- attribute by name that fails is the failure.
      fails "a throw sorting before the system wins over its absence" (drvWith "name = \"w\"; builder = \"b\"; all = throw \"all\";") "all",
      fails "a throw sorting after the system wins over its absence" (drvWith "name = \"w\"; builder = \"b\"; zz = throw \"zz\";") "zz",
      fails "a throw sorting before the outputs wins over a repeated output" (drvWith ("name = \"w\"; " <> plain <> " outputs = [ \"out\" \"out\" ]; foo = throw \"foo\";")) "foo",
      fails "a repeated output wins over a throw sorting after the outputs" (drvWith ("name = \"w\"; " <> plain <> " outputs = [ \"out\" \"out\" ]; zz = throw \"zz\";")) "duplicate derivation output 'out'",
      fails "a throw sorting before args wins over args" (drvWith ("name = \"w\"; " <> plain <> " aa = throw \"aa\"; args = throw \"args\";")) "aa",
      fails "args is read at its key, before the outputs" (drvWith ("name = \"w\"; " <> plain <> " args = [ (throw \"arg\") ]; outputs = [ \"out\" \"out\" ];")) "arg",
      fails "a throwing builder wins over a throwing system" (drvWith "name = \"w\"; builder = throw \"builder\"; system = throw \"system\";") "builder",
      fails "a structured derivation is read in the same order" (drvWith "name = \"w\"; builder = \"b\"; __structuredAttrs = true; all = throw \"all\";") "all",
      -- Upstream splits the outputs entry at whitespace (tokenizeString).
      runTest "an environment's outputs are the words of its entry" $
        assertEval
          "outputs-words"
          "builtins.toJSON (builtins.attrNames (derivationStrict { name = \"w\"; system = \"x\"; builder = \"/bin/sh\"; outputs = \" out\\tdev \"; }))"
          (mkStr "[\"dev\",\"drvPath\",\"out\"]"),
      fails "an empty name is invalid" (drvWith ("name = \"\"; " <> plain)) "invalid derivation name: name must not be empty. Please pass a different 'name'.",
      fails "a dot name is invalid" (drvWith ("name = \".\"; " <> plain)) "invalid derivation name: name '.' is not valid. Please pass a different 'name'.",
      fails "a dot component is invalid" (drvWith ("name = \".-a\"; " <> plain)) "invalid derivation name: name '.-a' is not valid: first dash-separated component must not be '.'. Please pass a different 'name'.",
      fails "an illegal character is named" (drvWith ("name = \"a b\"; " <> plain)) "invalid derivation name: name 'a b' contains illegal character ' '. Please pass a different 'name'."
    ]

-- | A path as a derivation's builder or system is copied to the store, as
-- its environment entry is, so the .drv's builder and platform fields hold
-- the store path.  Each drvPath is nix-instantiate 2.24.9's.
testDerivationPathFieldsIO :: IO [Bool]
testDerivationPathFieldsIO = do
  putStrLn "eval/derivation-path-fields-io"
  tmpBase <- getTemporaryDirectory
  let dir = tmpBase </> "nova-nix-test-derivation-path-fields"
      drvPathOf fields = "builtins.unsafeDiscardStringContext (derivation { name = \"a\"; " <> fields <> " }).drvPath"
      script = "/. + " <> nixQuotedPath (dir </> "b.sh")
  Dir.removePathForcibly dir
  createDirectoryIfMissing True dir
  BS.writeFile (dir </> "b.sh") "echo hi\n"
  results <-
    sequence
      [ runTestIO "a path builder is copied to the store" dir (drvPathOf ("system = \"x\"; builder = " <> script <> ";")) (mkStr "/nix/store/gz23n4381jlqwn1gvjqgs3411vr3w4nx-a.drv"),
        runTestIO "a path system is copied to the store" dir (drvPathOf ("system = " <> script <> "; builder = \"/bin/sh\";")) (mkStr "/nix/store/382y120pp033h5kx5zfw02q0pglf297l-a.drv")
      ]
  Dir.removePathForcibly dir
  pure results

-- | A derivation whose pass reads an @outputHash@ is fixed-output, an
-- empty one and one coerced from another type included
-- (@derivationStrictInternal@ in primops.cc): its outputs must be @out@
-- alone, and the hash is read with newHashAllowEmpty (hash.cc), so an
-- empty one is the all-zero digest of the algorithm @outputHashAlgo@
-- names.  A fixed output's path depends on its name, mode and hash alone,
-- so an empty hash's path is the all-zero hash's.  Each expected message
-- is upstream's at 2.24.9, read from its source.
testFixedOutputHash :: IO [Bool]
testFixedOutputHash = do
  putStrLn "eval/fixed-output-hash"
  tmpBase <- getTemporaryDirectory
  let drv fields = "(derivation { name = \"t\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; " <> fields <> " })"
      zeroSri = "sha256-" <> T.replicate 43 "A" <> "="
      pinned = "outputHash = \"" <> zeroSri <> "\";"
      samePath attr these those = drv these <> "." <> attr <> " == " <> drv those <> "." <> attr
      zeroPath fields = samePath "outPath" fields pinned
      gives label source = runTest label (assertEval label source (VBool True))
      fails label fields expected = runTest label (assertEvalError label (drv fields <> ".drvPath") expected)
      multipleOutputs = "multiple outputs are not supported in fixed-output derivations"
  emptyDrv <- evalDerivationIO tmpBase (drv "outputHash = \"\"; outputHashAlgo = \"sha256\";")
  sequence
    [ gives "an empty outputHash is the all-zero hash of outputHashAlgo" (zeroPath "outputHash = \"\"; outputHashAlgo = \"sha256\";"),
      runTest "an empty outputHash's .drv keeps the empty entry and names the all-zero hash" $
        assertRight "empty-outputhash-drv" emptyDrv $ \d -> case drvOutputs d of
          [DerivationOutput "out" _ "sha256" digest] ->
            assertEqual "empty-outputhash-drv" (T.replicate 64 "0", Just "") (digest, Map.lookup "outputHash" (drvEnv d))
          other -> Fail ("expected the one fixed output out, got " <> T.pack (show other)),
      gives "a null outputHash coerces to the empty one" (zeroPath "outputHash = null; outputHashAlgo = \"sha256\";"),
      gives "under __ignoreNulls a null outputHash is left out" (samePath "drvPath" "__ignoreNulls = true; outputHash = null;" "__ignoreNulls = true;"),
      gives "a structured empty outputHash is the all-zero hash" (zeroPath "__structuredAttrs = true; outputHash = \"\"; outputHashAlgo = \"sha256\";"),
      fails "an empty outputHash needs an algorithm" "outputHash = \"\";" "empty hash requires explicit hash algorithm",
      fails "an algorithm upstream does not know is none" "outputHash = \"\"; outputHashAlgo = \"sha3\";" "empty hash requires explicit hash algorithm",
      fails "an integer outputHash is its decimal" "outputHash = 5; outputHashAlgo = \"sha256\";" "hash '5' has wrong length for hash algorithm 'sha256'",
      fails "an integer outputHash with no algorithm has no type" "outputHash = 5;" "hash '5' does not include a type, nor is the type otherwise known from context",
      fails "a fixed output refuses a second output" ("outputs = [ \"out\" \"dev\" ]; " <> pinned) multipleOutputs,
      fails "a fixed output refuses an output other than out" ("outputs = [ \"dev\" ]; " <> pinned) multipleOutputs,
      fails "a fixed output's outputs are refused before their names are checked" ("__structuredAttrs = true; outputs = [ \"a b\" ]; " <> pinned) multipleOutputs,
      gives "out alone is a fixed output's own output" (samePath "outPath" ("outputs = [ \"out\" ]; " <> pinned) pinned),
      runTest "a missing builder is refused before the hash is read" $
        assertEvalError "fixed-output-builder" "(derivation { name = \"t\"; system = \"x\"; outputHash = \"\"; }).drvPath" "required attribute 'builder' missing"
    ]

-- | An @outputHashMode@ is read as upstream's @handleHashMode@ reads it
-- (primops.cc), in the pass over the attributes, for every derivation:
-- @nar@ and @recursive@ hash a NAR, @flat@ the bytes, @text@ and @git@
-- need experimental features nova-nix does not implement, and anything
-- else is refused.  Each expected message is upstream's at 2.24.9, read
-- from its source.
testOutputHashMode :: IO [Bool]
testOutputHashMode = do
  putStrLn "eval/output-hash-mode"
  tmpBase <- getTemporaryDirectory
  let drv fields = "(derivation { name = \"t\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; " <> fields <> " })"
      pinned = "outputHash = \"sha256-" <> T.replicate 43 "A" <> "=\";"
      moded mode = "outputHashMode = \"" <> mode <> "\"; " <> pinned
      comparePaths op these those = drv these <> ".outPath " <> op <> " " <> drv those <> ".outPath"
      gives label source = runTest label (assertEval label source (VBool True))
      fails label source expected = runTest label (assertEvalError label source expected)
      invalid mode = "invalid value '" <> mode <> "' for 'outputHashMode' attribute"
      disabled feature = "experimental Nix feature '" <> feature <> "' is disabled; add '--extra-experimental-features " <> feature <> "' to enable it"
      -- The .drv apart from the mode's own environment entry.
      withoutMode d = d {drvEnv = Map.delete "outputHashMode" (drvEnv d)}
  narDrv <- evalDerivationIO tmpBase (drv (moded "nar"))
  recursiveDrv <- evalDerivationIO tmpBase (drv (moded "recursive"))
  sequence
    [ gives "nar is the NAR hash recursive names" (comparePaths "==" (moded "nar") (moded "recursive")),
      gives "nar is not the flat hash" (comparePaths "!=" (moded "nar") (moded "flat")),
      gives "flat is the hash with no mode" (comparePaths "==" (moded "flat") pinned),
      gives "a structured nar is the NAR hash" (comparePaths "==" ("__structuredAttrs = true; " <> moded "nar") (moded "recursive")),
      gives "under __ignoreNulls a null mode is left out" (comparePaths "==" ("__ignoreNulls = true; outputHashMode = null; " <> pinned) pinned),
      runTest "nar gives recursive's derivation, its output hashed as a NAR" $
        assertRight "nar-drv" narDrv $ \nar ->
          assertRight "recursive-drv" recursiveDrv $ \recursive -> case drvOutputs nar of
            [DerivationOutput "out" _ "r:sha256" _] -> assertEqual "nar-drv" (withoutMode recursive) (withoutMode nar)
            other -> Fail ("expected the one NAR-hashed output out, got " <> T.pack (show other)),
      fails "an unknown mode is refused" (drv (moded "foo") <> ".drvPath") (invalid "foo"),
      fails "a mode is read with no outputHash" (drv "outputHashMode = \"foo\";" <> ".drvPath") (invalid "foo"),
      fails "a null mode coerces to the empty one, which is refused" (drv "outputHashMode = null;" <> ".drvPath") (invalid ""),
      fails "a structured unknown mode is refused" (drv "__structuredAttrs = true; outputHashMode = \"foo\";" <> ".drvPath") (invalid "foo"),
      fails "text needs dynamic-derivations" (drv "outputHashMode = \"text\";" <> ".drvPath") (disabled "dynamic-derivations"),
      fails "git needs git-hashing" (drv "outputHashMode = \"git\";" <> ".drvPath") (disabled "git-hashing"),
      fails
        "the mode is refused before the builder is checked"
        "(derivation { name = \"t\"; system = \"x\"; outputHashMode = \"foo\"; }).drvPath"
        (invalid "foo"),
      fails
        "the mode is refused before an attribute after it by name is read"
        (drv "outputHashMode = \"foo\"; zzz = throw \"later\";" <> ".drvPath")
        (invalid "foo"),
      fails
        "an attribute before the mode by name fails first"
        (drv "outputHashMode = \"foo\"; aaa = throw \"earlier\";" <> ".drvPath")
        "earlier",
      fails
        "tryEval does not catch a refused mode"
        ("(builtins.tryEval " <> drv "outputHashMode = \"foo\";" <> ".drvPath).success")
        (invalid "foo")
    ]

-- | A builtin taking a path coerces its argument as upstream's
-- coerceToPath does (eval.cc at 2.24.9), and baseNameOf and dirOf as
-- their coerceToString does.  Each expected value or message is
-- nix-instantiate 2.24.9's.
testPathArguments :: IO [Bool]
testPathArguments = do
  putStrLn "eval/path-arguments"
  let refuses label source = runTest label (assertEvalError label source "cannot coerce an integer to a string: 1")
      fails label source expected = runTest label (assertEvalError label source expected)
      gives label source expected = runTest label (assertEval label source expected)
  sequence
    [ refuses "readFile coerces its argument" "builtins.readFile 1",
      refuses "readDir coerces its argument" "builtins.readDir 1",
      refuses "pathExists coerces its argument" "builtins.pathExists 1",
      refuses "hashFile coerces its path" "builtins.hashFile \"sha256\" 1",
      refuses "readFileType coerces its argument" "builtins.readFileType 1",
      refuses "import coerces its argument" "import 1",
      refuses "scopedImport coerces its path" "builtins.scopedImport { } 1",
      refuses "builtins.path coerces its path" "builtins.path { path = 1; }",
      refuses "filterSource coerces its path" "builtins.filterSource (p: t: true) 1",
      refuses "toPath coerces its argument" "builtins.toPath 1",
      fails "a coerced path must be absolute" "builtins.toPath \"a\"" "string 'a' doesn't represent an absolute path",
      fails "builtins.path needs a path" "builtins.path { name = \"x\"; }" "missing required 'path' attribute in the first argument to builtins.path",
      fails "baseNameOf refuses null as coerceToString does" "builtins.baseNameOf null" "cannot coerce null to a string: null",
      fails "dirOf refuses a Boolean as coerceToString does" "builtins.dirOf true" "cannot coerce a Boolean to a string: true",
      gives "toPath gives a string" "builtins.typeOf (builtins.toPath \"/a/b\")" (mkStr "string"),
      gives "toPath canonicalizes an outPath" "builtins.toPath { outPath = \"/a//b/\"; }" (mkStr "/a/b"),
      gives "toPath keeps the context it coerced" "let d = derivation { name = \"d\"; system = \"x\"; builder = \"/bin/sh\"; }; in builtins.concatStringsSep \",\" (builtins.attrNames (builtins.getContext (builtins.toPath \"${d}/bin\")))" (mkStr "/nix/store/wwk71dy0w17kn5spjd8hbsrk6520xs11-d.drv"),
      gives "baseNameOf takes a __toString" "builtins.baseNameOf { __toString = s: \"/a/b\"; }" (mkStr "b"),
      gives "dirOf takes an outPath, giving a string" "builtins.dirOf { outPath = /a/b; }" (mkStr "/a"),
      gives "dirOf of a path stays a path" "builtins.typeOf (builtins.dirOf /a/b)" (mkStr "path")
    ]

-- | The same coercion reaching the filesystem: an outPath or a __toString
-- names the file read.
testPathArgumentsIO :: IO [Bool]
testPathArgumentsIO = do
  putStrLn "eval/path-arguments-io"
  -- readFileType refuses a symlink above its path, as upstream's does, and
  -- macOS keeps the temporary directory under /var, a link.
  tmpBase <- Dir.canonicalizePath =<< getTemporaryDirectory
  let dir = tmpBase </> "nova-nix-test-path-arguments"
      file = nixQuotedPath (dir </> "f")
  Dir.removePathForcibly dir
  createDirectoryIfMissing True dir
  BS.writeFile (dir </> "f") "contents"
  results <-
    sequence
      [ runTestIO "readFile reads an outPath" dir ("builtins.readFile { outPath = " <> file <> "; }") (mkStr "contents"),
        runTestIO "pathExists follows an outPath" dir ("builtins.pathExists { outPath = " <> file <> "; }") (VBool True),
        runTestIO "readFileType follows a __toString" dir ("builtins.readFileType { __toString = s: " <> file <> "; }") (mkStr "regular"),
        runTestIO "hashFile hashes an outPath" dir ("builtins.hashFile \"sha256\" { outPath = " <> file <> "; }") (mkStr "d1b2a59fbea7e20077af9f91b27e95e865061b270be03ff539ab3b73587882e8")
      ]
  Dir.removePathForcibly dir
  pure results

-- | @builtins.path@ reads its arguments as upstream's @prim_path@ does
-- (primops.cc).  @recursive = false@ copies the one file at the path by
-- its bytes, to the flat fixed-output path of those bytes, with the pin
-- read as their SHA-256 and the filter never called, and refuses a
-- directory as upstream's flat read does; an argument it does not take
-- is refused at its turn.  Each expected message is upstream's at
-- 2.24.9, read from its source.
testPathMethodIO :: IO [Bool]
testPathMethodIO = do
  putStrLn "eval/path-method-io"
  -- The flat copy reads the path with its symlinks resolved, and macOS
  -- keeps the temporary directory under /var, a link, so the refusal of
  -- a directory names the resolved path.
  tmpBase <- Dir.canonicalizePath =<< getTemporaryDirectory
  canLink <- symlinksAvailable
  let dir = tmpBase </> "nova-nix-test-path-method"
      contents = "flat\n"
      quoted name = nixQuotedPath (dir </> name)
      flatDigest = sha256Digest contents
      narDigest = sha256Digest (NAR.serialise (NAR.NarRegular False contents))
      pathOf args = "builtins.unsafeDiscardStringContext (builtins.path { " <> args <> " })"
      gives label args expected = runTestIO label dir (pathOf args) (mkStr expected)
      fails label source expected = runTestM label (assertEvalErrorIO dir label source expected)
      -- A message names the path as a path value, slash-spelled on Windows too.
      named name = T.replace "\\" "/" (T.pack (dir </> name))
      mismatch = "store path mismatch in (possibly filtered) path added from '" <> named "f.txt" <> "'"
      directory = "reading from file '" <> named "d" <> "': Is a directory"
      unsupported name = "unsupported argument '" <> name <> "' to 'addPath'"
      storePathText = either (fail . show) (pure . storePathToText defaultStoreDir)
  flatPath <- storePathText (makeFixedOutputPath "f.txt" "sha256" FlatIngestion flatDigest)
  narPath <- storePathText (makeFixedOutputPath "f.txt" "sha256" NarIngestion narDigest)
  linkPath <- storePathText (makeFixedOutputPath "link" "sha256" FlatIngestion flatDigest)
  scriptPath <- storePathText (makeFixedOutputPath "x.sh" "sha256" FlatIngestion flatDigest)
  Dir.removePathForcibly dir
  createDirectoryIfMissing True (dir </> "d")
  BS.writeFile (dir </> "f.txt") contents
  BS.writeFile (dir </> "x.sh") contents
  ExecBit.markExecutable (dir </> "x.sh")
  BS.writeFile (dir </> "d" </> "g.txt") "inner\n"
  when canLink $ Dir.createFileLink (dir </> "f.txt") (dir </> "link")
  results <-
    sequence $
      [ gives "recursive = false copies a file by its bytes" ("path = " <> quoted "f.txt" <> "; recursive = false;") flatPath,
        gives "a pin of the bytes gives the same path" ("path = " <> quoted "f.txt" <> "; recursive = false; sha256 = \"" <> Hash.bytesToHexText flatDigest <> "\";") flatPath,
        gives "recursive = true copies the NAR, as no recursive does" ("path = " <> quoted "f.txt" <> "; recursive = true;") narPath,
        gives "no recursive copies the NAR" ("path = " <> quoted "f.txt" <> ";") narPath,
        gives "an executable file's flat copy is its bytes alone" ("path = " <> quoted "x.sh" <> "; recursive = false;") scriptPath,
        runTestIO "the flat copy holds the file's bytes" dir ("builtins.readFile (builtins.path { path = " <> quoted "f.txt" <> "; recursive = false; })") (mkStr "flat\n"),
        gives "the filter is not called on a flat copy" ("path = " <> quoted "f.txt" <> "; recursive = false; filter = p: t: throw \"called\";") flatPath,
        fails "a pin of the NAR is refused under flat" ("builtins.path { path = " <> quoted "f.txt" <> "; recursive = false; sha256 = \"" <> Hash.bytesToHexText narDigest <> "\"; }") mismatch,
        fails "a pin of the bytes is refused under NAR" ("builtins.path { path = " <> quoted "f.txt" <> "; sha256 = \"" <> Hash.bytesToHexText flatDigest <> "\"; }") mismatch,
        fails "a directory is refused under flat" ("builtins.path { path = " <> quoted "d" <> "; recursive = false; }") directory,
        fails "a directory is refused under flat before a filter is called" ("builtins.path { path = " <> quoted "d" <> "; recursive = false; filter = p: t: throw \"called\"; }") directory,
        fails "recursive must be a Boolean" ("builtins.path { path = " <> quoted "f.txt" <> "; recursive = 1; }") "expected a Boolean but found an integer: 1",
        fails "an argument it does not take is refused" ("builtins.path { path = " <> quoted "f.txt" <> "; foo = 1; }") (unsupported "foo"),
        fails "a misspelt pin is refused, not passed over" ("builtins.path { path = " <> quoted "f.txt" <> "; sha265 = \"\"; }") (unsupported "sha265"),
        fails "an argument it does not take is refused before the path is looked for" "builtins.path { foo = 1; }" (unsupported "foo"),
        fails "an argument it does not take is refused before anything is copied" ("builtins.path { path = " <> quoted "missing" <> "; foo = 1; }") (unsupported "foo"),
        -- The arguments are read in the set's own order, by interned
        -- symbol, and both evaluators intern path before any name a
        -- program brings (sPath in the EvalState constructor in eval.cc
        -- at 2.24.9, StaticEvalSymbols in eval.hh at 2.36.0), so path is
        -- read first wherever the program writes it.
        fails "an argument read before the one it does not take fails first" "builtins.path { foo = 1; path = throw \"p\"; }" "p"
      ]
        ++ [gives "a symlink is copied flat as the file it names, under its own name" ("path = " <> quoted "link" <> "; recursive = false;") linkPath | canLink]
  Dir.removePathForcibly dir
  pure results

-- | The lines of context a failure gathers on its way out of
-- derivationStrict, and how the CLI reports them.  Only the IO
-- evaluator keeps them.
testDerivationTraceIO :: IO [Bool]
testDerivationTraceIO = do
  putStrLn "eval/derivation-trace"
  let derivationFailure attrs expected = do
        tmpBase <- getTemporaryDirectory
        result <- evalNixIOTraced tmpBase ("(derivation { builder = \"b\"; " <> attrs <> " }).drvPath")
        pure $ case result of
          Left failure -> assertEqual "derivation-failure" expected failure
          Right val -> Fail ("expected a failure, got " <> T.pack (show val))
  sequence
    [ -- A failure processing one attribute carries upstream's line naming
      -- it and its derivation (derivationStrictInternal, primops.cc at
      -- 2.24.9); the message itself is unchanged.
      runTestM
        "a failing derivation attribute is named in the failure's trace"
        ( derivationFailure
            "system = \"s\"; name = \"x\"; foo = { };"
            (EvalFailure ["while evaluating attribute 'foo' of derivation 'x'"] "cannot coerce a set to a string: { }")
        ),
      runTestM
        "a structured attribute with no JSON form is named in the failure's trace"
        ( derivationFailure
            "system = \"s\"; name = \"x\"; __structuredAttrs = true; foo = x: x;"
            (EvalFailure ["while evaluating attribute 'foo' of derivation 'x'"] "builtins.toJSON: cannot convert a function to JSON")
        ),
      -- prim_derivationStrict adds its own line outside forceStringNoCtx's.
      runTestM
        "a failing name carries both of upstream's lines"
        ( derivationFailure
            "system = \"s\"; name = throw \"n\";"
            ( EvalFailure
                [ "while evaluating the derivation attribute 'name'",
                  "while evaluating the `name` attribute passed to builtins.derivationStrict"
                ]
                "n"
            )
        ),
      runTestM
        "a switch that is not a Boolean is named in the failure's trace"
        ( derivationFailure
            "system = \"s\"; name = \"x\"; __ignoreNulls = 1;"
            (EvalFailure ["while evaluating the `__ignoreNulls` attribute passed to builtins.derivationStrict"] "expected a Boolean but found an integer: 1")
        ),
      runTestM
        "a structured-attributes switch that is not a Boolean is named in the failure's trace"
        ( derivationFailure
            "system = \"s\"; name = \"x\"; __structuredAttrs = 1;"
            (EvalFailure ["while evaluating the `__structuredAttrs` attribute passed to builtins.derivationStrict"] "expected a Boolean but found an integer: 1")
        ),
      runTestM
        "a structured string field that is not a string is named in the failure's trace"
        ( derivationFailure
            "system = \"s\"; name = \"x\"; __structuredAttrs = true; outputHash = 1;"
            (EvalFailure ["while evaluating attribute 'outputHash' of derivation 'x'"] "expected a string but found an integer: 1")
        ),
      runTestM
        "a refused outputHashMode is named in the failure's trace"
        ( derivationFailure
            "system = \"s\"; name = \"x\"; outputHashMode = \"foo\";"
            (EvalFailure ["while evaluating attribute 'outputHashMode' of derivation 'x'"] "invalid value 'foo' for 'outputHashMode' attribute")
        ),
      runTestM
        "a missing attribute is refused without a line naming it"
        (derivationFailure "name = \"x\";" (EvalFailure [] "required attribute 'system' missing")),
      runTestM "a failure in a dependency is traced through each derivation, outermost first" $ do
        tmpBase <- getTemporaryDirectory
        result <-
          evalNixIOTraced tmpBase $
            "let inner = derivation { name = \"inner\"; system = \"s\"; builder = \"b\"; bad = { }; };"
              <> " in (derivation { name = \"outer\"; system = \"s\"; builder = \"b\"; dep = inner; }).drvPath"
        pure $ case result of
          Left failure ->
            assertEqual
              "nested-trace"
              ["while evaluating attribute 'dep' of derivation 'outer'", "while evaluating attribute 'bad' of derivation 'inner'"]
              (failureTrace failure)
          Right val -> Fail ("expected a failure, got " <> T.pack (show val)),
      runTestM "a throw in a derivation attribute stays catchable" $ do
        tmpBase <- getTemporaryDirectory
        result <- evalNixIO tmpBase "(builtins.tryEval (derivation { name = \"x\"; system = \"s\"; builder = \"b\"; foo = throw \"t\"; }).drvPath).success"
        pure $ assertRight "trace-catchable" result (assertEqual "trace-catchable" (VBool False)),
      -- upstream's showErrorInfo and indent (error.cc at 2.24.9), checked
      -- against nix-instantiate 2.24.9's report for the same failure with
      -- its positioned lines left out.
      runTest "a failure renders as upstream reports it" $
        assertEqual
          "render-traced"
          "error:\n       \x2026 while evaluating attribute 'foo' of derivation 'x'\n\n       error: a\n       b"
          (renderEvalFailure (EvalFailure ["while evaluating attribute 'foo' of derivation 'x'"] "a\nb")),
      runTest "a failure with no trace renders on the error line" $
        assertEqual "render-untraced" "error: a\n       b" (renderEvalFailure (EvalFailure [] "a\nb")),
      -- showErrorInfo passes over a trace whose text is empty (error.cc).
      runTest "an empty line of context is left out" $
        assertEqual
          "render-empty-line"
          "error:\n       \x2026 c\n\n       error: x"
          (renderEvalFailure (EvalFailure ["", "c", ""] "x"))
    ]

-- | @builtins.addErrorContext@ forces its value and coerces its message
-- only when that fails, in the catch around the force, and the value's
-- failure escapes whatever the message does (@prim_addErrorContext@ in
-- primops.cc at 2.36.0).  The pure evaluator reports a failure's
-- message alone, so these see which failure escapes, not the line it
-- carries.
testErrorContext :: IO [Bool]
testErrorContext = do
  putStrLn "eval/error-context"
  let yields source expected = runTest source (assertEval source source expected)
      fails source expected = runTest (source <> " fails with " <> expected) (assertEvalError source source expected)
  sequence
    [ yields "builtins.addErrorContext (throw \"a\") 1" (VInt 1),
      yields "builtins.addErrorContext \"c\" (1 + 1)" (VInt 2),
      yields "let f = builtins.addErrorContext (throw \"a\"); in f 1" (VInt 1),
      yields "builtins.elemAt (map (builtins.addErrorContext (throw \"a\")) [ 1 ]) 0" (VInt 1),
      fails "builtins.addErrorContext \"ctx-marker\" (throw \"x\")" "x",
      fails "builtins.addErrorContext (throw \"a\") (throw \"b\")" "b",
      fails "builtins.addErrorContext 1 (throw \"b\")" "b",
      yields "(builtins.tryEval (builtins.addErrorContext \"c\" (throw \"x\"))).success" (VBool False),
      -- The message's type error leaves the throw, which tryEval catches.
      yields "(builtins.tryEval (builtins.addErrorContext 1 (throw \"x\"))).success" (VBool False)
    ]

-- | The line @builtins.addErrorContext@ adds to a failure's trace under
-- the IO evaluator, and how the CLI reports it: upstream's @addTrace@ in
-- @prim_addErrorContext@ (primops.cc), printed by @showErrorInfo@
-- (error.cc at 2.24.9) with its positioned lines left out.
testErrorContextIO :: IO [Bool]
testErrorContextIO = do
  putStrLn "eval/error-context-io"
  tmpBase <- getTemporaryDirectory
  let failure source check = do
        result <- evalNixIOTraced tmpBase source
        pure $ case result of
          Left failed -> check failed
          Right val -> Fail ("expected a failure, got " <> T.pack (show val))
      failsWith label source expected = runTestM label (failure source (assertEqual label expected))
      reports label source expected = runTestM label (failure source (assertEqual label expected . renderEvalFailure))
  sequence
    [ reports
        "a failure inside it reports the message as a line of its trace"
        "builtins.addErrorContext \"ctx-marker\" (throw \"x\")"
        "error:\n       \x2026 ctx-marker\n\n       error: x",
      runTestIO "its message is not forced when its value evaluates" tmpBase "builtins.addErrorContext (throw \"a\") 1" (VInt 1),
      failsWith
        "nested contexts are listed outermost first"
        "builtins.addErrorContext \"outer\" (builtins.addErrorContext \"inner\" (throw \"x\"))"
        (EvalFailure ["outer", "inner"] "x"),
      failsWith
        "a context is outside the lines its value's failure gathered"
        "builtins.addErrorContext \"c\" (derivation { name = \"x\"; system = \"s\"; builder = \"b\"; foo = { }; }).drvPath"
        (EvalFailure ["c", "while evaluating attribute 'foo' of derivation 'x'"] "cannot coerce a set to a string: { }"),
      failsWith
        "a context is outside the line listToAttrs gives an element that is not a set"
        "builtins.addErrorContext \"c\" (builtins.listToAttrs [ 1 ])"
        (EvalFailure ["c", "while evaluating an element of the list passed to builtins.listToAttrs"] "expected a set but found an integer: 1"),
      failsWith
        "a message coerces as a set with __toString"
        "builtins.addErrorContext { __toString = _: \"set-ctx\"; } (throw \"x\")"
        (EvalFailure ["set-ctx"] "x"),
      failsWith
        "a message that throws leaves the failure, with a line naming the message's own"
        "builtins.addErrorContext (throw \"a\") (throw \"b\")"
        (EvalFailure ["while evaluating the error message passed to builtins.addErrorContext, another error occurred: a"] "b"),
      failsWith
        "a message that does not coerce leaves the failure, with a line naming the message's own"
        "builtins.addErrorContext 1 (throw \"b\")"
        (EvalFailure ["while evaluating the error message passed to builtins.addErrorContext, another error occurred: cannot coerce an integer to a string: 1"] "b"),
      runTestIO "a message that does not coerce leaves a throw catchable" tmpBase "(builtins.tryEval (builtins.addErrorContext 1 (throw \"x\"))).success" (VBool False),
      reports "an empty message adds no line of its own" "builtins.addErrorContext \"\" (throw \"x\")" "error:\n       error: x",
      runTestIO "a throw inside it stays catchable" tmpBase "(builtins.tryEval (builtins.addErrorContext \"c\" (throw \"x\"))).success" (VBool False),
      runTestM "an abort inside it gains the line and escapes tryEval" $
        failure "builtins.tryEval (builtins.addErrorContext \"c\" (builtins.abort \"x\"))" (assertEqual "abort-trace" ["c"] . failureTrace),
      failsWith
        "a message that does not coerce leaves an abort escaping tryEval, with a line naming the message's own"
        "builtins.tryEval (builtins.addErrorContext 1 (builtins.abort \"x\"))"
        (EvalFailure ["while evaluating the error message passed to builtins.addErrorContext, another error occurred: cannot coerce an integer to a string: 1"] "evaluation aborted with the following error message: 'x'")
    ]

-- | The lines of context a failure gathers in listToAttrs, upstream's from
-- prim_listToAttrs (primops.cc): forceList's and forceAttrs's on their type
-- errors alone, forceStringNoCtx's on any failure reading the name, and
-- getAttr's on a missing attribute.  Only the IO evaluator keeps them.
testListToAttrsTraceIO :: IO [Bool]
testListToAttrsTraceIO = do
  putStrLn "eval/listToAttrs-trace"
  let listToAttrsFailure label argument expected = runTestM label $ do
        tmpBase <- getTemporaryDirectory
        result <- evalNixIOTraced tmpBase ("builtins.listToAttrs " <> argument)
        pure $ case result of
          Left failure -> assertEqual label expected failure
          Right val -> Fail ("expected a failure, got " <> T.pack (show val))
      elementLine = "while evaluating an element of the list passed to builtins.listToAttrs"
      nameLine = "while evaluating the `name` attribute of an element of the list passed to builtins.listToAttrs"
      pairLine = "in a {name=...; value=...;} pair"
  sequence
    [ listToAttrsFailure "an argument that is not a list" "1" (EvalFailure ["while evaluating the argument passed to builtins.listToAttrs"] "expected a list but found an integer: 1"),
      listToAttrsFailure "an element that is not a set" "[ 1 ]" (EvalFailure [elementLine] "expected a set but found an integer: 1"),
      listToAttrsFailure "an element that fails to evaluate" "[ (throw \"e\") ]" (EvalFailure [] "e"),
      listToAttrsFailure "an element without a name" "[ { value = 1; } ]" (EvalFailure [pairLine] "attribute 'name' missing"),
      listToAttrsFailure "a name that is not a string" "[ { name = 1; value = 1; } ]" (EvalFailure [nameLine] "expected a string but found an integer: 1"),
      listToAttrsFailure "a name that fails to evaluate" "[ { name = throw \"n\"; value = 1; } ]" (EvalFailure [nameLine] "n"),
      listToAttrsFailure "a first element of a name without a value" "[ { name = \"a\"; } ]" (EvalFailure [pairLine] "attribute 'value' missing")
    ]

-- | Regression: a thunk whose force throws a catchable error (builtins.throw,
-- a type error) must be restored to PENDING, not left BLACKHOLE - otherwise a
-- later force of the same shared thunk aborts with a bogus "infinite recursion"
-- that escapes tryEval.  Genuine self-recursion must still escape tryEval.
-- EvalIO-only: PureEval never blackholes.
testBlackholeRecoveryIO :: IO [Bool]
testBlackholeRecoveryIO = do
  putStrLn "eval/blackhole-recovery-io"
  sequence
    [ runTestIO
        "tryEval twice on a shared throwing thunk recovers (no bogus recursion)"
        "."
        "let x = builtins.throw \"boom\"; in (if (builtins.tryEval x).success then 1 else 0) + (if (builtins.tryEval x).success then 1 else 0)"
        (VInt 0),
      runTestIOFail
        "genuine infinite recursion still escapes tryEval"
        "."
        "builtins.tryEval (let y = y; in y)"
    ]

-- | builtins.path/filterSource with a filter: the tree is serialized with
-- rejected entries removed (a rejected directory prunes its subtree),
-- content-addressed over the FILTERED NAR, and materialized to the store.
testPathFilterIO :: IO [Bool]
testPathFilterIO = do
  putStrLn "eval/path-filter-io"
  testPathFilterBody

testPathFilterBody :: IO [Bool]
testPathFilterBody = do
  tmpBase <- getTemporaryDirectory
  let srcDir = tmpBase </> "nova-nix-test-path-filter"
      setup = do
        removeIfExists srcDir
        createDirectoryIfMissing True (srcDir </> "sub")
        createDirectoryIfMissing True (srcDir </> "dropdir")
        BS.writeFile (srcDir </> "keep.txt") "keep"
        BS.writeFile (srcDir </> "drop.log") "drop"
        BS.writeFile (srcDir </> "sub" </> "inner.txt") "inner"
        BS.writeFile (srcDir </> "dropdir" </> "x.txt") "gone"
      quoted = nixQuotedPath srcDir
      filterExpr = "(p: t: builtins.match \".*[.]log\" p == null && baseNameOf p != \"dropdir\")"
      filteredPath = "(builtins.path { path = " <> quoted <> "; name = \"src\"; filter = " <> filterExpr <> "; })"
      unfilteredPath = "(builtins.path { path = " <> quoted <> "; name = \"src\"; })"
  -- bracket_: cleanup runs even if tests throw
  bracket_ setup (removeIfExists srcDir) $
    sequence
      [ runTestIO
          "kept file survives with its content"
          "."
          ("builtins.readFile (" <> filteredPath <> " + \"/keep.txt\")")
          (mkStr "keep"),
        runTestIO
          "kept subtree survives"
          "."
          ("builtins.readFile (" <> filteredPath <> " + \"/sub/inner.txt\")")
          (mkStr "inner"),
        runTestIO
          "rejected file is dropped"
          "."
          ("builtins.pathExists (" <> filteredPath <> " + \"/drop.log\")")
          (VBool False),
        runTestIO
          "rejected directory prunes its subtree"
          "."
          ("builtins.pathExists (" <> filteredPath <> " + \"/dropdir\")")
          (VBool False),
        runTestIO
          "filtered and unfiltered store paths differ"
          "."
          (filteredPath <> " == " <> unfilteredPath)
          (VBool False),
        runTestIO
          "same filter yields the same store path"
          "."
          (filteredPath <> " == " <> filteredPath)
          (VBool True),
        runTestIO
          "filterSource is path-with-filter under the source basename"
          "."
          ( "builtins.filterSource (p: t: true) "
              <> quoted
              <> " == builtins.path { path = "
              <> quoted
              <> "; filter = (p: t: true); }"
          )
          (VBool True)
      ]

-- | Interpolating a path that is a symlink copies the link, and one below
-- a symlinked directory the file it names: upstream's copyPathToStore
-- stores path.resolveSymlinks(SymlinkResolution::Ancestors) under
-- path.baseName() (eval.cc at 2.34.9), where 2.24.9 resolved the path in
-- full and copied what a link points to.
testInterpolatedSymlinkIO :: IO [Bool]
testInterpolatedSymlinkIO = do
  putStrLn "eval/interpolated-symlink-io"
  canLink <- symlinksAvailable
  if not canLink
    then do
      putStrLn "  SKIP  needs symlink privilege"
      pure []
    else testInterpolatedSymlinkBody

testInterpolatedSymlinkBody :: IO [Bool]
testInterpolatedSymlinkBody = do
  tmpBase <- getTemporaryDirectory
  let dir = tmpBase </> "nova-nix-test-interpolated-symlink"
  Dir.removePathForcibly dir
  createDirectoryIfMissing True (dir </> "sub")
  BS.writeFile (dir </> "data.txt") "linked bytes"
  BS.writeFile (dir </> "sub" </> "inner.txt") "inner"
  Dir.createFileLink "data.txt" (dir </> "filelink")
  Dir.createDirectoryLink "sub" (dir </> "dirlink")
  let interpolated name = evalNixIO "." ("\"${/. + " <> nixQuotedPath (dir </> name) <> "}\"")
      copiedAs name entry =
        either (const Nothing) (Just . storePathToText defaultStoreDir) (makeFixedOutputPath (T.pack (takeFileName name)) "sha256" NarIngestion (sha256Digest (NAR.serialise entry)))
      copies label name entry = runTestM label $ do
        result <- interpolated name
        pure $ case (result, copiedAs name entry) of
          (_, Nothing) -> Fail "fixture store path rejected"
          (Right (VStr got _), Just expected) -> assertEqual (T.pack name) (TE.encodeUtf8 expected) got
          (Right other, _) -> Fail ("expected a string, got " <> T.pack (show other))
          (Left err, _) -> Fail ("eval failed: " <> err)
  results <-
    sequence
      [ copies "interpolating a link to a file copies the link" "filelink" (NAR.NarSymlink "data.txt"),
        copies "interpolating a link to a directory copies the link" "dirlink" (NAR.NarSymlink "sub"),
        copies "interpolating a path below a symlinked directory copies the file it names" ("dirlink" </> "inner.txt") (NAR.NarRegular False "inner")
      ]
  Dir.removePathForcibly dir
  pure results

-- | A failed source-path access reworded as upstream's PosixSourceAccessor
-- words it (2.24.9), from IOExceptions shaped as GHC raises them.
testSourceErrorMessage :: IO [Bool]
testSourceErrorMessage = do
  putStrLn "eval/source-error-message"
  let errnoFailure kind (Errno code) description =
        IOError Nothing kind "test" description (Just code) (Just "/d/x")
      absent = errnoFailure NoSuchThing eNOENT "No such file or directory"
      notDirectory = errnoFailure InappropriateType eNOTDIR "Not a directory"
      denied = errnoFailure PermissionDenied eACCES "Permission denied"
      isDirectory = errnoFailure InappropriateType eISDIR "Is a directory"
      -- GHC's openFile refuses a directory itself, with no errno.
      ghcDirectory = IOError Nothing InappropriateType "openFile" "is a directory" Nothing (Just "/d/dir")
      says label access path err expected =
        runTest label (assertEqual label expected (sourceErrorMessage access path err))
  sequence
    [ says "a stat of an absent path" StatPath "/d/nope" absent "path '/d/nope' does not exist",
      says "a stat through a file is absent too" StatPath "/d/file/x" notDirectory "path '/d/file/x' does not exist",
      says "a stat refused otherwise names the stat" StatPath "/d/x" denied "getting status of '/d/x': Permission denied",
      says "opening an absent file" ReadFile "/d/nope" absent "opening file '/d/nope': No such file or directory",
      says "opening a file refused" ReadFile "/d/x" denied "opening file '/d/x': Permission denied",
      says "reading a directory as a file" ReadFile "/d/dir" isDirectory "reading from file '/d/dir': Is a directory",
      says "GHC's own directory refusal reads as upstream's" ReadFile "/d/dir" ghcDirectory "reading from file '/d/dir': Is a directory",
      says "listing a file" ReadDirectory "/d/file" notDirectory "reading directory /d/file: Not a directory"
    ]

-- | The same failures through the IO evaluator, against a real tree.
-- POSIX only: Windows reports its own error text, and a path there is
-- spelled with a drive.
testSourceReadErrorsIO :: IO [Bool]
testSourceReadErrorsIO = do
  putStrLn "eval/source-read-errors-io"
  if SI.os == "mingw32"
    then do
      putStrLn "  SKIP  POSIX error text"
      pure []
    else testSourceReadErrorsBody

testSourceReadErrorsBody :: IO [Bool]
testSourceReadErrorsBody = do
  -- The temporary directory's own symlinks resolved (macOS keeps it under
  -- /var, a link), so the only link in play is the one made below.
  tmpBase <- Dir.canonicalizePath =<< getTemporaryDirectory
  let dir = tmpBase </> "nova-nix-test-source-read-errors"
  Dir.removePathForcibly dir
  createDirectoryIfMissing True (dir </> "sub")
  createDirectoryIfMissing True (dir </> "real")
  BS.writeFile (dir </> "file") "f"
  Dir.createDirectoryLink "real" (dir </> "link")
  let resolved = T.pack dir
      given = T.pack dir
      at name = "/. + " <> nixQuotedPath (dir </> name)
      fails label source expected = runTestM label (assertEvalErrorIO dir label source expected)
  results <-
    sequence
      [ fails "readFile of an absent file" ("builtins.readFile (" <> at "nope" <> ")") ("opening file '" <> resolved <> "/nope': No such file or directory"),
        fails "hashFile of an absent file" ("builtins.hashFile \"sha256\" (" <> at "nope" <> ")") ("opening file '" <> resolved <> "/nope': No such file or directory"),
        fails "readFile through a file" ("builtins.readFile (" <> at "file/x" <> ")") ("opening file '" <> resolved <> "/file/x': Not a directory"),
        fails "readFile of a directory" ("builtins.readFile (" <> at "sub" <> ")") ("reading from file '" <> resolved <> "/sub': Is a directory"),
        fails "readDir of an absent directory" ("builtins.readDir (" <> at "nope" <> ")") ("reading directory " <> resolved <> "/nope: No such file or directory"),
        fails "readDir of a file" ("builtins.readDir (" <> at "file" <> ")") ("reading directory " <> resolved <> "/file: Not a directory"),
        fails "readFileType of an absent path" ("builtins.readFileType (" <> at "nope" <> ")") ("path '" <> given <> "/nope' does not exist"),
        fails "interpolating an absent path" ("\"${" <> at "nope" <> "}\"") ("path '" <> resolved <> "/nope' does not exist"),
        fails "builtins.path of an absent path" ("builtins.path { path = " <> at "nope" <> "; }") ("path '" <> resolved <> "/nope' does not exist"),
        fails "importing an absent path" ("import (" <> at "nope" <> ")") ("path '" <> resolved <> "/nope' does not exist"),
        fails "importing a directory without default.nix" ("import (" <> at "sub" <> ")") ("opening file '" <> resolved <> "/sub/default.nix': No such file or directory"),
        -- Through a symlinked directory a read names the resolved path, as
        -- realisePath resolves it, and readFileType refuses the link.
        fails "readFile through a link names the resolved path" ("builtins.readFile (" <> at "link/nope" <> ")") ("opening file '" <> resolved <> "/real/nope': No such file or directory"),
        fails "readDir through a link names the resolved path" ("builtins.readDir (" <> at "link/nope" <> ")") ("reading directory " <> resolved <> "/real/nope: No such file or directory"),
        fails "import through a link names the resolved path" ("import (" <> at "link/nope" <> ")") ("path '" <> resolved <> "/real/nope' does not exist"),
        fails "readFileType through a link refuses the link" ("builtins.readFileType (" <> at "link/nope" <> ")") ("path '" <> given <> "/link' is a symlink")
      ]
  Dir.removePathForcibly dir
  pure results

-- | readDir over a directory that can be read but not searched, so that
-- every entry's lstat is refused.  Upstream's readDirectory gives such an
-- entry no type and prim_readDir binds it to a deferred readFileType of
-- its path under the directory with its symlinks resolved
-- (posix-source-accessor.cc and primops.cc at 2.24.9): the listing names
-- every entry, and only forcing one fails, as readFileType on it fails.
-- The owner keeps read permission and loses search; where that does not
-- refuse the lstat (Windows, where the mode does not govern a lookup, or
-- root) the group is skipped.
testReadDirUntypedEntriesIO :: IO [Bool]
testReadDirUntypedEntriesIO = do
  putStrLn "eval/readdir-untyped-entries-io"
  -- The temporary directory's own symlinks resolved (macOS keeps it under
  -- /var, a link), so the only link in play is the one made below.
  tmpBase <- Dir.canonicalizePath =<< getTemporaryDirectory
  let root = tmpBase </> "nova-nix-test-readdir-untyped"
      locked = root </> "locked"
      pathValue p = "(/. + " <> nixQuotedPath p <> ")"
      lockedValue = pathValue locked
      searchable = Dir.setOwnerReadable True (Dir.setOwnerSearchable True (Dir.setOwnerWritable True Dir.emptyPermissions))
      unsearchable = Dir.setOwnerReadable True Dir.emptyPermissions
      refused = "getting status of '" <> T.pack (locked </> "file") <> "': Permission denied"
      evalHere = evalNixIO tmpBase
  -- A run that died while the directory was unsearchable left it so, and
  -- it cannot be emptied until it is searchable again.
  leftLocked <- Dir.doesDirectoryExist locked
  when leftLocked (Dir.setPermissions locked searchable)
  Dir.removePathForcibly root
  createDirectoryIfMissing True (locked </> "sub")
  BS.writeFile (locked </> "file") "f"
  Dir.createDirectoryLink "locked" (root </> "link")
  let checks =
        [ runTestM "readDir names every entry of a directory whose entries cannot be stat'ed" $ do
            outcome <- evalHere ("builtins.attrNames (builtins.readDir " <> lockedValue <> ") == [ \"file\" \"sub\" ]")
            pure $ case outcome of
              Right (VBool True) -> Pass
              Right other -> Fail ("expected the names file and sub, got " <> T.pack (show other))
              Left err -> Fail ("readDir failed: " <> err),
          runTestM "forcing an entry readDir could not type fails as readFileType on it does" $ do
            forced <- evalHere ("(builtins.readDir " <> lockedValue <> ").file")
            direct <- evalHere ("builtins.readFileType " <> pathValue (locked </> "file"))
            pure (assertEqual "readdir-forced" (Left refused, Left refused) (void forced, void direct)),
          runTestM "readDir through a link defers readFileType of the entry under the resolved directory" $ do
            forced <- evalHere ("(builtins.readDir " <> pathValue (root </> "link") <> ").file")
            pure (assertEqual "readdir-forced-link" (Left refused) (void forced))
        ]
      probeThenCheck = do
        probe <- try (Dir.pathIsSymbolicLink (locked </> "file")) :: IO (Either IOException Bool)
        case probe of
          Right _ -> [] <$ putStrLn "  SKIP  a directory without search permission lets its entries be stat'ed here"
          Left _ -> sequence checks
  -- Search permission is restored however the checks end, and the tree
  -- removed after it.
  bracket_ (Dir.setPermissions locked unsearchable) (Dir.setPermissions locked searchable) probeThenCheck
    `finally` Dir.removePathForcibly root

-- | Watchdog for a deep force or print that ends because it remembers
-- the sets and lists it has entered.  One that forgets recurses without
-- end, its memory growing by hundreds of megabytes a second, so the bound
-- is short; one that remembers takes milliseconds.
repeatedWatchdogMicros :: Int
repeatedWatchdogMicros = 2 * 1000 * 1000

-- | An outcome reached within 'repeatedWatchdogMicros', or a failure
-- naming the test that did not finish.
finishesInTime :: Text -> IO TestResult -> IO TestResult
finishesInTime label action =
  fromMaybe (Fail (label <> ": did not finish within " <> T.pack (show repeatedWatchdogMicros) <> " microseconds")) <$> timeout repeatedWatchdogMicros action

-- | @builtins.path@ with no filter over a tree that contains a symlink.
-- The store-path name comes from the NAR serialization, which records
-- the link as a leaf entry - so the copy must replicate the link for
-- the stored bytes to still match the recorded content address.
testPathSymlinkIO :: IO [Bool]
testPathSymlinkIO = do
  putStrLn "eval/path-symlink-io"
  canLink <- symlinksAvailable
  if not canLink
    then do
      putStrLn "  SKIP  needs symlink privilege"
      pure []
    else testPathSymlinkBody

testPathSymlinkBody :: IO [Bool]
testPathSymlinkBody = do
  tmpBase <- getTemporaryDirectory
  let srcDir = tmpBase </> "nova-nix-test-path-symlink"
      cycleDir = tmpBase </> "nova-nix-test-path-cycle"
  Dir.removePathForcibly srcDir
  Dir.removePathForcibly cycleDir
  createDirectoryIfMissing True srcDir
  BS.writeFile (srcDir </> "data.txt") "linked bytes"
  Dir.createFileLink "data.txt" (srcDir </> "link")
  createDirectoryIfMissing True cycleDir
  BS.writeFile (cycleDir </> "f.txt") "f"
  Dir.createDirectoryLink "." (cycleDir </> "loop")
  linkEntry <- NAR.serialiseFromPath srcDir
  cycleEntry <- NAR.serialiseFromPath cycleDir
  let pathExprFor dir name =
        "builtins.path { path = " <> nixQuotedPath dir <> "; name = \"" <> name <> "\"; }"
      spFor name entry =
        makeFixedOutputPath name "sha256" NarIngestion (sha256Digest (NAR.serialise entry))
  results <- case (spFor "path-symlink-src" linkEntry, spFor "path-symlink-cycle" cycleEntry) of
    (Right linkSp, Right cycleSp) -> do
      -- The same mapping the evaluator's copy uses for its destination.
      evalStore <- evalNixIOStoreDir
      let linkDest = storePathToFilePath evalStore linkSp
          cycleDest = storePathToFilePath evalStore cycleSp
      -- A leftover materialization from an earlier (possibly pre-fix) run
      -- would short-circuit the copy under test.
      Dir.removePathForcibly linkDest
      Dir.removePathForcibly cycleDest
      linkEval <- evalNixIO "." (pathExprFor srcDir "path-symlink-src")
      cycleEval <- timeout walkWatchdogMicros (evalNixIO "." (pathExprFor cycleDir "path-symlink-cycle"))
      sequence
        [ runTestM "unfiltered builtins.path replicates a symlink" $
            case linkEval of
              Left err -> pure (Fail ("eval failed: " <> err))
              Right _ -> do
                isLink <- Dir.pathIsSymbolicLink (linkDest </> "link")
                if isLink
                  then do
                    target <- Dir.getSymbolicLinkTarget (linkDest </> "link")
                    pure (assertEqual "link target" "data.txt" target)
                  else pure (Fail "materialized 'link' is not a symlink"),
          runTestM "materialized bytes reproduce the recorded content address" $
            case linkEval of
              Left err -> pure (Fail ("eval failed: " <> err))
              Right _ -> do
                onDisk <- NAR.serialiseFromPath linkDest
                pure $ case spFor "path-symlink-src" onDisk of
                  Left err -> Fail ("on-disk tree's name rejected: " <> T.pack (show err))
                  Right recomputed -> assertEqual "recomputed path" linkSp recomputed,
          runTestM "builtins.path terminates on a link cycle" $
            case cycleEval of
              Nothing -> pure (Fail "copy did not terminate on a link cycle")
              Just (Left err) -> pure (Fail ("eval failed: " <> err))
              Just (Right _) -> do
                isLink <- Dir.pathIsSymbolicLink (cycleDest </> "loop")
                pure (if isLink then Pass else Fail "cycle link not replicated as a link")
        ]
    (badLink, badCycle) ->
      sequence
        [ runTest "path-symlink fixture store paths accepted" $
            Fail ("test store path rejected: " <> T.pack (show (badLink, badCycle)))
        ]
  Dir.removePathForcibly srcDir
  Dir.removePathForcibly cycleDir
  pure results

testImportIO :: IO [Bool]
testImportIO = do
  putStrLn "eval/import-io"
  tmpBase <- getTemporaryDirectory
  let testDir = tmpBase </> "nova-nix-test-import"
      subDir = testDir </> "sub"
      -- A thousand attributes, one of them a function.
      largeSetSource =
        "{ f = x: x; " <> T.concat ["a" <> T.pack (show i) <> " = " <> T.pack (show i) <> "; " | i <- [1 .. 999 :: Int]] <> "}"
      setup = do
        createDirectoryIfMissing True subDir
        TIO.writeFile (testDir </> "literal.nix") "42"
        TIO.writeFile (testDir </> "expr.nix") "1 + 2"
        TIO.writeFile (testDir </> "nested-inner.nix") "99"
        TIO.writeFile (testDir </> "nested-outer.nix") "import ./nested-inner.nix"
        TIO.writeFile (testDir </> "attrset.nix") "{ x = 1; y = 2; }"
        TIO.writeFile (testDir </> "large-set.nix") largeSetSource
        TIO.writeFile (testDir </> "uses-arg.nix") "let f = x: x + 10; in f 5"
        TIO.writeFile (testDir </> "positions.nix") "{\n  a = 1;\n}\n"
        TIO.writeFile (testDir </> "curpos.nix") "\n  __curPos\n"
        TIO.writeFile (subDir </> "from-sub.nix") "7"
      cleanup = do
        exists <- doesDirectoryExist testDir
        when exists (removeDirectoryRecursive testDir)
  -- bracket_: cleanup runs even if tests throw
  bracket_ setup cleanup $
    sequence
      [ -- import
        runTestIO "import literal" testDir "import ./literal.nix" (VInt 42),
        runTestIO "import expression" testDir "import ./expr.nix" (VInt 3),
        runTestIO "import nested (A imports B)" testDir "import ./nested-outer.nix" (VInt 99),
        runTestIO
          "import cache (same file twice)"
          testDir
          "(import ./literal.nix) + (import ./literal.nix)"
          (VInt 84),
        -- A function compares equal only to itself, so the two imports
        -- are equal only when they are one value: the file evaluated once.
        runTestIO
          "two imports of a file whose set is large are one value"
          testDir
          "import ./large-set.nix == import ./large-set.nix"
          (VBool True),
        runTestIOFail "import nonexistent -> error" testDir "import ./nonexistent.nix",
        runTestIO "import attrset + select" testDir "(import ./attrset.nix).x" (VInt 1),
        runTestIO "import let/lambda" testDir "import ./uses-arg.nix" (VInt 15),
        -- A position in an imported file names the file the import read.
        -- The import runs from the directory's canonical spelling, where
        -- the path as written (upstream's) and the path with its symlinks
        -- resolved (the one the import reads here) are the same.
        do
          canonicalDir <- Dir.canonicalizePath testDir
          file <- Dir.canonicalizePath (canonicalDir </> "positions.nix")
          runTestIO
            "unsafeGetAttrPos names the imported file"
            canonicalDir
            "let p = builtins.unsafeGetAttrPos \"a\" (import ./positions.nix); in \"${p.file}:${toString p.line}:${toString p.column}\""
            (mkStr (T.pack file <> ":2:3")),
        do
          canonicalDir <- Dir.canonicalizePath testDir
          file <- Dir.canonicalizePath (canonicalDir </> "curpos.nix")
          runTestIO
            "__curPos names the imported file"
            canonicalDir
            "let p = import ./curpos.nix; in \"${p.file}:${toString p.line}:${toString p.column}\""
            (mkStr (T.pack file <> ":2:3")),
        -- A string is coerced to a path only when it spells an absolute
        -- one; upstream refuses a relative string outright.
        runTestIOFail "import refuses a relative string" testDir "import \"./literal.nix\"",
        -- pathExists
        runTestIO
          "pathExists true"
          testDir
          ("builtins.pathExists " <> nixQuotedPath (testDir </> "literal.nix"))
          (VBool True),
        runTestIO
          "pathExists false"
          testDir
          ("builtins.pathExists " <> nixQuotedPath (testDir </> "nope.nix"))
          (VBool False),
        -- readFile
        runTestIO
          "readFile contents"
          testDir
          ("builtins.readFile " <> nixQuotedPath (testDir </> "literal.nix"))
          (mkStr "42"),
        runTestIOFail
          "readFile missing -> error"
          testDir
          ("builtins.readFile " <> nixQuotedPath (testDir </> "ghost.nix")),
        -- readDir: entries have correct file types
        runTestIO
          "readDir classifies directory"
          testDir
          ("(builtins.readDir " <> nixQuotedPath testDir <> ").sub")
          (mkStr "directory"),
        runTestIO
          "readDir classifies regular file"
          testDir
          ("builtins.getAttr \"literal.nix\" (builtins.readDir " <> nixQuotedPath testDir <> ")")
          (mkStr "regular")
      ]

-- ---------------------------------------------------------------------------
-- Tests: attribute names a program computes
-- ---------------------------------------------------------------------------

-- | Every reader of a computed attribute name reads it as upstream's
-- @forceStringNoCtx@ does (eval.cc and primops.cc at 2.24.9): a string,
-- with a name that has context refused, naming the context's first
-- element, and the same name with its context discarded read as before.
testAttrNameContext :: IO [Bool]
testAttrNameContext = do
  putStrLn "eval/attr-name-context"
  let named body =
        "let c = builtins.appendContext \"a\" { \"/nix/store/00000000000000000000000000000000-a\" = { path = true; }; };"
          <> " p = builtins.unsafeDiscardStringContext c; in "
          <> body
      refusal = "the string 'a' is not allowed to refer to a store path (such as '00000000000000000000000000000000-a')"
      refused reader body = runTest (reader <> " refuses a name with context") (assertEvalError reader (named body) refusal)
      kept reader body = runTest (reader <> " reads a name without context") (assertEval reader (named body) (VBool True))
  sequence
    [ refused "listToAttrs" "builtins.listToAttrs [ { name = c; value = 1; } ]",
      kept "listToAttrs" "builtins.listToAttrs [ { name = p; value = 1; } ] == { a = 1; }",
      refused "a dynamic attribute" "{ ${c} = 1; }",
      kept "a dynamic attribute" "{ ${p} = 1; } == { a = 1; }",
      refused "a select" "{ a = 1; }.${c}",
      kept "a select" "{ a = 1; }.${p} == 1",
      refused "a has-attr test" "{ a = 1; } ? ${c}",
      kept "a has-attr test" "{ a = 1; } ? ${p}",
      refused "getAttr" "builtins.getAttr c { a = 1; }",
      kept "getAttr" "builtins.getAttr p { a = 1; } == 1",
      refused "hasAttr" "builtins.hasAttr c { a = 1; }",
      kept "hasAttr" "builtins.hasAttr p { a = 1; }",
      refused "removeAttrs" "builtins.removeAttrs { a = 1; } [ c ]",
      kept "removeAttrs" "builtins.removeAttrs { a = 1; b = 2; } [ p ] == { b = 2; }",
      refused "catAttrs" "builtins.catAttrs c [ { a = 1; } ]",
      kept "catAttrs" "builtins.catAttrs p [ { a = 1; } ] == [ 1 ]",
      refused "groupBy" "builtins.groupBy (x: c) [ 1 ]",
      kept "groupBy" "builtins.groupBy (x: p) [ 1 ] == { a = [ 1 ]; }",
      -- The wrapper makes its output attributes with listToAttrs.
      refused "derivation" "builtins.seq (derivation { name = \"w\"; system = \"x\"; builder = \"/bin/sh\"; outputs = [ \"out\" c ]; }) null",
      -- forceStringNoCtx is forceString first, whose refusal of a value
      -- that is not a string is upstream's.
      runTest "a name that is not a string is refused with forceString's message" $
        assertEvalError "getAttr-int" "builtins.getAttr 1 { }" "expected a string but found an integer: 1",
      runTest "a select refuses a null name" $
        assertEvalError "select-null" "{ a = 1; }.${null}" "expected a string but found null: null",
      runTest "a dynamic attribute whose name is null binds nothing" $
        assertEval "dynamic-null" "{ ${null} = 1; } == { }" (VBool True)
    ]

-- ---------------------------------------------------------------------------
-- Tests: builtins that read a plain string
-- ---------------------------------------------------------------------------

-- | Source binding @c@ to the text with a plain path in its context and
-- @p@ to the same text without it, then evaluating the body.
withPlainStrings :: Text -> Text -> Text
withPlainStrings text body =
  "let c = builtins.appendContext \""
    <> text
    <> "\" { \"/nix/store/00000000000000000000000000000000-a\" = { path = true; }; };"
    <> " p = builtins.unsafeDiscardStringContext c; in "
    <> body

-- | Upstream's refusal of the string 'withPlainStrings' binds to @c@.
plainStringRefusal :: Text -> Text
plainStringRefusal text =
  "the string '" <> text <> "' is not allowed to refer to a store path (such as '00000000000000000000000000000000-a')"

-- | Every builtin that reads a plain string reads it as upstream's
-- @forceStringNoCtx@ does (eval.cc and primops.cc): a string with
-- context is refused, naming the context's first element, and the same
-- string with its context discarded is read as before.  @split@ gives
-- back a string its pattern does not match, context and all, as
-- upstream's returns its argument (@prim_split@ in primops.cc).
testPlainStringContext :: IO [Bool]
testPlainStringContext = do
  putStrLn "eval/plain-string-context"
  let refused reader text body =
        runTest (reader <> " refuses a string with context") $
          assertEvalError reader (withPlainStrings text body) (plainStringRefusal text)
      kept reader text body =
        runTest (reader <> " reads a string without context") $
          assertEval reader (withPlainStrings text body) (VBool True)
  sequence
    [ refused "match" "a" "builtins.match c \"a\"",
      kept "match" "a" "builtins.match p \"a\" == [ ]",
      refused "match applied in two steps" "a" "let m = builtins.match c; in m \"a\"",
      kept "match applied in two steps" "a" "let m = builtins.match p; in m \"a\" == [ ]",
      runTest "match takes a pattern with context, refusing it only once applied to a subject" $
        assertEval "match partial" (withPlainStrings "a" "builtins.isFunction (builtins.match c)") (VBool True),
      refused "split" "a" "builtins.split c \"bab\"",
      kept "split" "a" "builtins.split p \"bab\" == [ \"b\" [ ] \"b\" ]",
      refused "split applied in two steps" "a" "let s = builtins.split c; in s \"bab\"",
      refused "getEnv" "HOME" "builtins.getEnv c",
      kept "getEnv" "HOME" "builtins.getEnv p == \"\"",
      refused "toFile" "a" "builtins.toFile c \"x\"",
      refused "hashString" "sha256" "builtins.hashString c \"\"",
      kept "hashString" "sha256" "builtins.hashString p \"\" == \"e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855\"",
      refused "hashFile" "sha256" "builtins.hashFile c /nonexistent",
      refused "fromJSON" "1" "builtins.fromJSON c",
      kept "fromJSON" "1" "builtins.fromJSON p == 1",
      refused "placeholder" "out" "builtins.placeholder c",
      kept "placeholder" "out" "builtins.placeholder p == \"/1rz4g4znpzjwh1xymhjpm42vipw92pr73vdgl6xs1hycac8kf2n9\"",
      refused "parseDrvName" "a-1" "builtins.parseDrvName c",
      kept "parseDrvName" "a-1" "builtins.parseDrvName p == { name = \"a\"; version = \"1\"; }",
      refused "splitVersion" "1.2" "builtins.splitVersion c",
      kept "splitVersion" "1.2" "builtins.splitVersion p == [ \"1\" \"2\" ]",
      refused "compareVersions' first argument" "1" "builtins.compareVersions c \"1\"",
      refused "compareVersions' second argument" "1" "builtins.compareVersions \"1\" c",
      kept "compareVersions" "1" "builtins.compareVersions p p == 0",
      refused "findFile" "a" "builtins.findFile [ ] c",
      -- Upstream reads every entry before the name.
      runTest "findFile reports a failing entry ahead of a name with context" $
        assertEvalError "findFile entry first" (withPlainStrings "a" "builtins.findFile [ (throw \"entry\") ] c") "entry",
      -- forceStringNoCtx is forceString first, whose refusal of a value
      -- that is not a string is upstream's.
      runTest "a pattern that is not a string is refused with forceString's message" $
        assertEvalError "match-int" "builtins.match 1 \"a\"" "expected a string but found an integer: 1",
      runTest "a second version that is not a string is refused with forceString's message" $
        assertEvalError "compareVersions-null" "builtins.compareVersions \"1\" null" "expected a string but found null: null",
      -- The strings read with forceString keep taking context.
      runTest "match reads a subject with context, its groups carrying none" $
        assertEval "match subject" (withPlainStrings "a" "builtins.map builtins.hasContext (builtins.match \"(a)\" c) == [ false ]") (VBool True),
      runTest "hashString hashes a string with context" $
        assertEval "hashString input" (withPlainStrings "a" "builtins.hashString \"sha256\" c == builtins.hashString \"sha256\" p") (VBool True),
      runTest "split gives back a string its pattern does not match, context and all" $
        assertEval
          "split unmatched"
          (withPlainStrings "a" "let r = builtins.split \"x\" c; in r == [ c ] && builtins.getContext (builtins.head r) == builtins.getContext c")
          (VBool True),
      runTest "split applied in two steps gives back an unmatched string's context" $
        assertEval "split unmatched partial" (withPlainStrings "a" "let s = builtins.split \"x\"; in builtins.hasContext (builtins.head (s c))") (VBool True),
      runTest "the pieces of a string split matches carry no context" $
        assertEval
          "split matched"
          (withPlainStrings "a" "let r = builtins.split \"(a)\" c; in builtins.map builtins.hasContext [ (builtins.elemAt r 0) (builtins.head (builtins.elemAt r 1)) (builtins.elemAt r 2) ] == [ false false false ]")
          (VBool True)
    ]

-- | The readers of a plain string that reach the store or the
-- filesystem, with a string without context: each reads it as before.
testPlainStringContextIO :: IO [Bool]
testPlainStringContextIO = do
  putStrLn "eval/plain-string-context-io"
  tmpBase <- getTemporaryDirectory
  let dir = tmpBase </> "nova-nix-test-plain-string-context"
      file = nixQuotedPath (dir </> "f")
  Dir.removePathForcibly dir
  createDirectoryIfMissing True dir
  BS.writeFile (dir </> "f") "contents"
  results <-
    sequence
      [ runTestIO "toFile reads a name without context" dir (withPlainStrings "a" "builtins.toFile p \"x\" == builtins.toFile \"a\" \"x\"") (VBool True),
        runTestIO "hashFile reads an algorithm without context" dir (withPlainStrings "sha256" ("builtins.hashFile p " <> file)) (mkStr "d1b2a59fbea7e20077af9f91b27e95e865061b270be03ff539ab3b73587882e8"),
        runTestIO
          "findFile reads a name without context"
          dir
          (withPlainStrings "a" ("builtins.findFile [ { prefix = \"a\"; path = " <> nixQuotedPath dir <> "; } ] p"))
          (VPath (canonPathValue (T.pack dir)))
      ]
  Dir.removePathForcibly dir
  pure results

-- ---------------------------------------------------------------------------
-- Tests: builtins.unsafeGetAttrPos
-- ---------------------------------------------------------------------------

-- | The file the derivation wrapper's positions name, upstream's name for
-- its wrapper.
derivationWrapperFile :: Text
derivationWrapperFile = "/derivation-internal.nix"

-- | Each expected position is derived from upstream's source at 2.24.9:
-- @prim_unsafeGetAttrPos@ (primops.cc) answers @EvalState::mkPos@ of the
-- attribute's position, a set of @file@, @line@ and @column@ for one in a
-- file and null for none; the parser records a binding at its attribute
-- path's first token and an inherited name at itself (parser.y,
-- parser-state.hh); and the builtins that copy an attribute copy its
-- position.  The expressions put every attribute under test past line 1,
-- so the @builtins.toJSON (@ the helper prefixes moves no column.
testUnsafeGetAttrPos :: IO [Bool]
testUnsafeGetAttrPos = do
  putStrLn "eval/unsafeGetAttrPos"
  sequence
    [ inFile "a binding is at its name" "builtins.unsafeGetAttrPos \"a\" {\n  a = 1;\n}" (2, 3),
      noPosition "a missing attribute has none" "builtins.unsafeGetAttrPos \"b\" {\n  a = 1;\n}",
      inFile "an inherited name is at itself" "let x = 1; in builtins.unsafeGetAttrPos \"x\" {\n  inherit x;\n}" (2, 11),
      inFile "a name inherited from a set is at itself" "builtins.unsafeGetAttrPos \"y\" {\n  inherit ({ x = 1; y = 2; }) x y;\n}" (2, 33),
      inFile "a quoted inherited name is at its quote" "builtins.unsafeGetAttrPos \"or\" {\n  inherit ({ or = 1; }) \"or\";\n}" (2, 25),
      inFile "a quoted static name is at its quote" "builtins.unsafeGetAttrPos \"q\" {\n  \"q\" = 1;\n}" (2, 3),
      -- A column counts bytes (PosTable::operator[], nixexpr.cc), so the
      -- e-acute takes two.
      inFile "a column counts the bytes before it" "builtins.unsafeGetAttrPos \"b\" {\n  a = \"\233\"; b = 1;\n}" (2, 13),
      inFile "a dynamic name is at its interpolation" "builtins.unsafeGetAttrPos \"k\" {\n  ${\"k\" + \"\"} = 1;\n}" (2, 3),
      inFile "a name of an interpolated plain string is at its interpolation" "builtins.unsafeGetAttrPos \"k\" {\n  ${\"k\"} = 1;\n}" (2, 3),
      inFile "each level of a dotted path is at the path" "builtins.unsafeGetAttrPos \"c\" {\n  a.b.c = 1;\n}.a.b" (2, 3),
      inFile "a merged path's first definition keeps its position" "builtins.unsafeGetAttrPos \"a\" {\n  a.b = 1;\n  a.c = 2;\n}" (2, 3),
      -- A line ends at an LF, a CRLF or a bare CR (Pos::LinesIterator,
      -- position.cc), so the file spelled with each gives one line.
      inFile "a line ends at an LF" (afterCommentLine "\n") (3, 3),
      inFile "a line ends at a CRLF" (afterCommentLine "\r\n") (3, 3),
      inFile "a line ends at a bare CR" (afterCommentLine "\r") (3, 3),
      inFile "a path merged into keeps each definition's own" "builtins.unsafeGetAttrPos \"c\" {\n  a.b = 1;\n  a.c = 2;\n}.a" (3, 3),
      inFile "a set merged into a path keeps its bindings' own" "builtins.unsafeGetAttrPos \"b\" {\n  a.c = 1;\n  a = {\n    b = 2;\n  };\n}.a" (4, 5),
      inFile "a rec set's binding is at its name" "builtins.unsafeGetAttrPos \"b\" rec {\n  a = 1;\n  b = a;\n}" (3, 3),
      inFile "a rec set's dynamic name is at its interpolation" "builtins.unsafeGetAttrPos \"b\" rec {\n  a = \"b\";\n  ${a} = 2;\n}" (3, 3),
      inFile "a rec set with a dynamic name keeps its static names'" "builtins.unsafeGetAttrPos \"a\" rec {\n  a = \"b\";\n  ${a} = 2;\n}" (2, 3),
      inFile "an update keeps the left side's position" "builtins.unsafeGetAttrPos \"a\" ({\n  a = 1;\n  b = 2;\n} // {\n  b = 3;\n})" (2, 3),
      inFile "an update takes the right side's position with its value" "builtins.unsafeGetAttrPos \"b\" ({\n  a = 1;\n  b = 2;\n} // {\n  b = 3;\n})" (5, 3),
      inFile "removeAttrs keeps the positions it keeps" "builtins.unsafeGetAttrPos \"b\" (builtins.removeAttrs {\n  a = 1;\n  b = 2;\n} [ \"a\" ])" (3, 3),
      inFile "intersectAttrs keeps the right set's position" "builtins.unsafeGetAttrPos \"a\" (builtins.intersectAttrs { a = null; } {\n  a = 1;\n})" (2, 3),
      -- Upstream walks the left set when it is the smaller, and keeps the
      -- attribute it finds in the right (prim_intersectAttrs in
      -- primops.cc).
      inFile "intersectAttrs keeps the right set's position when the left is smaller" "builtins.unsafeGetAttrPos \"a\" (builtins.intersectAttrs { a = null; } {\n  a = 1;\n  b = 2;\n})" (2, 3),
      inFile "listToAttrs is at the element's value" "builtins.unsafeGetAttrPos \"k\" (builtins.listToAttrs [ {\n  name = \"k\";\n  value = 1;\n} ])" (3, 3),
      noPosition "mapAttrs makes attributes with none" "builtins.unsafeGetAttrPos \"a\" (builtins.mapAttrs (n: v: v) {\n  a = 1;\n})",
      noPosition "a builtin has none" "builtins.unsafeGetAttrPos \"map\" builtins",
      -- A derivation's own attributes are on the wrapper's lines
      -- ('derivationWrapperSource'), under upstream's file name.
      inWrapper "a derivation's type is in the wrapper" "type" (7, 7),
      inWrapper "a derivation's output is at the wrapper's listToAttrs value" "out" (6, 5),
      inWrapper "a derivation's inherited drvAttrs is at the name" "drvAttrs" (17, 15),
      inFile "a derivation keeps its argument's positions" (derivationAttrPos "name") (2, 3),
      -- Text with no file behind it records positions that name none,
      -- as upstream's Pos::String origin makes mkPos answer null.
      runTest "text with no file gives no position" $
        assertEval "text origin" "builtins.unsafeGetAttrPos \"a\" {\n  a = 1;\n}" VNull,
      runTest "the name must be a string" $
        assertEvalError "name" "builtins.unsafeGetAttrPos 1 { }" "expected a string but found an integer: 1",
      -- The set throws when forced, so only a name checked before the set
      -- is forced reports the name.
      runTest "the name is checked before the set is forced" $
        assertEvalError "name first" "builtins.unsafeGetAttrPos 1 (throw \"the set\")" "expected a string but found an integer: 1",
      runTest "the name may not carry context" $
        assertEvalError
          "name context"
          "builtins.unsafeGetAttrPos (builtins.appendContext \"x\" { \"/nix/store/00000000000000000000000000000000-a\" = { path = true; }; }) { }"
          "the string 'x' is not allowed to refer to a store path (such as '00000000000000000000000000000000-a')",
      runTest "the second argument must be a set" $
        assertEvalError "set" "builtins.unsafeGetAttrPos \"a\" 1" "expected a set but found an integer: 1"
    ]
  where
    inFile label source (line, column) = jsonInFile label source (positionJson positionsFile line column)
    inWrapper label name (line, column) = jsonInFile label (derivationAttrPos name) (positionJson derivationWrapperFile line column)
    noPosition label source = jsonInFile label source "null"
    derivationAttrPos name =
      "builtins.unsafeGetAttrPos \"" <> name <> "\" (derivation {\n  name = \"n\";\n  builder = \"b\";\n  system = \"s\";\n})"
    afterCommentLine ending =
      T.intercalate ending ["builtins.unsafeGetAttrPos \"b\" {", "  # a comment", "  b = 1;", "}"]

-- ---------------------------------------------------------------------------
-- Tests: Batch A - getEnv, currentTime, toPath
-- ---------------------------------------------------------------------------

testBatchA :: IO [Bool]
testBatchA = do
  putStrLn "eval/builtins-batchA"
  sequence
    [ -- getEnv
      runTest "getEnv pure returns empty" $
        assertEval "getEnv-pure" "builtins.getEnv \"HOME\"" (mkStr ""),
      runTest "getEnv type error" $
        assertEvalFail "getEnv-err" "builtins.getEnv 42",
      -- toPath
      runTest "toPath absolute" $
        assertEval "toPath-abs" "builtins.toPath \"/foo/bar\"" (mkStr "/foo/bar"),
      runTest "toPath rejects relative" $
        assertEvalFail "toPath-rel" "builtins.toPath \"foo/bar\"",
      runTest "toPath of a path gives its string" $
        assertEval "toPath-vpath" "builtins.toPath /foo/bar" (mkStr "/foo/bar"),
      runTest "toPath type error" $
        assertEvalFail "toPath-err" "builtins.toPath 42",
      runTest "toPath rejects empty" $
        assertEvalFail "toPath-empty" "builtins.toPath \"\"",
      -- currentTime
      runTest "currentTime is int" $
        assertEval "currentTime" "builtins.typeOf builtins.currentTime" (mkStr "int"),
      runTest "currentTime is 0 in pure" $
        assertEval "currentTime-pure" "builtins.currentTime" (VInt 0),
      runTest "currentTime >= 0" $
        assertEval "currentTime-pos" "builtins.currentTime >= 0" (VBool True)
    ]

-- ---------------------------------------------------------------------------
-- Tests: Batch A - IO tests (getEnv)
-- ---------------------------------------------------------------------------

testBatchAIO :: IO [Bool]
testBatchAIO = do
  putStrLn "eval/builtins-batchA-io"
  tmpBase <- getTemporaryDirectory
  let testDir = tmpBase </> "nova-nix-test-batchA"
  bracket_
    (createDirectoryIfMissing True testDir)
    ( do
        exists <- doesDirectoryExist testDir
        when exists (removeDirectoryRecursive testDir)
    )
    $ sequence
      [ -- getEnv HOME should be non-empty in IO mode
        do
          result <- evalNixIO testDir "builtins.getEnv \"PATH\""
          runTest "getEnv PATH non-empty (IO)" $ assertRight "getEnv-io" result $ \val ->
            case val of
              VStr s _ -> if BS.null s then Fail "PATH was empty" else Pass
              _ -> Fail ("expected VStr, got " <> T.pack (show val)),
        -- currentTime in IO should be > 0
        do
          result <- evalNixIO testDir "builtins.currentTime"
          runTest "currentTime > 0 (IO)" $ assertRight "currentTime-io" result $ \val ->
            case val of
              VInt n -> if n > 0 then Pass else Fail ("expected > 0, got " <> T.pack (show n))
              _ -> Fail ("expected VInt, got " <> T.pack (show val))
      ]

-- ---------------------------------------------------------------------------
-- Tests: environment values as bytes
-- ---------------------------------------------------------------------------

-- | An environment value reaches the evaluator as its bytes, through
-- @builtins.getEnv@ and through @NIX_PATH@, as upstream's @getenv@ reads
-- it; each case's outcome was read off nix-instantiate 2.33.2 under the
-- same environment.  A POSIX environment is bytes and a Windows one is
-- UTF-16, so each platform runs the cases its environment can express.
testEnvironmentBytes :: IO [Bool]
testEnvironmentBytes = do
  putStrLn "eval/environment-bytes"
  tmpBase <- getTemporaryDirectory
  let testDir = tmpBase </> "nova-nix-test-env-bytes"
      pkgDir = testDir </> "mypkg"
  bracket_
    ( do
        createDirectoryIfMissing True pkgDir
        BS.writeFile (pkgDir </> "default.nix") "42"
        BS.writeFile (testDir </> "name.bin") nonUtf8EnvName
    )
    (Dir.removePathForcibly testDir)
    $ do
      getEnvResults <- mapM (getEnvCase testDir) envByteCases
      nixPathResults <- mapM (nixPathCase testDir) envByteCases
      rest <-
        sequence
          [ withEnvBytes "a lookup passes a NIX_PATH entry whose prefix has no UTF-8 reading" (TE.encodeUtf8 nixPathVarName) ("\xFC=/nonexistent:mypkg=" <> TE.encodeUtf8 (T.pack pkgDir)) $
              assertEqual "lookup" (Right (VInt 42)) <$> evalNixIOWithNixPath testDir "import <mypkg>",
            withEnvBytes "getEnv looks a name up by its bytes" nonUtf8EnvName "hello" $
              expectBytes "name" "hello" <$> evalNixIO testDir "builtins.getEnv (builtins.readFile ./name.bin)",
            unpairedSurrogateGetEnv testDir,
            unpairedSurrogateNixPath testDir
          ]
      pure (getEnvResults ++ nixPathResults ++ rest)
  where
    getEnvCase dir (label, bytes) =
      withEnvBytes ("getEnv carries " <> label) (TE.encodeUtf8 testEnvVarName) bytes $
        expectBytes "getEnv" bytes <$> evalNixIO dir ("builtins.getEnv \"" <> testEnvVarName <> "\"")
    nixPathCase dir (label, bytes) =
      withEnvBytes ("NIX_PATH carries " <> label <> " into prefix and path") (TE.encodeUtf8 nixPathVarName) (bytes <> "=/x" <> bytes) $
        expectBytes "nixPath" (bytes <> "|/x" <> bytes) <$> evalNixIOWithNixPath dir "let e = builtins.head builtins.nixPath; in e.prefix + \"|\" + e.path"
    expectBytes label expected = \case
      Right (VStr actual _) -> assertEqual label expected actual
      other -> Fail (label <> ": expected a string, got " <> T.pack (show other))

-- | The values the environment cases carry: a byte with no UTF-8 reading,
-- a multibyte UTF-8 character, and nothing at all.
envByteCases :: [(Text, BS.ByteString)]
envByteCases =
  [ ("a byte with no UTF-8 reading", "\xFC"),
    ("a multibyte UTF-8 value", "caf\xC3\xA9"),
    ("an empty value", "")
  ]

testEnvVarName :: Text
testEnvVarName = "NOVA_NIX_TEST_ENV_BYTES"

nixPathVarName :: Text
nixPathVarName = "NIX_PATH"

-- | A variable name holding a byte with no UTF-8 reading.
nonUtf8EnvName :: BS.ByteString
nonUtf8EnvName = "NOVA_NIX_TEST_ENV_\xFC"

-- | Run one case with a variable set to the given bytes, restoring it
-- afterwards.  A Windows environment is UTF-16 and cannot hold a byte
-- with no UTF-8 reading, so such a case skips there.
withEnvBytes :: Text -> BS.ByteString -> BS.ByteString -> IO TestResult -> IO Bool
withEnvBytes label name value check = do
  spelledName <- envSpelling name
  spelledValue <- envSpelling value
  case (,) <$> spelledName <*> spelledValue of
    Nothing -> True <$ putStrLn ("  SKIP  " <> T.unpack label <> ": a Windows environment cannot hold a byte with no UTF-8 reading")
    Just (nameSpelling, valueSpelling) -> runTestM label (withEnvVar nameSpelling valueSpelling check)

-- | A Windows value holding an unpaired UTF-16 surrogate has no UTF-8
-- form, so getEnv refuses it rather than substituting U+FFFD.
unpairedSurrogateGetEnv :: FilePath -> IO Bool
unpairedSurrogateGetEnv dir
  | SI.os /= "mingw32" = True <$ putStrLn unpairedSurrogateSkip
  | otherwise =
      runTestM "getEnv refuses a value holding an unpaired surrogate" $
        withEnvVar (T.unpack testEnvVarName) "a\xD800" $ do
          result <- evalNixIO dir ("builtins.getEnv \"" <> testEnvVarName <> "\"")
          pure $ case result of
            Left err | unpairedSurrogateWords `T.isInfixOf` err -> Pass
            other -> Fail ("expected the unpaired-surrogate refusal, got " <> T.pack (show other))

-- | The same refusal for @NIX_PATH@, raised as the evaluation state is
-- created.
unpairedSurrogateNixPath :: FilePath -> IO Bool
unpairedSurrogateNixPath dir
  | SI.os /= "mingw32" = True <$ putStrLn unpairedSurrogateSkip
  | otherwise =
      runTestM "NIX_PATH holding an unpaired surrogate is refused" $
        withEnvVar (T.unpack nixPathVarName) "a=\xD800" $ do
          storeDir <- evalNixIOStoreDir
          outcome <- try (nixPathSearchPath >>= \searchPaths -> newEvalStateWith unrestrictedPolicy searchPaths (testSessionStore storeDir) storeDir dir)
          pure $ case outcome of
            Left (err :: IOException)
              | unpairedSurrogateWords `T.isInfixOf` T.pack (displayException err) -> Pass
              | otherwise -> Fail ("wrong error: " <> T.pack (displayException err))
            Right _ -> Fail "NIX_PATH was accepted"

unpairedSurrogateWords :: Text
unpairedSurrogateWords = "unpaired UTF-16 surrogate"

unpairedSurrogateSkip :: String
unpairedSurrogateSkip = "  SKIP  an unpaired UTF-16 surrogate exists only in a Windows environment"

-- ---------------------------------------------------------------------------
-- Tests: Batch B - placeholder, storePath
-- ---------------------------------------------------------------------------

testBatchB :: IO [Bool]
testBatchB = do
  putStrLn "eval/builtins-batchB"
  sequence
    [ -- placeholder
      runTest "placeholder out matches Nix hashPlaceholder" $
        assertRight "placeholder-out" (evalNix "builtins.placeholder \"out\"") $ \val ->
          case val of
            VStr p _ -> assertEqual "placeholder out" "/1rz4g4znpzjwh1xymhjpm42vipw92pr73vdgl6xs1hycac8kf2n9" p
            _ -> Fail ("expected VStr, got " <> T.pack (show val)),
      runTest "placeholder deterministic" $
        assertRight "placeholder-det" (evalNix "builtins.placeholder \"out\" == builtins.placeholder \"out\"") $ \val ->
          assertEqual "deterministic" (VBool True) val,
      runTest "placeholder out /= placeholder dev" $
        assertRight "placeholder-diff" (evalNix "builtins.placeholder \"out\" == builtins.placeholder \"dev\"") $ \val ->
          assertEqual "different" (VBool False) val,
      runTest "placeholder type error" $
        assertEvalFail "placeholder-err" "builtins.placeholder 42",
      -- storePath returns a STRING marked already-in-store (SCPlain context),
      -- not a bare path - the marker is what stops a later coercion re-NARing it.
      runTest "storePath returns an in-store string" $
        assertEval
          "storePath-isstring"
          "builtins.isString (builtins.storePath \"/nix/store/s66mzxpvicwk07gjbjfw9izjfa797vsw-hello-2.12.1\")"
          (VBool True),
      runTest "storePath result carries a plain-path context" $
        assertEval
          "storePath-hasctx"
          "(builtins.getContext (builtins.storePath \"/nix/store/s66mzxpvicwk07gjbjfw9izjfa797vsw-hello-2.12.1\")).\"/nix/store/s66mzxpvicwk07gjbjfw9izjfa797vsw-hello-2.12.1\".path"
          (VBool True),
      runTest "storePath preserves the path text" $
        assertEval
          "storePath-text"
          "builtins.unsafeDiscardStringContext (builtins.storePath \"/nix/store/s66mzxpvicwk07gjbjfw9izjfa797vsw-hello-2.12.1\")"
          (mkStr "/nix/store/s66mzxpvicwk07gjbjfw9izjfa797vsw-hello-2.12.1"),
      runTest "storePath invalid" $
        assertEvalFail "storePath-bad" "builtins.storePath \"/tmp/not-a-store-path\"",
      runTest "storePath type error" $
        assertEvalFail "storePath-err" "builtins.storePath 42"
    ]

-- ---------------------------------------------------------------------------
-- Tests: Batch C - findFile
-- ---------------------------------------------------------------------------

testBatchC :: IO [Bool]
testBatchC = do
  putStrLn "eval/builtins-batchC"
  sequence
    [ runTest "findFile empty list errors" $
        assertEvalFail "findFile-empty" "builtins.findFile [ ] \"foo\"",
      runTest "findFile type error arg1" $
        assertEvalFail "findFile-err1" "builtins.findFile 42 \"foo\"",
      runTest "findFile type error arg2" $
        assertEvalFail "findFile-err2" "builtins.findFile [ ] 42"
    ]

testBatchCIO :: IO [Bool]
testBatchCIO = do
  putStrLn "eval/builtins-batchC-io"
  tmpBase <- getTemporaryDirectory
  let testDir = tmpBase </> "nova-nix-test-batchC"
      nixpkgsDir = testDir </> "nixpkgs"
  bracket_
    ( do
        createDirectoryIfMissing True nixpkgsDir
        TIO.writeFile (nixpkgsDir </> "default.nix") "42"
    )
    ( do
        exists <- doesDirectoryExist testDir
        when exists (removeDirectoryRecursive testDir)
    )
    $ sequence
      [ runTestIO
          "findFile with matching entry"
          testDir
          ( "builtins.findFile [ { prefix = \"nixpkgs\"; path = "
              <> nixQuotedPath nixpkgsDir
              <> "; } ] \"nixpkgs\""
          )
          (VPath (canonPathValue (T.pack nixpkgsDir))),
        runTestIOFail
          "findFile no match"
          testDir
          "builtins.findFile [ { prefix = \"other\"; path = \"/nope\"; } ] \"nixpkgs\""
      ]

-- ---------------------------------------------------------------------------
-- Tests: Blackhole (infinite recursion detection)
-- ---------------------------------------------------------------------------

testBlackhole :: IO [Bool]
testBlackhole = do
  putStrLn "eval/blackhole"
  tmpBase <- getTemporaryDirectory
  sequence
    [ runTestIOFail "let x = x; in x" tmpBase "let x = x; in x",
      runTestIOFail "rec { a = a; }.a" tmpBase "rec { a = a; }.a",
      runTestIOFail "let a = b; b = a; in a" tmpBase "let a = b; b = a; in a",
      -- Non-recursive cases must still work
      runTestIO "rec { a = 1; b = a; }.b" tmpBase "rec { a = 1; b = a; }.b" (VInt 1),
      runTestIO "let a = 1; b = a + 1; in b" tmpBase "let a = 1; b = a + 1; in b" (VInt 2)
    ]

-- ---------------------------------------------------------------------------
-- Tests: Eval - Call depth (max-call-depth)
-- ---------------------------------------------------------------------------

-- | The issue's counter recursion to a given depth.
counterRecursion :: Int -> Text
counterRecursion n = "let f = n: if n == 0 then 0 else 1 + f (n - 1); in f " <> T.pack (show n)

-- | The identity applied to itself syntactically @n@ deep: a chain of pure
-- lambda frames with nothing else counted.
lambdaNesting :: Int -> Text
lambdaNesting n = "let f = x: x; in " <> T.replicate n "f (" <> "0" <> T.replicate n ")"

-- | Upstream's message, which the CLI prints after its own "error: ".
maxCallDepthMessage :: Text
maxCallDepthMessage = "stack overflow; max-call-depth exceeded"

-- | A result that must be the max-call-depth error, word for word.
expectMaxCallDepth :: Text -> Either Text NixValue -> TestResult
expectMaxCallDepth label result = case result of
  Left err
    | err == maxCallDepthMessage -> Pass
    | otherwise -> Fail (label <> ": wrong error: " <> err)
  Right val -> Fail (label <> ": expected the max-call-depth error but got: " <> T.pack (show val))

-- | IO evaluation under an explicit call-depth ceiling.
evalAtCeiling :: Word32 -> Text -> IO (Either Text NixValue)
evalAtCeiling limit source = case parseNix "." testOrigin (TE.encodeUtf8 source) of
  Left err -> pure (Left (T.pack (show err)))
  Right parsed -> do
    storeDir <- evalNixIOStoreDir
    st0 <- testEvalState storeDir "."
    let st = st0 {esCallDepth = topLevelCallDepth limit}
    runEvalIO st (evalParsed (pure (evalBaseEnv st)) parsed)

-- | The function-call nesting ceiling, upstream's max-call-depth (10000).
-- Every boundary here was read off nix-instantiate 2.33.2 and the 2.24.9
-- source: the count is the frames already active, checked on entry, so
-- 10001 nested lambda calls pass and 10002 fail, and the counter
-- recursion passes at 9999 and fails at 10000, its deepest frame being
-- the @n - 1@ forced inside @f 0@, a builtin call upstream's parser
-- desugars @-@ into.  The cases under a ceiling of 10 pin what costs a
-- frame and what does not, each boundary taken from the same upstream.
testCallDepth :: IO [Bool]
testCallDepth = do
  putStrLn "eval/call-depth"
  sequence
    [ failsIO "the issue expression is refused with upstream's words" (counterRecursion 100000),
      runTestIO "the counter recursion one under the ceiling evaluates" "." (counterRecursion 9999) (VInt 9999),
      failsIO "the counter recursion at the ceiling is refused" (counterRecursion 10000),
      runTest "pure evaluation enforces the ceiling too" $
        assertEval "pure-under" (lambdaNesting 10001) (VInt 0),
      runTest "pure evaluation refuses past the ceiling with upstream's words" $
        expectMaxCallDepth "pure-over" (evalNix (lambdaNesting 10002)),
      -- Under a ceiling of n, n + 1 nested lambda calls pass and n + 2 fail.
      passesAt 10 "lambda nesting one past the ceiling passes" (lambdaNesting 11) (VInt 0),
      failsAt 10 "lambda nesting two past the ceiling fails" (lambdaNesting 12),
      -- The operators upstream desugars into builtin calls cost a frame
      -- while their operands are forced; its native operators cost none.
      passesAt 10 "a subtraction at the ceiling passes" (wrapped 10 "1 - 1") (VInt 0),
      failsAt 10 "a subtraction one past the ceiling fails" (wrapped 11 "1 - 1"),
      failsAt 10 "a multiplication one past the ceiling fails" (wrapped 11 "2 * 1"),
      failsAt 10 "a division one past the ceiling fails" (wrapped 11 "2 / 1"),
      failsAt 10 "a less-than one past the ceiling fails" (wrapped 11 "1 < 2"),
      failsAt 10 "a less-or-equal one past the ceiling fails" (wrapped 11 "1 <= 2"),
      failsAt 10 "a greater-than one past the ceiling fails" (wrapped 11 "2 > 1"),
      failsAt 10 "a greater-or-equal one past the ceiling fails" (wrapped 11 "2 >= 1"),
      failsAt 10 "a negation one past the ceiling fails" (wrapped 11 "-1"),
      passesAt 10 "an equality one past the ceiling passes" (wrapped 11 "1 == 1") (VBool True),
      passesAt 10 "an inequality one past the ceiling passes" (wrapped 11 "1 != 2") (VBool True),
      passesAt 10 "a not one past the ceiling passes" (wrapped 11 "!true") (VBool False),
      passesAt 10 "an addition one past the ceiling passes" (wrapped 11 "1 + 1") (VInt 2),
      passesAt 10 "a list concatenation one past the ceiling passes" (wrapped 11 "([1] ++ [2]) == [1 2]") (VBool True),
      passesAt 10 "an attrset update one past the ceiling passes" (wrapped 11 "({ a = 1; } // { b = 2; }) == { a = 1; b = 2; }") (VBool True),
      -- A builtin costs a frame even when partially applied.
      failsAt 10 "a partially applied builtin one past the ceiling fails" (wrapped 11 "builtins.add 0"),
      failsAt 10 "a builtin application one past the ceiling fails" (wrapped 11 "builtins.add 0 0"),
      -- A functor costs two frames: its inner call is a callFunction of its own.
      passesAt 10 "functor nesting of five passes" (functorNesting 5) (VInt 0),
      failsAt 10 "functor nesting of six fails" (functorNesting 6),
      -- A __toString coercion is a callFunction.
      passesAt 10 "a __toString coercion at the ceiling passes" (wrapped 10 "\"${s}\"") (mkStr "a"),
      failsAt 10 "a __toString coercion one past the ceiling fails" (wrapped 11 "\"${s}\""),
      -- A higher-order builtin's callback is applied where its result is
      -- forced, not inside the builtin's frame: upstream's prim_map returns
      -- app values.  Forced by ==, which opens no frame, the callback at
      -- the ceiling passes; forced by elemAt, a frame deeper, it is
      -- elemAt's own call that fails.
      passesAt 10 "a map callback forced at the ceiling passes" (wrapped 10 "(builtins.map f [ 0 ]) == [ 0 ]") (VBool True),
      failsAt 10 "a map callback forced one past the ceiling fails" (wrapped 11 "(builtins.map f [ 0 ]) == [ 0 ]"),
      passesAt 10 "an elemAt over map under the ceiling passes" (wrapped 9 "builtins.elemAt (builtins.map f [ 0 ]) 0") (VInt 0),
      failsAt 10 "an elemAt over map at the ceiling fails" (wrapped 10 "builtins.elemAt (builtins.map f [ 0 ]) 0"),
      -- mapAttrs and zipAttrsWith apply @f name@ a frame below the value
      -- application: upstream builds two nested app values.
      passesAt 10 "a mapAttrs callback under the ceiling passes" (wrapped 9 "(builtins.mapAttrs (n: v: v) { a = 0; }).a") (VInt 0),
      failsAt 10 "a mapAttrs callback at the ceiling fails" (wrapped 10 "(builtins.mapAttrs (n: v: v) { a = 0; }).a"),
      passesAt 10 "a zipAttrsWith callback under the ceiling passes" (wrapped 9 "(builtins.zipAttrsWith (n: vs: 0) [ { a = 1; } ]).a") (VInt 0),
      failsAt 10 "a zipAttrsWith callback at the ceiling fails" (wrapped 10 "(builtins.zipAttrsWith (n: vs: 0) [ { a = 1; } ]).a"),
      -- A sort by builtins.lessThan itself opens no frame per comparison
      -- (upstream's prim_sort bypass); a lambda comparator costs its call
      -- and the < inside it.
      passesAt 10 "a sort by lessThan at the ceiling passes" (wrapped 10 "builtins.sort builtins.lessThan [ 2 1 ] == [ 1 2 ]") (VBool True),
      failsAt 10 "a sort by lessThan one past the ceiling fails" (wrapped 11 "builtins.sort builtins.lessThan [ 2 1 ] == [ 1 2 ]"),
      passesAt 10 "a sort by a lambda two under the ceiling passes" (wrapped 8 "builtins.sort (a: b: a < b) [ 2 1 ] == [ 1 2 ]") (VBool True),
      failsAt 10 "a sort by a lambda one under the ceiling fails" (wrapped 9 "builtins.sort (a: b: a < b) [ 2 1 ] == [ 1 2 ]"),
      passesAt 10 "the counter recursion to 9 passes under a ceiling of 10" (counterRecursion 9) (VInt 9),
      failsAt 10 "the counter recursion to 10 fails under a ceiling of 10" (counterRecursion 10),
      -- An eval error, not a throw: tryEval does not catch it.
      failsAt 10 "tryEval does not catch the ceiling" ("(builtins.tryEval (" <> lambdaNesting 12 <> ")).success"),
      -- A ceiling of 0 still admits one frame: the check is against the
      -- frames already active when the call is entered.
      passesAt 0 "one call passes under a ceiling of 0" "(x: x) 1" (VInt 1),
      failsAt 0 "a nested call fails under a ceiling of 0" "(x: x) ((x: x) 1)",
      passesAt 0 "a bare subtraction passes under a ceiling of 0" "1 - 1" (VInt 0),
      failsAt 0 "a subtraction inside a call fails under a ceiling of 0" "(x: x - 1) 1"
    ]
  where
    failsIO label source = evalNixIO "." source >>= runTest label . expectMaxCallDepth label
    passesAt limit label source expected = do
      result <- evalAtCeiling limit source
      runTest label $ assertRight label result $ assertEqual label expected
    failsAt limit label source = evalAtCeiling limit source >>= runTest label . expectMaxCallDepth label
    wrapped n inner = scope <> T.replicate n "f (" <> inner <> T.replicate n ")"
    functorNesting n = scope <> T.replicate n "g (" <> "0" <> T.replicate n ")"
    scope = "let f = x: x; g = { __functor = self: x: x; }; s = { __toString = self: \"a\"; }; in "

-- ---------------------------------------------------------------------------
-- Tests: Batch D - toFile
-- ---------------------------------------------------------------------------

testBatchD :: IO [Bool]
testBatchD = do
  putStrLn "eval/builtins-batchD"
  sequence
    [ runTest "toFile pure mode error" $
        assertEvalFail "toFile-pure" "builtins.toFile \"hello\" \"world\"",
      runTest "toFile type error arg1" $
        assertEvalFail "toFile-err1" "builtins.toFile 42 \"world\"",
      runTest "toFile type error arg2" $
        assertEvalFail "toFile-err2" "builtins.toFile \"hello\" 42"
    ]

-- ---------------------------------------------------------------------------
-- Tests: Batch E - scopedImport
-- ---------------------------------------------------------------------------

testBatchE :: IO [Bool]
testBatchE = do
  putStrLn "eval/builtins-batchE"
  sequence
    [ runTest "scopedImport pure error" $
        assertEvalFail "scopedImport-pure" "builtins.scopedImport { } ./foo.nix",
      runTest "scopedImport type error arg1" $
        assertEvalFail "scopedImport-err1" "builtins.scopedImport 42 ./foo.nix",
      runTest "scopedImport type error arg2" $
        assertEvalFail "scopedImport-err2" "builtins.scopedImport { } 42"
    ]

testBatchEIO :: IO [Bool]
testBatchEIO = do
  putStrLn "eval/builtins-batchE-io"
  tmpBase <- getTemporaryDirectory
  let testDir = tmpBase </> "nova-nix-test-batchE"
  bracket_
    ( do
        createDirectoryIfMissing True testDir
        TIO.writeFile (testDir </> "scoped.nix") "x"
    )
    ( do
        exists <- doesDirectoryExist testDir
        when exists (removeDirectoryRecursive testDir)
    )
    $ sequence
      [ runTestIO
          "scopedImport injects scope"
          testDir
          ("builtins.scopedImport { x = 42; } " <> nixQuotedPath (testDir </> "scoped.nix"))
          (VInt 42)
      ]

-- ---------------------------------------------------------------------------
-- Tests: Batch F - fetchurl, fetchTarball, fetchGit
-- ---------------------------------------------------------------------------

testBatchF :: IO [Bool]
testBatchF = do
  putStrLn "eval/builtins-batchF"
  sequence
    [ runTest "fetchurl pure error" $
        assertEvalFail "fetchurl-pure" "builtins.fetchurl \"http://example.com\"",
      runTest "fetchurl type error" $
        assertEvalFail "fetchurl-err" "builtins.fetchurl 42",
      runTest "fetchTarball pure error" $
        assertEvalFail "fetchTarball-pure" "builtins.fetchTarball \"http://example.com\"",
      runTest "fetchTarball type error" $
        assertEvalFail "fetchTarball-err" "builtins.fetchTarball 42",
      runTest "fetchGit pure error" $
        assertEvalFail "fetchGit-pure" "builtins.fetchGit \"http://example.com\"",
      runTest "fetchGit type error" $
        assertEvalFail "fetchGit-err" "builtins.fetchGit 42"
    ]

-- ---------------------------------------------------------------------------
-- Tests: Batch H - derivation
-- ---------------------------------------------------------------------------

testBatchH :: IO [Bool]
testBatchH = do
  putStrLn "eval/builtins-batchH"
  let withTwoOutputs expr =
        "let D = derivation { name = \"x\"; system = \"s\"; builder = \"b\"; outputs = [ \"out\" \"dev\" ]; }; in " <> expr
  sequence
    [ runTest "derivation has type" $
        assertEval
          "drv-type"
          "let d = derivation { name = \"hello\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; }; in d.type"
          (mkStr "derivation"),
      runTest "derivation has drvPath" $
        assertRight "drv-drvPath" (evalNix "let d = derivation { name = \"hello\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; }; in d.drvPath") $ \val ->
          case val of
            VStr p ctx ->
              if "/nix/store/" `BS.isPrefixOf` p && ".drv" `BS.isSuffixOf` p && ctx /= emptyContext
                then Pass
                else Fail ("bad drvPath: " <> bytesText p)
            _ -> Fail ("expected VStr with context, got " <> T.pack (show val)),
      runTest "derivation has outPath" $
        assertRight "drv-outPath" (evalNix "let d = derivation { name = \"hello\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; }; in d.outPath") $ \val ->
          case val of
            VStr p ctx ->
              if "/nix/store/" `BS.isPrefixOf` p && ctx /= emptyContext
                then Pass
                else Fail ("bad outPath: " <> bytesText p)
            _ -> Fail ("expected VStr with context, got " <> T.pack (show val)),
      -- 'derivation' is lazy (matches C++ Nix): the missing-required-attribute
      -- error fires when a path is forced (.drvPath), not at construction.
      runTest "derivation missing name" $
        assertEvalFail "drv-noname" "(derivation { system = \"x86_64-linux\"; builder = \"/bin/sh\"; }).drvPath",
      runTest "derivation missing system" $
        assertEvalFail "drv-nosys" "(derivation { name = \"hello\"; builder = \"/bin/sh\"; }).drvPath",
      runTest "derivation missing builder" $
        assertEvalFail "drv-nobuilder" "(derivation { name = \"hello\"; system = \"x86_64-linux\"; }).drvPath",
      runTest "derivation type error" $
        assertEvalFail "drv-tyerr" "derivation 42",
      runTest "derivation deterministic" $
        assertRight "drv-det" (evalNix "let d1 = derivation { name = \"a\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; }; d2 = derivation { name = \"a\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; }; in d1.drvPath == d2.drvPath") $ \val ->
          assertEqual "deterministic" (VBool True) val,
      -- The result has the shape of upstream's wrapper: every row below
      -- was read from nix-instantiate 2.33.2.
      runTest "derivation result carries exactly upstream's attributes" $
        assertEval
          "drv-attrnames"
          (withTwoOutputs "builtins.attrNames D == [ \"all\" \"builder\" \"dev\" \"drvAttrs\" \"drvPath\" \"name\" \"out\" \"outPath\" \"outputName\" \"outputs\" \"system\" \"type\" ]")
          (VBool True),
      runTest "derivation outputName is the first output" $
        assertEval "drv-outputname" (withTwoOutputs "D.outputName") (mkStr "out"),
      runTest "derivation default output is the first listed, not out" $
        assertEval
          "drv-first-output"
          "(derivation { name = \"x\"; system = \"s\"; builder = \"b\"; outputs = [ \"dev\" \"out\" ]; }).outputName"
          (mkStr "dev"),
      runTest "every output attribute is a complete derivation set" $
        assertEval "drv-output-set" (withTwoOutputs "builtins.attrNames D.dev == builtins.attrNames D") (VBool True),
      runTest "an output set names its own output" $
        assertEval "drv-output-name" (withTwoOutputs "D.dev.outputName") (mkStr "dev"),
      runTest "output sets refer back to each other" $
        assertEval "drv-output-cycle" (withTwoOutputs "D.dev.out.outPath == D.outPath && D.dev.dev.out.dev.outputName == \"dev\"") (VBool True),
      runTest "derivation all lists every output set in order" $
        assertEval "drv-all" (withTwoOutputs "map (o: o.outputName) D.all == [ \"out\" \"dev\" ]") (VBool True),
      runTest "derivation drvAttrs is the argument set" $
        assertEval "drv-drvattrs" (withTwoOutputs "builtins.attrNames D.drvAttrs == [ \"builder\" \"name\" \"outputs\" \"system\" ]") (VBool True),
      runTest "derivation leaks no internal attribute" $
        assertEval "drv-no-internal" (withTwoOutputs "D ? _derivation") (VBool False),
      -- derivationStrict is reached only through outPath and drvPath, so a
      -- derivation forced to WHNF forces none of its inputs.
      runTest "forcing a derivation to WHNF forces no input" $
        assertEval
          "drv-whnf"
          "builtins.seq (derivation { name = \"x\"; system = \"s\"; builder = throw \"forced\"; }) \"whnf-ok\""
          (mkStr "whnf-ok"),
      runTest "derivationStrict returns drvPath and one path per output" $
        assertEval
          "drvstrict-shape"
          "builtins.attrNames (derivationStrict { name = \"x\"; system = \"s\"; builder = \"b\"; outputs = [ \"out\" \"dev\" ]; }) == [ \"dev\" \"drvPath\" \"out\" ]"
          (VBool True),
      runTest "derivation is a lambda with upstream's formals" $
        assertEval "drv-functionargs" "builtins.functionArgs derivation == { outputs = true; }" (VBool True)
    ]

-- ---------------------------------------------------------------------------
-- Tests: StringContext (Phase 3, Batch 1)
-- ---------------------------------------------------------------------------

testStringContext :: IO [Bool]
testStringContext = do
  putStrLn "eval/string-context"
  let sp1 = StorePath "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "hello"
      sp2 = StorePath "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" "world"
  sequence
    [ runTest "StringContext Eq" $
        let ctx1 = StringContext (Set.singleton (SCPlain sp1))
            ctx2 = StringContext (Set.singleton (SCPlain sp1))
         in assertEqual "ctx-eq" ctx1 ctx2,
      runTest "mkStr constructor" $
        let v = mkStr "hello"
         in case v of
              VStr t ctx ->
                if t == "hello" && ctx == emptyContext
                  then Pass
                  else Fail "mkStr produced wrong value"
              _ -> Fail "mkStr did not produce VStr",
      runTest "mergeContexts mempty" $
        let ctx1 = StringContext (Set.singleton (SCPlain sp1))
            merged = ctx1 <> emptyContext
         in assertEqual "merge-mempty" ctx1 merged,
      runTest "mergeContexts union" $
        let ctx1 = StringContext (Set.singleton (SCPlain sp1))
            ctx2 = StringContext (Set.singleton (SCDrvOutput sp2 "out"))
            merged = ctx1 <> ctx2
            expected = StringContext (Set.fromList [SCPlain sp1, SCDrvOutput sp2 "out"])
         in assertEqual "merge-union" expected merged
    ]

-- ---------------------------------------------------------------------------
-- Tests: Context propagation (Phase 3, Batch 3)
-- ---------------------------------------------------------------------------

testContextPropagation :: IO [Bool]
testContextPropagation = do
  putStrLn "eval/context-propagation"
  sequence
    [ -- String equality ignores context
      runTest "string equality ignores context" $
        assertEval "str-eq-ctx" "\"hello\" == \"hello\"" (VBool True),
      -- String comparison ignores context
      runTest "string comparison ignores context" $
        assertEval "str-cmp-ctx" "\"a\" < \"b\"" (VBool True),
      -- Interpolation produces correct text
      runTest "interp text correct" $
        assertEval "interp-text" "let x = \"world\"; in \"hello ${x}\"" (mkStr "hello world"),
      -- String + merges (tested at value level)
      runTest "string + merges text" $
        assertEval "str-plus" "\"a\" + \"b\"" (mkStr "ab"),
      -- concatStringsSep merges text
      runTest "concatStringsSep result" $
        assertEval "css-text" "builtins.concatStringsSep \"-\" [\"a\" \"b\"]" (mkStr "a-b"),
      -- substring preserves text
      runTest "substring text" $
        assertEval "substr-text" "builtins.substring 1 2 \"hello\"" (mkStr "el"),
      -- unsafeDiscardStringContext strips context
      runTest "discardContext strips" $
        assertEval "discard-ctx" "builtins.unsafeDiscardStringContext \"hello\"" (mkStr "hello"),
      -- stringLength drops context (returns int)
      runTest "stringLength drops context" $
        assertEval "strlen-drop" "builtins.stringLength \"hello\"" (VInt 5),
      -- hashString drops context (returns string with no context)
      runTest "hashString result type" $
        assertRight "hash-type" (evalNix "builtins.typeOf (builtins.hashString \"sha256\" \"x\")") $ \val ->
          assertEqual "hash-typeof" (mkStr "string") val,
      -- replaceStrings text result
      runTest "replaceStrings text" $
        assertEval "replace-text" "builtins.replaceStrings [\"o\"] [\"0\"] \"foo\"" (mkStr "f00"),
      -- baseNameOf preserves text
      runTest "baseNameOf text" $
        assertEval "basename-text" "builtins.baseNameOf \"/foo/bar\"" (mkStr "bar"),
      -- dirOf preserves text
      runTest "dirOf text" $
        assertEval "dirof-text" "builtins.dirOf \"/foo/bar\"" (mkStr "/foo"),
      -- toString propagates
      runTest "toString on string" $
        assertEval "tostr-str" "builtins.toString \"hello\"" (mkStr "hello"),
      -- toString on int (no context)
      runTest "toString on int" $
        assertEval "tostr-int" "builtins.toString 42" (mkStr "42")
    ]

-- ---------------------------------------------------------------------------
-- Tests: Context helpers (Phase 3, Batch 2)
-- ---------------------------------------------------------------------------

testContextHelpers :: IO [Bool]
testContextHelpers = do
  putStrLn "eval/context-helpers"
  let sp1 = StorePath "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "hello"
      sp2 = StorePath "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" "world.drv"
      sp3 = StorePath "cccccccccccccccccccccccccccccccc" "source.tar.gz"
  sequence
    [ runTest "plainContext singleton" $
        let ctx = Context.plainContext sp1
         in assertEqual "plain" (StringContext (Set.singleton (SCPlain sp1))) ctx,
      runTest "drvOutputContext singleton" $
        let ctx = Context.drvOutputContext sp2 "out"
         in assertEqual "drvOut" (StringContext (Set.singleton (SCDrvOutput sp2 "out"))) ctx,
      runTest "allOutputsContext singleton" $
        let ctx = Context.allOutputsContext sp2
         in assertEqual "allOut" (StringContext (Set.singleton (SCAllOutputs sp2))) ctx,
      runTest "contextIsEmpty on mempty" $
        assertEqual "emptyCtx" True (Context.contextIsEmpty emptyContext),
      runTest "contextIsEmpty on non-empty" $
        assertEqual "nonEmptyCtx" False (Context.contextIsEmpty (Context.plainContext sp1)),
      runTest "extractInputSrcs" $
        let ctx = Context.plainContext sp1 <> Context.drvOutputContext sp2 "out"
         in assertEqual "srcs" [sp1] (Context.extractInputSrcs ctx),
      runTest "extractInputDrvs" $
        let ctx = Context.drvOutputContext sp2 "out" <> Context.drvOutputContext sp2 "dev" <> Context.plainContext sp3
            drvs = Context.extractInputDrvs ctx
         in case Map.lookup sp2 drvs of
              Just outs -> if length outs == 2 then Pass else Fail ("expected 2 outputs, got " <> T.pack (show (length outs)))
              Nothing -> Fail "sp2 not found in drvs",
      runTest "appendStrings merges" $
        let ctx1 = Context.plainContext sp1
            ctx2 = Context.drvOutputContext sp2 "out"
            (txt, ctx) = Context.appendStrings "hello" ctx1 "world" ctx2
         in if txt == "helloworld" && not (Context.contextIsEmpty ctx) then Pass else Fail "bad append",
      runTest "concatStrings empty" $
        let (txt, ctx) = Context.concatStrings []
         in if txt == "" && Context.contextIsEmpty ctx then Pass else Fail "bad empty concat",
      runTest "concatStrings merges all" $
        let ctx1 = Context.plainContext sp1
            ctx2 = Context.drvOutputContext sp2 "out"
            (txt, ctx) = Context.concatStrings [("a", ctx1), ("b", ctx2), ("c", mempty)]
         in if txt == "abc" && Set.size (unStringContext ctx) == 2 then Pass else Fail "bad concat"
    ]

-- ---------------------------------------------------------------------------
-- Tests: Derivation context + new builtins (Phase 3, Batch 4)
-- ---------------------------------------------------------------------------

testDrvContext :: IO [Bool]
testDrvContext = do
  putStrLn "eval/drv-context"
  sequence
    [ -- hasContext: plain string has no context
      runTest "hasContext on plain string" $
        assertEval "hasCtx-plain" "builtins.hasContext \"hello\"" (VBool False),
      -- hasContext: derivation outPath has context
      runTest "hasContext on drv outPath" $
        assertEval
          "hasCtx-drv"
          "builtins.hasContext (derivation { name = \"test\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; }).outPath"
          (VBool True),
      -- hasContext: after discardContext, no context
      runTest "hasContext after discard" $
        assertEval
          "hasCtx-discard"
          "builtins.hasContext (builtins.unsafeDiscardStringContext (derivation { name = \"test\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; }).outPath)"
          (VBool False),
      -- getContext: plain string returns empty attrset
      runTestM "getContext on plain string" $
        assertRightM "getCtx-plain" (evalNix "builtins.getContext \"hello\"") $ \val ->
          case val of
            VAttrs m -> (\empty -> if empty then Pass else Fail "expected empty attrset") <$> attrSetNull m
            _ -> pure (Fail ("expected VAttrs, got " <> T.pack (show val))),
      -- getContext: drv outPath has outputs entry
      runTestM "getContext on drv outPath"
        $ assertRightM
          "getCtx-drv"
          (evalNix "builtins.getContext (derivation { name = \"test\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; }).outPath")
        $ \val -> case val of
          VAttrs m -> do
            size <- attrSetSize m
            pure $
              if size == 1
                then Pass
                else Fail ("expected 1 entry, got " <> T.pack (show size))
          _ -> pure (Fail ("expected VAttrs, got " <> T.pack (show val))),
      -- getContext: drvPath has allOutputs
      runTest "getContext on drvPath has allOutputs"
        $ assertRight
          "getCtx-drvPath"
          (evalNix "let d = derivation { name = \"test\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; }; ctx = builtins.getContext d.drvPath; in builtins.length (builtins.attrNames ctx)")
        $ \val -> assertEqual "one-entry" (VInt 1) val,
      -- getContext keys are identity: canonical /nix/store spelling on
      -- every platform, never the platform file-path mapping.
      runTest "getContext key is canonical store path"
        $ assertRight
          "getCtx-canonical"
          (evalNix "let d = derivation { name = \"test\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; }; in builtins.elemAt (builtins.attrNames (builtins.getContext d.drvPath)) 0")
        $ \val -> case val of
          VStr key _
            | "/nix/store/" `BS.isPrefixOf` key && not ("\\" `BS.isInfixOf` key) -> Pass
            | otherwise -> Fail ("expected a canonical /nix/store key, got " <> bytesText key)
          _ -> Fail ("expected VStr, got " <> T.pack (show val)),
      -- appendContext: adds context to plain string
      runTest "appendContext adds context" $
        assertEval
          "appendCtx-add"
          "let ctx = builtins.listToAttrs [{ name = \"/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-foo\"; value = { path = true; }; }]; in builtins.hasContext (builtins.appendContext \"hello\" ctx)"
          (VBool True),
      -- appendContext: empty context is no-op
      runTest "appendContext empty is no-op" $
        assertEval "appendCtx-empty" "builtins.hasContext (builtins.appendContext \"hello\" {})" (VBool False),
      -- unsafeDiscardOutputDependency KEEPS a derivation-output (Built) ref
      -- unchanged (upstream), rather than dropping it as it once did.
      runTest "discardOutputDep keeps output-dependency context"
        $ assertRight
          "discardOutDep-keep"
          ( evalNix $
              T.concat
                [ "let d = derivation { name = \"test\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; }; ",
                  "stripped = builtins.unsafeDiscardOutputDependency d.outPath; ",
                  "in builtins.hasContext stripped"
                ]
          )
        $ \val -> assertEqual "keep-ctx" (VBool True) val,
      -- ctx3: an all-outputs (DrvDeep) reference on d.drvPath is DOWNGRADED to
      -- a plain path reference, not dropped.
      runTest "discardOutputDep downgrades drvPath to a plain ref" $
        assertEval
          "discardOutDep-downgrade"
          "let d = derivation { name = \"test\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; }; ctx = builtins.getContext (builtins.unsafeDiscardOutputDependency d.drvPath); key = builtins.elemAt (builtins.attrNames ctx) 0; in ctx.${key} == { path = true; }"
          (VBool True),
      -- ctx4: getContext renders each path's output names ascending.
      runTest "getContext output names are ascending" $
        assertEval
          "getctx-ascending"
          "let d = derivation { name = \"test\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; outputs = [ \"out\" \"dev\" \"lib\" ]; }; s = \"${d.out}${d.lib}${d.dev}\"; ctx = builtins.getContext s; key = builtins.elemAt (builtins.attrNames ctx) 0; in ctx.${key}.outputs == [ \"dev\" \"lib\" \"out\" ]"
          (VBool True),
      -- ctx5: addDrvOutputDependencies upgrades a plain .drv reference back to
      -- an all-outputs reference (the inverse of the ctx3 downgrade).
      runTest "addDrvOutputDependencies upgrades a plain drv ref" $
        assertEval
          "adddrvout-upgrade"
          "let d = derivation { name = \"test\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; }; plain = builtins.unsafeDiscardOutputDependency d.drvPath; ctx = builtins.getContext (builtins.addDrvOutputDependencies plain); key = builtins.elemAt (builtins.attrNames ctx) 0; in ctx.${key} == { allOutputs = true; }"
          (VBool True),
      runTest "addDrvOutputDependencies rejects a derivation output" $
        assertEvalFail
          "adddrvout-built"
          "let d = derivation { name = \"test\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; }; in builtins.addDrvOutputDependencies d.outPath",
      runTest "addDrvOutputDependencies rejects a multi-element context" $
        assertEvalFail
          "adddrvout-multi"
          "let d = derivation { name = \"test\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; }; in builtins.addDrvOutputDependencies \"${d.drvPath}${d.outPath}\"",
      -- drv1: a derivation embedding another's drvPath (an all-outputs ref)
      -- needs the referenced .drv's output names, which only the IO evaluator
      -- supplies; pure eval fails loudly rather than dropping the reference.
      runTest "deep drvPath reference fails loudly in pure eval" $
        assertEvalFail
          "drv1-pure-miss"
          "let dep = derivation { name = \"dep\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; }; main = derivation { name = \"main\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; ref = dep.drvPath; }; in main.drvPath",
      -- derivation outPath is a string (not path) with context
      runTest "drv outPath is VStr"
        $ assertRight
          "drv-outPath-type"
          (evalNix "builtins.typeOf (derivation { name = \"test\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; }).outPath")
        $ \val -> assertEqual "string-type" (mkStr "string") val,
      -- derivation drvPath is a string (not path) with context
      runTest "drv drvPath is VStr"
        $ assertRight
          "drv-drvPath-type"
          (evalNix "builtins.typeOf (derivation { name = \"test\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; }).drvPath")
        $ \val -> assertEqual "string-type" (mkStr "string") val,
      -- hasContext error on non-string
      runTest "hasContext type error" $
        assertEvalFail "hasCtx-err" "builtins.hasContext 42",
      -- getContext error on non-string
      runTest "getContext type error" $
        assertEvalFail "getCtx-err" "builtins.getContext 42",
      -- appendContext error on non-string first arg
      runTest "appendContext type error" $
        assertEvalFail "appendCtx-err" "builtins.appendContext 42 {}",
      -- deterministic: same derivation produces same paths
      runTest "derivation with context deterministic"
        $ assertRight
          "drv-det-ctx"
          (evalNix "let d1 = derivation { name = \"a\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; }; d2 = derivation { name = \"a\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; }; in d1.outPath == d2.outPath")
        $ \val -> assertEqual "deterministic" (VBool True) val
    ]

-- ---------------------------------------------------------------------------
-- Tests: store operations
-- ---------------------------------------------------------------------------

-- | Upstream value-conformance tests: each pins exact output bytes (hash
-- decode, number formatting) that must match what upstream Nix computes for
-- the same input, because these values can reach names, derivations, and
-- hashes.
testUpstreamConformance :: IO [Bool]
testUpstreamConformance = do
  putStrLn "eval/conformance"
  let xmlDocument body = mkStr ("<?xml version='1.0' encoding='utf-8'?>\n<expr>\n" <> body <> "</expr>\n")
      toBase16 hash = "builtins.convertHash { hash = \"" <> hash <> "\"; toHashFormat = \"base16\"; }"
      emptySha256 = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
      emptySri = "sha256-47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU="
      unknownSha3 = "unknown hash algorithm 'sha3', expect 'md5', 'sha1', 'sha256', or 'sha512'"
      badBase16 = T.replicate 63 "0" <> "g"
      badNix32 = T.replicate 51 "0" <> "e"
      -- 52 nix32 digits hold 260 bits, four more than a SHA-256 digest,
      -- and those four are the top of the first digit, one of which '2'
      -- sets.
      overflowingNix32 = "2" <> T.replicate 51 "0"
  sequence
    [ -- Hash decode is length-keyed per algorithm, never first-format-wins:
      -- a 52-char all-hex-digit string is a nix32 sha256 hash (64 hex chars
      -- after conversion), not a 26-byte hex string echoed back.
      runTest "hash decode keys on nix32 length" $
        assertEval
          "hash-nix32-length"
          "builtins.stringLength (builtins.convertHash { hash = \"0aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"; hashAlgo = \"sha256\"; toHashFormat = \"base16\"; })"
          (VInt 64),
      runTest "hash hex to nix32 round-trips" $
        assertEval
          "hash-roundtrip"
          "builtins.convertHash { hash = builtins.convertHash { hash = \"sha256:e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855\"; toHashFormat = \"nix32\"; }; hashAlgo = \"sha256\"; toHashFormat = \"base16\"; }"
          (mkStr "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"),
      runTest "hash base64 spelling decodes by length" $
        assertEval
          "hash-base64"
          "builtins.convertHash { hash = \"sha256:47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU=\"; toHashFormat = \"base16\"; }"
          (mkStr "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"),
      runTest "truncated hash is rejected" $
        assertEvalFail
          "hash-truncated"
          "builtins.convertHash { hash = \"e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b85\"; hashAlgo = \"sha256\"; toHashFormat = \"base16\"; }",
      -- An algorithm name upstream does not know is refused with
      -- parseHashAlgo's message (hash.cc at 2.24.9), and convertHash parses
      -- hashAlgo before the hash (prim_convertHash in primops.cc).
      runTest "unknown hash algorithm is rejected" $
        assertEvalError
          "hash-bad-algo"
          "builtins.convertHash { hash = \"00\"; hashAlgo = \"sha3\"; toHashFormat = \"base16\"; }"
          unknownSha3,
      runTest "hashString refuses an unknown algorithm with upstream's message" $
        assertEvalError "hashString-bad-algo" "builtins.hashString \"sha3\" \"\"" unknownSha3,
      -- A base16 or nix32 digest that does not decode is refused as the
      -- Hash constructor refuses it (hash.cc at 2.24.9).
      runTest "a base16 digest with a character outside its alphabet is refused" $
        assertEvalError "base16-invalid" (toBase16 ("sha256:" <> badBase16)) ("invalid base-16 hash '" <> badBase16 <> "'"),
      runTest "a nix32 digest with a character outside its alphabet is refused" $
        assertEvalError "base32-invalid" (toBase16 ("sha256:" <> badNix32)) ("invalid base-32 hash '" <> badNix32 <> "'"),
      runTest "a nix32 digest with bits past the end of the digest is refused" $
        assertEvalError "base32-overflow" (toBase16 ("sha256:" <> overflowingNix32)) ("invalid base-32 hash '" <> overflowingNix32 <> "'"),
      -- convertHash hands a given hashAlgo to Hash::parseAny
      -- (prim_convertHash in primops.cc), which refuses a tag that names
      -- another algorithm and needs one or the other (hash.cc).
      runTest "convertHash refuses a tag that names another algorithm than hashAlgo" $
        assertEvalError
          "convertHash-tag-mismatch"
          ("builtins.convertHash { hash = \"" <> emptySri <> "\"; hashAlgo = \"sha512\"; toHashFormat = \"base16\"; }")
          ("hash '" <> emptySri <> "' should have type 'sha512'"),
      runTest "convertHash refuses an unknown hashAlgo when the hash carries a tag" $
        assertEvalError
          "convertHash-tag-bad-algo"
          ("builtins.convertHash { hash = \"" <> emptySri <> "\"; hashAlgo = \"sha3\"; toHashFormat = \"base16\"; }")
          unknownSha3,
      runTest "convertHash accepts a tag that agrees with hashAlgo" $
        assertEval
          "convertHash-tag-agrees"
          ("builtins.convertHash { hash = \"" <> emptySri <> "\"; hashAlgo = \"sha256\"; toHashFormat = \"base16\"; }")
          (mkStr emptySha256),
      runTest "convertHash refuses an untagged hash without hashAlgo" $
        assertEvalError
          "convertHash-no-type"
          ("builtins.convertHash { hash = \"" <> emptySha256 <> "\"; toHashFormat = \"nix32\"; }")
          ("hash '" <> emptySha256 <> "' does not include a type, nor is the type otherwise known from context"),
      -- SRI digests get the same decoded-length check as every other
      -- spelling (upstream checks SRI too), at all three decode sites:
      -- convertHash, fixed-output outputHash, and the fetch/path pins.
      runTest "valid SRI digest converts" $
        assertEval
          "hash-sri-valid"
          "builtins.convertHash { hash = \"sha256-47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU=\"; toHashFormat = \"base16\"; }"
          (mkStr "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"),
      runTest "convertHash rejects a truncated SRI digest" $
        case evalNix "builtins.convertHash { hash = \"sha256-YWJj\"; toHashFormat = \"base16\"; }" of
          Left err
            | "invalid SRI hash 'YWJj'" `T.isInfixOf` err -> Pass
            | otherwise -> Fail ("expected an SRI length error, got: " <> err)
          Right val -> Fail ("expected failure, got: " <> T.pack (show val)),
      runTest "fixed-output outputHash rejects a truncated SRI digest" $
        case evalNix "(derivation { name = \"t\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; outputHash = \"sha256-YWJj\"; }).drvPath" of
          Left err
            | "invalid SRI hash 'YWJj'" `T.isInfixOf` err -> Pass
            | otherwise -> Fail ("expected an SRI length error, got: " <> err)
          Right val -> Fail ("expected failure, got: " <> T.pack (show val)),
      -- An SRI or prefixed outputHash carries its own algorithm tag, and
      -- an outputHashAlgo upstream knows must agree with it (hash.cc
      -- parseAny with an expected type).
      runTest "fixed-output SRI algorithm must match outputHashAlgo" $
        case evalNix "(derivation { name = \"t\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; outputHash = \"sha1-2jmj7l5rSw0yVb/vlWAYkK/YBwk=\"; outputHashAlgo = \"sha256\"; }).drvPath" of
          Left err
            | "should have type 'sha256'" `T.isInfixOf` err -> Pass
            | otherwise -> Fail ("expected a hash-type error, got: " <> err)
          Right val -> Fail ("expected failure, got: " <> T.pack (show val)),
      runTest "fixed-output prefixed algorithm must match outputHashAlgo" $
        case evalNix "(derivation { name = \"t\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; outputHash = \"md5:d41d8cd98f00b204e9800998ecf8427e\"; outputHashAlgo = \"sha256\"; }).drvPath" of
          Left err
            | "should have type 'sha256'" `T.isInfixOf` err -> Pass
            | otherwise -> Fail ("expected a hash-type error, got: " <> err)
          Right val -> Fail ("expected failure, got: " <> T.pack (show val)),
      runTest "fixed-output SRI agreeing with outputHashAlgo accepted" $
        case evalNix "(derivation { name = \"t\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; outputHash = \"sha256-47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU=\"; outputHashAlgo = \"sha256\"; }).drvPath" of
          Right _ -> Pass
          Left err -> Fail ("expected success, got: " <> err),
      -- outputHashAlgo is read in derivationStrictInternal (primops.cc)
      -- with parseHashAlgoOpt, which gives no algorithm for a name it does
      -- not know, so the hash's own tag decides, and an untagged hash has
      -- no type.
      runTest "fixed-output unknown outputHashAlgo gives way to the SRI tag" $
        assertEval
          "fixed-output-unknown-algo"
          ("let out = attrs: (derivation ({ name = \"t\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; outputHash = \"" <> emptySri <> "\"; } // attrs)).outPath; in out { outputHashAlgo = \"sha3\"; } == out { }")
          (VBool True),
      runTest "fixed-output unknown outputHashAlgo leaves an untagged hash with no type" $
        assertEvalError
          "fixed-output-unknown-algo-untagged"
          ("(derivation { name = \"t\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; outputHash = \"" <> emptySha256 <> "\"; outputHashAlgo = \"sha3\"; }).drvPath")
          ("hash '" <> emptySha256 <> "' does not include a type, nor is the type otherwise known from context"),
      runTest "builtins.path pin rejects a truncated SRI digest" $
        case evalNix "builtins.path { path = ./x; sha256 = \"sha256-YWJj\"; }" of
          Left err
            | "invalid SRI hash 'YWJj'" `T.isInfixOf` err -> Pass
            | otherwise -> Fail ("expected an SRI length error, got: " <> err)
          Right val -> Fail ("expected failure, got: " <> T.pack (show val)),
      runTest "builtins.path pin rejects a non-sha256 SRI digest" $
        case evalNix ("builtins.path { path = ./x; sha256 = \"sha512-" <> T.replicate 86 "A" <> "==\"; }") of
          Left err
            | "should have type 'sha256'" `T.isInfixOf` err -> Pass
            | otherwise -> Fail ("expected a hash-type error, got: " <> err)
          Right val -> Fail ("expected failure, got: " <> T.pack (show val)),
      -- Base64 is read as upstream's base64Decode reads it: up to the
      -- first '=', newlines skipped, the bits after the last whole byte
      -- dropped.  Five nixpkgs hashes rely on it: nmgui's, journalist's
      -- and quintom-cursor-theme's have text after the padding, guake's a
      -- space, and cosmic-workspaces-epoch's leftover bits that are not
      -- zero.
      runTest "an SRI hash with text after its padding decodes (nmgui)" $
        assertEval "nmgui" (toBase16 "sha256-HS/n40Ng8S5N14DtEH/upwlxdzwCoOEJA40EMdCcLw4=io") (mkStr "1d2fe7e34360f12e4dd780ed107feea70971773c02a0e109038d0431d09c2f0e"),
      runTest "an SRI hash with a character after its padding decodes to 32 bytes (journalist)" $
        assertEval "journalist" (toBase16 "sha256-2EJ96dhhU7FZxMkHOmQo79WCHu8U1AGgFf47FIuQdek=s") (mkStr "d8427de9d86153b159c4c9073a6428efd5821eef14d401a015fe3b148b9075e9"),
      runTest "an SRI hash with an alphabet character after its padding decodes to 32 bytes (quintom-cursor-theme)" $
        assertEval "quintom-cursor-theme" (toBase16 "sha256-Sec2DSnWYal6wzYzP9W+DDuTKHsFHWdRYyMzliMU5bU=A") (mkStr "49e7360d29d661a97ac336333fd5be0c3b93287b051d6751632333962314e5b5"),
      runTest "an SRI hash with a space after its padding decodes (guake)" $
        assertEval "guake" (toBase16 "sha256-RjGRFJDTQX2meAaw3UZi/3OxAtIHbRZVpXTbcJk/scY= ") (mkStr "4631911490d3417da67806b0dd4662ff73b102d2076d1655a574db70993fb1c6"),
      runTest "an SRI hash whose leftover bits are not zero decodes (cosmic-workspaces-epoch)" $
        assertEval "cosmic-workspaces-epoch" (toBase16 "sha256-z3xQ6Vgqkm8hYLo2550NbFRkTMRQ0F9zn85iobnykH5=") (mkStr "cf7c50e9582a926f2160ba36e79d0d6c54644cc450d05f739fce62a1b9f2907e"),
      runTest "a fixed-output hash with text after its padding has the path of its plain spelling" $
        assertEval
          "fixed-output-padding"
          "let out = hash: (derivation { name = \"t\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; outputHash = hash; }).outPath; in out \"sha256-HS/n40Ng8S5N14DtEH/upwlxdzwCoOEJA40EMdCcLw4=io\" == out \"sha256-HS/n40Ng8S5N14DtEH/upwlxdzwCoOEJA40EMdCcLw4=\""
          (VBool True),
      runTest "a newline in a base64 hash is skipped" $
        assertEval "base64-newline" (toBase16 "sha256-47DEQpj8HBSa+/TImW+5JCeu\\nQeRkm5NMpJWZG3hSuFU=") (mkStr "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"),
      -- A refused character and a wrong decoded length are one error
      -- upstream (the Hash constructor in hash.cc at 2.24.9, parseLowLevel
      -- there at 2.36.0), which drops the decoder's own exception.
      runTest "a carriage return in an SRI hash is refused" $
        assertEvalError "sri-carriage-return" (toBase16 "sha256-47DEQpj8HBSa+/TImW+5JCeu\\rQeRkm5NMpJWZG3hSuFU=") "invalid SRI hash '47DEQpj8HBSa+/TImW+5JCeu\rQeRkm5NMpJWZG3hSuFU='",
      runTest "a character outside the base64 alphabet in an SRI hash is refused" $
        assertEvalError "sri-url-safe" (toBase16 "sha256-47DEQpj8HBSa+_TImW+5JCeuQeRkm5NMpJWZG3hSuFU=") "invalid SRI hash '47DEQpj8HBSa+_TImW+5JCeuQeRkm5NMpJWZG3hSuFU='",
      runTest "a character outside the base64 alphabet in a prefixed hash is refused" $
        assertEvalError "base64-url-safe" (toBase16 "sha256:47DEQpj8HBSa+_TImW+5JCeuQeRkm5NMpJWZG3hSuFU=") "invalid base-64 hash '47DEQpj8HBSa+_TImW+5JCeuQeRkm5NMpJWZG3hSuFU='",
      -- The algorithm tag ends at the first ':', else at the first '-'
      -- (getParsedTypeAndSRI in hash.cc at 2.24.9, parseAnyHelper at
      -- 2.36.0), so a separator after the padding, which the decoder
      -- never reads, still decides the tag.
      runTest "a '-' after the padding of a prefixed hash is ignored" $
        assertEval "prefixed-dash" (toBase16 "md5:AAAAAAAAAAAAAAAAAAAAAA=-") (mkStr "00000000000000000000000000000000"),
      runTest "a ':' after the padding of an SRI hash makes its tag an unknown algorithm" $
        assertEvalError "sri-colon" (toBase16 "sha256-47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU=:x") "unknown hash algorithm 'sha256-47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU=', expect 'md5', 'sha1', 'sha256', or 'sha512'",
      runTest "a '-' after the padding of an untagged hash makes its start an unknown algorithm" $
        assertEvalError
          "untagged-dash"
          "builtins.convertHash { hash = \"AAAAAAAAAAAAAAAAAAAAAA=-\"; hashAlgo = \"md5\"; toHashFormat = \"base16\"; }"
          "unknown hash algorithm 'AAAAAAAAAAAAAAAAAAAAAA=', expect 'md5', 'sha1', 'sha256', or 'sha512'",
      -- Upstream keys a hash's spelling on its length in bytes, so a
      -- character UTF-8 spells in two bytes after the padding of a 24
      -- character md5 base64 hash makes it 25 long.
      runTest "a hash's spelling is keyed on its length in bytes" $
        assertEvalError "base64-length-bytes" (toBase16 "md5:AAAAAAAAAAAAAAAAAAAAAA=\233") "hash 'AAAAAAAAAAAAAAAAAAAAAA=\233' has wrong length for hash algorithm 'md5'",
      runTestM "base64 decodes as upstream's base64Decode loop over generated strings" $
        quickCheckPasses $
          QC.checkCoverage $
            QC.forAll genBase64Text $ \text ->
              let decoded = Hash.base64ToBytes text
               in QC.cover 20 (isJust decoded) "decoded"
                    $ QC.cover 20 (isNothing decoded) "refused"
                    $ decoded QC.=== (BS.pack <$> base64DecodeModel (BS.unpack (TE.encodeUtf8 text))),
      runTestM "base64 decodes what it encodes" $
        quickCheckPasses $
          QC.forAll (BS.pack <$> QC.listOf QC.arbitrary) $ \bytes ->
            Hash.base64ToBytes (B64.encode bytes) QC.=== Just bytes,
      -- toJSON floats: nlohmann's layout - shortest round-trip digits, .0
      -- kept on integral values, scientific outside point positions (-4, 15].
      runTest "toJSON float shortest digits" $
        assertEval "json-float-third" "builtins.toJSON (1.0 / 3.0)" (mkStr "0.3333333333333333"),
      runTest "toJSON integral float keeps .0" $
        assertEval "json-float-integral" "builtins.toJSON 100.0" (mkStr "100.0"),
      runTest "toJSON float plain decimal" $
        assertEval "json-float-tenth" "builtins.toJSON 0.1" (mkStr "0.1"),
      runTest "toJSON float zero" $
        assertEval "json-float-zero" "builtins.toJSON 0.0" (mkStr "0.0"),
      runTest "toJSON float widest plain integral" $
        assertEval "json-float-e14" "builtins.toJSON 1.0e14" (mkStr "100000000000000.0"),
      runTest "toJSON float first scientific integral" $
        assertEval "json-float-e15" "builtins.toJSON 1.0e15" (mkStr "1e+15"),
      runTest "toJSON float large exponent" $
        assertEval "json-float-e21" "builtins.toJSON 1.0e21" (mkStr "1e+21"),
      runTest "toJSON float negative exponent" $
        assertEval "json-float-e-5" "builtins.toJSON 1.0e-5" (mkStr "1e-05"),
      runTest "toJSON float smallest plain" $
        assertEval "json-float-e-4" "builtins.toJSON 1.0e-4" (mkStr "0.0001"),
      runTest "toJSON non-finite float is null" $
        assertEval "json-float-inf" "builtins.toJSON (1.0e308 * 10.0)" (mkStr "null"),
      -- fromJSON integers: int64 range stays int, wider falls back to float.
      runTest "fromJSON promotes past-64-bit integer to float" $
        assertEval "json-bigint-type" "builtins.typeOf (builtins.fromJSON \"999999999999999999999\")" (mkStr "float"),
      runTest "fromJSON past-64-bit integer value" $
        assertEval "json-bigint-val" "builtins.fromJSON \"999999999999999999999\" == 999999999999999999999.0" (VBool True),
      runTest "fromJSON int64 max stays an int" $
        assertEval "json-int64-max" "builtins.fromJSON \"9223372036854775807\"" (VInt 9223372036854775807),
      -- toXML floats: C++ default ostream formatting, 6 significant digits.
      runTest "toXML float 6 significant digits" $
        assertEval
          "xml-float-third"
          "builtins.toXML (1.0 / 3.0)"
          (xmlDocument "  <float value=\"0.333333\" />\n"),
      runTest "toXML float plain decimal" $
        assertEval
          "xml-float-centi"
          "builtins.toXML 0.01"
          (xmlDocument "  <float value=\"0.01\" />\n"),
      runTest "toXML integral float drops the point" $
        assertEval
          "xml-float-integral"
          "builtins.toXML 100.0"
          (xmlDocument "  <float value=\"100\" />\n"),
      runTest "toXML float large exponent" $
        assertEval
          "xml-float-e21"
          "builtins.toXML 1.0e21"
          (xmlDocument "  <float value=\"1e+21\" />\n"),
      runTest "toXML float small exponent" $
        assertEval
          "xml-float-e-5"
          "builtins.toXML 2.5e-5"
          (xmlDocument "  <float value=\"2.5e-05\" />\n"),
      -- The document is upstream's XMLWriter output (value-to-xml.cc and
      -- xml-writer.cc at 2.24.9): every element two spaces deeper than the
      -- one around it, a newline in a value written as a character
      -- reference.  Each expected document is nix-instantiate 2.24.9's and
      -- 2.33.2's.
      runTest "toXML indents each element under the one around it" $
        assertEval
          "xml-nesting"
          "builtins.toXML { s = \"a<b>&\\\"q'\\n\\tz\"; l = [ 1 { } ]; }"
          ( xmlDocument
              ( "  <attrs>\n    <attr name=\"l\">\n      <list>\n        <int value=\"1\" />\n"
                  <> "        <attrs>\n        </attrs>\n      </list>\n    </attr>\n"
                  <> "    <attr name=\"s\">\n      <string value=\"a&lt;b&gt;&amp;&quot;q'&#xA;\tz\" />\n"
                  <> "    </attr>\n  </attrs>\n"
              )
          ),
      runTest "toXML writes a lambda's argument pattern and leaves a primop unevaluated" $
        assertEval
          "xml-functions"
          "builtins.toXML [ (x: x) (args@{ z, ... }: 1) ({ b, a }: a) builtins.map (builtins.map (x: x)) ]"
          ( xmlDocument
              ( "  <list>\n    <function>\n      <varpat name=\"x\" />\n    </function>\n"
                  <> "    <function>\n      <attrspat ellipsis=\"1\" name=\"args\">\n        <attr name=\"z\" />\n"
                  <> "      </attrspat>\n    </function>\n"
                  <> "    <function>\n      <attrspat>\n        <attr name=\"a\" />\n        <attr name=\"b\" />\n"
                  <> "      </attrspat>\n    </function>\n"
                  <> "    <unevaluated />\n    <unevaluated />\n  </list>\n"
              )
          ),
      runTest "toXML writes a derivation in full once and repeated after" $
        assertEval
          "xml-derivations"
          "builtins.toXML [ { type = \"derivation\"; drvPath = \"/p\"; outPath = \"/o\"; a = 1; } { type = \"derivation\"; drvPath = \"/p\"; } { type = \"derivation\"; outPath = 2; } ]"
          ( xmlDocument
              ( "  <list>\n    <derivation drvPath=\"/p\" outPath=\"/o\">\n"
                  <> "      <attr name=\"a\">\n        <int value=\"1\" />\n      </attr>\n"
                  <> "      <attr name=\"drvPath\">\n        <string value=\"/p\" />\n      </attr>\n"
                  <> "      <attr name=\"outPath\">\n        <string value=\"/o\" />\n      </attr>\n"
                  <> "      <attr name=\"type\">\n        <string value=\"derivation\" />\n      </attr>\n"
                  <> "    </derivation>\n"
                  <> "    <derivation drvPath=\"/p\">\n      <repeated />\n    </derivation>\n"
                  <> "    <derivation>\n      <repeated />\n    </derivation>\n  </list>\n"
              )
          ),
      runTest "toXML keeps the contexts of the strings it writes" $
        assertEval
          "xml-context"
          "let d = derivation { name = \"x\"; system = \"s\"; builder = \"b\"; }; in builtins.getContext (builtins.toXML { s = \"${d}\"; }) == builtins.getContext \"${d}\""
          (VBool True),
      -- fromTOML integers: 64-bit signed range enforced, no silent wrap.
      runTest "fromTOML rejects past-64-bit integer" $
        assertEvalFail "toml-int-overflow" "builtins.fromTOML \"v = 99999999999999999999\"",
      runTest "fromTOML rejects past-64-bit hex integer" $
        assertEvalFail "toml-hex-overflow" "builtins.fromTOML \"v = 0xffffffffffffffff\"",
      runTest "fromTOML int64 max parses" $
        assertEval "toml-int64-max" "(builtins.fromTOML \"v = 9223372036854775807\").v" (VInt 9223372036854775807),
      runTest "fromTOML int64 min parses" $
        assertEval "toml-int64-min" "(builtins.fromTOML \"v = -9223372036854775808\").v == (0 - 9223372036854775807 - 1)" (VBool True),
      -- drv3: pure eval cannot read the store, so hashing a dependent
      -- derivation fails loudly rather than emitting a guessed input hash.
      runTest "dependent derivation drvPath fails loudly in pure eval" $
        assertEvalFail
          "drv-modulo-pure-miss"
          "let dep = derivation { name = \"dep\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; }; main = derivation { name = \"main\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; src = dep.outPath; }; in main.drvPath",
      -- Path values canonicalize lexically (CanonPath): no dot segment or
      -- doubled separator survives into a path value's text, so different
      -- spellings of the same path compare equal.
      runTest "path literal canonicalizes dot segments" $
        assertEval "path-canon-dot" "builtins.toString ./. == builtins.toString ./x/.." (VBool True),
      runTest "path literal collapses inner dot" $
        assertEval "path-canon-inner" "builtins.toString ./a/./b == builtins.toString ./a/b" (VBool True),
      runTest "path plus string canonicalizes" $
        assertEval "path-canon-plus" "./a + \"/../b\" == ./b" (VBool True),
      runTest "toPath canonicalizes" $
        assertEval "path-canon-topath" "builtins.toString (builtins.toPath \"/a/../b\")" (mkStr "/b"),
      runTest "toPath collapses inner dot" $
        assertEval "path-canon-topath-dot" "builtins.toString (builtins.toPath \"/a/./b\")" (mkStr "/a/b"),
      runTest "toPath preserves forward-slash root" $
        assertEval "path-canon-topath-root" "builtins.toString (builtins.toPath \"//nix//store\")" (mkStr "/nix/store")
    ]
  where
    quickCheckPasses :: (QC.Testable prop) => prop -> IO TestResult
    quickCheckPasses property = do
      result <- QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} property
      pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))

-- | Strings for the base64 decoder: mostly alphabet characters, with the
-- two it treats specially (@=@ and newline) and some it refuses (carriage
-- return, space, base64url's @-@ and @_@, @:@, NUL and one UTF-8 spells in
-- two bytes).
genBase64Text :: QC.Gen Text
genBase64Text =
  T.pack
    <$> QC.resize
      48
      ( QC.listOf
          ( QC.frequency
              [ (12, QC.elements (BS8.unpack base64Alphabet)),
                (2, pure '='),
                (2, pure '\n'),
                (1, QC.elements "\r -_:\0\233")
              ]
          )
      )

-- | The standard base64 alphabet, in digit order (util.cc @base64Chars@).
base64Alphabet :: BS.ByteString
base64Alphabet = BS8.pack (['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ "+/")

-- | Upstream's @base64Decode@ loop (util.cc at 2.24.9, @base64::decode@
-- in base-n.cc at 2.36.0), one byte of the string at a time, its
-- accumulator an unsigned 32-bit integer as there.  'Nothing' where
-- upstream throws.
base64DecodeModel :: [Word8] -> Maybe [Word8]
base64DecodeModel = go 0 0 []
  where
    go :: Word32 -> Int -> [Word8] -> [Word8] -> Maybe [Word8]
    go _ _ out [] = Just (reverse out)
    go acc bits out (c : rest)
      | c == byteOf '=' = Just (reverse out)
      | c == byteOf '\n' = go acc bits out rest
      | otherwise = do
          digit <- BS.elemIndex c base64Alphabet
          let shifted = shiftL acc 6 .|. fromIntegral digit
              filled = bits + 6
          if filled >= 8
            then go shifted (filled - 8) (fromIntegral (shiftR shifted (filled - 8)) : out) rest
            else go shifted filled out rest
    byteOf = fromIntegral . ord

-- | Eval-fidelity regression tests: each pins a semantic where the evaluator
-- must match upstream Nix.  All are parity-safe - none affects a derivation
-- or store-path hash.
testEvalFidelity :: IO [Bool]
testEvalFidelity = do
  putStrLn "eval/fidelity"
  sequence
    [ -- builtins.match: a non-participating capture group is null, not ""
      runTest "match null capture group" $
        assertEval "match-null" "builtins.isNull (builtins.elemAt (builtins.match \"(a)(b)?\" \"a\") 1)" (VBool True),
      runTest "match participating group value" $
        assertEval "match-val" "builtins.elemAt (builtins.match \"(a)(b)\" \"ab\") 1" (mkStr "b"),
      -- builtins.split: a non-participating group is null too
      runTest "split null capture group" $
        assertEval "split-null" "builtins.isNull (builtins.elemAt (builtins.elemAt (builtins.split \"(a)|(b)\" \"a\") 1) 1)" (VBool True),
      -- builtins.match is whole-string like regex_match: top-level
      -- alternation must not decay into a prefix/suffix match.
      runTest "match alternation is whole-string" $
        assertEval "match-alt" "builtins.match \"a|b\" \"ab\"" VNull,
      runTest "match alternation positive" $
        assertEval "match-alt-pos" "builtins.match \"a|b\" \"a\" == [ ]" (VBool True),
      -- No multiline mode: ^/$ anchor only at the string boundaries and
      -- '.' matches a newline, as POSIX ERE whole-string matching does.
      runTest "match does not anchor at inner newlines" $
        assertEval "match-nl" "builtins.match \"foo\" \"bar\\nfoo\"" VNull,
      runTest "match dot spans newline" $
        assertEval "match-dot-nl" "builtins.match \"(.*)\" \"a\\nb\" == [ \"a\\nb\" ]" (VBool True),
      runTest "split dot spans newline" $
        assertEval "split-dot-nl" "builtins.split \"a.b\" \"xa\\nby\" == [ \"x\" [ ] \"y\" ]" (VBool True),
      -- Derivations compare by outPath alone (C++ eqValues special case) -
      -- even when the rest of the attrs (or the key sets) differ.
      runTest "derivation eq by outPath" $
        assertEval
          "drv-eq"
          "{ type = \"derivation\"; outPath = \"/nix/store/a\"; f = (x: x); } == { type = \"derivation\"; outPath = \"/nix/store/a\"; f = (y: y); extra = 1; }"
          (VBool True),
      runTest "derivation neq by outPath" $
        assertEval
          "drv-neq"
          "{ type = \"derivation\"; outPath = \"/a\"; } == { type = \"derivation\"; outPath = \"/b\"; }"
          (VBool False),
      runTest "derivation-tagged sets without outPath deep-compare" $
        assertEval
          "drv-no-outpath"
          "{ type = \"derivation\"; n = 1; } == { type = \"derivation\"; n = 1; }"
          (VBool True),
      -- A throwing derivation attr fails evaluation - never a silent drop
      -- (a dropped attr once produced an empty no-input .drv that "built").
      runTest "derivation attr throw propagates" $
        assertEvalFail "drv-throw" "(derivation { name = \"x\"; system = \"s\"; builder = \"/b\"; src = throw \"boom\"; }).drvPath",
      -- __ignoreNulls drops null attrs; without it null coerces to "".
      runTest "derivation ignoreNulls drops null" $
        assertEval "drv-ignore-nulls" "builtins.isString (derivation { name = \"x\"; system = \"s\"; builder = \"/b\"; src = null; __ignoreNulls = true; }).drvPath" (VBool True),
      runTest "derivation null coerces without ignoreNulls" $
        assertEval "drv-null-empty" "builtins.isString (derivation { name = \"x\"; system = \"s\"; builder = \"/b\"; foo = null; }).drvPath" (VBool True),
      -- A literal string key is static, as in upstream's parser: legal in
      -- let, referenceable as a rec sibling.
      runTest "let with literal string key" $
        assertEval "let-string-key" "let \"x\" = 1; in x" (VInt 1),
      runTest "rec string key is a referenceable sibling" $
        assertEval "rec-string-key" "(rec { \"a\" = 1; b = a; }).b" (VInt 1),
      -- A dynamic TOP-LEVEL key in let is a parse error upstream; nested
      -- dynamic keys live in a nested attrset and stay legal.
      runTest "dynamic key in let rejected" $
        assertParseFail "let-dyn-key" "let k = \"y\"; in let ${k} = 1; in y",
      runTest "interpolated string key in let rejected" $
        assertParseFail "let-interp-key" "let k = \"y\"; in let \"${k}\" = 1; in y",
      runTest "nested dynamic key in let allowed" $
        assertEval "let-nested-dyn" "let k = \"q\"; in let a.${k} = 1; in a.q" (VInt 1),
      -- Nested interpolated strings inside braces: the lexer's brace-depth
      -- stack must restore the enclosing count when an interpolation
      -- closes, or the outer attrset's '}' mislexes as TokInterpClose.
      runTest "nested interpolated string inside attrset braces" $
        assertEval "nested-interp" "let y = \"v\"; in \"${ { a = \"${y}\"; }.a }\"" (mkStr "v"),
      runTest "nested interpolation in indented string" $
        assertEval "nested-interp-ind" "let y = \"w\"; in ''pre ${ { a = ''${y}''; }.a } post''" (mkStr "pre w post"),
      runTest "doubly nested interpolated strings" $
        assertEval "nested-interp-2" "let y = \"z\"; in \"${ { a = \"${ { b = \"${y}\"; }.b }\"; }.a }\"" (mkStr "z"),
      -- Attrpath merging (upstream parser.y addAttr): a literal set and a
      -- nested path under the same key merge; genuine duplicates error.
      runTest "literal set merges with attrpath (kept attr)" $
        assertEval "merge-keep" "{ a = { b = 1; }; a.c = 2; }.a.b" (VInt 1),
      runTest "literal set merges with attrpath (added attr)" $
        assertEval "merge-add" "{ a = { b = 1; }; a.c = 2; }.a.c" (VInt 2),
      runTest "duplicate plain key is a parse error" $
        assertParseFail "dup-key" "{ a = 1; a = 2; }",
      runTest "duplicate formal is rejected" $
        assertParseFail "dup-formal" "{ a, a }: a",
      runTest "duplicate formal with defaults is rejected" $
        assertParseFail "dup-formal-default" "{ a ? 1, a ? 2 }: a",
      runTest "duplicate inner key on merge is an error" $
        assertParseFail "dup-inner" "{ a.b = 1; a = { b = 2; }; }",
      runTest "inherit conflicting with a definition is an error" $
        assertParseFail "dup-inherit" "let q = 1; in { inherit q; q = 2; }",
      runTest "attrpath into a non-set is an error" $
        assertParseFail "merge-non-set" "{ a = 1; a.b = 2; }",
      -- rec markers in attrpath merging follow upstream's addAttr: the
      -- existing set's marker governs the result; the new set's marker is
      -- discarded.
      runTest "merge into rec literal keeps rec (sibling visible)" $
        assertEval "merge-rec-keep" "{ a = rec { b = 1; }; a.c = b; }.a.c" (VInt 1),
      runTest "merged-in rec marker is discarded (outer binding wins)" $
        assertEval "merge-rec-discard" "let d = 7; in { a = { x = 1; }; a = rec { c = d; }; }.a.c" (VInt 7),
      -- Exactly upstream's unprefixed global surface: fetchurl and toFile
      -- exist only under builtins.
      runTest "bare fetchurl is not a global" $
        assertEvalFail "no-bare-fetchurl" "fetchurl",
      runTest "bare toFile is not a global" $
        assertEvalFail "no-bare-tofile" "toFile",
      -- Upstream's ExprAttrs::eval binds a set's static names first, then
      -- each dynamic name in source order: it evaluates the name, skips a
      -- null, interns it, and raises an error naming where a name bound
      -- so far was defined, before it evaluates the next name
      -- (eval.cc).  A name is defined at its attribute path's first token
      -- (binds in parser.y at 2.24.9, binds1 at 2.36.0), and a position
      -- in text with no file is in upstream's string origin (Pos::print
      -- in position.cc).  A key of a plain string is a name, so each key
      -- here concatenates to stay dynamic.
      runTest "dynamic key colliding with static errors" $
        assertEvalError "dyn-collide" "{ b = 1; ${\"b\" + \"\"} = 2; }" (alreadyDefined "b" "1:3"),
      -- The name is written as upstream's printIdentifier writes an
      -- attribute name (print.cc at 2.24.9): one that is no identifier is
      -- a string literal.
      runTest "a colliding name that is no identifier is written quoted" $
        assertEvalError "dyn-collide-quoted" "{ \"a b\" = 1; ${\"a b\" + \"\"} = 2; }" (alreadyDefined "\"a b\"" "1:3"),
      runTest "rec dynamic key colliding with static errors" $
        assertEvalError "dyn-collide-rec" "rec { b = 1; p.q = 1; ${\"b\" + \"\"} = 2; }" (alreadyDefined "b" "1:7"),
      runTest "a dynamic name collides with a static name written after it, before the next dynamic name is evaluated" $
        assertEvalError "dyn-collide-static-after" "{ ${\"a\" + \"\"} = 1; ${throw \"y\"} = 2; a = 3; }" (alreadyDefined "a" "1:38"),
      runTest "a rec set's dynamic name collides with a static name written after it, before the next dynamic name is evaluated" $
        assertEvalError "dyn-collide-static-after-rec" "rec { ${\"a\" + \"\"} = 1; ${throw \"y\"} = 2; a = 3; }" (alreadyDefined "a" "1:42"),
      runTest "the first dynamic name in source order to collide is the one named" $
        assertEvalError "dyn-collide-first" "rec { ${\"b\" + \"\"} = 1; ${\"a\" + \"\"} = 2; a = 3; b = 4; }" (alreadyDefined "b" "1:48"),
      runTest "two equal dynamic names name where the first was defined" $
        assertEvalError "dyn-collide-dynamic" "{\n  ${\"a\" + \"\"} = 1;\n  ${\"a\" + \"\"} = 2;\n}" (alreadyDefined "a" "2:3"),
      runTest "an error in a later dynamic name does not win over an earlier collision" $
        assertEvalError "dyn-collide-before-error" "{ ${\"a\" + \"\"} = 1; ${\"a\" + \"\"} = 2; ${throw \"later\"} = 3; }" (alreadyDefined "a" "1:3"),
      runTest "an error in an earlier dynamic name wins over a later collision" $
        assertEvalError "dyn-error-before-collide" "{ ${throw \"earlier\"} = 1; a = 2; ${\"a\" + \"\"} = 3; }" "earlier",
      runTest "null dynamic names bind nothing and collide with nothing" $
        assertEval "dyn-null-twice" "{ ${null} = 1; a = 2; ${null} = 3; } == { a = 2; }" (VBool True)
          `andThen` assertEval "dyn-null-twice-rec" "rec { ${null} = 1; a = 2; ${null} = 3; } == { a = 2; }" (VBool True),
      runTest "a collision names a position in a file by the file's path" $
        assertEqual
          "dyn-collide-file"
          (Left ("dynamic attribute 'a' already defined at " <> positionsFile <> ":2:3"))
          (evalNixInFile "{\n  a = 1;\n  ${\"a\" + \"\"} = 2;\n}"),
      -- Higher-order list builtins pass elements as unforced thunks: an
      -- element the function never inspects may throw without failing.
      runTest "filter does not force elements" $
        assertEval "filter-lazy" "builtins.length (builtins.filter (x: true) [ (builtins.throw \"never\") ])" (VInt 1),
      runTest "any does not force undecided elements" $
        assertEval "any-lazy" "builtins.any (x: x) [ true (builtins.throw \"never\") ]" (VBool True),
      runTest "foldl' passes elements unforced" $
        assertEval "foldl-lazy" "builtins.foldl' (a: b: a) 0 [ (builtins.throw \"never\") ]" (VInt 0),
      -- Attrset equality stops at the first mismatch; later pairs are
      -- never forced.
      runTest "attrset eq short-circuits" $
        assertEval "eq-short" "{ a = 1; b = builtins.throw \"never\"; } == { a = 2; b = builtins.throw \"never\"; }" (VBool False),
      -- toString of a float is std::to_string's fixed 6 decimals; value
      -- printing and toJSON keep the trimmed form.
      runTest "toString float has fixed 6 decimals" $
        assertEval "tostr-float" "builtins.toString 1.5" (mkStr "1.500000"),
      runTest "toJSON float stays trimmed" $
        assertEval "tojson-float" "builtins.toJSON 1.5" (mkStr "1.5"),
      -- path + string-with-context is an error (a store-path reference
      -- cannot survive inside a path value).
      runTest "path plus string with context errors" $
        assertEvalFail "path-ctx" "./x + (derivation { name = \"q\"; system = \"s\"; builder = \"/b\"; }).drvPath",
      -- builtins.toJSON honors __toString, preferring it over outPath
      runTest "toJSON __toString" $
        assertEval "tojson-tostr" "builtins.toJSON { __toString = self: \"x\"; }" (mkStr "\"x\""),
      runTest "toJSON __toString beats outPath" $
        assertEval "tojson-tostr-out" "builtins.toJSON { __toString = self: \"a\"; outPath = \"/b\"; }" (mkStr "\"a\""),
      runTest "toJSON outPath still works" $
        assertEval "tojson-out" "builtins.toJSON { outPath = \"/b\"; }" (mkStr "\"/b\""),
      -- String interpolation is strict (coerceMore = False): scalars error
      runTest "interp rejects int" $
        assertEvalFail "interp-int" "\"v${1}\"",
      runTest "interp rejects bool" $
        assertEvalFail "interp-bool" "\"${true}\"",
      runTest "interp rejects null" $
        assertEvalFail "interp-null" "\"${null}\"",
      runTest "concatStringsSep rejects int" $
        assertEvalFail "ccs-int" "builtins.concatStringsSep \",\" [ 1 2 ]",
      -- builtins.toString stays permissive
      runTest "toString still coerces int" $
        assertEval "tostr-int" "builtins.toString 1" (mkStr "1"),
      runTest "interp via toString works" $
        assertEval "interp-tostr" "\"v${builtins.toString 1}\"" (mkStr "v1"),
      -- builtins.substring rejects a negative start position
      runTest "substring negative start errors" $
        assertEvalFail "substr-neg" "builtins.substring (0 - 1) 3 \"hello\"",
      -- an integer literal that overflows Int64 is a parse error, not a wrap
      runTest "integer literal overflow errors" $
        assertParseFail "int-overflow" "99999999999999999999",
      -- float exponents and leading-dot floats lex per the Nix grammar
      runTest "float exponent lexes as float" $
        assertEval "float-exp-type" "builtins.typeOf 6.022e23" (mkStr "float"),
      runTest "float negative exponent lexes" $
        assertEval "float-negexp-type" "builtins.typeOf 1.0e-3" (mkStr "float"),
      runTest "float exponent value" $
        assertEval "float-exp-val" "1.5e1 == 15.0" (VBool True),
      runTest "leading-dot float lexes as float" $
        assertEval "leading-dot-type" "builtins.typeOf .5" (mkStr "float"),
      runTest "leading-dot float value" $
        assertEval "leading-dot-val" ".5 == 0.5" (VBool True),
      -- PureEval tryEval must NOT catch abort - it propagates (matches C++ Nix)
      runTest "tryEval does not catch abort" $
        assertEvalFail "tryeval-abort" "(builtins.tryEval (builtins.abort \"x\")).success",
      runTest "every builtin builtins lists is in it" $
        let missing = [n | n <- map builtinName listedBuiltins, evalNix ("builtins ? \"" <> n <> "\"") /= Right (VBool True)]
         in if null missing then Pass else Fail ("not exposed: " <> T.intercalate ", " missing),
      runTest "every builtin is the one its name names" $
        let misnamed = [builtinName b | b <- [minBound .. maxBound], builtinNamed (builtinName b) /= Just b]
         in if null misnamed then Pass else Fail ("misnamed: " <> T.intercalate ", " misnamed)
    ]
  where
    -- Upstream's message for a dynamic name bound before
    -- (ExprAttrs::eval in eval.cc), the earlier definition at a line and
    -- column of a test's text, in guillemets (U+00AB, U+00BB) escaped to
    -- keep the source ASCII.
    alreadyDefined name lineColumn = "dynamic attribute '" <> name <> "' already defined at \x00ABstring\x00BB:" <> lineColumn

-- ---------------------------------------------------------------------------
-- Tests: NAR and hash known-answer vectors
-- ---------------------------------------------------------------------------

-- | fromTOML: multi-line values (the Cargo.lock shapes) and comment
-- placement relative to strings.
testFromTOML :: IO [Bool]
testFromTOML = do
  putStrLn "eval/fromTOML"
  sequence
    [ -- Key splitting and logical-line joining accumulate in chunks, so
      -- cost is linear in the input (per-character/per-line append was
      -- quadratic).  The watchdog turns a cost regression into a FAIL
      -- rather than a stuck suite.
      runTestM "fromTOML long key parses in linear time" $ do
        let source =
              "builtins.length (builtins.attrNames (builtins.fromTOML \""
                <> T.replicate 200000 "k"
                <> " = 1\"))"
            counted = evalNix source == Right (VInt 1)
        outcome <- timeout walkWatchdogMicros (evaluate counted)
        pure $ case outcome of
          Just True -> Pass
          Just False -> Fail "wrong parse for a long key"
          Nothing -> Fail "long key did not parse promptly",
      runTestM "fromTOML many continued lines join in linear time" $ do
        -- 4000 physical lines of 50 elements each: one ~400KB logical
        -- line, one 200000-element array.
        let line = T.intercalate " " (replicate 50 "1,")
            body = "x = [\n" <> T.intercalate "\n" (replicate 4000 line) <> "\n]"
            source = "builtins.length (builtins.fromTOML \"" <> body <> "\").x"
            counted = evalNix source == Right (VInt 200000)
        outcome <- timeout walkWatchdogMicros (evaluate counted)
        pure $ case outcome of
          Just True -> Pass
          Just False -> Fail "wrong parse for continued lines"
          Nothing -> Fail "continued lines did not join promptly",
      runTest "multi-line array" $
        assertEval
          "toml-ml-array"
          "builtins.concatStringsSep \",\" (builtins.fromTOML \"deps = [\\n \\\"bar\\\",\\n \\\"baz\\\",\\n]\\n\").deps"
          (mkStr "bar,baz"),
      runTest "Cargo.lock package shape" $
        assertEval
          "toml-cargo-lock"
          "(builtins.elemAt (builtins.fromTOML \"[[package]]\\nname = \\\"foo\\\"\\ndependencies = [\\n \\\"bar\\\",\\n]\\n\").package 0).name"
          (mkStr "foo"),
      runTest "multi-line basic string" $
        assertEval
          "toml-ml-string"
          "(builtins.fromTOML \"s = \\\"\\\"\\\"\\nline1\\nline2\\\"\\\"\\\"\").s"
          (mkStr "line1\nline2"),
      runTest "multi-line literal string" $
        assertEval
          "toml-ml-literal"
          "(builtins.fromTOML \"s = '''\\nkeep'''\").s"
          (mkStr "keep"),
      runTest "comment inside a multi-line array" $
        assertEval
          "toml-ml-comment"
          "builtins.concatStringsSep \",\" (builtins.fromTOML \"deps = [ # opening\\n \\\"bar\\\", # trailing\\n]\").deps"
          (mkStr "bar"),
      runTest "hash inside a string is content" $
        assertEval
          "toml-hash-content"
          "(builtins.fromTOML \"s = \\\"\\\"\\\"a#b\\\"\\\"\\\"\").s"
          (mkStr "a#b"),
      runTest "single-line values still parse" $
        assertEval
          "toml-single-line"
          "(builtins.fromTOML \"x = 4\\ny = \\\"z\\\" # cmt\").y"
          (mkStr "z")
    ]

testAttrPath :: IO [Bool]
testAttrPath = do
  putStrLn "\n-- Attribute paths (build -A) --"
  sequence
    [ runTest "a bare name is one component" $
        assertEqual "single" (Right ["hello"]) (parseAttrPath "hello"),
      runTest "dots separate components" $
        assertEqual "dotted" (Right ["a", "b", "c"]) (parseAttrPath "a.b.c"),
      runTest "a quoted component carries a dot" $
        assertEqual "quoted" (Right ["a", "b.c", "d"]) (parseAttrPath "a.\"b.c\".d"),
      -- Upstream's quotes concatenate rather than delimit, so a quoted run
      -- glues onto whatever sits beside it instead of standing alone.
      runTest "quotes concatenate onto the surrounding name" $
        assertEqual
          "glued"
          [Right ["foobar"], Right ["foobar"]]
          [parseAttrPath "\"foo\"bar", parseAttrPath "foo\"bar\""],
      -- The last component is kept only when non-empty, so these three
      -- select nothing at all rather than naming an empty attribute.
      runTest "an empty path selects nothing" $
        assertEqual
          "nothing"
          [Right [], Right [], Right ["a"]]
          [parseAttrPath "", parseAttrPath "\"\"", parseAttrPath "a."],
      -- An interior empty component survives tokenizing; selection rejects
      -- it, and only after checking the type of what it would index.
      runTest "an interior empty component survives tokenizing" $
        assertEqual
          "interior"
          [Right ["", "a"], Right ["a", "", "b"]]
          [parseAttrPath ".a", parseAttrPath "a..b"],
      runTest "an unclosed quote is an error" $
        assertEqual
          "unclosed"
          (Left "missing closing quote in selection path \'a.\"b\'")
          (parseAttrPath "a.\"b")
    ]

-- | The remembered-fetch record, on its own.  All three functions are pure,
-- and every property that keeps a warm cache honest is decided here: what
-- shares an entry, what round-trips, and what a torn file decodes to.
testFetchCache :: IO [Bool]
testFetchCache = do
  putStrLn "eval/fetch-cache"
  sequence
    [ runTest "the key separates every input that decides the answer" $
        let base = fetchCacheKey "https://example.com/r" "source" "abc" False False
            variants =
              [ fetchCacheKey "https://example.com/s" "source" "abc" False False,
                fetchCacheKey "https://example.com/r" "other" "abc" False False,
                fetchCacheKey "https://example.com/r" "source" "def" False False,
                fetchCacheKey "https://example.com/r" "source" "abc" True False,
                -- shallow decides revCount, so it cannot share an entry
                -- with a full clone of the same revision.
                fetchCacheKey "https://example.com/r" "source" "abc" False True
              ]
         in assertEqual "collisions with the base key" [] (filter (== base) variants),
      runTest "the same fetch keys the same both times" $
        assertEqual
          "repeat"
          (fetchCacheKey "https://example.com/r" "source" "abc" True True)
          (fetchCacheKey "https://example.com/r" "source" "abc" True True),
      runTest "an entry round-trips through encode and decode" $
        assertEqual "decode . encode" (Just sampleFetchCache) (decodeFetchCache (encodeFetchCache sampleFetchCache)),
      runTest "a torn entry decodes to nothing" $
        let whole = encodeFetchCache sampleFetchCache
            -- Every prefix, so the cut lands in each field in turn.  The
            -- last field is the one that matters: a cut before it leaves
            -- fewer than five lines and is rejected on shape alone, while
            -- a cut inside narHash still leaves five.
            torn = [T.take n whole | n <- [0 .. T.length whole - 1]]
         in assertEqual "prefixes that decoded" [] (filter (isJust . decodeFetchCache) torn),
      runTest "a non-decimal count is not an entry" $
        assertEqual
          "revCount"
          Nothing
          (decodeFetchCache (replaceCount (encodeFetchCache sampleFetchCache) "12x")),
      -- The hit path itself, end to end through builtins.fetchGit.  Nothing
      -- here can reach the network: the stub's createScratchDir throws, so
      -- a Right at all proves the fetch was skipped, and every field comes
      -- from the entry rather than from git.
      runTestM "a warm entry answers builtins.fetchGit without fetching" $
        case (evalWarmFetchGit sampleFetchCache (Set.singleton (fcStorePath sampleFetchCache)), parseStorePath defaultStoreDir (fcStorePath sampleFetchCache)) of
          (Left err, _) -> pure (Fail ("a warm cache still tried to fetch: " <> err))
          (Right _, Nothing) -> pure (Fail "the sample entry names no store path")
          (Right val, Just sampleStorePath) ->
            assertEqual
              "fields"
              [ ("outPath", storePathStringValue sampleStorePath),
                ("rev", mkStr (fcRev sampleFetchCache)),
                ("revCount", VInt 1234),
                ("narHash", mkStr ("sha256-" <> fcNarHash sampleFetchCache))
              ]
              <$> fetchGitFields val ["outPath", "rev", "revCount", "narHash"],
      -- The guard on the hit, on the axis that matters.  doesPathExist is
      -- False throughout the stub and adoptStorePath is what decides, so a
      -- hit that consulted mere existence would fail this and a hit that
      -- adopts the path passes the case above.  Adoption is what records
      -- the store write, and a hit that skipped it would hand back an
      -- outPath no derivation naming it could be built against.
      runTest "an entry whose path cannot be adopted refetches" $
        case evalWarmFetchGit sampleFetchCache Set.empty of
          Right _ -> Fail "an unadoptable entry was served from the cache"
          Left err
            | "createScratchDir" `T.isInfixOf` err -> Pass
            | otherwise -> Fail ("expected a refetch, got: " <> err),
      runTest "a bare ref is never served from the cache" $
        -- Whatever the cache holds, a ref means "wherever this branch
        -- points now", so the entry must not be consulted at all.
        case evalNixStubWith
          (warmEnv sampleFetchCache (Set.singleton (fcStorePath sampleFetchCache)))
          "builtins.fetchGit { url = \"https://example.com/r\"; ref = \"main\"; }" of
          Right _ -> Fail "a bare ref was served from the cache"
          Left err
            | "createScratchDir" `T.isInfixOf` err -> Pass
            | otherwise -> Fail ("expected a fetch, got: " <> err),
      runTest "a hash that is not base64 sha256 is not an entry" $
        assertEqual
          "narHash"
          [Nothing, Nothing, Nothing]
          ( map
              (decodeFetchCache . encodeFetchCache)
              [ sampleFetchCache {fcNarHash = T.take 43 (fcNarHash sampleFetchCache)},
                sampleFetchCache {fcNarHash = T.replicate 44 "="},
                sampleFetchCache {fcNarHash = T.take 43 (fcNarHash sampleFetchCache) <> "A"}
              ]
          )
    ]
  where
    -- Swap the encoded revCount line, which encodeFetchCache cannot
    -- produce: the field is an Integer on the way in.
    replaceCount encoded replacement = case T.splitOn "\n" encoded of
      [storePath, rev, _, lastModified, narHash] ->
        T.intercalate "\n" [storePath, rev, replacement, lastModified, narHash]
      _ -> encoded

-- | A stub store whose cache is warm for 'evalWarmFetchGit'\'s fetch, and
-- which will adopt exactly the given store paths.
warmEnv :: FetchCache -> Set.Set Text -> StubEnv
warmEnv fields adoptable =
  (stubEnv Map.empty)
    { seFetchCache =
        Map.singleton
          (fetchCacheKey "https://example.com/r" "source" warmFetchGitRev False False)
          (encodeFetchCache fields),
      seAdoptable = adoptable
    }

-- | The pinned revision 'evalWarmFetchGit' asks for.
warmFetchGitRev :: Text
warmFetchGitRev = T.replicate 40 "b"

-- | Evaluate a pinned @builtins.fetchGit@ against a warm cache.
evalWarmFetchGit :: FetchCache -> Set.Set Text -> Either Text NixValue
evalWarmFetchGit fields adoptable =
  evalNixStubWith
    (warmEnv fields adoptable)
    ("builtins.fetchGit { url = \"https://example.com/r\"; rev = \"" <> warmFetchGitRev <> "\"; }")

-- | Read the named attributes off a @fetchGit@ result, in the order asked:
-- those present and computed.
fetchGitFields :: NixValue -> [Text] -> IO [(Text, NixValue)]
fetchGitFields val names = case val of
  VAttrs attrs -> catMaybes <$> mapM (computedField attrs) names
  _ -> pure []
  where
    computedField attrs name =
      attrSetLookup name attrs >>= \case
        Just thunk -> fmap (name,) <$> readThunkValue thunk
        Nothing -> pure Nothing

-- | One recorded fetch, with a real 44-character base64 sha256.
sampleFetchCache :: FetchCache
sampleFetchCache =
  FetchCache
    { fcStorePath = "/nix/store/" <> T.replicate 32 "a" <> "-source",
      fcRev = T.replicate 40 "b",
      fcRevCount = 1234,
      fcLastModified = 1700000000,
      fcNarHash = T.replicate 43 "A" <> "="
    }

testFetchGitTransport :: IO [Bool]
testFetchGitTransport = do
  putStrLn "eval/fetchgit-transport"
  sequence
    [ runTest "allowed schemes pass" $
        assertEqual
          "schemes"
          [ Right "https://example.com/repo.git",
            Right "http://example.com/repo.git",
            Right "ssh://git@example.com/repo.git",
            Right "git://example.com/repo.git",
            Right "file:///srv/repo"
          ]
          ( map
              checkGitUrl
              [ "https://example.com/repo.git",
                "http://example.com/repo.git",
                "ssh://git@example.com/repo.git",
                "git://example.com/repo.git",
                "file:///srv/repo"
              ]
          ),
      runTest "scheme match is case-insensitive" $
        assertEqual "upper" (Right "HTTPS://example.com/r") (checkGitUrl "HTTPS://example.com/r"),
      runTest "scp-like remotes and local paths pass" $
        assertEqual
          "plain"
          [ Right "git@github.com:owner/repo.git",
            Right "user@[::1]:repo",
            Right "/srv/repo",
            Right "./repo",
            Right "C:\\src\\repo"
          ]
          ( map
              checkGitUrl
              [ "git@github.com:owner/repo.git",
                "user@[::1]:repo",
                "/srv/repo",
                "./repo",
                "C:\\src\\repo"
              ]
          ),
      runTest "helper transports are rejected" $
        let rejected = ["ext::sh -c 'printf x'", "fd::17", "my-helper_2::payload", "::payload"]
         in case [url | url <- rejected, either (const False) (const True) (checkGitUrl url)] of
              [] -> Pass
              slipped -> Fail ("helper urls accepted: " <> T.pack (show slipped)),
      runTest "an unknown explicit scheme is rejected" $
        assertLeft "hg scheme" (checkGitUrl "hg://example.com/repo"),
      runTest "the empty url is rejected" $
        assertLeft "empty" (checkGitUrl ""),
      runTestM "fetchGit refuses a helper transport at eval time" $ do
        outcome <- evalNixIO "." "builtins.fetchGit { url = \"ext::printf x\"; }"
        pure $ case outcome of
          Left err
            | "transport" `T.isInfixOf` err -> Pass
            | otherwise -> Fail ("wrong error: " <> err)
          Right _ -> Fail "helper transport url evaluated",
      -- A ref reaching git as an option is command execution: git parses
      -- options after the remote name, and --upload-pack names the
      -- transport command.  The leading-character class is what stops it.
      runTest "a ref that git would read as an option is rejected" $
        let rejected =
              [ "--upload-pack=touch /tmp/pwned",
                "-o",
                "--exec=sh",
                "",
                "branch with spaces",
                "semi;colon",
                "back\\slash"
              ]
         in case [ref | ref <- rejected, either (const False) (const True) (checkGitRef ref)] of
              [] -> Pass
              slipped -> Fail ("refs accepted: " <> T.pack (show slipped)),
      runTest "ordinary ref names are accepted" $
        let accepted = ["main", "HEAD", "release/1.0", "v2.3.4", "user/topic-1", "@", "a+b"]
         in case [ref | ref <- accepted, either (const True) (const False) (checkGitRef ref)] of
              [] -> Pass
              refused -> Fail ("refs refused: " <> T.pack (show refused)),
      runTest "a rev must be a full SHA-1" $
        let rejected =
              [ "--upload-pack=touch /tmp/pwned",
                "dcb93b58",
                "v1.0",
                "main",
                "",
                T.replicate 40 "g",
                T.replicate 41 "a"
              ]
         in case [rev | rev <- rejected, either (const False) (const True) (checkGitRev rev)] of
              [] -> Pass
              slipped -> Fail ("revs accepted: " <> T.pack (show slipped)),
      runTest "a full SHA-1 is accepted in either case" $
        let accepted = [T.replicate 40 "a", T.replicate 40 "F", "dcb93b58bd982ab64323bac2a8c11b138d6b55e5"]
         in case [rev | rev <- accepted, either (const True) (const False) (checkGitRev rev)] of
              [] -> Pass
              refused -> Fail ("revs refused: " <> T.pack (show refused)),
      runTestM "fetchGit refuses an option-shaped ref at eval time" $ do
        outcome <- evalNixIO "." "builtins.fetchGit { url = \"/srv/repo\"; ref = \"--upload-pack=id\"; }"
        pure $ case outcome of
          Left err
            | "ref is not a valid git ref name" `T.isInfixOf` err -> Pass
            | otherwise -> Fail ("wrong error: " <> err)
          Right _ -> Fail "option-shaped ref evaluated"
    ]

-- ---------------------------------------------------------------------------
-- builtins.fetchGit against a real repository
-- ---------------------------------------------------------------------------

testFetchGitShallow :: IO [Bool]
testFetchGitShallow = do
  tmpBase <- getTemporaryDirectory
  let repoDir = tmpBase </> "nova-nix-test-fetchgit-repo"
      tmpStore = tmpBase </> "nova-nix-test-fetchgit-store"
      cacheHome = tmpBase </> "nova-nix-test-fetchgit-cache"
      nixEscape = T.concatMap (\c -> if c == '\\' then "\\\\" else if c == '"' then "\\\"" else T.singleton c)
      urlAttr = "url = \"" <> nixEscape (T.pack repoDir) <> "\"; "
      evalFetch extra = evalNixIOStore (StoreDir tmpStore) "." ("builtins.fetchGit { " <> urlAttr <> extra <> "}")
  forceRemoveIfExists repoDir
  forceRemoveIfExists tmpStore
  forceRemoveIfExists cacheHome
  -- The fetch cache writes under XdgCache; point it at the scratch area
  -- for the duration so the suite never touches the developer's cache.
  savedCacheHome <- lookupEnv "XDG_CACHE_HOME"
  setEnv "XDG_CACHE_HOME" cacheHome
  shas <- makeGitFixture repoDir [("first.txt", "one\n"), ("second.txt", "two\n"), ("third.txt", "three\n")]
  results <-
    sequence
      [ -- Upstream reports revCount 0 under shallow, not an absent
        -- attribute and not the fetch depth: observed from
        -- nix-instantiate 2.33.2, where '.revCount or "ABSENT"' is 0.
        -- The tree itself is depth-independent, so both fetches must
        -- agree on narHash and outPath.
        runTestM "a shallow fetch reports revCount 0 for the same tree" $ do
          fullOutcome <- evalFetch ""
          shallowOutcome <- evalFetch "shallow = true; "
          case (fullOutcome, shallowOutcome) of
            (Left err, _) -> pure (Fail ("full fetch failed: " <> err))
            (_, Left err) -> pure (Fail ("shallow fetch failed: " <> err))
            (Right fullVal, Right shallowVal) -> do
              fullFields <- fetchGitFields fullVal ["revCount", "narHash", "outPath"]
              shallowFields <- fetchGitFields shallowVal ["revCount", "narHash", "outPath"]
              pure $ case (fullFields, shallowFields) of
                ([("revCount", VInt 3), ("narHash", fullHash), ("outPath", fullPath)], [("revCount", VInt 0), ("narHash", shallowHash), ("outPath", shallowPath)])
                  | fullHash == shallowHash && fullPath == shallowPath -> Pass
                  | otherwise -> Fail "shallow and full fetches disagree about the tree"
                _ -> Fail (T.pack (show (fullFields, shallowFields))),
        -- The rev itself is the refspec, so a depth-1 fetch of a rev
        -- that is not the branch tip still lands on exactly that rev.
        runTestM "a shallow fetch of a pinned rev checks out that rev" $ case shas of
          (firstSha : _) -> do
            outcome <- evalFetch ("shallow = true; rev = \"" <> firstSha <> "\"; ")
            case outcome of
              Left err -> pure (Fail err)
              Right val ->
                fetchGitFields val ["rev", "revCount", "outPath"] >>= \case
                  [("rev", revVal), ("revCount", VInt 0), ("outPath", outVal)]
                    | revVal == mkStr firstSha,
                      Just outPath <- fetchedStorePath outVal -> do
                        let outDir = storePathToFilePath (StoreDir tmpStore) outPath
                        firstThere <- Dir.doesFileExist (outDir </> "first.txt")
                        thirdThere <- Dir.doesFileExist (outDir </> "third.txt")
                        pure $
                          if firstThere && not thirdThere
                            then Pass
                            else Fail "outPath does not hold the pinned rev's tree"
                  fields -> pure (Fail (T.pack (show fields)))
          [] -> pure (Fail "fixture returned no commits"),
        -- Pinned and unpinned differ in where counting starts: from the
        -- rev, the root commit counts 1 (upstream 2.33.2 agrees), not
        -- the branch's 3.
        runTestM "a pinned rev fetch counts from the rev" $ case shas of
          (firstSha : _) -> do
            outcome <- evalFetch ("rev = \"" <> firstSha <> "\"; ")
            case outcome of
              Left err -> pure (Fail err)
              Right val ->
                fetchGitFields val ["rev", "revCount"] >>= \case
                  [("rev", revVal), ("revCount", VInt 1)]
                    | revVal == mkStr firstSha -> pure Pass
                  fields -> pure (Fail (T.pack (show fields)))
          [] -> pure (Fail "fixture returned no commits")
      ]
  maybe (unsetEnv "XDG_CACHE_HOME") (setEnv "XDG_CACHE_HOME") savedCacheHome
  forceRemoveIfExists repoDir
  forceRemoveIfExists tmpStore
  forceRemoveIfExists cacheHome
  pure results

-- | fetchGit's attributes: unsupported attributes refuse, a declared
-- narHash pins the tree on hit and fresh paths alike, allRefs widens the
-- fetch, and a failing fetch leaves no scratch clone behind.
testFetchGitPins :: IO [Bool]
testFetchGitPins = do
  tmpBase <- getTemporaryDirectory
  let tmpStore = tmpBase </> "nova-nix-test-pins-store"
      repoDir = tmpBase </> "nova-nix-test-pins-repo"
      cacheHome = tmpBase </> "nova-nix-test-pins-cache"
      urlText = T.concatMap (\c -> if c == '\\' then "\\\\" else T.singleton c) (T.pack repoDir)
      evalHere = evalNixIOStore (StoreDir tmpStore) "."
      fetchWith extra = evalHere ("builtins.fetchGit { url = \"" <> urlText <> "\"; " <> extra <> "}")
  forceRemoveIfExists tmpStore
  forceRemoveIfExists repoDir
  forceRemoveIfExists cacheHome
  savedCacheHome <- lookupEnv "XDG_CACHE_HOME"
  setEnv "XDG_CACHE_HOME" cacheHome
  shas <- makeGitFixture repoDir [("a.txt", "one\n")]
  baseBranch <- fixtureGit repoDir ["rev-parse", "--abbrev-ref", "HEAD"]
  _ <- fixtureGit repoDir ["checkout", "-q", "-b", "side"]
  BS.writeFile (repoDir </> "side.txt") (TE.encodeUtf8 "side\n")
  _ <- fixtureGit repoDir ["add", "-A"]
  _ <- fixtureGit repoDir ["commit", "--quiet", "-m", "side"]
  sideSha <- fixtureGit repoDir ["rev-parse", "HEAD"]
  _ <- fixtureGit repoDir ["checkout", "-q", T.unpack baseBranch]
  results <-
    sequence
      [ runTestM "an unsupported fetchGit attribute is refused" $ do
          outcome <- fetchWith "bogus = 1; "
          pure $ case outcome of
            Left err
              | "attribute 'bogus' not supported" `T.isInfixOf` err -> Pass
              | otherwise -> Fail ("wrong error: " <> err)
            Right _ -> Fail "an unsupported attribute was silently dropped",
        runTestM "a mismatched narHash pin is an error" $ case shas of
          (sha : _) -> do
            outcome <- fetchWith ("rev = \"" <> sha <> "\"; narHash = \"sha256-" <> T.replicate 43 "A" <> "=\"; ")
            pure $ case outcome of
              Left err
                | "NAR hash mismatch" `T.isInfixOf` err -> Pass
                | otherwise -> Fail ("wrong error: " <> err)
              Right _ -> Fail "a bogus narHash pin was silently ignored"
          [] -> pure (Fail "fixture returned no commits"),
        -- The second fetch of the same rev is a cache hit, so a matching
        -- pin passing here proves the check runs on the hit path too.
        runTestM "a matching narHash pin passes, including on a cache hit" $ case shas of
          (sha : _) -> do
            first <- fetchWith ("rev = \"" <> sha <> "\"; ")
            case first of
              Left err -> pure (Fail err)
              Right val ->
                fetchGitFields val ["narHash"] >>= \case
                  [("narHash", VStr hashBytes _)] -> do
                    let hash = TE.decodeUtf8Lenient hashBytes
                    second <- fetchWith ("rev = \"" <> sha <> "\"; narHash = \"" <> hash <> "\"; ")
                    pure $ case second of
                      Right _ -> Pass
                      Left err -> Fail ("a correct pin was refused: " <> err)
                  other -> pure (Fail (T.pack (show other)))
          [] -> pure (Fail "fixture returned no commits"),
        -- A remembered tree on disk with other metadata, as a writer that
        -- left it owner-writable with the time it wrote it would, is made
        -- canonical when a cache hit adopts it, as a fresh fetch's is.
        runTestM "a cache hit makes the remembered tree's metadata canonical" $ case shas of
          (sha : _) -> do
            first <- fetchWith ("rev = \"" <> sha <> "\"; ")
            case first of
              Left err -> pure (Fail err)
              Right val ->
                fetchGitFields val ["outPath"] >>= \case
                  [("outPath", outVal)] | Just sp <- fetchedStorePath outVal -> do
                    let dest = storePathToFilePath (StoreDir tmpStore) sp
                        file = dest </> "a.txt"
                    now <- Clock.getCurrentTime
                    traverse_
                      (\node -> Dir.getPermissions node >>= Dir.setPermissions node . Dir.setOwnerWritable True >> Dir.setModificationTime node now)
                      [file, dest]
                    aged <- traverse nodeStatus [dest, file]
                    second <- fetchWith ("rev = \"" <> sha <> "\"; ")
                    results <- traverse (uncurry canonicalAs) [(executablePermissions, dest), (readOnlyPermissions, file)]
                    pure $
                      assertEqual "aged" True (all (maybe True (\st -> nsPermissions st `elem` [0o755, 0o644] && nsModificationTime st /= storeModificationTime)) aged)
                        `andThen` either (Fail . ("the cache hit failed: " <>)) (const Pass) second
                        `andThen` foldr andThen Pass results
                  other -> pure (Fail (T.pack (show other)))
          [] -> pure (Fail "fixture returned no commits"),
        -- A declared narHash is read as Input::getNarHash reads it and
        -- the hashes compared (InputScheme::checkLocks in fetchers.cc
        -- at 2.24.9, Input::checkLocks at 2.36.0), so text after its
        -- padding, which base64 decoding never reads, still matches.
        runTestM "a narHash pin with text after its padding matches" $ case shas of
          (sha : _) -> do
            let pinned = "rev = \"" <> sha <> "\"; "
                fetched = "(builtins.fetchGit { url = \"" <> urlText <> "\"; " <> pinned <> "})"
            outcome <- fetchWith (pinned <> "narHash = " <> fetched <> ".narHash + \"io\"; ")
            pure $ case outcome of
              Right _ -> Pass
              Left err -> Fail ("a pin of the fetched hash was refused: " <> err)
          [] -> pure (Fail "fixture returned no commits"),
        runTestM "a narHash pin that is not SRI is refused" $ case shas of
          (sha : _) -> do
            let prefixed = "sha256:" <> T.replicate 64 "0"
            outcome <- fetchWith ("rev = \"" <> sha <> "\"; narHash = \"" <> prefixed <> "\"; ")
            pure $ case outcome of
              Left err -> assertEqual "narHash-not-sri" ("hash '" <> prefixed <> "' is not SRI") err
              Right _ -> Fail "a narHash pin that is not SRI was accepted"
          [] -> pure (Fail "fixture returned no commits"),
        runTestM "a narHash pin that is not SHA-256 is refused" $ case shas of
          (sha : _) -> do
            outcome <- fetchWith ("rev = \"" <> sha <> "\"; narHash = \"sha512-" <> T.replicate 86 "A" <> "==\"; ")
            pure $ case outcome of
              Left err -> assertEqual "narHash-not-sha256" "narHash must use SHA-256" err
              Right _ -> Fail "a sha512 narHash pin was accepted"
          [] -> pure (Fail "fixture returned no commits"),
        runTestM "allRefs finds a rev on an unfetched branch" $ do
          outcome <- fetchWith ("rev = \"" <> sideSha <> "\"; allRefs = true; ")
          case outcome of
            Left err -> pure (Fail err)
            Right val ->
              fetchGitFields val ["rev"] >>= \case
                [("rev", revVal)] | revVal == mkStr sideSha -> pure Pass
                other -> pure (Fail (T.pack (show other))),
        runTestM "a failing fetch leaves no scratch clone behind" $ do
          before <- filter ("nova-nix-fetchgit-" `T.isPrefixOf`) . map T.pack <$> Dir.listDirectory tmpBase
          outcome <- evalHere ("builtins.fetchGit { url = \"" <> urlText <> "-no-such-repo\"; }")
          after <- filter ("nova-nix-fetchgit-" `T.isPrefixOf`) . map T.pack <$> Dir.listDirectory tmpBase
          pure $ case outcome of
            Right _ -> Fail "a fetch of a missing repo succeeded"
            Left _
              | length after == length before -> Pass
              | otherwise -> Fail ("scratch clones leaked: " <> T.pack (show (length after - length before)))
      ]
  maybe (unsetEnv "XDG_CACHE_HOME") (setEnv "XDG_CACHE_HOME") savedCacheHome
  forceRemoveIfExists tmpStore
  forceRemoveIfExists repoDir
  forceRemoveIfExists cacheHome
  pure results

-- | String interpolation inside path literals, the construct
-- nixpkgs uses 89 times on modern trees.  Every rule here is pinned to
-- nix-instantiate 2.33.2's observed behavior: the result is a path,
-- pieces concatenate with no separator and canonicalize textually, a
-- path segment coerces without a store copy, context is refused, and
-- a trailing slash is a parse error.
testPathInterp :: IO [Bool]
testPathInterp = do
  tmpBase <- getTemporaryDirectory
  let fixtureDir = tmpBase </> "nova-nix-test-path-interp"
      tmpStore = tmpBase </> "nova-nix-test-path-interp-store"
  forceRemoveIfExists fixtureDir
  forceRemoveIfExists tmpStore
  Dir.createDirectoryIfMissing True (fixtureDir </> "sub")
  BS.writeFile (fixtureDir </> "sub" </> "f.nix") (TE.encodeUtf8 "42")
  let evalHere = evalNixIO fixtureDir
      baseText = canonPathValue (T.pack fixtureDir)
  results <-
    sequence
      [ runTestM "an interpolated path literal is a path" $ do
          outcome <- evalHere "let v = \"sub\"; in builtins.typeOf ./${v}"
          pure (assertRightValue outcome (mkStr "path")),
        runTestM "a runtime dot-dot canonicalizes textually" $ do
          outcome <- evalHere "let v = \"sub/..\"; in toString ./${v}"
          pure (assertRightValue outcome (mkStr baseText)),
        runTestM "pieces concatenate with no separator, mid-segment included" $ do
          outcome <- evalHere "let v = \"a\"; in toString ./pre${v}${v}post"
          pure (assertRightValue outcome (mkStr (baseText <> "/preaapost"))),
        runTestM "an empty segment after the root is the root" $ do
          outcome <- evalHere "toString /${\"\"}"
          pure (assertRightValue outcome (mkStr "/")),
        runTestM "a dotless relative literal interpolates" $ do
          outcome <- evalHere "let v = \"f.nix\"; in toString sub/${v}"
          pure (assertRightValue outcome (mkStr (baseText <> "/sub/f.nix"))),
        runTestM "a path segment must coerce to a string" $ do
          outcome <- evalHere "./${1}"
          pure $ case outcome of
            Left err
              | "cannot coerce an integer to a string" `T.isInfixOf` err -> Pass
              | otherwise -> Fail ("wrong error: " <> err)
            Right _ -> Fail "an integer segment was accepted",
        -- A path INSIDE a path interpolation appends its absolute text
        -- with no store copy (string interpolation would copy) -
        -- upstream's exact, odd, observable behavior.  The expected text
        -- goes through the canonicalizer because the separator between
        -- the two halves is platform-shaped: on POSIX the inner path's
        -- own leading slash serves, on Windows (C:...) the head's slash
        -- survives instead.
        runTestM "a path segment appends textually without a store copy" $ do
          outcome <- evalHere "toString ./${./sub}"
          pure (assertRightValue outcome (mkStr (canonPathValue (baseText <> "/" <> baseText <> "/sub")))),
        runTestM "a context-carrying segment is refused" $ do
          outcome <- evalNixIOStore (StoreDir tmpStore) fixtureDir "./${builtins.toFile \"n\" \"x\"}"
          pure $ case outcome of
            Left err
              | "cannot be appended to a path" `T.isInfixOf` err -> Pass
              | otherwise -> Fail ("wrong error: " <> err)
            Right _ -> Fail "a store-path segment was accepted",
        runTestM "a trailing slash is a parse error" $ do
          outcome <- evalHere "let v = \"a\"; in ./sub/${v}/"
          pure $ case outcome of
            Left err
              | "path has a trailing slash" `T.isInfixOf` err -> Pass
              | otherwise -> Fail ("wrong error: " <> err)
            Right _ -> Fail "a trailing slash parsed",
        runTestM "a plain trailing-slash literal is a parse error" $ do
          outcome <- evalHere "./sub/"
          pure $ case outcome of
            Left err
              | "path has a trailing slash" `T.isInfixOf` err -> Pass
              | otherwise -> Fail ("wrong error: " <> err)
            Right _ -> Fail "a trailing slash parsed",
        runTestM "import resolves an interpolated dot-dot path" $ do
          outcome <- evalHere "let v = \"sub\"; in import ./${v}/../sub/f.nix"
          pure (assertRightValue outcome (VInt 42)),
        -- An spath followed by /${...} is NOT one literal: it parses as
        -- application of the search path to a path, which fails at
        -- eval - upstream's exact shape.
        runTestM "a search path does not absorb an interpolation" $ do
          outcome <- evalHere "let v = \"a\"; in <nixpkgs>/${v}"
          pure $ case outcome of
            Left _ -> Pass
            Right val -> Fail ("spath absorbed the interpolation: " <> T.pack (show val))
      ]
  forceRemoveIfExists fixtureDir
  forceRemoveIfExists tmpStore
  pure results
  where
    assertRightValue outcome expected = case outcome of
      Right val | val == expected -> Pass
      other -> Fail (T.pack (show other))

-- | A path reached through an attrset's @outPath@ (or a @__toString@
-- result) coerces exactly as the same path written directly: copied to
-- the store, with the store path in the string context.  Rendering the
-- raw filesystem path, with no copy and no context, would give
-- @src = builtins.fetchGit { ... }@ empty inputSrcs and a drvPath
-- diverging from upstream.  Verified against nix-instantiate 2.33.2:
-- the attrset form's drvPath matches upstream byte for byte.
testOutPathCoercion :: IO [Bool]
testOutPathCoercion = do
  tmpBase <- getTemporaryDirectory
  let tmpStore = tmpBase </> "nova-nix-test-outpath-store"
      srcDir = tmpBase </> "nova-nix-test-outpath-src"
      cacheHome = tmpBase </> "nova-nix-test-outpath-cache"
      repoDir = tmpBase </> "nova-nix-test-outpath-repo"
      -- A RELATIVE path literal resolved against the eval's base dir: an
      -- absolute Windows path (C:/...) is not a path literal at all - it
      -- matches the URI rule (scheme:rest) and lexes as a string, which
      -- correctly never copies.  Relative spelling lexes as a path on
      -- every platform.
      srcLiteral = "./nova-nix-test-outpath-src" :: Text
      drvPathWith srcExpr =
        "(derivation { name = \"ctx-probe\"; system = \"x86_64-linux\"; "
          <> "builder = \"/bin/sh\"; args = [\"-c\" \"echo ok > $out\"]; "
          <> "src = "
          <> srcExpr
          <> "; }).drvPath"
      evalHere = evalNixIOStore (StoreDir tmpStore) tmpBase
      storeStr result = case result of
        Right (VStr s _) -> Just (TE.decodeUtf8Lenient s)
        _ -> Nothing
  forceRemoveIfExists tmpStore
  forceRemoveIfExists srcDir
  forceRemoveIfExists cacheHome
  forceRemoveIfExists repoDir
  Dir.createDirectoryIfMissing True srcDir
  BS.writeFile (srcDir </> "f.txt") (TE.encodeUtf8 "content\n")
  savedCacheHome <- lookupEnv "XDG_CACHE_HOME"
  setEnv "XDG_CACHE_HOME" cacheHome
  gitShas <- makeGitFixture repoDir [("g.txt", "tracked\n")]
  results <-
    sequence
      [ runTestM "an outPath attrset src derives the same drv as its path" $ do
          attrForm <- evalHere (drvPathWith ("{ outPath = " <> srcLiteral <> "; }"))
          pathForm <- evalHere (drvPathWith srcLiteral)
          pure $ case (attrForm, pathForm) of
            (Right a, Right b)
              | a == b -> Pass
              | otherwise -> Fail (T.pack (show (a, b)))
            other -> Fail (T.pack (show other)),
        runTestM "a fetchGit src derives the same drv as its outPath" $ case gitShas of
          (sha : _) -> do
            let fetch = "builtins.fetchGit { url = \"" <> T.concatMap (\c -> if c == '\\' then "\\\\" else T.singleton c) (T.pack repoDir) <> "\"; rev = \"" <> sha <> "\"; }"
            attrForm <- evalHere (drvPathWith fetch)
            outPathForm <- evalHere (drvPathWith ("(" <> fetch <> ").outPath"))
            pure $ case (attrForm, outPathForm) of
              (Right a, Right b)
                | a == b -> Pass
                | otherwise -> Fail (T.pack (show (a, b)))
              other -> Fail (T.pack (show other))
          [] -> pure (Fail "fixture returned no commits"),
        -- Text-only assertions here and below: a source copy is
        -- materialized by the build driver, not by a bare eval, so the
        -- observable at eval time is the store-path spelling.  The two
        -- drvPath tests above cover the context end (an uncontexted src
        -- would derive a different drv).
        runTestM "interpolating an outPath attrset coerces to its store path" $ do
          outcome <- evalHere ("\"${ { outPath = " <> srcLiteral <> "; } }\"")
          pure $ case storeStr outcome of
            Just text | isCanonicalStoreText text -> Pass
            _ -> Fail (T.pack (show outcome)),
        runTestM "a __toString path result coerces to its store path" $ do
          outcome <- evalHere ("\"${ { __toString = self: " <> srcLiteral <> "; } }\"")
          pure $ case storeStr outcome of
            Just text | isCanonicalStoreText text -> Pass
            _ -> Fail (T.pack (show outcome)),
        runTestM "adding an outPath attrset to a string coerces to its store path" $ do
          outcome <- evalHere ("\"src: \" + { outPath = " <> srcLiteral <> "; }")
          pure $ case storeStr outcome of
            Just text | Just rest <- T.stripPrefix "src: " text, isCanonicalStoreText rest -> Pass
            _ -> Fail (T.pack (show outcome)),
        runTestM "toJSON of an outPath attrset is its store path" $ do
          outcome <- evalHere ("builtins.toJSON { outPath = " <> srcLiteral <> "; }")
          pure $ case storeStr outcome of
            Just text
              | Just inner <- T.stripPrefix "\"" text >>= T.stripSuffix "\"",
                isCanonicalStoreText inner ->
                  Pass
            _ -> Fail (T.pack (show outcome))
      ]
  maybe (unsetEnv "XDG_CACHE_HOME") (setEnv "XDG_CACHE_HOME") savedCacheHome
  forceRemoveIfExists tmpStore
  forceRemoveIfExists srcDir
  forceRemoveIfExists cacheHome
  forceRemoveIfExists repoDir
  pure results

testScratchDirs :: IO [Bool]
testScratchDirs = do
  putStrLn "eval/scratch-dirs"
  st <- testEvalState platformStoreDir "."
  sequence
    [ runTestM "scratch dirs are distinct, real, and removable" $ do
        createdA <- runEvalIO st (createScratchDir "nova-nix-test-scratch-")
        createdB <- runEvalIO st (createScratchDir "nova-nix-test-scratch-")
        case (createdA, createdB) of
          (Right dirA, Right dirB)
            | dirA == dirB -> pure (Fail "two scratch dirs share a name")
            | otherwise -> do
                existedA <- Dir.doesDirectoryExist (T.unpack dirA)
                existedB <- Dir.doesDirectoryExist (T.unpack dirB)
                removed <- runEvalIO st (removeScratchDir dirA >> removeScratchDir dirB)
                goneA <- not <$> Dir.doesDirectoryExist (T.unpack dirA)
                goneB <- not <$> Dir.doesDirectoryExist (T.unpack dirB)
                pure $ case removed of
                  Left err -> Fail ("removeScratchDir failed: " <> err)
                  Right ()
                    | existedA && existedB && goneA && goneB -> Pass
                    | otherwise -> Fail "scratch dir lifecycle out of order"
          (Left err, _) -> pure (Fail ("createScratchDir failed: " <> err))
          (_, Left err) -> pure (Fail ("createScratchDir failed: " <> err)),
      runTestM "scratch names carry the requested prefix" $ do
        created <- runEvalIO st (createScratchDir "nova-nix-test-scratch-")
        case created of
          Left err -> pure (Fail ("createScratchDir failed: " <> err))
          Right dir -> do
            removed <- runEvalIO st (removeScratchDir dir)
            pure $ case removed of
              Left err -> Fail ("removeScratchDir failed: " <> err)
              Right ()
                | "nova-nix-test-scratch-" `T.isInfixOf` dir -> Pass
                | otherwise -> Fail ("prefix missing from: " <> dir)
    ]

-- ---------------------------------------------------------------------------
-- Tests: realising a string's context
-- ---------------------------------------------------------------------------

-- | What a fake store is asked, in order.
data FakeCall
  = FakeQueried !(Set.Set StorePath)
  | FakeAdded
  | FakeBuilt ![(StorePath, Set.Set Text)]
  deriving (Eq, Show)

-- | A store holding the paths the predicate admits and what is added to
-- it, answering every add and build as done and the build's with the stop
-- given, and recording each call.
fakeRealiseStore :: (StorePath -> Bool) -> Maybe BuildStop -> IO (StoreOps IO, IO [FakeCall])
fakeRealiseStore holds stop = do
  calls <- newIORef []
  added <- newIORef Set.empty
  let record call = modifyIORef' calls (call :)
      held path = (holds path ||) . Set.member path <$> readIORef added
      objectPaths objects =
        Set.fromList (Map.keys (eoSources objects) <> mapMaybe (parseStorePath defaultStoreDir) (Map.keys (eoStoreWrites objects) <> Map.keys (eoDerivations objects)))
      ops =
        StoreOps
          { soQueries =
              StoreQueries
                { sqValidPaths = \paths -> record (FakeQueried paths) >> Set.fromList <$> filterM held (Set.toList paths),
                  sqReferences = \_ -> pure (Right Set.empty)
                },
            soAddEvalObjects = \objects -> Right () <$ (record FakeAdded >> modifyIORef' added (<> objectPaths objects)),
            soAddSourceTree = \_ _ _ -> pure (Left "a tree no test asked for"),
            soBuild = \requests -> maybe (Right ()) Left stop <$ record (FakeBuilt requests),
            soCheck = \_ -> pure (Right ()),
            soAddOutLinkRoot = \link _ -> pure (Right link)
          }
  pure (ops, reverse <$> readIORef calls)

-- | A read-only session over a fake store's queries and build.
readOnlyOver :: StoreOps IO -> SessionStore
readOnlyOver ops = ReadOnlySession (soQueries ops) ops

-- | Evaluate source in a state, keeping the whole failure, its lines of
-- context included.
evalTracedIn :: EvalState -> FilePath -> Text -> IO (Either EvalFailure NixValue)
evalTracedIn st baseDir source = case parseNix baseDir testOrigin (TE.encodeUtf8 source) of
  Left err -> pure (Left (EvalFailure [] (T.pack (show err))))
  Right parsed -> runEvalIOTraced st (evalParsed (pure (evalBaseEnv st)) parsed)

-- | The planning of a context's realisation, which a session's store
-- answers for: what is asked in which order, what each session refuses,
-- and what a read-write one adds and builds.
testRealiseContext :: IO [Bool]
testRealiseContext = do
  putStrLn "eval/realise-context"
  let named name = StorePath (T.replicate 32 (T.take 1 name)) name
      drv = named "drv.drv"
      whole = named "other.drv"
      src = named "src"
      out = named "drv-out"
      dev = named "drv-dev"
      context = StringContext . Set.fromList
      request = contextRequest (context [SCDrvOutput drv "out", SCDrvOutput drv "dev", SCPlain src])
      outputPaths = Map.fromList [((drv, "out"), out), ((drv, "dev"), dev)]
      computed paths = Map.fromList [(path, Computed) | path <- paths]
      planned mode pending valid = planRealisation AllowImportFromDerivation mode (computed pending) (Set.fromList valid) outputPaths request
      plainRequest path = contextRequest (context [SCPlain path])
      logical = storePathToText defaultStoreDir
      origins = QC.elements [Fetched, Computed]
  sequence
    [ -- NixStringContext is a std::set of a variant ordered plain, whole
      -- derivation, output (value/context.hh at 2.24.9).
      runTest "a context is asked in upstream's order: plain paths, derivations named whole, then outputs, each by path" $
        assertEqual
          "request"
          (ContextRequest [src, whole, drv, drv] [(drv, "dev"), (drv, "out")])
          (contextRequest (context [SCDrvOutput drv "out", SCAllOutputs whole, SCDrvOutput drv "dev", SCPlain src])),
      runTest "the first path that is not valid is refused, in the request's order" $
        assertEqual "plan" (Left (PathNotValid src)) (planned ReadWrite [] []),
      runTest "a read-only session refuses a derivation evaluation computed, as upstream's read-only evaluation never writes one" $
        assertEqual "plan" (Left (PathNotValid drv)) (planned ReadOnly [drv] [src]),
      runTest "a read-only session reads what a fetcher fetched, as upstream's fetchers add it in any mode, and adds nothing" $
        assertEqual "plan" (Right (Plan False [] [] Nothing)) (planRealisation AllowImportFromDerivation ReadOnly (Map.singleton src Fetched) Set.empty Map.empty (plainRequest src)),
      runTest "a read-only session reads outputs the store holds, and adds and builds nothing" $
        assertEqual "plan" (Right (Plan False [] [dev, out] Nothing)) (planned ReadOnly [] [drv, src, out, dev]),
      runTest "a read-only session realises a derivation the store holds for its missing outputs, and adds nothing" $
        assertEqual "plan" (Right (Plan False [(drv, Set.fromList ["dev", "out"])] [dev, out] Nothing)) (planned ReadOnly [] [drv, src]),
      runTest "an output name whose path is not known is left to the realisation, which refuses one a derivation does not have" $
        assertEqual
          "plan"
          (Right (Plan False [(drv, Set.singleton "dev")] [out] Nothing))
          (planRealisation AllowImportFromDerivation ReadOnly Map.empty (Set.fromList [drv, src, out]) (Map.delete (drv, "dev") outputPaths) request),
      runTest "a read-write session counts what evaluation made as valid, adds it, and builds each derivation once for its missing outputs" $
        assertEqual "plan" (Right (Plan True [(drv, Set.fromList ["dev", "out"])] [dev, out] Nothing)) (planned ReadWrite [drv] [src]),
      runTest "a read-write session with every path valid adds and builds nothing" $
        assertEqual "plan" (Right (Plan False [] [dev, out] Nothing)) (planned ReadWrite [] [drv, src, out, dev]),
      runTest "a read-write session adds what evaluation made before a read of it, and builds nothing" $
        assertEqual "plan" (Right (Plan True [] [] Nothing)) (planRealisation AllowImportFromDerivation ReadWrite (computed [src]) Set.empty Map.empty (plainRequest src)),
      -- Carried out, a plan leaves nothing pending: what it adds and builds
      -- the store then holds.
      runTestM "a plan carried out leaves nothing to add or build" $ do
        result <-
          QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} $
            QC.forAll ((,) <$> QC.sublistOf [drv, src] <*> QC.sublistOf [drv, src, out, dev]) $ \(pending, valid) ->
              case planned ReadWrite pending valid of
                Left _ -> QC.property True
                Right plan ->
                  planRealisation AllowImportFromDerivation ReadWrite Map.empty (Set.fromList (pending <> valid <> planOutputs plan)) outputPaths request
                    QC.=== Right (Plan False [] (planOutputs plan) Nothing)
        pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result))),
      -- A read-write session adds whatever a read-only one reads, so it
      -- refuses nothing the read-only one would read.
      runTestM "whatever a read-only session can read, a read-write session can" $ do
        result <-
          QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} $
            QC.forAll ((,) . Map.fromList <$> QC.listOf ((,) <$> QC.elements [drv, src] <*> origins) <*> QC.sublistOf [drv, src, out, dev]) $ \(pending, valid) ->
              case planRealisation AllowImportFromDerivation ReadOnly pending (Set.fromList valid) outputPaths request of
                Left _ -> QC.property True
                Right _ -> QC.property (either (const False) (const True) (planRealisation AllowImportFromDerivation ReadWrite pending (Set.fromList valid) outputPaths request))
        pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result))),
      -- realiseContext refuses once every path is valid and before it
      -- builds (primops.cc), naming the first Built element of its set,
      -- so a valid output is refused too.
      runTest "with import from derivation off, a context naming an output is refused, though the output is valid" $
        assertEqual "plan" (Left (ImportFromDerivationOff drv "dev")) (planRealisation RefuseImportFromDerivation ReadWrite Map.empty (Set.fromList [drv, src, out, dev]) outputPaths request),
      runTest "with import from derivation off, a path that is not valid is refused first" $
        assertEqual "plan" (Left (PathNotValid src)) (planRealisation RefuseImportFromDerivation ReadOnly Map.empty Set.empty outputPaths request),
      runTest "with import from derivation off, a context that names no output is read" $
        assertEqual "plan" (Right (Plan False [] [] Nothing)) (planRealisation RefuseImportFromDerivation ReadOnly Map.empty (Set.singleton src) Map.empty (plainRequest src)),
      runTest "a traced import from derivation plans the warning naming its first output" $
        assertEqual
          "warning"
          (Right (Just ("built '" <> logical drv <> "^dev' during evaluation due to an import from derivation")))
          (planWarning <$> planRealisation TraceImportFromDerivation ReadWrite Map.empty (Set.fromList [drv, src, out, dev]) outputPaths request),
      runTest "a traced context that names no output plans no warning" $
        assertEqual "warning" (Right Nothing) (planWarning <$> planRealisation TraceImportFromDerivation ReadOnly Map.empty (Set.singleton src) Map.empty (plainRequest src)),
      -- The trace setting changes nothing but the warning.
      runTestM "a traced plan is the allowed one with its warning" $ do
        result <-
          QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} $
            QC.forAll ((,,) <$> QC.elements [ReadOnly, ReadWrite] <*> QC.sublistOf [drv, src] <*> QC.sublistOf [drv, src, out, dev]) $ \(mode, pending, valid) ->
              let plan imports = planRealisation imports mode (computed pending) (Set.fromList valid) outputPaths request
               in fmap (\traced -> traced {planWarning = Nothing}) (plan TraceImportFromDerivation) QC.=== plan AllowImportFromDerivation
        pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result))),
      runTest "a refusal reads as upstream's" $
        assertEqual
          "messages"
          [ "path '" <> logical drv <> "' is not valid",
            "cannot find '/somewhere', since path '" <> T.replicate 32 "d" <> "-drv.drv' is not valid",
            "the build's own words",
            "cannot build '" <> logical drv <> "^out' during evaluation because the option 'allow-import-from-derivation' is disabled",
            "cannot build '" <> logical drv <> "^out' during evaluation because the option 'allow-import-from-derivation' is disabled"
          ]
          [ contextRefusalMessage (PathNotValid drv),
            findFileRefusalMessage "/somewhere" (PathNotValid drv),
            findFileRefusalMessage "/somewhere" (RealisationFailed "the build's own words"),
            contextRefusalMessage (ImportFromDerivationOff drv "out"),
            findFileRefusalMessage "/somewhere" (ImportFromDerivationOff drv "out")
          ],
      runTest "a read-only substitution is refused in upstream's words" $
        assertEqual "message" ("cannot substitute path '" <> logical dev <> "' - no write access to the Nix store") (Realise.noWriteAccessMessage dev),
      -- isInDir (file-system.cc at 2.24.9) needs a character after the
      -- separator.
      runTest "a path is in the store only below the store directory, as upstream's isInStore asks" $
        assertEqual
          "in store"
          [False, False, False, True, True]
          (map isInStoreText ["/nix/store", "/nix/store/", "/nix/storefront/x", "/nix/store/x", logical dev]),
      runTest "an output's closure takes each path it references once, through a cycle" $
        let referencing = Map.fromList [(out, Set.singleton src), (src, Set.singleton dev), (dev, Set.singleton out)]
         in assertEqual
              "closure"
              (Right (Set.fromList [out, src, dev]))
              (runIdentity (referenceClosure (\path -> Identity (Right (Map.findWithDefault Set.empty path referencing))) [out]))
    ]

-- | The evaluator's realisation of a context over a fake store and over
-- the local store's read-only queries: what each session asks, adds and
-- builds, which builtins realise and in which order, what a read-only
-- session reads of what evaluation fetched, what is refused and in whose
-- words, and what a read-only session leaves of a store.
testRealiseContextIO :: IO [Bool]
testRealiseContextIO = do
  putStrLn "eval/realise-context-io"
  tmpBase <- getTemporaryDirectory
  environment <- getEnvironment
  let root = tmpBase </> "nova-nix-test-realise-context"
      storeDir = StoreDir (root </> "store")
      twoOutputs = "(derivation { name = \"two\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; outputs = [ \"out\" \"dev\" ]; })"
      outOfTwo = "\"${" <> twoOutputs <> "}\""
      fileUrl path = "file://" <> canonPathValue (T.pack path)
      nixString text = "\"" <> T.replace "\"" "\\\"" (T.replace "\\" "\\\\" text) <> "\""
      evalWith = evalUnder unrestrictedPolicy
      evalUnder policy store source = do
        st <- newEvalStateWith policy [] store storeDir root
        evalTracedIn st root source
      evalAt dir store source = do
        st <- newEvalStateWith unrestrictedPolicy [] store (StoreDir dir) root
        evalTracedIn st root source
      -- A read-only session's answer, the same as a read-write one's,
      -- with nothing written to its store, where the read-write one has
      -- written the object.
      computedWithoutWriting source = do
        let computing = root </> "computing-store"
            writing = root </> "writing-store"
        forceRemoveIfExists computing
        forceRemoveIfExists writing
        (ro, _) <- fakeRealiseStore (const False) Nothing
        (rw, _) <- fakeRealiseStore (const False) Nothing
        computed <- evalAt computing (readOnlyOver ro) source
        written <- evalAt writing (ReadWriteSession rw) source
        computingMade <- Dir.doesPathExist computing
        writtenThere <- case written of
          Right (VStr bytes _) | Just sp <- parseStorePath defaultStoreDir (TE.decodeUtf8 bytes) -> Dir.doesPathExist (storePathToFilePath (StoreDir writing) sp)
          _ -> pure False
        pure $ case (computed, written) of
          (Right answer, Right expected) | answer == expected, not computingMade, writtenThere -> Pass
          other -> Fail ("expected the read-write answer and no store, got: " <> T.pack (show (other, computingMade, writtenThere)))
      refusingImports = unrestrictedPolicy {epImportFromDerivation = RefuseImportFromDerivation}
      refusedImport failure =
        "cannot build '/nix/store/" `T.isPrefixOf` failureMessage failure
          && "-two.drv^out' during evaluation because the option 'allow-import-from-derivation' is disabled" `T.isSuffixOf` failureMessage failure
      refusedBeforeRealising answer asked = case answer of
        Left failure | refusedImport failure, FakeAdded `notElem` asked, null [() | FakeBuilt _ <- asked] -> Pass
        other -> Fail ("expected the refusal before any add or build, got: " <> T.pack (show (other, asked)))
      notValidDerivation failure =
        T.isPrefixOf "path '/nix/store/" (failureMessage failure) && T.isSuffixOf "-two.drv' is not valid" (failureMessage failure)
      readOnlyAnswer source = do
        (ops, _) <- fakeRealiseStore (const False) Nothing
        evalWith (readOnlyOver ops) source
      refusedRead source =
        readOnlyAnswer source <&> \case
          Left failure | notValidDerivation failure -> Pass
          other -> Fail ("expected the derivation refused before the read, got: " <> T.pack (show other))
      failsWith expected source =
        readOnlyAnswer source <&> \case
          Left failure | expected (failureMessage failure) -> Pass
          other -> Fail ("expected another failure, got: " <> T.pack (show other))
      tracedWith expected source =
        readOnlyAnswer source <&> \case
          Left failure | any expected (failureTrace failure) -> Pass
          other -> Fail ("expected another line of context, got: " <> T.pack (show other))
      readsAs expected source =
        readOnlyAnswer source <&> \case
          Right value | value == mkStr expected -> Pass
          other -> Fail ("expected " <> expected <> ", got: " <> T.pack (show other))
      addingTwo line = "while adding path '/nix/store/" `T.isPrefixOf` line && "-two'" `T.isSuffixOf` line
      readers =
        [ ("import", "import " <> outOfTwo),
          ("builtins.scopedImport", "builtins.scopedImport { } " <> outOfTwo),
          ("builtins.readFile", "builtins.readFile " <> outOfTwo),
          ("builtins.pathExists", "builtins.pathExists " <> outOfTwo),
          ("builtins.readDir", "builtins.readDir " <> outOfTwo),
          ("builtins.readFileType", "builtins.readFileType " <> outOfTwo),
          ("builtins.hashFile", "builtins.hashFile \"sha256\" " <> outOfTwo),
          ("builtins.path", "builtins.path { path = " <> outOfTwo <> "; }"),
          ("builtins.filterSource", "builtins.filterSource (p: t: true) " <> outOfTwo)
        ]
      emptyDatabaseStore = StoreDir (root </> "empty-database-store")
      stage = root </> "stage"
      tarball = root </> "fetched.tar.gz"
      fetchedFile = root </> "fetched.txt"
      repo = root </> "repo"
      gitFetch rev = "builtins.fetchGit { url = " <> nixString (T.pack repo) <> ";" <> maybe "" (\pinned -> " rev = \"" <> pinned <> "\";") rev <> " }"
      tarProcess =
        (Proc.proc "tar" ["-czf", tarball, "-C", stage, "top"])
          { Proc.env = Just (("COPYFILE_DISABLE", "1") : environment)
          }
  forceRemoveIfExists root
  createDirectoryIfMissing True (stage </> "top")
  createDirectoryIfMissing True (root </> "filtered")
  BS.writeFile (root </> "filtered" </> "child") "child\n"
  BS.writeFile (stage </> "top" </> "default.nix") "{ v = \"from a tarball\"; }\n"
  BS.writeFile fetchedFile "fetched bytes\n"
  createDirectoryIfMissing True (unStoreDir emptyDatabaseStore </> metaDirName)
  BS.writeFile (unStoreDir emptyDatabaseStore </> metaDirName </> dbFileName) ""
  revs <- makeGitFixture repo [("VERSION", "1.2.3\n")]
  readerResults <- traverse (\(reader, source) -> runTestM (reader <> " realises its argument's context before it reads") (refusedRead source)) readers
  results <-
    withEnvVar "XDG_CACHE_HOME" (root </> "cache") $
      sequence
        [ runTestM "a path value has no context, and the store is asked nothing" $ do
            (ops, calls) <- fakeRealiseStore (const False) Nothing
            answer <- evalWith (ReadWriteSession ops) "builtins.pathExists ./absent"
            asked <- calls
            pure (assertEqual "answer and calls" (Right (VBool False), []) (answer, asked)),
          runTestM "a plain path evaluation wrote is added before it is read, and nothing is built" $ do
            (ops, calls) <- fakeRealiseStore (const False) Nothing
            answer <- evalWith (ReadWriteSession ops) "builtins.readFile (builtins.toFile \"plain\" \"text\")"
            asked <- calls
            pure (assertEqual "answer and calls" (Right (mkStr "text"), [FakeAdded]) (answer, asked)),
          -- Once added, a write is the store's to answer for, so a second
          -- read asks the store instead of adding it again.
          runTestM "a write already added is asked of the store and not added again" $ do
            (ops, calls) <- fakeRealiseStore (const False) Nothing
            answer <- evalWith (ReadWriteSession ops) "let f = builtins.toFile \"twice\" \"text\"; in builtins.readFile f + builtins.readFile f"
            asked <- calls
            pure $ case (answer, asked) of
              (Right value, [FakeAdded, FakeQueried _]) | value == mkStr "texttext" -> Pass
              other -> Fail ("expected one add and then a question, got: " <> T.pack (show other)),
          runTestM "two outputs of one derivation are built in one request, after what evaluation made is added" $ do
            (ops, calls) <- fakeRealiseStore (const False) Nothing
            answer <- evalWith (ReadWriteSession ops) ("let d = " <> twoOutputs <> "; in builtins.pathExists \"${d.out}${d.dev}\"")
            asked <- calls
            pure $ case (answer, asked) of
              (Right (VBool False), [FakeQueried outputs, FakeAdded, FakeBuilt [(_, wanted)]])
                | Set.size outputs == 2, wanted == Set.fromList ["dev", "out"] -> Pass
              other -> Fail ("expected the outputs asked about, an add and one build, got: " <> T.pack (show other)),
          runTestM "a read-only session refuses a derivation evaluation computed in upstream's words, under its line of context, and adds and builds nothing" $ do
            (ops, calls) <- fakeRealiseStore (const False) Nothing
            answer <- evalWith (readOnlyOver ops) ("import " <> twoOutputs)
            asked <- calls
            pure $ case (answer, asked) of
              (Left failure, [FakeQueried _])
                | notValidDerivation failure,
                  any (T.isPrefixOf "while realising the context of path '/nix/store/") (failureTrace failure) ->
                    Pass
              other -> Fail ("expected the refusal under its line of context, got: " <> T.pack (show other)),
          runTestM "a read-only session reads an output the store holds, and adds and builds nothing" $ do
            (ops, calls) <- fakeRealiseStore (const True) Nothing
            answer <- evalWith (readOnlyOver ops) ("builtins.pathExists \"${" <> twoOutputs <> ".out}\"")
            asked <- calls
            pure $ case (answer, asked) of
              (Right (VBool False), [FakeQueried _, FakeQueried _]) -> Pass
              other -> Fail ("expected two questions and no add or build, got: " <> T.pack (show other)),
          runTestM "a read-only session realises a missing output of a derivation the store holds through its build, and adds nothing" $ do
            (ops, calls) <- fakeRealiseStore ((== "two.drv") . spName) Nothing
            answer <- evalWith (readOnlyOver ops) ("builtins.pathExists " <> outOfTwo)
            asked <- calls
            pure $ case (answer, asked) of
              (Right (VBool False), [FakeQueried _, FakeQueried _, FakeBuilt [(_, wanted)]]) | wanted == Set.singleton "out" -> Pass
              other -> Fail ("expected the output built and nothing added, got: " <> T.pack (show other)),
          -- realiseContext refuses once every path is valid and before it
          -- builds (primops.cc), whatever the session.
          runTestM "with import from derivation off, a read-write session refuses before it adds or builds, though the outputs are valid" $ do
            (ops, calls) <- fakeRealiseStore (const True) Nothing
            answer <- evalUnder refusingImports (ReadWriteSession ops) ("builtins.pathExists " <> outOfTwo)
            refusedBeforeRealising answer <$> calls,
          runTestM "with import from derivation off, a read-only session refuses an output of a derivation the store holds before it builds" $ do
            (ops, calls) <- fakeRealiseStore ((== "two.drv") . spName) Nothing
            answer <- evalUnder refusingImports (readOnlyOver ops) ("builtins.readFile " <> outOfTwo)
            refusedBeforeRealising answer <$> calls,
          -- addPath realises with isIFD at its default, true (primops.cc).
          runTestM "with import from derivation off, builtins.path refuses a path whose context names an output" $ do
            (ops, calls) <- fakeRealiseStore (const True) Nothing
            answer <- evalUnder refusingImports (ReadWriteSession ops) ("builtins.path { path = " <> outOfTwo <> "; }")
            refusedBeforeRealising answer <$> calls,
          -- tryEval catches only an AssertionError (prim_tryEval in
          -- primops.cc), and the refusal is an EvalBaseError, an
          -- IFDError at 2.34.9 (eval-error.hh).
          runTestM "builtins.tryEval does not catch the refusal of an import from derivation" $ do
            (ops, _) <- fakeRealiseStore (const True) Nothing
            answer <- evalUnder refusingImports (ReadWriteSession ops) ("builtins.tryEval (import " <> outOfTwo <> ")")
            pure $ case answer of
              Left failure | refusedImport failure -> Pass
              other -> Fail ("expected the refusal to escape tryEval, got: " <> T.pack (show other)),
          -- tryEval catches only an assertion (prim_tryEval in primops.cc).
          runTestM "builtins.tryEval does not catch a refusal" $
            refusedRead ("builtins.tryEval (import " <> twoOutputs <> ")"),
          -- prim_pathExists catches only a RestrictedPathError
          -- (primops.cc).
          runTestM "builtins.pathExists answers a failed build with the failure, not false" $ do
            (ops, _) <- fakeRealiseStore (const False) (Just (BuildStop "building two failed: exit 3: builder failed" 1))
            answer <- evalWith (ReadWriteSession ops) ("builtins.pathExists " <> outOfTwo)
            pure $ case answer of
              Left failure | failureMessage failure == "building two failed: exit 3: builder failed" -> Pass
              other -> Fail ("expected the build's failure, got: " <> T.pack (show other)),
          runTestM "builtins.findFile names the entry and the path's base name when the entry's context names a path that is not valid" $
            failsWith
              ( \message ->
                  "cannot find '/nix/store/" `T.isPrefixOf` message
                    && "-two', since path '" `T.isInfixOf` message
                    && "-two.drv' is not valid" `T.isSuffixOf` message
                    && not ("since path '/nix/store/" `T.isInfixOf` message)
              )
              ("let d = " <> twoOutputs <> "; in builtins.findFile [ { prefix = \"x\"; path = \"${d}\"; } ] \"x\""),
          -- Upstream's checks that come before realisePath or addPath
          -- (prim_hashFile, prim_filterSource and prim_path in primops.cc).
          runTestM "builtins.hashFile refuses an unknown algorithm before it realises its path" $
            failsWith ("unknown hash algorithm 'bogus'" `T.isPrefixOf`) ("builtins.hashFile \"bogus\" " <> outOfTwo),
          runTestM "builtins.filterSource forces its filter before it realises its path" $
            failsWith ("expected a function" `T.isPrefixOf`) ("builtins.filterSource 1 " <> outOfTwo),
          runTestM "builtins.path reads every argument before it realises its path" $
            failsWith (== "unsupported argument 'zzz' to 'addPath'") ("builtins.path { path = " <> outOfTwo <> "; zzz = 1; }"),
          -- prim_path in primops.cc forces the filter as a function and
          -- the name as a string with no context at their turns, and an
          -- empty name is the base name.
          runTestM "builtins.path refuses a filter that is not a function at its turn, before it realises its path" $
            failsWith ("expected a function" `T.isPrefixOf`) ("builtins.path { path = " <> outOfTwo <> "; filter = 1; }"),
          runTestM "builtins.path refuses a filter that is not a function for a flat copy too, under upstream's line of context" $
            tracedWith (== "while evaluating the `filter` parameter passed to builtins.path") "builtins.path { path = ./filtered/child; filter = 1; recursive = false; }",
          runTestM "builtins.path refuses a name that is not a string, under upstream's line of context" $
            tracedWith (== "while evaluating the `name` attribute passed to builtins.path") "builtins.path { path = ./filtered/child; name = 1; }",
          runTestM "builtins.path refuses a name with context" $
            failsWith ("the string '" `T.isPrefixOf`) "builtins.path { path = ./filtered/child; name = builtins.toFile \"n\" \"x\"; }",
          runTestM "builtins.path names a copy whose name is empty by the path's base name" $
            readOnlyAnswer "builtins.path { path = ./filtered/child; name = \"\"; }" <&> \case
              Right (VStr bytes _) | "-child" `BS.isSuffixOf` bytes -> Pass
              other -> Fail ("expected a copy named child, got: " <> T.pack (show other)),
          -- addPath's try block (primops.cc).
          runTestM "builtins.path gives a refusal of its path's context upstream's line of context" $
            tracedWith addingTwo ("builtins.path { path = " <> outOfTwo <> "; }"),
          runTestM "builtins.filterSource gives a refusal of its path's context upstream's line of context" $
            tracedWith addingTwo ("builtins.filterSource (p: t: true) " <> outOfTwo),
          runTestM "builtins.path gives a copy that fails upstream's line of context" $
            tracedWith (== ("while adding path '" <> canonPathValue (T.pack (root </> "absent")) <> "'")) "builtins.path { path = ./absent; }",
          runTestM "builtins.filterSource gives a failure in its filter upstream's line of context" $
            tracedWith (== ("while adding path '" <> canonPathValue (T.pack (root </> "filtered")) <> "'")) "builtins.filterSource (p: t: throw \"no\") ./filtered",
          -- Upstream's fetchers add what they fetch in any mode
          -- (fetch in fetchTree.cc, downloadFile in tarball.cc and
          -- Input::fetchToStore in fetchers.cc at 2.24.9), so a read-only
          -- evaluation reads it through its context.
          if SI.os == "mingw32"
            then runTest "a read-only session imports a fetchTarball result through its context (skipped: fetchTarball extracts through sh, which nova-nix does not assume on Windows)" Pass
            else runTestM "a read-only session imports a fetchTarball result through its context" $ do
              (tarCode, _, tarErr) <- Proc.readCreateProcessWithExitCode tarProcess ""
              case tarCode of
                ExitFailure n -> pure (Fail ("tar exited " <> T.pack (show n) <> ": " <> T.pack tarErr))
                ExitSuccess -> readsAs "from a tarball" ("(import \"${builtins.fetchTarball " <> nixString (fileUrl tarball) <> "}/default.nix\").v"),
          -- nix-instantiate --eval computes these paths without writing
          -- them (prim_toFile and addPath in primops.cc).
          runTestM "a read-only session computes the path of a toFile result and writes nothing" $
            computedWithoutWriting "builtins.toFile \"named\" \"text\"",
          runTestM "a read-only session computes the path of a builtins.path copy by its NAR and writes nothing" $
            computedWithoutWriting "builtins.path { path = ./filtered; }",
          runTestM "a read-only session computes the path of a flat builtins.path copy and writes nothing" $
            computedWithoutWriting "builtins.path { path = ./filtered/child; recursive = false; }",
          runTestM "a read-only session computes the path of a filtered copy and writes nothing" $
            computedWithoutWriting "builtins.filterSource (p: t: true) ./filtered",
          -- Upstream's fetchers add what they fetch in any mode (fetch in
          -- fetchTree.cc at 2.24.9).
          runTestM "a read-only session writes what a fetcher fetches" $ do
            let dir = root </> "fetching-store"
            (ops, _) <- fakeRealiseStore (const False) Nothing
            answer <- evalAt dir (readOnlyOver ops) ("builtins.fetchurl " <> nixString (fileUrl fetchedFile))
            case answer of
              Right val | Just sp <- fetchedStorePath val -> do
                there <- Dir.doesPathExist (storePathToFilePath (StoreDir dir) sp)
                pure (if there then Pass else Fail ("the fetch was not written to " <> storePathToText defaultStoreDir sp))
              other -> pure (Fail ("expected the fetched path, got: " <> T.pack (show other))),
          -- fetchTree.cc's fetch: the URLs name no file, so only the early
          -- exit answers.
          runTestM "a pinned fetchurl the store holds returns its path without downloading" $ do
            let pin = sha256Digest "pinned\n"
            held <- either (fail . show) pure (makeFixedOutputPath "held-file" "sha256" FlatIngestion pin)
            (ops, _) <- fakeRealiseStore (== held) Nothing
            answer <- evalWith (readOnlyOver ops) ("builtins.fetchurl { url = " <> nixString (fileUrl (root </> "absent" </> "held-file")) <> "; sha256 = \"" <> Hash.bytesToHexText pin <> "\"; }")
            pure (assertEqual "answer" (Right (storePathStringValue held)) (either (Left . failureMessage) Right answer)),
          runTestM "a pinned fetchTarball the store holds returns its path without downloading" $ do
            let pin = sha256Digest "a tree's NAR"
            held <- either (fail . show) pure (makeFixedOutputPath "source" "sha256" NarIngestion pin)
            (ops, _) <- fakeRealiseStore (== held) Nothing
            answer <- evalWith (readOnlyOver ops) ("builtins.fetchTarball { url = " <> nixString (fileUrl (root </> "absent" </> "held.tar.gz")) <> "; sha256 = \"" <> Hash.bytesToHexText pin <> "\"; }")
            pure (assertEqual "answer" (Right (storePathStringValue held)) (either (Left . failureMessage) Right answer)),
          -- A flat builtins.path copy of the same bytes under the fetch's
          -- name lands at the path the pin names, computed and unwritten
          -- in a read-only session, so only a read-write one holds it and
          -- the read-only one downloads from a URL that names nothing.
          runTestM "a path evaluation computed holds a pinned fetch in a read-write session and not in a read-only one" $ do
            let pin = sha256Digest "child\n"
                source =
                  "let copied = builtins.path { path = ./filtered/child; name = \"held-copy\"; recursive = false; }; in builtins.seq copied (builtins.fetchurl { url = "
                    <> nixString (fileUrl (root </> "absent" </> "held-copy"))
                    <> "; sha256 = \""
                    <> Hash.bytesToHexText pin
                    <> "\"; })"
            expected <- either (fail . show) pure (makeFixedOutputPath "held-copy" "sha256" FlatIngestion pin)
            (ro, _) <- fakeRealiseStore (const False) Nothing
            (rw, _) <- fakeRealiseStore (const False) Nothing
            readOnly <- evalWith (readOnlyOver ro) source
            readWrite <- evalWith (ReadWriteSession rw) source
            pure $ case (readOnly, readWrite) of
              (Left _, Right val) | fetchedStorePath val == Just expected -> Pass
              other -> Fail ("expected a download in the read-only session and the computed copy in the read-write one, got: " <> T.pack (show other)),
          runTestM "a read-only session reads a fetchurl result through its context" $
            readsAs "fetched bytes\n" ("builtins.readFile \"${builtins.fetchurl " <> nixString (fileUrl fetchedFile) <> "}\""),
          runTestM "a read-only session reads a fetchGit result through its context" $
            readsAs "1.2.3\n" ("builtins.readFile \"${" <> gitFetch Nothing <> "}/VERSION\""),
          -- The first session records the pinned fetch, and the second
          -- adopts the tree the record names: the repository is gone by
          -- then, so nothing is fetched again.
          runTestM "a read-only session reads a fetchGit result adopted from the fetch cache" $ do
            let pinned = gitFetch (listToMaybe revs)
                readVersion = "builtins.readFile \"${" <> pinned <> "}/VERSION\""
            fetched <- readsAs "1.2.3\n" readVersion
            Dir.renameDirectory repo (repo <> "-moved")
            adopted <- readsAs "1.2.3\n" readVersion
            pure (fetched `andThen` adopted),
          -- What a fetcher fetched is readable whatever the store says, so a
          -- read-only session does not ask.
          runTestM "a read-only session adds what a fetcher fetched as it lands, and asks the store nothing about it" $ do
            (ops, calls) <- fakeRealiseStore (const False) Nothing
            answer <- evalWith (readOnlyOver ops) ("builtins.readFile \"${builtins.fetchurl " <> nixString (fileUrl fetchedFile) <> "}\"")
            asked <- calls
            pure (assertEqual "answer and calls" (Right (mkStr "fetched bytes\n"), [FakeAdded]) (answer, asked)),
          runTestM "a read-only session over a store that does not exist refuses, and makes no directory, metadata, database or lock" $ do
            let absent = StoreDir (root </> "absent-store")
            st <- newEvalStateWith unrestrictedPolicy [] (ReadOnlySession (localQueries absent) unexpectedOps) absent root
            answer <- evalTracedIn st root ("import " <> twoOutputs)
            made <- Dir.doesPathExist (unStoreDir absent)
            pure $ case answer of
              Left failure | notValidDerivation failure, not made -> Pass
              other -> Fail ("expected the refusal and no store, got: " <> T.pack (show (other, made))),
          -- A read-only connection writes no page of the database; SQLite
          -- makes only the wal-index a WAL database needs beside it.
          runTestM "a read-only query answers from the store's database, leaves it as it was, and creates nothing beside it but SQLite's wal-index" $ do
            let held = StorePath (T.replicate 32 "h") "held"
                unheld = StorePath (T.replicate 32 "u") "unheld"
                dir = StoreDir (root </> "queried-store")
                metadata = unStoreDir dir </> metaDirName
                walIndex = Set.fromList [dbFileName <> "-wal", dbFileName <> "-shm"]
            bracket (openStore dir) closeStore $ \store -> do
              createDirectoryIfMissing True (storePathToFilePath (stDir store) held)
              registrationFor store held Nothing [] >>= registerPath (stDB store)
            before <- Set.fromList <$> Dir.listDirectory metadata
            database <- BS.readFile (metadata </> dbFileName)
            valid <- sqValidPaths (localQueries dir) (Set.fromList [held, unheld])
            references <- sqReferences (localQueries dir) held
            after <- Set.fromList <$> Dir.listDirectory metadata
            databaseAfter <- BS.readFile (metadata </> dbFileName)
            pure
              ( assertEqual "answers" (Set.singleton held, Right Set.empty) (valid, references)
                  `andThen` assertEqual "database" database databaseAfter
                  `andThen` assertEqual "beside it" Set.empty ((after `Set.difference` before) `Set.difference` walIndex)
              ),
          -- An empty database, as a store whose first open was cut short
          -- leaves, holds none of the store's tables: a connection that
          -- made them would fill it, and one that asked them would fail.
          runTestM "a read-only query of a database without the store's tables answers that it holds nothing, and writes nothing to it" $ do
            let held = StorePath (T.replicate 32 "h") "held"
            outcome <- try ((,) <$> sqValidPaths (localQueries emptyDatabaseStore) (Set.singleton held) <*> sqReferences (localQueries emptyDatabaseStore) held)
            bytes <- BS.readFile (unStoreDir emptyDatabaseStore </> metaDirName </> dbFileName)
            pure $ case (outcome :: Either SQL.SQLError (Set.Set StorePath, Either Text (Set.Set StorePath)), bytes) of
              (Right (valid, Right references), "") | Set.null valid, Set.null references -> Pass
              other -> Fail ("expected nothing held and the database still empty, got: " <> T.pack (show other)),
          runTestM "a read-only session over a database without the store's tables refuses a derivation in upstream's words" $ do
            st <- newEvalStateWith unrestrictedPolicy [] (ReadOnlySession (localQueries emptyDatabaseStore) unexpectedOps) emptyDatabaseStore root
            outcome <- try (evalTracedIn st root ("import " <> twoOutputs))
            pure $ case outcome :: Either SQL.SQLError (Either EvalFailure NixValue) of
              Right (Left failure) | notValidDerivation failure -> Pass
              other -> Fail ("expected the refusal, got: " <> T.pack (show other)),
          -- nova-nix holds no rows in a store upstream manages, so a tree
          -- there counts as valid (the design's Decided list).
          runTestM "a read-only query of a store upstream's database manages counts a tree that exists as valid, and opens no database there" $ do
            let upstreamRoot = root </> "upstream"
                planted = StoreDir (upstreamRoot </> "nix" </> "store")
                tree = StorePath (T.replicate 32 "t") "tree"
                missing = StorePath (T.replicate 32 "m") "missing"
                upstreamDb = upstreamRoot </> "nix" </> "var" </> "nix" </> "db" </> "db.sqlite"
            createDirectoryIfMissing True (takeDirectory upstreamDb)
            BS.writeFile upstreamDb ""
            createDirectoryIfMissing True (storePathToFilePath planted tree)
            valid <- sqValidPaths (localQueries planted) (Set.fromList [tree, missing])
            opened <- Dir.doesPathExist (unStoreDir planted </> metaDirName)
            pure (assertEqual "valid and opened" (Set.singleton tree, False) (valid, opened))
        ]
  forceRemoveIfExists root
  pure (readerResults <> results)

-- | Upstream's @autoCallFunction@ (eval.cc at 2.24.9) over the pure
-- evaluator, each case a set of @fun@, the value called, and @args@,
-- whose attributes are the arguments, unforced; and the targets and
-- arguments the command line names, as the parser reads them.
testTargetArgs :: IO [Bool]
testTargetArgs = do
  putStrLn "eval/auto-call"
  let autoCalled source = case evalNix source of
        Left err -> Left err
        Right (VAttrs attrs) -> runPureEval $ do
          fun <- attrSetLookup "fun" attrs >>= maybe (throwEvalError "no fun") force
          args <-
            attrSetLookup "args" attrs >>= maybe (throwEvalError "no args") force >>= \case
              VAttrs argSet -> attrSetToMap argSet
              _ -> throwEvalError "args is not a set"
          autoCallFunction args fun
        Right other -> Left ("not a set: " <> T.pack (show other))
      missingMessage name =
        T.intercalate
          "\n"
          [ "cannot evaluate a function that has an argument without a value ('" <> name <> "')",
            "Nix attempted to evaluate a function as a top level expression; in",
            "this case it must have its arguments supplied either by default",
            "values, or passed explicitly with '--arg' or '--argstr'. See",
            "https://nixos.org/manual/nix/stable/language/constructs.html#functions."
          ]
      parsed = fmap Cli.optCommand . Cli.parseArgs
      autoArgsOf = fmap Cli.optAutoArgs . Cli.parseArgs
  sequence
    [ runTest "a function is called with the arguments its formals name, its defaults filling the rest" $
        assertEqual "defaults" (Right (VInt 3)) (autoCalled "{ fun = { x, y ? 2 }: x + y; args = { x = 1; }; }"),
      runTest "an argument a function without an ellipsis does not take is not passed" $
        assertEqual "extra" (Right (VInt 1)) (autoCalled "{ fun = { x }: x; args = { x = 1; z = throw \"never\"; }; }"),
      runTest "a function with an ellipsis is passed every argument" $
        assertEqual "ellipsis" (Right (VInt 5)) (autoCalled "{ fun = { x, ... }@given: given.z; args = { x = 1; z = 5; }; }"),
      runTest "an argument the function does not use is not evaluated" $
        assertEqual "lazy" (Right (VInt 1)) (autoCalled "{ fun = { x, y }: x; args = { x = 1; y = throw \"never\"; }; }"),
      runTest "a formal with neither an argument nor a default fails with upstream's message" $
        assertEqual "missing" (Left (missingMessage "y")) (autoCalled "{ fun = { x, y }: x; args = { x = 1; }; }"),
      runTest "a set with __functor is called as the function its functor gives" $
        assertEqual "functor" (Right (VInt 11)) (autoCalled "{ fun = { k = 10; __functor = self: { x }: x + self.k; }; args = { x = 1; }; }"),
      runTest "a function without a set pattern, and a value that is no function, are left as they are" $
        case (autoCalled "{ fun = x: x; args = { x = 1; }; }", autoCalled "{ fun = 7; args = { x = 1; }; }") of
          (Right (VLambda {}), Right (VInt 7)) -> Pass
          other -> Fail (T.pack (show other)),
      runTest "an argument in angle brackets is a search path lookup, as upstream's lookupFileArg reads one" $
        assertEqual "build" (Right (Cli.CmdBuild (Cli.TargetSearchPath "nixpkgs") (Just "hello") (Cli.OutLinkAt "result"))) (parsed ["build", "<nixpkgs>", "-A", "hello"])
          `andThen` assertEqual "eval" (Right (Cli.CmdEval (Cli.TargetSearchPath "nixpkgs/lib"))) (parsed ["eval", "<nixpkgs/lib>"])
          `andThen` assertEqual "trust" (Right (Cli.CmdTrust (Cli.TargetSearchPath "x") Nothing)) (parsed ["trust", "<x>"])
          `andThen` assertEqual "two characters" (Right (Cli.CmdEval (Cli.TargetFile "<>"))) (parsed ["eval", "<>"])
          `andThen` assertEqual "unclosed" (Right (Cli.CmdEval (Cli.TargetFile "<x"))) (parsed ["eval", "<x"])
          `andThen` assertEqual "a path" (Right (Cli.CmdEval (Cli.TargetFile "./dir"))) (parsed ["eval", "./dir"]),
      runTest "--arg and --argstr name a function's arguments before or after the target, a later one for a name replacing an earlier" $
        assertEqual "build" (Right (Map.fromList [("s", Cli.AutoArgString "hi"), ("x", Cli.AutoArgExpr "1")])) (autoArgsOf ["build", "--arg", "x", "1", "f.nix", "--argstr", "s", "hi"])
          `andThen` assertEqual "eval, after the file" (Right (Map.fromList [("x", Cli.AutoArgExpr "2")])) (autoArgsOf ["eval", "f.nix", "--arg", "x", "2"])
          `andThen` assertEqual "eval, before it" (Right (Map.fromList [("x", Cli.AutoArgString "a")])) (autoArgsOf ["eval", "--argstr", "x", "a", "--expr", "1"])
          `andThen` assertEqual "trust" (Right (Map.fromList [("x", Cli.AutoArgExpr "1")])) (autoArgsOf ["trust", "--expr", "1", "--arg", "x", "1"])
          `andThen` assertEqual "replaced" (Right (Map.fromList [("x", Cli.AutoArgString "two")])) (autoArgsOf ["build", "--arg", "x", "1", "--argstr", "x", "two", "--expr", "1"]),
      runTest "--arg and --argstr without both a name and a value are refused" $
        assertEqual "none" (Left "--arg requires a name and a value") (parsed ["build", "--expr", "1", "--arg"])
          `andThen` assertEqual "one" (Left "--argstr requires a name and a value") (parsed ["eval", "--argstr", "x"])
          `andThen` assertEqual "after eval's target" (Left "--arg requires a name and a value") (parsed ["eval", "f.nix", "--arg", "x"])
    ]

-- ---------------------------------------------------------------------------
-- Tests: Phase 4 - search paths, dynamic keys, directory import
-- ---------------------------------------------------------------------------

testPhase4 :: IO [Bool]
testPhase4 = do
  putStrLn "phase4/search-paths"
  sequence
    [ -- parseNixPath tests
      runTestM "parseNixPath empty" $
        assertEqual "empty" [] <$> parseNixPath "",
      runTestM "parseNixPath single" $ do
        entries <- searchPathEntries "nixpkgs=/home/user/nixpkgs"
        pure $ case entries of
          [(Just (VStr "nixpkgs" _), Just (VStr "/home/user/nixpkgs" _))] -> Pass
          [_] -> Fail "wrong prefix/path"
          _ -> Fail ("expected one entry, got " <> T.pack (show (length entries))),
      runTestM "parseNixPath multiple" $
        assertEqual "count" 2 . length <$> parseNixPath "nixpkgs=/nix:custom=/opt",
      runTestM "parseNixPath channel entry" $ do
        entries <- searchPathEntries "nixpkgs=channel:nixos-24.11"
        pure $ case entries of
          [(Just (VStr "nixpkgs" _), Just (VStr "channel:nixos-24.11" _))] -> Pass
          [_] -> Fail "wrong prefix/path for channel"
          _ -> Fail ("expected one entry, got " <> T.pack (show (length entries))),
      runTest "splitNixPath drive letters and separators" $
        assertEqual
          "split"
          ["nixpkgs=C:\\nixpkgs", "custom=/opt/custom", "C:/x"]
          (splitNixPath "nixpkgs=C:\\nixpkgs:custom=/opt/custom:C:/x"),
      runTest "splitNixPath splits a Unix-style absolute path list" $
        assertEqual "split-unix" ["/foo", "/bar"] (splitNixPath "/foo:/bar"),
      runTest "splitNixPath splits a Unix entry after a drive entry" $
        assertEqual "split-mixed" ["C:\\a", "/foo"] (splitNixPath "C:\\a:/foo"),
      runTest "splitNixPath keeps a URL entry whole" $
        assertEqual
          "split-url"
          ["nixpkgs=https://example.com/nixpkgs.tar.gz", "custom=/opt"]
          (splitNixPath "nixpkgs=https://example.com/nixpkgs.tar.gz:custom=/opt"),
      runTest "splitNixPath keeps a channel entry whole" $
        assertEqual
          "split-channel"
          ["channel:nixos-24.11", "/foo"]
          (splitNixPath "channel:nixos-24.11:/foo"),
      runTest "splitNixPath keeps a prefixed channel entry whole" $
        assertEqual
          "split-channel-prefixed"
          ["nixpkgs=channel:nixos-24.11", "/foo"]
          (splitNixPath "nixpkgs=channel:nixos-24.11:/foo"),
      runTest "splitNixPath keeps a flake entry whole" $
        assertEqual "split-flake" ["flake:nixpkgs", "/foo"] (splitNixPath "flake:nixpkgs:/foo"),
      runTest "splitNixPath keeps every listed scheme whole" $
        assertEqual
          "split-schemes"
          ["http://h/a", "https://h/b", "file:///c", "channel://d", "git://h/e", "s3://b/f", "ssh://h/g"]
          (splitNixPath "http://h/a:https://h/b:file:///c:channel://d:git://h/e:s3://b/f:ssh://h/g"),
      runTest "splitNixPath splits an unlisted scheme at its colon" $
        assertEqual "split-unlisted" ["foo", "//x", "/bar"] (splitNixPath "foo://x:/bar"),
      runTest "splitNixPath drops empty entries" $
        assertEqual "split-empties" ["a", "b"] (splitNixPath "a::b:"),
      runTest "splitNixPath drops leading empty entries" $
        assertEqual "split-leading-empties" ["x"] (splitNixPath "::x"),
      runTest "splitNixPath matches a scheme case-sensitively" $
        assertEqual "split-scheme-case" ["HTTP", "//x", "/y"] (splitNixPath "HTTP://x:/y"),
      runTest "splitNixPath judges the scheme after the last = of the entry" $
        assertEqual "split-last-equals" ["a=b=https://x", "/y"] (splitNixPath "a=b=https://x:/y"),
      runTest "splitNixPath splits a URL port after the host" $
        assertEqual "split-port" ["https://example.com", "8080/x"] (splitNixPath "https://example.com:8080/x"),
      runTest "splitNixPath absorbs one run after a channel entry" $
        assertEqual "split-one-run" ["channel:channel", "x"] (splitNixPath "channel:channel:x"),
      runTest "splitNixPath keeps a bare channel colon" $
        assertEqual "split-channel-bare" ["channel:", "foo"] (splitNixPath "channel::foo"),
      runTest "isNixPathPseudoUrl: channel, flake and the listed schemes are fetched" $
        assertEqual
          "pseudo"
          [True, True, True, True, True]
          (map isNixPathPseudoUrl ["channel:nixos-24.11", "channel:", "flake:nixpkgs", "https://example.com/x.tar.gz", "file:///c"]),
      runTest "isNixPathPseudoUrl: a path, an unlisted scheme, a drive and a bare word are not" $
        assertEqual "not-pseudo" [False, False, False, False] (map isNixPathPseudoUrl ["/foo", "foo://x", "C:/x", "channel"]),
      runTest "splitNixPath drops a trailing empty entry" $
        assertEqual "split-trail" ["a"] (splitNixPath "a:"),
      runTestM "splitNixPath long entry splits in linear time" $ do
        let entry = TE.encodeUtf8 (T.replicate 2000000 "p")
            split = splitNixPath ("first:" <> entry) == ["first", entry]
        outcome <- timeout walkWatchdogMicros (evaluate split)
        pure $ case outcome of
          Just True -> Pass
          Just False -> Fail "wrong split for a long entry"
          Nothing -> Fail "split did not return promptly",
      runTestM "parseNixPath plain path" $ do
        entries <- searchPathEntries "/some/path"
        pure $ case entries of
          [(Just (VStr "" _), Just (VStr "/some/path" _))] -> Pass
          [_] -> Fail "wrong prefix/path for plain"
          _ -> Fail "expected one entry",
      -- A search path parses as the call __findFile __nixPath "name", as
      -- upstream's parser makes it.
      runTest "parse <nixpkgs>" $
        assertParse "search path" "<nixpkgs>" (EApp (EApp (EVar "__findFile") (EVar "__nixPath")) (EStr [StrLit "nixpkgs"])),
      runTest "parse <nixpkgs/lib>" $
        assertParse "search path with subpath" "<nixpkgs/lib>" (EApp (EApp (EVar "__findFile") (EVar "__nixPath")) (EStr [StrLit "nixpkgs/lib"])),
      -- The test evaluator has no search path, so the lookup misses.
      runTest "eval <nixpkgs> fails without path" $
        assertEvalFail "search path not found" "<nixpkgs>",
      -- Upstream's parser makes __findFile a variable of the call, so a
      -- binding of it in scope is the function a search path calls.
      runTest "a search path calls the __findFile in scope" $
        assertEval "findFile in scope" "let __findFile = path: name: name; in <foo>" (mkStr "foo"),
      -- Dynamic attribute keys.  A key of a plain string is a name, so
      -- each key here that is not a variable concatenates to stay dynamic.
      runTest "dynamic key basic" $
        assertEval "dynamic key" "{ ${\"hello\" + \"\"} = 42; }.hello" (VInt 42),
      runTest "dynamic key from let" $
        assertEval "dynamic key let" "let name = \"x\"; in { ${name} = 1; }.x" (VInt 1),
      runTest "dynamic key in select" $
        assertEval "dynamic key select" "let s = { x = 10; }; in s.${\"x\" + \"\"}" (VInt 10),
      runTest "dynamic key in hasAttr" $
        assertEval "dynamic key hasAttr" "let s = { x = 10; }; in s ? ${\"x\" + \"\"}" (VBool True),
      runTest "dynamic key hasAttr missing" $
        assertEval "dynamic key hasAttr missing" "let s = { x = 10; }; in s ? ${\"y\" + \"\"}" (VBool False),
      -- has-attr checks presence of the terminal attribute without
      -- forcing its value, matching upstream laziness.
      runTest "hasAttr does not force the final attribute" $
        assertEval "hasattr-lazy" "{ a = builtins.throw \"boom\"; } ? a" (VBool True),
      runTest "hasAttr path does not force the terminal attribute" $
        assertEval "hasattr-path-lazy" "{ a = { b = builtins.throw \"boom\"; }; } ? a.b" (VBool True),
      -- A null dynamic key in select or has-attr is a type error, as
      -- upstream; it is not treated as an absent attribute.
      runTest "null dynamic select key is a type error" $
        assertEvalFail "sel-null-key" "({ x = 1; }).${null}",
      runTest "null dynamic hasAttr key is a type error" $
        assertEvalFail "has-null-key" "{ x = 1; } ? ${null}",
      -- inherit inside a rec set with a dynamic key resolves against the
      -- enclosing scope, never the rec set's own binding.
      runTest "inherit in a dynamic-keyed rec set resolves outward" $
        assertEval "dyn-rec-inherit" "let v = 42; in (rec { ${\"d\" + \"\"} = 1; inherit v; }).v" (VInt 42),
      -- Dynamic attr inside string interpolation (the ${name} must not
      -- prematurely close the outer interpolation)
      runTest "dynamic key in string interp" $
        assertEval "dynamic key interp" "let s = { x = 10; }; in \"${toString s.${\"x\"}}\"" (VStr "10" mempty)
    ]

testPhase4IO :: IO [Bool]
testPhase4IO = do
  putStrLn "phase4/directory-import"
  tmpDir <- getTemporaryDirectory
  let testDir = tmpDir </> "nova-nix-phase4-test"
      subDir = testDir </> "mypkg"
      defaultNix = subDir </> "default.nix"
  -- Create temp directory structure
  createDirectoryIfMissing True subDir
  TIO.writeFile defaultNix "42"
  results <-
    sequence
      [ -- Directory import: import ./dir resolves to ./dir/default.nix
        runTestM "import directory" $ do
          result <- evalNixIO testDir ("import " <> T.pack "./mypkg")
          pure $ case result of
            Right (VInt 42) -> Pass
            Right other -> Fail ("expected VInt 42, got " <> T.pack (show other))
            Left err -> Fail ("eval error: " <> err),
        -- Search path with --nix-path equivalent (populated nixPath)
        runTestM "search path with populated nixPath" $ do
          nixPaths <- parseNixPath (TE.encodeUtf8 ("mypkg=" <> T.pack subDir))
          st <- newEvalStateWith unrestrictedPolicy nixPaths (ReadOnlySession (localQueries platformStoreDir) unexpectedOps) platformStoreDir testDir
          result <- runEvalIO st (eval (evalBaseEnv st) (EApp (EApp (EVar "__findFile") (EVar "__nixPath")) (EStr [StrLit "mypkg"])))
          pure $ case result of
            Right (VPath _) -> Pass
            Right other -> Fail ("expected VPath, got " <> T.pack (show other))
            Left err -> Fail ("eval error: " <> err)
      ]
  -- Cleanup
  removeDirectoryRecursive testDir
  pure results

-- | @builtins.toJSON@ of a path uses copy-to-store coercion (upstream
-- value-to-json.cc serializes paths with @copyToStore = true@): the JSON
-- text is the quoted source store path and the result carries its
-- context.  EvalIO-only: PureEval has no store-path computation.
testToJSONPathIO :: IO [Bool]
testToJSONPathIO = do
  putStrLn "eval/tojson-path-io"
  tmpDir <- getTemporaryDirectory
  let testDir = tmpDir </> "nova-nix-tojson-path-test"
  createDirectoryIfMissing True testDir
  TIO.writeFile (testDir </> "data.txt") "payload\n"
  results <-
    sequence
      [ runTestM "toJSON of a path is its quoted store path with context" $ do
          result <- evalNixIO testDir "builtins.toJSON ./data.txt"
          pure $ case result of
            Right (VStr json ctx) ->
              if "\"/nix/store/" `BS.isPrefixOf` json
                && "-data.txt\"" `BS.isSuffixOf` json
                && ctx /= emptyContext
                then Pass
                else Fail ("expected a quoted store path with context, got " <> bytesText json)
            Right other -> Fail ("expected VStr, got " <> T.pack (show other))
            Left err -> Fail ("eval error: " <> err)
      ]
  removeDirectoryRecursive testDir
  pure results

-- ---------------------------------------------------------------------------
-- Arena lifecycle guard (C FFI)
-- ---------------------------------------------------------------------------

-- | @builtins.storePath@ resolves a path's symlinks unless the path is
-- itself a store path (primops.cc at 2.24.9), so a link outside the store
-- into it names its target.  The links dangle on a runner with no store,
-- which upstream resolves the same way; Windows needs symlink privilege.
testStorePathLinksIO :: IO [Bool]
testStorePathLinksIO = do
  putStrLn "eval/storepath-links-io"
  canLink <- symlinksAvailable
  if not canLink || SI.os == "mingw32"
    then [] <$ putStrLn "  SKIP  needs POSIX symlinks"
    else do
      tmpBase <- Dir.canonicalizePath =<< getTemporaryDirectory
      let dir = tmpBase </> "nova-nix-test-storepath-links"
          target = "/nix/store/00000000000000000000000000000000-x"
          through name = "builtins.storePath (/. + " <> nixQuotedPath (dir </> name) <> ")"
      Dir.removePathForcibly dir
      createDirectoryIfMissing True dir
      Dir.createFileLink target (dir </> "path")
      Dir.createFileLink (target <> "/bin") (dir </> "sub")
      results <-
        sequence
          [ runTestIO "a link into the store names the store path" dir (through "path") (VStr (TE.encodeUtf8 (T.pack target)) (Context.plainContext (StorePath "00000000000000000000000000000000" "x"))),
            runTestIO "a link to a subpath names the subpath" dir (through "sub") (VStr (TE.encodeUtf8 (T.pack (target <> "/bin"))) (Context.plainContext (StorePath "00000000000000000000000000000000" "x")))
          ]
      Dir.removePathForcibly dir
      pure results

-- | A derivation attribute carrying another's drvPath brings that .drv's
-- whole closure into inputSrcs and inputDrvs, as upstream's
-- derivationStrict does (primops.cc at 2.24.9).  Each drvPath is
-- nix-instantiate 2.24.9's.
testDeepContextClosureIO :: IO [Bool]
testDeepContextClosureIO = do
  putStrLn "eval/deep-context-closure-io"
  tmpBase <- getTemporaryDirectory
  let plain name rest = "derivation { name = \"" <> name <> "\"; system = \"x\"; builder = \"/bin/sh\"; " <> rest <> "}"
      drvPathOf bindings body = "builtins.unsafeDiscardStringContext (let " <> bindings <> " in " <> body <> ").drvPath"
      a = "a = " <> plain "a" "" <> ";"
      m = "m = " <> plain "m" "dep = a; " <> ";"
      t = "t = " <> plain "t" "dep = m; " <> ";"
  sequence
    [ runTestIO "a drvPath brings its own .drv into inputSrcs" tmpBase (drvPathOf a (plain "b" "x = \"${a.drvPath}\"; ")) (mkStr "/nix/store/r3pz4ykyxg5s9f08fafcksw6bxfds40r-b.drv"),
      runTestIO "a drvPath brings the .drv files it depends on" tmpBase (drvPathOf (a <> " " <> m) (plain "b" "x = \"${m.drvPath}\"; ")) (mkStr "/nix/store/00pfjcd57v5dd5j4w4zmsp3s8lcpv6wg-b.drv"),
      runTestIO "the closure is followed to its end" tmpBase (drvPathOf (a <> " " <> m <> " " <> t) (plain "b" "x = \"${t.drvPath}\"; ")) (mkStr "/nix/store/f480ldqci72g5miaywmhljjlw1fg1y4i-b.drv")
    ]

-- | Run as a child, parse one source and compile it as an evaluation
-- compiles what it parses, then print each name the compile added to the
-- symbol table, one a line, in the order it added them.  Symbols are
-- numbered from 1 in that order (cbits/nn_symbol.c), and the child's
-- table holds only the startup names its session interned as it started.
internOrderChildFlag :: String
internOrderChildFlag = "--intern-order-child"

internOrderChild :: String -> IO ()
internOrderChild source = case parseNix testBaseDir testOrigin (TE.encodeUtf8 (T.pack source)) of
  Left err -> putStrLn ("error: " <> show err)
  Right parsed -> do
    before <- symbolCount
    _ <- compileParsed parsed
    after <- symbolCount
    mapM_ (TIO.putStrLn . symbolText . Symbol) [before + 1 .. after]

-- | Compile the source in a child process running 'internOrderChild' and
-- compare the names it interned, in order, with the expected ones.
internsInOrder :: Text -> Text -> [Text] -> IO Bool
internsInOrder label source expected = runTestM label $ do
  self <- getExecutablePath
  (_, out, _) <- Proc.readCreateProcessWithExitCode (Proc.proc self [internOrderChildFlag, T.unpack source]) ""
  pure (assertEqual label expected (T.lines (T.pack out)))

-- | Run as a child, parse one source as the file 'namesOnlyFile', build
-- the builtins once so the names interned at startup are in the table,
-- then evaluate the source as an evaluation does, and print each symbol
-- the evaluation added, one a line, in the order it added them, and the
-- failure if it failed.
namesOnlyChildFlag :: String
namesOnlyChildFlag = "--names-only-child"

-- | The flag that runs 'arenaCycleChild'.
arenaCycleChildFlag :: String
arenaCycleChildFlag = "--arena-cycle-child"

-- | Evaluate one program in two arena windows, one after the other, each
-- in a session of its own, and print each result: the second can read
-- nothing the first's teardown freed only if no constant of the program
-- points into the arena.  The program reaches every value the evaluator
-- shares: the @derivation@ wrapper, an empty list, builtins under their
-- own names and in @builtins@, and an imported file's value.
arenaCycleChild :: IO ()
arenaCycleChild = do
  mapM_ (const (inTestSession cycleOnce)) [1 :: Int, 2]
  where
    cycleOnce = do
      storeDir <- evalNixIOStoreDir
      tmpBase <- getTemporaryDirectory
      result <- evalNixIOStore storeDir tmpBase arenaCycleProgram
      putStrLn (either (("error: " <>) . T.unpack) show result)

-- | The program 'arenaCycleChild' evaluates.
arenaCycleProgram :: Text
arenaCycleProgram =
  "builtins.toJSON { drv = (derivation { name = \"cycle\"; builder = \"/bin/sh\"; system = \"x86_64-linux\"; }).drvPath; "
    <> "empty = [ ] ++ [ ]; mapped = map toString [ 1 2 ]; listed = builtins.map (x: x) [ 3 ]; "
    <> "imported = import (builtins.toFile \"cycle.nix\" \"{ a = 1; }\"); }"

-- | Where 'namesOnlyChild' says its source comes from: a position records
-- its file, which upstream keeps out of its symbol table.
namesOnlyFile :: Text
namesOnlyFile = "/nova-nix-test/znames-only.nix"

namesOnlyChild :: String -> IO ()
namesOnlyChild source = case parseNix testBaseDir (SourceFile namesOnlyFile) (TE.encodeUtf8 (T.pack source)) of
  Left err -> putStrLn ("error: " <> show err)
  Right parsed -> do
    storeDir <- evalNixIOStoreDir
    st <- testEvalState storeDir testBaseDir
    before <- symbolCount
    result <- runEvalIO st (evalParsed (pure (evalBaseEnv st)) parsed)
    after <- symbolCount
    mapM_ (TIO.putStrLn . symbolText . Symbol) [before + 1 .. after]
    either (\err -> TIO.putStrLn ("error: " <> err)) (const (pure ())) result

-- | Evaluate the source in a child process running 'namesOnlyChild' and
-- check that every symbol it added is one of the names, and that the
-- ones listed in @interned@ were added in that order.
internsNamesOnly :: Text -> Text -> [Text] -> [Text] -> IO Bool
internsNamesOnly label source names interned = runTestM label $ do
  self <- getExecutablePath
  (_, out, _) <- Proc.readCreateProcessWithExitCode (Proc.proc self [namesOnlyChildFlag, T.unpack source]) ""
  let added = T.lines (T.pack out)
  pure $
    assertEqual "symbols that are no name" [] (filter (`notElem` names) added)
      `andThen` assertEqual "names interned, in order" interned (filter (`elem` interned) added)

-- | The symbol table holds names alone, as upstream's does
-- (@SymbolTable@, symbol-table.hh at 2.24.9): a string value, a path, a
-- URI, a search path, a string's context and a source file's path take
-- no symbol, so none moves the place of a name interned after it, which
-- decides the order of a set's attributes.
testStringsOffSymbolsIO :: IO [Bool]
testStringsOffSymbolsIO = do
  putStrLn "eval/strings-off-symbols"
  sequence
    [ internsNamesOnly
        "an evaluation interns its names and no value it makes"
        ( T.unlines
            [ "let",
              "  zs = \"zlit\";",
              "  zi = \"zi${zs}\";",
              "  zj = ''",
              "    zind''\\t${zs}",
              "  '';",
              "  zu = http://zuri.example/zu;",
              "  zp = /zpath/zlit;",
              "  zq = /zpath/${zs};",
              "  zn = _: <zsearch>;",
              "  zl = builtins.split \"(zre)\" \"zazreb\";",
              "  zc = builtins.appendContext \"zctx\" { \"/nix/store/" <> censusStoreHash <> "-zctxname.drv\" = { path = true; outputs = [ \"zout\" ]; }; };",
              "in",
              "builtins.deepSeq [ zs zi zj zu zp zq zn zl zc ] null"
            ]
        )
        -- The names the source writes, and allOutputs, which appendContext
        -- interns as it runs, as upstream's does (prim_appendContext in
        -- context.cc).
        ( ["zs", "zi", "zj", "zu", "zp", "zq", "zn", "_", "zl", "zc", "builtins", "split", "appendContext", "deepSeq", "null", "true", "path", "outputs", "/nix/store/" <> censusStoreHash <> "-zctxname.drv"]
            ++ ["allOutputs"]
        )
        ["zs", "zi", "zj", "zu", "zp", "zq", "zn", "zl", "zc"],
      -- Upstream's listToAttrs interns each element's name as it reads it
      -- (prim_listToAttrs in primops.cc), so the first in symbol order is
      -- the first in the list, here the last by name.  The strings
      -- evaluated before, in name order, take no symbol.
      emits
        "string values equal to a set's names do not decide its order"
        "let early = builtins.concatStringsSep \",\" [ (\"z\" + \"a\") (\"z\" + \"b\") ]; in builtins.seq early (({ }: null) (builtins.listToAttrs [ { name = \"z\" + \"b\"; value = 1; } { name = \"z\" + \"a\"; value = 2; } ]))"
        ["error: function 'anonymous lambda' called with unexpected argument 'zb'"],
      emits
        "a string literal equal to a set's name does not decide its order"
        "let early = [ \"za\" \"zb\" ]; in builtins.seq (builtins.deepSeq early early) (({ }: null) (builtins.listToAttrs [ { name = \"z\" + \"b\"; value = 1; } { name = \"z\" + \"a\"; value = 2; } ]))"
        ["error: function 'anonymous lambda' called with unexpected argument 'zb'"]
    ]

-- | @listToAttrs@, @groupBy@ and @fromJSON@ intern each name of the set
-- they build where upstream's do, as they meet it: @listToAttrs@ an
-- element's name before it reads the next element (@prim_listToAttrs@ in
-- primops.cc), @groupBy@ the name its function returns before it calls the
-- function again (@prim_groupBy@ there), and @fromJSON@ a member's name
-- before its value (@JSONObjectState::key@ in json-to-value.cc).
-- The first name in symbol order is the unexpected argument a call names
-- and the pair @==@ decides at first, so each case runs in a child
-- process, whose symbol table holds the startup names alone, none of
-- these.  Each list and document gives its names out of name order.
testBuilderInterningIO :: IO [Bool]
testBuilderInterningIO = do
  putStrLn "eval/builder-interning-io"
  sequence
    [ emits
        "listToAttrs interns names in list order"
        "({ }: null) (builtins.listToAttrs [ { name = \"zb\"; value = 1; } { name = \"za\"; value = 2; } ])"
        [unexpected "zb"],
      -- The second element's name interns za as it evaluates, so zb is
      -- first only if it was interned when the first element was read.
      emits
        "listToAttrs interns a name before it reads the next element"
        "({ }: null) (builtins.listToAttrs [ { name = \"zb\"; value = 1; } { name = builtins.head (builtins.attrNames (builtins.listToAttrs [ { name = \"za\"; value = 1; } ])); value = 2; } ])"
        [unexpected "zb"],
      emits
        "groupBy interns names in list order"
        "({ }: null) (builtins.groupBy (x: x) [ \"zb\" \"za\" ])"
        [unexpected "zb"],
      emits
        "groupBy interns a name before it calls its function again"
        "({ }: null) (builtins.groupBy (x: if x == \"zb\" then x else builtins.head (builtins.attrNames (builtins.listToAttrs [ { name = x; value = 1; } ]))) [ \"zb\" \"za\" ])"
        [unexpected "zb"],
      emits
        "fromJSON interns names in document order"
        "({ }: null) (builtins.fromJSON \"{\\\"zb\\\": 1, \\\"za\\\": 2}\")"
        [unexpected "zb"],
      emits
        "fromJSON interns a member's name before the names in its value"
        "({ }: null) (let j = builtins.fromJSON \"{\\\"zb\\\": {\\\"za\\\": 1}}\"; in j // builtins.head (builtins.attrValues j))"
        [unexpected "zb"],
      -- The values are read through attrValues, which lists them by name,
      -- since a name written in source is interned as it parses.
      emits
        "fromJSON gives a repeated name its first place and its last value"
        "let j = builtins.fromJSON \"{\\\"zb\\\": 1, \\\"za\\\": 2, \\\"zb\\\": 3}\"; in builtins.trace (builtins.attrValues j) (({ }: null) j)"
        ["trace: [ 2 3 ]", unexpected "zb"],
      emits
        "fromJSON interns the names in a value a repeated name replaces"
        "let j = builtins.fromJSON \"{\\\"zc\\\": {\\\"zb\\\": 1}, \\\"zc\\\": 2, \\\"za\\\": 3}\"; in builtins.seq j (({ }: null) (builtins.listToAttrs [ { name = \"za\"; value = 1; } { name = \"zb\"; value = 2; } ]))"
        [unexpected "zb"],
      emits
        "== decides at the pair whose name listToAttrs met first"
        "builtins.listToAttrs [ { name = \"zb\"; value = throw \"b\"; } { name = \"za\"; value = 1; } ] == builtins.listToAttrs [ { name = \"zb\"; value = 1; } { name = \"za\"; value = throw \"a\"; } ]"
        ["error: b"]
    ]
  where
    unexpected name = "error: function 'anonymous lambda' called with unexpected argument '" <> name <> "'"

-- | A select interns a name it computes before it forces the value it
-- selects from, and a has-attribute test after it forces the value it
-- tests, as upstream's @ExprSelect::eval@ and @ExprOpHasAttr::eval@ call
-- @getName@, which interns a computed name, on either side of the force
-- (eval.cc).  The value forced is a set @listToAttrs@ builds, interning
-- its name as it runs, so the order of the two names shows which came
-- first.  It is the second in the path, since the first is forced before
-- any name is read.  Each case runs in a child, whose symbol table holds
-- neither name.
testSelectInterningIO :: IO [Bool]
testSelectInterningIO = do
  putStrLn "eval/select-interning-io"
  sequence
    [ internsNamesOnly
        "a select interns a computed name before it forces the value it selects from"
        "{ s = builtins.listToAttrs [ { name = \"z\" + \"b\"; value = 1; } ]; }.s.${\"z\" + \"a\"} or null"
        names
        ["za", "zb"],
      internsNamesOnly
        "a has-attribute test interns a computed name after it forces the value it tests"
        "{ s = builtins.listToAttrs [ { name = \"z\" + \"b\"; value = 1; } ]; } ? s.${\"z\" + \"a\"}"
        names
        ["zb", "za"],
      emits
        "a name a select computes precedes a name the value it selects from interns"
        "builtins.seq ({ s = builtins.listToAttrs [ { name = \"z\" + \"b\"; value = 1; } ]; }.s.${\"z\" + \"a\"} or null) (({ }: null) (builtins.listToAttrs [ { name = \"z\" + \"b\"; value = 1; } { name = \"z\" + \"a\"; value = 2; } ]))"
        ["error: function 'anonymous lambda' called with unexpected argument 'za'"]
    ]
  where
    names = ["s", "builtins", "listToAttrs", "name", "value", "za", "zb"]

-- | A set interns each dynamic name once it has evaluated it, before it
-- evaluates the next, as upstream's @ExprAttrs::eval@ in eval.cc does,
-- so dynamic names new to the symbol table take their places in source
-- order.  The first name in symbol order is the unexpected argument a
-- call names, so each case runs in a child process, whose symbol table
-- holds neither name.
testDynamicAttrInterningIO :: IO [Bool]
testDynamicAttrInterningIO = do
  putStrLn "eval/dynamic-attr-interning-io"
  sequence
    [ emits
        "a set interns its dynamic names in source order"
        "({ }: null) { ${\"z\" + \"b\"} = 1; ${\"z\" + \"a\"} = 2; }"
        [unexpected "zb"],
      emits
        "a rec set interns its dynamic names in source order"
        "({ }: null) (rec { ${\"z\" + \"b\"} = 1; ${\"z\" + \"a\"} = 2; })"
        [unexpected "zb"],
      -- Evaluating the second name interns za as listToAttrs builds its
      -- set, so zb is first only if it was interned before that.
      emits
        "a set interns a dynamic name before it evaluates the next"
        "({ }: null) { ${\"z\" + \"b\"} = 1; ${builtins.head (builtins.attrNames (builtins.listToAttrs [ { name = \"z\" + \"a\"; value = 1; } ]))} = 2; }"
        [unexpected "zb"]
    ]
  where
    unexpected name = "error: function 'anonymous lambda' called with unexpected argument '" <> name <> "'"

-- | @deepSeq@ and @--strict@ force a set's values in the order the set
-- holds them, as upstream's @forceValueDeep@ walks a set's @Bindings@
-- (eval.cc), which are sorted by @Symbol@ (the @Attr@ comparison in
-- attr-set.hh), whose order is the order the symbol table interned the
-- name in (the @Symbol@ comparison in symbol-table.hh).  Upstream's
-- parser interns a set's names in source order (@binds@ and @attrpath@
-- in parser.y at 2.24.9, @binds1@ at 2.36.0), so of two values that
-- fail, the one written first fails first here, where name order would
-- force the other.  The case runs in a child process, whose symbol
-- table holds neither name.
testDeepForceOrderIO :: IO [Bool]
testDeepForceOrderIO = do
  putStrLn "eval/deep-force-order-io"
  sequence
    [ emits
        "deepSeq forces a set's values in the order their names were interned"
        "builtins.deepSeq { zb = throw \"b\"; za = throw \"a\"; } null"
        ["error: b"]
    ]

-- | A binding, a list element, a call's argument or a default whose
-- value is a variable holds the variable's own cell where upstream's
-- @maybeThunk@ does (eval.cc), which @==@ shows of a function, equal
-- only to itself (@eqValues@ in eval.cc).  A global and a variable of
-- an enclosing scope are bound by then, and so is a recursive set's
-- static name by the time its dynamic ones are bound (@ExprAttrs::eval@
-- in eval.cc).  A variable only a with supplies never is
-- (@EvalState::lookupVar@ in eval.cc).  A recursive set, a @let@ and a
-- set pattern fill their slots in symbol order, so a variable naming a
-- sibling shares its cell only when the sibling's name was interned
-- first, and a recursive set that binds @__overrides@ shares none but
-- an inherited name's (@ExprAttrs::eval@ in eval.cc).  Symbol order is
-- the order the symbol table interned the names in, so each case runs
-- in a child process, whose table holds none of the names but the
-- globals and interns them in the order the source gives them, as
-- upstream's parser does; a @let@ that binds nothing the case uses puts
-- a set pattern's names in the order the case needs.
testSharedVariableCellsIO :: IO [Bool]
testSharedVariableCellsIO = do
  putStrLn "eval/shared-variable-cells-io"
  sequence
    [ evaluatesTo "a set's attribute and a list element share a global's cell" "builtins.catAttrs \"g\" [ { g = map; } ] == [ map ]" "true",
      evaluatesTo "two list elements share a global's cell" "[ map ] == [ map ]" "true",
      evaluatesTo "an inherited global shares its cell" "builtins.catAttrs \"map\" [ { inherit map; } ] == [ map ]" "true",
      evaluatesTo "a dynamic attribute shares a global's cell" "builtins.catAttrs \"g\" [ { ${\"g\" + \"\"} = map; } ] == [ map ]" "true",
      evaluatesTo "a let binding shares a global's cell" "let m = map; in [ m ] == [ map ]" "true",
      evaluatesTo "a call's argument shares a global's cell" "(x: [ x ]) map == [ map ]" "true",
      evaluatesTo "a recursive set's dynamic attribute shares a static sibling's cell" "let s = rec { f = x: x; ${\"g\" + \"\"} = f; }; in builtins.catAttrs \"f\" [ s ] == builtins.catAttrs \"g\" [ s ]" "true",
      evaluatesTo "a recursive set's attribute shares a sibling's cell bound before it" "let s = rec { f = x: x; g = f; }; in builtins.catAttrs \"f\" [ s ] == builtins.catAttrs \"g\" [ s ]" "true",
      evaluatesTo "a recursive set's attribute does not share a sibling's cell bound after it" "let s = rec { g = f; f = x: x; }; in builtins.catAttrs \"f\" [ s ] == builtins.catAttrs \"g\" [ s ]" "false",
      evaluatesTo "a chain of siblings shares the cell at its end" "let s = rec { f = x: x; g = f; h = g; }; in builtins.catAttrs \"f\" [ s ] == builtins.catAttrs \"h\" [ s ]" "true",
      evaluatesTo "a static attribute of a recursive set with a dynamic name shares a sibling's cell bound before it" "let s = rec { f = x: x; g = f; ${\"h\" + \"\"} = 1; }; in builtins.catAttrs \"f\" [ s ] == builtins.catAttrs \"g\" [ s ]" "true",
      evaluatesTo "a static attribute of a recursive set with a dynamic name does not share a sibling's cell bound after it" "let s = rec { g = f; f = x: x; ${\"h\" + \"\"} = 1; }; in builtins.catAttrs \"f\" [ s ] == builtins.catAttrs \"g\" [ s ]" "false",
      -- The inner set's static names are in a name table, whose entry for
      -- zq is not filled when zp's value is made, and a lookup by name
      -- passes over an entry not filled yet to the enclosing set's table,
      -- which binds zq too; zp's value reads its own frame's zq.
      evaluatesTo "a static attribute of a recursive set with a dynamic name reads the sibling it names, not an enclosing set's attribute of that name" "let zp = 0; in (rec { zq = 1; ${\"d\" + \"\"} = 2; s = rec { zp = zq; zq = x: x; ${\"h\" + \"\"} = 1; }; }).s.zp 5" "5",
      evaluatesTo "a recursive set's attribute shares the cell of a variable of an enclosing scope" "let f = x: x; in builtins.catAttrs \"g\" [ rec { g = f; } ] == [ f ]" "true",
      evaluatesTo "a let binding shares the cell of a variable of an enclosing scope" "let f = x: x; in let g = f; in [ f ] == [ g ]" "true",
      evaluatesTo "a default shares the cell of a variable of an enclosing scope" "let f = x: x; in ({ g ? f }: [ g ] == [ f ]) { }" "true",
      evaluatesTo "a let binding shares a sibling's cell bound before it" "let f = x: x; g = f; in [ f ] == [ g ]" "true",
      evaluatesTo "a let binding does not share a sibling's cell bound after it" "let g = f; f = x: x; in [ f ] == [ g ]" "false",
      evaluatesTo "a default shares the cell of a formal bound before it" "let za = 0; zb = 0; in ({ za, zb ? za }: [ za ] == [ zb ]) { za = x: x; }" "true",
      evaluatesTo "a default does not share the cell of a formal bound after it" "let zb = 0; za = 0; in ({ za, zb ? za }: [ za ] == [ zb ]) { za = x: x; }" "false",
      -- Upstream's parser interns a default's variable before the formal
      -- it defaults (expr_simple and formal in parser.y), so a formal
      -- written after the default that names it is bound before.
      evaluatesTo "a default shares the cell of a formal written after it" "({ zb ? za, za }: [ za ] == [ zb ]) { za = x: x; }" "true",
      evaluatesTo "a let binding shares the cell at the end of a chain declared out of symbol order" "let zl = 0; zm = 0; zn = 0; in let zn = zm; zm = zl; zl = x: x; in [ zl ] == [ zn ]" "true",
      evaluatesTo "a default shares the cell at the end of a chain declared out of symbol order" "let zr = 0; zt = 0; zx = 0; in ({ zx ? zt, zt ? zr, zr }: [ zr ] == [ zx ]) { zr = x: x; }" "true",
      -- Comparing the two sets would force the attribute that throws.
      evaluatesTo "a default naming the @-pattern shares the argument" "(args@{ zz ? args, ... }: [ zz ] == [ args ]) { f = throw \"boom\"; }" "true",
      evaluatesTo "a default naming a formal the argument holds shares the argument's attribute" "let za = 0; zb = 0; zc = 0; in ({ zc ? zb, zb ? za, za }: [ zb ] == [ zc ]) { za = 1; zb = x: x; }" "true",
      evaluatesTo "a variable a with supplies does not share its cell" "let s = { f = x: x; }; in with s; builtins.catAttrs \"f\" [ s ] == [ f ]" "false",
      evaluatesTo "a recursive set that binds __overrides does not share a sibling's cell" "let s = rec { __overrides = { }; f = x: x; g = f; }; in builtins.catAttrs \"f\" [ s ] == builtins.catAttrs \"g\" [ s ]" "false",
      evaluatesTo "a recursive set that binds __overrides makes a value a thunk of its own" "builtins.catAttrs \"g\" [ rec { __overrides = { }; g = map; } ] == [ map ]" "false",
      evaluatesTo "a recursive set with a dynamic name that binds __overrides makes a value a thunk of its own" "builtins.catAttrs \"g\" [ rec { __overrides = { }; g = map; ${\"h\" + \"\"} = 1; } ] == [ map ]" "false",
      evaluatesTo "a recursive set that binds __overrides shares an inherited global's cell" "builtins.catAttrs \"map\" [ rec { __overrides = { }; inherit map; } ] == [ map ]" "true",
      runTestM "a frame's variables share the cells upstream's binding in symbol order gives them, copied in any order" . quickCheckPasses $
        QC.forAll genFrame $ \(names, values) ->
          let slots = zip3 [0 ..] names values
              nameOf = (`Map.lookup` Map.fromList [(slot, name) | (slot, name, _) <- slots])
              ownCells = Map.fromList [(slot, slot) | (slot, _, _) <- slots]
              siblings = [SiblingVar name slot target | (slot, name, Just target) <- slots]
              shares = siblingShares nameOf siblings
              shareCell cells (slot, target) = maybe cells (\cell -> Map.insert slot cell cells) (Map.lookup target cells)
           in (foldl' shareCell ownCells shares, foldl' shareCell ownCells (reverse shares)) QC.=== (upstreamFill slots, upstreamFill slots),
      -- The compiler decides a frame's shares as it compiles the frame,
      -- each naming the slot at the end of its chain; names no other
      -- test interns keep their symbols in the order the code gives them.
      runTestM "a let's bindings hold their shares, each naming the slot at the end of its chain" $ do
        let bound name = NamedBinding (testPos 1 1) (StaticKey name)
        idx <- compileExpr (ELet [bound "zshareA" (ELit (NixInt 1)), bound "zshareB" (EResolvedVar 0 0), bound "zshareC" (EResolvedVar 0 1), bound "zshareD" (EResolvedVar 0 4), bound "zshareE" (ELit NixNull)] (ELit NixNull) NoCaptureInfo)
        BcBindings _ shares _ _ <- compiledBindings idx
        pairs <- foldBcShares (\found slot target -> pure ((slot, target) : found)) [] shares
        pure (assertEqual "shares" [(1, 0), (2, 0)] (reverse pairs)),
      runTestM "a plain set and a recursive set that binds __overrides hold no shares" $ do
        let bound name = NamedBinding (testPos 1 1) (StaticKey name)
            sharesOf expr = compileExpr expr >>= compiledBindings >>= \(BcBindings _ (BcShares count _) _ _) -> pure count
        plain <- sharesOf (EAttrs False [bound "zshareF" (ELit NixNull), bound "zshareG" (EVar "zshareF")] NoCaptureInfo)
        overridden <- sharesOf (EAttrs True [bound "__overrides" (EAttrs False [] NoCaptureInfo), bound "zshareH" (ELit NixNull), bound "zshareI" (EResolvedVar 0 1)] NoCaptureInfo)
        pure (assertEqual "plain" 0 plain `andThen` assertEqual "__overrides" 0 overridden),
      runTestM "a set pattern's defaults naming its slots share where upstream's do, each naming the slot it reads" $ do
        let formals =
              FormalNamedSet
                "zshareArgs"
                [ Formal "zshareP" (Just (EResolvedVar 0 4)),
                  Formal "zshareQ" (Just (EResolvedVar 0 1)),
                  Formal "zshareR" (Just (EResolvedVar 0 2)),
                  Formal "zshareS" (Just (EResolvedVar 0 0))
                ]
                False
        idx <- compileExpr (ELambda formals (ELit NixNull) NoCaptureInfo)
        flags <- cbcFlags idx
        decoded <- cbcArg1 idx >>= decodeFormals flags
        defaults <- case decoded of
          Just (EFNamedSet _ _ entries _) -> traverse (defaultSlot . efDefault) entries
          _ -> pure []
        pure (assertEqual "defaults" [("default", 4), ("shared", 1), ("shared", 2), ("shared", 0)] defaults)
    ]
  where
    evaluatesTo label source result = emits label ("builtins.trace (" <> source <> ") null") ["trace: " <> result]
    -- A default's kind and the slot its code names, a variable of level 0.
    defaultSlot :: FormalDefault -> IO (Text, Word32)
    defaultSlot = \case
      NoDefault -> pure ("none", 0)
      Default code -> ("default",) <$> cbcArg2 code
      SharedDefault code -> ("shared",) <$> cbcArg2 code
    quickCheckPasses property = do
      outcome <- QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} property
      pure (if QC.isSuccess outcome then Pass else Fail (T.pack (QC.output outcome)))
    -- A frame of up to six slots, each binding a name of its own, in any
    -- symbol order, and each holding either a variable naming one of the
    -- frame's slots, itself included, or some other value.
    genFrame = do
      size <- QC.choose (1, 6)
      names <- QC.shuffle (map Symbol [1 .. fromIntegral size])
      values <- QC.vectorOf size (QC.oneof [pure Nothing, Just <$> QC.choose (0, size - 1)])
      pure (names, values)
    -- Upstream's loop over a frame's slots, each its number, its name and
    -- the slot its variable names: the slots filled one at a time in
    -- symbol order, a variable taking the cell of the slot it names when
    -- that slot is filled and a thunk of its own when it is not, and any
    -- other value a cell of its own, each cell named by the slot it was
    -- made for.
    upstreamFill :: [(Int, Symbol, Maybe Int)] -> Map.Map Int Int
    upstreamFill slots = foldl' fill Map.empty (sortOn (\(_, name, _) -> name) slots)
      where
        fill env (slot, _, value) = Map.insert slot (fromMaybe slot (value >>= (`Map.lookup` env))) env

-- | A string's bytes are made once and shared by every cell that holds
-- the string, as upstream copies a @Value@ with the pointer to its bytes
-- (value.hh at 2.24.9), where a copy per cell would grow the string arena
-- each time a string passes from one cell to another.  A string literal's
-- value is its bytes in the code, as upstream's @ExprString@ hands out
-- the value it holds (@ExprString::maybeThunk@ in eval.cc).
testStringSharing :: IO [Bool]
testStringSharing = do
  putStrLn "eval/string-sharing"
  tmpBase <- getTemporaryDirectory
  sequence
    [ runTestM "a string, a string with context and a path read from one cell share their bytes with the cell that stores them" $ do
        let source =
              T.unlines
                [ "let",
                  "  s = builtins.concatStringsSep \"\" [ \"z\" \"pass\" ];",
                  "  c = builtins.appendContext \"zctx\" { \"/nix/store/" <> censusStoreHash <> "-zname.drv\" = { outputs = [ \"out\" ]; }; };",
                  "  p = /zpass + \"/x\";",
                  "  f = _: \"zlit\";",
                  "  pick = x: x;",
                  "  l = [ s (pick s) c (pick c) p (pick p) (f 1) (f 2) ];",
                  "in",
                  "builtins.deepSeq l l"
                ]
        result <- evalNixIO tmpBase source
        case result of
          Left failure -> pure (Fail failure)
          Right (VList list) -> do
            cells <- clistThunks list
            case cells of
              [s1, s2, c1, c2, p1, p2, l1, l2] -> do
                strs <- mapM cthunkPayload [s1, s2]
                literals <- mapM cthunkPayload [l1, l2]
                ctxs <- mapM cthunkPayload [c1, c2]
                paths <- mapM cthunkPayload [p1, p2]
                pure $
                  assertEqual "string" True (allSame strs)
                    `andThen` assertEqual "literal" True (allSame literals)
                    `andThen` assertEqual "string with context" True (allSame ctxs)
                    `andThen` assertEqual "path" True (allSame paths)
              _ -> pure (Fail ("expected eight cells, got " <> T.pack (show (length cells))))
          Right other -> pure (Fail ("expected a list, got " <> typeName other)),
      runTestM "every cell a string literal makes points at the literal's bytes in the code" $ do
        idx <- compileExpr (EStr [StrLit "zshared literal"])
        (_, dataOff) <- cbcCountedPayload idx =<< cbcArg1 idx
        code <- cbcData (dataOff + 1) >>= cbcString
        cells <- replicateM 2 (cheapThunkBc emptyEnv idx)
        held <- mapM (fmap castPtr . cthunkPayload . unThunk) cells
        values <- mapM readThunkValue cells
        pure $
          assertEqual "the code's string" [code, code] held
            `andThen` assertEqual "values" [Just (mkStr "zshared literal"), Just (mkStr "zshared literal")] values,
      runTestM "a URI literal's cell points at the URI's bytes in the code" $ do
        idx <- compileExpr (ELit (NixUri "zuri:shared"))
        code <- cbcArg1 idx >>= cbcString
        cell <- cheapThunkBc emptyEnv idx
        held <- castPtr <$> cthunkPayload (unThunk cell)
        value <- readThunkValue cell
        pure (assertEqual "the code's string" code held `andThen` assertEqual "value" (Just (mkStr "zuri:shared")) value),
      -- Only a corrupt store names a string no emit returned.
      runTestM "a string index no emit returned raises CInvariantViolated" $ do
        outcome <- try (cbcString maxBound)
        pure $ case outcome of
          Left (CInvariantViolated site _) -> assertEqual "site" "nn_bc_string" site
          Left other -> Fail ("expected CInvariantViolated, got " <> T.pack (show other))
          Right _ -> Fail "an index no emit returned read as a string"
    ]
  where
    allSame xs = and (zipWith (==) xs (drop 1 xs))

-- | Names are interned in the order upstream's parser interns them, the
-- order a set's attributes are stored in.  There is no Nix here to run,
-- so each expected order is derived from the actions of parser.y at
-- 2.24.9, cited per case: bison runs a rule's action once the rule's
-- right-hand side is parsed, so the name an action interns follows the
-- names inside the rule.  No name whose order a case checks is one
-- either evaluator interns at startup.  A name
-- noted after a function's body is followed by another name, so that the
-- compiler, which interns a function's argument after its body, cannot
-- give the order on its own.
testParseOrderInterningIO :: IO [Bool]
testParseOrderInterningIO = do
  putStrLn "eval/parse-order-interning-io"
  sequence
    [ internsInOrder "a let's bindings before its body (parser.y's binds and expr_function)" "let za = 1; zb = za; in { zc = zb; }" ["za", "zb", "zc"],
      internsInOrder "a variable where it is read, before the binding that defines it (parser.y's expr_simple)" "let zq = zt; zs = 1; zt = 2; in zq" ["zq", "zt", "zs"],
      internsInOrder "an argument after its body, an inner one first (parser.y's expr_function)" "with { }; [ (zx: zw: { zy = 1; }) zv ]" ["zy", "zw", "zx", "zv"],
      internsInOrder "formals before the body, each after its default (parser.y's formal)" "{ za ? { zb = 1; }, zc }: { zd = za; }" ["zb", "za", "zc", "zd"],
      internsInOrder "a variable a default reads before the formal it defaults (parser.y's expr_simple and formal)" "{ zx ? zy, zy }: zx" ["zy", "zx"],
      internsInOrder "an @-name written first after the formals and the body (parser.y's expr_function)" "with { }; [ (zn@{ za }: { zb = za; }) zv ]" ["za", "zb", "zn", "zv"],
      internsInOrder "an @-name written last after the formals and the body (parser.y's expr_function)" "with { }; [ ({ za }@zn: { zb = za; }) zv ]" ["za", "zb", "zn", "zv"],
      internsInOrder "an attribute path before its or default (parser.y's expr_select and attrpath)" "{ }.za.zb or { zc = 1; }" ["za", "zb", "zc"],
      internsInOrder "a has-attribute path after its operand (parser.y's expr_op)" "{ zc = 1; } ? za.zb" ["zc", "za", "zb"],
      internsInOrder "an attribute path written in several places in source order (parser.y's binds and attrpath)" "{ za.zb = 1; zc = 2; za.zd = 3; }" ["za", "zb", "zc", "zd"],
      internsInOrder "inherited names in order, after an inherit's source (parser.y's binds and attrs)" "with { }; { inherit ({ zc = 1; }) zb za; inherit zd; ze = 1; }" ["zc", "zb", "za", "zd", "ze"],
      internsInOrder "a with's scope before its body (parser.y's expr_function)" "with { zb = 1; }; { za = zb; }" ["zb", "za"],
      internsInOrder "a dynamic key's expression in place, and a key of a plain string as a name (string_attr in parser.y's attrpath)" "with { }; { ${zk} = 1; ${\"zm\"} = 2; \"zn\" = 3; zl = 4; }" ["zk", "zm", "zn", "zl"],
      internsInOrder "a key of a URI as a name where it is written (parser.y's expr_simple and attrpath)" "{ ${zu:v} = 1; zl = 2; }" ["zu:v", "zl"],
      internsInOrder "__curPos is no name (parser.y's expr_simple)" "{ za = __curPos; zb = 1; }" ["za", "zb"],
      -- An evaluation shows the order: the argument an unexpected-argument
      -- error names is the first in symbol order, here as under upstream
      -- (callFunction in eval.cc).
      emits "a let's bindings are interned before its body" "let za = 1; in ({ }: za) { zb = 1; za = 2; }" ["error: function 'anonymous lambda' called with unexpected argument 'za'"],
      emits "a function's formals are interned before its body" "({ za ? 1 }: ({ }: null) { zb = 1; za = 2; }) { }" ["error: function 'anonymous lambda' called with unexpected argument 'za'"],
      emits "an attribute path is interned before its or default" "{ }.za or (({ }: null) { zb = 1; za = 2; })" ["error: function 'anonymous lambda' called with unexpected argument 'za'"]
    ]

-- ---------------------------------------------------------------------------
-- Tests: upstream's startup names, and the call, select and abort errors
-- ---------------------------------------------------------------------------

-- | Run as a child, print every name the symbol table holds once its
-- session has started, one a line, in symbol order.
startupSymbolsChildFlag :: String
startupSymbolsChildFlag = "--startup-symbols-child"

startupSymbolsChild :: IO ()
startupSymbolsChild = do
  count <- symbolCount
  mapM_ (TIO.putStrLn . symbolText . Symbol) [1 .. count]

-- | Run as a child, evaluate one expression under the IO evaluator and
-- print its failure as 'show' writes it, which escapes what is not ASCII,
-- so the parent reads it whatever either process's locale.
failureChildFlag :: String
failureChildFlag = "--failure-child"

failureChild :: String -> IO ()
failureChild source = evalNixIOTraced "." (T.pack source) >>= putStrLn . either show (const "no failure")

-- | Evaluate the source in a child process running 'failureChild', whose
-- symbol table holds the startup names and the source's alone, and
-- compare its failure with the expected one.
failsInChild :: Text -> Text -> EvalFailure -> IO Bool
failsInChild label source expected = runTestM label $ do
  self <- getExecutablePath
  (_, out, _) <- Proc.readCreateProcessWithExitCode (Proc.proc self [failureChildFlag, T.unpack source]) ""
  pure (assertEqual label [show expected] (lines out))

-- | The failure of a call to a set pattern, with the line of context
-- upstream adds for the call (@callFunction@ in eval.cc).
callFailure :: Text -> Text -> EvalFailure
callFailure function what = EvalFailure ["from call site"] ("function '" <> function <> "' called " <> what)

-- | The symbol table once a session has started: the names upstream 2.24.9
-- interns as it starts with its default settings, each once, in the
-- order it first interns them.  @EvalState@'s constructor in eval.cc
-- interns its static symbols in the order eval.hh declares them (@sWith@
-- to @exprSymbols@), the empty name among them.  @createBaseEnv@
-- (primops.cc) then installs @builtins@, @true@, @false@,
-- @null@ and the constants @__currentTime@ to @__nixPath@, every
-- registered primop not gated on an experimental feature in the order it
-- registers (primops.cc from @primop_scopedImport@, then
-- primops/context.cc, fetchMercurial.cc, fetchTree.cc and fromTOML.cc),
-- and @derivation@, interning each name and then that name less a @__@
-- prefix (@addConstant@ and @addPrimOp@ in eval.cc).  Last come the names
-- the @derivation@ wrapper's parser interns that nothing before it did
-- (primops/derivation.nix, the grammar actions of parser.y).
upstreamStartupTable :: [Text]
upstreamStartupTable =
  [ "<with>",
    "outPath",
    "drvPath",
    "type",
    "meta",
    "name",
    "value",
    "system",
    "__overrides",
    "outputs",
    "outputName",
    "__ignoreNulls",
    "file",
    "line",
    "column",
    "__functor",
    "__toString",
    "right",
    "wrong",
    "__structuredAttrs",
    "allowedReferences",
    "allowedRequisites",
    "disallowedReferences",
    "disallowedRequisites",
    "maxSize",
    "maxClosureSize",
    "builder",
    "args",
    "__contentAddressed",
    "__impure",
    "outputHash",
    "outputHashAlgo",
    "outputHashMode",
    "recurseForDerivations",
    "description",
    "self",
    "",
    "startSet",
    "operator",
    "key",
    "path",
    "prefix",
    "outputSpecified",
    "__sub",
    "__lessThan",
    "__mul",
    "__div",
    "or",
    "__findFile",
    "__nixPath",
    "body",
    "builtins",
    "true",
    "false",
    "null",
    "__currentTime",
    "currentTime",
    "__currentSystem",
    "currentSystem",
    "__nixVersion",
    "nixVersion",
    "__storeDir",
    "storeDir",
    "__langVersion",
    "langVersion",
    "__traceVerbose",
    "traceVerbose",
    "nixPath",
    "scopedImport",
    "import",
    "__typeOf",
    "typeOf",
    "isNull",
    "__isFunction",
    "isFunction",
    "__isInt",
    "isInt",
    "__isFloat",
    "isFloat",
    "__isString",
    "isString",
    "__isBool",
    "isBool",
    "__isPath",
    "isPath",
    "__genericClosure",
    "genericClosure",
    "break",
    "abort",
    "throw",
    "__addErrorContext",
    "addErrorContext",
    "__ceil",
    "ceil",
    "__floor",
    "floor",
    "__tryEval",
    "tryEval",
    "__getEnv",
    "getEnv",
    "__seq",
    "seq",
    "__deepSeq",
    "deepSeq",
    "__trace",
    "trace",
    "__warn",
    "warn",
    "derivationStrict",
    "placeholder",
    "__toPath",
    "toPath",
    "__storePath",
    "storePath",
    "__pathExists",
    "pathExists",
    "baseNameOf",
    "dirOf",
    "__readFile",
    "readFile",
    "findFile",
    "__hashFile",
    "hashFile",
    "__readFileType",
    "readFileType",
    "__readDir",
    "readDir",
    "__toXML",
    "toXML",
    "__toJSON",
    "toJSON",
    "__fromJSON",
    "fromJSON",
    "__toFile",
    "toFile",
    "__filterSource",
    "filterSource",
    "__path",
    "__attrNames",
    "attrNames",
    "__attrValues",
    "attrValues",
    "__getAttr",
    "getAttr",
    "__unsafeGetAttrPos",
    "unsafeGetAttrPos",
    "__hasAttr",
    "hasAttr",
    "__isAttrs",
    "isAttrs",
    "removeAttrs",
    "__listToAttrs",
    "listToAttrs",
    "__intersectAttrs",
    "intersectAttrs",
    "__catAttrs",
    "catAttrs",
    "__functionArgs",
    "functionArgs",
    "__mapAttrs",
    "mapAttrs",
    "__zipAttrsWith",
    "zipAttrsWith",
    "__isList",
    "isList",
    "__elemAt",
    "elemAt",
    "__head",
    "head",
    "__tail",
    "tail",
    "map",
    "__filter",
    "filter",
    "__elem",
    "elem",
    "__concatLists",
    "concatLists",
    "__length",
    "length",
    "__foldl'",
    "foldl'",
    "__any",
    "any",
    "__all",
    "all",
    "__genList",
    "genList",
    "__sort",
    "sort",
    "__partition",
    "partition",
    "__groupBy",
    "groupBy",
    "__concatMap",
    "concatMap",
    "__add",
    "add",
    "sub",
    "mul",
    "div",
    "__bitAnd",
    "bitAnd",
    "__bitOr",
    "bitOr",
    "__bitXor",
    "bitXor",
    "lessThan",
    "toString",
    "__substring",
    "substring",
    "__stringLength",
    "stringLength",
    "__hashString",
    "hashString",
    "__convertHash",
    "convertHash",
    "__match",
    "match",
    "__split",
    "split",
    "__concatStringsSep",
    "concatStringsSep",
    "__replaceStrings",
    "replaceStrings",
    "__parseDrvName",
    "parseDrvName",
    "__compareVersions",
    "compareVersions",
    "__splitVersion",
    "splitVersion",
    "__unsafeDiscardStringContext",
    "unsafeDiscardStringContext",
    "__hasContext",
    "hasContext",
    "__unsafeDiscardOutputDependency",
    "unsafeDiscardOutputDependency",
    "__addDrvOutputDependencies",
    "addDrvOutputDependencies",
    "__getContext",
    "getContext",
    "__appendContext",
    "appendContext",
    "fetchMercurial",
    "__fetchurl",
    "fetchurl",
    "fetchTarball",
    "fetchGit",
    "fromTOML",
    "derivation",
    "strict",
    "drvAttrs",
    "commonAttrs",
    "outputsList",
    "x",
    "outputToAttrListElement"
  ]

-- | The names upstream's evaluator interns as it starts take the first
-- symbols, in its order, ahead of any name a program brings, and they
-- hold every name the root environment and @builtins@ hold.  The errors
-- that walk a set in symbol order show the order end to end, in a
-- child each, whose table holds the startup names and the source's alone.
-- Under upstream each pair of names is in another order than the source
-- gives them, and but for @x@ and the empty name in another order than
-- their own, so the expected name follows from upstream's startup order
-- and from neither of those.
testStartupSymbolsIO :: IO [Bool]
testStartupSymbolsIO = do
  putStrLn "eval/startup-symbols-io"
  self <- getExecutablePath
  let unexpected name = callFailure "anonymous lambda" ("with unexpected argument '" <> name <> "'")
      unknownTo = filter (`notElem` startupSymbols)
  sequence
    [ runTestM "a session's start interns upstream's startup names first, in upstream's order" $ do
        (_, out, _) <- Proc.readCreateProcessWithExitCode (Proc.proc self [startupSymbolsChildFlag]) ""
        pure (assertEqual "symbol table" upstreamStartupTable (T.lines (T.pack out))),
      runTest "the startup names hold upstream's base environment under both spellings" $
        assertEqual
          "missing"
          []
          (unknownTo (concatMap (\name -> name : maybeToList (T.stripPrefix "__" name)) (Set.toList upstreamBaseEnvNames))),
      runTest "builtins holds no name the startup names lack" $
        assertEval
          "builtins"
          ("builtins.toJSON (builtins.filter (name: !(builtins.elem name [ " <> T.unwords (map (\name -> "\"" <> name <> "\"") startupSymbols) <> " ])) (builtins.attrNames builtins))")
          (mkStr "[]"),
      -- Upstream registers fetchTree, fetchClosure and outputOf only with
      -- an experimental feature on, and nova-nix has no experimental
      -- feature to turn on.
      runTest "builtins holds no primop upstream registers only behind an experimental feature" $
        assertEval "gated" "builtins.toJSON (builtins.filter (name: builtins.hasAttr name builtins) [ \"fetchTree\" \"fetchClosure\" \"outputOf\" ])" (mkStr "[]"),
      failsInChild "a name upstream's derivation wrapper brings precedes a program's" "({ }: null) { zzz = 1; x = 2; }" (unexpected "x"),
      failsInChild "a static symbol precedes a primop's name" "({ }: null) { map = 1; outPath = 2; }" (unexpected "outPath"),
      failsInChild "an expression symbol precedes a constant's name" "({ }: null) { null = 1; or = 2; }" (unexpected "or"),
      failsInChild "primops of one file come in the order they register" "({ }: null) { seq = 1; throw = 2; }" (unexpected "throw"),
      failsInChild "primops.cc's primops precede primops/context.cc's" "({ }: null) { getContext = 1; toString = 2; }" (unexpected "toString"),
      failsInChild "primops/context.cc's primops precede fetchMercurial.cc's" "({ }: null) { fetchMercurial = 1; unsafeDiscardStringContext = 2; }" (unexpected "unsafeDiscardStringContext"),
      failsInChild "fetchMercurial.cc's primop precedes fetchTree.cc's" "({ }: null) { fetchGit = 1; fetchMercurial = 2; }" (unexpected "fetchMercurial"),
      failsInChild "a primop's name precedes the derivation wrapper's" "({ }: null) { commonAttrs = 1; toString = 2; }" (unexpected "toString"),
      -- The main program's environment holds the wrapper, so a name only
      -- this evaluator's wrapper used would be interned before the file
      -- imported after it names it, and ahead of zz.
      failsInChild
        "the derivation wrapper interns no name upstream's startup lacks, so an imported file's names keep its order"
        "({ }: null) (import (builtins.toFile \"names.nix\" \"{ zz = 1; shared = 2; entries = 3; paths = 4; entry = 5; }\"))"
        (unexpected "zz"),
      failsInChild "the empty name is a static symbol" "({ }: null) { zz = 1; \"\" = 2; }" (unexpected "\"\""),
      failsInChild "a missing argument is the first in symbol order" "({ aa, value }: 1) { }" (callFailure "anonymous lambda" "without required argument 'value'")
    ]

-- | The errors of a call to a set pattern, of a select and of @abort@, as
-- upstream words them at 2.24.9, with the lines of context it adds less
-- their positions.  A set pattern checks every formal it lacks before
-- anything unexpected (@callFunction@ in eval.cc) and names the function
-- after the attribute it is bound to (@ParserState::addAttr@ in
-- parser-state.hh, @ExprLambda::setName@ in nixexpr.cc); a select names
-- the name its set lacks (@ExprSelect::eval@ in eval.cc); @abort@ words
-- its message (@primop_abort@ in primops.cc).  Upstream also adds
-- @while calling the 'abort' builtin@ to @abort@'s failure and
-- @while calling a functor ...@ to a functor's (@callFunction@ in
-- eval.cc), which this evaluator does not add, so the expectations leave
-- them out.
testCallErrorsIO :: IO [Bool]
testCallErrorsIO = do
  putStrLn "eval/call-errors-io"
  tmpBase <- getTemporaryDirectory
  let fails label source expected = runTestM label $ do
        result <- evalNixIOTraced tmpBase source
        pure $ case result of
          Left failed -> assertEqual label expected failed
          Right val -> Fail ("expected a failure, got " <> T.pack (show val))
      missing function name = callFailure function ("without required argument '" <> name <> "'")
      unexpected name = callFailure "anonymous lambda" ("with unexpected argument '" <> name <> "'")
  sequence
    [ fails "an argument missing beside an unexpected one is named first" "({ a }: a) { b = 1; }" (missing "anonymous lambda" "a"),
      fails "a missing argument names the function" "({ a }: a) { }" (missing "anonymous lambda" "a"),
      fails "a select names the name its set lacks" "{ a = 1; }.b" (EvalFailure [] "attribute 'b' missing"),
      fails "abort words the message it was given" "abort \"boom\"" (EvalFailure [] "evaluation aborted with the following error message: 'boom'"),
      runTestM "a call's failure is reported after its line of context" $ do
        result <- evalNixIOTraced tmpBase "({ a }: a) { }"
        pure $ case result of
          Left failed -> assertEqual "report" "error:\n       \x2026 from call site\n\n       error: function 'anonymous lambda' called without required argument 'a'" (renderEvalFailure failed)
          Right val -> Fail ("expected a failure, got " <> T.pack (show val)),
      fails "a function bound in a let is named by its binding" "let f = { a }: a; in f { }" (missing "f" "a"),
      fails "a function bound in a set is named, and the function its body is" "{ f = x: { a }: a; }.f 1 { }" (missing "f" "a"),
      fails "a function at the end of an attribute path is named by its last name" "{ s.f = { a }: a; }.s.f { }" (missing "f" "a"),
      fails "an @-pattern's function is named by its binding" "let f = args@{ a }: a; in f { }" (missing "f" "a"),
      fails "an inherited function keeps its binding's name" "let f = { a }: a; s = { inherit f; }; in s.f { }" (missing "f" "a"),
      fails "a function read back from a forced binding keeps its name" "let f = { a }: a; in builtins.seq f (f { })" (missing "f" "a"),
      fails "an @-pattern's function read back from a forced binding keeps its name" "let f = args@{ a }: a; in builtins.seq f (f { })" (missing "f" "a"),
      fails "a functor is named __functor" "{ __functor = self: { a }: a; } { }" (missing "__functor" "a"),
      fails "a function a formal defaults to is anonymous" "({ g ? { a }: a }: g { }) { }" (missing "anonymous lambda" "a"),
      fails "a function under an assert is anonymous" "let f = assert true; { a }: a; in f { }" (missing "anonymous lambda" "a"),
      fails "an unexpected argument names the function" "let f = { }: 1; in f { a = 1; }" (callFailure "f" "with unexpected argument 'a'"),
      fails "a call with an unexpected argument forces no value of its argument" "({ za }: za) { za = throw \"a\"; zb = throw \"b\"; }" (unexpected "zb"),
      fails
        "an argument that is not a set"
        "({ a }: a) 1"
        (EvalFailure ["from call site", "while evaluating the value passed for the lambda argument"] "expected a set but found an integer: 1"),
      fails "a select into a value that is not a set" "let v = 1; in v.a" (EvalFailure ["while selecting an attribute"] "expected a set but found an integer: 1"),
      fails "a select names the name a set deeper in its path lacks" "{ a = { b = 1; }; }.a.c" (EvalFailure [] "attribute 'c' missing"),
      fails "a select reads a name before forcing the value it selects from" "{ a = throw \"x\"; }.a.${throw \"y\"}" (EvalFailure [] "y"),
      fails "a function is named by its binding as written" "{ \"a b\" = { a }: a; }.\"a b\" { }" (missing "a b" "a"),
      fails "a function bound to the empty name is named by it" "{ \"\" = { a }: a; }.\"\" { }" (missing "" "a"),
      fails "a missing argument prints bare" "({ a'-b }: 1) { }" (missing "anonymous lambda" "a'-b"),
      fails "an unexpected identifier prints bare" "({ }: null) { a'-b = 1; }" (unexpected "a'-b"),
      fails "an unexpected argument that is not an identifier prints as a string" "({ }: null) { \"a b\" = 1; }" (unexpected "\"a b\""),
      fails "an unexpected argument that starts with a digit prints as a string" "({ }: null) { \"1x\" = 1; }" (unexpected "\"1x\""),
      fails "an unexpected argument that is a keyword prints as a string" "({ }: null) { \"if\" = 1; }" (unexpected "\"if\""),
      fails "an unexpected or prints bare, as or is no keyword" "({ }: null) { or = 1; }" (unexpected "or"),
      fails "an unexpected argument past ASCII prints as a string" "({ }: null) { \"\x00e9\" = 1; }" (unexpected "\"\x00e9\""),
      fails
        "an unexpected argument prints with its quote and interpolation escaped"
        "({ }: null) { \"a\\\"b\\${c\\n\" = 1; }"
        (unexpected "\"a\\\"b\\${c\\n\""),
      fails "a select names a missing attribute that is not an identifier as a string" "{ a = 1; }.\"b c\"" (EvalFailure [] "attribute '\"b c\"' missing"),
      fails "a select names a missing keyword attribute as a string" "{ a = 1; }.\"if\"" (EvalFailure [] "attribute '\"if\"' missing"),
      fails "a select names the missing empty attribute as a string" "{ a = 1; }.\"\"" (EvalFailure [] "attribute '\"\"' missing"),
      fails "a select names a missing dynamic attribute as a string" "{ a = 1; }.${\"1x\"}" (EvalFailure [] "attribute '\"1x\"' missing"),
      runTestIO "a select into a value that is not a set takes its default" tmpBase "{ a = 1; }.a.b or 2" (VInt 2),
      runTestM "a set pattern's call fails at the first missing argument, else at the first unexpected one, in symbol order" . quickCheckPasses $
        QC.forAll genCall $ \(formals, ellipsis, args) -> QC.ioProperty $ do
          result <- evalNixIOTraced tmpBase (callSource formals ellipsis args)
          firstMissing <- firstBySymbol [name | (name, False) <- formals, name `notElem` args]
          firstUnexpected <- firstBySymbol [name | name <- args, name `notElem` map fst formals]
          let expected = case (firstMissing, firstUnexpected) of
                (Just name, _) -> Left (missing "anonymous lambda" name)
                (Nothing, Just name) | not ellipsis -> Left (unexpected name)
                _ -> Right (VInt 0)
          pure (result QC.=== expected)
    ]
  where
    quickCheckPasses property = do
      result <- QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} property
      pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))
    -- Formals, each with a default or without, whether an ellipsis
    -- follows them, and the names of the attributes passed, all drawn
    -- from one pool and written in any order.
    genCall = do
      let pool = ["pa", "pb", "pc", "pd", "pe"]
      formals <- QC.sublistOf pool >>= QC.shuffle >>= traverse (\name -> (name,) <$> QC.arbitrary)
      ellipsis <- QC.arbitrary
      args <- QC.sublistOf pool >>= QC.shuffle
      pure (formals, ellipsis, args)
    callSource formals ellipsis args =
      "({ "
        <> T.intercalate ", " ([if hasDefault then name <> " ? 0" else name | (name, hasDefault) <- formals] ++ ["..." | ellipsis])
        <> " }: 0) { "
        <> T.concat [name <> " = 0; " | name <- args]
        <> "}"
    -- The name with the least symbol, the one a set holding them all
    -- holds first.
    firstBySymbol names = fmap snd . listToMaybe . sortOn fst <$> traverse (\name -> (,name) <$> symbolIntern name) names

-- | @deepSeq@, @trace@ and @warn@ finish with their first argument before
-- they force their second (primops.cc at 2.24.9), however the second
-- arrives.  Each expected stream is nix-instantiate 2.24.9's: its trace
-- and warning lines, then the final error line.
testFirstArgumentOrderIO :: IO [Bool]
testFirstArgumentOrderIO = do
  putStrLn "eval/first-argument-order-io"
  sequence
    [ emits "trace prints before its second argument fails" "builtins.trace \"hi\" (throw \"b\")" ["trace: hi", "error: b"],
      emits "warn warns before its second argument fails" "builtins.warn \"w\" (throw \"b\")" ["evaluation warning: w", "error: b"],
      emits "deepSeq forces its first argument deeply before its second" "builtins.deepSeq { a = throw \"a\"; } (throw \"b\")" ["error: a"],
      emits "deepSeq applied to one argument first forces it on the second" "let f = builtins.deepSeq { a = throw \"a\"; }; in f (throw \"b\")" ["error: a"],
      emits "deepSeq mapped over a list forces its first argument first" "builtins.elemAt (map (builtins.deepSeq { a = throw \"a\"; }) [ (throw \"b\") ]) 0" ["error: a"],
      emits "trace inside deepSeq sees the forced value" "let x = { a = 1 + 1; }; in builtins.deepSeq x (builtins.trace x 1)" ["trace: { a = 2; }"]
    ]

-- | An empty hash is the all-zero digest of the algorithm its context
-- gives, which upstream warns of on stderr as it reads the hash
-- (newHashAllowEmpty in hash.cc): a fixed-output derivation's, and
-- the sha256 pin of fetchurl, fetchTarball and builtins.path, whose
-- copy then fails as upstream's does on a pin it does not match
-- (@fetch@ in fetchTree.cc, @addPath@ in primops.cc).  Each expected
-- stream is read from upstream's source at 2.24.9.
testEmptyHashIO :: IO [Bool]
testEmptyHashIO = do
  putStrLn "eval/empty-hash-io"
  tmpBase <- getTemporaryDirectory >>= Dir.canonicalizePath
  environment <- getEnvironment
  let root = tmpBase </> "nova-nix-test-empty-hash"
      file = root </> "data.txt"
      tree = root </> "tree"
      payload = "payload\n"
      tarballData = "data\n"
      pathText = canonPathValue . T.pack
      fileUrl p = "file://" <> pathText p
      warning algo zero = "warning: found empty hash, assuming '" <> algo <> "-" <> zero <> "'"
      sha256Warning = warning "sha256" (T.replicate 43 "A" <> "=")
      drvPath fields = "(derivation { name = \"t\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; " <> fields <> " }).drvPath"
      fetchMismatch url got =
        [ "error: hash mismatch in file downloaded from '" <> url <> "':",
          "  specified: sha256:" <> T.replicate 52 "0",
          "  got:       sha256:" <> Nix32.encode got
        ]
      pathMismatch = "error: store path mismatch in (possibly filtered) path added from '" <> pathText tree <> "'"
      tarballLabel = "an empty fetchTarball pin warns, then the extracted tree's hash is reported"
      -- Written from the fixture root, so tar is given no drive letter.
      tarProcess =
        (Proc.proc "tar" ["-czf", "tree.tar.gz", "-C", "stage", "top"])
          { Proc.cwd = Just root,
            Proc.env = Just (("COPYFILE_DISABLE", "1") : environment)
          }
      tarballCase
        | SI.os == "mingw32" = runTest (tarballLabel <> " (skipped: fetchTarball extracts through sh, which nova-nix does not assume on Windows)") Pass
        | otherwise = do
            (code, _, err) <- Proc.readCreateProcessWithExitCode tarProcess ""
            case code of
              ExitFailure n -> runTest tarballLabel (Fail ("tar exited " <> T.pack (show n) <> ": " <> T.pack err))
              ExitSuccess ->
                emits
                  tarballLabel
                  ("builtins.fetchTarball { url = \"" <> fileUrl (root </> "tree.tar.gz") <> "\"; sha256 = \"\"; }")
                  (sha256Warning : fetchMismatch (fileUrl (root </> "tree.tar.gz")) (sha256Digest (NAR.serialise (NAR.NarDirectory [("data", NAR.NarRegular False tarballData)]))))
  bracket_
    ( do
        forceRemoveIfExists root
        createDirectoryIfMissing True tree
        createDirectoryIfMissing True (root </> "stage" </> "top")
        BS.writeFile file payload
        BS.writeFile (tree </> "f") "tree\n"
        BS.writeFile (root </> "stage" </> "top" </> "data") tarballData
    )
    (forceRemoveIfExists root)
    $ sequence
      [ emits "an empty outputHash warns, naming the all-zero hash" (drvPath "outputHash = \"\"; outputHashAlgo = \"sha256\";") [sha256Warning],
        emits "the all-zero hash is outputHashAlgo's" (drvPath "outputHash = \"\"; outputHashAlgo = \"sha512\";") [warning "sha512" (T.replicate 86 "A" <> "==")],
        emits "an empty outputHash with no algorithm is refused without a warning" (drvPath "outputHash = \"\";") ["error: empty hash requires explicit hash algorithm"],
        emits
          "an empty fetchurl pin warns, then the file's hash is reported"
          ("builtins.fetchurl { url = \"" <> fileUrl file <> "\"; sha256 = \"\"; }")
          (sha256Warning : fetchMismatch (fileUrl file) (sha256Digest payload)),
        tarballCase,
        emits
          "an empty builtins.path pin warns, then the copy is refused"
          ("builtins.path { path = " <> pathText tree <> "; sha256 = \"\"; }")
          [sha256Warning, pathMismatch],
        emits
          "a filtered builtins.path is refused in the same words"
          ("builtins.path { path = " <> pathText tree <> "; filter = p: t: true; sha256 = \"\"; }")
          [sha256Warning, pathMismatch],
        -- The other warning derivationStrict gives, through the same
        -- logger (derivationStrictInternal in primops.cc at 2.24.9).
        emits
          "a structured derivation warns of a check it disables"
          (drvPath "__structuredAttrs = true; allowedReferences = [ ];")
          ["warning: In a derivation named 't', 'structuredAttrs' disables the effect of the derivation attribute 'allowedReferences'; use 'outputChecks.<output>.allowedReferences' instead"]
      ]

-- | Without @trace-verbose@, upstream binds @builtins.traceVerbose@ to
-- @prim_second@ (primops.cc at 2.24.9), which forces and returns its
-- second argument and never forces its first, whether it is applied
-- directly, through a partial application, or to an element a
-- higher-order builtin passes unforced.  The pure cases evaluate under
-- the default policy; the streams are what an IO evaluation writes to
-- stderr, where a trace would go.
testTraceVerboseOff :: IO [Bool]
testTraceVerboseOff = do
  putStrLn "eval/trace-verbose-off"
  sequence
    [ runTest "traceVerbose returns its second argument without forcing its first" $
        assertEval "direct" "builtins.traceVerbose (throw \"a\") 1" (VInt 1),
      runTest "traceVerbose applied to one argument is a partial application, its argument unforced" $
        assertEvalError
          "partial"
          "{} // builtins.traceVerbose (throw \"a\")"
          "expected a set but found the partially applied built-in function 'traceVerbose': \x00ABpartially applied primop traceVerbose\x00BB",
      runTest "traceVerbose's partial application mapped over a list leaves its argument unforced" $
        assertEval "mapped" "builtins.elemAt (map (builtins.traceVerbose (throw \"a\")) [ 1 ]) 0" (VInt 1),
      runTest "traceVerbose given an unforced list element as its first argument leaves it unforced" $
        assertEval "unforced-element" "builtins.foldl' (acc: builtins.traceVerbose) 0 [ (throw \"a\") ] 5" (VInt 5),
      runTest "traceVerbose forces its second argument" $
        assertEvalError "second" "builtins.traceVerbose 1 (throw \"b\")" "b",
      emits "traceVerbose prints nothing" "builtins.traceVerbose \"hi\" 1" [],
      emits "traceVerbose prints nothing before its second argument fails" "builtins.traceVerbose \"hi\" (throw \"b\")" ["error: b"]
    ]

-- ---------------------------------------------------------------------------
-- Tests: upstream conformance cases
-- ---------------------------------------------------------------------------

-- | Error kind for 'StubStoreEval', mirroring PureEval's split: only a
-- throw/assert is tryEval-catchable.
data StubErr = StubThrow !Text | StubOther !Text

-- | Pure evaluator over a stubbed store: 'readStoreDerivation' serves
-- derivations from a fixed map (keyed by canonical @.drv@ path text);
-- everything else behaves like 'PureEval'.  Exercises the
-- input-derivation-modulo STORE-READ arm hermetically - the real arm
-- reads the platform store, which dev machines and CI runners must not
-- depend on (the build matrix has no writable @/nix/store@).  Like
-- 'PureEval', its one effect is the C data layer.
newtype StubStoreEval a = StubStoreEval (StubEnv -> CData (Either StubErr a))

-- | What the stubbed store answers from.
data StubEnv = StubEnv
  { -- | Derivations 'readStoreDerivation' serves, by canonical @.drv@ path.
    seDrvs :: !(Map.Map Text Derivation),
    -- | Recorded fetches 'lookupFetchCache' serves, by key.
    seFetchCache :: !(Map.Map Text Text),
    -- | Store paths 'adoptStorePath' accepts.  Deliberately NOT the same
    -- answer as 'doesPathExist', which is 'False' throughout: on disk and
    -- registered are different facts, and a test where the two coincide
    -- cannot tell which one the code under test asked about.
    seAdoptable :: !(Set.Set Text),
    -- | The store directory 'qualifyPath' names paths against.
    seStoreDir :: !StoreDir
  }

-- | A stub store holding nothing but the given derivations, at the
-- default store directory.
stubEnv :: Map.Map Text Derivation -> StubEnv
stubEnv drvs = StubEnv {seDrvs = drvs, seFetchCache = Map.empty, seAdoptable = Set.empty, seStoreDir = defaultStoreDir}

-- | Run a stub evaluation as a value, as 'runPureEval' runs a pure one.
runStubStoreEvalWith :: StubEnv -> StubStoreEval a -> Either Text a
runStubStoreEvalWith env (StubStoreEval action) = case runCDataPure (action env) of
  Left (StubThrow msg) -> Left msg
  Left (StubOther msg) -> Left msg
  Right val -> Right val

-- | An answer read off the stub store.
stubAnswer :: (StubEnv -> a) -> StubStoreEval a
stubAnswer answer = StubStoreEval (pure . Right . answer)

-- | A failed stub evaluation.
stubFailure :: StubErr -> StubStoreEval a
stubFailure failure = StubStoreEval (const (pure (Left failure)))

instance Functor StubStoreEval where
  fmap f (StubStoreEval g) = StubStoreEval (fmap (fmap f) . g)

instance Applicative StubStoreEval where
  pure val = stubAnswer (const val)
  StubStoreEval mf <*> StubStoreEval ma = StubStoreEval $ \env ->
    mf env >>= either (pure . Left) (\f -> fmap f <$> ma env)

instance Monad StubStoreEval where
  StubStoreEval ma >>= f = StubStoreEval $ \env ->
    ma env >>= \case
      Left err -> pure (Left err)
      Right val -> let StubStoreEval mb = f val in mb env

instance MonadCData StubStoreEval where
  liftCData act = StubStoreEval (const (Right <$> act))

instance MonadEval StubStoreEval where
  throwEvalError msg = stubFailure (StubOther msg)
  throwCatchableError msg = stubFailure (StubThrow msg)
  abortEvaluation msg = stubFailure (StubOther msg)
  catchEvalError (StubStoreEval action) = StubStoreEval (fmap caught . action)
    where
      caught (Left (StubThrow msg)) = Right (Left msg)
      caught (Left other) = Left other
      caught (Right val) = Right (Right val)
  onEvalError (StubStoreEval action) _ = StubStoreEval action
  addEvaluatedTrace _ (StubStoreEval line) (StubStoreEval action) = StubStoreEval $ \env ->
    action env >>= \case
      Left failure -> Left failure <$ line env
      Right val -> pure (Right val)

  -- The stub keeps no call depth: it exists to answer store questions,
  -- and the ceiling is tested on the evaluators that enforce it.
  withCallFrame (StubStoreEval action) = StubStoreEval action
  doesPathExist _ _ = pure False
  listDirectory _ = throwEvalError "readDir: not available in the stub evaluator"
  importFile _ = throwEvalError "import: not available in the stub evaluator"
  getEnvVar _ = pure ""
  getCurrentTime = pure 0
  writeToStore _ _ _ = throwEvalError "toFile: not available in the stub evaluator"
  scopedImportFile _ _ = throwEvalError "scopedImport: not available in the stub evaluator"
  readFileBytes _ = throwEvalError "readFile: not available in the stub evaluator"
  getFileType _ = throwEvalError "readFileType: not available in the stub evaluator"
  runProcess _ _ _ = throwEvalError "runProcess: not available in the stub evaluator"
  createScratchDir _ = throwEvalError "createScratchDir: not available in the stub evaluator"
  removeScratchDir _ = pure ()
  copyPathToStore _ _ _ _ = throwEvalError "builtins.path: not available in the stub evaluator"
  narHashOfPath _ = throwEvalError "builtins.fetchGit: not available in the stub evaluator"
  setExecutableFile _ = throwEvalError "builtins.fetchGit: not available in the stub evaluator"
  lookupFetchCache key = stubAnswer (Map.lookup key . seFetchCache)
  adoptStorePath path = stubAnswer (Set.member path . seAdoptable)
  writeFetchCache _ _ = pure ()
  isExecutableFile _ = throwEvalError "builtins.path: not available in the stub evaluator"
  readSymlinkTarget _ = throwEvalError "builtins.path: not available in the stub evaluator"
  resolveSymlinks = pure
  addSourceNar _ _ = throwEvalError "builtins.path: not available in the stub evaluator"
  addFixedOutputFile _ _ _ = throwEvalError "builtins.fetchurl: not available in the stub evaluator"
  pinnedFetchHeld _ = pure False
  traceMessage _ = pure ()
  lookupDrvHash _ = pure Nothing
  cacheDrvHash _ _ = pure ()
  recordDrvAterm _ _ = pure ()
  readStoreDerivation sp = stubAnswer (Map.lookup (storePathToText defaultStoreDir sp) . seDrvs)
  lookupSessionDrv _ = pure Nothing
  lookupSessionReferences _ = pure Nothing
  realiseContext _ = pure (Right ())
  storeSourcePath = pure
  evalPolicy = pure unrestrictedPolicy
  checkUri _ = pure ()
  qualifyPath path = stubAnswer ((`qualifyPathText` path) . seStoreDir)
  resolvePathLiteral = pure . canonPath
  forceThunk evalFn (Thunk ptr) =
    cthunkState ptr >>= \case
      ThunkComputed -> readComputed ptr
      ThunkBlackhole -> abortEvaluation "infinite recursion encountered"
      _ {- PENDING -} -> readPending ptr >>= evalFn

-- | Parse and evaluate under the stubbed-store evaluator.
evalNixStub :: Map.Map Text Derivation -> Text -> Either Text NixValue
evalNixStub = evalNixStubWith . stubEnv

-- | 'evalNixStub' with the whole stub store spelled out.
evalNixStubWith :: StubEnv -> Text -> Either Text NixValue
evalNixStubWith env source = case parseNix testBaseDir testOrigin (TE.encodeUtf8 source) of
  Left err -> Left (T.pack (show err))
  Right parsed -> runStubStoreEvalWith env (evalParsed (builtinEnv unrestrictedPolicy 0 []) parsed)

-- | Upstream conformance cases with no direct test elsewhere.  Pure
-- cases here; filesystem-touching cases in 'testClassIFollowupsIO'.
testClassIFollowups :: IO [Bool]
testClassIFollowups = do
  putStrLn "eval/class-i-followups"
  let storeA = "/nix/store/" <> T.replicate 32 "a"
      enclosingCtx = StringContext (Set.singleton (SCPlain (StorePath (T.replicate 32 "a") "x")))
  sequence
    [ -- path + path concatenates the spellings and canonicalizes
      runTest "path + path canonicalizes the joined spelling" $
        assertEval "pp-canon" "builtins.toString (/a/b + /c/../d)" (mkStr "/a/b/d"),
      -- storePath on a subpath: value text is the subpath itself
      runTest "storePath on a subpath keeps text, contexts the enclosing path" $
        assertEval
          "storepath-sub"
          ("builtins.storePath " <> storeA <> "-x/sub/file")
          (VStr (TE.encodeUtf8 (storeA <> "-x/sub/file")) enclosingCtx),
      runTest "fromJSON falls to float past uint64" $
        assertEval "fromjson-float" "builtins.fromJSON \"18446744073709551616\"" (VFloat 18446744073709551616.0),
      -- the input-derivation-modulo STORE-READ arm: a dependent whose
      -- input .drv was NOT evaluated in-session resolves by reading the
      -- (stubbed) store, and lands on the same drvPath as the in-session
      -- cache-hit arm computes for the identical derivation
      runTestM "input modulo recurses through the store read (stub store)" $ do
        let depSrc = "derivation { name = \"dep\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; }"
        tmpBase <- getTemporaryDirectory
        depDrv <- evalDerivationIO tmpBase depSrc >>= either (fail . ("dep: " <>) . T.unpack) pure
        depPath <- case evalNix ("(" <> depSrc <> ").drvPath") of
          Right (VStr p _) -> pure (bytesText p)
          other -> fail ("dep.drvPath: " <> show other)
        let stubResult =
              evalNixStub (Map.singleton depPath depDrv) $
                T.concat
                  [ "(derivation { name = \"main\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; ",
                    "input = builtins.appendContext \"x\" { \"",
                    depPath,
                    "\" = { outputs = [\"out\"]; }; }; }).drvPath"
                  ]
        ioResult <-
          evalNixIO tmpBase $
            T.concat
              [ "let dep = ",
                depSrc,
                "; in (derivation { name = \"main\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; ",
                -- An attribute name may not carry context, so the key
                -- is the path's text alone, as nixpkgs writes it.
                "input = builtins.appendContext \"x\" { \"${builtins.unsafeDiscardStringContext dep.drvPath}\" = { outputs = [\"out\"]; }; }; }).drvPath"
              ]
        pure $ case (stubResult, ioResult) of
          (Right (VStr a _), Right (VStr b _))
            | a == b -> Pass
            | otherwise ->
                Fail ("store-read and in-session arms disagree: " <> bytesText a <> " vs " <> bytesText b)
          other -> Fail ("expected two drvPaths, got: " <> T.pack (show other))
    ]

-- | Filesystem-touching upstream conformance cases.
testClassIFollowupsIO :: IO [Bool]
testClassIFollowupsIO = do
  putStrLn "eval/class-i-followups-io"
  tmpDir <- getTemporaryDirectory
  let testDir = tmpDir </> "nova-nix-classi-test"
      testDirFwd = T.replace "\\" "/" (T.pack testDir)
  bracket_
    ( do
        createDirectoryIfMissing True (testDir </> "sub")
        TIO.writeFile (testDir </> "target.txt") "hit\n"
        TIO.writeFile (testDir </> "thefile") "payload\n"
        -- Imported files for the two path-resolution tests below.  Each is
        -- a directory deeper than the expression that names it, so a
        -- literal resolved against the wrong one is visibly wrong.
        TIO.writeFile (testDir </> "sub" </> "useq.nix") "q\n"
        TIO.writeFile (testDir </> "sub" </> "tilde.nix") "~/nova-tilde-probe\n"
    )
    ( do
        exists <- doesDirectoryExist testDir
        when exists (removeDirectoryRecursive testDir)
    )
    $ sequence
      [ -- ~/ expands against the home directory at path resolution
        runTestM "tilde path literal expands against the home directory" $ do
          home <- Dir.getHomeDirectory
          result <- evalNixIO testDir "builtins.toString ~/nova-tilde-probe"
          let expected = canonPathValue (T.pack (home </> "nova-tilde-probe"))
          pure $ assertRight "tilde" result $ \val ->
            assertEqual "tilde-expanded" (mkStr expected) val,
        -- The same literal inside an imported file.  Path resolution runs
        -- over every parsed file now, and joining ~/x to the importing
        -- directory would bury the tilde where nothing expands it.
        runTestM "tilde path literal expands the same inside an imported file" $ do
          home <- Dir.getHomeDirectory
          result <- evalNixIO testDir "builtins.toString (import ./sub/tilde.nix)"
          let expected = canonPathValue (T.pack (home </> "nova-tilde-probe"))
          pure $ assertRight "tilde-import" result $ \val ->
            assertEqual "tilde-expanded-in-import" (mkStr expected) val,
        -- A path literal names a location relative to the file it is
        -- written in.  This one is written here and forced during the
        -- evaluation of sub/useq.nix, where the base directory is sub/:
        -- resolving on force rather than on parse gave sub/target.txt.
        runTestM "a path literal resolves where it is written, not where it is forced" $ do
          let expr = "builtins.toString (builtins.scopedImport { q = ./target.txt; } ./sub/useq.nix)"
          result <- evalNixIO testDir expr
          let expected = canonPathValue (T.pack (testDir </> "target.txt"))
          pure $ assertRight "scoped-import-path" result $ \val ->
            assertEqual "resolved-against-writing-file" (mkStr expected) val,
        -- a search-path match is returned CANONICALIZED
        runTestM "findFile canonicalizes the matched candidate" $ do
          result <-
            evalNixIO testDir $
              T.concat
                ["builtins.findFile [ { prefix = \"\"; path = \"", testDirFwd, "/sub/..\"; } ] \"target.txt\""]
          pure $ assertRight "findfile-canon" result $ \val ->
            assertEqual "canonical" (VPath (testDirFwd <> "/target.txt")) val,
        -- the store copy is named by canonBaseName (the path's last segment)
        runTestM "source copy is named by canonBaseName" $ do
          result <- evalNixIO testDir "\"${./thefile}\""
          pure $ assertRight "canonbase" result $ \case
            VStr s ctx ->
              if "/nix/store/" `BS.isPrefixOf` s && "-thefile" `BS.isSuffixOf` s && ctx /= emptyContext
                then Pass
                else Fail ("expected a store path named -thefile with context, got " <> bytesText s)
            other -> Fail ("expected VStr, got " <> T.pack (show other)),
        -- canonBaseName's empty-name arm: the filesystem root has no
        -- base name to copy under, refused before any read.  POSIX only:
        -- a root path literal on Windows names a drive.
        if SI.os == "mingw32"
          then True <$ putStrLn "  SKIP  coercing the filesystem root errors (no base name): the root is a drive"
          else runTestM "coercing the filesystem root errors (no base name)" $ do
            result <- evalNixIO testDir "\"${/.}\""
            pure $ case result of
              Left err | "no base name" `T.isInfixOf` err -> Pass
              Left err -> Fail ("expected a base-name error, got: " <> err)
              Right val -> Fail ("expected failure, got " <> T.pack (show val))
      ]

-- | A path value inside a store path is copied under its base name, as
-- every path value is (upstream's copyPathToStore in eval.cc), never
-- coerced to itself.  The tree sits in a temporary store the way a
-- fetch or a channel leaves one, so the copy is read through the store
-- this evaluation was given: store text names the canonical directory,
-- where nothing is.  Each expected path is derived from the NAR the
-- copy must hash, so the base name and the bytes read are both pinned.
testInStoreCopyIO :: IO [Bool]
testInStoreCopyIO = do
  putStrLn "eval/in-store-copy-io"
  tmpBase <- getTemporaryDirectory
  shell <- findTestShell
  canLink <- symlinksAvailable
  let evalStore = tmpBase </> "nova-nix-test-in-store-copy"
      linkedStore = tmpBase </> "nova-nix-test-in-store-copy-linked"
      hostLinks = tmpBase </> "nova-nix-test-in-store-copy-host"
      linkSources = tmpBase </> "nova-nix-test-in-store-copy-link-sources"
      restoreStore = tmpBase </> "nova-nix-test-in-store-copy-restore"
      buildStore = tmpBase </> "nova-nix-test-in-store-copy-build"
      treeBase = T.replicate 32 "a" <> "-tree"
      fileBase = T.replicate 32 "b" <> "-small"
      otherBase = T.replicate 32 "c" <> "-other"
      srcBase = T.replicate 32 "d" <> "-src"
      inStore base = defaultStoreDirText <> "/" <> base
      onHost name = canonPathValue (T.pack (hostLinks </> "hl" </> name))
      tree = defaultStoreDirText <> "/" <> treeBase
      small = defaultStoreDirText <> "/" <> fileBase
      inTree name = tree <> "/" <> name
      dataContents = "in the store\n"
      smallContents = "small\n"
      srcFileContents = "reached through a host link\n"
      innerContents = "below a link\n"
      otherContents = "in another store path\n"
      scriptContents = "cp \"$1\" \"$out\"\n"
      caseHackName = "foo~nix~case~hack~1"
      nixEscape = T.concatMap (\c -> if c == '\\' then "\\\\" else if c == '"' then "\\\"" else T.singleton c)
      -- The tree a fetch would leave in the store: a file, a link to it
      -- where the platform makes links, a script the store holds
      -- executable, a directory holding a case-hacked sibling, and files
      -- that name the others by path literals.
      placeTree storeDir = do
        forceRemoveIfExists storeDir
        let dir = storeDir </> T.unpack treeBase
        createDirectoryIfMissing True (dir </> "cased")
        BS.writeFile (dir </> "data.txt") dataContents
        createDirectoryIfMissing True (dir </> "sub")
        BS.writeFile (dir </> "sub" </> "inner.txt") innerContents
        createDirectoryIfMissing True (storeDir </> T.unpack otherBase)
        BS.writeFile (storeDir </> T.unpack otherBase </> "f") otherContents
        when canLink $ do
          Dir.createFileLink "data.txt" (dir </> "link")
          Dir.createDirectoryLink "sub" (dir </> "dirlink")
          -- Absolute, as a NAR carries a link into another store path: it
          -- names the canonical directory, wherever the store is.
          Dir.createDirectoryLink (T.unpack (defaultStoreDirText <> "/" <> otherBase)) (dir </> "abslink")
          -- A store path a host link leads into, holding a relative link
          -- and absolute ones into another store path.
          let src = storeDir </> T.unpack srcBase
          createDirectoryIfMissing True src
          BS.writeFile (src </> "file.txt") srcFileContents
          Dir.createFileLink "file.txt" (src </> "rel")
          Dir.createDirectoryLink (T.unpack (inStore otherBase)) (src </> "abs")
          Dir.createFileLink (T.unpack (inStore otherBase <> "/f")) (src </> "absf")
        BS.writeFile (dir </> "run.sh") scriptContents
        ExecBit.markExecutable (dir </> "run.sh")
        BS.writeFile (dir </> "cased" </> "Foo") "upper"
        BS.writeFile (dir </> "cased" </> caseHackName) "lower"
        BS.writeFile (dir </> "use.nix") "\"${./data.txt}\"\n"
        BS.writeFile
          (dir </> "buildlink.nix")
          "shell: derivation { name = \"in-store-link\"; system = builtins.currentSystem; builder = shell; args = [ \"-c\" \"cp $src $out\" ]; src = ./abslink/f; }\n"
        BS.writeFile
          (dir </> "build.nix")
          "shell: derivation { name = \"in-store-build\"; system = builtins.currentSystem; builder = shell; args = [ ./run.sh ./data.txt ]; }\n"
        BS.writeFile (storeDir </> T.unpack fileBase) smallContents
      copyOf name entry = either (fail . show) pure (makeFixedOutputPath name "sha256" NarIngestion (sha256Digest (NAR.serialise entry)))
      -- What a build gave: its output checked when it succeeded.
      builtFrom storeDir result check = case result of
        Left err -> pure (Fail err)
        Right (BuildFailure msg code, store) -> Fail ("build failed (" <> T.pack (show code) <> "): " <> msg) <$ closeStore store
        Right (BuildSuccess out, store) -> check (storePathToFilePath (StoreDir storeDir) out) <* closeStore store
      -- A build whose source is the named source expression over a tree
      -- outside the store, which holds a directory, a file and a link to
      -- each.
      buildsFromLink srcOf builderScript check = do
        forceRemoveIfExists linkSources
        forceRemoveIfExists buildStore
        createDirectoryIfMissing True (linkSources </> "real")
        BS.writeFile (linkSources </> "real" </> "f") "in a directory\n"
        BS.writeFile (linkSources </> "file.txt") "a file\n"
        Dir.createDirectoryLink "real" (linkSources </> "dirlink")
        Dir.createFileLink "file.txt" (linkSources </> "filelink")
        result <-
          evalAndBuild (StoreDir buildStore) $
            T.concat
              [ "derivation { name = \"link-source\"; system = builtins.currentSystem; builder = \"" <> nixEscape shell <> "\"; ",
                "args = [ \"-c\" \"" <> builderScript <> "\" ]; src = " <> srcOf (canonPathValue (T.pack linkSources)) <> "; }"
              ]
        outcome <- builtFrom buildStore result check
        forceRemoveIfExists linkSources
        forceRemoveIfExists buildStore
        pure outcome
      inEval label source expected = runTestM label $ do
        result <- evalNixIOStore (StoreDir evalStore) tmpBase source
        pure (assertRight label result (assertEqual label expected))
      -- The same evaluation through a store directory that is a symlink
      -- to the evaluation's store.
      inLinkedStore label source expected = runTestM label $ do
        forceRemoveIfExists linkedStore
        Dir.createDirectoryLink evalStore linkedStore
        result <- evalNixIOStore (StoreDir linkedStore) tmpBase source
        Dir.removeDirectoryLink linkedStore
        pure (assertRight label result (assertEqual label expected))
  placeTree evalStore
  -- A folding volume holds the case-variant sibling under the suffix and
  -- reads it back without it, as the store reads its own paths.
  sensitivity <- probeCaseSensitivity evalStore
  let foldedName = if volumeCaseHack sensitivity == NAR.CaseHackEnabled then "foo" else BS8.pack caseHackName
  dataCopy <- copyOf "data.txt" (NAR.NarRegular False dataContents)
  smallCopy <- copyOf fileBase (NAR.NarRegular False smallContents)
  scriptCopy <- copyOf "run.sh" (NAR.NarRegular True scriptContents)
  casedCopy <- copyOf "cased" (NAR.NarDirectory [("Foo", NAR.NarRegular False "upper"), (foldedName, NAR.NarRegular False "lower")])
  -- The link's copy is the file it points to under the link's name.
  -- A coerced link's copy is the link; builtins.path's is what it
  -- points to, under the link's name.
  linkCopy <- copyOf "link" (NAR.NarSymlink "data.txt")
  linkTargetCopy <- copyOf "link" (NAR.NarRegular False dataContents)
  innerCopy <- copyOf "inner.txt" (NAR.NarRegular False innerContents)
  otherCopy <- copyOf "f" (NAR.NarRegular False otherContents)
  treePath <- maybe (fail "the tree names no store path") pure (parseStorePath defaultStoreDir tree)
  otherPath <- maybe (fail "no store path for the other tree") pure (parseStorePath defaultStoreDir (inStore otherBase))
  -- What builtins.path copies through a host link into the store: the
  -- target, read on in the store nova-nix was given.
  absCopy <- copyOf "abs" (NAR.NarDirectory [("f", NAR.NarRegular False otherContents)])
  relCopy <- copyOf "rel" (NAR.NarRegular False srcFileContents)
  subCopy <- copyOf "sub" (NAR.NarDirectory [("inner.txt", NAR.NarRegular False innerContents)])
  absfFlatCopy <- either (fail . show) pure (makeFixedOutputPath "absf" "sha256" FlatIngestion (sha256Digest otherContents))
  when canLink $ do
    forceRemoveIfExists hostLinks
    createDirectoryIfMissing True hostLinks
    Dir.createDirectoryLink (T.unpack (inStore srcBase)) (hostLinks </> "hl")
  let dataCopyText = storePathToText defaultStoreDir dataCopy
      -- 2.24.9's copyPathToStore and both tags' addPath resolve the path in
      -- full, and storePath resolves a path inside a store path: a link
      -- inside the store is followed as one outside it is.
      linkTests
        | canLink =
            [ inEval "a link inside a store path is copied as the link" ("\"${" <> inTree "link" <> "}\"") (storePathStringValue linkCopy),
              inEval "a coerced path inside a store path follows the links among its ancestors" ("\"${" <> inTree "dirlink/inner.txt" <> "}\"") (storePathStringValue innerCopy),
              inEval
                "an absolute link among a coerced path's ancestors is followed in the store nova-nix was given"
                ("\"${" <> inTree "abslink/f" <> "}\"")
                (storePathStringValue otherCopy),
              inEval "builtins.path copies what a link inside a store path points to" ("builtins.path { path = " <> inTree "link" <> "; }") (storePathStringValue linkTargetCopy),
              inEval
                "storePath names what a link inside a store path points to"
                ("builtins.storePath " <> inTree "link")
                (VStr (TE.encodeUtf8 (inTree "data.txt")) (StringContext (Set.singleton (SCPlain treePath)))),
              -- The store directory stands for the store wherever it is,
              -- so following links never leaves store text for the
              -- directory a symlinked store resolves to.
              inLinkedStore
                "a store directory that is a symlink keeps resolved store text in the store"
                ("builtins.storePath " <> inTree "link")
                (VStr (TE.encodeUtf8 (inTree "data.txt")) (StringContext (Set.singleton (SCPlain treePath)))),
              -- Upstream's store accessor looks for links only beneath the
              -- store directory, so a symlinked one is no symlinked ancestor.
              inLinkedStore "readFileType in a store directory that is a symlink" ("builtins.readFileType " <> inTree "data.txt") (mkStr "regular"),
              inLinkedStore "filterSource in a store directory that is a symlink" ("builtins.filterSource (p: t: true) " <> inTree "sub") (storePathStringValue subCopy),
              inLinkedStore
                "builtins.path with a filter in a store directory that is a symlink"
                ("builtins.path { path = " <> inTree "sub" <> "; filter = p: t: true; }")
                (storePathStringValue subCopy),
              -- A host link into the store is followed on inside the store
              -- nova-nix was given, as upstream resolves over the union of
              -- the host's file system and the store's.
              inEval "builtins.path follows a host link into the store and an absolute link inside it" ("builtins.path { path = " <> onHost "abs" <> "; }") (storePathStringValue absCopy),
              inEval "builtins.path follows a host link into the store and a relative link inside it" ("builtins.path { path = " <> onHost "rel" <> "; }") (storePathStringValue relCopy),
              inEval "filterSource follows a host link into the store and the links inside it" ("builtins.filterSource (p: t: true) " <> onHost "abs") (storePathStringValue absCopy),
              inEval
                "storePath follows a host link into the store and the links inside it"
                ("builtins.storePath " <> onHost "abs")
                (VStr (TE.encodeUtf8 (inStore otherBase)) (StringContext (Set.singleton (SCPlain otherPath)))),
              inEval
                "a flat builtins.path follows a host link into the store to a file in another store path"
                ("builtins.path { path = " <> onHost "absf" <> "; recursive = false; }")
                (storePathStringValue absfFlatCopy),
              -- The copy records the tree evaluation hashed for it, the
              -- path with the links among its ancestors resolved, and is
              -- restored from there: through an absolute link that is
              -- another store path in the store nova-nix was given.
              runTestM "a copy records the tree it was hashed from" $ do
                st <- testEvalState (StoreDir evalStore) tmpBase
                result <- evalNixIOIn st tmpBase ("\"${" <> inTree "abslink/f" <> "}\"")
                recorded <- sourceCopies <$> readIORef (esSourcePathCache st)
                let expected = Map.singleton otherCopy (defaultStoreDirText <> "/" <> otherBase <> "/f")
                pure (assertRight "recorded-copy" result (const (assertEqual "recorded-copy" expected recorded))),
              runTestM "e2e a build naming a path below an absolute link inside a store path restores it from the store" $ do
                placeTree buildStore
                result <- evalAndBuild (StoreDir buildStore) ("import " <> inTree "buildlink.nix" <> " \"" <> nixEscape shell <> "\"")
                outcome <- builtFrom buildStore result $ fmap (assertEqual "output" otherContents) . BS.readFile
                forceRemoveIfExists buildStore
                pure outcome,
              -- A source that is a link is copied as the link and builds:
              -- the builder sees a link where its source was.
              runTestM "e2e a source that is a link to a directory builds"
                $ buildsFromLink (<> "/dirlink") "if [ -L $src ]; then echo link > $out; fi"
                $ fmap (assertEqual "output" "link\n") . BS.readFile,
              runTestM "e2e a source that is a link to a file builds"
                $ buildsFromLink (<> "/filelink") "if [ -L $src ]; then echo link > $out; fi"
                $ fmap (assertEqual "output" "link\n") . BS.readFile,
              -- builtins.path copies what the link points to, a copy
              -- evaluation writes itself.
              runTestM "e2e a source builtins.path copies through a link builds from what it points to" $
                buildsFromLink (\dir -> "builtins.path { path = " <> dir <> "/dirlink; }") "cp -r $src $out" $
                  \out -> assertEqual "output" "in a directory\n" <$> BS.readFile (out </> "f")
            ]
        | otherwise = [True <$ putStrLn "  SKIP  links inside a store path: this platform makes no symlinks here"]
  results <-
    sequence $
      linkTests
        <> [ inEval "interpolating a path inside a store path gives its copy" ("\"${" <> inTree "data.txt" <> "}\"") (storePathStringValue dataCopy),
             inEval "a path that is exactly a store path is copied under its hash and name" ("\"${" <> small <> "}\"") (storePathStringValue smallCopy),
             inEval
               "concatStringsSep copies a path inside a store path"
               ("builtins.concatStringsSep \":\" [ " <> inTree "data.txt" <> " \"y\" ]")
               (VStr (TE.encodeUtf8 (dataCopyText <> ":y")) (StringContext (Set.singleton (SCPlain dataCopy)))),
             inEval
               "toJSON copies a path inside a store path"
               ("builtins.toJSON [ " <> inTree "data.txt" <> " ]")
               (VStr (TE.encodeUtf8 ("[\"" <> dataCopyText <> "\"]")) (StringContext (Set.singleton (SCPlain dataCopy)))),
             inEval "a path literal in a file imported from the store is copied" ("import " <> inTree "use.nix") (storePathStringValue dataCopy),
             inEval
               "substring keeps the copy's context"
               ("builtins.substring 0 10 " <> inTree "data.txt")
               (VStr "/nix/store" (StringContext (Set.singleton (SCPlain dataCopy)))),
             inEval "unsafeDiscardOutputDependency keeps the copy's context" ("builtins.unsafeDiscardOutputDependency " <> inTree "data.txt") (storePathStringValue dataCopy),
             runTestM "addDrvOutputDependencies judges a path inside a store path by its copy" $ do
               result <- evalNixIOStore (StoreDir evalStore) tmpBase ("builtins.addDrvOutputDependencies " <> inTree "data.txt")
               pure $ case result of
                 Left err -> assertEqual "add-output-copy" ("builtins.addDrvOutputDependencies: path " <> dataCopyText <> " is not a derivation") err
                 Right val -> Fail ("expected a refusal, got " <> T.pack (show val)),
             inEval "the copy keeps the executable bit the store holds" ("\"${" <> inTree "run.sh" <> "}\"") (storePathStringValue scriptCopy),
             inEval "the copy reads names as the store's volume holds them" ("\"${" <> inTree "cased" <> "}\"") (storePathStringValue casedCopy),
             runTestM "derivation args and env copy a path inside a store path" $ do
               result <-
                 evalDerivationIOStore (StoreDir evalStore) tmpBase $
                   T.concat
                     [ "derivation { name = \"in-store\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; ",
                       "args = [ " <> inTree "data.txt" <> " ]; src = " <> inTree "data.txt" <> "; }"
                     ]
               pure $ assertRight "in-store-drv" result $ \drv ->
                 assertEqual "args" [TE.encodeUtf8 dataCopyText] (drvArgs drv)
                   `andThen` assertEqual "env" (Just (TE.encodeUtf8 dataCopyText)) (Map.lookup "src" (drvEnv drv))
                   `andThen` assertEqual "inputSrcs" [dataCopy] (drvInputSrcs drv),
             -- The build driver restores each copy from where evaluation read
             -- it, the store dir it was given, and the restore must reproduce
             -- the address evaluation hashed.
             runTestM "materializeEvalSources restores a copy of a path in the store it was given" $ do
               placeTree restoreStore
               store <- openStore (StoreDir restoreStore)
               outcome <-
                 try (materializeEvalSources store (Map.fromList [(casedCopy, inTree "cased"), (smallCopy, small)]))
               valid <- mapM (isValid store) [casedCopy, smallCopy]
               restored <- BS.readFile (storePathToFilePath (StoreDir restoreStore) smallCopy)
               closeStore store
               forceRemoveIfExists restoreStore
               pure $ case outcome of
                 Left (failure :: IOException) -> Fail ("the restore failed: " <> T.pack (show failure))
                 Right () -> assertEqual "registered" [True, True] valid `andThen` assertEqual "restored" smallContents restored,
             -- A derivation in a file imported from the store, as nixpkgs is
             -- once it is fetched: its builder runs the script's copy, which
             -- copies the data file's, and the script's copy is restored
             -- executable, as the store holds the script.
             runTestM "e2e a build naming paths inside a store path finds their copies" $ do
               placeTree buildStore
               result <- evalAndBuild (StoreDir buildStore) ("import " <> inTree "build.nix" <> " \"" <> nixEscape shell <> "\"")
               outcome <- case result of
                 Left err -> pure (Fail err)
                 Right (BuildFailure msg code, store) -> Fail ("build failed (" <> T.pack (show code) <> "): " <> msg) <$ closeStore store
                 Right (BuildSuccess out, store) -> do
                   built <- BS.readFile (storePathToFilePath (StoreDir buildStore) out)
                   executable <- ExecBit.isExecutable (storePathToFilePath (StoreDir buildStore) scriptCopy)
                   closeStore store
                   pure (assertEqual "output" dataContents built `andThen` assertEqual "executable" True executable)
               forceRemoveIfExists buildStore
               pure outcome,
             -- The store directory and its ancestors stand for the store
             -- only on the way to a path inside it; under any other path
             -- they are host directories, whose links are followed.
             runTest "storeRootPrefix covers the store directory and its ancestors for a path in the store alone" $
               assertEqual
                 "store root"
                 [True, True, False, False, False]
                 [ storeRootPrefix ["nix", "store", "p"] ["nix"],
                   storeRootPrefix ["nix", "store", "p"] ["nix", "store"],
                   storeRootPrefix ["nix", "store", "p"] ["nix", "store", "p"],
                   storeRootPrefix ["nix", "var", "x"] ["nix"],
                   storeRootPrefix ["home", "nix", "store"] ["home"]
                 ],
             -- What Windows reports for a reparse point: a drive path or a
             -- relative one joins, a volume mount point's GUID name and a
             -- UNC or device path do not.
             runTest "windowsLinkTargetJoins takes a drive or relative target and leaves a volume, UNC or device one" $
               assertEqual
                 "joins"
                 [True, True, False, False, False]
                 (map windowsLinkTargetJoins ["C:\\target", "..\\sibling", "Volume{0a1b2c3d-0000-0000-0000-000000000000}\\", "UNC\\server\\share", "\\\\?\\C:\\x"]),
             runTest "pathCaseHack reads store text by the store's volume and a source by the process" $
               let modeFor sensitivityOf path expected =
                     if pathCaseHack sensitivityOf path == expected
                       then Pass
                       else Fail ("unexpected case-hack mode for " <> path)
                in modeFor CaseSensitive tree NAR.CaseHackDisabled
                     `andThen` modeFor CaseInsensitive tree NAR.CaseHackEnabled
                     `andThen` modeFor CaseSensitive "/src/f" processCaseHack
                     `andThen` modeFor CaseInsensitive "/src/f" processCaseHack
           ]
  forceRemoveIfExists evalStore
  forceRemoveIfExists hostLinks
  pure results

-- | The walk of a path through its symlinks is pure over path-value
-- text, the host's rules reading only a link's target, so Windows-shaped
-- paths and targets are walked here on every host, over a table of links.
testWalkPathRules :: IO [Bool]
testWalkPathRules = do
  putStrLn "eval/walk-path-rules"
  let walkOver rules links path =
        case runIdentity (walkPath rules ResolveFull (\_ _ -> Right ()) (Identity . (`Map.lookup` Map.fromList links)) path) of
          Right resolved -> Right resolved
          Left WalkLoops -> Left "loops"
          Left (WalkRefused refusal) -> absurd refusal
      walks label rules links path expected =
        runTest label (assertEqual label (Right expected) (walkOver rules links path :: Either Text Text))
      store name = "/nix/store/" <> T.replicate 32 name
  sequence
    [ -- A rooted value whose first component is a drive, as
      -- /. + "C:\\..." spells one, qualified with the store's drive.
      walks "a drive inside a Windows path starts it again at that drive" WindowsPaths [] "C:/C:/Users/RUNNER~1/b.sh" "C:/Users/RUNNER~1/b.sh",
      walks "a drive-lettered Windows path with no link walks to itself" WindowsPaths [] "C:/Users/RUNNER~1/b.sh" "C:/Users/RUNNER~1/b.sh",
      walks "on POSIX a component spelled like a drive is a name" PosixPaths [] "/C:/Users/b.sh" "/C:/Users/b.sh",
      -- Store text leads with a separator, and is absolute on every host,
      -- however Windows reports it.
      walks
        "an absolute store-text target is followed from the root under Windows rules"
        WindowsPaths
        [(store "a" <> "-tree/abslink", store "c" <> "-other")]
        (store "a" <> "-tree/abslink/f")
        (store "c" <> "-other/f"),
      walks
        "a backslashed store-text target is followed from the root under Windows rules"
        WindowsPaths
        [(store "a" <> "-tree/abslink", T.replace "/" "\\" (store "c" <> "-other"))]
        (store "a" <> "-tree/abslink/f")
        (store "c" <> "-other/f"),
      walks
        "an absolute store-text target is followed from the root under POSIX rules"
        PosixPaths
        [(store "a" <> "-tree/abslink", store "c" <> "-other")]
        (store "a" <> "-tree/abslink/f")
        (store "c" <> "-other/f"),
      -- A host link into the store, then a link inside it.
      walks
        "a Windows host link into the store is followed on inside it"
        WindowsPaths
        [("C:/Users/r/hl", "\\nix\\store\\" <> T.replicate 32 "d" <> "-src"), (store "d" <> "-src/abs", store "c" <> "-other")]
        "C:/Users/r/hl/abs"
        (store "c" <> "-other"),
      walks "a relative Windows target resumes in the link's directory" WindowsPaths [("C:/a/link", "..\\b")] "C:/a/link/f" "C:/b/f",
      walks "a drive-lettered Windows target restarts at that drive" WindowsPaths [("C:/a/link", "D:\\b")] "C:/a/link/f" "D:/b/f",
      walks "a Windows volume mount point is left in place" WindowsPaths [("C:/m", "Volume{0a1b2c3d-0000-0000-0000-000000000000}\\")] "C:/m/f" "C:/m/f",
      walks "on POSIX a backslash in a target is part of a name" PosixPaths [("/a/link", "b\\c")] "/a/link" "/a/b\\c",
      runTest "a link to itself loops" (assertEqual "loops" (Left "loops") (walkOver PosixPaths [("/a", "/a")] "/a/f" :: Either Text Text)),
      runTest "linkTarget reads a target by the host's rules" $
        assertEqual
          "targets"
          [ Just (AbsoluteTarget ["nix", "store", "x"]),
            Just (AbsoluteTarget ["nix", "store", "x"]),
            Just (RelativeTarget ["\\nix\\store\\x"]),
            Just (AbsoluteTarget ["C:", "x"]),
            Just (RelativeTarget ["C:x"]),
            Nothing
          ]
          [ Policy.linkTarget WindowsPaths "/nix/store/x",
            Policy.linkTarget WindowsPaths "\\nix\\store\\x",
            Policy.linkTarget PosixPaths "\\nix\\store\\x",
            Policy.linkTarget WindowsPaths "C:\\x",
            Policy.linkTarget WindowsPaths "C:x",
            Policy.linkTarget WindowsPaths "Volume{0a1b2c3d-0000-0000-0000-000000000000}\\"
          ]
    ]

-- ---------------------------------------------------------------------------
-- Tests: byte-indexed string semantics
-- ---------------------------------------------------------------------------

-- | The pinned byte-semantics suite: a Nix string is a byte string, so
-- lengths, slices, regexes, and replaceStrings all index BYTES, and the
-- hash of a value is the hash of exactly its bytes.  Multibyte content
-- enters through source literals here (the parser interns their UTF-8);
-- arbitrary/invalid bytes enter via readFile in the IO group below.
testByteStringSemantics :: IO [Bool]
testByteStringSemantics = do
  putStrLn "eval/byte-strings"
  sequence
    [ -- stringLength / substring index bytes
      runTest "stringLength counts bytes" $
        assertEval "len-bytes" "builtins.stringLength \"\228\"" (VInt 2),
      runTest "substring slices at byte offsets" $
        assertEval "substr-bytes" "builtins.stringLength (builtins.substring 0 1 \"\228\")" (VInt 1),
      runTest "mid-codepoint slices reassemble byte-exactly" $
        assertEval
          "substr-reassemble"
          "builtins.substring 0 1 \"\228\" + builtins.substring 1 1 \"\228\" == \"\228\""
          (VBool True),
      -- the hash sees exactly the value's bytes
      runTest "hashString of a mid-codepoint slice is the raw byte's sha256" $
        assertEval
          "hash-midbyte"
          "builtins.hashString \"sha256\" (builtins.substring 0 1 \"\228\")"
          (mkStr (Hash.bytesToHexText (sha256Digest (BS.take 1 (TE.encodeUtf8 "\228"))))),
      runTest "hashString of valid UTF-8 is unchanged by the byte layer" $
        assertEval
          "hash-utf8-stable"
          "builtins.hashString \"sha256\" \"\228\""
          (mkStr (Hash.bytesToHexText (sha256Digest (TE.encodeUtf8 "\228")))),
      -- regexes run over bytes ('.' matches ONE byte)
      runTest "match . does not match a 2-byte char" $
        assertEval "match-one-byte" "builtins.match \".\" \"\228\"" VNull,
      runTest "match .. matches a 2-byte char" $
        assertEval "match-two-bytes" "builtins.match \"..\" \"\228\" == []" (VBool True),
      runTest "a multibyte pattern matches its own bytes" $
        assertEval "match-multibyte-pattern" "builtins.match \"\228\" \"\228\" == []" (VBool True),
      -- replaceStrings with an empty 'from' steps one BYTE
      runTest "replaceStrings empty-from inserts between bytes" $
        assertEval
          "replace-empty-from"
          "builtins.stringLength (builtins.replaceStrings [\"\"] [\"-\"] \"\228\")"
          (VInt 5),
      runTest "replaceStrings empty-from result is byte-exact" $
        assertEval
          "replace-empty-from-bytes"
          "builtins.replaceStrings [\"\"] [\"-\"] \"\228\" == \"-\" + builtins.substring 0 1 \"\228\" + \"-\" + builtins.substring 1 1 \"\228\" + \"-\""
          (VBool True),
      -- strict-decode boundaries reject bytes that are not UTF-8
      runTest "toJSON rejects invalid UTF-8" $
        assertEvalFail "tojson-invalid" "builtins.toJSON (builtins.substring 0 1 \"\228\")",
      runTest "getAttr rejects an invalid-UTF-8 attr name" $
        assertEvalFail "getattr-invalid" "builtins.getAttr (builtins.substring 0 1 \"\228\") {}",
      -- toXML passes bytes through raw (upstream's serializer never validates)
      runTest "toXML passes a mid-codepoint byte through" $
        assertEval
          "toxml-bytes"
          "builtins.stringLength (builtins.toXML (builtins.substring 0 1 \"\228\")) == builtins.stringLength (builtins.toXML \"x\")"
          (VBool True),
      -- a search-path miss is a CATCHABLE error (upstream ThrownError;
      -- nixpkgs' impure.nix relies on tryEval catching it)
      runTest "search-path miss is tryEval-catchable" $
        assertEval
          "findFile-catchable"
          "(builtins.tryEval (builtins.findFile builtins.nixPath \"nope-missing\")).success"
          (VBool False)
    ]

-- | readFile returns the file's RAW BYTES: BOMs survive, UTF-16 is not
-- transcoded, invalid UTF-8 is representable, and only NUL is rejected.
-- The expected hashes are computed from the fixture bytes themselves, so
-- the assertions pin "hash of the value = hash of the file's bytes".
testByteStringSemanticsIO :: IO [Bool]
testByteStringSemanticsIO = do
  putStrLn "eval/readfile-bytes-io"
  tmpDir <- getTemporaryDirectory
  let testDir = tmpDir </> "nova-nix-bytefile-test"
      bomBytes = BS.pack [0xEF, 0xBB, 0xBF] <> "hi"
      -- UTF-16LE BOM + U+0101 (both payload bytes nonzero, so no NUL):
      -- raw passthrough is observable as 4 bytes instead of a decoded char.
      utf16Bytes = BS.pack [0xFF, 0xFE, 0x01, 0x01]
      -- UTF-16LE ASCII interleaves NUL bytes - upstream readFile REJECTS it.
      utf16AsciiBytes = BS.pack [0xFF, 0xFE, 0x68, 0x00, 0x69, 0x00]
      invalidBytes = "a" <> BS.singleton 0xFF <> "b"
      nulBytes = "a" <> BS.singleton 0x00 <> "b"
  bracket_
    ( do
        createDirectoryIfMissing True testDir
        BS.writeFile (testDir </> "bom.bin") bomBytes
        BS.writeFile (testDir </> "utf16.bin") utf16Bytes
        BS.writeFile (testDir </> "utf16-ascii.bin") utf16AsciiBytes
        BS.writeFile (testDir </> "invalid.bin") invalidBytes
        BS.writeFile (testDir </> "nul.bin") nulBytes
        BS.writeFile (testDir </> "umlaut.bin") (TE.encodeUtf8 "\228")
    )
    ( do
        exists <- doesDirectoryExist testDir
        when exists (removeDirectoryRecursive testDir)
    )
    $ sequence
      [ runTestIO
          "readFile keeps a BOM (raw bytes)"
          testDir
          "builtins.stringLength (builtins.readFile ./bom.bin)"
          (VInt 5),
        runTestIO
          "readFile does not transcode UTF-16"
          testDir
          "builtins.stringLength (builtins.readFile ./utf16.bin)"
          (VInt 4),
        runTestIOFail
          "readFile rejects UTF-16 ASCII (its NUL bytes)"
          testDir
          "builtins.readFile ./utf16-ascii.bin",
        runTestIO
          "readFile passes invalid UTF-8 through"
          testDir
          "builtins.stringLength (builtins.readFile ./invalid.bin)"
          (VInt 3),
        runTestIO
          "readFile bytes hash as read (BOM file)"
          testDir
          "builtins.hashString \"sha256\" (builtins.readFile ./bom.bin)"
          (mkStr (Hash.bytesToHexText (sha256Digest bomBytes))),
        runTestIO
          "readFile bytes hash as read (UTF-16 file)"
          testDir
          "builtins.hashString \"sha256\" (builtins.readFile ./utf16.bin)"
          (mkStr (Hash.bytesToHexText (sha256Digest utf16Bytes))),
        runTestIO
          "readFile of invalid UTF-8 hashes its raw bytes"
          testDir
          "builtins.hashString \"sha256\" (builtins.readFile ./invalid.bin)"
          (mkStr (Hash.bytesToHexText (sha256Digest invalidBytes))),
        runTestIO
          "readFile round-trips a multibyte literal"
          testDir
          "builtins.readFile ./umlaut.bin == \"\228\""
          (VBool True),
        runTestIOFail
          "readFile rejects an embedded NUL"
          testDir
          "builtins.readFile ./nul.bin"
      ]

-- ---------------------------------------------------------------------------
-- Tests: evaluation policy - restricted and pure modes
-- ---------------------------------------------------------------------------

-- | A string naming a relative path is refused wherever a path is coerced
-- from a string, as upstream's coerceToPath refuses it ("string '%s'
-- doesn't represent an absolute path", observed from nix-instantiate
-- 2.33.2 in every mode).  Load bearing for the access policy: a relative
-- string would be checked against one location and opened at another.
testAbsolutePathStrings :: IO [Bool]
testAbsolutePathStrings = do
  putStrLn "eval/absolute-path-strings"
  tmpBase <- getTemporaryDirectory
  let testDir = tmpBase </> "nova-nix-test-abs-strings"
      relativeRefused label source =
        runTestM label $ do
          result <- evalNixIO testDir source
          pure $ case result of
            Left err
              | "string 'foo' doesn't represent an absolute path" `T.isInfixOf` err -> Pass
              | otherwise -> Fail ("expected the absolute-path refusal, got: " <> err)
            Right val -> Fail ("expected a refusal, got: " <> T.pack (show val))
  bracket_
    (createDirectoryIfMissing True testDir >> BS.writeFile (testDir </> "foo") "1\n")
    (forceRemoveIfExists testDir)
    $ sequence
      [ relativeRefused "readFile of a relative string" "builtins.readFile \"foo\"",
        relativeRefused "import of a relative string" "import \"foo\"",
        relativeRefused "pathExists of a relative string is an error, not false" "builtins.pathExists \"foo\"",
        relativeRefused "readDir of a relative string" "builtins.readDir \"foo\"",
        relativeRefused "readFileType of a relative string" "builtins.readFileType \"foo\"",
        relativeRefused "hashFile of a relative string" "builtins.hashFile \"sha256\" \"foo\"",
        relativeRefused "scopedImport of a relative string" "builtins.scopedImport { } \"foo\"",
        -- The search-path miss carries upstream's hint, byte for byte.
        runTest "a search path miss names the hint" $
          assertEqual
            "miss"
            (Left "file 'nope' was not found in the Nix search path (add it using $NIX_PATH or -I)")
            (evalNix "<nope>")
      ]

-- | The pure rules behind the modes, each pinned to upstream's code at
-- 2.24.9: CanonPath::isAllowed over the allow list, isAllowedURI and
-- checkURI over allowed-uris, the git input's URL form, the nix.conf
-- settings, and the builtins table under pure-eval.
testEvalPolicyRules :: IO [Bool]
testEvalPolicyRules = do
  putStrLn "eval/policy-rules"
  let oneRoot = allowPathIn "/a/b" noAllowedPaths
      twoRoots = allowPathIn "/x" oneRoot
      driveRoot = allowPathIn "C:/a/b" noAllowedPaths
      github = ["https://github.com/NixOS"]
      -- Successive nix.conf texts, weakest first, served to the expander
      -- as files.
      config texts =
        let paths = [asciiOsPath ("/policy/" ++ show index ++ ".conf") | index <- [1 .. length texts]]
            files = Map.fromList (zip paths texts)
            readFrom path = Identity (maybe (Left Config.ConfigFileMissing) Right (Map.lookup path files))
         in runIdentity (Config.loadConfig readFrom paths Nothing)
      cfgPure = fmap ncPureEval . config
      cfgRestrict = fmap ncRestrictEval . config
      cfgUris = fmap ncAllowedUris . config
  sequence
    [ -- CanonPath::isAllowed
      runTest "an allowed prefix admits itself" $
        assertEqual "self" True (isAllowedPath oneRoot "/a/b"),
      runTest "an allowed prefix admits what lies within it" $
        assertEqual "within" True (isAllowedPath oneRoot "/a/b/c/d"),
      runTest "an allowed prefix admits its ancestors" $
        assertEqual "ancestor" True (isAllowedPath oneRoot "/a") `andThen` assertEqual "root" True (isAllowedPath oneRoot "/"),
      runTest "a sibling sharing a text prefix is not within" $
        assertEqual "sibling" False (isAllowedPath oneRoot "/a/bc"),
      runTest "a path beside the prefix is refused" $
        assertEqual "beside" False (isAllowedPath oneRoot "/a/c") `andThen` assertEqual "elsewhere" False (isAllowedPath twoRoots "/y"),
      runTest "nothing allowed admits nothing, the root included" $
        assertEqual "empty" False (isAllowedPath noAllowedPaths "/") `andThen` assertEqual "empty-etc" False (isAllowedPath noAllowedPaths "/etc"),
      runTest "the spelling of an allowed prefix is canonicalized" $
        assertEqual "canon" True (isAllowedPath (allowPathIn "/a//b/./" noAllowedPaths) "/a/b/c"),
      -- The rules are platform-independent, so a Windows path value
      -- (C:/..., the drive designator its first component) is decided
      -- the same on every host.
      runTest "a drive designator is the first component" $
        assertEqual "within" True (isAllowedPath driveRoot "C:/a/b/c")
          `andThen` assertEqual "self" True (isAllowedPath driveRoot "C:/a/b")
          `andThen` assertEqual "drive-root" True (isAllowedPath driveRoot "C:/")
          `andThen` assertEqual "beside" False (isAllowedPath driveRoot "C:/a/c")
          `andThen` assertEqual "sibling" False (isAllowedPath driveRoot "C:/a/bc")
          `andThen` assertEqual "other-drive" False (isAllowedPath driveRoot "D:/a/b/c")
          `andThen` assertEqual "rootless" False (isAllowedPath driveRoot "/a/b/c")
          `andThen` assertEqual "first" "C:/" (joinComponents (take 1 (pathComponents "C:/a/b")))
          `andThen` assertEqual "first-posix" "/a" (joinComponents (take 1 (pathComponents "/a/b"))),
      -- isAllowedURI
      runTest "a URI equal to a prefix is allowed" $
        assertEqual "equal" True (isAllowedUri github "https://github.com/NixOS"),
      runTest "a URI under a prefix at a slash boundary is allowed" $
        assertEqual "subpath" True (isAllowedUri github "https://github.com/NixOS/patchelf.git"),
      runTest "a prefix does not admit a longer name" $
        assertEqual "github.co" False (isAllowedUri ["https://github.co"] "https://github.com")
          `andThen` assertEqual "NixOSx" False (isAllowedUri github "https://github.com/NixOSx"),
      runTest "a prefix ending in a slash admits below it but not itself" $
        assertEqual "below" True (isAllowedUri ["https://a/dir/"] "https://a/dir/x")
          `andThen` assertEqual "itself" False (isAllowedUri ["https://a/dir/"] "https://a/dir"),
      runTest "a bare scheme prefix admits its whole scheme" $
        assertEqual "https:" True (isAllowedUri ["https:"] "https://anything/at/all")
          `andThen` assertEqual "git+file:" True (isAllowedUri ["git+file:"] "git+file:///repo?exportIgnore=1")
          `andThen` assertEqual "http:" False (isAllowedUri ["https:"] "http://x"),
      -- checkURI
      runTest "nothing is checked unless restrict-eval is on" $
        assertEqual "off" (Right ()) (uriAccess defaultStoreDir purePolicy noAllowedPaths "https://x"),
      runTest "a URI outside the list is refused with upstream's wording" $
        assertEqual "refused" (Left "access to URI 'https://x/y' is forbidden in restricted mode") (uriAccess defaultStoreDir restrictedPolicy noAllowedPaths "https://x/y"),
      runTest "a path URI is checked against the allowed paths" $
        assertEqual "allowed" (Right ()) (uriAccess defaultStoreDir restrictedPolicy oneRoot "/a/b/f")
          `andThen` assertEqual "refused" (Left "access to absolute path '/etc/passwd' is forbidden in restricted mode") (uriAccess defaultStoreDir restrictedPolicy oneRoot "/etc/passwd"),
      runTest "a file URL is checked as its path, rooted" $
        assertEqual "allowed" (Right ()) (uriAccess defaultStoreDir restrictedPolicy oneRoot "file:///a/b/f")
          `andThen` assertEqual "rooted" (Left "access to absolute path '/etc/passwd' is forbidden in restricted mode") (uriAccess defaultStoreDir restrictedPolicy oneRoot "file://etc/passwd"),
      runTest "a file URL with a drive is checked as that drive's path" $
        assertEqual "allowed" (Right ()) (uriAccess defaultStoreDir restrictedPolicy driveRoot "file://C:/a/b/f")
          `andThen` assertEqual "refused" (Left "access to absolute path 'C:/a/c' is forbidden in restricted mode") (uriAccess defaultStoreDir restrictedPolicy driveRoot "file://C:/a/c"),
      -- The path a file URL names with no drive is checked on the store's
      -- drive, where the fetch reads it, not on the current one.
      runTest "a path or file URL with no drive is checked on the store's drive" $
        assertEqual "path" (Right ()) (uriAccess windowsStoreDir restrictedPolicy driveRoot "/a/b/f")
          `andThen` assertEqual "url" (Right ()) (uriAccess windowsStoreDir restrictedPolicy driveRoot "file:///a/b/f")
          `andThen` assertEqual "rootless" (Right ()) (uriAccess windowsStoreDir restrictedPolicy driveRoot "file://a/b/f")
          `andThen` assertEqual "named" (Left "access to absolute path 'C:/etc/passwd' is forbidden in restricted mode") (uriAccess windowsStoreDir restrictedPolicy driveRoot "file:///etc/passwd")
          `andThen` assertEqual "other-drive" (Left "access to absolute path 'D:/a/b/f' is forbidden in restricted mode") (uriAccess (StoreDir "D:\\s") restrictedPolicy driveRoot "/a/b/f"),
      runTest "the path refusal names pure mode when it is on" $
        assertEqual "pure" "access to absolute path '/etc' is forbidden in pure evaluation mode (use '--impure' to override)" (forbiddenPathMessage purePolicy "/etc")
          `andThen` assertEqual "both" "access to absolute path '/etc' is forbidden in pure evaluation mode (use '--impure' to override)" (forbiddenPathMessage purePolicy {epRestrictEval = True} "/etc"),
      -- Input::toURLString for a git input (strings observed from
      -- nix-instantiate 2.33.2, whose renderer is 2.24.9's)
      runTest "an https url renders with git+ and exportIgnore" $
        assertEqual "https" "git+https://github.com/NixOS/patchelf.git?exportIgnore=1" (gitInputUrl (gitInput "https://github.com/NixOS/patchelf.git")),
      runTest "a ref follows exportIgnore in key order" $
        assertEqual "ref" "git+https://github.com/NixOS/patchelf.git?exportIgnore=1&ref=master" (gitInputUrl (gitInput "https://github.com/NixOS/patchelf.git") {fgaRef = Just "master"}),
      runTest "a rev renders lowercase after ref" $
        assertEqual "rev" "git+https://h/r?exportIgnore=1&ref=main&rev=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" (gitInputUrl (gitInput "https://h/r") {fgaRef = Just "main", fgaRev = Just "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"}),
      runTest "shallow renders as a flag" $
        assertEqual "shallow" "git+ssh://git@github.com/NixOS/patchelf.git?exportIgnore=1&shallow=1" (gitInputUrl (gitInput "ssh://git@github.com/NixOS/patchelf.git") {fgaShallow = True}),
      runTest "submodules replaces exportIgnore" $
        assertEqual "submodules" "git+https://h/r?submodules=1" (gitInputUrl (gitInput "https://h/r") {fgaSubmodules = True}),
      runTest "a bare path becomes a file URL" $
        assertEqual "path" "git+file:///p/repo?exportIgnore=1" (gitInputUrl (gitInput "/p/repo")),
      runTest "an scp-like remote becomes ssh" $
        assertEqual "scp" "git+ssh://git@github.com/NixOS/patchelf.git?exportIgnore=1" (gitInputUrl (gitInput "git@github.com:NixOS/patchelf.git")),
      runTest "the git scheme keeps its name" $
        assertEqual "git" "git://h/r?exportIgnore=1" (gitInputUrl (gitInput "git://h/r")),
      runTest "a path is percent-encoded as upstream encodes it" $
        assertEqual "space" "git+file:///p/my%20repo?exportIgnore=1" (gitInputUrl (gitInput "/p/my repo")),
      -- What a fetch hands curl and git: a local path on the drive the
      -- evaluator reads it on, in the file://C:/ form the URI gate reads.
      runTest "a file URL with no drive reaches the tool on the store's drive" $
        assertEqual "file" (Right "file://C:/probe/f.txt") (onWindowsStore (localFileUrl "file:///probe/f.txt"))
          `andThen` assertEqual "drive" (Right "file://D:/probe/f.txt") (onWindowsStore (localFileUrl "file://D:/probe/f.txt"))
          `andThen` assertEqual "rootless" (Right "file://probe/f.txt") (onWindowsStore (localFileUrl "file://probe/f.txt"))
          `andThen` assertEqual "store" (Right "file:///nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-x") (onWindowsStore (localFileUrl "file:///nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-x"))
          `andThen` assertEqual "https" (Right "https://h/x") (onWindowsStore (localFileUrl "https://h/x"))
          `andThen` assertEqual "posix" (Right "file:///probe/f.txt") (onPosixStore (localFileUrl "file:///probe/f.txt")),
      runTest "a local git remote with no drive reaches git on the store's drive" $
        assertEqual "bare" (Right "C:/probe/repo") (onWindowsStore (gitRemote "/probe/repo"))
          `andThen` assertEqual "file" (Right "file://C:/probe/repo") (onWindowsStore (gitRemote "file:///probe/repo"))
          `andThen` assertEqual "scp" (Right "git@github.com:NixOS/patchelf.git") (onWindowsStore (gitRemote "git@github.com:NixOS/patchelf.git"))
          `andThen` assertEqual "unc" (Right "//server/share/repo") (onWindowsStore (gitRemote "//server/share/repo"))
          `andThen` assertEqual "unc-url" (Right "file:////server/share/repo") (onWindowsStore (gitRemote "file:////server/share/repo"))
          `andThen` assertEqual "https" (Right "https://h/r") (onWindowsStore (gitRemote "https://h/r"))
          `andThen` assertEqual "posix" (Right "/probe/repo") (onPosixStore (gitRemote "/probe/repo")),
      -- nix.conf
      runTest "pure-eval and restrict-eval parse upstream's Boolean spellings" $
        assertEqual "true" (Right True) (cfgPure ["pure-eval = true"])
          `andThen` assertEqual "yes" (Right True) (cfgPure ["pure-eval = yes"])
          `andThen` assertEqual "1" (Right True) (cfgRestrict ["restrict-eval = 1"])
          `andThen` assertEqual "false" (Right False) (cfgPure ["pure-eval = true", "pure-eval = false"])
          `andThen` assertEqual "no" (Right False) (cfgRestrict ["restrict-eval = no"])
          `andThen` assertEqual "0" (Right False) (cfgRestrict ["restrict-eval = 0"]),
      runTest "a misspelled Boolean is an error with upstream's wording" $
        assertEqual "typo" (Left "Boolean setting 'pure-eval' has invalid value 'ture'") (cfgPure ["pure-eval = ture"]),
      runTest "allowed-uris is a list and extra- appends to it" $
        assertEqual "uris" (Right ["https://a", "https://b", "https://c"]) (cfgUris ["allowed-uris = https://a https://b", "extra-allowed-uris = https://c"]),
      runTest "extra- on a Boolean names no setting and is ignored" $
        assertEqual "extra-bool" (Right False) (cfgPure ["extra-pure-eval = true"]),
      runTest "the default config has both modes off" $
        assertEqual "defaults" (Right (False, False, [])) ((\c -> (ncRestrictEval c, ncPureEval c, ncAllowedUris c)) <$> config []),
      -- enableImportFromDerivation and, from 2.34.9,
      -- traceImportFromDerivation in eval-settings.hh.
      runTest "import from derivation is allowed and not traced by default, and nix.conf sets each" $
        let imports = fmap (\c -> (ncAllowImportFromDerivation c, ncTraceImportFromDerivation c)) . config
         in assertEqual "defaults" (Right (True, False)) (imports [])
              `andThen` assertEqual "set" (Right (False, True)) (imports ["allow-import-from-derivation = false\ntrace-import-from-derivation = true"])
              `andThen` assertEqual "later wins" (Right (True, False)) (imports ["allow-import-from-derivation = false", "allow-import-from-derivation = true"]),
      -- the builtins table
      runTest "currentTime and currentSystem are absent under pure-eval" $
        assertEqual "time" (Right (VBool False)) (evalNixWith purePolicy "builtins ? currentTime")
          `andThen` assertEqual "system" (Right (VBool False)) (evalNixWith purePolicy "builtins ? currentSystem")
          `andThen` assertEqual "time-restricted" (Right (VBool True)) (evalNixWith restrictedPolicy "builtins ? currentTime")
          `andThen` assertEqual "time-default" (Right (VBool True)) (evalNixWith unrestrictedPolicy "builtins ? currentSystem")
    ]
  where
    onWindowsStore = runStubStoreEvalWith (stubEnv Map.empty) {seStoreDir = windowsStoreDir}
    onPosixStore = runStubStoreEvalWith (stubEnv Map.empty)
    gitInput url =
      FetchGitArgs
        { fgaUrl = url,
          fgaName = "source",
          fgaRef = Nothing,
          fgaRev = Nothing,
          fgaSubmodules = False,
          fgaShallow = False,
          fgaAllRefs = False,
          fgaNarHash = Nothing
        }

-- | 'evalNixIO' under a policy: the roots are allowed first, and the
-- search path is what the caller passes, as the CLI would pass it.
evalPolicyIO :: EvalPolicy -> [Text] -> [Thunk] -> FilePath -> Text -> IO (Either Text NixValue)
evalPolicyIO policy roots searchPaths baseDir source = do
  storeDir <- evalNixIOStoreDir
  case parseNixWithScope (rootScopeNames policy) baseDir testOrigin (TE.encodeUtf8 source) of
    Left err -> pure (Left (T.pack (show err)))
    Right parsed -> do
      st <- newEvalStateWith policy searchPaths (testSessionStore storeDir) storeDir baseDir
      mapM_ (allowEvalPath st) roots
      runEvalIO st (evalParsed (pure (evalBaseEnv st)) parsed)

-- | The battery the modes were pinned against, run through EvalIO: every
-- expression here was run through nix-instantiate under restrict-eval and
-- pure-eval, and the expected text is what upstream printed, except where
-- the oracle version (2.24.9) differs from the installed 2.33.2 and the
-- oracle's source wins (which prefix a refusal names, the fetchGit
-- wording).  The fixture lives at a symlink-free root: a root behind a
-- symlinked directory is refused by upstream too, since its allow list
-- holds the spelling given and the walk resolves the link.
testEvalPolicyIO :: IO [Bool]
testEvalPolicyIO = do
  putStrLn "eval/policy-io"
  tmpBase <- getTemporaryDirectory >>= Dir.canonicalizePath
  links <- symlinksAvailable
  let root = tmpBase </> "nova-nix-test-policy"
      allowed = root </> "allowed"
      outside = root </> "outside"
      repo = allowed </> "repo"
      pathText = canonPathValue . T.pack
      forbidden policy p = "access to absolute path '" <> pathText p <> "' is forbidden " <> modeText policy
      modeText policy
        | epPureEval policy = "in pure evaluation mode (use '--impure' to override)"
        | otherwise = "in restricted mode"
      -- The first prefix the walk pushes: /tmp on Linux, /private on
      -- macOS, the drive root C:/ on Windows.
      firstComponent = joinComponents (take 1 (pathComponents (pathText allowed)))
      -- A fixture path as a path literal, relative to the base directory
      -- (allowed): the one spelling that lexes as a path on every
      -- platform, since a drive-letter path (C:/...) is a URI token to
      -- upstream's lexer and to this one.
      lit p = "../" <> pathText (makeRelative root p)
      quoted p = "\"" <> pathText p <> "\""
      run policy roots = evalPolicyIO policy roots [] allowed
      restricted = run restrictedPolicy [pathText allowed]
      pureRun = run purePolicy [pathText allowed]
      withUris uris = run restrictedPolicy {epAllowedUris = uris} [pathText allowed]
      expectLeft label expected action =
        runTestM label $ do
          result <- action
          pure $ case result of
            Left err -> assertEqual label expected err
            Right val -> Fail (label <> ": expected a refusal, got " <> T.pack (show val))
      expectRight label expected action =
        runTestM label $ assertRight label <$> action <*> pure (assertEqual label expected)
      expectRightWith label check action =
        runTestM label $ (\r -> assertRight label r check) <$> action
      repoUrl = "git+file://" <> pathText repo <> "?exportIgnore=1"
      unrestricted = run unrestrictedPolicy []
      symlinkMessage p = "path '" <> pathText p <> "' is a symlink"
      -- A case that needs a symlink on disk, or the record of why it did
      -- not run.
      linked label mk = if links then mk label else runTest (label <> " (skipped: no symlinks here)") Pass
  bracket_
    ( do
        forceRemoveIfExists root
        createDirectoryIfMissing True (allowed </> "sub")
        createDirectoryIfMissing True outside
        BS.writeFile (allowed </> "data.txt") "hello\n"
        BS.writeFile (allowed </> "sub" </> "data.txt") "inner\n"
        BS.writeFile (allowed </> "y.nix") "7\n"
        BS.writeFile (outside </> "x.nix") "42\n"
        BS.writeFile (outside </> "secret.txt") "secret\n"
        when links $ do
          Dir.createFileLink (outside </> "secret.txt") (allowed </> "link")
          Dir.createDirectoryLink outside (allowed </> "dirlink")
          Dir.createFileLink "nowhere" (allowed </> "dangling")
          createDirectoryIfMissing True (allowed </> "dir")
          Dir.createFileLink (outside </> "x.nix") (allowed </> "dir" </> "default.nix")
        createDirectoryIfMissing True repo
        _ <- fixtureGit repo ["init", "--quiet"]
        BS.writeFile (repo </> "f.txt") "in repo\n"
        _ <- fixtureGit repo ["add", "-A"]
        _ <- fixtureGit repo ["commit", "--quiet", "-m", "init"]
        pure ()
    )
    (forceRemoveIfExists root)
    $ do
      rev <- T.strip <$> fixtureGit repo ["rev-parse", "HEAD"]
      -- The fetch cache writes under XdgCache; point it at the fixture for
      -- the duration so the suite never touches the developer's cache.
      savedCacheHome <- lookupEnv "XDG_CACHE_HOME"
      setEnv "XDG_CACHE_HOME" (root </> "cache")
      results <-
        sequence
          [ -- restricted: the allow list
            expectRight "a file under an allowed root reads" (mkStr "hello\n") (restricted ("builtins.readFile " <> lit (allowed </> "data.txt"))),
            expectRight "a file deeper under an allowed root reads" (mkStr "inner\n") (restricted ("builtins.readFile " <> lit (allowed </> "sub" </> "data.txt"))),
            expectLeft "a file beside the allowed root is refused, naming the first prefix outside the list" (forbidden restrictedPolicy outside) (restricted ("builtins.readFile " <> lit (outside </> "secret.txt"))),
            expectLeft "with nothing allowed the refusal names the first component" (forbidden restrictedPolicy (T.unpack firstComponent)) (run restrictedPolicy [] ("builtins.readFile " <> lit (allowed </> "data.txt"))),
            expectLeft "a string spelling the path is refused the same way" (forbidden restrictedPolicy outside) (restricted ("builtins.readFile " <> quoted (outside </> "secret.txt"))),
            expectRight "import under the root works" (VInt 7) (restricted ("import " <> lit (allowed </> "y.nix"))),
            expectLeft "import beside the root is refused" (forbidden restrictedPolicy outside) (restricted ("import " <> lit (outside </> "x.nix"))),
            expectRight "pathExists is true under the root" (VBool True) (restricted ("builtins.pathExists " <> lit (allowed </> "data.txt"))),
            expectRight "pathExists is false beside it, not an error" (VBool False) (restricted ("builtins.pathExists " <> lit (outside </> "secret.txt"))),
            runTestM "readDir under the root lists" $
              restricted ("builtins.readDir " <> lit (allowed </> "sub")) >>= \case
                Right (VAttrs attrs) -> assertEqual "entries" 1 <$> attrSetSize attrs
                other -> pure (Fail (T.pack (show other))),
            expectLeft "readDir beside the root is refused" (forbidden restrictedPolicy outside) (restricted ("builtins.readDir " <> lit outside)),
            expectLeft "readFileType names the path asked for, no walk" (forbidden restrictedPolicy (outside </> "secret.txt")) (restricted ("builtins.readFileType " <> lit (outside </> "secret.txt"))),
            expectLeft "hashFile walks like readFile" (forbidden restrictedPolicy outside) (restricted ("builtins.hashFile \"sha256\" " <> lit (outside </> "secret.txt"))),
            expectRight "getEnv answers empty" (mkStr "") (restricted "builtins.getEnv \"PATH\""),
            expectRight "a toFile result is readable afterwards" (mkStr "y") (restricted "builtins.readFile (builtins.toFile \"x\" \"y\")"),
            expectRightWith "a source copy under the root is allowed" (\case VStr s _ -> assertEqual "store" True (BS.isPrefixOf "/nix/store/" s); other -> Fail (T.pack (show other))) (restricted "\"${./data.txt}\""),
            expectLeft "a source copy beside the root is refused" (forbidden restrictedPolicy outside) (restricted ("\"${" <> lit (outside </> "secret.txt") <> "}\"")),
            expectLeft "builtins.path beside the root is refused" (forbidden restrictedPolicy outside) (restricted ("builtins.path { path = " <> lit (outside </> "secret.txt") <> "; }")),
            expectLeft "filterSource beside the root is refused" (forbidden restrictedPolicy outside) (restricted ("builtins.filterSource (p: t: true) " <> lit outside)),
            expectLeft "tryEval does not catch a refusal" (forbidden restrictedPolicy outside) (restricted ("builtins.tryEval (builtins.readFile " <> lit (outside </> "secret.txt") <> ")")),
            linked "a symlink under the root pointing outside is refused at its target" (\label -> expectLeft label (forbidden restrictedPolicy outside) (restricted ("builtins.readFile " <> lit (allowed </> "link")))),
            -- A coerced path's own link is read in place, as 2.34.9 and
            -- newer copy it, and only its ancestors are walked: the link
            -- is copied whatever it points to, while a link above the path
            -- is followed and its target checked.
            -- The link as the host's walk dumps it, its target spelled as
            -- the host reports it.
            linked "interpolating a symlink under the root pointing outside copies the link" $ \label -> do
              linkEntry <- ExecBit.serialiseFromPath processCaseHack (allowed </> "link")
              let expected = either (const "") (TE.encodeUtf8 . storePathToText defaultStoreDir) (makeFixedOutputPath "link" "sha256" NarIngestion (sha256Digest (NAR.serialise linkEntry)))
              case linkEntry of
                NAR.NarSymlink _ ->
                  expectRightWith
                    label
                    ( \case
                        VStr got _ -> assertEqual label expected got
                        other -> Fail (label <> ": expected a string, got " <> T.pack (show other))
                    )
                    (restricted ("\"${" <> lit (allowed </> "link") <> "}\""))
                _ -> runTest label (Fail "the fixture link does not dump as a link"),
            linked "interpolating a path below a symlinked directory pointing outside is refused at its target" (\label -> expectLeft label (forbidden restrictedPolicy outside) (restricted ("\"${" <> lit (allowed </> "dirlink" </> "x.nix") <> "}\""))),
            linked "a directory import whose default.nix is a symlink outside the root is refused at the link's target" (\label -> expectLeft label (forbidden restrictedPolicy outside) (restricted ("import " <> lit (allowed </> "dir")))),
            linked "scopedImport of that directory is refused the same way" (\label -> expectLeft label (forbidden restrictedPolicy outside) (restricted ("builtins.scopedImport {} " <> lit (allowed </> "dir")))),
            linked "pathExists checks a symlink to a forbidden file in place: true" (\label -> expectRight label (VBool True) (restricted ("builtins.pathExists " <> lit (allowed </> "link")))),
            linked "pathExists checks a symlink to a forbidden directory in place: true" (\label -> expectRight label (VBool True) (restricted ("builtins.pathExists " <> lit (allowed </> "dirlink")))),
            linked "pathExists of a dangling symlink is true" (\label -> expectRight label (VBool True) (restricted ("builtins.pathExists " <> lit (allowed </> "dangling")))),
            linked "pathExists through a symlinked directory follows the ancestor, which is refused: false" (\label -> expectRight label (VBool False) (restricted ("builtins.pathExists " <> lit (allowed </> "dirlink" </> "secret.txt")))),
            linked "pathExists with a trailing slash follows the final link, which is refused: false" (\label -> expectRight label (VBool False) (restricted ("builtins.pathExists \"" <> pathText (allowed </> "dirlink") <> "/\""))),
            linked "readFileType through a symlinked directory is refused at the symlink" (\label -> expectLeft label (symlinkMessage (allowed </> "dirlink")) (restricted ("builtins.readFileType " <> lit (allowed </> "dirlink" </> "secret.txt")))),
            linked "readFileType through a symlinked directory is refused in the default mode too" (\label -> expectLeft label (symlinkMessage (allowed </> "dirlink")) (unrestricted ("builtins.readFileType " <> lit (allowed </> "dirlink" </> "secret.txt")))),
            linked "pathExists of a dangling symlink is true in the default mode too" (\label -> expectRight label (VBool True) (unrestricted ("builtins.pathExists " <> lit (allowed </> "dangling")))),
            linked "builtins.path resolves a symlink root and stores its target" (\label -> expectRight label (mkStr "directory") (unrestricted ("builtins.readFileType (builtins.path { path = " <> lit (allowed </> "dirlink") <> "; })"))),
            linked "filterSource resolves a symlink root and stores its target" (\label -> expectRight label (mkStr "directory") (unrestricted ("builtins.readFileType (builtins.filterSource (p: t: true) " <> lit (allowed </> "dirlink") <> ")"))),
            expectRight "pathExists with a trailing slash on a file is false" (VBool False) (unrestricted ("builtins.pathExists \"" <> pathText (allowed </> "data.txt") <> "/\"")),
            expectRight "pathExists with a trailing slash on a directory is true" (VBool True) (unrestricted ("builtins.pathExists \"" <> pathText (allowed </> "sub") <> "/\"")),
            expectRight "pathExists with a trailing slash and dot on a directory is true" (VBool True) (unrestricted ("builtins.pathExists \"" <> pathText (allowed </> "sub") <> "/.\"")),
            -- restricted: URIs
            expectLeft "a fetch of an unlisted URI is refused before any download" "access to URI 'https://example.invalid/x' is forbidden in restricted mode" (restricted "builtins.fetchurl \"https://example.invalid/x\""),
            expectLeft "a file URL beside the root is refused as its path" (forbidden restrictedPolicy (outside </> "secret.txt")) (restricted ("builtins.fetchurl \"file://" <> pathText (outside </> "secret.txt") <> "\"")),
            expectRightWith "a file URL under the root fetches" (\val -> maybe (Fail (T.pack (show val))) (const Pass) (fetchedStorePath val)) (restricted ("builtins.fetchurl { url = \"file://" <> pathText (allowed </> "data.txt") <> "\"; sha256 = \"5891b5b522d5df086d0ff0b110fbd9d21bb4fc7163af34d08286a2e846f6be03\"; }")),
            expectLeft "fetchTarball of an unlisted URI is refused" "access to URI 'https://example.invalid/x.tar.gz' is forbidden in restricted mode" (restricted "builtins.fetchTarball \"https://example.invalid/x.tar.gz\""),
            expectLeft "an allowed-uris prefix admits only its subpaths" "access to URI 'https://example.invalid/dir2/x' is forbidden in restricted mode" (withUris ["https://example.invalid/dir"] "builtins.fetchurl { url = \"https://example.invalid/dir2/x\"; sha256 = \"5891b5b522d5df086d0ff0b110fbd9d21bb4fc7163af34d08286a2e846f6be03\"; }"),
            runTestM "an allowed-uris prefix lets the fetch proceed to the network" $ do
              result <- withUris ["https://example.invalid/dir"] "builtins.fetchurl { url = \"https://example.invalid/dir/x\"; sha256 = \"5891b5b522d5df086d0ff0b110fbd9d21bb4fc7163af34d08286a2e846f6be03\"; }"
              pure $ case result of
                Left err | "is forbidden" `T.isInfixOf` err -> Fail ("the gate refused an allowed URI: " <> err)
                _ -> Pass,
            expectLeft "fetchGit of an allowed path is still a URI, checked in its git+file form" ("access to URI '" <> repoUrl <> "' is forbidden in restricted mode") (restricted ("builtins.fetchGit " <> quoted repo)),
            expectRight "an allowed git+file prefix admits the fetch and its tree is readable afterwards" (mkStr "in repo\n") (withUris ["git+file://" <> pathText allowed] ("builtins.readFile ((builtins.fetchGit " <> quoted repo <> ").outPath + \"/f.txt\")")),
            -- restricted: the search path
            expectRight "a search path root is where <name> resolves and reads" (VInt 7) (parseNixPath (TE.encodeUtf8 ("allowed=" <> pathText allowed)) >>= \searchPaths -> evalPolicyIO restrictedPolicy [pathText allowed] searchPaths allowed "import <allowed/y.nix>"),
            expectLeft "a search path miss carries the NIX_PATH hint and is catchable" "file 'nope' was not found in the Nix search path (add it using $NIX_PATH or -I)" (restricted "<nope>"),
            expectRight "tryEval catches the miss" (VBool False) (restricted "(builtins.tryEval <nope>).success"),
            -- pure
            expectLeft "pure mode refuses with its own clause" (forbidden purePolicy outside) (pureRun ("builtins.readFile " <> lit (outside </> "secret.txt"))),
            expectRight "pure mode still reads under an allowed root" (mkStr "hello\n") (pureRun ("builtins.readFile " <> lit (allowed </> "data.txt"))),
            expectRight "pure mode has no currentTime" (VBool False) (pureRun "builtins ? currentTime"),
            expectLeft "pure mode words the search path miss differently" "cannot look up '<nope>' in pure evaluation mode (use '--impure' to override)" (pureRun "<nope>"),
            expectRight "the pure miss is catchable too" (VBool False) (pureRun "(builtins.tryEval <nope>).success"),
            expectLeft "storePath is refused in pure mode before its argument is read" "'builtins.storePath' is not allowed in pure evaluation mode" (pureRun "builtins.storePath 42"),
            expectLeft "fetchurl needs a hash in pure mode, checked before any download" "in pure evaluation mode, 'fetchurl' requires a 'sha256' argument" (pureRun "builtins.fetchurl \"https://example.invalid/x\""),
            expectLeft "fetchTarball needs a hash in pure mode" "in pure evaluation mode, 'fetchTarball' requires a 'sha256' argument" (pureRun "builtins.fetchTarball \"https://example.invalid/x.tar.gz\""),
            expectLeft "fetchGit without a rev is unlocked in pure mode" ("in pure evaluation mode, 'fetchGit' will not fetch unlocked input '" <> repoUrl <> "'") (pureRun ("builtins.fetchGit " <> quoted repo)),
            expectRight "fetchGit with a rev is locked and fetches in pure mode" (mkStr "in repo\n") (pureRun ("builtins.readFile ((builtins.fetchGit { url = " <> quoted repo <> "; rev = \"" <> rev <> "\"; }).outPath + \"/f.txt\")")),
            expectLeft "a home-relative literal cannot resolve in pure mode" "the path '~/x' can not be resolved in pure mode" (pureRun "~/x"),
            expectRight "pure mode's getEnv answers empty" (mkStr "") (pureRun "builtins.getEnv \"PATH\""),
            expectRight "a path literal is a value, read or not" (mkStr (pathText (outside </> "secret.txt"))) (pureRun ("builtins.toString " <> lit (outside </> "secret.txt"))),
            -- unrestricted stays unrestricted
            expectRight "the default policy reads anywhere" (mkStr "secret\n") (run unrestrictedPolicy [] ("builtins.readFile " <> lit (outside </> "secret.txt")))
          ]
      maybe (unsetEnv "XDG_CACHE_HOME") (setEnv "XDG_CACHE_HOME") savedCacheHome
      pure results

-- | A rooted path with no drive opens what its drive-qualified spelling
-- opens when the store is on that drive, at every site a path value
-- reaches the filesystem and under the access policy.  The suite's store
-- is under the temporary directory, so it shares that directory's drive,
-- and on Windows CI the working directory is on another (D:), so a path
-- resolved on the current drive misses.  On POSIX the two spellings are
-- one path.
testDrivelessPathsIO :: IO [Bool]
testDrivelessPathsIO = do
  putStrLn "eval/driveless-paths-io"
  tmpBase <- Dir.canonicalizePath =<< getTemporaryDirectory
  let dir = tmpBase </> "nova-nix-test-driveless"
      qualified = canonPathValue (T.pack dir)
      driveless = canonPathValue (T.pack ("/" <> dropDrive dir))
      quoted = nixQuotedPath . T.unpack
      -- A fixture by its drive-less spelling, as a string and as a path.
      str name = quoted (driveless <> "/" <> name)
      pathOf name = "(/. + " <> str name <> ")"
      gives label = runTestIO label dir
      -- The drive-qualified side is a relative literal, which resolves
      -- against the fixture directory with its drive, or a string that
      -- spells the drive where a fetch takes no path.
      same label drivelessSide qualifiedSide = gives label ("(" <> drivelessSide <> ") == (" <> qualifiedSide <> ")") (VBool True)
      restricted roots = evalPolicyIO restrictedPolicy roots [] dir
      readsUnder label roots source =
        runTestM label $ (\r -> assertRight label r (assertEqual label (mkStr "probe"))) <$> restricted roots source
  bracket_
    ( do
        forceRemoveIfExists dir
        createDirectoryIfMissing True (dir </> "repo")
        createDirectoryIfMissing True (dir </> "sub")
        BS.writeFile (dir </> "f.txt") "probe"
        BS.writeFile (dir </> "g.nix") "7\n"
        BS.writeFile (dir </> "sub" </> "h.txt") "inner\n"
        BS.writeFile (dir </> "repo" </> "r.txt") "in repo\n"
        _ <- fixtureGit (dir </> "repo") ["init", "--quiet"]
        _ <- fixtureGit (dir </> "repo") ["add", "-A"]
        _ <- fixtureGit (dir </> "repo") ["commit", "--quiet", "-m", "init"]
        pure ()
    )
    (forceRemoveIfExists dir)
    $ sequence
      [ gives "pathExists of a string" ("builtins.pathExists " <> str "f.txt") (VBool True),
        gives "pathExists of a path" ("builtins.pathExists " <> pathOf "f.txt") (VBool True),
        gives "readFile" ("builtins.readFile " <> str "f.txt") (mkStr "probe"),
        gives "readDir" ("builtins.attrNames (builtins.readDir " <> quoted driveless <> ") == [ \"f.txt\" \"g.nix\" \"repo\" \"sub\" ]") (VBool True),
        gives "readFileType" ("builtins.readFileType " <> str "f.txt") (mkStr "regular"),
        gives "import" ("import " <> str "g.nix") (VInt 7),
        gives "scopedImport" ("builtins.scopedImport { } " <> str "g.nix") (VInt 7),
        gives "findFile" ("builtins.readFile (builtins.findFile [ { prefix = \"d\"; path = " <> quoted driveless <> "; } ] \"d/f.txt\")") (mkStr "probe"),
        same "hashFile" ("builtins.hashFile \"sha256\" " <> str "f.txt") "builtins.hashFile \"sha256\" ./f.txt",
        same "a copy by interpolation" ("\"${" <> pathOf "f.txt" <> "}\"") "\"${./f.txt}\"",
        same "builtins.path" ("builtins.path { path = " <> pathOf "sub" <> "; }") "builtins.path { path = ./sub; }",
        same "filterSource" ("builtins.filterSource (p: t: true) " <> pathOf "sub") "builtins.filterSource (p: t: true) ./sub",
        same "fetchurl of a file URL" ("builtins.fetchurl \"file://" <> driveless <> "/f.txt\"") ("builtins.fetchurl \"file://" <> qualified <> "/f.txt\""),
        same "fetchGit of a local path" ("(builtins.fetchGit " <> str "repo" <> ").outPath") ("(builtins.fetchGit " <> quoted (qualified <> "/repo") <> ").outPath"),
        readsUnder "a root allowed with no drive admits its drive-qualified spelling" [driveless] ("builtins.readFile " <> quoted (qualified <> "/f.txt")),
        readsUnder "a root allowed with its drive admits the drive-less spelling" [qualified] ("builtins.readFile " <> str "f.txt")
      ]
