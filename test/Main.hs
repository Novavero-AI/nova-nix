{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as TarEntry
import qualified Codec.Compression.Zstd.Lazy as ZstdL
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (ErrorCall (..), SomeAsyncException (..), SomeException, asyncExceptionToException, bracket_, evaluate, fromException, throwIO, try)
import Control.Monad (filterM, void, when)
import Data.Bits (shiftR, (.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.IO as TIO
import qualified Database.SQLite.Simple as SQL
import Foreign.Ptr (castPtr)
import Foreign.StablePtr (StablePtr, castPtrToStablePtr, castStablePtrToPtr, deRefStablePtr, freeStablePtr, newStablePtr)
import Nix.Builder (BuildConfig (..), BuildResult (..), buildDerivation, buildPath, buildWithDeps, defaultBuildConfig, unionEnvs, verifyFetchHash)
import Nix.Builder.Unpack (UnpackLimits (..), builtinUnpackBuilder, entryComponents, envSrcs, resolveLinkTarget)
import Nix.Builtins (builtinEnv, parseNixPath, splitNixPath)
import qualified Nix.DependencyGraph as DepGraph
import Nix.Derivation (Derivation (..), DerivationOutput (..), Platform (..), currentPlatform, fromATerm, platformToText, toATerm, toATermForHash)
import Nix.Eval (MonadEval (..), NixValue (..), StringContext (..), StringContextElement (..), Thunk (..), attrSetFromMap, attrSetLookup, attrSetNull, attrSetSize, builtinNames, checkGitUrl, emptyContext, emptyEnv, eval, force, mkStr, readThunkValue, runPureEval)
import Nix.Eval.Arena (arenaDestroy, arenaInit)
import Nix.Eval.CAttrSet (cattrsetFreeze, cattrsetInsert, cattrsetKeys, cattrsetLookup, cattrsetNew, cattrsetSize, cattrsetUnion)
import Nix.Eval.CBytecode (binaryAdd, captureSlots, captureWithScopes, cbcArg1, cbcArg2, cbcArg3, cbcData, cbcFlags, cbcOpCount, cbcOpcode, cbcShortArg, formalName, formalNamedSet, formalSet, strpartInterp, strpartLit, unaryNegate, pattern OpApp, pattern OpAssert, pattern OpAttrs, pattern OpBinary, pattern OpHasAttr, pattern OpIf, pattern OpIndStr, pattern OpLambda, pattern OpLet, pattern OpList, pattern OpLitBool, pattern OpLitFloat, pattern OpLitInt, pattern OpLitNull, pattern OpLitPath, pattern OpLitUri, pattern OpResolvedVar, pattern OpSelect, pattern OpStr, pattern OpUnary, pattern OpVar, pattern OpWith, pattern OpWithVar)
import Nix.Eval.CThunk (CThunkPtr, cthunkCount, cthunkGet, cthunkGetBcIdx, cthunkMarkBlackhole, cthunkNewBc, cthunkNewComputed, cthunkPayload, cthunkSetComputed, cthunkState)
import Nix.Eval.CanonPath (canonPath, canonPathValue)
import Nix.Eval.Compile (compileExpr)
import qualified Nix.Eval.Context as Context
import Nix.Eval.IO (EvalState (..), newEvalState, runEvalIO)
import Nix.Eval.Symbol (Symbol (..), symbolCount, symbolIntern, symbolLen, symbolText)
import Nix.Eval.Types (emptyCList)
import Nix.Expr.Resolve (staticGlobalNames)
import Nix.Expr.Types
import Nix.Hash (makeFixedOutputPath, sha256Digest)
import qualified Nix.Hash as Hash
import Nix.Parser (parseNix)
import Nix.Parser.Lexer (Located (..), Token (..), tokenize)
import Nix.Push (PushArtifact (..), PushCompression (..), checkRecordedNarHash, computeClosure, loadApiKeyFile, mkNarInfo, mkPushArtifact, narFileName, narHashMatches, parsePushCompression, planMissing, storePathBasename, stripHashPrefix)
import Nix.Store (DeleteOutcome (..), Store (..), acquirePathLock, addToStore, caseHackDiskNames, closeStore, copyPathInto, deleteStorePathRaw, isSafeNarName, isValid, materializeEvalSources, openStore, orderLinks, pathExists, registrationFor, releasePathLock, resolveDeleteTarget, scanReferences, scanTempReferences, setReadOnly, tryAcquirePathLock, writeDrv, writeDrvClosure)
import Nix.Store.CaseSensitive (trySetCaseSensitiveDir)
import Nix.Store.DB (PathInfo (..), PathRegistration (..), closeStoreDB, dbFileName, isValidPath, metaDirName, openStoreDB, queryAllValidPaths, queryDeriver, queryPathInfo, queryReferences, registerPath, registerPaths)
import Nix.Store.Path (StoreDir (..), StorePath, defaultStoreDir, defaultStoreDirText, isCanonicalStoreText, parseStorePath, platformStoreDir, platformStoreDirText, storePathToFilePath, storePathToText, storeTextToFilePath, windowsStoreDir)
import Nix.Store.Path.Internal (StorePath (..))
import qualified Nix.Substituter as Subst
import qualified NovaCache.Base64 as B64
import qualified NovaCache.Hash as CHash
import qualified NovaCache.NAR as NAR
import qualified NovaCache.NarInfo as NarInfo
import qualified NovaCache.Signing as Signing
import qualified NovaCache.Zstd as CZstd
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getPermissions, getTemporaryDirectory, removeDirectoryRecursive, writable)
import qualified System.Directory as Dir
import System.Exit (ExitCode (..), exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.IO (BufferMode (..), hSetBuffering, stdout)
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Info as SI
import qualified System.Process as Proc
import System.Timeout (timeout)

-- ---------------------------------------------------------------------------
-- Test harness
-- ---------------------------------------------------------------------------

data TestResult = Pass | Fail !Text

runTest :: Text -> TestResult -> IO Bool
runTest name result = case result of
  Pass -> do
    putStrLn $ "  PASS  " ++ T.unpack name
    pure True
  Fail msg -> do
    putStrLn $ "  FAIL  " ++ T.unpack name ++ ": " ++ T.unpack msg
    pure False

-- | Like 'runTest' but for tests that need IO to produce their result.
-- An exception escaping the action becomes one FAIL line instead of
-- aborting the whole suite (which would skip every later group and its
-- cleanup).
runTestM :: Text -> IO TestResult -> IO Bool
runTestM name action = do
  outcome <- try action
  runTest name $ case outcome of
    Left (e :: SomeException) -> Fail ("uncaught exception: " <> T.pack (show e))
    Right result -> result

assertEqual :: (Eq a, Show a) => Text -> a -> a -> TestResult
assertEqual label expected actual
  | expected == actual = Pass
  | otherwise =
      Fail $
        label
          <> ": expected "
          <> T.pack (show expected)
          <> " but got "
          <> T.pack (show actual)

-- | Render string-value bytes in a failure message (display-only decode).
bytesText :: BS.ByteString -> Text
bytesText = TE.decodeUtf8With lenientDecode

assertRight :: (Show e) => Text -> Either e a -> (a -> TestResult) -> TestResult
assertRight label result check = case result of
  Left err -> Fail (label <> ": got error: " <> T.pack (show err))
  Right val -> check val

assertLeft :: (Show a) => Text -> Either e a -> TestResult
assertLeft _ (Left _) = Pass
assertLeft label (Right val) = Fail (label <> ": expected error but got: " <> T.pack (show val))

-- | Helper: parse and check result.
assertParse :: Text -> Text -> Expr -> TestResult
assertParse label source expected =
  assertRight label (parseNix "<test>" source) $ \actual ->
    assertEqual label expected actual

-- | Helper: extract just token types from Located list (drop positions and EOF).
tokenTypes :: [Located] -> [Token]
tokenTypes = filter (/= TokEOF) . map locToken

-- | Helper: parse Nix source and evaluate with builtinEnv.  Parse errors
-- are tagged so failure assertions can tell them apart from eval errors.
evalNix :: Text -> Either Text NixValue
evalNix source = case parseNix "<test>" source of
  Left err -> Left (parseErrorTag <> T.pack (show err))
  Right expr -> runPureEval (eval (builtinEnv 0 []) expr)

-- | Prefix marking a parse (not eval) failure in 'evalNix' results.
parseErrorTag :: Text
parseErrorTag = "parse error: "

-- | Assert that a Nix expression evaluates to the expected value.
assertEval :: Text -> Text -> NixValue -> TestResult
assertEval label source expected =
  assertRight label (evalNix source) $ \actual ->
    assertEqual label expected actual

-- | Assert that a Nix expression parses but fails to EVALUATE.  A parse
-- failure fails the assertion: a test documenting a runtime error must
-- not keep passing after a regression stops the construct from parsing.
assertEvalFail :: Text -> Text -> TestResult
assertEvalFail label source = case evalNix source of
  Left err
    | parseErrorTag `T.isPrefixOf` err ->
        Fail (label <> ": expected an eval error but the source did not parse: " <> err)
    | otherwise -> Pass
  Right val -> Fail (label <> ": expected eval failure but got: " <> T.pack (show val))

-- | Assert that a Nix expression fails at PARSE time.
assertParseFail :: Text -> Text -> TestResult
assertParseFail label source = case evalNix source of
  Left err
    | parseErrorTag `T.isPrefixOf` err -> Pass
    | otherwise ->
        Fail (label <> ": expected a parse error but evaluation failed instead: " <> err)
  Right val -> Fail (label <> ": expected parse failure but got: " <> T.pack (show val))

-- ---------------------------------------------------------------------------
-- Shell discovery for builder tests
-- ---------------------------------------------------------------------------

-- | Find a POSIX-compatible shell for builder tests.
-- On Unix, always @\/bin\/sh@.  On Windows, searches for @bash.exe@
-- at known Git for Windows locations first, then PATH.  Checks known
-- paths first to avoid picking up the WSL launcher at
-- @C:\\Windows\\System32\\bash.exe@ which exits 1 when WSL is not
-- configured.  Real Nix builders always use bash from the store -
-- this bridges the gap until nova-nix bootstraps its own bash
-- derivation.
findTestShell :: IO Text
findTestShell = case SI.os of
  "mingw32" -> do
    let known =
          [ "C:\\Program Files\\Git\\bin\\bash.exe",
            "C:\\Program Files (x86)\\Git\\bin\\bash.exe"
          ]
    found <- filterM Dir.doesFileExist known
    case found of
      (p : _) -> pure (T.pack p)
      [] -> do
        inPath <- Dir.findExecutable "bash"
        case inPath of
          Just p -> pure (T.pack p)
          Nothing ->
            error
              "bash not found: install Git for Windows or add bash to PATH"
  _ -> pure "/bin/sh"

-- ---------------------------------------------------------------------------
-- Tests: Expr types (existing)
-- ---------------------------------------------------------------------------

testExprTypes :: IO [Bool]
testExprTypes = do
  putStrLn "expr/types"
  sequence
    [ runTest "int literal" $
        assertEqual "ELit NixInt" (ELit (NixInt 42)) (ELit (NixInt 42)),
      runTest "bool literal" $
        assertEqual "ELit NixBool" (ELit (NixBool True)) (ELit (NixBool True)),
      runTest "null literal" $
        assertEqual "ELit NixNull" (ELit NixNull) (ELit NixNull),
      runTest "var" $
        assertEqual "EVar" (EVar "x") (EVar "x"),
      runTest "string parts" $
        let parts = [StrLit "hello ", StrInterp (EVar "name")]
         in assertEqual "EStr" (EStr parts) (EStr parts),
      runTest "binary op" $
        let expr = EBinary OpAdd (ELit (NixInt 1)) (ELit (NixInt 2))
         in assertEqual "EBinary" expr expr,
      runTest "lambda" $
        let expr = ELambda (FormalName "x") (EVar "x") NoCaptureInfo
         in assertEqual "ELambda" expr expr,
      runTest "let binding" $
        let expr = ELet [NamedBinding [StaticKey "x"] (ELit (NixInt 1))] (EVar "x") NoCaptureInfo
         in assertEqual "ELet" expr expr,
      runTest "attrs" $
        let expr = EAttrs False [NamedBinding [StaticKey "a"] (ELit (NixInt 1))] NoCaptureInfo
         in assertEqual "EAttrs" expr expr,
      runTest "if-then-else" $
        let expr = EIf (ELit (NixBool True)) (ELit (NixInt 1)) (ELit (NixInt 2))
         in assertEqual "EIf" expr expr
    ]

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
      -- platform store dir; everything else is the path as written.
      runTest "store text maps into the platform store dir" $
        assertEqual
          "mapped"
          (unStoreDir platformStoreDir <> "/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-x/sub/f")
          (storeTextToFilePath "/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-x/sub/f"),
      runTest "bare store dir text maps to the platform store root" $
        assertEqual "mapped-root" (unStoreDir platformStoreDir) (storeTextToFilePath "/nix/store"),
      runTest "store text with a backslash subpath maps" $
        assertEqual
          "mapped-backslash"
          (unStoreDir platformStoreDir <> "\\bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb-y")
          (storeTextToFilePath "/nix/store\\bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb-y"),
      runTest "non-store text is the path as written" $
        assertEqual "unmapped" "/etc/hosts" (storeTextToFilePath "/etc/hosts"),
      runTest "a store-prefix-like name does not map" $
        assertEqual "unmapped-like" "/nix/storefoo" (storeTextToFilePath "/nix/storefoo"),
      runTest "isCanonicalStoreText accepts both separators and rejects lookalikes" $
        assertEqual
          "predicate"
          [True, True, True, False, False]
          ( map
              isCanonicalStoreText
              ["/nix/store", "/nix/store/x", "/nix/store\\x", "/nix/storefoo", "C:\\nix\\store\\x"]
          )
    ]

-- ---------------------------------------------------------------------------
-- Tests: Derivation (existing)
-- ---------------------------------------------------------------------------

testDerivation :: IO [Bool]
testDerivation = do
  putStrLn "derivation"
  sequence
    [ runTest "current platform is known" $
        case currentPlatform of
          OtherPlatform _ -> Fail "currentPlatform returned OtherPlatform"
          _ -> Pass
    ]

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
      -- Integer overflow is an eval error (Nix 2.24 semantics), never a
      -- two's-complement wrap; the boundary itself still computes.
      runTest "add overflow fails" $
        assertEvalFail "add-overflow" "9223372036854775807 + 1",
      runTest "sub overflow fails" $
        assertEvalFail "sub-overflow" "(-9223372036854775807) - 2",
      runTest "mul overflow fails" $
        assertEvalFail "mul-overflow" "builtins.mul 9223372036854775807 2",
      runTest "div minBound by -1 overflows" $
        assertEvalFail "div-overflow" "((-9223372036854775807) - 1) / (-1)",
      runTest "negate minBound overflows" $
        assertEvalFail "neg-overflow" "-((-9223372036854775807) - 1)",
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
        assertEvalFail "lte-err" "1 <= \"a\""
    ]

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
        assertEval "dot-path" "{ a.b.c = 1; }.a.b.c" (VInt 1)
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
        assertEval "concat" "builtins.length ([ 1 ] ++ [ 2 3 ])" (VInt 3)
    ]

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
      -- AST test: parse "with a; b" produces EWithVar
      runTest "parse with produces EWithVar" $
        assertRight "with-ast" (parseNix "<test>" "with a; b") $ \case
          EWith (EVar "a") (EWithVar "b") -> Pass
          other -> Fail ("expected EWith (EVar a) (EWithVar b), got: " <> T.pack (show other)),
      -- AST test: formal wins over with
      runTest "parse lambda formal wins over with" $
        assertRight "formal-wins" (parseNix "<test>" "x: with a; x") $ \case
          ELambda _ (EWith _ (EResolvedVar 0 0)) _ -> Pass
          other -> Fail ("expected formal to win, got: " <> T.pack (show other)),
      -- Trimming test: lambda inside with gets CapturesWithScopes
      runTest "with lambda trimmed with CapturesWithScopes" $
        assertRight "with-trim" (parseNix "<test>" "with a; x: b + x") $ \case
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
      -- Indented-string escapes are opaque to indentation stripping: an
      -- escaped newline is content, not a line break, so text after it
      -- is not at line start and the column scan does not reset.
      runTest "escaped newline does not strip following text" $
        assertEval "ind-esc-nl-strip" "''  a''\\n  b'' == \"a\\n  b\"" (VBool True),
      runTest "escaped newline does not lower common indent" $
        assertEval "ind-esc-nl-indent" "''\n    x''\\n y\n    z'' == \"x\\n y\\nz\"" (VBool True),
      runTest "toString int" $
        assertEval "toStr" "builtins.toString 42" (mkStr "42")
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
      runTest "elemAt out of bounds" $
        assertEvalFail "elemAt-oob" "builtins.elemAt [ 1 2 ] 5",
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
-- Tests: Lexer
-- ---------------------------------------------------------------------------

testLexer :: IO [Bool]
testLexer = do
  putStrLn "parser/lexer"
  sequence
    [ runTest "integer" $
        assertRight "lex int" (tokenize "<test>" "42") $ \toks ->
          assertEqual "tokens" [TokInt 42] (tokenTypes toks),
      runTest "float" $
        assertRight "lex float" (tokenize "<test>" "3.14") $ \toks ->
          assertEqual "tokens" [TokFloat 3.14] (tokenTypes toks),
      runTest "trailing-dot float lexes as one float token" $
        assertRight "lex trailing-dot" (tokenize "<test>" "12.") $ \toks ->
          assertEqual "tokens" [TokFloat 12.0] (tokenTypes toks),
      -- Maximal munch: a dot-led run with a /-segment is one PATH token
      -- (upstream's PATH regex), while a slashless dot-run is not.
      runTest "dot-relative path lexes as one token" $
        assertRight "lex dot-path" (tokenize "<test>" ".github/x") $ \toks ->
          assertEqual "tokens" [TokPath ".github/x"] (tokenTypes toks),
      runTest "leading-dot float still lexes as float" $
        assertRight "lex dot-float" (tokenize "<test>" ".5") $ \toks ->
          assertEqual "tokens" [TokFloat 0.5] (tokenTypes toks),
      runTest "trailing-dot float with exponent lexes as one float token" $
        assertRight "lex trailing-dot-exp" (tokenize "<test>" "12.e2") $ \toks ->
          assertEqual "tokens" [TokFloat 1200.0] (tokenTypes toks),
      -- Float literals convert with a single rounding (strtod semantics).
      -- The exact value here sits between representable ..992 and ..994,
      -- nearer ..994; rounding whole and fraction separately lands on ..992.
      runTest "float literal rounds once at the 2^53 boundary" $
        assertRight "lex float-tie" (tokenize "<test>" "9007199254740993.5") $ \toks ->
          assertEqual "tokens" [TokFloat 9007199254740994.0] (tokenTypes toks),
      -- A float 10 ^^ exponent overflows on subnormals and flushes to 0.
      runTest "subnormal float literal survives" $
        assertRight "lex float-subnormal" (tokenize "<test>" "1.0e-320") $ \toks ->
          assertEqual "tokens" [TokFloat 1.0e-320] (tokenTypes toks),
      -- Bounded number lexing: a literal's cost must follow its length,
      -- never its exponent's magnitude, and megadigit literals must
      -- resolve promptly.  The watchdog turns a cost regression into a
      -- FAIL instead of a stuck suite; the checks force full values.
      runTestM "huge positive exponent saturates to Infinity" $ do
        let saturated = case tokenize "<test>" "1.0e999999999" of
              Right toks | [TokFloat f] <- tokenTypes toks -> isInfinite f && f > 0
              _ -> False
        outcome <- timeout walkWatchdogMicros (evaluate saturated)
        pure $ case outcome of
          Just True -> Pass
          Just False -> Fail "wrong tokens for a huge positive exponent"
          Nothing -> Fail "lexing did not return promptly",
      runTestM "huge negative exponent saturates to zero" $ do
        let flushed = case tokenize "<test>" "1.0e-999999999" of
              Right toks -> tokenTypes toks == [TokFloat 0.0]
              Left _ -> False
        outcome <- timeout walkWatchdogMicros (evaluate flushed)
        pure $ case outcome of
          Just True -> Pass
          Just False -> Fail "wrong tokens for a huge negative exponent"
          Nothing -> Fail "lexing did not return promptly",
      runTestM "zero mantissa ignores a huge exponent" $ do
        let zeroed = case tokenize "<test>" "0.0e999999999" of
              Right toks -> tokenTypes toks == [TokFloat 0.0]
              Left _ -> False
        outcome <- timeout walkWatchdogMicros (evaluate zeroed)
        pure $ case outcome of
          Just True -> Pass
          Just False -> Fail "wrong tokens for a zero mantissa"
          Nothing -> Fail "lexing did not return promptly",
      runTestM "megadigit integer literal rejects promptly" $ do
        let rejected = case tokenize "<test>" (T.replicate 1000000 "9") of
              Left _ -> True
              Right _ -> False
        outcome <- timeout walkWatchdogMicros (evaluate rejected)
        pure $ case outcome of
          Just True -> Pass
          Just False -> Fail "megadigit literal was not rejected"
          Nothing -> Fail "range rejection did not return promptly",
      runTestM "megadigit mantissa still rounds correctly" $ do
        -- 1.999...9e5 sits within half an ulp of 200000.0; the kept-768
        -- digits plus the sticky digit must round it there.
        let rounded = case tokenize "<test>" ("1." <> T.replicate 1000000 "9" <> "e5") of
              Right toks -> tokenTypes toks == [TokFloat 200000.0]
              Left _ -> False
        outcome <- timeout walkWatchdogMicros (evaluate rounded)
        pure $ case outcome of
          Just True -> Pass
          Just False -> Fail "wrong rounding for a megadigit mantissa"
          Nothing -> Fail "lexing did not return promptly",
      runTest "true" $
        assertRight "lex true" (tokenize "<test>" "true") $ \toks ->
          assertEqual "tokens" [TokTrue] (tokenTypes toks),
      runTest "false" $
        assertRight "lex false" (tokenize "<test>" "false") $ \toks ->
          assertEqual "tokens" [TokFalse] (tokenTypes toks),
      runTest "null" $
        assertRight "lex null" (tokenize "<test>" "null") $ \toks ->
          assertEqual "tokens" [TokNull] (tokenTypes toks),
      runTest "identifier" $
        assertRight "lex ident" (tokenize "<test>" "foo") $ \toks ->
          assertEqual "tokens" [TokIdent "foo"] (tokenTypes toks),
      runTest "hyphened identifier" $
        assertRight "lex hyphened" (tokenize "<test>" "hello-world") $ \toks ->
          assertEqual "tokens" [TokIdent "hello-world"] (tokenTypes toks),
      runTest "path ./foo" $
        assertRight "lex path" (tokenize "<test>" "./foo") $ \toks ->
          assertEqual "tokens" [TokPath "./foo"] (tokenTypes toks),
      runTest "path ~/foo" $
        assertRight "lex path home" (tokenize "<test>" "~/foo") $ \toks ->
          assertEqual "tokens" [TokPath "~/foo"] (tokenTypes toks),
      runTest "search path" $
        assertRight "lex search path" (tokenize "<test>" "<nixpkgs>") $ \toks ->
          assertEqual "tokens" [TokSearchPath "nixpkgs"] (tokenTypes toks),
      runTest "URI" $
        assertRight "lex uri" (tokenize "<test>" "https://example.com") $ \toks ->
          assertEqual "tokens" [TokUri "https://example.com"] (tokenTypes toks),
      -- Upstream URI = [a-zA-Z][a-zA-Z0-9+.-]*:[uri-char]+ with flex
      -- maximal munch: no // required, and the URI beats identifiers,
      -- keywords, and the lambda colon when it matches more characters.
      runTest "scheme-only URI x:y" $
        assertRight "lex uri x:y" (tokenize "<test>" "x:y") $ \toks ->
          assertEqual "tokens" [TokUri "x:y"] (tokenTypes toks),
      runTest "scheme-only URI mailto" $
        assertRight "lex uri mailto" (tokenize "<test>" "mailto:foo@example.com") $ \toks ->
          assertEqual "tokens" [TokUri "mailto:foo@example.com"] (tokenTypes toks),
      runTest "URI scheme allows + . -" $
        assertRight "lex uri scheme" (tokenize "<test>" "a+b.c:d") $ \toks ->
          assertEqual "tokens" [TokUri "a+b.c:d"] (tokenTypes toks),
      runTest "URI beats keyword by maximal munch" $
        assertRight "lex uri keyword" (tokenize "<test>" "then:x") $ \toks ->
          assertEqual "tokens" [TokUri "then:x"] (tokenTypes toks),
      runTest "hash is not a URI char (starts a comment)" $
        assertRight "lex uri hash" (tokenize "<test>" "http://a#frag") $ \toks ->
          assertEqual "tokens" [TokUri "http://a"] (tokenTypes toks),
      runTest "apostrophe and star are URI chars" $
        assertRight "lex uri quote star" (tokenize "<test>" "http://e.com/a'b*c") $ \toks ->
          assertEqual "tokens" [TokUri "http://e.com/a'b*c"] (tokenTypes toks),
      runTest "lambda colon needs the space" $
        assertRight "lex lambda colon" (tokenize "<test>" "x: y") $ \toks ->
          assertEqual "tokens" [TokIdent "x", TokColon, TokIdent "y"] (tokenTypes toks),
      runTest "underscore cannot start a URI scheme" $
        assertRight "lex underscore colon" (tokenize "<test>" "_a:b") $ \toks ->
          assertEqual "tokens" [TokIdent "_a", TokColon, TokIdent "b"] (tokenTypes toks),
      -- Raw CR/CRLF normalizes to LF in double-quoted strings (upstream
      -- unescapeStr); an escaped CR survives; indented strings keep CR
      -- verbatim because upstream's IND_STRING chunks skip unescapeStr.
      runTest "CRLF in double-quoted string normalizes to LF" $
        assertRight "lex crlf string" (tokenize "<test>" "\"a\r\nb\"") $ \toks ->
          assertEqual "tokens" [TokStringOpen, TokStringLit "a\nb", TokStringClose] (tokenTypes toks),
      runTest "lone CR in double-quoted string normalizes to LF" $
        assertRight "lex cr string" (tokenize "<test>" "\"a\rb\"") $ \toks ->
          assertEqual "tokens" [TokStringOpen, TokStringLit "a\nb", TokStringClose] (tokenTypes toks),
      runTest "escaped CR stays a literal CR" $
        assertRight "lex escaped cr" (tokenize "<test>" "\"a\\\rb\"") $ \toks ->
          assertEqual "tokens" [TokStringOpen, TokStringLit "a\rb", TokStringClose] (tokenTypes toks),
      runTest "indented string keeps CRLF verbatim" $
        assertRight "lex ind crlf" (tokenize "<test>" "''a\r\nb''") $ \toks ->
          assertEqual "tokens" [TokIndStringOpen, TokStringLit "a\r\nb", TokIndStringClose] (tokenTypes toks),
      runTest "multi-char operators" $
        assertRight "lex ops" (tokenize "<test>" "++ // -> == != && || <= >=") $ \toks ->
          assertEqual
            "tokens"
            [TokConcat, TokUpdate, TokImpl, TokEq, TokNeq, TokAnd, TokOr, TokLte, TokGte]
            (tokenTypes toks),
      runTest "single-char operators" $
        assertRight "lex single ops" (tokenize "<test>" "+ - * ! ? < >") $ \toks ->
          assertEqual
            "tokens"
            [TokPlus, TokMinus, TokStar, TokNot, TokQuestion, TokLt, TokGt]
            (tokenTypes toks),
      runTest "division vs path" $
        assertRight "lex div" (tokenize "<test>" "6 / 3") $ \toks ->
          assertEqual "tokens" [TokInt 6, TokSlash, TokInt 3] (tokenTypes toks),
      runTest "string tokens" $
        assertRight "lex string" (tokenize "<test>" "\"hello\"") $ \toks ->
          assertEqual
            "tokens"
            [TokStringOpen, TokStringLit "hello", TokStringClose]
            (tokenTypes toks),
      runTest "empty string" $
        assertRight "lex empty string" (tokenize "<test>" "\"\"") $ \toks ->
          assertEqual
            "tokens"
            [TokStringOpen, TokStringClose]
            (tokenTypes toks),
      runTest "string interpolation tokens" $
        assertRight "lex interp" (tokenize "<test>" "\"a${x}b\"") $ \toks ->
          assertEqual
            "tokens"
            [ TokStringOpen,
              TokStringLit "a",
              TokInterpOpen,
              TokIdent "x",
              TokInterpClose,
              TokStringLit "b",
              TokStringClose
            ]
            (tokenTypes toks),
      runTest "line comment" $
        assertRight "lex comment" (tokenize "<test>" "# comment\n42") $ \toks ->
          assertEqual "tokens" [TokInt 42] (tokenTypes toks),
      runTest "block comment" $
        assertRight "lex block comment" (tokenize "<test>" "/* comment */ 42") $ \toks ->
          assertEqual "tokens" [TokInt 42] (tokenTypes toks),
      runTest "ellipsis" $
        assertRight "lex ellipsis" (tokenize "<test>" "...") $ \toks ->
          assertEqual "tokens" [TokEllipsis] (tokenTypes toks),
      runTest "punctuation" $
        assertRight "lex punct" (tokenize "<test>" ". @ : ; = ,") $ \toks ->
          assertEqual
            "tokens"
            [TokDot, TokAt, TokColon, TokSemicolon, TokAssign, TokComma]
            (tokenTypes toks),
      runTest "delimiters" $
        assertRight "lex delimiters" (tokenize "<test>" "( ) { } [ ]") $ \toks ->
          assertEqual
            "tokens"
            [TokLParen, TokRParen, TokLBrace, TokRBrace, TokLBracket, TokRBracket]
            (tokenTypes toks),
      runTest "keywords" $
        assertRight "lex keywords" (tokenize "<test>" "if then else let in with assert rec inherit") $ \toks ->
          assertEqual
            "tokens"
            [TokIf, TokThen, TokElse, TokLet, TokIn, TokWith, TokAssert, TokRec, TokInherit]
            (tokenTypes toks),
      runTest "or is identifier" $
        assertRight "lex or" (tokenize "<test>" "or") $ \toks ->
          assertEqual "tokens" [TokIdent "or"] (tokenTypes toks),
      runTest "string escape sequences" $
        assertRight "lex escapes" (tokenize "<test>" "\"\\n\\t\\\\\\\"\"") $ \toks ->
          assertEqual
            "tokens"
            [TokStringOpen, TokStringLit "\n\t\\\"", TokStringClose]
            (tokenTypes toks)
    ]

-- ---------------------------------------------------------------------------
-- Tests: Parser expressions
-- ---------------------------------------------------------------------------

testParserExprs :: IO [Bool]
testParserExprs = do
  putStrLn "parser/exprs"
  sequence
    [ -- Atoms
      runTest "parse int" $
        assertParse "int" "42" (ELit (NixInt 42)),
      runTest "parse float" $
        assertParse "float" "3.14" (ELit (NixFloat 3.14)),
      runTest "parse true" $
        assertParse "true" "true" (ELit (NixBool True)),
      runTest "parse false" $
        assertParse "false" "false" (ELit (NixBool False)),
      runTest "parse null" $
        assertParse "null" "null" (ELit NixNull),
      runTest "parse var" $
        assertParse "var" "x" (EVar "x"),
      runTest "parse empty string" $
        assertParse "empty string" "\"\"" (EStr []),
      runTest "parse string literal" $
        assertParse "string" "\"hello\"" (EStr [StrLit "hello"]),
      runTest "parse string interpolation" $
        assertParse
          "interp"
          "\"hello ${name}\""
          (EStr [StrLit "hello ", StrInterp (EVar "name")]),
      runTest "parse nested string interpolation" $
        assertParse
          "nested interp"
          "\"${\"inner\"}\""
          (EStr [StrInterp (EStr [StrLit "inner"])]),
      -- Arithmetic
      runTest "parse add" $
        assertParse "add" "1 + 2" (EBinary OpAdd (ELit (NixInt 1)) (ELit (NixInt 2))),
      runTest "parse sub" $
        assertParse "sub" "3 - 1" (EBinary OpSub (ELit (NixInt 3)) (ELit (NixInt 1))),
      runTest "parse mul" $
        assertParse "mul" "2 * 3" (EBinary OpMul (ELit (NixInt 2)) (ELit (NixInt 3))),
      runTest "parse left-assoc add" $
        assertParse
          "left-assoc"
          "1 + 2 + 3"
          (EBinary OpAdd (EBinary OpAdd (ELit (NixInt 1)) (ELit (NixInt 2))) (ELit (NixInt 3))),
      runTest "parse precedence mul over add" $
        assertParse
          "precedence"
          "1 + 2 * 3"
          (EBinary OpAdd (ELit (NixInt 1)) (EBinary OpMul (ELit (NixInt 2)) (ELit (NixInt 3)))),
      -- Unary
      runTest "parse negation" $
        assertParse "negate" "-1" (EUnary OpNegate (ELit (NixInt 1))),
      runTest "parse logical not" $
        assertParse "not" "!true" (EUnary OpNot (ELit (NixBool True))),
      -- Non-associative
      runTest "parse eq" $
        assertParse "eq" "1 == 2" (EBinary OpEq (ELit (NixInt 1)) (ELit (NixInt 2))),
      runTest "parse lt" $
        assertParse "lt" "1 < 2" (EBinary OpLt (ELit (NixInt 1)) (ELit (NixInt 2))),
      -- Right-associative
      runTest "parse implication" $
        assertParse
          "impl"
          "a -> b -> c"
          (EBinary OpImpl (EVar "a") (EBinary OpImpl (EVar "b") (EVar "c"))),
      runTest "parse concat right" $
        assertParse
          "concat"
          "a ++ b ++ c"
          (EBinary OpConcat (EVar "a") (EBinary OpConcat (EVar "b") (EVar "c"))),
      runTest "parse update right" $
        assertParse
          "update"
          "a // b // c"
          (EBinary OpUpdate (EVar "a") (EBinary OpUpdate (EVar "b") (EVar "c"))),
      -- Lambda
      -- After variable resolution, lambda-bound vars become EResolvedVar.
      -- FormalName "x" maps to slot 0; FormalSet [a,b] maps to a=0, b=1;
      -- FormalNamedSet "args" [a] maps to args=0, a=1.
      runTest "parse simple lambda" $
        assertParse "lambda" "x: x" (ELambda (FormalName "x") (EResolvedVar 0 0) NoCaptureInfo),
      runTest "parse set pattern lambda" $
        assertParse
          "set pattern"
          "{ a, b }: a"
          ( ELambda
              (FormalSet [Formal "a" Nothing, Formal "b" Nothing] False)
              (EResolvedVar 0 0)
              NoCaptureInfo
          ),
      runTest "parse set pattern with defaults" $
        assertParse
          "defaults"
          "{ a ? 1 }: a"
          ( ELambda
              (FormalSet [Formal "a" (Just (ELit (NixInt 1)))] False)
              (EResolvedVar 0 0)
              NoCaptureInfo
          ),
      runTest "parse set pattern with ellipsis" $
        assertParse
          "ellipsis"
          "{ a, ... }: a"
          ( ELambda
              (FormalSet [Formal "a" Nothing] True)
              (EResolvedVar 0 0)
              NoCaptureInfo
          ),
      runTest "parse named set pattern (name@{...})" $
        assertParse
          "named set"
          "args@{ a }: a"
          ( ELambda
              (FormalNamedSet "args" [Formal "a" Nothing] False)
              (EResolvedVar 0 1)
              NoCaptureInfo
          ),
      runTest "parse named set pattern ({...}@name)" $
        assertParse
          "set@name"
          "{ a }@args: a"
          ( ELambda
              (FormalNamedSet "args" [Formal "a" Nothing] False)
              (EResolvedVar 0 1)
              NoCaptureInfo
          ),
      -- Application
      runTest "parse application" $
        assertParse "app" "f x" (EApp (EVar "f") (EVar "x")),
      runTest "parse left-assoc application" $
        assertParse "app left" "f x y" (EApp (EApp (EVar "f") (EVar "x")) (EVar "y")),
      runTest "parse application with parens" $
        assertParse
          "app parens"
          "f (1 + 2)"
          (EApp (EVar "f") (EBinary OpAdd (ELit (NixInt 1)) (ELit (NixInt 2)))),
      -- Select
      runTest "parse select" $
        assertParse "select" "a.b" (ESelect (EVar "a") [StaticKey "b"] Nothing),
      runTest "parse nested select" $
        assertParse
          "nested select"
          "a.b.c"
          (ESelect (EVar "a") [StaticKey "b", StaticKey "c"] Nothing),
      runTest "parse select or default" $
        assertParse
          "select or"
          "a.b or 1"
          (ESelect (EVar "a") [StaticKey "b"] (Just (ELit (NixInt 1)))),
      runTest "parse has-attr" $
        assertParse "has-attr" "a ? b" (EHasAttr (EVar "a") [StaticKey "b"]),
      -- Attr sets
      runTest "parse empty attrs" $
        assertParse "empty attrs" "{ }" (EAttrs False [] NoCaptureInfo),
      runTest "parse attrs with binding" $
        assertParse
          "attrs"
          "{ a = 1; }"
          (EAttrs False [NamedBinding [StaticKey "a"] (ELit (NixInt 1))] NoCaptureInfo),
      runTest "parse rec attrs" $
        assertParse
          "rec attrs"
          "rec { a = 1; }"
          (EAttrs True [NamedBinding [StaticKey "a"] (ELit (NixInt 1))] NoCaptureInfo),
      -- inherit x y; is desugared to x = x; y = y; by the resolution pass
      -- (needed because lambda formals are positional, not name-based).
      runTest "parse inherit" $
        assertParse
          "inherit"
          "{ inherit x y; }"
          (EAttrs False [NamedBinding [StaticKey "x"] (EVar "x"), NamedBinding [StaticKey "y"] (EVar "y")] NoCaptureInfo),
      runTest "parse inherit from" $
        assertParse
          "inherit from"
          "{ inherit (a) x; }"
          (EAttrs False [Inherit (Just (EVar "a")) ["x"]] NoCaptureInfo),
      -- Let/if/with/assert
      runTest "parse let" $
        assertParse
          "let"
          "let x = 1; in x"
          (ELet [NamedBinding [StaticKey "x"] (ELit (NixInt 1))] (EResolvedVar 0 0) NoCaptureInfo),
      runTest "parse if-then-else" $
        assertParse
          "if"
          "if true then 1 else 2"
          (EIf (ELit (NixBool True)) (ELit (NixInt 1)) (ELit (NixInt 2))),
      runTest "parse with" $
        assertParse
          "with"
          "with a; b"
          (EWith (EVar "a") (EWithVar "b")),
      runTest "parse assert" $
        assertParse
          "assert"
          "assert true; 1"
          (EAssert (ELit (NixBool True)) (ELit (NixInt 1))),
      -- Lists
      runTest "parse empty list" $
        assertParse "empty list" "[ ]" (EList []),
      runTest "parse list elements" $
        assertParse
          "list"
          "[ 1 2 3 ]"
          (EList [ELit (NixInt 1), ELit (NixInt 2), ELit (NixInt 3)]),
      -- Parens
      runTest "parse parens" $
        assertParse "parens" "(42)" (ELit (NixInt 42)),
      -- 'or' as identifier
      runTest "or as identifier" $
        assertParse "or ident" "or" (EVar "or"),
      -- 'or' as attr key
      runTest "or as attr key" $
        assertParse
          "or attr key"
          "{ or = 1; }"
          (EAttrs False [NamedBinding [StaticKey "or"] (ELit (NixInt 1))] NoCaptureInfo)
    ]

-- ---------------------------------------------------------------------------
-- Tests: Parser errors
-- ---------------------------------------------------------------------------

testParserErrors :: IO [Bool]
testParserErrors = do
  putStrLn "parser/errors"
  sequence
    [ runTest "empty input" $
        assertLeft "empty" (parseNix "<test>" ""),
      runTest "unclosed paren" $
        assertLeft "unclosed paren" (parseNix "<test>" "(1"),
      runTest "unclosed string" $
        assertLeft "unclosed string" (parseNix "<test>" "\"hello"),
      runTest "unclosed brace" $
        assertLeft "unclosed brace" (parseNix "<test>" "{ a = 1;"),
      runTest "missing semicolon" $
        assertLeft "missing semi" (parseNix "<test>" "{ a = 1 }"),
      runTest "unclosed bracket" $
        assertLeft "unclosed bracket" (parseNix "<test>" "[ 1 2"),
      runTest "unexpected token" $
        assertLeft "unexpected" (parseNix "<test>" ")")
    ]

-- ---------------------------------------------------------------------------
-- Tests: Parser integration
-- ---------------------------------------------------------------------------

testParserIntegration :: IO [Bool]
testParserIntegration = do
  putStrLn "parser/integration"
  sequence
    [ runTest "shell.nix pattern" $
        assertRight "shell.nix" (parseNix "<test>" "{ pkgs ? import <nixpkgs> {} }: pkgs.mkShell { buildInputs = [ pkgs.ghc ]; }") $ \case
          ELambda {} -> Pass
          other -> Fail ("expected ELambda, got: " <> T.pack (show other)),
      runTest "let with multiple bindings" $
        assertParse
          "multi-let"
          "let x = 1; y = 2; in x + y"
          ( ELet
              [ NamedBinding [StaticKey "x"] (ELit (NixInt 1)),
                NamedBinding [StaticKey "y"] (ELit (NixInt 2))
              ]
              (EBinary OpAdd (EResolvedVar 0 0) (EResolvedVar 0 1))
              NoCaptureInfo
          ),
      runTest "nested attr set" $
        -- Normalization hoists a nested attrpath into nested literal sets
        -- (upstream parser.y addAttr), so a.b.c = 1 parses like
        -- a = { b = { c = 1; }; }.
        assertParse
          "nested attrs"
          "{ a.b.c = 1; d = { e = 2; }; }"
          ( EAttrs
              False
              [ NamedBinding
                  [StaticKey "a"]
                  ( EAttrs
                      False
                      [ NamedBinding
                          [StaticKey "b"]
                          (EAttrs False [NamedBinding [StaticKey "c"] (ELit (NixInt 1))] NoCaptureInfo)
                      ]
                      NoCaptureInfo
                  ),
                NamedBinding [StaticKey "d"] (EAttrs False [NamedBinding [StaticKey "e"] (ELit (NixInt 2))] NoCaptureInfo)
              ]
              NoCaptureInfo
          ),
      runTest "indented string" $
        assertRight "ind string" (parseNix "<test>" "''hello''") $ \case
          EIndStr _ -> Pass
          other -> Fail ("expected EIndStr, got: " <> T.pack (show other)),
      -- Positional let/rec resolution tests
      runTest "let inherit from outer lambda" $
        assertRight "let-inherit-lambda" (parseNix "<test>" "x: let inherit x; in x") $ \case
          -- x: let inherit x; in x
          -- The lambda formal x is at level 0, index 0.
          -- The let scope is level 0 (for the let body).
          -- inherit x desugars to x = x where RHS resolves against outer
          -- (the lambda scope), so the let binding's RHS is EResolvedVar 0 0
          -- (one level up from the let to the lambda).
          -- The body x resolves to level 0, index 0 (the let scope).
          ELambda _ (ELet [NamedBinding [StaticKey "x"] _rhsExpr] (EResolvedVar 0 0) _) _ -> Pass
          other -> Fail ("expected ELambda with let-inherit, got: " <> T.pack (show other)),
      runTest "nested lambda in let" $
        assertEval
          "let-nested-lambda"
          "let f = x: x + 1; g = y: f y; in g 5"
          (VInt 6),
      runTest "rec attrs positional resolution" $
        assertEval
          "rec-positional"
          "let s = rec { a = 1; b = a + 1; }; in s.b"
          (VInt 2)
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
      -- other store-path parse boundary ('E' is outside nix-base32).
      runTest "storePath rejects a non-base32 hash" $
        assertEvalFail
          "storePath-badhash"
          "builtins.storePath \"/nix/store/EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE-x\"",
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
        assertEval "nixVersion" "builtins.nixVersion" (mkStr "2.24.0"),
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
        assertEval "funcArgs-simple" "builtins.functionArgs (x: x)" (VAttrs (attrSetFromMap Map.empty)),
      runTest "functionArgs type error" $
        assertEvalFail "funcArgs-err" "builtins.functionArgs 42",
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
          (VInt 2)
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

-- | Parse and evaluate Nix source using the IO evaluator.
evalNixIO :: FilePath -> Text -> IO (Either Text NixValue)
evalNixIO baseDir source = case parseNix "<test>" source of
  Left err -> pure (Left (T.pack (show err)))
  Right expr -> do
    st <- newEvalState baseDir
    runEvalIO st (eval (builtinEnv (esTimestamp st) (esSearchPaths st)) expr)

-- | Run a named IO eval test - single label, no double-wrapping.
runTestIO :: Text -> FilePath -> Text -> NixValue -> IO Bool
runTestIO label baseDir source expected = do
  result <- evalNixIO baseDir source
  runTest label $ assertRight label result $ \actual ->
    assertEqual label expected actual

-- | Run a named IO eval test that should fail.
runTestIOFail :: Text -> FilePath -> Text -> IO Bool
runTestIOFail label baseDir source = do
  result <- evalNixIO baseDir source
  runTest label $ assertLeft label result

-- | Quoted path literal for embedding absolute paths in Nix source.
nixQuotedPath :: FilePath -> Text
nixQuotedPath p = T.pack (show p)

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

-- | Whether this machine can materialize eval outputs at the platform
-- store root (eval-time store writes resolve there by design).  Absent
-- and uncreatable - macOS's sealed read-only /, a root-owned /nix on a
-- Nix-installed Linux box - the store-writing eval tests skip loudly
-- rather than fail on machines that cannot host a store.  Linux CI
-- provisions /nix so coverage stays real there.
storeRootAvailable :: IO Bool
storeRootAvailable = do
  outcome <- try (createDirectoryIfMissing True (T.unpack platformStoreDirText))
  case outcome of
    Left (_ :: SomeException) -> pure False
    Right () -> writable <$> getPermissions (T.unpack platformStoreDirText)

-- | builtins.path/filterSource with a filter: the tree is serialized with
-- rejected entries removed (a rejected directory prunes its subtree),
-- content-addressed over the FILTERED NAR, and materialized to the store.
testPathFilterIO :: IO [Bool]
testPathFilterIO = do
  putStrLn "eval/path-filter-io"
  usableStore <- storeRootAvailable
  if not usableStore
    then do
      putStrLn "  SKIP  platform store root unavailable; cannot materialize eval outputs here"
      pure []
    else testPathFilterBody

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

-- | Whether this host can create symlinks (Windows needs Developer Mode
-- or elevation).  The symlink-walk fixtures cannot be built without it,
-- so their groups skip loudly rather than fail.
symlinksAvailable :: IO Bool
symlinksAvailable = do
  tmpBase <- getTemporaryDirectory
  let probeDir = tmpBase </> "nova-nix-test-link-probe"
  Dir.removePathForcibly probeDir
  createDirectoryIfMissing True probeDir
  BS.writeFile (probeDir </> "target.txt") "t"
  outcome <- try (Dir.createFileLink "target.txt" (probeDir </> "link"))
  Dir.removePathForcibly probeDir
  pure $ case (outcome :: Either SomeException ()) of
    Left _ -> False
    Right () -> True

-- | Watchdog for cycle-termination tests: with symlinks as leaves every
-- walk returns promptly, while a regression to link-following recurses
-- forever - and a hung suite is worse than a failed one.
walkWatchdogMicros :: Int
walkWatchdogMicros = 30 * 1000 * 1000

-- | @builtins.path@ with no filter over a tree that contains a symlink.
-- The store-path name comes from the NAR serialization, which records
-- the link as a leaf entry - so the copy must replicate the link for
-- the stored bytes to still match the recorded content address.
testPathSymlinkIO :: IO [Bool]
testPathSymlinkIO = do
  putStrLn "eval/path-symlink-io"
  usableStore <- storeRootAvailable
  canLink <- symlinksAvailable
  if not (usableStore && canLink)
    then do
      putStrLn "  SKIP  needs a writable platform store root and symlink privilege"
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
        makeFixedOutputPath name "sha256" "recursive" (sha256Digest (NAR.serialise entry))
  results <- case (spFor "path-symlink-src" linkEntry, spFor "path-symlink-cycle" cycleEntry) of
    (Right linkSp, Right cycleSp) -> do
      -- The same mapping the evaluator's copy uses for its destination.
      let linkDest = storePathToFilePath platformStoreDir linkSp
          cycleDest = storePathToFilePath platformStoreDir cycleSp
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
      setup = do
        createDirectoryIfMissing True subDir
        TIO.writeFile (testDir </> "literal.nix") "42"
        TIO.writeFile (testDir </> "expr.nix") "1 + 2"
        TIO.writeFile (testDir </> "nested-inner.nix") "99"
        TIO.writeFile (testDir </> "nested-outer.nix") "import ./nested-inner.nix"
        TIO.writeFile (testDir </> "attrset.nix") "{ x = 1; y = 2; }"
        TIO.writeFile (testDir </> "uses-arg.nix") "let f = x: x + 10; in f 5"
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
        runTestIOFail "import nonexistent -> error" testDir "import ./nonexistent.nix",
        runTestIO "import attrset + select" testDir "(import ./attrset.nix).x" (VInt 1),
        runTestIO "import let/lambda" testDir "import ./uses-arg.nix" (VInt 15),
        -- import accepts strings (real Nix coerces string to path)
        runTestIO "import accepts string" testDir "import \"./literal.nix\"" (VInt 42),
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
        assertEval "toPath-abs" "builtins.toPath \"/foo/bar\"" (VPath "/foo/bar"),
      runTest "toPath rejects relative" $
        assertEvalFail "toPath-rel" "builtins.toPath \"foo/bar\"",
      runTest "toPath passthrough VPath" $
        assertEval "toPath-vpath" "builtins.toPath (builtins.toPath \"/foo/bar\")" (VPath "/foo/bar"),
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
-- Tests: Batch G - ATerm serialization
-- ---------------------------------------------------------------------------

testBatchG :: IO [Bool]
testBatchG = do
  putStrLn "derivation/aterm"
  let minimalDrv =
        Derivation
          { drvOutputs = [],
            drvInputDrvs = Map.empty,
            drvInputSrcs = [],
            drvPlatform = X86_64_Linux,
            drvBuilder = "/bin/sh",
            drvArgs = [],
            drvEnv = Map.empty
          }
  let drvWithOutput =
        minimalDrv
          { drvOutputs =
              [ DerivationOutput
                  { doName = "out",
                    doPath = StorePath "abc" "hello",
                    doHashAlgo = "",
                    doHash = ""
                  }
              ]
          }
  let drvWithEnv =
        minimalDrv
          { drvEnv = Map.fromList [("name", "hello"), ("system", "x86_64-linux")]
          }
  sequence
    [ runTest "ATerm minimal" $
        let aterm = toATerm minimalDrv
         in if BS.isPrefixOf "Derive(" aterm && BS.isSuffixOf ")" aterm
              then Pass
              else Fail ("bad ATerm: " <> bytesText aterm),
      runTest "ATerm has output" $
        let aterm = toATerm drvWithOutput
         in if "\"out\"" `BS.isInfixOf` aterm
              then Pass
              else Fail ("missing output in ATerm: " <> bytesText aterm),
      runTest "ATerm env sorted" $
        let aterm = toATerm drvWithEnv
         in -- "name" should come before "system" in sorted order
            case (BS.breakSubstring "\"name\"" aterm, BS.breakSubstring "\"system\"" aterm) of
              ((before1, _), (before2, _)) ->
                if BS.length before1 < BS.length before2
                  then Pass
                  else Fail ("env not sorted in ATerm: " <> bytesText aterm),
      runTest "ATerm string escaping" $
        let drv = minimalDrv {drvEnv = Map.fromList [("msg", "hello\nworld")]}
            aterm = toATerm drv
         in if "\\n" `BS.isInfixOf` aterm
              then Pass
              else Fail ("missing escaped newline: " <> bytesText aterm),
      runTest "ATerm deterministic" $
        assertEqual "deterministic" (toATerm minimalDrv) (toATerm minimalDrv),
      -- platformToText
      runTest "platformToText linux" $
        assertEqual "linux" "x86_64-linux" (platformToText X86_64_Linux),
      runTest "platformToText darwin" $
        assertEqual "darwin" "x86_64-darwin" (platformToText X86_64_Darwin),
      runTest "platformToText aarch64-darwin" $
        assertEqual "aarch64" "aarch64-darwin" (platformToText Aarch64_Darwin),
      runTest "platformToText windows" $
        assertEqual "windows" "x86_64-windows" (platformToText X86_64_Windows),
      runTest "platformToText aarch64-linux" $
        assertEqual "aarch64-linux" "aarch64-linux" (platformToText Aarch64_Linux),
      runTest "platformToText other" $
        assertEqual "other" "riscv64-freebsd" (platformToText (OtherPlatform "riscv64-freebsd"))
    ]

-- ---------------------------------------------------------------------------
-- Tests: Batch H - derivation
-- ---------------------------------------------------------------------------

testBatchH :: IO [Bool]
testBatchH = do
  putStrLn "eval/builtins-batchH"
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
          assertEqual "deterministic" (VBool True) val
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
      runTest "getContext on plain string" $
        assertRight "getCtx-plain" (evalNix "builtins.getContext \"hello\"") $ \val ->
          case val of
            VAttrs m -> if attrSetNull m then Pass else Fail "expected empty attrset"
            _ -> Fail ("expected VAttrs, got " <> T.pack (show val)),
      -- getContext: drv outPath has outputs entry
      runTest "getContext on drv outPath"
        $ assertRight
          "getCtx-drv"
          (evalNix "builtins.getContext (derivation { name = \"test\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; }).outPath")
        $ \val -> case val of
          VAttrs m ->
            if attrSetSize m == 1
              then Pass
              else Fail ("expected 1 entry, got " <> T.pack (show (attrSetSize m)))
          _ -> Fail ("expected VAttrs, got " <> T.pack (show val)),
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
-- Tests: DependencyGraph (Phase 3, Batch 5)
-- ---------------------------------------------------------------------------

testDepGraph :: IO [Bool]
testDepGraph = do
  putStrLn "dep-graph"
  let mkSP = StorePath
      spA = mkSP "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "a.drv"
      spB = mkSP "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" "b.drv"
      spC = mkSP "cccccccccccccccccccccccccccccccccc" "c.drv"
      spD = mkSP "dddddddddddddddddddddddddddddddd" "d.drv"
      baseDrv =
        Derivation
          { drvOutputs = [],
            drvInputDrvs = Map.empty,
            drvInputSrcs = [],
            drvPlatform = X86_64_Linux,
            drvBuilder = "/bin/sh",
            drvArgs = [],
            drvEnv = Map.empty
          }
      -- Single node: A has no deps
      drvA = baseDrv
      -- Linear chain: B depends on C
      drvB = baseDrv {drvInputDrvs = Map.singleton spC ["out"]}
      -- C has no deps
      drvC = baseDrv
      -- Diamond: D depends on B and C, B depends on C
      drvD = baseDrv {drvInputDrvs = Map.fromList [(spB, ["out"]), (spC, ["out"])]}
      -- Cycle: A depends on B, B depends on A
      drvACycle = baseDrv {drvInputDrvs = Map.singleton spB ["out"]}
      drvBCycle = baseDrv {drvInputDrvs = Map.singleton spA ["out"]}
      readSingle _ = Left "not found"
      readChain sp
        | sp == spC = Right drvC
        | otherwise = Left ("unknown drv: " <> spName sp)
      readDiamond sp
        | sp == spB = Right drvB
        | sp == spC = Right drvC
        | otherwise = Left ("unknown drv: " <> spName sp)
      readCycle sp
        | sp == spB = Right drvBCycle
        | sp == spA = Right drvACycle
        | otherwise = Left ("unknown drv: " <> spName sp)
      -- Self-loop: A depends on itself
      drvSelf = baseDrv {drvInputDrvs = Map.singleton spA ["out"]}
      readSelf sp
        | sp == spA = Right drvSelf
        | otherwise = Left ("unknown drv: " <> spName sp)
  sequence
    [ -- Single node
      runTest "single node graph" $ case DepGraph.buildDepGraph readSingle drvA spA of
        Right (DepGraph.DepGraph g) -> assertEqual "single-size" 1 (Map.size g)
        Left err -> Fail ("unexpected error: " <> err),
      -- Linear chain A to C: topoSort should give [C, A]
      runTest "linear chain topo" $ case DepGraph.buildDepGraph readChain drvB spB of
        Right graph -> case DepGraph.topoSort graph of
          DepGraph.TopoSorted order ->
            case order of
              [first, _] | first == spC -> Pass
              _ -> Fail ("bad order: " <> T.pack (show order))
          DepGraph.TopoCycle cyc -> Fail ("unexpected cycle: " <> T.pack (show cyc))
        Left err -> Fail ("graph build failed: " <> err),
      -- Diamond D to B,C; B to C: topoSort should have C first, D last
      runTest "diamond topo" $ case DepGraph.buildDepGraph readDiamond drvD spD of
        Right graph -> case DepGraph.topoSort graph of
          DepGraph.TopoSorted order ->
            case order of
              [first, _, lastElem] | first == spC, lastElem == spD -> Pass
              _ -> Fail ("bad diamond order: " <> T.pack (show order))
          DepGraph.TopoCycle cyc -> Fail ("unexpected cycle: " <> T.pack (show cyc))
        Left err -> Fail ("graph build failed: " <> err),
      -- transitiveDeps
      runTest "transitiveDeps diamond" $ case DepGraph.buildDepGraph readDiamond drvD spD of
        Right graph ->
          let deps = DepGraph.transitiveDeps graph spD
           in if Set.size deps == 2 && Set.member spB deps && Set.member spC deps
                then Pass
                else Fail ("bad transitive deps: " <> T.pack (show deps))
        Left err -> Fail ("graph build failed: " <> err),
      -- directDeps
      runTest "directDeps diamond" $ case DepGraph.buildDepGraph readDiamond drvD spD of
        Right graph ->
          let deps = DepGraph.directDeps graph spD
           in assertEqual "direct-count" 2 (length deps)
        Left err -> Fail ("graph build failed: " <> err),
      -- Missing .drv causes failure
      runTest "missing drv fails" $ case DepGraph.buildDepGraph readSingle drvB spB of
        Left _ -> Pass
        Right _ -> Fail "expected failure for missing drv",
      -- Single node topoSort
      runTest "single node topoSort" $ case DepGraph.buildDepGraph readSingle drvA spA of
        Right graph -> case DepGraph.topoSort graph of
          DepGraph.TopoSorted [x] -> assertEqual "single-topo" spA x
          other -> Fail ("unexpected topo result: " <> T.pack (show other))
        Left err -> Fail ("graph build failed: " <> err),
      -- buildDepGraph with mock for cycle detection
      -- (Cycle detection happens at topoSort level, not buildDepGraph)
      runTest "cycle detection" $ case DepGraph.buildDepGraph readCycle drvACycle spA of
        Right graph -> case DepGraph.topoSort graph of
          DepGraph.TopoCycle _ -> Pass
          -- A "sorted" cyclic graph means Kahn's silently dropped the
          -- cycle: buildWithDeps would then loop or build with unbuilt
          -- inputs.
          DepGraph.TopoSorted order ->
            Fail ("cyclic graph topo-sorted as " <> T.pack (show (length order)) <> " nodes")
        -- A loud rejection at graph-build time also counts as detection.
        Left _ -> Pass,
      -- A root that depends on itself: the walk marks the root visited
      -- on enqueue, so it terminates - and per contract the result
      -- excludes the root, leaving nothing.
      runTestM "transitiveDeps terminates on a self-loop" $ do
        outcome <-
          timeout walkWatchdogMicros $
            evaluate $ case DepGraph.buildDepGraph readSelf drvSelf spA of
              Left _ -> Nothing
              Right graph -> Just $! DepGraph.transitiveDeps graph spA
        pure $ case outcome of
          Nothing -> Fail "did not terminate on a self-loop"
          Just Nothing -> Fail "graph build failed"
          Just (Just deps) -> assertEqual "self-loop deps" Set.empty deps,
      -- A two-node cycle already terminated; pin that the root stays
      -- excluded from its own transitive closure.
      runTestM "transitiveDeps two-node cycle excludes the root" $ do
        outcome <-
          timeout walkWatchdogMicros $
            evaluate $ case DepGraph.buildDepGraph readCycle drvACycle spA of
              Left _ -> Nothing
              Right graph -> Just $! DepGraph.transitiveDeps graph spA
        pure $ case outcome of
          Nothing -> Fail "did not terminate on a cycle"
          Just Nothing -> Fail "graph build failed"
          Just (Just deps) -> assertEqual "cycle deps" (Set.singleton spB) deps
    ]

-- ---------------------------------------------------------------------------
-- Tests: Substituter (Phase 3, Batch 6)
-- ---------------------------------------------------------------------------

-- | A fake HTTP body reader: yields the given chunks, then empty
-- forever (an HTTP BodyReader is just an IO ByteString).
chunkReader :: [BS.ByteString] -> IO (IO BS.ByteString)
chunkReader chunks = do
  ref <- newIORef chunks
  pure $
    atomicModifyIORef' ref $ \case
      [] -> ([], BS.empty)
      (c : cs) -> (cs, c)

-- | Drain a chunk source to its byte count - the shape of a streaming
-- consumer, for tests that only care whether the pipeline fails.
drainChunkSource :: IO BS.ByteString -> IO (Either Subst.AttemptFailure Int)
drainChunkSource pull = go 0
  where
    go n = do
      chunk <- pull
      if BS.null chunk
        then pure (Right n)
        else let total = n + BS.length chunk in total `seq` go total

testSubstituter :: IO [Bool]
testSubstituter = do
  putStrLn "substituter"
  sequence
    [ -- sortCaches: priority ordering
      runTest "sortCaches priority ordering" $
        let c1 = Subst.CacheConfig "https://a.example.com" "key-a" 40
            c2 = Subst.CacheConfig "https://b.example.com" "key-b" 10
            c3 = Subst.CacheConfig "https://c.example.com" "key-c" 30
            sorted = Subst.sortCaches [c1, c2, c3]
         in case sorted of
              [s1, s2, s3] ->
                if Subst.ccPriority s1 == 10 && Subst.ccPriority s2 == 30 && Subst.ccPriority s3 == 40
                  then Pass
                  else Fail ("bad order: " <> T.pack (show (map Subst.ccPriority sorted)))
              _ -> Fail "expected 3 caches",
      -- sortCaches: empty list
      runTest "sortCaches empty" $
        assertEqual "empty-sort" [] (Subst.sortCaches []),
      -- readBodyCapped: the client-side body cap (mirror of the server's
      -- readBodyLimited).  A body reader is just IO ByteString yielding
      -- chunks then empty.
      runTestM "readBodyCapped concatenates under the cap" $ do
        reader <- chunkReader ["abc", "def", "g"]
        body <- Subst.readBodyCapped 10 reader
        pure (assertEqual "under cap" (Just "abcdefg") body),
      runTestM "readBodyCapped allows exactly the cap" $ do
        reader <- chunkReader ["abcde", "fghij"]
        body <- Subst.readBodyCapped 10 reader
        pure (assertEqual "at cap" (Just "abcdefghij") body),
      runTestM "readBodyCapped aborts past the cap" $ do
        reader <- chunkReader ["abcdef", "ghijkl"]
        body <- Subst.readBodyCapped 10 reader
        pure (assertEqual "over cap" Nothing body),
      -- decompressorFor decides support from the narinfo's declared
      -- values alone, so unsupported compression rejects before any
      -- NAR download; xz resolves a bounded decompressor.
      runTest "decompressorFor xz resolves" $
        case Subst.decompressorFor 1024 "xz" of
          Right _ -> Pass
          Left err -> Fail ("xz rejected: " <> err),
      runTestM "decompressorFor none is identity" $
        case Subst.decompressorFor 5 "none" of
          Right decompress -> assertEqual "identity" (Right "bytes") <$> decompress "bytes"
          Left err -> pure (Fail ("expected none to be supported, got: " <> err)),
      -- verifyNarSize: the declared NarSize is a signed claim that flows
      -- into the store DB, so it must equal the downloaded byte count.
      runTest "verifyNarSize accepts matching size" $
        assertEqual
          "narsize-match"
          (Right ())
          (Subst.verifyNarSize ((sampleNarInfo sampleNarHash) {NarInfo.niNarSize = toInteger (BS.length sampleNarBytes)}) sampleNarBytes),
      runTest "verifyNarSize rejects mismatched size" $
        case Subst.verifyNarSize ((sampleNarInfo sampleNarHash) {NarInfo.niNarSize = toInteger (BS.length sampleNarBytes) + 1}) sampleNarBytes of
          Left err | "size mismatch" `T.isInfixOf` err -> Pass
          other -> Fail ("expected size mismatch, got: " <> T.pack (show other)),
      -- decompressNar: "none" passes through
      runTestM "decompressNar none" $
        let input = "fake nar data"
         in assertEqual "decompress-none" (Right input) <$> Subst.decompressNar (toInteger (BS.length input)) "none" input,
      -- decompressNar: an empty Compression field means bzip2 upstream
      -- (the field's historical default), never identity, so the empty
      -- spelling must decode a real bzip2 body exactly as the named
      -- one does.
      runTestM "decompressNar empty means bzip2" $
        case B64.decode bzip2FixtureB64 of
          Left err -> pure (Fail ("fixture base64 does not decode: " <> T.pack err))
          Right compressed -> do
            out <- Subst.decompressNar xzFixtureSize "" compressed
            pure $ case out of
              Right decoded -> assertEqual "empty-is-bzip2" xzFixturePayload decoded
              Left err -> Fail ("empty Compression failed to decode as bzip2: " <> err),
      -- decompressNar: bzip2 decodes under the same declared-NarSize
      -- bound as the other codecs; the fixture comes from the bzip2
      -- CLI, so the decoder is checked against the format's own
      -- reference encoder rather than its own library.
      runTestM "decompressNar bzip2 bounded roundtrip" $
        case B64.decode bzip2FixtureB64 of
          Left err -> pure (Fail ("fixture base64 does not decode: " <> T.pack err))
          Right compressed -> do
            out <- Subst.decompressNar xzFixtureSize "bzip2" compressed
            pure $ case out of
              Right decoded -> assertEqual "bzip2-roundtrip" xzFixturePayload decoded
              Left err -> Fail ("bzip2 roundtrip failed: " <> err),
      runTestM "decompressNar bzip2 over-bound rejects" $
        case B64.decode bzip2FixtureB64 of
          Left err -> pure (Fail ("fixture base64 does not decode: " <> T.pack err))
          Right compressed -> do
            out <- Subst.decompressNar (xzFixtureSize - 1) "bzip2" compressed
            pure $ case out of
              Left err | "NarSize" `T.isInfixOf` err -> Pass
              other -> Fail ("expected over-bound rejection, got: " <> T.pack (show other)),
      -- decompressNar: xz decodes under the declared-NarSize bound
      -- (the fixture is a real 112-byte xz stream of 1792 payload
      -- bytes, embedded base64 to keep the source ASCII).
      runTestM "decompressNar xz bounded roundtrip" $
        case B64.decode xzFixtureB64 of
          Left err -> pure (Fail ("fixture base64 does not decode: " <> T.pack err))
          Right compressed -> do
            out <- Subst.decompressNar xzFixtureSize "xz" compressed
            pure $ case out of
              Right decoded -> assertEqual "xz-roundtrip" xzFixturePayload decoded
              Left err -> Fail ("xz roundtrip failed: " <> err),
      -- The output bound is exact: one byte under the real size must
      -- refuse, naming the declared NarSize.
      runTestM "decompressNar xz over-bound rejects" $
        case B64.decode xzFixtureB64 of
          Left err -> pure (Fail ("fixture base64 does not decode: " <> T.pack err))
          Right compressed -> do
            out <- Subst.decompressNar (xzFixtureSize - 1) "xz" compressed
            pure $ case out of
              Left err | "NarSize" `T.isInfixOf` err -> Pass
              other -> Fail ("expected over-bound rejection, got: " <> T.pack (show other)),
      -- decompressNar: bytes that are not an xz stream are an error,
      -- never silently passed through
      runTestM "decompressNar xz garbage rejects" $ do
        out <- Subst.decompressNar 64 "xz" "not an xz stream"
        pure $ case out of
          Left _ -> Pass
          Right _ -> Fail "garbage decoded as xz",
      -- decompressNar: zstd decodes under the same declared-NarSize
      -- bound; the fixture compresses with the sublibrary's own
      -- encoder, the same pairing the push path ships.
      runTestM "decompressNar zstd bounded roundtrip" $ do
        out <- Subst.decompressNar xzFixtureSize "zstd" zstdFixtureCompressed
        pure $ case out of
          Right decoded -> assertEqual "zstd-roundtrip" xzFixturePayload decoded
          Left err -> Fail ("zstd roundtrip failed: " <> err),
      runTestM "decompressNar zstd over-bound rejects" $ do
        out <- Subst.decompressNar (xzFixtureSize - 1) "zstd" zstdFixtureCompressed
        pure $ case out of
          Left err | "NarSize" `T.isInfixOf` err -> Pass
          other -> Fail ("expected over-bound rejection, got: " <> T.pack (show other)),
      -- decompressNar: unknown compression
      runTestM "decompressNar unknown" $ do
        out <- Subst.decompressNar 4 "brotli" "data"
        pure $ case out of
          Left _ -> Pass
          Right _ -> Fail "expected error for unknown",
      -- parseReferences: narinfo references are wire-format basenames
      runTest "parseReferences basenames" $
        let refs = ["aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-hello", "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb-dep-2.0"]
            expected =
              [ StorePath "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "hello",
                StorePath "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" "dep-2.0"
              ]
         in assertEqual "parse-refs" (Right expected) (Subst.parseReferences refs),
      -- parseReferences: a malformed token is a loud error, never dropped
      runTest "parseReferences malformed rejected" $
        case Subst.parseReferences ["not-a-store-path"] of
          Left _ -> Pass
          Right refs -> Fail ("expected rejection, got: " <> T.pack (show refs)),
      -- parseDeriver: basename becomes the DB's full path text
      runTest "parseDeriver basename" $
        let sp = StorePath "cccccccccccccccccccccccccccccccc" "hello.drv"
            expected = Just (T.pack (storePathToFilePath defaultStoreDir sp))
         in assertEqual "parse-deriver" (Right expected) (Subst.parseDeriver defaultStoreDir (Just "cccccccccccccccccccccccccccccccc-hello.drv")),
      -- parseDeriver: upstream's unknown-deriver sentinel means no deriver
      runTest "parseDeriver unknown sentinel" $
        assertEqual "parse-deriver-unknown" (Right Nothing) (Subst.parseDeriver defaultStoreDir (Just "unknown-deriver")),
      -- parseDeriver: absent stays absent, malformed is an error
      runTest "parseDeriver absent and malformed" $
        case (Subst.parseDeriver defaultStoreDir Nothing, Subst.parseDeriver defaultStoreDir (Just "bogus")) of
          (Right Nothing, Left _) -> Pass
          other -> Fail ("unexpected: " <> T.pack (show other)),
      -- Recorded production narinfos from cache.nixos.org (paths this
      -- codebase did not produce): the parse -> validate -> signature
      -- pipeline against real cache data, the production ed25519
      -- signature verified offline with the shipped public key.
      runTest "recorded cache.nixos.org narinfo verifies end to end" $
        case NarInfo.parseNarInfo recordedHelloNarInfo of
          Left err -> Fail ("parse failed: " <> T.pack err)
          Right ni ->
            case Subst.validateNarInfoFields ni >> Subst.verifySigs Subst.defaultCacheConfig ni of
              Left err -> Fail ("validate/verify failed: " <> err)
              Right () -> assertEqual "compression" "xz" (NarInfo.niCompression ni),
      -- References of a path built by Hydra, including the self
      -- reference, and its Deriver - both produced by upstream Nix.
      runTest "recorded narinfo references and deriver parse" $
        case NarInfo.parseNarInfo recordedHelloNarInfo of
          Left err -> Fail ("parse failed: " <> T.pack err)
          Right ni ->
            case (Subst.parseReferences (NarInfo.niReferences ni), Subst.parseDeriver defaultStoreDir (NarInfo.niDeriver ni)) of
              (Right refs, Right (Just _)) ->
                assertEqual
                  "reference hashes"
                  ["dska0s3gxxd8azbsn85pwm6xcqhivldw", "jppkr0h8aap5z7m4xy4vg5yrwlly9h2v"]
                  (map spHash refs)
              other -> Fail ("references/deriver did not parse: " <> T.pack (show other)),
      -- The empty References line ("References: " with a trailing
      -- space) production caches emit for leaf paths.
      runTest "recorded no-reference narinfo verifies" $
        case NarInfo.parseNarInfo recordedHelloDocNarInfo of
          Left err -> Fail ("parse failed: " <> T.pack err)
          Right ni ->
            case Subst.validateNarInfoFields ni >> Subst.verifySigs Subst.defaultCacheConfig ni >> Subst.parseReferences (NarInfo.niReferences ni) of
              Right [] -> Pass
              other -> Fail ("expected verified empty references, got: " <> T.pack (show other)),
      -- The fingerprint covers NarSize: altering the signed claim must
      -- break the production signature.
      runTest "tampered recorded narinfo fails signature" $
        case NarInfo.parseNarInfo recordedHelloNarInfo of
          Left err -> Fail ("parse failed: " <> T.pack err)
          Right ni ->
            case Subst.verifySigs Subst.defaultCacheConfig (ni {NarInfo.niNarSize = NarInfo.niNarSize ni + 1}) of
              Left _ -> Pass
              Right () -> Fail "signature verified over an altered NarSize",
      -- Streaming pipeline: the chunk-fed download materializes,
      -- hashes, and verifies without ever holding the NAR.  The tree
      -- carries the interesting shapes: nesting, an executable, a
      -- symlink, and a sibling pair colliding on folding filesystems -
      -- the digest equality proves the materialized tree re-serialises
      -- to its NAR on every platform strategy.
      -- Every length word, tag, and padding run must survive splitting
      -- across Await boundaries, so the whole materialization runs at
      -- several chunk sizes including single-byte feeds.
      runTestM "streaming unpack materializes and verifies across chunk sizes" $ do
        tmpBase <- getTemporaryDirectory
        let tmpDir = tmpBase </> "nova-nix-test-stream-unpack"
            chunkSizes = [1, 7, 8, 11] :: [Int]
            runAt n = do
              let dest = tmpDir </> ("out-" <> show n)
              source <- chunkReader (streamChunks n streamTestNar)
              result <- Subst.consumeNarStream dest (streamTestNarInfo streamTestNar) streamTestDigest source
              onDisk <- NAR.serialiseFromPath dest
              pure $ case result of
                Left err ->
                  Just ("chunk size " <> T.pack (show n) <> ": " <> Subst.attemptFailureMessage err)
                Right narByteCount
                  | narByteCount == BS.length streamTestNar
                      && CHash.hashBytes (NAR.serialise onDisk) == streamTestDigest ->
                      Nothing
                  | otherwise ->
                      Just ("chunk size " <> T.pack (show n) <> ": streamed tree diverges from its NAR")
        forceRemoveIfExists tmpDir
        createDirectoryIfMissing True tmpDir
        outcomes <- mapM runAt chunkSizes
        forceRemoveIfExists tmpDir
        pure $ case catMaybes outcomes of
          [] -> Pass
          (msg : _) -> Fail msg,
      -- A truncated stream is a loud parse failure, never a short
      -- tree - and it retries: truncation and a torn transfer
      -- parse-fail the same way.
      runTestM "streaming unpack refuses a truncated stream as transient" $ do
        tmpBase <- getTemporaryDirectory
        let tmpDir = tmpBase </> "nova-nix-test-stream-trunc"
        forceRemoveIfExists tmpDir
        createDirectoryIfMissing True tmpDir
        source <- chunkReader (streamChunks 7 (BS.take (BS.length streamTestNar - 10) streamTestNar))
        result <- Subst.consumeNarStream (tmpDir </> "out") (streamTestNarInfo streamTestNar) streamTestDigest source
        forceRemoveIfExists tmpDir
        pure $ case result of
          Left (Subst.TransientFailure _) -> Pass
          Left (Subst.FatalFailure err) -> Fail ("truncation classified fatal: " <> err)
          Right _ -> Fail "truncated NAR stream was accepted",
      -- A digest mismatch reports before the tree is trusted, and it
      -- is fatal: the size matched, so the transfer completed.
      runTestM "streaming unpack refuses a digest mismatch as fatal" $ do
        tmpBase <- getTemporaryDirectory
        let tmpDir = tmpBase </> "nova-nix-test-stream-digest"
        forceRemoveIfExists tmpDir
        createDirectoryIfMissing True tmpDir
        source <- chunkReader (streamChunks 7 streamTestNar)
        result <- Subst.consumeNarStream (tmpDir </> "out") (streamTestNarInfo streamTestNar) (CHash.hashBytes "not the nar") source
        forceRemoveIfExists tmpDir
        pure $ case result of
          Left (Subst.FatalFailure err) | "hash mismatch" `T.isInfixOf` err -> Pass
          Left other -> Fail ("expected fatal hash mismatch, got: " <> Subst.attemptFailureMessage other)
          Right _ -> Fail "digest mismatch was accepted",
      -- A narinfo lying about NarSize in either direction is refused
      -- at NarDone - the grammar completed, so the mismatch is the
      -- narinfo misdeclaring, and fatal.
      runTestM "streaming unpack refuses an overdeclared NarSize" $ do
        tmpBase <- getTemporaryDirectory
        let tmpDir = tmpBase </> "nova-nix-test-stream-oversize"
            lying = (streamTestNarInfo streamTestNar) {NarInfo.niNarSize = toInteger (BS.length streamTestNar) + 1}
        forceRemoveIfExists tmpDir
        createDirectoryIfMissing True tmpDir
        source <- chunkReader (streamChunks 7 streamTestNar)
        result <- Subst.consumeNarStream (tmpDir </> "out") lying streamTestDigest source
        forceRemoveIfExists tmpDir
        pure $ case result of
          Left (Subst.FatalFailure err) | "size mismatch" `T.isInfixOf` err -> Pass
          Left other -> Fail ("expected fatal size mismatch, got: " <> Subst.attemptFailureMessage other)
          Right _ -> Fail "overdeclared NarSize was accepted",
      runTestM "streaming unpack refuses an underdeclared NarSize" $ do
        tmpBase <- getTemporaryDirectory
        let tmpDir = tmpBase </> "nova-nix-test-stream-undersize"
            lying = (streamTestNarInfo streamTestNar) {NarInfo.niNarSize = toInteger (BS.length streamTestNar) - 1}
        forceRemoveIfExists tmpDir
        createDirectoryIfMissing True tmpDir
        source <- chunkReader (streamChunks 7 streamTestNar)
        result <- Subst.consumeNarStream (tmpDir </> "out") lying streamTestDigest source
        forceRemoveIfExists tmpDir
        pure $ case result of
          Left (Subst.FatalFailure err) | "size mismatch" `T.isInfixOf` err -> Pass
          Left other -> Fail ("expected fatal size mismatch, got: " <> Subst.attemptFailureMessage other)
          Right _ -> Fail "underdeclared NarSize was accepted",
      -- cappedBodySource: the streaming mirror of readBodyCapped -
      -- a body past the key-trusted declared size aborts mid-stream.
      runTestM "cappedBodySource aborts past the cap" $ do
        reader <- chunkReader ["abcdef", "ghijkl"]
        source <- Subst.cappedBodySource 10 reader
        firstChunk <- source
        overCap <- try source :: IO (Either SomeException BS.ByteString)
        pure $ case (firstChunk, overCap) of
          ("abcdef", Left _) -> Pass
          other -> Fail ("expected abort past the cap, got: " <> T.pack (show other)),
      -- withDecompressedSource: xz chunks decompress under the
      -- declared bound; the support decision mirrors decompressorFor.
      runTestM "withDecompressedSource xz chunked roundtrip" $
        case B64.decode xzFixtureB64 of
          Left err -> pure (Fail ("fixture base64 does not decode: " <> T.pack err))
          Right compressed -> do
            source <- chunkReader (streamChunks 7 compressed)
            result <- Subst.withDecompressedSource xzFixtureSize "xz" source $ \pull ->
              let go acc = do
                    chunk <- pull
                    if BS.null chunk
                      then pure (Right (BS.concat (reverse acc)))
                      else go (chunk : acc)
               in go []
            pure $ case result of
              Right out | out == xzFixturePayload -> Pass
              other -> Fail ("xz source roundtrip diverged: " <> T.pack (show (fmap BS.length other))),
      runTestM "withDecompressedSource zstd chunked roundtrip" $ do
        source <- chunkReader (streamChunks 7 zstdFixtureCompressed)
        result <- Subst.withDecompressedSource xzFixtureSize "zstd" source $ \pull ->
          let go acc = do
                chunk <- pull
                if BS.null chunk
                  then pure (Right (BS.concat (reverse acc)))
                  else go (chunk : acc)
           in go []
        pure $ case result of
          Right out | out == xzFixturePayload -> Pass
          other -> Fail ("zstd source roundtrip diverged: " <> T.pack (show (fmap BS.length other))),
      runTestM "withDecompressedSource xz single-byte chunks" $
        case B64.decode xzFixtureB64 of
          Left err -> pure (Fail ("fixture base64 does not decode: " <> T.pack err))
          Right compressed -> do
            source <- chunkReader (streamChunks 1 compressed)
            result <- Subst.withDecompressedSource xzFixtureSize "xz" source $ \pull ->
              let go acc = do
                    chunk <- pull
                    if BS.null chunk
                      then pure (Right (BS.concat (reverse acc)))
                      else go (chunk : acc)
               in go []
            pure $ case result of
              Right out | out == xzFixturePayload -> Pass
              other -> Fail ("xz 1-byte roundtrip diverged: " <> T.pack (show (fmap BS.length other))),
      -- The LIVE pipeline's defenses, not the strict oracle's: hostile
      -- compressed input through withDecompressedSource must land in
      -- the Left channel with the right retry class, never escape as
      -- an exception.
      runTestM "withDecompressedSource xz over-bound is fatal" $
        case B64.decode xzFixtureB64 of
          Left err -> pure (Fail ("fixture base64 does not decode: " <> T.pack err))
          Right compressed -> do
            source <- chunkReader (streamChunks 7 compressed)
            result <- Subst.withDecompressedSource (xzFixtureSize - 1) "xz" source drainChunkSource
            pure $ case result of
              Left (Subst.FatalFailure err) | "NarSize" `T.isInfixOf` err -> Pass
              other -> Fail ("expected fatal over-bound, got: " <> T.pack (show (void other))),
      runTestM "withDecompressedSource xz garbage is transient" $ do
        source <- chunkReader ["not an xz stream"]
        result <- Subst.withDecompressedSource 64 "xz" source drainChunkSource
        pure $ case result of
          Left (Subst.TransientFailure _) -> Pass
          other -> Fail ("expected transient stream error, got: " <> T.pack (show (void other))),
      runTestM "withDecompressedSource xz truncated is transient" $
        case B64.decode xzFixtureB64 of
          Left err -> pure (Fail ("fixture base64 does not decode: " <> T.pack err))
          Right compressed -> do
            source <- chunkReader (streamChunks 7 (BS.take 40 compressed))
            result <- Subst.withDecompressedSource xzFixtureSize "xz" source drainChunkSource
            pure $ case result of
              Left (Subst.TransientFailure _) -> Pass
              other -> Fail ("expected transient truncation, got: " <> T.pack (show (void other))),
      -- The failure contract of the post-download pipeline: every
      -- failing materialization removes the tree it wrote, and a
      -- clean one registers the verified metadata.
      runTestM "materialize registers a verified tree" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-materialize-ok"
        forceRemoveIfExists tmpStore
        createDirectoryIfMissing True tmpStore
        store <- openStore (StoreDir tmpStore)
        let sp = StorePath sampleHash "stream"
            destPath = storePathToFilePath (StoreDir tmpStore) sp
        source <- chunkReader (streamChunks 7 streamTestNar)
        result <- Subst.materializeNarFromSource store sp (streamTestNarInfo streamTestNar) streamTestDigest [] Nothing source
        survived <- doesDirectoryExist destPath
        closeStore store
        forceRemoveIfExists tmpStore
        pure $ case result of
          Left err -> Fail ("materialize failed: " <> Subst.attemptFailureMessage err)
          Right reg
            | not survived -> Fail "verified tree missing after materialize"
            | prNarSize reg /= BS.length streamTestNar -> Fail "registration NarSize diverges"
            | prNarHash reg /= CHash.formatNixHash streamTestDigest -> Fail "registration NarHash diverges"
            | otherwise -> Pass,
      runTestM "materialize removes the tree on a digest mismatch" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-materialize-digest"
        forceRemoveIfExists tmpStore
        createDirectoryIfMissing True tmpStore
        store <- openStore (StoreDir tmpStore)
        let sp = StorePath sampleHash "stream"
            destPath = storePathToFilePath (StoreDir tmpStore) sp
        source <- chunkReader (streamChunks 7 streamTestNar)
        result <- Subst.materializeNarFromSource store sp (streamTestNarInfo streamTestNar) (CHash.hashBytes "not the nar") [] Nothing source
        survived <- doesDirectoryExist destPath
        closeStore store
        forceRemoveIfExists tmpStore
        pure $ case result of
          Right _ -> Fail "digest mismatch was accepted"
          Left (Subst.TransientFailure err) -> Fail ("mismatch classified transient: " <> err)
          Left (Subst.FatalFailure err)
            | not ("hash mismatch" `T.isInfixOf` err) -> Fail ("unexpected failure: " <> err)
            | survived -> Fail "tree survived a digest mismatch"
            | otherwise -> Pass,
      runTestM "materialize removes the tree on a truncated stream" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-materialize-trunc"
        forceRemoveIfExists tmpStore
        createDirectoryIfMissing True tmpStore
        store <- openStore (StoreDir tmpStore)
        let sp = StorePath sampleHash "stream"
            destPath = storePathToFilePath (StoreDir tmpStore) sp
        source <- chunkReader (streamChunks 7 (BS.take (BS.length streamTestNar - 10) streamTestNar))
        result <- Subst.materializeNarFromSource store sp (streamTestNarInfo streamTestNar) streamTestDigest [] Nothing source
        survived <- doesDirectoryExist destPath
        closeStore store
        forceRemoveIfExists tmpStore
        pure $ case result of
          Right _ -> Fail "truncated stream was accepted"
          Left (Subst.FatalFailure err) -> Fail ("truncation classified fatal: " <> err)
          Left (Subst.TransientFailure _)
            | survived -> Fail "tree survived a truncated stream"
            | otherwise -> Pass,
      -- Retry classification: server-side statuses retry, a 404 on an
      -- object the narinfo just promised is deterministic.
      runTest "httpStatusFailure classes" $
        case (Subst.httpStatusFailure 404, Subst.httpStatusFailure 500, Subst.httpStatusFailure 429, Subst.httpStatusFailure 403) of
          (Subst.FatalFailure _, Subst.TransientFailure _, Subst.TransientFailure _, Subst.FatalFailure _) -> Pass
          other -> Fail ("unexpected status classes: " <> T.pack (show other)),
      -- The download cap derives from the SIGNED NarSize; the unsigned
      -- FileSize may only lower it, never raise it.
      runTest "downloadCapFor lets FileSize lower but never raise the cap" $
        let base = streamTestNarInfo streamTestNar
            narSize = NarInfo.niNarSize base
            ceilingCap = Subst.compressedBodyCeiling narSize
            absent = Subst.downloadCapFor base {NarInfo.niFileSize = Nothing}
            lowered = Subst.downloadCapFor base {NarInfo.niFileSize = Just 100}
            inflated = Subst.downloadCapFor base {NarInfo.niFileSize = Just (ceilingCap * 1000)}
         in case (absent, lowered, inflated) of
              (Right capAbsent, Right capLowered, Right capInflated)
                | toInteger capAbsent == ceilingCap
                    && capLowered == 100
                    && toInteger capInflated == ceilingCap ->
                    Pass
              other -> Fail ("unexpected caps: " <> T.pack (show other)),
      runTest "compressedBodyCeiling floors small and scales large" $
        let smallCeiling = Subst.compressedBodyCeiling 10
            largeSize = 64 * 1024 * 1024
            largeCeiling = Subst.compressedBodyCeiling largeSize
         in if smallCeiling == 10 + 64 * 1024 && largeCeiling == largeSize + largeSize `div` 64
              then Pass
              else Fail ("unexpected ceilings: " <> T.pack (show (smallCeiling, largeCeiling))),
      -- The zstd mirror of the xz hostile trio: over-bound output is
      -- deterministic, while a corrupt frame and a frame cut off
      -- mid-way both retry - the codec judges end of input by
      -- libzstd's own frame-boundary signal, so truncation refuses
      -- here instead of yielding a short output.
      runTestM "withDecompressedSource zstd over-bound is fatal" $ do
        source <- chunkReader (streamChunks 7 zstdFixtureCompressed)
        result <- Subst.withDecompressedSource (xzFixtureSize - 1) "zstd" source drainChunkSource
        pure $ case result of
          Left (Subst.FatalFailure err) | "NarSize" `T.isInfixOf` err -> Pass
          other -> Fail ("expected fatal over-bound, got: " <> T.pack (show (void other))),
      runTestM "withDecompressedSource zstd garbage is transient" $ do
        source <- chunkReader ["not a zstd frame"]
        result <- Subst.withDecompressedSource 64 "zstd" source drainChunkSource
        pure $ case result of
          Left (Subst.TransientFailure _) -> Pass
          other -> Fail ("expected transient stream error, got: " <> T.pack (show (void other))),
      runTestM "withDecompressedSource zstd truncated is transient" $ do
        source <- chunkReader (streamChunks 7 (BS.take (BS.length zstdFixtureCompressed - 8) zstdFixtureCompressed))
        result <- Subst.withDecompressedSource xzFixtureSize "zstd" source drainChunkSource
        pure $ case result of
          Left (Subst.TransientFailure _) -> Pass
          other -> Fail ("expected transient truncation, got: " <> T.pack (show (void other))),
      -- The codec refuses a truncated frame, and the whole pipeline
      -- must carry that refusal into the Left channel as retryable
      -- rather than let a short NAR reach the store.
      runTestM "zstd truncated body fails the pipeline as transient" $ do
        tmpBase <- getTemporaryDirectory
        let tmpDir = tmpBase </> "nova-nix-test-zstd-trunc"
            compressedNar = CZstd.compress CZstd.defaultCompressionLevel streamTestNar
        forceRemoveIfExists tmpDir
        createDirectoryIfMissing True tmpDir
        source <- chunkReader (streamChunks 7 (BS.take (BS.length compressedNar - 8) compressedNar))
        result <-
          Subst.withDecompressedSource (toInteger (BS.length streamTestNar)) "zstd" source $
            Subst.consumeNarStream (tmpDir </> "out") ((streamTestNarInfo streamTestNar) {NarInfo.niCompression = "zstd"}) streamTestDigest
        forceRemoveIfExists tmpDir
        pure $ case result of
          Left (Subst.TransientFailure _) -> Pass
          Left (Subst.FatalFailure err) -> Fail ("zstd truncation classified fatal: " <> err)
          Right _ -> Fail "truncated zstd body was accepted",
      -- The bzip2 mirror of the xz hostile trio, over a fixture the
      -- bzip2 CLI produced: over-bound output is deterministic, while
      -- garbage and a truncated stream retry.
      runTestM "withDecompressedSource bzip2 chunked roundtrip" $
        case B64.decode bzip2FixtureB64 of
          Left err -> pure (Fail ("fixture base64 does not decode: " <> T.pack err))
          Right compressed -> do
            source <- chunkReader (streamChunks 7 compressed)
            result <- Subst.withDecompressedSource xzFixtureSize "bzip2" source $ \pull ->
              let go acc = do
                    chunk <- pull
                    if BS.null chunk
                      then pure (Right (BS.concat (reverse acc)))
                      else go (chunk : acc)
               in go []
            pure $ case result of
              Right out | out == xzFixturePayload -> Pass
              other -> Fail ("bzip2 source roundtrip diverged: " <> T.pack (show (fmap BS.length other))),
      runTestM "withDecompressedSource bzip2 over-bound is fatal" $
        case B64.decode bzip2FixtureB64 of
          Left err -> pure (Fail ("fixture base64 does not decode: " <> T.pack err))
          Right compressed -> do
            source <- chunkReader (streamChunks 7 compressed)
            result <- Subst.withDecompressedSource (xzFixtureSize - 1) "bzip2" source drainChunkSource
            pure $ case result of
              Left (Subst.FatalFailure err) | "NarSize" `T.isInfixOf` err -> Pass
              other -> Fail ("expected fatal over-bound, got: " <> T.pack (show (void other))),
      runTestM "withDecompressedSource bzip2 garbage is transient" $ do
        source <- chunkReader ["not a bzip2 stream"]
        result <- Subst.withDecompressedSource 64 "bzip2" source drainChunkSource
        pure $ case result of
          Left (Subst.TransientFailure _) -> Pass
          other -> Fail ("expected transient stream error, got: " <> T.pack (show (void other))),
      runTestM "withDecompressedSource bzip2 truncated is transient" $
        case B64.decode bzip2FixtureB64 of
          Left err -> pure (Fail ("fixture base64 does not decode: " <> T.pack err))
          Right compressed -> do
            source <- chunkReader (streamChunks 7 (BS.take (BS.length compressed - 8) compressed))
            result <- Subst.withDecompressedSource xzFixtureSize "bzip2" source drainChunkSource
            pure $ case result of
              Left (Subst.TransientFailure _) -> Pass
              other -> Fail ("expected transient truncation, got: " <> T.pack (show (void other))),
      -- A bzip2-compressed NAR through the whole post-download
      -- pipeline: the format the historical caches serve decompresses,
      -- hashes, parses, and materializes in one bounded pass.
      runTestM "bzip2 NAR streams through the pipeline" $
        case B64.decode bzip2NarFixtureB64 of
          Left err -> pure (Fail ("fixture base64 does not decode: " <> T.pack err))
          Right compressed -> do
            tmpBase <- getTemporaryDirectory
            let tmpDir = tmpBase </> "nova-nix-test-bzip2-pipeline"
                destPath = tmpDir </> "out"
            forceRemoveIfExists tmpDir
            createDirectoryIfMissing True tmpDir
            source <- chunkReader (streamChunks 7 compressed)
            result <-
              Subst.withDecompressedSource (toInteger (BS.length bzip2NarFixture)) "bzip2" source $
                Subst.consumeNarStream
                  destPath
                  ((streamTestNarInfo bzip2NarFixture) {NarInfo.niCompression = "bzip2"})
                  (CHash.hashBytes bzip2NarFixture)
            materialized <- Dir.doesFileExist (destPath </> "greeting")
            forceRemoveIfExists tmpDir
            pure $ case result of
              Left err -> Fail ("bzip2 pipeline failed: " <> Subst.attemptFailureMessage err)
              Right narBytes
                | narBytes /= BS.length bzip2NarFixture -> Fail "bzip2 pipeline counted the wrong NAR size"
                | not materialized -> Fail "bzip2 pipeline left no tree at the destination"
                | otherwise -> Pass,
      runTest "streaming compression support matches the strict set" $
        case (Subst.streamingDecompressionSupported "xz", Subst.streamingDecompressionSupported "zstd", Subst.streamingDecompressionSupported "bzip2", Subst.streamingDecompressionSupported "brotli") of
          (Right (), Right (), Right (), Left _) -> Pass
          other -> Fail ("unexpected support set: " <> T.pack (show other)),
      -- The support set is encoded twice - the strict decompressor and
      -- the streaming decision - so their agreement is pinned across
      -- the codecs either could plausibly grow, not left to comments.
      runTest "strict and streaming support sets agree" $
        let agrees compression =
              case (Subst.decompressorFor 1 compression, Subst.streamingDecompressionSupported compression) of
                (Right _, Right ()) -> True
                (Left _, Left _) -> True
                _ -> False
         in if all agrees ["none", "", "xz", "zstd", "bzip2", "brotli"]
              then Pass
              else Fail "strict and streaming compression support drifted",
      -- The strict path is the streaming path's differential oracle:
      -- the same NAR materialized by both must produce the same tree.
      runTestM "strict and streaming unpack agree" $ do
        tmpBase <- getTemporaryDirectory
        let tmpDir = tmpBase </> "nova-nix-test-stream-oracle"
        forceRemoveIfExists tmpDir
        createDirectoryIfMissing True tmpDir
        strictOutcome <- case NAR.deserialise streamTestNar of
          Left err -> pure (Left (T.pack err))
          Right entry -> Subst.unpackNarEntry (tmpDir </> "strict") entry
        source <- chunkReader (streamChunks 11 streamTestNar)
        streamOutcome <- Subst.consumeNarStream (tmpDir </> "streamed") (streamTestNarInfo streamTestNar) streamTestDigest source
        strictTree <- NAR.serialiseFromPath (tmpDir </> "strict")
        streamedTree <- NAR.serialiseFromPath (tmpDir </> "streamed")
        forceRemoveIfExists tmpDir
        pure $ case (strictOutcome, streamOutcome) of
          (Right (), Right _)
            | NAR.serialise strictTree == NAR.serialise streamedTree -> Pass
            | otherwise -> Fail "strict and streaming trees diverge"
          other -> Fail ("oracle setup failed: " <> T.pack (show other)),
      -- trySubstitute: empty caches returns SubstNotFound
      runTestM "trySubstitute no caches" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-subst"
        createDirectoryIfMissing True tmpStore
        store <- openStore (StoreDir tmpStore)
        result <- Subst.trySubstitute store [] (StorePath "test" "hello")
        closeStore store
        removeDirectoryRecursive tmpStore
        pure (assertEqual "no-caches" Subst.SubstNotFound result),
      -- defaultCacheConfig has correct URL
      runTest "defaultCacheConfig url" $
        assertEqual "default-url" "https://cache.nixos.org" (Subst.ccUrl Subst.defaultCacheConfig),
      -- defaultCacheConfig has priority 40
      runTest "defaultCacheConfig priority" $
        assertEqual "default-prio" 40 (Subst.ccPriority Subst.defaultCacheConfig),
      -- verifyNarHash: matching NAR hash accepted (HIGH#2 integrity gate);
      -- the returned digest is the declared hash, decoded.
      runTest "verifyNarHash accepts matching hash" $
        case Subst.verifyNarHash (sampleNarInfo sampleNarHash) sampleNarBytes of
          Right declared
            | CHash.formatNixHash declared == sampleNarHash -> Pass
            | otherwise -> Fail "returned digest does not match the declared hash"
          Left err -> Fail ("expected acceptance, got: " <> err),
      -- verifyNarHash: mismatched NAR hash rejected
      runTest "verifyNarHash rejects mismatched hash" $
        case Subst.verifyNarHash (sampleNarInfo wrongNarHash) sampleNarBytes of
          Left _ -> Pass
          Right _ -> Fail "expected mismatch rejection",
      -- verifyNarHash: malformed NAR hash rejected
      runTest "verifyNarHash rejects malformed hash" $
        case Subst.verifyNarHash (sampleNarInfo "not-a-hash") sampleNarBytes of
          Left _ -> Pass
          Right _ -> Fail "expected malformed-hash rejection",
      -- narInfoMatchesPath: identity match accepted
      runTest "narInfoMatchesPath accepts matching identity" $
        if Subst.narInfoMatchesPath (StorePath sampleHash "hello") (sampleNarInfo sampleNarHash)
          then Pass
          else Fail "expected identity match",
      -- narInfoMatchesPath: identity mismatch rejected
      runTest "narInfoMatchesPath rejects mismatched identity" $
        if Subst.narInfoMatchesPath (StorePath (T.replicate 32 "b") "hello") (sampleNarInfo sampleNarHash)
          then Fail "expected identity mismatch"
          else Pass,
      -- tryCachesWith: the first success stops the scan (later caches
      -- are never contacted)
      runTestM "tryCachesWith success stops scan" $ do
        calls <- newIORef (0 :: Int)
        withChainLock "nova-nix-test-chain-stop" $ \lock -> do
          let hit = PathRegistration (StorePath (T.replicate 32 "d") "hit") "sha256:d" 1 Nothing []
              attempt cache = do
                atomicModifyIORef' calls (\n -> (n + 1, ()))
                pure $
                  if Subst.ccUrl cache == "https://b"
                    then Subst.SubstSuccess hit lock
                    else Subst.SubstNotFound
          result <- Subst.tryCachesWith attempt [chainCache "https://a", chainCache "https://b", chainCache "https://c"]
          made <- readIORef calls
          pure $
            if result == Subst.SubstSuccess hit lock && made == 2
              then Pass
              else Fail ("got " <> T.pack (show result) <> " after " <> T.pack (show made) <> " attempts"),
      -- tryCachesWith: an already-valid path found mid-scan is terminal
      -- like a success - later caches are never contacted
      runTestM "tryCachesWith already-valid stops scan" $ do
        calls <- newIORef (0 :: Int)
        let attempt _ = do
              atomicModifyIORef' calls (\n -> (n + 1, ()))
              pure Subst.SubstAlreadyValid
        result <- Subst.tryCachesWith attempt [chainCache "https://a", chainCache "https://b"]
        made <- readIORef calls
        pure $
          if result == Subst.SubstAlreadyValid && made == 1
            then Pass
            else Fail ("got " <> T.pack (show result) <> " after " <> T.pack (show made) <> " attempts"),
      -- tryCachesWith: an erroring cache falls through to a later hit
      -- instead of aborting the chain
      runTestM "tryCachesWith error falls through to next cache" $
        withChainLock "nova-nix-test-chain-fallthrough" $ \lock -> do
          let hit = PathRegistration (StorePath (T.replicate 32 "f") "hit") "sha256:f" 1 Nothing []
              attempt cache
                | Subst.ccUrl cache == "https://a" = pure (Subst.SubstError "transient 500")
                | otherwise = pure (Subst.SubstSuccess hit lock)
          result <- Subst.tryCachesWith attempt [chainCache "https://a", chainCache "https://b"]
          pure (assertEqual "fallthrough" (Subst.SubstSuccess hit lock) result),
      -- tryCachesWith: all misses stay SubstNotFound
      runTestM "tryCachesWith all misses" $ do
        result <- Subst.tryCachesWith (\_ -> pure Subst.SubstNotFound) [chainCache "https://a", chainCache "https://b"]
        pure (assertEqual "misses" Subst.SubstNotFound result),
      -- tryCachesWith: with no hit anywhere, the FIRST error is reported,
      -- tagged with the URL of the cache that produced it
      runTestM "tryCachesWith reports first error tagged with cache" $ do
        let attempt cache
              | Subst.ccUrl cache == "https://a" = pure Subst.SubstNotFound
              | Subst.ccUrl cache == "https://b" = pure (Subst.SubstError "first")
              | otherwise = pure (Subst.SubstError "second")
        result <- Subst.tryCachesWith attempt [chainCache "https://a", chainCache "https://b", chainCache "https://c"]
        pure $ case result of
          Subst.SubstError err
            | "https://b" `T.isPrefixOf` err && "first" `T.isSuffixOf` err -> Pass
          other -> Fail ("expected tagged first error, got " <> T.pack (show other)),
      -- catchSync (the split every attempt wraps itself in): a
      -- synchronous exception folds into SubstError and the scan falls
      -- through to the next cache
      runTestM "catchSync folds sync exception into SubstError" $ do
        let attempt cache =
              Subst.catchSync
                ( if Subst.ccUrl cache == "https://a"
                    then throwIO (ErrorCall "disk full")
                    else pure Subst.SubstNotFound
                )
                (pure . Subst.SubstError . T.pack . show)
        result <- Subst.tryCachesWith attempt [chainCache "https://a", chainCache "https://b"]
        pure $ case result of
          Subst.SubstError err
            | "disk full" `T.isInfixOf` err -> Pass
          other -> Fail ("expected SubstError from sync exception, got " <> T.pack (show other)),
      -- catchSync: an asynchronous exception thrown mid-attempt
      -- propagates out of the cache scan instead of folding into
      -- SubstError - cancellation must abort substitution, never be
      -- spent as fallthrough to the next cache or to a local build
      runTestM "catchSync propagates async exception out of the scan" $ do
        scanned <- newIORef (0 :: Int)
        let attempt _ =
              Subst.catchSync
                ( do
                    atomicModifyIORef' scanned (\n -> (n + 1, ()))
                    throwIO (asyncExceptionToException (ErrorCall "cancelled"))
                )
                (pure . Subst.SubstError . T.pack . show)
        outcome <- try (Subst.tryCachesWith attempt [chainCache "https://a", chainCache "https://b"])
        made <- readIORef scanned
        pure $ case (outcome :: Either SomeException Subst.SubstResult) of
          Left escaped -> case fromException escaped of
            Just (SomeAsyncException _)
              | made == 1 -> Pass
              | otherwise -> Fail ("scan continued past the interrupt: " <> T.pack (show made) <> " attempts")
            Nothing -> Fail ("wrong exception escaped: " <> T.pack (show escaped))
          Right result -> Fail ("async exception folded into " <> T.pack (show result)),
      -- clearStaleDestination: removes a read-only leftover tree (the
      -- crash-between-unpack-and-register wedge)
      runTestM "clearStaleDestination removes read-only leftovers" $ do
        tmpBase <- getTemporaryDirectory
        let staleRoot = tmpBase </> "nova-nix-test-stale-dest"
            staleFile = staleRoot </> "bin" </> "tool"
        createDirectoryIfMissing True (staleRoot </> "bin")
        writeFile staleFile "leftover"
        perms <- getPermissions staleFile
        Dir.setPermissions staleFile (Dir.setOwnerWritable False perms)
        Subst.clearStaleDestination staleRoot
        gone <- Dir.doesPathExist staleRoot
        pure (if gone then Fail "stale tree survived" else Pass),
      -- clearStaleDestination: a missing destination is a no-op
      runTestM "clearStaleDestination missing path no-op" $ do
        tmpBase <- getTemporaryDirectory
        outcome <- try (Subst.clearStaleDestination (tmpBase </> "nova-nix-test-no-such-dir"))
        pure $ case (outcome :: Either SomeException ()) of
          Right () -> Pass
          Left e -> Fail ("threw: " <> T.pack (show e)),
      -- unpackNarEntry: traversal-shaped entry names from an untrusted
      -- cache are rejected before anything is written
      runTestM "unpackNarEntry rejects unsafe entry names" $ do
        tmpBase <- getTemporaryDirectory
        let dest = tmpBase </> "nova-nix-test-unpack-unsafe"
            evil name = NAR.NarDirectory [(name, NAR.NarRegular False "x")]
        results <- mapM (Subst.unpackNarEntry dest . evil) ["..", ".", "", "a/b", "a\\b"]
        Subst.clearStaleDestination dest
        pure $
          if all (\case Left _ -> True; Right () -> False) results
            then Pass
            else Fail ("accepted an unsafe name: " <> T.pack (show results)),
      -- unpackNarEntry: names and targets arrive as the raw bytes the
      -- wire carries; the store materializes only valid-Unicode names,
      -- so a byte name with no Unicode reading refuses the unpack
      -- before anything is written
      runTestM "unpackNarEntry refuses a non-UTF-8 entry name" $ do
        tmpBase <- getTemporaryDirectory
        let dest = tmpBase </> "nova-nix-test-unpack-rawname"
            tree = NAR.NarDirectory [(BS.pack [0xFF], NAR.NarRegular False "x")]
        result <- Subst.unpackNarEntry dest tree
        Subst.clearStaleDestination dest
        pure $ case result of
          Left err -> if "not valid UTF-8" `T.isInfixOf` err then Pass else Fail ("wrong error: " <> err)
          Right () -> Fail "accepted a non-UTF-8 entry name",
      runTestM "unpackNarEntry refuses a non-UTF-8 symlink target" $ do
        tmpBase <- getTemporaryDirectory
        let dest = tmpBase </> "nova-nix-test-unpack-rawtarget"
            tree = NAR.NarDirectory [("link", NAR.NarSymlink (BS.pack [0xFF]))]
        result <- Subst.unpackNarEntry dest tree
        Subst.clearStaleDestination dest
        pure $ case result of
          Left err -> if "not valid UTF-8" `T.isInfixOf` err then Pass else Fail ("wrong error: " <> err)
          Right () -> Fail "accepted a non-UTF-8 symlink target",
      -- unpackNarEntry: a NAR symlink materializes as a REAL link or fails
      -- loudly - never as a regular file holding the target text, which
      -- would silently diverge from the signed NAR hash
      runTestM "unpackNarEntry symlink is real or loud" $ do
        tmpBase <- getTemporaryDirectory
        let dest = tmpBase </> "nova-nix-test-unpack-symlink"
            tree =
              NAR.NarDirectory
                [ ("real", NAR.NarRegular False "contents"),
                  ("link", NAR.NarSymlink "real")
                ]
        Subst.clearStaleDestination dest
        result <- Subst.unpackNarEntry dest tree
        outcome <- case result of
          Right () -> do
            isLink <- Dir.pathIsSymbolicLink (dest </> "link")
            pure (if isLink then Pass else Fail "symlink materialized as a non-link")
          Left _ -> do
            leftBehind <- Dir.doesFileExist (dest </> "link")
            pure (if leftBehind then Fail "failed loudly but left a regular file behind" else Pass)
        Subst.clearStaleDestination dest
        pure outcome,
      -- unpackNarEntry: a directory-target symlink that sorts BEFORE its
      -- target still gets the directory link flavor (second-pass typing)
      runTestM "unpackNarEntry types forward dir symlink" $ do
        tmpBase <- getTemporaryDirectory
        let dest = tmpBase </> "nova-nix-test-unpack-dirlink"
            tree =
              NAR.NarDirectory
                [ ("alink", NAR.NarSymlink "zdir"),
                  ("zdir", NAR.NarDirectory [("f", NAR.NarRegular False "x")])
                ]
        Subst.clearStaleDestination dest
        result <- Subst.unpackNarEntry dest tree
        outcome <- case result of
          Left _ -> pure Pass -- symlinks unavailable here; the loud failure is the contract
          Right () -> do
            throughLink <- doesDirectoryExist (dest </> "alink")
            pure (if throughLink then Pass else Fail "dir symlink not traversable (wrong flavor)")
        Subst.clearStaleDestination dest
        pure outcome,
      -- Folding sibling names MATERIALIZE on every platform: true names
      -- via the NTFS per-directory flag on Windows, upstream's case-hack
      -- renaming on macOS, plain files on Linux.  The platform serialiser
      -- reverses whichever branch ran, so the tree re-serialises to its
      -- original NAR on all three.
      runTestM "folding sibling names materialize and round-trip" $ do
        tmpBase <- getTemporaryDirectory
        let dest = tmpBase </> "nova-nix-test-unpack-fold"
            tree =
              NAR.NarDirectory
                [ ("Foo", NAR.NarRegular False "upper"),
                  ("foo", NAR.NarRegular False "lower")
                ]
        Subst.clearStaleDestination dest
        result <- Subst.unpackNarEntry dest tree
        outcome <- case result of
          Left err -> pure (Fail ("unpack failed: " <> err))
          Right () -> do
            onDisk <- NAR.serialiseFromPath dest
            if onDisk /= tree
              then pure (Fail "materialized tree does not re-serialise to its NAR")
              else
                if SI.os == "mingw32"
                  then do
                    -- The flag path, not the hack: both TRUE names hold
                    -- distinct contents inside the case-sensitive dir.
                    upper <- BS.readFile (dest </> "Foo")
                    lower <- BS.readFile (dest </> "foo")
                    pure (assertEqual "true names on NTFS" ("upper", "lower") (upper, lower))
                  else pure Pass
        Subst.clearStaleDestination dest
        pure outcome,
      -- The pure case-hack naming: later case variants gain the
      -- reversible suffix with a per-name counter.  The fold key is
      -- platform-derived, so Linux (no folding) keeps every name.
      runTest "caseHackDiskNames renames later case variants" $
        let resolved = caseHackDiskNames ["Foo", "foo", "fOO", "bar"]
         in if SI.os == "mingw32" || SI.os == "darwin"
              then
                assertEqual
                  "hack naming"
                  [("Foo", "Foo"), ("foo", "foo~nix~case~hack~1"), ("fOO", "fOO~nix~case~hack~2"), ("bar", "bar")]
                  resolved
              else
                assertEqual
                  "identity naming"
                  [("Foo", "Foo"), ("foo", "foo"), ("fOO", "fOO"), ("bar", "bar")]
                  resolved,
      -- Where the platform serialiser strips the suffix, an incoming
      -- name carrying it must reject (it could not round-trip); on
      -- Linux such a name is legitimate and materializes verbatim.
      runTestM "incoming case-hack suffix names reject where the serialiser strips" $ do
        tmpBase <- getTemporaryDirectory
        let dest = tmpBase </> "nova-nix-test-unpack-suffix"
            tree = NAR.NarDirectory [("x~nix~case~hack~1", NAR.NarRegular False "v")]
        Subst.clearStaleDestination dest
        result <- Subst.unpackNarEntry dest tree
        outcome <-
          if SI.os == "mingw32" || SI.os == "darwin"
            then case result of
              Left _ -> pure Pass
              Right () -> pure (Fail "suffix-bearing name accepted where the serialiser strips")
            else case result of
              Right () -> do
                kept <- BS.readFile (dest </> "x~nix~case~hack~1")
                pure (assertEqual "verbatim on Linux" "v" kept)
              Left err -> pure (Fail ("Linux rejected a legitimate name: " <> err))
        Subst.clearStaleDestination dest
        pure outcome,
      -- The capability probe itself: NTFS grants the per-directory flag
      -- (CI runners and the dev box run NTFS temp dirs); other
      -- platforms report unsupported.
      runTestM "trySetCaseSensitiveDir reflects platform support" $ do
        tmpBase <- getTemporaryDirectory
        let dir = tmpBase </> "nova-nix-test-csdir"
        Subst.clearStaleDestination dir
        createDirectoryIfMissing True dir
        flagged <- trySetCaseSensitiveDir dir
        Subst.clearStaleDestination dir
        pure (assertEqual "flag support" (SI.os == "mingw32") flagged),
      -- unpackAndVerify re-serialises the materialized tree and checks it
      -- against the declared hash before returning a registration: a
      -- faithful round-trip passes, and the registration carries the
      -- declared hash.
      runTestM "unpackAndVerify accepts a tree that reproduces its hash" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-unpack-verify"
        forceRemoveIfExists tmpStore
        store <- openStore (StoreDir tmpStore)
        let sp = StorePath (T.replicate 32 "b") "roundtrip"
            tree =
              NAR.NarDirectory
                [ ("data.txt", NAR.NarRegular False "verified bytes"),
                  ("sub", NAR.NarDirectory [("inner", NAR.NarRegular False "nested")])
                ]
            rawNar = NAR.serialise tree
            narHash = CHash.formatNixHash (CHash.hashBytes rawNar)
            info =
              NarInfo.NarInfo
                { NarInfo.niStorePath = storePathToText defaultStoreDir sp,
                  NarInfo.niUrl = "nar/roundtrip.nar",
                  NarInfo.niCompression = "none",
                  NarInfo.niFileHash = Nothing,
                  NarInfo.niFileSize = Nothing,
                  NarInfo.niNarHash = narHash,
                  NarInfo.niNarSize = fromIntegral (BS.length rawNar),
                  NarInfo.niReferences = [],
                  NarInfo.niDeriver = Nothing,
                  NarInfo.niSigs = [],
                  NarInfo.niCA = Nothing
                }
        result <- Subst.unpackAndVerify store sp info rawNar
        onDisk <- BS.readFile (storePathToFilePath (stDir store) sp </> "data.txt")
        -- A success carries the path lock still held; release it before
        -- the store teardown (Windows cannot delete a file a handle
        -- holds open).
        case result of
          Subst.SubstSuccess _ lock -> releasePathLock lock
          _ -> pure ()
        closeStore store
        forceRemoveIfExists tmpStore
        pure $ case result of
          Subst.SubstSuccess reg _ ->
            if prNarHash reg == narHash && onDisk == "verified bytes"
              then Pass
              else Fail "registration or on-disk bytes diverge from the declared hash"
          other -> Fail ("unpackAndVerify failed: " <> T.pack (show other))
    ]
  where
    chainCache url = Subst.CacheConfig url "unused-key" 10
    -- A real held lock for tests that construct 'SubstSuccess' by hand:
    -- the constructor carries the path's lock, so a fabricated success
    -- needs a genuine one, taken in a scratch directory.
    withChainLock dirName act = do
      tmpBase <- getTemporaryDirectory
      let lockRoot = tmpBase </> (dirName :: FilePath)
      forceRemoveIfExists lockRoot
      createDirectoryIfMissing True lockRoot
      lock <- acquirePathLock (StoreDir lockRoot) (StorePath (T.replicate 32 "d") "hit")
      result <- act lock
      releasePathLock lock
      forceRemoveIfExists lockRoot
      pure result
    sampleNarBytes = "nova-nix nar sample bytes" :: BS.ByteString
    sampleNarHash = CHash.formatNixHash (CHash.hashBytes sampleNarBytes)
    wrongNarHash = CHash.formatNixHash (CHash.hashBytes ("different bytes" :: BS.ByteString))
    sampleHash = T.replicate 32 "a"
    sampleNarInfo narHash =
      NarInfo.NarInfo
        { NarInfo.niStorePath = "/nix/store/" <> sampleHash <> "-hello",
          NarInfo.niUrl = "nar/sample.nar",
          NarInfo.niCompression = "none",
          NarInfo.niFileHash = Just sampleNarHash,
          NarInfo.niFileSize = Just 0,
          NarInfo.niNarHash = narHash,
          NarInfo.niNarSize = fromIntegral (BS.length sampleNarBytes),
          NarInfo.niReferences = [],
          NarInfo.niDeriver = Nothing,
          NarInfo.niSigs = [],
          NarInfo.niCA = Nothing
        }
    -- A real xz stream: "nova-nix xz fixture payload\n" 64 times
    -- (1792 bytes) compressed with xz -9 to 112 bytes, embedded as
    -- base64 so the test source stays ASCII.
    xzFixturePayload = BS.concat (replicate 64 "nova-nix xz fixture payload\n")
    xzFixtureSize = toInteger (BS.length xzFixturePayload)
    xzFixtureB64 =
      "/Td6WFoAAATm1rRGAgAhARwAAAAQz1jM4Ab/AC1dADcbyzKSinKbUBue2bMHcUt1zJQMzAec5W6XwPbK3dKJtL3q9K3rfg1KQ/ZOAAAAAAAWUPRD8XwohgABSYAOAAAA2Eg7urHEZ/sCAAAAAARZWg==" :: Text
    -- cache.nixos.org narinfos recorded 2026-08-20 (nixos-25.05
    -- channel), verbatim: GNU hello (references including itself,
    -- deriver, production signature) and a doc output whose References
    -- line is empty.
    recordedHelloNarInfo =
      T.unlines
        [ "StorePath: /nix/store/jppkr0h8aap5z7m4xy4vg5yrwlly9h2v-hello-2.12.1",
          "URL: nar/1m1sbal63vqhlvbcxzdj8yr6fhhld7dsyhxbxwip54dgvg56rjnc.nar.xz",
          "Compression: xz",
          "FileHash: sha256:1m1sbal63vqhlvbcxzdj8yr6fhhld7dsyhxbxwip54dgvg56rjnc",
          "FileSize: 52056",
          "NarHash: sha256:0s8wi24d3bc4zxyxq3hffj102zi7cjp3wqnpj6nhqrwgjsgqff81",
          "NarSize: 249480",
          "References: dska0s3gxxd8azbsn85pwm6xcqhivldw-glibc-2.40-66 jppkr0h8aap5z7m4xy4vg5yrwlly9h2v-hello-2.12.1",
          "Deriver: a9y211d3lq3yf2mpfvb72f0qvsl0wi3s-hello-2.12.1.drv",
          "Sig: cache.nixos.org-1:xV9xUvtPGHTSo6eeFWYYhaNFiH8MYSXKZzmxe0cf+acdDkIYxFPLC/MNc1CjlbsVoaUwGhcigUdr9eW2+W1UAw=="
        ]
    recordedHelloDocNarInfo =
      T.unlines
        [ "StorePath: /nix/store/4xrqj3kcd89bsg5crbpnx1ppg7nxw9xp-hello-1.0.0.2-doc",
          "URL: nar/1r86zzj3g8jhzyvi17vchsj3frrqk5l329viq2i24iish53x922l.nar.xz",
          "Compression: xz",
          "FileHash: sha256:1r86zzj3g8jhzyvi17vchsj3frrqk5l329viq2i24iish53x922l",
          "FileSize: 892",
          "NarHash: sha256:0amizmj17fgj5sz1gvr743kncrzwvq747lml3zhd374f14hzgd7a",
          "NarSize: 2072",
          "References: ",
          "Deriver: 5knzybx32a3iq53jjw6xd6fn4awn2l3h-hello-1.0.0.2.drv",
          "Sig: cache.nixos.org-1:jUFHex7R7zPonZZqjVy/OOTguUZZE/sityC0zvu3sb35Az8zBTROeFhUC/J/UFx8Ui8UflA7uHyLJm5Z+Ca6Bw=="
        ]
    -- A NAR with the shapes streaming unpack must materialize:
    -- nesting, an executable, a symlink, a zero-byte file, an empty
    -- directory, and a sibling pair that collides on folding
    -- filesystems.  Entries in NAR name order.
    -- The executable bit only where the platform can round-trip it
    -- from disk - Windows cannot, the same constraint the NAR spec
    -- vectors note, until #35 gives it a real model.  The symlink
    -- target nests only off Windows: separator-bearing targets do not
    -- round-trip Windows disk serialisation (#112 tracks the fix, and
    -- lifting this split is its undo).
    streamTestExec = SI.os /= "mingw32"
    streamTestLinkTarget
      | SI.os == "mingw32" = "Makefile"
      | otherwise = "bin/tool"
    streamTestNar =
      NAR.serialise
        ( NAR.NarDirectory
            [ ("Makefile", NAR.NarRegular False "all:\n"),
              ("bin", NAR.NarDirectory [("tool", NAR.NarRegular streamTestExec "#!/bin/sh\n")]),
              ("empty", NAR.NarRegular False ""),
              ("emptydir", NAR.NarDirectory []),
              ("link", NAR.NarSymlink streamTestLinkTarget),
              ("makefile", NAR.NarRegular False "lower\n")
            ]
        )
    streamTestDigest = CHash.hashBytes streamTestNar
    streamTestNarInfo rawNar =
      NarInfo.NarInfo
        { NarInfo.niStorePath = "/nix/store/" <> sampleHash <> "-stream",
          NarInfo.niUrl = "nar/stream.nar",
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
    streamChunks chunkLen bytes
      | BS.null bytes = []
      | otherwise = BS.take chunkLen bytes : streamChunks chunkLen (BS.drop chunkLen bytes)
    -- The xz fixture's payload, compressed by nova-cache:zstandard's
    -- own encoder - the exact pairing the push path ships.
    zstdFixtureCompressed = CZstd.compress CZstd.defaultCompressionLevel xzFixturePayload
    -- The same payload compressed by the bzip2 CLI (bzip2 -9),
    -- embedded base64 so the test source stays ASCII.  The reference
    -- encoder is deliberately the format's own tool and not the
    -- decoder's library: nova-cache:bzip2 decodes only, so a
    -- self-produced fixture could not catch the two disagreeing.
    bzip2FixtureB64 =
      "QlpoOTFBWSZTWTop7coAAf/RgAAQQAInJddwIACQKZMTIMjAqqBkDaJtTgCQKgZA5AyBAFwNwMAeAYAgCQLgXA+AoBUCQIAkCAJAUAgCgH4u5IpwoSB0U9uU" :: Text
    -- A NAR every platform materializes identically (no executable
    -- bit, no separator-bearing symlink target), and the bzip2 CLI's
    -- compression of exactly these bytes.
    bzip2NarFixture =
      NAR.serialise
        (NAR.NarDirectory [("greeting", NAR.NarRegular False "nova-nix bzip2 fixture\n")])
    bzip2NarFixtureB64 =
      "QlpoOTFBWSZTWa3VDM4AAFJ5gG7yAIBAYjAAP+ffcCAAlIaTJM1PUnqD1DJpoHo01BkkA9I0AAAAtARwcfWQAq1WLMjpmBpVKUAJM6EA8IDQEABOzcRgdDpvigJ6ZZa760fpxZtj+ts6GDiuqt/B6UuthWIrabWmZTkFlI46C8MmbMoVmMYwEYRUCYQW/F3JFOFCQrdUMzg=" :: Text

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
        held <- acquirePathLock dir sp
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
      -- The already-valid short-circuit: a registered path substitutes
      -- as SubstAlreadyValid with no network and no disk writes - the
      -- configured cache is unreachable, so any contact would surface
      -- as an error result.
      runTestM "trySubstitute adopts an already-valid path untouched" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-lock-valid"
        forceRemoveIfExists tmpStore
        store <- openStore (StoreDir tmpStore)
        let sp = StorePath (T.replicate 32 "c") "validpath"
            destPath = storePathToFilePath (StoreDir tmpStore) sp
        createDirectoryIfMissing True destPath
        BS.writeFile (destPath </> "payload") "registered bytes"
        reg <- registrationFor store sp Nothing []
        registerPath (stDB store) reg
        result <- Subst.trySubstitute store [unreachableCache] sp
        survivor <- BS.readFile (destPath </> "payload")
        -- The short-circuit released the lock, so the path must be
        -- lockable again at once.
        reLock <- tryAcquirePathLock (StoreDir tmpStore) sp
        mapM_ releasePathLock reLock
        closeStore store
        forceRemoveIfExists tmpStore
        pure $ case (result, reLock) of
          (Subst.SubstAlreadyValid, Just _)
            | survivor == "registered bytes" -> Pass
            | otherwise -> Fail "tree touched during already-valid adoption"
          (Subst.SubstAlreadyValid, Nothing) -> Fail "lock still held after already-valid return"
          (other, _) -> Fail ("expected SubstAlreadyValid, got: " <> T.pack (show other)),
      -- The strict pipeline honors the same short-circuit: fed a
      -- DIFFERENT (self-consistent) NAR for an already-valid path, it
      -- must adopt the registered tree rather than overwrite it.
      runTestM "unpackAndVerify adopts an already-valid path untouched" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-lock-valid-strict"
        forceRemoveIfExists tmpStore
        store <- openStore (StoreDir tmpStore)
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
        store <- openStore (StoreDir tmpStore)
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
    -- Never contacted when the already-valid short-circuit holds; a
    -- regression that reaches for the network fails loudly here.
    unreachableCache = Subst.CacheConfig "http://127.0.0.1:9" "unused-key" 10
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
-- Tests: Build orchestrator (Phase 3, Batch 7)
-- ---------------------------------------------------------------------------

testBuildOrchestrator :: IO [Bool]
testBuildOrchestrator = do
  putStrLn "build/orchestrator"
  sequence
    [ -- BuildConfig has caches field
      runTest "defaultBuildConfig has empty caches" $
        assertEqual "empty-caches" [] (bcCaches (defaultBuildConfig defaultStoreDir)),
      -- The build PATH must never open with the build working directory:
      -- a bare-name builder has no directory to derive.
      runTest "bare-name builder derives no PATH entry" $
        let entries = T.splitOn (if SI.os == "mingw32" then ";" else ":") (buildPath "bash")
         in assertEqual "no-dot" False (any (\e -> e == "." || T.isPrefixOf "./" e || T.isPrefixOf ".\\" e) entries),
      runTest "dot-relative builder derives no PATH entry" $
        let entries = T.splitOn (if SI.os == "mingw32" then ";" else ":") (buildPath "./bash")
         in assertEqual "no-dot-rel" False (any (\e -> e == "." || T.isPrefixOf "./" e || T.isPrefixOf ".\\" e) entries),
      runTest "absolute builder opens PATH with its own directory" $
        let path = buildPath ("/store/aaa-bootstrap/bin" </> "bash")
         in assertEqual "builder-dir-first" (Just "/store/aaa-bootstrap/bin") (listToMaybe (T.splitOn (if SI.os == "mingw32" then ";" else ":") path)),
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
      -- BuildConfig with caches
      runTest "BuildConfig accepts caches" $
        let cache = Subst.CacheConfig "https://cache.example.com" "key" 10
            config = (defaultBuildConfig defaultStoreDir) {bcCaches = [cache]}
         in assertEqual "one-cache" 1 (length (bcCaches config)),
      -- buildWithDeps on a simple derivation (no deps, builder fails but graph resolves)
      runTestM "buildWithDeps single drv" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-orch"
        createDirectoryIfMissing True tmpStore
        store <- openStore (StoreDir tmpStore)
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
        let config = (defaultBuildConfig (StoreDir tmpStore)) {bcTmpDir = tmpBase </> "nova-nix-test-orch-tmp"}
        result <- buildWithDeps config store drv drvSP
        closeStore store
        forceRemoveIfExists tmpStore
        forceRemoveIfExists (bcTmpDir config)
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
         in case DepGraph.buildDepGraph readFn drvACyc spA of
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
         in case DepGraph.buildDepGraph readFn drv (StorePath "rrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr" "root.drv") of
              Left _ -> Pass
              Right _ -> Fail "expected failure for missing .drv",
      -- derivation with context creates populated inputDrvs.  Hashing a
      -- dependent derivation reads input modulo hashes from the drv-hash
      -- cache, which only the IO evaluator maintains, so this runs under
      -- evalNixIO (an in-session dependency hits the cache bottom-up).
      runTestM "derivation context populates inputDrvs" $ do
        tmpBase <- getTemporaryDirectory
        result <-
          evalNixIO tmpBase $
            T.concat
              [ "let dep = derivation { name = \"dep\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; }; ",
                "main = derivation { name = \"main\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; src = dep.outPath; }; ",
                "in main._derivation"
              ]
        pure $ assertRight "drv-ctx-inputs" result $ \case
          VDerivation drv ->
            if Map.null (drvInputDrvs drv)
              then Fail "expected non-empty drvInputDrvs"
              else Pass
          _ -> Fail "expected VDerivation",
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
          evalNixIO tmpBase $
            T.concat
              [ "let dep = ",
                depSrc,
                "; main = derivation { name = \"main\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; ref = dep.drvPath; }; ",
                "in main._derivation"
              ]
        pure $ case (depPathR, result) of
          (Right (VStr depPathBytes _), Right (VDerivation drv)) ->
            case parseStorePath defaultStoreDir (bytesText depPathBytes) of
              Nothing -> Fail ("unparseable dep drvPath: " <> bytesText depPathBytes)
              Just depSP -> case Map.lookup depSP (drvInputDrvs drv) of
                Just outs
                  | Set.fromList outs == Set.fromList ["dev", "out"] -> Pass
                  | otherwise -> Fail ("expected the full output set [dev, out], got " <> T.pack (show outs))
                Nothing -> Fail "dep .drv missing from inputDrvs despite the deep drvPath ref"
          other -> Fail ("expected dep drvPath and main._derivation, got " <> T.pack (show other))
    ]

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

removeIfExists :: FilePath -> IO ()
removeIfExists path = do
  exists <- doesDirectoryExist path
  when exists (removeDirectoryRecursive path)

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
        -- permission to delete a live dependency.
        runTestM "db register with unregistered ref throws" $ do
          db <- openStoreDB storeDir
          let ghost = StorePath "jjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjj" "ghost"
              sp = StorePath "kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk" "orphan-edge"
          outcome <- try (registerPath db (PathRegistration sp "sha256:oe" 13 Nothing [ghost]))
          closeStoreDB db
          pure $ case (outcome :: Either SomeException ()) of
            Left _ -> Pass
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

testPushPure :: IO [Bool]
testPushPure = do
  putStrLn "push/narinfo"
  let hashA = T.replicate 32 "a"
      hashB = T.replicate 32 "b"
      spA = StorePath hashA "hello-1.0"
      spB = StorePath hashB "dep-2.0"
      narHash = "sha256:0123abcdef"
      narBytes = BS.replicate 1234 0x6e
      artifactNone = mkPushArtifact PushNone narHash narBytes
      artifactZstd = mkPushArtifact PushZstd narHash narBytes
      -- References deliberately unsorted; deriver present.
      ni = mkNarInfo artifactNone spA [spB, spA] (Just spB)
      niZstd = mkNarInfo artifactZstd spA [spB, spA] (Just spB)
  sequence
    [ runTest "narinfo StorePath is canonical /nix/store" $
        assertEqual "StorePath" ("/nix/store/" <> hashA <> "-hello-1.0") (NarInfo.niStorePath ni),
      runTest "narinfo URL is nar/<digest>.nar" $
        assertEqual "URL" "nar/0123abcdef.nar" (NarInfo.niUrl ni),
      runTest "compression none mirrors NAR fields into file fields" $
        assertEqual
          "file fields"
          ("none", Just (NarInfo.niNarHash ni), Just (NarInfo.niNarSize ni))
          (NarInfo.niCompression ni, NarInfo.niFileHash ni, NarInfo.niFileSize ni),
      runTest "narinfo sizes carry through" $
        assertEqual "NarSize" 1234 (NarInfo.niNarSize ni),
      -- The zstd artifact: file fields describe the compressed object,
      -- named by its own file hash, while the NAR fields stay the
      -- archive's - and the substituter's strict decoder round-trips
      -- the bytes under the declared NarSize bound.
      runTest "zstd artifact declares the compressed object" $
        assertEqual
          "zstd fields"
          ("zstd", Just (paFileHash artifactZstd), Just (fromIntegral (paFileSize artifactZstd)), 1234)
          (NarInfo.niCompression niZstd, NarInfo.niFileHash niZstd, NarInfo.niFileSize niZstd, NarInfo.niNarSize niZstd),
      -- Ground truth, not field copying: the file fields must equal
      -- independent computation over the artifact's actual bytes, and
      -- must NOT collapse into the NAR fields (the copy-paste hazard
      -- for a compressible input like this one).
      runTest "zstd file fields are computed from the compressed bytes" $
        let independentHash = CHash.formatNixHash (CHash.hashBytes (paBytes artifactZstd))
         in if paFileHash artifactZstd == independentHash
              && paFileSize artifactZstd == BS.length (paBytes artifactZstd)
              && paFileHash artifactZstd /= narHash
              && paFileSize artifactZstd /= BS.length narBytes
              then Pass
              else Fail "zstd file fields do not match the compressed bytes",
      runTest "none artifact IS the NAR" $
        if paBytes artifactNone == narBytes
          && paFileHash artifactNone == narHash
          && paFileSize artifactNone == BS.length narBytes
          then Pass
          else Fail "none artifact diverges from its NAR",
      runTest "zstd object is named by its independently computed file hash" $
        assertEqual
          "zstd URL"
          ("nar/" <> stripHashPrefix (CHash.formatNixHash (CHash.hashBytes (paBytes artifactZstd))) <> ".nar.zst")
          (NarInfo.niUrl niZstd),
      -- mkNarInfo cannot be handed NAR fields that disagree with the
      -- artifact: they now come from the artifact itself.
      runTest "narinfo NAR fields come from the artifact" $
        assertEqual
          "NAR fields"
          (narHash, toInteger (BS.length narBytes))
          (NarInfo.niNarHash niZstd, NarInfo.niNarSize niZstd),
      runTestM "zstd artifact round-trips through the substituter decoder" $ do
        out <- Subst.decompressNar 1234 "zstd" (paBytes artifactZstd)
        pure (assertEqual "push-substitute roundtrip" (Right narBytes) out),
      -- The CLI vocabulary is the wire vocabulary, parsed in one place.
      runTest "parsePushCompression accepts the register and rejects by name" $
        case (parsePushCompression "none", parsePushCompression "zstd", parsePushCompression "brotli") of
          (Right PushNone, Right PushZstd, Left err)
            | "none, zstd" `T.isInfixOf` err -> Pass
          other -> Fail ("unexpected parse outcomes: " <> T.pack (show other)),
      runTest "references are sorted basenames" $
        assertEqual
          "References"
          [hashA <> "-hello-1.0", hashB <> "-dep-2.0"]
          (NarInfo.niReferences ni),
      runTest "deriver renders as a basename" $
        assertEqual "Deriver" (Just (hashB <> "-dep-2.0")) (NarInfo.niDeriver ni),
      runTest "no client-side signatures" $
        assertEqual "Sigs" ([] :: [Text]) (NarInfo.niSigs ni),
      runTest "planMissing keeps only uncached paths" $
        assertEqual
          "missing"
          [hashB]
          (map spHash (planMissing (Set.singleton hashA) [spA, spB])),
      runTest "stripHashPrefix drops the algo tag" $
        assertEqual "tagged" "deadbeef" (stripHashPrefix "sha256:deadbeef"),
      runTest "stripHashPrefix tolerates a bare digest" $
        assertEqual "bare" "deadbeef" (stripHashPrefix "deadbeef"),
      runTest "storePathBasename joins hash and name" $
        assertEqual "basename" (hashA <> "-hello-1.0") (storePathBasename spA),
      runTest "narFileName appends .nar" $
        assertEqual "file" "0123abcdef.nar" (narFileName narHash),
      -- checkRecordedNarHash: the pre-upload integrity gate
      runTest "push gate accepts a registered matching path" $
        assertEqual
          "gate ok"
          (Right ())
          (checkRecordedNarHash (Just (pathInfoFor narHash)) narHash spA),
      runTest "push gate refuses an unregistered path" $
        case checkRecordedNarHash Nothing narHash spA of
          Left err
            | "not registered" `T.isInfixOf` err -> Pass
            | otherwise -> Fail ("wrong error: " <> err)
          Right () -> Fail "unregistered path must not be publishable",
      runTest "push gate refuses a changed-on-disk path" $
        case checkRecordedNarHash (Just (pathInfoFor "sha256:other")) narHash spA of
          Left err
            | "DB recorded" `T.isInfixOf` err -> Pass
            | otherwise -> Fail ("wrong error: " <> err)
          Right () -> Fail "hash mismatch must not be publishable",
      -- narHashMatches decodes both spellings before comparing.  Today
      -- parseNixHash reads only the sha256:nix-base32 spelling, so a
      -- base16-recorded digest (both literals spell the empty-string
      -- sha256) falls back to text equality and refuses - the safe
      -- direction.  When the parser learns more spellings (foreign
      -- caches), this pin flips and the gate widens with it.
      runTest "push gate pins the accepted hash spellings" $
        if narHashMatches
          "sha256:e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
          "sha256:0mdqa9w1p6cmli6976v4wi0sw9r4p5prkj7lzfd1877wk11c9c73"
          then Fail "base16 spelling unexpectedly matched - widen this pin"
          else Pass,
      runTest "push gate still refuses different digests" $
        if narHashMatches
          "sha256:0mdqa9w1p6cmli6976v4wi0sw9r4p5prkj7lzfd1877wk11c9c73"
          (Hash.formatNixHash (Hash.hashBytes "different"))
          then Fail "distinct digests must not match"
          else Pass
    ]
  where
    pathInfoFor recordedHash =
      PathInfo
        { piPath = "/nix/store/" <> T.replicate 32 "a" <> "-hello-1.0",
          piNarHash = recordedHash,
          piNarSize = 1234,
          piDeriver = Nothing,
          piRegTime = 0
        }

-- | Closure computation against a real (temp) store database.
testPushClosureIO :: IO [Bool]
testPushClosureIO = do
  putStrLn "push/closure"
  withTempStore $ \store -> do
    let spTop = StorePath (T.replicate 32 "1") "top"
        spMid = StorePath (T.replicate 32 "2") "mid"
        spLeaf = StorePath (T.replicate 32 "3") "leaf"
        regFor sp refs =
          PathRegistration
            { prPath = sp,
              prNarHash = "sha256:" <> T.replicate 52 "0",
              prNarSize = 1,
              prDeriver = Nothing,
              prReferences = refs
            }
    registerPaths (stDB store) [regFor spTop [spMid], regFor spMid [spLeaf], regFor spLeaf []]
    fromTop <- computeClosure store [spTop]
    fromBoth <- computeClosure store [spTop, spLeaf]
    sequence
      [ runTest "closure walks transitive references" $
          assertRight "fromTop" fromTop $ \closure ->
            assertEqual
              "hashes"
              (Set.fromList (map spHash [spTop, spMid, spLeaf]))
              (Set.fromList (map spHash closure)),
        runTest "closure deduplicates across roots" $
          assertRight "fromBoth" fromBoth $ \closure ->
            assertEqual "count" (3 :: Int) (length closure),
        -- loadApiKeyFile: keys copied through Windows editors arrive with
        -- a BOM and CRLF; skipping the normalization turns every push
        -- into an auth rejection with no visible cause.
        runTestM "loadApiKeyFile strips BOM and CRLF" $ do
          tmpBase <- getTemporaryDirectory
          let keyFile = tmpBase </> "nova-nix-test-api-key"
          BS.writeFile keyFile (BS.pack [0xEF, 0xBB, 0xBF] <> "the-secret-key\r\n")
          loaded <- loadApiKeyFile keyFile
          Dir.removeFile keyFile
          pure (assertEqual "normalized key" (Right "the-secret-key") loaded),
        runTestM "loadApiKeyFile rejects an effectively-empty key file" $ do
          tmpBase <- getTemporaryDirectory
          let keyFile = tmpBase </> "nova-nix-test-api-key-empty"
          BS.writeFile keyFile (BS.pack [0xEF, 0xBB, 0xBF] <> "  \r\n")
          loaded <- loadApiKeyFile keyFile
          Dir.removeFile keyFile
          pure $ case loaded of
            Left err | "empty" `T.isInfixOf` err -> Pass
            other -> Fail ("expected empty-key rejection, got: " <> T.pack (show other)),
        runTestM "loadApiKeyFile reports a missing file" $ do
          tmpBase <- getTemporaryDirectory
          loaded <- loadApiKeyFile (tmpBase </> "nova-nix-test-no-such-key-file")
          pure $ case loaded of
            Left err | "cannot read key file" `T.isInfixOf` err -> Pass
            other -> Fail ("expected read error, got: " <> T.pack (show other))
      ]

-- | Helper: create a fresh temp store for IO tests.
withTempStore :: (Store -> IO [Bool]) -> IO [Bool]
withTempStore action = do
  tmpBase <- getTemporaryDirectory
  let tmpStore = tmpBase </> "nova-nix-test-store-ops"
  forceRemoveIfExists tmpStore
  createDirectoryIfMissing True tmpStore
  store <- openStore (StoreDir tmpStore)
  results <- action store
  closeStore store
  forceRemoveIfExists tmpStore
  pure results

-- | Recursively restore writable permissions then remove.
-- Needed because addToStore/setReadOnly makes paths read-only.
forceRemoveIfExists :: FilePath -> IO ()
forceRemoveIfExists path = do
  exists <- doesDirectoryExist path
  when exists $ do
    restoreWritable path
    removeDirectoryRecursive path

restoreWritable :: FilePath -> IO ()
restoreWritable path = do
  isDir <- doesDirectoryExist path
  when isDir $ do
    perms <- getPermissions path
    Dir.setPermissions path (Dir.setOwnerWritable True perms)
    entries <- Dir.listDirectory path
    mapM_ (restoreWritable . (path </>)) entries
  isFile <- Dir.doesFileExist path
  when isFile $ do
    perms <- getPermissions path
    Dir.setPermissions path (Dir.setOwnerWritable True perms)

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
    buildTmp <- (</> "nova-nix-test-trivial-build-tmp") <$> getTemporaryDirectory
    forceRemoveIfExists buildTmp
    createDirectoryIfMissing True buildTmp
    let config = (defaultBuildConfig storeDir) {bcTmpDir = buildTmp}
    result <- buildDerivation config store drv
    forceRemoveIfExists buildTmp
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
                else Fail ("NAR hash/size not real: " <> T.pack (show narInfo))
          ]
      BuildFailure msg code ->
        sequence
          [ runTest "trivial build succeeds" $
              Fail ("build failed (exit " <> T.pack (show code) <> "): " <> msg)
          ]

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
    buildTmp <- (</> "nova-nix-test-sde-build-tmp") <$> getTemporaryDirectory
    forceRemoveIfExists buildTmp
    createDirectoryIfMissing True buildTmp
    let config = (defaultBuildConfig storeDir) {bcTmpDir = buildTmp}
    result <- buildDerivation config store drv
    forceRemoveIfExists buildTmp
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

-- | Zstd compression level for test archives (zstd's own default).
testZstdCompressionLevel :: Int
testZstdCompressionLevel = 3

-- | Build a @.tar.zst@ archive from tar entries, MSYS2-package style.
compressArchive :: [TarEntry.Entry] -> BL.ByteString
compressArchive = ZstdL.compress testZstdCompressionLevel . Tar.write

-- | Test-setup helpers: the paths and link targets below are short literals,
-- so encoding them cannot fail; 'error' marks setup bugs, not test failures.
tarPathOrDie :: Bool -> FilePath -> TarEntry.TarPath
tarPathOrDie isDir p =
  either (error . ("test tar path: " ++)) id (TarEntry.toTarPath isDir p)

linkOrDie :: FilePath -> TarEntry.LinkTarget
linkOrDie t =
  fromMaybe (error ("test link target: " ++ t)) (TarEntry.toLinkTarget t)

tarDir :: FilePath -> TarEntry.Entry
tarDir p = TarEntry.directoryEntry (tarPathOrDie True p)

tarFile :: FilePath -> BL.ByteString -> TarEntry.Entry
tarFile p = TarEntry.fileEntry (tarPathOrDie False p)

tarExecFile :: FilePath -> BL.ByteString -> TarEntry.Entry
tarExecFile p content = (tarFile p content) {TarEntry.entryPermissions = 0o755}

tarHardLink :: FilePath -> FilePath -> TarEntry.Entry
tarHardLink p target =
  TarEntry.simpleEntry (tarPathOrDie False p) (Tar.HardLink (linkOrDie target))

tarSymLink :: FilePath -> FilePath -> TarEntry.Entry
tarSymLink p target =
  TarEntry.simpleEntry (tarPathOrDie False p) (Tar.SymbolicLink (linkOrDie target))

-- | Step 3 of the ladder: @builtin:unpack@, the stage-0 seed extractor.
-- Two zstd-compressed tar archives sharing a top-level prefix (the MSYS2
-- @mingw64\/@ shape) are extracted into ONE output: regular files, an
-- executable, a hardlink and a relative symlink (both materialized as
-- copies), with pacman metadata (@.PKGINFO@\/@.MTREE@) skipped.  A second
-- build proves cross-archive file collisions fail loudly, and further builds
-- that a parent-climbing (..) entry and drive-rooted file, symlink, and
-- hardlink paths are all rejected.
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
      -- that now (#101) - each entry is parsed and rendered through the
      -- build's store dir, which for these builds is the temp store.
      let archiveFile name = storePathToFilePath storeDir (StorePath (T.replicate 32 "0") name)
          archiveTools = archiveFile "pkg-tools.tar.zst"
          archiveData = archiveFile "pkg-data.tar.zst"
          archiveCollide = archiveFile "pkg-collide.tar.zst"
          archiveEscape = archiveFile "pkg-escape.tar.zst"
          archiveRootFile = archiveFile "pkg-root-file.tar.zst"
          archiveEscapeSymlink = archiveFile "pkg-escape-symlink.tar.zst"
          archiveEscapeHardlink = archiveFile "pkg-escape-hardlink.tar.zst"
          archiveDirLink = archiveFile "pkg-dirlink.tar.zst"
          archiveDirLinkBase = archiveFile "pkg-dirlink-base.tar.zst"
          archiveDirLinkCollide = archiveFile "pkg-dirlink-collide.tar.zst"
          archiveHardLinkCollide = archiveFile "pkg-hardlink-collide.tar.zst"
          archiveManyEntries = archiveFile "pkg-many-entries.tar.zst"
          archiveBigFile = archiveFile "pkg-big-file.tar.zst"
          archiveAmplify = archiveFile "pkg-amplify.tar.zst"
      BL.writeFile archiveTools $
        compressArchive
          [ tarDir "pkg",
            tarDir "pkg/bin",
            tarExecFile "pkg/bin/tool.exe" "tool-payload",
            tarHardLink "pkg/bin/tool-link.exe" "pkg/bin/tool.exe",
            tarSymLink "pkg/bin/sh.exe" "tool.exe",
            tarFile ".PKGINFO" "pkgname = tools"
          ]
      BL.writeFile archiveData $
        compressArchive
          [ tarDir "pkg",
            tarDir "pkg/share",
            tarFile "pkg/share/data.txt" "shared-data",
            tarFile ".MTREE" "mtree-bytes"
          ]
      BL.writeFile archiveCollide $
        compressArchive [tarFile "pkg/bin/tool.exe" "conflicting"]
      BL.writeFile archiveEscape $
        compressArchive [tarFile "../evil.txt" "escape"]
      -- A leading '/' entry: rooted at the current drive.  tar stores paths with
      -- '/', so this is the on-disk form even of a Windows-native "\..." name.
      BL.writeFile archiveRootFile $
        compressArchive [tarFile "/planted-abs.txt" "rooted-file"]
      -- Escaping link targets: '..' climbs above the archive root.  Unlike a
      -- rooted ('/') target, this builds portably - tar's toLinkTarget accepts a
      -- relative path on every host - so the real unpacker is exercised end to end.
      BL.writeFile archiveEscapeSymlink $
        compressArchive [tarSymLink "sneaky-link.txt" "../escape-link-target.txt"]
      BL.writeFile archiveEscapeHardlink $
        compressArchive [tarHardLink "sneaky-hardlink.txt" "../escape-hardlink-target.txt"]
      -- A directory symlink entry with a fresh destination: materialized
      -- by copying its target tree.
      BL.writeFile archiveDirLink $
        compressArchive
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
        compressArchive [tarDir "pkg", tarFile "pkg/original.txt" "original"]
      BL.writeFile archiveDirLinkCollide $
        compressArchive
          [ tarDir "smuggle",
            tarFile "smuggle/extra.txt" "smuggled",
            tarSymLink "pkg" "smuggle"
          ]
      BL.writeFile archiveHardLinkCollide $
        compressArchive
          [ tarDir "smuggle",
            tarFile "smuggle/extra.txt" "smuggled",
            tarHardLink "pkg" "smuggle"
          ]
      -- Budget probes, built against tiny caps injected via BuildConfig.
      BL.writeFile archiveManyEntries $
        compressArchive [tarFile ("e" <> show i <> ".txt") "x" | i <- [1 :: Int .. 6]]
      BL.writeFile archiveBigFile $
        compressArchive [tarFile "big.bin" (BL.replicate 100 55)]
      -- Two directory symlinks each re-copy the 40-byte payload: the
      -- copies must charge the budget like first-class content.
      BL.writeFile archiveAmplify $
        compressArchive
          [ tarDir "base",
            tarFile "base/blob.bin" (BL.replicate 40 55),
            tarSymLink "dup1" "base",
            tarSymLink "dup2" "base"
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
      buildTmp <- (</> "nova-nix-test-unpack-build-tmp") <$> getTemporaryDirectory
      forceRemoveIfExists buildTmp
      createDirectoryIfMissing True buildTmp
      let config = (defaultBuildConfig storeDir) {bcTmpDir = buildTmp}
      seedResult <- buildDerivation config store (mkUnpackDrv seedOut [archiveTools, archiveData])
      collideResult <- buildDerivation config store (mkUnpackDrv collideOut [archiveTools, archiveCollide])
      escapeResult <- buildDerivation config store (mkUnpackDrv escapeOut [archiveEscape])
      rootFileResult <- buildDerivation config store (mkUnpackDrv rootFileOut [archiveRootFile])
      escapeSymlinkResult <- buildDerivation config store (mkUnpackDrv escapeSymlinkOut [archiveEscapeSymlink])
      escapeHardlinkResult <- buildDerivation config store (mkUnpackDrv escapeHardlinkOut [archiveEscapeHardlink])
      dirLinkResult <- buildDerivation config store (mkUnpackDrv dirLinkOut [archiveDirLink])
      dirLinkCollideResult <- buildDerivation config store (mkUnpackDrv dirLinkCollideOut [archiveDirLinkBase, archiveDirLinkCollide])
      hardLinkCollideResult <- buildDerivation config store (mkUnpackDrv hardLinkCollideOut [archiveDirLinkBase, archiveHardLinkCollide])
      let tinyBudget bytes entries =
            config {bcUnpackLimits = UnpackLimits {ulMaxBytes = bytes, ulMaxEntries = entries}}
      manyEntriesResult <- buildDerivation (tinyBudget 1000000 4) store (mkUnpackDrv manyEntriesOut [archiveManyEntries])
      bigFileResult <- buildDerivation (tinyBudget 64 1000) store (mkUnpackDrv bigFileOut [archiveBigFile])
      amplifyResult <- buildDerivation (tinyBudget 100 1000) store (mkUnpackDrv amplifyOut [archiveAmplify])
      forceRemoveIfExists buildTmp
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
          pkgInfo <- Dir.doesFileExist (outRoot </> ".PKGINFO")
          mtree <- Dir.doesFileExist (outRoot </> ".MTREE")
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
              runTest "unpack: pacman metadata skipped" $
                if pkgInfo || mtree
                  then Fail ".PKGINFO/.MTREE leaked into the output"
                  else Pass,
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
              rejectedWith "size budget" amplifyResult
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
      pure (seedChecks ++ collideChecks ++ escapeChecks ++ maliciousChecks ++ dirLinkChecks ++ collisionGuardChecks ++ budgetChecks ++ guardChecks)

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
    buildTmp <- (</> "nova-nix-test-dep-build-tmp") <$> getTemporaryDirectory
    forceRemoveIfExists buildTmp
    createDirectoryIfMissing True buildTmp
    let config = (defaultBuildConfig storeDir) {bcTmpDir = buildTmp}
    result <- buildWithDeps config store rootDrv rootDrvSP
    depValid <- isValid store depOut
    rootValid <- isValid store rootOut
    forceRemoveIfExists buildTmp
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

-- | Upstream value-conformance tests: each pins exact output bytes (hash
-- decode, number formatting) that must match what upstream Nix computes for
-- the same input, because these values can reach names, derivations, and
-- hashes.
testUpstreamConformance :: IO [Bool]
testUpstreamConformance = do
  putStrLn "eval/conformance"
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
      runTest "unknown hash algorithm is rejected" $
        assertEvalFail
          "hash-bad-algo"
          "builtins.convertHash { hash = \"00\"; hashAlgo = \"sha3\"; toHashFormat = \"base16\"; }",
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
            | "wrong length" `T.isInfixOf` err -> Pass
            | otherwise -> Fail ("expected an SRI length error, got: " <> err)
          Right val -> Fail ("expected failure, got: " <> T.pack (show val)),
      runTest "fixed-output outputHash rejects a truncated SRI digest" $
        case evalNix "(derivation { name = \"t\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; outputHash = \"sha256-YWJj\"; }).drvPath" of
          Left err
            | "wrong length" `T.isInfixOf` err -> Pass
            | otherwise -> Fail ("expected an SRI length error, got: " <> err)
          Right val -> Fail ("expected failure, got: " <> T.pack (show val)),
      -- An SRI or prefixed outputHash carries its own algorithm tag; a
      -- non-empty outputHashAlgo must agree with it (hash.cc parseAny
      -- with an expected type), and an unknown declared algorithm is an
      -- error even when the spelling carries a valid tag of its own.
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
      runTest "fixed-output unknown outputHashAlgo rejected despite SRI tag" $
        case evalNix "(derivation { name = \"t\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; outputHash = \"sha256-47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU=\"; outputHashAlgo = \"sha3\"; }).drvPath" of
          Left err
            | "unknown hash algorithm" `T.isInfixOf` err -> Pass
            | otherwise -> Fail ("expected an unknown-algorithm error, got: " <> err)
          Right val -> Fail ("expected failure, got: " <> T.pack (show val)),
      runTest "builtins.path pin rejects a truncated SRI digest" $
        case evalNix "builtins.path { path = ./x; sha256 = \"sha256-YWJj\"; }" of
          Left err
            | "wrong length" `T.isInfixOf` err -> Pass
            | otherwise -> Fail ("expected an SRI length error, got: " <> err)
          Right val -> Fail ("expected failure, got: " <> T.pack (show val)),
      runTest "builtins.path pin rejects a non-sha256 SRI digest" $
        case evalNix ("builtins.path { path = ./x; sha256 = \"sha512-" <> T.replicate 86 "A" <> "==\"; }") of
          Left err
            | "should have type 'sha256'" `T.isInfixOf` err -> Pass
            | otherwise -> Fail ("expected a hash-type error, got: " <> err)
          Right val -> Fail ("expected failure, got: " <> T.pack (show val)),
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
          (mkStr "<?xml version='1.0' encoding='utf-8'?>\n<expr>\n<float value=\"0.333333\" />\n</expr>\n"),
      runTest "toXML float plain decimal" $
        assertEval
          "xml-float-centi"
          "builtins.toXML 0.01"
          (mkStr "<?xml version='1.0' encoding='utf-8'?>\n<expr>\n<float value=\"0.01\" />\n</expr>\n"),
      runTest "toXML integral float drops the point" $
        assertEval
          "xml-float-integral"
          "builtins.toXML 100.0"
          (mkStr "<?xml version='1.0' encoding='utf-8'?>\n<expr>\n<float value=\"100\" />\n</expr>\n"),
      runTest "toXML float large exponent" $
        assertEval
          "xml-float-e21"
          "builtins.toXML 1.0e21"
          (mkStr "<?xml version='1.0' encoding='utf-8'?>\n<expr>\n<float value=\"1e+21\" />\n</expr>\n"),
      runTest "toXML float small exponent" $
        assertEval
          "xml-float-e-5"
          "builtins.toXML 2.5e-5"
          (mkStr "<?xml version='1.0' encoding='utf-8'?>\n<expr>\n<float value=\"2.5e-05\" />\n</expr>\n"),
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
      -- Dynamic keys colliding with an existing attr are an eval error,
      -- never a silent last-win merge.
      runTest "dynamic key colliding with static errors" $
        assertEvalFail "dyn-collide" "{ b = 1; ${\"b\"} = 2; }",
      runTest "rec dynamic key colliding with static errors" $
        assertEvalFail "dyn-collide-rec" "rec { b = 1; p.q = 1; ${\"b\"} = 2; }",
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
      -- every builtin in the registry (the source of builtinNames/builtinArity)
      -- is actually exposed in the builtins set - guards against builtinRegistry
      -- drifting from the exposed builtins
      runTest "every registered builtin is exposed" $
        let missing = [n | n <- builtinNames, evalNix ("builtins ? \"" <> n <> "\"") /= Right (VBool True)]
         in if null missing then Pass else Fail ("not exposed: " <> T.intercalate ", " missing)
    ]

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
      -- verifyFetchHash: the composed builtin:fetchurl integrity gate that
      -- guards every fixed-output seed fetch
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

-- | Encode one NAR string per the spec: little-endian u64 length, the
-- bytes, zero-padding to the next 8-byte boundary.  Written here from the
-- format definition, deliberately independent of nova-cache's encoder, so
-- these vectors pin the wire layout rather than the library against
-- itself.
narSpecStr :: BS.ByteString -> BS.ByteString
narSpecStr s = lenLE <> s <> padding
  where
    n = BS.length s
    lenLE = BS.pack [fromIntegral ((n `shiftR` (8 * i)) .&. 0xff) | i <- [0 .. 7]]
    padding = BS.replicate ((8 - n `mod` 8) `mod` 8) 0

-- | Hand-encoded NAR of a directory holding an executable, a plain file,
-- and a symlink - covering entry ordering, the executable marker, content
-- padding, and all node kinds.
narSpecVector :: BS.ByteString
narSpecVector =
  BS.concat $
    map
      narSpecStr
      [ "nix-archive-1",
        "(",
        "type",
        "directory",
        "entry",
        "(",
        "name",
        "bin",
        "node",
        "(",
        "type",
        "directory",
        "entry",
        "(",
        "name",
        "tool",
        "node",
        "(",
        "type",
        "regular",
        "executable",
        "",
        "contents",
        "#!x",
        ")",
        ")",
        ")",
        ")",
        "entry",
        "(",
        "name",
        "data.txt",
        "node",
        "(",
        "type",
        "regular",
        "contents",
        "hello\n",
        ")",
        ")",
        "entry",
        "(",
        "name",
        "link",
        "node",
        "(",
        "type",
        "symlink",
        "target",
        "data.txt",
        ")",
        ")",
        ")"
      ]

-- | The in-memory tree 'narSpecVector' encodes.
narSpecTree :: NAR.NarEntry
narSpecTree =
  NAR.NarDirectory
    [ ("bin", NAR.NarDirectory [("tool", NAR.NarRegular True "#!x")]),
      ("data.txt", NAR.NarRegular False "hello\n"),
      ("link", NAR.NarSymlink "data.txt")
    ]

-- | Hand-encoded NAR of just @data.txt@, for the on-disk addToStore vector
-- (no executable entry: Windows has no exec bit to round-trip from disk).
narSpecFileVector :: BS.ByteString
narSpecFileVector =
  BS.concat $
    map
      narSpecStr
      [ "nix-archive-1",
        "(",
        "type",
        "directory",
        "entry",
        "(",
        "name",
        "data.txt",
        "node",
        "(",
        "type",
        "regular",
        "contents",
        "hello\n",
        ")",
        ")",
        ")"
      ]

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
          Right _ -> Fail "helper transport url evaluated"
    ]

testScratchDirs :: IO [Bool]
testScratchDirs = do
  putStrLn "eval/scratch-dirs"
  st <- newEvalState "."
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
        store <- openStore (StoreDir tmpStore)
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
    sequence
      [ runTestM "referenced path is refused with its referrers listed" $ do
          registerPaths
            (stDB store)
            [ PathRegistration dep "sha256:d" 1 Nothing [],
              PathRegistration user "sha256:u" 1 Nothing [dep]
            ]
          outcome <- deleteStorePathRaw store (basenameOf dep)
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
          userGone <- deleteStorePathRaw store (basenameOf user)
          depGone <- deleteStorePathRaw store (basenameOf dep)
          depValid <- isValid store dep
          userValid <- isValid store user
          pure $ case (userGone, depGone) of
            (Right _, Right _)
              | not depValid && not userValid -> Pass
              | otherwise -> Fail "rows survived deletion"
            other -> Fail ("chain deletion failed: " <> T.pack (show other)),
        runTestM "a registered row without a tree deletes (repair case)" $ do
          let ghost = StorePath (T.replicate 32 "c") "del-ghost"
          registerPath (stDB store) (PathRegistration ghost "sha256:g" 1 Nothing [])
          outcome <- deleteStorePathRaw store (basenameOf ghost)
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
          outcome <- deleteStorePathRaw store strayBase
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
          setReadOnly treePath
          outcome <- deleteStorePathRaw store (basenameOf solid)
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
          outcome <- deleteStorePathRaw store (basenameOf selfp)
          pure (assertEqual "self delete" (Right (DeleteOutcome True False)) outcome),
        runTestM "a target with neither row nor tree is an error" $ do
          outcome <- deleteStorePathRaw store (T.replicate 32 "9" <> "-del-nothing")
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
           in assertEqual "all resolve" (replicate 4 (Right nameBase)) (map (resolveDeleteTarget sd) spellings),
        runTest "resolveDeleteTarget refuses traversal shapes" $
          let refused =
                [ ".nova-nix",
                  "..",
                  ".",
                  "",
                  T.pack (unStoreDir sd) <> "/",
                  "/somewhere/else/" <> T.replicate 32 "h" <> "-name",
                  T.replicate 32 "h" <> "-na:me"
                ]
           in case [t | t <- refused, either (const False) (const True) (resolveDeleteTarget sd t)] of
                [] -> Pass
                accepted -> Fail ("accepted: " <> T.pack (show accepted)),
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
          outcome <- deleteStorePathRaw store lockBase
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
          outcome <- deleteStorePathRaw store (basenameOf object)
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
          outcome <- deleteStorePathRaw store junkName
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
          holder <- acquirePathLock sd guarded
          done <- newEmptyMVar
          _ <- forkIO (deleteStorePathRaw store (basenameOf guarded) >>= putMVar done)
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
              Fail ("delete never completed after release: " <> T.pack (show other))
      ]

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
        -- at the destination by an interrupted copy must be cleared and
        -- re-copied, never registered as-is.
        runTestM "materializeEvalSources re-copies a mismatched tree" $ do
          tmpBase <- getTemporaryDirectory
          let srcDir = tmpBase </> "nova-nix-test-adopt-src"
          removeIfExists srcDir
          createDirectoryIfMissing True srcDir
          TIO.writeFile (srcDir </> "data.txt") "real content"
          entry <- NAR.serialiseFromPath srcDir
          case makeFixedOutputPath "nova-nix-test-adopt-src" "sha256" "recursive" (sha256Digest (NAR.serialise entry)) of
            Left err -> pure (Fail ("test store path rejected: " <> T.pack (show err)))
            Right sp -> do
              let spText = storePathToText defaultStoreDir sp
                  dest = storePathToFilePath (stDir store) sp
              -- Fake an interrupted earlier copy: wrong partial content.
              removeIfExists dest
              createDirectoryIfMissing True dest
              TIO.writeFile (dest </> "data.txt") "partial garbage"
              materializeEvalSources store (Map.fromList [(T.pack srcDir, spText)])
              adopted <- TIO.readFile (dest </> "data.txt")
              registered <- isValid store sp
              removeIfExists srcDir
              pure $
                if adopted == "real content" && registered
                  then Pass
                  else Fail ("expected re-copied content, got: " <> adopted),
        -- A tree that DOES reproduce its store path is adopted untouched.
        -- The source is deleted before the call: adoption never reads it,
        -- so a wrongful re-copy attempt throws and fails the test.
        runTestM "materializeEvalSources adopts a matching tree untouched" $ do
          tmpBase <- getTemporaryDirectory
          let srcDir = tmpBase </> "nova-nix-test-adopt-ok"
          removeIfExists srcDir
          createDirectoryIfMissing True srcDir
          TIO.writeFile (srcDir </> "data.txt") "same bytes"
          entry <- NAR.serialiseFromPath srcDir
          case makeFixedOutputPath "nova-nix-test-adopt-ok" "sha256" "recursive" (sha256Digest (NAR.serialise entry)) of
            Left err -> pure (Fail ("test store path rejected: " <> T.pack (show err)))
            Right sp -> do
              let spText = storePathToText defaultStoreDir sp
                  dest = storePathToFilePath (stDir store) sp
              removeIfExists dest
              createDirectoryIfMissing True dest
              TIO.writeFile (dest </> "data.txt") "same bytes"
              removeIfExists srcDir
              materializeEvalSources store (Map.fromList [(T.pack srcDir, spText)])
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
        -- scanTempReferences records the self/cross-output edges that the
        -- store-prefix scan cannot see (outputs embedding their TEMP path)
        runTestM "scanTempReferences finds temp-dir embedding" $ do
          tmpBase <- getTemporaryDirectory
          let scanDir = tmpBase </> "nova-nix-test-scan-temp"
              fakeTempOut = tmpBase </> "nova-nix-build" </> "fake-out"
          removeIfExists scanDir
          createDirectoryIfMissing True scanDir
          let selfSp = StorePath "cccccccccccccccccccccccccccccccc" "self"
          BS.writeFile
            (scanDir </> "wrapper.sh")
            (TE.encodeUtf8 (T.pack ("exec " <> fakeTempOut <> "/bin/tool")))
          found <- scanTempReferences [(fakeTempOut, selfSp)] scanDir
          removeIfExists scanDir
          pure (assertEqual "self-ref found" [selfSp] found),
        runTestM "scanTempReferences negative" $ do
          tmpBase <- getTemporaryDirectory
          let scanDir = tmpBase </> "nova-nix-test-scan-temp-neg"
          removeIfExists scanDir
          createDirectoryIfMissing True scanDir
          let selfSp = StorePath "cccccccccccccccccccccccccccccccc" "self"
          BS.writeFile (scanDir </> "clean.txt") "no temp paths embedded here"
          found <- scanTempReferences [(tmpBase </> "nova-nix-build" </> "fake-out", selfSp)] scanDir
          removeIfExists scanDir
          pure (assertEqual "no refs" [] found),
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
        -- setReadOnly works on a plain directory
        runTestM "setReadOnly makes dir read-only" $ do
          tmpBase <- getTemporaryDirectory
          let roDir = tmpBase </> "nova-nix-test-readonly"
          removeIfExists roDir
          createDirectoryIfMissing True roDir
          writeFile (roDir </> "f.txt") "content"
          setReadOnly roDir
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
        [ -- A link out of the tree must not have the read-only bit
          -- pushed through to its target.
          runTestM "setReadOnly does not mark link targets outside the tree" $ do
            tmpBase <- getTemporaryDirectory
            let base = tmpBase </> "nova-nix-test-ro-links"
                outside = base </> "outside"
                tree = base </> "tree"
            Dir.removePathForcibly base
            createDirectoryIfMissing True outside
            createDirectoryIfMissing True tree
            BS.writeFile (outside </> "victim.txt") "victim"
            BS.writeFile (tree </> "inside.txt") "inside"
            Dir.createFileLink (outside </> "victim.txt") (tree </> "file-link")
            Dir.createDirectoryLink outside (tree </> "dir-link")
            setReadOnly tree
            victimWritable <- writable <$> getPermissions (outside </> "victim.txt")
            insideWritable <- writable <$> getPermissions (tree </> "inside.txt")
            Dir.removePathForcibly base
            pure $
              if victimWritable && not insideWritable
                then Pass
                else
                  Fail
                    ( "victim writable="
                        <> T.pack (show victimWritable)
                        <> " inside writable="
                        <> T.pack (show insideWritable)
                    ),
          -- A link cycle must not recurse the walk forever.
          runTestM "setReadOnly terminates on a link cycle" $ do
            tmpBase <- getTemporaryDirectory
            let tree = tmpBase </> "nova-nix-test-ro-cycle"
            Dir.removePathForcibly tree
            createDirectoryIfMissing True (tree </> "sub")
            BS.writeFile (tree </> "sub" </> "f.txt") "f"
            Dir.createDirectoryLink tree (tree </> "sub" </> "loop")
            outcome <- timeout walkWatchdogMicros (setReadOnly tree)
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
  sequence
    [ runTest "chain orders targets first" $
        let pending = [(root </> "l1", "l2"), (root </> "l2", "l3"), (root </> "l3", "real")]
         in assertEqual
              "chain"
              [root </> "l3", root </> "l2", root </> "l1"]
              (map fst (orderLinks pending)),
      runTest "already-ordered chain is stable" $
        let pending = [(root </> "l3", "real"), (root </> "l2", "l3"), (root </> "l1", "l2")]
         in assertEqual
              "stable"
              [root </> "l3", root </> "l2", root </> "l1"]
              (map fst (orderLinks pending)),
      runTest "link through a linked directory follows it" $
        let pending = [(root </> "a", "dirlink/x"), (root </> "dirlink", "realdir")]
         in assertEqual
              "through-dir"
              [root </> "dirlink", root </> "a"]
              (map fst (orderLinks pending)),
      runTest "parent-relative target orders after its link" $
        let pending = [(root </> "sub" </> "l", "../other"), (root </> "other", "real")]
         in assertEqual
              "dotdot"
              [root </> "other", root </> "sub" </> "l"]
              (map fst (orderLinks pending)),
      runTest "cycle members keep input order at the end" $
        let pending = [(root </> "a", "b"), (root </> "b", "a"), (root </> "c", "real")]
         in assertEqual
              "cycle"
              [root </> "c", root </> "a", root </> "b"]
              (map fst (orderLinks pending)),
      runTest "self-target link survives to the fallback" $
        assertEqual
          "self"
          [root </> "x"]
          (map fst (orderLinks [(root </> "x", "x")])),
      runTestM "long chain orders in linear time" $ do
        -- 20000 links each targeting the next: the ready-set rounds
        -- this replaced were quadratic here.
        let chain =
              [ (root </> ("l" <> show i), T.pack ("l" <> show (i + 1)))
              | i <- [1 :: Int .. 20000]
              ]
                ++ [(root </> "l20001", "real")]
            complete = length (orderLinks chain) == length chain
        outcome <- timeout walkWatchdogMicros (evaluate complete)
        pure $ case outcome of
          Just True -> Pass
          Just False -> Fail "ordering dropped links"
          Nothing -> Fail "ordering did not return promptly"
    ]

-- ---------------------------------------------------------------------------
-- Tests: fromATerm + Derivation Output Population (Phase 2, Batch 3)
-- ---------------------------------------------------------------------------

-- | A simple test derivation for round-trip testing.
simpleTestDrv :: Derivation
simpleTestDrv =
  Derivation
    { drvOutputs =
        [ DerivationOutput
            { doName = "out",
              doPath = StorePath "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "hello-1.0",
              doHashAlgo = "",
              doHash = ""
            }
        ],
      drvInputDrvs = Map.empty,
      drvInputSrcs = [],
      drvPlatform = X86_64_Linux,
      drvBuilder = "/nix/store/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb-bash-5.2/bin/bash",
      drvArgs = ["-e", "/nix/store/cccccccccccccccccccccccccccccccc-stdenv/setup"],
      drvEnv = Map.fromList [("name", "hello-1.0"), ("system", "x86_64-linux")]
    }

-- | A complex test derivation with multiple outputs, input drvs, and input srcs.
complexTestDrv :: Derivation
complexTestDrv =
  Derivation
    { drvOutputs =
        [ DerivationOutput "dev" (StorePath "dddddddddddddddddddddddddddddddd" "pkg-2.0-dev") "" "",
          DerivationOutput "out" (StorePath "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "pkg-2.0") "" ""
        ],
      drvInputDrvs =
        Map.fromList
          -- Hash fixtures stay inside the nix-base32 alphabet (no e o u t):
          -- fromATerm parses these through parseStorePath, which now
          -- charset-checks the hash component.
          [ (StorePath "cccccccccccccccccccccccccccccccc" "dep1.drv", ["out"]),
            (StorePath "ffffffffffffffffffffffffffffffff" "dep2.drv", ["lib", "out"])
          ],
      drvInputSrcs = [StorePath "gggggggggggggggggggggggggggggggg" "source.tar.gz"],
      drvPlatform = Aarch64_Darwin,
      drvBuilder = "/nix/store/hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh-bash/bin/bash",
      drvArgs = ["-e", "build.sh"],
      drvEnv =
        Map.fromList
          [ ("buildInputs", "/nix/store/cccccccccccccccccccccccccccccccc-dep1"),
            ("name", "pkg-2.0"),
            ("out", "/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-pkg-2.0"),
            ("system", "aarch64-darwin")
          ]
    }

testFromATerm :: IO [Bool]
testFromATerm = do
  putStrLn "derivation/fromATerm"
  sequence
    [ -- Round-trip: simple derivation
      runTest "fromATerm round-trip simple" $
        assertEqual "simple round-trip" (Right simpleTestDrv) (fromATerm (toATerm simpleTestDrv)),
      -- Round-trip: complex derivation
      runTest "fromATerm round-trip complex" $
        assertEqual "complex round-trip" (Right complexTestDrv) (fromATerm (toATerm complexTestDrv)),
      -- A .drv read from disk is input: an output name that violates the
      -- store-name rules (here a traversal shape that would later join
      -- the build dir) must refuse at parse.
      runTest "fromATerm rejects a traversal-shaped output name" $
        let evil =
              simpleTestDrv
                { drvOutputs =
                    [DerivationOutput "../evil" (StorePath (T.replicate 32 "a") "pkg") "" ""]
                }
         in case fromATerm (toATerm evil) of
              Left _ -> Pass
              Right _ -> Fail "parsed a drv with a traversal output name",
      -- drv4: the modulo-substitution section merges input-drv entries that
      -- share a modulo-hash key, unioning their output-name sets, so it is
      -- byte-identical to the already-merged form.  (Just subs replaces the
      -- whole input-drv section, so simpleTestDrv's own inputs are irrelevant.)
      runTest "inputDrvsSubst merges equal modulo keys" $
        assertEqual
          "subst-merge"
          (toATermForHash False (Just [("00000000", ["dev", "out"])]) simpleTestDrv)
          (toATermForHash False (Just [("00000000", ["out"]), ("00000000", ["dev"])]) simpleTestDrv),
      runTest "inputDrvsSubst unions and dedups merged out-names" $
        assertEqual
          "subst-union"
          (toATermForHash False (Just [("aabbccdd", ["dev", "out"])]) simpleTestDrv)
          (toATermForHash False (Just [("aabbccdd", ["out"]), ("aabbccdd", ["out", "dev"])]) simpleTestDrv),
      -- Distinct keys are untouched: still one entry each, ascending by key.
      runTest "inputDrvsSubst preserves distinct keys in order" $
        assertEqual
          "subst-distinct"
          (toATermForHash False (Just [("00000000", ["out"]), ("11111111", ["out"])]) simpleTestDrv)
          (toATermForHash False (Just [("11111111", ["out"]), ("00000000", ["out"])]) simpleTestDrv),
      -- Round-trip: empty derivation (no outputs, no inputs, no args, no env)
      runTest "fromATerm round-trip empty" $
        let emptyDrv =
              Derivation
                { drvOutputs = [],
                  drvInputDrvs = Map.empty,
                  drvInputSrcs = [],
                  drvPlatform = X86_64_Linux,
                  drvBuilder = "/bin/true",
                  drvArgs = [],
                  drvEnv = Map.empty
                }
         in assertEqual "empty round-trip" (Right emptyDrv) (fromATerm (toATerm emptyDrv)),
      -- Reject empty string
      runTest "fromATerm rejects empty" $
        assertLeft "empty" (fromATerm ""),
      -- Reject malformed
      runTest "fromATerm rejects malformed" $
        assertLeft "malformed" (fromATerm "NotADerivation"),
      -- Reject truncated
      runTest "fromATerm rejects truncated" $
        assertLeft "truncated" (fromATerm "Derive(["),
      -- Escaping round-trip: strings with special chars
      runTest "fromATerm escaping round-trip" $
        let escapeDrv =
              Derivation
                { drvOutputs =
                    [ DerivationOutput
                        { doName = "out",
                          doPath = StorePath "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "esc-test",
                          doHashAlgo = "",
                          doHash = ""
                        }
                    ],
                  drvInputDrvs = Map.empty,
                  drvInputSrcs = [],
                  drvPlatform = X86_64_Linux,
                  drvBuilder = "/bin/bash",
                  drvArgs = ["-c", "echo \"hello\nworld\""],
                  drvEnv = Map.fromList [("msg", "line1\nline2\ttab\\slash")]
                }
         in assertEqual "escape round-trip" (Right escapeDrv) (fromATerm (toATerm escapeDrv)),
      -- A non-standard escape keeps the byte and drops the backslash,
      -- matching upstream's .drv string parser.
      runTest "fromATerm drops the backslash on a non-standard escape" $
        let aterm = "Derive([(\"out\",\"/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-x\",\"\",\"\")],[],[],\"x86_64-linux\",\"/bin/sh\",[],[(\"k\",\"a\\xb\")])"
         in assertRight "nonstandard escape" (fromATerm aterm) $ \drv ->
              assertEqual "escape dropped" (Just "axb") (Map.lookup "k" (drvEnv drv)),
      -- writeDrv writes correct ATerm
      runTestM "writeDrv writes correct ATerm" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-writeDrv"
        removeIfExists tmpStore
        store <- openStore (StoreDir tmpStore)
        let sp = StorePath "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "test.drv"
            destFile = storePathToFilePath (stDir store) sp
        writeDrv store simpleTestDrv sp
        contents <- BS.readFile destFile
        closeStore store
        removeIfExists tmpStore
        pure (assertEqual "writeDrv content" (toATerm simpleTestDrv) contents),
      -- The .drv closure registers as store objects: a row for every
      -- recipe plus edges to its input sources and input .drvs, so an
      -- output reference scan that names an input .drv resolves instead
      -- of failing the registration batch.
      runTestM "writeDrvClosure registers recipes with their references" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-drv-closure"
            sd = StoreDir tmpStore
        removeIfExists tmpStore
        store <- openStore sd
        let srcSp = StorePath "gggggggggggggggggggggggggggggggg" "source.tar.gz"
            drvASp = StorePath "cccccccccccccccccccccccccccccccc" "dep1.drv"
            drvBSp = StorePath "dddddddddddddddddddddddddddddddd" "top.drv"
            drvB =
              simpleTestDrv
                { drvInputDrvs = Map.fromList [(drvASp, ["out"])],
                  drvInputSrcs = [srcSp]
                }
        -- The source row registers first, as in the build driver, where
        -- materializeEvalSources precedes the closure write.
        registerPath (stDB store) (PathRegistration srcSp "sha256:0" 0 Nothing [])
        writeDrvClosure
          store
          ( Map.fromList
              [ (storePathToText defaultStoreDir drvASp, toATerm simpleTestDrv),
                (storePathToText defaultStoreDir drvBSp, toATerm drvB)
              ]
          )
        validA <- isValid store drvASp
        validB <- isValid store drvBSp
        refsB <- queryReferences (stDB store) drvBSp
        closeStore store
        removeIfExists tmpStore
        let expectedRefs =
              sort
                [ T.pack (storePathToFilePath sd drvASp),
                  T.pack (storePathToFilePath sd srcSp)
                ]
        pure $
          if not validA
            then Fail "input .drv not registered"
            else
              if not validB
                then Fail "root .drv not registered"
                else assertEqual "drv references" expectedRefs (sort refsB),
      -- builtinDerivation populates drvOutputs
      runTest "builtinDerivation populates drvOutputs"
        $ assertRight
          "drvOutputs"
          (evalNix "let d = derivation { name = \"test\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; }; in d._derivation")
        $ \val -> case val of
          VDerivation drv ->
            case drvOutputs drv of
              [] -> Fail "drvOutputs is empty"
              (firstOut : _) ->
                if doName firstOut == "out"
                  then Pass
                  else Fail ("first output name: " <> doName firstOut)
          _ -> Fail ("expected VDerivation, got " <> T.pack (show val)),
      -- builtinDerivation multi-output populates drvOutputs
      runTest "builtinDerivation multi-output"
        $ assertRight
          "multi-output"
          (evalNix "let d = derivation { name = \"multi\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; outputs = [\"out\" \"dev\"]; }; in d._derivation")
        $ \val -> case val of
          VDerivation drv ->
            let names = map doName (drvOutputs drv)
             in if names == ["out", "dev"]
                  then Pass
                  else Fail ("output names: " <> T.pack (show names))
          _ -> Fail ("expected VDerivation, got " <> T.pack (show val)),
      -- builtinDerivation populates drvEnv with the output paths ($out, ...)
      -- and the build attributes.  Note: the .drv env does NOT contain a
      -- "drvPath" key - matching C++ Nix, which never writes one.
      runTest "builtinDerivation populates drvEnv"
        $ assertRight
          "drvEnv"
          (evalNix "let d = derivation { name = \"test\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; }; in d._derivation")
        $ \val -> case val of
          VDerivation drv
            | Just op <- Map.lookup "out" (drvEnv drv),
              "/nix/store/" `BS.isPrefixOf` op,
              Just nm <- Map.lookup "name" (drvEnv drv),
              nm == "test" ->
                Pass
            | otherwise ->
                Fail ("drvEnv keys: " <> T.pack (show (Map.toList (drvEnv drv))))
          _ -> Fail ("expected VDerivation, got " <> T.pack (show val))
    ]

-- ---------------------------------------------------------------------------
-- Tests: Builder (Phase 2, Batch 4)
-- ---------------------------------------------------------------------------

-- | Create a minimal Derivation for builder tests.
-- The shell path is discovered once via 'findTestShell' and threaded through.
-- All scripts are POSIX shell - bash is used on every platform.
mkTestBuildDrv :: Text -> StorePath -> Text -> Derivation
mkTestBuildDrv shell outSP script =
  Derivation
    { drvOutputs =
        [ DerivationOutput
            { doName = "out",
              doPath = outSP,
              doHashAlgo = "",
              doHash = ""
            }
        ],
      drvInputDrvs = Map.empty,
      drvInputSrcs = [],
      drvPlatform = currentPlatform,
      drvBuilder = TE.encodeUtf8 shell,
      drvArgs = ["-c", TE.encodeUtf8 script],
      drvEnv = Map.fromList [("name", "test-build"), ("system", TE.encodeUtf8 (platformToText currentPlatform))]
    }

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
        store <- openStore (StoreDir tmpStore)
        let outSP = StorePath "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1" "simple-test"
            drv = mkTestBuildDrv shell outSP "mkdir -p $out && echo hello > $out/result.txt"
            config = (defaultBuildConfig (stDir store)) {bcTmpDir = tmpBase </> "nova-nix-test-builder1-tmp"}
        result <- buildDerivation config store drv
        closeStore store
        let ret = case result of
              BuildSuccess sp -> assertEqual "success path" outSP sp
              BuildFailure msg code -> Fail ("build failed (" <> T.pack (show code) <> "): " <> msg)
        forceRemoveIfExists tmpStore
        forceRemoveIfExists (bcTmpDir config)
        pure ret,
      -- Build with specific file content
      runTestM "build writes file content" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-builder2"
        forceRemoveIfExists tmpStore
        store <- openStore (StoreDir tmpStore)
        let outSP = StorePath "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" "content-test"
            drv = mkTestBuildDrv shell outSP "mkdir -p $out && echo 'test content 42' > $out/data.txt"
            config = (defaultBuildConfig (stDir store)) {bcTmpDir = tmpBase </> "nova-nix-test-builder2-tmp"}
        result <- buildDerivation config store drv
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
        forceRemoveIfExists (bcTmpDir config)
        pure ret,
      -- Missing builder fails
      runTestM "missing builder fails" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-builder3"
        forceRemoveIfExists tmpStore
        store <- openStore (StoreDir tmpStore)
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
            config = (defaultBuildConfig (stDir store)) {bcTmpDir = tmpBase </> "nova-nix-test-builder3-tmp"}
        result <- buildDerivation config store drv
        closeStore store
        forceRemoveIfExists tmpStore
        forceRemoveIfExists (bcTmpDir config)
        pure $ case result of
          BuildFailure _ _ -> Pass
          BuildSuccess _ -> Fail "expected failure for missing builder",
      -- Exit failure returns error code
      runTestM "exit failure returns error code" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-builder4"
        forceRemoveIfExists tmpStore
        store <- openStore (StoreDir tmpStore)
        let outSP = StorePath "dddddddddddddddddddddddddddddddd" "exitfail"
            drv = mkTestBuildDrv shell outSP "exit 42"
            config = (defaultBuildConfig (stDir store)) {bcTmpDir = tmpBase </> "nova-nix-test-builder4-tmp"}
        result <- buildDerivation config store drv
        closeStore store
        forceRemoveIfExists tmpStore
        forceRemoveIfExists (bcTmpDir config)
        pure $ case result of
          BuildFailure _ code -> if code == 42 then Pass else Fail ("expected code 42, got " <> T.pack (show code))
          BuildSuccess _ -> Fail "expected failure",
      -- A builder that THROWS (nonexistent executable) must not leak the
      -- deterministic build dir: stale contents would fake an archive
      -- collision on the next builtin:unpack run of the same drv.
      runTestM "crashed build removes its build dir" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-builder-leak"
            tmpBuild = tmpBase </> "nova-nix-test-builder-leak-tmp"
        forceRemoveIfExists tmpStore
        forceRemoveIfExists tmpBuild
        store <- openStore (StoreDir tmpStore)
        let outSP = StorePath "pppppppppppppppppppppppppppppppp" "leaktest"
            drv = mkTestBuildDrv (T.pack (tmpBase </> "no-such-builder.exe")) outSP "unused"
            config = (defaultBuildConfig (stDir store)) {bcTmpDir = tmpBuild}
        result <- buildDerivation config store drv
        leftovers <- do
          exists <- doesDirectoryExist tmpBuild
          if exists then Dir.listDirectory tmpBuild else pure []
        closeStore store
        forceRemoveIfExists tmpStore
        forceRemoveIfExists tmpBuild
        pure $ case result of
          BuildFailure _ _
            | null leftovers -> Pass
            | otherwise -> Fail ("build dir leaked: " <> T.pack (show leftovers))
          BuildSuccess _ -> Fail "expected failure for a nonexistent builder",
      -- Output at expected path
      runTestM "output at expected store path" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-builder5"
        forceRemoveIfExists tmpStore
        store <- openStore (StoreDir tmpStore)
        let outSP = StorePath "cccccccccccccccccccccccccccccccc" "pathtest"
            drv = mkTestBuildDrv shell outSP "mkdir -p $out && touch $out/marker"
            config = (defaultBuildConfig (stDir store)) {bcTmpDir = tmpBase </> "nova-nix-test-builder5-tmp"}
        result <- buildDerivation config store drv
        ret <- case result of
          BuildSuccess _ -> do
            let expectedDir = storePathToFilePath (stDir store) outSP
            exists <- doesDirectoryExist expectedDir
            pure $ if exists then Pass else Fail "output dir doesn't exist at expected path"
          BuildFailure msg code -> pure (Fail ("build failed (" <> T.pack (show code) <> "): " <> msg))
        closeStore store
        forceRemoveIfExists tmpStore
        forceRemoveIfExists (bcTmpDir config)
        pure ret,
      -- A leftover tree at the output path that is NOT valid in the DB has
      -- unknowable integrity (interrupted earlier run); the build replaces
      -- it with the fresh output - upstream's delete-then-move - clearing
      -- read-only marks rather than adopting stale bytes.
      runTestM "stale leftover output is replaced by the fresh build" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-builder-stale"
        forceRemoveIfExists tmpStore
        store <- openStore (StoreDir tmpStore)
        let outSP = StorePath "qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq" "staletest"
            drv = mkTestBuildDrv shell outSP "mkdir -p $out && echo fresh > $out/data.txt"
            config = (defaultBuildConfig (stDir store)) {bcTmpDir = tmpBase </> "nova-nix-test-builder-stale-tmp"}
            targetPath = storePathToFilePath (stDir store) outSP
        -- Fake the interrupted run: stale read-only content at the output's
        -- store location, never registered.
        createDirectoryIfMissing True targetPath
        TIO.writeFile (targetPath </> "data.txt") "stale"
        setReadOnly targetPath
        result <- buildDerivation config store drv
        content <- TIO.readFile (targetPath </> "data.txt")
        registered <- isValid store outSP
        closeStore store
        forceRemoveIfExists tmpStore
        forceRemoveIfExists (bcTmpDir config)
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
        store <- openStore (StoreDir tmpStore)
        let outSP = StorePath "ffffffffffffffffffffffffffffffff" "dbtest"
            drv = mkTestBuildDrv shell outSP "mkdir -p $out && touch $out/file"
            config = (defaultBuildConfig (stDir store)) {bcTmpDir = tmpBase </> "nova-nix-test-builder6-tmp"}
        result <- buildDerivation config store drv
        ret <- case result of
          BuildSuccess _ -> do
            valid <- isValid store outSP
            pure $ if valid then Pass else Fail "path not valid in DB after build"
          BuildFailure msg code -> pure (Fail ("build failed (" <> T.pack (show code) <> "): " <> msg))
        closeStore store
        forceRemoveIfExists tmpStore
        forceRemoveIfExists (bcTmpDir config)
        pure ret,
      -- Multiple outputs
      runTestM "multiple outputs" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-builder7"
        forceRemoveIfExists tmpStore
        store <- openStore (StoreDir tmpStore)
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
                  drvEnv = Map.fromList [("name", "multi")]
                }
            config = (defaultBuildConfig (stDir store)) {bcTmpDir = tmpBase </> "nova-nix-test-builder7-tmp"}
        result <- buildDerivation config store drv
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
        forceRemoveIfExists (bcTmpDir config)
        pure ret,
      -- Builder succeeds but doesn't create $out, so the build fails
      runTestM "missing output fails build" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-builder-noout"
        forceRemoveIfExists tmpStore
        store <- openStore (StoreDir tmpStore)
        let outSP = StorePath "iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii" "nooutput"
            drv = mkTestBuildDrv shell outSP "echo 'forgot to create output'"
            config = (defaultBuildConfig (stDir store)) {bcTmpDir = tmpBase </> "nova-nix-test-builder-noout-tmp"}
        result <- buildDerivation config store drv
        closeStore store
        forceRemoveIfExists tmpStore
        forceRemoveIfExists (bcTmpDir config)
        pure $ case result of
          BuildFailure msg _ ->
            if T.isInfixOf "outputs missing" msg
              then Pass
              else Fail ("expected 'outputs missing' error, got: " <> msg)
          BuildSuccess _ -> Fail "expected failure when builder doesn't create $out",
      -- File output (not directory) should succeed
      runTestM "file output succeeds" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-builder-fileout"
        forceRemoveIfExists tmpStore
        store <- openStore (StoreDir tmpStore)
        let outSP = StorePath "jjjjjjjjjjjjjjjjjjjjjjjjjjjjjj" "fileout"
            drv = mkTestBuildDrv shell outSP "echo 'I am a file output' > $out"
            config = (defaultBuildConfig (stDir store)) {bcTmpDir = tmpBase </> "nova-nix-test-builder-fileout-tmp"}
        result <- buildDerivation config store drv
        closeStore store
        forceRemoveIfExists tmpStore
        forceRemoveIfExists (bcTmpDir config)
        pure $ case result of
          BuildSuccess sp -> assertEqual "file output path" outSP sp
          BuildFailure msg code -> Fail ("file output build failed (" <> T.pack (show code) <> "): " <> msg),
      -- Cleanup after failure
      runTestM "cleanup after failure" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-builder8"
        forceRemoveIfExists tmpStore
        store <- openStore (StoreDir tmpStore)
        let outSP = StorePath "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh" "cleantest"
            drv = mkTestBuildDrv shell outSP "exit 1"
            tmpDir = tmpBase </> "nova-nix-test-builder8-tmp"
            config = (defaultBuildConfig (stDir store)) {bcTmpDir = tmpDir}
        _ <- buildDerivation config store drv
        -- Build dir should be cleaned up
        let buildDir = tmpDir </> T.unpack (spHash outSP)
        buildDirExists <- doesDirectoryExist buildDir
        closeStore store
        forceRemoveIfExists tmpStore
        forceRemoveIfExists tmpDir
        pure $ if not buildDirExists then Pass else Fail "build dir not cleaned up after failure",
      -- Fixed-output: registration re-checks the placed bytes
      runTestM "fixed-output flat output verifies and registers" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-fo1"
        forceRemoveIfExists tmpStore
        store <- openStore (StoreDir tmpStore)
        let payload = "fixed output payload\n"
            digest = maybe "" Hash.bytesToHexText (Hash.rawHashWithAlgo "sha256" payload)
            outSP = StorePath "ffffffffffffffffffffffffffffff01" "fo-flat-good"
            drv = mkFixedOutputDrv shell outSP "printf 'fixed output payload\\n' > $out" "sha256" digest
            config = (defaultBuildConfig (stDir store)) {bcTmpDir = tmpBase </> "nova-nix-test-fo1-tmp"}
        result <- buildDerivation config store drv
        registered <- isValid store outSP
        closeStore store
        forceRemoveIfExists tmpStore
        forceRemoveIfExists (bcTmpDir config)
        pure $ case result of
          BuildSuccess _
            | registered -> Pass
            | otherwise -> Fail "built but not registered valid"
          BuildFailure msg code -> Fail ("expected success (" <> T.pack (show code) <> "): " <> msg),
      runTestM "fixed-output flat mismatch fails and removes the output" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-fo2"
        forceRemoveIfExists tmpStore
        store <- openStore (StoreDir tmpStore)
        let digest = maybe "" Hash.bytesToHexText (Hash.rawHashWithAlgo "sha256" "expected content\n")
            outSP = StorePath "ffffffffffffffffffffffffffffff02" "fo-flat-bad"
            drv = mkFixedOutputDrv shell outSP "printf 'tampered content\\n' > $out" "sha256" digest
            config = (defaultBuildConfig (stDir store)) {bcTmpDir = tmpBase </> "nova-nix-test-fo2-tmp"}
        result <- buildDerivation config store drv
        registered <- isValid store outSP
        onDisk <- Dir.doesPathExist (storePathToFilePath (stDir store) outSP)
        closeStore store
        forceRemoveIfExists tmpStore
        forceRemoveIfExists (bcTmpDir config)
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
        store <- openStore (StoreDir tmpStore)
        let tree = NAR.NarDirectory [("data.txt", NAR.NarRegular False "inner bytes\n")]
            digest = maybe "" Hash.bytesToHexText (Hash.rawHashWithAlgo "sha256" (NAR.serialise tree))
            outSP = StorePath "ffffffffffffffffffffffffffffff03" "fo-rec-good"
            drv = mkFixedOutputDrv shell outSP "mkdir -p $out && printf 'inner bytes\\n' > $out/data.txt" "r:sha256" digest
            config = (defaultBuildConfig (stDir store)) {bcTmpDir = tmpBase </> "nova-nix-test-fo3-tmp"}
        result <- buildDerivation config store drv
        registered <- isValid store outSP
        closeStore store
        forceRemoveIfExists tmpStore
        forceRemoveIfExists (bcTmpDir config)
        pure $ case result of
          BuildSuccess _
            | registered -> Pass
            | otherwise -> Fail "built but not registered valid"
          BuildFailure msg code -> Fail ("expected success (" <> T.pack (show code) <> "): " <> msg),
      runTestM "fixed-output recursive mismatch fails the build" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-fo4"
        forceRemoveIfExists tmpStore
        store <- openStore (StoreDir tmpStore)
        let tree = NAR.NarDirectory [("data.txt", NAR.NarRegular False "declared bytes\n")]
            digest = maybe "" Hash.bytesToHexText (Hash.rawHashWithAlgo "sha256" (NAR.serialise tree))
            outSP = StorePath "ffffffffffffffffffffffffffffff04" "fo-rec-bad"
            drv = mkFixedOutputDrv shell outSP "mkdir -p $out && printf 'other bytes\\n' > $out/data.txt" "r:sha256" digest
            config = (defaultBuildConfig (stDir store)) {bcTmpDir = tmpBase </> "nova-nix-test-fo4-tmp"}
        result <- buildDerivation config store drv
        registered <- isValid store outSP
        closeStore store
        forceRemoveIfExists tmpStore
        forceRemoveIfExists (bcTmpDir config)
        pure $ case result of
          BuildFailure msg _
            | "hash mismatch" `T.isInfixOf` msg && not registered -> Pass
            | otherwise -> Fail ("wrong failure shape: " <> msg)
          BuildSuccess _ -> Fail "mismatched fixed output registered",
      runTestM "fixed-output flat mode rejects a directory output" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-fo5"
        forceRemoveIfExists tmpStore
        store <- openStore (StoreDir tmpStore)
        let digest = maybe "" Hash.bytesToHexText (Hash.rawHashWithAlgo "sha256" "whatever\n")
            outSP = StorePath "ffffffffffffffffffffffffffffff05" "fo-flat-dir"
            drv = mkFixedOutputDrv shell outSP "mkdir -p $out && printf 'x' > $out/f" "sha256" digest
            config = (defaultBuildConfig (stDir store)) {bcTmpDir = tmpBase </> "nova-nix-test-fo5-tmp"}
        result <- buildDerivation config store drv
        closeStore store
        forceRemoveIfExists tmpStore
        forceRemoveIfExists (bcTmpDir config)
        pure $ case result of
          BuildFailure msg _
            | "directory" `T.isInfixOf` msg -> Pass
            | otherwise -> Fail ("wrong failure: " <> msg)
          BuildSuccess _ -> Fail "directory output passed a flat fixed-output check"
    ]

-- ---------------------------------------------------------------------------
-- Tests: CLI Integration (Phase 2, Batch 5)
-- ---------------------------------------------------------------------------

-- | End-to-end: eval .nix source, extract derivation, build, verify output.
evalAndBuild :: StoreDir -> Text -> IO (Either Text (BuildResult, Store))
evalAndBuild storeDir source = do
  case parseNix "<test>" source of
    Left err -> pure (Left ("parse error: " <> T.pack (show err)))
    Right expr -> do
      st <- newEvalState "."
      evalResult <- runEvalIO st $ do
        val <- eval (builtinEnv (esTimestamp st) (esSearchPaths st)) expr
        -- 'derivation' is lazy now; force _derivation so the peek below sees it
        -- (and so a missing required attr surfaces as an eval error).
        case val of
          VAttrs attrs -> maybe (pure ()) (void . force) (attrSetLookup "_derivation" attrs)
          _ -> pure ()
        pure val
      case evalResult of
        Left err -> pure (Left ("eval error: " <> err))
        Right val -> case val of
          VAttrs attrs -> case attrSetLookup "_derivation" attrs >>= readThunkValue of
            Just (VDerivation drv) -> do
              store <- openStore storeDir
              tmpBase <- getTemporaryDirectory
              let config = (defaultBuildConfig storeDir) {bcTmpDir = tmpBase </> "nova-nix-e2e-tmp"}
              result <- buildDerivation config store drv
              pure (Right (result, store))
            _ -> pure (Left "no _derivation in result attrs")
          _ -> pure (Left "result is not an attrset")

testE2E :: IO [Bool]
testE2E = do
  putStrLn "cli/e2e"
  shell <- findTestShell
  sequence
    [ -- End-to-end: eval -> build a simple derivation
      runTestM "e2e eval -> build" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-e2e1"
        forceRemoveIfExists tmpStore
        -- Build the Nix source with the discovered shell path.
        -- Nix strings use \\ for literal backslash, \" for literal quote.
        let nixEscape = T.concatMap (\c -> if c == '\\' then "\\\\" else if c == '"' then "\\\"" else T.singleton c)
            e2eSource =
              T.concat
                [ "derivation { name = \"e2e-test\"; system = builtins.currentSystem; ",
                  "builder = \"" <> nixEscape shell <> "\"; ",
                  "args = [\"-c\" \"mkdir -p $out && echo e2e > $out/e2e.txt\"]; }"
                ]
        result <- evalAndBuild (StoreDir tmpStore) e2eSource
        ret <- case result of
          Left err -> pure (Fail err)
          Right (BuildSuccess _, store) -> do
            closeStore store
            pure Pass
          Right (BuildFailure msg code, store) -> do
            closeStore store
            pure (Fail ("build failed (" <> T.pack (show code) <> "): " <> msg))
        forceRemoveIfExists tmpStore
        forceRemoveIfExists (tmpBase </> "nova-nix-e2e-tmp")
        pure ret,
      -- Parse error produces Left
      runTestM "e2e parse error" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-e2e2"
        forceRemoveIfExists tmpStore
        result <- evalAndBuild (StoreDir tmpStore) "{{ invalid nix"
        forceRemoveIfExists tmpStore
        pure $ case result of
          Left msg ->
            if "parse error" `T.isInfixOf` msg
              then Pass
              else Fail ("expected parse error, got: " <> msg)
          Right _ -> Fail "expected error but got success",
      -- Eval error produces Left
      runTestM "e2e eval error" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-e2e3"
        forceRemoveIfExists tmpStore
        result <- evalAndBuild (StoreDir tmpStore) "derivation { }"
        forceRemoveIfExists tmpStore
        pure $ case result of
          Left msg ->
            if "error" `T.isInfixOf` T.toLower msg
              then Pass
              else Fail ("expected eval error, got: " <> msg)
          Right _ -> Fail "expected error but got success",
      -- E2E with subprocess: nova-nix eval on a .nix file
      runTestM "e2e nova-nix eval subprocess" $ do
        tmpBase <- getTemporaryDirectory
        let tmpDir = tmpBase </> "nova-nix-test-e2e-sub"
            nixFile = tmpDir </> "test.nix"
        forceRemoveIfExists tmpDir
        createDirectoryIfMissing True tmpDir
        writeFile nixFile "1 + 2"
        -- Run nova-nix eval via Process
        (exitCode, stdoutStr, stderrStr) <-
          Proc.readCreateProcessWithExitCode
            (Proc.proc "cabal" ["run", "nova-nix", "--", "eval", nixFile])
            ""
        forceRemoveIfExists tmpDir
        pure $ case exitCode of
          ExitSuccess ->
            let nonEmpty = filter (not . T.null) (T.lines (T.pack stdoutStr))
                lastLine = case reverse nonEmpty of
                  (l : _) -> Just l
                  [] -> Nothing
             in if lastLine == Just "3"
                  then Pass
                  else Fail ("expected 3 as last line, got: " <> T.pack stdoutStr)
          ExitFailure code ->
            Fail ("nova-nix eval failed (" <> T.pack (show code) <> "): stderr=" <> T.pack stderrStr)
    ]

-- ---------------------------------------------------------------------------
-- Tests: Phase 4 - search paths, dynamic keys, directory import
-- ---------------------------------------------------------------------------

testPhase4 :: IO [Bool]
testPhase4 = do
  putStrLn "phase4/search-paths"
  sequence
    [ -- parseNixPath tests
      runTest "parseNixPath empty" $
        assertEqual "empty" [] (parseNixPath ""),
      runTest "parseNixPath single" $
        let result = parseNixPath "nixpkgs=/home/user/nixpkgs"
         in case result of
              [thunk] | Just (VAttrs m) <- readThunkValue thunk ->
                case (attrSetLookup "prefix" m >>= readThunkValue, attrSetLookup "path" m >>= readThunkValue) of
                  (Just (VStr "nixpkgs" _), Just (VStr "/home/user/nixpkgs" _)) -> Pass
                  _ -> Fail "wrong prefix/path"
              _ -> Fail ("expected one entry, got " <> T.pack (show (length result))),
      runTest "parseNixPath multiple" $
        assertEqual "count" 2 (length (parseNixPath "nixpkgs=/nix:custom=/opt")),
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
      runTest "splitNixPath keeps interior empty entries" $
        assertEqual "split-empty" ["a", "", "b"] (splitNixPath "a::b"),
      runTest "splitNixPath drops a trailing empty entry" $
        assertEqual "split-trail" ["a"] (splitNixPath "a:"),
      runTestM "splitNixPath long entry splits in linear time" $ do
        let entry = T.replicate 2000000 "p"
            split = splitNixPath ("first:" <> entry) == ["first", entry]
        outcome <- timeout walkWatchdogMicros (evaluate split)
        pure $ case outcome of
          Just True -> Pass
          Just False -> Fail "wrong split for a long entry"
          Nothing -> Fail "split did not return promptly",
      runTest "parseNixPath plain path" $
        let result = parseNixPath "/some/path"
         in case result of
              [thunk] | Just (VAttrs m) <- readThunkValue thunk ->
                case (attrSetLookup "prefix" m >>= readThunkValue, attrSetLookup "path" m >>= readThunkValue) of
                  (Just (VStr "" _), Just (VStr "/some/path" _)) -> Pass
                  _ -> Fail "wrong prefix/path for plain"
              _ -> Fail "expected one entry",
      -- ESearchPath desugars to __findFile __nixPath "name" during resolution
      runTest "parse <nixpkgs>" $
        assertParse "search path" "<nixpkgs>" (EApp (EApp (EVar "__findFile") (EVar "__nixPath")) (EStr [StrLit "nixpkgs"])),
      runTest "parse <nixpkgs/lib>" $
        assertParse "search path with subpath" "<nixpkgs/lib>" (EApp (EApp (EVar "__findFile") (EVar "__nixPath")) (EStr [StrLit "nixpkgs/lib"])),
      -- ESearchPath eval (should fail in pure mode since no search paths)
      runTest "eval <nixpkgs> fails without path" $
        assertEvalFail "search path not found" "<nixpkgs>",
      -- Dynamic attribute keys
      runTest "dynamic key basic" $
        assertEval "dynamic key" "{ ${\"hello\"} = 42; }.hello" (VInt 42),
      runTest "dynamic key from let" $
        assertEval "dynamic key let" "let name = \"x\"; in { ${name} = 1; }.x" (VInt 1),
      runTest "dynamic key in select" $
        assertEval "dynamic key select" "let s = { x = 10; }; in s.${\"x\"}" (VInt 10),
      runTest "dynamic key in hasAttr" $
        assertEval "dynamic key hasAttr" "let s = { x = 10; }; in s ? ${\"x\"}" (VBool True),
      runTest "dynamic key hasAttr missing" $
        assertEval "dynamic key hasAttr missing" "let s = { x = 10; }; in s ? ${\"y\"}" (VBool False),
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
        assertEval "dyn-rec-inherit" "let v = 42; in (rec { ${\"d\"} = 1; inherit v; }).v" (VInt 42),
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
          st <- newEvalState testDir
          let nixPaths = parseNixPath ("mypkg=" <> T.pack subDir)
              env = builtinEnv (esTimestamp st) nixPaths
          result <- runEvalIO st (eval env (EApp (EApp (EVar "__findFile") (EVar "__nixPath")) (EStr [StrLit "mypkg"])))
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
-- Symbol interning (C FFI)
-- ---------------------------------------------------------------------------

testSymbol :: IO [Bool]
testSymbol = do
  putStrLn "symbol"
  -- Symbol table is initialized by arenaInit in main bracket.
  sequence
    [ runTestM "intern returns non-zero" $ do
        sym <- symbolIntern "hello"
        pure (if unSymbol sym /= 0 then Pass else Fail "got symbol 0"),
      runTestM "intern same string returns same symbol" $ do
        sym1 <- symbolIntern "name"
        sym2 <- symbolIntern "name"
        pure (assertEqual "same symbol" sym1 sym2),
      runTestM "intern different strings returns different symbols" $ do
        sym1 <- symbolIntern "foo"
        sym2 <- symbolIntern "bar"
        pure (if sym1 /= sym2 then Pass else Fail "symbols should differ"),
      runTestM "symbolText round-trips" $ do
        sym <- symbolIntern "version"
        let txt = symbolText sym
        pure (assertEqual "text" "version" txt),
      runTestM "symbolLen correct" $ do
        sym <- symbolIntern "outputs"
        pure (assertEqual "len" 7 (symbolLen sym)),
      runTestM "empty string interns" $ do
        sym <- symbolIntern ""
        let txt = symbolText sym
        pure (assertEqual "empty" "" txt),
      runTestM "symbolCount tracks unique entries" $ do
        _ <- symbolIntern "alpha"
        _ <- symbolIntern "beta"
        _ <- symbolIntern "alpha"
        count <- symbolCount
        -- count includes all symbols interned in this bracket,
        -- so at least the ones from prior tests plus alpha + beta
        pure (if count >= 2 then Pass else Fail ("count too low: " <> T.pack (show count))),
      runTestM "many symbols (stress)" $ do
        let names = map (\i -> "pkg_" <> T.pack (show (i :: Int))) [1 .. 1000]
        syms <- mapM symbolIntern names
        -- All unique
        let unique = length (Set.fromList (map unSymbol syms))
        pure (assertEqual "1000 unique" 1000 unique)
    ]

-- ---------------------------------------------------------------------------
-- C attribute set (FFI)
-- ---------------------------------------------------------------------------

-- | Cast a StablePtr to CThunkPtr for CAttrSet tests.
-- CAttrSet stores void* - we use StablePtrs as opaque values in tests.
spToCPtr :: StablePtr a -> CThunkPtr
spToCPtr = castPtr . castStablePtrToPtr

-- | Cast a CThunkPtr back to StablePtr for CAttrSet test verification.
cptrToSp :: CThunkPtr -> StablePtr a
cptrToSp = castPtrToStablePtr . castPtr

testCAttrSet :: IO [Bool]
testCAttrSet = do
  putStrLn "cattrset"
  -- Symbol table is initialized by arenaInit in main bracket.
  sequence
    [ runTestM "new/free" $ do
        _set <- cattrsetNew 16
        -- set freed by arenaDestroy via nn_attrset_free_all
        pure Pass,
      runTestM "insert + freeze + lookup" $ do
        set <- cattrsetNew 4
        kName <- symbolIntern "name"
        kVer <- symbolIntern "version"
        valName <- newStablePtr ("hello" :: Text)
        valVer <- newStablePtr ("1.0" :: Text)
        cattrsetInsert set kName (spToCPtr valName)
        cattrsetInsert set kVer (spToCPtr valVer)
        cattrsetFreeze set
        result <- cattrsetLookup set kName
        case result of
          Nothing -> do
            -- set freed by arenaDestroy via nn_attrset_free_all
            freeStablePtr valName
            freeStablePtr valVer
            pure (Fail "lookup returned Nothing")
          Just cptr -> do
            val <- deRefStablePtr (cptrToSp cptr) :: IO Text
            -- set freed by arenaDestroy via nn_attrset_free_all
            freeStablePtr valName
            freeStablePtr valVer
            pure (assertEqual "lookup name" "hello" val),
      runTestM "lookup missing key returns Nothing" $ do
        set <- cattrsetNew 4
        kFoo <- symbolIntern "foo"
        kBar <- symbolIntern "bar"
        sp <- newStablePtr ("x" :: Text)
        cattrsetInsert set kFoo (spToCPtr sp)
        cattrsetFreeze set
        result <- cattrsetLookup set kBar
        -- set freed by arenaDestroy via nn_attrset_free_all
        freeStablePtr sp
        pure (case result of Nothing -> Pass; Just _ -> Fail "expected Nothing"),
      runTestM "size after freeze" $ do
        set <- cattrsetNew 4
        k1 <- symbolIntern "a"
        k2 <- symbolIntern "b"
        k3 <- symbolIntern "c"
        sp <- newStablePtr (42 :: Int)
        cattrsetInsert set k1 (spToCPtr sp)
        cattrsetInsert set k2 (spToCPtr sp)
        cattrsetInsert set k3 (spToCPtr sp)
        cattrsetFreeze set
        n <- cattrsetSize set
        -- set freed by arenaDestroy via nn_attrset_free_all
        freeStablePtr sp
        pure (assertEqual "size" 3 n),
      runTestM "duplicate keys: last writer wins" $ do
        set <- cattrsetNew 4
        kName <- symbolIntern "name"
        sp1 <- newStablePtr ("first" :: Text)
        sp2 <- newStablePtr ("second" :: Text)
        cattrsetInsert set kName (spToCPtr sp1)
        cattrsetInsert set kName (spToCPtr sp2)
        cattrsetFreeze set
        n <- cattrsetSize set
        result <- cattrsetLookup set kName
        val <- case result of
          Nothing -> pure "MISSING"
          Just cptr -> deRefStablePtr (cptrToSp cptr)
        -- set freed by arenaDestroy via nn_attrset_free_all
        freeStablePtr sp1
        freeStablePtr sp2
        pure
          ( if n == 1 && val == ("second" :: Text)
              then Pass
              else Fail ("size=" <> T.pack (show n) <> " val=" <> val)
          ),
      runTestM "keys returned sorted" $ do
        set <- cattrsetNew 8
        -- Insert in reverse order; after freeze keys should be sorted by symbol ID
        k1 <- symbolIntern "zzz"
        k2 <- symbolIntern "aaa"
        k3 <- symbolIntern "mmm"
        sp <- newStablePtr (0 :: Int)
        cattrsetInsert set k1 (spToCPtr sp)
        cattrsetInsert set k2 (spToCPtr sp)
        cattrsetInsert set k3 (spToCPtr sp)
        cattrsetFreeze set
        keys <- cattrsetKeys set
        -- set freed by arenaDestroy via nn_attrset_free_all
        freeStablePtr sp
        -- Keys should be sorted by symbol ID (ascending)
        let ids = map unSymbol keys
            sorted = ids == foldl (\acc x -> acc ++ [x]) [] (Set.toAscList (Set.fromList ids))
        pure (if sorted then Pass else Fail ("unsorted: " <> T.pack (show ids))),
      runTestM "union right-biased" $ do
        setA <- cattrsetNew 4
        setB <- cattrsetNew 4
        kX <- symbolIntern "x"
        kY <- symbolIntern "y"
        kZ <- symbolIntern "z"
        spA <- newStablePtr ("fromA" :: Text)
        spB <- newStablePtr ("fromB" :: Text)
        spZ <- newStablePtr ("onlyA" :: Text)
        cattrsetInsert setA kX (spToCPtr spA)
        cattrsetInsert setA kZ (spToCPtr spZ)
        cattrsetInsert setB kX (spToCPtr spB)
        cattrsetInsert setB kY (spToCPtr spB)
        cattrsetFreeze setA
        cattrsetFreeze setB
        merged <- cattrsetUnion setA setB
        n <- cattrsetSize merged
        resultX <- cattrsetLookup merged kX
        valX <- case resultX of
          Nothing -> pure "MISSING"
          Just cptr -> deRefStablePtr (cptrToSp cptr)
        -- sets freed by arenaDestroy via nn_attrset_free_all
        freeStablePtr spA
        freeStablePtr spB
        freeStablePtr spZ
        pure
          ( if n == 3 && valX == ("fromB" :: Text)
              then Pass
              else Fail ("size=" <> T.pack (show n) <> " x=" <> valX)
          ),
      runTestM "stress: 10k entries" $ do
        set <- cattrsetNew 1024
        sp <- newStablePtr (0 :: Int)
        syms <- mapM (\i -> symbolIntern ("key_" <> T.pack (show (i :: Int)))) [1 .. 10000]
        mapM_ (\sym -> cattrsetInsert set sym (spToCPtr sp)) syms
        cattrsetFreeze set
        n <- cattrsetSize set
        -- Spot-check a lookup from the middle of the key range
        hit <- case drop 5000 syms of
          middleSym : _ -> cattrsetLookup set middleSym
          [] -> pure Nothing
        -- set freed by arenaDestroy via nn_attrset_free_all
        freeStablePtr sp
        pure
          ( if n == 10000 && isJust hit
              then Pass
              else Fail ("size=" <> T.pack (show n))
          )
    ]

-- ---------------------------------------------------------------------------
-- C thunk arena (FFI)
-- ---------------------------------------------------------------------------

testCThunk :: IO [Bool]
testCThunk = do
  putStrLn "cthunk"
  -- Arena is already initialized by main's bracket.
  -- Each test uses the shared arena (thunks accumulate - that's fine).
  sequence
    [ runTestM "new pending + state" $ do
        sp <- newStablePtr ("pending" :: Text)
        ptr <- cthunkNewBc 0 (castStablePtrToPtr sp)
        state <- cthunkState ptr
        pure (assertEqual "state" 0 state),
      runTestM "new computed + state" $ do
        sp <- newStablePtr ("computed" :: Text)
        ptr <- cthunkNewComputed (castStablePtrToPtr sp)
        state <- cthunkState ptr
        pure (assertEqual "state" 1 state),
      runTestM "payload round-trips (pending)" $ do
        sp <- newStablePtr ("hello" :: Text)
        ptr <- cthunkNewBc 0 (castStablePtrToPtr sp)
        payload <- cthunkPayload ptr
        val <- deRefStablePtr (castPtrToStablePtr payload) :: IO Text
        pure (assertEqual "payload" "hello" val),
      runTestM "payload round-trips (computed)" $ do
        sp <- newStablePtr (42 :: Int)
        ptr <- cthunkNewComputed (castStablePtrToPtr sp)
        payload <- cthunkPayload ptr
        val <- deRefStablePtr (castPtrToStablePtr payload) :: IO Int
        pure (assertEqual "payload" 42 val),
      runTestM "mark_blackhole succeeds on pending" $ do
        sp <- newStablePtr ("x" :: Text)
        ptr <- cthunkNewBc 0 (castStablePtrToPtr sp)
        ok <- cthunkMarkBlackhole ptr
        state <- cthunkState ptr
        pure
          ( if ok && state == 2
              then Pass
              else Fail ("ok=" <> T.pack (show ok) <> " state=" <> T.pack (show state))
          ),
      runTestM "mark_blackhole fails on computed" $ do
        sp <- newStablePtr ("x" :: Text)
        ptr <- cthunkNewComputed (castStablePtrToPtr sp)
        ok <- cthunkMarkBlackhole ptr
        pure (if not ok then Pass else Fail "should have failed"),
      runTestM "mark_blackhole fails on blackhole" $ do
        sp <- newStablePtr ("x" :: Text)
        ptr <- cthunkNewBc 0 (castStablePtrToPtr sp)
        _ <- cthunkMarkBlackhole ptr
        ok <- cthunkMarkBlackhole ptr
        pure (if not ok then Pass else Fail "should have failed"),
      runTestM "set_computed returns old payload" $ do
        pendingSp <- newStablePtr ("old" :: Text)
        ptr <- cthunkNewBc 0 (castStablePtrToPtr pendingSp)
        _ <- cthunkMarkBlackhole ptr
        computedSp <- newStablePtr ("new" :: Text)
        oldPayload <- cthunkSetComputed ptr (castStablePtrToPtr computedSp)
        oldVal <- deRefStablePtr (castPtrToStablePtr oldPayload) :: IO Text
        state <- cthunkState ptr
        newPayload <- cthunkPayload ptr
        newVal <- deRefStablePtr (castPtrToStablePtr newPayload) :: IO Text
        freeStablePtr pendingSp
        pure
          ( if oldVal == "old" && newVal == "new" && state == 1
              then Pass
              else
                Fail
                  ( "old="
                      <> oldVal
                      <> " new="
                      <> newVal
                      <> " state="
                      <> T.pack (show state)
                  )
          ),
      runTestM "set_computed on pending returns old payload" $ do
        sp <- newStablePtr ("x" :: Text)
        ptr <- cthunkNewBc 0 (castStablePtrToPtr sp)
        valSp <- newStablePtr ("v" :: Text)
        oldPayload <- cthunkSetComputed ptr (castStablePtrToPtr valSp)
        -- set_computed accepts PENDING (direct memoization, no blackhole step)
        oldVal <- deRefStablePtr (castPtrToStablePtr oldPayload) :: IO Text
        freeStablePtr sp
        pure (assertEqual "old payload" "x" oldVal),
      runTestM "count tracks allocations" $ do
        countBefore <- cthunkCount
        sp <- newStablePtr (0 :: Int)
        _ <- cthunkNewBc 0 (castStablePtrToPtr sp)
        _ <- cthunkNewBc 0 (castStablePtrToPtr sp)
        _ <- cthunkNewComputed (castStablePtrToPtr sp)
        countAfter <- cthunkCount
        let delta = countAfter - countBefore
        pure (assertEqual "delta" 3 delta),
      runTestM "get retrieves by index" $ do
        countBefore <- cthunkCount
        sp <- newStablePtr ("indexed" :: Text)
        ptr <- cthunkNewBc 0 (castStablePtrToPtr sp)
        retrieved <- cthunkGet countBefore
        stateOrig <- cthunkState ptr
        stateRetrieved <- cthunkState retrieved
        pure
          ( if ptr == retrieved && stateOrig == stateRetrieved
              then Pass
              else Fail "get returned wrong pointer"
          ),
      runTestM "stress: 100k thunks" $ do
        countBefore <- cthunkCount
        sp <- newStablePtr (0 :: Int)
        mapM_ (\_ -> cthunkNewBc 0 (castStablePtrToPtr sp)) [(1 :: Int) .. 100000]
        countAfter <- cthunkCount
        let delta = countAfter - countBefore
        -- Spot-check: retrieve one from the middle
        midPtr <- cthunkGet (countBefore + 50000)
        midState <- cthunkState midPtr
        pure
          ( if delta == 100000 && midState == 0
              then Pass
              else
                Fail
                  ( "delta="
                      <> T.pack (show delta)
                      <> " midState="
                      <> T.pack (show midState)
                  )
          )
    ]

-- ---------------------------------------------------------------------------
-- Bytecode compilation tests
-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- Tests: Class I conformance follow-ups (issue #50)
-- ---------------------------------------------------------------------------

-- | Error kind for 'StubStoreEval', mirroring PureEval's split: only a
-- throw/assert is tryEval-catchable.
data StubErr = StubThrow !Text | StubOther !Text

-- | Pure evaluator over a stubbed store: 'readStoreDerivation' serves
-- derivations from a fixed map (keyed by canonical @.drv@ path text);
-- everything else behaves like 'PureEval'.  Exercises the
-- input-derivation-modulo STORE-READ arm hermetically - the real arm
-- reads the platform store, which dev machines and CI runners must not
-- depend on (the build matrix has no writable @/nix/store@).
newtype StubStoreEval a = StubStoreEval (Map.Map Text Derivation -> Either StubErr a)

runStubStoreEval :: Map.Map Text Derivation -> StubStoreEval a -> Either Text a
runStubStoreEval drvs (StubStoreEval action) = case action drvs of
  Left (StubThrow msg) -> Left msg
  Left (StubOther msg) -> Left msg
  Right val -> Right val

instance Functor StubStoreEval where
  fmap f (StubStoreEval g) = StubStoreEval (fmap f . g)

instance Applicative StubStoreEval where
  pure val = StubStoreEval (const (Right val))
  StubStoreEval mf <*> StubStoreEval ma = StubStoreEval $ \drvs -> mf drvs <*> ma drvs

instance Monad StubStoreEval where
  StubStoreEval ma >>= f = StubStoreEval $ \drvs -> case ma drvs of
    Left err -> Left err
    Right val -> let StubStoreEval mb = f val in mb drvs

instance MonadEval StubStoreEval where
  throwEvalError msg = StubStoreEval (const (Left (StubOther msg)))
  throwCatchableError msg = StubStoreEval (const (Left (StubThrow msg)))
  abortEvaluation msg = StubStoreEval (const (Left (StubOther ("evaluation aborted: " <> msg))))
  catchEvalError (StubStoreEval action) = StubStoreEval $ \drvs -> case action drvs of
    Left (StubThrow msg) -> Right (Left msg)
    Left other -> Left other
    Right val -> Right (Right val)
  doesPathExist _ = pure False
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
  copyPathToStore _ _ _ = throwEvalError "builtins.path: not available in the stub evaluator"
  narHashOfPath _ = throwEvalError "builtins.fetchGit: not available in the stub evaluator"
  isExecutableFile _ = throwEvalError "builtins.path: not available in the stub evaluator"
  readSymlinkTarget _ = throwEvalError "builtins.path: not available in the stub evaluator"
  addSourceNar _ _ = throwEvalError "builtins.path: not available in the stub evaluator"
  addFixedOutputFile _ _ = throwEvalError "builtins.fetchurl: not available in the stub evaluator"
  traceMessage _ = pure ()
  lookupDrvHash _ = pure Nothing
  cacheDrvHash _ _ = pure ()
  recordDrvAterm _ _ = pure ()
  readStoreDerivation sp =
    StubStoreEval $ \drvs -> Right (Map.lookup (storePathToText defaultStoreDir sp) drvs)
  lookupSessionDrv _ = pure Nothing
  storeSourcePath = pure
  resolvePathLiteral = pure . canonPath
  forceThunk evalFn thunk@(Thunk ptr) = case readThunkValue thunk of
    Just val -> pure val
    Nothing -> case unsafePerformIO (cthunkState ptr) of
      2 {- BLACKHOLE -} -> abortEvaluation "infinite recursion encountered"
      _ {- PENDING -} ->
        let bcIdx = unsafePerformIO (cthunkGetBcIdx ptr)
            envSp = unsafePerformIO (cthunkPayload ptr)
            env = unsafePerformIO (deRefStablePtr (castPtrToStablePtr envSp))
         in evalFn env bcIdx

-- | Parse and evaluate under the stubbed-store evaluator.
evalNixStub :: Map.Map Text Derivation -> Text -> Either Text NixValue
evalNixStub drvs source = case parseNix "<test>" source of
  Left err -> Left (T.pack (show err))
  Right expr -> runStubStoreEval drvs (eval (builtinEnv 0 []) expr)

-- | Class I conformance follow-ups (issue #50): behaviors that landed
-- with #37 but had no direct test.  Pure cases here; filesystem-touching
-- cases in 'testClassIFollowupsIO'.
testClassIFollowups :: IO [Bool]
testClassIFollowups = do
  putStrLn "eval/class-i-followups"
  let storeA = "/nix/store/" <> T.replicate 32 "a"
      enclosingCtx = StringContext (Set.singleton (SCPlain (StorePath (T.replicate 32 "a") "x")))
  sequence
    [ -- path + path concatenates the spellings and canonicalizes
      runTest "path + path canonicalizes the joined spelling" $
        assertEval "pp-canon" "builtins.toString (/a/b + /c/../d)" (mkStr "/a/b/d"),
      -- concatStringsSep coerces a path element; an in-store path takes
      -- the short-circuit (itself + enclosing context, no store copy)
      runTest "concatStringsSep coerces an in-store path element" $
        assertEval
          "csep-path"
          ("builtins.concatStringsSep \":\" [ " <> storeA <> "-x \"y\" ]")
          (VStr (TE.encodeUtf8 (storeA <> "-x:y")) enclosingCtx),
      -- the same in-store short-circuit for general interpolation, on a
      -- SUBPATH: the text is the subpath, the context the enclosing path
      runTest "interpolating an in-store subpath keeps text, contexts the enclosing path" $
        assertEval
          "interp-substore"
          ("\"${" <> storeA <> "-x/sub}\"")
          (VStr (TE.encodeUtf8 (storeA <> "-x/sub")) enclosingCtx),
      -- storePath on a subpath: value text is the subpath itself
      runTest "storePath on a subpath keeps text, contexts the enclosing path" $
        assertEval
          "storepath-sub"
          ("builtins.storePath " <> storeA <> "-x/sub/file")
          (VStr (TE.encodeUtf8 (storeA <> "-x/sub/file")) enclosingCtx),
      -- fromJSON integers in the (int64 max, uint64 max] band arrive
      -- two's-complement wrapped, as upstream's nlohmann handoff
      runTest "fromJSON wraps int64 max + 1" $
        assertEval "fromjson-wrap-min" "builtins.fromJSON \"9223372036854775808\"" (VInt minBound),
      runTest "fromJSON wraps uint64 max" $
        assertEval "fromjson-wrap-neg1" "builtins.fromJSON \"18446744073709551615\"" (VInt (-1)),
      runTest "fromJSON falls to float past uint64" $
        assertEval "fromjson-float" "builtins.fromJSON \"18446744073709551616\"" (VFloat 18446744073709551616.0),
      -- the input-derivation-modulo STORE-READ arm: a dependent whose
      -- input .drv was NOT evaluated in-session resolves by reading the
      -- (stubbed) store, and lands on the same drvPath as the in-session
      -- cache-hit arm computes for the identical derivation
      runTestM "input modulo recurses through the store read (stub store)" $ do
        let depSrc = "derivation { name = \"dep\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; }"
        depDrv <- case evalNix ("(" <> depSrc <> ")._derivation") of
          Right (VDerivation d) -> pure d
          other -> fail ("dep._derivation: " <> show other)
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
        tmpBase <- getTemporaryDirectory
        ioResult <-
          evalNixIO tmpBase $
            T.concat
              [ "let dep = ",
                depSrc,
                "; in (derivation { name = \"main\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; ",
                "input = builtins.appendContext \"x\" { \"${dep.drvPath}\" = { outputs = [\"out\"]; }; }; }).drvPath"
              ]
        pure $ case (stubResult, ioResult) of
          (Right (VStr a _), Right (VStr b _))
            | a == b -> Pass
            | otherwise ->
                Fail ("store-read and in-session arms disagree: " <> bytesText a <> " vs " <> bytesText b)
          other -> Fail ("expected two drvPaths, got: " <> T.pack (show other))
    ]

-- | Filesystem-touching Class I follow-ups (issue #50).
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
        -- base name to copy under (toPath keeps the probe off the
        -- platform filesystem - the error fires before any read)
        runTestM "coercing the filesystem root errors (no base name)" $ do
          result <- evalNixIO testDir "\"${builtins.toPath \"/\"}\""
          pure $ case result of
            Left err | "no base name" `T.isInfixOf` err -> Pass
            Left err -> Fail ("expected a base-name error, got: " <> err)
            Right val -> Fail ("expected failure, got " <> T.pack (show val))
      ]

-- | Bytecode short_arg spill: op-level payload counts at or above the
-- 0xFFFF sentinel move to the first data word, so literals are no longer
-- capped at 65535 elements.  The two exact-boundary list cases pin the
-- inline maximum (65534) and the first spilled count (65535); the rest
-- drive each counted-op kind (list, attrs, let, string parts, attr path)
-- well past the old ceiling.
testBytecodeCountSpill :: IO [Bool]
testBytecodeCountSpill = do
  putStrLn "bytecode/count-spill"
  let listOf n = "builtins.length [ " <> T.unwords (replicate n "1") <> " ]"
      bigAttrsBody = T.concat [T.pack ("a" <> show i <> " = " <> show i <> "; ") | i <- [0 :: Int .. 69999]]
  sequence
    [ runTest "list literal at the inline count maximum (65534)" $
        assertEval "spill-list-inline-max" (listOf 65534) (VInt 65534),
      runTest "list literal at the first spilled count (65535)" $
        assertEval "spill-list-first-spill" (listOf 65535) (VInt 65535),
      runTest "spilled list preserves element order" $
        assertEval
          "spill-list-order"
          ("builtins.elemAt [ " <> T.unwords (map (T.pack . show) [0 :: Int .. 69999]) <> " ] 69999")
          (VInt 69999),
      runTest "spilled attrset literal binds every attribute" $
        assertEval
          "spill-attrs-count"
          ("builtins.length (builtins.attrNames { " <> bigAttrsBody <> "})")
          (VInt 70000),
      runTest "spilled attrset lookup reads the right value" $
        assertEval
          "spill-attrs-lookup"
          ("{ " <> bigAttrsBody <> "}.a69999")
          (VInt 69999),
      runTest "spilled let binds every name" $
        assertEval
          "spill-let"
          ("let " <> T.concat [T.pack ("v" <> show i <> " = " <> show i <> "; ") | i <- [0 :: Int .. 69999]] <> "in v69999")
          (VInt 69999),
      runTest "spilled interpolated string keeps every part" $
        assertEval
          "spill-string-parts"
          ("builtins.stringLength \"" <> T.concat (replicate 70000 "${\"x\"}") <> "\"")
          (VInt 70000),
      runTest "spilled attr path walks (set ? long.path)" $
        assertEval
          "spill-attrpath"
          ("{ } ? " <> T.intercalate "." (map (\i -> T.pack ("p" <> show i)) [0 :: Int .. 69999]))
          (VBool False)
    ]

-- | C value structs carry element counts as a full uint32; a narrower
-- count would wrap at 65536 and size the C array smaller than the fill
-- loop that follows it.  Both cases marshal counts past that boundary
-- and force the value twice - the first force writes the C struct (the
-- marshal side), the second reads it back off the computed thunk cell
-- (the unmarshal side).
testValueCountWidths :: IO [Bool]
testValueCountWidths = do
  putStrLn "value/count-widths"
  let formalsBody = T.intercalate ", " [T.pack ("a" <> show i) | i <- [0 :: Int .. 69999]]
  sequence
    [ runTest "lambda with 70000 formals round-trips through the C closure" $
        assertEval
          "width-lambda-formals"
          ( "let f = { "
              <> formalsBody
              <> " }: 1; in builtins.seq (builtins.functionArgs f) (builtins.length (builtins.attrNames (builtins.functionArgs f)))"
          )
          (VInt 70000),
      runTest "string context with 70000 elements round-trips through the C thunk" $
        assertEval
          "width-ctxstr-elems"
          ( "let s = builtins.appendContext \"x\" (builtins.listToAttrs (builtins.genList (i: { "
              <> "name = \"/nix/store/00000000000000000000000000000000-p\" + builtins.toString i; "
              <> "value = { path = true; }; }) 70000)); "
              <> "in builtins.seq s (builtins.length (builtins.attrNames (builtins.getContext s)))"
          )
          (VInt 70000)
    ]

-- ---------------------------------------------------------------------------
-- Tests: Class B name validation at write sinks (issue #39)
-- ---------------------------------------------------------------------------

-- | Class B (issue #39): the store-path name rules hold at path
-- CONSTRUCTION, not only at parse.  Derivation names, output names, and
-- fetchurl basenames reject exactly what the parse boundary rejects,
-- before any write path is built from them - and names the old ad hoc
-- sink checks over-rejected (an interior @..@) are valid again.
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
        failsWith "name-empty" (drvWith "\"\"") "the name is empty",
      runTest "212-character derivation name is rejected" $
        failsWith "name-long" (drvWith ("\"" <> T.replicate 212 "a" <> "\"")) "above the 211 maximum",
      runTest "derivation name '..' is rejected" $
        failsWith "name-dotdot" (drvWith "\"..\"") "dash-separated component",
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
      -- Both fields clean on their own, only the COMPOSED drvName-output
      -- crosses the length cap: the construction-side check catches what
      -- the field-level checks cannot.
      runTest "composed name-output past the length cap is rejected" $
        failsWith
          "composed-long"
          ( "(derivation { name = \""
              <> T.replicate 208 "a"
              <> "\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; outputs = [ \"dev\" ]; }).drvPath"
          )
          "invalid store path name",
      -- fetchurl derives the store name from the URL alone, so a bad
      -- basename fails BEFORE the download: under PureEval a reachable
      -- download would surface as "runProcess: not available" instead.
      runTest "fetchurl rejects a backslash basename before fetching" $
        failsWith "fetchurl-backslash" "builtins.fetchurl \"http://e/a\\\\b\"" "invalid store path name",
      runTest "fetchurl rejects a dot-segment basename before fetching" $
        failsWith "fetchurl-dotdot" "builtins.fetchurl \"http://e/..\"" "invalid store path name"
    ]

-- | The NAR entry-name check: rejects what escapes the tree and what the
-- Win32 path layer silently rewrites (stream colons, device stems,
-- trailing dot or space); ordinary names pass, including ones Windows
-- merely refuses loudly.
testNarNameSafety :: IO [Bool]
testNarNameSafety = do
  putStrLn "store/nar-name-safety"
  let rejects name = if isSafeNarName name then Fail ("expected rejection: " <> T.pack (show name)) else Pass
      accepts name = if isSafeNarName name then Pass else Fail ("expected acceptance: " <> T.pack (show name))
      allOf = foldr keepFirstFail Pass
      keepFirstFail Pass acc = acc
      keepFirstFail failure _ = failure
  sequence
    [ runTest "empty and dot names are rejected" $
        allOf [rejects "", rejects ".", rejects ".."],
      runTest "separators are rejected" $
        allOf [rejects "a/b", rejects "a\\b"],
      runTest "an embedded NUL is rejected" $
        rejects "a\NULb",
      runTest "colons are rejected (drive prefix and stream form)" $
        allOf [rejects "C:evil", rejects "a:b", rejects ":"],
      runTest "reserved device stems are rejected case-insensitively" $
        allOf [rejects "NUL", rejects "nul.txt", rejects "CON", rejects "com3", rejects "COM0.log", rejects "lpt9"],
      runTest "a space-padded device stem is rejected" $
        rejects "Nul .txt",
      runTest "a superscript-digit device stem is rejected" $
        rejects "COM\185",
      runTest "a trailing dot or space is rejected" $
        allOf [rejects "x.", rejects "x "],
      runTest "ordinary names pass" $
        allOf [accepts "a", accepts ".hidden", accepts "a.b", accepts "a b", accepts " a"],
      runTest "near-miss device names pass" $
        allOf [accepts "com10", accepts "COM", accepts "NULx", accepts "xNUL.txt"],
      runTest "a loud-refuse character stays allowed" $
        accepts "a\"b"
    ]

-- | IO-evaluator write sinks reject an invalid name BEFORE any write:
-- every case here errors out against a scratch dir with no store traffic.
testStoreNameSinksIO :: IO [Bool]
testStoreNameSinksIO = do
  putStrLn "store/name-sinks-io"
  tmpBase <- getTemporaryDirectory
  let testDir = tmpBase </> "nova-nix-test-name-sinks"
      needleTest label source needle = do
        result <- evalNixIO testDir source
        runTest label $ case result of
          Left err
            | needle `T.isInfixOf` err -> Pass
            | otherwise -> Fail (label <> ": wrong error: " <> err)
          Right val -> Fail (label <> ": expected an eval error, got " <> T.pack (show val))
  bracket_
    ( do
        createDirectoryIfMissing True (testDir </> "tree")
        TIO.writeFile (testDir </> "tree" </> "f.txt") "payload\n"
    )
    ( do
        exists <- doesDirectoryExist testDir
        when exists (removeDirectoryRecursive testDir)
    )
    $ sequence
      [ needleTest "toFile rejects a stream-colon name" "builtins.toFile \"a:b\" \"x\"" "invalid store path name",
        needleTest "toFile rejects a dot-segment name" "builtins.toFile \"..\" \"x\"" "invalid store path name",
        needleTest
          "path rejects a traversal name override"
          "builtins.path { path = ./tree; name = \"../../evil\"; }"
          "invalid store path name",
        needleTest
          "path with a filter rejects a drive-prefixed name"
          "builtins.path { path = ./tree; name = \"C:evil\"; filter = (_: _: true); }"
          "invalid store path name",
        needleTest
          "source coercion rejects a basename outside the name rules"
          "\"${./. + \"/sp ace\"}\""
          "invalid store path name"
      ]

testBytecodeCompile :: IO [Bool]
testBytecodeCompile = do
  putStrLn "bytecode"
  -- Arena (including bytecode store) already initialized by main's bracket.
  sequence
    [ runTestM "compile ELit NixInt" $ do
        idx <- compileExpr (ELit (NixInt 42))
        op <- cbcOpcode idx
        a1 <- cbcArg1 idx
        a2 <- cbcArg2 idx
        pure
          ( if op == OpLitInt && a1 == 42 && a2 == 0
              then Pass
              else Fail ("op=" <> T.pack (show op) <> " a1=" <> T.pack (show a1))
          ),
      runTestM "compile ELit NixInt negative" $ do
        idx <- compileExpr (ELit (NixInt (-1)))
        op <- cbcOpcode idx
        a1 <- cbcArg1 idx
        a2 <- cbcArg2 idx
        -- -1 as uint64 = 0xFFFFFFFFFFFFFFFF, lo=0xFFFFFFFF, hi=0xFFFFFFFF
        pure
          ( if op == OpLitInt && a1 == 0xFFFFFFFF && a2 == 0xFFFFFFFF
              then Pass
              else
                Fail
                  ( "a1="
                      <> T.pack (show a1)
                      <> " a2="
                      <> T.pack (show a2)
                  )
          ),
      runTestM "compile ELit NixBool" $ do
        idxT <- compileExpr (ELit (NixBool True))
        idxF <- compileExpr (ELit (NixBool False))
        opT <- cbcOpcode idxT
        opF <- cbcOpcode idxF
        saT <- cbcShortArg idxT
        saF <- cbcShortArg idxF
        pure
          ( if opT == OpLitBool && saT == 1 && opF == OpLitBool && saF == 0
              then Pass
              else Fail "bool encoding mismatch"
          ),
      runTestM "compile ELit NixNull" $ do
        idx <- compileExpr (ELit NixNull)
        op <- cbcOpcode idx
        pure (assertEqual "opcode" OpLitNull op),
      runTestM "compile EResolvedVar" $ do
        idx <- compileExpr (EResolvedVar 3 7)
        op <- cbcOpcode idx
        a1 <- cbcArg1 idx
        a2 <- cbcArg2 idx
        pure
          ( if op == OpResolvedVar && a1 == 3 && a2 == 7
              then Pass
              else
                Fail
                  ( "op="
                      <> T.pack (show op)
                      <> " a1="
                      <> T.pack (show a1)
                      <> " a2="
                      <> T.pack (show a2)
                  )
          ),
      runTestM "compile EVar" $ do
        idx <- compileExpr (EVar "hello")
        op <- cbcOpcode idx
        sym <- cbcArg1 idx
        let symText = symbolText (Symbol sym)
        pure
          ( if op == OpVar && symText == "hello"
              then Pass
              else Fail ("op=" <> T.pack (show op) <> " sym=" <> symText)
          ),
      runTestM "compile EApp" $ do
        idx <- compileExpr (EApp (EVar "f") (ELit (NixInt 1)))
        op <- cbcOpcode idx
        funcIdx <- cbcArg1 idx
        argIdx <- cbcArg2 idx
        funcOp <- cbcOpcode funcIdx
        argOp <- cbcOpcode argIdx
        pure
          ( if op == OpApp && funcOp == OpVar && argOp == OpLitInt
              then Pass
              else
                Fail
                  ( "op="
                      <> T.pack (show op)
                      <> " funcOp="
                      <> T.pack (show funcOp)
                      <> " argOp="
                      <> T.pack (show argOp)
                  )
          ),
      runTestM "compile EIf" $ do
        idx <-
          compileExpr
            (EIf (ELit (NixBool True)) (ELit (NixInt 1)) (ELit (NixInt 2)))
        op <- cbcOpcode idx
        condIdx <- cbcArg1 idx
        thenIdx <- cbcArg2 idx
        elseIdx <- cbcArg3 idx
        condOp <- cbcOpcode condIdx
        thenA1 <- cbcArg1 thenIdx
        elseA1 <- cbcArg1 elseIdx
        pure
          ( if op == OpIf && condOp == OpLitBool && thenA1 == 1 && elseA1 == 2
              then Pass
              else Fail "if structure mismatch"
          ),
      runTestM "compile EBinary" $ do
        idx <-
          compileExpr
            (EBinary OpAdd (ELit (NixInt 10)) (ELit (NixInt 20)))
        op <- cbcOpcode idx
        fl <- cbcFlags idx
        leftIdx <- cbcArg1 idx
        rightIdx <- cbcArg2 idx
        leftA1 <- cbcArg1 leftIdx
        rightA1 <- cbcArg1 rightIdx
        pure
          ( if op == OpBinary && fl == binaryAdd && leftA1 == 10 && rightA1 == 20
              then Pass
              else Fail "binary structure mismatch"
          ),
      runTestM "compile EUnary" $ do
        idx <- compileExpr (EUnary OpNegate (ELit (NixInt 5)))
        op <- cbcOpcode idx
        fl <- cbcFlags idx
        operandIdx <- cbcArg1 idx
        operandA1 <- cbcArg1 operandIdx
        pure
          ( if op == OpUnary && fl == unaryNegate && operandA1 == 5
              then Pass
              else Fail "unary structure mismatch"
          ),
      runTestM "compile EList" $ do
        idx <-
          compileExpr
            (EList [ELit (NixInt 1), ELit (NixInt 2), ELit (NixInt 3)])
        op <- cbcOpcode idx
        count <- cbcShortArg idx
        dataOff <- cbcArg1 idx
        c0 <- cbcData dataOff
        c1 <- cbcData (dataOff + 1)
        c2 <- cbcData (dataOff + 2)
        c0a1 <- cbcArg1 c0
        c1a1 <- cbcArg1 c1
        c2a1 <- cbcArg1 c2
        pure
          ( if op == OpList && count == 3 && c0a1 == 1 && c1a1 == 2 && c2a1 == 3
              then Pass
              else
                Fail
                  ( "op="
                      <> T.pack (show op)
                      <> " count="
                      <> T.pack (show count)
                  )
          ),
      runTestM "compile EStr with interpolation" $ do
        idx <-
          compileExpr
            (EStr [StrLit "hello ", StrInterp (EVar "name"), StrLit "!"])
        op <- cbcOpcode idx
        count <- cbcShortArg idx
        dataOff <- cbcArg1 idx
        -- 3 parts x 2 words each = 6 data words
        tag0 <- cbcData dataOff
        _val0 <- cbcData (dataOff + 1)
        tag1 <- cbcData (dataOff + 2)
        val1 <- cbcData (dataOff + 3)
        tag2 <- cbcData (dataOff + 4)
        -- tag0=0(lit), tag1=1(interp), tag2=0(lit)
        interpOp <- cbcOpcode val1
        pure
          ( if op == OpStr
              && count == 3
              && tag0 == strpartLit
              && tag1 == strpartInterp
              && tag2 == strpartLit
              && interpOp == OpVar
              then Pass
              else
                Fail
                  ( "op="
                      <> T.pack (show op)
                      <> " count="
                      <> T.pack (show count)
                      <> " tag0="
                      <> T.pack (show tag0)
                      <> " tag1="
                      <> T.pack (show tag1)
                  )
          ),
      runTestM "compile EWith" $ do
        idx <- compileExpr (EWith (EVar "lib") (EVar "x"))
        op <- cbcOpcode idx
        scopeIdx <- cbcArg1 idx
        bodyIdx <- cbcArg2 idx
        scopeOp <- cbcOpcode scopeIdx
        bodyOp <- cbcOpcode bodyIdx
        pure
          ( if op == OpWith && scopeOp == OpVar && bodyOp == OpVar
              then Pass
              else Fail "with structure mismatch"
          ),
      runTestM "compile EAssert" $ do
        idx <- compileExpr (EAssert (ELit (NixBool True)) (ELit (NixInt 1)))
        op <- cbcOpcode idx
        condIdx <- cbcArg1 idx
        bodyIdx <- cbcArg2 idx
        condOp <- cbcOpcode condIdx
        bodyOp <- cbcOpcode bodyIdx
        pure
          ( if op == OpAssert && condOp == OpLitBool && bodyOp == OpLitInt
              then Pass
              else Fail "assert structure mismatch"
          ),
      runTestM "compile ELambda (FormalName)" $ do
        idx <-
          compileExpr
            (ELambda (FormalName "x") (EResolvedVar 0 0) NoCaptureInfo)
        op <- cbcOpcode idx
        fl <- cbcFlags idx
        bodyIdx <- cbcArg2 idx
        bodyOp <- cbcOpcode bodyIdx
        pure
          ( if op == OpLambda && fl == formalName && bodyOp == OpResolvedVar
              then Pass
              else
                Fail
                  ( "op="
                      <> T.pack (show op)
                      <> " flags="
                      <> T.pack (show fl)
                  )
          ),
      runTestM "compile ELet" $ do
        idx <-
          compileExpr
            ( ELet
                [NamedBinding [StaticKey "x"] (ELit (NixInt 42))]
                (EResolvedVar 0 0)
                NoCaptureInfo
            )
        op <- cbcOpcode idx
        count <- cbcShortArg idx
        bodyIdx <- cbcArg2 idx
        bodyOp <- cbcOpcode bodyIdx
        pure
          ( if op == OpLet && count == 1 && bodyOp == OpResolvedVar
              then Pass
              else
                Fail
                  ( "op="
                      <> T.pack (show op)
                      <> " count="
                      <> T.pack (show count)
                  )
          ),
      runTestM "compile EAttrs" $ do
        idx <-
          compileExpr
            ( EAttrs
                False
                [NamedBinding [StaticKey "a"] (ELit (NixInt 1))]
                NoCaptureInfo
            )
        op <- cbcOpcode idx
        fl <- cbcFlags idx
        count <- cbcShortArg idx
        pure
          ( if op == OpAttrs && fl == 0 && count == 1
              then Pass
              else Fail "attrs structure mismatch"
          ),
      runTestM "compile EAttrs recursive" $ do
        idx <-
          compileExpr
            ( EAttrs
                True
                [ NamedBinding [StaticKey "x"] (ELit (NixInt 1)),
                  NamedBinding [StaticKey "y"] (EResolvedVar 0 0)
                ]
                (Captures [(0, 0)])
            )
        op <- cbcOpcode idx
        fl <- cbcFlags idx
        count <- cbcShortArg idx
        pure
          ( if op == OpAttrs && fl == 1 && count == 2
              then Pass
              else
                Fail
                  ( "flags="
                      <> T.pack (show fl)
                      <> " count="
                      <> T.pack (show count)
                  )
          ),
      runTestM "compile ESelect" $ do
        idx <-
          compileExpr
            (ESelect (EVar "x") [StaticKey "a"] Nothing)
        op <- cbcOpcode idx
        fl <- cbcFlags idx
        pure
          ( if op == OpSelect && fl == 0
              then Pass
              else Fail ("flags=" <> T.pack (show fl))
          ),
      runTestM "compile ESelect with default" $ do
        idx <-
          compileExpr
            (ESelect (EVar "x") [StaticKey "a"] (Just (ELit NixNull)))
        op <- cbcOpcode idx
        fl <- cbcFlags idx
        defIdx <- cbcArg3 idx
        defOp <- cbcOpcode defIdx
        pure
          ( if op == OpSelect && fl == 1 && defOp == OpLitNull
              then Pass
              else Fail ("flags=" <> T.pack (show fl))
          ),
      runTestM "compile EHasAttr" $ do
        idx <-
          compileExpr
            (EHasAttr (EVar "x") [StaticKey "a", StaticKey "b"])
        op <- cbcOpcode idx
        pathLen <- cbcShortArg idx
        pure
          ( if op == OpHasAttr && pathLen == 2
              then Pass
              else
                Fail
                  ( "op="
                      <> T.pack (show op)
                      <> " pathLen="
                      <> T.pack (show pathLen)
                  )
          ),
      -- ESearchPath is desugared by resolver to __findFile __nixPath "name",
      -- so the compiler sees an EApp chain, not a raw ESearchPath.
      runTestM "compile desugared search path" $ do
        idx <- compileExpr (EApp (EApp (EVar "__findFile") (EVar "__nixPath")) (EStr [StrLit "nixpkgs"]))
        op <- cbcOpcode idx
        pure
          ( if op == OpApp
              then Pass
              else Fail ("expected OpApp, got op=" <> T.pack (show op))
          ),
      runTestM "op_count grows after compilation" $ do
        before <- cbcOpCount
        _ <- compileExpr (EApp (EVar "f") (ELit (NixInt 1)))
        after <- cbcOpCount
        pure
          ( if after > before
              then Pass
              else
                Fail
                  ( "before="
                      <> T.pack (show before)
                      <> " after="
                      <> T.pack (show after)
                  )
          ),
      runTestM "compile ELit NixFloat" $ do
        idx <- compileExpr (ELit (NixFloat 3.14))
        op <- cbcOpcode idx
        pure (assertEqual "opcode" OpLitFloat op),
      runTestM "compile ELit NixUri" $ do
        idx <- compileExpr (ELit (NixUri "https://example.com"))
        op <- cbcOpcode idx
        sym <- cbcArg1 idx
        pure
          ( if op == OpLitUri && symbolText (Symbol sym) == "https://example.com"
              then Pass
              else Fail "uri mismatch"
          ),
      runTestM "compile ELit NixPath" $ do
        idx <- compileExpr (ELit (NixPath "/nix/store/foo"))
        op <- cbcOpcode idx
        sym <- cbcArg1 idx
        pure
          ( if op == OpLitPath && symbolText (Symbol sym) == "/nix/store/foo"
              then Pass
              else Fail "path mismatch"
          ),
      runTestM "compile Inherit binding" $ do
        idx <-
          compileExpr
            ( EAttrs
                False
                [Inherit Nothing ["x", "y"]]
                NoCaptureInfo
            )
        op <- cbcOpcode idx
        count <- cbcShortArg idx
        pure
          ( if op == OpAttrs && count == 1
              then Pass
              else Fail "inherit binding mismatch"
          ),
      runTestM "compile ELambda (FormalSet)" $ do
        idx <-
          compileExpr
            ( ELambda
                (FormalSet [Formal "a" Nothing, Formal "b" (Just (ELit (NixInt 0)))] False)
                (EResolvedVar 0 0)
                NoCaptureInfo
            )
        op <- cbcOpcode idx
        fl <- cbcFlags idx
        pure
          ( if op == OpLambda && fl == formalSet
              then Pass
              else Fail ("flags=" <> T.pack (show fl))
          ),
      runTestM "compile ELambda (FormalNamedSet)" $ do
        idx <-
          compileExpr
            ( ELambda
                (FormalNamedSet "args" [Formal "x" Nothing] True)
                (EResolvedVar 0 0)
                NoCaptureInfo
            )
        op <- cbcOpcode idx
        fl <- cbcFlags idx
        pure
          ( if op == OpLambda && fl == formalNamedSet
              then Pass
              else Fail ("flags=" <> T.pack (show fl))
          ),
      runTestM "compile CaptureInfo (Captures)" $ do
        idx <-
          compileExpr
            ( ELambda
                (FormalName "x")
                (EResolvedVar 0 0)
                (Captures [(1, 2), (3, 4)])
            )
        op <- cbcOpcode idx
        capOff <- cbcArg3 idx
        capTag <- cbcData capOff
        capCount <- cbcData (capOff + 1)
        capL0 <- cbcData (capOff + 2)
        capI0 <- cbcData (capOff + 3)
        capL1 <- cbcData (capOff + 4)
        capI1 <- cbcData (capOff + 5)
        pure
          ( if op == OpLambda
              && capTag == captureSlots
              && capCount == 2
              && capL0 == 1
              && capI0 == 2
              && capL1 == 3
              && capI1 == 4
              then Pass
              else
                Fail
                  ( "capTag="
                      <> T.pack (show capTag)
                      <> " capCount="
                      <> T.pack (show capCount)
                  )
          ),
      runTestM "compile CaptureInfo (CapturesWithScopes)" $ do
        idx <-
          compileExpr
            ( ELambda
                (FormalName "x")
                (EResolvedVar 0 0)
                (CapturesWithScopes [(0, 0)])
            )
        capOff <- cbcArg3 idx
        capTag <- cbcData capOff
        pure (assertEqual "captureTag" captureWithScopes capTag),
      runTestM "compile EWithVar" $ do
        idx <- compileExpr (EWithVar "dynamic")
        op <- cbcOpcode idx
        sym <- cbcArg1 idx
        pure
          ( if op == OpWithVar && symbolText (Symbol sym) == "dynamic"
              then Pass
              else Fail "withvar mismatch"
          ),
      runTestM "compile EIndStr" $ do
        idx <- compileExpr (EIndStr [StrLit "indented"])
        op <- cbcOpcode idx
        count <- cbcShortArg idx
        pure
          ( if op == OpIndStr && count == 1
              then Pass
              else Fail "indstr mismatch"
          )
    ]

-- ---------------------------------------------------------------------------
-- Tests: substituter signature verification (the --trusted-key trust gate)
-- ---------------------------------------------------------------------------

-- | Minimal narinfo for signing tests; the fingerprint covers StorePath,
-- NarHash, NarSize, and References.
sigTestNarInfo :: NarInfo.NarInfo
sigTestNarInfo =
  NarInfo.NarInfo
    { NarInfo.niStorePath = "/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-hello-1.0",
      NarInfo.niUrl = "nar/aaaa.nar",
      NarInfo.niCompression = "none",
      NarInfo.niFileHash = Just ("sha256:" <> T.replicate 52 "0"),
      NarInfo.niFileSize = Just 1,
      NarInfo.niNarHash = "sha256:" <> T.replicate 52 "0",
      NarInfo.niNarSize = 1,
      NarInfo.niReferences = [],
      NarInfo.niDeriver = Nothing,
      NarInfo.niSigs = [],
      NarInfo.niCA = Nothing
    }

-- | A substituter cache trusting the given public key text.
sigTestCache :: Text -> Subst.CacheConfig
sigTestCache publicKeyText =
  Subst.CacheConfig
    { Subst.ccUrl = "http://localhost/test-cache",
      Subst.ccPublicKey = publicKeyText,
      Subst.ccPriority = 40
    }

-- | Deterministic Ed25519 test keys: a nix secret key file is
-- seed(32) + public(32), and both signing and public-key derivation use
-- only the seed, so the public half can be zeros here.  Both keys carry
-- the SAME name so the wrong-key test exercises the cryptographic
-- verification, not just the name comparison.
sigTestKeyText, sigTestWrongKeyText :: Text
sigTestKeyText = "test-key-1:" <> B64.encode (BS.pack ([1 .. 32] ++ replicate 32 0))
sigTestWrongKeyText = "test-key-1:" <> B64.encode (BS.pack ([101 .. 132] ++ replicate 32 0))

-- | Derive (trusted cache, signature by the trusted key, signature by an
-- impostor key with the same name).  Left on any setup failure.
sigTestSetup :: Either String (Subst.CacheConfig, Text, Text)
sigTestSetup = do
  secretKey <- Signing.parseSecretKey sigTestKeyText
  impostorKey <- Signing.parseSecretKey sigTestWrongKeyText
  publicKey <- Signing.toPublicKey secretKey
  let publicKeyText = Signing.renderPublicKey publicKey
  validSig <- Signing.sign secretKey sigTestNarInfo
  impostorSig <- Signing.sign impostorKey sigTestNarInfo
  pure (sigTestCache publicKeyText, validSig, impostorSig)

-- | verifySigs is the security boundary behind @--trusted-key@: unsigned
-- narinfos, wrong-key signatures, and malformed trusted keys must all be
-- rejected; a valid signature must be accepted.
testVerifySigs :: IO [Bool]
testVerifySigs = do
  putStrLn "substituter/verify-sigs"
  case sigTestSetup of
    Left err -> (: []) <$> runTest "verifySigs test setup" (Fail (T.pack err))
    Right (cache, validSig, impostorSig) ->
      sequence
        [ runTest "valid signature accepted" $
            assertEqual "verify ok" (Right ()) (Subst.verifySigs cache sigTestNarInfo {NarInfo.niSigs = [validSig]}),
          runTest "unsigned narinfo rejected" $
            assertLeft "no sigs" (Subst.verifySigs cache sigTestNarInfo),
          runTest "same-name signature from a different key rejected" $
            assertLeft "impostor sig" (Subst.verifySigs cache sigTestNarInfo {NarInfo.niSigs = [impostorSig]}),
          runTest "valid signature among invalid ones accepted" $
            assertEqual "any valid" (Right ()) (Subst.verifySigs cache sigTestNarInfo {NarInfo.niSigs = [impostorSig, validSig]}),
          runTest "signature under a different key name rejected" $
            assertLeft "name mismatch" (Subst.verifySigs cache sigTestNarInfo {NarInfo.niSigs = ["other-key:" <> T.drop (T.length "test-key-1:") validSig]}),
          runTest "malformed trusted public key rejected" $
            assertLeft "bad pubkey" (Subst.verifySigs (sigTestCache "not-a-key") sigTestNarInfo {NarInfo.niSigs = [validSig]})
        ]

-- | Narinfo field validation gates the pipeline ahead of the signed
-- fingerprint: a malformed field must fail as a parse error before its
-- text can reach the fingerprint or anything downstream.
testNarInfoValidation :: IO [Bool]
testNarInfoValidation = do
  putStrLn "substituter/narinfo-field-validation"
  sequence
    [ runTest "well-formed narinfo passes" $
        assertEqual "valid" (Right ()) (Subst.validateNarInfoFields sigTestNarInfo),
      runTest "malformed StorePath rejected" $
        assertLeft "bad store path" (Subst.validateNarInfoFields sigTestNarInfo {NarInfo.niStorePath = "/nix/store/zzz"}),
      runTest "derivation StorePath rejected" $
        assertLeft "drv path" (Subst.validateNarInfoFields sigTestNarInfo {NarInfo.niStorePath = "/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-hello-1.0.drv"}),
      runTest "malformed reference rejected" $
        assertLeft "bad reference" (Subst.validateNarInfoFields sigTestNarInfo {NarInfo.niReferences = ["../escape"]}),
      runTest "malformed NarHash rejected" $
        assertLeft "bad narhash" (Subst.validateNarInfoFields sigTestNarInfo {NarInfo.niNarHash = "sha256:nope"}),
      runTest "negative NarSize rejected" $
        assertLeft "negative narsize" (Subst.validateNarInfoFields sigTestNarInfo {NarInfo.niNarSize = -1}),
      runTest "field validation reports ahead of signature verification" $
        case sigTestSetup of
          Left err -> Fail (T.pack err)
          Right (cache, validSig, _) ->
            case Subst.narInfoPreflight cache sigTestNarInfo {NarInfo.niStorePath = "/nix/store/zzz", NarInfo.niSigs = [validSig]} of
              Left err
                | "invalid narinfo" `T.isInfixOf` err -> Pass
                | otherwise -> Fail ("expected the field-validation error, got: " <> err)
              Right () -> Fail "expected failure, got a clean preflight"
    ]

-- ---------------------------------------------------------------------------
-- Tests: resolver static globals stay in sync with the root env
-- ---------------------------------------------------------------------------

-- | Every name the resolver treats as a static global must actually be
-- bound in the root environment: 'Nix.Expr.Resolve.resolveVar' leaves such
-- names as 'EVar' even under a @with@, so an unbound one would surface as
-- an undefined variable.  Layering keeps Resolve from importing Builtins,
-- so this test is the sync guarantee between the two lists.
testStaticGlobalsSync :: IO [Bool]
testStaticGlobalsSync = do
  putStrLn "resolve/static-globals-sync"
  mapM checkBound (Set.toList staticGlobalNames)
  where
    checkBound name =
      runTest ("static global '" <> name <> "' is bound in the root env") $
        assertEval ("global-" <> name) ("builtins.seq " <> name <> " true") (VBool True)

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- Tests: byte-indexed string semantics (issue #34)
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

main :: IO ()
main = bracket_ arenaInit arenaDestroy $ do
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
          testSourceDateEpochIO,
          testUnpackBuildIO,
          testDependentBuildIO,
          testEvalFidelity,
          testUpstreamConformance,
          testHashHelpers,
          testNarKnownAnswer,
          testFetchGitTransport,
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
          testEvalRecAttrs,
          testEvalLists,
          testEvalLambda,
          testEvalWith,
          testEvalBuiltins,
          testFromTOML,
          testEvalErrors,
          testEvalHigherOrder,
          testLexer,
          testParserExprs,
          testParserErrors,
          testParserIntegration,
          testStaticGlobalsSync,
          testBatch1,
          testBatch2,
          testBatch3,
          testBatch4,
          testBatch5,
          testBatch6,
          testBatch7,
          testImportPure,
          testImportIO,
          testBlackholeRecoveryIO,
          testPathFilterIO,
          testPathSymlinkIO,
          testBatchA,
          testBatchAIO,
          testBatchB,
          testBatchC,
          testBatchCIO,
          testBlackhole,
          testBatchD,
          testBatchE,
          testBatchEIO,
          testBatchF,
          testBatchG,
          testBatchH,
          testStringContext,
          testContextHelpers,
          testContextPropagation,
          testDrvContext,
          testDepGraph,
          testSubstituter,
          testPathLocks,
          testVerifySigs,
          testNarInfoValidation,
          testPushPure,
          testPushClosureIO,
          testBuildOrchestrator,
          testStoreDB,
          testParseStorePath,
          testStoreOps,
          testStoreDelete,
          testSymlinkWalksIO,
          testLinkOrdering,
          testFromATerm,
          testBuilder,
          testE2E,
          testPhase4,
          testPhase4IO,
          testToJSONPathIO,
          testByteStringSemantics,
          testByteStringSemanticsIO,
          testSymbol,
          testCAttrSet,
          testCThunk,
          testBytecodeCompile,
          testBytecodeCountSpill,
          testValueCountWidths,
          testStoreNameSinks,
          testNarNameSafety,
          testStoreNameSinksIO,
          testClassIFollowups,
          testClassIFollowupsIO
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
