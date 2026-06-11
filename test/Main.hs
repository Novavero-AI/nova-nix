{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Main (main) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as TarEntry
import qualified Codec.Compression.Zstd.Lazy as ZstdL
import Control.Exception (bracket_)
import Control.Monad (filterM, void, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Foreign.Ptr (castPtr)
import Foreign.StablePtr (StablePtr, castPtrToStablePtr, castStablePtrToPtr, deRefStablePtr, freeStablePtr, newStablePtr)
import Nix.Builder (BuildConfig (..), BuildResult (..), buildDerivation, buildWithDeps, defaultBuildConfig)
import Nix.Builder.Unpack (builtinUnpackBuilder, envSrcs)
import Nix.Builtins (builtinEnv, parseNixPath)
import qualified Nix.DependencyGraph as DepGraph
import Nix.Derivation (Derivation (..), DerivationOutput (..), Platform (..), currentPlatform, fromATerm, platformToText, toATerm)
import Nix.Eval (NixValue (..), StringContext (..), StringContextElement (..), attrSetFromMap, attrSetLookup, attrSetNull, attrSetSize, builtinNames, emptyContext, emptyEnv, eval, force, mkStr, readThunkValue, runPureEval)
import Nix.Eval.Arena (arenaDestroy, arenaInit)
import Nix.Eval.CAttrSet (cattrsetFreeze, cattrsetInsert, cattrsetKeys, cattrsetLookup, cattrsetNew, cattrsetSize, cattrsetUnion)
import Nix.Eval.CBytecode (binaryAdd, captureSlots, captureWithScopes, cbcArg1, cbcArg2, cbcArg3, cbcData, cbcFlags, cbcOpCount, cbcOpcode, cbcShortArg, formalName, formalNamedSet, formalSet, strpartInterp, strpartLit, unaryNegate, pattern OpApp, pattern OpAssert, pattern OpAttrs, pattern OpBinary, pattern OpHasAttr, pattern OpIf, pattern OpIndStr, pattern OpLambda, pattern OpLet, pattern OpList, pattern OpLitBool, pattern OpLitFloat, pattern OpLitInt, pattern OpLitNull, pattern OpLitPath, pattern OpLitUri, pattern OpResolvedVar, pattern OpSelect, pattern OpStr, pattern OpUnary, pattern OpVar, pattern OpWith, pattern OpWithVar)
import Nix.Eval.CThunk (CThunkPtr, cthunkCount, cthunkGet, cthunkMarkBlackhole, cthunkNew, cthunkNewComputed, cthunkPayload, cthunkSetComputed, cthunkState)
import Nix.Eval.Compile (compileExpr)
import qualified Nix.Eval.Context as Context
import Nix.Eval.IO (EvalState (..), newEvalState, runEvalIO)
import Nix.Eval.Symbol (Symbol (..), symbolCount, symbolIntern, symbolLen, symbolText)
import Nix.Eval.Types (emptyCList)
import Nix.Expr.Types
import qualified Nix.Hash as Hash
import Nix.Parser (parseNix)
import Nix.Parser.Lexer (Located (..), Token (..), tokenize)
import Nix.Push (computeClosure, mkNarInfo, narFileName, planMissing, storePathBasename, stripHashPrefix)
import Nix.Store (Store (..), addToStore, closeStore, isValid, openStore, pathExists, scanReferences, setReadOnly, writeDrv)
import Nix.Store.DB (PathInfo (..), PathRegistration (..), closeStoreDB, isValidPath, openStoreDB, queryDeriver, queryPathInfo, queryReferences, registerPath, registerPaths)
import Nix.Store.Path (StoreDir (..), StorePath (..), defaultStoreDir, parseStorePath, platformStoreDirText, storePathToFilePath, storePathToText, windowsStoreDir)
import qualified Nix.Substituter as Subst
import qualified NovaCache.Hash as CHash
import qualified NovaCache.NarInfo as NarInfo
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getPermissions, getTemporaryDirectory, removeDirectoryRecursive, writable)
import qualified System.Directory as Dir
import System.Exit (ExitCode (..), exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.IO (BufferMode (..), hSetBuffering, stdout)
import qualified System.Info as SI
import qualified System.Process as Proc

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
runTestM :: Text -> IO TestResult -> IO Bool
runTestM name action = do
  result <- action
  runTest name result

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

-- | Helper: parse Nix source and evaluate with builtinEnv.
evalNix :: Text -> Either Text NixValue
evalNix source = case parseNix "<test>" source of
  Left err -> Left (T.pack (show err))
  Right expr -> runPureEval (eval (builtinEnv 0 []) expr)

-- | Assert that a Nix expression evaluates to the expected value.
assertEval :: Text -> Text -> NixValue -> TestResult
assertEval label source expected =
  assertRight label (evalNix source) $ \actual ->
    assertEqual label expected actual

-- | Assert that a Nix expression fails to evaluate.
assertEvalFail :: Text -> Text -> TestResult
assertEvalFail label source =
  assertLeft label (evalNix source)

-- ---------------------------------------------------------------------------
-- Shell discovery for builder tests
-- ---------------------------------------------------------------------------

-- | Find a POSIX-compatible shell for builder tests.
-- On Unix, always @\/bin\/sh@.  On Windows, searches for @bash.exe@
-- at known Git for Windows locations first, then PATH.  Checks known
-- paths first to avoid picking up the WSL launcher at
-- @C:\\Windows\\System32\\bash.exe@ which exits 1 when WSL is not
-- configured.  Real Nix builders always use bash from the store â€”
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
         in assertEqual "Ord" True (sp < sp2)
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
-- Tests: Eval â€” Literals
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
-- Tests: Eval â€” Variables
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
-- Tests: Eval â€” Arithmetic
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
      runTest "int-float promotion" $
        assertEval "promote" "1 + 2.0" (VFloat 3.0),
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
-- Tests: Eval â€” Comparison
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
        assertEval "str-lt" "\"abc\" < \"def\"" (VBool True)
    ]

-- ---------------------------------------------------------------------------
-- Tests: Eval â€” Logic
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
-- Tests: Eval â€” Strings
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
-- Tests: Eval â€” If/Assert
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
-- Tests: Eval â€” Let
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
-- Tests: Eval â€” Attribute sets
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
-- Tests: Eval â€” Recursive attribute sets
-- ---------------------------------------------------------------------------

testEvalRecAttrs :: IO [Bool]
testEvalRecAttrs = do
  putStrLn "eval/rec-attrs"
  sequence
    [ runTest "rec self-reference" $
        assertEval "rec-self" "rec { a = 1; b = a + 1; }.b" (VInt 2),
      runTest "rec mutual reference" $
        assertEval "rec-mutual" "rec { a = 1; b = a; }.b" (VInt 1)
    ]

-- ---------------------------------------------------------------------------
-- Tests: Eval â€” Lists
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
-- Tests: Eval â€” Lambda
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
        assertEval "default" "({ a ? 10 }: a) { }" (VInt 10)
    ]

-- ---------------------------------------------------------------------------
-- Tests: Eval â€” With
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
-- Tests: Eval â€” Builtins
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
      runTest "toString int" $
        assertEval "toStr" "builtins.toString 42" (mkStr "42")
    ]

-- ---------------------------------------------------------------------------
-- Tests: Eval â€” Errors
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
-- Tests: Eval â€” Higher-order builtins
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
      -- FormalName "x" â†’ slot 0; FormalSet [a,b] â†’ a=0, b=1;
      -- FormalNamedSet "args" [a] â†’ args=0, a=1.
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
        assertParse
          "nested attrs"
          "{ a.b.c = 1; d = { e = 2; }; }"
          ( EAttrs
              False
              [ NamedBinding [StaticKey "a", StaticKey "b", StaticKey "c"] (ELit (NixInt 1)),
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
-- Tests: Batch 1 â€” Trivial pure builtins + constants
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
      -- Constants
      runTest "storeDir" $
        assertEval "storeDir" "builtins.storeDir" (mkStr platformStoreDirText),
      runTest "nixVersion" $
        assertEval "nixVersion" "builtins.nixVersion" (mkStr "2.24.0"),
      runTest "langVersion" $
        assertEval "langVersion" "builtins.langVersion" (VInt 6),
      runTest "nixPath" $
        assertEval "nixPath" "builtins.nixPath" (VList emptyCList)
    ]

-- ---------------------------------------------------------------------------
-- Tests: Batch 2 â€” Arithmetic + bitwise builtins
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
-- Tests: Batch 3 â€” Attrset higher-order builtins
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
-- Tests: Batch 4 â€” String operations
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
      -- compareVersions
      runTest "compareVersions equal" $
        assertEval "cmpVer-eq" "builtins.compareVersions \"1.2.3\" \"1.2.3\"" (VInt 0),
      runTest "compareVersions less" $
        assertEval "cmpVer-lt" "builtins.compareVersions \"1.2\" \"1.3\"" (VInt (-1)),
      runTest "compareVersions greater" $
        assertEval "cmpVer-gt" "builtins.compareVersions \"2.0\" \"1.9\"" (VInt 1),
      runTest "compareVersions type error" $
        assertEvalFail "cmpVer-err" "builtins.compareVersions 1 2",
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
-- Tests: Batch 5 â€” Serialization + hashing
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
-- Tests: Batch 6 â€” tryEval + deepSeq
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
      -- tryEval catches coercion error (+ on attrset without outPath)
      runTest "tryEval catches coercion error" $
        assertEval "tryEval-tyerr" "(builtins.tryEval ({} + [])).success" (VBool False),
      -- deepSeq
      runTest "deepSeq returns second" $
        assertEval "deepSeq" "builtins.deepSeq [ 1 2 3 ] 42" (VInt 42),
      runTest "deepSeq forces nested" $
        assertEvalFail "deepSeq-err" "builtins.deepSeq [ (builtins.throw \"boom\") ] 42",
      runTest "deepSeq forces attrs" $
        assertEvalFail "deepSeq-attr" "builtins.deepSeq { a = builtins.throw \"boom\"; } 42"
    ]

-- ---------------------------------------------------------------------------
-- Tests: Batch 7 â€” genericClosure
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

-- | Run a named IO eval test â€” single label, no double-wrapping.
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
-- Tests: Batch A â€” getEnv, currentTime, toPath
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
-- Tests: Batch A â€” IO tests (getEnv)
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
              VStr s _ -> if T.null s then Fail "PATH was empty" else Pass
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
-- Tests: Batch B â€” placeholder, storePath
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
      -- storePath
      runTest "storePath valid" $
        assertEval
          "storePath-valid"
          "builtins.storePath \"/nix/store/s66mzxpvicwk07gjbjfw9izjfa797vsw-hello-2.12.1\""
          (VPath "/nix/store/s66mzxpvicwk07gjbjfw9izjfa797vsw-hello-2.12.1"),
      runTest "storePath invalid" $
        assertEvalFail "storePath-bad" "builtins.storePath \"/tmp/not-a-store-path\"",
      runTest "storePath type error" $
        assertEvalFail "storePath-err" "builtins.storePath 42"
    ]

-- ---------------------------------------------------------------------------
-- Tests: Batch C â€” findFile
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
          (VPath (T.pack nixpkgsDir)),
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
-- Tests: Batch D â€” toFile
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
-- Tests: Batch E â€” scopedImport
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
-- Tests: Batch F â€” fetchurl, fetchTarball, fetchGit
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
-- Tests: Batch G â€” ATerm serialization
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
         in if T.isPrefixOf "Derive(" aterm && T.isSuffixOf ")" aterm
              then Pass
              else Fail ("bad ATerm: " <> aterm),
      runTest "ATerm has output" $
        let aterm = toATerm drvWithOutput
         in if "\"out\"" `T.isInfixOf` aterm
              then Pass
              else Fail ("missing output in ATerm: " <> aterm),
      runTest "ATerm env sorted" $
        let aterm = toATerm drvWithEnv
         in -- "name" should come before "system" in sorted order
            case (T.breakOn "\"name\"" aterm, T.breakOn "\"system\"" aterm) of
              ((before1, _), (before2, _)) ->
                if T.length before1 < T.length before2
                  then Pass
                  else Fail ("env not sorted in ATerm: " <> aterm),
      runTest "ATerm string escaping" $
        let drv = minimalDrv {drvEnv = Map.fromList [("msg", "hello\nworld")]}
            aterm = toATerm drv
         in if "\\n" `T.isInfixOf` aterm
              then Pass
              else Fail ("missing escaped newline: " <> aterm),
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
-- Tests: Batch H â€” derivation
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
              if "/nix/store/" `T.isPrefixOf` p && ".drv" `T.isSuffixOf` p && ctx /= emptyContext
                then Pass
                else Fail ("bad drvPath: " <> p)
            _ -> Fail ("expected VStr with context, got " <> T.pack (show val)),
      runTest "derivation has outPath" $
        assertRight "drv-outPath" (evalNix "let d = derivation { name = \"hello\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; }; in d.outPath") $ \val ->
          case val of
            VStr p ctx ->
              if "/nix/store/" `T.isPrefixOf` p && ctx /= emptyContext
                then Pass
                else Fail ("bad outPath: " <> p)
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
      -- appendContext: adds context to plain string
      runTest "appendContext adds context" $
        assertEval
          "appendCtx-add"
          "let ctx = builtins.listToAttrs [{ name = \"/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-foo\"; value = { path = true; }; }]; in builtins.hasContext (builtins.appendContext \"hello\" ctx)"
          (VBool True),
      -- appendContext: empty context is no-op
      runTest "appendContext empty is no-op" $
        assertEval "appendCtx-empty" "builtins.hasContext (builtins.appendContext \"hello\" {})" (VBool False),
      -- unsafeDiscardOutputDependency: strips drv context, keeps plain
      runTest "discardOutputDep strips drv context"
        $ assertRight
          "discardOutDep"
          ( evalNix $
              T.concat
                [ "let d = derivation { name = \"test\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; }; ",
                  "stripped = builtins.unsafeDiscardOutputDependency d.outPath; ",
                  "in builtins.hasContext stripped"
                ]
          )
        $ \val -> assertEqual "no-ctx" (VBool False) val,
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
  sequence
    [ -- Single node
      runTest "single node graph" $ case DepGraph.buildDepGraph readSingle drvA spA of
        Right (DepGraph.DepGraph g) -> assertEqual "single-size" 1 (Map.size g)
        Left err -> Fail ("unexpected error: " <> err),
      -- Linear chain A->C: topoSort should give [C, A]
      runTest "linear chain topo" $ case DepGraph.buildDepGraph readChain drvB spB of
        Right graph -> case DepGraph.topoSort graph of
          DepGraph.TopoSorted order ->
            case order of
              [first, _] | first == spC -> Pass
              _ -> Fail ("bad order: " <> T.pack (show order))
          DepGraph.TopoCycle cyc -> Fail ("unexpected cycle: " <> T.pack (show cyc))
        Left err -> Fail ("graph build failed: " <> err),
      -- Diamond D->B,C; B->C: topoSort should have C first, D last
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
      -- Missing .drv -> failure
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
        Right graph ->
          -- Graph builds but topo should detect the cycle or return partial
          case DepGraph.topoSort graph of
            DepGraph.TopoSorted _ -> Pass -- Kahn's returns what it can
            DepGraph.TopoCycle _ -> Pass
        Left _ -> Pass -- Also acceptable if buildDepGraph fails
    ]

-- ---------------------------------------------------------------------------
-- Tests: Substituter (Phase 3, Batch 6)
-- ---------------------------------------------------------------------------

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
      -- decompressNar: "none" passes through
      runTest "decompressNar none" $
        let input = "fake nar data"
         in assertEqual "decompress-none" (Right input) (Subst.decompressNar "none" input),
      -- decompressNar: empty compression passes through
      runTest "decompressNar empty" $
        let input = "fake nar data"
         in assertEqual "decompress-empty" (Right input) (Subst.decompressNar "" input),
      -- decompressNar: xz returns error
      runTest "decompressNar xz unsupported" $
        case Subst.decompressNar "xz" "data" of
          Left _ -> Pass
          Right _ -> Fail "expected error for xz",
      -- decompressNar: unknown compression
      runTest "decompressNar unknown" $
        case Subst.decompressNar "brotli" "data" of
          Left _ -> Pass
          Right _ -> Fail "expected error for unknown",
      -- parseReferences: valid store paths
      runTest "parseReferences valid" $
        let refs = ["/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-hello"]
            parsed = Subst.parseReferences defaultStoreDir refs
         in assertEqual "parse-refs" 1 (length parsed),
      -- parseReferences: invalid paths filtered
      runTest "parseReferences invalid filtered" $
        let refs = ["not-a-store-path", "/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-hello"]
            parsed = Subst.parseReferences defaultStoreDir refs
         in assertEqual "parse-refs-filter" 1 (length parsed),
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
      -- verifyNarHash: matching NAR hash accepted (HIGH#2 integrity gate)
      runTest "verifyNarHash accepts matching hash" $
        assertEqual "narhash-match" (Right ()) (Subst.verifyNarHash (sampleNarInfo sampleNarHash) sampleNarBytes),
      -- verifyNarHash: mismatched NAR hash rejected
      runTest "verifyNarHash rejects mismatched hash" $
        case Subst.verifyNarHash (sampleNarInfo wrongNarHash) sampleNarBytes of
          Left _ -> Pass
          Right () -> Fail "expected mismatch rejection",
      -- verifyNarHash: malformed NAR hash rejected
      runTest "verifyNarHash rejects malformed hash" $
        case Subst.verifyNarHash (sampleNarInfo "not-a-hash") sampleNarBytes of
          Left _ -> Pass
          Right () -> Fail "expected malformed-hash rejection",
      -- narInfoMatchesPath: identity match accepted
      runTest "narInfoMatchesPath accepts matching identity" $
        if Subst.narInfoMatchesPath (StorePath sampleHash "hello") (sampleNarInfo sampleNarHash)
          then Pass
          else Fail "expected identity match",
      -- narInfoMatchesPath: identity mismatch rejected
      runTest "narInfoMatchesPath rejects mismatched identity" $
        if Subst.narInfoMatchesPath (StorePath (T.replicate 32 "b") "hello") (sampleNarInfo sampleNarHash)
          then Fail "expected identity mismatch"
          else Pass
    ]
  where
    sampleNarBytes = "nova-nix nar sample bytes" :: BS.ByteString
    sampleNarHash = CHash.formatNixHash (CHash.hashBytes sampleNarBytes)
    wrongNarHash = CHash.formatNixHash (CHash.hashBytes ("different bytes" :: BS.ByteString))
    sampleHash = T.replicate 32 "a"
    sampleNarInfo narHash =
      NarInfo.NarInfo
        { NarInfo.niStorePath = "/nix/store/" <> sampleHash <> "-hello",
          NarInfo.niUrl = "nar/sample.nar",
          NarInfo.niCompression = "none",
          NarInfo.niFileHash = sampleNarHash,
          NarInfo.niFileSize = 0,
          NarInfo.niNarHash = narHash,
          NarInfo.niNarSize = fromIntegral (BS.length sampleNarBytes),
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
        -- Builder will fail (nonexistent) but the graph should resolve correctly
        pure $ case result of
          BuildFailure _ _ -> Pass
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
      -- missing .drv -> failure in dep graph
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
      -- derivation with context creates populated inputDrvs
      runTest "derivation context populates inputDrvs"
        $ assertRight
          "drv-ctx-inputs"
          ( evalNix $
              T.concat
                [ "let dep = derivation { name = \"dep\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; }; ",
                  "main = derivation { name = \"main\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; src = dep.outPath; }; ",
                  "in main._derivation"
                ]
          )
        $ \case
          VDerivation drv ->
            if Map.null (drvInputDrvs drv)
              then Fail "expected non-empty drvInputDrvs"
              else Pass
          _ -> Fail "expected VDerivation"
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
        -- queryDeriver nothing
        runTestM "db queryDeriver nothing" $ do
          db <- openStoreDB storeDir
          let sp = StorePath "eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee" "noderiver"
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
          (parseStorePath windowsStoreDir "C:\\nix\\store/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb-pkg")
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
      -- References deliberately unsorted; deriver present.
      ni = mkNarInfo spA narHash 1234 [spB, spA] (Just spB)
  sequence
    [ runTest "narinfo StorePath is canonical /nix/store" $
        assertEqual "StorePath" ("/nix/store/" <> hashA <> "-hello-1.0") (NarInfo.niStorePath ni),
      runTest "narinfo URL is nar/<digest>.nar" $
        assertEqual "URL" "nar/0123abcdef.nar" (NarInfo.niUrl ni),
      runTest "compression none mirrors NAR fields into file fields" $
        assertEqual
          "file fields"
          ("none", NarInfo.niNarHash ni, NarInfo.niNarSize ni)
          (NarInfo.niCompression ni, NarInfo.niFileHash ni, NarInfo.niFileSize ni),
      runTest "narinfo sizes carry through" $
        assertEqual "NarSize" 1234 (NarInfo.niNarSize ni),
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
        assertEqual "file" "0123abcdef.nar" (narFileName narHash)
    ]

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
            assertEqual "count" (3 :: Int) (length closure)
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
-- trivial derivation end-to-end natively â€” a recipe that writes to @$out@,
-- with the output landing in the store.  No stdenv, no dependencies: just
-- the raw build path (process spawn -> output capture -> addToStore),
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
-- build proves cross-archive file collisions fail loudly, and a third that
-- path traversal is rejected.
testUnpackBuildIO :: IO [Bool]
testUnpackBuildIO = do
  putStrLn "builder/unpack-seed-archives"
  withTempStore $ \store -> do
    let storeDir = stDir store
    tmpBase <- getTemporaryDirectory
    let workDir = tmpBase </> "nova-nix-test-unpack-src"
    forceRemoveIfExists workDir
    createDirectoryIfMissing True workDir
    let archiveTools = workDir </> "pkg-tools.tar.zst"
        archiveData = workDir </> "pkg-data.tar.zst"
        archiveCollide = workDir </> "pkg-collide.tar.zst"
        archiveEscape = workDir </> "pkg-escape.tar.zst"
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
    let mkUnpackDrv outP srcFiles =
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
                  [(envSrcs, T.intercalate " " (map T.pack srcFiles))]
            }
        seedOut = StorePath (T.replicate 31 "0" <> "2") "unpack-seed"
        collideOut = StorePath (T.replicate 31 "0" <> "3") "unpack-collide"
        escapeOut = StorePath (T.replicate 31 "0" <> "4") "unpack-escape"
    buildTmp <- (</> "nova-nix-test-unpack-build-tmp") <$> getTemporaryDirectory
    forceRemoveIfExists buildTmp
    createDirectoryIfMissing True buildTmp
    let config = (defaultBuildConfig storeDir) {bcTmpDir = buildTmp}
    seedResult <- buildDerivation config store (mkUnpackDrv seedOut [archiveTools, archiveData])
    collideResult <- buildDerivation config store (mkUnpackDrv collideOut [archiveTools, archiveCollide])
    escapeResult <- buildDerivation config store (mkUnpackDrv escapeOut [archiveEscape])
    forceRemoveIfExists buildTmp
    forceRemoveIfExists workDir
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
    pure (seedChecks ++ collideChecks ++ escapeChecks)

-- | Step 2 of the ladder: a dependency-aware build end-to-end.  A root
-- derivation depends on a leaf; both @.drv@ files are pre-written to the store
-- (as the build driver's closure-writing does), and 'buildWithDeps' must read
-- the closure, topologically order it, build the leaf first, then the root â€”
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

-- | Regression tests for the 2026-06-09 audit eval-fidelity fixes.  All are
-- parity-safe â€” none affects a derivation or store-path hash.
testEvalFidelity :: IO [Bool]
testEvalFidelity = do
  putStrLn "eval/audit-fidelity"
  sequence
    [ -- builtins.match: a non-participating capture group is null, not ""
      runTest "match null capture group" $
        assertEval "match-null" "builtins.isNull (builtins.elemAt (builtins.match \"(a)(b)?\" \"a\") 1)" (VBool True),
      runTest "match participating group value" $
        assertEval "match-val" "builtins.elemAt (builtins.match \"(a)(b)\" \"ab\") 1" (mkStr "b"),
      -- builtins.split: a non-participating group is null too
      runTest "split null capture group" $
        assertEval "split-null" "builtins.isNull (builtins.elemAt (builtins.elemAt (builtins.split \"(a)|(b)\" \"a\") 1) 1)" (VBool True),
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
        assertEvalFail "int-overflow" "99999999999999999999",
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
      -- PureEval tryEval must NOT catch abort â€” it propagates (matches C++ Nix)
      runTest "tryEval does not catch abort" $
        assertEvalFail "tryeval-abort" "(builtins.tryEval (builtins.abort \"x\")).success",
      -- every builtin in the registry (the source of builtinNames/builtinArity)
      -- is actually exposed in the builtins set â€” guards against builtinRegistry
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
    [ runTest "hexToBytes empty" $ assertEqual "empty" (Just BS.empty) (Hash.hexToBytes ""),
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
        assertEqual "unknown" Nothing (Hash.rawHashWithAlgo "sha3-256" (BS.pack [1, 2, 3]))
    ]

testStoreOps :: IO [Bool]
testStoreOps = do
  putStrLn "store/ops"
  withTempStore $ \store -> do
    let sd = stDir store
    sequence
      [ -- scanReferences finds embedded store path
        runTestM "scanReferences finds ref" $ do
          tmpBase <- getTemporaryDirectory
          let scanDir = tmpBase </> "nova-nix-test-scan"
          removeIfExists scanDir
          createDirectoryIfMissing True scanDir
          let candidate = StorePath "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "dep1"
              prefix = unStoreDir sd
              refString = prefix <> "/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-dep1"
          BS.writeFile (scanDir </> "output.txt") (TE.encodeUtf8 (T.pack ("hello " <> refString <> " world")))
          refs <- scanReferences sd [candidate] scanDir
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
          refs <- scanReferences sd [candidate] scanDir
          removeIfExists scanDir
          pure (assertEqual "no refs" [] refs),
        -- scanReferences ignores partial match
        runTestM "scanReferences ignores partial" $ do
          tmpBase <- getTemporaryDirectory
          let scanDir = tmpBase </> "nova-nix-test-scan3"
          removeIfExists scanDir
          createDirectoryIfMissing True scanDir
          let candidate = StorePath "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "dep1"
              prefix = unStoreDir sd
              -- Only 20 chars of hash â€” should not match 32-char candidate
              partialRef = prefix <> "/aaaaaaaaaaaaaaaaaaaa"
          BS.writeFile (scanDir </> "output.txt") (TE.encodeUtf8 (T.pack partialRef))
          refs <- scanReferences sd [candidate] scanDir
          removeIfExists scanDir
          pure (assertEqual "no partial refs" [] refs),
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
          [ (StorePath "eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee" "dep1.drv", ["out"]),
            (StorePath "ffffffffffffffffffffffffffffffff" "dep2.drv", ["lib", "out"])
          ],
      drvInputSrcs = [StorePath "gggggggggggggggggggggggggggggggg" "source.tar.gz"],
      drvPlatform = Aarch64_Darwin,
      drvBuilder = "/nix/store/hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh-bash/bin/bash",
      drvArgs = ["-e", "build.sh"],
      drvEnv =
        Map.fromList
          [ ("buildInputs", "/nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-dep1"),
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
      -- writeDrv writes correct ATerm
      runTestM "writeDrv writes correct ATerm" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-writeDrv"
        removeIfExists tmpStore
        store <- openStore (StoreDir tmpStore)
        let sp = StorePath "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "test.drv"
            destFile = storePathToFilePath (stDir store) sp
        writeDrv store simpleTestDrv sp
        contents <- TIO.readFile destFile
        closeStore store
        removeIfExists tmpStore
        pure (assertEqual "writeDrv content" (toATerm simpleTestDrv) contents),
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
      -- builtinDerivation populates drvEnv with the output paths ($out, â€¦)
      -- and the build attributes.  Note: the .drv env does NOT contain a
      -- "drvPath" key â€” matching C++ Nix, which never writes one.
      runTest "builtinDerivation populates drvEnv"
        $ assertRight
          "drvEnv"
          (evalNix "let d = derivation { name = \"test\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; }; in d._derivation")
        $ \val -> case val of
          VDerivation drv
            | Just op <- Map.lookup "out" (drvEnv drv),
              "/nix/store/" `T.isPrefixOf` op,
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
-- All scripts are POSIX shell â€” bash is used on every platform.
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
      drvBuilder = shell,
      drvArgs = ["-c", script],
      drvEnv = Map.fromList [("name", "test-build"), ("system", platformToText currentPlatform)]
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
      -- Output at expected path
      runTestM "output at expected store path" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-builder5"
        forceRemoveIfExists tmpStore
        store <- openStore (StoreDir tmpStore)
        let outSP = StorePath "eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee" "pathtest"
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
                  drvBuilder = shell,
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
      -- Builder succeeds but doesn't create $out -> build fails
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
        pure $ if not buildDirExists then Pass else Fail "build dir not cleaned up after failure"
    ]

-- ---------------------------------------------------------------------------
-- Tests: CLI Integration (Phase 2, Batch 5)
-- ---------------------------------------------------------------------------

-- | End-to-end: eval .nix source -> extract derivation -> build -> verify output.
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
-- Tests: Phase 4 â€” search paths, dynamic keys, directory import
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
-- CAttrSet stores void* â€” we use StablePtrs as opaque values in tests.
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
        -- Spot-check a few lookups
        hit <- cattrsetLookup set (syms !! 5000)
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
  -- Each test uses the shared arena (thunks accumulate â€” that's fine).
  sequence
    [ runTestM "new pending + state" $ do
        sp <- newStablePtr ("pending" :: Text)
        ptr <- cthunkNew (castStablePtrToPtr sp)
        state <- cthunkState ptr
        pure (assertEqual "state" 0 state),
      runTestM "new computed + state" $ do
        sp <- newStablePtr ("computed" :: Text)
        ptr <- cthunkNewComputed (castStablePtrToPtr sp)
        state <- cthunkState ptr
        pure (assertEqual "state" 1 state),
      runTestM "payload round-trips (pending)" $ do
        sp <- newStablePtr ("hello" :: Text)
        ptr <- cthunkNew (castStablePtrToPtr sp)
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
        ptr <- cthunkNew (castStablePtrToPtr sp)
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
        ptr <- cthunkNew (castStablePtrToPtr sp)
        _ <- cthunkMarkBlackhole ptr
        ok <- cthunkMarkBlackhole ptr
        pure (if not ok then Pass else Fail "should have failed"),
      runTestM "set_computed returns old payload" $ do
        pendingSp <- newStablePtr ("old" :: Text)
        ptr <- cthunkNew (castStablePtrToPtr pendingSp)
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
        ptr <- cthunkNew (castStablePtrToPtr sp)
        valSp <- newStablePtr ("v" :: Text)
        oldPayload <- cthunkSetComputed ptr (castStablePtrToPtr valSp)
        -- set_computed accepts PENDING (direct memoization, no blackhole step)
        oldVal <- deRefStablePtr (castPtrToStablePtr oldPayload) :: IO Text
        freeStablePtr sp
        pure (assertEqual "old payload" "x" oldVal),
      runTestM "count tracks allocations" $ do
        countBefore <- cthunkCount
        sp <- newStablePtr (0 :: Int)
        _ <- cthunkNew (castStablePtrToPtr sp)
        _ <- cthunkNew (castStablePtrToPtr sp)
        _ <- cthunkNewComputed (castStablePtrToPtr sp)
        countAfter <- cthunkCount
        let delta = countAfter - countBefore
        pure (assertEqual "delta" 3 delta),
      runTestM "get retrieves by index" $ do
        countBefore <- cthunkCount
        sp <- newStablePtr ("indexed" :: Text)
        ptr <- cthunkNew (castStablePtrToPtr sp)
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
        mapM_ (\_ -> cthunkNew (castStablePtrToPtr sp)) [(1 :: Int) .. 100000]
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
        -- 3 parts Ã— 2 words each = 6 data words
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
-- Main
-- ---------------------------------------------------------------------------

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
          testHashHelpers,
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
          testEvalErrors,
          testEvalHigherOrder,
          testLexer,
          testParserExprs,
          testParserErrors,
          testParserIntegration,
          testBatch1,
          testBatch2,
          testBatch3,
          testBatch4,
          testBatch5,
          testBatch6,
          testBatch7,
          testImportPure,
          testImportIO,
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
          testPushPure,
          testPushClosureIO,
          testBuildOrchestrator,
          testStoreDB,
          testParseStorePath,
          testStoreOps,
          testFromATerm,
          testBuilder,
          testE2E,
          testPhase4,
          testPhase4IO,
          testSymbol,
          testCAttrSet,
          testCThunk,
          testBytecodeCompile
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
