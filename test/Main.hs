{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Exception (bracket_)
import Control.Monad (when)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Nix.Builtins (builtinEnv)
import Nix.Derivation (Derivation (..), DerivationOutput (..), Platform (..), currentPlatform, platformToText, toATerm)
import Nix.Eval (Env (..), NixValue (..), Thunk (..), emptyEnv, eval, runPureEval)
import Nix.Eval.IO (EvalState (..), newEvalState, runEvalIO)
import Nix.Expr.Types
import Nix.Parser (parseNix)
import Nix.Parser.Lexer (Located (..), Token (..), tokenize)
import Nix.Store.Path (StoreDir (..), StorePath (..), defaultStoreDir, storePathToFilePath, windowsStoreDir)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getTemporaryDirectory, removeDirectoryRecursive)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.IO (BufferMode (..), hSetBuffering, stdout)

-- ---------------------------------------------------------------------------
-- Test harness (same pattern as gbnet-hs, nova-cache)
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
  Right expr -> runPureEval (eval (builtinEnv 0) expr)

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
        let expr = ELambda (FormalName "x") (EVar "x")
         in assertEqual "ELambda" expr expr,
      runTest "let binding" $
        let expr = ELet [NamedBinding [StaticKey "x"] (ELit (NixInt 1))] (EVar "x")
         in assertEqual "ELet" expr expr,
      runTest "attrs" $
        let expr = EAttrs False [NamedBinding [StaticKey "a"] (ELit (NixInt 1))]
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
      runTest "store path to filepath" $
        assertEqual
          "storePathToFilePath"
          "/nix/store/s66mzxpvicwk07gjbjfw9izjfa797vsw-hello-2.12.1"
          (storePathToFilePath defaultStoreDir sp),
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
-- Tests: Eval — Literals
-- ---------------------------------------------------------------------------

testEvalLiterals :: IO [Bool]
testEvalLiterals = do
  putStrLn "eval/literals"
  sequence
    [ runTest "empty env" $
        assertEqual "emptyEnv" 0 (length (envBindings emptyEnv)),
      runTest "int" $
        assertEval "int" "42" (VInt 42),
      runTest "float" $
        assertEval "float" "3.14" (VFloat 3.14),
      runTest "bool true" $
        assertEval "true" "true" (VBool True),
      runTest "null" $
        assertEval "null" "null" VNull,
      runTest "string" $
        assertEval "string" "\"hello\"" (VStr "hello")
    ]

-- ---------------------------------------------------------------------------
-- Tests: Eval — Variables
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
-- Tests: Eval — Arithmetic
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
        assertEvalFail "div0" "1 / 0"
    ]

-- ---------------------------------------------------------------------------
-- Tests: Eval — Comparison
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
-- Tests: Eval — Logic
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
-- Tests: Eval — Strings
-- ---------------------------------------------------------------------------

testEvalStrings :: IO [Bool]
testEvalStrings = do
  putStrLn "eval/strings"
  sequence
    [ runTest "string concat" $
        assertEval "concat" "\"hello\" + \" world\"" (VStr "hello world"),
      runTest "string interpolation" $
        assertEval "interp" "let x = \"world\"; in \"hello ${x}\"" (VStr "hello world"),
      runTest "interpolation coerce int" $
        assertEval "coerce-int" "\"val=${builtins.toString 42}\"" (VStr "val=42"),
      runTest "empty string" $
        assertEval "empty" "\"\"" (VStr "")
    ]

-- ---------------------------------------------------------------------------
-- Tests: Eval — If/Assert
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
-- Tests: Eval — Let
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
-- Tests: Eval — Attribute sets
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
-- Tests: Eval — Recursive attribute sets
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
-- Tests: Eval — Lists
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
-- Tests: Eval — Lambda
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
-- Tests: Eval — With
-- ---------------------------------------------------------------------------

testEvalWith :: IO [Bool]
testEvalWith = do
  putStrLn "eval/with"
  sequence
    [ runTest "with basic" $
        assertEval "with" "with { a = 1; }; a" (VInt 1),
      runTest "with lexical wins" $
        assertEval "lexical" "let a = 1; in with { a = 2; }; a" (VInt 1)
    ]

-- ---------------------------------------------------------------------------
-- Tests: Eval — Builtins
-- ---------------------------------------------------------------------------

testEvalBuiltins :: IO [Bool]
testEvalBuiltins = do
  putStrLn "eval/builtins"
  sequence
    [ runTest "typeOf int" $
        assertEval "typeOf-int" "builtins.typeOf 42" (VStr "int"),
      runTest "typeOf string" $
        assertEval "typeOf-str" "builtins.typeOf \"hi\"" (VStr "string"),
      runTest "isNull true" $
        assertEval "isNull-t" "builtins.isNull null" (VBool True),
      runTest "isNull false" $
        assertEval "isNull-f" "builtins.isNull 1" (VBool False),
      runTest "stringLength" $
        assertEval "strlen" "builtins.stringLength \"hello\"" (VInt 5),
      runTest "toString int" $
        assertEval "toStr" "builtins.toString 42" (VStr "42")
    ]

-- ---------------------------------------------------------------------------
-- Tests: Eval — Errors
-- ---------------------------------------------------------------------------

testEvalErrors :: IO [Bool]
testEvalErrors = do
  putStrLn "eval/errors"
  sequence
    [ runTest "type error in add" $
        assertEvalFail "type-add" "1 + true",
      runTest "call non-function" $
        assertEvalFail "call-non" "42 1",
      runTest "builtins.throw" $
        assertEvalFail "throw" "builtins.throw \"boom\""
    ]

-- ---------------------------------------------------------------------------
-- Tests: Eval — Higher-order builtins
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
        assertEval "map-empty" "builtins.map (x: x) [ ]" (VList []),
      -- filter
      runTest "filter match" $
        assertEval "filter" "builtins.filter (x: x == 2) [ 1 2 3 ] == [ 2 ]" (VBool True),
      runTest "filter none" $
        assertEval "filter-none" "builtins.filter (x: false) [ 1 2 ] == [ ]" (VBool True),
      -- foldl'
      runTest "foldl' sum" $
        assertEval "foldl-sum" "builtins.foldl' (a: b: a + b) 0 [ 1 2 3 ]" (VInt 6),
      runTest "foldl' string concat" $
        assertEval "foldl-str" "builtins.foldl' (a: b: a + b) \"\" [ \"x\" \"y\" \"z\" ]" (VStr "xyz"),
      runTest "foldl' empty" $
        assertEval "foldl-empty" "builtins.foldl' (a: b: a + b) 0 [ ]" (VInt 0),
      -- genList
      runTest "genList basic" $
        assertEval "genList" "builtins.genList (i: i * 2) 4 == [ 0 2 4 6 ]" (VBool True),
      runTest "genList zero" $
        assertEval "genList-0" "builtins.genList (i: i) 0" (VList []),
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
        assertEval "substr" "builtins.substring 1 2 \"hello\"" (VStr "el"),
      runTest "substring clamped" $
        assertEval "substr-clamp" "builtins.substring 3 100 \"hello\"" (VStr "lo"),
      -- concatStringsSep
      runTest "concatStringsSep" $
        assertEval "concatSep" "builtins.concatStringsSep \", \" [ \"a\" \"b\" \"c\" ]" (VStr "a, b, c"),
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
      runTest "parse simple lambda" $
        assertParse "lambda" "x: x" (ELambda (FormalName "x") (EVar "x")),
      runTest "parse set pattern lambda" $
        assertParse
          "set pattern"
          "{ a, b }: a"
          ( ELambda
              (FormalSet [Formal "a" Nothing, Formal "b" Nothing] False)
              (EVar "a")
          ),
      runTest "parse set pattern with defaults" $
        assertParse
          "defaults"
          "{ a ? 1 }: a"
          ( ELambda
              (FormalSet [Formal "a" (Just (ELit (NixInt 1)))] False)
              (EVar "a")
          ),
      runTest "parse set pattern with ellipsis" $
        assertParse
          "ellipsis"
          "{ a, ... }: a"
          ( ELambda
              (FormalSet [Formal "a" Nothing] True)
              (EVar "a")
          ),
      runTest "parse named set pattern (name@{...})" $
        assertParse
          "named set"
          "args@{ a }: a"
          ( ELambda
              (FormalNamedSet "args" [Formal "a" Nothing] False)
              (EVar "a")
          ),
      runTest "parse named set pattern ({...}@name)" $
        assertParse
          "set@name"
          "{ a }@args: a"
          ( ELambda
              (FormalNamedSet "args" [Formal "a" Nothing] False)
              (EVar "a")
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
        assertParse "empty attrs" "{ }" (EAttrs False []),
      runTest "parse attrs with binding" $
        assertParse
          "attrs"
          "{ a = 1; }"
          (EAttrs False [NamedBinding [StaticKey "a"] (ELit (NixInt 1))]),
      runTest "parse rec attrs" $
        assertParse
          "rec attrs"
          "rec { a = 1; }"
          (EAttrs True [NamedBinding [StaticKey "a"] (ELit (NixInt 1))]),
      runTest "parse inherit" $
        assertParse
          "inherit"
          "{ inherit x y; }"
          (EAttrs False [Inherit Nothing ["x", "y"]]),
      runTest "parse inherit from" $
        assertParse
          "inherit from"
          "{ inherit (a) x; }"
          (EAttrs False [Inherit (Just (EVar "a")) ["x"]]),
      -- Let/if/with/assert
      runTest "parse let" $
        assertParse
          "let"
          "let x = 1; in x"
          (ELet [NamedBinding [StaticKey "x"] (ELit (NixInt 1))] (EVar "x")),
      runTest "parse if-then-else" $
        assertParse
          "if"
          "if true then 1 else 2"
          (EIf (ELit (NixBool True)) (ELit (NixInt 1)) (ELit (NixInt 2))),
      runTest "parse with" $
        assertParse
          "with"
          "with a; b"
          (EWith (EVar "a") (EVar "b")),
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
          (EAttrs False [NamedBinding [StaticKey "or"] (ELit (NixInt 1))])
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
              (EBinary OpAdd (EVar "x") (EVar "y"))
          ),
      runTest "nested attr set" $
        assertParse
          "nested attrs"
          "{ a.b.c = 1; d = { e = 2; }; }"
          ( EAttrs
              False
              [ NamedBinding [StaticKey "a", StaticKey "b", StaticKey "c"] (ELit (NixInt 1)),
                NamedBinding [StaticKey "d"] (EAttrs False [NamedBinding [StaticKey "e"] (ELit (NixInt 2))])
              ]
          ),
      runTest "indented string" $
        assertRight "ind string" (parseNix "<test>" "''hello''") $ \case
          EIndStr _ -> Pass
          other -> Fail ("expected EIndStr, got: " <> T.pack (show other))
    ]

-- ---------------------------------------------------------------------------
-- Tests: Batch 1 — Trivial pure builtins + constants
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
        assertEval "discard" "builtins.unsafeDiscardStringContext \"hello\"" (VStr "hello"),
      -- unsafeDiscardOutputDependency
      runTest "discardOutputDep" $
        assertEval "discardOut" "builtins.unsafeDiscardOutputDependency \"hello\"" (VStr "hello"),
      -- baseNameOf
      runTest "baseNameOf string" $
        assertEval "baseName-str" "builtins.baseNameOf \"/foo/bar/baz\"" (VStr "baz"),
      runTest "baseNameOf path" $
        assertEval "baseName-path" "builtins.baseNameOf ./foo/bar" (VStr "bar"),
      runTest "baseNameOf no slash" $
        assertEval "baseName-flat" "builtins.baseNameOf \"filename\"" (VStr "filename"),
      runTest "baseNameOf type error" $
        assertEvalFail "baseName-err" "builtins.baseNameOf 42",
      -- dirOf
      runTest "dirOf string" $
        assertEval "dirOf-str" "builtins.dirOf \"/foo/bar/baz\"" (VStr "/foo/bar"),
      runTest "dirOf no slash" $
        assertEval "dirOf-flat" "builtins.dirOf \"filename\"" (VStr "."),
      -- concatLists
      runTest "concatLists basic" $
        assertEval "concatLists" "builtins.concatLists [ [ 1 2 ] [ 3 ] [ 4 5 ] ] == [ 1 2 3 4 5 ]" (VBool True),
      runTest "concatLists empty" $
        assertEval "concatLists-empty" "builtins.concatLists [ ]" (VList []),
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
        assertEval "storeDir" "builtins.storeDir" (VStr "/nix/store"),
      runTest "nixVersion" $
        assertEval "nixVersion" "builtins.nixVersion" (VStr "2.24.0"),
      runTest "langVersion" $
        assertEval "langVersion" "builtins.langVersion" (VInt 6),
      runTest "nixPath" $
        assertEval "nixPath" "builtins.nixPath" (VList [])
    ]

-- ---------------------------------------------------------------------------
-- Tests: Batch 2 — Arithmetic + bitwise builtins
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
-- Tests: Batch 3 — Attrset higher-order builtins
-- ---------------------------------------------------------------------------

testBatch3 :: IO [Bool]
testBatch3 = do
  putStrLn "eval/builtins-batch3"
  sequence
    [ -- mapAttrs
      runTest "mapAttrs basic" $
        assertEval "mapAttrs" "(builtins.mapAttrs (name: val: val + 1) { a = 1; b = 2; }).a" (VInt 2),
      runTest "mapAttrs name usage" $
        assertEval "mapAttrs-name" "(builtins.mapAttrs (name: val: name) { a = 1; }).a" (VStr "a"),
      runTest "mapAttrs type error" $
        assertEvalFail "mapAttrs-err" "builtins.mapAttrs (n: v: v) [ 1 ]",
      -- functionArgs
      runTest "functionArgs set pattern" $
        assertEval "funcArgs" "(builtins.functionArgs ({ a, b ? 1 }: a)).b" (VBool True),
      runTest "functionArgs no default" $
        assertEval "funcArgs-nodef" "(builtins.functionArgs ({ a, b ? 1 }: a)).a" (VBool False),
      runTest "functionArgs simple lambda" $
        assertEval "funcArgs-simple" "builtins.functionArgs (x: x)" (VAttrs Map.empty),
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
-- Tests: Batch 4 — String operations
-- ---------------------------------------------------------------------------

testBatch4 :: IO [Bool]
testBatch4 = do
  putStrLn "eval/builtins-batch4"
  sequence
    [ -- replaceStrings
      runTest "replaceStrings basic" $
        assertEval "replace" "builtins.replaceStrings [ \"o\" ] [ \"0\" ] \"foobar\"" (VStr "f00bar"),
      runTest "replaceStrings multi" $
        assertEval "replace-multi" "builtins.replaceStrings [ \"a\" \"b\" ] [ \"A\" \"B\" ] \"abc\"" (VStr "ABc"),
      runTest "replaceStrings empty from" $
        assertEval "replace-empty" "builtins.replaceStrings [ \"\" ] [ \"x\" ] \"ab\"" (VStr "xaxbx"),
      runTest "replaceStrings no match" $
        assertEval "replace-nomatch" "builtins.replaceStrings [ \"z\" ] [ \"Z\" ] \"abc\"" (VStr "abc"),
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
        assertEval "splitVer" "builtins.splitVersion \"1.2.3\" == [ \"1\" \".\" \"2\" \".\" \"3\" ]" (VBool True),
      runTest "splitVersion pre" $
        assertEval "splitVer-pre" "builtins.splitVersion \"1.2pre\" == [ \"1\" \".\" \"2\" \"pre\" ]" (VBool True),
      runTest "splitVersion type error" $
        assertEvalFail "splitVer-err" "builtins.splitVersion 42",
      -- parseDrvName
      runTest "parseDrvName basic" $
        assertEval "parseDrv" "(builtins.parseDrvName \"hello-1.2.3\").name" (VStr "hello"),
      runTest "parseDrvName version" $
        assertEval "parseDrv-ver" "(builtins.parseDrvName \"hello-1.2.3\").version" (VStr "1.2.3"),
      runTest "parseDrvName no version" $
        assertEval "parseDrv-nover" "(builtins.parseDrvName \"hello\").version" (VStr ""),
      runTest "parseDrvName type error" $
        assertEvalFail "parseDrv-err" "builtins.parseDrvName 42"
    ]

-- ---------------------------------------------------------------------------
-- Tests: Batch 5 — Serialization + hashing
-- ---------------------------------------------------------------------------

testBatch5 :: IO [Bool]
testBatch5 = do
  putStrLn "eval/builtins-batch5"
  sequence
    [ -- toJSON
      runTest "toJSON int" $
        assertEval "toJSON-int" "builtins.toJSON 42" (VStr "42"),
      runTest "toJSON string" $
        assertEval "toJSON-str" "builtins.toJSON \"hello\"" (VStr "\"hello\""),
      runTest "toJSON null" $
        assertEval "toJSON-null" "builtins.toJSON null" (VStr "null"),
      runTest "toJSON bool" $
        assertEval "toJSON-bool" "builtins.toJSON true" (VStr "true"),
      runTest "toJSON list" $
        assertEval "toJSON-list" "builtins.toJSON [ 1 2 3 ]" (VStr "[1,2,3]"),
      runTest "toJSON attrs" $
        assertEval "toJSON-attrs" "builtins.toJSON { a = 1; }" (VStr "{\"a\":1}"),
      runTest "toJSON lambda error" $
        assertEvalFail "toJSON-fn" "builtins.toJSON (x: x)",
      -- fromJSON
      runTest "fromJSON int" $
        assertEval "fromJSON-int" "builtins.fromJSON \"42\"" (VInt 42),
      runTest "fromJSON string" $
        assertEval "fromJSON-str" "builtins.fromJSON \"\\\"hello\\\"\"" (VStr "hello"),
      runTest "fromJSON null" $
        assertEval "fromJSON-null" "builtins.fromJSON \"null\"" VNull,
      runTest "fromJSON bool" $
        assertEval "fromJSON-bool" "builtins.fromJSON \"true\"" (VBool True),
      runTest "fromJSON array" $
        assertEval "fromJSON-arr" "builtins.length (builtins.fromJSON \"[1,2,3]\")" (VInt 3),
      runTest "fromJSON object" $
        assertEval "fromJSON-obj" "(builtins.fromJSON \"{\\\"a\\\": 1}\").a" (VInt 1),
      runTest "fromJSON roundtrip" $
        assertEval "fromJSON-rt" "builtins.fromJSON (builtins.toJSON { a = 1; b = [ 2 3 ]; })" (VAttrs (Map.fromList [("a", Evaluated (VInt 1)), ("b", Evaluated (VList [Evaluated (VInt 2), Evaluated (VInt 3)]))])),
      runTest "fromJSON invalid" $
        assertEvalFail "fromJSON-bad" "builtins.fromJSON \"not json\"",
      -- hashString
      runTest "hashString sha256" $
        assertEval "hash-sha256" "builtins.hashString \"sha256\" \"hello\"" (VStr "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824"),
      runTest "hashString md5" $
        assertEval "hash-md5" "builtins.hashString \"md5\" \"hello\"" (VStr "5d41402abc4b2a76b9719d911017c592"),
      runTest "hashString sha1" $
        assertEval "hash-sha1" "builtins.hashString \"sha1\" \"hello\"" (VStr "aaf4c61ddcc5e8a2dabede0f3b482cd9aea9434d"),
      runTest "hashString unknown algo" $
        assertEvalFail "hash-bad" "builtins.hashString \"sha999\" \"hello\"",
      runTest "hashString type error" $
        assertEvalFail "hash-err" "builtins.hashString \"sha256\" 42"
    ]

-- ---------------------------------------------------------------------------
-- Tests: Batch 6 — tryEval + deepSeq
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
      -- tryEval catches type error
      runTest "tryEval catches type error" $
        assertEval "tryEval-tyerr" "(builtins.tryEval (1 + \"a\")).success" (VBool False),
      -- deepSeq
      runTest "deepSeq returns second" $
        assertEval "deepSeq" "builtins.deepSeq [ 1 2 3 ] 42" (VInt 42),
      runTest "deepSeq forces nested" $
        assertEvalFail "deepSeq-err" "builtins.deepSeq [ (builtins.throw \"boom\") ] 42",
      runTest "deepSeq forces attrs" $
        assertEvalFail "deepSeq-attr" "builtins.deepSeq { a = builtins.throw \"boom\"; } 42"
    ]

-- ---------------------------------------------------------------------------
-- Tests: Batch 7 — genericClosure
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
        assertEval "typeof-import" "builtins.typeOf import" (VStr "lambda"),
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
    runEvalIO st (eval (builtinEnv (esTimestamp st)) expr)

-- | Run a named IO eval test — single label, no double-wrapping.
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
        runTestIOFail "import nonexistent → error" testDir "import ./nonexistent.nix",
        runTestIO "import attrset + select" testDir "(import ./attrset.nix).x" (VInt 1),
        runTestIO "import let/lambda" testDir "import ./uses-arg.nix" (VInt 15),
        -- import rejects strings (real Nix semantics)
        runTestIOFail "import rejects string" testDir "import \"./literal.nix\"",
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
          (VStr "42"),
        runTestIOFail
          "readFile missing → error"
          testDir
          ("builtins.readFile " <> nixQuotedPath (testDir </> "ghost.nix")),
        -- readDir: entries have correct file types
        runTestIO
          "readDir classifies directory"
          testDir
          ("(builtins.readDir " <> nixQuotedPath testDir <> ").sub")
          (VStr "directory"),
        runTestIO
          "readDir classifies regular file"
          testDir
          ("builtins.getAttr \"literal.nix\" (builtins.readDir " <> nixQuotedPath testDir <> ")")
          (VStr "regular")
      ]

-- ---------------------------------------------------------------------------
-- Tests: Batch A — getEnv, currentTime, toPath
-- ---------------------------------------------------------------------------

testBatchA :: IO [Bool]
testBatchA = do
  putStrLn "eval/builtins-batchA"
  sequence
    [ -- getEnv
      runTest "getEnv pure returns empty" $
        assertEval "getEnv-pure" "builtins.getEnv \"HOME\"" (VStr ""),
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
        assertEval "currentTime" "builtins.typeOf builtins.currentTime" (VStr "int"),
      runTest "currentTime is 0 in pure" $
        assertEval "currentTime-pure" "builtins.currentTime" (VInt 0),
      runTest "currentTime >= 0" $
        assertEval "currentTime-pos" "builtins.currentTime >= 0" (VBool True)
    ]

-- ---------------------------------------------------------------------------
-- Tests: Batch A — IO tests (getEnv)
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
              VStr s -> if T.null s then Fail "PATH was empty" else Pass
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
-- Tests: Batch B — placeholder, storePath
-- ---------------------------------------------------------------------------

testBatchB :: IO [Bool]
testBatchB = do
  putStrLn "eval/builtins-batchB"
  sequence
    [ -- placeholder
      runTest "placeholder out starts with /nix/store/" $
        assertRight "placeholder-prefix" (evalNix "builtins.placeholder \"out\"") $ \val ->
          case val of
            VPath p -> if "/nix/store/" `T.isPrefixOf` p then Pass else Fail ("bad prefix: " <> p)
            _ -> Fail ("expected VPath, got " <> T.pack (show val)),
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
-- Tests: Batch C — findFile
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
-- Tests: Batch D — toFile
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
-- Tests: Batch E — scopedImport
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
-- Tests: Batch F — fetchurl, fetchTarball, fetchGit
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
-- Tests: Batch G — ATerm serialization
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
-- Tests: Batch H — derivation
-- ---------------------------------------------------------------------------

testBatchH :: IO [Bool]
testBatchH = do
  putStrLn "eval/builtins-batchH"
  sequence
    [ runTest "derivation has type" $
        assertEval
          "drv-type"
          "let d = derivation { name = \"hello\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; }; in d.type"
          (VStr "derivation"),
      runTest "derivation has drvPath" $
        assertRight "drv-drvPath" (evalNix "let d = derivation { name = \"hello\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; }; in d.drvPath") $ \val ->
          case val of
            VPath p -> if "/nix/store/" `T.isPrefixOf` p && ".drv" `T.isSuffixOf` p then Pass else Fail ("bad drvPath: " <> p)
            _ -> Fail ("expected VPath, got " <> T.pack (show val)),
      runTest "derivation has outPath" $
        assertRight "drv-outPath" (evalNix "let d = derivation { name = \"hello\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; }; in d.outPath") $ \val ->
          case val of
            VPath p -> if "/nix/store/" `T.isPrefixOf` p then Pass else Fail ("bad outPath: " <> p)
            _ -> Fail ("expected VPath, got " <> T.pack (show val)),
      runTest "derivation missing name" $
        assertEvalFail "drv-noname" "derivation { system = \"x86_64-linux\"; builder = \"/bin/sh\"; }",
      runTest "derivation missing system" $
        assertEvalFail "drv-nosys" "derivation { name = \"hello\"; builder = \"/bin/sh\"; }",
      runTest "derivation missing builder" $
        assertEvalFail "drv-nobuilder" "derivation { name = \"hello\"; system = \"x86_64-linux\"; }",
      runTest "derivation type error" $
        assertEvalFail "drv-tyerr" "derivation 42",
      runTest "derivation deterministic" $
        assertRight "drv-det" (evalNix "let d1 = derivation { name = \"a\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; }; d2 = derivation { name = \"a\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; }; in d1.drvPath == d2.drvPath") $ \val ->
          assertEqual "deterministic" (VBool True) val
    ]

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "nova-nix test suite"
  putStrLn "==================="
  results <-
    concat
      <$> sequence
        [ testExprTypes,
          testStorePaths,
          testDerivation,
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
          testBatchD,
          testBatchE,
          testBatchEIO,
          testBatchF,
          testBatchG,
          testBatchH
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
