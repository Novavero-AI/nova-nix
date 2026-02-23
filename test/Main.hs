{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Data.Text (Text)
import qualified Data.Text as T
import Nix.Derivation (Platform (..), currentPlatform)
import Nix.Eval (Env (..), emptyEnv)
import Nix.Expr.Types
import Nix.Parser (parseNix)
import Nix.Parser.Lexer (Located (..), Token (..), tokenize)
import Nix.Store.Path (StoreDir (..), StorePath (..), defaultStoreDir, storePathToFilePath, windowsStoreDir)
import System.Exit (exitFailure, exitSuccess)
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
-- Tests: Eval (existing)
-- ---------------------------------------------------------------------------

testEval :: IO [Bool]
testEval = do
  putStrLn "eval"
  sequence
    [ runTest "empty env" $
        assertEqual "emptyEnv" 0 (length ((\(Nix.Eval.Env m) -> m) emptyEnv))
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
          testEval,
          testLexer,
          testParserExprs,
          testParserErrors,
          testParserIntegration
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
