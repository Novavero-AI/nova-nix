{-# LANGUAGE LambdaCase #-}

-- | Tests of the lexer, the parser and scope resolution.
module Test.Parser
  ( testExprTypes,
    testLexer,
    testLineEndings,
    testParserExprs,
    testParserErrors,
    testSourceBytes,
    testStringAttrKeys,
    testParserIntegration,
    testCurPos,
    testStaticGlobalsSync,
    testUndefinedVariables,
    testUndefinedVariablesIO,
    testSourceBytesIO,
  )
where

import Control.Exception (bracket_, evaluate)
import Control.Monad (when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Word (Word8)
import Foreign.Ptr (castPtr)
import Nix.Builtins (builtinEnv, rootScopeNames)
import Nix.Eval (NixValue (..), currentSystemStr, emptyContext, unrestrictedPolicy)
import Nix.Eval.CEnv (cenvNameTable)
import Nix.Eval.Types (AttrSet (..), Env (..), attrSetKeys)
import Nix.Expr.Resolve (impureOnlyGlobalNames, staticGlobalNames)
import Nix.Expr.Types
import Nix.Parser (ParseError (..), SourceError (..), parseNix, parsedExpr, parsedNames)
import Nix.Parser.Lexer (Located (..), Token (..), tokenize)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getTemporaryDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.Timeout (timeout)
import Test.Harness (TestResult (..), andThen, assertEqual, assertEval, assertEvalError, assertEvalFail, assertLeft, assertParse, assertParseFail, assertParseIn, assertRight, emits, evalNix, evalNixBytes, evalNixIO, evalNixWith, jsonInFile, nixQuotedPath, positionJson, positionsFile, purePolicy, runTest, runTestIO, runTestM, testBaseDir, testOrigin, testPos, upstreamBaseEnvNames, walkWatchdogMicros)
import qualified Test.QuickCheck as QC

-- ---------------------------------------------------------------------------
-- Test harness
-- ---------------------------------------------------------------------------

-- | Helper: extract just token types from Located list (drop positions and EOF).
tokenTypes :: [Located] -> [Token]
tokenTypes = filter (/= TokEOF) . map locToken

-- | The syntax error a source fails to parse with.
syntaxErrorOf :: Text -> Either Text ParseError
syntaxErrorOf = syntaxErrorOfBytes . TE.encodeUtf8

-- | 'syntaxErrorOf' over source bytes, which need not be UTF-8.
syntaxErrorOfBytes :: BS.ByteString -> Either Text ParseError
syntaxErrorOfBytes source = case parseNix testBaseDir testOrigin source of
  Left (SyntaxError err) -> Right err
  Left other -> Left ("expected a syntax error, got " <> T.pack (show other))
  Right _ -> Left "parsed unexpectedly"

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
        let expr = ELet [NamedBinding (testPos 1 5) (StaticKey "x") (ELit (NixInt 1))] (EVar "x") NoCaptureInfo
         in assertEqual "ELet" expr expr,
      runTest "attrs" $
        let expr = EAttrs False [NamedBinding (testPos 1 3) (StaticKey "a") (ELit (NixInt 1))] NoCaptureInfo
         in assertEqual "EAttrs" expr expr,
      runTest "if-then-else" $
        let expr = EIf (ELit (NixBool True)) (ELit (NixInt 1)) (ELit (NixInt 2))
         in assertEqual "EIf" expr expr
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
        let rejected = case tokenize "<test>" (BS8.replicate 1000000 '9') of
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
        let rounded = case tokenize "<test>" ("1." <> BS8.replicate 1000000 '9' <> "e5") of
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
      -- The opener is '' with an optional run of spaces and one newline
      -- (upstream lexer.l); a tab is not part of it.
      runTest "indented string opener consumes trailing spaces and newline" $
        assertRight "lex ind opener" (tokenize "<test>" "''  \n  a''") $ \toks ->
          assertEqual "tokens" [TokIndStringOpen, TokStringLit "  a", TokIndStringClose] (tokenTypes toks),
      runTest "indented string opener stops at a tab" $
        assertRight "lex ind opener tab" (tokenize "<test>" "''\t\na''") $ \toks ->
          assertEqual "tokens" [TokIndStringOpen, TokStringLit "\t\na", TokIndStringClose] (tokenTypes toks),
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
            (tokenTypes toks),
      runTest "a string keeps bytes that are not UTF-8" $
        assertRight "lex invalid UTF-8" (tokenize "<test>" (BS.pack [0x22, 0x61, 0xFF, 0xC3, 0x28, 0x22])) $ \toks ->
          assertEqual "tokens" [TokStringOpen, TokStringLit (BS.pack [0x61, 0xFF, 0xC3, 0x28]), TokStringClose] (tokenTypes toks),
      -- Upstream's column is one past a position's byte offset from the
      -- start of its line (PosTable::operator[], nixexpr.cc at 2.24.9), so
      -- the e-acute's two bytes take two columns and x is at 6.
      runTest "columns count bytes" $
        assertRight "lex columns" (tokenize "<test>" (TE.encodeUtf8 "\"\233\" x")) $ \toks ->
          assertEqual "x" [(1, 6)] [(locLine tok, locCol tok) | tok <- toks, locToken tok == TokIdent "x"],
      runTest "a syntax error's column counts the bytes before it" $
        assertRight "error column" (syntaxErrorOf "\"\233\" )") $ \err ->
          assertEqual "position" (1, 6) (peLine err, peCol err),
      runTest "an escaped newline in an indented string ends a line" $
        assertRight "lex escaped newline" (tokenize "<test>" "''a''\\\n'' x") $ \toks ->
          assertEqual "x" [(2, 4)] [(locLine tok, locCol tok) | tok <- toks, locToken tok == TokIdent "x"],
      -- Upstream's ID class is ASCII (lexer.l), so a letter outside it is
      -- the character no rule but the catch-all takes.
      runTest "a letter outside ASCII starts no token" $
        assertEqual "message" (Left "unexpected character: \233") (lexOutcome (TE.encodeUtf8 "\233")),
      runTest "a byte that is no character is named as a byte" $
        assertEqual "message" (Left "unexpected byte 0xff") (lexOutcome (BS.pack [0xFF])),
      -- Upstream's whitespace is [ \t\r\n] (lexer.l).
      runTest "a vertical tab is not whitespace" $
        assertEqual "message" (Left "unexpected character: \v") (lexOutcome "1\v")
    ]
  where
    lexOutcome source = either (Left . peMessage) (Right . length) (tokenize "<test>" source)

-- ---------------------------------------------------------------------------
-- Tests: Line endings
-- ---------------------------------------------------------------------------

-- | A line ends at an LF, a CRLF or a bare CR, as upstream's position
-- table splits a source (@Pos::LinesIterator@, position.cc at 2.24.9), and
-- a line comment ends before the first CR or LF (@\#[^\\r\\n]*@, lexer.l).
testLineEndings :: IO [Bool]
testLineEndings = do
  putStrLn "parser/line-endings"
  let quickCheckPasses property = do
        result <- QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} property
        pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))
      positionsIn label source expected =
        assertRight label (tokenize "<test>" source) $ \toks ->
          assertEqual label expected [(locLine tok, locCol tok) | tok <- toks, locToken tok /= TokEOF]
      xAt label source expected =
        assertRight label (tokenize "<test>" source) $ \toks ->
          assertEqual label [expected] [(locLine tok, locCol tok) | tok <- toks, locToken tok == TokIdent "x"]
  sequence
    [ runTest "a line comment ends at a carriage return" $
        assertEval "comment" "# c\r1" (VInt 1),
      runTest "a line comment's CRLF ends one line" $
        xAt "comment crlf" "# c\r\nx" (2, 1),
      runTest "an LF, a CRLF and a bare CR each end one line" $
        positionsIn "endings" "a\nb\r\nc\rd" [(1, 1), (2, 1), (3, 1), (4, 1)],
      runTest "an LF before a CR ends a line of its own" $
        xAt "lf cr" "\n\rx" (3, 1),
      runTest "a bare CR ends a line in a block comment" $
        xAt "block comment" "/* a\r */ x" (2, 5),
      runTest "a bare CR ends a line in a double-quoted string" $
        xAt "string" "\"a\rb\" x" (2, 4),
      runTest "a bare CR ends a line in an indented string" $
        xAt "indented string" "''a\rb'' x" (2, 5),
      runTest "an escaped bare CR ends a line" $
        xAt "escaped cr" "\"\\\r\" x" (2, 3),
      -- The escape takes the CR and the string's next run its LF; the two
      -- are still one line ending.
      runTest "an escaped CR and the LF after it end one line" $
        xAt "escaped crlf" "\"\\\r\n\" x" (2, 3),
      runTest "an indented string's escaped CR and the LF after it end one line" $
        xAt "indented escaped crlf" "''a''\\\r\n'' x" (2, 4),
      -- Upstream's opener is @''(\ *\n)?@ (lexer.l): its newline is an LF,
      -- so after a CR the opening line is content.
      runTest "a CRLF after an indented string's opener is content" $
        assertRight "opener crlf" (tokenize "<test>" "''\r\na''") $ \toks ->
          assertEqual "tokens" [TokIndStringOpen, TokStringLit "\r\na", TokIndStringClose] (tokenTypes toks),
      -- Upstream puts an end-of-file syntax error at the end of the input
      -- (yyerror, parser.y at 2.24.9), which a trailing comment runs to.
      runTest "the end of input after a line comment is past the comment" $
        assertRight "eof after comment" (syntaxErrorOf "(\n# c") $ \err ->
          assertEqual "position" (2, 4) (peLine err, peCol err),
      runTestM "a source's tokens stand on the same lines whichever line endings it uses" $
        quickCheckPasses $
          QC.forAll genLineEndingSource $ \pieces ->
            case tokensOutsideStrings (renderSource (map withLf pieces)) of
              Left err -> QC.counterexample (show err) False
              Right expected -> tokensOutsideStrings (renderSource pieces) QC.=== Right expected
    ]
  where
    -- A string's content is not compared: an indented string keeps its
    -- CRs, and the content after an opener starts on the opening line
    -- unless an LF ends it.
    tokensOutsideStrings source = do
      toks <- tokenize "<test>" source
      pure [(locToken tok, locLine tok, locCol tok) | tok <- toks, not (isStringContent (locToken tok))]
    isStringContent (TokStringLit _) = True
    isStringContent (TokStringEsc _) = True
    isStringContent _ = False
    withLf (SourceLineEnding _) = SourceLineEnding Lf
    withLf piece = piece

-- | A line ending of the kind upstream's position table splits at.
data LineEnding = Lf | CrLf | Cr
  deriving (Show)

-- | A piece of a generated source: bytes, or a line ending.
data SourcePiece = SourceBytes !BS.ByteString | SourceLineEnding !LineEnding
  deriving (Show)

-- | The bytes of a generated source.
renderSource :: [SourcePiece] -> BS.ByteString
renderSource = BS.concat . map pieceBytes
  where
    pieceBytes (SourceBytes bytes) = bytes
    pieceBytes (SourceLineEnding Lf) = "\n"
    pieceBytes (SourceLineEnding CrLf) = "\r\n"
    pieceBytes (SourceLineEnding Cr) = "\r"

-- | Sources whose line endings sit wherever the lexer can meet one:
-- between tokens, at a line comment's end, in a block comment, in a
-- double-quoted and an indented string, after an indented string's
-- opener, and escaped in either string.  Each fragment ends at a space,
-- so no two fragments run together into another token, and a byte other
-- than LF follows each line ending, so no bare CR and the LF after it
-- read as one CRLF.
genLineEndingSource :: QC.Gen [SourcePiece]
genLineEndingSource = concat <$> QC.listOf (QC.oneof (map sequence fragments))
  where
    ending = SourceLineEnding <$> QC.elements [Lf, CrLf, Cr]
    bytes = pure . SourceBytes
    fragments =
      [ [bytes "x "],
        [bytes "1 "],
        [bytes "+ "],
        [bytes "\"\195\169\" "],
        [ending, bytes " "],
        [bytes "# c", ending, bytes " "],
        [bytes "/* c", ending, bytes "c */ "],
        [bytes "\"s", ending, bytes "s\" "],
        [bytes "\"\\", ending, bytes "\" "],
        [bytes "''", ending, bytes "s'' "],
        [bytes "''s", ending, bytes "s'' "],
        [bytes "''s''\\", ending, bytes "'' "]
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
        assertParseIn ["x"] "var" "x" (EVar "x"),
      runTest "parse empty string" $
        assertParse "empty string" "\"\"" (EStr []),
      runTest "parse string literal" $
        assertParse "string" "\"hello\"" (EStr [StrLit "hello"]),
      runTest "parse string interpolation" $
        assertParseIn
          ["name"]
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
        assertParseIn
          ["a", "b", "c"]
          "impl"
          "a -> b -> c"
          (EBinary OpImpl (EVar "a") (EBinary OpImpl (EVar "b") (EVar "c"))),
      runTest "parse concat right" $
        assertParseIn
          ["a", "b", "c"]
          "concat"
          "a ++ b ++ c"
          (EBinary OpConcat (EVar "a") (EBinary OpConcat (EVar "b") (EVar "c"))),
      runTest "parse update right" $
        assertParseIn
          ["a", "b", "c"]
          "update"
          "a // b // c"
          (EBinary OpUpdate (EVar "a") (EBinary OpUpdate (EVar "b") (EVar "c"))),
      -- Upstream's grammar binds -, then ?, then ++ tighter than * and +.
      runTest "negation binds tighter than ?" $
        assertParseIn ["a"] "negate over has-attr" "-a ? b" (EHasAttr (EUnary OpNegate (EVar "a")) [StaticKey "b"]),
      runTest "negation's operand is an application" $
        assertParseIn ["f", "x"] "negate an application" "-f x ? y" (EHasAttr (EUnary OpNegate (EApp (EVar "f") (EVar "x"))) [StaticKey "y"]),
      runTest "? binds tighter than ++" $
        assertParseIn ["a", "b"] "has-attr over concat" "a ++ b ? c" (EBinary OpConcat (EVar "a") (EHasAttr (EVar "b") [StaticKey "c"])),
      runTest "++ binds tighter than * and +" $
        assertParseIn
          ["a", "b", "c", "d"]
          "concat over arithmetic"
          "a * b ++ c + d"
          (EBinary OpAdd (EBinary OpMul (EVar "a") (EBinary OpConcat (EVar "b") (EVar "c"))) (EVar "d")),
      -- A prefix operator's operand runs to the end of the operators
      -- tighter than it, wherever the prefix operator stands.
      runTest "! as the operand of a tighter operator takes in the operators tighter than !" $
        assertParseIn
          ["a", "b", "c"]
          "not under add"
          "a + !b + c"
          (EBinary OpAdd (EVar "a") (EUnary OpNot (EBinary OpAdd (EVar "b") (EVar "c")))),
      runTest "! binds tighter than //" $
        assertParseIn ["a", "b"] "not over update" "!a // b" (EBinary OpUpdate (EUnary OpNot (EVar "a")) (EVar "b")),
      runTest "negation of ! takes in what ! takes in" $
        assertParseIn ["a", "b"] "negate a not" "-!a + b" (EUnary OpNegate (EUnary OpNot (EBinary OpAdd (EVar "a") (EVar "b")))),
      runTest "operators that group neither way do not chain" $
        foldr
          andThen
          Pass
          [assertParseFail source source | source <- ["1 == 2 == 3", "1 == 2 != 3", "1 < 2 > 3", "1 == 2 < 3 == 4"]],
      runTest "a second ? applies to the first's whole" $
        assertParseIn ["a"] "has-attr twice" "a ? b ? c" (EHasAttr (EHasAttr (EVar "a") [StaticKey "b"]) [StaticKey "c"]),
      runTest "a negated number has no attribute, as upstream reads -5 ? a" $
        assertEval "negate has-attr" "-5 ? a" (VBool False),
      runTest "! under a tighter operator parses, and an unforced binding of it never fails" $
        assertEval "not under add, unforced" "let x = 1 + !true; in 2" (VInt 2),
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
        assertParseIn ["f", "x"] "app" "f x" (EApp (EVar "f") (EVar "x")),
      runTest "parse left-assoc application" $
        assertParseIn ["f", "x", "y"] "app left" "f x y" (EApp (EApp (EVar "f") (EVar "x")) (EVar "y")),
      runTest "parse application with parens" $
        assertParseIn
          ["f"]
          "app parens"
          "f (1 + 2)"
          (EApp (EVar "f") (EBinary OpAdd (ELit (NixInt 1)) (ELit (NixInt 2)))),
      -- Select
      runTest "parse select" $
        assertParseIn ["a"] "select" "a.b" (ESelect (EVar "a") [StaticKey "b"] Nothing),
      runTest "parse nested select" $
        assertParseIn
          ["a"]
          "nested select"
          "a.b.c"
          (ESelect (EVar "a") [StaticKey "b", StaticKey "c"] Nothing),
      runTest "parse select or default" $
        assertParseIn
          ["a"]
          "select or"
          "a.b or 1"
          (ESelect (EVar "a") [StaticKey "b"] (Just (ELit (NixInt 1)))),
      runTest "parse has-attr" $
        assertParseIn ["a"] "has-attr" "a ? b" (EHasAttr (EVar "a") [StaticKey "b"]),
      -- Attr sets
      runTest "parse empty attrs" $
        assertParse "empty attrs" "{ }" (EAttrs False [] NoCaptureInfo),
      runTest "parse attrs with binding" $
        assertParse
          "attrs"
          "{ a = 1; }"
          (EAttrs False [NamedBinding (testPos 1 3) (StaticKey "a") (ELit (NixInt 1))] NoCaptureInfo),
      runTest "parse rec attrs" $
        assertParse
          "rec attrs"
          "rec { a = 1; }"
          (EAttrs True [NamedBinding (testPos 1 7) (StaticKey "a") (ELit (NixInt 1))] NoCaptureInfo),
      -- inherit x y; is one binding per name, each holding the variable it
      -- copies, which resolution binds like any other variable.
      runTest "parse inherit" $
        assertParseIn
          ["x", "y"]
          "inherit"
          "{ inherit x y; }"
          (EAttrs False [Inherit (testPos 1 11) "x" (EVar "x"), Inherit (testPos 1 13) "y" (EVar "y")] NoCaptureInfo),
      runTest "parse inherit from" $
        assertParseIn
          ["a"]
          "inherit from"
          "{ inherit (a) x; }"
          (EAttrs False [InheritFrom (EVar "a") [InheritedName (testPos 1 15) "x"]] NoCaptureInfo),
      -- Let/if/with/assert
      runTest "parse let" $
        assertParse
          "let"
          "let x = 1; in x"
          (ELet [NamedBinding (testPos 1 5) (StaticKey "x") (ELit (NixInt 1))] (EResolvedVar 0 0) NoCaptureInfo),
      runTest "parse if-then-else" $
        assertParse
          "if"
          "if true then 1 else 2"
          (EIf (ELit (NixBool True)) (ELit (NixInt 1)) (ELit (NixInt 2))),
      runTest "parse with" $
        assertParseIn
          ["a"]
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
        assertParseIn ["or"] "or ident" "or" (EVar "or"),
      -- 'or' as attr key
      runTest "or as attr key" $
        assertParse
          "or attr key"
          "{ or = 1; }"
          (EAttrs False [NamedBinding (testPos 1 3) (StaticKey "or") (ELit (NixInt 1))] NoCaptureInfo)
    ]

-- ---------------------------------------------------------------------------
-- Tests: Parser errors
-- ---------------------------------------------------------------------------

testParserErrors :: IO [Bool]
testParserErrors = do
  putStrLn "parser/errors"
  sequence
    [ runTest "empty input" $
        assertLeft "empty" (parseNix testBaseDir testOrigin ""),
      runTest "unclosed paren" $
        assertLeft "unclosed paren" (parseNix testBaseDir testOrigin "(1"),
      runTest "unclosed string" $
        assertLeft "unclosed string" (parseNix testBaseDir testOrigin "\"hello"),
      runTest "unclosed brace" $
        assertLeft "unclosed brace" (parseNix testBaseDir testOrigin "{ a = 1;"),
      runTest "missing semicolon" $
        assertLeft "missing semi" (parseNix testBaseDir testOrigin "{ a = 1 }"),
      runTest "unclosed bracket" $
        assertLeft "unclosed bracket" (parseNix testBaseDir testOrigin "[ 1 2"),
      runTest "unexpected token" $
        assertLeft "unexpected" (parseNix testBaseDir testOrigin ")"),
      -- A failure deep in a lambda body is reported where it happened:
      -- a parsed lambda header commits, instead of backtracking into
      -- the attr-set reading and reporting its early stumble (which would
      -- put a real error at line 410 of a nixpkgs file at line 2).
      runTest "deep formals-lambda body error reports its own line" $
        assertRight "deep body" (syntaxErrorOf "{ a, b }:\n{\n  x = 1;\n  y = (;\n}") $ \err ->
          assertEqual "error line" 4 (peLine err),
      runTest "simple-lambda body error reports its own line" $
        assertRight "simple body" (syntaxErrorOf "x:\n(;") $ \err ->
          assertEqual "error line" 2 (peLine err),
      -- The empty attr set is a successful PREFIX parse of "{ }: body",
      -- so without commitment the outer parser complained at the colon.
      runTest "empty-formals lambda body error reports its own line" $
        assertRight "empty formals body" (syntaxErrorOf "{ }:\n(;") $ \err ->
          assertEqual "error line" 2 (peLine err),
      -- When neither reading parses, the deeper failure wins: the
      -- malformed formal at column 7, not the attr-set branch's
      -- stumble over the first comma at column 4.
      runTest "a malformed formal beats the attr-set branch's earlier error" $
        assertRight "malformed formal" (syntaxErrorOf "{ a, b.c }: x") $ \err ->
          assertEqual "deeper than the attr-set stumble" True ((peLine err, peCol err) > (1, 4)),
      -- End-of-input failures carry no position and must rank as the
      -- furthest a branch can get.
      runTest "truncated lambda reports end of input" $
        assertRight "truncated lambda" (syntaxErrorOf "{ a, b }:") $ \err ->
          assertEqual "end of input" True ("end of input" `T.isInfixOf` peMessage err),
      -- Upstream's attribute name is an identifier or or (the attr rule of
      -- parser.y), so its grammar refuses any other keyword as one, at the
      -- keyword.
      runTest "a keyword other than or is no attribute name, refused where it stands" $
        let refusedAt source = (\err -> (peLine err, peCol err)) <$> syntaxErrorOf source
            cases keyword =
              [ ("{ a." <> keyword <> " = 1; }", (1, 5)),
                ("x." <> keyword, (1, 3)),
                ("x ? " <> keyword, (1, 5)),
                ("{ inherit " <> keyword <> "; }", (1, 11)),
                ("{ inherit (x) " <> keyword <> "; }", (1, 15))
              ]
                ++ [("{ " <> keyword <> " = 1; }", (1, 3)) | keyword /= "inherit"]
            sources = concatMap cases refusedAttrKeywords
         in assertEqual "positions" [(source, Right at) | (source, at) <- sources] [(source, refusedAt source) | (source, _) <- sources],
      runTest "or, true, false and null are attribute names" $
        assertEval "names" "let s = { or = 1; true = 2; false = 3; null = 4; }; in s.or + s.true + s.false + s.null" (VInt 10),
      runTest "or, true, false and null are inherited names" $
        assertEval "inherited" "let s = { inherit ({ or = 1; true = 2; false = 3; null = 4; }) or true false null; }; in s.or + s.true + s.false + s.null" (VInt 10),
      runTest "a keyword written as a string is an attribute name" $
        assertEval "quoted" "let s = { \"if\" = 1; inherit ({ \"rec\" = 2; }) \"rec\"; }; in s.\"if\" + s.\"rec\"" (VInt 3)
    ]

-- | The keywords upstream's lexer makes tokens of their own (its rules
-- from @if@ to @inherit@ in lexer.l), none of which its grammar takes as
-- an attribute name.
refusedAttrKeywords :: [Text]
refusedAttrKeywords = ["if", "then", "else", "assert", "with", "let", "in", "rec", "inherit"]

-- ---------------------------------------------------------------------------
-- Tests: Source bytes
-- ---------------------------------------------------------------------------

-- | A source is bytes, as upstream's lexer reads it (lexer.l at 2.24.9): a
-- string literal's bytes are its value's whether or not they are UTF-8,
-- every other token is ASCII, and a byte order mark selects UTF-16, read
-- as its UTF-8.
testSourceBytes :: IO [Bool]
testSourceBytes = do
  putStrLn "parser/source-bytes"
  let quickCheckPasses property = do
        result <- QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} property
        pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))
      -- NUL is left out: upstream's unescapeStr stops at one (lexer.l), so
      -- a literal holding it is no round trip there.
      literalBytes = BS.pack <$> QC.listOf (QC.choose (1, maxBound :: Word8))
      literalText = T.pack <$> QC.listOf (QC.arbitrary `QC.suchThat` (/= '\NUL'))
      -- An LF would end the line; 'stringLiteralText' escapes a CR.
      lineText = T.pack <$> QC.listOf (QC.arbitrary `QC.suchThat` (/= '\n'))
      utf16Le = (BS.pack [0xFF, 0xFE] <>) . TE.encodeUtf16LE
      utf16Be = (BS.pack [0xFE, 0xFF] <>) . TE.encodeUtf16BE
      errorAt err = (peLine err, peCol err, peMessage err)
  sequence
    [ runTestM "a string literal's bytes are its value's" $
        quickCheckPasses $
          QC.forAll literalBytes $ \bytes ->
            evalNixBytes (stringLiteralBytes bytes) QC.=== Right (VStr bytes emptyContext),
      runTestM "a UTF-16 source is read as its UTF-8, in either byte order" $
        quickCheckPasses $
          QC.forAll literalText $ \text ->
            let expected = Right (VStr (TE.encodeUtf8 text) emptyContext)
             in evalNixBytes (utf16Le (stringLiteralText text)) QC.=== expected
                  QC..&&. evalNixBytes (utf16Be (stringLiteralText text)) QC.=== expected,
      runTest "an indented string keeps bytes that are not UTF-8" $
        assertEqual "value" (Right (VStr (BS.pack [0x61, 0xFF, 0x62]) emptyContext)) (evalNixBytes ("''" <> BS.pack [0x61, 0xFF, 0x62] <> "''")),
      runTest "a comment may hold bytes that are not UTF-8" $
        assertEqual "value" (Right (VInt 1)) (evalNixBytes ("# " <> BS.pack [0xFF] <> "\n/* " <> BS.pack [0xC3, 0x28] <> " */ 1")),
      -- Upstream's PATH_CHAR is ASCII (lexer.l): the byte ends the path
      -- and starts no token of its own.
      runTest "a byte outside ASCII ends a path" $
        assertEqual "error" (Right (1, 4, "unexpected byte 0xff")) (errorAt <$> syntaxErrorOfBytes ("./a" <> BS.pack [0xFF] <> "b")),
      runTest "a string attribute name must be UTF-8" $
        assertEqual "error" (Right (1, 3, "attribute name is not valid UTF-8")) (errorAt <$> syntaxErrorOfBytes ("{ \"" <> BS.pack [0xFF] <> "\" = 1; }")),
      -- A dot commits to the key after it, so the key's error is the one
      -- reported, not a complaint about the dot.
      runTest "a string attribute name after a dot must be UTF-8" $
        assertEqual "error" (Right (1, 5, "attribute name is not valid UTF-8")) (errorAt <$> syntaxErrorOfBytes ("{ a.\"" <> BS.pack [0xFF] <> "\" = 1; }")),
      runTest "a string name after a dot in a has-attribute path must be UTF-8" $
        assertEqual "error" (Right (1, 9, "attribute name is not valid UTF-8")) (errorAt <$> syntaxErrorOfBytes ("{ } ? a.\"" <> BS.pack [0xFF] <> "\"")),
      runTest "a quoted inherit name must be UTF-8" $
        assertEqual "error" (Right (1, 17, "attribute name is not valid UTF-8")) (errorAt <$> syntaxErrorOfBytes ("{ inherit ({ }) \"" <> BS.pack [0xFF] <> "\"; }")),
      runTest "a UTF-8 byte order mark is dropped" $
        assertEqual "value" (Right (VInt 1)) (evalNixBytes (BS.pack [0xEF, 0xBB, 0xBF] <> "1")),
      runTest "an unpaired UTF-16 surrogate is a syntax error where it stands" $
        assertEqual "error" (Right (2, 3, "unpaired UTF-16 surrogate")) (errorAt <$> syntaxErrorOfBytes (utf16Le "1\n +" <> BS.pack [0x00, 0xD8])),
      runTest "a bare CR ends a line for a UTF-16 source's error" $
        assertEqual "error" (Right (2, 3, "unpaired UTF-16 surrogate")) (errorAt <$> syntaxErrorOfBytes (utf16Le "1\r +" <> BS.pack [0x00, 0xD8])),
      runTest "a UTF-16 source with an odd byte at its end is a syntax error" $
        assertEqual "error" (Right (1, 2, "UTF-16 source with an odd number of bytes")) (errorAt <$> syntaxErrorOfBytes (utf16Be "1" <> BS.pack [0x41])),
      runTest "a UTF-16 source's error column counts the UTF-8 bytes before it" $
        assertEqual "error" (Right (1, 7, "unpaired UTF-16 surrogate")) (errorAt <$> syntaxErrorOfBytes (utf16Le "\"\233\" +" <> BS.pack [0x00, 0xD8])),
      -- Upstream's column is one past a position's byte offset from the
      -- start of its line (PosTable::operator[], nixexpr.cc at 2.24.9).
      runTestM "a token's column is one past the bytes before it on its line" $
        quickCheckPasses $
          QC.forAll lineText $ \text ->
            let literal = TE.encodeUtf8 (stringLiteralText text)
                columnsOfX = map (\tok -> (locLine tok, locCol tok)) . filter ((== TokIdent "x") . locToken)
             in (columnsOfX <$> tokenize "<test>" (literal <> " x")) QC.=== Right [(1, BS.length literal + 2)]
    ]

-- | Bytes as a double-quoted literal of them: each byte with a meaning
-- there escaped ('literalEscape'), every other kept as it is.
stringLiteralBytes :: BS.ByteString -> BS.ByteString
stringLiteralBytes bytes = "\"" <> BS8.concatMap escaped bytes <> "\""
  where
    escaped c = maybe (BS8.singleton c) TE.encodeUtf8 (literalEscape c)

-- | Text as a double-quoted literal of it, escaped as 'stringLiteralBytes'.
stringLiteralText :: Text -> Text
stringLiteralText text = "\"" <> T.concatMap escaped text <> "\""
  where
    escaped c = fromMaybe (T.singleton c) (literalEscape c)

-- | The escape a character needs in a double-quoted literal: the quote,
-- the backslash, the dollar, and the carriage return, which a raw literal
-- reads as a newline (unescapeStr, lexer.l at 2.24.9).
literalEscape :: Char -> Maybe Text
literalEscape '"' = Just "\\\""
literalEscape '\\' = Just "\\\\"
literalEscape '$' = Just "\\$"
literalEscape '\r' = Just "\\r"
literalEscape _ = Nothing

-- ---------------------------------------------------------------------------
-- Tests: String attribute keys
-- ---------------------------------------------------------------------------

-- | A key written as a string or as @${ expr }@ is a name when upstream's
-- parser builds its expression as a plain string, and dynamic otherwise
-- (the @attrpath@ and @attrs@ rules of parser.y).
testStringAttrKeys :: IO [Bool]
testStringAttrKeys = do
  putStrLn "parser/string-attr-keys"
  let quickCheckPasses property = do
        result <- QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} property
        pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))
      parsedAs source = (\parsed -> (parsedNames parsed, parsedExpr parsed)) <$> parseNix testBaseDir testOrigin (TE.encodeUtf8 source)
      keyText = T.pack <$> QC.listOf (QC.arbitrary `QC.suchThat` (/= '\NUL'))
      errorAt err = (peLine err, peCol err, peMessage err)
      boundAt name = EAttrs False [NamedBinding (testPos 1 3) (StaticKey name) (ELit (NixInt 1))] NoCaptureInfo
  sequence
    [ runTest "a key of an interpolated plain string is a name" $
        assertParse "plain string" "{ ${\"a\"} = 1; }" (boundAt "a"),
      runTest "a key of a parenthesized plain string is a name" $
        assertParse "parenthesized" "{ ${(\"a\")} = 1; }" (boundAt "a"),
      runTest "a key of an interpolated URI is a name" $
        assertParse "uri" "{ ${a:b} = 1; }" (boundAt "a:b"),
      runTestM "a key interpolating a string literal is the key the literal is" $
        quickCheckPasses $
          QC.forAll keyText $ \text ->
            let literal = stringLiteralText text
             in parsedAs ("{ ${" <> literal <> "} = 1; }") QC.=== parsedAs ("{ " <> literal <> " = 1; }"),
      runTest "an interpolated name must be UTF-8" $
        assertEqual "error" (Right (1, 3, "attribute name is not valid UTF-8")) (errorAt <$> syntaxErrorOfBytes ("{ ${\"" <> BS.pack [0xFF] <> "\"} = 1; }")),
      runTest "a key of an interpolated plain string binds in a let" $
        assertEval "let" "let ${\"a\"} = 1; in a" (VInt 1),
      runTest "a key of a URI binds in a let" $
        assertEval "let uri" "let ${a:b} = 1; in 2" (VInt 2),
      runTest "a key of an interpolated plain string is inherited" $
        assertEval "inherit" "let a = 1; in { inherit ${\"a\"}; }.a" (VInt 1),
      runTest "a key of an interpolated plain string is inherited from a set" $
        assertEval "inherit from" "{ inherit ({ a = 1; }) ${\"a\"}; }.a" (VInt 1),
      runTest "a key of an interpolated plain string is in a rec set's scope" $
        assertEval "rec" "(rec { ${\"a\"} = 1; b = a; }).b" (VInt 1),
      runTest "a path through an interpolated plain string merges with the name written bare" $
        assertEval "merge" "{ ${\"a\"}.b = 1; a.c = 2; }.a.b" (VInt 1),
      runTest "an interpolated plain string naming an attribute twice is a parse error" $
        assertParseFail "duplicate" "{ b = 1; ${\"b\"} = 2; }",
      runTest "a key of an interpolated string with interpolation stays dynamic" $
        assertParseFail "interpolated" "let ${\"${\"a\"}\"} = 1; in a",
      runTest "an inherited key that is dynamic is refused at its start" $
        assertEqual "error" (Right (1, 25, "dynamic attributes not allowed in inherit")) (errorAt <$> syntaxErrorOf "let a = 1; in { inherit ${\"a\" + \"\"}; }"),
      runTest "an inherited string with interpolation is refused at its quote" $
        assertEqual "error" (Right (1, 11, "dynamic attributes not allowed in inherit")) (errorAt <$> syntaxErrorOf "{ inherit \"${\"a\"}\"; }")
    ]

-- ---------------------------------------------------------------------------
-- Tests: Parser integration
-- ---------------------------------------------------------------------------

testParserIntegration :: IO [Bool]
testParserIntegration = do
  putStrLn "parser/integration"
  sequence
    [ runTest "shell.nix pattern" $
        assertRight "shell.nix" (parsedExpr <$> parseNix testBaseDir testOrigin "{ pkgs ? import <nixpkgs> {} }: pkgs.mkShell { buildInputs = [ pkgs.ghc ]; }") $ \case
          ELambda {} -> Pass
          other -> Fail ("expected ELambda, got: " <> T.pack (show other)),
      runTest "let with multiple bindings" $
        assertParse
          "multi-let"
          "let x = 1; y = 2; in x + y"
          ( ELet
              [ NamedBinding (testPos 1 5) (StaticKey "x") (ELit (NixInt 1)),
                NamedBinding (testPos 1 12) (StaticKey "y") (ELit (NixInt 2))
              ]
              (EBinary OpAdd (EResolvedVar 0 0) (EResolvedVar 0 1))
              NoCaptureInfo
          ),
      runTest "nested attr set" $
        -- Normalization hoists a nested attrpath into nested literal sets
        -- (upstream parser.y addAttr), so a.b.c = 1 parses like
        -- a = { b = { c = 1; }; }, every level at the path's position.
        assertParse
          "nested attrs"
          "{ a.b.c = 1; d = { e = 2; }; }"
          ( EAttrs
              False
              [ NamedBinding
                  (testPos 1 3)
                  (StaticKey "a")
                  ( EAttrs
                      False
                      [ NamedBinding
                          (testPos 1 3)
                          (StaticKey "b")
                          (EAttrs False [NamedBinding (testPos 1 3) (StaticKey "c") (ELit (NixInt 1))] NoCaptureInfo)
                      ]
                      NoCaptureInfo
                  ),
                NamedBinding (testPos 1 14) (StaticKey "d") (EAttrs False [NamedBinding (testPos 1 20) (StaticKey "e") (ELit (NixInt 2))] NoCaptureInfo)
              ]
              NoCaptureInfo
          ),
      runTest "indented string" $
        assertRight "ind string" (parsedExpr <$> parseNix testBaseDir testOrigin "''hello''") $ \case
          EIndStr _ -> Pass
          other -> Fail ("expected EIndStr, got: " <> T.pack (show other)),
      runTest "indented string escape is its own part" $
        assertRight "ind string esc" (parsedExpr <$> parseNix testBaseDir testOrigin "''a'''b''") $ \case
          EIndStr [StrLit "a", StrEsc "''", StrLit "b"] -> Pass
          other -> Fail ("expected literal, escape, literal, got: " <> T.pack (show other)),
      -- Positional let/rec resolution tests
      runTest "let inherit from outer lambda" $
        assertRight "let-inherit-lambda" (parsedExpr <$> parseNix testBaseDir testOrigin "x: let inherit x; in x") $ \case
          -- x: let inherit x; in x
          -- The lambda formal x is at level 0, index 0.
          -- The let scope is level 0 (for the let body).
          -- The inherited variable resolves against the outer (lambda)
          -- scope, one level up from the let it is evaluated in.
          -- The body x resolves to level 0, index 0 (the let scope).
          ELambda _ (ELet [Inherit _ "x" _rhsExpr] (EResolvedVar 0 0) _) _ -> Pass
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
-- Tests: __curPos
-- ---------------------------------------------------------------------------

-- | Each expected value is derived from upstream's source at 2.24.9: the
-- parser reads every @__curPos@ its @expr_simple@ rule reaches as an
-- @ExprPos@ at the identifier, whatever binds the name, while the
-- variable an @inherit@ copies is an @ExprVar@ built without that check
-- (parser.y); and @ExprPos::eval@ answers @EvalState::mkPos@ of that
-- position (eval.cc), the position set in a file and null in text with
-- no file behind it (@parseExprFromString@'s @Pos::String@).  The
-- expressions put every @__curPos@ under test past line 1, so the
-- @builtins.toJSON (@ 'jsonInFile' prefixes moves no column.
testCurPos :: IO [Bool]
testCurPos = do
  putStrLn "parse/curPos"
  sequence
    [ runTest "the parser reads __curPos as a position" $
        assertParse "position node" "__curPos" (EPos (SrcPos testOrigin 1 1)),
      inFile "a file gives the position it is written at" "\n  __curPos" (2, 3),
      inFile "a position in a set is at the identifier" "{\n  a = __curPos;\n}.a" (2, 7),
      inFile "a position in a function body is at the identifier" "(x:\n  __curPos) 1" (2, 3),
      inFile "a let binding the name does not shadow it" "let __curPos = 1; in\n  __curPos" (2, 3),
      inFile "a formal of the name does not shadow it" "(__curPos:\n  __curPos) 1" (2, 3),
      inFile "a with supplying the name does not shadow it" "with { __curPos = 1; };\n  __curPos" (2, 3),
      -- A column counts bytes (PosTable::operator[], nixexpr.cc), so the
      -- e-acute takes two.
      inFile "a column counts the bytes before it" "{\n  a = \"\233\"; b = __curPos;\n}.b" (2, 17),
      -- mkPos fills the set with buildBindings' alloc, whose attributes
      -- are at noPos, which mkPos answers null for.
      jsonInFile "a position's attributes have no position" "builtins.unsafeGetAttrPos \"line\" (\n  __curPos)" "null",
      runTest "text with no file gives null" $
        assertEval "text" "__curPos" VNull,
      runTest "a let binding the name in text gives null, not the binding" $
        assertEval "text let" "let __curPos = 1; in __curPos" VNull,
      runTest "an inherited __curPos is a variable" $
        assertEval "inherit" "let __curPos = 1; in { inherit __curPos; }.__curPos" (VInt 1),
      runTest "an inherited __curPos nothing binds is undefined" $
        assertUnbound "inherit unbound" "{ inherit __curPos; }" "__curPos",
      runTest "an attribute may be named __curPos" $
        assertEval "attribute" "{ __curPos = 1; }.__curPos" (VInt 1)
    ]
  where
    inFile label source (line, column) = jsonInFile label source (positionJson positionsFile line column)

-- ---------------------------------------------------------------------------
-- Tests: resolver static globals stay in sync with the root env
-- ---------------------------------------------------------------------------

-- | The two @addConstant@ calls marked @impureOnly@, which upstream skips
-- under @pure-eval@.
upstreamImpureOnlyNames :: Set.Set Text
upstreamImpureOnlyNames = Set.fromList ["__currentSystem", "__currentTime"]

-- | Binding accepts exactly the names upstream's base environment holds,
-- and the root environment binds exactly the names binding accepts, under
-- each policy, every one of them to a value that forces.
testStaticGlobalsSync :: IO [Bool]
testStaticGlobalsSync = do
  putStrLn "resolve/static-globals-sync"
  let pureNames = Set.difference upstreamBaseEnvNames upstreamImpureOnlyNames
  unrestrictedRoot <- rootEnvNames unrestrictedPolicy
  pureRoot <- rootEnvNames purePolicy
  sequence
    [ runTest "binding accepts upstream's base environment" $
        assertSameNames "static globals" upstreamBaseEnvNames staticGlobalNames,
      runTest "pure evaluation drops upstream's impure-only constants" $
        assertEqual "impure-only" upstreamImpureOnlyNames impureOnlyGlobalNames
          `andThen` assertSameNames "pure scope" pureNames (rootScopeNames purePolicy),
      runTest "the root env binds exactly those names" $
        assertSameNames "root env" upstreamBaseEnvNames unrestrictedRoot
          `andThen` assertSameNames "pure root env" pureNames pureRoot,
      runTest "every root env name forces" $
        assertEqual
          "names that fail to force"
          []
          [ name
          | name <- Set.toList upstreamBaseEnvNames,
            evalNix ("builtins.seq " <> name <> " true") /= Right (VBool True)
          ],
      -- A program's names are interned before the strings its root
      -- environment binds, which upstream never interns, so they keep the
      -- order the program gives them.  It runs in a process of its own,
      -- whose symbol table nothing else has filled.
      emits
        "a program's names are interned before the strings its root environment binds"
        ("({ }: null) { a = 1; \"" <> currentSystemStr <> "\" = 2; }")
        ["error: function 'anonymous lambda' called with unexpected argument 'a'"],
      -- Upstream's base environment holds one value per primop, which
      -- every file reads, so sets copied out of builtins in two files hold
      -- the same values, and == takes each pair as equal without
      -- comparing the functions.
      runTestM "a builtin read in an imported file is the same value as in the importer" $ do
        tmpBase <- getTemporaryDirectory
        let dir = tmpBase </> "nova-nix-test-builtin-cells"
        bracket_
          (createDirectoryIfMissing True dir >> TIO.writeFile (dir </> "b.nix") "builtins")
          (removeDirectoryRecursive dir)
          $ do
            result <- evalNixIO dir "builtins.intersectAttrs { map = 1; } builtins == builtins.intersectAttrs { map = 1; } (import ./b.nix)"
            pure (assertRight "builtin-cells" result (assertEqual "builtin-cells" (VBool True)))
    ]
  where
    rootEnvNames policy = do
      Env envPtr <- builtinEnv policy 0 []
      tablePtr <- cenvNameTable envPtr
      Set.fromList <$> attrSetKeys (AttrSet (castPtr tablePtr))
    assertSameNames label expected actual =
      assertEqual
        (label <> ": (missing, extra)")
        ([], [])
        (Set.toList (Set.difference expected actual), Set.toList (Set.difference actual expected))

-- ---------------------------------------------------------------------------
-- Tests: variables are bound before evaluation (upstream's bindVars)
-- ---------------------------------------------------------------------------

-- | Assert that binding rejects a source for the named variable, before
-- anything is evaluated.
assertUnbound :: Text -> Text -> Text -> TestResult
assertUnbound label source name = case parseNix testBaseDir testOrigin (TE.encodeUtf8 source) of
  Left err -> assertEqual label (UndefinedVariable name) err
  Right parsed -> Fail (label <> ": bound, got " <> T.pack (show (parsedExpr parsed)))

-- | Assert that every variable in a source binds.
assertBinds :: Text -> Text -> TestResult
assertBinds label source = assertRight label (parseNix testBaseDir testOrigin (TE.encodeUtf8 source)) (const Pass)

-- | Upstream rejects a variable no scope binds and no @with@ could supply
-- while binding (nixexpr.cc, ExprVar::bindVars), wherever it sits; only a
-- name under a @with@ waits for evaluation.  Each expectation here is
-- nix-instantiate 2.33.2's.
testUndefinedVariables :: IO [Bool]
testUndefinedVariables = do
  putStrLn "resolve/undefined-variables"
  sequence
    [ runTest "an unbound variable in an unused let binding is rejected" $
        assertUnbound "unused-let" "let x = y; in 1" "y",
      runTest "an unbound variable on a branch not taken is rejected" $
        assertUnbound "untaken-branch" "if true then 1 else y" "y",
      runTest "tryEval has nothing to catch: binding fails first" $
        assertUnbound "tryEval" "builtins.tryEval (let x = y; in 1)" "y",
      runTest "the failure reads as upstream's" $
        assertEvalError "message" "builtins.tryEval (let x = y; in 1)" "undefined variable 'y'",
      runTest "the first unbound variable in source order is reported" $
        assertUnbound "first" "let a = b; c = d; in 1" "b",
      runTest "a name under a with binds" $
        assertBinds "with-binds" "with {}; y",
      runTest "a name no with supplies fails when evaluated" $
        assertEvalError "with-eval" "with {}; y" "undefined variable 'y'",
      runTest "an unused binding may read a name only a with could supply" $
        assertEval "with-unused" "with {}; let x = y; in 1" (VInt 1),
      runTest "a with supplies a name a let binding reads" $
        assertEval "with-let" "with { y = 3; }; let x = y; in x" (VInt 3),
      runTest "a nested let shadows the enclosing one" $
        assertEval "let-shadow" "let y = 1; in let y = 2; x = y; in x" (VInt 2),
      runTest "a nested rec reads the enclosing rec" $
        assertEval "rec-nested" "rec { a = 1; b = rec { c = a; }.c; }.b" (VInt 1),
      runTest "a rec reading a name it does not define is rejected" $
        assertUnbound "rec-missing" "rec { a = b; }" "b",
      runTest "a lambda formal shadows a with" $
        assertEval "formal-over-with" "(x: with { x = 2; }; x) 1" (VInt 1),
      runTest "a formal default reads a sibling formal" $
        assertEval "formal-default" "({ a, b ? a }: b) { a = 3; }" (VInt 3),
      runTest "an unbound formal default is rejected" $
        assertUnbound "formal-default-unbound" "{ a ? b }: a" "b",
      runTest "inherit of an unbound name in a let is rejected" $
        assertUnbound "let-inherit" "let inherit x; in 1" "x",
      runTest "inherit of an unbound name in a set is rejected" $
        assertUnbound "set-inherit" "{ inherit x; }" "x",
      runTest "inherit of an unbound name in a rec is rejected" $
        assertUnbound "rec-inherit" "rec { inherit x; }" "x",
      runTest "inherit reads a name only a with supplies" $
        assertEval "with-inherit" "with { x = 5; }; rec { inherit x; }.x" (VInt 5),
      runTest "inherit under an empty with fails only when selected" $
        assertEvalError "with-inherit-missing" "with {}; { inherit x; }.x" "undefined variable 'x'",
      runTest "inherit in a rec reads the enclosing binding, not its own" $
        assertEval "rec-inherit-outer" "(x: rec { inherit x; y = x; }.y) 4" (VInt 4),
      runTest "inherit from an unbound source is rejected" $
        assertUnbound "inherit-from-unbound" "{ inherit (s) x; }" "s",
      runTest "inherit from selects lazily" $
        assertEval "inherit-from-lazy" "let s = {}; in builtins.attrNames { inherit (s) x; } == [ \"x\" ]" (VBool True),
      runTest "inherit from fails when the attribute is selected" $
        assertEvalFail "inherit-from-missing" "let s = {}; in { inherit (s) x; }.x",
      runTest "an inherit clause cannot repeat a name" $
        assertParseFail "inherit-repeat" "let x = 1; in { inherit x x; }",
      runTest "an inherit-from clause cannot repeat a name" $
        assertParseFail "inherit-from-repeat" "let s = { x = 1; }; in { inherit (s) x x; }",
      runTest "__curPos is upstream syntax, not a variable" $
        assertEval "curpos" "if false then __curPos else 1" (VInt 1),
      runTest "inherit in a static-key rec reads the enclosing rec" $
        assertEval "rec-inherit-static" "rec { q = 0; x = 1; y = rec { a = 1; inherit x; }; }.y.x" (VInt 1),
      runTest "a __-prefixed builtin on a branch not taken binds" $
        assertEval "prefixed-untaken" "if false then __typeOf 1 else 2" (VInt 2),
      runTest "a __-prefixed builtin is the builtin" $
        assertEval "prefixed-value" "__typeOf 1 == \"int\"" (VBool True),
      runTest "a global is not shadowed by a with" $
        assertEval "prefixed-over-with" "with { __typeOf = 1; }; __typeOf 2 == \"int\"" (VBool True),
      runTest "fetchMercurial binds as a builtin" $
        assertEval "fetchMercurial-binds" "builtins.typeOf fetchMercurial == \"lambda\"" (VBool True),
      runTest "calling fetchMercurial fails as a builtin nova-nix lacks" $
        assertEvalError "fetchMercurial-call" "fetchMercurial {}" "unknown builtin 'fetchMercurial'",
      runTest "pure evaluation leaves upstream's impure-only constants unbound" $
        assertEqual "pure-currentTime" (Left "undefined variable '__currentTime'") (evalNixWith purePolicy "if false then __currentTime else 1"),
      runTest "pure evaluation binds the other globals" $
        assertEqual "pure-typeOf" (Right (VInt 1)) (evalNixWith purePolicy "if false then __typeOf else 1")
    ]

-- | The same check for files: upstream binds each file as it parses it, so
-- an import fails on an unbound variable the importing code never reaches,
-- and @scopedImport@ binds its scope's names around the file.  The inherit
-- cases whose regression is a self-reference run here too.
testUndefinedVariablesIO :: IO [Bool]
testUndefinedVariablesIO = do
  putStrLn "resolve/undefined-variables-io"
  tmpBase <- getTemporaryDirectory
  let testDir = tmpBase </> "nova-nix-test-undefined-variables"
      branchFile = nixQuotedPath (testDir </> "branch.nix")
      scopedFile = nixQuotedPath (testDir </> "scoped.nix")
      scopedWithFile = nixQuotedPath (testDir </> "scoped-with.nix")
      expectError label source expected = do
        result <- evalNixIO testDir source
        runTest label $ case result of
          Left err -> assertEqual label expected err
          Right val -> Fail (label <> ": expected an error, got " <> T.pack (show val))
  bracket_
    ( do
        createDirectoryIfMissing True testDir
        TIO.writeFile (testDir </> "branch.nix") "{ flag }:\nif flag then 1 else unboundName\n"
        TIO.writeFile (testDir </> "scoped.nix") "if false then notInScope else fromScope\n"
        TIO.writeFile (testDir </> "scoped-with.nix") "with { fromScope = \"with\"; }; fromScope\n"
    )
    ( do
        exists <- doesDirectoryExist testDir
        when exists (removeDirectoryRecursive testDir)
    )
    $ sequence
      [ expectError
          "an import fails on an unbound variable on a branch not taken"
          ("(import " <> branchFile <> ") { flag = true; }")
          "undefined variable 'unboundName'",
        expectError
          "tryEval cannot catch an import that fails to bind"
          ("builtins.tryEval (import " <> branchFile <> ")")
          "undefined variable 'unboundName'",
        runTestIO
          "an import that is never forced does not fail"
          testDir
          ("let f = import " <> branchFile <> "; in 1")
          (VInt 1),
        expectError
          "scopedImport binds against its scope"
          ("builtins.scopedImport { fromScope = 7; } " <> scopedFile)
          "undefined variable 'notInScope'",
        runTestIO
          "scopedImport scope names bind"
          testDir
          ("builtins.scopedImport { fromScope = 7; notInScope = 1; } " <> scopedFile)
          (VInt 7),
        runTestIO
          "a scopedImport scope name is not shadowed by a with"
          testDir
          ("builtins.scopedImport { fromScope = 7; } " <> scopedWithFile)
          (VInt 7),
        -- Evaluated where a self-reference is caught as infinite recursion
        -- rather than looping, which is what reading the inherited name
        -- from the rec's own scope would do.
        runTestIO
          "inherit in a dynamic-key rec reads the enclosing rec, not its own"
          testDir
          "rec { ${\"q\" + \"\"} = 0; x = 1; y = rec { ${\"a\" + \"\"} = 1; inherit x; }; }.y.x"
          (VInt 1),
        expectError
          "inherit of a name no with supplies is undefined, not the rec's own"
          "with {}; rec { ${\"a\" + \"\"} = 1; inherit x; }.x"
          "undefined variable 'x'"
      ]

-- ---------------------------------------------------------------------------
-- Tests: byte-indexed string semantics
-- ---------------------------------------------------------------------------

-- | A file is lexed as its bytes, as upstream reads it (eval.cc,
-- @parseExprFromFile@ at 2.24.9): a string literal in one keeps a byte
-- that is not UTF-8.  A byte order mark selects UTF-16, read as its UTF-8,
-- or marks UTF-8.
testSourceBytesIO :: IO [Bool]
testSourceBytesIO = do
  putStrLn "parser/source-bytes-io"
  tmpDir <- getTemporaryDirectory
  let testDir = tmpDir </> "nova-nix-source-bytes-test"
      invalid = BS.pack [0x61, 0xFF, 0x62]
      named = "{ name = \"caf\233\"; }"
      name = VStr (TE.encodeUtf8 "caf\233") emptyContext
  bracket_
    ( do
        createDirectoryIfMissing True testDir
        BS.writeFile (testDir </> "invalid.nix") ("\"" <> invalid <> "\"")
        BS.writeFile (testDir </> "utf16le.nix") (BS.pack [0xFF, 0xFE] <> TE.encodeUtf16LE named)
        BS.writeFile (testDir </> "utf16be.nix") (BS.pack [0xFE, 0xFF] <> TE.encodeUtf16BE named)
        BS.writeFile (testDir </> "utf8mark.nix") (BS.pack [0xEF, 0xBB, 0xBF] <> TE.encodeUtf8 named)
    )
    ( do
        exists <- doesDirectoryExist testDir
        when exists (removeDirectoryRecursive testDir)
    )
    $ sequence
      [ runTestIO "a string literal in a file keeps a byte that is not UTF-8" testDir "import ./invalid.nix" (VStr invalid emptyContext),
        runTestIO "a UTF-16 LE file with a byte order mark is read" testDir "(import ./utf16le.nix).name" name,
        runTestIO "a UTF-16 BE file with a byte order mark is read" testDir "(import ./utf16be.nix).name" name,
        runTestIO "a UTF-8 file with a byte order mark is read" testDir "(import ./utf8mark.nix).name" name
      ]
