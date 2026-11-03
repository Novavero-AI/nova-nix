-- | Lexer for the Nix language: source bytes to @['Located']@.
--
-- Handles all Nix tokens including string interpolation via a mode stack.
-- Entirely pure - no IO.
--
-- The source is bytes, as upstream's flex scanner reads it (lexer.l at
-- 2.24.9).  Every token class is ASCII, as upstream's are, so a byte
-- outside ASCII never continues an identifier, number, path, URI or run of
-- whitespace, while string literals and comments hold whatever bytes they
-- hold: a string chunk is the source's bytes with its escapes resolved,
-- which is what the eval string gets.  Bytes are matched as Latin-1
-- 'Char's through "Data.ByteString.Char8" so the grammar reads as
-- character literals, which is why no predicate here is one of
-- "Data.Char"'s Unicode classes: 'Data.Char.isAlpha' calls the byte 0xE9
-- a letter.
--
-- A column counts characters: a byte that continues a UTF-8 sequence does
-- not advance it, so valid UTF-8 advances one column per character.
module Nix.Parser.Lexer
  ( -- * Tokens
    Token (..),
    Located (..),

    -- * Tokenizing
    tokenize,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, ord)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Nix.Parser.ParseError (ParseError (..))
import Numeric (showHex)

-- | A positioned token.
data Located = Located
  { locLine :: !Int,
    locCol :: !Int,
    locToken :: !Token
  }
  deriving (Show)

-- | All tokens in the Nix language.
data Token
  = -- Keywords
    TokIf
  | TokThen
  | TokElse
  | TokLet
  | TokIn
  | TokWith
  | TokAssert
  | TokRec
  | TokInherit
  | TokTrue
  | TokFalse
  | TokNull
  | -- Identifiers and literals
    TokIdent !Text
  | TokInt !Int64
  | TokFloat !Double
  | TokUri !Text
  | TokPath !Text
  | TokSearchPath !Text
  | -- | Interpolated path literal: the head piece opens (its text
    -- carried), 'TokPathLit' chunks and 'TokInterpOpen'/'TokInterpClose'
    -- pairs follow, and 'TokPathEnd' closes.  A path has no closing
    -- delimiter of its own, so the lexer synthesizes the end token at
    -- the first non-path character, the way upstream emits PATH_END.
    TokPathInterpStart !Text
  | TokPathLit !Text
  | TokPathEnd
  | -- Strings
    TokStringOpen
  | TokStringClose
  | TokIndStringOpen
  | TokIndStringClose
  | -- | A literal chunk of a string: the source's bytes, escapes resolved.
    TokStringLit !ByteString
  | -- | Resolved escape text inside an indented string (@'''@, @''$@,
    -- @''${@, @''\x@).  Kept apart from 'TokStringLit' because upstream's
    -- lexer.l emits an escape without the hasIndentation mark: the
    -- indentation scan skips it (it ends a line's leading whitespace
    -- without being measured) while the strip runs over it as over any
    -- literal.
    TokStringEsc !ByteString
  | TokInterpOpen
  | TokInterpClose
  | -- Operators
    TokPlus
  | TokMinus
  | TokStar
  | TokSlash
  | TokConcat
  | TokUpdate
  | TokNot
  | TokAnd
  | TokOr
  | TokImpl
  | TokEq
  | TokNeq
  | TokLt
  | TokLte
  | TokGt
  | TokGte
  | -- Special
    TokQuestion
  | TokDot
  | TokEllipsis
  | TokAt
  | TokColon
  | TokSemicolon
  | TokAssign
  | TokComma
  | -- Delimiters
    TokLParen
  | TokRParen
  | TokLBrace
  | TokRBrace
  | TokLBracket
  | TokRBracket
  | -- End of input
    TokEOF
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Lexer state
-- ---------------------------------------------------------------------------

-- | Which mode the lexer is in (for string and path interpolation).
data LexMode
  = ModeNormal
  | ModeString
  | ModeIndString
  | ModePath
  deriving (Eq, Show)

-- | Internal lexer state.
--
-- @lsBraceDepth@ counts unmatched @{@ in the CURRENT normal-mode context;
-- a @}@ at depth 0 closes the current interpolation.  Entering @${@ from a
-- string pushes the enclosing context's count onto @lsBraceStack@ and
-- starts a fresh count; the matching interpolation close pops it back.
-- Without the stack, a nested interpolated string inside braces zeroes the
-- enclosing count and the outer attrset's @}@ mislexes as TokInterpClose.
data LexState = LexState
  { lsInput :: !ByteString,
    lsFile :: !Text,
    lsLine :: !Int,
    lsCol :: !Int,
    lsModes :: ![LexMode],
    lsBraceDepth :: !Int,
    lsBraceStack :: ![Int],
    -- | Path-lookahead watermark: every position whose remaining input is
    -- LONGER than this byte count lies inside an already-scanned
    -- slash-free path-char run, so 'looksLikePathFrom' answers False
    -- without rescanning.  Without it, a long dot-and-ident run
    -- (@x ? p0.p1. ... .p69999@) rescans the rest of the run at every
    -- token - a quadratic that took minutes at 70000 segments.  The
    -- input only ever shrinks, so a recorded run end stays comparable.
    lsNoSlashFloor :: !Int,
    -- | The same watermark for the URI lookahead: positions inside an
    -- already-scanned scheme-char run that ended at a NON-colon carry no
    -- URI, so 'uriSpanFrom' answers Nothing without rescanning.  Dots
    -- are scheme chars, so the same long dot-run is quadratic without it.
    lsNoColonFloor :: !Int
  }

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Tokenize a Nix source file. Returns tokens or a lex error.
tokenize :: Text -> ByteString -> Either ParseError [Located]
tokenize fileName source =
  let initialState =
        LexState
          { lsInput = source,
            lsFile = fileName,
            lsLine = 1,
            lsCol = 1,
            lsModes = [ModeNormal],
            lsBraceDepth = 0,
            lsBraceStack = [],
            -- No runs scanned yet: nothing may shortcut.
            lsNoSlashFloor = maxBound,
            lsNoColonFloor = maxBound
          }
   in lexLoop initialState []

-- ---------------------------------------------------------------------------
-- Main loop
-- ---------------------------------------------------------------------------

lexLoop :: LexState -> [Located] -> Either ParseError [Located]
lexLoop st acc = case lsModes st of
  (ModeString : _) -> lexStringMode st acc
  (ModeIndString : _) -> lexIndStringMode st acc
  (ModePath : _) -> lexPathMode st acc
  _ -> lexNormalMode st acc

lexNormalMode :: LexState -> [Located] -> Either ParseError [Located]
lexNormalMode st acc = case BC.uncons (lsInput st) of
  Nothing -> Right (reverse (Located (lsLine st) (lsCol st) TokEOF : acc))
  Just (c, rest) ->
    -- Lazy: forced only by the three path guards below.  Branches taken
    -- when the guard says "not a path" continue with 'flooredSt' so a
    -- freshly recorded slash-free run is remembered, not rescanned.
    let (startsPath, advancedFloor) = looksLikePathFrom (lsNoSlashFloor st) (lsInput st)
        flooredSt = st {lsNoSlashFloor = advancedFloor}
     in lexNormalModeAt st flooredSt startsPath c rest acc

-- | The normal-mode dispatch, after the path lookahead has been prepared.
-- @st@ is the incoming state; @flooredSt@ carries the advanced path
-- watermark for the continuations that bypassed a path reading.
lexNormalModeAt :: LexState -> LexState -> Bool -> Char -> ByteString -> [Located] -> Either ParseError [Located]
lexNormalModeAt st flooredSt startsPath c rest acc = case c of
  _ | isNixSpace c -> lexNormalMode (skipWhitespace st) acc
  '#' -> lexNormalMode (skipLineComment st) acc
  '/'
    | Just '*' <- safeHead rest ->
        case skipBlockComment (advanceCol 2 st {lsInput = BS.drop 2 (lsInput st)}) of
          Left err -> Left err
          Right newSt -> lexNormalMode newSt acc
  '"' ->
    let tok = Located (lsLine st) (lsCol st) TokStringOpen
        newSt = advanceCol 1 st {lsInput = rest, lsModes = ModeString : lsModes st}
     in lexLoop newSt (tok : acc)
  '\''
    | Just '\'' <- safeHead rest ->
        let tok = Located (lsLine st) (lsCol st) TokIndStringOpen
            opened = advanceCol 2 st {lsInput = BS.drop 2 (lsInput st), lsModes = ModeIndString : lsModes st}
         in lexLoop (skipIndStringOpenerLine opened) (tok : acc)
  '.'
    | Just '.' <- safeHead rest,
      Just '.' <- safeHead (BS.drop 1 rest) ->
        emit3 st TokEllipsis acc
  '.'
    -- Maximal munch, as upstream's PATH regex: a dot-led path-char run
    -- containing a /-segment is ONE path token (./x, ../x, .github/x,
    -- even .5/x) - the path reading beats the float and TokDot readings.
    -- A dot-run with no slash falls through: .5 is a float, x.y selects.
    | startsPath -> lexPath st acc
    | Just d <- safeHead rest,
      isDigit d ->
        lexLeadingDotFloat flooredSt acc
  '.' -> emit1 flooredSt TokDot acc
  ',' -> emit1 st TokComma acc
  ';' -> emit1 st TokSemicolon acc
  ':' -> emit1 st TokColon acc
  '@' -> emit1 st TokAt acc
  '?' -> emit1 st TokQuestion acc
  '(' -> emit1 st TokLParen acc
  ')' -> emit1 st TokRParen acc
  '[' -> emit1 st TokLBracket acc
  ']' -> emit1 st TokRBracket acc
  '{' ->
    let tok = Located (lsLine st) (lsCol st) TokLBrace
        newSt = advanceCol 1 st {lsInput = rest, lsBraceDepth = lsBraceDepth st + 1}
     in lexNormalMode newSt (tok : acc)
  '}' ->
    case lsModes st of
      -- closing an interpolation: pop back to string/indstring mode and
      -- restore the enclosing normal-mode context's brace count.
      (ModeNormal : outerMode : restModes)
        | lsBraceDepth st == 0,
          outerMode == ModeString || outerMode == ModeIndString || outerMode == ModePath ->
            let tok = Located (lsLine st) (lsCol st) TokInterpClose
                (restoredDepth, restoredStack) = case lsBraceStack st of
                  (saved : outerSaved) -> (saved, outerSaved)
                  [] -> (0, [])
                newSt =
                  advanceCol
                    1
                    st
                      { lsInput = rest,
                        lsModes = outerMode : restModes,
                        lsBraceDepth = restoredDepth,
                        lsBraceStack = restoredStack
                      }
             in lexLoop newSt (tok : acc)
      _ ->
        let depth = lsBraceDepth st
            newDepth = if depth > 0 then depth - 1 else 0
            tok = Located (lsLine st) (lsCol st) TokRBrace
            newSt = advanceCol 1 st {lsInput = rest, lsBraceDepth = newDepth}
         in lexNormalMode newSt (tok : acc)
  '+' | Just '+' <- safeHead rest -> emit2 st TokConcat acc
  '+' -> emit1 st TokPlus acc
  '*' -> emit1 st TokStar acc
  '-' | Just '>' <- safeHead rest -> emit2 st TokImpl acc
  '-' -> emit1 st TokMinus acc
  '!' | Just '=' <- safeHead rest -> emit2 st TokNeq acc
  '!' -> emit1 st TokNot acc
  '&' | Just '&' <- safeHead rest -> emit2 st TokAnd acc
  '|' | Just '|' <- safeHead rest -> emit2 st TokOr acc
  '=' | Just '=' <- safeHead rest -> emit2 st TokEq acc
  '=' -> emit1 st TokAssign acc
  '<' | Just '=' <- safeHead rest -> emit2 st TokLte acc
  '<'
    | maybe False (\ch -> isAsciiAlpha ch || ch == '_') (safeHead rest) ->
        lexSearchPath st acc
  '<' -> emit1 st TokLt acc
  '>' | Just '=' <- safeHead rest -> emit2 st TokGte acc
  '>' -> emit1 st TokGt acc
  '/' | Just '/' <- safeHead rest -> emit2 st TokUpdate acc
  '/'
    -- A '/' that begins a path segment (slash followed by a path char)
    -- starts a path: /abs/path.  A bare '/' (followed by whitespace) is the
    -- division operator: a / b.  Relative paths like a/b are caught by the
    -- path guard further down, before the identifier/number cases.
    | startsPath -> lexPath st acc
    | otherwise -> emit1 flooredSt TokSlash acc
  '$'
    | Just '{' <- safeHead rest ->
        -- Increment brace depth so the closing } is TokRBrace, not
        -- TokInterpClose.  Without this, ${name} inside a string
        -- interpolation like "${env.${name}}" prematurely ends the
        -- outer interpolation.
        let tok = Located (lsLine st) (lsCol st) TokInterpOpen
            newSt = advanceCol 2 st {lsInput = BS.drop 1 rest, lsBraceDepth = lsBraceDepth st + 1}
         in lexNormalMode newSt (tok : acc)
  '~'
    | Just '/' <- safeHead rest ->
        lexPath st acc
  -- A path-char run that contains a '/' segment is a path, not an
  -- identifier or number: a/b and 6/2 lex as paths, matching Nix.
  _ | startsPath -> lexPath st acc
  _ | isDigit c -> lexNumber flooredSt acc
  _ | isIdentStart c -> lexIdentOrKeyword flooredSt acc
  _ ->
    Left
      ParseError
        { peFile = lsFile st,
          peLine = lsLine st,
          peCol = lsCol st,
          peMessage = unexpectedInput c rest
        }

-- | What a parse error says of input that starts no token: the character
-- there when its bytes are one (ASCII, or a whole UTF-8 sequence), and
-- otherwise the byte, which shown as a Latin-1 character would name a
-- character the source does not hold.
unexpectedInput :: Char -> ByteString -> Text
unexpectedInput c rest =
  case TE.decodeUtf8' (BC.cons c (BS.take (utf8TrailLength c) rest)) of
    Right character -> "unexpected character: " <> character
    Left _ -> "unexpected byte 0x" <> T.pack (showHex (ord c) "")

-- | How many bytes after a UTF-8 lead byte its sequence takes, by the
-- lead's high bits; 0 for any other byte.  The decode that reads them
-- decides whether they form a character.
utf8TrailLength :: Char -> Int
utf8TrailLength c
  | c >= '\xF0' = 3
  | c >= '\xE0' = 2
  | c >= '\xC0' = 1
  | otherwise = 0

-- ---------------------------------------------------------------------------
-- String modes
-- ---------------------------------------------------------------------------

lexStringMode :: LexState -> [Located] -> Either ParseError [Located]
lexStringMode st acc = case BC.uncons (lsInput st) of
  Nothing ->
    Left
      ParseError
        { peFile = lsFile st,
          peLine = lsLine st,
          peCol = lsCol st,
          peMessage = "unterminated string"
        }
  Just (c, rest) -> case c of
    '"' ->
      let tok = Located (lsLine st) (lsCol st) TokStringClose
          newSt = advanceCol 1 st {lsInput = rest, lsModes = safeTail (lsModes st)}
       in lexLoop newSt (tok : acc)
    '$'
      | Just '{' <- safeHead rest ->
          let tok = Located (lsLine st) (lsCol st) TokInterpOpen
              newSt =
                advanceCol
                  2
                  st
                    { lsInput = BS.drop 1 rest,
                      lsModes = ModeNormal : lsModes st,
                      -- Fresh count for the interpolation body; the
                      -- enclosing context's count is restored at the
                      -- matching TokInterpClose.
                      lsBraceDepth = 0,
                      lsBraceStack = lsBraceDepth st : lsBraceStack st
                    }
           in lexLoop newSt (tok : acc)
    _ -> lexStringLiteral st acc

-- | The rest of an indented string's opening line.  Upstream's opener is
-- @''@ with an optional run of spaces and one newline (lexer.l:
-- @\'\'(\ *\n)?@), so content starts on the next line and the opening
-- line's trailing spaces never reach the indentation scan.  Only spaces
-- qualify: a tab after @''@ is content, as are spaces not followed by a
-- newline.
skipIndStringOpenerLine :: LexState -> LexState
skipIndStringOpenerLine st =
  case BC.uncons (BC.dropWhile (== ' ') (lsInput st)) of
    Just ('\n', rest) -> st {lsInput = rest, lsLine = lsLine st + 1, lsCol = 1}
    _ -> st

lexIndStringMode :: LexState -> [Located] -> Either ParseError [Located]
lexIndStringMode st acc
  | BS.null (lsInput st) =
      Left
        ParseError
          { peFile = lsFile st,
            peLine = lsLine st,
            peCol = lsCol st,
            peMessage = "unterminated indented string"
          }
  | otherwise =
      let input = lsInput st
       in case BC.uncons input of
            Just ('\'', rest1) | Just ('\'', rest2) <- BC.uncons rest1 ->
              -- Check for escape sequences: ''', ''$, ''\x, ''${
              case BC.uncons rest2 of
                Just ('\'', rest3) ->
                  -- ''' escapes a literal '' (two quotes) - Nix's escape for the
                  -- indented-string terminator, not a single quote.
                  let escTok = Located (lsLine st) (lsCol st) (TokStringEsc "''")
                      newSt = advanceCol 3 st {lsInput = rest3}
                   in lexIndStringMode newSt (escTok : acc)
                Just ('$', rest3)
                  | Just ('{', rest4) <- BC.uncons rest3 ->
                      -- ''${ is a literal ${
                      let escTok = Located (lsLine st) (lsCol st) (TokStringEsc "${")
                          newSt = advanceCol 4 st {lsInput = rest4}
                       in lexIndStringMode newSt (escTok : acc)
                Just ('$', rest3) ->
                  -- ''$ (without brace) is a literal $
                  let escTok = Located (lsLine st) (lsCol st) (TokStringEsc "$")
                      newSt = advanceCol 3 st {lsInput = rest3}
                   in lexIndStringMode newSt (escTok : acc)
                Just ('\\', rest3) ->
                  -- ''\x is an escape sequence
                  case BC.uncons rest3 of
                    Just (escaped, rest4) ->
                      let escTok = Located (lsLine st) (lsCol st) (TokStringEsc (escapedBytes escaped))
                          newSt = advanceEscaped escaped (advanceCol 3 st {lsInput = rest4})
                       in lexIndStringMode newSt (escTok : acc)
                    Nothing ->
                      Left
                        ParseError
                          { peFile = lsFile st,
                            peLine = lsLine st,
                            peCol = lsCol st,
                            peMessage = "unterminated escape in indented string"
                          }
                _ ->
                  -- '' followed by non-escape closes the indented string
                  let tok = Located (lsLine st) (lsCol st) TokIndStringClose
                      newSt = advanceCol 2 st {lsInput = rest2, lsModes = safeTail (lsModes st)}
                   in lexLoop newSt (tok : acc)
            Just ('$', rest1)
              | Just ('{', rest2) <- BC.uncons rest1 ->
                  -- \${ in indented string is interpolation
                  let tok = Located (lsLine st) (lsCol st) TokInterpOpen
                      newSt =
                        advanceCol
                          2
                          st
                            { lsInput = rest2,
                              lsModes = ModeNormal : lsModes st,
                              -- Fresh count; enclosing context restored at
                              -- the matching TokInterpClose.
                              lsBraceDepth = 0,
                              lsBraceStack = lsBraceDepth st : lsBraceStack st
                            }
                   in lexLoop newSt (tok : acc)
            _ -> lexIndStringLiteral st acc

-- | Lex a literal segment inside a regular string.  A run of bytes the
-- string grammar gives no meaning is taken whole, and the pieces are
-- joined once when the chunk ends, so a chunk that is one such run is a
-- slice of the source.
lexStringLiteral :: LexState -> [Located] -> Either ParseError [Located]
lexStringLiteral st0 acc = go st0 []
  where
    go st pieces =
      let (run, afterRun) = BC.break endsStringRun (lsInput st)
          runSt = advanceCols run st {lsInput = afterRun}
          withRun = run : pieces
       in case BC.uncons afterRun of
            Nothing ->
              Left
                ParseError
                  { peFile = lsFile runSt,
                    peLine = lsLine runSt,
                    peCol = lsCol runSt,
                    peMessage = "unterminated string"
                  }
            Just (c, rest) -> case c of
              '"' -> finishChunk runSt withRun
              -- \$$ is two literal dollars (maximal munch): the second $ cannot begin an
              -- interpolation, so $${ does not interpolate (documented Nix behavior).
              '$'
                | Just '$' <- safeHead rest ->
                    go (advanceCol 2 runSt {lsInput = BS.drop 1 rest}) ("$$" : withRun)
                | Just '{' <- safeHead rest -> finishChunk runSt withRun
                | otherwise -> go (advanceCol 1 runSt {lsInput = rest}) ("$" : withRun)
              '\\' -> case BC.uncons rest of
                Just (escaped, rest2) ->
                  let newSt = advanceEscaped escaped (advanceCol 1 runSt {lsInput = rest2})
                   in go newSt (escapedBytes escaped : withRun)
                Nothing ->
                  Left
                    ParseError
                      { peFile = lsFile runSt,
                        peLine = lsLine runSt,
                        peCol = lsCol runSt,
                        peMessage = "unterminated escape in string"
                      }
              '\n' ->
                let newSt = runSt {lsInput = rest, lsLine = lsLine runSt + 1, lsCol = 1}
                 in go newSt ("\n" : withRun)
              -- The carriage return, the run's one other end.  Raw CR and
              -- CRLF normalize to LF, matching upstream unescapeStr
              -- (lexer.l).  Only double-quoted strings do this:
              -- indented-string chunks bypass unescapeStr upstream and keep
              -- CR verbatim.  An ESCAPED CR (backslash before it) stays
              -- literal via the escape branch above, also matching upstream.
              _ ->
                let afterEol = case BC.uncons rest of
                      Just ('\n', afterCrlf) -> afterCrlf
                      _ -> rest
                    newSt = runSt {lsInput = afterEol, lsLine = lsLine runSt + 1, lsCol = 1}
                 in go newSt ("\n" : withRun)
    finishChunk st pieces
      | BS.null chunk = lexStringMode st acc
      | otherwise =
          let tok = Located (lsLine st0) (lsCol st0) (TokStringLit chunk)
           in lexStringMode st (tok : acc)
      where
        chunk = BS.concat (reverse pieces)

-- | The bytes that end a plain run in a double-quoted string, each of
-- which means something there.
endsStringRun :: Char -> Bool
endsStringRun c = c == '"' || c == '$' || c == '\\' || c == '\n' || c == '\r'

-- | What an escaped byte stands for, as upstream's unescapeStr reads it
-- (lexer.l): @n@, @r@ and @t@ name control characters, and any other byte
-- stands for itself, the backslash dropped (@\\q@ is @q@).
escapedBytes :: Char -> ByteString
escapedBytes 'n' = "\n"
escapedBytes 'r' = "\r"
escapedBytes 't' = "\t"
escapedBytes other = BC.singleton other

-- | Lex a literal segment inside an indented string.
-- No escape sequences here (those are handled by 'lexIndStringMode'),
-- so the chunk is identical to the source bytes - scan runs, then slice
-- once at the end.
--
-- Raw CR\/CRLF is deliberately NOT normalized here: upstream's indented
-- string chunks bypass unescapeStr (lexer.l), and stripIndentation treats
-- CR as ordinary content, so only double-quoted strings normalize line
-- endings.
lexIndStringLiteral :: LexState -> [Located] -> Either ParseError [Located]
lexIndStringLiteral st0 acc = go st0
  where
    startInput = lsInput st0
    go st =
      let (run, afterRun) = BC.break endsIndStringRun (lsInput st)
          runSt = advanceCols run st {lsInput = afterRun}
       in case BC.uncons afterRun of
            Nothing ->
              Left
                ParseError
                  { peFile = lsFile runSt,
                    peLine = lsLine runSt,
                    peCol = lsCol runSt,
                    peMessage = "unterminated indented string"
                  }
            Just ('\'', rest)
              | Just '\'' <- safeHead rest -> finishChunk runSt
              | otherwise -> go (advanceCol 1 runSt {lsInput = rest})
            Just ('$', rest)
              -- \$$ is two literal dollars; the second cannot begin an
              -- interpolation (matches lexStringLiteral and Nix).
              | Just '$' <- safeHead rest -> go (advanceCol 2 runSt {lsInput = BS.drop 1 rest})
              | Just '{' <- safeHead rest -> finishChunk runSt
              | otherwise -> go (advanceCol 1 runSt {lsInput = rest})
            -- The newline, the run's one other end.
            Just (_, rest) -> go runSt {lsInput = rest, lsLine = lsLine runSt + 1, lsCol = 1}
    finishChunk st
      | BS.null chunk = lexIndStringMode st acc
      | otherwise =
          let tok = Located (lsLine st0) (lsCol st0) (TokStringLit chunk)
           in lexIndStringMode st (tok : acc)
      where
        chunk = BS.take (BS.length startInput - BS.length (lsInput st)) startInput

-- | The bytes that end a plain run in an indented string.
endsIndStringRun :: Char -> Bool
endsIndStringRun c = c == '\'' || c == '$' || c == '\n'

-- ---------------------------------------------------------------------------
-- Numbers
-- ---------------------------------------------------------------------------

lexNumber :: LexState -> [Located] -> Either ParseError [Located]
lexNumber st acc =
  let (digitBytes, after) = BC.span isDigit (lsInput st)
      digits = asciiText digitBytes
      len = BS.length digitBytes
   in case BC.uncons after of
        -- A '.' after the integer part starts a float even with no fractional
        -- digits or with only an exponent - Nix's grammar is [0-9]+\.[0-9]*(exp)?,
        -- so 12. and 12.e5 are floats, not an integer followed by a dot.
        Just ('.', after2) ->
          let (decimals, after3) = BC.span isDigit after2
              (expVal, after4, expLen) = lexExponent after3
              fullLen = len + 1 + BS.length decimals + expLen
              val = readDouble digits (asciiText decimals) expVal
              tok = Located (lsLine st) (lsCol st) (TokFloat val)
              newSt = advanceCol fullLen st {lsInput = after4}
           in lexNormalMode newSt (tok : acc)
        _
          -- C++ Nix rejects out-of-range integer literals rather than
          -- silently wrapping modulo 2^64.  Range is decided by digit
          -- count first: 'readInteger' is quadratic in the digit count,
          -- and the guard must not pay that on input it rejects.
          | not (integerLiteralInRange digits) ->
              Left
                ParseError
                  { peFile = lsFile st,
                    peLine = lsLine st,
                    peCol = lsCol st,
                    peMessage = "integer literal out of range: " <> digits
                  }
          | otherwise ->
              let val = fromIntegral (readInteger digits) :: Int64
                  tok = Located (lsLine st) (lsCol st) (TokInt val)
                  newSt = advanceCol len st {lsInput = after}
               in lexNormalMode newSt (tok : acc)

-- | Read an integer from text without using the partial 'read'.
readInteger :: Text -> Integer
readInteger = T.foldl' (\n c -> n * decimalBase + fromIntegral (fromEnum c - zeroOrd)) 0

-- | Digit count of @maxBound :: Int64@ (9223372036854775807).  A literal
-- with more significant digits is out of range on count alone.
int64MaxDigits :: Int
int64MaxDigits = 19

-- | Whether an all-digit literal fits 'Int64', decided by significant
-- digit count before any bignum is built.
integerLiteralInRange :: Text -> Bool
integerLiteralInRange digits =
  let significant = T.dropWhile (== '0') digits
      count = T.length significant
   in count < int64MaxDigits
        || (count == int64MaxDigits && readInteger significant <= toInteger (maxBound :: Int64))

-- | Read a floating-point literal from its integer, decimal, and exponent
-- parts.  The digits form one exact 'Rational' scaled by the exponent, and
-- 'fromRational' rounds once - the correctly-rounded conversion C++ Nix
-- gets from strtod (lexer.l float rule).  Rounding the whole, fraction,
-- and exponent steps separately drifts an ulp from upstream on long
-- literals and flushes subnormals (e.g. 1.0e-320) to zero.  Total - no
-- 'read', no exceptions.
--
-- The exact path runs only within double's decimal range: @10 ^^ scale@
-- materializes a bignum of |scale| digits, so the cost must follow the
-- literal's length, never the exponent's magnitude (@1.0e999999999@
-- would otherwise build a gigabyte of Rational).  A scale provably past
-- the overflow bound saturates to Infinity and past the underflow bound
-- to 0.0 - the values strtod rounds such literals to.  Mantissas keep
-- 'mantissaDigitBound' significant digits, a dropped nonzero tail
-- standing in as one sticky digit; past that bound the tail cannot
-- change the correctly-rounded result.
readDouble :: Text -> Text -> Integer -> Double
readDouble intPart decPart expVal
  | T.null significantAll = 0.0
  | overflows = positiveInfinity
  | underflows = 0.0
  | otherwise = fromRational (toRational mantissa * fromInteger decimalBase ^^ scale)
  where
    significantAll = T.dropWhile (== '0') (intPart <> decPart)
    truncated = T.length significantAll > mantissaDigitBound
    (keptDigits, droppedTail) = T.splitAt mantissaDigitBound significantAll
    stickyDigit = if T.any (/= '0') droppedTail then "1" else "0"
    mantissaText = if truncated then keptDigits <> stickyDigit else significantAll
    mantissa = readInteger mantissaText
    -- Each dropped tail digit shifts the represented value's scale up by
    -- one; the appended sticky digit takes the place of the last one.
    droppedCount = if truncated then T.length droppedTail - 1 else 0
    scale = expVal - toInteger (T.length decPart) + toInteger droppedCount
    -- mantissa has exactly digitCount digits (leading digit nonzero), so
    -- the value lies in [10^(digitCount-1+scale), 10^(digitCount+scale)).
    digitCount = toInteger (T.length mantissaText)
    overflows = digitCount - 1 + scale >= doubleOverflowExp10
    underflows = digitCount + scale <= doubleUnderflowExp10

-- | Significant decimal digits kept of a float mantissa.  Correctly
-- rounding binary64 never needs more than 767 significant digits (the
-- longest exactly-representable double and every rounding midpoint fit
-- in 767), so with one sticky digit for the dropped tail, 768 kept
-- digits decide every rounding exactly as the full literal would.
mantissaDigitBound :: Int
mantissaDigitBound = 768

-- | Any value at or above 10^309 exceeds double's maximum (~1.798e308)
-- and rounds to Infinity.
doubleOverflowExp10 :: Integer
doubleOverflowExp10 = 309

-- | Any value below 10^-324 is under half the smallest denormal
-- (~4.94e-324) and rounds to 0.0.
doubleUnderflowExp10 :: Integer
doubleUnderflowExp10 = -324

-- | IEEE positive infinity, strtod's overflow result.  Float literals
-- are unsigned at the lexer (minus is an operator), so only the
-- positive infinity is ever produced.
positiveInfinity :: Double
positiveInfinity = 1 / 0

-- | Consume an optional exponent @[eE][+-]?[0-9]+@ after a float's digits.
-- Returns the signed exponent, the remaining input, and the characters
-- consumed.  An @e@ not followed by digits is not an exponent (consumes 0), so
-- e.g. @1.5e@ lexes as the float @1.5@ followed by the identifier @e@.
lexExponent :: ByteString -> (Integer, ByteString, Int)
lexExponent input =
  case BC.uncons input of
    Just (e, afterE)
      | e == 'e' || e == 'E' ->
          let (sign, afterSign, signLen) = case BC.uncons afterE of
                Just ('+', rest) -> (1, rest, 1)
                Just ('-', rest) -> (-1, rest, 1)
                _ -> (1, afterE, 0)
              (expDigits, afterExp) = BC.span isDigit afterSign
           in if BS.null expDigits
                then (0, input, 0)
                else (sign * exponentMagnitude (asciiText expDigits), afterExp, 1 + signLen + BS.length expDigits)
    _ -> (0, input, 0)

-- | The magnitude of an exponent's digit run.  Reading n digits builds
-- an n-digit bignum quadratically, so runs past 'exponentDigitBound'
-- saturate to a stand-in already so far outside double's range that
-- 'readDouble' collapses it to the same Infinity or 0.0 the exact
-- exponent would round to.
exponentMagnitude :: Text -> Integer
exponentMagnitude expDigits =
  let significant = T.dropWhile (== '0') expDigits
   in if T.length significant > exponentDigitBound
        then saturatedExponent
        else readInteger significant

-- | Exponent digit runs past this bound saturate (see
-- 'exponentMagnitude').
exponentDigitBound :: Int
exponentDigitBound = 18

-- | Stand-in exponent magnitude past 'exponentDigitBound': any exponent
-- with more significant digits is at least 10^18, and double's whole
-- decimal range spans only around 10^+-324.
saturatedExponent :: Integer
saturatedExponent = 10 ^ (18 :: Int)

-- | Lex a leading-dot float like @.5@ or @.5e3@ (Nix's @0?\\.[0-9]+@ form).
-- The current input begins with the @.@.
lexLeadingDotFloat :: LexState -> [Located] -> Either ParseError [Located]
lexLeadingDotFloat st acc =
  let afterDot = BS.drop 1 (lsInput st)
      (decimals, after3) = BC.span isDigit afterDot
      (expVal, after4, expLen) = lexExponent after3
      fullLen = 1 + BS.length decimals + expLen
      val = readDouble "" (asciiText decimals) expVal
      tok = Located (lsLine st) (lsCol st) (TokFloat val)
      newSt = advanceCol fullLen st {lsInput = after4}
   in lexNormalMode newSt (tok : acc)

-- | Base for decimal digit accumulation.
decimalBase :: Integer
decimalBase = 10

-- | Ordinal of ASCII @\'0\'@ for digit-to-int conversion.
zeroOrd :: Int
zeroOrd = fromEnum '0'

-- ---------------------------------------------------------------------------
-- Identifiers and keywords
-- ---------------------------------------------------------------------------

lexIdentOrKeyword :: LexState -> [Located] -> Either ParseError [Located]
lexIdentOrKeyword st acc =
  let input = lsInput st
      (ident, after) = BC.span isIdentChar input
      (uriReading, advancedFloor) = uriSpanFrom (lsNoColonFloor st) input
   in case uriReading of
        -- Flex maximal munch: the URI rule beats identifiers AND keywords
        -- whenever it matches more characters, so @x:y@ is the URI "x:y"
        -- (the classic reason the identity function must be written
        -- @x: x@) and @mailto:a\@b.com@ needs no @//@.
        Just (uri, afterUri)
          | BS.length uri > BS.length ident ->
              let tok = Located (lsLine st) (lsCol st) (TokUri (asciiText uri))
                  newSt = advanceCol (BS.length uri) st {lsInput = afterUri}
               in lexNormalMode newSt (tok : acc)
        _ ->
          let tok = Located (lsLine st) (lsCol st) (identToToken ident)
              newSt = advanceCol (BS.length ident) st {lsInput = after, lsNoColonFloor = advancedFloor}
           in lexNormalMode newSt (tok : acc)

-- | Match upstream's URI token at the start of the input:
-- @[a-zA-Z][a-zA-Z0-9+.-]*:[uri-char]+@ (lexer.l).  The scheme starts
-- with a letter (never @_@), and one URI char after the colon suffices -
-- scheme-only URIs like @mailto:x@ count.  Returns the URI bytes and the
-- remaining input.
--
-- Threads the 'lsNoColonFloor' watermark: when the scheme-char run ends
-- at a NON-colon, no position inside that run can start a URI (a suffix
-- of the run spans to the same terminator), so the run's end is recorded
-- and later positions inside it answer Nothing in O(1).  A run ending at
-- @:@ records nothing - a shorter suffix of it may itself be a URI.
uriSpanFrom :: Int -> ByteString -> (Maybe (ByteString, ByteString), Int)
uriSpanFrom noColonFloor input
  | BS.length input > noColonFloor = (Nothing, noColonFloor)
  | otherwise = case BC.uncons input of
      Just (schemeStart, _)
        | isAsciiAlpha schemeStart ->
            let (scheme, afterScheme) = BC.span isSchemeChar input
             in case BS.stripPrefix ":" afterScheme of
                  Just afterColon
                    | (body, afterUri) <- BC.span isUriChar afterColon,
                      not (BS.null body) ->
                        (Just (BS.take (BS.length input - BS.length afterUri) input, afterUri), noColonFloor)
                  Just _ -> (Nothing, noColonFloor)
                  Nothing -> (Nothing, BS.length input - BS.length scheme)
      _ -> (Nothing, noColonFloor)

identToToken :: ByteString -> Token
identToToken "if" = TokIf
identToToken "then" = TokThen
identToToken "else" = TokElse
identToToken "let" = TokLet
identToToken "in" = TokIn
identToToken "with" = TokWith
identToToken "assert" = TokAssert
identToToken "rec" = TokRec
identToToken "inherit" = TokInherit
identToToken "true" = TokTrue
identToToken "false" = TokFalse
identToToken "null" = TokNull
identToToken name = TokIdent (asciiText name)

-- ---------------------------------------------------------------------------
-- Paths and search paths
-- ---------------------------------------------------------------------------

lexPath :: LexState -> [Located] -> Either ParseError [Located]
lexPath st acc =
  let (pathBytes, after) = BC.span isPathChar (lsInput st)
      pathText = asciiText pathBytes
      newSt = advanceCol (BS.length pathBytes) st {lsInput = after}
   in if "${" `BS.isPrefixOf` after
        then
          -- The literal continues through an interpolation: the head
          -- piece opens and 'lexPathMode' carries on, upstream's
          -- INPATH machinery.
          let tok = Located (lsLine st) (lsCol st) (TokPathInterpStart pathText)
           in lexPathMode (newSt {lsModes = ModePath : lsModes newSt}) (tok : acc)
        else
          if "/" `BS.isSuffixOf` pathBytes
            then pathTrailingSlashError st
            else
              let tok = Located (lsLine st) (lsCol st) (TokPath pathText)
               in lexNormalMode newSt (tok : acc)

-- | Inside an interpolated path literal, after the head piece: literal
-- chunks, @${@ interpolations, and the synthesized end.  Mirrors the
-- string modes, with two path-specific rules from upstream's lexer: any
-- non-path character ends the literal (there is no closing delimiter),
-- and ending while the last piece ends in @/@ is the "path has a
-- trailing slash" parse error.
lexPathMode :: LexState -> [Located] -> Either ParseError [Located]
lexPathMode st acc
  | "${" `BS.isPrefixOf` lsInput st =
      let tok = Located (lsLine st) (lsCol st) TokInterpOpen
          newSt =
            advanceCol
              2
              st
                { lsInput = BS.drop 2 (lsInput st),
                  lsModes = ModeNormal : lsModes st,
                  -- Fresh count for the interpolation body; the
                  -- enclosing context's count is restored at the
                  -- matching TokInterpClose.
                  lsBraceDepth = 0,
                  lsBraceStack = lsBraceDepth st : lsBraceStack st
                }
       in lexLoop newSt (tok : acc)
  | otherwise =
      let (chunk, after) = BC.span isPathChar (lsInput st)
          newSt = advanceCol (BS.length chunk) st {lsInput = after}
          endTok = Located (lsLine newSt) (lsCol newSt) TokPathEnd
          popped = newSt {lsModes = safeTail (lsModes newSt)}
       in if BS.null chunk
            then -- The character after an interpolation is not a path
            -- character: the literal ends right there.
              lexNormalMode popped (endTok : acc)
            else
              if "${" `BS.isPrefixOf` after
                then
                  let tok = Located (lsLine st) (lsCol st) (TokPathLit (asciiText chunk))
                   in lexPathMode newSt (tok : acc)
                else
                  if "/" `BS.isSuffixOf` chunk
                    then pathTrailingSlashError st
                    else
                      let tok = Located (lsLine st) (lsCol st) (TokPathLit (asciiText chunk))
                       in lexNormalMode popped (endTok : tok : acc)

-- | Upstream's INPATH_SLASH error, verbatim: a path literal may not end
-- with @/@, whether plain or after an interpolation.
pathTrailingSlashError :: LexState -> Either ParseError [Located]
pathTrailingSlashError st =
  Left
    ParseError
      { peFile = lsFile st,
        peLine = lsLine st,
        peCol = lsCol st,
        peMessage = "path has a trailing slash"
      }

lexSearchPath :: LexState -> [Located] -> Either ParseError [Located]
lexSearchPath st acc =
  -- st is at '<', skip it
  let after = BS.drop 1 (lsInput st)
      (name, after2) = BC.span isSearchPathChar after
   in case BC.uncons after2 of
        Just ('>', after3) ->
          let tok = Located (lsLine st) (lsCol st) (TokSearchPath (asciiText name))
              totalLen = BS.length name + 2 -- < + name + >
              newSt = advanceCol totalLen st {lsInput = after3}
           in lexNormalMode newSt (tok : acc)
        _ ->
          -- Not a search path, just '<'
          emit1 st TokLt acc

-- ---------------------------------------------------------------------------
-- Comments
-- ---------------------------------------------------------------------------

skipWhitespace :: LexState -> LexState
skipWhitespace st =
  let (spaces, rest) = BC.span isNixSpace (lsInput st)
   in advanceOver spaces st {lsInput = rest}

skipLineComment :: LexState -> LexState
skipLineComment st = case BC.elemIndex '\n' (lsInput st) of
  Just newline -> st {lsInput = BS.drop (newline + 1) (lsInput st), lsLine = lsLine st + 1, lsCol = 1}
  Nothing -> st {lsInput = BS.empty}

-- | Skip a block comment's body and its @*\/@, the opening @\/*@ already
-- consumed.  The comment ends at the first @*\/@, as upstream's rule
-- (lexer.l) ends it.
skipBlockComment :: LexState -> Either ParseError LexState
skipBlockComment st =
  let (body, after) = BS.breakSubstring "*/" (lsInput st)
      bodySt = advanceOver body st {lsInput = after}
   in if BS.null after
        then
          Left
            ParseError
              { peFile = lsFile bodySt,
                peLine = lsLine bodySt,
                peCol = lsCol bodySt,
                peMessage = "unterminated block comment"
              }
        else Right (advanceCol 2 bodySt {lsInput = BS.drop 2 after})

-- ---------------------------------------------------------------------------
-- Character predicates
-- ---------------------------------------------------------------------------

-- | Upstream's whitespace class, @[ \\t\\r\\n]@ (lexer.l).  A vertical tab,
-- a form feed and every non-ASCII space start no token there.
isNixSpace :: Char -> Bool
isNixSpace c = c == ' ' || c == '\t' || c == '\r' || c == '\n'

-- | An ASCII letter, upstream's @[a-zA-Z]@.
isAsciiAlpha :: Char -> Bool
isAsciiAlpha c = isAsciiUpper c || isAsciiLower c

-- | An ASCII letter or digit, upstream's @[a-zA-Z0-9]@.
isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum c = isAsciiAlpha c || isDigit c

isIdentStart :: Char -> Bool
isIdentStart c = isAsciiAlpha c || c == '_'

isIdentChar :: Char -> Bool
isIdentChar c = isAsciiAlphaNum c || c == '_' || c == '\'' || c == '-'

isPathChar :: Char -> Bool
isPathChar c = isAsciiAlphaNum c || c `elem` ("/.~_-+" :: [Char])

-- | Does the maximal path-char run at the start of the input contain a
-- @/@-segment (a slash followed by a non-slash path char)?  If so it lexes
-- as a path rather than an identifier, number, or division operator -
-- matching Nix, where @a/b@ and @/abs/path@ are paths, @a / b@ (slash
-- surrounded by whitespace) is division, and @a // b@ is the update operator.
--
-- Threads the 'lsNoSlashFloor' watermark: a position inside an
-- already-scanned SLASH-FREE run answers False in O(1), and a freshly
-- scanned slash-free run records its end (no position within it can start
-- a path, since the run's characters and its terminator are the same
-- bytes every later check would rescan).  A run that CONTAINS a slash
-- records nothing: a later position inside it can legitimately answer
-- differently (@a//b.c/d@ is update-then-path).  The remaining input's
-- length is the O(1) position measure, monotone under suffixing.
looksLikePathFrom :: Int -> ByteString -> (Bool, Int)
looksLikePathFrom noSlashFloor input
  | BS.length input > noSlashFloor = (False, noSlashFloor)
  | otherwise =
      let run = BC.takeWhile isPathChar input
          (_, slashAndRest) = BC.break (== '/') run
          -- Upstream's PATH_SEG rule: a run ending in @/@ immediately
          -- followed by @${@ opens an interpolated path (@./${v}@,
          -- @/${v}@, @a/${v}@), even though the slash has no segment
          -- after it yet - the segment arrives at eval.  Such runs
          -- contain a slash, so the slash-free watermark is untouched.
          interpFollows =
            "/" `BS.isSuffixOf` run
              && "${" `BS.isPrefixOf` BS.drop (BS.length run) input
       in if BS.null slashAndRest
            then (False, BS.length input - BS.length run)
            else case BC.uncons (BS.drop 1 slashAndRest) of
              Just (afterSlash, _) -> (interpFollows || (isPathChar afterSlash && afterSlash /= '/'), noSlashFloor)
              Nothing -> (interpFollows, noSlashFloor)

isSearchPathChar :: Char -> Bool
isSearchPathChar c = isAsciiAlphaNum c || c `elem` ("/.~_-+" :: [Char])

-- | Chars allowed in a URI scheme after the leading letter, per upstream
-- lexer.l: @[a-zA-Z][a-zA-Z0-9+.-]*@.
isSchemeChar :: Char -> Bool
isSchemeChar c = isAsciiAlphaNum c || c `elem` ("+.-" :: [Char])

-- | Chars allowed after the scheme colon, exactly upstream lexer.l's URI
-- class @[a-zA-Z0-9%\/?:\@&=+$,-_.!~*']@.  Notably @#@ is NOT a URI char
-- (it starts a comment mid-URI upstream), while @*@ and @'@ are.
isUriChar :: Char -> Bool
isUriChar c = isAsciiAlphaNum c || c `elem` ("%/?:@&=+$,-_.!~*'" :: [Char])

-- | A byte that continues a UTF-8 sequence, @10xxxxxx@.
isContinuationByte :: Char -> Bool
isContinuationByte c = c >= '\x80' && c < '\xC0'

-- | The text of bytes an ASCII class matched.  Latin-1 agrees with ASCII
-- on every byte such a class admits, and 'TE.decodeLatin1' is total.
asciiText :: ByteString -> Text
asciiText = TE.decodeLatin1

-- ---------------------------------------------------------------------------
-- Emit helpers
-- ---------------------------------------------------------------------------

emit1 :: LexState -> Token -> [Located] -> Either ParseError [Located]
emit1 st tok acc =
  let located = Located (lsLine st) (lsCol st) tok
      newSt = advanceCol 1 st {lsInput = BS.drop 1 (lsInput st)}
   in lexNormalMode newSt (located : acc)

emit2 :: LexState -> Token -> [Located] -> Either ParseError [Located]
emit2 st tok acc =
  let located = Located (lsLine st) (lsCol st) tok
      newSt = advanceCol 2 st {lsInput = BS.drop 2 (lsInput st)}
   in lexNormalMode newSt (located : acc)

emit3 :: LexState -> Token -> [Located] -> Either ParseError [Located]
emit3 st tok acc =
  let located = Located (lsLine st) (lsCol st) tok
      newSt = advanceCol 3 st {lsInput = BS.drop 3 (lsInput st)}
   in lexNormalMode newSt (located : acc)

-- ---------------------------------------------------------------------------
-- State helpers
-- ---------------------------------------------------------------------------

advanceCol :: Int -> LexState -> LexState
advanceCol n st = st {lsCol = lsCol st + n}

-- | The columns a run of bytes with no newline in it takes: one per byte
-- that does not continue a UTF-8 sequence, so one per character of valid
-- UTF-8.
charColumns :: ByteString -> Int
charColumns = BC.foldl' (\n c -> if isContinuationByte c then n else n + 1) 0

-- | Advance over a run of bytes with no newline in it.
advanceCols :: ByteString -> LexState -> LexState
advanceCols run st = st {lsCol = lsCol st + charColumns run}

-- | Advance over a run of bytes that may hold newlines.
advanceOver :: ByteString -> LexState -> LexState
advanceOver run st = case BC.elemIndexEnd '\n' run of
  Nothing -> advanceCols run st
  Just lastNewline ->
    st
      { lsLine = lsLine st + BC.count '\n' run,
        lsCol = 1 + charColumns (BS.drop (lastNewline + 1) run)
      }

-- | Advance past the byte an escape names: a newline starts a line, a byte
-- continuing a UTF-8 sequence stays in its character's column, and any
-- other byte takes one.
advanceEscaped :: Char -> LexState -> LexState
advanceEscaped c st
  | c == '\n' = st {lsLine = lsLine st + 1, lsCol = 1}
  | isContinuationByte c = st
  | otherwise = st {lsCol = lsCol st + 1}

safeHead :: ByteString -> Maybe Char
safeHead = fmap fst . BC.uncons

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_ : xs) = xs
