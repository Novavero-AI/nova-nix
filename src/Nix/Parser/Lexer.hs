-- | Lexer for the Nix language: 'Text' to @['Located']@.
--
-- Handles all Nix tokens including string interpolation via a mode stack.
-- Entirely pure - no IO.
module Nix.Parser.Lexer
  ( -- * Tokens
    Token (..),
    Located (..),

    -- * Tokenizing
    tokenize,
  )
where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Foreign (lengthWord8)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Nix.Parser.ParseError (ParseError (..))

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
  | -- Strings
    TokStringOpen
  | TokStringClose
  | TokIndStringOpen
  | TokIndStringClose
  | TokStringLit !Text
  | -- | Resolved escape text inside an indented string (@'''@, @''$@,
    -- @''${@, @''\x@).  Kept apart from 'TokStringLit' because escapes
    -- are opaque to indentation stripping (upstream lexer.l emits them
    -- without the hasIndentation mark): they end start-of-line
    -- whitespace but are never scanned or stripped.
    TokStringEsc !Text
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

-- | Which mode the lexer is in (for string interpolation).
data LexMode
  = ModeNormal
  | ModeString
  | ModeIndString
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
  { lsInput :: !Text,
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
tokenize :: Text -> Text -> Either ParseError [Located]
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
  _ -> lexNormalMode st acc

lexNormalMode :: LexState -> [Located] -> Either ParseError [Located]
lexNormalMode st acc = case T.uncons (lsInput st) of
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
lexNormalModeAt :: LexState -> LexState -> Bool -> Char -> Text -> [Located] -> Either ParseError [Located]
lexNormalModeAt st flooredSt startsPath c rest acc = case c of
  _ | isSpace c -> lexNormalMode (skipWhitespace st) acc
  '#' -> lexNormalMode (skipLineComment st) acc
  '/'
    | Just '*' <- safeHead rest ->
        case skipBlockComment (advanceCol 2 st {lsInput = T.drop 2 (lsInput st)}) of
          Left err -> Left err
          Right newSt -> lexNormalMode newSt acc
  '"' ->
    let tok = Located (lsLine st) (lsCol st) TokStringOpen
        newSt = advanceCol 1 st {lsInput = rest, lsModes = ModeString : lsModes st}
     in lexLoop newSt (tok : acc)
  '\''
    | Just '\'' <- safeHead rest ->
        let tok = Located (lsLine st) (lsCol st) TokIndStringOpen
            newSt = advanceCol 2 st {lsInput = T.drop 2 (lsInput st), lsModes = ModeIndString : lsModes st}
         in lexLoop newSt (tok : acc)
  '.'
    | Just '.' <- safeHead rest,
      Just '.' <- safeHead (T.drop 1 rest) ->
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
          outerMode == ModeString || outerMode == ModeIndString ->
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
    | maybe False (\ch -> isAlpha ch || ch == '_') (safeHead rest) ->
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
            newSt = advanceCol 2 st {lsInput = T.drop 1 rest, lsBraceDepth = lsBraceDepth st + 1}
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
          peMessage = "unexpected character: " <> T.singleton c
        }

-- ---------------------------------------------------------------------------
-- String modes
-- ---------------------------------------------------------------------------

lexStringMode :: LexState -> [Located] -> Either ParseError [Located]
lexStringMode st acc = case T.uncons (lsInput st) of
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
                    { lsInput = T.drop 1 rest,
                      lsModes = ModeNormal : lsModes st,
                      -- Fresh count for the interpolation body; the
                      -- enclosing context's count is restored at the
                      -- matching TokInterpClose.
                      lsBraceDepth = 0,
                      lsBraceStack = lsBraceDepth st : lsBraceStack st
                    }
           in lexLoop newSt (tok : acc)
    _ -> lexStringLiteral st acc

lexIndStringMode :: LexState -> [Located] -> Either ParseError [Located]
lexIndStringMode st acc
  | T.null (lsInput st) =
      Left
        ParseError
          { peFile = lsFile st,
            peLine = lsLine st,
            peCol = lsCol st,
            peMessage = "unterminated indented string"
          }
  | otherwise =
      let input = lsInput st
       in case T.uncons input of
            Just ('\'', rest1) | Just ('\'', rest2) <- T.uncons rest1 ->
              -- Check for escape sequences: ''', ''$, ''\x, ''${
              case T.uncons rest2 of
                Just ('\'', rest3) ->
                  -- ''' escapes a literal '' (two quotes) - Nix's escape for the
                  -- indented-string terminator, not a single quote.
                  let escTok = Located (lsLine st) (lsCol st) (TokStringEsc "''")
                      newSt = advanceCol 3 st {lsInput = rest3}
                   in lexIndStringMode newSt (escTok : acc)
                Just ('$', rest3)
                  | Just ('{', rest4) <- T.uncons rest3 ->
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
                  case T.uncons rest3 of
                    Just (ec, rest4) ->
                      let escaped = case ec of
                            'n' -> "\n"
                            't' -> "\t"
                            'r' -> "\r"
                            '\\' -> "\\"
                            -- Unknown escape: Nix drops the backslash (''\q -> q).
                            _ -> T.singleton ec
                          escTok = Located (lsLine st) (lsCol st) (TokStringEsc escaped)
                          newSt = advanceCol 4 st {lsInput = rest4}
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
              | Just ('{', rest2) <- T.uncons rest1 ->
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

-- | Lex a literal segment inside a regular string.
-- Uses 'TB.Builder' for O(1) amortized append instead of O(n) 'T.snoc'.
lexStringLiteral :: LexState -> [Located] -> Either ParseError [Located]
lexStringLiteral st0 acc = go st0 mempty
  where
    go st !builder = case T.uncons (lsInput st) of
      Nothing ->
        Left
          ParseError
            { peFile = lsFile st,
              peLine = lsLine st,
              peCol = lsCol st,
              peMessage = "unterminated string"
            }
      Just (c, rest) -> case c of
        '"' -> finishChunk st builder
        -- \$$ is two literal dollars (maximal munch): the second $ cannot begin an
        -- interpolation, so $${ does not interpolate (documented Nix behavior).
        '$'
          | Just '$' <- safeHead rest ->
              go (advanceCol 2 st {lsInput = T.drop 1 rest}) (builder <> TB.singleton '$' <> TB.singleton '$')
        '$' | Just '{' <- safeHead rest -> finishChunk st builder
        '\\' -> case T.uncons rest of
          Just (ec, rest2) ->
            let escaped = case ec of
                  'n' -> TB.singleton '\n'
                  't' -> TB.singleton '\t'
                  'r' -> TB.singleton '\r'
                  '\\' -> TB.singleton '\\'
                  '"' -> TB.singleton '"'
                  '$' -> TB.singleton '$'
                  -- Unknown escape: Nix drops the backslash (\q -> q).
                  _ -> TB.singleton ec
                newSt = advanceBy ec (advanceCol 1 st {lsInput = rest2})
             in go newSt (builder <> escaped)
          Nothing ->
            Left
              ParseError
                { peFile = lsFile st,
                  peLine = lsLine st,
                  peCol = lsCol st,
                  peMessage = "unterminated escape in string"
                }
        '\n' ->
          let newSt = st {lsInput = rest, lsLine = lsLine st + 1, lsCol = 1}
           in go newSt (builder <> TB.singleton '\n')
        -- Raw CR and CRLF normalize to LF, matching upstream unescapeStr
        -- (lexer.l).  Only double-quoted strings do this: indented-string
        -- chunks bypass unescapeStr upstream and keep CR verbatim.  An
        -- ESCAPED CR (backslash before it) stays literal via the escape
        -- branch above, also matching upstream.
        '\r' ->
          let afterEol = case T.uncons rest of
                Just ('\n', afterCrlf) -> afterCrlf
                _ -> rest
              newSt = st {lsInput = afterEol, lsLine = lsLine st + 1, lsCol = 1}
           in go newSt (builder <> TB.singleton '\n')
        _ ->
          let newSt = advanceCol 1 st {lsInput = rest}
           in go newSt (builder <> TB.singleton c)
    finishChunk st builder
      | builderIsEmpty = lexStringMode st acc
      | otherwise =
          let chunk = TL.toStrict (TB.toLazyText builder)
              tok = Located (lsLine st0) (lsCol st0) (TokStringLit chunk)
           in lexStringMode st (tok : acc)
      where
        builderIsEmpty = TL.null (TB.toLazyText builder)

-- | Lex a literal segment inside an indented string.
-- No escape sequences here (those are handled by 'lexIndStringMode'),
-- so the chunk is identical to the source text - count chars, then slice
-- once at the end.  This avoids O(n^2) 'T.snoc' allocation.
--
-- Raw CR\/CRLF is deliberately NOT normalized here: upstream's indented
-- string chunks bypass unescapeStr (lexer.l), and stripIndentation treats
-- CR as ordinary content, so only double-quoted strings normalize line
-- endings.
lexIndStringLiteral :: LexState -> [Located] -> Either ParseError [Located]
lexIndStringLiteral st0 acc = go st0 0
  where
    startInput = lsInput st0
    go st !consumed
      | T.null (lsInput st) =
          Left
            ParseError
              { peFile = lsFile st,
                peLine = lsLine st,
                peCol = lsCol st,
                peMessage = "unterminated indented string"
              }
      | otherwise =
          let input = lsInput st
           in case T.uncons input of
                Just ('\'', rest1)
                  | Just ('\'', _) <- T.uncons rest1 ->
                      finishChunk st consumed
                Just ('$', rest1)
                  | Just ('$', _) <- T.uncons rest1 ->
                      -- \$$ is two literal dollars; the second cannot begin an
                      -- interpolation (matches lexStringLiteral and Nix).
                      go (advanceCol 2 st {lsInput = T.drop 1 rest1}) (consumed + 2)
                Just ('$', rest1)
                  | Just ('{', _) <- T.uncons rest1 ->
                      finishChunk st consumed
                Just ('\n', rest1) ->
                  let newSt = st {lsInput = rest1, lsLine = lsLine st + 1, lsCol = 1}
                   in go newSt (consumed + 1)
                Just (_c, rest1) ->
                  let newSt = advanceCol 1 st {lsInput = rest1}
                   in go newSt (consumed + 1)
                Nothing ->
                  Left
                    ParseError
                      { peFile = lsFile st,
                        peLine = lsLine st,
                        peCol = lsCol st,
                        peMessage = "unterminated indented string"
                      }
    finishChunk st consumed
      | consumed == 0 = lexIndStringMode st acc
      | otherwise =
          let chunk = T.take consumed startInput
              tok = Located (lsLine st0) (lsCol st0) (TokStringLit chunk)
           in lexIndStringMode st (tok : acc)

-- ---------------------------------------------------------------------------
-- Numbers
-- ---------------------------------------------------------------------------

lexNumber :: LexState -> [Located] -> Either ParseError [Located]
lexNumber st acc =
  let (digits, after) = T.span isDigit (lsInput st)
      len = T.length digits
   in case T.uncons after of
        -- A '.' after the integer part starts a float even with no fractional
        -- digits or with only an exponent - Nix's grammar is [0-9]+\.[0-9]*(exp)?,
        -- so 12. and 12.e5 are floats, not an integer followed by a dot.
        Just ('.', after2) ->
          let (decimals, after3) = T.span isDigit after2
              (expVal, after4, expLen) = lexExponent after3
              fullLen = T.length digits + 1 + T.length decimals + expLen
              val = readDouble digits decimals expVal
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
lexExponent :: Text -> (Integer, Text, Int)
lexExponent input =
  case T.uncons input of
    Just (e, afterE)
      | e == 'e' || e == 'E' ->
          let (sign, afterSign, signLen) = case T.uncons afterE of
                Just ('+', rest) -> (1, rest, 1)
                Just ('-', rest) -> (-1, rest, 1)
                _ -> (1, afterE, 0)
              (expDigits, afterExp) = T.span isDigit afterSign
           in if T.null expDigits
                then (0, input, 0)
                else (sign * exponentMagnitude expDigits, afterExp, 1 + signLen + T.length expDigits)
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
  let afterDot = T.drop 1 (lsInput st)
      (decimals, after3) = T.span isDigit afterDot
      (expVal, after4, expLen) = lexExponent after3
      fullLen = 1 + T.length decimals + expLen
      val = readDouble "" decimals expVal
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
      (ident, after) = T.span isIdentChar input
      (uriReading, advancedFloor) = uriSpanFrom (lsNoColonFloor st) input
   in case uriReading of
        -- Flex maximal munch: the URI rule beats identifiers AND keywords
        -- whenever it matches more characters, so @x:y@ is the URI "x:y"
        -- (the classic reason the identity function must be written
        -- @x: x@) and @mailto:a\@b.com@ needs no @//@.
        Just (uri, afterUri)
          | T.length uri > T.length ident ->
              let tok = Located (lsLine st) (lsCol st) (TokUri uri)
                  newSt = advanceCol (T.length uri) st {lsInput = afterUri}
               in lexNormalMode newSt (tok : acc)
        _ ->
          let tok = Located (lsLine st) (lsCol st) (identToToken ident)
              newSt = advanceCol (T.length ident) st {lsInput = after, lsNoColonFloor = advancedFloor}
           in lexNormalMode newSt (tok : acc)

-- | Match upstream's URI token at the start of the input:
-- @[a-zA-Z][a-zA-Z0-9+.-]*:[uri-char]+@ (lexer.l).  The scheme starts
-- with a letter (never @_@), and one URI char after the colon suffices -
-- scheme-only URIs like @mailto:x@ count.  Returns the URI text and the
-- remaining input.
--
-- Threads the 'lsNoColonFloor' watermark: when the scheme-char run ends
-- at a NON-colon, no position inside that run can start a URI (a suffix
-- of the run spans to the same terminator), so the run's end is recorded
-- and later positions inside it answer Nothing in O(1).  A run ending at
-- @:@ records nothing - a shorter suffix of it may itself be a URI.
uriSpanFrom :: Int -> Text -> (Maybe (Text, Text), Int)
uriSpanFrom noColonFloor input
  | lengthWord8 input > noColonFloor = (Nothing, noColonFloor)
  | otherwise = case T.uncons input of
      Just (schemeStart, _)
        | isAlpha schemeStart ->
            let (scheme, afterScheme) = T.span isSchemeChar input
             in case T.stripPrefix ":" afterScheme of
                  Just afterColon
                    | (body, afterUri) <- T.span isUriChar afterColon,
                      not (T.null body) ->
                        (Just (scheme <> ":" <> body, afterUri), noColonFloor)
                  Just _ -> (Nothing, noColonFloor)
                  Nothing -> (Nothing, lengthWord8 input - lengthWord8 scheme)
      _ -> (Nothing, noColonFloor)

identToToken :: Text -> Token
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
identToToken name = TokIdent name

-- ---------------------------------------------------------------------------
-- Paths and search paths
-- ---------------------------------------------------------------------------

lexPath :: LexState -> [Located] -> Either ParseError [Located]
lexPath st acc =
  let (pathText, after) = T.span isPathChar (lsInput st)
      len = T.length pathText
      tok = Located (lsLine st) (lsCol st) (TokPath pathText)
      newSt = advanceCol len st {lsInput = after}
   in lexNormalMode newSt (tok : acc)

lexSearchPath :: LexState -> [Located] -> Either ParseError [Located]
lexSearchPath st acc =
  -- st is at '<', skip it
  let after = T.drop 1 (lsInput st)
      (name, after2) = T.span isSearchPathChar after
   in case T.uncons after2 of
        Just ('>', after3) ->
          let tok = Located (lsLine st) (lsCol st) (TokSearchPath name)
              totalLen = T.length name + 2 -- < + name + >
              newSt = advanceCol totalLen st {lsInput = after3}
           in lexNormalMode newSt (tok : acc)
        _ ->
          -- Not a search path, just '<'
          emit1 st TokLt acc

-- ---------------------------------------------------------------------------
-- Comments
-- ---------------------------------------------------------------------------

skipWhitespace :: LexState -> LexState
skipWhitespace st = case T.uncons (lsInput st) of
  Just ('\n', rest) -> skipWhitespace st {lsInput = rest, lsLine = lsLine st + 1, lsCol = 1}
  Just (c, rest) | isSpace c -> skipWhitespace (advanceCol 1 st {lsInput = rest})
  _ -> st

skipLineComment :: LexState -> LexState
skipLineComment st =
  let (_, after) = T.break (== '\n') (lsInput st)
   in case T.uncons after of
        Just ('\n', rest) -> st {lsInput = rest, lsLine = lsLine st + 1, lsCol = 1}
        _ -> st {lsInput = after}

skipBlockComment :: LexState -> Either ParseError LexState
skipBlockComment st = case T.uncons (lsInput st) of
  Nothing ->
    Left
      ParseError
        { peFile = lsFile st,
          peLine = lsLine st,
          peCol = lsCol st,
          peMessage = "unterminated block comment"
        }
  Just (c, rest) -> case c of
    '*'
      | Just '/' <- safeHead rest ->
          Right (advanceCol 2 st {lsInput = T.drop 1 rest})
    '\n' -> skipBlockComment st {lsInput = rest, lsLine = lsLine st + 1, lsCol = 1}
    _ -> skipBlockComment (advanceCol 1 st {lsInput = rest})

-- ---------------------------------------------------------------------------
-- Character predicates
-- ---------------------------------------------------------------------------

isIdentStart :: Char -> Bool
isIdentStart c = isAlpha c || c == '_'

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_' || c == '\'' || c == '-'

isPathChar :: Char -> Bool
isPathChar c = isAlphaNum c || c `elem` ("/.~_-+" :: [Char])

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
-- differently (@a//b.c/d@ is update-then-path).  'lengthWord8' is the
-- O(1) position measure; byte counts, so it is monotone under suffixing.
looksLikePathFrom :: Int -> Text -> (Bool, Int)
looksLikePathFrom noSlashFloor input
  | lengthWord8 input > noSlashFloor = (False, noSlashFloor)
  | otherwise =
      let run = T.takeWhile isPathChar input
          (_, slashAndRest) = T.break (== '/') run
       in if T.null slashAndRest
            then (False, lengthWord8 input - lengthWord8 run)
            else case T.uncons (T.drop 1 slashAndRest) of
              Just (afterSlash, _) -> (isPathChar afterSlash && afterSlash /= '/', noSlashFloor)
              Nothing -> (False, noSlashFloor)

isSearchPathChar :: Char -> Bool
isSearchPathChar c = isAlphaNum c || c `elem` ("/.~_-+" :: [Char])

-- | Chars allowed in a URI scheme after the leading letter, per upstream
-- lexer.l: @[a-zA-Z][a-zA-Z0-9+.-]*@.
isSchemeChar :: Char -> Bool
isSchemeChar c = isAlphaNum c || c `elem` ("+.-" :: [Char])

-- | Chars allowed after the scheme colon, exactly upstream lexer.l's URI
-- class @[a-zA-Z0-9%\/?:\@&=+$,-_.!~*']@.  Notably @#@ is NOT a URI char
-- (it starts a comment mid-URI upstream), while @*@ and @'@ are.
isUriChar :: Char -> Bool
isUriChar c = isAlphaNum c || c `elem` ("%/?:@&=+$,-_.!~*'" :: [Char])

-- ---------------------------------------------------------------------------
-- Emit helpers
-- ---------------------------------------------------------------------------

emit1 :: LexState -> Token -> [Located] -> Either ParseError [Located]
emit1 st tok acc =
  let located = Located (lsLine st) (lsCol st) tok
      newSt = advanceCol 1 st {lsInput = T.drop 1 (lsInput st)}
   in lexNormalMode newSt (located : acc)

emit2 :: LexState -> Token -> [Located] -> Either ParseError [Located]
emit2 st tok acc =
  let located = Located (lsLine st) (lsCol st) tok
      newSt = advanceCol 2 st {lsInput = T.drop 2 (lsInput st)}
   in lexNormalMode newSt (located : acc)

emit3 :: LexState -> Token -> [Located] -> Either ParseError [Located]
emit3 st tok acc =
  let located = Located (lsLine st) (lsCol st) tok
      newSt = advanceCol 3 st {lsInput = T.drop 3 (lsInput st)}
   in lexNormalMode newSt (located : acc)

-- ---------------------------------------------------------------------------
-- State helpers
-- ---------------------------------------------------------------------------

advanceCol :: Int -> LexState -> LexState
advanceCol n st = st {lsCol = lsCol st + n}

advanceBy :: Char -> LexState -> LexState
advanceBy '\n' st = st {lsLine = lsLine st + 1, lsCol = 1}
advanceBy _ st = st {lsCol = lsCol st + 1}

safeHead :: Text -> Maybe Char
safeHead t = case T.uncons t of
  Just (c, _) -> Just c
  Nothing -> Nothing

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_ : xs) = xs
