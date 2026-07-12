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
data LexState = LexState
  { lsInput :: !Text,
    lsFile :: !Text,
    lsLine :: !Int,
    lsCol :: !Int,
    lsModes :: ![LexMode],
    lsBraceDepth :: !Int
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
            lsBraceDepth = 0
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
  Just (c, rest) -> case c of
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
      | Just '/' <- safeHead rest ->
          lexPath st acc
      | Just '.' <- safeHead rest,
        Just c2 <- safeHead (T.drop 1 rest),
        c2 == '/' ->
          lexPath st acc
      | Just d <- safeHead rest,
        isDigit d ->
          lexLeadingDotFloat st acc
    '.' -> emit1 st TokDot acc
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
        -- closing an interpolation: pop back to string/indstring mode
        (ModeNormal : outerMode : restModes)
          | lsBraceDepth st == 0,
            outerMode == ModeString || outerMode == ModeIndString ->
              let tok = Located (lsLine st) (lsCol st) TokInterpClose
                  newSt = advanceCol 1 st {lsInput = rest, lsModes = outerMode : restModes}
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
      | looksLikePath (lsInput st) -> lexPath st acc
      | otherwise -> emit1 st TokSlash acc
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
    _ | looksLikePath (lsInput st) -> lexPath st acc
    _ | isDigit c -> lexNumber st acc
    _ | isIdentStart c -> lexIdentOrKeyword st acc
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
                      lsBraceDepth = 0
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
                  let litTok = Located (lsLine st) (lsCol st) (TokStringLit "''")
                      newSt = advanceCol 3 st {lsInput = rest3}
                   in lexIndStringMode newSt (litTok : acc)
                Just ('$', rest3)
                  | Just ('{', rest4) <- T.uncons rest3 ->
                      -- ''${ is a literal ${
                      let litTok = Located (lsLine st) (lsCol st) (TokStringLit "${")
                          newSt = advanceCol 4 st {lsInput = rest4}
                       in lexIndStringMode newSt (litTok : acc)
                Just ('$', rest3) ->
                  -- ''$ (without brace) is a literal $
                  let litTok = Located (lsLine st) (lsCol st) (TokStringLit "$")
                      newSt = advanceCol 3 st {lsInput = rest3}
                   in lexIndStringMode newSt (litTok : acc)
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
                          litTok = Located (lsLine st) (lsCol st) (TokStringLit escaped)
                          newSt = advanceCol 4 st {lsInput = rest4}
                       in lexIndStringMode newSt (litTok : acc)
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
                              lsBraceDepth = 0
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
              val = applyExponent (readDouble digits decimals) expVal
              tok = Located (lsLine st) (lsCol st) (TokFloat val)
              newSt = advanceCol fullLen st {lsInput = after4}
           in lexNormalMode newSt (tok : acc)
        _
          -- C++ Nix rejects out-of-range integer literals rather than
          -- silently wrapping modulo 2^64.
          | readInteger digits > toInteger (maxBound :: Int64) ->
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

-- | Read a floating-point number from its integer and decimal parts.
-- Total - no 'read', no exceptions.
readDouble :: Text -> Text -> Double
readDouble intPart decPart =
  let whole = fromIntegral (readInteger intPart) :: Double
      fracDigits = T.length decPart
      frac = fromIntegral (readInteger decPart) / (decimalBase' ^ fracDigits)
   in whole + frac

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
                else (sign * readInteger expDigits, afterExp, 1 + signLen + T.length expDigits)
    _ -> (0, input, 0)

-- | Scale a float mantissa by @10 ^^ exp@ (exp may be negative).
applyExponent :: Double -> Integer -> Double
applyExponent mantissa expVal = mantissa * (10 ^^ expVal)

-- | Lex a leading-dot float like @.5@ or @.5e3@ (Nix's @0?\\.[0-9]+@ form).
-- The current input begins with the @.@.
lexLeadingDotFloat :: LexState -> [Located] -> Either ParseError [Located]
lexLeadingDotFloat st acc =
  let afterDot = T.drop 1 (lsInput st)
      (decimals, after3) = T.span isDigit afterDot
      (expVal, after4, expLen) = lexExponent after3
      fullLen = 1 + T.length decimals + expLen
      val = applyExponent (readDouble "" decimals) expVal
      tok = Located (lsLine st) (lsCol st) (TokFloat val)
      newSt = advanceCol fullLen st {lsInput = after4}
   in lexNormalMode newSt (tok : acc)

-- | Base for decimal digit accumulation.
decimalBase :: Integer
decimalBase = 10

-- | Floating-point decimal base for fraction computation.
decimalBase' :: Double
decimalBase' = 10.0

-- | Ordinal of ASCII @\'0\'@ for digit-to-int conversion.
zeroOrd :: Int
zeroOrd = fromEnum '0'

-- ---------------------------------------------------------------------------
-- Identifiers and keywords
-- ---------------------------------------------------------------------------

lexIdentOrKeyword :: LexState -> [Located] -> Either ParseError [Located]
lexIdentOrKeyword st acc =
  let (ident, after) = T.span isIdentChar (lsInput st)
      len = T.length ident
   in -- Check for URI: identifier followed by ://
      case T.stripPrefix "://" after of
        Just afterScheme ->
          let (uriRest, afterUri) = T.span isUriChar afterScheme
              full = ident <> "://" <> uriRest
              fullLen = T.length full
              tok = Located (lsLine st) (lsCol st) (TokUri full)
              newSt = advanceCol fullLen st {lsInput = afterUri}
           in lexNormalMode newSt (tok : acc)
        Nothing ->
          let tok = Located (lsLine st) (lsCol st) (identToToken ident)
              newSt = advanceCol len st {lsInput = after}
           in lexNormalMode newSt (tok : acc)

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
looksLikePath :: Text -> Bool
looksLikePath input =
  case T.break (== '/') (T.takeWhile isPathChar input) of
    (_, slashAndRest) -> case T.uncons (T.drop 1 slashAndRest) of
      Just (afterSlash, _) -> isPathChar afterSlash && afterSlash /= '/'
      Nothing -> False

isSearchPathChar :: Char -> Bool
isSearchPathChar c = isAlphaNum c || c `elem` ("/.~_-+" :: [Char])

isUriChar :: Char -> Bool
isUriChar c = isAlphaNum c || c `elem` ("%/?:@&=+$,#._~!-" :: [Char])

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
