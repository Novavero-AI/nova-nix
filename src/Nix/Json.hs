-- | JSON as upstream Nix writes and reads it.
--
-- Upstream serializes through nlohmann::json, so @builtins.toJSON@, the
-- @__json@ environment entry of a structured-attributes derivation and the
-- @.attrs.json@ file its builder reads are all nlohmann's @dump()@ with no
-- indentation.  A derivation's @__json@ is hashed into its store path, so
-- 'renderJson' is identity, not presentation: every byte of it is
-- nlohmann's.  'parseJson' is the way back, for the builder, which reads
-- @__json@ out of a @.drv@ the way upstream's @nlohmann::json::parse@ does.
module Nix.Json
  ( -- * Values
    Json (..),

    -- * Rendering
    renderJson,
    renderJsonWith,
    formatJsonFloat,

    -- * Parsing
    parseJson,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Char (chr, isDigit, ord)
import Data.Int (Int64)
import Data.List (intersperse)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word64)
import Numeric (floatToDigits, showHex)
import Text.Read (readMaybe)

-- ---------------------------------------------------------------------------
-- Values
-- ---------------------------------------------------------------------------

-- | A JSON value.  Strings are 'Text' because nlohmann refuses a string
-- that is not UTF-8 in both directions, so bytes reach JSON decoded once,
-- at the boundary that produced them.  Integers and floats stay apart, as
-- nlohmann keeps them: @1@ and @1.0@ render differently, and a builder's
-- @.attrs.sh@ treats them differently.
data Json
  = JsonNull
  | JsonBool !Bool
  | -- | Rendered in decimal.  'parseJson' produces one only in nlohmann's
    -- integer range, signed 64-bit below zero and unsigned 64-bit above.
    JsonInt !Integer
  | -- | A non-finite float renders as @null@, as nlohmann writes one.
    JsonFloat !Double
  | JsonString !Text
  | JsonArray ![Json]
  | -- | Members render in byte order of their names, the order of the
    -- @std::map@ nlohmann keeps them in.  'Text' compares by code point,
    -- which on UTF-8 is the same order.
    JsonObject !(Map Text Json)
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Rendering
-- ---------------------------------------------------------------------------

-- | nlohmann's @dump()@: no whitespace, members in name order, strings
-- escaped as 'escapeJsonString' describes, floats as 'formatJsonFloat'.
renderJson :: Json -> ByteString
renderJson = renderJsonWith id

-- | 'renderJson' with every string, member names included, passed through
-- a rewrite before it is escaped.  Upstream's builder rewrites the rendered
-- text instead (@rewriteStrings@ over @json.dump()@).  The two agree while
-- neither side of a rewrite holds a character JSON escapes; rewriting
-- first is what keeps the result JSON when a replacement does hold one,
-- as a Windows store directory holds backslashes.
renderJsonWith :: (Text -> Text) -> Json -> ByteString
renderJsonWith rewrite = BL.toStrict . BB.toLazyByteString . render
  where
    render JsonNull = "null"
    render (JsonBool True) = "true"
    render (JsonBool False) = "false"
    render (JsonInt n) = BB.integerDec n
    render (JsonFloat d)
      | isNaN d || isInfinite d = "null"
      | otherwise = TE.encodeUtf8Builder (formatJsonFloat d)
    render (JsonString s) = quoted s
    render (JsonArray items) = BB.char7 '[' <> commaSeparated (map render items) <> BB.char7 ']'
    render (JsonObject members) =
      BB.char7 '{' <> commaSeparated (map member (Map.toAscList members)) <> BB.char7 '}'
    member (name, value) = quoted name <> BB.char7 ':' <> render value
    quoted s = BB.char7 '"' <> escapeJsonString (rewrite s) <> BB.char7 '"'
    commaSeparated = mconcat . intersperse (BB.char7 ',')

-- | nlohmann's @dump_escaped@ without @ensure_ascii@: two-character escapes
-- for the quote, the backslash, backspace, form feed, newline, carriage
-- return and tab; @\\u00xx@ in lowercase hex for the other control
-- characters; every other character as its UTF-8 bytes, DEL and @/@
-- included.
escapeJsonString :: Text -> BB.Builder
escapeJsonString s = case T.break needsEscape s of
  (plain, rest) ->
    TE.encodeUtf8Builder plain <> case T.uncons rest of
      Nothing -> mempty
      Just (c, more) -> escapeChar c <> escapeJsonString more
  where
    needsEscape c = c == '"' || c == '\\' || c < ' '
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\b' = "\\b"
    escapeChar '\f' = "\\f"
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar c = "\\u00" <> BB.word8HexFixed (fromIntegral (ord c))

-- | Format a finite float exactly as nlohmann's @to_chars@ does: shortest
-- round-trip digits, laid out as plain decimal only while the decimal
-- point lands within positions 'jsonMinPointPos'..'jsonMaxPointPos', a
-- @.0@ suffix on integral values, and otherwise @d.ddde+XX@ with a signed
-- exponent of at least two digits.  Zero is @0.0@ (sign preserved).
-- Digits come from 'floatToDigits', which is always shortest; nlohmann's
-- grisu2 can emit a longer-than-shortest form for rare values, an accepted
-- divergence.  Non-finite input is the caller's concern (JSON spells it
-- @null@).
formatJsonFloat :: Double -> Text
formatJsonFloat d
  | isNegativeZero d = "-0.0"
  | d == 0 = "0.0"
  | d < 0 = "-" <> formatJsonFloat (negate d)
  | otherwise =
      let (digitList, pointPos) = floatToDigits 10 d
          digits = concatMap show digitList
       in T.pack (jsonFloatLayout digits (length digits) pointPos)

-- | Positional layout of shortest digits with the decimal point at
-- @pointPos@, replicating nlohmann's @format_buffer@ branch by branch.
jsonFloatLayout :: String -> Int -> Int -> String
jsonFloatLayout digits digitCount pointPos
  -- Integral value with the point in plain range: digits, zeros, ".0".
  | digitCount <= pointPos && pointPos <= jsonMaxPointPos =
      digits <> replicate (pointPos - digitCount) '0' <> ".0"
  -- Point falls inside the digit run.
  | 0 < pointPos && pointPos <= jsonMaxPointPos =
      take pointPos digits <> "." <> drop pointPos digits
  -- Small magnitude: leading "0." and padding zeros.
  | jsonMinPointPos < pointPos && pointPos <= 0 =
      "0." <> replicate (negate pointPos) '0' <> digits
  -- Scientific notation.
  | otherwise = mantissa <> "e" <> exponentDigits (pointPos - 1)
  where
    mantissa = case digits of
      [single] -> [single]
      lead : rest -> lead : '.' : rest
      [] -> "0" -- unreachable: a positive double yields at least one digit

-- | nlohmann @format_buffer@ bounds (@kMaxExp@ = double's @digits10@,
-- @kMinExp@): plain decimal only while the decimal point position is in
-- (-4, 15]; everything else is scientific.
jsonMaxPointPos :: Int
jsonMaxPointPos = 15

-- | Lower point-position bound, exclusive.  See 'jsonMaxPointPos'.
jsonMinPointPos :: Int
jsonMinPointPos = -4

-- | nlohmann's @append_exponent@: the sign always, then the magnitude
-- zero-padded to at least two digits (@+05@, @-21@, @+308@).
exponentDigits :: Int -> String
exponentDigits e
  | e < 0 = '-' : padded (negate e)
  | otherwise = '+' : padded e
  where
    padded n
      | n < 10 = '0' : show n
      | otherwise = show n

-- ---------------------------------------------------------------------------
-- Parsing
-- ---------------------------------------------------------------------------

-- | Read one JSON document as nlohmann's @parse@ does: a single value with
-- optional whitespace around it, a leading byte order mark skipped, and
-- nothing after it.  A member name given twice keeps its last value, as
-- nlohmann's DOM parser assigns it.  The input is 'Text' because nlohmann
-- rejects a document that is not UTF-8; decoding it is that check.
parseJson :: Text -> Either Text Json
parseJson input = do
  (value, rest) <- parseValue (skipSpace (dropByteOrderMark input))
  let trailing = skipSpace rest
  if T.null trailing
    then Right value
    else Left ("unexpected content after the JSON value: " <> excerpt trailing)
  where
    dropByteOrderMark t = fromMaybe t (T.stripPrefix (T.singleton byteOrderMark) t)

-- | U+FEFF, which nlohmann skips at the start of a document.
byteOrderMark :: Char
byteOrderMark = '\xFEFF'

-- | JSON's insignificant whitespace: space, tab, newline, carriage return.
skipSpace :: Text -> Text
skipSpace = T.dropWhile (\c -> c == ' ' || c == '\t' || c == '\n' || c == '\r')

-- | The start of the input a parse stopped at, for an error message.
excerpt :: Text -> Text
excerpt t
  | T.null t = "end of input"
  | otherwise = "'" <> T.take excerptLength t <> "'"

-- | How much of the offending input an error quotes.
excerptLength :: Int
excerptLength = 20

-- | One value, its leading whitespace already skipped.
parseValue :: Text -> Either Text (Json, Text)
parseValue t = case T.uncons t of
  Nothing -> Left "unexpected end of input, expected a JSON value"
  Just ('{', rest) -> parseObject (skipSpace rest)
  Just ('[', rest) -> parseArray (skipSpace rest)
  Just ('"', rest) -> do
    (s, afterString) <- parseString rest
    pure (JsonString s, afterString)
  Just ('t', _) -> literal "true" (JsonBool True)
  Just ('f', _) -> literal "false" (JsonBool False)
  Just ('n', _) -> literal "null" JsonNull
  Just (c, _)
    | c == '-' || isDigit c -> parseNumber t
  Just _ -> Left ("unexpected " <> excerpt t <> ", expected a JSON value")
  where
    literal word value = case T.stripPrefix word t of
      Just rest -> Right (value, rest)
      Nothing -> Left ("invalid literal " <> excerpt t)

-- | The elements of an array, after its @[@ and any whitespace.
parseArray :: Text -> Either Text (Json, Text)
parseArray t = case T.uncons t of
  Just (']', rest) -> Right (JsonArray [], rest)
  _ -> elements [] t
  where
    elements !acc s = do
      (value, rest) <- parseValue s
      let after = skipSpace rest
      case T.uncons after of
        Just (',', more) -> elements (value : acc) (skipSpace more)
        Just (']', more) -> Right (JsonArray (reverse (value : acc)), more)
        _ -> Left ("unexpected " <> excerpt after <> " in an array, expected ',' or ']'")

-- | The members of an object, after its @{@ and any whitespace.
parseObject :: Text -> Either Text (Json, Text)
parseObject t = case T.uncons t of
  Just ('}', rest) -> Right (JsonObject Map.empty, rest)
  _ -> members Map.empty t
  where
    members !acc s = case T.uncons s of
      Just ('"', afterQuote) -> do
        (name, afterName) <- parseString afterQuote
        let beforeColon = skipSpace afterName
        case T.uncons beforeColon of
          Just (':', afterColon) -> do
            (value, afterValue) <- parseValue (skipSpace afterColon)
            let updated = Map.insert name value acc
                after = skipSpace afterValue
            case T.uncons after of
              Just (',', more) -> members updated (skipSpace more)
              Just ('}', more) -> Right (JsonObject updated, more)
              _ -> Left ("unexpected " <> excerpt after <> " in an object, expected ',' or '}'")
          _ -> Left ("unexpected " <> excerpt beforeColon <> " after a member name, expected ':'")
      _ -> Left ("unexpected " <> excerpt s <> " in an object, expected a member name")

-- | The rest of a string after its opening quote.  A control character
-- must arrive escaped; nlohmann rejects one written raw.
parseString :: Text -> Either Text (Text, Text)
parseString = collect []
  where
    collect !chunks t =
      let (plain, rest) = T.break special t
          gathered = plain : chunks
       in case T.uncons rest of
            Nothing -> Left "unterminated string"
            Just ('"', more) -> Right (T.concat (reverse gathered), more)
            Just ('\\', more) -> do
              (decoded, afterEscape) <- parseEscape more
              collect (decoded : gathered) afterEscape
            Just (c, _) -> Left ("control character U+" <> hex4 (ord c) <> " must be escaped")
    special c = c == '"' || c == '\\' || c < ' '
    hex4 n = T.justifyRight 4 '0' (T.toUpper (T.pack (showHex n "")))

-- | One escape sequence, after its backslash.  A @\\u@ escape naming a
-- UTF-16 high surrogate must be followed by one naming a low surrogate,
-- and a low surrogate alone is an error, as nlohmann's lexer has it.
parseEscape :: Text -> Either Text (Text, Text)
parseEscape t = case T.uncons t of
  Just ('"', rest) -> Right ("\"", rest)
  Just ('\\', rest) -> Right ("\\", rest)
  Just ('/', rest) -> Right ("/", rest)
  Just ('b', rest) -> Right ("\b", rest)
  Just ('f', rest) -> Right ("\f", rest)
  Just ('n', rest) -> Right ("\n", rest)
  Just ('r', rest) -> Right ("\r", rest)
  Just ('t', rest) -> Right ("\t", rest)
  Just ('u', rest) -> do
    (unit, afterUnit) <- codeUnit rest
    codePoint unit afterUnit
  _ -> Left ("invalid escape \\" <> T.take 1 t)
  where
    codePoint unit afterUnit
      | isHighSurrogate unit = case T.stripPrefix "\\u" afterUnit of
          Just afterMarker -> do
            (low, afterLow) <- codeUnit afterMarker
            if isLowSurrogate low
              then Right (T.singleton (chr (surrogateBase + (unit - highSurrogateMin) * surrogateSpan + (low - lowSurrogateMin))), afterLow)
              else Left "a high surrogate must be followed by a low surrogate"
          Nothing -> Left "a high surrogate must be followed by a low surrogate"
      | isLowSurrogate unit = Left "a low surrogate must follow a high surrogate"
      | otherwise = Right (T.singleton (chr unit), afterUnit)
    isHighSurrogate u = u >= highSurrogateMin && u < lowSurrogateMin
    isLowSurrogate u = u >= lowSurrogateMin && u < lowSurrogateMin + surrogateSpan

-- | UTF-16 surrogate ranges: high surrogates from U+D800, low from U+DC00,
-- each 0x400 wide, a pair naming a code point from U+10000.
highSurrogateMin, lowSurrogateMin, surrogateSpan, surrogateBase :: Int
highSurrogateMin = 0xD800
lowSurrogateMin = 0xDC00
surrogateSpan = 0x400
surrogateBase = 0x10000

-- | The four hex digits of a @\\u@ escape, either case.
codeUnit :: Text -> Either Text (Int, Text)
codeUnit t =
  let (digits, rest) = T.splitAt 4 t
   in case traverse hexValue (T.unpack digits) of
        Just values
          | length values == 4 -> Right (foldl' (\acc v -> acc * 16 + v) 0 values, rest)
        _ -> Left ("invalid \\u escape " <> excerpt t)
  where
    hexValue c
      | isDigit c = Just (ord c - ord '0')
      | c >= 'a' && c <= 'f' = Just (ord c - ord 'a' + 10)
      | c >= 'A' && c <= 'F' = Just (ord c - ord 'A' + 10)
      | otherwise = Nothing

-- | A number, as nlohmann's lexer scans it: an optional minus, an integer
-- part with no leading zero, an optional fraction and exponent.  One with
-- neither is an integer while it fits nlohmann's integer types (strtoll
-- below zero, strtoull above) and a float past them, as nlohmann falls
-- back; any other is a float (strtod, which 'readMaybe' matches: correctly
-- rounded, infinite past the double range).  An infinite one is an error,
-- nlohmann's out_of_range 406.
parseNumber :: Text -> Either Text (Json, Text)
parseNumber t = do
  let (sign, afterSign) = case T.stripPrefix "-" t of
        Just rest -> ("-", rest)
        Nothing -> ("", t)
  (integral, afterIntegral) <- integerPart afterSign
  (fraction, afterFraction) <- optionalPart "." afterIntegral
  (exponentText, afterExponent) <- exponentPart afterFraction
  let lexeme = sign <> integral <> fraction <> exponentText
      asFloat = case readMaybe (T.unpack lexeme) of
        Just d
          | isInfinite d -> Left ("number overflow parsing '" <> lexeme <> "'")
          | otherwise -> Right (JsonFloat d, afterExponent)
        Nothing -> Left ("invalid number " <> excerpt t)
  if T.null fraction && T.null exponentText
    then case readMaybe (T.unpack lexeme) of
      Just n
        | n >= toInteger (minBound :: Int64) && n <= toInteger (maxBound :: Word64) -> Right (JsonInt n, afterExponent)
      _ -> asFloat
    else asFloat
  where
    integerPart s = case T.uncons s of
      Just ('0', rest) -> Right ("0", rest)
      Just (c, _)
        | isDigit c -> Right (T.span isDigit s)
      _ -> Left ("invalid number " <> excerpt t)
    optionalPart marker s = case T.stripPrefix marker s of
      Nothing -> Right ("", s)
      Just rest -> digitsAfter marker rest
    exponentPart s = case T.uncons s of
      Just (e, rest)
        | e == 'e' || e == 'E' -> case T.uncons rest of
            Just (signChar, afterSignChar)
              | signChar == '+' || signChar == '-' -> digitsAfter (T.pack [e, signChar]) afterSignChar
            _ -> digitsAfter (T.singleton e) rest
      _ -> Right ("", s)
    digitsAfter marker s = case T.span isDigit s of
      (digits, rest)
        | T.null digits -> Left ("invalid number " <> excerpt t)
        | otherwise -> Right (marker <> digits, rest)
