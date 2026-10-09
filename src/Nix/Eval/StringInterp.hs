-- | String coercion and indented-string whitespace stripping.
--
-- Provides 'coerceToString' (string interpolation and @builtins.toString@),
-- 'concatChunks' (a double-quoted string's parts joined) and
-- 'stripIndentedChunks' (the indented-string indentation algorithm, applied
-- by the bytecode evaluator).  Force/apply are passed in as parameters to
-- break the import cycle with @Nix.Eval@.
module Nix.Eval.StringInterp
  ( StringChunk (..),
    concatChunks,
    stripIndentedChunks,
    CoercePath,
    coerceToString,
    formatNixFloat,
    formatJsonFloat,
    formatXmlFloat,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Nix.Eval.Types (MonadEval (..), NixValue (..), StringContext, Thunk, attrSetLookup, emptyContext, typeName)
import Numeric (floatToDigits, showFFloat)

-- | Force a thunk to a value.
type Force m = Thunk -> m NixValue

-- | Apply a function value to an argument value.
type Apply m = NixValue -> NixValue -> m NixValue

-- | What the caller does with a coerced path: the verbatim text for
-- non-copying coercions (@builtins.toString@), the source store path
-- with context for copy-to-store coercions (interpolation, derivation
-- fields, @builtins.toJSON@).  A parameter for the same reason
-- 'Force' and 'Apply' are: the store copy lives above this module,
-- and the attrset case's recursion must carry the caller's choice -
-- an @outPath@ reached through an attrset coerces exactly as the same
-- path written directly would.
type CoercePath m = Text -> m (ByteString, StringContext)

-- | One evaluated part of a string, with the three-way distinction
-- upstream's @stripIndentation@ sees: a literal is measured by the
-- indentation scan and stripped; an escape (@'''@, @''\n@, ...) is
-- stripped but never measured, since upstream's lexer emits it without
-- the hasIndentation mark; an interpolated value is opaque to both and
-- carries the context it was coerced with.
data StringChunk
  = ChunkLit !ByteString
  | ChunkEsc !ByteString
  | ChunkInterp !ByteString !StringContext
  deriving (Eq, Show)

-- | The bytes a chunk contributes, whatever its kind.
chunkBytes :: StringChunk -> ByteString
chunkBytes (ChunkLit t) = t
chunkBytes (ChunkEsc t) = t
chunkBytes (ChunkInterp t _) = t

-- | The combined context of the interpolated chunks.
chunksContext :: [StringChunk] -> StringContext
chunksContext chunks = mconcat [c | ChunkInterp _ c <- chunks]

-- | Join evaluated chunks as a double-quoted string does: bytes in order,
-- nothing stripped.
concatChunks :: [StringChunk] -> (ByteString, StringContext)
concatChunks chunks = (BS.concat (map chunkBytes chunks), chunksContext chunks)

-- | Strip the common indentation from the evaluated parts of an indented
-- string, as upstream's @ParserState::stripIndentation@ (parser-state.hh
-- at 2.24.9, the same through 2.33.2) does at parse time.  Three of its
-- rules are not the obvious ones:
--
-- * The minimum indent starts at 'indentSentinel', and only a character
--   that is neither a space nor a newline lowers it, an escape or an
--   interpolation at line start counting as one.  A string with no such
--   line loses every leading space on every line.
--
-- * Only a space is indentation.  A tab fixes the minimum at its column
--   and stays in the output.
--
-- * Once the last part is stripped, a final line holding only spaces is
--   removed from it, so the closing @''@'s own indentation never reaches
--   the value.  The rule runs on that part's text alone: a string ending
--   in an interpolation keeps whatever follows it.
--
-- Escapes are stripped like literals and skipped by the scan, so an
-- escaped newline starts a line whose spaces are stripped but were never
-- measured; interpolated values are opaque to both passes and leave the
-- line mid-way.  The scan is byte-level via the Char8 view: it only ever
-- compares against space and newline, single bytes in UTF-8 that never
-- occur inside a multi-byte sequence, so multi-byte content passes
-- through untouched.
stripIndentedChunks :: [StringChunk] -> (ByteString, StringContext)
stripIndentedChunks chunks =
  (BS.concat (chunksStrip (chunksMinIndent chunks) chunks), chunksContext chunks)

-- | Upstream's starting minimum (@size_t minIndent = 1000000@): until a
-- line is measured, every leading space is indentation.
indentSentinel :: Int
indentSentinel = 1000000

-- | Scan state: whether the current line has held only spaces so far,
-- how many, and the minimum measured.
data ScanState = ScanState !Bool !Int !Int

-- | The common indentation of the literal chunks.
chunksMinIndent :: [StringChunk] -> Int
chunksMinIndent chunks =
  let ScanState _ _ measured = foldl' scanChunk (ScanState True 0 indentSentinel) chunks
   in measured
  where
    scanChunk st (ChunkLit t) = BC.foldl' scanChar st t
    scanChunk st _ = endLeadingSpace st
    scanChar (ScanState True cur minIndent) ' ' = ScanState True (cur + 1) minIndent
    scanChar (ScanState _ _ minIndent) '\n' = ScanState True 0 minIndent
    scanChar st@(ScanState True _ _) _ = endLeadingSpace st
    scanChar st _ = st
    endLeadingSpace (ScanState True cur minIndent) = ScanState False cur (min minIndent cur)
    endLeadingSpace st = st

-- | Strip state carried across parts: whether the current line has held
-- only spaces so far, and how many of them have been seen.
data StripState = StripState !Bool !Int

-- | Strip @minIndent@ leading spaces from every line of the literal and
-- escape chunks; an interpolated chunk is emitted verbatim.
chunksStrip :: Int -> [StringChunk] -> [ByteString]
chunksStrip minIndent = go (StripState True 0)
  where
    go _ [] = []
    go _ (ChunkInterp t _ : rest) = t : go (StripState False 0) rest
    go st (ChunkLit t : rest) = stripText st t rest
    go st (ChunkEsc t : rest) = stripText st t rest
    stripText st t rest =
      let (stripped, next) = stripLines minIndent st t
       in finishPart rest stripped : go next rest
    -- Upstream's @n == 1@: only the last part sheds a final line of spaces.
    finishPart [] = dropSpaceOnlyLastLine
    finishPart _ = id

-- | Strip one part line by line.  At line start a space is dropped while
-- fewer than @minIndent@ of the line's spaces have been seen (upstream's
-- @curDropped++ >= minIndent@ keeps the rest); a line that is still only
-- spaces at the end of the part hands its count to the next part.
stripLines :: Int -> StripState -> ByteString -> (ByteString, StripState)
stripLines minIndent = go []
  where
    go acc st t
      | BS.null t = (BS.concat (reverse acc), st)
      | otherwise =
          let (line, afterLine) = BC.break (== '\n') t
              (kept, lineEnd) = stripLine st line
           in case BC.uncons afterLine of
                Just (_, remaining) -> go ("\n" : kept : acc) (StripState True 0) remaining
                Nothing -> (BS.concat (reverse (kept : acc)), lineEnd)
    stripLine st@(StripState False _) line = (line, st)
    stripLine (StripState True seen) line
      | BS.null content = (BS.drop dropped spaces, StripState True (seen + spaceCount))
      | otherwise = (BS.drop dropped line, StripState False 0)
      where
        (spaces, content) = BC.span (== ' ') line
        spaceCount = BS.length spaces
        dropped = max 0 (min spaceCount (minIndent - seen))

-- | Remove a final line that holds only spaces, keeping the newline before
-- it (upstream: @find_last_of('\n')@, then nothing but spaces past it).
-- A part with no newline is left alone.
dropSpaceOnlyLastLine :: ByteString -> ByteString
dropSpaceOnlyLastLine s = case BC.elemIndexEnd '\n' s of
  Just p | BC.all (== ' ') (BS.drop (p + 1) s) -> BS.take (p + 1) s
  _ -> s

-- | Coerce a Nix value to a (byte) string.
--
-- The @coerceMore@ flag mirrors C++ Nix's @coerceToString@ argument: when
-- 'True' (e.g. @builtins.toString@, derivation-env values) ints, floats, bools
-- and null coerce permissively; when 'False' (string interpolation,
-- @builtins.concatStringsSep@) those are type errors, matching C++ Nix.
-- Strings, paths, and attribute sets with @__toString@/@outPath@ coerce in
-- both modes; lists and bare functions are always errors.
coerceToString :: (MonadEval m) => Bool -> Force m -> Apply m -> CoercePath m -> NixValue -> m (ByteString, StringContext)
coerceToString _ _ _ _ (VStr s ctx) = pure (s, ctx)
coerceToString _ _ _ coercePathFn (VPath p) = coercePathFn p
coerceToString True _ _ _ (VInt n) = pure (BC.pack (show n), emptyContext)
coerceToString True _ _ _ (VFloat n) = pure (TE.encodeUtf8 (formatNixFloatFixed n), emptyContext)
coerceToString True _ _ _ VNull = pure ("", emptyContext)
coerceToString True _ _ _ (VBool True) = pure ("1", emptyContext)
coerceToString True _ _ _ (VBool False) = pure ("", emptyContext)
-- Attribute sets: try __toString first, then outPath (both modes).
-- The recursion carries the caller's path coercion, so a path-valued
-- @outPath@ lands on the caller's path case, not a fixed one.
coerceToString coerceMore forceFn applyFn coercePathFn (VAttrs attrs) =
  case attrSetLookup "__toString" attrs of
    Just toStrThunk -> do
      toStrFn <- forceFn toStrThunk
      result <- applyFn toStrFn (VAttrs attrs)
      coerceToString coerceMore forceFn applyFn coercePathFn result
    Nothing -> case attrSetLookup "outPath" attrs of
      Just outPathThunk -> do
        outPathVal <- forceFn outPathThunk
        coerceToString coerceMore forceFn applyFn coercePathFn outPathVal
      Nothing ->
        throwEvalError "cannot coerce a set to a string (missing __toString or outPath)"
coerceToString _ _ _ _ other =
  throwEvalError ("cannot coerce " <> typeName other <> " to a string")

-- | Format a float the way C++ Nix's coerceToString does -
-- @std::to_string@, i.e. FIXED 6 decimal places with no trimming:
-- @toString 1.5@ is @"1.500000"@.  Derivation env values and
-- @builtins.toString@ both see this form, so it is hash-relevant.
formatNixFloatFixed :: Double -> Text
formatNixFloatFixed n
  | isNaN n = "nan"
  | isInfinite n = if n > 0 then "inf" else "-inf"
  | otherwise = T.pack (showFFloat (Just 6) n "")

-- | Format a float for value display and JSON: 6 fixed decimal places,
-- then strip trailing zeros and an unnecessary decimal point.  E.g.
-- @1.0@ becomes @"1"@, @3.14@ becomes @"3.14"@.
formatNixFloat :: Double -> Text
formatNixFloat n
  | isNaN n = "nan"
  | isInfinite n = if n > 0 then "inf" else "-inf"
  | otherwise =
      let fixed = showFFloat (Just 6) n ""
       in T.pack (stripZeros fixed)
  where
    stripZeros s
      | '.' `elem` s = reverse (dropDot (dropWhile (== '0') (reverse s)))
      | otherwise = s
    dropDot ('.' : rest) = rest
    dropDot xs = xs

-- | Format a finite float exactly as the JSON serializer upstream links
-- (nlohmann @to_chars@): shortest round-trip digits, laid out as plain
-- decimal only while the decimal point lands within positions
-- 'jsonMinPointPos'..'jsonMaxPointPos', a @.0@ suffix on integral values,
-- and otherwise @d.ddde+XX@ with a signed exponent of at least two digits.
-- Zero is @0.0@ (sign preserved).  Digits come from 'floatToDigits', which
-- is always shortest; nlohmann's grisu2 can emit a longer-than-shortest
-- form for rare values, an accepted divergence.  Non-finite input is the
-- caller's concern (JSON spells it @null@).
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
  | otherwise = mantissa <> "e" <> signedExponent (pointPos - 1)
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

-- | Exponent suffix shared by the JSON and XML float layouts: sign always
-- present, magnitude zero-padded to at least two digits (@+05@, @-21@).
signedExponent :: Int -> String
signedExponent e
  | e < 0 = '-' : padded (negate e)
  | otherwise = '+' : padded e
  where
    padded n
      | n < 10 = '0' : show n
      | otherwise = show n

-- | Format a float as upstream @toXML@ renders one - C++ @operator<<@ on a
-- default-format ostream: 6 significant digits, trailing zeros stripped,
-- plain decimal only for decimal exponents in [-4, 5], otherwise
-- @d.ddde+XX@ with a signed exponent of at least two digits.  Rounding is
-- half-even on the exact binary value, matching a correctly-rounded printf.
formatXmlFloat :: Double -> Text
formatXmlFloat d
  | isNaN d = "nan"
  | isInfinite d = if d > 0 then "inf" else "-inf"
  | d == 0 = if isNegativeZero d then "-0" else "0"
  | d < 0 = "-" <> formatXmlFloat (negate d)
  | otherwise = T.pack (xmlFloatPositive d)

-- | 6-significant-digit @%g@ layout of a positive finite double.
xmlFloatPositive :: Double -> String
xmlFloatPositive d =
  let exact = toRational d
      roughExp = decimalExponentOf exact
      rounded = round (exact * 10 ^^ (xmlSigDigits - 1 - roughExp)) :: Integer
      -- Rounding can carry into a new leading digit (999999.9 -> 1000000).
      (sigDigits, pointExp) =
        if rounded >= 10 ^ xmlSigDigits
          then (rounded `div` 10, roughExp + 1)
          else (rounded, roughExp)
      digits = show sigDigits
   in if xmlMinFixedExp <= pointExp && pointExp < xmlSigDigits
        then fixedForm digits pointExp
        else sciForm digits pointExp
  where
    fixedForm digits pointExp
      | pointExp >= 0 =
          let (intPart, fracPart) = splitAt (pointExp + 1) digits
           in joinFraction intPart (stripTrailingZeros fracPart)
      | otherwise =
          joinFraction "0" (stripTrailingZeros (replicate (negate pointExp - 1) '0' <> digits))
    sciForm digits pointExp =
      joinFraction (take 1 digits) (stripTrailingZeros (drop 1 digits))
        <> "e"
        <> signedExponent pointExp
    joinFraction intPart fracPart
      | null fracPart = intPart
      | otherwise = intPart <> "." <> fracPart
    stripTrailingZeros = reverse . dropWhile (== '0') . reverse

-- | @%g@ default precision: 6 significant digits.
xmlSigDigits :: Int
xmlSigDigits = 6

-- | @%g@ switches to scientific below a decimal exponent of -4.
xmlMinFixedExp :: Int
xmlMinFixedExp = -4

-- | The decimal exponent @e@ of a positive rational: the unique @e@ with
-- @10^e <= r < 10^(e+1)@.  A float log gives the estimate; the exact
-- comparisons correct it, since the log is off by one near powers of ten.
decimalExponentOf :: Rational -> Int
decimalExponentOf r = correct (floor (logBase 10 (fromRational r :: Double)))
  where
    correct e
      | 10 ^^ e > r = correct (e - 1)
      | 10 ^^ (e + 1) <= r = correct (e + 1)
      | otherwise = e
