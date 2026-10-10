-- | The value printers upstream writes values with.
--
-- @printValue@ (print.cc at Nix 2.24.9) quotes a value in a message.
-- @builtins.trace@ prints a value that is not a string through it, and a
-- type error that shows the offending value prints it after the type, as
-- in @expected a set but found an integer: 1@.
--
-- Upstream prints with one of two option sets: its defaults, which print
-- the whole value, and @errorPrintOptions@ (print-options.hh), which cut
-- an oversized value short so it cannot flood an error message.
--
-- Neither option set forces anything, so in 'printValue' an element or
-- attribute not yet evaluated prints as upstream's thunk marker, and which
-- ones do follows nova-nix's thunk allocation.  That is upstream's for a
-- literal written in the list or set itself, for a function's argument,
-- and for anything already forced.  It is not where upstream hands over a
-- value without a thunk and nova-nix makes one (an indented string, a
-- @~\/@ path, a variable bound to a literal by @let@ or @rec@, a global
-- such as @map@).  A function value carries neither the name nor
-- the source position upstream prints after @lambda@, so its marker holds
-- the word alone.  A set or list reached a second time prints again where
-- upstream marks it repeated, which needs value identity this printer
-- does not track.
--
-- Upstream's @printAmbiguous@ (print-ambiguous.cc) writes what
-- @nix-instantiate --eval@ prints, and 'printAmbiguous' writes what
-- @nova-nix eval@ prints.  It renders to bytes: a string's payload is
-- bytes that need not be UTF-8, and upstream writes them as they are.
module Nix.Eval.Print
  ( PrintOptions (..),
    printValue,
    printAmbiguous,
    formatXmlFloat,
  )
where

import Control.Monad.State.Strict (State, evalState, gets, modify')
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List (partition)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Nix.Eval.Types (AttrSet, NixValue (..), Thunk (..), attrSetToAscList, bytesToTextLossy, clistThunks, readThunkValue, thunkUnderEvaluation)

-- | Which of upstream's option sets a print follows.
data PrintOptions
  = -- | Upstream's defaults: the whole value, as @builtins.trace@ prints it.
    PrintInFull
  | -- | @errorPrintOptions@: nesting, attributes, list items and string
    -- bytes past its limits are cut, and what was cut is counted instead.
    PrintForError

-- | A bound one of the option sets puts on a print.
data Limit = Unlimited | AtMost !Int

-- | Whether a count is still inside a bound.
within :: Int -> Limit -> Bool
within _ Unlimited = True
within count (AtMost bound) = count < bound

-- | @errorPrintOptions@ (print-options.hh at 2.24.9): how deep a print
-- goes, and how many attributes, list items and string bytes it shows.
errorMaxDepth, errorMaxAttrs, errorMaxListItems, errorMaxStringLength :: Int
errorMaxDepth = 10
errorMaxAttrs = 10
errorMaxListItems = 10
errorMaxStringLength = 1024

depthLimit, attrLimit, itemLimit, stringLimit :: PrintOptions -> Limit
depthLimit = limitedTo errorMaxDepth
attrLimit = limitedTo errorMaxAttrs
itemLimit = limitedTo errorMaxListItems
stringLimit = limitedTo errorMaxStringLength

-- | The defaults bound nothing; @errorPrintOptions@ bounds at its value.
limitedTo :: Int -> PrintOptions -> Limit
limitedTo _ PrintInFull = Unlimited
limitedTo bound PrintForError = AtMost bound

-- | How many attributes and list items one print has shown.  Upstream's
-- bounds on them count over the whole print, not per set or list.
data Shown = Shown {shownAttrs :: !Int, shownItems :: !Int}

type Printer = State Shown

-- | Render a value as upstream's @printValue@ renders it under the given
-- options.
printValue :: PrintOptions -> NixValue -> Text
printValue options val = evalState (printAt options 0 val) (Shown 0 0)

-- | A value at a nesting depth, the top-level value being at 0.
printAt :: PrintOptions -> Int -> NixValue -> Printer Text
printAt options depth val = case val of
  VInt n -> pure (T.pack (show n))
  VFloat f -> pure (formatXmlFloat f)
  VBool True -> pure "true"
  VBool False -> pure "false"
  VNull -> pure "null"
  VStr bytes _ -> pure (printString options bytes)
  VPath p -> pure p
  VList cl -> printList options depth (map Thunk (clistThunks cl))
  VAttrs attrs -> printAttrs options depth attrs
  -- What upstream prints for a lambda whose expression it cannot reach.
  VLambda {} -> pure (marker "lambda")
  VBuiltin name [] -> pure (marker ("primop " <> name))
  VBuiltin name _ -> pure (marker ("partially applied primop " <> name))
  -- A compiled pattern exists only among the arguments of a partially
  -- applied match or split and names neither, so it prints as upstream
  -- prints an application whose primop it cannot name.
  VCompiledRegex _ -> pure (marker "partially applied primop")

-- | Upstream's @printAttrs@.  A set's attributes count toward the bound
-- after the attributes of any set inside them, as upstream counts one
-- only once its value has printed.
printAttrs :: PrintOptions -> Int -> AttrSet -> Printer Text
printAttrs options depth attrs
  | within depth (depthLimit options) =
      bracketed "{" "}" <$> printBounded (attrLimit options) shownAttrs countAttr elidedAttrs (map printAttr (orderedAttrs options attrs))
  | otherwise = pure "{ ... }"
  where
    printAttr (name, thunk) = do
      shown <- printThunk options (depth + 1) thunk
      pure (builderText (printIdentifier (TE.encodeUtf8 name)) <> " = " <> shown <> ";")
    countAttr counts = counts {shownAttrs = shownAttrs counts + 1}
    elidedAttrs left = elided left "attribute" "attributes"

-- | Upstream's @printList@.
printList :: PrintOptions -> Int -> [Thunk] -> Printer Text
printList options depth thunks
  | within depth (depthLimit options) =
      bracketed "[" "]" <$> printBounded (itemLimit options) shownItems countItem elidedItems (map (printThunk options (depth + 1)) thunks)
  | otherwise = pure "[ ... ]"
  where
    countItem counts = counts {shownItems = shownItems counts + 1}
    elidedItems left = elided left "item" "items"

-- | Print the entries of one set or list while the print's running count
-- is inside the bound, then, in place of the rest, how many were left.
printBounded :: Limit -> (Shown -> Int) -> (Shown -> Shown) -> (Int -> Text) -> [Printer Text] -> Printer [Text]
printBounded bound counted count elision = go
  where
    go :: [Printer Text] -> Printer [Text]
    go [] = pure []
    go entries@(entry : rest) = gets counted >>= continueAt
      where
        continueAt sofar
          | within sofar bound = do
              shown <- entry
              modify' count
              (shown :) <$> go rest
          | otherwise = pure [elision (length entries)]

-- | A set's attributes in upstream's order: by name, with @_type@ and
-- @type@ ahead of the rest wherever attributes are bounded (print.cc sorts
-- by its @ImportantFirstAttrNameCmp@ then).
orderedAttrs :: PrintOptions -> AttrSet -> [(Text, Thunk)]
orderedAttrs options attrs = case attrLimit options of
  Unlimited -> byName
  AtMost _ -> important <> rest
  where
    byName = attrSetToAscList attrs
    (important, rest) = partition ((`elem` importantAttrNames) . fst) byName

-- | Upstream's @isImportantAttrName@.
importantAttrNames :: [Text]
importantAttrNames = ["_type", "type"]

-- | An element or attribute value, read without forcing it: its value
-- once it has one, and otherwise upstream's marker for a thunk, or for a
-- thunk whose force is still running.
printThunk :: PrintOptions -> Int -> Thunk -> Printer Text
printThunk options depth thunk = case readThunkValue thunk of
  Just val -> printAt options depth val
  Nothing
    | thunkUnderEvaluation thunk -> pure (marker "potential infinite recursion")
    | otherwise -> pure (marker "thunk")

-- | Upstream's @printLiteralString@: quoted and escaped so the lexer reads
-- the same string back.  The cut and the count of what it left out are in
-- bytes, as upstream's are.  A message is 'Text', so a multi-byte
-- character the cut splits decodes to U+FFFD where upstream writes the
-- partial bytes raw.
printString :: PrintOptions -> ByteString -> Text
printString options bytes = builderText (quotedBytes shown bytes) <> elision
  where
    total = BS.length bytes
    shown = case stringLimit options of
      Unlimited -> total
      AtMost bound -> min bound total
    elision
      | total == shown = ""
      | otherwise = " " <> elided (total - shown) "byte" "bytes"

-- | The first @count@ bytes as a string literal.
quotedBytes :: Int -> ByteString -> BB.Builder
quotedBytes count bytes = "\"" <> escapeBytes count bytes <> "\""

-- | The first @count@ bytes, escaped.  The test for @${@ looks one byte
-- past the cut, as upstream's does, so a @$@ shown last is escaped when
-- the @{@ after it was cut.
escapeBytes :: Int -> ByteString -> BB.Builder
escapeBytes count bytes = case BC.uncons bytes of
  Just (c, rest) | count > 0 -> escapeChar c rest <> escapeBytes (count - 1) rest
  _ -> mempty

escapeChar :: Char -> ByteString -> BB.Builder
escapeChar c rest = case c of
  '"' -> "\\\""
  '\\' -> "\\\\"
  '\n' -> "\\n"
  '\r' -> "\\r"
  '\t' -> "\\t"
  '$' | "{" `BS.isPrefixOf` rest -> "\\$"
  _ -> BB.char8 c

-- | Upstream's @printElided@, with its @pluralize@: how many of a thing a
-- bound left out.
elided :: Int -> Text -> Text -> Text
elided 1 single _ = marker ("1 " <> single <> " elided")
elided count _ plural = marker (T.pack (show count) <> " " <> plural <> " elided")

-- | Upstream brackets its markers in guillemets (U+00AB, U+00BB), written
-- as escapes to keep the source ASCII.
marker :: Text -> Text
marker inner = "\x00AB" <> inner <> "\x00BB"

-- | Rendered bytes as message text.
builderText :: BB.Builder -> Text
builderText = bytesToTextLossy . BL.toStrict . BB.toLazyByteString

-- ---------------------------------------------------------------------------
-- The eval result
-- ---------------------------------------------------------------------------

-- | Render a value as @nix-instantiate --eval@ prints its result, through
-- upstream's @printAmbiguous@ (print-ambiguous.cc at 2.24.9), without the
-- newline the command writes after it.  A string prints as its own bytes,
-- escaped as 'printValue' escapes it and never cut, and a float in a
-- default ostream's six-digit form, as 'formatXmlFloat' writes it.
printAmbiguous :: NixValue -> BB.Builder
printAmbiguous val = case val of
  VInt n -> BB.int64Dec n
  VFloat f -> TE.encodeUtf8Builder (formatXmlFloat f)
  VBool True -> "true"
  VBool False -> "false"
  VNull -> "null"
  VStr bytes _ -> quotedBytes (BS.length bytes) bytes
  VPath p -> TE.encodeUtf8Builder p
  VList cl -> bracketed "[" "]" (map (printElement . Thunk) (clistThunks cl))
  VAttrs attrs -> bracketed "{" "}" (map printAttribute (attrSetToAscList attrs))
  VLambda {} -> "<LAMBDA>"
  VBuiltin _ [] -> "<PRIMOP>"
  VBuiltin _ _ -> "<PRIMOP-APP>"
  -- A compiled pattern exists only among the arguments of a partially
  -- applied match or split, so it prints as that application does.
  VCompiledRegex _ -> "<PRIMOP-APP>"
  where
    printAttribute (name, thunk) =
      printIdentifier (TE.encodeUtf8 name) <> " = " <> printElement thunk <> ";"

-- | Upstream follows the opening bracket and every item with a space, so
-- an empty list is @[ ]@.  Both printers bracket this way.
bracketed :: (IsString s, Monoid s) => s -> s -> [s] -> s
bracketed open close items = open <> " " <> foldMap (<> " ") items <> close

-- | A list element or attribute value: its value when it has one, and
-- upstream's marker for a thunk when it was never forced.
printElement :: Thunk -> BB.Builder
printElement thunk = maybe "<CODE>" printAmbiguous (readThunkValue thunk)

-- | Upstream's @printIdentifier@ (print.cc), which @printAmbiguous@ writes
-- an attribute name through (nixexpr.cc's @operator<<@ on a symbol): a
-- name the lexer reads back as an identifier prints bare, and any other
-- prints as a string literal.  The test is on bytes, so a byte past ASCII
-- always quotes.
printIdentifier :: ByteString -> BB.Builder
printIdentifier name
  | isBareIdentifier name = BB.byteString name
  | otherwise = quotedBytes (BS.length name) name

isBareIdentifier :: ByteString -> Bool
isBareIdentifier name = case BC.uncons name of
  Just (first, rest) ->
    identifierStart first
      && BC.all identifierChar rest
      && name `notElem` reservedKeywords
  Nothing -> False
  where
    identifierStart c = isAsciiLower c || isAsciiUpper c || c == '_'
    identifierChar c = identifierStart c || isDigit c || c == '\'' || c == '-'

-- | Upstream's @isReservedKeyword@ set (print.cc).  @or@ is not in it: the
-- lexer accepts @or@ as an attribute name.
reservedKeywords :: [ByteString]
reservedKeywords = ["if", "then", "else", "assert", "with", "let", "in", "rec", "inherit"]

-- ---------------------------------------------------------------------------
-- Floats
-- ---------------------------------------------------------------------------

-- | Exponent suffix of the XML float layout, as printf writes it: sign
-- always present, magnitude zero-padded to at least two digits (@+05@,
-- @-21@).
signedExponent :: Int -> String
signedExponent e
  | e < 0 = '-' : padded (negate e)
  | otherwise = '+' : padded e
  where
    padded n
      | n < 10 = '0' : show n
      | otherwise = show n

-- | Format a float as upstream @toXML@ and @printValue@ render one - C++
-- @operator<<@ on a default-format ostream: 6 significant digits, trailing
-- zeros stripped, plain decimal only for decimal exponents in [-4, 5],
-- otherwise @d.ddde+XX@ with a signed exponent of at least two digits.
-- Rounding is half-even on the exact binary value, matching a
-- correctly-rounded printf.
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
