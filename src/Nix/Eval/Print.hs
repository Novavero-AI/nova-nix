{-# LANGUAGE LambdaCase #-}

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
-- the word alone.
--
-- Upstream's @printAmbiguous@ (print-ambiguous.cc) writes what
-- @nix-instantiate --eval@ prints, and 'printAmbiguous' writes what
-- @nova-nix eval@ prints.  It renders to bytes: a string's payload is
-- bytes that need not be UTF-8, and upstream writes them as they are.
--
-- Both printers, like upstream's, print a set or list they reach a second
-- time as a repeated marker, so a value that holds itself, as every
-- derivation does through its @all@ and output attributes, prints to an
-- end.  The same means the same set or list, not an equal one: two sets
-- made apart with the same attributes both print in full.
module Nix.Eval.Print
  ( PrintOptions (..),
    printValue,
    printAmbiguous,
    printSymbol,
    formatXmlFloat,
  )
where

import Control.Monad.State.Strict (StateT, evalStateT, gets, lift, modify')
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List (partition)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Nix.Eval.Builtin (builtinName)
import Nix.Eval.CData (MonadCData)
import Nix.Eval.CList (CListPtr)
import Nix.Eval.CThunk (CThunkPtr)
import Nix.Eval.Types (AttrSet (..), CAttrSet, CList (..), NixValue (..), Thunk (..), attrSetNull, attrSetToAscList, bytesToTextLossy, clistLen, clistThunks, readThunkValue, thunkUnderEvaluation)

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

-- | What one print has done so far: how many attributes and list items it
-- has shown, and which sets and lists it has entered.  Upstream counts
-- toward its bounds, and remembers what it has entered, over the whole
-- print, not per set or list.  'printAmbiguous' bounds nothing, so only
-- what it has entered changes there.
data Shown = Shown
  { shownAttrs :: !Int,
    shownItems :: !Int,
    shownEntered :: !(Set Entered)
  }

-- | A print in progress, reading the C data layer as it goes.
type Printer m = StateT Shown m

-- | A print that has shown and entered nothing yet.
nothingShown :: Shown
nothingShown = Shown 0 0 Set.empty

-- | Render a value as upstream's @printValue@ renders it under the given
-- options.
printValue :: (MonadCData m) => PrintOptions -> NixValue -> m Text
printValue options val = evalStateT (printAt options 0 Nothing val) nothingShown

-- | A value at a nesting depth, the top-level value being at 0, with the
-- cell it was read from.  The top-level value is handed over without one.
printAt :: (MonadCData m) => PrintOptions -> Int -> Maybe Thunk -> NixValue -> Printer m Text
printAt options depth cell val = case val of
  VInt n -> pure (T.pack (show n))
  VFloat f -> pure (formatXmlFloat f)
  VBool True -> pure "true"
  VBool False -> pure "false"
  VNull -> pure "null"
  VStr bytes _ -> pure (printString options bytes)
  VPath p -> pure p
  VList cl -> do
    key <- lift (printedList cell cl)
    entering repeatedMarker key (printList options depth cl)
  VAttrs attrs -> entering repeatedMarker (Just (printedSet attrs)) (printAttrs options depth attrs)
  -- What upstream prints for a lambda whose expression it cannot reach.
  VLambda {} -> pure (marker "lambda")
  VBuiltin builtin [] -> pure (marker ("primop " <> builtinName builtin))
  VBuiltin builtin _ -> pure (marker ("partially applied primop " <> builtinName builtin))

-- | Upstream's @printAttrs@.  A set's attributes count toward the bound
-- after the attributes of any set inside them, as upstream counts one
-- only once its value has printed.
printAttrs :: (MonadCData m) => PrintOptions -> Int -> AttrSet -> Printer m Text
printAttrs options depth attrs
  | within depth (depthLimit options) = do
      ordered <- lift (orderedAttrs options attrs)
      bracketed "{" "}" <$> printBounded (attrLimit options) shownAttrs countAttr elidedAttrs (map printAttr ordered)
  | otherwise = pure "{ ... }"
  where
    printAttr (name, thunk) = do
      shown <- printThunk options (depth + 1) thunk
      pure (printSymbol name <> " = " <> shown <> ";")
    countAttr counts = counts {shownAttrs = shownAttrs counts + 1}
    elidedAttrs left = elided left "attribute" "attributes"

-- | Upstream's @printList@.  The elements are read only inside the depth
-- bound, so a list past it prints as @[ ... ]@ without reading any.
printList :: (MonadCData m) => PrintOptions -> Int -> CList -> Printer m Text
printList options depth cl
  | within depth (depthLimit options) = do
      thunks <- lift (clistThunks cl)
      bracketed "[" "]" <$> printBounded (itemLimit options) shownItems countItem elidedItems (map (printThunk options (depth + 1) . Thunk) thunks)
  | otherwise = pure "[ ... ]"
  where
    countItem counts = counts {shownItems = shownItems counts + 1}
    elidedItems left = elided left "item" "items"

-- | Print the entries of one set or list while the print's running count
-- is inside the bound, then, in place of the rest, how many were left.
printBounded :: (Monad m) => Limit -> (Shown -> Int) -> (Shown -> Shown) -> (Int -> Text) -> [Printer m Text] -> Printer m [Text]
printBounded _ _ _ _ [] = pure []
printBounded bound counted count elision entries@(entry : rest) = do
  sofar <- gets counted
  if within sofar bound
    then do
      shown <- entry
      modify' count
      (shown :) <$> printBounded bound counted count elision rest
    else pure [elision (length entries)]

-- | A set's attributes in upstream's order: by name, with @_type@ and
-- @type@ ahead of the rest wherever attributes are bounded (print.cc sorts
-- by its @ImportantFirstAttrNameCmp@ then).
orderedAttrs :: (MonadCData m) => PrintOptions -> AttrSet -> m [(Text, Thunk)]
orderedAttrs options attrs = inOrder <$> attrSetToAscList attrs
  where
    inOrder byName = case attrLimit options of
      Unlimited -> byName
      AtMost _ -> uncurry (<>) (partition ((`elem` importantAttrNames) . fst) byName)

-- | Upstream's @isImportantAttrName@.
importantAttrNames :: [Text]
importantAttrNames = ["_type", "type"]

-- | An element or attribute value, read without forcing it: its value
-- once it has one, and otherwise upstream's marker for a thunk, or for a
-- thunk whose force is still running.
printThunk :: (MonadCData m) => PrintOptions -> Int -> Thunk -> Printer m Text
printThunk options depth thunk =
  lift (readThunkValue thunk) >>= \case
    Just val -> printAt options depth (Just thunk) val
    Nothing -> do
      underEvaluation <- lift (thunkUnderEvaluation thunk)
      pure (marker (if underEvaluation then "potential infinite recursion" else "thunk"))

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
-- Repeated sets and lists
-- ---------------------------------------------------------------------------

-- | A set or list a print has entered, by the identity upstream's printers
-- remember one by.  Each is a C pointer, reached from the value the print
-- was handed, which is older than every force the print runs, so no close
-- of one frees it ("Nix.Eval.CRegion"), and a pointer names one set, list
-- or cell for the whole print.
data Entered
  = -- | A set, by its C set, as upstream remembers a set by its
    -- @Bindings@: every empty set upstream shares is the one
    -- 'Nix.Eval.Types.emptyAttrSet' here, and an update with an empty
    -- operand is the other operand's set, as upstream's is.
    EnteredSet !CAttrSet
  | -- | A list, by the cell it was read from, as upstream remembers a list
    -- by its @Value@.
    EnteredCell !CThunkPtr
  | -- | A list, by its C list, which every cell holding the list shares.
    EnteredList !CListPtr
  deriving (Eq, Ord)

-- | Print a set or list the first time a print enters it, and the given
-- marker each time after.  One with no identity prints in full each time.
entering :: (Monad m) => a -> Maybe Entered -> Printer m a -> Printer m a
entering _ Nothing body = body
entering repeated (Just key) body = gets (Set.member key . shownEntered) >>= enterUnlessSeen
  where
    enterUnlessSeen True = pure repeated
    enterUnlessSeen False = modify' (\shown -> shown {shownEntered = Set.insert key (shownEntered shown)}) >> body

-- | Upstream's @printRepeated@.
repeatedMarker :: Text
repeatedMarker = marker "repeated"

-- | What 'printValue' enters a set by (print.cc:332 at 2.24.9): its
-- bindings, an empty set's included.
printedSet :: AttrSet -> Entered
printedSet = EnteredSet . unAttrSet

-- | What 'printValue' enters a list with elements by (print.cc:410): the
-- cell holding it.  The top-level value has no cell, so a list there is
-- entered by nothing.  Upstream's top-level @Value@ is the one its caller
-- hands over: a temporary for a coercion (eval.cc:2010-2014), which
-- nothing else holds either, and for @builtins.trace@ the argument itself
-- (primops.cc:1018), so a traced list that holds itself prints one level
-- deeper here before the marker.
printedList :: (MonadCData m) => Maybe Thunk -> CList -> m (Maybe Entered)
printedList cell cl = do
  len <- clistLen cl
  pure (if len == 0 then Nothing else EnteredCell . unThunk <$> cell)

-- | What 'printAmbiguous' enters a set with attributes by
-- (print-ambiguous.cc:39 at 2.24.9): its bindings.  An empty set is
-- entered by nothing and always prints in full.
resultSet :: (MonadCData m) => AttrSet -> m (Maybe Entered)
resultSet attrs = do
  empty <- attrSetNull attrs
  pure (if empty then Nothing else Just (EnteredSet (unAttrSet attrs)))

-- | What 'printAmbiguous' enters a list with elements by
-- (print-ambiguous.cc:53): where its elements are.  Upstream keeps a short
-- list's elements inside its @Value@ and a longer one's in an array every
-- copy of that @Value@ shares (value.hh:407-410), so a short list is
-- entered by the cell holding it and a longer one by its C list.  The
-- top-level value has no cell, and upstream's is nix-instantiate's own
-- copy (nix-instantiate.cc:50), which nothing else holds, so a short list
-- there is entered by nothing.
resultList :: (MonadCData m) => Maybe Thunk -> CList -> m (Maybe Entered)
resultList cell cl = enteredBy <$> clistLen cl
  where
    enteredBy len
      | len == 0 = Nothing
      | len <= inlineListElements = EnteredCell . unThunk <$> cell
      | otherwise = Just (EnteredList (unCList cl))

-- | The most elements upstream keeps inside a list's @Value@
-- (@smallList@, value.hh:250 at 2.24.9).
inlineListElements :: Int
inlineListElements = 2

-- ---------------------------------------------------------------------------
-- The eval result
-- ---------------------------------------------------------------------------

-- | Render a value as @nix-instantiate --eval@ prints its result, through
-- upstream's @printAmbiguous@ (print-ambiguous.cc at 2.24.9), without the
-- newline the command writes after it.  A string prints as its own bytes,
-- escaped as 'printValue' escapes it and never cut, and a float in a
-- default ostream's six-digit form, as 'formatXmlFloat' writes it.
printAmbiguous :: (MonadCData m) => NixValue -> m BB.Builder
printAmbiguous val = evalStateT (printAmbiguousAt Nothing val) nothingShown
-- The executable prints its result in IO, and a result can be the whole
-- of a large value: left to the library's flags, that print ran on the
-- dictionary-passing code.
{-# SPECIALIZE printAmbiguous :: NixValue -> IO BB.Builder #-}

-- | A value with the cell it was read from, as 'printAt' takes one.
printAmbiguousAt :: (MonadCData m) => Maybe Thunk -> NixValue -> Printer m BB.Builder
printAmbiguousAt cell val = case val of
  VInt n -> pure (BB.int64Dec n)
  VFloat f -> pure (TE.encodeUtf8Builder (formatXmlFloat f))
  VBool True -> pure "true"
  VBool False -> pure "false"
  VNull -> pure "null"
  VStr bytes _ -> pure (quotedBytes (BS.length bytes) bytes)
  VPath p -> pure (TE.encodeUtf8Builder p)
  VList cl -> do
    key <- lift (resultList cell cl)
    entering repeatedBytes key (lift (clistThunks cl) >>= fmap (bracketed "[" "]") . traverse (printElement . Thunk))
  VAttrs attrs -> do
    key <- lift (resultSet attrs)
    entering repeatedBytes key (lift (attrSetToAscList attrs) >>= fmap (bracketed "{" "}") . traverse printAttribute)
  VLambda {} -> pure "<LAMBDA>"
  VBuiltin _ [] -> pure "<PRIMOP>"
  VBuiltin _ _ -> pure "<PRIMOP-APP>"
  where
    repeatedBytes = TE.encodeUtf8Builder repeatedMarker
    printAttribute (name, thunk) = do
      shown <- printElement thunk
      pure (printIdentifier (TE.encodeUtf8 name) <> " = " <> shown <> ";")

-- | Upstream follows the opening bracket and every item with a space, so
-- an empty list is @[ ]@.  Both printers bracket this way.
bracketed :: (IsString s, Monoid s) => s -> s -> [s] -> s
bracketed open close items = open <> " " <> foldMap (<> " ") items <> close

-- | A list element or attribute value: its value when it has one, and
-- upstream's marker for a thunk when it was never forced.
printElement :: (MonadCData m) => Thunk -> Printer m BB.Builder
printElement thunk = lift (readThunkValue thunk) >>= maybe (pure "<CODE>") (printAmbiguousAt (Just thunk))

-- | A name as upstream writes a symbol into a message: its @operator<<@ on
-- a @SymbolStr@ writes it through @printIdentifier@ (nixexpr.cc:20-24 at
-- 2.24.9, 2.34.9 and 2.35.4), so a name that is not an identifier, the
-- empty name or a keyword prints as a string literal.
printSymbol :: Text -> Text
printSymbol = builderText . printIdentifier . TE.encodeUtf8

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
