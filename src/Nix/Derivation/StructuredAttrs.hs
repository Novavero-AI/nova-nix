-- | Structured attributes: a derivation that passes its attributes to the
-- builder as one JSON object rather than one environment variable each.
--
-- == The two halves
--
-- Evaluation (@__structuredAttrs = true@) renders the attributes into a
-- single object and stores it in the derivation's environment under
-- 'structuredAttrsKey', so attributes may be nested sets and lists where
-- the environment would need strings.  The @.drv@ hashes that entry, so
-- its bytes are upstream's ('Nix.Json.renderJson').
--
-- Building reads the entry back, replaces @outputs@ with an object naming
-- each output's path, and hands the builder two files instead of the
-- attributes themselves: @.attrs.json@, the object as JSON, and
-- @.attrs.sh@, the part of it bash can hold as @declare@ statements
-- ('renderAttrsShell').  A derivation is built this way when its
-- environment holds the entry, the test upstream's @ParsedDerivation@
-- makes (@src\/libstore\/parsed-derivations.cc@ at 2.24.9).
module Nix.Derivation.StructuredAttrs
  ( -- * The environment entry
    structuredAttrsKey,
    StructuredAttrs (..),
    encodeStructuredAttrs,
    decodeStructuredAttrs,

    -- * What the builder reads
    withOutputPlaceholders,
    renderAttrsJson,
    renderAttrsShell,
    stringsAttr,
  )
where

import Data.ByteString (ByteString)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Int (Int32)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Float (double2Float)
import Nix.Hash (hashPlaceholder)
import Nix.Json (Json (..), parseJson, renderJson, renderJsonWith)

-- | The environment entry holding a structured-attributes derivation's
-- attributes.
structuredAttrsKey :: Text
structuredAttrsKey = "__json"

-- | The member upstream fills with the output paths before the builder
-- runs, replacing whatever the attributes held there (the list of output
-- names, when the derivation declared one).
outputsMember :: Text
outputsMember = "outputs"

-- | A derivation's attributes, by name.
newtype StructuredAttrs = StructuredAttrs (Map Text Json)
  deriving (Eq, Show)

-- | The bytes of the 'structuredAttrsKey' entry.
encodeStructuredAttrs :: StructuredAttrs -> ByteString
encodeStructuredAttrs (StructuredAttrs members) = renderJson (JsonObject members)

-- | Read the 'structuredAttrsKey' entry back.  It must hold an object, or
-- @null@, which nlohmann turns into an empty object the moment upstream
-- assigns @outputs@ into it; any other value has no member to assign.
decodeStructuredAttrs :: Text -> Either Text StructuredAttrs
decodeStructuredAttrs encoded = do
  value <- parseJson encoded
  case value of
    JsonObject members -> Right (StructuredAttrs members)
    JsonNull -> Right (StructuredAttrs Map.empty)
    _ -> Left "the attributes are not a JSON object"

-- | Set @outputs@ to an object naming each output's placeholder, as
-- upstream's @prepareStructuredAttrs@ does; the build rewrites each
-- placeholder to the path the output is written to, along with every
-- other placeholder the attributes hold.
withOutputPlaceholders :: [Text] -> StructuredAttrs -> StructuredAttrs
withOutputPlaceholders outputNames (StructuredAttrs members) =
  StructuredAttrs (Map.insert outputsMember outputs members)
  where
    outputs = JsonObject (Map.fromList [(name, JsonString (hashPlaceholder name)) | name <- outputNames])

-- | The @.attrs.json@ a builder reads: the attributes as JSON, every
-- string passed through the build's rewrite (see 'Nix.Json.renderJsonWith'
-- for why before escaping rather than after rendering).
renderAttrsJson :: (Text -> Text) -> StructuredAttrs -> ByteString
renderAttrsJson rewrite (StructuredAttrs members) = renderJsonWith rewrite (JsonObject members)

-- | Upstream's @writeStructuredAttrsShell@: one @declare@ per attribute
-- whose name is a shell variable name and whose value bash can hold.  A
-- scalar becomes a variable, an array of scalars an indexed array, an
-- object of scalars an associative array; anything else, an array or
-- object holding a nested one or a non-integral number included, is
-- left out.  Arrays and objects keep the space upstream writes after
-- each element.
renderAttrsShell :: StructuredAttrs -> Text
renderAttrsShell (StructuredAttrs members) = T.concat (mapMaybe declaration (Map.toAscList members))
  where
    declaration (name, value)
      | not (isShellVarName name) = Nothing
      | Just scalar <- shellScalar value = Just ("declare " <> name <> "=" <> scalar <> "\n")
      | JsonArray items <- value,
        Just scalars <- traverse shellScalar items =
          Just ("declare -a " <> name <> "=(" <> T.concat (map (<> " ") scalars) <> ")\n")
      | JsonObject entries <- value,
        Just pairs <- traverse shellEntry (Map.toAscList entries) =
          Just ("declare -A " <> name <> "=(" <> T.concat pairs <> ")\n")
      | otherwise = Nothing
    shellEntry (key, value) = (\scalar -> "[" <> shellQuote key <> "]=" <> scalar <> " ") <$> shellScalar value

-- | upstream's @std::regex@ @[A-Za-z_][A-Za-z0-9_]*@, matched against the
-- whole name.
isShellVarName :: Text -> Bool
isShellVarName name = case T.uncons name of
  Just (first, rest) -> (isAsciiLetter first || first == '_') && T.all (\c -> isAsciiLetter c || isDigit c || c == '_') rest
  Nothing -> False
  where
    isAsciiLetter c = isAsciiUpper c || isAsciiLower c

-- | upstream's @handleSimpleType@: a string single-quoted, an integral
-- number in decimal, @null@ as an empty quoted string, a boolean as @1@ or
-- nothing.
--
-- Upstream tests a number for integrality as a C @float@ and prints it
-- after @static_cast<int>@, so an integer keeps its low 32 bits and a float
-- that is integral at single precision truncates toward zero.  A float
-- past @int@'s range is undefined behaviour in C++; the value here is what
-- an aarch64 build of upstream writes for it (saturation, observed with
-- Nix 2.33.2 on aarch64-darwin), where x86-64 writes @INT_MIN@ for every
-- such value.
shellScalar :: Json -> Maybe Text
shellScalar (JsonString s) = Just (shellQuote s)
shellScalar (JsonInt n) = Just (T.pack (show (fromInteger n :: Int32)))
shellScalar (JsonFloat d)
  | isIntegralAsFloat (double2Float d) = Just (T.pack (show (truncateToInt32 d)))
  | otherwise = Nothing
shellScalar JsonNull = Just "''"
shellScalar (JsonBool True) = Just "1"
shellScalar (JsonBool False) = Just ""
shellScalar (JsonArray _) = Nothing
shellScalar (JsonObject _) = Nothing

-- | C's @ceil(f) == f@: true of an infinity, false of NaN.
isIntegralAsFloat :: Float -> Bool
isIntegralAsFloat f
  | isNaN f = False
  | isInfinite f = True
  | otherwise = fromInteger (ceiling f) == f

-- | @static_cast<int>@ of a double as aarch64 performs it: toward zero,
-- saturating at the ends of the range, NaN to zero.
truncateToInt32 :: Double -> Int32
truncateToInt32 d
  | isNaN d = 0
  | d >= fromIntegral (maxBound :: Int32) = maxBound
  | d <= fromIntegral (minBound :: Int32) = minBound
  | otherwise = fromInteger (truncate d)

-- | Upstream's @shellEscape@: single quotes around the whole, each single
-- quote inside closed, escaped and reopened.
shellQuote :: Text -> Text
shellQuote s = "'" <> T.replace "'" "'\\''" s <> "'"

-- | A list-of-strings attribute (upstream's @getStringsAttr@ in structured
-- mode): absent is 'Nothing', and anything but a list of strings is an
-- error rather than a guess.
stringsAttr :: Text -> StructuredAttrs -> Either Text (Maybe [Text])
stringsAttr name (StructuredAttrs members) = case Map.lookup name members of
  Nothing -> Right Nothing
  Just (JsonArray items) | Just strings <- traverse asString items -> Right (Just strings)
  Just _ -> Left ("attribute '" <> name <> "' must be a list of strings")
  where
    asString (JsonString s) = Just s
    asString _ = Nothing
