-- | A fetcher input's attributes, as upstream's fetchers hold them
-- (attrs.cc at 2.36.0): each a string, an unsigned integer or a Boolean,
-- by name in name order, and read back by accessors that refuse a missing
-- or mistyped attribute in upstream's words.
module Nix.Fetch.Attrs
  ( -- * Attributes
    AttrValue (..),
    Attrs,

    -- * Reading
    maybeStrAttr,
    strAttr,
    maybeIntAttr,
    maybeBoolAttr,

    -- * Rendering
    attrsJson,
    attrsJsonText,
    attrsToQuery,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word64)
import Nix.Fetch.Url (Checked, bytesToTextLossy, refuse)
import Nix.Json (Json (..), renderJson)

-- | One attribute's value.  A string is bytes, as a Nix string is.
data AttrValue
  = AttrString !ByteString
  | AttrInt !Word64
  | AttrBool !Bool
  deriving (Eq, Ord, Show)

-- | An input's attributes by name.
type Attrs = Map Text AttrValue

-- | A string attribute, if there is one.
maybeStrAttr :: Text -> Attrs -> Checked (Maybe ByteString)
maybeStrAttr name attrs = case Map.lookup name attrs of
  Nothing -> pure Nothing
  Just (AttrString value) -> pure (Just value)
  Just _ -> refuse ("input attribute '" <> name <> "' is not a string " <> attrsJsonText attrs)

-- | A string attribute the input must have.
strAttr :: Text -> Attrs -> Checked ByteString
strAttr name attrs = maybeStrAttr name attrs >>= maybe (refuse ("input attribute '" <> name <> "' is missing")) pure

-- | An integer attribute, if there is one.
maybeIntAttr :: Text -> Attrs -> Checked (Maybe Word64)
maybeIntAttr name attrs = case Map.lookup name attrs of
  Nothing -> pure Nothing
  Just (AttrInt value) -> pure (Just value)
  Just _ -> refuse ("input attribute '" <> name <> "' is not an integer")

-- | A Boolean attribute, if there is one.
maybeBoolAttr :: Text -> Attrs -> Checked (Maybe Bool)
maybeBoolAttr name attrs = case Map.lookup name attrs of
  Nothing -> pure Nothing
  Just (AttrBool value) -> pure (Just value)
  Just _ -> refuse ("input attribute '" <> name <> "' is not a Boolean")

-- | The attributes as a JSON object, as upstream's @attrsToJSON@ makes
-- it.  A string with no UTF-8 reading, which nlohmann refuses to write,
-- has its bytes read leniently: the object is only ever shown in a
-- message.
attrsJson :: Attrs -> Json
attrsJson = JsonObject . Map.map valueJson
  where
    valueJson value = case value of
      AttrString s -> JsonString (bytesToTextLossy s)
      AttrInt n -> JsonInt (toInteger n)
      AttrBool b -> JsonBool b

-- | 'attrsJson' as nlohmann's @dump@ writes it.
attrsJsonText :: Attrs -> Text
attrsJsonText = bytesToTextLossy . renderJson . attrsJson

-- | The attributes as a URL query, as upstream's @attrsToQuery@ makes it:
-- an integer in decimal and a Boolean as @1@ or @0@.
attrsToQuery :: Attrs -> Map ByteString ByteString
attrsToQuery = Map.fromList . map query . Map.toList
  where
    query (name, value) = (TE.encodeUtf8 name, rendered value)
    rendered value = case value of
      AttrString s -> s
      AttrInt n -> BC.pack (show n)
      AttrBool b -> if b then "1" else "0"
