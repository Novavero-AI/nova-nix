-- | The narinfo compression register - one vocabulary for every
-- consumer (the substituter's dispatch, and push once it writes
-- compressed objects), so the accepted set exists exactly once and
-- cannot drift between encodings.
--
-- Upstream note: C++ Nix decodes an absent or empty @Compression@
-- field as @bzip2@ (the field's historical default), and nova-cache's
-- parser matches that coercion, so the empty string never reaches this
-- register from a parsed narinfo.  It is still mapped to the name
-- upstream would decode, so a hand-built narinfo rejects with the true
-- diagnosis instead of an empty-looking message.
module Nix.Compression
  ( NarCompression (..),
    parseNarCompression,
    compressionNameNone,
    compressionNameXz,
    defaultedCompressionName,
  )
where

import Data.Text (Text)
import qualified Data.Text as T

-- | A compression codec the substituter can decode.  Parsing into this
-- sum happens once at the narinfo boundary; every dispatch downstream
-- is an exhaustive match, so a new codec is added by extending the type
-- and following the compiler.
data NarCompression
  = CompressionNone
  | CompressionXz
  deriving (Eq, Show)

-- | The wire spelling of the identity codec.
compressionNameNone :: Text
compressionNameNone = "none"

-- | The wire spelling of the xz codec (cache.nixos.org's format).
compressionNameXz :: Text
compressionNameXz = "xz"

-- | What upstream decodes when a narinfo omits the @Compression@ field.
defaultedCompressionName :: Text
defaultedCompressionName = "bzip2"

-- | Parse a narinfo @Compression@ value.  Unsupported codecs reject by
-- name; the empty string rejects as the codec upstream would default
-- it to (see the module header).
parseNarCompression :: Text -> Either Text NarCompression
parseNarCompression name
  | name == compressionNameNone = Right CompressionNone
  | name == compressionNameXz = Right CompressionXz
  | T.null name = Left ("unsupported compression: " <> defaultedCompressionName)
  | otherwise = Left ("unsupported compression: " <> name)
