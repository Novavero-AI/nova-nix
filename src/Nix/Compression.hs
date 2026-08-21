-- | The narinfo compression register - one vocabulary for every
-- consumer (the substituter's dispatch, and push once it writes
-- compressed objects), so the accepted set exists exactly once and
-- cannot drift between encodings.
--
-- Upstream note: C++ Nix decodes an absent or empty @Compression@
-- field as @bzip2@ (the field's historical default).  nova-cache's
-- parser defaults only the ABSENT key; a present-but-empty
-- @Compression:@ line parses to the empty string and arrives here
-- verbatim, so this register handles that spelling itself - the
-- 'T.null' branch is reached by ordinary narinfos off the wire and is
-- not dead code.  Both spellings decode to 'CompressionBzip2', so
-- such a narinfo substitutes exactly as upstream would rather than
-- failing over a field that looks like nothing at all.
module Nix.Compression
  ( NarCompression (..),
    parseNarCompression,
    compressionNameNone,
    compressionNameXz,
    compressionNameZstd,
    compressionNameBzip2,
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
  | CompressionZstd
  | CompressionBzip2
  deriving (Eq, Show)

-- | The wire spelling of the identity codec.
compressionNameNone :: Text
compressionNameNone = "none"

-- | The wire spelling of the xz codec (cache.nixos.org's format).
compressionNameXz :: Text
compressionNameXz = "xz"

-- | The wire spelling of the zstd codec (the modern caches' format,
-- and the compression @nova-nix push@ can produce).
compressionNameZstd :: Text
compressionNameZstd = "zstd"

-- | The wire spelling of the bzip2 codec (the historical caches'
-- format, and what upstream decodes an absent field as).
compressionNameBzip2 :: Text
compressionNameBzip2 = "bzip2"

-- | Parse a narinfo @Compression@ value.  Unsupported codecs reject by
-- name; the empty string decodes as the codec upstream defaults it to
-- (see the module header).
parseNarCompression :: Text -> Either Text NarCompression
parseNarCompression name
  | name == compressionNameNone = Right CompressionNone
  | name == compressionNameXz = Right CompressionXz
  | name == compressionNameZstd = Right CompressionZstd
  | name == compressionNameBzip2 || T.null name = Right CompressionBzip2
  | otherwise = Left ("unsupported compression: " <> name)
