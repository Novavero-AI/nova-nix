-- | Cryptographic hashing for the Nix store.
--
-- == How Nix uses hashes
--
-- Everything in Nix is content-addressed. A store path like:
--
-- @\/nix\/store\/s66mzxpvicwk07gjbjfw9izjfa797vsw-hello-2.12.1@
--
-- That @s66mzx...@ hash encodes ALL inputs that went into building the
-- package: source code, compiler version, flags, dependencies (which are
-- themselves hashes).  Change any input → different hash → different path →
-- completely isolated from the original.
--
-- This is why Nix can have multiple versions of the same package installed
-- simultaneously without conflict. They live at different store paths
-- because their input hashes differ.
--
-- == Hash types in Nix
--
-- * __Input hash__ (derivation hash): SHA-256 of all build inputs.
--   Computed BEFORE building.  This is the hash in the store path.
-- * __Output hash__ (NAR hash): SHA-256 of the built output serialized
--   as a NAR archive.  Computed AFTER building.  Stored in the narinfo
--   for integrity verification.
-- * __File hash__: SHA-256 of the compressed @.nar.xz@ file.  For
--   network transfer integrity.
--
-- nova-cache already handles output hashes and file hashes.  This module
-- adds input hash (derivation hash) computation for the evaluator, plus
-- shared hashing utilities used by the evaluator and IO layer.
module Nix.Hash
  ( -- * Derivation hashing
    DrvHash (..),
    hashDerivation,

    -- * Shared hashing utilities
    sha256Hex,
    truncatedBase32,
    byteToHex,

    -- * Re-exports from nova-cache
    hashBytes,
    formatNixHash,
    parseNixHash,
  )
where

import qualified Crypto.Hash as CH
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word8)
import NovaCache.Base32 (encode)
import NovaCache.Hash (formatNixHash, hashBytes, parseNixHash)

-- | A derivation hash — the input hash that determines the store path.
-- This is computed from the derivation's inputs, NOT from the build output.
-- Stored as the Nix-formatted hash string (e.g. "sha256:0abc...").
newtype DrvHash = DrvHash {unDrvHash :: Text}
  deriving (Eq, Show)

-- | Hash a serialized derivation (.drv file contents) to produce the
-- input hash used in store path computation.
--
-- The derivation is first converted to ATerm format, then SHA-256 hashed,
-- then truncated to 160 bits (20 bytes) and encoded in Nix base-32.
-- This produces the 32-character hash in the store path.
hashDerivation :: Text -> DrvHash
hashDerivation drvText =
  let drvBytes = encodeUtf8 drvText
      nixHash = hashBytes drvBytes
   in DrvHash (formatNixHash nixHash)

-- | SHA-256 hex digest of a ByteString.
sha256Hex :: BS.ByteString -> Text
sha256Hex bs =
  let digest = CH.hash bs :: CH.Digest CH.SHA256
      bytes = BA.unpack digest
   in T.pack (concatMap byteToHex bytes)

-- | Truncate a SHA-256 digest to 20 bytes and Nix base-32 encode.
truncatedBase32 :: BS.ByteString -> Text
truncatedBase32 bs =
  let digest = CH.hash bs :: CH.Digest CH.SHA256
      bytes20 = BS.pack (take 20 (BA.unpack digest :: [Word8]))
   in encode bytes20

-- | Format a single byte as two lowercase hex digits.
byteToHex :: Word8 -> String
byteToHex w =
  let (hi, lo) = quotRem (fromIntegral w :: Int) 16
      hexDigit n
        | n < 10 = toEnum (fromEnum '0' + n)
        | otherwise = toEnum (fromEnum 'a' + n - 10)
   in [hexDigit hi, hexDigit lo]
