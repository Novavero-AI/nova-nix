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
-- the @makeStorePath@ family that constructs content-addressed store paths
-- exactly as C++ Nix does.
module Nix.Hash
  ( -- * Derivation hashing
    DrvHash (..),
    hashDerivation,

    -- * Shared hashing utilities
    sha256Hex,
    truncatedBase32,
    hashPlaceholder,
    byteToHex,

    -- * Store path construction (Nix @makeStorePath@ family)
    makeStorePath,
    makeTextPath,
    makeFixedOutputPath,
    makeOutputPath,
    compressHash,
    sha256Digest,
    bytesToHexText,

    -- * Re-exports from nova-cache
    hashBytes,
    formatNixHash,
    parseNixHash,
  )
where

import qualified Crypto.Hash as CH
import Data.Bits (xor)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import Data.List (foldl')
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word8)
import Nix.Store.Path (StoreDir (..), StorePath (..), defaultStoreDir, storePathToText)
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

-- | Compress a SHA-256 digest to 20 bytes via XOR-folding and Nix base-32
-- encode.  Matches C++ Nix @compressHash@: bytes beyond the target length
-- are XOR'd back onto the prefix, preserving information from the full
-- digest rather than simply truncating.
truncatedBase32 :: BS.ByteString -> Text
truncatedBase32 bs =
  let digest = CH.hash bs :: CH.Digest CH.SHA256
      allBytes = BA.unpack digest :: [Word8]
      compressed = compressHash 20 allBytes
   in encode (BS.pack compressed)

-- | Nix's output placeholder for @builtins.placeholder@: @\/@ followed by the
-- full SHA-256 of @"nix-output:" <> name@ in Nix base-32 (no truncation, no
-- store-dir prefix).  Matches C++ Nix @hashPlaceholder@.
hashPlaceholder :: Text -> Text
hashPlaceholder name =
  "/" <> encode (sha256Digest (encodeUtf8 ("nix-output:" <> name)))

-- | XOR-fold a hash to @targetLen@ bytes, matching C++ Nix @compressHash@.
-- Each source byte is XOR'd into position @i mod targetLen@.
compressHash :: Int -> [Word8] -> [Word8]
compressHash targetLen bytes =
  let arr0 = replicate targetLen 0
      fold_ acc (i, b) =
        let pos = i `mod` targetLen
         in zipWith (\j x -> if j == pos then xor x b else x) [0 :: Int ..] acc
   in foldl' fold_ arr0 (zip [0 ..] bytes)

-- | Format a single byte as two lowercase hex digits.
byteToHex :: Word8 -> String
byteToHex w =
  let (hi, lo) = quotRem (fromIntegral w :: Int) 16
      hexDigit n
        | n < 10 = toEnum (fromEnum '0' + n)
        | otherwise = toEnum (fromEnum 'a' + n - 10)
   in [hexDigit hi, hexDigit lo]

-- ---------------------------------------------------------------------------
-- Store path construction — the Nix @makeStorePath@ family
--
-- These mirror C++ Nix exactly so nova-nix's computed store paths byte-match
-- @nix-instantiate@.  All hashing is done against the canonical @\/nix\/store@
-- directory ('defaultStoreDir') so paths are host-independent: the same
-- derivation hashes identically on Windows, Linux, and macOS.
-- ---------------------------------------------------------------------------

-- | Length in bytes a store-path hash is compressed to (160 bits → 32 base-32
-- characters).
storePathHashBytes :: Int
storePathHashBytes = 20

-- | Raw 32-byte SHA-256 digest of a ByteString (not hex-encoded).
sha256Digest :: BS.ByteString -> BS.ByteString
sha256Digest bs = BA.convert (CH.hash bs :: CH.Digest CH.SHA256)

-- | Lowercase base-16 of raw bytes (two hex characters per byte).
bytesToHexText :: BS.ByteString -> Text
bytesToHexText = T.pack . concatMap byteToHex . BS.unpack

-- | The core store-path construction primitive.  Given a @type@ string, the
-- inner content digest (raw SHA-256 bytes), and a name, produce the store
-- path.  Mirrors C++ Nix @makeStorePath@:
--
-- @
-- s = type ":sha256:" hex(innerDigest) ":" storeDir ":" name
-- hash = compressHash(sha256(s), 20)
-- path = storeDir "/" base32(hash) "-" name
-- @
--
-- The @type@ string varies by caller: @\"text\"@ (+ references) for @.drv@
-- and @toFile@ paths, @\"output:<id>\"@ for derivation outputs, @\"source\"@
-- for recursive fixed-output paths.
makeStorePath :: StoreDir -> Text -> BS.ByteString -> Text -> StorePath
makeStorePath (StoreDir dir) typ innerDigest name =
  let preimage =
        typ
          <> ":sha256:"
          <> bytesToHexText innerDigest
          <> ":"
          <> T.pack dir
          <> ":"
          <> name
      compressed = compressHash storePathHashBytes (BS.unpack (sha256Digest (encodeUtf8 preimage)))
   in StorePath (encode (BS.pack compressed)) name

-- | Construct a text store path (used for @.drv@ files and @builtins.toFile@).
-- The references are embedded in the @type@ string — @\"text\"@ followed by
-- each referenced store path — which is why a derivation's @.drv@ path depends
-- on the paths of all its inputs.  @contentsDigest@ is the SHA-256 of the file
-- contents (the ATerm, for a @.drv@).
makeTextPath :: Text -> BS.ByteString -> [StorePath] -> StorePath
makeTextPath name contentsDigest refs =
  let sortedRefs = Set.toAscList (Set.fromList refs)
      typ = "text" <> T.concat [":" <> storePathToText defaultStoreDir r | r <- sortedRefs]
   in makeStorePath defaultStoreDir typ contentsDigest name

-- | Construct a fixed-output store path.  @foHashDigest@ is the raw bytes of
-- the EXPECTED output hash (e.g. a tarball's SHA-256).  @mode@ is @\"flat\"@
-- or @\"recursive\"@.  Mirrors C++ Nix @makeFixedOutputPath@:
--
-- * @sha256@ + @recursive@ → @makeStorePath \"source\" foHash name@
-- * otherwise → @makeStorePath \"output:out\" sha256(\"fixed:out:\" prefix algo \":\" hex \":\") name@
makeFixedOutputPath :: Text -> Text -> Text -> BS.ByteString -> StorePath
makeFixedOutputPath name algo mode foHashDigest
  | algo == "sha256" && mode == "recursive" =
      makeStorePath defaultStoreDir "source" foHashDigest name
  | otherwise =
      let prefix = if mode == "recursive" then "r:" else ""
          inner = "fixed:out:" <> prefix <> algo <> ":" <> bytesToHexText foHashDigest <> ":"
          innerDigest = sha256Digest (encodeUtf8 inner)
       in makeStorePath defaultStoreDir "output:out" innerDigest name

-- | Construct an input-addressed output store path.  @moduloDigest@ is the raw
-- bytes of @hashDerivationModulo@ (masked).  Mirrors C++ Nix @makeOutputPath@:
-- the path name gets an @-<output>@ suffix for non-@out@ outputs.
makeOutputPath :: Text -> BS.ByteString -> Text -> StorePath
makeOutputPath outName moduloDigest drvName =
  let pathName = if outName == "out" then drvName else drvName <> "-" <> outName
   in makeStorePath defaultStoreDir ("output:" <> outName) moduloDigest pathName
