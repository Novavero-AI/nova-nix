{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- | Cryptographic hashing for the Nix store.
--
-- == How Nix uses hashes
--
-- Every store path is named by a hash. A store path like:
--
-- @\/nix\/store\/s66mzxpvicwk07gjbjfw9izjfa797vsw-hello-2.12.1@
--
-- That @s66mzx...@ hash encodes ALL inputs that went into building the
-- package: source code, compiler version, flags, dependencies (which are
-- themselves hashes).  Change any input and you get a different hash, a
-- different path, completely isolated from the original.
--
-- This is why Nix can have multiple versions of the same package installed
-- simultaneously without conflict. They live at different store paths
-- because their input hashes differ.
--
-- == Hash types in Nix
--
-- * __Input hash__ (derivation hash): SHA-256 of all build inputs.
--   Computed BEFORE building.  This is the hash in an input-addressed
--   output's store path; a fixed output's path hashes its declared
--   content instead ('makeFixedOutputPath').
-- * __Output hash__ (NAR hash): SHA-256 of the built output serialized
--   as a NAR archive.  Computed AFTER building.  Stored in the narinfo
--   for integrity verification.
-- * __File hash__: SHA-256 of the compressed @.nar.xz@ file.  For
--   network transfer integrity.
--
-- nova-cache already handles output hashes and file hashes.  This module
-- provides the shared hashing utilities, the @makeStorePath@ family that
-- constructs store paths exactly as C++ Nix does, and
-- the reading of a hash from each spelling upstream accepts.
module Nix.Hash
  ( -- * Shared hashing utilities
    sha256Hex,
    truncatedBase32,
    hashPlaceholder,
    byteToHex,
    hexToBytes,
    base64ToBytes,
    rawHashWithAlgo,
    IncrementalHash,
    hashInitWithAlgo,
    hashUpdateChunk,
    hashFinalizeBytes,
    hashAlgoBytes,
    hexHashLen,
    nix32HashLen,
    base64HashLen,

    -- * Hash spellings
    unknownHashAlgo,
    parseHashAlgo,
    parseAnyHash,
    digestByLength,
    sriDigest,

    -- * Store path construction (Nix @makeStorePath@ family)
    makeStorePath,
    makeTextPath,
    FileIngestionMethod (..),
    fileIngestionPrefix,
    makeFixedOutputPath,
    makeSourcePath,
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
import Data.Bits (shiftL, shiftR, xor, (.|.))
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import Data.Char (digitToInt, isAsciiLower, isAsciiUpper, isDigit, isHexDigit, ord)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word8)
import Nix.Store.Path (BadStorePath (BadStorePath), StoreDir (..), StorePath, checkStorePathName, defaultStoreDir, storePathToText)
import Nix.Store.Path.Internal (StorePath (StorePath))
import qualified NovaCache.Base32 as Nix32
import NovaCache.Hash (formatNixHash, hashBytes, parseNixHash)

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
   in Nix32.encode (compressHash 20 (BA.convert digest))

-- | Nix's output placeholder for @builtins.placeholder@: @\/@ followed by the
-- full SHA-256 of @"nix-output:" <> name@ in Nix base-32 (no truncation, no
-- store-dir prefix).  Matches C++ Nix @hashPlaceholder@.
hashPlaceholder :: Text -> Text
hashPlaceholder name =
  "/" <> Nix32.encode (sha256Digest (encodeUtf8 ("nix-output:" <> name)))

-- | XOR-fold a hash to @targetLen@ bytes, matching C++ Nix @compressHash@
-- (hash.cc): byte @i@ is XOR'd into position @i mod targetLen@, which is
-- the hash's @targetLen@-byte chunks XOR'd together, a short last chunk
-- into the prefix.  Empty for a length below one, which leaves no
-- position to fold into.
compressHash :: Int -> BS.ByteString -> BS.ByteString
compressHash targetLen digest
  | targetLen < 1 = BS.empty
  | otherwise = foldl' xorChunk (BS.replicate targetLen 0) (chunks digest)
  where
    chunks bytes
      | BS.null bytes = []
      | otherwise = let (chunk, rest) = BS.splitAt targetLen bytes in chunk : chunks rest
    xorChunk acc chunk = BS.packZipWith xor acc chunk <> BS.drop (BS.length chunk) acc

-- | Format a single byte as two lowercase hex digits.
byteToHex :: Word8 -> String
byteToHex w =
  let (hi, lo) = quotRem (fromIntegral w :: Int) 16
      hexDigit n
        | n < 10 = toEnum (fromEnum '0' + n)
        | otherwise = toEnum (fromEnum 'a' + n - 10)
   in [hexDigit hi, hexDigit lo]

-- ---------------------------------------------------------------------------
-- Store path construction - the Nix @makeStorePath@ family
--
-- These mirror C++ Nix exactly so nova-nix's computed store paths byte-match
-- @nix-instantiate@.  All hashing is done against the canonical @\/nix\/store@
-- directory ('defaultStoreDir') so paths are host-independent: the same
-- derivation hashes identically on Windows, Linux, and macOS.
-- ---------------------------------------------------------------------------

-- | Length in bytes a store-path hash is compressed to (160 bits down to 32 base-32
-- characters).
storePathHashBytes :: Int
storePathHashBytes = 20

-- | Raw 32-byte SHA-256 digest of a ByteString (not hex-encoded).
sha256Digest :: BS.ByteString -> BS.ByteString
sha256Digest bs = BA.convert (CH.hash bs :: CH.Digest CH.SHA256)

-- | Lowercase base-16 of raw bytes (two hex characters per byte).
bytesToHexText :: BS.ByteString -> Text
bytesToHexText = T.pack . concatMap byteToHex . BS.unpack

-- | Decode a lowercase/uppercase base-16 string to raw bytes.  Returns
-- 'Nothing' for odd length or non-hex characters.  This is the canonical
-- form a @.drv@ stores a fixed-output hash in, so the builder reads it back
-- with this rather than re-parsing the user's original SRI/base32 string.
hexToBytes :: Text -> Maybe BS.ByteString
hexToBytes t
  | T.null t = Just BS.empty
  | odd (T.length t) = Nothing
  | T.all isHexDigit t = Just (BS.pack (pairs (T.unpack t)))
  | otherwise = Nothing
  where
    pairs [] = []
    pairs (hi : lo : rest) = fromIntegral (digitToInt hi * 16 + digitToInt lo) : pairs rest
    pairs [_] = [] -- unreachable: even length checked above

-- | Decode base64 as upstream's @base64Decode@ does (util.cc at 2.24.9,
-- @base64::decode@ in base-n.cc at 2.36.0).  Reading stops at the first @=@,
-- so nothing after it is read; a newline is skipped; any other character
-- outside the standard alphabet, a carriage return among them, refuses the
-- input; and the bits left after the last whole byte are dropped, whatever
-- their value.
base64ToBytes :: Text -> Maybe BS.ByteString
base64ToBytes encoded =
  BS.pack . sextetsToBytes <$> traverse sextet (T.unpack (T.filter (/= '\n') (T.takeWhile (/= '=') encoded)))
  where
    sextet :: Char -> Maybe Word8
    sextet c
      | isAsciiUpper c = Just (fromIntegral (ord c - ord 'A'))
      | isAsciiLower c = Just (fromIntegral (ord c - ord 'a' + 26))
      | isDigit c = Just (fromIntegral (ord c - ord '0' + 52))
      | c == '+' = Just 62
      | c == '/' = Just 63
      | otherwise = Nothing
    -- Four sextets make three bytes, a final three make two and a final
    -- two make one; a final one is six bits, short of a byte.  'shiftL'
    -- on a 'Word8' drops the bits that belong to the previous byte.
    sextetsToBytes (a : b : c : d : rest) = joinFirst a b : joinSecond b c : joinThird c d : sextetsToBytes rest
    sextetsToBytes [a, b, c] = [joinFirst a b, joinSecond b c]
    sextetsToBytes [a, b] = [joinFirst a b]
    sextetsToBytes [_] = []
    sextetsToBytes [] = []
    joinFirst a b = shiftL a 2 .|. shiftR b 4
    joinSecond b c = shiftL b 4 .|. shiftR c 2
    joinThird c d = shiftL c 6 .|. d

-- | Raw digest of a 'BS.ByteString' under a named algorithm (@sha256@,
-- @sha512@, @sha1@, @md5@).  Returns 'Nothing' for an unknown algorithm.
-- Used to verify a fixed-output fetch against its declared @outputHashAlgo@.
rawHashWithAlgo :: Text -> BS.ByteString -> Maybe BS.ByteString
rawHashWithAlgo algo bytes = case algo of
  "sha256" -> Just (BA.convert (CH.hash bytes :: CH.Digest CH.SHA256))
  "sha512" -> Just (BA.convert (CH.hash bytes :: CH.Digest CH.SHA512))
  "sha1" -> Just (BA.convert (CH.hash bytes :: CH.Digest CH.SHA1))
  "md5" -> Just (BA.convert (CH.hash bytes :: CH.Digest CH.MD5))
  _ -> Nothing

-- | An in-progress digest under a named algorithm - the incremental
-- form of 'rawHashWithAlgo', for hashing a stream chunk by chunk
-- without materializing the whole input.
data IncrementalHash = forall a. (CH.HashAlgorithm a) => IncrementalHash !(CH.Context a)

-- | Start an incremental digest.  'Nothing' for an unknown algorithm
-- name (same names 'rawHashWithAlgo' accepts).
hashInitWithAlgo :: Text -> Maybe IncrementalHash
hashInitWithAlgo algo = case algo of
  "sha256" -> Just (IncrementalHash (CH.hashInit :: CH.Context CH.SHA256))
  "sha512" -> Just (IncrementalHash (CH.hashInit :: CH.Context CH.SHA512))
  "sha1" -> Just (IncrementalHash (CH.hashInit :: CH.Context CH.SHA1))
  "md5" -> Just (IncrementalHash (CH.hashInit :: CH.Context CH.MD5))
  _ -> Nothing

-- | Absorb one chunk.
hashUpdateChunk :: IncrementalHash -> BS.ByteString -> IncrementalHash
hashUpdateChunk (IncrementalHash ctx) chunk = IncrementalHash (CH.hashUpdate ctx chunk)

-- | Finish, yielding the same raw digest bytes 'rawHashWithAlgo'
-- produces for the concatenated chunks.
hashFinalizeBytes :: IncrementalHash -> BS.ByteString
hashFinalizeBytes (IncrementalHash ctx) = BA.convert (CH.hashFinalize ctx)

-- | Raw digest size in bytes of a supported hash algorithm.  'Nothing' for
-- an unknown algorithm name.
hashAlgoBytes :: Text -> Maybe Int
hashAlgoBytes algo = case algo of
  "md5" -> Just 16
  "sha1" -> Just 20
  "sha256" -> Just 32
  "sha512" -> Just 64
  _ -> Nothing

-- The three spelling lengths of an @n@-byte digest, mirroring upstream
-- (@hash.cc@ @base16Len@\/@base32Len@\/@base64Len@).  For every supported
-- digest size the three lengths are pairwise distinct, which is what makes
-- length-keyed hash-format detection sound.

-- | Character length of an @n@-byte digest spelled in base-16.
hexHashLen :: Int -> Int
hexHashLen n = 2 * n

-- | Character length of an @n@-byte digest spelled in nix-base32.
nix32HashLen :: Int -> Int
nix32HashLen n = (8 * n - 1) `div` 5 + 1

-- | Character length of an @n@-byte digest spelled in padded base64.
base64HashLen :: Int -> Int
base64HashLen n = 4 * ((n + 2) `div` 3)

-- ---------------------------------------------------------------------------
-- Hash spellings
--
-- A hash read from text, in each spelling upstream accepts.
-- ---------------------------------------------------------------------------

-- | Upstream's refusal of an algorithm name it does not know
-- (@parseHashAlgo@ in hash.cc at 2.24.9).
unknownHashAlgo :: Text -> Text
unknownHashAlgo algo = "unknown hash algorithm '" <> algo <> "', expect 'md5', 'sha1', 'sha256', or 'sha512'"

-- | The digest size in bytes of an algorithm upstream knows, or upstream's
-- refusal of the name (@parseHashAlgo@ in hash.cc at 2.24.9).
parseHashAlgo :: Text -> Either Text Int
parseHashAlgo algo = maybe (Left (unknownHashAlgo algo)) Right (hashAlgoBytes algo)

-- | Parse a hash as upstream's @Hash::parseAny@ does (hash.cc at 2.24.9).
-- The algorithm is the one the hash's own tag names, else @expected@; when
-- both are given they must be the same.  @expected@ is a name 'parseHashAlgo'
-- accepts.
parseAnyHash :: Maybe Text -> Text -> Either Text (Text, BS.ByteString)
parseAnyHash expected original =
  hashSpelling original >>= \case
    SriHash algo digest -> agreeing algo (sriDigest algo digest)
    PrefixedHash algo digest -> agreeing algo (digestByLength algo digest)
    UntaggedHash -> case expected of
      Just algo -> (algo,) <$> digestByLength algo original
      Nothing -> Left ("hash '" <> original <> "' does not include a type, nor is the type otherwise known from context")
  where
    agreeing algo decoded = case expected of
      Just want | want /= algo -> Left ("hash '" <> original <> "' should have type '" <> want <> "'")
      _ -> (algo,) <$> decoded

-- | Decode a digest that is not SRI, keyed by its length as upstream's
-- @Hash@ constructor keys it (hash.cc at 2.24.9, @baseFromSize@ there at
-- 2.36.0): an algorithm's base16, nix32, and base64 spellings have pairwise
-- distinct lengths, so the input length selects the decoder and every other
-- length is an error.  Trying decoders in sequence instead mis-reads edge
-- inputs - an all-hex-digit string of nix32 length is a nix32 hash, and a
-- truncated hash must be rejected, not decoded by whichever shorter format
-- happens to accept it.  The length is counted in bytes, as upstream counts
-- it, not in characters: base64 decoding ignores what follows the padding,
-- and a character there that UTF-8 spells in more than one byte must still
-- make the length wrong.
digestByLength :: Text -> Text -> Either Text BS.ByteString
digestByLength algo s = parseHashAlgo algo >>= byLength
  where
    len = BS.length (encodeUtf8 s)
    byLength size
      | len == hexHashLen size = sizedDigest "base-16" size s (hexToBytes s)
      | len == nix32HashLen size = sizedDigest "base-32" size s (either (const Nothing) Just (Nix32.decode s))
      | len == base64HashLen size = sizedDigest "base-64" size s (base64ToBytes s)
      | otherwise = Left ("hash '" <> s <> "' has wrong length for hash algorithm '" <> algo <> "'")

-- | Decode an SRI digest, base64 of whatever length, as the @isSRI@ branch
-- of upstream's @Hash@ constructor does (hash.cc at 2.24.9).
sriDigest :: Text -> Text -> Either Text BS.ByteString
sriDigest algo b64 = do
  size <- parseHashAlgo algo
  sizedDigest "SRI" size b64 (base64ToBytes b64)

-- | A decoded digest of the algorithm's size, or upstream's refusal of
-- the spelling it was decoded from: @invalid base-16 hash@ and
-- @invalid base-32 hash@ for a character outside the alphabet or nix32
-- bits past the digest's end (the @Hash@ constructor in hash.cc at 2.24.9),
-- and @invalid SRI hash@ or @invalid base-64 hash@ for base64 that is refused
-- or decodes to the wrong length (its @isSRI@ branch).  A refused base64
-- character is the same error as a wrong length there: the constructor
-- catches @base64Decode@'s exception without rethrowing it, so the length
-- check fails on the empty result.
sizedDigest :: Text -> Int -> Text -> Maybe BS.ByteString -> Either Text BS.ByteString
sizedDigest spelling size encoded decoded = case decoded of
  Just bytes | BS.length bytes == size -> Right bytes
  _ -> Left ("invalid " <> spelling <> " hash '" <> encoded <> "'")

-- | A hash string, its algorithm tag split off.
data HashSpelling
  = -- | @algo:digest@, the digest in base16, nix32 or base64 by its length.
    PrefixedHash !Text !Text
  | -- | @algo-digest@ (SRI), the digest in base64.
    SriHash !Text !Text
  | -- | No tag: the algorithm comes from context.
    UntaggedHash

-- | Split a hash string's algorithm tag off as upstream's
-- @getParsedTypeAndSRI@ does (hash.cc at 2.24.9): at the first
-- @:@, else at the first @-@, which marks SRI.  The tag is everything
-- before the separator, and one that names no known algorithm is an
-- error.  Base64 decoding ignores what follows the padding, so this split
-- is what refuses a separator there: @sha256-...=:x@ names the algorithm
-- @sha256-...=@.
hashSpelling :: Text -> Either Text HashSpelling
hashSpelling s
  | Just (tag, digest) <- splitTag ":" = tagged PrefixedHash tag digest
  | Just (tag, digest) <- splitTag "-" = tagged SriHash tag digest
  | otherwise = Right UntaggedHash
  where
    splitTag sep = case T.breakOn sep s of
      (tag, rest) | not (T.null rest) -> Just (tag, T.drop 1 rest)
      _ -> Nothing
    tagged spelling tag digest = spelling tag digest <$ parseHashAlgo tag

-- | The core store-path construction primitive.  Given a @type@ string, the
-- inner content digest (raw SHA-256 bytes), and a name, produce the store
-- path, as upstream Nix builds one:
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
--
-- 'Left' when the name breaks the store-path name rules
-- ('checkStorePathName'), carrying the hash part the path would have had:
-- upstream hashes before it checks the name, and its refusal names both.
-- Construction validates like the parse boundary does, so every
-- t'StorePath' that names a path has a clean name (the masking
-- placeholder of "Nix.Store.Path.Internal" has an empty one and never
-- reaches a sink): the write sinks downstream
-- (store copies, the builder's delete-and-move) never see a path that
-- resolves outside the store root, and no sink needs its own ad hoc name
-- check.
makeStorePath :: StoreDir -> Text -> BS.ByteString -> Text -> Either BadStorePath StorePath
makeStorePath (StoreDir dir) typ innerDigest name =
  let preimage =
        typ
          <> ":sha256:"
          <> bytesToHexText innerDigest
          <> ":"
          <> T.pack dir
          <> ":"
          <> name
      hashPart = Nix32.encode (compressHash storePathHashBytes (sha256Digest (encodeUtf8 preimage)))
   in case checkStorePathName name of
        Left err -> Left (BadStorePath hashPart err)
        Right () -> Right (StorePath hashPart name)

-- | Construct a text store path (used for @.drv@ files and @builtins.toFile@).
-- The references are embedded in the @type@ string - @\"text\"@ followed by
-- each referenced store path - which is why a derivation's @.drv@ path depends
-- on the paths of all its inputs.  @contentsDigest@ is the SHA-256 of the file
-- contents (the ATerm, for a @.drv@).  'Left' on an invalid name, as
-- 'makeStorePath'.
makeTextPath :: Text -> BS.ByteString -> [StorePath] -> Either BadStorePath StorePath
makeTextPath name contentsDigest refs =
  let sortedRefs = Set.toAscList (Set.fromList refs)
      typ = "text" <> T.concat [":" <> storePathToText defaultStoreDir r | r <- sortedRefs]
   in makeStorePath defaultStoreDir typ contentsDigest name

-- | How a fixed output's contents are hashed: upstream's
-- @FileIngestionMethod@ (file-content-address.hh) without the git method its
-- @git-hashing@ experimental feature adds.
data FileIngestionMethod
  = -- | The bytes of a single file (upstream's @Flat@).
    FlatIngestion
  | -- | The NAR serialisation of a tree (upstream's @NixArchive@).
    NarIngestion
  deriving (Eq, Show)

-- | What a method puts before the algorithm in a fixed output's hash
-- fields and path (upstream's @makeFileIngestionPrefix@,
-- content-address.cc): @r:@ for a NAR hash, and nothing for a flat one,
-- which predates the prefix.
fileIngestionPrefix :: FileIngestionMethod -> Text
fileIngestionPrefix FlatIngestion = ""
fileIngestionPrefix NarIngestion = "r:"

-- | Construct a fixed-output store path.  @foHashDigest@ is the raw bytes of
-- the EXPECTED output hash (e.g. a tarball's SHA-256), taken under
-- @method@.  Mirrors C++ Nix @makeFixedOutputPath@ (store-api.cc at
-- 2.24.9, store-dir-config.cc from 2.34.9):
--
-- * @sha256@ under 'NarIngestion' yields 'makeSourcePath' with no references
-- * otherwise, @makeStorePath \"output:out\" sha256(\"fixed:out:\" prefix algo \":\" hex \":\") name@
--
-- 'Left' on an invalid name, as 'makeStorePath'.
makeFixedOutputPath :: Text -> Text -> FileIngestionMethod -> BS.ByteString -> Either BadStorePath StorePath
makeFixedOutputPath name algo method foHashDigest
  | NarIngestion <- method,
    algo == "sha256" =
      makeSourcePath name foHashDigest Set.empty
  | otherwise =
      let inner = "fixed:out:" <> fileIngestionPrefix method <> algo <> ":" <> bytesToHexText foHashDigest <> ":"
          innerDigest = sha256Digest (encodeUtf8 inner)
       in makeStorePath defaultStoreDir "output:out" innerDigest name

-- | The path of a tree added by the SHA-256 of its NAR, @narDigest@, with
-- the store paths it references: upstream's @makeFixedOutputPath@ under
-- @NixArchive@ and SHA-256 (store-dir-config.cc from 2.34.9, store-api.cc at
-- 2.24.9), whose type is @source@ followed by each reference after a colon,
-- in path order, as @makeType@ beside it appends them.  Upstream's @makeType@
-- also marks a self-reference with @:self@, which no caller here makes.
-- 'Left' on an invalid name, as 'makeStorePath'.
makeSourcePath :: Text -> BS.ByteString -> Set.Set StorePath -> Either BadStorePath StorePath
makeSourcePath name narDigest refs =
  makeStorePath defaultStoreDir typ narDigest name
  where
    typ = "source" <> T.concat [":" <> storePathToText defaultStoreDir r | r <- Set.toAscList refs]

-- | Construct an input-addressed output store path.  @moduloDigest@ is the raw
-- bytes of @hashDerivationModulo@ (masked).  Mirrors C++ Nix @makeOutputPath@:
-- the path name gets an @-<output>@ suffix for non-@out@ outputs.  The
-- composed name is what gets validated - a name and output each clean on
-- their own can still compose past the length limit, so the check belongs
-- here, after composition.  'Left' as 'makeStorePath'.
makeOutputPath :: Text -> BS.ByteString -> Text -> Either BadStorePath StorePath
makeOutputPath outName moduloDigest drvName =
  let pathName = if outName == "out" then drvName else drvName <> "-" <> outName
   in makeStorePath defaultStoreDir ("output:" <> outName) moduloDigest pathName
