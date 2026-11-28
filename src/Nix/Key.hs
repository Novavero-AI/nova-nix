-- | Signing keys as upstream spells, reads and makes them, for
-- @key generate-secret@ and @key convert-secret-to-public@.
--
-- A key, like a signature, is spelled as a name, a colon and the base64 of
-- its bytes ('parseColonBase64').  An Ed25519 secret key is 64 bytes: the
-- seed it was made from and then the public key of that seed, as libsodium
-- lays one out, and its public key is read off as those last 32 bytes, as
-- upstream reads it, rather than derived again from the seed.  The two
-- agree for every key 'generateSecretKey' makes; for a key whose halves do
-- not match, the second half is what upstream prints.
--
-- Names and keys are bytes from end to end: a name is whatever the command
-- line or standard input gave, and is printed back unchanged.
module Nix.Key
  ( -- * The spelling of keys and signatures
    ColonBase64Refusal (..),
    parseColonBase64,
    colonBase64Message,
    renderColonBase64,

    -- * Secret keys
    SecretKey,
    KeyRefusal (..),
    keyRefusalMessage,
    parseSecretKey,
    generateSecretKey,
    renderSecretKey,
    renderPublicKey,
  )
where

import qualified Crypto.PubKey.Ed25519 as Ed25519
import Data.Bifunctor (first)
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (chr)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)
import Nix.Hash (base64ToBytes)
import qualified NovaCache.Base64 as B64

-- ---------------------------------------------------------------------------
-- The spelling of keys and signatures
-- ---------------------------------------------------------------------------

-- | Why a spelling is not a name, a colon and base64.
data ColonBase64Refusal
  = -- | It has no colon, nothing before its first colon, or no bytes
    -- after it.
    ColonBase64Corrupt
  | -- | Its base64 holds this byte, which base64 does not.
    ColonBase64BadCharacter !Word8
  deriving (Eq, Show)

-- | A name and the bytes its base64 gives, from a spelling, as upstream
-- reads a key or a signature: the name is what comes before the first
-- colon and must not be empty, and the base64 after it is read as
-- 'base64ToBytes' reads it, up to its first @=@ and skipping line feeds,
-- and must give at least one byte.
parseColonBase64 :: ByteString -> Either ColonBase64Refusal (ByteString, ByteString)
parseColonBase64 spelled = case BS.break (== colonByte) spelled of
  (name, rest)
    | not (BS.null name),
      Just encoded <- BS.stripPrefix (BS.singleton colonByte) rest ->
        -- Latin-1 gives each byte a character of its own, so a byte
        -- outside base64 is a character outside it.
        case base64ToBytes (TE.decodeLatin1 encoded) of
          Just bytes
            | not (BS.null bytes) -> Right (name, bytes)
            | otherwise -> Left ColonBase64Corrupt
          Nothing -> Left (maybe ColonBase64Corrupt ColonBase64BadCharacter (BS.find (not . readAsBase64) (BS.takeWhile (/= paddingByte) encoded)))
  _ -> Left ColonBase64Corrupt

-- | What upstream says of a refused spelling of the named kind (@key@ or
-- @signature@).  A byte that is not ASCII is shown as the character of its
-- value, where upstream writes the byte itself.
colonBase64Message :: Text -> ColonBase64Refusal -> Text
colonBase64Message kind refusal = case refusal of
  ColonBase64Corrupt -> kind <> " is corrupt"
  ColonBase64BadCharacter byte -> "invalid character in Base64 string: '" <> T.singleton (chr (fromIntegral byte)) <> "'"

-- | A name and bytes spelled as upstream spells a key or a signature: the
-- name, a colon and the padded base64 of the bytes.
renderColonBase64 :: ByteString -> ByteString -> ByteString
renderColonBase64 name bytes = name <> BS.singleton colonByte <> TE.encodeUtf8 (B64.encode bytes)

-- | Whether upstream's base64 reader takes a byte before the padding: one
-- of the standard alphabet, or a line feed, which it skips.
readAsBase64 :: Word8 -> Bool
readAsBase64 byte =
  (byte >= 0x41 && byte <= 0x5a)
    || (byte >= 0x61 && byte <= 0x7a)
    || (byte >= 0x30 && byte <= 0x39)
    || byte `elem` [0x2b, 0x2f, 0x0a]

-- | The byte that ends a key's or a signature's name.
colonByte :: Word8
colonByte = 0x3a

-- | The byte at which base64 reading stops.
paddingByte :: Word8
paddingByte = 0x3d

-- ---------------------------------------------------------------------------
-- Secret keys
-- ---------------------------------------------------------------------------

-- | An Ed25519 secret key and its name.  Only 'parseSecretKey' and
-- 'generateSecretKey' make one, so the bytes are always 64.
data SecretKey = SecretKey !ByteString !ByteString

-- | The bytes of an Ed25519 secret key, libsodium's
-- @crypto_sign_SECRETKEYBYTES@.
ed25519SecretKeyBytes :: Int
ed25519SecretKeyBytes = 64

-- | The bytes of an Ed25519 public key, the last of a secret key's,
-- libsodium's @crypto_sign_PUBLICKEYBYTES@.
ed25519PublicKeyBytes :: Int
ed25519PublicKeyBytes = 32

-- | Why a secret key does not read.
data KeyRefusal
  = -- | It is not a name, a colon and base64.
    KeyUnreadable !ColonBase64Refusal
  | -- | Its bytes are not 64.
    SecretKeyNotValid
  deriving (Eq, Show)

-- | A refused secret key as upstream's command reports it on standard
-- error where that is not a terminal, without the final line feed.  A key
-- that is not a name, a colon and base64 fails with the trace upstream
-- adds as it decodes one, which names the key it has not yet read, so the
-- name is always empty; upstream sets the trace and then the error on
-- lines of their own below an @error:@ line, indented by that prefix's
-- width, the trace after an ellipsis.  A key of the wrong size fails on
-- one line.
keyRefusalMessage :: KeyRefusal -> ByteString
keyRefusalMessage refusal = case refusal of
  KeyUnreadable unreadable ->
    BS.intercalate
      "\n"
      [ "error:",
        indentation <> TE.encodeUtf8 (T.singleton traceEllipsis) <> " while decoding key named ''",
        "",
        indentation <> errorPrefix <> unreadableMessage unreadable
      ]
  SecretKeyNotValid -> errorPrefix <> "secret key is not valid"
  where
    indentation = BS.replicate (BS.length errorPrefix) spaceByte
    unreadableMessage unreadable = case unreadable of
      ColonBase64Corrupt -> "key is corrupt"
      ColonBase64BadCharacter byte -> "invalid character in Base64 string: '" <> BS.singleton byte <> "'"

-- | What starts the line of an error.
errorPrefix :: ByteString
errorPrefix = "error: "

-- | What starts a line of an error's trace.
traceEllipsis :: Char
traceEllipsis = '\x2026'

-- | A space.
spaceByte :: Word8
spaceByte = 0x20

-- | A secret key from its spelling, all of it as given, as upstream reads
-- one: 'parseColonBase64', then 64 bytes.  Nothing around it is trimmed,
-- but the base64 is read up to its first @=@, so what follows a padded
-- key, a line feed or a carriage return, is not read, while a carriage
-- return or a space before the padding is refused and a line feed there
-- skipped.
parseSecretKey :: ByteString -> Either KeyRefusal SecretKey
parseSecretKey spelled = do
  (name, bytes) <- first KeyUnreadable (parseColonBase64 spelled)
  if BS.length bytes == ed25519SecretKeyBytes
    then Right (SecretKey name bytes)
    else Left SecretKeyNotValid

-- | A new secret key with the given name: a seed from the system's source
-- of randomness and the public key of that seed, as libsodium's
-- @crypto_sign_keypair@ makes the key upstream generates.
generateSecretKey :: ByteString -> IO SecretKey
generateSecretKey name = do
  seed <- Ed25519.generateSecretKey
  pure (SecretKey name (BA.convert seed <> BA.convert (Ed25519.toPublic seed)))

-- | A secret key spelled as upstream spells one.
renderSecretKey :: SecretKey -> ByteString
renderSecretKey (SecretKey name bytes) = renderColonBase64 name bytes

-- | The public key of a secret key, spelled as upstream spells one: the
-- secret key's name and its last 32 bytes.
renderPublicKey :: SecretKey -> ByteString
renderPublicKey (SecretKey name bytes) = renderColonBase64 name (BS.takeEnd ed25519PublicKeyBytes bytes)
