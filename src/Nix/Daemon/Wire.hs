{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The framing upstream's worker protocol puts on bytes, as upstream's
-- @serialise.hh@ and @serialise.cc@ define it (at 2.35.4): every number is
-- a 64-bit little-endian word, a string is its length, its bytes and zero
-- padding to a multiple of eight, a list is its count and then its items,
-- and a framed stream is a run of chunks, each its length and its bytes,
-- ended by an empty one.
--
-- A t'Decoder' reads incrementally: 'startDecoding' gives a t'Decoding'
-- that asks for input until it has a value, so a connection hands it each
-- read as it arrives, and a framed stream is read a chunk at a time with
-- 'getFrame'.  Encoders are @bytestring@ 'Builder's.  Nothing here knows
-- what the values mean; "Nix.Daemon.Protocol" reads and writes those.
module Nix.Daemon.Wire
  ( -- * Decoding
    Decoder,
    DecodeError (..),
    malformed,
    Decoding (..),
    startDecoding,
    feedInput,
    runDecoder,

    -- ** Numbers
    getWord64,
    getWord32,
    getWord8,
    getInt64,
    getBool,
    getCount,

    -- ** Strings and lists
    getBytes,
    getList,

    -- ** Framed streams
    getFrame,

    -- * Encoding
    putNum,
    putBool,
    putBytes,
    putList,
    putFrame,
    putFrameEnd,
  )
where

import Control.Monad (replicateM, unless)
import Control.Monad.Except (ExceptT (..), runExceptT)
import qualified Data.Binary.Get as Get
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BB
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32, Word64, Word8)

-- ---------------------------------------------------------------------------
-- Decoding
-- ---------------------------------------------------------------------------

-- | A reader of wire bytes.  It fails with upstream's message where
-- upstream's reader throws, and asks for more input where upstream's would
-- block on its source.
newtype Decoder a = Decoder (ExceptT Text Get.Get a)
  deriving (Functor, Applicative, Monad)

-- | Why a t'Decoder' gave no value.
data DecodeError
  = -- | The input ended inside a value, where upstream's read throws
    -- @EndOfFile@.
    InputEnded
  | -- | Bytes upstream refuses, with the message it refuses them with.
    Malformed !Text
  deriving (Eq, Show)

-- | Refuse what has been read, with upstream's message for it.
malformed :: Text -> Decoder a
malformed = Decoder . ExceptT . pure . Left

-- | A decode in progress.
data Decoding a
  = -- | The value, and the input that followed it.
    Decoded !a !ByteString
  | -- | More input is needed: the next chunk, or 'Nothing' at the end of
    -- the input.
    NeedInput (Maybe ByteString -> Decoding a)
  | -- | The input holds no value.
    DecodeFailed !DecodeError

-- | Begin decoding, before any input has arrived.
startDecoding :: Decoder a -> Decoding a
startDecoding (Decoder reader) = fromBinary (Get.runGetIncremental (runExceptT reader))
  where
    -- A binary failure is always the end of the input: no reader here
    -- calls 'fail', and every refusal is the 'Left' of 'Malformed'.
    fromBinary step = case step of
      Get.Done rest _ (Right value) -> Decoded value rest
      Get.Done _ _ (Left message) -> DecodeFailed (Malformed message)
      Get.Fail {} -> DecodeFailed InputEnded
      Get.Partial more -> NeedInput (fromBinary . more)

-- | Hand a decode the next chunk of input, or 'Nothing' at its end.  A
-- decode that is over keeps a chunk after its value as input that followed
-- it.
feedInput :: Maybe ByteString -> Decoding a -> Decoding a
feedInput chunk decoding = case decoding of
  NeedInput more -> more chunk
  Decoded value rest -> Decoded value (maybe rest (rest <>) chunk)
  failed@(DecodeFailed _) -> failed

-- | Decode a value from input that is all there is, giving the value and
-- the bytes after it.
runDecoder :: Decoder a -> ByteString -> Either DecodeError (a, ByteString)
runDecoder reader input = case feedInput Nothing (feedInput (Just input) (startDecoding reader)) of
  Decoded value rest -> Right (value, rest)
  DecodeFailed err -> Left err
  -- A decode told that its input ended never asks for more.
  NeedInput _ -> Left InputEnded

-- | Lift a reader of @binary@'s into a t'Decoder'.
fromGet :: Get.Get a -> Decoder a
fromGet = Decoder . ExceptT . fmap Right

-- ---------------------------------------------------------------------------
-- Numbers
-- ---------------------------------------------------------------------------

-- | A 64-bit word, as upstream's @readNum<uint64_t>@ reads it.
getWord64 :: Decoder Word64
getWord64 = fromGet Get.getWord64le

-- | A word read as upstream's @readNum<T>@ reads it for a type whose
-- largest value is @bound@: a larger one is refused, and the message names
-- the type as @typeid(T).name()@ does under the Itanium ABI on 64-bit
-- Linux, which is what upstream prints there.
getBounded :: Text -> Word64 -> Decoder Word64
getBounded typeName bound = getWord64 >>= checked
  where
    checked n
      | n > bound = malformed ("serialised integer " <> T.pack (show n) <> " is too large for type '" <> typeName <> "'")
      | otherwise = pure n

-- | A number upstream reads as an @unsigned int@ (its @readInt@).
getWord32 :: Decoder Word32
getWord32 = fromIntegral <$> getBounded "j" (fromIntegral (maxBound :: Word32))

-- | A number upstream reads as a @uint8_t@.
getWord8 :: Decoder Word8
getWord8 = fromIntegral <$> getBounded "h" (fromIntegral (maxBound :: Word8))

-- | A number upstream reads as an @int64_t@ or a @time_t@: a word above
-- the largest of those is refused, so a negative one, which upstream
-- writes as its two's complement, does not read back.
getInt64 :: Decoder Int64
getInt64 = fromIntegral <$> getBounded "l" (fromIntegral (maxBound :: Int64))

-- | A flag, which upstream reads as a @bool@ through @readNum@, so a word
-- other than 0 or 1 is refused.
getBool :: Decoder Bool
getBool = (/= 0) <$> getBounded "b" 1

-- | A length or a count, which upstream reads as a @size_t@.  One larger
-- than this process can hold is refused; upstream's @size_t@ holds every
-- word, and it goes on to allocate a string that long or to read a list's
-- items until its input ends.
getCount :: Decoder Int
getCount = fromIntegral <$> getBounded "m" (fromIntegral (maxBound :: Int))

-- ---------------------------------------------------------------------------
-- Strings and lists
-- ---------------------------------------------------------------------------

-- | Bytes are padded with zeros to a multiple of this many.
wordBytes :: Int
wordBytes = 8

-- | How many zero bytes follow a string of the given length.
paddingLength :: Int -> Int
paddingLength len = negate len `mod` wordBytes

-- | A string, as upstream's @readString@ reads it: its length, its bytes,
-- and padding, which must be zeros.  The bytes are copied out of the
-- input, so a string kept does not keep the chunk it arrived in.
getBytes :: Decoder ByteString
getBytes = do
  len <- getCount
  bytes <- fromGet (Get.getByteString len)
  padding <- fromGet (Get.getByteString (paddingLength len))
  unless (BS.all (== 0) padding) (malformed "non-zero padding")
  pure $! BS.copy bytes

-- | A list: its count, then each item.
getList :: Decoder a -> Decoder [a]
getList item = getCount >>= (`replicateM` item)

-- ---------------------------------------------------------------------------
-- Framed streams
-- ---------------------------------------------------------------------------

-- | The next chunk of a framed stream, or 'Nothing' at the empty chunk
-- that ends it, as upstream's @FramedSource@ reads them: a length read as
-- an @unsigned int@, then that many bytes with no padding.  The chunk is
-- a slice of the input, for a caller that passes it on rather than keeping
-- it.
getFrame :: Decoder (Maybe ByteString)
getFrame = getWord32 >>= chunkOf
  where
    chunkOf len
      | len == 0 = pure Nothing
      | otherwise = Just <$> fromGet (Get.getByteString (fromIntegral len))

-- ---------------------------------------------------------------------------
-- Encoding
-- ---------------------------------------------------------------------------

-- | A number as the 64-bit little-endian word upstream writes for every
-- integer type; a negative one is its two's complement, as upstream's
-- conversion to @uint64_t@ makes it.
putNum :: (Integral a) => a -> Builder
putNum n = BB.word64LE (fromIntegral n)

-- | A flag, as 0 or 1.
putBool :: Bool -> Builder
putBool = putNum . fromEnum

-- | A string: its length, its bytes and zero padding.
putBytes :: ByteString -> Builder
putBytes bytes =
  putNum len <> BB.byteString bytes <> BB.byteString (BS.replicate (paddingLength len) 0)
  where
    len = BS.length bytes

-- | A list: its count, then each item.
putList :: (a -> Builder) -> [a] -> Builder
putList item items = putNum (length items) <> foldMap item items

-- | The longest chunk upstream's @FramedSource@ reads, whose length is an
-- @unsigned int@.
maxFrameLength :: Int
maxFrameLength = fromIntegral (maxBound :: Word32)

-- | Bytes as chunks of a framed stream, as upstream's @FramedSink@ writes
-- them: each chunk its length and its bytes.  Nothing is written for no
-- bytes, since an empty chunk ends the stream, and bytes longer than a
-- chunk can hold go as several.
putFrame :: ByteString -> Builder
putFrame bytes
  | BS.null bytes = mempty
  | otherwise =
      let (chunk, rest) = BS.splitAt maxFrameLength bytes
       in putNum (BS.length chunk) <> BB.byteString chunk <> putFrame rest

-- | The empty chunk that ends a framed stream.
putFrameEnd :: Builder
putFrameEnd = putNum (0 :: Word64)
