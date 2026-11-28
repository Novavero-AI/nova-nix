{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The compressed streams nova-nix opens, told apart by their first
-- bytes, as libarchive's filters tell them apart, and decompressed as a
-- pull source so no stream is held whole.
module Nix.Compression.Stream
  ( -- * Formats
    Compression (..),
    compressionOf,
    compressionMagicBytes,

    -- * Streaming
    withDecompressed,
    decodingFailure,

    -- * Failures
    xzReason,
    bzip2Reason,
  )
where

import qualified Codec.Compression.Zlib.Internal as Zlib
import Control.Exception (Exception, Handler (..), catches, throwIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified NovaCache.Bzip2 as Bzip2
import qualified NovaCache.Xz as Xz
import qualified NovaCache.Zstd as Zstd

-- ---------------------------------------------------------------------------
-- Formats
-- ---------------------------------------------------------------------------

-- | A compressed stream, or none.
data Compression = Gzip | Zstd | Xz | Bzip2 | Uncompressed
  deriving (Eq, Show)

-- | The stream a file's first bytes start, told by its magic.
compressionOf :: ByteString -> Compression
compressionOf bytes
  | gzipMagic `BS.isPrefixOf` bytes = Gzip
  | zstdMagic `BS.isPrefixOf` bytes || startsSkippableFrame bytes = Zstd
  | xzMagic `BS.isPrefixOf` bytes = Xz
  | startsBzip2 bytes = Bzip2
  | otherwise = Uncompressed

-- | The most bytes 'compressionOf' reads: an xz stream's magic.
compressionMagicBytes :: Int
compressionMagicBytes = BS.length xzMagic

-- | A gzip member's first bytes: the magic and the deflate method, the
-- only one gzip defines.
gzipMagic :: ByteString
gzipMagic = BS.pack [0x1F, 0x8B, 0x08]

-- | A zstd frame's magic number, as a file starts with it.
zstdMagic :: ByteString
zstdMagic = BS.pack [0x28, 0xB5, 0x2F, 0xFD]

-- | Whether bytes start a zstd skippable frame, as @pzstd@'s output does:
-- a magic number whose low four bits are free, then the rest of its
-- little-endian word.
startsSkippableFrame :: ByteString -> Bool
startsSkippableFrame bytes = case BS.uncons bytes of
  Just (lead, rest) -> lead >= skippableLow && lead <= skippableHigh && skippableTail `BS.isPrefixOf` rest
  Nothing -> False
  where
    skippableLow = 0x50
    skippableHigh = 0x5F
    skippableTail = BS.pack [0x2A, 0x4D, 0x18]

-- | An xz stream's header magic.
xzMagic :: ByteString
xzMagic = BS.pack [0xFD, 0x37, 0x7A, 0x58, 0x5A, 0x00]

-- | Whether bytes start a bzip2 stream: its magic, then the block size in
-- hundreds of kilobytes, a digit from one to nine.
startsBzip2 :: ByteString -> Bool
startsBzip2 bytes = case BS.stripPrefix bzip2Magic bytes >>= BS.uncons of
  Just (size, _) -> size >= smallestBlockSize && size <= largestBlockSize
  Nothing -> False
  where
    smallestBlockSize = 0x31
    largestBlockSize = 0x39

-- | A bzip2 stream's magic, which a block size digit follows.
bzip2Magic :: ByteString
bzip2Magic = "BZh"

-- ---------------------------------------------------------------------------
-- Streaming
-- ---------------------------------------------------------------------------

-- | A gzip stream the decoder refused, thrown from the decompressed
-- source as the other decoders throw theirs.
newtype GzipFailure = GzipFailure Zlib.DecompressError
  deriving (Show)

instance Exception GzipFailure

-- | A source's bytes decompressed as the compression says, as a source
-- whose empty chunk ends it.  A corrupt stream is thrown from the pull,
-- for 'decodingFailure' to read.  No bound is put on what a decoder
-- gives, since a caller reads the stream as it arrives and never holds
-- it whole; xz's decoder keeps nova-cache's bound on its own memory.
withDecompressed :: Compression -> IO ByteString -> (IO ByteString -> IO a) -> IO a
withDecompressed compression source consume = case compression of
  Uncompressed -> consume source
  Gzip -> withGzipSource source consume
  Xz -> Xz.withXzSource Xz.XzLimits {Xz.xzMaxOutputBytes = maxBound, Xz.xzMaxDecoderMemoryBytes = Xz.defaultXzDecoderMemoryBytes} source consume
  Zstd -> Zstd.withZstdSource Zstd.ZstdLimits {Zstd.zstdMaxOutputBytes = maxBound} source consume
  Bzip2 -> Bzip2.withBzip2Source Bzip2.Bzip2Limits {Bzip2.bzip2MaxOutputBytes = maxBound} source consume

-- | A gzip stream decompressed as a source, every member in turn, as
-- libarchive's gzip filter reads one.
withGzipSource :: IO ByteString -> (IO ByteString -> IO a) -> IO a
withGzipSource source consume = do
  state <- newIORef (Zlib.decompressIO Zlib.gzipFormat Zlib.defaultDecompressParams)
  let pull =
        readIORef state >>= \case
          Zlib.DecompressInputRequired supply -> do
            chunk <- source
            supply chunk >>= writeIORef state
            pull
          Zlib.DecompressOutputAvailable output next -> do
            next >>= writeIORef state
            pure output
          Zlib.DecompressStreamEnd _ -> pure BS.empty
          Zlib.DecompressStreamError err -> throwIO (GzipFailure err)
  consume pull

-- | Run a read of a decompressed source, a corrupt stream giving the
-- reason the decoder gives for it.
decodingFailure :: IO a -> IO (Either Text a)
decodingFailure action =
  (Right <$> action)
    `catches` [ Handler (\(GzipFailure err) -> pure (Left ("gzip stream error: " <> T.pack (show err)))),
                Handler (pure . Left . xzReason),
                Handler (pure . Left . zstdReason),
                Handler (pure . Left . bzip2Reason)
              ]

-- ---------------------------------------------------------------------------
-- Failures
-- ---------------------------------------------------------------------------

-- | An xz decoding failure, as a message gives it.
xzReason :: Xz.XzError -> Text
xzReason failure = case failure of
  Xz.XzStreamError message -> "xz stream error: " <> T.pack message
  Xz.XzOutputOverBound bound -> "xz stream decompresses to more than " <> T.pack (show bound) <> " bytes"
  Xz.XzMemoryOverBound bound -> "xz stream needs more than " <> T.pack (show bound) <> " bytes of decoder memory"

-- | A bzip2 decoding failure, as a message gives it.
bzip2Reason :: Bzip2.Bzip2Error -> Text
bzip2Reason failure = case failure of
  Bzip2.Bzip2StreamError message -> "bzip2 stream error: " <> T.pack message
  Bzip2.Bzip2OutputOverBound bound -> "bzip2 stream decompresses to more than " <> T.pack (show bound) <> " bytes"

-- | A zstd decoding failure, as a message gives it.
zstdReason :: Zstd.ZstdError -> Text
zstdReason failure = case failure of
  Zstd.ZstdStreamError message -> "zstd stream error: " <> T.pack message
  Zstd.ZstdOutputOverBound bound -> "zstd stream decompresses to more than " <> T.pack (show bound) <> " bytes"
