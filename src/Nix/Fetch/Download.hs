{-# LANGUAGE ScopedTypeVariables #-}

-- | The evaluator's downloads: a URL's body read as a pull source, from
-- the file a @file:\/\/@ URL names or through nova-nix's HTTP client
-- ("Nix.Http"), each attempt under upstream's retry policy.  Upstream
-- reads both through curl, whose file protocol reads the file the URL's
-- decoded path names.
module Nix.Fetch.Download
  ( DownloadSource (..),
    urlSource,
    unescapeUrlPath,
    withUrlBody,
    downloadBytes,
  )
where

import Control.Exception (IOException, catch)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (fromRight)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)
import qualified Network.HTTP.Client as HTTP
import Nix.Eval.Policy (fileUrlPrefix)
import Nix.Http (FetchRetryPolicy, attemptFailureMessage, downloadUrlWith, fetchWriteFailure, ioRetryEffects, retryTransient)
import Nix.Logger (Logger)
import System.IO (IOMode (ReadMode), withBinaryFile)

-- | Where a download reads a URL's body from.
data DownloadSource
  = -- | The file a @file:\/\/@ URL names.
    LocalFile !FilePath
  | -- | A URL the HTTP client requests.
    RemoteUrl !Text
  deriving (Eq, Show)

-- | Where a download reads a URL from: for a @file:\/\/@ URL the file its
-- path names, decoded as curl decodes it ('unescapeUrlPath'), and
-- otherwise the URL.  A path that decodes to bytes with no UTF-8 reading
-- is read as written.
urlSource :: Text -> DownloadSource
urlSource url = case T.stripPrefix fileUrlPrefix url of
  Just path -> LocalFile (T.unpack (fromRight path (TE.decodeUtf8' (unescapeUrlPath (TE.encodeUtf8 path)))))
  Nothing -> RemoteUrl url

-- | A URL path's bytes with each @%@ and two hex digits decoded, as curl
-- decodes a @file:\/\/@ URL's path; a @%@ without two hex digits after it
-- stays as it is.
unescapeUrlPath :: ByteString -> ByteString
unescapeUrlPath = BS.pack . go . BS.unpack
  where
    go bytes = case bytes of
      percent : high : low : rest
        | percent == percentByte,
          Just decoded <- (\h l -> h * 16 + l) <$> hexValue high <*> hexValue low ->
            decoded : go rest
      byte : rest -> byte : go rest
      [] -> []
    percentByte = 0x25

-- | The value of a hex digit's byte.
hexValue :: Word8 -> Maybe Word8
hexValue byte
  | byte >= 0x30 && byte <= 0x39 = Just (byte - 0x30)
  | byte >= 0x41 && byte <= 0x46 = Just (byte - 0x41 + 10)
  | byte >= 0x61 && byte <= 0x66 = Just (byte - 0x61 + 10)
  | otherwise = Nothing

-- | The bytes read from a local file at a time.
localChunkBytes :: Int
localChunkBytes = 65536

-- | Read a URL's body as a pull source whose empty chunk ends it, handing
-- the source to the consumer, and give what the consumer gives.  An
-- attempt whose transfer fails in a way a retry could outlive is made
-- again from the start, so a consumer starts from nothing each time; a
-- failure no retry changes, a local file that cannot be read among them,
-- is the message "Nix.Http" words it in.  The URL names the source in
-- messages.
withUrlBody :: Logger -> FetchRetryPolicy -> Text -> DownloadSource -> (IO ByteString -> IO a) -> IO (Either Text a)
withUrlBody logger policy url source consume =
  first attemptFailureMessage <$> retryTransient policy (ioRetryEffects logger) attempt
  where
    attempt = case source of
      LocalFile path ->
        (Right <$> withBinaryFile path ReadMode (\handle -> consume (BS.hGetSome handle localChunkBytes)))
          `catch` \(err :: IOException) -> pure (Left (fetchWriteFailure url err))
      RemoteUrl remote -> downloadUrlWith remote (fmap Right . consume . HTTP.brRead)

-- | A URL's body, whole, as upstream's @downloadFile@ reads one.
downloadBytes :: Logger -> FetchRetryPolicy -> Text -> DownloadSource -> IO (Either Text ByteString)
downloadBytes logger policy url source = withUrlBody logger policy url source (drain [])
  where
    drain held pull = do
      chunk <- pull
      if BS.null chunk then pure (BS.concat (reverse held)) else drain (chunk : held) pull
