-- | The HTTP transfer layer every download nova-nix makes goes through:
-- the User-Agent each request carries, and upstream's retry policy with
-- the classification that decides what a retry can outlive.
--
-- Upstream serves every download, a fetcher's and a substituter's alike,
-- through one @FileTransfer@ (@libstore/filetransfer.cc@ at 2.24.9), with
-- one @download-attempts@ setting, one backoff and one table of the
-- failures that are never retried.  @builtin:fetchurl@ ("Nix.Builder")
-- and the substituter ("Nix.Substituter") share this module for the same
-- reason: a source tarball, a narinfo and a NAR fail and retry alike.
module Nix.Http
  ( -- * Identity
    userAgent,
    withUserAgent,

    -- * Attempt failures
    AttemptFailure (..),
    attemptFailureMessage,
    catchSync,

    -- * Retry policy
    FetchRetryPolicy (..),
    defaultFetchRetryPolicy,
    RetryEffects (..),
    ioRetryEffects,
    retryTransient,
    retryDelayMs,

    -- * Classification
    TransferError (..),
    statusError,
    withTransfer,
    transferBodyReader,
    transferFailureHandlers,
    fetchStatusFailure,
    fetchExceptionFailure,
    fetchWriteFailure,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (Handler (..), IOException, SomeAsyncException (..), SomeException, catch, displayException, fromException, throwIO)
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Version (showVersion)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types.Header (hUserAgent)
import qualified Network.HTTP.Types.Status as HTTP
import Paths_nova_nix (version)
import System.IO (stderr)
import System.Random (randomRIO)

-- ---------------------------------------------------------------------------
-- Identity
-- ---------------------------------------------------------------------------

-- | @nova-nix\/VERSION (+repository)@: the product token upstream's shape
-- calls for, and a comment naming where the client comes from.
--
-- Upstream identifies itself on every request as @curl\/VERSION
-- Nix\/VERSION@.  A request with no User-Agent is what bot mitigation in
-- front of a server scores lowest: ftp.gnu.org answers one with 403
-- outright, and Cloudflare in front of cache.novavero.ai challenged every
-- request a GitHub runner made without one while passing the same requests
-- carrying this header.  The fetcher sent one; the substituter and push
-- sent none, so the public cache could be read by a person and not by the
-- tool built to read it.  The version comes from Cabal, so the header
-- names the build that made the request rather than a spelling that can
-- drift at release time.
userAgent :: BS8.ByteString
userAgent = BS8.pack ("nova-nix/" <> showVersion version <> " (+https://github.com/Novavero-AI/nova-nix)")

-- | The request with 'userAgent' as its only User-Agent, whatever it
-- carried before.
withUserAgent :: HTTP.Request -> HTTP.Request
withUserAgent request =
  request
    { HTTP.requestHeaders =
        (hUserAgent, userAgent) : filter ((/= hUserAgent) . fst) (HTTP.requestHeaders request)
    }

-- ---------------------------------------------------------------------------
-- Attempt failures
-- ---------------------------------------------------------------------------

-- | How one download attempt failed, deciding whether the retry budget
-- applies.  'TransientFailure' is a failure a fresh attempt could
-- plausibly complete; 'FatalFailure' is deterministic - the same served
-- object fails the same way every time.
data AttemptFailure
  = TransientFailure !Text
  | FatalFailure !Text
  deriving (Eq, Show)

-- | The failure's message, independent of its retry class.
attemptFailureMessage :: AttemptFailure -> Text
attemptFailureMessage failure = case failure of
  TransientFailure msg -> msg
  FatalFailure msg -> msg

-- | Run an action, passing only synchronous exceptions to the handler.
-- Asynchronous exceptions (a Ctrl-C, a timeout) re-throw untouched: an
-- interrupt converted into a recoverable failure would be spent as
-- retry budget, as fallthrough to the next cache, or as a local build
-- instead of aborting.  Every catch-all on the download, substitution
-- and build paths goes through this one split.
catchSync :: IO a -> (SomeException -> IO a) -> IO a
catchSync action handler = action `catch` classify
  where
    classify someErr
      | Just (SomeAsyncException _) <- fromException someErr = throwIO someErr
      | otherwise = handler someErr

-- ---------------------------------------------------------------------------
-- Retry policy
-- ---------------------------------------------------------------------------

-- | How many times one download is tried before it fails: upstream's
-- @download-attempts@ default (@libstore/filetransfer.hh@ at 2.24.9,
-- @Setting<unsigned int> tries{this, 5, "download-attempts", ...}@).
downloadAttempts :: Int
downloadAttempts = 5

-- | The delay before the first retry, in milliseconds; every later retry
-- doubles it (@FileTransferRequest::baseRetryTimeMs = 250@ in the same
-- header).
retryBaseDelayMs :: Int
retryBaseDelayMs = 250

-- | The spread upstream adds to the backoff exponent so that clients
-- which failed together do not retry together: a uniform draw from
-- @[0, 0.5)@ (@filetransfer.cc@ at 2.24.9, line 500).
retryJitterCeiling :: Double
retryJitterCeiling = 0.5

microsPerMilli :: Int
microsPerMilli = 1000

-- | How often one download is tried and how long to wait between tries.
-- 'defaultFetchRetryPolicy' is upstream's; tests shrink the delay.
data FetchRetryPolicy = FetchRetryPolicy
  { frpAttempts :: !Int,
    frpBaseDelayMs :: !Int
  }
  deriving (Eq, Show)

-- | Upstream's policy: five attempts, 250 ms before the first retry.
defaultFetchRetryPolicy :: FetchRetryPolicy
defaultFetchRetryPolicy = FetchRetryPolicy {frpAttempts = downloadAttempts, frpBaseDelayMs = retryBaseDelayMs}

-- | The delay before retry number @retry@ (counting from one), given the
-- jitter drawn for it: upstream's @baseRetryTimeMs * 2 ^ (attempt - 1 +
-- jitter)@ (@filetransfer.cc@ at 2.24.9, line 500), truncated to whole
-- milliseconds as its assignment to an @int@ truncates.
retryDelayMs :: FetchRetryPolicy -> Int -> Double -> Int
retryDelayMs policy retry jitter =
  truncate (fromIntegral (frpBaseDelayMs policy) * 2 ** (fromIntegral (retry - 1) + jitter) :: Double)

-- | What the retry loop needs from the outside world, injected so the
-- policy is testable with no clock, no entropy and no network.
data RetryEffects m = RetryEffects
  { reSleepMs :: !(Int -> m ()),
    reJitter :: !(m Double),
    reWarn :: !(Text -> m ())
  }

-- | The real effects: a sleep, a uniform jitter, and a warning on stderr
-- in the shape of upstream's @warn("%s; retrying in %d ms", ...)@.
ioRetryEffects :: RetryEffects IO
ioRetryEffects =
  RetryEffects
    { reSleepMs = threadDelay . (* microsPerMilli),
      reJitter = randomRIO (0, retryJitterCeiling),
      reWarn = TIO.hPutStrLn stderr . ("warning: " <>)
    }

-- | Run one download's attempt under the retry policy: a
-- 'TransientFailure' is tried again after the backoff until the attempts
-- are spent, while a 'FatalFailure' or a success ends the loop at once.
-- Exceptions propagate, so a cancellation is never spent as retry budget.
retryTransient :: (Monad m) => FetchRetryPolicy -> RetryEffects m -> m (Either AttemptFailure a) -> m (Either AttemptFailure a)
retryTransient policy effects action = go 1
  where
    go !attempt = do
      result <- action
      case result of
        Left (TransientFailure err)
          | attempt < frpAttempts policy -> do
              jitter <- reJitter effects
              let delay = retryDelayMs policy attempt jitter
              reWarn effects (err <> "; retrying in " <> T.pack (show delay) <> " ms")
              reSleepMs effects delay
              go (attempt + 1)
        _ -> pure result

-- ---------------------------------------------------------------------------
-- Classification
-- ---------------------------------------------------------------------------

-- | 'HTTP.withResponse', with the body read through
-- 'transferBodyReader', so every failure of the connection is an
-- 'HTTP.HttpException', the body's included.
withTransfer :: HTTP.Request -> HTTP.Manager -> (HTTP.Response HTTP.BodyReader -> IO a) -> IO a
withTransfer request manager consume =
  HTTP.withResponse request manager (consume . fmap (transferBodyReader request))

-- | A response body reader whose failures are all
-- 'HTTP.HttpException's.  The client's manager wraps what opening a
-- response throws (a reset, a TLS failure) into
-- 'HTTP.InternalException', but hands the body reader over unwrapped
-- (@responseOpen@ in http-client 0.7), so a connection reset or a torn
-- TLS record mid-body arrives as a bare 'IOException' or TLS exception,
-- the shape of a failure writing the output.  Wrapping the read the way
-- the manager wraps the open keeps the two apart: a transport failure is
-- transient wherever it happens, as curl's receive errors are upstream,
-- and a write failure is not.  The reader only ever reads the
-- connection, so every synchronous exception it raises is the
-- transport's; an asynchronous one passes through untouched.
transferBodyReader :: HTTP.Request -> HTTP.BodyReader -> HTTP.BodyReader
transferBodyReader request reader = reader `catchSync` asTransportFailure
  where
    asTransportFailure err = case fromException err :: Maybe HTTP.HttpException of
      Just _ -> throwIO err
      Nothing -> throwIO (HTTP.HttpExceptionRequest request (HTTP.InternalException err))

-- | What a transfer attempt throws, turned into its failure: an
-- 'HTTP.HttpException' by 'fetchExceptionFailure', an 'IOException' by
-- 'fetchWriteFailure'.  Nothing else is caught, so an asynchronous
-- exception (an interrupt, a cancelled build) leaves the attempt as it
-- arrived and is never spent as retry budget, and anything unforeseen
-- reaches the caller's own boundary.
transferFailureHandlers :: Text -> [Handler (Either AttemptFailure a)]
transferFailureHandlers url =
  [ Handler (pure . Left . fetchExceptionFailure url),
    Handler (pure . Left . fetchWriteFailure url)
  ]

-- | Upstream's verdict on a response that is not a success
-- (@FileTransfer::Error@, @filetransfer.hh@ at 2.24.9, whose
-- @Interrupted@ is an asynchronous exception here, never a value).  Only
-- 'Transient' is retried.  A binary cache store reads 'NotFound' and
-- 'Forbidden' alike as the file not being in that cache
-- (@http-binary-cache-store.cc@ at 2.24.9, lines 123, 160 and 182),
-- since an S3 bucket that cannot be listed answers a missing key with
-- 403.
data TransferError
  = -- | 404 or 410: the file is not there.
    NotFound
  | -- | 401, 403 or 407: the request was refused.
    Forbidden
  | -- | A failure no retry can change.
    Misc
  | -- | A failure a retry could outlive.
    Transient
  deriving (Eq, Show)

-- | Classify a response status the way upstream's transfer layer does,
-- in its order (@filetransfer.cc@ at 2.24.9, lines 423-441): 404 and 410
-- are 'NotFound'; 401, 403 and 407 are 'Forbidden'; every other 4xx
-- except 408 and 429 (the server timed out waiting for the request, or
-- asked for a slower pace) is 'Misc', as are 501, 505 and 511 (the
-- server cannot speak this protocol, or a captive portal is in the way).
-- Everything else is 'Transient', the remaining 5xx included.
statusError :: HTTP.Status -> TransferError
statusError status
  | status `elem` [HTTP.status404, HTTP.status410] = NotFound
  | status `elem` [HTTP.status401, HTTP.status403, HTTP.status407] = Forbidden
  | HTTP.statusIsClientError status && status `notElem` [HTTP.status408, HTTP.status429] = Misc
  | status `elem` [HTTP.status501, HTTP.status505, HTTP.status511] = Misc
  | otherwise = Transient

-- | A response status as an attempt's failure, by 'statusError': only a
-- 'Transient' one is retried.
fetchStatusFailure :: Text -> HTTP.Status -> AttemptFailure
fetchStatusFailure url status = case statusError status of
  Transient -> TransientFailure message
  NotFound -> FatalFailure message
  Forbidden -> FatalFailure message
  Misc -> FatalFailure message
  where
    message = "HTTP " <> T.pack (show (HTTP.statusCode status)) <> " fetching " <> url

-- | Classify a failure of the HTTP client, by upstream's list of curl
-- results that are not retried (@filetransfer.cc@ at 2.24.9, lines
-- 443-465).  A URL the client cannot parse (@CURLE_URL_MALFORMAT@) is an
-- 'HTTP.InvalidUrlException' from the parser, except for an empty host,
-- which the parser accepts and the connection lookup rejects as
-- 'HTTP.InvalidDestinationHost' on every attempt.  A scheme the client
-- does not speak (@CURLE_UNSUPPORTED_PROTOCOL@) is an
-- 'HTTP.InvalidUrlException' too, or 'HTTP.TlsNotSupported' when the
-- manager has no TLS.  A redirect loop (@CURLE_TOO_MANY_REDIRECTS@)
-- completes the deterministic set.  Every other transport failure is
-- transient, name resolution and TLS included, as upstream has it.  The
-- message names the URL and the failure only: the client's own rendering
-- prints the whole request record over a dozen lines, and a retry would
-- repeat it.
fetchExceptionFailure :: Text -> HTTP.HttpException -> AttemptFailure
fetchExceptionFailure url err = case err of
  HTTP.InvalidUrlException _ reason -> FatalFailure (describeFailure url reason)
  HTTP.HttpExceptionRequest _ content -> case content of
    HTTP.InvalidDestinationHost _ -> FatalFailure (describeFailure url "empty host")
    HTTP.TlsNotSupported -> FatalFailure (describeFailure url "TLS is not supported")
    HTTP.TooManyRedirects _ -> FatalFailure (describeFailure url "too many redirects")
    _ -> TransientFailure (describeFailure url (show content))

-- | A failure writing what the transfer delivered: upstream's
-- @CURLE_WRITE_ERROR@, never retried, since the disk that refused one
-- write refuses the next.  An 'IOException' reaching the attempt boundary
-- is one, because 'withTransfer' has already raised every failure of the
-- connection as an 'HTTP.HttpException'.
fetchWriteFailure :: Text -> IOException -> AttemptFailure
fetchWriteFailure url err = FatalFailure (describeFailure url (displayException err))

describeFailure :: Text -> String -> Text
describeFailure url detail = "download error fetching " <> url <> ": " <> T.pack detail
