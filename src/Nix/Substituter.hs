{-# LANGUAGE ScopedTypeVariables #-}

-- | Binary substituter - download pre-built paths from remote caches.
--
-- == How substitution works
--
-- Before building a derivation, Nix checks if the output already exists
-- in a binary cache.  The protocol:
--
-- 1. Compute the output store path hash from the derivation
-- 2. @GET https:\/\/cache.example.com\/\<hash\>.narinfo@
-- 3. If 200: parse the narinfo (NAR hash, size, references, signature)
-- 4. Validate the narinfo's fields, then verify the signature against a
--    trusted public key
-- 5. @GET https:\/\/cache.example.com\/nar\/\<narhash\>.nar.xz@ and, in
--    one bounded streaming pass, decompress, hash, parse, and unpack
--    into the store path
-- 6. Verify the declared NAR hash and size against the streamed bytes,
--    then re-verify the materialized tree from disk
-- 7. Register in the store DB with references from narinfo
--
-- If the cache doesn't have it (404), fall through to building locally.
--
-- == Cache priority
--
-- Multiple caches can be configured, checked in priority order:
--
-- @
-- substituters = https:\/\/cache.novavero.ai https:\/\/cache.nixos.org
-- trusted-public-keys = cache.novavero.ai-1:... cache.nixos.org-1:...
-- @
--
-- Our nova-cache server implements this protocol.  The narinfo format,
-- NAR serialization, signature verification - all handled by the
-- @nova-cache@ library.  This module orchestrates the HTTP requests
-- and store registration.
module Nix.Substituter
  ( -- * Substitution
    SubstResult (..),
    trySubstitute,

    -- * Cache configuration
    CacheConfig (..),
    defaultCacheConfig,

    -- * Failure classification
    AttemptFailure (..),
    attemptFailureMessage,
    httpStatusFailure,
    compressedBodyCeiling,
    downloadCapFor,

    -- * Pure helpers (exported for testing)
    maxNarInfoBody,
    readBodyCapped,
    sortCaches,
    tryCachesWith,
    validateNarInfoFields,
    narInfoPreflight,
    verifySigs,
    verifyNarHash,
    verifyNarSize,
    narInfoMatchesPath,
    decompressorFor,
    decompressNar,
    streamingDecompressionSupported,
    withDecompressedSource,
    cappedBodySource,
    materializeNarFromSource,
    consumeNarStream,
    unpackNarEntry,
    unpackAndVerify,
    clearStaleDestination,
    parseReferences,
    parseDeriver,
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Exception (Exception, SomeAsyncException (..), SomeException, catch, fromException, onException, throwIO, try)
import Control.Monad (void)
import qualified Data.ByteString as BS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word64)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS
import qualified Network.HTTP.Types.Status as HTTP
import Nix.Compression (NarCompression (..), parseNarCompression)
import Nix.Store (Store (..), abortNarUnpack, finishNarUnpack, newNarUnpackSink, setReadOnly, sinkNarEvent, unpackNarEntry)
import Nix.Store.DB (PathRegistration (..))
import Nix.Store.Path (StoreDir, StorePath (spHash), parseStorePathBaseName, storePathHashLen, storePathToFilePath)
import qualified NovaCache.Hash as Hash
import qualified NovaCache.NAR as NAR
import qualified NovaCache.NAR.Stream as Stream
import qualified NovaCache.NarInfo as NarInfo
import qualified NovaCache.Signing as Signing
import qualified NovaCache.Validate as Validate
import qualified NovaCache.Xz as Xz
import qualified System.Directory as Dir

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Configuration for a binary cache.
data CacheConfig = CacheConfig
  { -- | Base URL of the cache (e.g. @https:\/\/cache.novavero.ai@).
    ccUrl :: !Text,
    -- | Trusted public key for signature verification (@name:base64key@).
    ccPublicKey :: !Text,
    -- | Priority (lower = checked first). cache.nixos.org is 40.
    ccPriority :: !Int
  }
  deriving (Eq, Show)

-- | Default cache configuration for cache.nixos.org.
defaultCacheConfig :: CacheConfig
defaultCacheConfig =
  CacheConfig
    { ccUrl = "https://cache.nixos.org",
      ccPublicKey = "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=",
      ccPriority = 40
    }

-- | Result of a substitution attempt.
data SubstResult
  = -- | Verified and unpacked on disk, NOT yet registered: the carried
    -- registration is recorded by the caller, which batches every output
    -- of a derivation into one 'registerPaths' transaction so
    -- cross-output reference edges are never dropped.
    SubstSuccess !PathRegistration
  | -- | Cache doesn't have this path.
    SubstNotFound
  | -- | Download or verification failed.
    SubstError !Text
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Main substitution logic
-- ---------------------------------------------------------------------------

-- | Try to substitute a store path from configured caches.
--
-- Checks each cache in priority order.  Returns on first success; a
-- failing cache falls through to the remaining ones.  Uses nova-cache
-- library for narinfo parsing, NAR unpacking, and signature verification.
--
-- On success the path is unpacked and read-only on disk but NOT
-- registered - the caller records the returned 'PathRegistration'
-- (see 'SubstSuccess').
trySubstitute :: Store -> [CacheConfig] -> StorePath -> IO SubstResult
trySubstitute _ [] _ = pure SubstNotFound
trySubstitute store caches sp = do
  -- Reuse the process-global TLS manager (connection pooling / keep-alive)
  -- rather than creating a fresh one per call and per output.
  manager <- HTTPS.getGlobalManager
  tryCachesWith (\cache -> tryOneCache manager store cache sp) (sortCaches caches)

-- | Fold per-cache attempts in priority order.  The first success wins and
-- stops the scan.  An erroring cache falls through to the remaining ones -
-- a transient failure (DNS, TLS, HTTP 500, unsupported compression) from a
-- higher-priority cache must not mask a hit in the next - and the first
-- error, tagged with its cache URL, is reported only when no cache has the
-- path.
tryCachesWith :: (Monad m) => (CacheConfig -> m SubstResult) -> [CacheConfig] -> m SubstResult
tryCachesWith attempt = go Nothing
  where
    go firstErr [] = pure (maybe SubstNotFound SubstError firstErr)
    go firstErr (cache : rest) = do
      result <- attempt cache
      case result of
        SubstSuccess found -> pure (SubstSuccess found)
        SubstNotFound -> go firstErr rest
        SubstError err -> go (firstErr <|> Just (ccUrl cache <> ": " <> err)) rest

-- | Attempt substitution from a single cache, catching all exceptions.
tryOneCache :: HTTP.Manager -> Store -> CacheConfig -> StorePath -> IO SubstResult
tryOneCache mgr store cache sp = do
  result <- try (substituteFromCache mgr store cache sp)
  case result of
    Left (err :: SomeException) ->
      pure (SubstError ("substitution exception: " <> T.pack (show err)))
    Right substResult -> pure substResult

-- | Substitution pipeline for a single cache.
--
-- Each step is a pure or IO action that produces @Either@ on failure.
-- The pipeline short-circuits on the first error via early return.
substituteFromCache :: HTTP.Manager -> Store -> CacheConfig -> StorePath -> IO SubstResult
substituteFromCache mgr store cache sp = do
  -- 1. Fetch narinfo
  narInfoResult <- fetchNarInfo mgr cache sp
  case narInfoResult of
    Left notFoundOrErr -> pure notFoundOrErr
    Right narInfo
      -- 1b. The served narinfo must describe the requested path.  A
      -- misconfigured or hostile cache could return a validly-signed narinfo
      -- for a DIFFERENT path under this hash's URL.
      | not (narInfoMatchesPath sp narInfo) ->
          pure
            ( SubstError
                ( "narinfo identity mismatch: requested "
                    <> spHash sp
                    <> ", narinfo names "
                    <> NarInfo.niStorePath narInfo
                )
            )
      | otherwise ->
          -- 2-4. The pure preflight, then stream: download,
          -- decompress, hash, parse, and materialize in one bounded
          -- pass ('streamNarIntoStore').
          case narInfoPreflight cache narInfo of
            Left err -> pure (SubstError err)
            Right () -> streamWithRetry mgr store cache sp narInfo

-- | The pure preflight the pipeline runs before any download is paid
-- for, everything decided from the narinfo alone: field validation
-- FIRST - above all before 'verifySigs' builds the signed fingerprint
-- from the fields - then the signature, then streaming decompression
-- support.  Unsupported compression rejects here: the value is known
-- from the narinfo, and a multi-hundred-MB download that can only
-- fail in decompression is pure waste.  The strict 'decompressorFor'
-- encodes the same support set; the suite pins their agreement.
narInfoPreflight :: CacheConfig -> NarInfo.NarInfo -> Either Text ()
narInfoPreflight cache narInfo = do
  validateNarInfoFields narInfo
  verifySigs cache narInfo
  streamingDecompressionSupported (NarInfo.niCompression narInfo)

-- | Validate narinfo field syntax before anything consumes the fields -
-- above all before 'verifySigs' builds the signed fingerprint from them.
-- The fingerprint is delimited text (semicolons between fields, commas
-- between references), so each field must have parsed as well-formed
-- before it is spliced in; a malformed narinfo fails here as a plain
-- parse error instead of flowing onward.  The checks are nova-cache's
-- 'Validate.validateNarInfo': store path, references, hash spellings,
-- sizes, and the no-@.drv@ rule.
validateNarInfoFields :: NarInfo.NarInfo -> Either Text ()
validateNarInfoFields narInfo = case Validate.validateNarInfo narInfo of
  Right _ -> Right ()
  Left errs ->
    Left ("invalid narinfo: " <> T.intercalate "; " (map renderValidationError errs))

-- | One 'Validate.ValidationError' in the register the other
-- substitution errors use.
renderValidationError :: Validate.ValidationError -> Text
renderValidationError verr = case verr of
  Validate.NegativeFileSize n -> "negative FileSize " <> T.pack (show n)
  Validate.NegativeNarSize n -> "negative NarSize " <> T.pack (show n)
  Validate.InvalidStorePath raw parseErr -> fieldError "StorePath" raw parseErr
  Validate.InvalidFileHash raw parseErr -> fieldError "FileHash" raw parseErr
  Validate.InvalidNarHash raw parseErr -> fieldError "NarHash" raw parseErr
  Validate.InvalidReference raw parseErr -> fieldError "Reference" raw parseErr
  Validate.NarHashMismatch expected actual ->
    "NarHash mismatch: declared " <> expected <> ", computed " <> actual
  Validate.FileHashMismatch expected actual ->
    "FileHash mismatch: declared " <> expected <> ", computed " <> actual
  Validate.SignatureInvalid sig -> "signature failed verification: " <> sig
  Validate.NoSignatures -> "narinfo has no signatures"
  Validate.DerivationStorePath path -> "StorePath names a derivation: " <> path
  where
    fieldError field raw parseErr =
      field <> " '" <> raw <> "' does not parse: " <> T.pack parseErr

-- | Verify the NAR hash and size, deserialize, unpack to the store, and set
-- permissions.  Returns the path's registration for the caller to record;
-- no database write happens here (see 'SubstSuccess').  The strict
-- counterpart of the streaming pipeline, kept as its differential
-- oracle: the suite materializes the same NAR through both and
-- requires identical trees.
unpackAndVerify :: Store -> StorePath -> NarInfo.NarInfo -> BS.ByteString -> IO SubstResult
unpackAndVerify store sp narInfo rawNar =
  -- Verify the downloaded NAR's hash matches the (signed) narinfo BEFORE
  -- trusting its bytes.  The NAR hash is the content-addressed integrity
  -- contract; the signature only attests to the narinfo, not the body, so
  -- network corruption or a compromised cache must be caught here.
  -- Narinfo metadata is likewise parsed before any disk write: a malformed
  -- narinfo must not leave an unpacked-but-unregistered path behind.
  case verifiedInputs of
    Left err -> pure (SubstError err)
    Right (declared, (refs, deriver)) -> case NAR.deserialise rawNar of
      Left err -> pure (SubstError ("NAR deserialisation failed: " <> T.pack err))
      Right narEntry -> do
        let destPath = storePathToFilePath (stDir store) sp
        unpackResult <- try $ do
          clearStaleDestination destPath
          unpackNarEntry destPath narEntry
        case (unpackResult :: Either SomeException (Either Text ())) of
          Left err -> pure (SubstError ("unpack failed: " <> T.pack (show err)))
          Right (Left err) -> pure (SubstError ("unpack failed: " <> err))
          Right (Right ()) -> do
            setReadOnly destPath
            -- A path registered valid must match its recorded hash ON
            -- DISK, not merely in the downloaded bytes: any divergence
            -- the filesystem introduced between the NAR and the
            -- materialized tree (name folding, link replication) must
            -- surface here, before the row exists.  A mismatching tree
            -- is removed - left in place it would be adopted by
            -- existence checks at this path.
            onDisk <- NAR.serialiseFromPath destPath
            case verifyNarHash narInfo (NAR.serialise onDisk) of
              Left _ -> do
                Dir.removePathForcibly destPath
                pure
                  ( SubstError
                      ( "unpacked tree does not reproduce the declared NAR hash at "
                          <> T.pack destPath
                      )
                  )
              Right _ ->
                pure $
                  SubstSuccess
                    PathRegistration
                      { prPath = sp,
                        -- The canonical spelling of the verified digest,
                        -- so the DB converges on one hash spelling
                        -- regardless of the cache's.
                        prNarHash = Hash.formatNixHash declared,
                        -- The verified actual byte count (equal to the declared
                        -- NarSize per 'verifyNarSize') - no Integer conversion
                        -- that could wrap.
                        prNarSize = BS.length rawNar,
                        prDeriver = deriver,
                        prReferences = refs
                      }
  where
    verifiedInputs = do
      declared <- verifyNarHash narInfo rawNar
      verifyNarSize narInfo rawNar
      meta <- registrationMeta
      pure (declared, meta)
    registrationMeta = do
      refs <- parseReferences (NarInfo.niReferences narInfo)
      deriver <- parseDeriver (stDir store) (NarInfo.niDeriver narInfo)
      pure (refs, deriver)

-- | Verify that the downloaded NAR bytes hash to the narinfo's declared
-- NarHash, returning the decoded digest on success so registration can
-- record its canonical spelling.  Compares decoded hash bytes, so any
-- valid encoding of the digest validates.
verifyNarHash :: NarInfo.NarInfo -> BS.ByteString -> Either Text Hash.NixHash
verifyNarHash narInfo rawNar =
  case Hash.parseNixHash (NarInfo.niNarHash narInfo) of
    Left err -> Left ("invalid narinfo NarHash: " <> T.pack err)
    Right declared
      | declared == actual -> Right declared
      | otherwise ->
          Left
            ( "NAR hash mismatch: narinfo declares "
                <> NarInfo.niNarHash narInfo
                <> " but downloaded bytes hash to "
                <> Hash.formatNixHash actual
            )
  where
    actual = Hash.hashBytes rawNar

-- | Verify that the downloaded NAR's byte count equals the narinfo's
-- declared NarSize.  The hash check pins the content, but the size is a
-- separate signed claim that flows into the store DB (and from there
-- into re-pushed narinfos), so a wrong declaration must be rejected
-- rather than recorded.
verifyNarSize :: NarInfo.NarInfo -> BS.ByteString -> Either Text ()
verifyNarSize narInfo rawNar
  | toInteger (BS.length rawNar) == NarInfo.niNarSize narInfo = Right ()
  | otherwise =
      Left
        ( "NAR size mismatch: narinfo declares "
            <> T.pack (show (NarInfo.niNarSize narInfo))
            <> " bytes but downloaded "
            <> T.pack (show (BS.length rawNar))
        )

-- | Whether a served narinfo describes the requested path: the hash component
-- of its declared StorePath must equal the requested path's hash.  Only the
-- hash is compared, since the cache may use a different store directory.
narInfoMatchesPath :: StorePath -> NarInfo.NarInfo -> Bool
narInfoMatchesPath sp narInfo =
  storePathHashOf (NarInfo.niStorePath narInfo) == Just (spHash sp)

-- | Extract the leading hash from a full store path's basename, if well-formed
-- (@\<hash\>-\<name\>@ with a hash of the expected length).
storePathHashOf :: Text -> Maybe Text
storePathHashOf path =
  let base = T.takeWhileEnd (\c -> c /= '/' && c /= '\\') path
      (hashPart, rest) = T.splitAt storePathHashLen base
   in if T.length hashPart == storePathHashLen && T.isPrefixOf "-" rest
        then Just hashPart
        else Nothing

-- ---------------------------------------------------------------------------
-- HTTP fetching
-- ---------------------------------------------------------------------------

-- | HTTP status code constants.
httpOk :: Int
httpOk = 200

httpNotFound :: Int
httpNotFound = 404

-- | Cap on a narinfo response body, mirroring nova-cache's server-side
-- @maxNarInfoBodySize@ - the server bounds what it reads, and the
-- client bounds what any cache in its list can make it buffer.
-- Narinfo is small key-value text; 4 MB is far beyond any real one.
maxNarInfoBody :: Int
maxNarInfoBody = 4 * 1024 * 1024

-- | Read an HTTP response body in bounded chunks up to a byte cap -
-- 'Nothing' once the cap is exceeded, so an over-large body aborts
-- mid-stream instead of buffering without limit.  The client-side
-- mirror of nova-cache's @readBodyLimited@.
readBodyCapped :: Int -> HTTP.BodyReader -> IO (Maybe BS.ByteString)
readBodyCapped cap reader = go [] 0
  where
    go chunks !total = do
      chunk <- HTTP.brRead reader
      if BS.null chunk
        then pure (Just (BS.concat (reverse chunks)))
        else
          let newTotal = total + BS.length chunk
           in if newTotal > cap
                then pure Nothing
                else go (chunk : chunks) newTotal

-- | Fetch a narinfo from a cache.
-- Returns @Left SubstNotFound@ on 404, @Left (SubstError msg)@ on other errors.
fetchNarInfo :: HTTP.Manager -> CacheConfig -> StorePath -> IO (Either SubstResult NarInfo.NarInfo)
fetchNarInfo mgr cache sp = do
  let url = T.unpack (ccUrl cache) <> "/" <> T.unpack (spHash sp) <> ".narinfo"
  request <- HTTP.parseRequest url
  HTTP.withResponse request mgr $ \response -> do
    let code = HTTP.statusCode (HTTP.responseStatus response)
    -- Lenient decode: the body is cache-controlled bytes, and a stray
    -- invalid UTF-8 sequence must surface as a narinfo parse error, not an
    -- impure UnicodeException (the push side decodes the same way).
    if code == httpOk
      then do
        body <- readBodyCapped maxNarInfoBody (HTTP.responseBody response)
        case body of
          Nothing ->
            pure (Left (SubstError ("narinfo body exceeds " <> T.pack (show maxNarInfoBody) <> " bytes")))
          Just bytes -> case NarInfo.parseNarInfo (TE.decodeUtf8Lenient bytes) of
            Left err -> pure (Left (SubstError ("narinfo parse error: " <> T.pack err)))
            Right ni -> pure (Right ni)
      else
        if code == httpNotFound
          then pure (Left SubstNotFound)
          else pure (Left (SubstError ("narinfo fetch failed: HTTP " <> T.pack (show code))))

-- | How many times to attempt a NAR download before giving up and letting the
-- caller fall back to a local build.  Matches Nix's @download-attempts@ default.
narDownloadAttempts :: Int
narDownloadAttempts = 5

-- | Base delay between NAR download attempts, in microseconds.  The delay grows
-- linearly with each retry (0.5s, 1s, ...).
narRetryBaseDelayMicros :: Int
narRetryBaseDelayMicros = 500000

-- | Stream one substitution end to end, retrying transient failures.
--
-- By the time this runs the narinfo has already been fetched and
-- signature-verified, so the cache claims to hold this path.  Failures
-- carry their own retry class: a 'TransientFailure' - transport
-- errors, torn or truncated transfers, anything a fresh attempt could
-- plausibly complete - consumes retry budget with linear backoff,
-- while a 'FatalFailure' - a completed transfer that verifies wrong,
-- a body past its signed ceiling, a 4xx - ends the attempt at once:
-- retrying a deterministic failure only delays the local-build
-- fallback, and upstream's transfer layer likewise retries only the
-- transport class.  Every attempt starts from a clean slate
-- ('streamNarIntoStore' clears the destination first, and every
-- failure path, exceptions included, removes what it wrote).  A 404
-- on the narinfo itself (a genuine cache miss) is handled earlier in
-- 'fetchNarInfo' and never reaches here.
streamWithRetry :: HTTP.Manager -> Store -> CacheConfig -> StorePath -> NarInfo.NarInfo -> IO SubstResult
streamWithRetry mgr store cache sp narInfo = attempt narDownloadAttempts
  where
    attempt remaining = do
      outcome <- streamNarIntoStore mgr cache store sp narInfo
      case outcome of
        Right registration -> pure (SubstSuccess registration)
        Left (FatalFailure err) -> pure (SubstError err)
        Left (TransientFailure err)
          | remaining <= 1 -> pure (SubstError err)
          | otherwise -> do
              threadDelay (narRetryBaseDelayMicros * (narDownloadAttempts - remaining + 1))
              attempt (remaining - 1)

-- | How one streaming attempt failed, deciding whether the retry
-- budget applies.  'TransientFailure' is a failure a fresh attempt
-- could plausibly complete; 'FatalFailure' is deterministic - the
-- same served object fails the same way every time.
data AttemptFailure
  = TransientFailure !Text
  | FatalFailure !Text
  deriving (Eq, Show)

-- | The failure's message, independent of its retry class.
attemptFailureMessage :: AttemptFailure -> Text
attemptFailureMessage failure = case failure of
  TransientFailure msg -> msg
  FatalFailure msg -> msg

-- | Thrown inside the streaming pipeline where a chunk convention has
-- no error channel (the capped body source); converted back to the
-- pipeline's 'Left' at the attempt boundary in 'streamNarIntoStore'.
newtype StreamAbort = StreamAbort Text
  deriving (Show)

instance Exception StreamAbort

-- | Convert synchronous exceptions from one download attempt into the
-- pipeline's failure channel, so the retry budget governs them and the
-- caller's cleanup contract holds on every exit.  Asynchronous
-- exceptions (cancellation) re-throw untouched: an interrupt must
-- never be spent as retry budget.
tryAttempt :: IO (Either AttemptFailure a) -> IO (Either AttemptFailure a)
tryAttempt action = action `catch` handler
  where
    handler someErr
      | Just (SomeAsyncException _) <- fromException someErr = throwIO someErr
      | Just (StreamAbort msg) <- fromException someErr =
          -- A body past its ceiling: the cap derives from the signed
          -- NarSize, so a longer body is the server misdeclaring, not
          -- a hiccup.
          pure (Left (FatalFailure msg))
      | Just (httpErr :: HTTP.HttpException) <- fromException someErr =
          -- Dropped connections, resets, timeouts - the class the
          -- retry budget exists for.
          pure (Left (TransientFailure ("HTTP transport failure: " <> T.pack (show httpErr))))
      | otherwise =
          -- Local failures (a full disk, a permission error) do not
          -- heal by re-downloading.
          pure (Left (FatalFailure ("substitution attempt failed: " <> T.pack (show (someErr :: SomeException)))))

-- | HTTP status codes whose failures a retry could plausibly outlive.
httpRequestTimeout :: Int
httpRequestTimeout = 408

httpTooManyRequests :: Int
httpTooManyRequests = 429

httpServerErrorFloor :: Int
httpServerErrorFloor = 500

-- | Classify a non-200 NAR response: server-side and rate-limit
-- statuses are transient, every other status (above all a 404 on an
-- object the narinfo just promised) is deterministic.
httpStatusFailure :: Int -> AttemptFailure
httpStatusFailure code
  | code == httpRequestTimeout || code == httpTooManyRequests || code >= httpServerErrorFloor =
      TransientFailure message
  | otherwise = FatalFailure message
  where
    message = "NAR download failed: HTTP " <> T.pack (show code)

-- | One streaming substitution attempt: download, decompress, hash,
-- parse, and materialize in a single bounded pass, then verify.
-- Memory is bounded by the decoder's buffers and the parser's
-- structural-string cap, never by archive or file size.
--
-- Disk writes begin before the NAR hash can be known - the price of
-- never holding the archive, and exactly upstream's ordering - so
-- every failure path, exceptions included, removes the tree it wrote
-- ('materializeNarFromSource' owns that contract; 'tryAttempt'
-- classifies what escapes it).  The narinfo metadata registration
-- needs (declared hash, references, deriver) still parses BEFORE the
-- first byte downloads: a malformed narinfo must not leave an
-- unpacked-but-unregistered path behind.
streamNarIntoStore :: HTTP.Manager -> CacheConfig -> Store -> StorePath -> NarInfo.NarInfo -> IO (Either AttemptFailure PathRegistration)
streamNarIntoStore mgr cache store sp narInfo = case preflight of
  Left err -> pure (Left (FatalFailure err))
  Right (declaredDigest, refs, deriver, downloadCap) -> do
    let destPath = storePathToFilePath (stDir store) sp
        narUrl = T.unpack (ccUrl cache) <> "/" <> T.unpack (NarInfo.niUrl narInfo)
    clearStaleDestination destPath
    request <- HTTP.parseRequest narUrl
    tryAttempt $ HTTP.withResponse request mgr $ \response -> do
      let code = HTTP.statusCode (HTTP.responseStatus response)
      if code /= httpOk
        then pure (Left (httpStatusFailure code))
        else do
          source <- cappedBodySource downloadCap (HTTP.responseBody response)
          materializeNarFromSource store sp narInfo declaredDigest refs deriver source
  where
    preflight = do
      declaredDigest <- case Hash.parseNixHash (NarInfo.niNarHash narInfo) of
        Left err -> Left ("invalid narinfo NarHash: " <> T.pack err)
        Right digest -> Right digest
      refs <- parseReferences (NarInfo.niReferences narInfo)
      deriver <- parseDeriver (stDir store) (NarInfo.niDeriver narInfo)
      downloadCap <- downloadCapFor narInfo
      pure (declaredDigest, refs, deriver, downloadCap)

-- | The byte cap on the compressed NAR body, derived from the SIGNED
-- NarSize: the Ed25519 fingerprint covers StorePath, NarHash, NarSize,
-- and references - not FileSize - so the unsigned FileSize may only
-- LOWER the cap, never raise it.  A rewritten FileSize on an otherwise
-- validly-signed narinfo must not buy an unbounded download.
downloadCapFor :: NarInfo.NarInfo -> Either Text Int
downloadCapFor narInfo
  | narSize < 0 || narSize > toInteger (maxBound :: Int) =
      Left ("narinfo declares an unusable NAR size: " <> T.pack (show narSize))
  | otherwise =
      let signedCeiling = compressedBodyCeiling narSize
          capped = maybe signedCeiling (min signedCeiling . max 0) (NarInfo.niFileSize narInfo)
       in Right (fromInteger (min capped (toInteger (maxBound :: Int))))
  where
    narSize = NarInfo.niNarSize narInfo

-- | The most a compressed NAR body may legitimately exceed its NarSize
-- by: xz and zstd expand incompressible input by well under one
-- percent of framing overhead, so a 1/64 (~1.6%) margin plus a fixed
-- floor for small NARs is generous for any real codec, while a
-- hostile FileSize claiming orders of magnitude more is refused.
compressedBodyCeiling :: Integer -> Integer
compressedBodyCeiling narSize =
  narSize + max compressionOverheadFloorBytes (narSize `div` compressionOverheadDivisor)

-- | Fixed overhead floor for small NARs, where framing dominates.
compressionOverheadFloorBytes :: Integer
compressionOverheadFloorBytes = 64 * 1024

-- | Proportional overhead margin: 1/64 of the NarSize.
compressionOverheadDivisor :: Integer
compressionOverheadDivisor = 64

-- | Materialize a NAR from a compressed chunk source into the store
-- path: decompress, hash, parse, and unpack in one pass, then verify
-- the tree on disk and build its registration.  The post-download half
-- of 'streamNarIntoStore', taking a plain chunk source so the failure
-- contract is testable without HTTP.
--
-- The cleanup contract: every failing exit - the pipeline's 'Left',
-- a verification mismatch, or an exception (asynchronous included) -
-- removes the destination tree, so no partial or unverified tree ever
-- survives at the store path.
materializeNarFromSource :: Store -> StorePath -> NarInfo.NarInfo -> Hash.NixHash -> [StorePath] -> Maybe Text -> IO BS.ByteString -> IO (Either AttemptFailure PathRegistration)
materializeNarFromSource store sp narInfo declaredDigest refs deriver source =
  materialize `onException` Dir.removePathForcibly destPath
  where
    destPath = storePathToFilePath (stDir store) sp
    materialize = do
      streamed <-
        withDecompressedSource (NarInfo.niNarSize narInfo) (NarInfo.niCompression narInfo) source $
          consumeNarStream destPath narInfo declaredDigest
      case streamed of
        Left err -> do
          Dir.removePathForcibly destPath
          pure (Left err)
        Right narByteCount -> do
          setReadOnly destPath
          -- A path registered valid must match its recorded hash ON
          -- DISK, not merely in the streamed bytes: any divergence the
          -- filesystem introduced between the NAR and the materialized
          -- tree must surface here, before the row exists.  The recheck
          -- streams too, so its memory no longer scales with the path.
          onDiskDigest <- hashPathStreaming destPath
          if onDiskDigest /= declaredDigest
            then do
              Dir.removePathForcibly destPath
              pure (Left (FatalFailure ("unpacked tree does not reproduce the declared NAR hash at " <> T.pack destPath)))
            else
              pure $
                Right
                  PathRegistration
                    { prPath = sp,
                      -- The canonical spelling of the verified digest,
                      -- so the DB converges on one hash spelling
                      -- regardless of the cache's.
                      prNarHash = Hash.formatNixHash declaredDigest,
                      prNarSize = narByteCount,
                      prDeriver = deriver,
                      prReferences = refs
                    }

-- | Drive the decompressed chunk source through incremental hashing,
-- the streaming NAR parser, and the store's streaming unpack sink,
-- returning the verified NAR byte count.  The hash context folds over
-- exactly the bytes the parser consumes, so the digest is of the NAR
-- the tree was built from.
consumeNarStream :: FilePath -> NarInfo.NarInfo -> Hash.NixHash -> IO BS.ByteString -> IO (Either AttemptFailure Int)
consumeNarStream destPath narInfo declaredDigest narSource = do
  sink <- newNarUnpackSink destPath
  go sink Hash.hashInit 0 Stream.narStream `onException` abortNarUnpack sink
  where
    go sink !ctx !narBytes step = case step of
      Stream.NarAwait continue -> do
        chunk <- narSource
        go sink (Hash.hashUpdate ctx chunk) (narBytes + BS.length chunk) (continue chunk)
      Stream.NarYield event next -> do
        sunk <- sinkNarEvent sink event
        case sunk of
          Left err -> do
            abortNarUnpack sink
            -- A name or shape the store refuses is a property of the
            -- archive, not of this transfer.
            pure (Left (FatalFailure err))
          Right () -> go sink ctx narBytes next
      Stream.NarFail msg -> do
        abortNarUnpack sink
        -- A truncated body and a torn transfer parse-fail the same
        -- way, so the retry budget applies.
        pure (Left (TransientFailure ("NAR stream parse failed: " <> T.pack msg)))
      Stream.NarDone -> do
        let digest = Hash.hashFinalize ctx
        if toInteger narBytes /= NarInfo.niNarSize narInfo
          then do
            abortNarUnpack sink
            -- The grammar completed, so the transfer was whole: a
            -- size that still disagrees is the narinfo misdeclaring.
            pure
              ( Left
                  ( FatalFailure
                      ( "NAR size mismatch: narinfo declares "
                          <> T.pack (show (NarInfo.niNarSize narInfo))
                          <> " bytes but the stream carried "
                          <> T.pack (show narBytes)
                      )
                  )
              )
          else
            if digest /= declaredDigest
              then do
                abortNarUnpack sink
                -- Size matched, so the transfer completed; wrong
                -- bytes are deterministic corruption, not a hiccup.
                pure
                  ( Left
                      ( FatalFailure
                          ( "NAR hash mismatch: narinfo declares "
                              <> NarInfo.niNarHash narInfo
                              <> " but downloaded bytes hash to "
                              <> Hash.formatNixHash digest
                          )
                      )
                  )
              else do
                finished <- finishNarUnpack sink
                case finished of
                  Left err -> pure (Left (FatalFailure err))
                  Right () -> pure (Right narBytes)

-- | Stream an HTTP body as a chunk source bounded by the download cap
-- 'downloadCapFor' derived from the signed NarSize -
-- 'readBodyCapped''s discipline without the buffering.  Exceeding the
-- cap throws 'StreamAbort'; the attempt boundary converts it back to
-- the pipeline's error channel.
cappedBodySource :: Int -> HTTP.BodyReader -> IO (IO BS.ByteString)
cappedBodySource cap reader = do
  countRef <- newIORef 0
  pure $ do
    chunk <- HTTP.brRead reader
    consumed <- readIORef countRef
    let total = consumed + BS.length chunk
    if total > cap
      then throwIO (StreamAbort ("NAR body exceeds its download ceiling (" <> T.pack (show cap) <> " bytes)"))
      else do
        writeIORef countRef total
        pure chunk

-- | Hash a store path's NAR serialisation without materializing it:
-- 'NAR.withNarSource' streams the tree and the digest folds over the
-- chunks.
hashPathStreaming :: FilePath -> IO Hash.NixHash
hashPathStreaming path = NAR.withNarSource NAR.defaultCaseHack path $ \pull ->
  let go !ctx = do
        chunk <- pull
        if BS.null chunk
          then pure (Hash.hashFinalize ctx)
          else go (Hash.hashUpdate ctx chunk)
   in go Hash.hashInit

-- | Whether the streaming pipeline can decompress a narinfo
-- @Compression@ value, decided from the value alone so unsupported
-- compression rejects before any download.  Both this and the strict
-- 'decompressorFor' dispatch through 'parseNarCompression', so the
-- support set exists exactly once.
streamingDecompressionSupported :: Text -> Either Text ()
streamingDecompressionSupported = void . parseNarCompression

-- | Run a consumer over the decompressed view of a chunk source:
-- identity for 'CompressionNone', nova-cache's bounded decoder for
-- 'CompressionXz' (output capped at the declared NarSize; thrown
-- 'Xz.XzError's convert to the pipeline's error channel here, carrying
-- their retry class from 'xzFailure').
withDecompressedSource :: Integer -> Text -> IO BS.ByteString -> (IO BS.ByteString -> IO (Either AttemptFailure a)) -> IO (Either AttemptFailure a)
withDecompressedSource declaredNarSize compression source consume =
  case parseNarCompression compression of
    Left err -> pure (Left (FatalFailure err))
    Right CompressionNone -> consume source
    Right CompressionXz -> case xzLimitsFor declaredNarSize of
      Left err -> pure (Left (FatalFailure err))
      Right limits ->
        Xz.withXzSource limits source consume
          `catch` \xzErr -> pure (Left (xzFailure xzErr))

-- | Classify a decoder failure: a stream error is how truncation and
-- torn transfers surface, so it retries; output or memory past the
-- declared bounds is the served object misdeclaring - deterministic.
xzFailure :: Xz.XzError -> AttemptFailure
xzFailure xzErr = case xzErr of
  Xz.XzStreamError _ -> TransientFailure rendered
  Xz.XzOutputOverBound _ -> FatalFailure rendered
  Xz.XzMemoryOverBound _ -> FatalFailure rendered
  where
    rendered = renderXzError xzErr

-- ---------------------------------------------------------------------------
-- Pure helpers
-- ---------------------------------------------------------------------------

-- | Sort caches by priority (lower = first).
sortCaches :: [CacheConfig] -> [CacheConfig]
sortCaches = sortBy (comparing ccPriority)

-- | Verify narinfo signatures against the cache's trusted public key.
-- At least one signature must match.
verifySigs :: CacheConfig -> NarInfo.NarInfo -> Either Text ()
verifySigs cache narInfo =
  case Signing.parsePublicKey (ccPublicKey cache) of
    Left err -> Left ("invalid public key: " <> T.pack err)
    Right pubKey ->
      let sigs = NarInfo.niSigs narInfo
       in if null sigs
            then Left "narinfo has no signatures"
            else
              if any (Signing.verify pubKey narInfo) sigs
                then Right ()
                else Left "no valid signature found"

-- | The decompressor for a narinfo @Compression@ value, decided from
-- the narinfo's declared values alone so unsupported compression
-- rejects before any download.  'CompressionNone' is identity;
-- 'CompressionXz' - cache.nixos.org's format - decompresses bounded
-- by the declared NarSize, a signed claim the caller validates before
-- resolving the decompressor.  Dispatches through
-- 'parseNarCompression' like the streaming path, so the support set
-- exists exactly once.
decompressorFor :: Integer -> Text -> Either Text (BS.ByteString -> Either Text BS.ByteString)
decompressorFor declaredNarSize compression = do
  kind <- parseNarCompression compression
  case kind of
    CompressionNone -> Right Right
    CompressionXz -> xzDecompressor declaredNarSize

-- | Decompress NAR data based on the compression type from narinfo,
-- bounded by the declared NarSize.  Support is decided by
-- 'decompressorFor'; this applies the result.
decompressNar :: Integer -> Text -> BS.ByteString -> Either Text BS.ByteString
decompressNar declaredNarSize compression narData =
  decompressorFor declaredNarSize compression >>= ($ narData)

-- | The bounds for one xz decode: output capped at the narinfo's
-- declared NarSize, decoder memory at nova-cache's default, so a
-- hostile stream can expand to neither more output nor more decoder
-- state than the narinfo promised.  Narinfo validation upstream
-- already rejected a negative size; the guard keeps the function
-- total for direct callers, and a size past Word64 cannot name a
-- real NAR.
xzLimitsFor :: Integer -> Either Text Xz.XzLimits
xzLimitsFor declaredNarSize
  | declaredNarSize < 0 || declaredNarSize > toInteger (maxBound :: Word64) =
      Left ("xz decompression bound out of range: " <> T.pack (show declaredNarSize))
  | otherwise =
      Right
        Xz.XzLimits
          { Xz.xzMaxOutputBytes = fromInteger declaredNarSize,
            Xz.xzMaxDecoderMemoryBytes = Xz.defaultXzDecoderMemoryBytes
          }

-- | The whole-buffer bounded xz decompressor for a narinfo's declared
-- NarSize - the strict counterpart of 'withDecompressedSource''s
-- streaming path, sharing its bounds through 'xzLimitsFor'.
xzDecompressor :: Integer -> Either Text (BS.ByteString -> Either Text BS.ByteString)
xzDecompressor declaredNarSize = do
  limits <- xzLimitsFor declaredNarSize
  Right (either (Left . renderXzError) Right . Xz.decompress limits)

-- | One 'Xz.XzError' in the register the other substitution errors use.
renderXzError :: Xz.XzError -> Text
renderXzError xzErr = case xzErr of
  Xz.XzStreamError msg -> "xz stream error: " <> T.pack msg
  Xz.XzOutputOverBound bound ->
    "xz output exceeds the declared NarSize (" <> T.pack (show bound) <> " bytes)"
  Xz.XzMemoryOverBound bound ->
    "xz decoder memory over its cap (" <> T.pack (show bound) <> " bytes)"

-- | Parse narinfo references (store path basenames, e.g.
-- @abc...-glibc-2.40@) into StorePaths.  A malformed token is an error,
-- not filtered: silently dropping a reference registers the path with a
-- hole in its closure, which re-push then publishes as a signed narinfo
-- missing runtime deps, and GC reads as permission to delete them.
parseReferences :: [Text] -> Either Text [StorePath]
parseReferences = traverse parseRef
  where
    parseRef ref =
      maybe (Left ("malformed narinfo reference: " <> ref)) Right (parseStorePathBaseName ref)

-- | The sentinel upstream caches emit for a path whose deriver is unknown.
unknownDeriverSentinel :: Text
unknownDeriverSentinel = "unknown-deriver"

-- | Parse a narinfo Deriver - a store path basename on the wire, or the
-- @unknown-deriver@ sentinel - into the full path text the store DB
-- records (the form 'Nix.Push' parses back with 'parseStorePath').
parseDeriver :: StoreDir -> Maybe Text -> Either Text (Maybe Text)
parseDeriver _ Nothing = Right Nothing
parseDeriver storeDir (Just txt)
  | txt == unknownDeriverSentinel = Right Nothing
  | otherwise = case parseStorePathBaseName txt of
      Just sp -> Right (Just (T.pack (storePathToFilePath storeDir sp)))
      Nothing -> Left ("malformed narinfo deriver: " <> txt)

-- ---------------------------------------------------------------------------
-- NAR unpacking
-- ---------------------------------------------------------------------------

-- | Remove a leftover destination tree before unpacking.  A crash (or a
-- failed registration) between 'setReadOnly' here and the caller's
-- 'Nix.Store.DB.registerPaths' leaves a read-only, unregistered tree;
-- unpacking over it would then fail with permission-denied on every
-- retry, permanently wedging substitution of that path.
-- 'Dir.removePathForcibly' clears read-only marks and accepts a missing
-- path, so the fresh unpack always starts from a clean slate.
clearStaleDestination :: FilePath -> IO ()
clearStaleDestination = Dir.removePathForcibly

-- unpackNarEntry lives in 'Nix.Store' (one tree-materializer for the
-- codebase) and is re-exported here for its historical callers and tests.
