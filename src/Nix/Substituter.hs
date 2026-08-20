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
-- 5. @GET https:\/\/cache.example.com\/nar\/\<narhash\>.nar.xz@
-- 6. Decompress, verify NAR hash, unpack into store path
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

    -- * Pure helpers (exported for testing)
    maxNarInfoBody,
    readBodyCapped,
    sortCaches,
    tryCachesWith,
    validateNarInfoFields,
    verifyAndDecompress,
    verifySigs,
    verifyNarHash,
    verifyNarSize,
    narInfoMatchesPath,
    decompressorFor,
    decompressNar,
    unpackNarEntry,
    unpackAndVerify,
    clearStaleDestination,
    parseReferences,
    parseDeriver,
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import qualified Data.ByteString as BS
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word64)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS
import qualified Network.HTTP.Types.Status as HTTP
import Nix.Store (Store (..), setReadOnly, unpackNarEntry)
import Nix.Store.DB (PathRegistration (..))
import Nix.Store.Path (StoreDir, StorePath (spHash), parseStorePathBaseName, storePathHashLen, storePathToFilePath)
import qualified NovaCache.Hash as Hash
import qualified NovaCache.NAR as NAR
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
          -- 2-4. Verify, download, decompress (pure pipeline after fetch)
          case verifyAndDecompress cache mgr narInfo of
            Left err -> pure (SubstError err)
            Right fetchDecompress -> do
              narBytes <- fetchDecompress
              case narBytes of
                Left err -> pure (SubstError err)
                Right rawNar -> unpackAndVerify store sp narInfo rawNar

-- | Pure pipeline: validate the narinfo's fields, verify the signature,
-- and resolve the decompressor, then produce an IO action that downloads
-- and decompresses.  Unsupported
-- compression rejects HERE, before the action runs: the value is known
-- from the narinfo, and a multi-hundred-MB download that can only fail
-- in decompression is pure waste.
verifyAndDecompress ::
  CacheConfig ->
  HTTP.Manager ->
  NarInfo.NarInfo ->
  Either Text (IO (Either Text BS.ByteString))
verifyAndDecompress cache mgr narInfo = do
  validateNarInfoFields narInfo
  verifySigs cache narInfo
  decompress <- decompressorFor (NarInfo.niNarSize narInfo) (NarInfo.niCompression narInfo)
  pure $ do
    downloaded <- downloadNarWithRetry mgr cache narInfo
    pure $ downloaded >>= decompress

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
-- no database write happens here (see 'SubstSuccess').
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

-- | Download a NAR, retrying transient failures.
--
-- By the time this runs the narinfo has already been fetched and signature-
-- verified, so the cache claims to hold this path: a failed blob fetch (a
-- transient HTTP error, a stale-negative at a CDN edge, or a dropped
-- connection) is far more likely a hiccup than a real miss.  Retrying a few
-- times is much cheaper than the local rebuild a hard failure forces.  A 404
-- on the narinfo itself (a genuine cache miss) is handled earlier in
-- 'fetchNarInfo' and never reaches here.
downloadNarWithRetry :: HTTP.Manager -> CacheConfig -> NarInfo.NarInfo -> IO (Either Text BS.ByteString)
downloadNarWithRetry mgr cache narInfo = attempt narDownloadAttempts
  where
    attempt remaining = do
      outcome <- try (downloadNar mgr cache narInfo)
      case outcome of
        Right (Right bytes) -> pure (Right bytes)
        Right (Left err) -> retryOr err remaining
        Left (e :: SomeException) -> retryOr ("NAR download error: " <> T.pack (show e)) remaining
    retryOr err remaining
      | remaining <= 1 = pure (Left err)
      | otherwise = do
          threadDelay (narRetryBaseDelayMicros * (narDownloadAttempts - remaining + 1))
          attempt (remaining - 1)

-- | Download the NAR file referenced by a narinfo.
--
-- The whole NAR is realized in memory (nova-cache's 'NAR.deserialise' consumes
-- a strict 'BS.ByteString'), but never more of it than the narinfo declares:
-- the narinfo was signature-verified before this runs, so its FileSize /
-- NarSize is the key-trusted bound, and a body that exceeds it aborts
-- mid-stream instead of buffering without limit - the excess bytes could not
-- hash-verify anyway.  Streaming the verify itself would need a streaming NAR
-- parser that nova-cache does not yet provide.
downloadNar :: HTTP.Manager -> CacheConfig -> NarInfo.NarInfo -> IO (Either Text BS.ByteString)
downloadNar mgr cache narInfo = do
  let narUrl = T.unpack (ccUrl cache) <> "/" <> T.unpack (NarInfo.niUrl narInfo)
      declared = fromMaybe (NarInfo.niNarSize narInfo) (NarInfo.niFileSize narInfo)
  if declared < 0 || declared > toInteger (maxBound :: Int)
    then pure (Left ("narinfo declares an unusable NAR size: " <> T.pack (show declared)))
    else do
      request <- HTTP.parseRequest narUrl
      HTTP.withResponse request mgr $ \response -> do
        let code = HTTP.statusCode (HTTP.responseStatus response)
        if code == httpOk
          then do
            body <- readBodyCapped (fromInteger declared) (HTTP.responseBody response)
            case body of
              Nothing ->
                pure (Left ("NAR body exceeds the declared size (" <> T.pack (show declared) <> " bytes)"))
              Just bytes -> pure (Right bytes)
          else pure (Left ("NAR download failed: HTTP " <> T.pack (show code)))

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
-- rejects before any download.  @\"none\"@ and @\"\"@ are identity.
-- @\"xz\"@ - cache.nixos.org's format - decompresses bounded by the
-- declared NarSize, a signed claim the caller validates before
-- resolving the decompressor.
decompressorFor :: Integer -> Text -> Either Text (BS.ByteString -> Either Text BS.ByteString)
decompressorFor declaredNarSize compression
  | compression == "none" || T.null compression = Right Right
  | compression == "xz" = xzDecompressor declaredNarSize
  | otherwise = Left ("unsupported compression: " <> compression)

-- | Decompress NAR data based on the compression type from narinfo,
-- bounded by the declared NarSize.  Support is decided by
-- 'decompressorFor'; this applies the result.
decompressNar :: Integer -> Text -> BS.ByteString -> Either Text BS.ByteString
decompressNar declaredNarSize compression narData =
  decompressorFor declaredNarSize compression >>= ($ narData)

-- | The bounded xz decompressor for a narinfo's declared NarSize,
-- nova-cache's decoder underneath: output is capped at the declared
-- size and decoder memory at nova-cache's default, so a hostile
-- stream can expand to neither more output nor more decoder state
-- than the narinfo promised.  Narinfo validation upstream already
-- rejected a negative size; the guard keeps the function total for
-- direct callers, and a size past Word64 cannot name a real NAR.
xzDecompressor :: Integer -> Either Text (BS.ByteString -> Either Text BS.ByteString)
xzDecompressor declaredNarSize
  | declaredNarSize < 0 || declaredNarSize > toInteger (maxBound :: Word64) =
      Left ("xz decompression bound out of range: " <> T.pack (show declaredNarSize))
  | otherwise = Right (either (Left . renderXzError) Right . Xz.decompress limits)
  where
    limits =
      Xz.XzLimits
        { Xz.xzMaxOutputBytes = fromInteger declaredNarSize,
          Xz.xzMaxDecoderMemoryBytes = Xz.defaultXzDecoderMemoryBytes
        }

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
