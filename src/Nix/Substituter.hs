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
-- 4. Verify the signature against a trusted public key
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
    sortCaches,
    verifySigs,
    verifyNarHash,
    narInfoMatchesPath,
    decompressNar,
    unpackNarEntry,
    parseReferences,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS
import qualified Network.HTTP.Types.Status as HTTP
import Nix.Store (Store (..), setReadOnly)
import Nix.Store.DB (PathRegistration (..), registerPath)
import Nix.Store.Path (StoreDir, StorePath (..), parseStorePath, storePathHashLen, storePathToFilePath)
import qualified NovaCache.Hash as Hash
import qualified NovaCache.NAR as NAR
import qualified NovaCache.NarInfo as NarInfo
import qualified NovaCache.Signing as Signing
import System.Directory (createDirectoryIfMissing, setPermissions)
import qualified System.Directory as Dir
import System.FilePath (takeDirectory, (</>))

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
  = -- | Successfully substituted - path is now in the store.
    SubstSuccess !StorePath
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
-- Checks each cache in priority order.  Returns on first success.
-- Uses nova-cache library for narinfo parsing, NAR unpacking,
-- and signature verification.
trySubstitute :: Store -> [CacheConfig] -> StorePath -> IO SubstResult
trySubstitute _ [] _ = pure SubstNotFound
trySubstitute store caches sp = do
  -- Reuse the process-global TLS manager (connection pooling / keep-alive)
  -- rather than creating a fresh one per call and per output.
  manager <- HTTPS.getGlobalManager
  tryCaches manager (sortCaches caches) sp
  where
    tryCaches _ [] _ = pure SubstNotFound
    tryCaches mgr (cache : rest) storePath = do
      result <- tryOneCache mgr store cache storePath
      case result of
        SubstNotFound -> tryCaches mgr rest storePath
        other -> pure other

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
                Right rawNar -> unpackAndRegister store sp narInfo rawNar

-- | Pure pipeline: verify signature, then produce an IO action
-- that downloads and decompresses.
verifyAndDecompress ::
  CacheConfig ->
  HTTP.Manager ->
  NarInfo.NarInfo ->
  Either Text (IO (Either Text BS.ByteString))
verifyAndDecompress cache mgr narInfo = do
  verifySigs cache narInfo
  let compression = NarInfo.niCompression narInfo
  pure $ do
    downloaded <- downloadNarWithRetry mgr cache narInfo
    pure $ downloaded >>= decompressNar compression

-- | Verify the NAR hash, deserialize, unpack to the store, set permissions,
-- and register.
unpackAndRegister :: Store -> StorePath -> NarInfo.NarInfo -> BS.ByteString -> IO SubstResult
unpackAndRegister store sp narInfo rawNar =
  -- Verify the downloaded NAR's hash matches the (signed) narinfo BEFORE
  -- trusting its bytes.  The NAR hash is the content-addressed integrity
  -- contract; the signature only attests to the narinfo, not the body, so
  -- network corruption or a compromised cache must be caught here.
  case verifyNarHash narInfo rawNar of
    Left err -> pure (SubstError err)
    Right () -> case NAR.deserialise rawNar of
      Left err -> pure (SubstError ("NAR deserialisation failed: " <> T.pack err))
      Right narEntry -> do
        let destPath = storePathToFilePath (stDir store) sp
        unpackResult <- try (unpackNarEntry destPath narEntry)
        case (unpackResult :: Either SomeException (Either Text ())) of
          Left err -> pure (SubstError ("unpack failed: " <> T.pack (show err)))
          Right (Left err) -> pure (SubstError ("unpack failed: " <> err))
          Right (Right ()) -> do
            setReadOnly destPath
            registerPath
              (stDB store)
              PathRegistration
                { prPath = sp,
                  prNarHash = NarInfo.niNarHash narInfo,
                  prNarSize = fromInteger (NarInfo.niNarSize narInfo),
                  prDeriver = NarInfo.niDeriver narInfo,
                  prReferences = parseReferences (stDir store) (NarInfo.niReferences narInfo)
                }
            pure (SubstSuccess sp)

-- | Verify that the downloaded NAR bytes hash to the narinfo's declared
-- NarHash.  Compares decoded hash bytes, so any valid encoding of the digest
-- validates; @prNarHash@ stays sourced from the (now-verified) narinfo.
verifyNarHash :: NarInfo.NarInfo -> BS.ByteString -> Either Text ()
verifyNarHash narInfo rawNar =
  case Hash.parseNixHash (NarInfo.niNarHash narInfo) of
    Left err -> Left ("invalid narinfo NarHash: " <> T.pack err)
    Right declared
      | declared == actual -> Right ()
      | otherwise ->
          Left
            ( "NAR hash mismatch: narinfo declares "
                <> NarInfo.niNarHash narInfo
                <> " but downloaded bytes hash to "
                <> Hash.formatNixHash actual
            )
  where
    actual = Hash.hashBytes rawNar

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

-- | Fetch a narinfo from a cache.
-- Returns @Left SubstNotFound@ on 404, @Left (SubstError msg)@ on other errors.
fetchNarInfo :: HTTP.Manager -> CacheConfig -> StorePath -> IO (Either SubstResult NarInfo.NarInfo)
fetchNarInfo mgr cache sp = do
  let url = T.unpack (ccUrl cache) <> "/" <> T.unpack (spHash sp) <> ".narinfo"
  request <- HTTP.parseRequest url
  response <- HTTP.httpLbs request mgr
  let code = HTTP.statusCode (HTTP.responseStatus response)
  if code == httpOk
    then case NarInfo.parseNarInfo (TE.decodeUtf8 (LBS.toStrict (HTTP.responseBody response))) of
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
-- a strict 'BS.ByteString'), so a very large path briefly spikes RSS.  Streaming
-- would need a streaming NAR parser that nova-cache does not yet provide.
downloadNar :: HTTP.Manager -> CacheConfig -> NarInfo.NarInfo -> IO (Either Text BS.ByteString)
downloadNar mgr cache narInfo = do
  let narUrl = T.unpack (ccUrl cache) <> "/" <> T.unpack (NarInfo.niUrl narInfo)
  request <- HTTP.parseRequest narUrl
  response <- HTTP.httpLbs request mgr
  let code = HTTP.statusCode (HTTP.responseStatus response)
  if code == httpOk
    then pure (Right (LBS.toStrict (HTTP.responseBody response)))
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

-- | Decompress NAR data based on the compression type from narinfo.
--
-- Currently supports @\"none\"@ (raw NAR) and @\"\"@ (empty = no compression).
-- XZ decompression requires enabling the @compression@ flag in nova-cache.
decompressNar :: Text -> BS.ByteString -> Either Text BS.ByteString
decompressNar compression narData
  | compression == "none" || T.null compression = Right narData
  | compression == "xz" = Left "xz decompression not yet available (enable nova-cache compression flag)"
  | otherwise = Left ("unsupported compression: " <> compression)

-- | Parse narinfo references (store path basenames) into StorePaths.
parseReferences :: StoreDir -> [Text] -> [StorePath]
parseReferences storeDir refs =
  [sp | ref <- refs, Just sp <- [parseStorePath storeDir ref]]

-- ---------------------------------------------------------------------------
-- NAR unpacking
-- ---------------------------------------------------------------------------

-- | Unpack a NarEntry tree to a filesystem destination.  Returns @Left@ on an
-- unsafe entry name (path traversal); these come from untrusted cache data, so
-- a typed failure is used instead of a partial 'error'.
unpackNarEntry :: FilePath -> NAR.NarEntry -> IO (Either Text ())
unpackNarEntry path entry = case entry of
  NAR.NarRegular isExec contents -> do
    createDirectoryIfMissing True (takeDirectory path)
    BS.writeFile path contents
    when isExec $ do
      perms <- Dir.getPermissions path
      setPermissions path (Dir.setOwnerExecutable True perms)
    pure (Right ())
  NAR.NarSymlink target -> do
    createDirectoryIfMissing True (takeDirectory path)
    -- On Windows, symlinks require elevated permissions.
    -- Fall back to writing the target as a text file.
    result <- try (Dir.createFileLink (T.unpack target) path)
    case result of
      Right () -> pure (Right ())
      Left (_ :: SomeException) -> do
        writeFile path (T.unpack target)
        pure (Right ())
  NAR.NarDirectory entries -> do
    createDirectoryIfMissing True path
    unpackChildren path entries

-- | Unpack directory children, short-circuiting with a typed failure on the
-- first unsafe entry name rather than crashing on untrusted input.
unpackChildren :: FilePath -> [(Text, NAR.NarEntry)] -> IO (Either Text ())
unpackChildren _ [] = pure (Right ())
unpackChildren path ((name, child) : rest)
  | not (isSafeNarName name) =
      pure (Left ("unsafe NAR directory entry name: " <> name))
  | otherwise = do
      result <- unpackNarEntry (path </> T.unpack name) child
      case result of
        Left err -> pure (Left err)
        Right () -> unpackChildren path rest

-- | Validate that a NAR entry name is safe (no path traversal).
isSafeNarName :: Text -> Bool
isSafeNarName name =
  not (T.null name)
    && name /= ".."
    && name /= "."
    && not (T.isInfixOf "/" name)
    && not (T.isInfixOf "\\" name)
