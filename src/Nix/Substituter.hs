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
    tryCachesWith,
    verifySigs,
    verifyNarHash,
    verifyNarSize,
    narInfoMatchesPath,
    decompressorFor,
    decompressNar,
    unpackNarEntry,
    clearStaleDestination,
    parseReferences,
    parseDeriver,
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (filterM, when)
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
import Nix.Store.DB (PathRegistration (..))
import Nix.Store.Path (StoreDir, StorePath (..), parseStorePathBaseName, storePathHashLen, storePathToFilePath)
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

-- | Pure pipeline: verify signature and resolve the decompressor, then
-- produce an IO action that downloads and decompresses.  Unsupported
-- compression rejects HERE, before the action runs: the value is known
-- from the narinfo, and a multi-hundred-MB download that can only fail
-- in decompression is pure waste.
verifyAndDecompress ::
  CacheConfig ->
  HTTP.Manager ->
  NarInfo.NarInfo ->
  Either Text (IO (Either Text BS.ByteString))
verifyAndDecompress cache mgr narInfo = do
  verifySigs cache narInfo
  decompress <- decompressorFor (NarInfo.niCompression narInfo)
  pure $ do
    downloaded <- downloadNarWithRetry mgr cache narInfo
    pure $ downloaded >>= decompress

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
  case verifyNarHash narInfo rawNar >> verifyNarSize narInfo rawNar >> registrationMeta of
    Left err -> pure (SubstError err)
    Right (refs, deriver) -> case NAR.deserialise rawNar of
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
            pure $
              SubstSuccess
                PathRegistration
                  { prPath = sp,
                    prNarHash = NarInfo.niNarHash narInfo,
                    -- The verified actual byte count (equal to the declared
                    -- NarSize per 'verifyNarSize') - no Integer conversion
                    -- that could wrap.
                    prNarSize = BS.length rawNar,
                    prDeriver = deriver,
                    prReferences = refs
                  }
  where
    registrationMeta = do
      refs <- parseReferences (NarInfo.niReferences narInfo)
      deriver <- parseDeriver (stDir store) (NarInfo.niDeriver narInfo)
      pure (refs, deriver)

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

-- | Fetch a narinfo from a cache.
-- Returns @Left SubstNotFound@ on 404, @Left (SubstError msg)@ on other errors.
fetchNarInfo :: HTTP.Manager -> CacheConfig -> StorePath -> IO (Either SubstResult NarInfo.NarInfo)
fetchNarInfo mgr cache sp = do
  let url = T.unpack (ccUrl cache) <> "/" <> T.unpack (spHash sp) <> ".narinfo"
  request <- HTTP.parseRequest url
  response <- HTTP.httpLbs request mgr
  let code = HTTP.statusCode (HTTP.responseStatus response)
  -- Lenient decode: the body is cache-controlled bytes, and a stray
  -- invalid UTF-8 sequence must surface as a narinfo parse error, not an
  -- impure UnicodeException (the push side decodes the same way).
  if code == httpOk
    then case NarInfo.parseNarInfo (TE.decodeUtf8Lenient (LBS.toStrict (HTTP.responseBody response))) of
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

-- | The decompressor for a narinfo @Compression@ value, decided from the
-- value alone so unsupported compression rejects before any download.
-- Currently @\"none\"@ and @\"\"@ (identity); xz returns with the
-- foreign-cache substitution feature.
decompressorFor :: Text -> Either Text (BS.ByteString -> Either Text BS.ByteString)
decompressorFor compression
  | compression == "none" || T.null compression = Right Right
  | compression == "xz" = Left "xz decompression not yet available"
  | otherwise = Left ("unsupported compression: " <> compression)

-- | Decompress NAR data based on the compression type from narinfo.
-- Support is decided by 'decompressorFor'; this applies the result.
decompressNar :: Text -> BS.ByteString -> Either Text BS.ByteString
decompressNar compression narData = decompressorFor compression >>= ($ narData)

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

-- | Unpack a NarEntry tree to a filesystem destination.  Returns @Left@ on an
-- unsafe entry name (path traversal); these come from untrusted cache data, so
-- a typed failure is used instead of a partial 'error'.
--
-- Regular files and directories are written in one pass; symlinks are
-- created in a second pass, after their targets are materialized.  Windows
-- symlinks are typed (file vs directory) and the NAR format does not record
-- the target's kind, so the only reliable way to pick the flavor is to look
-- at the target on disk - which may sort after the link within the tree.
unpackNarEntry :: FilePath -> NAR.NarEntry -> IO (Either Text ())
unpackNarEntry path entry = do
  walked <- unpackTree path entry
  case walked of
    Left err -> pure (Left err)
    Right links -> createSymlinks links

-- | First unpack pass: write regular files and directories, recording
-- symlinks as (link path, target) for the second pass.
unpackTree :: FilePath -> NAR.NarEntry -> IO (Either Text [(FilePath, Text)])
unpackTree path entry = case entry of
  NAR.NarRegular isExec contents -> do
    createDirectoryIfMissing True (takeDirectory path)
    BS.writeFile path contents
    when isExec $ do
      perms <- Dir.getPermissions path
      setPermissions path (Dir.setOwnerExecutable True perms)
    pure (Right [])
  NAR.NarSymlink target -> pure (Right [(path, target)])
  NAR.NarDirectory entries -> do
    createDirectoryIfMissing True path
    unpackChildren path entries

-- | Unpack directory children, short-circuiting with a typed failure on the
-- first unsafe entry name rather than crashing on untrusted input.
unpackChildren :: FilePath -> [(Text, NAR.NarEntry)] -> IO (Either Text [(FilePath, Text)])
unpackChildren _ [] = pure (Right [])
unpackChildren path ((name, child) : rest)
  | not (isSafeNarName name) =
      pure (Left ("unsafe NAR directory entry name: " <> name))
  | otherwise = do
      result <- unpackTree (path </> T.unpack name) child
      case result of
        Left err -> pure (Left err)
        Right links -> do
          restResult <- unpackChildren path rest
          case restResult of
            Left err -> pure (Left err)
            Right moreLinks -> pure (Right (links <> moreLinks))

-- | Second unpack pass: create the recorded symlinks.  Links whose targets
-- already exist are created first - possibly unblocking link-to-link
-- chains - so the Windows link flavor (file vs directory) is read off the
-- real target.  When no pending link's target exists, the remainder are
-- dangling and their kind is unknowable: they default to file links.
createSymlinks :: [(FilePath, Text)] -> IO (Either Text ())
createSymlinks [] = pure (Right ())
createSymlinks pending = do
  ready <- filterM targetExists pending
  case ready of
    [] -> createAll pending
    _ -> do
      made <- createAll ready
      case made of
        Left err -> pure (Left err)
        Right () -> createSymlinks (filter (`notElem` ready) pending)
  where
    targetExists (linkPath, target) =
      Dir.doesPathExist (takeDirectory linkPath </> T.unpack target)
    createAll [] = pure (Right ())
    createAll (link : rest) = do
      made <- uncurry createSymlink link
      case made of
        Left err -> pure (Left err)
        Right () -> createAll rest

-- | Create one symlink, choosing the Windows flavor from the target's kind.
-- A creation failure is loud: the old fallback of writing the target text
-- as a regular file registered a tree whose NAR hash differed from the
-- signed narinfo's - silent store corruption that a later push refuses to
-- publish.  Failing lets the caller fall back to a local build.
createSymlink :: FilePath -> Text -> IO (Either Text ())
createSymlink linkPath target = do
  createDirectoryIfMissing True (takeDirectory linkPath)
  let targetStr = T.unpack target
  targetIsDir <- Dir.doesDirectoryExist (takeDirectory linkPath </> targetStr)
  result <-
    try $
      if targetIsDir
        then Dir.createDirectoryLink targetStr linkPath
        else Dir.createFileLink targetStr linkPath
  case result of
    Right () -> pure (Right ())
    Left (e :: SomeException) ->
      pure
        ( Left
            ( "cannot create symlink "
                <> T.pack linkPath
                <> " -> "
                <> target
                <> ": "
                <> T.pack (show e)
                <> " (on Windows this needs Developer Mode or elevation)"
            )
        )

-- | Validate that a NAR entry name is safe (no path traversal).
isSafeNarName :: Text -> Bool
isSafeNarName name =
  not (T.null name)
    && name /= ".."
    && name /= "."
    && not (T.isInfixOf "/" name)
    && not (T.isInfixOf "\\" name)
