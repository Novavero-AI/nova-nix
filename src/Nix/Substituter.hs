{-# LANGUAGE ScopedTypeVariables #-}

-- | Binary substituter — download pre-built paths from remote caches.
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
-- NAR serialization, signature verification — all handled by the
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
    decompressNar,
    unpackNarEntry,
    parseReferences,
  )
where

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
import Nix.Store.Path (StoreDir, StorePath (..), parseStorePath, storePathToFilePath)
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
  = -- | Successfully substituted — path is now in the store.
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
  manager <- HTTPS.newTlsManager
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
    Right narInfo -> do
      -- 2–4. Verify, download, decompress (pure pipeline after fetch)
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
    downloaded <- downloadNar mgr cache narInfo
    pure $ downloaded >>= decompressNar compression

-- | Deserialize a NAR, unpack to the store, set permissions, and register.
unpackAndRegister :: Store -> StorePath -> NarInfo.NarInfo -> BS.ByteString -> IO SubstResult
unpackAndRegister store sp narInfo rawNar =
  case NAR.deserialise rawNar of
    Left err -> pure (SubstError ("NAR deserialisation failed: " <> T.pack err))
    Right narEntry -> do
      let destPath = storePathToFilePath (stDir store) sp
      unpackResult <- try (unpackNarEntry destPath narEntry)
      case (unpackResult :: Either SomeException ()) of
        Left err -> pure (SubstError ("unpack failed: " <> T.pack (show err)))
        Right () -> do
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

-- | Download the NAR file referenced by a narinfo.
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

-- | Unpack a NarEntry tree to a filesystem destination.
unpackNarEntry :: FilePath -> NAR.NarEntry -> IO ()
unpackNarEntry path entry = case entry of
  NAR.NarRegular isExec contents -> do
    createDirectoryIfMissing True (takeDirectory path)
    BS.writeFile path contents
    when isExec $ do
      perms <- Dir.getPermissions path
      setPermissions path (Dir.setOwnerExecutable True perms)
  NAR.NarSymlink target -> do
    createDirectoryIfMissing True (takeDirectory path)
    -- On Windows, symlinks require elevated permissions.
    -- Fall back to writing the target as a text file.
    result <- try (Dir.createFileLink (T.unpack target) path)
    case result of
      Right () -> pure ()
      Left (_ :: SomeException) ->
        writeFile path (T.unpack target)
  NAR.NarDirectory entries -> do
    createDirectoryIfMissing True path
    mapM_
      ( \(name, child) ->
          if isSafeNarName name
            then unpackNarEntry (path </> T.unpack name) child
            else error ("unpackNarEntry: unsafe directory entry name: " <> T.unpack name)
      )
      entries

-- | Validate that a NAR entry name is safe (no path traversal).
isSafeNarName :: Text -> Bool
isSafeNarName name =
  not (T.null name)
    && name /= ".."
    && name /= "."
    && not (T.isInfixOf "/" name)
    && not (T.isInfixOf "\\" name)
