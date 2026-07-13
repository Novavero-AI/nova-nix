{-# LANGUAGE ScopedTypeVariables #-}

-- | Push store paths to a nova-cache binary cache.
--
-- == The push choreography
--
-- 1. Compute the full reference closure of the requested paths from the
--    store database - a cache must never hold a path without its
--    dependencies, or substitution 404s on cold machines.
-- 2. Fetch @\/narinfo-hashes@ from the cache and skip paths already there.
-- 3. Upload every missing NAR, then every narinfo.  NARs go first
--    globally: a narinfo is the public announcement that a path is
--    available, so it must never appear before its bytes do.
-- 4. Verify one round-trip: fetch a narinfo back and check the server
--    signed it.
--
-- == Path forms
--
-- The store database and filesystem use the platform store dir
-- (@C:\\nix\\store@ on Windows).  Published narinfos always use the
-- canonical store dir (@\/nix\/store@) with forward slashes - the form the
-- store-path hashes were computed against and the form cache servers
-- validate.  'StorePath' itself is directory-agnostic, so translation is
-- just a matter of which renderer runs at which boundary.
--
-- == Compression
--
-- Uploads use @Compression: none@: the substituter round-trips it on every
-- platform today (XZ support is gated behind nova-cache's @compression@
-- flag, which is off on Windows), and the dominant seed payloads are
-- already-compressed source tarballs.
module Nix.Push
  ( -- * Configuration
    PushConfig (..),
    PushSummary (..),

    -- * Pushing
    pushPaths,
    computeClosure,
    loadApiKeyFile,

    -- * Pure pieces (exported for tests)
    mkNarInfo,
    planMissing,
    narFileName,
    stripHashPrefix,
    storePathBasename,
    checkRecordedNarHash,
  )
where

import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_, unless, when)
import Control.Monad.Except (ExceptT (..), liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import Data.List (sort)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS
import qualified Network.HTTP.Types as HTTP
import Nix.Store (Store (..), queryDeriver, queryPathInfo, queryReferences)
import qualified Nix.Store.DB as DB
import Nix.Store.Path (StorePath (..), defaultStoreDir, parseStorePath, storePathToFilePath, storePathToText)
import qualified NovaCache.Hash as Hash
import qualified NovaCache.NAR as NAR
import NovaCache.NarInfo (NarInfo (..), parseNarInfo, renderNarInfo)
import NovaCache.Signing (normalizeKeyText)
import System.IO (stderr)

-- ---------------------------------------------------------------------------
-- Named constants
-- ---------------------------------------------------------------------------

-- | Cache endpoint listing all stored narinfo hashes, one per line.
narinfoHashesEndpoint :: Text
narinfoHashesEndpoint = "narinfo-hashes"

-- | URL path segment under which NAR files live.
narDirSegment :: Text
narDirSegment = "nar"

-- | File extension for an uncompressed NAR.
narExtension :: Text
narExtension = ".nar"

-- | Extension for narinfo objects.
narInfoExtension :: Text
narInfoExtension = ".narinfo"

-- | The narinfo @Compression@ value used for uploads.
compressionNone :: Text
compressionNone = "none"

-- | Authorization scheme prefix for the API key.
bearerPrefix :: BS.ByteString
bearerPrefix = "Bearer "

-- | HTTP success status code.
httpStatusOk :: Int
httpStatusOk = 200

-- ---------------------------------------------------------------------------
-- Configuration and results
-- ---------------------------------------------------------------------------

-- | Where and how to push.
data PushConfig = PushConfig
  { -- | Cache base URL, no trailing slash (e.g. @https:\/\/cache.example.com@).
    pcCacheUrl :: !Text,
    -- | Bearer token for authenticated writes.  'Nothing' sends no
    -- Authorization header (only useful against an open-writes server).
    pcApiKey :: !(Maybe Text)
  }
  deriving (Eq, Show)

-- | What a push accomplished.
data PushSummary = PushSummary
  { -- | Paths uploaded (NAR + narinfo).
    psPushed :: !Int,
    -- | Closure paths skipped because the cache already had them.
    psSkipped :: !Int
  }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Key loading
-- ---------------------------------------------------------------------------

-- | Read an API key from a file, dropping byte-order marks and surrounding
-- whitespace.  Key files written by Windows tooling are a known source of
-- BOM contamination; 'normalizeKeyText' makes the transfer byte-clean.
loadApiKeyFile :: FilePath -> IO (Either Text Text)
loadApiKeyFile path = do
  attempt <- try (TIO.readFile path)
  pure $ case attempt of
    Left (e :: SomeException) -> Left ("cannot read key file: " <> T.pack (show e))
    Right raw ->
      let key = normalizeKeyText raw
       in if T.null key
            then Left ("key file is empty: " <> T.pack path)
            else Right key

-- ---------------------------------------------------------------------------
-- Closure computation
-- ---------------------------------------------------------------------------

-- | Compute the full reference closure of the given paths from the store
-- database (breadth-first over @Refs@).  Fails if a recorded reference does
-- not parse as a store path - that would mean a corrupt database, and
-- pushing a closure with holes would poison the cache.
computeClosure :: Store -> [StorePath] -> IO (Either Text [StorePath])
computeClosure store roots = runExceptT (go Set.empty [] roots)
  where
    go :: Set Text -> [StorePath] -> [StorePath] -> ExceptT Text IO [StorePath]
    go _ acc [] = pure (reverse acc)
    go seen acc (sp : rest)
      | spHash sp `Set.member` seen = go seen acc rest
      | otherwise = do
          refTexts <- liftIO (queryReferences (stDB store) sp)
          refs <- liftEither (traverse parseRef refTexts)
          go (Set.insert (spHash sp) seen) (sp : acc) (refs ++ rest)
    parseRef txt = case parseStorePath (stDir store) txt of
      Just sp -> Right sp
      Nothing -> Left ("unparseable reference in store DB: " <> txt)

-- ---------------------------------------------------------------------------
-- Pure planning and rendering
-- ---------------------------------------------------------------------------

-- | Paths whose narinfo hash the cache does not already have.
planMissing :: Set Text -> [StorePath] -> [StorePath]
planMissing remote = filter (\sp -> not (spHash sp `Set.member` remote))

-- | The basename form used in narinfo @References@ and @Deriver@ fields:
-- @\<hash\>-\<name\>@ with no store dir.
storePathBasename :: StorePath -> Text
storePathBasename sp = spHash sp <> "-" <> spName sp

-- | Drop a @\<algo\>:@ prefix from a formatted hash, leaving the digest.
stripHashPrefix :: Text -> Text
stripHashPrefix h = case T.breakOn ":" h of
  (_, rest) | not (T.null rest) -> T.drop 1 rest
  _ -> h

-- | The NAR object filename for a given (uncompressed) NAR hash.
narFileName :: Text -> Text
narFileName narHash = stripHashPrefix narHash <> narExtension

-- | Construct the narinfo describing a store path.
--
-- With @Compression: none@ the file fields equal the NAR fields.  The
-- @StorePath@ uses the canonical @\/nix\/store@ form; references and the
-- deriver are basenames, matching the wire format.
mkNarInfo :: StorePath -> Text -> Int -> [StorePath] -> Maybe StorePath -> NarInfo
mkNarInfo sp narHash narSize refs deriver =
  NarInfo
    { niStorePath = storePathToText defaultStoreDir sp,
      niUrl = narDirSegment <> "/" <> narFileName narHash,
      niCompression = compressionNone,
      niFileHash = Just narHash,
      niFileSize = Just (fromIntegral narSize),
      niNarHash = narHash,
      niNarSize = fromIntegral narSize,
      niReferences = sort (map storePathBasename refs),
      niDeriver = storePathBasename <$> deriver,
      niSigs = [],
      niCA = Nothing
    }

-- ---------------------------------------------------------------------------
-- Push orchestration
-- ---------------------------------------------------------------------------

-- | Push the closure of the given paths to the cache.
--
-- Serializes one NAR at a time (bounded memory), uploads all NARs before
-- any narinfo, and finishes with a signed round-trip check.  Any failure
-- aborts with an error; a partial push leaves only orphaned NARs behind,
-- which are invisible to clients.
pushPaths :: PushConfig -> Store -> [StorePath] -> IO (Either Text PushSummary)
pushPaths cfg store roots = do
  attempt <- try (runExceptT run)
  pure $ case attempt of
    Left (e :: SomeException) -> Left ("push failed: " <> T.pack (show e))
    Right r -> r
  where
    run :: ExceptT Text IO PushSummary
    run = do
      manager <- liftIO newPushManager
      closure <- ExceptT (computeClosure store roots)
      remote <- fetchRemoteHashes manager cfg
      let missing = planMissing remote closure
          skipped = length closure - length missing
      when (skipped > 0) $
        logLine ("[skip]  " <> T.pack (show skipped) <> " path(s) already cached")
      -- Phase 1: NARs.  One at a time; bytes are dropped after upload.
      pairs <- forM missing $ \sp -> do
        narInfo <- uploadNar manager cfg store sp
        pure (sp, narInfo)
      -- Phase 2: narinfos, only after every NAR is in place.
      forM_ pairs $ \(sp, narInfo) -> do
        uploadNarInfo manager cfg sp narInfo
        logLine ("[push]  " <> storePathBasename sp)
      -- Round-trip: the first pushed narinfo must come back signed.
      case pairs of
        ((sp, _) : _) -> verifySignedRoundTrip manager cfg sp
        [] -> pure ()
      pure PushSummary {psPushed = length pairs, psSkipped = skipped}

-- | HTTP manager for pushes.  Large NAR uploads over residential uplinks
-- can take many minutes, so the response timeout is disabled rather than
-- guessed at.
newPushManager :: IO HTTP.Manager
newPushManager =
  HTTP.newManager
    HTTPS.tlsManagerSettings {HTTP.managerResponseTimeout = HTTP.responseTimeoutNone}

-- | Fetch the set of narinfo hashes the cache already stores.  The
-- endpoint is push-tool plumbing and the server requires the write key
-- for it (nova-cache 0.5), so the request authenticates like the PUTs.
fetchRemoteHashes :: HTTP.Manager -> PushConfig -> ExceptT Text IO (Set Text)
fetchRemoteHashes manager cfg = do
  let url = pcCacheUrl cfg <> "/" <> narinfoHashesEndpoint
  response <- httpRequest manager "GET" url (authHeaders cfg) Nothing
  let code = HTTP.statusCode (HTTP.responseStatus response)
  unless (code == httpStatusOk) $
    throwError ("GET " <> narinfoHashesEndpoint <> " returned HTTP " <> T.pack (show code))
  body <- decodeUtf8Lenient (responseBytes response)
  pure (Set.fromList (filter (not . T.null) (map T.strip (T.lines body))))

-- | Serialize a store path to a NAR, cross-check the database NAR hash,
-- and upload the bytes.  Returns the narinfo to publish in phase 2.
uploadNar :: HTTP.Manager -> PushConfig -> Store -> StorePath -> ExceptT Text IO NarInfo
uploadNar manager cfg store sp = do
  let physicalPath = storePathToFilePath (stDir store) sp
  narEntry <- liftIO (NAR.serialiseFromPath physicalPath)
  let narBytes = NAR.serialise narEntry
      narHash = Hash.formatNixHash (Hash.hashBytes narBytes)
      narSize = BS.length narBytes
  recorded <- liftIO (queryPathInfo (stDB store) sp)
  liftEither (checkRecordedNarHash recorded narHash sp)
  refTexts <- liftIO (queryReferences (stDB store) sp)
  refs <- liftEither (traverse (parseRefText store) refTexts)
  deriverText <- liftIO (queryDeriver (stDB store) sp)
  let deriver = deriverText >>= parseStorePath (stDir store)
      narInfo = mkNarInfo sp narHash narSize refs deriver
      url = pcCacheUrl cfg <> "/" <> niUrl narInfo
  logLine
    ( "[nar]   "
        <> storePathBasename sp
        <> " ("
        <> T.pack (show narSize)
        <> " bytes)"
    )
  response <- httpRequest manager "PUT" url (authHeaders cfg) (Just narBytes)
  expectOk ("PUT " <> niUrl narInfo) response
  pure narInfo

-- | Pre-upload integrity gate.  The store DB recorded the path's NAR hash
-- at registration; a mismatch now means the path changed on disk - refuse
-- to publish corruption.  An on-disk path with NO registration (an
-- interrupted build) is refused too: its references are unknown, so
-- publishing would fabricate an empty-reference narinfo and serve a
-- closure with holes.
checkRecordedNarHash :: Maybe DB.PathInfo -> Text -> StorePath -> Either Text ()
checkRecordedNarHash recorded narHash sp = case recorded of
  Nothing ->
    Left
      ( "store integrity: "
          <> storePathBasename sp
          <> " is on disk but not registered as valid; refusing to publish it with unknown references"
      )
  Just info
    | DB.piNarHash info /= narHash ->
        Left
          ( "store integrity: "
              <> storePathBasename sp
              <> " hashes to "
              <> narHash
              <> " but the DB recorded "
              <> DB.piNarHash info
          )
    | otherwise -> Right ()

-- | Upload a rendered narinfo (the server validates and signs it).
uploadNarInfo :: HTTP.Manager -> PushConfig -> StorePath -> NarInfo -> ExceptT Text IO ()
uploadNarInfo manager cfg sp narInfo = do
  let object = spHash sp <> narInfoExtension
      url = pcCacheUrl cfg <> "/" <> object
      body = TE.encodeUtf8 (renderNarInfo narInfo)
  response <- httpRequest manager "PUT" url (authHeaders cfg) (Just body)
  expectOk ("PUT " <> object) response

-- | Fetch a just-pushed narinfo back and require a signature on it.
verifySignedRoundTrip :: HTTP.Manager -> PushConfig -> StorePath -> ExceptT Text IO ()
verifySignedRoundTrip manager cfg sp = do
  let object = spHash sp <> narInfoExtension
      url = pcCacheUrl cfg <> "/" <> object
  response <- httpRequest manager "GET" url [] Nothing
  expectOk ("GET " <> object) response
  body <- decodeUtf8Lenient (responseBytes response)
  narInfo <- liftEither (either (Left . T.pack) Right (parseNarInfo body))
  when (null (niSigs narInfo)) $
    throwError ("round-trip check failed: " <> object <> " came back unsigned")
  logLine "[ok]    round-trip verified, signature present"

-- ---------------------------------------------------------------------------
-- HTTP plumbing
-- ---------------------------------------------------------------------------

-- | Issue an HTTP request with optional body, converting any exception
-- into a push error.
httpRequest ::
  HTTP.Manager ->
  BS.ByteString ->
  Text ->
  [(HTTP.HeaderName, BS.ByteString)] ->
  Maybe BS.ByteString ->
  ExceptT Text IO (HTTP.Response BS.ByteString)
httpRequest manager method url headers body = ExceptT $ do
  attempt <- try $ do
    request0 <- HTTP.parseRequest (T.unpack url)
    let request =
          request0
            { HTTP.method = method,
              HTTP.requestHeaders = headers,
              HTTP.requestBody = maybe (HTTP.requestBody request0) HTTP.RequestBodyBS body
            }
    response <- HTTP.httpLbs request manager
    pure (BS.toStrict <$> response)
  pure $ case attempt of
    Left (e :: SomeException) -> Left ("HTTP error for " <> url <> ": " <> T.pack (show e))
    Right response -> Right response

-- | Authorization headers for authenticated writes.
authHeaders :: PushConfig -> [(HTTP.HeaderName, BS.ByteString)]
authHeaders cfg = case pcApiKey cfg of
  Nothing -> []
  Just key -> [("Authorization", bearerPrefix <> TE.encodeUtf8 key)]

-- | Require a 200 response, with a readable error otherwise.
expectOk :: Text -> HTTP.Response BS.ByteString -> ExceptT Text IO ()
expectOk label response = do
  let code = HTTP.statusCode (HTTP.responseStatus response)
  unless (code == httpStatusOk) $ do
    body <- decodeUtf8Lenient (responseBytes response)
    throwError (label <> " returned HTTP " <> T.pack (show code) <> ": " <> T.take 200 body)

-- | The response body bytes.
responseBytes :: HTTP.Response BS.ByteString -> BS.ByteString
responseBytes = HTTP.responseBody

-- | Decode response bytes leniently (error bodies may not be UTF-8).
decodeUtf8Lenient :: (Monad m) => BS.ByteString -> ExceptT Text m Text
decodeUtf8Lenient = pure . TE.decodeUtf8With (\_ _ -> Just '\xFFFD')

-- | Parse a reference path text from the database.
parseRefText :: Store -> Text -> Either Text StorePath
parseRefText store txt = case parseStorePath (stDir store) txt of
  Just sp -> Right sp
  Nothing -> Left ("unparseable reference in store DB: " <> txt)

-- | Progress line on stderr (stdout stays reserved for results).
logLine :: Text -> ExceptT Text IO ()
logLine = liftIO . TIO.hPutStrLn stderr
