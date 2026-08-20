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
-- Uploads default to @Compression: none@ - the cache's existing
-- content is uncompressed, and mixing artifact kinds is an operator
-- decision, not a default flip.  'PushZstd' compresses each NAR with
-- nova-cache's zstandard encoder before upload: the narinfo's file
-- fields describe the compressed artifact, its object name carries
-- the compressed file hash, and the substituter decompresses under
-- the declared NarSize bound.
module Nix.Push
  ( -- * Configuration
    PushConfig (..),
    PushCompression (..),
    PushSummary (..),

    -- * Pushing
    pushPaths,
    computeClosure,
    loadApiKeyFile,

    -- * Pure pieces (exported for tests)
    parsePushCompression,
    pushCompressionValues,
    mkNarInfo,
    mkPushArtifact,
    PushArtifact (..),
    planMissing,
    narFileName,
    stripHashPrefix,
    storePathBasename,
    checkRecordedNarHash,
    narHashMatches,
  )
where

import Control.Exception (SomeException, try)
import Control.Monad (foldM, forM, forM_, unless, when)
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
import Nix.Compression (compressionNameNone, compressionNameZstd)
import Nix.Store (Store (..), queryDeriver, queryPathInfo, queryReferences)
import qualified Nix.Store.DB as DB
import qualified Nix.Store.ExecBit as ExecBit
import Nix.Store.Path (StorePath (spHash, spName), defaultStoreDir, parseStorePath, storePathToFilePath, storePathToText)
import qualified NovaCache.Hash as Hash
import qualified NovaCache.NAR as NAR
import NovaCache.NarInfo (NarInfo (..), parseNarInfo, renderNarInfo)
import NovaCache.Signing (normalizeKeyText)
import qualified NovaCache.Zstd as Zstd
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

-- | Extension for zstd-compressed NAR objects.
narZstExtension :: Text
narZstExtension = ".nar.zst"

-- | Authorization scheme prefix for the API key.
bearerPrefix :: BS.ByteString
bearerPrefix = "Bearer "

-- | HTTP success status code.
httpStatusOk :: Int
httpStatusOk = 200

-- ---------------------------------------------------------------------------
-- Configuration and results
-- ---------------------------------------------------------------------------

-- | How each NAR is packaged for upload.  A property of the
-- destination cache, as upstream models compression on binary cache
-- stores - not a per-path whim.
data PushCompression = PushNone | PushZstd
  deriving (Eq, Show)

-- | The accepted @--compression@ spellings, for parser errors and the
-- CLI help line - one rendering of the register in 'Nix.Compression'.
pushCompressionValues :: Text
pushCompressionValues = compressionNameNone <> ", " <> compressionNameZstd

-- | Parse the CLI spelling of a push compression.  The spellings are
-- the narinfo @Compression@ names from 'Nix.Compression', so the CLI
-- vocabulary and the wire vocabulary cannot drift.
parsePushCompression :: Text -> Either Text PushCompression
parsePushCompression name
  | name == compressionNameNone = Right PushNone
  | name == compressionNameZstd = Right PushZstd
  | otherwise =
      Left
        ( "unknown --compression "
            <> name
            <> " (expected: "
            <> pushCompressionValues
            <> ")"
        )

-- | Where and how to push.
data PushConfig = PushConfig
  { -- | Cache base URL, no trailing slash (e.g. @https:\/\/cache.example.com@).
    pcCacheUrl :: !Text,
    -- | Bearer token for authenticated writes.  'Nothing' sends no
    -- Authorization header (only useful against an open-writes server).
    pcApiKey :: !(Maybe Text),
    -- | Artifact packaging for this destination (see 'PushCompression').
    pcCompression :: !PushCompression
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
--
-- The bytes are decoded as UTF-8 EXPLICITLY: locale text IO decodes by
-- console code page, which turns the BOM bytes into codepage characters
-- that no normalizer recognizes - on the very consoles the BOM handling
-- exists for.
loadApiKeyFile :: FilePath -> IO (Either Text Text)
loadApiKeyFile path = do
  attempt <- try (BS.readFile path)
  pure $ case attempt of
    Left (e :: SomeException) -> Left ("cannot read key file: " <> T.pack (show e))
    Right bytes -> case TE.decodeUtf8' bytes of
      Left _ -> Left ("key file is not valid UTF-8: " <> T.pack path)
      Right raw ->
        let key = normalizeKeyText raw
         in if T.null key
              then Left ("key file is empty: " <> T.pack path)
              else Right key

-- ---------------------------------------------------------------------------
-- Closure computation
-- ---------------------------------------------------------------------------

-- | Compute the full reference closure of the given paths from the store
-- database, in reverse-topological order: every path's references precede
-- it (postorder over @Refs@; a self-reference or already-visited path is
-- skipped).  Publishing narinfos in this order never announces a path
-- whose references are not yet visible.  Fails if a recorded reference
-- does not parse as a store path - that would mean a corrupt database,
-- and pushing a closure with holes would poison the cache.
computeClosure :: Store -> [StorePath] -> IO (Either Text [StorePath])
computeClosure store roots =
  runExceptT (reverse . snd <$> foldM visit (Set.empty, []) roots)
  where
    visit :: (Set Text, [StorePath]) -> StorePath -> ExceptT Text IO (Set Text, [StorePath])
    visit (seen, acc) sp
      | spHash sp `Set.member` seen = pure (seen, acc)
      | otherwise = do
          refTexts <- liftIO (queryReferences (stDB store) sp)
          refs <- liftEither (traverse parseRef refTexts)
          (seenAfterRefs, accAfterRefs) <- foldM visit (Set.insert (spHash sp) seen, acc) refs
          pure (seenAfterRefs, sp : accAfterRefs)
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

-- | The uploadable artifact for one NAR under the configured
-- compression: the bytes the cache stores, the file fields the
-- narinfo declares, and the object name.  With 'PushNone' every field
-- equals the NAR's own, byte-identical to what this module always
-- pushed.
data PushArtifact = PushArtifact
  { paBytes :: !BS.ByteString,
    paFileHash :: !Text,
    paFileSize :: !Int,
    paCompressionText :: !Text,
    paObjectName :: !Text,
    -- | The source NAR's hash and byte count, recorded at packaging
    -- time: a narinfo built from this artifact can only ever describe
    -- one byte stream, so the NAR fields and file fields cannot be
    -- paired wrongly by a caller.
    paNarHash :: !Text,
    paNarSize :: !Int
  }
  deriving (Eq, Show)

-- | Package one NAR for upload.  The zstd object is named by its own
-- (compressed) file hash, the convention the public caches follow.
mkPushArtifact :: PushCompression -> Text -> BS.ByteString -> PushArtifact
mkPushArtifact compression narHash narBytes = case compression of
  PushNone ->
    PushArtifact
      { paBytes = narBytes,
        paFileHash = narHash,
        paFileSize = BS.length narBytes,
        paCompressionText = compressionNameNone,
        paObjectName = narFileName narHash,
        paNarHash = narHash,
        paNarSize = BS.length narBytes
      }
  PushZstd ->
    let compressed = Zstd.compress Zstd.defaultCompressionLevel narBytes
        fileHash = Hash.formatNixHash (Hash.hashBytes compressed)
     in PushArtifact
          { paBytes = compressed,
            paFileHash = fileHash,
            paFileSize = BS.length compressed,
            paCompressionText = compressionNameZstd,
            paObjectName = stripHashPrefix fileHash <> narZstExtension,
            paNarHash = narHash,
            paNarSize = BS.length narBytes
          }

-- | Construct the narinfo describing a store path.  Every artifact
-- and NAR field - file hash, file size, compression, object name, NAR
-- hash, NAR size - comes from the one 'PushArtifact', so the narinfo
-- is internally consistent by construction.  The @StorePath@ uses
-- the canonical @\/nix\/store@ form; references and the deriver are
-- basenames, matching the wire format.
mkNarInfo :: PushArtifact -> StorePath -> [StorePath] -> Maybe StorePath -> NarInfo
mkNarInfo artifact sp refs deriver =
  NarInfo
    { niStorePath = storePathToText defaultStoreDir sp,
      niUrl = narDirSegment <> "/" <> paObjectName artifact,
      niCompression = paCompressionText artifact,
      niFileHash = Just (paFileHash artifact),
      niFileSize = Just (fromIntegral (paFileSize artifact)),
      niNarHash = paNarHash artifact,
      niNarSize = fromIntegral (paNarSize artifact),
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
      -- Phase 2: narinfos, only after every NAR is in place, in the
      -- closure's deps-first order: a freshly announced path's references
      -- are always already announced.
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
  narEntry <- liftIO (ExecBit.serialiseFromPath physicalPath)
  let narBytes = NAR.serialise narEntry
      narHash = Hash.formatNixHash (Hash.hashBytes narBytes)
      narSize = BS.length narBytes
  recorded <- liftIO (queryPathInfo (stDB store) sp)
  liftEither (checkRecordedNarHash recorded narHash sp)
  refTexts <- liftIO (queryReferences (stDB store) sp)
  refs <- liftEither (traverse (parseRefText store) refTexts)
  deriverText <- liftIO (queryDeriver (stDB store) sp)
  let deriver = deriverText >>= parseStorePath (stDir store)
      artifact = mkPushArtifact (pcCompression cfg) narHash narBytes
      narInfo = mkNarInfo artifact sp refs deriver
      url = pcCacheUrl cfg <> "/" <> niUrl narInfo
  logLine
    ( "[nar]   "
        <> storePathBasename sp
        <> " ("
        <> T.pack (show narSize)
        <> " bytes)"
    )
  response <- httpRequest manager "PUT" url (authHeaders cfg) (Just (paBytes artifact))
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
    | not (narHashMatches (DB.piNarHash info) narHash) ->
        Left
          ( "store integrity: "
              <> storePathBasename sp
              <> " hashes to "
              <> narHash
              <> " but the DB recorded "
              <> DB.piNarHash info
          )
    | otherwise -> Right ()

-- | Recorded and computed NAR hashes match when their decoded digests
-- agree, so any valid spelling of one digest matches (a foreign cache
-- may record base16 where local hashing renders nix-base32).  A
-- recorded value no spelling parses falls back to exact text equality,
-- keeping legacy rows comparable rather than un-checkable.
narHashMatches :: Text -> Text -> Bool
narHashMatches recorded computed =
  case (Hash.parseNixHash recorded, Hash.parseNixHash computed) of
    (Right a, Right b) -> a == b
    _ -> recorded == computed

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
