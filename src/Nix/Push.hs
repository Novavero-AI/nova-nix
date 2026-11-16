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
--
-- == Uploading in parts
--
-- A proxy in front of a cache can refuse a request body over its own
-- limit, as Cloudflare in front of the public cache refuses one over
-- 100 MB with 413.  A NAR up to 'pcPartSize' goes in one
-- @PUT nar\/\<file\>@; a larger one goes through nova-cache's multipart
-- upload: @POST nar\/\<file\>\/uploads@ begins it, each
-- @PUT nar\/\<file\>\/uploads\/\<id\>\/\<n\>@ sends one part in order,
-- and @POST nar\/\<file\>\/uploads\/\<id\>@ completes it with the part
-- count and the total size.  A cache that answers the beginning with
-- 404, as a nova-cache without multipart upload does, gets the NAR in
-- one @PUT@.
--
-- The retries follow nova-cache's rule for a client.  A part, or the
-- request that begins an upload, is sent again under 'pcRetry' when it
-- fails as upstream's transfer layer retries a transfer, its response
-- lost or an error status such as 503 ('Nix.Http.exceptionFailure',
-- 'Nix.Http.statusError'): the server replaces a part it holds, and a
-- repeated beginning only leaves an empty upload it reclaims.  A part
-- answered 404 or 409 means the server no longer holds the parts before
-- it, so a new upload begins.  A completion answered with anything but
-- 200, or not at all, may still have joined the NAR, so the push goes
-- on and the narinfo decides: the server checks its FileSize and
-- FileHash against the NAR it holds, and when it refuses the narinfo,
-- the NAR is uploaded again.  A push stops after a fixed number of
-- uploads of one NAR, so a cache that keeps losing parts cannot hold it
-- in a loop.
--
-- The NAR is held whole, as for one @PUT@, and each part is a slice of
-- it.  Holding one part at a time would need a streaming zstd encoder,
-- which nova-cache's one-shot 'Zstd.compress' is not, and two passes
-- over the path, since an upload is begun under the object's name and a
-- zstd object is named by the hash of the whole compressed file.
module Nix.Push
  ( -- * Configuration
    PushConfig (..),
    PushCompression (..),
    PushSummary (..),
    PartSize,
    mkPartSize,
    partSizeBytes,
    defaultPartSize,

    -- * Pushing
    pushPaths,
    computeClosure,
    preparePath,
    loadApiKeyFile,

    -- * Pure pieces (exported for tests)
    parsePushCompression,
    pushCompressionValues,
    mkNarInfo,
    mkPushArtifact,
    PushArtifact (..),
    planMissing,
    isDerivationPath,
    outputPathsOnly,
    narFileName,
    stripHashPrefix,
    storePathBasename,
    checkRecordedNarHash,
    narHashMatches,
    splitParts,
  )
where

import Control.Exception (IOException, try)
import Control.Monad (foldM, forM, forM_, unless, when)
import Control.Monad.Except (ExceptT (..), liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.Char (isDigit)
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
import Nix.Http (AttemptFailure (..), FetchRetryPolicy, TransferError (..), attemptFailureMessage, catchSync, exceptionFailure, ioRetryEffects, retryTransient, statusError, withTransfer, withUserAgent)
import Nix.Store (Store (..), queryDeriver, queryPathInfo, queryReferences, volumeCaseHack)
import qualified Nix.Store.DB as DB
import qualified Nix.Store.ExecBit as ExecBit
import Nix.Store.Path (StorePath (spHash, spName), defaultStoreDir, parseStorePath, storePathBasename, storePathToFilePath, storePathToText)
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

-- | The most characters of a server's error message an error quotes.
errorBodyChars :: Int
errorBodyChars = 200

-- | Path segment under @nar\/\<file\>@ where nova-cache keeps the file's
-- multipart uploads.
uploadsSegment :: Text
uploadsSegment = "uploads"

-- | Keys of the body that completes a multipart upload, as nova-cache
-- reads them.
partsField, fileSizeField :: Text
partsField = "Parts"
fileSizeField = "FileSize"

-- | Hex digits in the id nova-cache issues for an upload: 128 random
-- bits, in lowercase.
uploadIdDigits :: Int
uploadIdDigits = 32

-- | The most uploads of one NAR a push begins.  A part answered 404 or
-- 409, or a narinfo refused after a completion that was not answered
-- with 200, begins one more; past this many the push fails, so a cache
-- that keeps losing parts cannot hold it in a loop.
maxNarUploads :: Int
maxNarUploads = 3

-- ---------------------------------------------------------------------------
-- Configuration and results
-- ---------------------------------------------------------------------------

-- | How each NAR is packaged for upload.  A property of the
-- destination cache, as upstream models compression on binary cache
-- stores - not a per-path whim.
data PushCompression = PushNone | PushZstd
  deriving (Eq, Show)

-- | The accepted @--compression@ spellings, for parser errors and the
-- CLI help line - one rendering of the register in "Nix.Compression".
pushCompressionValues :: Text
pushCompressionValues = compressionNameNone <> ", " <> compressionNameZstd

-- | Parse the CLI spelling of a push compression.  The spellings are
-- the narinfo @Compression@ names from "Nix.Compression", so the CLI
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
    pcCompression :: !PushCompression,
    -- | The most bytes of a NAR one request carries: a NAR up to this
    -- size goes in one @PUT@, a larger one in parts of this size.  A
    -- property of the destination, whose proxy sets the limit.
    pcPartSize :: !PartSize,
    -- | How often a request of a multipart upload that is safe to send
    -- again is tried, and the wait between tries.
    pcRetry :: !FetchRetryPolicy
  }
  deriving (Eq, Show)

-- | The most bytes of a NAR one request carries, at least one.  The
-- constructor is not exported, so every value is 'defaultPartSize' or
-- one 'mkPartSize' accepted.
newtype PartSize = PartSize Int
  deriving (Eq, Show)

-- | A part size of the given number of bytes; 'Nothing' below one, where
-- no part could carry any of a NAR.
mkPartSize :: Int -> Maybe PartSize
mkPartSize bytes
  | bytes >= 1 = Just (PartSize bytes)
  | otherwise = Nothing

-- | The bytes a part size allows.
partSizeBytes :: PartSize -> Int
partSizeBytes (PartSize bytes) = bytes

-- | 64 MiB.  Cloudflare in front of the public cache refuses a request
-- body over 100 MB with 413, and 64 MiB is under that whether the limit
-- counts 10^8 bytes or 100 MiB.  nova-cache holds a NAR to 4 GiB and an
-- upload to 10,000 parts, so a NAR at its cap takes 64 parts of this
-- size.
defaultPartSize :: PartSize
defaultPartSize = PartSize (64 * 1024 * 1024)

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
    Left (e :: IOException) -> Left ("cannot read key file: " <> T.pack (show e))
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

-- | Whether a store path is a derivation: a recipe, not an output.
isDerivationPath :: StorePath -> Bool
isDerivationPath sp = derivationExtension `T.isSuffixOf` spName sp

-- | Every valid path that is not a derivation: outputs and sources alike.
-- A store registers its derivations as valid paths like anything else, but
-- a binary cache serves build outputs only: nova-cache refuses a narinfo
-- for a @.drv@ outright, and no substituter asks a cache for a recipe it
-- can rebuild from the expression.  @push --all@ therefore means every
-- valid path except derivations.
outputPathsOnly :: [StorePath] -> [StorePath]
outputPathsOnly = filter (not . isDerivationPath)

-- | The suffix that marks a derivation's store path.
derivationExtension :: Text
derivationExtension = ".drv"

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
-- hash, NAR size - comes from the one t'PushArtifact', so the narinfo
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

-- | A NAR's bytes as the parts of a multipart upload, in order: each
-- holds the part size but the last, which holds the rest.  A part is a
-- slice of the bytes, not a copy.
splitParts :: PartSize -> BS.ByteString -> [BS.ByteString]
splitParts size bytes
  | BS.null bytes = []
  | otherwise = part : splitParts size rest
  where
    (part, rest) = BS.splitAt (partSizeBytes size) bytes

-- ---------------------------------------------------------------------------
-- Push orchestration
-- ---------------------------------------------------------------------------

-- | Push the closure of the given paths to the cache.
--
-- Serializes one NAR at a time (bounded memory), uploads all NARs before
-- any narinfo, and finishes with a signed round-trip check.  Any failure
-- aborts with an error; a partial push leaves only orphaned NARs behind,
-- which are invisible to clients.  An interrupt is not a failure: it
-- stops the push and propagates.
pushPaths :: PushConfig -> Store -> [StorePath] -> IO (Either Text PushSummary)
pushPaths cfg store roots =
  -- Every synchronous exception: the push raises sqlite-simple's from the
  -- database and IO errors from the serialiser and the log, and each is a
  -- failed push to report.
  runExceptT run `catchSync` \e -> pure (Left ("push failed: " <> T.pack (show e)))
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
      uploaded <- forM missing $ \sp -> do
        (narInfo, upload) <- uploadNar manager cfg store 0 sp
        pure (sp, narInfo, upload)
      -- Phase 2: narinfos, only after every NAR is in place, in the
      -- closure's deps-first order: a freshly announced path's references
      -- are always already announced.  A NAR whose upload was not
      -- confirmed is checked here, by its narinfo, and not in phase 1,
      -- since a narinfo accepted there would announce a path before its
      -- references.
      forM_ uploaded $ \(sp, narInfo, upload) -> do
        publishNarInfo manager cfg store sp narInfo upload
        logLine ("[push]  " <> storePathBasename sp)
      -- Round-trip: the first pushed narinfo must come back signed.
      case uploaded of
        ((sp, _, _) : _) -> verifySignedRoundTrip manager cfg sp
        [] -> pure ()
      pure PushSummary {psPushed = length uploaded, psSkipped = skipped}

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
  expectOk ("GET " <> narinfoHashesEndpoint) response
  pure (Set.fromList (filter (not . T.null) (map T.strip (T.lines (responseText response)))))

-- | Serialize a store path to a NAR, cross-check the database NAR hash,
-- and upload the bytes, given how many uploads of the NAR this push has
-- begun before.  Returns the narinfo to publish in phase 2, and whether
-- the cache confirmed the NAR.
uploadNar :: HTTP.Manager -> PushConfig -> Store -> Int -> StorePath -> ExceptT Text IO (NarInfo, NarUpload)
uploadNar manager cfg store begun sp = do
  (artifact, narInfo) <- preparePath (pcCompression cfg) store sp
  logLine
    ( "[nar]   "
        <> storePathBasename sp
        <> " ("
        <> T.pack (show (paNarSize artifact))
        <> " bytes)"
    )
  upload <- sendNar manager cfg begun (niUrl narInfo) (paBytes artifact)
  pure (narInfo, upload)

-- | What a push uploads for one store path: its NAR, packaged as the
-- compression asks, and the narinfo that describes it.  The NAR is
-- serialized from disk and held to the NAR hash the database recorded
-- ('checkRecordedNarHash'); the references and the deriver come from the
-- path's row.
preparePath :: PushCompression -> Store -> StorePath -> ExceptT Text IO (PushArtifact, NarInfo)
preparePath compression store sp = do
  let physicalPath = storePathToFilePath (stDir store) sp
  narEntry <- liftIO (ExecBit.serialiseFromPath (volumeCaseHack (stCaseSensitivity store)) physicalPath)
  let narBytes = NAR.serialise narEntry
      narHash = Hash.formatNixHash (Hash.hashBytes narBytes)
  recorded <- liftIO (queryPathInfo (stDB store) sp)
  liftEither (checkRecordedNarHash recorded narHash sp)
  refTexts <- liftIO (queryReferences (stDB store) sp)
  refs <- liftEither (traverse (parseRefText store) refTexts)
  deriverText <- liftIO (queryDeriver (stDB store) sp)
  let deriver = deriverText >>= parseStorePath (stDir store)
      artifact = mkPushArtifact compression narHash narBytes
  pure (artifact, mkNarInfo artifact sp refs deriver)

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

-- | Recorded and computed NAR hashes match when both parse as
-- @sha256:@ and nix-base32 and their decoded digests agree.  A recorded
-- value that does not parse, such as a foreign cache's base16, falls
-- back to exact text equality, keeping legacy rows comparable rather
-- than un-checkable.
narHashMatches :: Text -> Text -> Bool
narHashMatches recorded computed =
  case (Hash.parseNixHash recorded, Hash.parseNixHash computed) of
    (Right a, Right b) -> a == b
    _ -> recorded == computed

-- | Upload a rendered narinfo (the server validates and signs it).  The
-- server checks a narinfo's FileSize and FileHash against the NAR it
-- holds and refuses one whose NAR it does not hold, or holds otherwise,
-- with 400.  After a NAR's upload was not confirmed, a 400 is that
-- refusal, so the NAR is uploaded again, while fewer than
-- 'maxNarUploads' uploads of it have been begun, and the narinfo is
-- sent after it.
publishNarInfo :: HTTP.Manager -> PushConfig -> Store -> StorePath -> NarInfo -> NarUpload -> ExceptT Text IO ()
publishNarInfo manager cfg store sp narInfo upload = do
  let body = TE.encodeUtf8 (renderNarInfo narInfo)
  response <- httpRequest manager "PUT" (cacheUrl cfg object) (authHeaders cfg) (Just body)
  let status = HTTP.responseStatus response
      refused = describeResponse label response
  unless (status == HTTP.status200) $ case upload of
    NarUnconfirmed begun completion
      | status == HTTP.status400 && begun < maxNarUploads -> do
          logLine ("[retry] " <> refused <> "; uploading the NAR again")
          (resent, reupload) <- uploadNar manager cfg store begun sp
          publishNarInfo manager cfg store sp resent reupload
      | otherwise ->
          throwError (refused <> "; the NAR's last completion: " <> completion <> " (" <> countedUploads begun <> " begun)")
    NarStored -> throwError refused
  where
    object = spHash sp <> narInfoExtension
    label = "PUT " <> object

-- | Fetch a just-pushed narinfo back and require a signature on it.
verifySignedRoundTrip :: HTTP.Manager -> PushConfig -> StorePath -> ExceptT Text IO ()
verifySignedRoundTrip manager cfg sp = do
  let object = spHash sp <> narInfoExtension
      url = pcCacheUrl cfg <> "/" <> object
  response <- httpRequest manager "GET" url [] Nothing
  expectOk ("GET " <> object) response
  narInfo <- liftEither (either (Left . T.pack) Right (parseNarInfo (responseText response)))
  when (null (niSigs narInfo)) $
    throwError ("round-trip check failed: " <> object <> " came back unsigned")
  logLine "[ok]    round-trip verified, signature present"

-- ---------------------------------------------------------------------------
-- Sending a NAR
-- ---------------------------------------------------------------------------

-- | Whether the cache is known to hold a NAR once its upload ends.
data NarUpload
  = -- | The cache answered the upload with 200.
    NarStored
  | -- | A multipart upload's completion was answered with something
    -- other than 200, or not at all, so the NAR may or may not be in
    -- place, and its narinfo decides.  (uploads of the NAR begun so far,
    -- the completion's answer)
    NarUnconfirmed !Int !Text

-- | An upload, by the id nova-cache issued for it: 'uploadIdDigits'
-- lowercase hex digits, checked before the id goes into a request path.
newtype UploadId = UploadId Text

-- | The id in a response that begins an upload, or 'Nothing' for text
-- that is not one nova-cache issues.
parseUploadId :: Text -> Maybe UploadId
parseUploadId txt
  | T.length txt == uploadIdDigits && T.all isLowerHex txt = Just (UploadId txt)
  | otherwise = Nothing
  where
    isLowerHex c = isDigit c || (c >= 'a' && c <= 'f')

-- | How the cache answered the request that begins an upload.
data Begun
  = -- | 201, with the upload's id.
    Begun !UploadId
  | -- | 404: the cache has no multipart upload.
    NoMultipart

-- | How the cache answered the parts of an upload sent so far.
data PartsAnswer
  = -- | 200 to each: the cache holds them.
    PartsHeld
  | -- | 404 or 409 to the last: the cache no longer holds the parts
    -- before it, so the upload cannot be completed.  (the answer)
    PartsLost !Text

-- | Send a NAR's bytes to its object: in one @PUT@ when they fit in one
-- part, in parts otherwise.
sendNar :: HTTP.Manager -> PushConfig -> Int -> Text -> BS.ByteString -> ExceptT Text IO NarUpload
sendNar manager cfg begun object bytes
  | BS.length bytes <= partSizeBytes (pcPartSize cfg) = putWholeNar manager cfg object bytes
  | otherwise = uploadInParts manager cfg begun object bytes

-- | Send a NAR's bytes in one @PUT@.
putWholeNar :: HTTP.Manager -> PushConfig -> Text -> BS.ByteString -> ExceptT Text IO NarUpload
putWholeNar manager cfg object bytes = do
  response <- httpRequest manager "PUT" (cacheUrl cfg object) (authHeaders cfg) (Just bytes)
  NarStored <$ expectOk ("PUT " <> object) response

-- | Send a NAR's bytes through a multipart upload, given how many uploads
-- of it this push has begun before: begin one, send the parts, and
-- complete it.  An upload whose parts the cache lost is begun again, up
-- to 'maxNarUploads' in all.
uploadInParts :: HTTP.Manager -> PushConfig -> Int -> Text -> BS.ByteString -> ExceptT Text IO NarUpload
uploadInParts manager cfg begun object bytes = do
  started <- beginUpload manager cfg uploads
  case started of
    NoMultipart -> do
      logLine ("[nar]   POST " <> uploads <> " returned HTTP 404, so the NAR goes in one request")
      putWholeNar manager cfg object bytes
    Begun (UploadId uploadId) -> do
      let upload = uploads <> "/" <> uploadId
      answer <- sendParts manager cfg upload parts
      case answer of
        PartsHeld -> completeUpload manager cfg (begun + 1) upload (length parts) (BS.length bytes)
        PartsLost lost
          | begun + 1 < maxNarUploads -> do
              logLine ("[retry] " <> lost <> "; beginning a new upload")
              uploadInParts manager cfg (begun + 1) object bytes
          | otherwise ->
              throwError (lost <> " (" <> countedUploads maxNarUploads <> " begun)")
  where
    uploads = object <> "/" <> uploadsSegment
    parts = splitParts (pcPartSize cfg) bytes

-- | Begin an upload, again under the retry policy while the response is
-- lost or is one a retry could outlive.
beginUpload :: HTTP.Manager -> PushConfig -> Text -> ExceptT Text IO Begun
beginUpload manager cfg uploads =
  retrying cfg (answer <$> tryRequest manager "POST" (cacheUrl cfg uploads) (authHeaders cfg) Nothing)
  where
    label = "POST " <> uploads
    answer (Left err) = Left (transportFailure label err)
    answer (Right response)
      | status == HTTP.status201 =
          maybe
            (Left (FatalFailure (label <> " answered with no upload id: " <> T.take errorBodyChars (responseText response))))
            (Right . Begun)
            (parseUploadId (T.strip (responseText response)))
      | status == HTTP.status404 = Right NoMultipart
      | otherwise = Left (statusFailure label response)
      where
        status = HTTP.responseStatus response

-- | Send the parts in order from part 1, stopping at the first the cache
-- answers with 'PartsLost'.
sendParts :: HTTP.Manager -> PushConfig -> Text -> [BS.ByteString] -> ExceptT Text IO PartsAnswer
sendParts manager cfg upload = go . zip [1 :: Int ..]
  where
    go [] = pure PartsHeld
    go ((number, part) : rest) = do
      answer <- sendPart manager cfg (upload <> "/" <> T.pack (show number)) part
      case answer of
        PartsHeld -> go rest
        lost@(PartsLost _) -> pure lost

-- | Send one part, again under the retry policy while the response is
-- lost or is one a retry could outlive: the cache replaces a part it
-- holds, so a part that did arrive is not doubled.
sendPart :: HTTP.Manager -> PushConfig -> Text -> BS.ByteString -> ExceptT Text IO PartsAnswer
sendPart manager cfg path part =
  retrying cfg (answer <$> tryRequest manager "PUT" (cacheUrl cfg path) (authHeaders cfg) (Just part))
  where
    label = "PUT " <> path
    answer (Left err) = Left (transportFailure label err)
    answer (Right response)
      | status == HTTP.status200 = Right PartsHeld
      | status `elem` [HTTP.status404, HTTP.status409] = Right (PartsLost (describeResponse label response))
      | otherwise = Left (statusFailure label response)
      where
        status = HTTP.responseStatus response

-- | Complete an upload, declaring how many parts it holds and how many
-- bytes they join to.  Never sent again here: the server joins the whole
-- NAR before it answers, so an answer other than 200, or none, leaves
-- the NAR unconfirmed for its narinfo to decide, and fails nothing.  The
-- HTTP client itself sends a request once more when a connection it kept
-- open turns out closed before any answer; a completion that arrives
-- twice gets an error or joins the same parts, and the narinfo decides
-- either way.
completeUpload :: HTTP.Manager -> PushConfig -> Int -> Text -> Int -> Int -> ExceptT Text IO NarUpload
completeUpload manager cfg begun upload count size = do
  answer <- liftIO (tryRequest manager "POST" (cacheUrl cfg upload) (authHeaders cfg) (Just totals))
  case answer of
    Right response | HTTP.responseStatus response == HTTP.status200 -> pure NarStored
    unconfirmed -> do
      let completion = either (attemptFailureMessage . transportFailure label) (describeResponse label) unconfirmed
      logLine ("[nar]   " <> completion <> "; the narinfo decides whether the NAR is stored")
      pure (NarUnconfirmed begun completion)
  where
    label = "POST " <> upload
    totals =
      TE.encodeUtf8
        ( T.unlines
            [ partsField <> ": " <> T.pack (show count),
              fileSizeField <> ": " <> T.pack (show size)
            ]
        )

-- ---------------------------------------------------------------------------
-- HTTP plumbing
-- ---------------------------------------------------------------------------

-- | Issue an HTTP request with optional body.  The body is read through
-- 'withTransfer', so a connection lost mid-body is an
-- 'HTTP.HttpException' like every other failure of the request, and that
-- is the one exception caught.
tryRequest ::
  HTTP.Manager ->
  BS.ByteString ->
  Text ->
  [(HTTP.HeaderName, BS.ByteString)] ->
  Maybe BS.ByteString ->
  IO (Either HTTP.HttpException (HTTP.Response BS.ByteString))
tryRequest manager method url headers body = try $ do
  request0 <- HTTP.parseRequest (T.unpack url)
  let request =
        withUserAgent
          request0
            { HTTP.method = method,
              HTTP.requestHeaders = headers,
              HTTP.requestBody = maybe (HTTP.requestBody request0) HTTP.RequestBodyBS body
            }
  withTransfer request manager $ \response -> do
    chunks <- HTTP.brConsume (HTTP.responseBody response)
    pure (BS.concat chunks <$ response)

-- | 'tryRequest', with a failure of the request as a push error.
httpRequest ::
  HTTP.Manager ->
  BS.ByteString ->
  Text ->
  [(HTTP.HeaderName, BS.ByteString)] ->
  Maybe BS.ByteString ->
  ExceptT Text IO (HTTP.Response BS.ByteString)
httpRequest manager method url headers body =
  ExceptT (first failed <$> tryRequest manager method url headers body)
  where
    failed = attemptFailureMessage . exceptionFailure (\detail -> "HTTP error for " <> url <> ": " <> T.pack detail)

-- | Run a request that is safe to send again under the push's retry
-- policy: a 'TransientFailure' is sent again after upstream's backoff,
-- which upstream's transfer layer applies to an upload as to a download
-- (@filetransfer.cc@ at 2.24.9, lines 421-509), and a 'FatalFailure', or
-- the last attempt's failure, fails the push.
retrying :: PushConfig -> IO (Either AttemptFailure a) -> ExceptT Text IO a
retrying cfg attempt =
  ExceptT (first attemptFailureMessage <$> retryTransient (pcRetry cfg) ioRetryEffects attempt)

-- | A response the protocol gives no meaning, as an attempt's failure:
-- transient when it is an error upstream's transfer layer retries
-- ('statusError'), fatal when it is any other error or no error at all.
statusFailure :: Text -> HTTP.Response BS.ByteString -> AttemptFailure
statusFailure label response
  | not (HTTP.statusIsClientError status || HTTP.statusIsServerError status) = FatalFailure message
  | otherwise = case statusError status of
      Transient -> TransientFailure message
      NotFound -> FatalFailure message
      Forbidden -> FatalFailure message
      Misc -> FatalFailure message
  where
    status = HTTP.responseStatus response
    message = describeResponse label response

-- | A request that got no response, as an attempt's failure, classified
-- as upstream classifies a transfer's ('exceptionFailure'), with a
-- message naming the request and the client's failure.
transportFailure :: Text -> HTTP.HttpException -> AttemptFailure
transportFailure label = exceptionFailure (\detail -> label <> " failed: " <> T.pack detail)

-- | A count of uploads with its noun.
countedUploads :: Int -> Text
countedUploads 1 = "1 upload"
countedUploads n = T.pack (show n) <> " uploads"

-- | A response as an error: the request, the status, and the start of
-- the server's message.
describeResponse :: Text -> HTTP.Response BS.ByteString -> Text
describeResponse label response =
  label
    <> " returned HTTP "
    <> T.pack (show (HTTP.statusCode (HTTP.responseStatus response)))
    <> ": "
    <> T.take errorBodyChars (responseText response)

-- | The URL of a path under the cache.
cacheUrl :: PushConfig -> Text -> Text
cacheUrl cfg path = pcCacheUrl cfg <> "/" <> path

-- | Authorization headers for authenticated writes.
authHeaders :: PushConfig -> [(HTTP.HeaderName, BS.ByteString)]
authHeaders cfg = case pcApiKey cfg of
  Nothing -> []
  Just key -> [("Authorization", bearerPrefix <> TE.encodeUtf8 key)]

-- | Require a 200 response, with a readable error otherwise.
expectOk :: Text -> HTTP.Response BS.ByteString -> ExceptT Text IO ()
expectOk label response =
  unless (HTTP.responseStatus response == HTTP.status200) $
    throwError (describeResponse label response)

-- | The response body as text, decoded leniently: an error body need not
-- be UTF-8, and what the protocol answers otherwise is ASCII.
responseText :: HTTP.Response BS.ByteString -> Text
responseText = TE.decodeUtf8With (\_ _ -> Just '\xFFFD') . HTTP.responseBody

-- | Parse a reference path text from the database.
parseRefText :: Store -> Text -> Either Text StorePath
parseRefText store txt = case parseStorePath (stDir store) txt of
  Just sp -> Right sp
  Nothing -> Left ("unparseable reference in store DB: " <> txt)

-- | Progress line on stderr (stdout stays reserved for results).
logLine :: Text -> ExceptT Text IO ()
logLine = liftIO . TIO.hPutStrLn stderr
