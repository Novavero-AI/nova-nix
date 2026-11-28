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
-- 5. @GET https:\/\/cache.example.com\/\<URL\>@, the file the narinfo's
--    @URL@ field names, and, in one bounded streaming pass, decompress,
--    hash, parse, and unpack into the store path
-- 6. Verify the declared NAR hash and size against the streamed bytes,
--    then re-verify the materialized tree from disk and hold it to the
--    content address the narinfo states ('checkStatedAddress')
-- 7. Register in the store DB with references from narinfo
--
-- A build reads a path's narinfo before anything is downloaded, to realise
-- the references it declares first ('queryReferencesWith'), and then
-- fetches the path from the cache that served that narinfo, without asking
-- again ('fetchSubstituteWith').  Each cache that answers is asked about
-- each path once in a run, so a closure costs one narinfo request a path
-- from each cache it asks, and a query that failed is made again.  A path
-- that cache fails to deliver goes to the caches after it, their
-- references first ('queryNextOfferWith').
--
-- A cache that lacks the path (404, or another status upstream reads as
-- the file not being there) is passed over, and what no cache holds
-- builds locally.  A cache that fails is told apart from one that lacks
-- the path, as upstream tells them apart: a failed narinfo query and a
-- substitute that fails to arrive stop the build, unless
-- 'Nix.Realise.BuildFromSource' (upstream's @fallback@) says to build
-- instead ("Nix.Realise").
--
-- The fetch runs under an exclusive per-path lock
-- ("Nix.Store.Lock"), upstream's pathlocks protocol: taken once the
-- narinfo is judged and before any deletion or NAR download, validity
-- re-checked under it (another process's
-- finished path is adopted without touching disk), and held until the
-- caller's registration transaction commits.
--
-- == Caches in a directory
--
-- A @file:\/\/@ URL names a cache in a directory on this machine, as
-- upstream's local binary cache store reads one: the layout a push writes
-- there ("Nix.Push.Directory"), read through the same steps, with each
-- request a file read instead.  A narinfo or NAR file that is not there is
-- the cache lacking it, as a 404 is; any other failure to read one fails
-- as a failed transfer does, though once, since upstream retries only
-- downloads.  A file a narinfo names must lie below the directory
-- ('cacheFilePath').  The first query of a run opens each such cache as
-- upstream opens it ('openCacheDirectory'): one that cannot be opened, or
-- whose @nix-cache-info@ names another store directory, is passed over for
-- the run after a warning, as upstream drops a substituter it cannot
-- open, and one whose @Priority@ does not read fails the query whatever
-- the fallback, as it fails upstream's command.
--
-- == Cache priority
--
-- Multiple caches can be configured, checked in priority order:
--
-- @
-- substituters = https:\/\/cache.novavero.ai file:\/\/\/srv\/cache
-- trusted-public-keys = cache.novavero.ai-1:... cache.example.org-1:...
-- @
--
-- A @file:\/\/@ cache is asked at the priority its @nix-cache-info@
-- states, or at upstream's default of 0 when it states none, as upstream
-- reads that file for a cache's priority; caches of equal priority are
-- asked in the order configured.  An HTTP cache's @nix-cache-info@ is not
-- read: it is asked at the priority its t'CacheConfig' gives, where
-- upstream asks at the one the file states (cache.nixos.org states 40).
-- So among HTTP caches the configured priorities decide alone, and a
-- @file:\/\/@ cache stating a priority between an HTTP cache's stated one
-- and its configured one is asked on the other side of it from where
-- upstream asks it.
--
-- Our nova-cache server implements this protocol.  The narinfo format,
-- NAR serialization, signature verification - all handled by the
-- @nova-cache@ library.  This module orchestrates the requests and
-- builds each path's registration, which "Nix.Builder" records.
module Nix.Substituter
  ( -- * Substituting a closure
    SubstResult (..),
    Substitute,
    substitutePath,
    substituteCache,
    CacheMemo,
    emptyCacheMemo,
    queryReferencesWith,
    queryNextOfferWith,
    fetchSubstituteWith,

    -- * Cache configuration
    CacheConfig (..),
    defaultCacheConfig,

    -- * Failure classification
    AttemptFailure (..),
    attemptFailureMessage,
    narStreamFailure,
    withXzAttempt,
    catchSync,
    compressedBodyCeiling,
    downloadCapFor,

    -- * Verified NAR bytes
    VerifiedNar,
    verifyNar,
    verifiedNarBytes,
    verifiedNarDigest,

    -- * Pure helpers (exported for testing)
    maxNarInfoBody,
    readBodyCapped,
    validateNarInfoFields,
    NarInfoAnswer (..),
    judgeNarInfo,
    verifySigs,
    verifyNarHash,
    verifyNarSize,
    checkStatedAddress,
    narInfoMatchesPath,
    decompressorFor,
    decompressNar,
    streamingDecompressionSupported,
    withDecompressedSource,
    cappedBodySource,
    materializeNarFromSource,
    consumeNarStream,
    unpackAndVerify,
    clearStaleDestination,
    parseReferences,
    parseDeriver,
  )
where

import Control.Exception (Exception, IOException, SomeException, bracket, catch, catches, displayException, onException, throwIO, tryJust)
import Control.Monad (guard, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (StateT, gets, modify')
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.Either (isRight)
import Data.Foldable (traverse_)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word64)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS
import qualified Network.HTTP.Types.Status as HTTP
import Nix.CacheDirectory (CacheInfo (..), CacheOpenFailure (..), cacheFilePath, fileUrlDirectory, openCacheDirectory)
import Nix.Compression (NarCompression (..), parseNarCompression)
import Nix.Daemon.Protocol (ContentAddress, ContentAddressMethod (..), Signature, caAlgo, caDigest, caMethod, parseContentAddress, parseSignature)
import Nix.Hash (FileIngestionMethod (..), ModuloHash, moduloHashFinalize, moduloHashInit, moduloHashUpdate)
import Nix.Http (AttemptFailure (..), FetchRetryPolicy, RetryEffects, TransferError (..), attemptFailureMessage, catchSync, fetchStatusFailure, retryTransient, statusError, transferFailureHandlers, withTransfer, withUserAgent)
import Nix.Logger (Logger (..), logWarning)
import Nix.Realise (Fallback (..), Offer (..))
import Nix.Store (CaseSensitivity, NarStreamFailure (..), PathLock, Store (..), UpstreamHazard (..), abortNarUnpack, acquirePathLock, canonicalizeMetadata, finishNarUnpack, isValid, newNarUnpackSink, releasePathLock, sinkNarStream, unpackNarEntry, upstreamManagedRefusal, volumeCaseHack)
import Nix.Store.DB (PathRegistration (..), Ultimate (..))
import qualified Nix.Store.ExecBit as ExecBit
import Nix.Store.Path (StoreDir, StorePath (spHash, spName), parseStorePathBaseName, storePathHashLen, storePathToFilePath, storePathToText)
import qualified NovaCache.Base32 as Nix32
import qualified NovaCache.Bzip2 as Bzip2
import qualified NovaCache.Hash as Hash
import qualified NovaCache.NAR as NAR
import qualified NovaCache.NarInfo as NarInfo
import qualified NovaCache.Signing as Signing
import qualified NovaCache.Validate as Validate
import qualified NovaCache.Xz as Xz
import qualified NovaCache.Zstd as Zstd
import qualified System.Directory as Dir
import System.IO (IOMode (ReadMode), hClose, openBinaryFile, withBinaryFile)
import System.IO.Error (isDoesNotExistError)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Configuration for a binary cache.
data CacheConfig = CacheConfig
  { -- | URL of the cache: the base URL of an HTTP cache (e.g.
    -- @https:\/\/cache.novavero.ai@), whose trailing slashes are dropped
    -- when the cache is opened, as upstream drops them, or the @file:\/\/@
    -- URL of a directory (e.g. @file:\/\/\/srv\/cache@, or
    -- @file:\/\/\/D:\/cache@ on Windows).
    ccUrl :: !Text,
    -- | Trusted public keys (@name:base64key@).  A narinfo is accepted
    -- when ANY of them verifies a signature, matching upstream's flat
    -- @trusted-public-keys@ set: the keys are not bound to a particular
    -- substituter, and one signed by any trusted key is enough.
    ccPublicKeys :: ![Text],
    -- | Priority (lower = checked first) of an HTTP cache;
    -- 'defaultCacheConfig' gives cache.nixos.org 40.  A @file:\/\/@ cache
    -- is asked at the priority its @nix-cache-info@ states instead.
    ccPriority :: !Int
  }
  deriving (Eq, Show)

-- | Where a cache's files are read.
data CacheTransport
  = -- | Over HTTP, below the cache's URL.
    HttpTransport
  | -- | From the directory the cache's @file:\/\/@ URL names.
    DirectoryTransport !FilePath
  deriving (Eq, Show)

-- | A cache as a run asks it: its configuration, with the priority it is
-- asked at, and where its files are read.  Only opening a cache makes one
-- ('openCache').
data OpenedCache = OpenedCache !CacheConfig !CacheTransport
  deriving (Eq, Show)

-- | The configuration of an opened cache.
openedConfig :: OpenedCache -> CacheConfig
openedConfig (OpenedCache cache _) = cache

-- | Default cache configuration for cache.nixos.org.
defaultCacheConfig :: CacheConfig
defaultCacheConfig =
  CacheConfig
    { ccUrl = "https://cache.nixos.org",
      ccPublicKeys = ["cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="],
      ccPriority = 40
    }

-- | Result of fetching a path from the cache that offered it
-- ('fetchSubstituteWith').
data SubstResult
  = -- | Verified and unpacked on disk, NOT yet registered: the carried
    -- registration is recorded by the caller, which batches every output
    -- of a derivation into one 'Nix.Store.DB.registerPaths' transaction so
    -- cross-output reference edges are never dropped.  The path's lock
    -- rides along STILL HELD, because the exclusion must survive until
    -- that transaction commits - released earlier, another process could
    -- meet the unpacked-but-unregistered window and delete the tree the
    -- row is about to describe.  The caller releases it after
    -- registration, on every exit path ("Nix.Builder").
    SubstSuccess !PathRegistration !PathLock
  | -- | The path was already valid under its lock - another process
    -- registered it while this one waited - so its work is adopted:
    -- nothing was downloaded, nothing touched disk, and no registration
    -- is needed.  The caller treats this as success.  No lock rides
    -- along; it was released before returning.
    SubstAlreadyValid
  | -- | The cache no longer holds the path's NAR, which upstream passes
    -- over without failing the substitution (@SubstituteGone@).
    SubstNotFound
  | -- | The cache failed to deliver the path: the download or its
    -- verification.  Upstream's failed substitution.  Or the store is one
    -- upstream Nix's database may also manage, refused before the NAR is
    -- asked for ("Nix.Store.Upstream").
    SubstError !Text
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Main substitution logic
-- ---------------------------------------------------------------------------

-- | A path as one cache offers it: the path, that cache, the narinfo it
-- served for the path, and the caches after it in priority order.  The
-- narinfo validated, describes the path and a trusted key signed it
-- ('judgeNarInfo'), and the references realised before the path arrives are
-- the ones it declares.  The constructor is not exported: only the reference
-- queries make one ('queryReferencesWith', 'queryNextOfferWith'), so a fetch
-- is never handed a narinfo that skipped those checks or one checked for
-- another path, and the NAR and the references always come from the same
-- cache, as upstream's substitution goal copies a path from the substituter
-- whose path info it queried.
data Substitute = Substitute !StorePath !OpenedCache !NarInfo.NarInfo ![OpenedCache]
  deriving (Eq, Show)

-- | The path a t'Substitute' fetches.
substitutePath :: Substitute -> StorePath
substitutePath (Substitute sp _ _ _) = sp

-- | The cache a t'Substitute' comes from, with the priority it was asked
-- at.
substituteCache :: Substitute -> CacheConfig
substituteCache (Substitute _ cache _ _) = openedConfig cache

-- | What the caches have said in one run.  Each @file:\/\/@ cache is opened
-- once a run, where upstream opens each substituter once a process, so a
-- command that makes several runs opens it, and warns of one it passes
-- over, in each.  Each cache that answers about a path is asked once: the
-- narinfo it served, which validated, or 'Nothing' when it does not hold
-- the path.  A query that
-- failed is not remembered, so the next one asks again, and neither is a
-- cache whose @Priority@ did not read.  Upstream's per-store path-info cache
-- keeps the same: the info a substituter returned, a path it lacks
-- included, and never a failure, which is thrown before the cache is
-- written.  Its entries expire after @narinfo-cache-positive-ttl@ and
-- @narinfo-cache-negative-ttl@ (a month and an hour) and are also kept on
-- disk across runs; these last one run.
data CacheMemo = CacheMemo
  { -- | What opening each @file:\/\/@ cache found, by its URL.
    memoDirectories :: !(Map Text DirectoryOpening),
    -- | What each cache said about each path, by narinfo URL.
    memoNarInfos :: !(Map Text (Maybe NarInfo.NarInfo))
  }

-- | What opening a @file:\/\/@ cache found.
data DirectoryOpening
  = -- | The cache's directory, and the priority it is asked at.
    DirectoryOpened !FilePath !Int
  | -- | The cache is passed over for the run, as a warning said when it
    -- was opened.
    DirectoryDropped

-- | A run that has asked no cache anything.
emptyCacheMemo :: CacheMemo
emptyCacheMemo = CacheMemo {memoDirectories = Map.empty, memoNarInfos = Map.empty}

-- | Fetch a path from the cache that offered it, as that cache's narinfo
-- describes it, asking no cache anything more: the build's half of a
-- substitution whose reference query already read the narinfo
-- ('queryReferencesWith').  The path is the one the narinfo was checked
-- for, which the t'Substitute' carries, and the caller has made what it
-- references valid first, or the database refuses the registration this
-- returns.
--
-- Upstream's per-path substitution protocol: take the path's exclusive
-- lock FIRST, before any deletion or download, and re-check validity
-- under it - another process may have registered the path while this
-- one waited, and its finished work must be adopted
-- ('SubstAlreadyValid') rather than deleted and redone.  The lock then
-- holds across the delete, the download, materialization, and the
-- on-disk recheck; a successful result carries it still held (see
-- 'SubstSuccess'), and every other exit releases it here.  On success the
-- path is unpacked and read-only on disk but NOT registered - the caller
-- records the returned t'PathRegistration'.  The NAR request runs under
-- the given retry policy.
--
-- A store upstream Nix's database may also manage is refused
-- ('SubstError') before the lock is taken and before the NAR is asked for
-- ("Nix.Store.Upstream"): the destination is cleared before unpacking,
-- and a path upstream registered has no row here.  A NAR the cache no
-- longer holds is 'SubstNotFound', upstream's @SubstituteGone@.  A failure
-- is tagged with the cache's URL, and a synchronous exception is one
-- ('catchSync'), while an asynchronous one propagates: an interrupt
-- mid-download must abort the substitution, never send the path to the
-- next cache and from there to a local build.
fetchSubstituteWith :: FetchRetryPolicy -> RetryEffects IO -> Store -> Substitute -> IO SubstResult
fetchSubstituteWith policy effects store (Substitute sp cache narInfo _) =
  underPathLock store sp $ \lock -> do
    result <-
      streamFromCache (Retrying policy effects) store cache sp narInfo lock
        `catchSync` \err -> pure (SubstError ("substitution exception: " <> T.pack (show err)))
    pure $ case result of
      SubstError err -> SubstError (ccUrl (openedConfig cache) <> ": " <> err)
      other -> other

-- | Run a substitution of a path under its lock: refuse a store upstream
-- Nix's database may also manage before the lock is taken, adopt a path
-- valid under the lock ('SubstAlreadyValid'), and release the lock on
-- every exit but a success, which carries it still held.
underPathLock :: Store -> StorePath -> (PathLock -> IO SubstResult) -> IO SubstResult
underPathLock store sp substituteLocked = do
  refusal <- upstreamManagedRefusal ReplacesUpstreamPaths (stDir store)
  maybe substitute (pure . SubstError) refusal
  where
    substitute = do
      lock <- acquirePathLock (stLogger store) (stDir store) sp
      locked lock `onException` releasePathLock lock
    locked lock = do
      valid <- isValid store sp
      if valid
        then do
          releasePathLock lock
          pure SubstAlreadyValid
        else do
          result <- substituteLocked lock
          case result of
            SubstSuccess _ _ -> pure result
            other -> do
              releasePathLock lock
              pure other

-- | What the caches say about a path's references, from the first cache
-- whose narinfo may be used, with the t'Substitute' that fetches the path
-- from that cache.  References arrive before the path that names them
-- ("Nix.Realise"), so this runs ahead of any download, and a narinfo passes
-- the same checks as a substitution before its references are believed.
--
-- The caches are opened first ('openCaches'), and a cache whose
-- @Priority@ does not read fails the query with 'QueryFailed' whatever the
-- fallback.  They are asked in priority order, as upstream's substitution
-- goal asks them.  One without the path is passed over.  So is one whose
-- narinfo names another path or no trusted key signed, and the scan says
-- why.  A query that fails, once its retries are spent or on a narinfo
-- that does not validate, ends the scan with 'QueryFailed' under
-- 'NoFallback', as upstream rethrows it.  Under 'BuildFromSource' it is
-- reported and passed over, as upstream logs it and asks the next cache.
-- What the scan says, and the warning for a cache passed over, goes to
-- the logger.
--
-- A cache that answered about the path before in this run answers from
-- the run's t'CacheMemo' instead, so a closure requests each narinfo
-- once and a path no cache holds is looked for once; a query that failed
-- is made again.
queryReferencesWith :: Logger -> FetchRetryPolicy -> RetryEffects IO -> Fallback -> [CacheConfig] -> StorePath -> StateT CacheMemo IO (Offer Substitute)
queryReferencesWith logger policy effects fallback caches sp = do
  opened <- openCaches logger caches
  case opened of
    Left failure -> pure (QueryFailed failure)
    Right usable -> offerFrom logger (Retrying policy effects) fallback (sortCaches usable) sp

-- | 'queryReferencesWith' over the caches after the one a t'Substitute'
-- came from, for a path that substitute did not deliver: upstream's goal
-- moving on to its next substituter.
queryNextOfferWith :: Logger -> FetchRetryPolicy -> RetryEffects IO -> Fallback -> Substitute -> StateT CacheMemo IO (Offer Substitute)
queryNextOfferWith logger policy effects fallback (Substitute sp _ _ later) = offerFrom logger (Retrying policy effects) fallback later sp

-- | The first offer for a path among caches in the order given.
offerFrom :: Logger -> Retrying -> Fallback -> [OpenedCache] -> StorePath -> StateT CacheMemo IO (Offer Substitute)
offerFrom _ _ _ [] _ = pure NotOffered
offerFrom logger retry fallback (opened : rest) sp = do
  answer <- askCache retry opened sp
  let next = offerFrom logger retry fallback rest sp
      cache = openedConfig opened
      failed failure = case fallback of
        NoFallback -> pure (QueryFailed (ccUrl cache <> ": " <> spName sp <> ": " <> failure))
        BuildFromSource -> liftIO (reportCache logger cache sp failure) >> next
  case answer of
    NarInfoUsable narInfo -> either failed (pure . Offered (Substitute sp opened narInfo rest)) (parseReferences (NarInfo.niReferences narInfo))
    NarInfoAbsent -> next
    NarInfoPassedOver reason -> liftIO (reportCache logger cache sp reason) >> next
    NarInfoFailed failure -> failed failure

-- | Tell the logger what one cache did with a path.
reportCache :: Logger -> CacheConfig -> StorePath -> Text -> IO ()
reportCache logger cache sp message =
  logMessage logger ("  [subst] " <> ccUrl cache <> ": " <> spName sp <> ": " <> message)

-- ---------------------------------------------------------------------------
-- Opening caches
-- ---------------------------------------------------------------------------

-- | The caches a run asks, each opened once in the run ('openCache'): an
-- HTTP cache as configured, its URL without trailing slashes, and a
-- @file:\/\/@ cache as upstream opens a local binary cache store
-- ('openCacheDirectory'), asked at the priority its @nix-cache-info@
-- states, or at upstream's default when it states none.  One that cannot
-- be opened is left out after a warning to the logger, as upstream drops
-- a substituter it cannot open.  One whose @Priority@ does not read
-- is the 'Left', as it fails upstream's command, and is opened again by
-- the next query.
openCaches :: Logger -> [CacheConfig] -> StateT CacheMemo IO (Either Text [OpenedCache])
openCaches logger caches = fmap catMaybes . sequenceA <$> traverse (openCache logger) caches

-- | One cache, opened for the run ('openCaches'): 'Nothing' when it is
-- passed over.
openCache :: Logger -> CacheConfig -> StateT CacheMemo IO (Either Text (Maybe OpenedCache))
openCache logger cache = case fileUrlDirectory "state its priority in its nix-cache-info" (T.unpack url) of
  Nothing -> pure (Right (Just (OpenedCache cache {ccUrl = T.dropWhileEnd (== '/') url} HttpTransport)))
  Just named -> do
    remembered <- gets (Map.lookup url . memoDirectories)
    fmap usable <$> maybe (openDirectory named) (pure . Right) remembered
  where
    url = ccUrl cache
    usable opening = case opening of
      DirectoryOpened dir priority -> Just (OpenedCache cache {ccPriority = priority} (DirectoryTransport dir))
      DirectoryDropped -> Nothing
    openDirectory named = case named of
      Left refusal -> dropped refusal
      Right dir -> do
        outcome <- liftIO (openCacheDirectory url dir)
        case outcome of
          Left (CachePriorityInvalid message) -> pure (Left message)
          Left (CacheUnopenable message) -> dropped message
          Right info -> remember (DirectoryOpened dir (fromMaybe defaultCachePriority (ciPriority info)))
    dropped message = do
      liftIO (logWarning logger message)
      remember DirectoryDropped
    remember :: DirectoryOpening -> StateT CacheMemo IO (Either Text DirectoryOpening)
    remember opening = do
      modify' (\memo -> memo {memoDirectories = Map.insert url opening (memoDirectories memo)})
      pure (Right opening)

-- | The priority a @file:\/\/@ cache is asked at when its @nix-cache-info@
-- states none: upstream's default for every store's @priority@ setting,
-- which that file's field replaces.
defaultCachePriority :: Int
defaultCachePriority = 0

-- | Sort caches by priority, lower first, and those of one priority in the
-- order given, as upstream sorts its substituters.
sortCaches :: [OpenedCache] -> [OpenedCache]
sortCaches = sortBy (comparing (ccPriority . openedConfig))

-- | The retry policy and its effects, carried to every request one
-- substitution makes.
data Retrying = Retrying !FetchRetryPolicy !(RetryEffects IO)

-- | Run one request's attempt under the carried policy when its cache is
-- served over HTTP.  A directory's file is read once, as upstream retries
-- downloads alone.
attemptOn :: Retrying -> OpenedCache -> IO (Either AttemptFailure a) -> IO (Either AttemptFailure a)
attemptOn (Retrying policy effects) (OpenedCache _ transport) attempt = case transport of
  HttpTransport -> retryTransient policy effects attempt
  DirectoryTransport _ -> attempt

-- | Stream a path's NAR from the cache whose narinfo is given
-- ('streamWithRetry').  Unsupported compression rejects before the
-- download: the value is known from the narinfo, and a multi-hundred-MB
-- download that can only fail in decompression is pure waste.  It is
-- still a failed substitute, as upstream's decompressor fails one.  The
-- strict 'decompressorFor' encodes the same support set; the suite pins
-- their agreement.
streamFromCache :: Retrying -> Store -> OpenedCache -> StorePath -> NarInfo.NarInfo -> PathLock -> IO SubstResult
streamFromCache retry store cache sp narInfo lock =
  case streamingDecompressionSupported (NarInfo.niCompression narInfo) of
    Left err -> pure (SubstError err)
    Right () -> streamWithRetry retry store cache sp narInfo lock

-- | One cache's narinfo for a path, judged before anything is downloaded.
data NarInfoAnswer
  = -- | A narinfo for the path that validates and that a trusted key
    -- signed.
    NarInfoUsable !NarInfo.NarInfo
  | -- | The cache does not hold the path.
    NarInfoAbsent
  | -- | A narinfo this build passes over, and why: one naming another
    -- path, which upstream reads as the path not being in that cache, or
    -- one no trusted key signed, which it warns about and skips.
    NarInfoPassedOver !Text
  | -- | The query failed: the transfer once its retries are spent, a body
    -- that does not parse or validate, or a trusted key that does not
    -- parse.
    NarInfoFailed !Text
  deriving (Eq, Show)

-- | One cache's narinfo for a path, judged ('judgeNarInfo'): from the
-- t'CacheMemo' when the run has had it from that cache, and otherwise
-- fetched under the substitution's retry policy and remembered unless the
-- query failed.  A narinfo whose fields do not validate is a failed query,
-- as upstream's parser throws on one.
askCache :: Retrying -> OpenedCache -> StorePath -> StateT CacheMemo IO NarInfoAnswer
askCache retry opened sp = do
  remembered <- gets (Map.lookup url . memoNarInfos)
  case remembered of
    Just held -> pure (heldAnswer held)
    Nothing -> do
      fetched <- liftIO (fetchNarInfo retry opened narInfoFile)
      case fetched of
        Left failure -> pure (NarInfoFailed failure)
        Right held -> do
          when (isRight (traverse_ validateNarInfoFields held)) $
            modify' (\memo -> memo {memoNarInfos = Map.insert url held (memoNarInfos memo)})
          pure (heldAnswer held)
  where
    cache = openedConfig opened
    narInfoFile = spHash sp <> ".narinfo"
    url = ccUrl cache <> "/" <> narInfoFile
    heldAnswer = maybe NarInfoAbsent (judgeNarInfo cache sp)

-- | A narinfo request under the substitution's retry policy: what the cache
-- holds, or why the query failed.
fetchNarInfo :: Retrying -> OpenedCache -> Text -> IO (Either Text (Maybe NarInfo.NarInfo))
fetchNarInfo retry cache file =
  (first attemptFailureMessage <$> attemptOn retry cache (fetchNarInfoOnce cache file))
    `catchSync` \err -> pure (Left ("query exception: " <> T.pack (show err)))

-- | Whether a served narinfo may be used, decided from the narinfo alone, in
-- upstream's order.  The fields validate first, standing in for upstream's
-- narinfo parser, and above all before 'verifySigs' builds the signed
-- fingerprint from them.  The narinfo must then describe the requested path
-- (@goodStorePath@ in @Store::queryPathInfo@ at 2.24.9): a misconfigured or
-- hostile cache could return a validly signed narinfo for a different path
-- under this hash's URL.  Last come the trusted keys, which upstream parses
-- when it checks the signature, and a signature one of them verifies
-- (@PathSubstitutionGoal::init@).
judgeNarInfo :: CacheConfig -> StorePath -> NarInfo.NarInfo -> NarInfoAnswer
judgeNarInfo cache sp narInfo
  | Left err <- validateNarInfoFields narInfo = NarInfoFailed err
  | not (narInfoMatchesPath sp narInfo) = NarInfoPassedOver ("narinfo names " <> NarInfo.niStorePath narInfo)
  | Left err <- trustedKeys cache = NarInfoFailed err
  | otherwise = either NarInfoPassedOver (const (NarInfoUsable narInfo)) (verifySigs cache narInfo)

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

-- | NAR bytes that passed 'verifyNar': they hash to the narinfo's
-- signed NarHash and count its signed NarSize, with the content address
-- the narinfo states, which the unpacked tree is held to
-- ('checkStatedAddress').  The constructor is not
-- exported, so 'verifyNar' is the only way to obtain one and
-- 'unpackVerifiedNar', the strict pipeline's unpack sink, cannot be
-- reached with bytes that skipped the check: verify-then-unpack is a
-- type, not a statement order.  The decoded digest rides along because
-- registration records its canonical spelling and the on-disk recheck
-- compares against it, so neither re-parses the narinfo's NarHash.
-- Only the strict pipeline can carry this: the streaming pipeline
-- writes to disk before the hash can be known (upstream's ordering)
-- and relies on its cleanup contract instead
-- ('materializeNarFromSource').
data VerifiedNar = VerifiedNar !BS.ByteString !Hash.NixHash !(Maybe Text)
  deriving (Eq, Show)

-- | The verified bytes.
verifiedNarBytes :: VerifiedNar -> BS.ByteString
verifiedNarBytes (VerifiedNar bytes _ _) = bytes

-- | The digest the verified bytes hash to, decoded from the narinfo's
-- NarHash.
verifiedNarDigest :: VerifiedNar -> Hash.NixHash
verifiedNarDigest (VerifiedNar _ digest _) = digest

-- | The content address the narinfo states, if it states one.
verifiedNarAddress :: VerifiedNar -> Maybe Text
verifiedNarAddress (VerifiedNar _ _ address) = address

-- | The only constructor of a t'VerifiedNar': the hash check, then the
-- size check, with the decoded declared digest carried out.  The NAR
-- hash is the content-addressed integrity contract; the signature only
-- attests to the narinfo, not the body, so network corruption or a
-- compromised cache is caught here, before the bytes are trusted.
verifyNar :: NarInfo.NarInfo -> BS.ByteString -> Either Text VerifiedNar
verifyNar narInfo rawNar = do
  declared <- verifyNarHash narInfo rawNar
  verifyNarSize narInfo rawNar
  pure (VerifiedNar rawNar declared (NarInfo.niCA narInfo))

-- | Verify the NAR hash and size, deserialize, unpack to the store, and
-- set permissions: 'verifyNar', then 'unpackVerifiedNar'.  Returns the
-- path's registration for the caller to record; no database write
-- happens here (see 'SubstSuccess').  The strict counterpart of the
-- streaming pipeline, kept as its differential oracle: the suite
-- materializes the same NAR through both and requires identical trees.
unpackAndVerify :: Store -> StorePath -> NarInfo.NarInfo -> BS.ByteString -> IO SubstResult
unpackAndVerify store sp narInfo rawNar =
  -- Narinfo metadata is parsed before any disk write: a malformed
  -- narinfo must not leave an unpacked-but-unregistered path behind.
  case verifiedInputs of
    Left err -> pure (SubstError err)
    Right (verified, refs, deriver) -> unpackVerifiedNar store sp verified refs deriver (narInfoSignatures narInfo)
  where
    verifiedInputs = do
      verified <- verifyNar narInfo rawNar
      refs <- parseReferences (NarInfo.niReferences narInfo)
      deriver <- parseDeriver (stDir store) (NarInfo.niDeriver narInfo)
      pure (verified, refs, deriver)

-- | The strict pipeline's unpack sink: deserialize a t'VerifiedNar',
-- materialize it at the store path, seal it read-only, and recheck the
-- tree on disk against the carried digest before building its
-- registration from the references, deriver and signatures.  Follows the same
-- per-path lock protocol as the live pipeline: the lock is taken
-- before the stale destination is cleared, validity re-checks under
-- it, and a success carries the lock still held.  A store upstream
-- Nix's database may also manage is refused before the lock, as the
-- live pipeline refuses it.
unpackVerifiedNar :: Store -> StorePath -> VerifiedNar -> [StorePath] -> Maybe Text -> Set Signature -> IO SubstResult
unpackVerifiedNar store sp verified refs deriver sigs =
  case NAR.deserialise (verifiedNarBytes verified) of
    Left err -> pure (SubstError ("NAR deserialisation failed: " <> T.pack err))
    Right narEntry -> do
      refusal <- upstreamManagedRefusal ReplacesUpstreamPaths (stDir store)
      maybe (unpackUnderLock narEntry) (pure . SubstError) refusal
  where
    unpackUnderLock narEntry = do
      lock <- acquirePathLock (stLogger store) (stDir store) sp
      result <- unpackLocked narEntry lock `onException` releasePathLock lock
      case result of
        SubstSuccess _ _ -> pure result
        other -> do
          releasePathLock lock
          pure other
    digest = verifiedNarDigest verified
    destPath = storePathToFilePath (stDir store) sp
    unpackLocked narEntry lock = do
      alreadyValid <- isValid store sp
      if alreadyValid
        then pure SubstAlreadyValid
        else do
          unpackResult <-
            fmap
              Right
              ( do
                  clearStaleDestination destPath
                  unpackNarEntry (stCaseSensitivity store) destPath narEntry
              )
              `catchSync` (pure . Left)
          case (unpackResult :: Either SomeException (Either Text ())) of
            Left err -> pure (SubstError ("unpack failed: " <> T.pack (show err)))
            Right (Left err) -> pure (SubstError ("unpack failed: " <> err))
            Right (Right ()) -> do
              canonicalizeMetadata destPath
              -- A path registered valid must match its recorded hash ON
              -- DISK, not merely in the verified bytes: any divergence
              -- the filesystem introduced between the NAR and the
              -- materialized tree (name folding, link replication) must
              -- surface here, before the row exists.  A mismatching tree
              -- is removed - left in place it would be adopted by
              -- existence checks at this path.
              onDisk <- ExecBit.serialiseFromPath (volumeCaseHack (stCaseSensitivity store)) destPath
              addressed <-
                if Hash.hashBytes (NAR.serialise onDisk) /= digest
                  then pure (Left ("unpacked tree does not reproduce the declared NAR hash at " <> T.pack destPath))
                  else checkStatedAddress (volumeCaseHack (stCaseSensitivity store)) (stDir store) sp destPath (verifiedNarAddress verified)
              case addressed of
                Left err -> do
                  Dir.removePathForcibly destPath
                  pure (SubstError err)
                Right address ->
                  pure $
                    SubstSuccess
                      PathRegistration
                        { prPath = sp,
                          -- The canonical spelling of the verified digest,
                          -- so the DB converges on one hash spelling
                          -- regardless of the cache's.
                          prNarHash = Hash.formatNixHash digest,
                          -- The verified actual byte count (equal to the
                          -- declared NarSize per 'verifyNarSize') - no
                          -- Integer conversion that could wrap.
                          prNarSize = BS.length (verifiedNarBytes verified),
                          prDeriver = deriver,
                          prReferences = refs,
                          prUltimate = NotUltimate,
                          prSignatures = sigs,
                          prContentAddress = address
                        }
                      lock

-- | Hold a substituted tree to the content address its narinfo states, as
-- upstream's store holds a path it adds once the NAR's hash and size are
-- checked: the tree is hashed as the address's method reads it, its NAR,
-- or for a flat or text address the bytes of the regular file it must
-- be, modulo the path's hash part, and refused in upstream's words when
-- that differs.  A narinfo that states no address holds the tree to its
-- NarHash alone, as upstream's does.  The address the tree was held to,
-- if any, is the one its registration records.
checkStatedAddress :: NAR.CaseHack -> StoreDir -> StorePath -> FilePath -> Maybe Text -> IO (Either Text (Maybe ContentAddress))
checkStatedAddress caseHack dir sp destPath stated = case stated of
  Nothing -> pure (Right Nothing)
  Just spelled -> case parseContentAddress spelled of
    Left err -> pure (Left ("narinfo CA '" <> spelled <> "' does not parse: " <> err))
    Right address -> case moduloHashInit (caAlgo address) (TE.encodeUtf8 (spHash sp)) of
      Nothing -> pure (Left ("narinfo CA '" <> spelled <> "' names an algorithm nova-nix cannot hash with"))
      Just start -> (>>= compared address) <$> treeDigest caseHack (caMethod address) start destPath
  where
    compared address got
      | got == caDigest address = Right (Just address)
      | otherwise =
          Left
            ( "ca hash mismatch importing path '"
                <> storePathToText dir sp
                <> "';\n  specified: "
                <> caAlgo address
                <> ":"
                <> Nix32.encode (caDigest address)
                <> "\n  got:       "
                <> caAlgo address
                <> ":"
                <> Nix32.encode got
            )

-- | A tree's digest modulo a hash part as a content address's method
-- reads the tree: its NAR, or the bytes of the regular file a flat or
-- text address names.
treeDigest :: NAR.CaseHack -> ContentAddressMethod -> ModuloHash -> FilePath -> IO (Either Text BS.ByteString)
treeDigest caseHack method start path = case method of
  FixedMethod NarIngestion -> Right <$> ExecBit.withNarSource caseHack path (absorb start)
  FixedMethod FlatIngestion -> fileDigest
  TextMethod -> fileDigest
  where
    absorb !state pull = do
      chunk <- pull
      if BS.null chunk then pure (moduloHashFinalize state) else absorb (moduloHashUpdate state chunk) pull
    fileDigest = do
      link <- Dir.pathIsSymbolicLink path
      regular <- if link then pure False else Dir.doesFileExist path
      if regular
        then Right <$> withBinaryFile path ReadMode (\handle -> absorb start (BS.hGetSome handle fileReadChunkBytes))
        else pure (Left ("'" <> T.pack path <> "' is not a regular file, which its flat content address names"))

-- | How many bytes of a file 'treeDigest' reads at a time.
fileReadChunkBytes :: Int
fileReadChunkBytes = 64 * 1024

-- | Verify that the downloaded NAR bytes hash to the narinfo's declared
-- NarHash, returning the decoded digest on success so registration can
-- record its canonical spelling.  Compares decoded hash bytes; the
-- declared NarHash parses only as @sha256:@ and nix-base32
-- (@parseNixHash@).
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

-- | Whether a served narinfo describes the requested path: the hash and
-- name components of its declared StorePath must equal the requested
-- path's, as upstream Nix holds one.  Only the base name is compared;
-- 'validateNarInfoFields', which 'judgeNarInfo' runs first, holds the
-- StorePath to the canonical store directory.
narInfoMatchesPath :: StorePath -> NarInfo.NarInfo -> Bool
narInfoMatchesPath sp narInfo =
  storePathBaseOf (NarInfo.niStorePath narInfo) == Just (spHash sp, spName sp)

-- | The hash and the name of a full store path's base name, if well-formed
-- (@\<hash\>-\<name\>@ with a hash of the expected length).
storePathBaseOf :: Text -> Maybe (Text, Text)
storePathBaseOf path =
  let base = T.takeWhileEnd (\c -> c /= '/' && c /= '\\') path
      (hashPart, rest) = T.splitAt storePathHashLen base
   in case T.stripPrefix "-" rest of
        Just name | T.length hashPart == storePathHashLen -> Just (hashPart, name)
        _ -> Nothing

-- ---------------------------------------------------------------------------
-- Reading a cache's files
-- ---------------------------------------------------------------------------

-- | Cap on a narinfo response body, mirroring nova-cache's server-side
-- @maxNarInfoBodySize@ - the server bounds what it reads, and the
-- client bounds what any cache in its list can make it buffer.
-- Narinfo is small key-value text; 4 MB is far beyond any real one.
maxNarInfoBody :: Int
maxNarInfoBody = 4 * 1024 * 1024

-- | Read a chunk source, an HTTP response body or a file, in bounded
-- chunks up to a byte cap - 'Nothing' once the cap is exceeded, so an
-- over-large body aborts mid-stream instead of buffering without limit.
-- The client-side mirror of nova-cache's @readBodyLimited@.
readBodyCapped :: Int -> IO BS.ByteString -> IO (Maybe BS.ByteString)
readBodyCapped cap reader = go [] 0
  where
    go chunks !total = do
      chunk <- reader
      if BS.null chunk
        then pure (Just (BS.concat (reverse chunks)))
        else
          let newTotal = total + BS.length chunk
           in if newTotal > cap
                then pure Nothing
                else go (chunk : chunks) newTotal

-- | Read one file of a cache, named relative to it, the consumer given
-- the file's bytes as a chunk source: @Right Nothing@ when the cache does
-- not hold the file.
--
-- Over HTTP, that is a 'NotFound' or 'Forbidden' answer (404, 410, 401,
-- 403, 407), the cache saying so, not a failure to retry: upstream's
-- transfer layer retries neither, and its binary cache store reads both
-- as the file not being there.  Any other status is a failure, retried
-- when upstream retries it, and the transfer layer classifies what the
-- request throws ('transferFailureHandlers').  The process-global TLS
-- manager serves every request, for its connection pooling and
-- keep-alive.
--
-- From a directory, it is the file not existing, as upstream's local
-- binary cache store reads one.  A name the cache cannot hold
-- ('cacheFilePath'), and any other failure to read the file or to write
-- what it carries, fails, and is never retried ('attemptOn').
readCacheFile :: OpenedCache -> Text -> (IO BS.ByteString -> IO (Either AttemptFailure a)) -> IO (Either AttemptFailure (Maybe a))
readCacheFile (OpenedCache cache transport) file consume = case transport of
  HttpTransport -> overHttp `catches` transferFailureHandlers url
  DirectoryTransport dir -> either (pure . Left . FatalFailure) fromFile (cacheFilePath dir file)
  where
    url = ccUrl cache <> "/" <> file
    overHttp = do
      manager <- HTTPS.getGlobalManager
      request <- withUserAgent <$> HTTP.parseRequest (T.unpack url)
      withTransfer request manager $ \response ->
        let status = HTTP.responseStatus response
         in if status == HTTP.status200
              then fmap Just <$> consume (HTTP.responseBody response)
              else pure (unserved status)
    unserved status = case statusError status of
      NotFound -> Right Nothing
      Forbidden -> Right Nothing
      Misc -> Left (fetchStatusFailure url status)
      Transient -> Left (fetchStatusFailure url status)
    -- Only the open's does-not-exist is the cache lacking the file: one
    -- raised while the consumer writes is a failure like any other.
    fromFile path =
      bracket (tryJust (guard . isDoesNotExistError) (openBinaryFile path ReadMode)) (traverse_ hClose) (readOpened . either (const Nothing) Just)
        `catch` readFailure
    readOpened opened = case opened of
      Nothing -> pure (Right Nothing)
      Just fileHandle -> fmap Just <$> consume (BS.hGetSome fileHandle fileReadChunkBytes)
    readFailure :: IOException -> IO (Either AttemptFailure b)
    readFailure err = pure (Left (FatalFailure (T.pack (displayException err))))

-- | One narinfo request: @Right Nothing@ when the cache does not hold it
-- ('readCacheFile').  A narinfo that arrived whole and does not parse, or
-- overruns 'maxNarInfoBody', is the cache's bytes and fails at once.
fetchNarInfoOnce :: OpenedCache -> Text -> IO (Either AttemptFailure (Maybe NarInfo.NarInfo))
fetchNarInfoOnce cache file = readCacheFile cache file $ \body -> do
  capped <- readBodyCapped maxNarInfoBody body
  pure $ case capped of
    Nothing ->
      Left (FatalFailure ("narinfo body exceeds " <> T.pack (show maxNarInfoBody) <> " bytes"))
    -- Lenient decode: the body is cache-controlled bytes, and a stray
    -- invalid UTF-8 sequence must surface as a narinfo parse error, not
    -- an impure UnicodeException (the push side decodes the same way).
    Just bytes -> first (\err -> FatalFailure ("narinfo parse error: " <> T.pack err)) (NarInfo.parseNarInfo (TE.decodeUtf8Lenient bytes))

-- | Stream one substitution end to end, each attempt under the
-- substitution's retry policy where the cache is served over HTTP
-- ('fetchSubstituteWith', 'attemptOn').
--
-- By the time this runs the narinfo has already been fetched and
-- signature-verified, so the cache claims to hold this path.  Failures
-- carry their own retry class: a 'TransientFailure' - transport
-- errors, torn or truncated transfers, a status upstream retries,
-- anything a fresh attempt could plausibly complete - consumes retry
-- budget with upstream's backoff, while a 'FatalFailure' - a completed
-- transfer that verifies wrong, a body past its signed ceiling, a
-- status upstream never retries, a write the disk refused - ends the
-- attempt at once: retrying a deterministic failure only delays the
-- outcome.  Every attempt starts from a clean slate
-- ('streamNarIntoStore' clears the destination first, and every
-- failure path, exceptions included, removes what it wrote), so a retry
-- restarts the NAR from its first byte, where upstream resumes from the
-- written offset when the server accepts ranges and otherwise retries
-- only a transfer that wrote nothing yet.  A NAR the narinfo named and
-- the cache does not hold ('readCacheFile') is gone: one request, as
-- upstream's @SubstituteGone@, reported and then a miss the cache scan
-- passes over, because upstream's substitution goal treats a gone
-- substitute like one that never existed rather than as a failure.  A
-- narinfo the cache does not hold is a miss handled earlier in 'askCache'
-- and never reaches here.
streamWithRetry :: Retrying -> Store -> OpenedCache -> StorePath -> NarInfo.NarInfo -> PathLock -> IO SubstResult
streamWithRetry retry store cache sp narInfo lock = do
  outcome <- attemptOn retry cache (streamNarIntoStore cache store sp narInfo)
  case outcome of
    Right (Just registration) -> pure (SubstSuccess registration lock)
    Right Nothing -> do
      reportCache (stLogger store) (openedConfig cache) sp ("file '" <> NarInfo.niUrl narInfo <> "' does not exist in the binary cache")
      pure SubstNotFound
    Left failure -> pure (SubstError (attemptFailureMessage failure))

-- | Thrown inside the streaming pipeline where a chunk convention has
-- no error channel (the capped body source); converted back to the
-- pipeline's 'Left' at the attempt boundary in 'streamNarIntoStore'.
newtype StreamAbort = StreamAbort Text
  deriving (Show)

instance Exception StreamAbort

-- | Convert a t'StreamAbort' one attempt throws into the pipeline's
-- failure channel, so the retry budget governs it and the caller's
-- cleanup contract holds on every exit.  A body past its ceiling is the
-- cache misdeclaring, not a hiccup, as the cap derives from the signed
-- NarSize.  What the transport throws, 'readCacheFile' classifies, and
-- nothing else is caught, so an interrupt is never spent as retry budget.
tryAttempt :: IO (Either AttemptFailure a) -> IO (Either AttemptFailure a)
tryAttempt action = action `catch` \(StreamAbort msg) -> pure (Left (FatalFailure msg))

-- | One streaming substitution attempt: download, decompress, hash,
-- parse, and materialize in a single bounded pass, then verify.
-- Memory is bounded by the decoder's buffers and the parser's
-- structural-string cap, never by archive or file size.
--
-- Disk writes begin before the NAR hash can be known - the price of
-- never holding the archive, and exactly upstream's ordering - so
-- every failure path, exceptions included, removes the tree it wrote
-- ('materializeNarFromSource' owns that contract; 'readCacheFile' and
-- 'tryAttempt' classify what escapes it).  The narinfo metadata
-- registration needs (declared hash, references, deriver) still parses
-- BEFORE the first byte downloads: a malformed narinfo must not leave an
-- unpacked-but-unregistered path behind.  'Nothing' when the cache no
-- longer holds the NAR ('streamWithRetry').
streamNarIntoStore :: OpenedCache -> Store -> StorePath -> NarInfo.NarInfo -> IO (Either AttemptFailure (Maybe PathRegistration))
streamNarIntoStore cache store sp narInfo = case preflight of
  Left err -> pure (Left (FatalFailure err))
  Right (declaredDigest, refs, deriver, downloadCap) -> do
    clearStaleDestination (storePathToFilePath (stDir store) sp)
    tryAttempt $
      readCacheFile cache (NarInfo.niUrl narInfo) $ \body -> do
        source <- cappedBodySource downloadCap body
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

-- | The byte cap on the compressed NAR body: the 'compressedBodyCeiling'
-- of the SIGNED NarSize.  The Ed25519 fingerprint covers StorePath,
-- NarHash, NarSize and references, so no unsigned field moves the cap,
-- and a narinfo whose signature verifies cannot buy a larger download.
--
-- FileSize takes no part.  Upstream reads it only for progress and
-- download statistics (@build\/substitution-goal.cc@ and @misc.cc@ at
-- 2.35.4, @build\/substitution-goal.cc@ and @store-api.cc@ at 2.24.9) and
-- checks a substituted NAR against NarHash and NarSize alone, as
-- 'consumeNarStream' does.  A cache that names a NAR by its NarHash, as
-- cache.nixos.org names its zstd NARs, serves one file for every store
-- path with the same NAR, and an upload that compresses that NAR again
-- replaces the file whose FileSize and FileHash an earlier narinfo
-- records.
downloadCapFor :: NarInfo.NarInfo -> Either Text Int
downloadCapFor narInfo
  | narSize < 0 || narSize > toInteger (maxBound :: Int) =
      Left ("narinfo declares an unusable NAR size: " <> T.pack (show narSize))
  | otherwise =
      Right (fromInteger (min (compressedBodyCeiling narSize) (toInteger (maxBound :: Int))))
  where
    narSize = NarInfo.niNarSize narInfo

-- | The most a compressed NAR body may legitimately exceed its NarSize
-- by: xz and zstd expand incompressible input by well under one
-- percent of framing overhead, so a 1/64 (~1.6%) margin plus a fixed
-- floor for small NARs is generous for any real codec, while a body
-- orders of magnitude larger is refused.
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
          consumeNarStream (stCaseSensitivity store) destPath narInfo declaredDigest
      case streamed of
        Left err -> do
          Dir.removePathForcibly destPath
          pure (Left err)
        Right narByteCount -> do
          canonicalizeMetadata destPath
          -- A path registered valid must match its recorded hash ON
          -- DISK, not merely in the streamed bytes: any divergence the
          -- filesystem introduced between the NAR and the materialized
          -- tree must surface here, before the row exists.  The recheck
          -- streams too, so its memory does not scale with the path.
          -- Through 'ExecBit', not nova-cache's walk directly: on Windows
          -- the sink writes the exec bit to a stream that walk cannot see,
          -- and a verifier reading the flag back out of the file extension
          -- would reject exactly the paths this representation exists for.
          -- On Unix it is still the streaming hash.
          onDiskDigest <- ExecBit.narHashOfPath (volumeCaseHack (stCaseSensitivity store)) destPath
          addressed <-
            if onDiskDigest /= declaredDigest
              then pure (Left ("unpacked tree does not reproduce the declared NAR hash at " <> T.pack destPath))
              else checkStatedAddress (volumeCaseHack (stCaseSensitivity store)) (stDir store) sp destPath (NarInfo.niCA narInfo)
          case addressed of
            Left err -> do
              Dir.removePathForcibly destPath
              pure (Left (FatalFailure err))
            Right address ->
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
                      prReferences = refs,
                      prUltimate = NotUltimate,
                      prSignatures = narInfoSignatures narInfo,
                      prContentAddress = address
                    }

-- | Drive the decompressed chunk source through incremental hashing,
-- the streaming NAR parser, and the store's streaming unpack sink,
-- returning the verified NAR byte count.  The hash context folds over
-- exactly the bytes the parser consumes, so the digest is of the NAR
-- the tree was built from.  The sensitivity is the destination
-- volume's, 'stCaseSensitivity' when the destination is a store path.
consumeNarStream :: CaseSensitivity -> FilePath -> NarInfo.NarInfo -> Hash.NixHash -> IO BS.ByteString -> IO (Either AttemptFailure Int)
consumeNarStream sensitivity destPath narInfo declaredDigest narSource = do
  sink <- newNarUnpackSink sensitivity destPath
  streamed <- sinkNarStream sink narSource
  case streamed of
    Left failure -> pure (Left (narStreamFailure failure))
    Right (digest, narBytes)
      | toInteger narBytes /= NarInfo.niNarSize narInfo -> do
          abortNarUnpack sink
          -- The grammar completed, so the transfer was whole: a size
          -- that still disagrees is the narinfo misdeclaring.
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
      | digest /= declaredDigest -> do
          abortNarUnpack sink
          -- Size matched, so the transfer completed; wrong bytes are
          -- deterministic corruption, not a hiccup.
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
      | otherwise -> do
          finished <- finishNarUnpack sink
          pure (either (Left . FatalFailure) (const (Right narBytes)) finished)

-- | Why a streamed NAR stopped, as the attempt's failure.  A name or
-- shape the store refuses is a property of the archive, not of this
-- transfer, so no retry can change it.  A truncated body and a torn
-- transfer parse-fail the same way, so a parse failure spends the retry
-- budget.
narStreamFailure :: NarStreamFailure -> AttemptFailure
narStreamFailure failure = case failure of
  NarStreamRefused err -> FatalFailure err
  NarStreamMalformed msg -> TransientFailure ("NAR stream parse failed: " <> msg)

-- | Stream a chunk source, an HTTP response body or a file, bounded by
-- the download cap, which 'downloadCapFor' derives from the signed
-- NarSize alone - 'readBodyCapped'\'s discipline without the buffering.
-- Exceeding the cap throws t'StreamAbort'; the attempt boundary converts
-- it back to the pipeline's error channel.
cappedBodySource :: Int -> IO BS.ByteString -> IO (IO BS.ByteString)
cappedBodySource cap reader = do
  countRef <- newIORef 0
  pure $ do
    chunk <- reader
    consumed <- readIORef countRef
    let total = consumed + BS.length chunk
    if total > cap
      then throwIO (StreamAbort ("NAR body exceeds its download ceiling (" <> T.pack (show cap) <> " bytes)"))
      else do
        writeIORef countRef total
        pure chunk

-- | Whether the streaming pipeline can decompress a narinfo
-- @Compression@ value, decided from the value alone so unsupported
-- compression rejects before any download.  Both this and the strict
-- 'decompressorFor' dispatch through 'parseNarCompression', so the
-- support set exists exactly once.
streamingDecompressionSupported :: Text -> Either Text ()
streamingDecompressionSupported = void . parseNarCompression

-- | Run a consumer over the decompressed view of a chunk source:
-- identity for 'CompressionNone', nova-cache's bounded decoders for
-- 'CompressionXz', 'CompressionZstd', and 'CompressionBzip2' (output
-- capped at the declared NarSize; thrown codec errors convert to the
-- pipeline's error channel here, carrying their retry class from
-- 'xzFailure', 'zstdFailure', and 'bzip2Failure').
withDecompressedSource :: Integer -> Text -> IO BS.ByteString -> (IO BS.ByteString -> IO (Either AttemptFailure a)) -> IO (Either AttemptFailure a)
withDecompressedSource declaredNarSize compression source consume =
  case parseNarCompression compression of
    Left err -> pure (Left (FatalFailure err))
    Right CompressionNone -> consume source
    Right CompressionXz -> case xzLimitsFor declaredNarSize of
      Left err -> pure (Left (FatalFailure err))
      Right limits -> withXzAttempt limits source consume
    Right CompressionZstd -> case zstdLimitsFor declaredNarSize of
      Left err -> pure (Left (FatalFailure err))
      Right limits ->
        Zstd.withZstdSource limits source consume
          `catch` \zstdErr -> pure (Left (zstdFailure zstdErr))
    Right CompressionBzip2 -> case bzip2LimitsFor declaredNarSize of
      Left err -> pure (Left (FatalFailure err))
      Right limits ->
        Bzip2.withBzip2Source limits source consume
          `catch` \bzip2Err -> pure (Left (bzip2Failure bzip2Err))

-- | Run a consumer over the xz-decompressed view of a chunk source
-- ('Xz.withXzSource'), a decoder failure ending the attempt with its
-- class ('xzFailure') where the decoder throws it.  A failure of the
-- compressed source itself passes through as it was thrown.
withXzAttempt :: Xz.XzLimits -> IO BS.ByteString -> (IO BS.ByteString -> IO (Either AttemptFailure a)) -> IO (Either AttemptFailure a)
withXzAttempt limits source consume =
  Xz.withXzSource limits source consume `catch` (pure . Left . xzFailure)

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

-- | 'xzFailure'\'s zstd counterpart, under the same taxonomy.
zstdFailure :: Zstd.ZstdError -> AttemptFailure
zstdFailure zstdErr = case zstdErr of
  Zstd.ZstdStreamError _ -> TransientFailure rendered
  Zstd.ZstdOutputOverBound _ -> FatalFailure rendered
  where
    rendered = renderZstdError zstdErr

-- | 'xzFailure'\'s bzip2 counterpart, under the same taxonomy.
bzip2Failure :: Bzip2.Bzip2Error -> AttemptFailure
bzip2Failure bzip2Err = case bzip2Err of
  Bzip2.Bzip2StreamError _ -> TransientFailure rendered
  Bzip2.Bzip2OutputOverBound _ -> FatalFailure rendered
  where
    rendered = renderBzip2Error bzip2Err

-- ---------------------------------------------------------------------------
-- Pure helpers
-- ---------------------------------------------------------------------------

-- | The signatures a narinfo carries, which a substituted path is
-- registered with, as upstream registers the path info it copied.  A
-- @Sig@ line that does not parse, which upstream would refuse the narinfo
-- for, is left out: the narinfo was accepted for a signature that did
-- verify.
narInfoSignatures :: NarInfo.NarInfo -> Set Signature
narInfoSignatures = Set.fromList . concatMap (either (const []) pure . parseSignature . TE.encodeUtf8) . NarInfo.niSigs

-- | Verify narinfo signatures against the cache's trusted public keys.
-- Accepts when at least one signature verifies under at least one key,
-- upstream's any-trusted-key rule.
verifySigs :: CacheConfig -> NarInfo.NarInfo -> Either Text ()
verifySigs cache narInfo =
  case NarInfo.niSigs narInfo of
    [] -> Left "narinfo has no signatures"
    sigs -> do
      keys <- trustedKeys cache
      if any (\key -> any (Signing.verify key narInfo) sigs) keys
        then Right ()
        else Left "no valid signature found"

-- | The cache's trusted public keys.  A malformed key is an error rather
-- than a silently skipped one: upstream rejects the configuration, so a
-- typo cannot quietly narrow the trusted set to the keys that parsed.
trustedKeys :: CacheConfig -> Either Text [Signing.PublicKey]
trustedKeys = traverse parseKey . ccPublicKeys
  where
    parseKey raw = case Signing.parsePublicKey raw of
      Left err -> Left ("invalid public key: " <> T.pack err)
      Right pubKey -> Right pubKey

-- | The whole-buffer decompressor for a narinfo @Compression@ value,
-- decided from the narinfo's declared values alone so unsupported
-- compression rejects before any download.  'CompressionNone' is
-- identity; 'CompressionXz' (cache.nixos.org's format),
-- 'CompressionZstd' (the modern caches'), and 'CompressionBzip2' (the
-- historical caches', and what an absent field means) decompress
-- bounded by the declared NarSize, a signed claim the caller validates
-- before resolving the decompressor.  The resolved function runs in IO
-- because the zstd and bzip2 decoders are IO-native (see
-- 'NovaCache.Zstd' and 'NovaCache.Bzip2'); the strict shape follows its
-- codecs.  Dispatches through 'parseNarCompression' like the streaming
-- path, so the support set exists exactly once.
decompressorFor :: Integer -> Text -> Either Text (BS.ByteString -> IO (Either Text BS.ByteString))
decompressorFor declaredNarSize compression = do
  kind <- parseNarCompression compression
  case kind of
    CompressionNone -> Right (pure . Right)
    CompressionXz -> do
      limits <- xzLimitsFor declaredNarSize
      Right (pure . either (Left . renderXzError) Right . Xz.decompress limits)
    CompressionZstd -> do
      limits <- zstdLimitsFor declaredNarSize
      Right (fmap (either (Left . renderZstdError) Right) . Zstd.decompress limits)
    CompressionBzip2 -> do
      limits <- bzip2LimitsFor declaredNarSize
      Right (fmap (either (Left . renderBzip2Error) Right) . Bzip2.decompress limits)

-- | Decompress NAR data based on the compression type from narinfo,
-- bounded by the declared NarSize.  Support is decided by
-- 'decompressorFor'; this applies the result.
decompressNar :: Integer -> Text -> BS.ByteString -> IO (Either Text BS.ByteString)
decompressNar declaredNarSize compression narData =
  case decompressorFor declaredNarSize compression of
    Left err -> pure (Left err)
    Right decompress -> decompress narData

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

-- | One 'Xz.XzError' in the register the other substitution errors use.
renderXzError :: Xz.XzError -> Text
renderXzError xzErr = case xzErr of
  Xz.XzStreamError msg -> "xz stream error: " <> T.pack msg
  Xz.XzOutputOverBound bound ->
    "xz output exceeds the declared NarSize (" <> T.pack (show bound) <> " bytes)"
  Xz.XzMemoryOverBound bound ->
    "xz decoder memory over its cap (" <> T.pack (show bound) <> " bytes)"

-- | The bounds for one zstd decode: output capped at the narinfo's
-- declared NarSize; decoder state rides libzstd's built-in window
-- limit (see 'NovaCache.Zstd').  The same totality guard as
-- 'xzLimitsFor'.
zstdLimitsFor :: Integer -> Either Text Zstd.ZstdLimits
zstdLimitsFor declaredNarSize
  | declaredNarSize < 0 || declaredNarSize > toInteger (maxBound :: Word64) =
      Left ("zstd decompression bound out of range: " <> T.pack (show declaredNarSize))
  | otherwise = Right Zstd.ZstdLimits {Zstd.zstdMaxOutputBytes = fromInteger declaredNarSize}

-- | One 'Zstd.ZstdError' in the register the other substitution
-- errors use.
renderZstdError :: Zstd.ZstdError -> Text
renderZstdError zstdErr = case zstdErr of
  Zstd.ZstdStreamError msg -> "zstd stream error: " <> T.pack msg
  Zstd.ZstdOutputOverBound bound ->
    "zstd output exceeds the declared NarSize (" <> T.pack (show bound) <> " bytes)"

-- | The bounds for one bzip2 decode: output capped at the narinfo's
-- declared NarSize.  There is no decoder-memory knob to set - bzip2
-- carries no attacker-chosen dictionary size, so decoder state is a
-- small constant of the format (see 'NovaCache.Bzip2').  The same
-- totality guard as 'xzLimitsFor'.
bzip2LimitsFor :: Integer -> Either Text Bzip2.Bzip2Limits
bzip2LimitsFor declaredNarSize
  | declaredNarSize < 0 || declaredNarSize > toInteger (maxBound :: Word64) =
      Left ("bzip2 decompression bound out of range: " <> T.pack (show declaredNarSize))
  | otherwise = Right Bzip2.Bzip2Limits {Bzip2.bzip2MaxOutputBytes = fromInteger declaredNarSize}

-- | One 'Bzip2.Bzip2Error' in the register the other substitution
-- errors use.
renderBzip2Error :: Bzip2.Bzip2Error -> Text
renderBzip2Error bzip2Err = case bzip2Err of
  Bzip2.Bzip2StreamError msg -> "bzip2 stream error: " <> T.pack msg
  Bzip2.Bzip2OutputOverBound bound ->
    "bzip2 output exceeds the declared NarSize (" <> T.pack (show bound) <> " bytes)"

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
-- records (the form "Nix.Push" parses back with 'Nix.Store.Path.parseStorePath').
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
-- failed registration) between 'canonicalizeMetadata' here and the
-- caller's 'Nix.Store.DB.registerPaths' leaves a read-only, unregistered
-- tree; unpacking over it would then fail with permission-denied on every
-- retry, permanently wedging substitution of that path.
-- 'Dir.removePathForcibly' clears read-only marks and accepts a missing
-- path, so the fresh unpack always starts from a clean slate.
-- Substitution callers reach this only holding the path's exclusive
-- lock ('underPathLock', 'unpackVerifiedNar'): unlocked, this deletion is
-- exactly the race that lets one process remove a tree another just
-- registered.
clearStaleDestination :: FilePath -> IO ()
clearStaleDestination = Dir.removePathForcibly
