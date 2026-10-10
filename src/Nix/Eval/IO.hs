{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | IO-based Nix evaluator.
--
-- Provides 'EvalIO', a concrete 'MonadEval' instance that performs
-- real file-system access.  The @import@ builtin reads, parses, and
-- evaluates @.nix@ files with a per-process import cache.
--
-- @
-- st <- newEvalState "/path/to/project"
-- result <- runEvalIO st (eval (builtinEnv unrestrictedPolicy 0 []) expr)
-- @
module Nix.Eval.IO
  ( -- * Evaluator
    EvalIO,
    runEvalIO,

    -- * State
    EvalState (..),
    newEvalState,
    allowEvalPath,

    -- * Errors
    EvalErrorKind (..),
    NixEvalError (..),
    NixAbortError (..),
  )
where

import Control.Exception (Exception, IOException, SomeAsyncException, SomeException, displayException, fromException, onException, throwIO, try)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (..), ask, asks, local)
import Crypto.Random (getRandomBytes)
import qualified Data.ByteString as BS
import Data.Either (fromRight, isRight)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.StablePtr (castPtrToStablePtr, castStablePtrToPtr, deRefStablePtr, freeStablePtr, newStablePtr)
import Nix.Builtins (builtinEnv, builtinEnvWithScope, parseNixPath, rootScopeNames)
import Nix.Derivation (fromATerm)
import Nix.Environment (EnvLookup (..), lookupEnvBytes)
import Nix.Eval (eval)
import Nix.Eval.CList (CList (..))
import Nix.Eval.CThunk (CThunkPtr, cthunkGetAttrs, cthunkGetBcIdx, cthunkGetBool, cthunkGetCtxStr, cthunkGetFloat, cthunkGetInt, cthunkGetLambda, cthunkGetList, cthunkGetPath, cthunkGetStr, cthunkMarkBlackhole, cthunkMarkPending, cthunkPayload, cthunkSetComputed, cthunkSetComputedAttrs, cthunkSetComputedBool, cthunkSetComputedCtxStr, cthunkSetComputedFloat, cthunkSetComputedInt, cthunkSetComputedLambda, cthunkSetComputedList, cthunkSetComputedNull, cthunkSetComputedPath, cthunkSetComputedStr, cthunkState, cthunkValueTag)
import Nix.Eval.CallDepth (CallDepth, defaultMaxCallDepth, enterCallFrame, topLevelCallDepth)
import Nix.Eval.CanonPath (canonBaseName, canonPath, canonPathValue)
import Nix.Eval.Policy (AllowedPaths, EvalPolicy (..), allowPathIn, forbiddenPathMessage, isAbsolutePath, isAllowedPath, isAllowedPrefix, joinComponents, noAllowedPaths, pathComponents, pathsRestricted, unrestrictedPolicy, uriAccess)
import Nix.Eval.Symbol (Symbol (..), symbolBytes, symbolIntern, symbolInternBytes, symbolText)
import Nix.Eval.Types (AttrSet (..), Env (..), MonadEval (..), NixValue (..), PathExistence (..), Thunk (..), attrSetSize, bytesToTextLossy, emptyContext, marshalLambda, marshalStringContext, storePathOrThrow, unmarshalLambdaValue, unmarshalStringContext, pattern ValueAttrs, pattern ValueBool, pattern ValueCtxStr, pattern ValueFloat, pattern ValueInt, pattern ValueLambda, pattern ValueList, pattern ValueNull, pattern ValuePath, pattern ValueStr)
import Nix.Expr.Resolve (undefinedVariableMessage)
import Nix.Hash (bytesToHexText, makeFixedOutputPath, makeTextPath, sha256Digest)
import Nix.Parser (SourceError (..), parseNixWithScope, readFileAutoEncoding)
import Nix.Store (unpackNarEntry)
import Nix.Store.CaseSensitive (probeCaseSensitivity, processCaseHack, volumeCaseHack)
import qualified Nix.Store.ExecBit as ExecBit
import qualified Nix.Store.Path as SP
import qualified NovaCache.NAR as NAR
import qualified System.Directory as Dir
import System.Exit (ExitCode (..))
import System.FilePath (isAbsolute, isPathSeparator, isRelative, takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)
import qualified System.Process as Proc

-- ---------------------------------------------------------------------------
-- Error type
-- ---------------------------------------------------------------------------

-- | How an eval-time failure interacts with @builtins.tryEval@:
-- 'ErrorThrown' (@builtins.throw@, a failed @assert@) is catchable,
-- matching upstream's ThrownError\/AssertionError; 'ErrorUncatchable'
-- (type errors, missing attributes, IO failures) escapes tryEval like
-- every other upstream EvalError.
data EvalErrorKind = ErrorThrown | ErrorUncatchable
  deriving (Eq, Show)

-- | Evaluation error surfaced as an IO exception.
data NixEvalError = NixEvalError !EvalErrorKind !Text
  deriving (Show)

instance Exception NixEvalError

-- | Abort error - NOT catchable by tryEval (matches real Nix semantics).
newtype NixAbortError = NixAbortError Text
  deriving (Show)

instance Exception NixAbortError

-- ---------------------------------------------------------------------------
-- State
-- ---------------------------------------------------------------------------

-- | Shared state for IO evaluation.
--
-- 'esImportCache' is a shared mutable cache (global across all frames).
-- Single-threaded only - switch to @MVar@ or @TVar@ if concurrent
-- evaluation is ever added.
--
-- 'esBaseDir' is immutable per frame - @import@ uses 'local' to set it
-- for nested evaluations, so it is exception-safe with no save\/restore.
--
-- 'esSearchPaths' holds parsed @NIX_PATH@ entries as thunks, populating
-- @builtins.nixPath@.
data EvalState = EvalState
  { esImportCache :: !(IORef (Map FilePath NixValue)),
    -- | Cache of derivation modulo-hashes (drv store path to hex), populated
    -- bottom-up by 'builtinDerivationStrict' so input derivations can be
    -- substituted by their content hashes when computing output paths.
    esDrvModuloCache :: !(IORef (Map Text Text)),
    -- | Accumulated @.drv@ closure (drv store path text to its ATerm bytes),
    -- recorded bottom-up by 'builtinDerivationStrict'.  The build driver reads
    -- this after evaluation to write every input @.drv@ to the store before
    -- building.
    esDrvClosure :: !(IORef (Map Text BS.ByteString)),
    -- | Cache of source path to its store path (recursive NAR hash), so a path
    -- literal used across many derivations is hashed only once.
    esSourcePathCache :: !(IORef (Map Text Text)),
    -- | Every store object evaluation wrote (store path to its
    -- references), for the build driver to register before building -
    -- eval has no store DB handle.  All of them, not only
    -- @builtins.toFile@: an unrecorded write reaches @drvInputSrcs@ as
    -- an unregistered path and fails the build that names it.
    esStoreWriteCache :: !(IORef (Map Text ([SP.StorePath], SP.StoreWriteMode))),
    esBaseDir :: !FilePath,
    -- | Where store objects live on this machine, so eval's own reads and
    -- writes honor @--store@ the same way the builder does.
    esStoreDir :: !SP.StoreDir,
    esTimestamp :: !Int64,
    esSearchPaths :: ![Thunk],
    -- | The call frames active around the current evaluation and the
    -- @max-call-depth@ ceiling.  Per frame like 'esBaseDir':
    -- 'withCallFrame' scopes it with 'local', so a failure unwinding
    -- through a frame leaves the count it found, upstream's RAII guard
    -- with nothing to restore.
    esCallDepth :: !CallDepth,
    -- | What this evaluation may reach: upstream's @restrict-eval@ and
    -- @pure-eval@ settings and the @allowed-uris@ list.  Unrestricted
    -- unless the caller says otherwise, as upstream's defaults are.
    esPolicy :: !EvalPolicy,
    -- | The allowed path prefixes under a restricting policy: the search
    -- path roots the caller seeded with 'allowEvalPath', plus every store
    -- object this evaluation copies, fetches or writes (upstream's
    -- @allowPath@ after each such step), plus the fetchers' scratch
    -- directories.  Mutable because it grows as evaluation proceeds, like
    -- the import cache.
    esAllowedPaths :: !(IORef AllowedPaths)
  }

-- | Create a fresh evaluation state rooted at the given directory, reading
-- and writing store objects under the given store directory.
-- Reads @NIX_PATH@ from the environment to populate search paths, its
-- bytes unchanged, as upstream's @initGC@ reads it
-- (src/libexpr/eval-gc.cc at Nix 2.24.9).  The one refusal is a Windows
-- @NIX_PATH@ holding an unpaired UTF-16 surrogate, which has no UTF-8
-- form and raises an 'IOException'.  The call-depth ceiling starts at
-- upstream's default; a caller with a configured one replaces
-- 'esCallDepth'.
--
-- The state is unrestricted.  A restricting policy is set on 'esPolicy',
-- and the roots it should admit are seeded with 'allowEvalPath' before
-- evaluation starts, as upstream's constructor allows its lookup path.
newEvalState :: SP.StoreDir -> FilePath -> IO EvalState
newEvalState storeDir baseDir = do
  cache <- newIORef Map.empty
  drvCache <- newIORef Map.empty
  drvClosure <- newIORef Map.empty
  srcCache <- newIORef Map.empty
  storeWriteCache <- newIORef Map.empty
  allowedPaths <- newIORef noAllowedPaths
  now <- floor <$> getPOSIXTime :: IO Int64
  nixPath <- lookupEnvBytes nixPathVar
  searchPaths <- case nixPath of
    EnvUnset -> pure []
    EnvValue value -> pure (parseNixPath value)
    EnvUnpairedSurrogate ->
      ioError (userError "NIX_PATH holds an unpaired UTF-16 surrogate, which has no UTF-8 form")
  pure
    EvalState
      { esImportCache = cache,
        esDrvModuloCache = drvCache,
        esDrvClosure = drvClosure,
        esSourcePathCache = srcCache,
        esStoreWriteCache = storeWriteCache,
        esBaseDir = baseDir,
        esStoreDir = storeDir,
        esTimestamp = now,
        esSearchPaths = searchPaths,
        esCallDepth = topLevelCallDepth defaultMaxCallDepth,
        esPolicy = unrestrictedPolicy,
        esAllowedPaths = allowedPaths
      }

-- | Grant access to a path prefix (upstream @EvalState::allowPath@): the
-- caller seeds the search path roots before evaluation, and the instance
-- adds every store object the evaluation produces.  Like upstream's, this
-- is a no-op when nothing restricts access, so an unrestricted evaluation
-- does not accumulate a set it never consults.
allowEvalPath :: EvalState -> Text -> IO ()
allowEvalPath st path =
  when (pathsRestricted (esPolicy st)) $
    modifyIORef' (esAllowedPaths st) (allowPathIn path)

-- ---------------------------------------------------------------------------
-- EvalIO newtype
-- ---------------------------------------------------------------------------

-- | IO evaluation monad.  Wraps @ReaderT EvalState IO@.
newtype EvalIO a = EvalIO {unEvalIO :: ReaderT EvalState IO a}
  deriving (Functor, Applicative, Monad)

-- ---------------------------------------------------------------------------
-- MonadEval instance
-- ---------------------------------------------------------------------------

instance MonadEval EvalIO where
  throwEvalError msg = EvalIO (liftIO (throwIO (NixEvalError ErrorUncatchable msg)))
  throwCatchableError msg = EvalIO (liftIO (throwIO (NixEvalError ErrorThrown msg)))
  abortEvaluation msg = EvalIO (liftIO (throwIO (NixAbortError msg)))

  -- tryEval semantics: recover from a throw/assert only; an uncatchable
  -- eval error is rethrown (aborts are a separate exception type and
  -- never enter the 'try').
  catchEvalError (EvalIO action) = EvalIO $ do
    st <- ask
    result <- liftIO (try (runReaderT action st))
    case result of
      Left (NixEvalError ErrorThrown msg) -> pure (Left msg)
      Left err@(NixEvalError ErrorUncatchable _) -> liftIO (throwIO err)
      Right val -> pure (Right val)

  -- Over IO exceptions, so eval errors, aborts, and IO failures
  -- crossing 'wrapIO' all trigger the cleanup before propagating.
  onEvalError (EvalIO action) (EvalIO cleanup) = EvalIO $ do
    st <- ask
    liftIO (runReaderT action st `onException` runReaderT cleanup st)

  withCallFrame (EvalIO action) = EvalIO $ do
    depth <- asks esCallDepth
    either
      (unEvalIO . throwEvalError)
      (\deeper -> local (\s -> s {esCallDepth = deeper}) action)
      (enterCallFrame depth)

  -- A refused path reads as absent, as upstream's pathExists turns a
  -- RestrictedPathError into false.  The query decides the walk and the
  -- stat as prim_pathExists does: a plain path is resolved through its
  -- ancestors only and lstat'ed in place, so a symlink exists whatever
  -- it points at; a path that must be a directory is resolved fully and
  -- must stat as one.
  doesPathExist query path = do
    allowed <- pathAllowed (existenceResolution query) path
    if allowed
      then evalStoreTextPath path >>= \resolved -> wrapIO (existsAs query resolved)
      else pure False

  listDirectory path = do
    accessPath path
    dir <- evalStoreTextPath path
    wrapIO $ do
      entries <- Dir.listDirectory dir
      mapM (classifyEntry dir) entries

  importFile rawPath = do
    baseDir <- EvalIO (asks esBaseDir)
    timestamp <- EvalIO (asks esTimestamp)
    searchPaths <- EvalIO (asks esSearchPaths)
    policy <- EvalIO (asks esPolicy)
    -- Resolved, and so checked, before the cache is consulted, as
    -- upstream resolves the file before its fileEvalCache lookup: a
    -- cached import is no more readable than a fresh one.
    (target, ioTarget) <- resolveImportTarget baseDir rawPath
    -- Check import cache (readIORef cannot throw, no wrapIO needed)
    cacheRef <- EvalIO (asks esImportCache)
    cache <- EvalIO (liftIO (readIORef cacheRef))
    case Map.lookup target cache of
      Just cached -> pure cached
      Nothing -> do
        source <- wrapIO (readFileAutoEncoding ioTarget)
        let fileDir = takeDirectory target
        case parseNixWithScope (rootScopeNames policy) fileDir (T.pack target) source of
          Left err -> rejectSource "import" target err
          Right expr -> do
            -- local sets new base dir for nested imports - pure, exception-safe
            let nested =
                  EvalIO
                    ( local
                        (\s -> s {esBaseDir = fileDir})
                        (unEvalIO (eval (builtinEnv policy timestamp searchPaths) expr))
                    )
            result <- nested
            -- Skip caching very large attr sets (e.g. all-packages.nix
            -- with 30k+ entries) so GC can reclaim them.  nixpkgs only
            -- imports all-packages.nix once at the top level, so skipping
            -- the cache for it has zero performance cost.
            let shouldCache = case result of
                  VAttrs attrs -> attrSetSize attrs < importCacheMaxAttrs
                  _ -> True
            when shouldCache $
              wrapIO (modifyIORef' cacheRef (Map.insert target result))
            pure result

  -- Upstream's getEnv answers empty under either setting
  -- (@prim_getEnv@: @restrictEval || pureEval ? "" : getEnv(name)@).
  -- The read cannot throw, so it needs no wrapIO.
  getEnvVar name = do
    policy <- EvalIO (asks esPolicy)
    if pathsRestricted policy
      then pure BS.empty
      else do
        found <- EvalIO (liftIO (lookupEnvBytes name))
        case found of
          EnvUnset -> pure BS.empty
          EnvValue value -> pure value
          EnvUnpairedSurrogate ->
            throwEvalError ("builtins.getEnv: the value of '" <> bytesToTextLossy name <> "' holds an unpaired UTF-16 surrogate, which has no UTF-8 form")

  lookupDrvHash key = EvalIO $ do
    ref <- asks esDrvModuloCache
    liftIO (Map.lookup key <$> readIORef ref)

  cacheDrvHash key val = EvalIO $ do
    ref <- asks esDrvModuloCache
    liftIO (modifyIORef' ref (Map.insert key val))

  recordDrvAterm key aterm = EvalIO $ do
    ref <- asks esDrvClosure
    liftIO (modifyIORef' ref (Map.insert key aterm))

  -- Read a .drv from the store on a modulo-hash cache miss (a cross-session or
  -- appendContext reference).  Mirrors the build side's readDrvFromStore: map
  -- to the on-disk path, read the raw bytes, parse the ATerm byte-level (env
  -- values keep arbitrary bytes).  A file that cannot be read or does not
  -- parse is 'Nothing', which the caller turns into a loud modulo-hash error.
  readStoreDerivation sp = EvalIO $ do
    filePath <- asks ((`storeFilePath` sp) . esStoreDir)
    result <- liftIO (try (BS.readFile filePath) :: IO (Either IOException BS.ByteString))
    pure $ case result of
      Left _ -> Nothing
      Right bytes -> either (const Nothing) Just (fromATerm bytes)

  -- Look up a derivation recorded earlier this session by its .drv path,
  -- reusing the esDrvClosure ATerm map (populated bottom-up by recordDrvAterm).
  -- This recovers an in-session all-outputs reference's output names without a
  -- disk read - the .drv is not written to the store until after evaluation.
  lookupSessionDrv drvPathText = EvalIO $ do
    ref <- asks esDrvClosure
    closure <- liftIO (readIORef ref)
    pure (Map.lookup drvPathText closure >>= either (const Nothing) Just . fromATerm)

  storeSourcePath rawPath = do
    ref <- EvalIO (asks esSourcePathCache)
    cached <- EvalIO (liftIO (Map.lookup rawPath <$> readIORef ref))
    case cached of
      Just hit -> pure hit
      Nothing -> do
        -- Upstream names the copy baseNameOf(canonicalized path); path
        -- values arrive canonicalized, so the last segment is the name.
        -- The name is checked before the tree read: it derives from the
        -- path alone, and the tree behind it can be arbitrarily large.
        let name = canonBaseName rawPath
            copyContext = "cannot copy '" <> rawPath <> "' to the store"
        when (T.null name) $
          throwEvalError (copyContext <> ": the path has no base name")
        case SP.checkStorePathName name of
          Left err -> throwEvalError (copyContext <> ": " <> SP.storePathNameErrorText err)
          Right () -> pure ()
        accessPath rawPath
        resolvedSource <- evalStoreTextPath rawPath
        -- A path already in the store never reaches here (it coerces to
        -- itself), so this is a source, read as the build driver's
        -- restore reads it again.
        entry <- wrapIO (ExecBit.serialiseFromPath processCaseHack resolvedSource)
        let narDigest = sha256Digest (NAR.serialise entry)
        sp <- storePathOrThrow copyContext (makeFixedOutputPath name "sha256" "recursive" narDigest)
        let spText = canonicalStorePathText sp
        EvalIO (liftIO (modifyIORef' ref (Map.insert rawPath spText)))
        -- The copy is readable afterwards, as upstream's copyPathToStore
        -- allows its destination.
        allowPath spText
        pure spText

  getCurrentTime = EvalIO (asks esTimestamp)

  writeToStore name contents refs = do
    -- Upstream's text-path scheme via makeTextPath - the same scheme .drv
    -- paths use, so it is parity-validated: type @text:<refs>@, flat
    -- sha256 of the contents, canonical store dir.  Construction also
    -- validates the name, so the write below never targets a path
    -- outside the store root.  The contents are the string's RAW BYTES,
    -- hashed and written as-is: no encoding step, and no text-mode IO
    -- (which would CRLF-translate on Windows and store bytes that no
    -- longer match the hash that named the path).
    sp <- storePathOrThrow "builtins.toFile" (makeTextPath name (sha256Digest contents) refs)
    filePath <- evalFilePath sp
    let storePath = canonicalStorePathText sp
    wrapIO $ do
      Dir.createDirectoryIfMissing True (takeDirectory filePath)
      -- Adopt an existing file only when its bytes are these bytes: an
      -- interrupted earlier run can leave a truncated file here, and
      -- skipping on bare existence adopted it under this path's hash.
      -- 'Dir.removePathForcibly' clears read-only marks and accepts a
      -- missing path, so a sealed or squatting leftover rewrites too.
      existing <- readBytesIfPresent filePath
      unless (existing == Just contents) $ do
        Dir.removePathForcibly filePath
        BS.writeFile filePath contents
    recordStoreWrite storePath refs SP.WriteText
    -- Upstream's toFile returns the path through allowAndSetStorePathString.
    allowPath storePath
    pure storePath

  scopedImportFile scope rawPath = do
    baseDir <- EvalIO (asks esBaseDir)
    timestamp <- EvalIO (asks esTimestamp)
    searchPaths <- EvalIO (asks esSearchPaths)
    policy <- EvalIO (asks esPolicy)
    (target, ioTarget) <- resolveImportTarget baseDir rawPath
    source <- wrapIO (readFileAutoEncoding ioTarget)
    let fileDir = takeDirectory target
    case parseNixWithScope (Set.union (Set.fromList (map fst scope)) (rootScopeNames policy)) fileDir (T.pack target) source of
      Left err -> rejectSource "scopedImport" target err
      Right expr -> do
        -- No import cache for scoped imports (different scopes = different results)
        let scopedEnv = builtinEnvWithScope policy timestamp searchPaths scope
        EvalIO
          ( local
              (\s -> s {esBaseDir = fileDir})
              (unEvalIO (eval scopedEnv expr))
          )

  readFileBytes path = do
    accessPath path
    evalStoreTextPath path >>= \resolved -> wrapIO (BS.readFile resolved)

  -- An lstat of the path itself, no symlink walk: upstream's readFileType
  -- realises its argument without resolution, the allow list checks the
  -- path it is handed (so a refusal names the path asked for), and the
  -- posix accessor beneath refuses a symlink anywhere above it, in every
  -- mode, so the type of a path reached through a symlinked directory is
  -- never revealed.
  getFileType path = do
    accessPathDirect path
    refuseSymlinkedAncestor path
    evalStoreTextPath path >>= \resolved -> wrapIO (classifyPath resolved)

  runProcess cmd cmdArgs stdinText = wrapIO $ do
    let cp =
          (Proc.proc (T.unpack cmd) (map T.unpack cmdArgs))
            { Proc.std_in = Proc.CreatePipe,
              Proc.std_out = Proc.CreatePipe,
              Proc.std_err = Proc.CreatePipe
            }
    (exitCode, stdoutStr, stderrStr) <-
      Proc.readCreateProcessWithExitCode cp (T.unpack stdinText)
    let code = case exitCode of
          ExitSuccess -> 0
          ExitFailure n -> n
    pure (code, T.pack stdoutStr, T.pack stderrStr)

  createScratchDir prefix = do
    dir <- wrapIO $ do
      tmpBase <- Dir.getTemporaryDirectory
      suffix <- getRandomBytes scratchSuffixBytes
      -- Forward-slash join: the scratch path feeds sh pipelines (tar -C)
      -- and store copies, both of which accept '/' on every host.
      let dir = tmpBase <> "/" <> T.unpack (prefix <> bytesToHexText suffix)
      -- createDirectory is exclusive: an already-existing path fails the
      -- fetch rather than being silently adopted.
      Dir.createDirectory dir
      pure (T.pack dir)
    -- The evaluator's own scratch area, not ambient filesystem: what a
    -- fetch lands there is read back under the same rule that makes a
    -- fetched store path readable afterwards.  Allowed under its resolved
    -- spelling too, because the walk follows symlinks and a temp dir can
    -- sit behind one (macOS's /var is /private/var), and under the given
    -- spelling so the walk may traverse the link's own ancestors.
    resolved <- wrapIO (Dir.canonicalizePath (T.unpack dir))
    allowPath dir
    allowPath (T.pack resolved)
    pure dir

  removeScratchDir dir = wrapIO (Dir.removePathForcibly (T.unpack dir))

  copyPathToStore srcPath name expectedSha256 = do
    -- The name is checked before the tree read: it arrives independently
    -- of the source, and the tree can be arbitrarily large.
    let copyContext = "cannot copy '" <> srcPath <> "' to the store"
    case SP.checkStorePathName name of
      Left err -> throwEvalError (copyContext <> ": " <> SP.storePathNameErrorText err)
      Right () -> pure ()
    -- Content-addressed like upstream addToStore: recursive NAR sha256
    -- under the caller's name.  Same content means same path, so the
    -- existence check in copyToStoreIfMissing is sound - changed source
    -- content can never serve stale bytes from an earlier copy (the old
    -- scheme hashed the path STRING, so it did exactly that).
    -- The tree behind the root's symlinks, under the root's own name, as
    -- upstream's copyPathToStore and addPath store path.resolveSymlinks()
    -- under path.baseName(); the walk checks the policy on the way.
    resolvedText <- resolveSymlinks srcPath
    caseHack <- readCaseHack resolvedText
    resolvedSource <- evalStoreTextPath resolvedText
    entry <- wrapIO (ExecBit.serialiseFromPath caseHack resolvedSource)
    let narDigest = sha256Digest (NAR.serialise entry)
    case expectedSha256 of
      Just (subject, expected)
        | expected /= narDigest ->
            throwEvalError
              ( subject
                  <> ": hash mismatch: expected sha256:"
                  <> bytesToHexText expected
                  <> ", got sha256:"
                  <> bytesToHexText narDigest
              )
      _ -> pure ()
    sp <- storePathOrThrow copyContext (makeFixedOutputPath name "sha256" "recursive" narDigest)
    destFilePath <- evalFilePath sp
    let destPath = canonicalStorePathText sp
    -- Restored from the entry just hashed, not copied from the source:
    -- the store's volume may fold a pair the source's keeps apart, and
    -- the tree that lands is exactly the one the address names.
    wrapIO (unpackToStoreVerified destFilePath entry narDigest) >>= either throwEvalError pure
    recordStoreWrite destPath [] SP.WriteRecursive
    allowPath destPath
    pure destPath

  narHashOfPath path = do
    accessPath path
    caseHack <- readCaseHack path
    resolved <- evalStoreTextPath path
    wrapIO (sha256Digest . NAR.serialise <$> ExecBit.serialiseFromPath caseHack resolved)

  isExecutableFile path = do
    accessPath path
    evalStoreTextPath path >>= \resolved -> wrapIO (ExecBit.isExecutable resolved)

  setExecutableFile path = do
    accessPath path
    evalStoreTextPath path >>= \resolved -> wrapIO (ExecBit.markExecutable resolved)

  lookupFetchCache key = wrapIO $ do
    file <- fetchCacheFile key
    there <- Dir.doesFileExist file
    if not there
      then pure Nothing
      else do
        recorded <- try (BS.readFile file) :: IO (Either IOException BS.ByteString)
        pure $ case recorded of
          Left _ -> Nothing
          Right bytes -> either (const Nothing) Just (TE.decodeUtf8' bytes)

  adoptStorePath path = do
    resolved <- evalStoreTextPath path
    there <- wrapIO (Dir.doesPathExist resolved)
    -- Same recording the uncached fetch's copyPathToStore would have done.
    -- materializeEvalStoreWrites skips a path that is already valid, so
    -- re-recording one costs nothing and closes the case where the row is
    -- missing.
    when there $ do
      recordStoreWrite path [] SP.WriteRecursive
      -- A fetch result, whether fetched now or remembered: upstream
      -- allows the store path every fetcher returns.
      allowPath path
    pure there

  writeFetchCache key value = wrapIO $ do
    file <- fetchCacheFile key
    -- The cache is an optimisation: a machine that cannot write one still
    -- has to be able to build.
    _ <-
      try
        ( do
            Dir.createDirectoryIfMissing True (takeDirectory file)
            -- Written beside the entry and renamed onto it, so a reader
            -- sees either the whole entry or none of it. A plain write can
            -- be cut short and leave a torn file behind, and an entry is
            -- trusted for as long as its store path survives.
            let staging = file ++ ".tmp"
            BS.writeFile staging (TE.encodeUtf8 value)
            Dir.renameFile staging file
        ) ::
        IO (Either IOException ())
    pure ()

  -- The two checks of getFileType: upstream's readLink asserts no
  -- symlink above the path it is handed as well.
  readSymlinkTarget path = do
    accessPathDirect path
    refuseSymlinkedAncestor path
    evalStoreTextPath path >>= \resolved -> wrapIO (T.pack <$> Dir.getSymbolicLinkTarget resolved)

  addSourceNar name narBytes =
    case NAR.deserialise narBytes of
      -- Unreachable in practice: the bytes come from NAR.serialise of a
      -- tree this process just built.  Kept total for the FFI-adjacent
      -- boundary rather than trusting the round trip.
      Left err -> throwEvalError ("builtins.path: internal NAR round-trip error: " <> T.pack err)
      Right entry -> do
        sp <- storePathOrThrow "builtins.path" (makeFixedOutputPath name "sha256" "recursive" (sha256Digest narBytes))
        destFilePath <- evalFilePath sp
        let destPath = canonicalStorePathText sp
        -- Raised here rather than inside 'wrapIO', which would render
        -- the refusal through an exception's own text ("user error (...)").
        wrapIO (unpackToStoreVerified destFilePath entry (sha256Digest narBytes)) >>= either throwEvalError pure
        recordStoreWrite destPath [] SP.WriteRecursive
        allowPath destPath
        pure destPath

  addFixedOutputFile name bytes = do
    -- Canonical fixed-output path: a sha256-pinned fetch must land at the same
    -- store path C++ Nix computes, so it stays reproducible and cache-compatible.
    sp <- storePathOrThrow "builtins.fetchurl" (makeFixedOutputPath name "sha256" "flat" (sha256Digest bytes))
    filePath <- evalFilePath sp
    let storePath = canonicalStorePathText sp
    wrapIO $ do
      Dir.createDirectoryIfMissing True (takeDirectory filePath)
      -- Same verified adoption as the other writers: this one used to
      -- write unconditionally, which both adopted nothing (fine) and
      -- crashed on a sealed leftover (the write hits read-only).
      existing <- readBytesIfPresent filePath
      unless (existing == Just bytes) $ do
        Dir.removePathForcibly filePath
        BS.writeFile filePath bytes
    recordStoreWrite storePath [] SP.WriteFlat
    allowPath storePath
    pure storePath

  traceMessage msg = EvalIO (liftIO (hPutStrLn stderr (T.unpack msg)))

  evalPolicy = EvalIO (asks esPolicy)

  checkUri uri = do
    st <- EvalIO ask
    allowed <- EvalIO (liftIO (readIORef (esAllowedPaths st)))
    either throwEvalError pure (uriAccess (esPolicy st) allowed uri)

  -- Store text stays canonical in the value domain, as it does for an
  -- import target ('resolveImportTarget' says why); every other path
  -- resolves as a platform path, the policy walk having followed the
  -- same links first.
  resolveSymlinks path = do
    accessPath path
    if SP.isCanonicalStoreText path
      then pure (canonPath path)
      else wrapIO (canonPathValue . T.pack <$> Dir.canonicalizePath (T.unpack path))

  resolvePathLiteral path = do
    baseDir <- EvalIO (asks esBaseDir)
    policy <- EvalIO (asks esPolicy)
    -- ~/x resolves against the home directory (upstream lexes HPATH and
    -- expands it at eval); everything else relative joins the base dir.
    -- Both end at the producer gate ('canonPathValue'): the value is
    -- absolute, lexically canonical, and slash-spelled regardless of
    -- the base dir's native spelling - platform separators exist only
    -- at the filesystem boundary.  Under pure-eval the home directory is
    -- ambient state, and upstream's parser refuses the literal outright
    -- (parser.y, the HPATH rule); the same refusal lands here, where the
    -- expansion is.
    expanded <- case T.stripPrefix "~/" path of
      Just below
        | epPureEval policy ->
            throwEvalError ("the path '" <> path <> "' can not be resolved in pure mode")
        | otherwise -> do
            home <- wrapIO Dir.getHomeDirectory
            pure (home </> T.unpack below)
      Nothing -> pure (T.unpack path)
    let absolute = if isRelative expanded then baseDir </> expanded else expanded
    pure (canonPathValue (T.pack absolute))

  forceThunk evalFn (Thunk ptr) = do
    -- Force protocol: PENDING to BLACKHOLE to COMPUTED with memoization.
    -- Scalar values (int/float/bool/null) are stored inline in the
    -- thunk payload (no StablePtr), dispatched via val_tag.
    --
    -- Blackhole detection: PENDING thunks are marked BLACKHOLE before
    -- evaluation begins.  If evaluation re-enters the same thunk, it
    -- sees BLACKHOLE and reports infinite recursion.  This is safe with
    -- knot-tying (evalRecAttrs, evalLet, matchFormalSet) because those
    -- patterns create distinct thunks sharing an env - no thunk ever
    -- forces itself.
    state <- EvalIO (liftIO (cthunkState ptr))
    case state of
      1 {- COMPUTED -} ->
        EvalIO (liftIO (readComputed ptr))
      2 {- BLACKHOLE -} ->
        -- Infinite recursion is non-catchable (like abort), matching C++ Nix.
        -- tryEval must NOT catch blackholes - using abortEvaluation ensures
        -- the error propagates through tryEval/catchEvalError.
        abortEvaluation "infinite recursion encountered"
      _ {- PENDING -} -> do
        -- Bytecode thunks: read bc_idx + StablePtr Env.
        -- The Expr is gone (replaced by bc_idx in the struct).
        -- The Env is still a StablePtr (for knot-tying laziness).
        bcIdx <- EvalIO (liftIO (cthunkGetBcIdx ptr))
        envSp <- EvalIO (liftIO (cthunkPayload ptr))
        let pendingSp = castPtrToStablePtr envSp
        env <- EvalIO (liftIO (deRefStablePtr pendingSp))
        -- Mark blackhole BEFORE evaluation - any re-entry hits the
        -- BLACKHOLE branch above.
        _ <- EvalIO (liftIO (cthunkMarkBlackhole ptr))
        -- If the force throws (a builtins.throw caught by an upstream tryEval,
        -- a type error, a failed import), restore the thunk to PENDING and
        -- rethrow, so a later force of this shared thunk re-evaluates instead of
        -- taking the BLACKHOLE branch above and aborting with a bogus "infinite
        -- recursion".  Mirrors C++ Nix forceValue: catch (...) { restore; throw }.
        -- A genuine self-recursion still rethrows its NixAbortError, which
        -- escapes tryEval exactly as before.
        val <- EvalIO $ do
          st <- ask
          liftIO
            (runReaderT (unEvalIO (evalFn env bcIdx)) st `onException` cthunkMarkPending ptr)
        oldPayload <- EvalIO (liftIO (storeComputed ptr val))
        -- Free the pending env StablePtr.
        when (oldPayload /= nullPtr) $
          EvalIO (liftIO (freeStablePtr (castPtrToStablePtr oldPayload)))
        pure val

-- | Store a computed NixValue in a C thunk.
-- Scalars (int, float, bool, null) are stored inline (no StablePtr).
-- Complex values use StablePtr.  Returns old payload for cleanup.
storeComputed :: CThunkPtr -> NixValue -> IO (Ptr ())
storeComputed ptr val = case val of
  VInt n -> cthunkSetComputedInt ptr n
  VFloat d -> cthunkSetComputedFloat ptr d
  VBool b -> cthunkSetComputedBool ptr (if b then 1 else 0)
  VNull -> cthunkSetComputedNull ptr
  VAttrs (AttrSet cset) -> cthunkSetComputedAttrs ptr (castPtr cset)
  VPath p -> do
    Symbol sym <- symbolIntern p
    cthunkSetComputedPath ptr sym
  VStr t ctx
    | ctx == emptyContext -> do
        Symbol sym <- symbolInternBytes t
        cthunkSetComputedStr ptr sym
    | otherwise -> do
        csptr <- marshalStringContext t ctx
        cthunkSetComputedCtxStr ptr (castPtr csptr)
  VList (CList clistPtr) -> cthunkSetComputedList ptr (castPtr clistPtr)
  VLambda (Env envPtr) formals bodyBcIdx -> do
    lamPtr <- marshalLambda envPtr formals bodyBcIdx
    cthunkSetComputedLambda ptr lamPtr
  _ -> do
    valSp <- newStablePtr val
    cthunkSetComputed ptr (castStablePtrToPtr valSp)

-- | Read a computed NixValue from a C thunk.
-- Dispatches on val_tag: scalars are read inline, complex via StablePtr.
readComputed :: CThunkPtr -> IO NixValue
readComputed ptr = do
  tag <- cthunkValueTag ptr
  case tag of
    ValueInt -> VInt <$> cthunkGetInt ptr
    ValueFloat -> VFloat <$> cthunkGetFloat ptr
    ValueBool -> (\b -> VBool (b /= 0)) <$> cthunkGetBool ptr
    ValueNull -> pure VNull
    ValueStr -> do
      sym <- cthunkGetStr ptr
      pure (VStr (symbolBytes (Symbol sym)) emptyContext)
    ValuePath -> VPath . symbolText . Symbol <$> cthunkGetPath ptr
    ValueList -> do
      listPtr <- cthunkGetList ptr
      pure (VList (CList (castPtr listPtr)))
    ValueAttrs -> VAttrs . AttrSet . castPtr <$> cthunkGetAttrs ptr
    ValueCtxStr -> do
      csptr <- cthunkGetCtxStr ptr
      uncurry VStr <$> unmarshalStringContext (castPtr csptr)
    ValueLambda -> do
      lamPtr <- cthunkGetLambda ptr
      unmarshalLambdaValue lamPtr
    _ {- PTR -} -> do
      payloadPtr <- cthunkPayload ptr
      deRefStablePtr (castPtrToStablePtr payloadPtr)

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

-- | Maximum attribute set size for import caching.  Results larger than this
-- are not cached, allowing GC to reclaim them.  Prevents the import cache
-- from retaining huge attr sets like nixpkgs' 30k-entry all-packages.nix.
importCacheMaxAttrs :: Int
importCacheMaxAttrs = 1000

-- | The variable upstream reads its lookup path from.
nixPathVar :: BS.ByteString
nixPathVar = "NIX_PATH"

-- | Random bytes in a scratch-dir name suffix (hex-encoded).  128 bits:
-- unguessable by another local process, collision-free in practice.
scratchSuffixBytes :: Int
scratchSuffixBytes = 16

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- Access policy
-- ---------------------------------------------------------------------------

-- | Admit a store object this evaluation produced (upstream's @allowPath@
-- after a copy, fetch or write).
allowPath :: Text -> EvalIO ()
allowPath path = do
  st <- EvalIO ask
  EvalIO (liftIO (allowEvalPath st path))

-- | Refuse a read unless the whole path, symlinks resolved, lies in the
-- allowed prefixes.  Upstream's accessor checks every prefix as its
-- symlink walk pushes it (@SourceAccessor::resolveSymlinks@ through
-- @FilteringSourceAccessor::maybeLstat@, which throws at 2.24.9), so the
-- refusal names the first prefix outside the allow list - @/etc@ for
-- @/etc/passwd@ when nothing under @/etc@ is allowed - and a symlink's
-- target is checked where the walk follows it.  A no-op when nothing
-- restricts access.
accessPath :: Text -> EvalIO ()
accessPath path = do
  st <- EvalIO ask
  when (pathsRestricted (esPolicy st)) $ do
    allowed <- EvalIO (liftIO (readIORef (esAllowedPaths st)))
    walkAllowed ResolveFull st allowed path >>= either throwEvalError pure

-- | Refuse a read unless the path itself is allowed, no symlink walk: the
-- check upstream's accessor makes on an lstat or readlink of the path it
-- is handed.  The refusal names the path asked for.
accessPathDirect :: Text -> EvalIO ()
accessPathDirect path = do
  st <- EvalIO ask
  when (pathsRestricted (esPolicy st)) $ do
    allowed <- EvalIO (liftIO (readIORef (esAllowedPaths st)))
    let canonical = canonPathValue path
    unless (isAbsolutePath canonical) $ throwEvalError (relativePathMessage path)
    unless (isAllowedPath allowed canonical) $
      throwEvalError (forbiddenPathMessage (esPolicy st) canonical)

-- | Whether the walk would admit the path, for the one reader that
-- answers a Boolean instead of refusing.
pathAllowed :: SymlinkResolution -> Text -> EvalIO Bool
pathAllowed mode path = do
  st <- EvalIO ask
  if pathsRestricted (esPolicy st)
    then do
      allowed <- EvalIO (liftIO (readIORef (esAllowedPaths st)))
      isRight <$> walkAllowed mode st allowed path
    else pure True

-- | Refuse a path with a symlink anywhere above it, naming the deepest
-- one: upstream's posix accessor asserts this on an lstat or readlink of
-- the path it is handed (@PosixSourceAccessor::assertNoSymlinks@ over
-- the parent, at 2.24.9), in every mode, and names the first symlink it
-- meets walking up from the parent.
refuseSymlinkedAncestor :: Text -> EvalIO ()
refuseSymlinkedAncestor path = do
  st <- EvalIO ask
  mapM_ (check st) (strictAncestors (canonPathValue path))
  where
    check st ancestor = do
      target <- EvalIO (liftIO (symlinkTargetAt st ancestor))
      when (isJust target) $
        throwEvalError ("path '" <> ancestor <> "' is a symlink")

-- | A canonical path's ancestors below the root, deepest first.
strictAncestors :: Text -> [Text]
strictAncestors path =
  [joinComponents (take n components) | n <- [length components - 1, length components - 2 .. 1]]
  where
    components = pathComponents path

-- | How a 'PathExistence' query walks the path: a directory is demanded
-- through the final symlink, an entry is found in place.
existenceResolution :: PathExistence -> SymlinkResolution
existenceResolution ExistsAsDirectory = ResolveFull
existenceResolution ExistsAsEntry = ResolveAncestors

-- | The stat a 'PathExistence' query ends in: a directory through every
-- symlink, or an entry of any kind in place.
existsAs :: PathExistence -> FilePath -> IO Bool
existsAs ExistsAsDirectory = Dir.doesDirectoryExist
existsAs ExistsAsEntry = lstatExists

-- | Whether an entry is at the path, a dangling symlink included: an
-- lstat, as upstream's @maybeLstat@ is.  'Dir.pathIsSymbolicLink' is the
-- lstat the directory package exposes, and it throws for a missing path,
-- so any failure is absence, as it is for 'Dir.doesPathExist'.
lstatExists :: FilePath -> IO Bool
lstatExists fp = do
  outcome <- try (Dir.pathIsSymbolicLink fp) :: IO (Either IOException Bool)
  pure (isRight outcome)

-- | Upstream's @coerceToPath@ refusal of a relative string.  The builtins
-- refuse one before it gets here; the gate repeats the refusal because
-- its soundness depends on it.
relativePathMessage :: Text -> Text
relativePathMessage path = "string '" <> path <> "' doesn't represent an absolute path"

-- | Which symlinks a walk follows, upstream's @SymlinkResolution@: every
-- one, or every one but the last component's, which is then checked in
-- place.
data SymlinkResolution = ResolveFull | ResolveAncestors
  deriving (Eq, Show)

-- | The symlink-resolving prefix walk of upstream's
-- @SourceAccessor::resolveSymlinks@ at 2.24.9, with the allow list
-- consulted where its @maybeLstat@ would be: each component is pushed,
-- the prefix so far must be allowed, and a symlink at that prefix has
-- its target spliced in front of what remains (an absolute target
-- restarts at the root).  'Left' carries the refusal for the first
-- prefix outside the allow list.  Under 'ResolveAncestors' the last
-- component's own symlink is left alone, as upstream consults
-- @maybeLstat@ only while something remains to resolve.
walkAllowed :: SymlinkResolution -> EvalState -> AllowedPaths -> Text -> EvalIO (Either Text ())
walkAllowed mode st allowed path
  | not (isAbsolutePath canonical) = pure (Left (relativePathMessage path))
  | otherwise = go symlinkFollowLimit [] (pathComponents canonical)
  where
    canonical = canonPathValue path
    policy = esPolicy st
    go _ _ [] = pure (Right ())
    go linksLeft done (component : rest)
      | component == "." = go linksLeft done rest
      | component == ".." = go linksLeft (drop 1 done) rest
      | not (isAllowedPrefix allowed prefix) = pure (Left (forbiddenPathMessage policy prefixText))
      | otherwise = do
          target <- if followsLinkAt rest then EvalIO (liftIO (symlinkTargetAt st prefixText)) else pure Nothing
          case target of
            Nothing -> go linksLeft prefixReversed rest
            Just _
              | linksLeft == 0 ->
                  throwEvalError ("infinite symlink recursion in path '" <> canonical <> "'")
            Just link ->
              let linkComponents = filter (not . T.null) (T.split isPathSeparator link)
                  -- Spliced in front of the remainder; the walk resumes from
                  -- the root for an absolute target and from the link's own
                  -- directory otherwise.
                  resumeFrom = if isAbsoluteTarget link then [] else done
               in go (linksLeft - 1) resumeFrom (linkComponents ++ rest)
      where
        prefixReversed = component : done
        prefix = reverse prefixReversed
        prefixText = joinComponents prefix
    followsLinkAt rest = mode == ResolveFull || not (null rest)
    isAbsoluteTarget link = T.isPrefixOf "/" link || isAbsolute (T.unpack link)

-- | The target of the symlink at a path value, read through the store
-- mapping, or 'Nothing' when there is no symlink there.  A path that
-- does not exist is simply not a symlink; the read that follows reports
-- it missing.
symlinkTargetAt :: EvalState -> Text -> IO (Maybe Text)
symlinkTargetAt st pathText = do
  let fsPath = SP.storeTextToFilePath (esStoreDir st) pathText
  outcome <-
    try
      ( do
          isLink <- Dir.pathIsSymbolicLink fsPath
          if isLink then Just . T.pack <$> Dir.getSymbolicLinkTarget fsPath else pure Nothing
      ) ::
      IO (Either IOException (Maybe Text))
  pure (fromRight Nothing outcome)

-- | Upstream's bound on symlinks followed while resolving one path.
symlinkFollowLimit :: Int
symlinkFollowLimit = 1024

-- | Where a recorded fetch is kept: one file per key, under this user's
-- cache directory, named by the key's own hash. Outside the store on
-- purpose - it's a note about work already done, not a derivation input.
fetchCacheFile :: Text -> IO FilePath
fetchCacheFile key = do
  dir <- Dir.getXdgDirectory Dir.XdgCache "nova-nix/fetch"
  pure (dir </> T.unpack (bytesToHexText (sha256Digest (TE.encodeUtf8 key))))

-- | Where a store path lives on this machine: this evaluation's store dir
-- mapped to a filesystem path.  This is the 'SP.StorePath' direction;
-- path-value text goes through 'evalStoreTextPath'.  Both must read the
-- same store dir, since a store honored on one side and not the other
-- writes to one directory and reads from another.
storeFilePath :: SP.StoreDir -> SP.StorePath -> FilePath
storeFilePath = SP.storePathToFilePath

-- | Record a store object evaluation just wrote, so the build driver
-- registers it before a derivation naming it is built.  Every eval-time
-- writer goes through this: a write that skips it lands in
-- @drvInputSrcs@ with no registration row and fails the build with
-- \"references unregistered path\".  The mode names the scheme that
-- constructed the path, so materialization can verify the on-disk
-- content reproduces it before registering.
recordStoreWrite :: Text -> [SP.StorePath] -> SP.StoreWriteMode -> EvalIO ()
recordStoreWrite storePath refs mode = do
  cacheRef <- EvalIO (asks esStoreWriteCache)
  EvalIO (liftIO (modifyIORef' cacheRef (Map.insert storePath (refs, mode))))

-- | The file's bytes, 'Nothing' when nothing readable is there: absent,
-- or something unreadable squatting on the name; either way the writer
-- must replace rather than adopt.
readBytesIfPresent :: FilePath -> IO (Maybe BS.ByteString)
readBytesIfPresent path = do
  result <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
  pure (either (const Nothing) Just result)

-- | The NAR serialisation of what is on disk, 'Nothing' when nothing
-- serialisable is there.  Through 'ExecBit.serialiseFromPath', the
-- same walk every producer and verifier uses, so the comparison sees
-- the executable flags the store's model records, not the platform's
-- permission guesses.
narBytesIfPresent :: NAR.CaseHack -> FilePath -> IO (Maybe BS.ByteString)
narBytesIfPresent caseHack path = do
  onDisk <- Dir.doesPathExist path
  if not onDisk
    then pure Nothing
    else do
      result <- try (ExecBit.serialiseFromPath caseHack path) :: IO (Either IOException NAR.NarEntry)
      pure (either (const Nothing) (Just . NAR.serialise) result)

-- | The case-hack mode a read of path-value text runs under.  Store
-- text names a tree on the store's volume, read back as the store
-- reads its own paths ('volumeCaseHack' of the probed volume); any
-- other path is a source, read under the process's setting
-- ('processCaseHack').
readCaseHack :: Text -> EvalIO NAR.CaseHack
readCaseHack path
  | SP.isCanonicalStoreText path = do
      storeRoot <- EvalIO (asks (SP.unStoreDir . esStoreDir))
      wrapIO (volumeCaseHack <$> probeCaseSensitivity storeRoot)
  | otherwise = pure processCaseHack

-- | 'storeFilePath' against the store this evaluation was given.
evalFilePath :: SP.StorePath -> EvalIO FilePath
evalFilePath sp = EvalIO (asks ((`storeFilePath` sp) . esStoreDir))

-- | Resolve path-value text to a filesystem location against the store
-- this evaluation was given.  The read counterpart of 'evalFilePath':
-- both sides of every eval-time store access must agree on the store
-- dir, or a redirected store is written to and read from two places.
evalStoreTextPath :: Text -> EvalIO FilePath
evalStoreTextPath txt = EvalIO (asks ((`SP.storeTextToFilePath` txt) . esStoreDir))

-- | A store path's identity: the canonical @/nix/store@ spelling every
-- platform shares.  Hashes and eval-visible strings carry this form; it
-- never names a location on disk.
canonicalStorePathText :: SP.StorePath -> Text
canonicalStorePathText = SP.storePathToText SP.defaultStoreDir

-- | Fail an import whose file does not parse or bind.  Both are uncatchable,
-- as upstream's ParseError and UndefinedVarError are to @builtins.tryEval@.
-- An unbound variable reads exactly as upstream's message does; a syntax
-- error keeps the parser's detail behind the builtin and file it came from.
rejectSource :: Text -> FilePath -> SourceError -> EvalIO a
rejectSource builtinName target err = throwEvalError $ case err of
  SyntaxError syntax -> builtinName <> " " <> T.pack target <> ": " <> T.pack (show syntax)
  UndefinedVariable name -> undefinedVariableMessage name

-- | Resolve an import's raw path to the value-domain target (the import
-- cache key, the parse name, and the base dir the file's relative path
-- literals resolve against) and the filesystem location to read, with
-- both the path named and the file resolved checked against the policy:
-- upstream's @resolveExprPath@ walks the path it is handed, and
-- @parseExprFromFile@ walks the file it reads after @default.nix@ has
-- been appended, so a directory whose @default.nix@ is a symlink is
-- refused at the link's target.
--
-- Store text stays canonical in the value domain: 'Dir.canonicalizePath'
-- would attach the working drive to the rooted @/nix@ prefix on Windows,
-- so store text gets lexical canonicalization ('canonPath') only, and
-- its reads resolve through 'evalStoreTextPath'.  Every other path
-- resolves and canonicalizes as a platform path, where the two returned
-- forms coincide.
resolveImportTarget :: FilePath -> Text -> EvalIO (FilePath, FilePath)
resolveImportTarget baseDir rawPath = do
  accessPath rawPath
  (target, ioTarget) <- locate
  accessPath (T.pack target)
  pure (target, ioTarget)
  where
    locate
      | SP.isCanonicalStoreText rawPath = do
          let valueBase = T.unpack (canonPath rawPath)
          resolvedBase <- evalStoreTextPath (T.pack valueBase)
          isDir <- wrapIO (Dir.doesDirectoryExist resolvedBase)
          -- The value-domain join stays "/" so the canonical spelling survives.
          let valueTarget = if isDir then valueBase <> "/default.nix" else valueBase
          resolvedTarget <- evalStoreTextPath (T.pack valueTarget)
          pure (valueTarget, resolvedTarget)
      | otherwise = do
          let raw = T.unpack rawPath
              resolved = if isRelative raw then baseDir </> raw else raw
          canonical <- wrapIO (Dir.canonicalizePath resolved)
          -- Directory import: append /default.nix if target is a directory
          target <- wrapIO $ do
            isDir <- Dir.doesDirectoryExist canonical
            pure (if isDir then canonical </> "default.nix" else canonical)
          pure (target, target)

-- | Classify a filesystem path as @"regular"@, @"directory"@, @"symlink"@,
-- or @"unknown"@ - matching Nix's @builtins.readDir@ / @readFileType@.
classifyPath :: FilePath -> IO Text
classifyPath fp =
  firstMatch
    "unknown"
    [ (Dir.pathIsSymbolicLink fp, "symlink"),
      (Dir.doesDirectoryExist fp, "directory"),
      (Dir.doesFileExist fp, "regular")
    ]

-- | Return the label of the first predicate that holds, or the default.
firstMatch :: Text -> [(IO Bool, Text)] -> IO Text
firstMatch def [] = pure def
firstMatch def ((test, label) : rest) =
  test >>= \case
    True -> pure label
    False -> firstMatch def rest

-- | Classify a directory entry (name relative to parent).
classifyEntry :: FilePath -> FilePath -> IO (Text, Text)
classifyEntry parentDir name = do
  ty <- classifyPath (parentDir </> name)
  pure (T.pack name, ty)

-- | Convert IO exceptions to eval errors.
-- Guards against double-wrapping: if the exception is already a
-- 'NixEvalError', it is re-thrown as-is.
wrapIO :: IO a -> EvalIO a
wrapIO action = EvalIO $ liftIO $ do
  result <- try action
  case result of
    Right val -> pure val
    Left (err :: SomeException)
      | Just (_ :: SomeAsyncException) <- fromException err -> throwIO err
      | Just abortErr <- fromException err -> throwIO (abortErr :: NixAbortError)
      | Just nixErr <- fromException err -> throwIO (nixErr :: NixEvalError)
      | otherwise -> throwIO (NixEvalError ErrorUncatchable (T.pack (displayException err)))

-- | Run an IO evaluation, returning @Left@ on error.
--
-- Catches 'NixEvalError' (throw) and 'NixAbortError' (abort).
-- Async exceptions (@StackOverflow@, @ThreadKilled@, etc.) propagate uncaught.
runEvalIO :: EvalState -> EvalIO a -> IO (Either Text a)
runEvalIO st (EvalIO action) = do
  result <- try (runReaderT action st)
  case result of
    Right val -> pure (Right val)
    Left (err :: SomeException)
      | Just (_ :: SomeAsyncException) <- fromException err -> throwIO err
      | Just (NixEvalError _ msg) <- fromException err -> pure (Left msg)
      | Just (NixAbortError msg) <- fromException err -> pure (Left msg)
      | otherwise -> pure (Left (T.pack (displayException err)))

-- ---------------------------------------------------------------------------
-- Store copy helpers
-- ---------------------------------------------------------------------------

-- | Unpack a NAR entry at its content-addressed destination.  An
-- existing destination is adopted only when its NAR digest is the
-- expected one - same content means same path, so a matching tree is
-- byte-identical by construction; anything else (an interrupted
-- earlier unpack, a squatter) is cleared and unpacked afresh.  The
-- entry is the one evaluation hashed to name the path, so the tree
-- that lands is the one the address names, with the store volume's
-- own sibling-name handling.
unpackToStoreVerified :: FilePath -> NAR.NarEntry -> BS.ByteString -> IO (Either Text ())
unpackToStoreVerified dest entry expectedDigest = do
  Dir.createDirectoryIfMissing True (takeDirectory dest)
  -- Probed per write rather than carried in 'EvalState': the store
  -- directory need not exist when evaluation starts, and the probe
  -- answers for a path on disk.  It was created just above, and the
  -- cost is one pathconf call per tree.
  sensitivity <- probeCaseSensitivity (takeDirectory dest)
  onDiskNar <- narBytesIfPresent (volumeCaseHack sensitivity) dest
  if (sha256Digest <$> onDiskNar) == Just expectedDigest
    then pure (Right ())
    else do
      Dir.removePathForcibly dest
      unpackNarEntry sensitivity dest entry
