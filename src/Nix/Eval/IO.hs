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
-- result <- runEvalIO st (eval (builtinEnv 0 []) expr)
-- @
module Nix.Eval.IO
  ( -- * Evaluator
    EvalIO,
    runEvalIO,

    -- * State
    EvalState (..),
    newEvalState,

    -- * Errors
    EvalErrorKind (..),
    NixEvalError (..),
    NixAbortError (..),
  )
where

import Control.Exception (Exception, SomeAsyncException, SomeException, displayException, fromException, onException, throwIO, try)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (..), ask, asks, local)
import Crypto.Random (getRandomBytes)
import qualified Data.ByteString as BS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.StablePtr (castPtrToStablePtr, castStablePtrToPtr, deRefStablePtr, freeStablePtr, newStablePtr)
import Nix.Builtins (builtinEnv, builtinEnvWithScope, parseNixPath)
import Nix.Derivation (fromATerm)
import Nix.Eval (eval)
import Nix.Eval.CList (CList (..))
import Nix.Eval.CThunk (CThunkPtr, cthunkGetAttrs, cthunkGetBcIdx, cthunkGetBool, cthunkGetCtxStr, cthunkGetFloat, cthunkGetInt, cthunkGetLambda, cthunkGetList, cthunkGetPath, cthunkGetStr, cthunkMarkBlackhole, cthunkMarkPending, cthunkPayload, cthunkSetComputed, cthunkSetComputedAttrs, cthunkSetComputedBool, cthunkSetComputedCtxStr, cthunkSetComputedFloat, cthunkSetComputedInt, cthunkSetComputedLambda, cthunkSetComputedList, cthunkSetComputedNull, cthunkSetComputedPath, cthunkSetComputedStr, cthunkState, cthunkValueTag)
import Nix.Eval.CanonPath (canonBaseName, canonPath, canonPathValue)
import Nix.Eval.Symbol (Symbol (..), symbolBytes, symbolIntern, symbolInternBytes, symbolText)
import Nix.Eval.Types (AttrSet (..), Env (..), MonadEval (..), NixValue (..), Thunk (..), attrSetSize, emptyContext, marshalLambda, marshalStringContext, storePathOrThrow, unmarshalLambdaValue, unmarshalStringContext, pattern ValueAttrs, pattern ValueBool, pattern ValueCtxStr, pattern ValueFloat, pattern ValueInt, pattern ValueLambda, pattern ValueList, pattern ValueNull, pattern ValuePath, pattern ValueStr)
import Nix.Expr.Types (AttrKey (..), Binding (..), Expr (..), Formal (..), Formals (..), NixAtom (..), StringPart (..))
import Nix.Hash (bytesToHexText, makeFixedOutputPath, makeTextPath, sha256Digest)
import Nix.Parser (parseNix, readFileAutoEncoding)
import Nix.Store (copyPathInto, unpackNarEntry)
import qualified Nix.Store.Path as SP
import qualified NovaCache.NAR as NAR
import qualified System.Directory as Dir
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (isRelative, takeDirectory, (</>))
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
    esBaseDir :: !FilePath,
    esTimestamp :: !Int64,
    esSearchPaths :: ![Thunk]
  }

-- | Create a fresh evaluation state rooted at the given directory.
-- Reads @NIX_PATH@ from the environment to populate search paths.
newEvalState :: FilePath -> IO EvalState
newEvalState baseDir = do
  cache <- newIORef Map.empty
  drvCache <- newIORef Map.empty
  drvClosure <- newIORef Map.empty
  srcCache <- newIORef Map.empty
  now <- floor <$> getPOSIXTime :: IO Int64
  nixPathStr <- lookupEnvText "NIX_PATH"
  let searchPaths = case nixPathStr of
        Just val -> parseNixPath (T.pack val)
        Nothing -> []
  pure
    EvalState
      { esImportCache = cache,
        esDrvModuloCache = drvCache,
        esDrvClosure = drvClosure,
        esSourcePathCache = srcCache,
        esBaseDir = baseDir,
        esTimestamp = now,
        esSearchPaths = searchPaths
      }

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

  doesPathExist path = wrapIO (Dir.doesPathExist (SP.storeTextToFilePath path))

  listDirectory path = wrapIO $ do
    let dir = SP.storeTextToFilePath path
    entries <- Dir.listDirectory dir
    mapM (classifyEntry dir) entries

  importFile rawPath = do
    baseDir <- EvalIO (asks esBaseDir)
    timestamp <- EvalIO (asks esTimestamp)
    searchPaths <- EvalIO (asks esSearchPaths)
    (target, ioTarget) <- resolveImportTarget baseDir rawPath
    -- Check import cache (readIORef cannot throw, no wrapIO needed)
    cacheRef <- EvalIO (asks esImportCache)
    cache <- EvalIO (liftIO (readIORef cacheRef))
    case Map.lookup target cache of
      Just cached -> pure cached
      Nothing -> do
        source <- wrapIO (readFileAutoEncoding ioTarget)
        case parseNix (T.pack target) source of
          Left err ->
            throwEvalError
              ("import " <> T.pack target <> ": " <> T.pack (show err))
          Right rawExpr -> do
            -- Resolve relative paths in the AST to absolute, matching
            -- real Nix (which resolves at parse time).  This ensures paths
            -- captured in closures remain valid after the import scope ends.
            let fileDir = takeDirectory target
                expr = resolveRelativePaths fileDir rawExpr
            -- local sets new base dir for nested imports - pure, exception-safe
            let nested =
                  EvalIO
                    ( local
                        (\s -> s {esBaseDir = fileDir})
                        (unEvalIO (eval (builtinEnv timestamp searchPaths) expr))
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

  getEnvVar name = wrapIO $ do
    mval <- lookupEnvText (T.unpack name)
    pure (maybe "" T.pack mval)

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
  -- values keep arbitrary bytes).  Any failure - absent file, malformed ATerm
  -- - is 'Nothing', which the caller turns into a loud modulo-hash error.
  readStoreDerivation sp = EvalIO $ do
    let filePath = platformFilePath sp
    result <- liftIO (try (BS.readFile filePath) :: IO (Either SomeException BS.ByteString))
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
        entry <- wrapIO (NAR.serialiseFromPath (SP.storeTextToFilePath rawPath))
        let narDigest = sha256Digest (NAR.serialise entry)
        sp <- storePathOrThrow copyContext (makeFixedOutputPath name "sha256" "recursive" narDigest)
        let spText = canonicalStorePathText sp
        EvalIO (liftIO (modifyIORef' ref (Map.insert rawPath spText)))
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
    let filePath = platformFilePath sp
        storePath = canonicalStorePathText sp
    wrapIO $ do
      Dir.createDirectoryIfMissing True (takeDirectory filePath)
      BS.writeFile filePath contents
    pure storePath

  scopedImportFile scope rawPath = do
    baseDir <- EvalIO (asks esBaseDir)
    timestamp <- EvalIO (asks esTimestamp)
    searchPaths <- EvalIO (asks esSearchPaths)
    (target, ioTarget) <- resolveImportTarget baseDir rawPath
    source <- wrapIO (readFileAutoEncoding ioTarget)
    case parseNix (T.pack target) source of
      Left err ->
        throwEvalError
          ("scopedImport " <> T.pack target <> ": " <> T.pack (show err))
      Right rawExpr -> do
        let fileDir = takeDirectory target
            expr = resolveRelativePaths fileDir rawExpr
        -- No import cache for scoped imports (different scopes = different results)
        let scopedEnv = builtinEnvWithScope timestamp searchPaths scope
        EvalIO
          ( local
              (\s -> s {esBaseDir = fileDir})
              (unEvalIO (eval scopedEnv expr))
          )

  readFileBytes path = wrapIO (BS.readFile (SP.storeTextToFilePath path))

  getFileType path = wrapIO (classifyPath (SP.storeTextToFilePath path))

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

  createScratchDir prefix = wrapIO $ do
    tmpBase <- Dir.getTemporaryDirectory
    suffix <- getRandomBytes scratchSuffixBytes
    -- Forward-slash join: the scratch path feeds sh pipelines (tar -C)
    -- and store copies, both of which accept '/' on every host.
    let dir = tmpBase <> "/" <> T.unpack (prefix <> bytesToHexText suffix)
    -- createDirectory is exclusive: an already-existing path fails the
    -- fetch rather than being silently adopted.
    Dir.createDirectory dir
    pure (T.pack dir)

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
    entry <- wrapIO (NAR.serialiseFromPath (SP.storeTextToFilePath srcPath))
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
    let destFilePath = platformFilePath sp
        destPath = canonicalStorePathText sp
    wrapIO (copyToStoreIfMissing (SP.storeTextToFilePath srcPath) destFilePath (takeDirectory destFilePath))
    pure destPath

  narHashOfPath path =
    wrapIO (sha256Digest . NAR.serialise <$> NAR.serialiseFromPath (SP.storeTextToFilePath path))

  isExecutableFile path = wrapIO (Dir.executable <$> Dir.getPermissions (SP.storeTextToFilePath path))

  readSymlinkTarget path = wrapIO (T.pack <$> Dir.getSymbolicLinkTarget (SP.storeTextToFilePath path))

  addSourceNar name narBytes =
    case NAR.deserialise narBytes of
      -- Unreachable in practice: the bytes come from NAR.serialise of a
      -- tree this process just built.  Kept total for the FFI-adjacent
      -- boundary rather than trusting the round trip.
      Left err -> throwEvalError ("builtins.path: internal NAR round-trip error: " <> T.pack err)
      Right entry -> do
        sp <- storePathOrThrow "builtins.path" (makeFixedOutputPath name "sha256" "recursive" (sha256Digest narBytes))
        let destFilePath = platformFilePath sp
            destPath = canonicalStorePathText sp
        wrapIO $ do
          alreadyThere <- Dir.doesPathExist destFilePath
          unless alreadyThere $ do
            Dir.createDirectoryIfMissing True (takeDirectory destFilePath)
            unpacked <- unpackNarEntry destFilePath entry
            either (throwIO . userError . T.unpack) pure unpacked
        pure destPath

  addFixedOutputFile name bytes = do
    -- Canonical fixed-output path: a sha256-pinned fetch must land at the same
    -- store path C++ Nix computes, so it stays reproducible and cache-compatible.
    sp <- storePathOrThrow "builtins.fetchurl" (makeFixedOutputPath name "sha256" "flat" (sha256Digest bytes))
    let filePath = platformFilePath sp
        storePath = canonicalStorePathText sp
    wrapIO $ do
      Dir.createDirectoryIfMissing True (takeDirectory filePath)
      BS.writeFile filePath bytes
    pure storePath

  traceMessage msg = EvalIO (liftIO (hPutStrLn stderr (T.unpack msg)))

  resolvePathLiteral path = do
    baseDir <- EvalIO (asks esBaseDir)
    -- ~/x resolves against the home directory (upstream lexes HPATH and
    -- expands it at eval); everything else relative joins the base dir.
    -- Both end at the producer gate ('canonPathValue'): the value is
    -- absolute, lexically canonical, and slash-spelled regardless of
    -- the base dir's native spelling - platform separators exist only
    -- at the filesystem boundary.
    expanded <- case T.stripPrefix "~/" path of
      Just below -> do
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

-- | Random bytes in a scratch-dir name suffix (hex-encoded).  128 bits:
-- unguessable by another local process, collision-free in practice.
scratchSuffixBytes :: Int
scratchSuffixBytes = 16

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Where a store path lives on this machine: the platform store dir
-- mapped to a filesystem path.  Every eval-time read and write of a
-- store object resolves through this, landing in the same store the
-- builder and CLI operate on.
platformFilePath :: SP.StorePath -> FilePath
platformFilePath = SP.storePathToFilePath SP.platformStoreDir

-- | A store path's identity: the canonical @/nix/store@ spelling every
-- platform shares.  Hashes and eval-visible strings carry this form; it
-- never names a location on disk.
canonicalStorePathText :: SP.StorePath -> Text
canonicalStorePathText = SP.storePathToText SP.defaultStoreDir

-- | Resolve an import's raw path to the value-domain target (the import
-- cache key, the parse name, and the base dir the file's relative path
-- literals resolve against) and the filesystem location to read.
--
-- Store text stays canonical in the value domain: 'Dir.canonicalizePath'
-- would attach the working drive to the rooted @/nix@ prefix on Windows,
-- so store text gets lexical canonicalization ('canonPath') only, and
-- its reads resolve through 'SP.storeTextToFilePath'.  Every other path
-- resolves and canonicalizes as a platform path, where the two returned
-- forms coincide.
resolveImportTarget :: FilePath -> Text -> EvalIO (FilePath, FilePath)
resolveImportTarget baseDir rawPath
  | SP.isCanonicalStoreText rawPath = do
      let valueBase = T.unpack (canonPath rawPath)
      isDir <- wrapIO (Dir.doesDirectoryExist (SP.storeTextToFilePath (T.pack valueBase)))
      -- The value-domain join stays "/" so the canonical spelling survives.
      let valueTarget = if isDir then valueBase <> "/default.nix" else valueBase
      pure (valueTarget, SP.storeTextToFilePath (T.pack valueTarget))
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

-- | Look up an environment variable, returning Nothing if unset.
lookupEnvText :: String -> IO (Maybe String)
lookupEnvText name = do
  result <- try (lookupEnv name)
  case (result :: Either SomeException (Maybe String)) of
    Left _ -> pure Nothing
    Right mval -> pure mval

-- ---------------------------------------------------------------------------
-- Path resolution (matching real Nix: resolve at parse time)
-- ---------------------------------------------------------------------------

-- | Resolve all relative 'NixPath' literals in an expression to absolute
-- paths relative to the given directory.  Real Nix resolves path literals
-- at parse time based on the source file location.  We do the same right
-- after parsing in 'importFile' so that paths captured in closures remain
-- valid after the import scope ends.
resolveRelativePaths :: FilePath -> Expr -> Expr
resolveRelativePaths dir = goExpr
  where
    goExpr expr = case expr of
      ELit (NixPath p)
        | isRelative (T.unpack p) ->
            ELit (NixPath (T.pack (dir </> T.unpack p)))
      ELit _ -> expr
      EStr parts -> EStr (map goPart parts)
      EIndStr parts -> EIndStr (map goPart parts)
      EVar _ -> expr
      EWithVar _ -> expr
      EResolvedVar _ _ -> expr
      EAttrs isRec bindings captureInfo -> EAttrs isRec (map goBinding bindings) captureInfo
      EList elems -> EList (map goExpr elems)
      ESelect target path mDef ->
        ESelect (goExpr target) (map goKey path) (fmap goExpr mDef)
      EHasAttr target path -> EHasAttr (goExpr target) (map goKey path)
      EApp f x -> EApp (goExpr f) (goExpr x)
      ELambda formals body captures -> ELambda (goFormals formals) (goExpr body) captures
      ELet bindings body captureInfo -> ELet (map goBinding bindings) (goExpr body) captureInfo
      EIf c t f -> EIf (goExpr c) (goExpr t) (goExpr f)
      EWith scope body -> EWith (goExpr scope) (goExpr body)
      EAssert cond body -> EAssert (goExpr cond) (goExpr body)
      EUnary op e -> EUnary op (goExpr e)
      EBinary op l r -> EBinary op (goExpr l) (goExpr r)
      ESearchPath _ -> expr

    goPart part = case part of
      StrLit _ -> part
      StrInterp e -> StrInterp (goExpr e)

    goBinding binding = case binding of
      NamedBinding path e -> NamedBinding (map goKey path) (goExpr e)
      Inherit from names -> Inherit (fmap goExpr from) names

    goKey key = case key of
      StaticKey _ -> key
      DynamicKey e -> DynamicKey (goExpr e)

    goFormals formals = case formals of
      FormalName _ -> formals
      FormalSet fs ellipsis -> FormalSet (map goFormal fs) ellipsis
      FormalNamedSet n fs ellipsis -> FormalNamedSet n (map goFormal fs) ellipsis

    goFormal (Formal n mDef) = Formal n (fmap goExpr mDef)

-- ---------------------------------------------------------------------------
-- Store copy helpers
-- ---------------------------------------------------------------------------

-- | Copy a source path (file or directory) to the store if not already
-- present.  The copy is 'copyPathInto', which replicates symlinks as
-- symlinks: the destination's name came from a NAR hash computed by
-- 'NovaCache.NAR.serialiseFromPath', which treats links as leaves, so a
-- dereferencing copy would store bytes that do not match their own
-- content address (and would not terminate on a link cycle).
copyToStoreIfMissing :: FilePath -> FilePath -> FilePath -> IO ()
copyToStoreIfMissing src dest storeDir = do
  Dir.createDirectoryIfMissing True storeDir
  alreadyExists <- Dir.doesPathExist dest
  unless alreadyExists (copyPathInto src dest)
