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
    NixEvalError (..),
    NixAbortError (..),
  )
where

import Control.Exception (Exception, SomeAsyncException, SomeException, displayException, fromException, onException, throwIO, try)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (..), ask, asks, local)
import qualified Data.ByteString as BS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.StablePtr (castPtrToStablePtr, castStablePtrToPtr, deRefStablePtr, freeStablePtr, newStablePtr)
import Nix.Builtins (builtinEnv, builtinEnvWithScope, parseNixPath)
import Nix.Eval (eval)
import Nix.Eval.CList (CList (..))
import Nix.Eval.CThunk (CThunkPtr, cthunkGetAttrs, cthunkGetBcIdx, cthunkGetBool, cthunkGetCtxStr, cthunkGetFloat, cthunkGetInt, cthunkGetLambda, cthunkGetList, cthunkGetPath, cthunkGetStr, cthunkMarkBlackhole, cthunkMarkPending, cthunkPayload, cthunkSetComputed, cthunkSetComputedAttrs, cthunkSetComputedBool, cthunkSetComputedCtxStr, cthunkSetComputedFloat, cthunkSetComputedInt, cthunkSetComputedLambda, cthunkSetComputedList, cthunkSetComputedNull, cthunkSetComputedPath, cthunkSetComputedStr, cthunkState, cthunkValueTag)
import Nix.Eval.Symbol (Symbol (..), symbolIntern, symbolText)
import Nix.Eval.Types (AttrSet (..), Env (..), MonadEval (..), NixValue (..), Thunk (..), attrSetSize, emptyContext, marshalLambda, marshalStringContext, unmarshalLambdaValue, unmarshalStringContext, pattern ValueAttrs, pattern ValueBool, pattern ValueCtxStr, pattern ValueFloat, pattern ValueInt, pattern ValueLambda, pattern ValueList, pattern ValueNull, pattern ValuePath, pattern ValueStr)
import Nix.Expr.Types (AttrKey (..), Binding (..), Expr (..), Formal (..), Formals (..), NixAtom (..), StringPart (..))
import Nix.Hash (makeFixedOutputPath, sha256Digest, sha256Hex, truncatedBase32)
import Nix.Parser (parseNix, readFileAutoEncoding)
import qualified Nix.Store.Path as SP
import qualified NovaCache.NAR as NAR
import qualified System.Directory as Dir
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (isRelative, takeDirectory, takeFileName, (</>))
import System.IO (hPutStrLn, stderr)
import qualified System.Process as Proc

-- ---------------------------------------------------------------------------
-- Error type
-- ---------------------------------------------------------------------------

-- | Evaluation error surfaced as an IO exception (catchable by tryEval).
newtype NixEvalError = NixEvalError Text
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
    -- | Accumulated @.drv@ closure (drv store path text to its ATerm), recorded
    -- bottom-up by 'builtinDerivationStrict'.  The build driver reads this after
    -- evaluation to write every input @.drv@ to the store before building.
    esDrvClosure :: !(IORef (Map Text Text)),
    -- | Cache of source path to its store path (recursive NAR hash), so a path
    -- literal used across many derivations is hashed only once.
    esSourcePathCache :: !(IORef (Map Text Text)),
    esBaseDir :: !FilePath,
    esStoreDir :: !FilePath,
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
        esStoreDir = T.unpack SP.platformStoreDirText,
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
  throwEvalError msg = EvalIO (liftIO (throwIO (NixEvalError msg)))
  abortEvaluation msg = EvalIO (liftIO (throwIO (NixAbortError msg)))

  catchEvalError (EvalIO action) = EvalIO $ do
    st <- ask
    result <- liftIO (try (runReaderT action st))
    pure (case result of Left (NixEvalError msg) -> Left msg; Right val -> Right val)

  readFileText path = wrapIO (readFileAutoEncoding (T.unpack path))

  doesPathExist path = wrapIO (Dir.doesPathExist (T.unpack path))

  listDirectory path = wrapIO $ do
    let dir = T.unpack path
    entries <- Dir.listDirectory dir
    mapM (classifyEntry dir) entries

  importFile rawPath = do
    baseDir <- EvalIO (asks esBaseDir)
    timestamp <- EvalIO (asks esTimestamp)
    searchPaths <- EvalIO (asks esSearchPaths)
    let raw = T.unpack rawPath
        resolved = if isRelative raw then baseDir </> raw else raw
    canonical <- wrapIO (Dir.canonicalizePath resolved)
    -- Directory import: append /default.nix if target is a directory
    target <- wrapIO $ do
      isDir <- Dir.doesDirectoryExist canonical
      pure (if isDir then canonical </> "default.nix" else canonical)
    -- Check import cache (readIORef cannot throw, no wrapIO needed)
    cacheRef <- EvalIO (asks esImportCache)
    cache <- EvalIO (liftIO (readIORef cacheRef))
    case Map.lookup target cache of
      Just cached -> pure cached
      Nothing -> do
        source <- wrapIO (readFileAutoEncoding target)
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

  storeSourcePath rawPath = do
    ref <- EvalIO (asks esSourcePathCache)
    cached <- EvalIO (liftIO (Map.lookup rawPath <$> readIORef ref))
    case cached of
      Just hit -> pure hit
      Nothing -> do
        entry <- wrapIO (NAR.serialiseFromPath (T.unpack rawPath))
        let narDigest = sha256Digest (NAR.serialise entry)
            name = T.pack (takeFileName (T.unpack rawPath))
            sp = makeFixedOutputPath name "sha256" "recursive" narDigest
            spText = SP.storePathToText SP.defaultStoreDir sp
        EvalIO (liftIO (modifyIORef' ref (Map.insert rawPath spText)))
        pure spText

  getCurrentTime = EvalIO (asks esTimestamp)

  writeToStore name contents = do
    -- Validate name to prevent path traversal
    when (T.any (== '/') name) $
      throwEvalError ("writeToStore: name must not contain '/': " <> name)
    when (T.any (== '\\') name) $
      throwEvalError ("writeToStore: name must not contain '\\': " <> name)
    when (T.isInfixOf ".." name) $
      throwEvalError ("writeToStore: name must not contain '..': " <> name)
    when (T.any (== '\0') name) $
      throwEvalError ("writeToStore: name must not contain null bytes: " <> name)
    storeDir <- EvalIO (asks esStoreDir)
    -- The preimage is built inline against the LIVE storeDir (esStoreDir), not
    -- via Nix.Hash.makeStorePath which pins the canonical defaultStoreDir for
    -- cross-platform .drv-hash parity.  toFile outputs are host-local, so they
    -- must use the running store dir - keep this format in sync with
    -- makeStorePath's preimage (type:sha256:hex:storeDir:name).
    let contentHash = sha256Hex (encodeUtf8 contents)
        inner = "nix-store:sha256:" <> contentHash <> ":" <> T.pack storeDir <> ":" <> name
        pathHash = truncatedBase32 (encodeUtf8 inner)
        basename = pathHash <> "-" <> name
        -- File path uses platform separator for I/O
        filePath = storeDir </> T.unpack basename
        -- Store path text always uses forward slash (Nix convention)
        storePath = T.pack storeDir <> "/" <> basename
    wrapIO $ do
      Dir.createDirectoryIfMissing True storeDir
      TIO.writeFile filePath contents
    pure storePath

  scopedImportFile scope rawPath = do
    baseDir <- EvalIO (asks esBaseDir)
    timestamp <- EvalIO (asks esTimestamp)
    searchPaths <- EvalIO (asks esSearchPaths)
    let raw = T.unpack rawPath
        resolved = if isRelative raw then baseDir </> raw else raw
    canonical <- wrapIO (Dir.canonicalizePath resolved)
    -- Directory import: append /default.nix if target is a directory
    target <- wrapIO $ do
      isDir <- Dir.doesDirectoryExist canonical
      pure (if isDir then canonical </> "default.nix" else canonical)
    source <- wrapIO (readFileAutoEncoding target)
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

  readFileBytes path = wrapIO (BS.readFile (T.unpack path))

  getFileType path = wrapIO (classifyPath (T.unpack path))

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

  copyPathToStore srcPath name = do
    -- Validate name to prevent path traversal (matches writeToStore)
    when (T.any (== '/') name) $
      throwEvalError ("copyPathToStore: name must not contain '/': " <> name)
    when (T.any (== '\\') name) $
      throwEvalError ("copyPathToStore: name must not contain '\\': " <> name)
    when (T.isInfixOf ".." name) $
      throwEvalError ("copyPathToStore: name must not contain '..': " <> name)
    when (T.any (== '\0') name) $
      throwEvalError ("copyPathToStore: name must not contain null bytes: " <> name)
    storeDir <- EvalIO (asks esStoreDir)
    -- Inline host-local store-path preimage (see writeToStore's note): uses the
    -- live esStoreDir, not makeStorePath's pinned defaultStoreDir.
    let contentHash = sha256Hex (encodeUtf8 ("source:" <> srcPath <> ":" <> name))
        inner = "nix-store:sha256:" <> contentHash <> ":" <> T.pack storeDir <> ":" <> name
        pathHash = truncatedBase32 (encodeUtf8 inner)
        basename = pathHash <> "-" <> name
        -- File path uses platform separator for I/O
        destFilePath = storeDir </> T.unpack basename
        -- Store path text always uses forward slash (Nix convention)
        destPath = T.pack storeDir <> "/" <> basename
    wrapIO (copyToStoreIfMissing (T.unpack srcPath) destFilePath storeDir)
    pure destPath

  addFixedOutputFile name bytes = do
    -- Canonical fixed-output path: a sha256-pinned fetch must land at the same
    -- store path C++ Nix computes, so it stays reproducible and cache-compatible.
    let sp = makeFixedOutputPath name "sha256" "flat" (sha256Digest bytes)
        filePath = SP.storePathToFilePath SP.defaultStoreDir sp
        storePath = SP.storePathToText SP.defaultStoreDir sp
    wrapIO $ do
      Dir.createDirectoryIfMissing True (takeDirectory filePath)
      BS.writeFile filePath bytes
    pure storePath

  traceMessage msg = EvalIO (liftIO (hPutStrLn stderr (T.unpack msg)))

  resolvePathLiteral path = do
    baseDir <- EvalIO (asks esBaseDir)
    let raw = T.unpack path
    if isRelative raw
      then pure (T.pack (baseDir </> raw))
      else pure path

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
        Symbol sym <- symbolIntern t
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
      pure (VStr (symbolText (Symbol sym)) emptyContext)
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

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

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
      | otherwise -> throwIO (NixEvalError (T.pack (displayException err)))

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
      | Just (NixEvalError msg) <- fromException err -> pure (Left msg)
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

-- | Copy a source path (file or directory) to the store if not already present.
copyToStoreIfMissing :: FilePath -> FilePath -> FilePath -> IO ()
copyToStoreIfMissing src dest storeDir = do
  Dir.createDirectoryIfMissing True storeDir
  alreadyExists <- Dir.doesPathExist dest
  unless alreadyExists (copyPath src dest)

-- | Copy a file or directory tree to a destination.
copyPath :: FilePath -> FilePath -> IO ()
copyPath src dest = do
  isDir <- Dir.doesDirectoryExist src
  if isDir
    then do
      Dir.createDirectoryIfMissing True dest
      entries <- Dir.listDirectory src
      mapM_ (\entry -> copyPath (src </> entry) (dest </> entry)) entries
    else Dir.copyFile src dest
