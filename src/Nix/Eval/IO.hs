{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
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
  )
where

import Control.Exception (Exception, SomeException, displayException, fromException, throwIO, try)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (..), ask, asks, local)
import qualified Data.ByteString as BS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import Nix.Builtins (builtinEnv, builtinEnvWithScope, parseNixPath)
import Nix.Eval (eval)
import Nix.Eval.Types (MonadEval (..), NixValue (..), Thunk (..), ThunkCell (..))
import Nix.Expr.Types (AttrKey (..), Binding (..), Expr (..), Formal (..), Formals (..), NixAtom (..), StringPart (..))
import Nix.Hash (sha256Hex, truncatedBase32)
import Nix.Parser (parseNix)
import qualified System.Directory as Dir
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (isRelative, takeDirectory, (</>))
import qualified System.Process as Proc

-- ---------------------------------------------------------------------------
-- Error type
-- ---------------------------------------------------------------------------

-- | Evaluation error surfaced as an IO exception.
newtype NixEvalError = NixEvalError Text
  deriving (Show)

instance Exception NixEvalError

-- ---------------------------------------------------------------------------
-- State
-- ---------------------------------------------------------------------------

-- | Shared state for IO evaluation.
--
-- 'esImportCache' is a shared mutable cache (global across all frames).
-- Single-threaded only — switch to @MVar@ or @TVar@ if concurrent
-- evaluation is ever added.
--
-- 'esBaseDir' is immutable per frame — @import@ uses 'local' to set it
-- for nested evaluations, so it is exception-safe with no save\/restore.
--
-- 'esSearchPaths' holds parsed @NIX_PATH@ entries as thunks, populating
-- @builtins.nixPath@.
data EvalState = EvalState
  { esImportCache :: !(IORef (Map FilePath NixValue)),
    esBaseDir :: !FilePath,
    esStoreDir :: !FilePath,
    esTimestamp :: !Integer,
    esSearchPaths :: ![Thunk]
  }

-- | Create a fresh evaluation state rooted at the given directory.
-- Reads @NIX_PATH@ from the environment to populate search paths.
newEvalState :: FilePath -> IO EvalState
newEvalState baseDir = do
  cache <- newIORef Map.empty
  now <- fmap floor getPOSIXTime :: IO Integer
  nixPathStr <- lookupEnvText "NIX_PATH"
  let searchPaths = case nixPathStr of
        Just val -> parseNixPath (T.pack val)
        Nothing -> []
  pure
    EvalState
      { esImportCache = cache,
        esBaseDir = baseDir,
        esStoreDir = "/nix/store",
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

  catchEvalError (EvalIO action) = EvalIO $ do
    st <- ask
    result <- liftIO (try (runReaderT action st))
    pure (case result of Left (NixEvalError msg) -> Left msg; Right val -> Right val)

  readFileText path = wrapIO (TIO.readFile (T.unpack path))

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
        source <- wrapIO (TIO.readFile target)
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
            -- local sets new base dir for nested imports — pure, exception-safe
            let nested =
                  EvalIO
                    ( local
                        (\s -> s {esBaseDir = fileDir})
                        (unEvalIO (eval (builtinEnv timestamp searchPaths) expr))
                    )
            result <- nested
            wrapIO (modifyIORef' cacheRef (Map.insert target result))
            pure result

  getEnvVar name = wrapIO $ do
    mval <- lookupEnvText (T.unpack name)
    pure (maybe "" T.pack mval)

  getCurrentTime = EvalIO (asks esTimestamp)

  writeToStore name contents = do
    -- Validate name to prevent path traversal
    when (T.any (== '/') name) $
      throwEvalError ("writeToStore: name must not contain '/': " <> name)
    when (T.isInfixOf ".." name) $
      throwEvalError ("writeToStore: name must not contain '..': " <> name)
    when (T.any (== '\0') name) $
      throwEvalError ("writeToStore: name must not contain null bytes: " <> name)
    storeDir <- EvalIO (asks esStoreDir)
    let contentHash = sha256Hex (encodeUtf8 contents)
        inner = "nix-store:sha256:" <> contentHash <> ":" <> T.pack storeDir <> ":" <> name
        pathHash = truncatedBase32 (encodeUtf8 inner)
        storePath = T.pack storeDir <> "/" <> pathHash <> "-" <> name
        filePath = T.unpack storePath
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
    source <- wrapIO (TIO.readFile canonical)
    case parseNix (T.pack canonical) source of
      Left err ->
        throwEvalError
          ("scopedImport " <> T.pack canonical <> ": " <> T.pack (show err))
      Right rawExpr -> do
        let fileDir = takeDirectory canonical
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

  resolvePathLiteral path = do
    baseDir <- EvalIO (asks esBaseDir)
    let raw = T.unpack path
    if isRelative raw
      then pure (T.pack (baseDir </> raw))
      else pure path

  forceThunk _ (Evaluated val) = pure val
  forceThunk evalFn (ThunkRef ref) = do
    -- Per-thunk IORef memoization.  On first force, overwrites
    -- Pending with Computed — dropping the Expr and Env so they
    -- become unreachable and are GC'd.  Matches C++ Nix which
    -- mutates Value structs in place.
    cell <- EvalIO (liftIO (readIORef ref))
    case cell of
      Computed val -> pure val
      Pending expr env -> do
        val <- evalFn env expr
        EvalIO (liftIO (writeIORef ref (Computed val)))
        pure val

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Classify a filesystem path as @"regular"@, @"directory"@, @"symlink"@,
-- or @"unknown"@ — matching Nix's @builtins.readDir@ / @readFileType@.
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
    Left (err :: SomeException) ->
      case fromException err of
        Just nixErr -> throwIO (nixErr :: NixEvalError)
        Nothing -> throwIO (NixEvalError (T.pack (displayException err)))

-- | Run an IO evaluation, returning @Left@ on error.
--
-- Note: only catches 'NixEvalError'.  Async exceptions
-- (@StackOverflow@, @ThreadKilled@, etc.) propagate uncaught.
runEvalIO :: EvalState -> EvalIO a -> IO (Either Text a)
runEvalIO st (EvalIO action) = do
  result <- try (runReaderT action st)
  pure (case result of Left (NixEvalError msg) -> Left msg; Right val -> Right val)

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
      EAttrs isRec bindings -> EAttrs isRec (map goBinding bindings)
      EList elems -> EList (map goExpr elems)
      ESelect target path mDef ->
        ESelect (goExpr target) (map goKey path) (fmap goExpr mDef)
      EHasAttr target path -> EHasAttr (goExpr target) (map goKey path)
      EApp f x -> EApp (goExpr f) (goExpr x)
      ELambda formals body -> ELambda (goFormals formals) (goExpr body)
      ELet bindings body -> ELet (map goBinding bindings) (goExpr body)
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
