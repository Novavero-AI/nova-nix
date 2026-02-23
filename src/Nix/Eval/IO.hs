{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | IO-based Nix evaluator.
--
-- Provides 'EvalIO', a concrete 'MonadEval' instance that performs
-- real file-system access.  The @import@ builtin reads, parses, and
-- evaluates @.nix@ files with a per-process import cache.
--
-- @
-- st <- newEvalState "/path/to/project"
-- result <- runEvalIO st (eval (builtinEnv 0) expr)
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
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (..), asks, local)
import qualified Crypto.Hash as CH
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word8)
import Nix.Builtins (builtinEnv, builtinEnvWithScope)
import Nix.Eval (eval)
import Nix.Eval.Types (MonadEval (..), NixValue)
import Nix.Parser (parseNix)
import NovaCache.Base32 (encode)
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
-- Single-threaded only — switch to 'MVar' or 'TVar' if concurrent
-- evaluation is ever added.
--
-- 'esBaseDir' is immutable per frame — @import@ uses 'local' to set it
-- for nested evaluations, so it is exception-safe with no save\/restore.
data EvalState = EvalState
  { esImportCache :: !(IORef (Map FilePath NixValue)),
    esBaseDir :: !FilePath,
    esStoreDir :: !FilePath,
    esTimestamp :: !Integer
  }

-- | Create a fresh evaluation state rooted at the given directory.
newEvalState :: FilePath -> IO EvalState
newEvalState baseDir = do
  cache <- newIORef Map.empty
  now <- fmap (floor . toRational) getPOSIXTime :: IO Integer
  pure
    EvalState
      { esImportCache = cache,
        esBaseDir = baseDir,
        esStoreDir = "/nix/store",
        esTimestamp = now
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
    st <- asks id
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
    let raw = T.unpack rawPath
        resolved = if isRelative raw then baseDir </> raw else raw
    canonical <- wrapIO (Dir.canonicalizePath resolved)
    -- Check import cache (readIORef cannot throw, no wrapIO needed)
    cacheRef <- EvalIO (asks esImportCache)
    cache <- EvalIO (liftIO (readIORef cacheRef))
    case Map.lookup canonical cache of
      Just cached -> pure cached
      Nothing -> do
        source <- wrapIO (TIO.readFile canonical)
        case parseNix (T.pack canonical) source of
          Left err ->
            throwEvalError
              ("import " <> T.pack canonical <> ": " <> T.pack (show err))
          Right expr -> do
            -- local sets new base dir for nested imports — pure, exception-safe
            let nested =
                  EvalIO
                    ( local
                        (\s -> s {esBaseDir = takeDirectory canonical})
                        (unEvalIO (eval (builtinEnv timestamp) expr))
                    )
            result <- nested
            wrapIO (modifyIORef' cacheRef (Map.insert canonical result))
            pure result

  getEnvVar name = wrapIO $ do
    mval <- lookupEnvText (T.unpack name)
    pure (maybe "" T.pack mval)

  getCurrentTime = EvalIO (asks esTimestamp)

  writeToStore name contents = do
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
    let raw = T.unpack rawPath
        resolved = if isRelative raw then baseDir </> raw else raw
    canonical <- wrapIO (Dir.canonicalizePath resolved)
    source <- wrapIO (TIO.readFile canonical)
    case parseNix (T.pack canonical) source of
      Left err ->
        throwEvalError
          ("scopedImport " <> T.pack canonical <> ": " <> T.pack (show err))
      Right expr -> do
        -- No import cache for scoped imports (different scopes = different results)
        let scopedEnv = builtinEnvWithScope timestamp scope
        EvalIO
          ( local
              (\s -> s {esBaseDir = takeDirectory canonical})
              (unEvalIO (eval scopedEnv expr))
          )

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

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Classify a directory entry as @"regular"@, @"directory"@, or @"symlink"@
-- (matching Nix's @builtins.readDir@ semantics).
classifyEntry :: FilePath -> FilePath -> IO (Text, Text)
classifyEntry parentDir name = do
  let fullPath = parentDir </> name
  isSym <- Dir.pathIsSymbolicLink fullPath
  if isSym
    then pure (T.pack name, "symlink")
    else do
      isDir <- Dir.doesDirectoryExist fullPath
      if isDir
        then pure (T.pack name, "directory")
        else pure (T.pack name, "regular")

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
-- ('StackOverflow', 'ThreadKilled', etc.) propagate uncaught.
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

-- | SHA-256 hex digest of a ByteString.
sha256Hex :: BS.ByteString -> Text
sha256Hex bs =
  let digest = CH.hash bs :: CH.Digest CH.SHA256
      bytes = BA.unpack digest
   in T.pack (concatMap byteToHex bytes)

-- | Truncate a SHA-256 digest to 20 bytes and Nix-base32 encode.
truncatedBase32 :: BS.ByteString -> Text
truncatedBase32 bs =
  let digest = CH.hash bs :: CH.Digest CH.SHA256
      bytes20 = BS.pack (take 20 (BA.unpack digest :: [Word8]))
   in encode bytes20

-- | Format a single byte as two hex digits.
byteToHex :: Word8 -> String
byteToHex w =
  let (hi, lo) = quotRem (fromIntegral w :: Int) 16
      hexDigit n = "0123456789abcdef" !! n
   in [hexDigit hi, hexDigit lo]
