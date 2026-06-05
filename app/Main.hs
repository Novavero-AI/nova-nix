-- | nova-nix CLI entry point.
--
-- Commands:
--
-- @
-- nova-nix eval  FILE.nix                  Evaluate a .nix file, print result
-- nova-nix eval  --expr 'EXPR'             Evaluate an inline expression
-- nova-nix build FILE.nix                  Build a derivation from a .nix file
-- @
--
-- Flags:
--
-- @
-- --nix-path NAME=PATH   Add a search path entry (repeatable, merged with NIX_PATH)
-- --expr EXPR            Evaluate an inline expression instead of a file
-- @
module Main (main) where

import Control.Monad (void, (>=>))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Nix.Builder (BuildConfig (..), BuildResult (..), buildWithDeps, defaultBuildConfig)
import Nix.Builtins (builtinEnv, parseNixPath)
import Nix.Derivation (Derivation (..), DerivationOutput (..))
import Nix.Eval (MonadEval, NixValue (..), Thunk (..), attrSetFromMap, attrSetLookup, attrSetToAscList, attrSetToMap, eval, evaluated, force, readThunkValue)
import Nix.Eval.Arena (arenaInit)
import Nix.Eval.IO (EvalState (..), newEvalState, runEvalIO)
import Nix.Eval.Types (clistFromThunks, clistThunks, thunkToCPtr)
import Nix.Parser (parseNix, readFileAutoEncoding)
import Nix.Store (Store, closeStore, openStore, writeDrv)
import Nix.Store.Path (StorePath, defaultStoreDir, parseStorePath, platformStoreDir, storePathToFilePath)
import Paths_nova_nix (getDataDir)
import System.Directory (getCurrentDirectory, getTemporaryDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory)
import System.IO (BufferMode (..), hPutStrLn, hSetBuffering, stderr, stdout)

-- ---------------------------------------------------------------------------
-- Argument parsing
-- ---------------------------------------------------------------------------

-- | Parsed CLI options.
data CliOpts = CliOpts
  { optNixPaths :: ![T.Text],
    optStrict :: !Bool,
    optCommand :: !Command
  }

data Command
  = CmdEvalFile !FilePath
  | CmdEvalExpr !T.Text
  | CmdBuild !FilePath
  | CmdHelp

parseArgs :: [String] -> CliOpts
parseArgs = go (CliOpts [] False CmdHelp)
  where
    go opts [] = opts
    go opts ("--nix-path" : val : rest) =
      go (opts {optNixPaths = optNixPaths opts ++ [T.pack val]}) rest
    go opts ("--strict" : rest) =
      go (opts {optStrict = True}) rest
    go opts ("eval" : rest) = goEval opts rest
    go opts ("build" : path : rest) =
      go (opts {optCommand = CmdBuild path}) rest
    go opts _ = opts
    -- Sub-parser for eval: handles --strict and --expr interleaved with the file arg.
    goEval opts [] = opts
    goEval opts ("--strict" : rest) = goEval (opts {optStrict = True}) rest
    goEval opts ("--nix-path" : val : rest) =
      goEval (opts {optNixPaths = optNixPaths opts ++ [T.pack val]}) rest
    goEval opts ("--expr" : expr : rest) =
      go (opts {optCommand = CmdEvalExpr (T.pack expr)}) rest
    goEval opts (path : rest) =
      go (opts {optCommand = CmdEvalFile path}) rest

-- | Merge --nix-path entries, bundled data dir, and NIX_PATH search paths.
-- The data dir is appended last so user paths take priority.
mergeSearchPaths :: [T.Text] -> FilePath -> [Thunk] -> [Thunk]
mergeSearchPaths extraPaths dataDir envPaths =
  concatMap parseNixPath extraPaths ++ envPaths ++ parseNixPath (T.pack dataDir)

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  -- Initialize C data layer (symbol interning, thunk arena, env allocator)
  arenaInit
  args <- getArgs
  dataDir <- getDataDir
  let opts = parseArgs args
  case optCommand opts of
    CmdEvalFile filePath -> evalFile (optStrict opts) (optNixPaths opts) dataDir filePath
    CmdEvalExpr expr -> evalExpr (optStrict opts) (optNixPaths opts) dataDir expr
    CmdBuild filePath -> buildFile (optNixPaths opts) dataDir filePath
    CmdHelp -> do
      hPutStrLn stderr "Usage: nova-nix [--nix-path NAME=PATH] <command>"
      hPutStrLn stderr ""
      hPutStrLn stderr "Commands:"
      hPutStrLn stderr "  eval FILE.nix          Evaluate a .nix file, print result"
      hPutStrLn stderr "  eval --expr 'EXPR'     Evaluate an inline expression"
      hPutStrLn stderr "  build FILE.nix         Build a derivation from a .nix file"
      hPutStrLn stderr ""
      hPutStrLn stderr "Flags:"
      hPutStrLn stderr "  --strict               Deep-force all thunks before printing (warning: OOM on large results)"
      hPutStrLn stderr "  --nix-path NAME=PATH   Add search path (repeatable, merged with NIX_PATH)"
      exitFailure

-- | Evaluate a .nix file and print the result.
evalFile :: Bool -> [T.Text] -> FilePath -> FilePath -> IO ()
evalFile strict extraPaths dataDir filePath = do
  source <- readFileAutoEncoding filePath
  case parseNix (T.pack filePath) source of
    Left err -> do
      hPutStrLn stderr ("parse error: " ++ show err)
      exitFailure
    Right expr -> do
      st0 <- newEvalState (takeDirectory filePath)
      let searchPaths = mergeSearchPaths extraPaths dataDir (esSearchPaths st0)
          st = st0 {esSearchPaths = searchPaths}
      result <-
        runEvalIO st $
          eval (builtinEnv (esTimestamp st) searchPaths) expr >>= finalize strict
      case result of
        Left err -> do
          TIO.hPutStrLn stderr ("error: " <> err)
          exitFailure
        Right forced -> TIO.putStrLn (prettyValue forced)

-- | Evaluate an inline expression and print the result.
evalExpr :: Bool -> [T.Text] -> FilePath -> T.Text -> IO ()
evalExpr strict extraPaths dataDir source = do
  case parseNix "<expr>" source of
    Left err -> do
      hPutStrLn stderr ("parse error: " ++ show err)
      exitFailure
    Right expr -> do
      cwd <- getCurrentDirectory
      st0 <- newEvalState cwd
      let searchPaths = mergeSearchPaths extraPaths dataDir (esSearchPaths st0)
          st = st0 {esSearchPaths = searchPaths}
      result <-
        runEvalIO st $
          eval (builtinEnv (esTimestamp st) searchPaths) expr >>= finalize strict
      case result of
        Left err -> do
          TIO.hPutStrLn stderr ("error: " <> err)
          exitFailure
        Right forced -> TIO.putStrLn (prettyValue forced)

-- | Parse, evaluate, extract derivation, build, and print result.
buildFile :: [T.Text] -> FilePath -> FilePath -> IO ()
buildFile extraPaths dataDir filePath = do
  source <- readFileAutoEncoding filePath
  case parseNix (T.pack filePath) source of
    Left err -> do
      hPutStrLn stderr ("parse error: " ++ show err)
      exitFailure
    Right expr -> do
      st0 <- newEvalState (takeDirectory filePath)
      let searchPaths = mergeSearchPaths extraPaths dataDir (esSearchPaths st0)
          st = st0 {esSearchPaths = searchPaths}
      result <- runEvalIO st $ do
        val <- eval (builtinEnv (esTimestamp st) searchPaths) expr
        -- 'derivation' is a lazy wrapper now; force the attrs extractDerivation
        -- reads (a build legitimately needs the drvPath + closure).
        case val of
          VAttrs attrs ->
            mapM_
              (\k -> maybe (pure ()) (void . force) (attrSetLookup k attrs))
              ["_derivation", "drvPath"]
          _ -> pure ()
        pure val
      case result of
        Left err -> do
          TIO.hPutStrLn stderr ("eval error: " <> err)
          exitFailure
        Right val -> do
          (drv, drvSP) <- extractDerivation val
          store <- openStore platformStoreDir
          buildResult <- buildAndRegister store drv drvSP
          closeStore store
          case buildResult of
            BuildSuccess sp ->
              TIO.putStrLn (T.pack (storePathToFilePath platformStoreDir sp))
            BuildFailure msg code -> do
              TIO.hPutStrLn stderr ("build failed (exit " <> T.pack (show code) <> "): " <> msg)
              exitFailure

-- | Extract a Derivation and its store path from an evaluated value.
-- The value must be a VAttrs with type = "derivation", a _derivation key
-- holding the Derivation struct, and a drvPath key holding the .drv store path.
-- Both are computed by builtinDerivation during evaluation.
extractDerivation :: NixValue -> IO (Derivation, StorePath)
extractDerivation (VAttrs attrs) = do
  -- Check type = "derivation"
  case attrSetLookup "type" attrs of
    Just thunk | Just (VStr "derivation" _) <- readThunkValue thunk -> pure ()
    _ -> do
      hPutStrLn stderr "error: result is not a derivation (no type = \"derivation\")"
      exitFailure
  -- Extract the Derivation struct from _derivation
  drv <- case attrSetLookup "_derivation" attrs of
    Just thunk | Just (VDerivation d) <- readThunkValue thunk -> pure d
    _ -> do
      hPutStrLn stderr "error: derivation result missing _derivation field"
      exitFailure
  -- Extract drvPath — this is the store path of the .drv file itself,
  -- computed by hashing the ATerm serialization during evaluation.
  drvSP <- case attrSetLookup "drvPath" attrs of
    Just thunk | Just (VStr path _) <- readThunkValue thunk -> case parseStorePath defaultStoreDir path of
      Just sp -> pure sp
      Nothing -> do
        TIO.hPutStrLn stderr ("error: invalid drvPath: " <> path)
        exitFailure
    _ -> do
      hPutStrLn stderr "error: derivation result missing drvPath"
      exitFailure
  pure (drv, drvSP)
extractDerivation _ = do
  hPutStrLn stderr "error: result is not a derivation"
  exitFailure

-- | Write the .drv file to the store and build with dependency resolution.
-- The drvPath is the store path of the .drv file itself, extracted from
-- the evaluation result alongside the Derivation struct.
buildAndRegister :: Store -> Derivation -> StorePath -> IO BuildResult
buildAndRegister store drv drvSP = do
  -- Write the .drv file to store at its content-addressed path
  writeDrv store drv drvSP
  -- Build with dependency resolution
  tmpDir <- getTemporaryDirectory
  let config =
        (defaultBuildConfig platformStoreDir)
          { bcTmpDir = tmpDir
          }
  buildWithDeps config store drv drvSP

-- ---------------------------------------------------------------------------
-- Output formatting
-- ---------------------------------------------------------------------------

-- | Optionally deep-force a value before printing.
-- With @--strict@, all thunks are recursively materialized.
-- Without it, thunks display as @«thunk»@ — safe for large results.
finalize :: (MonadEval m) => Bool -> NixValue -> m NixValue
finalize True = deepForceValue
finalize False = pure

-- ---------------------------------------------------------------------------
-- Deep-force and pretty-print
-- ---------------------------------------------------------------------------

-- | Recursively force all thunks in a value, returning the fully
-- materialized tree.  Unlike 'deepForce' (which returns @()@), this
-- rebuilds the value with all thunks replaced by computed C thunks.
deepForceValue :: (MonadEval m) => NixValue -> m NixValue
deepForceValue (VList cl) = do
  let thunks = map Thunk (clistThunks cl)
  forced <- mapM (force >=> deepForceValue) thunks
  pure (VList (clistFromThunks (map (thunkToCPtr . evaluated) forced)))
deepForceValue (VAttrs attrs) = do
  let m = attrSetToMap attrs
  forced <- mapM (force >=> deepForceValue) m
  pure (VAttrs (attrSetFromMap (Map.map evaluated forced)))
deepForceValue val = pure val

-- | Nix-style pretty-printing of a fully forced value.
prettyValue :: NixValue -> T.Text
prettyValue (VInt n) = T.pack (show n)
prettyValue (VFloat f) = T.pack (show f)
prettyValue (VBool True) = "true"
prettyValue (VBool False) = "false"
prettyValue VNull = "null"
prettyValue (VStr s _) = "\"" <> escapeNixString s <> "\""
prettyValue (VPath p) = p
prettyValue (VList cl) =
  "[ " <> T.intercalate " " (map (prettyThunk . Thunk) (clistThunks cl)) <> " ]"
prettyValue (VAttrs attrs) =
  let entries = attrSetToAscList attrs
      rendered = map (\(k, t) -> k <> " = " <> prettyThunk t <> ";") entries
   in "{ " <> T.intercalate " " rendered <> " }"
prettyValue (VLambda {}) = "«lambda»"
prettyValue (VBuiltin name _) = "«builtin " <> name <> "»"
prettyValue (VCompiledRegex _) = "«compiled-regex»"
prettyValue (VDerivation drv) =
  case drvOutputs drv of
    (out : _) -> "«derivation " <> T.pack (storePathToFilePath platformStoreDir (doPath out)) <> "»"
    [] -> "«derivation»"

-- | Pretty-print a thunk.  After deep-forcing, all thunks should be
-- computed thunks render their value; pending thunks render as a placeholder.
prettyThunk :: Thunk -> T.Text
prettyThunk thunk = maybe "«thunk»" prettyValue (readThunkValue thunk)

-- | Escape a string for Nix-style output (quotes, backslashes, newlines, tabs, carriage returns).
escapeNixString :: T.Text -> T.Text
escapeNixString = T.concatMap escapeChar
  where
    escapeChar '\\' = "\\\\"
    escapeChar '"' = "\\\""
    escapeChar '\n' = "\\n"
    escapeChar '\t' = "\\t"
    escapeChar '\r' = "\\r"
    escapeChar c = T.singleton c
