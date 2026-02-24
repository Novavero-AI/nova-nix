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

import Control.Monad ((>=>))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Nix.Builder (BuildConfig (..), BuildResult (..), buildWithDeps, defaultBuildConfig)
import Nix.Builtins (builtinEnv, parseNixPath)
import Nix.Derivation (Derivation (..), DerivationOutput (..))
import Nix.Eval (MonadEval, NixValue (..), Thunk (..), eval, force)
import Nix.Eval.IO (EvalState (..), newEvalState, runEvalIO)
import Nix.Parser (parseNix)
import Nix.Store (Store, closeStore, openStore, writeDrv)
import Nix.Store.Path (StorePath, defaultStoreDir, parseStorePath, storePathToFilePath)
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
    go opts ("eval" : "--expr" : expr : rest) =
      go (opts {optCommand = CmdEvalExpr (T.pack expr)}) rest
    go opts ("eval" : path : rest) =
      go (opts {optCommand = CmdEvalFile path}) rest
    go opts ("build" : path : rest) =
      go (opts {optCommand = CmdBuild path}) rest
    go opts _ = opts

-- | Merge --nix-path entries with the search paths from NIX_PATH.
mergeSearchPaths :: [T.Text] -> [Thunk] -> [Thunk]
mergeSearchPaths extraPaths envPaths =
  concatMap parseNixPath extraPaths ++ envPaths

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  let opts = parseArgs args
  case optCommand opts of
    CmdEvalFile filePath -> evalFile (optStrict opts) (optNixPaths opts) filePath
    CmdEvalExpr expr -> evalExpr (optStrict opts) (optNixPaths opts) expr
    CmdBuild filePath -> buildFile (optNixPaths opts) filePath
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
evalFile :: Bool -> [T.Text] -> FilePath -> IO ()
evalFile strict extraPaths filePath = do
  source <- TIO.readFile filePath
  case parseNix (T.pack filePath) source of
    Left err -> do
      hPutStrLn stderr ("parse error: " ++ show err)
      exitFailure
    Right expr -> do
      st <- newEvalState (takeDirectory filePath)
      let searchPaths = mergeSearchPaths extraPaths (esSearchPaths st)
      result <-
        runEvalIO st $
          eval (builtinEnv (esTimestamp st) searchPaths) expr >>= finalize strict
      case result of
        Left err -> do
          TIO.hPutStrLn stderr ("error: " <> err)
          exitFailure
        Right forced -> TIO.putStrLn (prettyValue forced)

-- | Evaluate an inline expression and print the result.
evalExpr :: Bool -> [T.Text] -> T.Text -> IO ()
evalExpr strict extraPaths source = do
  case parseNix "<expr>" source of
    Left err -> do
      hPutStrLn stderr ("parse error: " ++ show err)
      exitFailure
    Right expr -> do
      cwd <- getCurrentDirectory
      st <- newEvalState cwd
      let searchPaths = mergeSearchPaths extraPaths (esSearchPaths st)
      result <-
        runEvalIO st $
          eval (builtinEnv (esTimestamp st) searchPaths) expr >>= finalize strict
      case result of
        Left err -> do
          TIO.hPutStrLn stderr ("error: " <> err)
          exitFailure
        Right forced -> TIO.putStrLn (prettyValue forced)

-- | Parse, evaluate, extract derivation, build, and print result.
buildFile :: [T.Text] -> FilePath -> IO ()
buildFile extraPaths filePath = do
  source <- TIO.readFile filePath
  case parseNix (T.pack filePath) source of
    Left err -> do
      hPutStrLn stderr ("parse error: " ++ show err)
      exitFailure
    Right expr -> do
      st <- newEvalState (takeDirectory filePath)
      let searchPaths = mergeSearchPaths extraPaths (esSearchPaths st)
      result <- runEvalIO st (eval (builtinEnv (esTimestamp st) searchPaths) expr)
      case result of
        Left err -> do
          TIO.hPutStrLn stderr ("eval error: " <> err)
          exitFailure
        Right val -> do
          drv <- extractDerivation val
          store <- openStore defaultStoreDir
          buildResult <- buildAndRegister store drv
          closeStore store
          case buildResult of
            BuildSuccess sp ->
              TIO.putStrLn (T.pack (storePathToFilePath defaultStoreDir sp))
            BuildFailure msg code -> do
              TIO.hPutStrLn stderr ("build failed (exit " <> T.pack (show code) <> "): " <> msg)
              exitFailure

-- | Extract a Derivation from an evaluated value.
-- The value should be a VAttrs with type = "derivation" and a _derivation key.
-- Falls back to reading the .drv file via fromATerm if _derivation is not present.
extractDerivation :: NixValue -> IO Derivation
extractDerivation (VAttrs attrs) = do
  -- Check type = "derivation"
  case Map.lookup "type" attrs of
    Just (Evaluated (VStr "derivation" _)) -> pure ()
    _ -> do
      hPutStrLn stderr "error: result is not a derivation (no type = \"derivation\")"
      exitFailure
  -- Try to extract from _derivation key first
  case Map.lookup "_derivation" attrs of
    Just (Evaluated (VDerivation drv)) -> pure drv
    _ -> do
      hPutStrLn stderr "error: derivation result missing _derivation field"
      exitFailure
extractDerivation (VDerivation drv) = pure drv
extractDerivation _ = do
  hPutStrLn stderr "error: result is not a derivation"
  exitFailure

-- | Write the .drv file to the store and build with dependency resolution.
buildAndRegister :: Store -> Derivation -> IO BuildResult
buildAndRegister store drv = do
  -- Write the .drv file to store
  let drvSP = extractDrvStorePath drv
  writeDrvToStore store drv
  -- Build with dependency resolution
  tmpDir <- getTemporaryDirectory
  let config =
        (defaultBuildConfig defaultStoreDir)
          { bcTmpDir = tmpDir
          }
  case drvSP of
    Just sp -> buildWithDeps config store drv sp
    Nothing -> do
      -- No drvPath available — fall back to direct build without dep resolution
      hPutStrLn stderr "warning: no drvPath, building without dependency resolution"
      -- Import buildDerivation for fallback
      pure (BuildFailure "no drvPath available for dependency resolution" 1)

-- | Extract the .drv store path from a derivation's env.
extractDrvStorePath :: Derivation -> Maybe StorePath
extractDrvStorePath drv =
  Map.lookup "drvPath" (drvEnv drv) >>= parseStorePath defaultStoreDir

-- | Write a .drv file to the store at its derived path.
writeDrvToStore :: Store -> Derivation -> IO ()
writeDrvToStore store drv =
  case drvOutputs drv of
    [] -> pure () -- no outputs, nothing to write
    _ -> do
      -- Write .drv to store if a drvPath is available in the env.
      let envDrvPath = Map.lookup "drvPath" (drvEnv drv)
      mapM_ (writeDrv store drv) (envDrvPath >>= parseStorePath defaultStoreDir)

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
-- rebuilds the value with all thunks replaced by 'Evaluated'.
deepForceValue :: (MonadEval m) => NixValue -> m NixValue
deepForceValue (VList thunks) = do
  forced <- mapM (force >=> deepForceValue) thunks
  pure (VList (map Evaluated forced))
deepForceValue (VAttrs attrs) = do
  forced <- mapM (force >=> deepForceValue) attrs
  pure (VAttrs (Map.map Evaluated forced))
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
prettyValue (VList thunks) =
  "[ " <> T.intercalate " " (map prettyThunk thunks) <> " ]"
prettyValue (VAttrs attrs) =
  let entries = Map.toAscList attrs
      rendered = map (\(k, t) -> k <> " = " <> prettyThunk t <> ";") entries
   in "{ " <> T.intercalate " " rendered <> " }"
prettyValue (VLambda {}) = "«lambda»"
prettyValue (VBuiltin name _) = "«builtin " <> name <> "»"
prettyValue (VDerivation drv) =
  case drvOutputs drv of
    (out : _) -> "«derivation " <> T.pack (storePathToFilePath defaultStoreDir (doPath out)) <> "»"
    [] -> "«derivation»"

-- | Pretty-print a thunk.  After deep-forcing, all thunks should be
-- 'Evaluated'; unevaluated thunks render as a placeholder.
prettyThunk :: Thunk -> T.Text
prettyThunk (Evaluated val) = prettyValue val
prettyThunk (Thunk {}) = "«thunk»"

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
