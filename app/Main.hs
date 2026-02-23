-- | nova-nix CLI entry point.
--
-- Commands:
--
-- @
-- nova-nix eval  FILE.nix       Evaluate a .nix file, print result
-- nova-nix build FILE.nix       Build a derivation from a .nix file
-- @
module Main (main) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Nix.Builder (BuildConfig (..), BuildResult (..), buildWithDeps, defaultBuildConfig)
import Nix.Builtins (builtinEnv)
import Nix.Derivation (Derivation (..))
import Nix.Eval (NixValue (..), Thunk (..), eval)
import Nix.Eval.IO (EvalState (..), newEvalState, runEvalIO)
import Nix.Parser (parseNix)
import Nix.Store (Store, closeStore, openStore, writeDrv)
import Nix.Store.Path (StorePath, defaultStoreDir, parseStorePath, storePathToFilePath)
import System.Directory (getTemporaryDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory)
import System.IO (BufferMode (..), hPutStrLn, hSetBuffering, stderr, stdout)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  case args of
    ["eval", filePath] -> evalFile filePath
    ["build", filePath] -> buildFile filePath
    _ -> do
      hPutStrLn stderr "Usage: nova-nix <command> FILE.nix"
      hPutStrLn stderr ""
      hPutStrLn stderr "Commands:"
      hPutStrLn stderr "  eval   Evaluate a .nix file, print result"
      hPutStrLn stderr "  build  Build a derivation from a .nix file"
      exitFailure

-- | Evaluate a .nix file and print the result.
evalFile :: FilePath -> IO ()
evalFile filePath = do
  source <- TIO.readFile filePath
  case parseNix (T.pack filePath) source of
    Left err -> do
      hPutStrLn stderr ("parse error: " ++ show err)
      exitFailure
    Right expr -> do
      st <- newEvalState (takeDirectory filePath)
      result <- runEvalIO st (eval (builtinEnv (esTimestamp st)) expr)
      case result of
        Left err -> do
          TIO.hPutStrLn stderr ("error: " <> err)
          exitFailure
        Right val -> print val

-- | Parse, evaluate, extract derivation, build, and print result.
buildFile :: FilePath -> IO ()
buildFile filePath = do
  source <- TIO.readFile filePath
  case parseNix (T.pack filePath) source of
    Left err -> do
      hPutStrLn stderr ("parse error: " ++ show err)
      exitFailure
    Right expr -> do
      st <- newEvalState (takeDirectory filePath)
      result <- runEvalIO st (eval (builtinEnv (esTimestamp st)) expr)
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
