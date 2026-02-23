-- | nova-nix CLI entry point.
--
-- Commands:
--
-- @
-- nova-nix eval FILE.nix         Evaluate a .nix file, print result
-- @
module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Nix.Builtins (builtinEnv)
import Nix.Eval (eval)
import Nix.Eval.IO (EvalState (..), newEvalState, runEvalIO)
import Nix.Parser (parseNix)
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
    _ -> do
      hPutStrLn stderr "Usage: nova-nix eval FILE.nix"
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
