-- | nova-nix CLI entry point.
--
-- Commands:
--
-- @
-- nova-nix eval FILE.nix         Evaluate a .nix file, print result
-- nova-nix build FILE.nix        Build a derivation from a .nix file
-- nova-nix store info             Show store location and stats
-- nova-nix store gc               Garbage collect unreferenced paths
-- nova-nix store verify           Verify store integrity
-- @
module Main (main) where

import System.IO (BufferMode (..), hSetBuffering, stdout)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "nova-nix 0.1.0.0 — Windows-native Nix implementation"
  putStrLn "Not yet implemented. See https://github.com/Novavero-AI/nova-nix"
