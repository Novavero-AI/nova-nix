-- | Derivation builder — executes build recipes.
--
-- == The build process
--
-- When Nix needs to build a derivation (cache miss), it:
--
-- 1. Creates a temp directory for the build
-- 2. Sets up the environment: only store paths from @inputDrvs@ and
--    @inputSrcs@ are visible.  PATH contains only the builder and
--    specified dependencies.  No internet access (in sandboxed mode).
-- 3. Runs the builder (usually @bash -e \/nix\/store\/...-stdenv\/setup@)
-- 4. The @setup@ script sources the derivation's environment variables,
--    then runs @genericBuild@ which calls @unpackPhase@, @patchPhase@,
--    @configurePhase@, @buildPhase@, @installPhase@, @fixupPhase@, etc.
-- 5. Builder writes output to @$out@ (the output store path)
-- 6. Nix scans the output for references to other store paths
-- 7. Output is moved to the store and registered
--
-- == On Windows
--
-- The key difference is process creation.  Linux uses @fork\/exec@ with
-- namespace isolation.  We use 'System.Process.createProcess' which maps
-- to @CreateProcess@ on Windows — native, no POSIX layer.
--
-- For now, builds run without sandboxing (same as Nix on macOS did for
-- years).  Future work: Windows Job Objects for resource limits,
-- App Containers for filesystem isolation.
--
-- We ship @bash.exe@ (from MSYS2) as the default builder on Windows.
-- Same approach as Git for Windows.
module Nix.Builder
  ( -- * Build execution
    BuildResult (..),
    buildDerivation,

    -- * Build configuration
    BuildConfig (..),
    defaultBuildConfig,
  )
where

import Data.Text (Text)
import Nix.Derivation (Derivation)
import Nix.Store.Path (StoreDir, StorePath)

-- | Configuration for the build environment.
data BuildConfig = BuildConfig
  { -- | Store directory (where outputs go).
    bcStoreDir :: !StoreDir,
    -- | Temp directory for builds (cleaned after each build).
    bcTmpDir :: !FilePath,
    -- | Path to bash executable (shipped with nova-nix on Windows).
    bcBashPath :: !FilePath,
    -- | Enable sandboxing (not yet implemented on Windows).
    bcSandbox :: !Bool
  }
  deriving (Eq, Show)

-- | Default build configuration.
defaultBuildConfig :: StoreDir -> BuildConfig
defaultBuildConfig dir =
  BuildConfig
    { bcStoreDir = dir,
      bcTmpDir = "/tmp/nova-nix-build", -- TODO: platform-specific
      bcBashPath = "/bin/bash", -- TODO: ship bash.exe on Windows
      bcSandbox = False
    }

-- | Result of a build attempt.
data BuildResult
  = -- | Build succeeded. Output registered at this store path.
    BuildSuccess !StorePath
  | -- | Build failed with an error message and exit code.
    BuildFailure !Text !Int
  deriving (Eq, Show)

-- | Build a derivation: run its builder and capture output.
--
-- This is the core build loop:
-- 1. Check all input derivations are built (or substitute them)
-- 2. Set up temp build directory
-- 3. Set environment variables from the derivation
-- 4. Run the builder with the specified args
-- 5. On success: hash output, move to store, register in DB
-- 6. On failure: report error, clean up
buildDerivation :: BuildConfig -> Derivation -> IO BuildResult
buildDerivation _config _drv =
  -- TODO: implement the build loop
  pure $ BuildFailure "Builder not yet implemented" 1
