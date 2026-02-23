{-# LANGUAGE ScopedTypeVariables #-}

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

import Control.Exception (SomeException, try)
import Control.Monad (when)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Nix.Derivation (Derivation (..), DerivationOutput (..))
import Nix.Store (Store (..), addToStore, isValid, scanReferences)
import Nix.Store.Path (StoreDir (..), StorePath (..), storePathToFilePath)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, removeDirectoryRecursive)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import qualified System.Info
import qualified System.Process as Proc

-- ---------------------------------------------------------------------------
-- Named constants
-- ---------------------------------------------------------------------------

-- | Environment variable for the build top directory.
envNixBuildTop :: Text
envNixBuildTop = "NIX_BUILD_TOP"

-- | Environment variable for the temp directory.
envTmpDir :: Text
envTmpDir = "TMPDIR"

-- | Environment variable for the Nix store path.
envNixStore :: Text
envNixStore = "NIX_STORE"

-- | Environment variable for PATH.
envPath :: Text
envPath = "PATH"

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

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
      bcTmpDir = "/tmp/nova-nix-build",
      bcBashPath = "/bin/bash",
      bcSandbox = False
    }

-- | Result of a build attempt.
data BuildResult
  = -- | Build succeeded. Output registered at this store path.
    BuildSuccess !StorePath
  | -- | Build failed with an error message and exit code.
    BuildFailure !Text !Int
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Build loop
-- ---------------------------------------------------------------------------

-- | Build a derivation: run its builder and capture output.
--
-- 1. Validate all inputs exist in the store
-- 2. Create temp build directory with output subdirs
-- 3. Set up environment from derivation + standard vars
-- 4. Run the builder process
-- 5. On success: scan references, move outputs to store, register
-- 6. On failure: clean up and report error
-- 7. Exception safety: wrap in try, convert to BuildFailure
buildDerivation :: BuildConfig -> Store -> Derivation -> IO BuildResult
buildDerivation config store drv = do
  result <- try (buildDerivationInner config store drv)
  case result of
    Right buildResult -> pure buildResult
    Left (err :: SomeException) ->
      pure (BuildFailure ("build exception: " <> T.pack (show err)) 1)

buildDerivationInner :: BuildConfig -> Store -> Derivation -> IO BuildResult
buildDerivationInner config store drv = do
  -- 1. Validate input sources exist
  inputsOk <- validateInputs store drv
  case inputsOk of
    Left errMsg -> pure (BuildFailure errMsg 1)
    Right () -> do
      -- 2. Create temp build directory
      let buildDir = computeBuildDir config drv
      createDirectoryIfMissing True buildDir

      -- 3. Create output directories inside build dir
      let outputDirs = [(doName out, buildDir </> T.unpack (doName out)) | out <- drvOutputs drv]
      mapM_ (createDirectoryIfMissing True . snd) outputDirs

      -- 4. Set up environment
      let environ = buildEnvironment config drv buildDir outputDirs

      -- 5. Run the builder
      let builderPath = T.unpack (drvBuilder drv)
          builderArgs = map T.unpack (drvArgs drv)
      exitResult <- runBuilder builderPath builderArgs environ buildDir
      case exitResult of
        Left (exitCode, stderr_) -> do
          -- 6. Failure: clean up
          cleanupBuildDir buildDir
          pure (BuildFailure ("builder failed: " <> stderr_) exitCode)
        Right () -> do
          -- 7. Success: register each output
          registerResult <- registerOutputs config store drv buildDir outputDirs
          -- Clean up build dir
          cleanupBuildDir buildDir
          pure registerResult

-- ---------------------------------------------------------------------------
-- Input validation
-- ---------------------------------------------------------------------------

-- | Check that all input sources and input derivation outputs are valid.
validateInputs :: Store -> Derivation -> IO (Either Text ())
validateInputs store drv = do
  -- Check input sources
  srcResults <- mapM (isValid store) (drvInputSrcs drv)
  let missingSrcs = [sp | (sp, valid) <- zip (drvInputSrcs drv) srcResults, not valid]
  if not (null missingSrcs)
    then pure (Left ("missing input sources: " <> T.intercalate ", " (map formatSP missingSrcs)))
    else do
      -- Check input derivation outputs
      let inputDrvPaths = Map.keys (drvInputDrvs drv)
      drvResults <- mapM (isValid store) inputDrvPaths
      let missingDrvs = [sp | (sp, valid) <- zip inputDrvPaths drvResults, not valid]
      if not (null missingDrvs)
        then pure (Left ("missing input derivations: " <> T.intercalate ", " (map formatSP missingDrvs)))
        else pure (Right ())

-- | Format a StorePath for error messages.
formatSP :: StorePath -> Text
formatSP sp = spHash sp <> "-" <> spName sp

-- ---------------------------------------------------------------------------
-- Build directory
-- ---------------------------------------------------------------------------

-- | Compute a unique build directory path based on the first output hash.
computeBuildDir :: BuildConfig -> Derivation -> FilePath
computeBuildDir config drv =
  let uniqueSuffix = case drvOutputs drv of
        (out : _) -> T.unpack (spHash (doPath out))
        [] -> "no-output"
   in bcTmpDir config </> uniqueSuffix

-- | Remove the build directory, ignoring errors.
cleanupBuildDir :: FilePath -> IO ()
cleanupBuildDir dir = do
  exists <- doesDirectoryExist dir
  when exists $ do
    result <- try (removeDirectoryRecursive dir)
    case (result :: Either SomeException ()) of
      Right () -> pure ()
      Left _ -> pure () -- Best effort cleanup

-- ---------------------------------------------------------------------------
-- Environment
-- ---------------------------------------------------------------------------

-- | Build the process environment from the derivation env + standard vars.
buildEnvironment ::
  BuildConfig ->
  Derivation ->
  FilePath ->
  [(Text, FilePath)] ->
  Map Text Text
buildEnvironment config drv buildDir outputDirs =
  let -- Start with derivation environment
      baseEnv = drvEnv drv
      -- Add output paths: $out, $dev, etc.
      outputEnv = Map.fromList [(name, T.pack path) | (name, path) <- outputDirs]
      -- Standard build variables
      standardEnv =
        Map.fromList
          [ (envNixBuildTop, T.pack buildDir),
            (envTmpDir, T.pack buildDir),
            (homeEnvVar, T.pack buildDir),
            (envNixStore, T.pack (unStoreDir (bcStoreDir config))),
            (envPath, defaultBuildPath)
          ]
   in -- Priority: output paths > derivation env > standard env
      Map.unions [outputEnv, baseEnv, standardEnv]

-- | Default PATH for builds.  Includes the store directory plus standard
-- system directories so that basic commands (mkdir, cp, echo) are available.
-- In a future sandboxed build, this will be restricted to only the
-- derivation's declared inputs.
defaultBuildPath :: Text
defaultBuildPath =
  if System.Info.os == "mingw32"
    then "C:\\Windows\\System32;C:\\Windows"
    else "/usr/bin:/bin:/usr/local/bin"

-- | The home directory environment variable name (platform-dependent).
homeEnvVar :: Text
homeEnvVar =
  if System.Info.os == "mingw32"
    then "USERPROFILE"
    else "HOME"

-- ---------------------------------------------------------------------------
-- Process execution
-- ---------------------------------------------------------------------------

-- | Run the builder process, returning either (exitCode, stderr) on failure
-- or () on success.
runBuilder ::
  FilePath ->
  [String] ->
  Map Text Text ->
  FilePath ->
  IO (Either (Int, Text) ())
runBuilder builderPath builderArgs environ workDir = do
  let envList = [(T.unpack k, T.unpack v) | (k, v) <- Map.toList environ]
      cp =
        (Proc.proc builderPath builderArgs)
          { Proc.cwd = Just workDir,
            Proc.env = Just envList,
            Proc.std_out = Proc.CreatePipe,
            Proc.std_err = Proc.CreatePipe
          }
  (exitCode, _stdout, stderr_) <- Proc.readCreateProcessWithExitCode cp ""
  case exitCode of
    ExitSuccess -> pure (Right ())
    ExitFailure code -> pure (Left (code, T.pack stderr_))

-- ---------------------------------------------------------------------------
-- Output registration
-- ---------------------------------------------------------------------------

-- | After a successful build, register each output in the store.
registerOutputs ::
  BuildConfig ->
  Store ->
  Derivation ->
  FilePath ->
  [(Text, FilePath)] ->
  IO BuildResult
registerOutputs config store drv _buildDir outputDirs = do
  let allCandidates = collectAllCandidates drv
      drvPathText = case drvOutputs drv of
        [] -> Nothing
        _ -> Just (T.pack (storePathToFilePath (bcStoreDir config) (StorePath "unknown" "unknown")))
  -- Register each output
  results <- mapM (registerSingleOutput config store allCandidates drvPathText) (zip (drvOutputs drv) outputDirs)
  case sequence results of
    Left errMsg -> pure (BuildFailure errMsg 1)
    Right _ ->
      -- Return the first output's store path
      case drvOutputs drv of
        (firstOut : _) -> pure (BuildSuccess (doPath firstOut))
        [] -> pure (BuildFailure "no outputs defined" 1)

-- | Register a single output: scan references, move to store, register in DB.
registerSingleOutput ::
  BuildConfig ->
  Store ->
  [StorePath] ->
  Maybe Text ->
  (DerivationOutput, (Text, FilePath)) ->
  IO (Either Text ())
registerSingleOutput config store candidates drvPathText (output, (_outName, outDir)) = do
  let targetSP = doPath output
      targetPath = storePathToFilePath (bcStoreDir config) targetSP
  -- Check if output directory exists (builder should have created it)
  exists <- doesDirectoryExist outDir
  if not exists
    then pure (Left ("output directory missing: " <> T.pack outDir))
    else do
      -- Scan for references in the output
      refs <- scanReferences (bcStoreDir config) candidates outDir
      -- Check if already in store (e.g. from a previous build)
      alreadyExists <- doesDirectoryExist targetPath
      if alreadyExists
        then pure (Right ())
        else do
          -- Move to store and register
          addToStore store outDir targetSP drvPathText refs
          pure (Right ())

-- | Collect all candidate store paths from the derivation's inputs.
-- Used for reference scanning.
collectAllCandidates :: Derivation -> [StorePath]
collectAllCandidates drv =
  let inputSrcs = drvInputSrcs drv
      inputDrvPaths = Map.keys (drvInputDrvs drv)
      outputPaths = map doPath (drvOutputs drv)
   in inputSrcs ++ inputDrvPaths ++ outputPaths
