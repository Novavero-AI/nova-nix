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
    buildWithDeps,

    -- * Build configuration
    BuildConfig (..),
    defaultBuildConfig,
  )
where

import Control.Exception (SomeException, try)
import Control.Monad (filterM, when)
import Data.Char (toLower)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Nix.DependencyGraph (DepGraph, TopoResult (..), buildDepGraph, topoSort)
import qualified Nix.DependencyGraph
import Nix.Derivation (Derivation (..), DerivationOutput (..), fromATerm)
import Nix.Store (Store (..), addToStore, isValid, scanReferences)
import Nix.Store.Path (StoreDir (..), StorePath (..), storePathToFilePath)
import Nix.Substituter (CacheConfig, SubstResult (..), trySubstitute)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesPathExist, removeDirectoryRecursive)
import qualified System.Environment
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, takeFileName, (</>))
import qualified System.IO
import qualified System.IO.Unsafe
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
    bcSandbox :: !Bool,
    -- | Binary caches to try before building (checked in priority order).
    bcCaches :: ![CacheConfig]
  }
  deriving (Eq, Show)

-- | Default build configuration.
defaultBuildConfig :: StoreDir -> BuildConfig
defaultBuildConfig dir =
  BuildConfig
    { bcStoreDir = dir,
      bcTmpDir =
        if isWindows
          then "C:\\Temp\\nova-nix-build"
          else "/tmp/nova-nix-build",
      bcBashPath =
        if isWindows
          then "bash" -- rely on PATH (MSYS2/Git Bash)
          else "/bin/bash",
      bcSandbox = False,
      bcCaches = []
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

      -- 3. Compute output paths (but do NOT pre-create them — the builder
      --    is responsible for creating $out, $dev, etc.)
      let outputDirs = [(doName out, buildDir </> T.unpack (doName out)) | out <- drvOutputs drv]

      -- 4. Set up environment
      let builderPath = T.unpack (drvBuilder drv)
          environ = buildEnvironment config drv builderPath buildDir outputDirs

          -- 5. Run the builder
          builderArgs = map T.unpack (drvArgs drv)
      exitResult <- runBuilder builderPath builderArgs environ buildDir
      case exitResult of
        Left (exitCode, stderrText) -> do
          -- 6. Failure: clean up
          cleanupBuildDir buildDir
          pure (BuildFailure ("builder failed: " <> stderrText) exitCode)
        Right () -> do
          -- 7. Success: validate that the builder created all expected outputs.
          --    Outputs may be files or directories — both are valid (real Nix
          --    allows $out to be a single file, a directory tree, or a symlink).
          missing <- filterM (fmap not . doesPathExist . snd) outputDirs
          case missing of
            [] -> do
              registerResult <- registerOutputs config store drv buildDir outputDirs
              cleanupBuildDir buildDir
              pure registerResult
            _ -> do
              cleanupBuildDir buildDir
              let names = T.intercalate ", " (map fst missing)
              pure (BuildFailure ("builder succeeded but outputs missing: " <> names) 1)

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
-- The builder path is used to derive PATH entries — the builder's own
-- directory and its sibling @usr\/bin@ are included so that coreutils
-- shipped alongside the builder (e.g. Git for Windows' MSYS2 tools)
-- are available.  This mirrors real Nix where PATH contains only
-- store paths from declared build dependencies.
buildEnvironment ::
  BuildConfig ->
  Derivation ->
  FilePath ->
  FilePath ->
  [(Text, FilePath)] ->
  Map Text Text
buildEnvironment config drv builderPath buildDir outputDirs =
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
            (envPath, buildPath builderPath)
          ]
   in -- Priority: output paths > derivation env > standard env
      Map.unions [outputEnv, baseEnv, standardEnv]

-- | Construct the build PATH from the builder's location.
-- Includes the builder's directory, its sibling @usr\/bin@ (for MSYS2
-- coreutils bundled with Git for Windows), and system directories.
-- On a bootstrapped store, the builder's dir IS a store path, so this
-- naturally becomes a store-only PATH.
buildPath :: FilePath -> Text
buildPath builderPath =
  let builderDir = takeDirectory builderPath
      parentDir = takeDirectory builderDir
      -- Builder's own dir + coreutils sibling (MSYS2 layout)
      builderDirs = [builderDir, parentDir </> "usr" </> "bin"]
      systemDirs =
        if System.Info.os == "mingw32"
          then ["C:\\Windows\\System32", "C:\\Windows"]
          else ["/usr/bin", "/bin", "/usr/local/bin"]
      sep = if System.Info.os == "mingw32" then ";" else ":"
   in T.intercalate sep (map T.pack (builderDirs ++ systemDirs))

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
--
-- The build environment is overlaid on top of the inherited system
-- environment.  Build variables take priority, but system-critical
-- variables (e.g. SYSTEMROOT on Windows) pass through.  This matches
-- unsandboxed build behavior — proper isolation comes with Phase 5.
runBuilder ::
  FilePath ->
  [String] ->
  Map Text Text ->
  FilePath ->
  IO (Either (Int, Text) ())
runBuilder builderPath builderArgs buildEnv workDir = do
  systemEnv <- System.Environment.getEnvironment
  let systemMap = Map.fromList [(T.pack k, T.pack v) | (k, v) <- systemEnv]
      -- Build env wins over system env
      mergedEnv = Map.union buildEnv systemMap
      envList = [(T.unpack k, T.unpack v) | (k, v) <- Map.toList mergedEnv]
      cp =
        (mkBuilderProcess builderPath builderArgs)
          { Proc.cwd = Just workDir,
            Proc.env = Just envList,
            Proc.std_out = Proc.CreatePipe,
            Proc.std_err = Proc.CreatePipe
          }
  (exitCode, _stdout, stderrText) <- Proc.readCreateProcessWithExitCode cp ""
  case exitCode of
    ExitSuccess -> pure (Right ())
    ExitFailure code -> pure (Left (code, T.pack stderrText))

-- | Create the appropriate process spec for the builder.
--
-- On Windows, cmd.exe uses its own command line parser — @\/c@ takes a
-- raw command string, not individually-quoted arguments.  GHC's 'Proc.proc'
-- wraps each arg in double quotes for the @CommandLineToArgvW@ convention,
-- but cmd.exe doesn't use that convention, so a derivation like:
--
-- @
-- derivation { builder = "cmd.exe"; args = [ "\/c" "echo Hello" ]; ... }
-- @
--
-- would fail because GHC quotes @echo Hello@ → @\"echo Hello\"@, and
-- cmd.exe tries to find an executable literally named @\"echo Hello\"@.
--
-- Fix: when the builder is cmd.exe with @\/c@, use 'Proc.shell' which
-- passes the command string directly to @cmd.exe \/c@ without quoting.
-- For all other builders, use 'Proc.proc' (standard @CommandLineToArgvW@).
mkBuilderProcess :: FilePath -> [String] -> Proc.CreateProcess
mkBuilderProcess builder args
  | isWindows,
    isCmdExe builder,
    Just cmdString <- extractCmdString args =
      Proc.shell cmdString
  | otherwise = Proc.proc builder args

-- | Check if the builder is cmd.exe (case-insensitive, handles full paths).
isCmdExe :: FilePath -> Bool
isCmdExe path = map toLower (takeFileName path) == "cmd.exe"

-- | Extract the raw command string from cmd.exe args.
-- Looks for @\/c@ (case-insensitive) and joins everything after it.
extractCmdString :: [String] -> Maybe String
extractCmdString (flag : cmdArgs)
  | map toLower flag == "/c" = Just (unwords cmdArgs)
extractCmdString _ = Nothing

-- | Whether we are running on Windows (compile-time constant via 'System.Info').
isWindows :: Bool
isWindows = System.Info.os == "mingw32"

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
      -- Deriver path is not available from the Derivation type alone;
      -- the caller (buildWithDeps) would need to pass it through.
      -- Register with no deriver for now — queryDeriver will return Nothing.
      drvPathText = Nothing
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
  -- Check if output exists (file or directory — builder creates it)
  exists <- doesPathExist outDir
  if not exists
    then pure (Left ("output missing: " <> T.pack outDir))
    else do
      -- Scan for references in the output
      refs <- scanReferences (bcStoreDir config) candidates outDir
      -- Check if already in store (e.g. from a previous build)
      alreadyExists <- doesPathExist targetPath
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

-- ---------------------------------------------------------------------------
-- Dependency-aware build orchestration
-- ---------------------------------------------------------------------------

-- | Build a derivation and all its transitive dependencies.
--
-- 1. Build the dependency graph by reading .drv files from the store.
-- 2. Topologically sort: leaves (no deps) first.
-- 3. For each dependency in build order:
--    a. Already in store? Skip.
--    b. Available in a binary cache? Substitute.
--    c. Otherwise: build locally.
-- 4. Build the root derivation last.
--
-- Returns 'BuildSuccess' with the root output path on success, or
-- 'BuildFailure' if any dependency fails to build or substitute.
buildWithDeps :: BuildConfig -> Store -> Derivation -> StorePath -> IO BuildResult
buildWithDeps config store rootDrv rootDrvPath =
  case buildDepGraph (readDrvFromStore config) rootDrv rootDrvPath of
    Left err -> pure (BuildFailure ("dependency graph error: " <> err) 1)
    Right depGraph ->
      case topoSort depGraph of
        TopoCycle _ ->
          pure (BuildFailure "dependency cycle detected" 1)
        TopoSorted buildOrder -> do
          let drvMap = Map.fromList [(sp, drv) | sp <- buildOrder, Just drv <- [lookupDrv depGraph sp]]
          result <- buildInOrder config store drvMap buildOrder
          case result of
            Left err -> pure (BuildFailure err 1)
            Right () ->
              case drvOutputs rootDrv of
                (firstOut : _) -> pure (BuildSuccess (doPath firstOut))
                [] -> pure (BuildFailure "no outputs defined" 1)

-- | Look up a derivation in the dependency graph.
lookupDrv :: DepGraph -> StorePath -> Maybe Derivation
lookupDrv (Nix.DependencyGraph.DepGraph g) sp =
  Nix.DependencyGraph.dnDerivation <$> Map.lookup sp g

-- | Read and parse a .drv file from the store.
--
-- Since 'buildDepGraph' is pure but needs to read immutable .drv files,
-- we use 'System.IO.Unsafe.unsafePerformIO'.  This is safe because .drv
-- files are write-once: their content is determined by their hash, so
-- repeated reads always yield the same result.
readDrvFromStore :: BuildConfig -> StorePath -> Either Text Derivation
readDrvFromStore config sp =
  let drvFilePath = storePathToFilePath (bcStoreDir config) sp
   in case unsafeReadFile drvFilePath of
        Nothing -> Left ("cannot read .drv file: " <> T.pack drvFilePath)
        Just content -> fromATerm content

-- | Read a file as Text, returning Nothing on any error.
-- Used only for reading immutable .drv files from the store.
unsafeReadFile :: FilePath -> Maybe Text
unsafeReadFile path =
  case System.IO.Unsafe.unsafePerformIO (try (TIO.readFile path)) of
    Left (_ :: SomeException) -> Nothing
    Right content -> Just content

-- ---------------------------------------------------------------------------
-- Build in topological order
-- ---------------------------------------------------------------------------

-- | Status tag for dependency resolution status messages.
data DepStatus = Cached | Substituted | Building

-- | Format a status tag for display.
statusTag :: DepStatus -> Text
statusTag Cached = "[cached]"
statusTag Substituted = "[subst] "
statusTag Building = "[build] "

-- | Build dependencies in topological order.
-- Skips paths already in the store, tries substitution, then builds.
buildInOrder :: BuildConfig -> Store -> Map StorePath Derivation -> [StorePath] -> IO (Either Text ())
buildInOrder _ _ _ [] = pure (Right ())
buildInOrder config store drvMap (sp : rest) =
  case Map.lookup sp drvMap of
    Nothing ->
      -- Not a derivation we know about — might be a source path.  Skip.
      buildInOrder config store drvMap rest
    Just drv -> do
      status <- resolveDep config store drv
      case status of
        Right depStatus -> do
          logDepStatus depStatus drv
          buildInOrder config store drvMap rest
        Left err ->
          pure (Left ("building " <> formatDrvName drv <> " failed: " <> err))

-- | Resolve a single dependency: check cache, try substitution, or build.
resolveDep :: BuildConfig -> Store -> Derivation -> IO (Either Text DepStatus)
resolveDep config store drv = do
  cached <- isOutputCached store drv
  if cached
    then pure (Right Cached)
    else do
      substituted <- trySubstituteOutputs config store drv
      if substituted
        then pure (Right Substituted)
        else do
          result <- buildDerivation config store drv
          case result of
            BuildSuccess _ -> pure (Right Building)
            BuildFailure msg code ->
              pure (Left ("exit " <> T.pack (show code) <> ": " <> msg))

-- | Check whether the first output of a derivation is already in the store.
isOutputCached :: Store -> Derivation -> IO Bool
isOutputCached store drv = case drvOutputs drv of
  (out : _) -> isValid store (doPath out)
  [] -> pure False

-- | Log dependency resolution status to stderr.
logDepStatus :: DepStatus -> Derivation -> IO ()
logDepStatus status drv =
  TIO.hPutStrLn System.IO.stderr ("  " <> statusTag status <> " " <> formatDrvName drv)

-- | Try to substitute all outputs of a derivation from binary caches.
-- Returns True if all outputs were successfully substituted.
trySubstituteOutputs :: BuildConfig -> Store -> Derivation -> IO Bool
trySubstituteOutputs config store drv
  | null (bcCaches config) = pure False
  | otherwise = do
      results <- mapM (trySubstitute store (bcCaches config) . doPath) (drvOutputs drv)
      pure (all isSubstSuccess results)
  where
    isSubstSuccess (SubstSuccess _) = True
    isSubstSuccess _ = False

-- | Format a derivation name for status output.
formatDrvName :: Derivation -> Text
formatDrvName drv =
  case Map.lookup "name" (drvEnv drv) of
    Just n -> n
    Nothing -> case drvOutputs drv of
      (out : _) -> spName (doPath out)
      [] -> "<unknown>"
