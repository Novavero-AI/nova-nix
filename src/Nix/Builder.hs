{-# LANGUAGE ScopedTypeVariables #-}

-- | Derivation builder - executes build recipes.
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
-- to @CreateProcess@ on Windows - native, no POSIX layer.
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

    -- * Pure pieces (exported for tests)
    verifyFetchHash,
  )
where

import Control.Exception (IOException, SomeException, displayException, finally, try)
import Control.Monad (filterM, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toLower)
import Data.Either (fromRight)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS
import qualified Network.HTTP.Types.Status as HTTP
import Nix.Builder.Unpack (builtinUnpackBuilder, runBuiltinUnpack)
import Nix.DependencyGraph (DepGraph, TopoResult (..), buildDepGraph, topoSort)
import qualified Nix.DependencyGraph
import Nix.Derivation (Derivation (..), DerivationOutput (..), fromATerm)
import Nix.Hash (bytesToHexText, hexToBytes, rawHashWithAlgo)
import Nix.Store (PathRegistration, Store (..), isValid, placeInStore, registerPaths, scanReferences, scanTempReferences)
import Nix.Store.Path (StoreDir (..), StorePath (..), storePathToFilePath)
import Nix.Substituter (CacheConfig, SubstResult (..), trySubstitute)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesPathExist, removeDirectoryRecursive, removePathForcibly)
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

-- | The magic builder string for the built-in URL fetcher.  A derivation with
-- this builder is not executed as a process - the Builder downloads its @url@
-- and verifies it against @outputHash@ (see 'runBuiltinFetchurl').
builtinFetchurlBuilder :: Text
builtinFetchurlBuilder = "builtin:fetchurl"

-- | Derivation environment keys read by @builtin:fetchurl@.
envUrl, envOut :: Text
envUrl = "url"
envOut = "out"

-- | HTTP success status code.
httpStatusOk :: Int
httpStatusOk = 200

-- | User-Agent sent by @builtin:fetchurl@.  Some upstream servers (e.g.
-- ftp.gnu.org) reject requests with no User-Agent as bot traffic with a 403,
-- so the fetcher identifies itself like any other download client.
fetchUserAgent :: BS.ByteString
fetchUserAgent = "nova-nix (+https://github.com/Novavero-AI/nova-nix)"

-- | Environment variable for the reproducible-builds.org build timestamp.
envSourceDateEpoch :: Text
envSourceDateEpoch = "SOURCE_DATE_EPOCH"

-- | The fixed build timestamp handed to every builder: 1980-01-01 UTC.
-- Determinism-aware tools (binutils @ld@ writes it into the PE header
-- instead of the wall clock; gcc uses it for @__DATE__@\/@__TIME__@)
-- produce identical output across builds.  1980 rather than 0 because
-- zip timestamps cannot represent dates before 1980 - the same value
-- nixpkgs' stdenv uses.  A derivation env may override it.
sourceDateEpochValue :: Text
sourceDateEpochValue = "315532800"

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
  -- 1. Validate inputs (sources + input-derivation outputs) exist
  inputsOk <- validateInputs config store drv
  case inputsOk of
    Left errMsg -> pure (BuildFailure errMsg 1)
    Right () -> do
      -- 2. Create temp build directory.  The location is deterministic
      --    (computeBuildDir), so a crashed earlier run may have left stale
      --    contents that builtin:unpack would misread as an archive
      --    collision - always start from an empty directory, and always
      --    remove it on the way out (finally), builder exceptions included.
      let buildDir = computeBuildDir config drv
      removePathForcibly buildDir
      createDirectoryIfMissing True buildDir
      (`finally` cleanupBuildDir buildDir) $ do
        -- 3. Compute output paths (but do NOT pre-create them - the builder
        --    is responsible for creating $out, $dev, etc.)
        let outputDirs = [(doName out, buildDir </> T.unpack (doName out)) | out <- drvOutputs drv]

        -- 4. Set up environment
        let builderPath = T.unpack (drvBuilder drv)
            environ = buildEnvironment config drv builderPath buildDir outputDirs

            -- 5. Run the builder
            builderArgs = map T.unpack (drvArgs drv)
        -- A "builtin:" builder (e.g. builtin:fetchurl) is handled in-process;
        -- everything else is a normal subprocess.  The output validation and
        -- registration below are shared by both paths.
        exitResult <- case drvBuilder drv of
          b
            | b == builtinFetchurlBuilder -> runBuiltinFetchurl drv outputDirs
            | b == builtinUnpackBuilder -> runBuiltinUnpack drv outputDirs
          _ -> runBuilder builderPath builderArgs environ buildDir
        case exitResult of
          Left (exitCode, stderrText) ->
            pure (BuildFailure ("builder failed: " <> stderrText) exitCode)
          Right () -> do
            -- 6. Success: validate that the builder created all expected
            --    outputs.  Outputs may be files or directories - both are
            --    valid (real Nix allows $out to be a single file, a
            --    directory tree, or a symlink).
            missing <- filterM (fmap not . doesPathExist . snd) outputDirs
            case missing of
              [] -> registerOutputs config store drv buildDir outputDirs
              _ -> do
                let names = T.intercalate ", " (map fst missing)
                pure (BuildFailure ("builder succeeded but outputs missing: " <> names) 1)

-- ---------------------------------------------------------------------------
-- Input validation
-- ---------------------------------------------------------------------------

-- | Check that all inputs needed to build a derivation are present in the store.
--
-- Input sources must be valid.  For input derivations, the artifacts a build
-- actually reads are their realized OUTPUT paths, not the @.drv@ recipe files,
-- so we resolve each requested output (by reading the input @.drv@ the build
-- driver materialized) and validate those outputs.  Dependencies are realized
-- in topological order, so a dependency's outputs are present by the time a
-- dependent is validated.
validateInputs :: BuildConfig -> Store -> Derivation -> IO (Either Text ())
validateInputs config store drv = do
  -- Check input sources
  srcResults <- mapM (isValid store) (drvInputSrcs drv)
  let missingSrcs = [sp | (sp, valid) <- zip (drvInputSrcs drv) srcResults, not valid]
  if not (null missingSrcs)
    then pure (Left ("missing input sources: " <> T.intercalate ", " (map formatSP missingSrcs)))
    else case resolveInputOutputs config drv of
      Left err -> pure (Left err)
      Right requiredOutputs -> do
        outResults <- mapM (isValid store) requiredOutputs
        let missingOutputs = [sp | (sp, valid) <- zip requiredOutputs outResults, not valid]
        if not (null missingOutputs)
          then pure (Left ("missing input derivation outputs: " <> T.intercalate ", " (map formatSP missingOutputs)))
          else pure (Right ())

-- | Resolve each input derivation's requested output names to their store
-- paths by reading the input @.drv@ from the store.  @Left@ if an input @.drv@
-- cannot be read or names an output the derivation does not define.
resolveInputOutputs :: BuildConfig -> Derivation -> Either Text [StorePath]
resolveInputOutputs config drv =
  concat <$> traverse resolveOne (Map.toList (drvInputDrvs drv))
  where
    resolveOne (inputDrvPath, wantedOutputs) = do
      inputDrv <- readDrvFromStore config inputDrvPath
      let outputMap = Map.fromList [(doName o, doPath o) | o <- drvOutputs inputDrv]
      traverse (lookupOutput inputDrvPath outputMap) wantedOutputs
    lookupOutput inputDrvPath outputMap name =
      case Map.lookup name outputMap of
        Just sp -> Right sp
        Nothing ->
          Left ("input derivation " <> formatSP inputDrvPath <> " has no output '" <> name <> "'")

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
-- The builder path is used to derive PATH entries - the builder's own
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
            (envPath, buildPath builderPath),
            (envSourceDateEpoch, sourceDateEpochValue)
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
-- unsandboxed build behavior - proper isolation comes with Phase 5.
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
-- On Windows, cmd.exe uses its own command line parser - @\/c@ takes a
-- raw command string, not individually-quoted arguments.  GHC's 'Proc.proc'
-- wraps each arg in double quotes for the @CommandLineToArgvW@ convention,
-- but cmd.exe doesn't use that convention, so a derivation like:
--
-- @
-- derivation { builder = "cmd.exe"; args = [ "\/c" "echo Hello" ]; ... }
-- @
--
-- would fail because GHC quotes @echo Hello@ as @\"echo Hello\"@, and
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
-- Built-in fetcher (builtin:fetchurl)
-- ---------------------------------------------------------------------------

-- | Run a @builtin:fetchurl@ derivation: download its @url@ into @$out@ and
-- verify the bytes against the derivation's @outputHash@.  Nix's bootstrap
-- fetcher is baked into the binary because nothing can be fetched before a
-- fetcher exists.  Returns the same @Either (exit, msg) ()@ shape as
-- 'runBuilder', so the shared output-registration path is reused unchanged.
runBuiltinFetchurl :: Derivation -> [(Text, FilePath)] -> IO (Either (Int, Text) ())
runBuiltinFetchurl drv outputDirs =
  case (Map.lookup envUrl (drvEnv drv), lookup envOut outputDirs, fixedOutput) of
    (Nothing, _, _) -> pure (Left (1, "builtin:fetchurl: derivation has no 'url'"))
    (_, Nothing, _) -> pure (Left (1, "builtin:fetchurl: derivation defines no 'out' output"))
    (_, _, Nothing) -> pure (Left (1, "builtin:fetchurl: 'out' output has no fixed-output hash"))
    (Just url, Just outPath, Just out) -> do
      downloaded <- downloadUrl url
      case downloaded of
        Left err -> pure (Left (1, "builtin:fetchurl: " <> err))
        Right body -> do
          BS.writeFile outPath body
          pure (verifyFetchHash url out body)
  where
    -- builtin:fetchurl derivations are always fixed-output; the expected hash
    -- lives in the canonical output spec (doHashAlgo + doHash), not the env.
    fixedOutput = case drvOutputs drv of
      (out : _) | not (T.null (doHashAlgo out)) -> Just out
      _ -> Nothing

-- | Download a URL to a strict 'BS.ByteString' using nova-nix's own linked
-- HTTP client (the same 'Network.HTTP.Client' the substituter uses) - no
-- external @curl@, which is what makes this a genuine builtin.  Any network
-- exception is turned into a 'Left' so it becomes a clean build failure.
downloadUrl :: Text -> IO (Either Text BS.ByteString)
downloadUrl url = do
  attempt <- try fetch
  pure $ case attempt of
    Left (e :: SomeException) -> Left ("download error: " <> T.pack (show e))
    Right result -> result
  where
    fetch :: IO (Either Text BS.ByteString)
    fetch = do
      manager <- HTTP.newManager HTTPS.tlsManagerSettings
      request0 <- HTTP.parseRequest (T.unpack url)
      let request = request0 {HTTP.requestHeaders = ("User-Agent", fetchUserAgent) : HTTP.requestHeaders request0}
      response <- HTTP.httpLbs request manager
      pure (bodyOrError response)
    bodyOrError response
      | code == httpStatusOk = Right (LBS.toStrict (HTTP.responseBody response))
      | otherwise = Left ("HTTP " <> T.pack (show code) <> " fetching " <> url)
      where
        code = HTTP.statusCode (HTTP.responseStatus response)

-- | Verify the fetched bytes against the derivation's fixed-output hash, read
-- from the canonical output spec.  @doHashAlgo@ is @sha256@/@sha512@/... (or
-- @r:sha256@ for recursive); @doHash@ is the base-16 digest eval normalized the
-- user's hash into.  Flat mode is fully supported across algorithms; recursive
-- (unpack/executable) fetches are a separate feature - they require unpacking
-- the download - and fail with a clear message rather than a wrong result.
verifyFetchHash :: Text -> DerivationOutput -> BS.ByteString -> Either (Int, Text) ()
verifyFetchHash url out body
  | recursive =
      Left (1, "builtin:fetchurl: recursive outputHashMode (unpack/executable) not yet supported; fetch flat and unpack in a build phase")
  | otherwise = case (hexToBytes (doHash out), rawHashWithAlgo algo body) of
      (Nothing, _) -> Left (1, "builtin:fetchurl: malformed expected hash for " <> url)
      (_, Nothing) -> Left (1, "builtin:fetchurl: unsupported hash algorithm '" <> algo <> "'")
      (Just expected, Just got)
        | expected == got -> Right ()
        | otherwise ->
            Left
              ( 1,
                "builtin:fetchurl: hash mismatch for "
                  <> url
                  <> "\n  expected: "
                  <> algoField
                  <> ":"
                  <> doHash out
                  <> "\n  got:      "
                  <> algoField
                  <> ":"
                  <> bytesToHexText got
              )
  where
    algoField = doHashAlgo out
    (recursive, algo) = case T.stripPrefix "r:" algoField of
      Just rest -> (True, rest)
      Nothing -> (False, algoField)

-- ---------------------------------------------------------------------------
-- Output registration
-- ---------------------------------------------------------------------------

-- | After a successful build, register every output in the store.
--
-- Outputs are placed first and registered together (one transaction) so
-- intra-derivation cross-output references survive (see 'registerPaths').
registerOutputs ::
  BuildConfig ->
  Store ->
  Derivation ->
  FilePath ->
  [(Text, FilePath)] ->
  IO BuildResult
registerOutputs config store drv _buildDir outputDirs = do
  let -- Candidates for reference scanning: input sources, the input
      -- derivations' realized OUTPUT paths, and this derivation's own outputs.
      inputOutputs = fromRight [] (resolveInputOutputs config drv)
      allCandidates = collectAllCandidates drv ++ inputOutputs
      -- Deriver path is not available from the Derivation type alone;
      -- the caller (buildWithDeps) would need to pass it through.
      -- Register with no deriver for now - queryDeriver will return Nothing.
      drvPathText = Nothing
      -- (temp output dir, final store path) for every output, used to detect
      -- self- and cross-output references that appear as build-temp paths.
      tempPairs = [(outDir, doPath out) | (out, (_, outDir)) <- zip (drvOutputs drv) outputDirs]
  -- Phase 1: scan references and move each output into the store, collecting a
  -- PathRegistration (no DB writes yet).  Already-valid outputs are skipped.
  prepared <- mapM (prepareOutput config store allCandidates tempPairs drvPathText) (zip (drvOutputs drv) outputDirs)
  case sequence prepared of
    Left errMsg -> pure (BuildFailure errMsg 1)
    Right regs -> do
      -- Phase 2: register ALL outputs in one transaction.
      registerPaths (stDB store) (catMaybes regs)
      case drvOutputs drv of
        (firstOut : _) -> pure (BuildSuccess (doPath firstOut))
        [] -> pure (BuildFailure "no outputs defined" 1)

-- | Prepare a single output for registration: skip if already valid in the DB,
-- otherwise scan references (input refs plus self/cross-output temp refs) and
-- place it in the store, returning its 'PathRegistration'.  Returns @Nothing@
-- for an output already registered as valid.
prepareOutput ::
  BuildConfig ->
  Store ->
  [StorePath] ->
  [(FilePath, StorePath)] ->
  Maybe Text ->
  (DerivationOutput, (Text, FilePath)) ->
  IO (Either Text (Maybe PathRegistration))
prepareOutput config store candidates tempPairs drvPathText (output, (_outName, outDir)) = do
  let targetSP = doPath output
      targetPath = storePathToFilePath (bcStoreDir config) targetSP
  -- Check the build actually produced the output (file or directory).
  exists <- doesPathExist outDir
  if not exists
    then pure (Left ("output missing: " <> T.pack outDir))
    else do
      -- Validity is a DB fact, not mere disk presence: an output left on disk
      -- by an interrupted run is NOT valid, and build outputs are not
      -- content-addressed, so a leftover cannot be verified against its path.
      -- Replace it with the freshly built output (upstream's delete-then-move
      -- at output registration) rather than adopt unverifiable bytes;
      -- 'removePathForcibly' clears read-only marks, so a tree an earlier run
      -- already marked read-only cannot wedge the replacement.
      valid <- isValid store targetSP
      if valid
        then pure (Right Nothing)
        else do
          onDisk <- doesPathExist targetPath
          when onDisk (removePathForcibly targetPath)
          inputRefs <- scanReferences candidates outDir
          selfRefs <- scanTempReferences tempPairs outDir
          let refs = dedupStorePaths (inputRefs ++ selfRefs)
          reg <- placeInStore store outDir targetSP drvPathText refs
          pure (Right (Just reg))

-- | Deduplicate store paths by their (unique) hash.
dedupStorePaths :: [StorePath] -> [StorePath]
dedupStorePaths = Map.elems . Map.fromList . map (\sp -> (spHash sp, sp))

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
      -- Not a derivation we know about - might be a source path.  Skip.
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
--
-- Registration is all-or-nothing and batched: every output is verified
-- and unpacked first, then recorded in one 'registerPaths' transaction,
-- so cross-output reference edges land after both endpoints' rows exist
-- and a partial substitution registers nothing (the subsequent build
-- starts from unregistered outputs and 'prepareOutput' replaces the
-- leftover unpacked trees).
--
-- A registration the database REFUSES - its unregistered-referent guard,
-- reachable only through cache-declared references this store has never
-- seen - is a substitution failure like any other: the transaction
-- recorded nothing, so report it and fall back to building locally.
trySubstituteOutputs :: BuildConfig -> Store -> Derivation -> IO Bool
trySubstituteOutputs config store drv
  | null (bcCaches config) = pure False
  | otherwise = do
      results <- mapM (trySubstitute store (bcCaches config) . doPath) (drvOutputs drv)
      case traverse substRegistration results of
        Just regs -> do
          registered <- try (registerPaths (stDB store) regs)
          case registered of
            Right () -> pure True
            Left (e :: IOException) -> do
              TIO.hPutStrLn
                System.IO.stderr
                ("  [subst] registration refused, building instead: " <> T.pack (displayException e))
              pure False
        Nothing -> pure False
  where
    substRegistration (SubstSuccess reg) = Just reg
    substRegistration _ = Nothing

-- | Format a derivation name for status output.
formatDrvName :: Derivation -> Text
formatDrvName drv =
  case Map.lookup "name" (drvEnv drv) of
    Just n -> n
    Nothing -> case drvOutputs drv of
      (out : _) -> spName (doPath out)
      [] -> "<unknown>"
