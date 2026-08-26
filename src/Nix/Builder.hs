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
    BuilderSpawn (..),
    buildPath,
    execWrapperConfig,
    execWrapperFor,
    rewriteEnv,
    rewritePlaceholders,
    scrubAmbient,
    unionEnvs,
    verifyFetchHash,
  )
where

import Control.Exception (IOException, SomeException, displayException, finally, onException, try)
import Control.Monad (filterM, unless, when)
import qualified Data.ByteString as BS
import Data.Char (toLower, toUpper)
import Data.Either (fromRight)
import Data.Foldable (for_)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.IO as TIO
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS
import qualified Network.HTTP.Types.Status as HTTP
import Nix.Builder.Unpack (UnpackLimits, builtinUnpackBuilder, defaultUnpackLimits, runBuiltinUnpack)
import Nix.DependencyGraph (DepGraph, TopoResult (..), buildDepGraph, topoSort)
import qualified Nix.DependencyGraph
import Nix.Derivation (Derivation (..), DerivationOutput (..), currentPlatform, fromATerm, platformToText)
import Nix.Hash (IncrementalHash, bytesToHexText, hashFinalizeBytes, hashInitWithAlgo, hashPlaceholder, hashUpdateChunk, hexToBytes, makeStorePath, rawHashWithAlgo)
import Nix.Store (PathLock, PathRegistration, Store (..), acquirePathLock, isValid, placeInStore, registerPaths, releasePathLock, scanReferences, scanTempReferences)
import qualified Nix.Store.ExecBit as ExecBit
import Nix.Store.Path (StoreDir (..), StorePath (spHash, spName), StorePathNameError, defaultStoreDir, defaultStoreDirText, storePathToFilePath, unStoreDir)
import Nix.Substituter (CacheConfig, SubstResult (..), catchSync, trySubstitute)
import qualified NovaCache.NAR as NAR
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

-- | The four temp-directory names, every one pointed at the build
-- directory, matching upstream's single assignment of all four.  Tools
-- disagree about which one they read (POSIX tools take @TMPDIR@, msys
-- reads @TMPDIR@ and @TMP@, native Windows tools take @TMP@ then
-- @TEMP@), and a scrubbed environment that repoints only one leaves
-- the others to fall back to a directory the build cannot own - on
-- Windows, GetTempPath bottoms out at the Windows directory itself.
envTmpDir, envTempDir, envTmp, envTemp :: Text
envTmpDir = "TMPDIR"
envTempDir = "TEMPDIR"
envTmp = "TMP"
envTemp = "TEMP"

-- | Environment variable for the Nix store path.
envNixStore :: Text
envNixStore = "NIX_STORE"

-- | Ambient variables a Windows builder may inherit, by uppercase-folded
-- name.  SystemRoot is a hard execution requirement - the CLR's crypto
-- provider fails to load without it (verified live on a clean Windows 11
-- box), while plain Win32 binaries merely tolerate its absence - and
-- SystemDrive and windir are the aliases some tools consult instead of it.
windowsAmbientAllowlist :: [Text]
windowsAmbientAllowlist = [systemRootKey, "SYSTEMDRIVE", "WINDIR"]

-- | The folded name 'scrubAmbient' derives COMSPEC from.
systemRootKey :: Text
systemRootKey = "SYSTEMROOT"

-- | The command interpreter variable, synthesized from SystemRoot rather
-- than inherited: programs shelling out through the C runtime's
-- @system()@ read it from the block.
envComspec :: Text
envComspec = "COMSPEC"

-- | Executable-extension resolution, pinned instead of inherited so name
-- resolution does not drift with host configuration.  cmd.exe's own
-- built-in default when the variable is absent adds @.VBS;.JS;.WS;.MSC@;
-- a build has no business resolving those implicitly.
envPathext, pathextValue :: Text
envPathext = "PATHEXT"
pathextValue = ".COM;.EXE;.BAT;.CMD"

-- | Environment variable for PATH.
envPath :: Text
envPath = "PATH"

-- | The magic builder string for the built-in URL fetcher.  A derivation with
-- this builder is not executed as a process - the Builder downloads its @url@
-- and verifies it against @outputHash@ (see 'runBuiltinFetchurl').
-- Bytes, matching the 'drvBuilder' field it is compared against.
builtinFetchurlBuilder :: BS.ByteString
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
    bcCaches :: ![CacheConfig],
    -- | Extraction budget for @builtin:unpack@ builds.
    bcUnpackLimits :: !UnpackLimits,
    -- | Launchers for derivations whose @system@ this machine cannot execute
    -- directly, keyed by that system string exactly as
    -- 'Nix.Derivation.platformToText' spells it (@x86_64-windows@ -> a wine
    -- binary). Prepended to the builder at the spawn boundary only, so the
    -- derivation itself - and its store paths - stay identical to a native
    -- build's. A foreign system with no entry here is refused rather than
    -- spawned natively; see 'execWrapperFor'.
    bcExecWrappers :: !(Map Text FilePath)
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
      bcCaches = [],
      bcExecWrappers = Map.empty,
      bcUnpackLimits = defaultUnpackLimits
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
-- 7. Exception safety: synchronous exceptions convert to BuildFailure;
--    asynchronous exceptions propagate ('catchSync') - an interrupt
--    must abort the build, never be reported as a build failure.
buildDerivation :: BuildConfig -> Store -> Derivation -> IO BuildResult
buildDerivation config store drv =
  buildDerivationInner config store drv
    `catchSync` \err -> pure (BuildFailure ("build exception: " <> T.pack (show err)) 1)

-- | Build under the outputs' path locks.  A derivation another process
-- finished while we waited for them is adopted rather than redone:
-- rebuilding would delete a registered path out from under whoever is
-- already using it, which is why substitution adopts a finished path too.
buildDerivationInner :: BuildConfig -> Store -> Derivation -> IO BuildResult
buildDerivationInner config store drv =
  withOutputLocks config store drv (maybe (buildUnderLock config store drv) (pure . BuildSuccess))

-- | Hold every output's path lock for the whole build, and report
-- whether the derivation was already finished by the time they were
-- granted.
--
-- #119 and #121 made delete, materialize and register one critical
-- section under @\<store-path\>.lock@.  The build path never joined it,
-- so two concurrent builds of one derivation shared a build directory
-- ('computeBuildDir' is deterministic), interleaved their writes into a
-- single tree, and each registered it as valid.  Nothing failed; the
-- registered output was simply not what either build produced.
--
-- Outputs are locked in 'StorePath' order so two processes racing the
-- same multi-output derivation request them in the same sequence.  A
-- build holds locks for its own outputs only - dependencies are
-- resolved before 'buildDerivation' is called, never underneath it - so
-- no process holds one lock while waiting on another, and there is no
-- cycle to deadlock on.
--
-- The validity re-check happens under the locks, not before them:
-- checked earlier it would answer about a moment that has already
-- passed by the time the build starts.
withOutputLocks :: BuildConfig -> Store -> Derivation -> (Maybe StorePath -> IO a) -> IO a
withOutputLocks config store drv act = do
  heldLocks <- newIORef []
  let takeLock sp = do
        lock <- acquirePathLock (bcStoreDir config) sp
        atomicModifyIORef' heldLocks (\locks -> (lock : locks, ()))
  ( do
      mapM_ takeLock (sort (map doPath (drvOutputs drv)))
      finishedElsewhere <- allOutputsValid store drv
      act finishedElsewhere
    )
    `finally` (readIORef heldLocks >>= mapM_ releasePathLock)

-- | The derivation's first output path when EVERY output is already
-- registered, otherwise 'Nothing'.
--
-- Every output, never just the first: a partially valid multi-output
-- derivation still has to build, and treating it as finished would leave
-- the missing outputs missing.  Both the lock-held re-check and the gate
-- in 'resolveDep' ask this one question, so the two cannot disagree.
allOutputsValid :: Store -> Derivation -> IO (Maybe StorePath)
allOutputsValid store drv = case map doPath (drvOutputs drv) of
  [] -> pure Nothing
  outputPaths@(firstOutput : _) -> do
    validity <- mapM (isValid store) outputPaths
    pure (if and validity then Just firstOutput else Nothing)

-- | The build itself, with every output's lock already held.
buildUnderLock :: BuildConfig -> Store -> Derivation -> IO BuildResult
buildUnderLock config store drv = do
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
        --    is responsible for creating $out, $dev, etc.).
        --
        --    $out is the FINAL store path now, as upstream's is: a temp
        --    path recorded by the build (a compiler driver, a libtool
        --    archive) would otherwise outlive the temp dir it named.
        outputValidity <- mapM (isValid store . doPath) (drvOutputs drv)
        case traverse outputPlan (zip (drvOutputs drv) outputValidity) of
          Left nameErr -> pure (BuildFailure ("output name is not a store path name: " <> T.pack (show nameErr)) 1)
          Right plans -> runPlannedBuild config store drv buildDir plans

-- | Where the builder writes each output, and where that has to end up.
--
-- The two coincide for an output being built: writing into the final store
-- path is what this whole arrangement is for.  They differ for an output
-- that is already valid, which the builder is still handed (a package with
-- a @dev@ output has one builder writing both) but which must not be
-- touched: it is registered, its @NarHash@ describes its current bytes,
-- and 'placeInStore' has already sealed it read-only.
data OutputPlan = OutputPlan
  { -- | The derivation output's name, as it reaches the environment.
    opName :: !Text,
    -- | Where the output has to be when the build is done.
    opFinal :: !StorePath,
    -- | Where the builder is told to write.
    opScratch :: !StorePath,
    -- | Whether 'opFinal' is already registered, and so must survive.
    opValid :: !Bool
  }

-- | Plan one output, given whether its final path is already valid.
outputPlan :: (DerivationOutput, Bool) -> Either StorePathNameError OutputPlan
outputPlan (out, valid)
  | not valid = Right (OutputPlan (doName out) (doPath out) (doPath out) False)
  | otherwise = do
      fallback <- fallbackOutputPath (doPath out)
      pure (OutputPlan (doName out) (doPath out) fallback True)

-- | A scratch store path for an output that must not be written to, built
-- so it cannot collide with any real store path.
--
-- Upstream's @makeFallbackPath@ (@derivation-builder.cc@, the @StorePath@
-- overload) with the same shape: a bogus @rewrite:@ path type and an
-- all-zeroes inner hash, keyed on the output path it stands in for.
-- Upstream also keys on the @.drv@ path; that is not reachable here, and
-- the output path already names one output of one derivation, so the
-- result is just as unique.
fallbackOutputPath :: StorePath -> Either StorePathNameError StorePath
fallbackOutputPath sp =
  makeStorePath
    defaultStoreDir
    ("rewrite:" <> spHash sp <> "-" <> spName sp)
    (BS.replicate 32 0)
    (spName sp)

-- | Parse @--exec-wrapper SYSTEM=PATH@ specs into the map 'bcExecWrappers'
-- wants.
--
-- @SYSTEM@ is a derivation's own @system@ string, spelled as
-- 'Nix.Derivation.platformToText' spells it, because 'execWrapperFor'
-- matches it by equality and nothing else.
--
-- A system named twice is an error rather than last-one-wins: two specs for
-- one system is a mistake in the invocation either way, and silently
-- picking one of them means a build runs through a launcher the operator
-- did not think they had asked for.
execWrapperConfig :: [String] -> Either Text (Map Text FilePath)
execWrapperConfig = foldl' step (Right Map.empty)
  where
    step acc spec = do
      wrappers <- acc
      (system, path) <- one spec
      if Map.member system wrappers
        then Left ("--exec-wrapper names " <> system <> " twice")
        else Right (Map.insert system path wrappers)
    one spec = case break (== '=') spec of
      (system, '=' : path)
        | not (null system), not (null path) -> Right (T.pack system, path)
      _ -> Left ("--exec-wrapper expects SYSTEM=PATH, got: " <> T.pack spec)

-- | How this machine can spawn a derivation's builder.
data BuilderSpawn
  = -- | The derivation targets this machine's platform; spawn it directly.
    SpawnNative
  | -- | Spawn through this launcher, named for the derivation's system.
    SpawnThrough !FilePath
  | -- | Nothing here can execute this derivation's builder; the payload is
    -- the system string that has no launcher.
    SpawnUnsupported !Text
  deriving (Eq, Show)

-- | How to spawn this derivation's builder on this machine.
--
-- \"This is my platform\" and \"I have no launcher for this platform\" are
-- different answers: collapsing them to \"spawn directly\" runs a foreign
-- builder natively and reports whatever the loader says, after the whole
-- closure has already been realized. A system this machine cannot execute
-- and has not been told how to is refused instead.
execWrapperFor :: BuildConfig -> Derivation -> BuilderSpawn
execWrapperFor config drv
  | drvPlatform drv == currentPlatform = SpawnNative
  | otherwise = case Map.lookup system (bcExecWrappers config) of
      Just launcher -> SpawnThrough launcher
      Nothing -> SpawnUnsupported system
  where
    system = platformToText (drvPlatform drv)

-- | The build, once every output knows where it is written and where it
-- belongs.
runPlannedBuild :: BuildConfig -> Store -> Derivation -> FilePath -> [OutputPlan] -> IO BuildResult
runPlannedBuild config store drv buildDir plans = do
  let scratchDir p = storePathToFilePath (bcStoreDir config) (opScratch p)
      outputDirs = [(opName p, scratchDir p) | p <- plans]
      -- Every scratch path is the build's to use: the ones that are final
      -- paths may hold a tree an interrupted run left behind, and the
      -- fallbacks may hold one an interrupted run failed to discard.
      clearScratch = for_ plans (removePathForcibly . scratchDir)
  clearScratch
  createDirectoryIfMissing True (unStoreDir (bcStoreDir config))
  -- The cleanup contract: the build directory is not where outputs live
  -- any more, so an interrupt or a throw between the builder starting and
  -- registration committing would otherwise strand an unregistered tree at
  -- a real store path.  Once the registrations are committed there is
  -- nothing left to take back, and the flag says so.
  committed <- newIORef False
  let cleanupUnlessCommitted = do
        done <- readIORef committed
        unless done clearScratch
  ( do
      -- 4. Decode builder/args/env for the spawn boundary.  Derivation
      --    strings are BYTES (identity); spawning a process needs real
      --    text, so invalid UTF-8 in any of them is a clean build
      --    failure, never a mojibake spawn.  The builtin builders skip
      --    this - they read the byte fields directly.
      exitResult <- case drvBuilder drv of
        b
          | b == builtinFetchurlBuilder -> runBuiltinFetchurl drv outputDirs
          | b == builtinUnpackBuilder -> runBuiltinUnpack (bcStoreDir config) (bcUnpackLimits config) drv outputDirs
        _ -> case (execWrapperFor config drv, decodeBuilderStrings drv) of
          (SpawnUnsupported system, _) ->
            pure
              ( Left
                  ( 1,
                    "no way to run a "
                      <> system
                      <> " builder on this machine; pass --exec-wrapper "
                      <> system
                      <> "=PATH to name a launcher for it"
                  )
              )
          (_, Left errMsg) -> pure (Left (1, errMsg))
          (spawn, Right (builderText, argTexts, decodedEnv)) ->
            let -- onStore first, so nothing a placeholder expands to gets
                -- rewritten a second time.
                rewrite = rewritePlaceholders outputDirs . onStore config
                builderPath = T.unpack (onStore config builderText)
                environ = buildEnvironment config (rewriteEnv rewrite decodedEnv) builderPath buildDir outputDirs
                builderArgs = map (T.unpack . rewrite) argTexts
                -- Env still names the real builder - a launcher only
                -- changes what's spawned, not what the derivation says.
                (spawnPath, spawnArgs) = case spawn of
                  SpawnThrough launcher -> (launcher, builderPath : builderArgs)
                  _ -> (builderPath, builderArgs)
             in -- 5. Run the builder
                runBuilder spawnPath spawnArgs environ buildDir
      case exitResult of
        Left (exitCode, stderrText) -> do
          -- Whatever the builder wrote is in the store now, so failure has
          -- to take it back out: an unregistered tree there is invisible to
          -- the database and would only confuse the next run.
          clearScratch
          pure (BuildFailure ("builder failed: " <> stderrText) exitCode)
        Right () -> do
          -- 6. Success: validate that the builder created all expected
          --    outputs.  Outputs may be files or directories - both are
          --    valid (real Nix allows $out to be a single file, a
          --    directory tree, or a symlink).
          missing <- filterM (fmap not . doesPathExist . snd) outputDirs
          case missing of
            _ : _ -> do
              let names = T.intercalate ", " (map fst missing)
              -- The builder may have produced some outputs before omitting
              -- another.  None were registered, so take those orphaned
              -- trees back out of the store.
              clearScratch
              pure (BuildFailure ("builder succeeded but outputs missing: " <> names) 1)
            [] -> do
              result <- registerOutputs config store drv buildDir plans
              case result of
                BuildSuccess _ -> do
                  writeIORef committed True
                  -- Upstream discards a valid output's redirected copy
                  -- rather than reading it; only the fallbacks go, the
                  -- registered trees stay.
                  for_ [p | p <- plans, opValid p] (removePathForcibly . scratchDir)
                _ -> clearScratch
              pure result
    )
    `onException` cleanupUnlessCommitted

-- | Decode a derivation's builder path, arguments, and env values from
-- their identity bytes to the 'Text' the process-spawn boundary needs.
-- @Left@ names the offending field.
decodeBuilderStrings :: Derivation -> Either Text (Text, [Text], Map Text Text)
decodeBuilderStrings drv = do
  builderText <- decodeField "the builder path" (drvBuilder drv)
  argTexts <- traverse (decodeField "a builder argument") (drvArgs drv)
  envTexts <-
    Map.traverseWithKey
      (\key val -> decodeField ("environment variable '" <> key <> "'") val)
      (drvEnv drv)
  pure (builderText, argTexts, envTexts)
  where
    decodeField what bytes = case TE.decodeUtf8' bytes of
      Right t -> Right t
      Left _ -> Left ("cannot spawn builder: " <> what <> " contains invalid UTF-8")

-- ---------------------------------------------------------------------------
-- Input validation
-- ---------------------------------------------------------------------------

-- | Rewrite a derivation string's store paths from their identity (the
-- canonical @\/nix\/store@ spelling) to where this machine actually keeps
-- them, at the spawn boundary. A no-op when the two coincide.
onStore :: BuildConfig -> Text -> Text
onStore config
  | storeDirText == defaultStoreDirText = id
  | otherwise = T.replace (defaultStoreDirText <> "/") (storeDirText <> "/")
  where
    storeDirText = T.pack (unStoreDir (bcStoreDir config))

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

-- | Replace each output's @builtins.placeholder@ sentinel with its
-- build-time path (the same value @$out@ carries).  Only the args and the
-- environment are rewritten; the builder path is left alone, matching
-- upstream, which assigns @builder = drv->builder@ unrewritten and rewrites
-- only @drv->args@ (local-derivation-goal.cc:2170,2176).
rewritePlaceholders :: [(Text, FilePath)] -> Text -> Text
rewritePlaceholders outputDirs value =
  foldr substitute value outputDirs
  where
    substitute (name, path) = T.replace (hashPlaceholder name) (T.pack path)

-- | Apply a build-time rewrite to an environment's names as well as its
-- values.  Upstream rewrites the whole @name=value@ string
-- (local-derivation-goal.cc:2004), so a placeholder in a dynamic attribute
-- name is substituted too; rewriting only the values would hand the builder
-- a variable whose name still held the sentinel.
rewriteEnv :: (Text -> Text) -> Map Text Text -> Map Text Text
rewriteEnv rewrite = Map.fromList . map rewritePair . Map.toList
  where
    rewritePair (name, value) = (rewrite name, rewrite value)

-- | Build the process environment from the derivation env + standard vars.
-- The builder path is used to derive PATH entries - the builder's own
-- directory and its sibling @usr\/bin@ are included so that coreutils
-- shipped alongside the builder (e.g. Git for Windows' MSYS2 tools)
-- are available.  This mirrors real Nix where PATH contains only
-- store paths from declared build dependencies.
buildEnvironment ::
  BuildConfig ->
  Map Text Text ->
  FilePath ->
  FilePath ->
  [(Text, FilePath)] ->
  Map Text Text
buildEnvironment config decodedEnv builderPath buildDir outputDirs =
  let -- Start with the derivation environment (values decoded at the
      -- spawn boundary by 'decodeBuilderStrings')
      baseEnv = decodedEnv
      -- Add output paths: $out, $dev, etc.
      outputEnv = Map.fromList [(name, T.pack path) | (name, path) <- outputDirs]
      -- Standard build variables
      standardEnv =
        Map.fromList
          [ (envNixBuildTop, T.pack buildDir),
            (envTmpDir, T.pack buildDir),
            (envTempDir, T.pack buildDir),
            (envTmp, T.pack buildDir),
            (envTemp, T.pack buildDir),
            (homeEnvVar, T.pack buildDir),
            (envNixStore, T.pack (unStoreDir (bcStoreDir config))),
            (envPath, buildPath builderPath),
            (envSourceDateEpoch, sourceDateEpochValue)
          ]
   in -- Priority: output paths > derivation env > standard env
      unionEnvs [outputEnv, baseEnv, standardEnv]

-- | Union environment maps left to right (an earlier map's variable wins).
-- On Windows environment names are one case-insensitive namespace, so
-- displacement folds names by per-character uppercase - matching the
-- kernel's env-name comparison - and the winner keeps its own spelling:
-- a build's @PATH@ must displace an inherited @Path@, or both would land
-- in the child's environment block and which one the child sees would be
-- runtime-dependent.  On Unix names are distinct by case and this is
-- 'Map.unions'.
unionEnvs :: [Map Text Text] -> Map Text Text
unionEnvs envs
  | isWindows =
      let foldName = T.map toUpper
          folded =
            [ Map.fromList [(foldName name, (name, val)) | (name, val) <- Map.toList env]
            | env <- envs
            ]
       in Map.fromList (Map.elems (Map.unions folded))
  | otherwise = Map.unions envs

-- | The ambient environment a builder is allowed to see.  Everything
-- else is scrubbed: upstream builds in a cleared environment (its
-- initEnv begins with @env.clear()@ and the child is exec'd with
-- exactly that block), so an undeclared ambient variable reaching a
-- build embeds machine-specific data in its output - undermining the
-- reproducibility SOURCE_DATE_EPOCH exists to pin.
--
-- On Unix the allowance is empty.  A Windows process block cannot be:
-- 'windowsAmbientAllowlist' names what passes through, COMSPEC is
-- synthesized from the allowed SystemRoot, and PATHEXT is pinned to
-- 'pathextValue'.  Names compare case-insensitively on Windows, so the
-- allowlist matches by folded name and a passed variable keeps its
-- ambient spelling.
scrubAmbient :: Map Text Text -> Map Text Text
scrubAmbient ambient
  | not isWindows = Map.empty
  | otherwise =
      let foldName = T.map toUpper
          folded = Map.fromList [(foldName name, (name, val)) | (name, val) <- Map.toList ambient]
          allowed = Map.fromList [pair | key <- windowsAmbientAllowlist, Just pair <- [Map.lookup key folded]]
          synthesized =
            Map.fromList $
              (envPathext, pathextValue)
                : [ (envComspec, root <> "\\System32\\cmd.exe")
                  | Just (_, root) <- [Map.lookup systemRootKey folded]
                  ]
       in unionEnvs [allowed, synthesized]

-- | Construct the build PATH from the builder's location.
-- Includes the builder's directory, its sibling @usr\/bin@ (for MSYS2
-- coreutils bundled with Git for Windows), and system directories.
-- On a bootstrapped store, the builder's dir IS a store path, so this
-- naturally becomes a store-only PATH.
--
-- A bare-name or dot-relative builder has no directory to derive: @.@
-- here would put the BUILD WORKING DIRECTORY first on PATH, so a file
-- dropped into the build dir would resolve ahead of every tool.  Such
-- builders get the system directories only.
buildPath :: FilePath -> Text
buildPath builderPath =
  let builderDir = takeDirectory builderPath
      parentDir = takeDirectory builderDir
      -- Builder's own dir + coreutils sibling (MSYS2 layout)
      builderDirs
        | builderDir == "." = []
        | otherwise = [builderDir, parentDir </> "usr" </> "bin"]
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
-- The child's environment is exactly the build environment plus
-- 'scrubAmbient''s allowance - the ambient environment does not flow
-- through.  Filesystem and process isolation remain future work; the
-- environment no longer waits on them.
runBuilder ::
  FilePath ->
  [String] ->
  Map Text Text ->
  FilePath ->
  IO (Either (Int, Text) ())
runBuilder builderPath builderArgs buildEnv workDir = do
  systemEnv <- System.Environment.getEnvironment
  let systemMap = Map.fromList [(T.pack k, T.pack v) | (k, v) <- systemEnv]
      -- Build env wins collisions, displacing case variants on Windows
      mergedEnv = unionEnvs [buildEnv, scrubAmbient systemMap]
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
    (Just urlBytes, Just outPath, Just out) -> case TE.decodeUtf8' urlBytes of
      Left _ -> pure (Left (1, "builtin:fetchurl: 'url' contains invalid UTF-8"))
      Right url
        -- Mode and algorithm reject before any network traffic.
        | recursive -> pure (Left (1, recursiveUnsupportedMessage))
        | otherwise -> case hashInitWithAlgo algo of
            Nothing ->
              pure (Left (1, "builtin:fetchurl: unsupported hash algorithm '" <> algo <> "'"))
            Just ctx -> do
              downloaded <- downloadUrlTo url outPath ctx
              case downloaded of
                Left err -> pure (Left (1, "builtin:fetchurl: " <> err))
                Right digest -> pure (verifyFetchedDigest url out digest)
        where
          (recursive, algo) = splitHashMode (doHashAlgo out)
  where
    -- builtin:fetchurl derivations are always fixed-output; the expected hash
    -- lives in the canonical output spec (doHashAlgo + doHash), not the env.
    fixedOutput = case drvOutputs drv of
      (out : _) | not (T.null (doHashAlgo out)) -> Just out
      _ -> Nothing

-- | Download a URL to a file using nova-nix's own linked HTTP client
-- (the same 'Network.HTTP.Client' the substituter uses) - no external
-- @curl@, which is what makes this a genuine builtin.  The body streams
-- to disk through the incremental hash chunk by chunk, so memory stays
-- at chunk size no matter the download's size, and the returned digest
-- is of exactly the written bytes.  Any network exception is turned
-- into a 'Left' so it becomes a clean build failure.
downloadUrlTo :: Text -> FilePath -> IncrementalHash -> IO (Either Text BS.ByteString)
downloadUrlTo url outPath ctx0 = do
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
      HTTP.withResponse request manager $ \response -> do
        let code = HTTP.statusCode (HTTP.responseStatus response)
        if code /= httpStatusOk
          then pure (Left ("HTTP " <> T.pack (show code) <> " fetching " <> url))
          else System.IO.withBinaryFile outPath System.IO.WriteMode $ \handle ->
            let consume !ctx = do
                  chunk <- HTTP.brRead (HTTP.responseBody response)
                  if BS.null chunk
                    then pure (Right (hashFinalizeBytes ctx))
                    else do
                      BS.hPut handle chunk
                      consume (hashUpdateChunk ctx chunk)
             in consume ctx0

-- | Verify the fetched bytes against the derivation's fixed-output hash, read
-- from the canonical output spec.  @doHashAlgo@ is @sha256@/@sha512@/... (or
-- @r:sha256@ for recursive); @doHash@ is the base-16 digest eval normalized the
-- user's hash into.  Flat mode is fully supported across algorithms; recursive
-- (unpack/executable) fetches are a separate feature - they require unpacking
-- the download - and fail with a clear message rather than a wrong result.
verifyFetchHash :: Text -> DerivationOutput -> BS.ByteString -> Either (Int, Text) ()
verifyFetchHash url out body
  | recursive = Left (1, recursiveUnsupportedMessage)
  | otherwise = case (hexToBytes (doHash out), rawHashWithAlgo algo body) of
      (Nothing, _) -> Left (1, malformedExpectedHash url)
      (_, Nothing) -> Left (1, "builtin:fetchurl: unsupported hash algorithm '" <> algo <> "'")
      (Just _, Just got) -> verifyFetchedDigest url out got
  where
    (recursive, algo) = splitHashMode (doHashAlgo out)

-- | Compare an already-computed digest of the fetched bytes against the
-- derivation's fixed-output hash - the streaming twin of
-- 'verifyFetchHash', which hashes a buffered body.
verifyFetchedDigest :: Text -> DerivationOutput -> BS.ByteString -> Either (Int, Text) ()
verifyFetchedDigest url out got = case hexToBytes (doHash out) of
  Nothing -> Left (1, malformedExpectedHash url)
  Just expected
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

-- | Split @outputHashAlgo@ into the recursive-mode marker and the bare
-- algorithm name (@r:sha256@ is recursive @sha256@).
splitHashMode :: Text -> (Bool, Text)
splitHashMode algoField = case T.stripPrefix "r:" algoField of
  Just rest -> (True, rest)
  Nothing -> (False, algoField)

recursiveUnsupportedMessage :: Text
recursiveUnsupportedMessage =
  "builtin:fetchurl: recursive outputHashMode (unpack/executable) not yet supported; fetch flat and unpack in a build phase"

malformedExpectedHash :: Text -> Text
malformedExpectedHash url = "builtin:fetchurl: malformed expected hash for " <> url

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
  [OutputPlan] ->
  IO BuildResult
registerOutputs config store drv _buildDir plans = do
  let -- Candidates for reference scanning: input sources, the input
      -- derivations' realized OUTPUT paths, and this derivation's own outputs.
      inputOutputs = fromRight [] (resolveInputOutputs config drv)
      allCandidates = collectAllCandidates drv ++ inputOutputs
      -- Deriver path is not available from the Derivation type alone;
      -- the caller (buildWithDeps) would need to pass it through.
      -- Register with no deriver for now - queryDeriver will return Nothing.
      drvPathText = Nothing
      -- (fallback dir, the output it stood in for) for every already-valid
      -- output.  Nothing may reference one: the tree is discarded after the
      -- build, so a reference to it is a dangling edge, and upstream avoids
      -- the same thing by rewriting the hashes back out of the contents.
      -- Scanning for it and failing is louder and needs no rewriting.
      fallbackPairs =
        [ (storePathToFilePath (bcStoreDir config) (opScratch p), opFinal p)
        | p <- plans,
          opValid p
        ]
  -- Phase 1: scan references and move each output into the store, collecting a
  -- PathRegistration (no DB writes yet).  Already-valid outputs are skipped.
  prepared <- mapM (prepareOutput config store allCandidates fallbackPairs drvPathText) (zip (drvOutputs drv) plans)
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
  (DerivationOutput, OutputPlan) ->
  IO (Either Text (Maybe PathRegistration))
prepareOutput config store candidates fallbackPairs drvPathText (output, plan) = do
  let targetSP = doPath output
      targetPath = storePathToFilePath (bcStoreDir config) targetSP
      outDir = storePathToFilePath (bcStoreDir config) (opScratch plan)
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
          -- The builder wrote straight into the store path, so there is
          -- nothing to clear: outDir IS targetPath.  A stale tree from an
          -- interrupted run was removed before the build, not here.
          --
          -- One scan, not two: 'candidates' already carries this
          -- derivation's own outputs, and outDir is the final path, so a
          -- self- or cross-output reference is an ordinary hit.
          leaked <- scanTempReferences fallbackPairs outDir
          case leaked of
            _ : _ -> do
              -- A fallback path is about to stop existing, so a produced
              -- output naming one would be registered with a dangling
              -- reference.  Fail loudly instead, and take the tree back
              -- out so no later run meets it.
              removePathForcibly outDir
              pure
                ( Left
                    ( "output "
                        <> doName output
                        <> " refers to the scratch path of an already-valid output ("
                        <> T.intercalate ", " [spName sp <> "@" <> spHash sp | sp <- leaked]
                        <> "); rebuild every output of this derivation instead"
                    )
                )
            [] -> do
              refs <- dedupStorePaths <$> scanReferences candidates outDir
              reg <- placeInStore store outDir targetSP drvPathText refs
              fixedCheck <- verifyFixedOutput output targetPath
              case fixedCheck of
                Left err -> do
                  -- Wrong bytes for a declared content address: remove the
                  -- placed tree so a later run cannot meet it on disk.
                  removePathForcibly targetPath
                  pure (Left err)
                Right () -> pure (Right (Just reg))

-- | Re-check a placed fixed-output output against its declared hash.
-- A fixed-output derivation declares both its store path and, via the
-- output spec (@doHashAlgo@ + @doHash@), the exact content the path
-- must carry - but the builder process writes the actual bytes, so the
-- placed result must reproduce the declared digest before it may
-- register as valid (upstream: the fixed-output check in its
-- registerOutputs).  Flat mode hashes the output file's bytes;
-- recursive (@r:@) mode hashes the canonical NAR of the placed tree.
-- Non-fixed outputs (empty @doHashAlgo@) pass unchecked.
verifyFixedOutput :: DerivationOutput -> FilePath -> IO (Either Text ())
verifyFixedOutput out placedPath
  | T.null (doHashAlgo out) = pure (Right ())
  | recursive = do
      -- Re-serialises the placed tree: the registration's NAR hash is
      -- always sha256, while the declared algorithm may be any.
      entry <- ExecBit.serialiseFromPath placedPath
      pure (finish (rawHashWithAlgo algo (NAR.serialise entry)))
  | otherwise = do
      isDir <- doesDirectoryExist placedPath
      if isDir
        then pure (Left (subject <> ": flat outputHashMode, but the output is a directory"))
        else finish <$> hashFileWithAlgo algo placedPath
  where
    (recursive, algo) = splitHashMode (doHashAlgo out)
    subject = "fixed-output '" <> spName (doPath out) <> "'"
    finish Nothing =
      Left (subject <> ": unsupported hash algorithm '" <> algo <> "'")
    finish (Just got) = case hexToBytes (doHash out) of
      Nothing -> Left (subject <> ": malformed expected hash")
      Just expected
        | expected == got -> Right ()
        | otherwise ->
            Left
              ( subject
                  <> ": hash mismatch\n  expected: "
                  <> doHashAlgo out
                  <> ":"
                  <> doHash out
                  <> "\n  got:      "
                  <> doHashAlgo out
                  <> ":"
                  <> bytesToHexText got
              )

-- | Stream a file through the incremental hash without materializing it
-- (a fixed-output file is input-sized).  'Nothing' for an unsupported
-- algorithm.
hashFileWithAlgo :: Text -> FilePath -> IO (Maybe BS.ByteString)
hashFileWithAlgo algo path = case hashInitWithAlgo algo of
  Nothing -> pure Nothing
  Just initialCtx -> System.IO.withBinaryFile path System.IO.ReadMode $ \handle ->
    let consume !ctx = do
          chunk <- BS.hGet handle fixedOutputHashChunk
          if BS.null chunk
            then pure (Just (hashFinalizeBytes ctx))
            else consume (hashUpdateChunk ctx chunk)
     in consume initialCtx

-- | Read-chunk size for hashing a placed output file.
fixedOutputHashChunk :: Int
fixedOutputHashChunk = 65536

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

-- | Read a file's raw bytes, returning Nothing on any error.  Used only
-- for reading immutable .drv files from the store - byte IO, never
-- text-mode (no locale decode, no newline translation).
unsafeReadFile :: FilePath -> Maybe BS.ByteString
unsafeReadFile path =
  case System.IO.Unsafe.unsafePerformIO (try (BS.readFile path)) of
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
  -- Every output, not just the first: a derivation whose first output is
  -- valid and whose second was deleted would otherwise be logged cached
  -- and skipped, and the missing output would stay missing.  Upstream
  -- skips a build only when all of them are valid.
  cached <- isJust <$> allOutputsValid store drv
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

-- | Log dependency resolution status to stderr.
logDepStatus :: DepStatus -> Derivation -> IO ()
logDepStatus status drv =
  TIO.hPutStrLn System.IO.stderr ("  " <> statusTag status <> " " <> formatDrvName drv)

-- | Try to substitute all outputs of a derivation from binary caches.
-- Returns True if every output was substituted or already valid.
--
-- Registration is all-or-nothing and batched: every output is verified
-- and unpacked first, then recorded in one 'registerPaths' transaction,
-- so cross-output reference edges land after both endpoints' rows exist
-- and a partial substitution registers nothing (the subsequent build
-- starts from unregistered outputs and 'prepareOutput' replaces the
-- leftover unpacked trees).
--
-- Each substituted output arrives with its path lock STILL HELD (see
-- 'Nix.Substituter.SubstSuccess'), and the exclusion must survive until
-- the registration transaction commits - released earlier, another
-- process could meet the unpacked-but-unregistered window and delete
-- the tree the row is about to describe.  Every held lock is released
-- here on every exit path (finally), including failures that never
-- reach registration.
--
-- A registration the database REFUSES - its unregistered-referent guard,
-- reachable only through cache-declared references this store has never
-- seen - is a substitution failure like any other: the transaction
-- recorded nothing, so report it and fall back to building locally.
trySubstituteOutputs :: BuildConfig -> Store -> Derivation -> IO Bool
trySubstituteOutputs config store drv
  | null (bcCaches config) = pure False
  | otherwise = do
      heldLocks <- newIORef []
      registerSubstituted heldLocks
        `finally` (readIORef heldLocks >>= mapM_ releasePathLock)
  where
    registerSubstituted :: IORef [PathLock] -> IO Bool
    registerSubstituted heldLocks = do
      results <- mapM (substituteOne heldLocks . doPath) (drvOutputs drv)
      case traverse substOutcome results of
        Nothing -> pure False
        Just outcomes -> do
          registered <- try (registerPaths (stDB store) (catMaybes outcomes))
          case registered of
            Right () -> pure True
            Left (e :: IOException) -> do
              TIO.hPutStrLn
                System.IO.stderr
                ("  [subst] registration refused, building instead: " <> T.pack (displayException e))
              pure False
    -- Record a returned lock the moment it exists, so the enclosing
    -- finally owns it even when a later output's attempt fails or
    -- throws.
    substituteOne heldLocks sp = do
      result <- trySubstitute store (bcCaches config) sp
      case result of
        SubstSuccess _ lock -> atomicModifyIORef' heldLocks (\locks -> (lock : locks, ()))
        _ -> pure ()
      pure result
    -- An already-valid output counts as substituted but contributes no
    -- registration row (and holds no lock).
    substOutcome (SubstSuccess reg _) = Just (Just reg)
    substOutcome SubstAlreadyValid = Just Nothing
    substOutcome SubstNotFound = Nothing
    substOutcome (SubstError _) = Nothing

-- | Format a derivation name for status output (display-only, so the
-- byte-string env value decodes lossily).
formatDrvName :: Derivation -> Text
formatDrvName drv =
  case Map.lookup "name" (drvEnv drv) of
    Just n -> TE.decodeUtf8With lenientDecode n
    Nothing -> case drvOutputs drv of
      (out : _) -> spName (doPath out)
      [] -> "<unknown>"
