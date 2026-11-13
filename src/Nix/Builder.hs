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
-- == Checking
--
-- Under 'BuildCheck', upstream's @nix-build --check@, 'buildWithDeps'
-- builds its derivation again beside the valid outputs and compares what
-- comes out with them ('checkDerivation').  That rebuild registers nothing
-- and leaves the outputs as they are.  What comes before it adds to the
-- store as any build does: an output that is not valid is substituted
-- first, and the inputs are substituted or built.
--
-- == On Windows
--
-- The key difference is process creation.  Upstream on Linux uses
-- @fork\/exec@ with namespace isolation.  nova-nix spawns builders through
-- "System.Process", which reaches @CreateProcess@ on Windows with no POSIX
-- layer, and runs them without filesystem or network isolation.
--
-- The builder's process tree runs inside a Win32 job object with
-- kill-on-job-close ('Proc.use_process_jobs'), so an interrupt or a
-- builder exit reaps grandchildren instead of orphaning them.  Filesystem
-- and privilege isolation are still future work: a restricted token with a
-- store-granting ACL (the security boundary), then AppContainer for more.
--
-- We ship @bash.exe@ (from MSYS2) as the default builder on Windows.
-- Same approach as Git for Windows.
module Nix.Builder
  ( -- * Build execution
    BuildResult (..),
    buildDerivation,
    checkDerivation,
    buildWithDeps,
    substitutePaths,

    -- * Build configuration
    BuildConfig (..),
    defaultBuildConfig,

    -- * Pure pieces (exported for tests)
    AttrsDelivery (..),
    attrsDelivery,
    BuilderSpawn (..),
    buildPath,
    closureUnder,
    execWrapperConfig,
    execWrapperFor,
    spawnFor,
    ReferenceCycle (..),
    orderByReferences,
    rewriteEnv,
    rewritePlaceholders,
    scrubAmbient,
    unionEnvs,
    verifyFetchHash,
    fetchUrlsFromEnv,
    tryFetchUrlsWith,
  )
where

import Control.Exception (IOException, bracket, catch, catches, displayException, finally, onException, try)
import Control.Monad (filterM, foldM, unless, when)
import Control.Monad.Except (ExceptT (..), liftEither, runExceptT, throwError, withExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (bimap, first)
import qualified Data.ByteString as BS
import Data.Char (isAscii, isSpace, toLower, toUpper)
import Data.Foldable (for_)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe, maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.IO as TIO
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS
import qualified Network.HTTP.Types.Status as HTTP
import Nix.Builder.Log (openLogPipe, streamBuildLog)
import Nix.Builder.Unpack (UnpackLimits, builtinUnpackBuilder, defaultUnpackLimits, runBuiltinUnpack)
import Nix.DependencyGraph (TopoResult (..), buildDepGraph, topoSort)
import Nix.Derivation (Derivation (..), DerivationOutput (..), Platform, currentPlatform, extraPlatforms, fromATerm, platformToText)
import Nix.Derivation.StructuredAttrs (decodeStructuredAttrs, renderAttrsJson, renderAttrsShell, stringsAttr, structuredAttrsKey, withOutputPlaceholders)
import Nix.Hash (IncrementalHash, bytesToHexText, hashFinalizeBytes, hashInitWithAlgo, hashPlaceholder, hashUpdateChunk, hexToBytes, makeStorePath, rawHashWithAlgo, sha256Digest)
import Nix.Http (AttemptFailure (..), FetchRetryPolicy, attemptFailureMessage, catchSync, defaultFetchRetryPolicy, fetchStatusFailure, ioRetryEffects, retryTransient, transferFailureHandlers, withTransfer, withUserAgent)
import Nix.Push (narHashMatches)
import Nix.Realise (BuildMode (..), CheckVerdict (..), Fallback (..), FetchOutcome (..), RealiseEffects (..), RealiseSettings (..), Realised (..), SubstEffects (..), SubstitutesSetting (..), realise, realiseFailureMessage, realiseFailureStatus)
import Nix.Store (PathInfo (..), PathLock, PathRegistration, Store (..), acquirePathLock, isValid, placeInStore, queryPathInfo, queryReferences, registerPaths, releasePathLock, rewriteBytes, rewriteStoreHashes, scanReferences, volumeCaseHack)
import qualified Nix.Store.ExecBit as ExecBit
import Nix.Store.Path (BadStorePath, StoreDir (..), StorePath (spHash, spName), defaultStoreDir, defaultStoreDirText, parseStorePath, storePathToFilePath, storePathToText, unStoreDir)
import Nix.Substituter (CacheConfig, SubstResult (..), queryReferencesWith, trySubstituteWith)
import qualified NovaCache.Base32 as Nix32
import qualified NovaCache.Hash as Hash
import qualified NovaCache.NAR as NAR
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesPathExist, removeDirectoryRecursive, removePathForcibly)
import qualified System.Environment
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, takeFileName, (</>))
import qualified System.IO
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
envUrl, envUrls, envOut :: Text
envUrl = "url"
envUrls = "urls"
envOut = "out"

-- | HTTP success status code.
httpStatusOk :: Int
httpStatusOk = 200

-- | Environment variable for the reproducible-builds.org build timestamp.
envSourceDateEpoch :: Text
envSourceDateEpoch = "SOURCE_DATE_EPOCH"

-- | The derivation attribute naming ambient variables a fixed-output
-- build may read, upstream's carve-out for fetchers that need proxies.
envImpureEnvVars :: Text
envImpureEnvVars = "impureEnvVars"

-- | The variables naming a structured derivation's two attribute files.
envNixAttrsJsonFile, envNixAttrsShFile :: Text
envNixAttrsJsonFile = "NIX_ATTRS_JSON_FILE"
envNixAttrsShFile = "NIX_ATTRS_SH_FILE"

-- | Where in the build directory a structured derivation's attributes
-- land, under upstream's names.
attrsJsonFileName, attrsShellFileName :: FilePath
attrsJsonFileName = ".attrs.json"
attrsShellFileName = ".attrs.sh"

-- | The attribute listing the attributes a derivation passes as files.
envPassAsFile :: Text
envPassAsFile = "passAsFile"

-- | What upstream adds to an attribute's name to name the variable holding
-- the path of its file, and what it puts before the hash of that name to
-- name the file.
passedFileVarSuffix, passedFilePrefix :: Text
passedFileVarSuffix = "Path"
passedFilePrefix = ".attr-"

-- | The bytes upstream's @tokenizeString@ splits a list attribute at by
-- default (@src\/libutil\/util.hh@ at 2.24.9).  Not 'isSpace': a form feed,
-- a vertical tab or a non-ASCII space does not separate there.
tokenSeparators :: BS.ByteString
tokenSeparators = " \t\n\r"

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
    -- | Binary caches to try before building (checked in priority order).
    bcCaches :: ![CacheConfig],
    -- | What a failed substitute leads to: upstream's @fallback@.
    bcFallback :: !Fallback,
    -- | Whether a derivation's @allowSubstitutes@ attribute is heeded:
    -- upstream's @always-allow-substitutes@.
    bcSubstitutes :: !SubstitutesSetting,
    -- | What 'buildWithDeps' does with the derivation it is given:
    -- upstream's build mode, which @nix-build --check@ sets.  Nothing
    -- else reads it, and 'buildDerivation' always builds.
    bcMode :: !BuildMode,
    -- | Extraction budget for @builtin:unpack@ builds.
    bcUnpackLimits :: !UnpackLimits,
    -- | How a download is retried: each URL of a @builtin:fetchurl@
    -- before the next is tried, and each narinfo and NAR the substituter
    -- requests, as upstream's one @download-attempts@ setting governs
    -- every transfer.
    bcFetchRetry :: !FetchRetryPolicy,
    -- | Launchers for derivations whose @system@ this machine cannot execute
    -- directly, keyed by that system string exactly as
    -- 'Nix.Derivation.platformToText' spells it (@x86_64-windows@ -> a wine
    -- binary). Prepended to the builder at the spawn boundary only, so the
    -- derivation itself - and its store paths - stay identical to a native
    -- build's. A foreign system with no entry here is refused rather than
    -- spawned natively; see 'execWrapperFor'.
    bcExecWrappers :: !(Map Text FilePath),
    -- | Where a builder's output goes, a line at a time as each line
    -- completes ("Nix.Builder.Log").  Upstream's default logger prints
    -- every build-log line to standard error, which is where
    -- 'defaultBuildConfig' points this.
    bcBuildLog :: !System.IO.Handle
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
      bcCaches = [],
      bcFallback = NoFallback,
      bcSubstitutes = HeedAllowSubstitutes,
      bcMode = BuildNormal,
      bcExecWrappers = Map.empty,
      bcUnpackLimits = defaultUnpackLimits,
      bcFetchRetry = defaultFetchRetryPolicy,
      bcBuildLog = System.IO.stderr
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

-- | Build a derivation, given its @.drv@ path, which names it in a failure
-- as upstream's do: run its builder and capture output.
--
-- 1. Validate all inputs exist in the store
-- 2. Create temp build directory
-- 3. Set up environment from derivation + standard vars
-- 4. Run the builder process, which writes each output at its store path
-- 5. On success: scan the outputs for references and register them,
--    failing on a reference cycle between them ('registerOutputs')
-- 6. On failure: clean up and report error
-- 7. Exception safety: synchronous exceptions convert to BuildFailure;
--    asynchronous exceptions propagate ('catchSync') - an interrupt
--    must abort the build, never be reported as a build failure.
buildDerivation :: BuildConfig -> Store -> Derivation -> StorePath -> IO BuildResult
buildDerivation config store drv drvPath =
  either stoppedResult BuildSuccess <$> buildOutputs config store drv drvPath

-- | Why a build stopped short of what it was for: the message and exit
-- code a 'BuildFailure' reports.
data BuildStop = BuildStop !Text !Int

-- | The build result a stopped build reports.
stoppedResult :: BuildStop -> BuildResult
stoppedResult (BuildStop message code) = BuildFailure message code

-- | A build's synchronous exception as the stop it reports.  An
-- asynchronous one propagates ('catchSync').
stoppingOnException :: IO (Either BuildStop a) -> IO (Either BuildStop a)
stoppingOnException build =
  build `catchSync` \err -> pure (Left (BuildStop ("build exception: " <> T.pack (show err)) 1))

-- | Build under the outputs' path locks, and answer the first output's
-- path.  A derivation another process finished while we waited for them is
-- adopted rather than redone: rebuilding would delete a registered path
-- out from under whoever is already using it, which is why substitution
-- adopts a finished path too.
buildOutputs :: BuildConfig -> Store -> Derivation -> StorePath -> IO (Either BuildStop StorePath)
buildOutputs config store drv drvPath =
  stoppingOnException $
    withOutputLocks config store drv (maybe build (pure . Right))
  where
    build = buildUnderLock config store drv RedirectValid (registerOutputs config store drv drvPath)

-- | Check a derivation, given its @.drv@ path: build it again beside its
-- valid outputs and compare what comes out with what the store holds.
-- This is the build upstream runs under @bmCheck@, without what comes
-- before it: "Nix.Realise" substitutes an output that is not valid and
-- realises the inputs, and 'buildWithDeps' under 'BuildCheck' does both.
--
-- The rebuild registers nothing, leaves the outputs as they are, and keeps
-- nothing it wrote.  'Left' is why there was no comparison to answer with,
-- naming the derivation as 'buildWithDeps' names one that failed to build:
-- its builder failed, or an output is not valid, so there is nothing to
-- compare a rebuild with.
checkDerivation :: BuildConfig -> Store -> Derivation -> StorePath -> IO (Either Text CheckVerdict)
checkDerivation config store drv drvPath =
  first (stopReport drv) <$> checkOutputs config store drv drvPath

-- | 'checkDerivation', under the outputs' path locks, which upstream holds
-- through a check as through any build (@DerivationGoal::tryToBuild@ at
-- 2.24.9).  The builder writes every output to a scratch path
-- ('RedirectAll'), so the store's copies stay as they are, and what it
-- wrote is gone when this returns.
--
-- What a check compares with is each output's registration, so that is
-- what it asks for once the locks are held, and it is the one place that
-- decides whether the outputs are valid: the rebuild that follows is
-- handed the recorded hashes and asks the database nothing more.  An
-- output with no registration fails the check with upstream's message for
-- that state.  Upstream reaches it after running the builder
-- (@assertPathValidity@); the builder's work could only replace that
-- failure with another, so it is not run.
checkOutputs :: BuildConfig -> Store -> Derivation -> StorePath -> IO (Either BuildStop CheckVerdict)
checkOutputs config store drv drvPath =
  stoppingOnException $
    withOutputLocks config store drv (const compareUnderLock)
  where
    compareUnderLock = do
      recorded <- traverse (queryPathInfo (stDB store) . doPath) (drvOutputs drv)
      case sequence recorded of
        Nothing -> pure (Left (BuildStop unexpectedlyInvalidMessage 1))
        -- A check that compared nothing has not passed.
        Just [] -> pure (Left (BuildStop noOutputsMessage 1))
        Just registrations ->
          buildUnderLock config store drv RedirectAll (compareOutputs config store drv drvPath (map piNarHash registrations))

-- | Upstream's failure for a check that finds an output it was to compare
-- with not valid (@DerivationGoal::assertPathValidity@, derivation-goal.cc
-- at 2.24.9).
unexpectedlyInvalidMessage :: Text
unexpectedlyInvalidMessage = "some outputs are unexpectedly invalid"

-- | The failure for a derivation that defines no output.
noOutputsMessage :: Text
noOutputsMessage = "no outputs defined"

-- | Hold every output's path lock for the whole build, and report
-- whether the derivation was already finished by the time they were
-- granted.
--
-- Delete, materialize and register are one critical section under
-- @\<store-path\>.lock@, and the build path joins it.  Without it, two
-- concurrent builds of one derivation would share a build directory
-- ('computeBuildDir' is deterministic), interleave their writes into a
-- single tree, and each register it as valid.  Nothing would fail; the
-- registered output would simply not be what either build produced.
--
-- Outputs are locked in 'lockOrder', so two processes racing the same
-- multi-output derivation request them in the same sequence and neither
-- can hold a lock the other needs while waiting on one it holds.  A
-- build holds locks for its own outputs only - dependencies are
-- resolved before 'buildDerivation' is called, never underneath it - so
-- that sequence is the only one there is to agree on.
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
      mapM_ takeLock (lockOrder (map doPath (drvOutputs drv)))
      finishedElsewhere <- allOutputsValid store drv
      act finishedElsewhere
    )
    `finally` (readIORef heldLocks >>= mapM_ releasePathLock)

-- | The order a process takes more than one path lock in.  Substitution
-- holds each lock until its batch registers, and a build until it finishes,
-- so both have to use this order: the blocking lock has no timeout, and two
-- processes taking the same paths in different orders would wait forever.
lockOrder :: [StorePath] -> [StorePath]
lockOrder = sort

-- | The derivation's first output path when EVERY output is already
-- registered, otherwise 'Nothing'.
--
-- Every output, never just the first: a partially valid multi-output
-- derivation still has to build, and treating it as finished would leave
-- the missing outputs missing.  Both the lock-held re-check and the gate
-- in 'realiseEffects' ask this one question, so the two cannot disagree.
allOutputsValid :: Store -> Derivation -> IO (Maybe StorePath)
allOutputsValid store drv = case map doPath (drvOutputs drv) of
  [] -> pure Nothing
  outputPaths@(firstOutput : _) -> do
    validity <- mapM (isValid store) outputPaths
    pure (if and validity then Just firstOutput else Nothing)

-- | What a build does with the outputs its builder wrote, given the
-- closure of its inputs and where each output was written: registers them
-- ('registerOutputs'), or compares them with the store's
-- ('compareOutputs').
type Finish a = Set StorePath -> [OutputPlan] -> IO (Either BuildStop a)

-- | Which outputs the builder is handed a scratch path for
-- ('outputPlan').
data Redirected
  = -- | Those that are valid, which a build has to leave as they are.
    RedirectValid
  | -- | Every one.  A check's outputs are all valid: 'checkOutputs' has
    -- their registrations in hand, under the locks this runs under.
    RedirectAll

-- | The build itself, with every output's lock already held.
buildUnderLock :: BuildConfig -> Store -> Derivation -> Redirected -> Finish a -> IO (Either BuildStop a)
buildUnderLock config store drv redirected finish = do
  -- 1. Validate inputs (sources + input-derivation outputs) exist, and
  --    take their closure: what an output may reference besides the
  --    derivation's own outputs.
  inputs <- validateInputs config store drv >>= either (pure . Left) (inputClosure store)
  case inputs of
    Left errMsg -> pure (Left (BuildStop errMsg 1))
    Right inputPaths -> do
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
        outputValidity <- case redirected of
          RedirectValid -> mapM (isValid store . doPath) (drvOutputs drv)
          RedirectAll -> pure (map (const True) (drvOutputs drv))
        case traverse outputPlan (zip (drvOutputs drv) outputValidity) of
          Left nameErr -> pure (Left (BuildStop ("output name is not a store path name: " <> T.pack (show nameErr)) 1))
          Right plans -> runPlannedBuild config drv buildDir plans (finish inputPaths plans)

-- | Where the builder writes each output, and where that has to end up.
--
-- The two coincide for an output being built: writing into the final store
-- path is what this whole arrangement is for.  They differ for an output
-- that is already valid, which the builder is still handed (a package with
-- a @dev@ output has one builder writing both) but which must not be
-- touched: it is registered, its @NarHash@ describes its current bytes,
-- and 'placeInStore' has already sealed it read-only.  A check builds a
-- derivation whose outputs are all valid, so there every output is written
-- to a scratch path, as upstream writes a valid output wherever it builds
-- outside a Linux chroot (@startBuilder@, local-derivation-goal.cc:539-554
-- at 2.24.9).
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
outputPlan :: (DerivationOutput, Bool) -> Either BadStorePath OutputPlan
outputPlan (out, valid)
  | not valid = Right (OutputPlan (doName out) (doPath out) (doPath out) False)
  | otherwise = do
      fallback <- fallbackOutputPath (doPath out)
      pure (OutputPlan (doName out) (doPath out) fallback True)

-- | A scratch store path for an output that must not be written to, built
-- so it cannot collide with any real store path.
--
-- Upstream's @makeFallbackPath@ (the @StorePath@ overload, in
-- @src/libstore/unix/build/local-derivation-goal.cc@ at 2.24.9) with the
-- same shape: a bogus @rewrite:@ path type and an all-zeroes inner hash,
-- keyed on the output path it stands in for.
-- Upstream's type string also names the @.drv@ path
-- (@rewrite:\<drv\>:\<path\>@), so the scratch path a builder sees differs
-- from upstream's.  The output path already names one output of one
-- derivation, so the result is just as unique.
fallbackOutputPath :: StorePath -> Either BadStorePath StorePath
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
execWrapperFor = spawnFor currentPlatform

-- | 'execWrapperFor' with the host named, so the rule can be checked for
-- every host on whichever one runs the suite.  A derivation for the host's
-- own system, or for one the host executes through its compatibility layer
-- ('extraPlatforms'), spawns directly whatever launchers are configured:
-- upstream runs its extra platforms natively and has no launcher notion.
spawnFor :: Platform -> BuildConfig -> Derivation -> BuilderSpawn
spawnFor host config drv
  | platform == host || platform `elem` extraPlatforms host = SpawnNative
  | otherwise = case Map.lookup system (bcExecWrappers config) of
      Just launcher -> SpawnThrough launcher
      Nothing -> SpawnUnsupported system
  where
    platform = drvPlatform drv
    system = platformToText platform

-- | The build, once every output knows where it is written and where it
-- belongs: run the builder, then @finish@ with what it wrote.
runPlannedBuild :: BuildConfig -> Derivation -> FilePath -> [OutputPlan] -> IO (Either BuildStop a) -> IO (Either BuildStop a)
runPlannedBuild config drv buildDir plans finish = do
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
  -- a real store path.  Once @finish@ has succeeded there is nothing left
  -- to take back, and the flag says so: a build's registrations are
  -- committed, and a check wrote to scratch paths alone.
  committed <- newIORef False
  let cleanupUnlessCommitted = do
        done <- readIORef committed
        unless done clearScratch
  ( do
      -- 4. Decode builder/args/env for the spawn boundary.  Derivation
      --    strings are BYTES (identity); spawning a process needs real
      --    text, so invalid UTF-8 in any of them is a clean build
      --    failure, never a mojibake spawn.  An attribute that reaches
      --    the builder as a file stays bytes ('attrsDelivery').  The
      --    builtin builders skip this - they read the byte fields
      --    directly.
      exitResult <- case drvBuilder drv of
        b
          | b == builtinFetchurlBuilder -> first (uncurry BuilderRefused) <$> runBuiltinFetchurl (bcFetchRetry config) drv outputDirs
          | b == builtinUnpackBuilder -> first (uncurry BuilderRefused) <$> runBuiltinUnpack (bcStoreDir config) (bcUnpackLimits config) drv outputDirs
        _ -> case (execWrapperFor config drv, decodeBuilderStrings drv) of
          (SpawnUnsupported system, _) ->
            pure
              ( Left
                  ( BuilderRefused
                      1
                      ( "no way to run a "
                          <> system
                          <> " builder on this machine; pass --exec-wrapper "
                          <> system
                          <> "=PATH to name a launcher for it"
                      )
                  )
              )
          (_, Left errMsg) -> pure (Left (BuilderRefused 1 errMsg))
          (spawn, Right (builderText, argTexts)) ->
            let builderPath = T.unpack (onStore config builderText)
                builderArgs = map (T.unpack . buildRewrite config outputDirs) argTexts
                -- Env still names the real builder - a launcher only
                -- changes what's spawned, not what the derivation says.
                (spawnPath, spawnArgs) = case spawn of
                  SpawnThrough launcher -> (launcher, builderPath : builderArgs)
                  _ -> (builderPath, builderArgs)
                -- Only a fixed-output derivation's impureEnvVars reach
                -- the spawn: upstream gates the carve-out on the
                -- derivation type being non-sandboxed, which for us is
                -- exactly the fixed-output case.
                carriesHash out = not (T.null (doHashAlgo out))
                impureVars delivery
                  | any carriesHash (drvOutputs drv) = adImpureEnvVars delivery
                  | otherwise = Right []
             in case attrsDelivery config drv buildDir outputDirs of
                  Left err -> pure (Left (BuilderRefused 1 err))
                  Right delivery -> case impureVars delivery of
                    Left err -> pure (Left (BuilderRefused 1 err))
                    Right passed -> do
                      for_ (adFiles delivery) (uncurry BS.writeFile)
                      -- 5. Run the builder
                      runBuilder (bcBuildLog config) spawnPath spawnArgs (buildEnvironment config (adEnv delivery) builderPath buildDir) passed buildDir
      case exitResult of
        Left failure -> do
          -- Whatever the builder wrote is in the store now, so failure has
          -- to take it back out: an unregistered tree there is invisible to
          -- the database and would only confuse the next run.
          clearScratch
          pure (Left (builderStop failure))
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
              pure (Left (BuildStop ("builder succeeded but outputs missing: " <> names) 1))
            [] -> do
              finished <- finish
              case finished of
                Right _ -> do
                  writeIORef committed True
                  -- Only the fallbacks go; the registered trees stay.
                  -- After a build they are the redirected copies of
                  -- outputs that were already valid, which upstream
                  -- discards unread.  After a check they are every output
                  -- the builder wrote, compared by now and not kept,
                  -- whatever the comparison found.
                  for_ [p | p <- plans, opValid p] (removePathForcibly . scratchDir)
                Left _ -> clearScratch
              pure finished
    )
    `onException` cleanupUnlessCommitted

-- | Why the builder step of a build failed, with the exit code the
-- build reports.
data BuilderFailure
  = -- | The builder ran and exited unsuccessfully.  What it wrote is
    -- already in the build log, which upstream's default logger does not
    -- repeat in the failure: it shows the log's last lines there only
    -- when it has not been printing them (@log-lines@, derivation-goal.cc
    -- at 2.24.9).
    BuilderExited !Int
  | -- | The builder did not run, or a builtin builder refused, for the
    -- reason given.
    BuilderRefused !Int !Text

-- | The stop a failed builder step reports.
builderStop :: BuilderFailure -> BuildStop
builderStop failure = case failure of
  BuilderExited code -> BuildStop "builder failed" code
  BuilderRefused code reason -> BuildStop ("builder failed: " <> reason) code

-- | Decode a derivation's builder path and arguments from their identity
-- bytes to the 'Text' the process-spawn boundary needs.  @Left@ names the
-- offending field.  The environment is decoded where 'attrsDelivery'
-- decides what of it reaches the builder as variables.
decodeBuilderStrings :: Derivation -> Either Text (Text, [Text])
decodeBuilderStrings drv = do
  builderText <- decodeSpawnField "the builder path" (drvBuilder drv)
  argTexts <- traverse (decodeSpawnField "a builder argument") (drvArgs drv)
  pure (builderText, argTexts)

-- | Decode one derivation string for the spawn, @what@ naming it in the
-- failure.
decodeSpawnField :: Text -> BS.ByteString -> Either Text Text
decodeSpawnField what bytes = case TE.decodeUtf8' bytes of
  Right t -> Right t
  Left _ -> Left ("cannot spawn builder: " <> what <> " contains invalid UTF-8")

-- | 'decodeSpawnField' for an environment variable.
decodeVariable :: Text -> BS.ByteString -> Either Text Text
decodeVariable name = decodeSpawnField ("environment variable '" <> name <> "'")

-- ---------------------------------------------------------------------------
-- Input validation
-- ---------------------------------------------------------------------------

-- | Rewrite a derivation string's store paths from their identity (the
-- canonical @\/nix\/store@ spelling) to where this machine actually keeps
-- them, at the spawn boundary. A no-op when the two coincide.
onStore :: BuildConfig -> Text -> Text
onStore config = maybe id (uncurry T.replace) (storeRewrite config)

-- | The replacement 'onStore' makes, when the two spellings differ.
storeRewrite :: BuildConfig -> Maybe (Text, Text)
storeRewrite config
  | storeDirText == defaultStoreDirText = Nothing
  | otherwise = Just (defaultStoreDirText <> "/", storeDirText <> "/")
  where
    storeDirText = T.pack (unStoreDir (bcStoreDir config))

-- | Check that all inputs needed to build a derivation are present in the
-- store, and return them: the input sources, then the input derivations'
-- requested outputs.
--
-- Input sources must be valid.  For input derivations, the artifacts a build
-- actually reads are their realized OUTPUT paths, not the @.drv@ recipe files,
-- so we resolve each requested output (by reading the input @.drv@ the build
-- driver materialized) and validate those outputs.  "Nix.Realise" realizes a
-- derivation's inputs before it builds, so their outputs are present by the
-- time it is validated.
validateInputs :: BuildConfig -> Store -> Derivation -> IO (Either Text [StorePath])
validateInputs config store drv = do
  -- Check input sources
  srcResults <- mapM (isValid store) (drvInputSrcs drv)
  let missingSrcs = [sp | (sp, valid) <- zip (drvInputSrcs drv) srcResults, not valid]
  if not (null missingSrcs)
    then pure (Left ("missing input sources: " <> T.intercalate ", " (map formatSP missingSrcs)))
    else do
      resolved <- resolveInputOutputs config drv
      case resolved of
        Left err -> pure (Left err)
        Right requiredOutputs -> do
          outResults <- mapM (isValid store) requiredOutputs
          let missingOutputs = [sp | (sp, valid) <- zip requiredOutputs outResults, not valid]
          if not (null missingOutputs)
            then pure (Left ("missing input derivation outputs: " <> T.intercalate ", " (map formatSP missingOutputs)))
            else pure (Right (drvInputSrcs drv ++ requiredOutputs))

-- | Resolve each input derivation's requested output names to their store
-- paths by reading the input @.drv@ from the store.  @Left@ if an input @.drv@
-- cannot be read or names an output the derivation does not define.
resolveInputOutputs :: BuildConfig -> Derivation -> IO (Either Text [StorePath])
resolveInputOutputs config drv =
  runExceptT (concat <$> traverse resolveOne (Map.toList (drvInputDrvs drv)))
  where
    resolveOne (inputDrvPath, wantedOutputs) = do
      inputDrv <- ExceptT (readDrvFromStore config inputDrvPath)
      let outputMap = Map.fromList [(doName o, doPath o) | o <- drvOutputs inputDrv]
      liftEither (traverse (lookupOutput inputDrvPath outputMap) wantedOutputs)
    lookupOutput inputDrvPath outputMap name =
      case Map.lookup name outputMap of
        Just sp -> Right sp
        Nothing ->
          Left ("input derivation " <> formatSP inputDrvPath <> " has no output '" <> name <> "'")

-- | The closure of a build's inputs under the references the store records:
-- the paths besides its own outputs that an output may reference.  This is
-- upstream's @inputPaths@, the closures of the requested input outputs and
-- of the input sources (derivation-goal.cc:601-613 at 2.24.9).  An output
-- that embeds a dependency of a dependency must record it, or a collection
-- deletes it while the output still uses it.  As in upstream's, an input
-- derivation's @.drv@ is in it only when an input source reaches it.
--
-- A recorded reference that does not parse is a corrupt database, and fails
-- the build rather than dropping out of the scan.
inputClosure :: Store -> [StorePath] -> IO (Either Text (Set StorePath))
inputClosure store = runExceptT . closureUnder recorded
  where
    recorded :: StorePath -> ExceptT Text IO [StorePath]
    recorded sp = do
      referenceTexts <- liftIO (queryReferences (stDB store) sp)
      liftEither (traverse parseRecorded referenceTexts)
    parseRecorded text = case parseStorePath (stDir store) text of
      Just sp -> Right sp
      Nothing -> Left ("unparseable reference in store DB: " <> text)

-- | Every node reachable from the roots through the references lookup, the
-- roots included.  Each node is looked up once, so a reference cycle (the
-- outputs of one derivation may reference each other) ends.
closureUnder :: (Monad m, Ord a) => (a -> m [a]) -> [a] -> m (Set a)
closureUnder references = go Set.empty
  where
    go !seen [] = pure seen
    go !seen (node : pending)
      | Set.member node seen = go seen pending
      | otherwise = do
          referenced <- references node
          go (Set.insert node seen) (referenced ++ pending)

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

-- | Remove the build directory, ignoring a filesystem failure to remove
-- it.  It runs as the build's 'finally', where a directory that will not
-- go away must not replace the build's own outcome.
cleanupBuildDir :: FilePath -> IO ()
cleanupBuildDir dir = do
  exists <- doesDirectoryExist dir
  when exists $
    removeDirectoryRecursive dir `catch` \(_ :: IOException) -> pure ()

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
  foldr (uncurry T.replace) value (placeholderRewrites outputDirs)

-- | The replacements 'rewritePlaceholders' makes, one per output.
placeholderRewrites :: [(Text, FilePath)] -> [(Text, Text)]
placeholderRewrites outputDirs = [(hashPlaceholder name, T.pack path) | (name, path) <- outputDirs]

-- | The build's rewrite of a derivation string: store paths to where this
-- machine keeps them, then each output's placeholder to its build-time
-- path.  'onStore' goes first, so nothing a placeholder expands to is
-- rewritten a second time.
buildRewrite :: BuildConfig -> [(Text, FilePath)] -> Text -> Text
buildRewrite config outputDirs = rewritePlaceholders outputDirs . onStore config

-- | 'buildRewrite' over bytes, for an attribute that reaches the builder
-- as a file and so need not be text.  The replacements go in the order
-- 'buildRewrite' makes them ('rewritePlaceholders' folds from the right),
-- and on UTF-8 the two agree: an encoded needle matches only at a
-- character boundary.
buildRewriteBytes :: BuildConfig -> [(Text, FilePath)] -> BS.ByteString -> BS.ByteString
buildRewriteBytes config outputDirs =
  rewriteBytes
    [ (TE.encodeUtf8 from, TE.encodeUtf8 to)
    | (from, to) <- maybeToList (storeRewrite config) <> reverse (placeholderRewrites outputDirs)
    ]

-- | Apply a build-time rewrite to an environment's names as well as its
-- values.  Upstream rewrites the whole @name=value@ string
-- (local-derivation-goal.cc:2004), so a placeholder in a dynamic attribute
-- name is substituted too; rewriting only the values would hand the builder
-- a variable whose name still held the sentinel.
rewriteEnv :: (Text -> Text) -> Map Text Text -> Map Text Text
rewriteEnv rewrite = Map.fromList . map rewritePair . Map.toList
  where
    rewritePair (name, value) = (rewrite name, rewrite value)

-- | What a builder receives of its derivation beyond the builder path and
-- arguments.
data AttrsDelivery = AttrsDelivery
  { -- | Variables the derivation contributes to the environment.
    adEnv :: !(Map Text Text),
    -- | Files written into the build directory before the builder starts.
    adFiles :: ![(FilePath, BS.ByteString)],
    -- | The @impureEnvVars@ attribute, which matters only to a fixed-output
    -- derivation, so a malformed one fails only such a build, as upstream.
    adImpureEnvVars :: !(Either Text [Text])
  }
  deriving (Eq, Show)

-- | How the derivation's attributes reach the builder: as environment
-- variables, outputs included, or for a derivation carrying structured
-- attributes, as @.attrs.json@ and @.attrs.sh@ in the build directory
-- named by two variables, with no attribute and no output in the
-- environment (upstream's @initTmpDir@ and @writeStructuredAttrs@,
-- local-derivation-goal.cc at 2.24.9).
--
-- Without structured attributes, each attribute @passAsFile@ names
-- ('splitPassAsFile') is written to a file in the build directory instead,
-- holding the bytes its variable would have held, and @\<name\>Path@
-- names the file.  An attribute that is itself called @\<name\>Path@
-- keeps its own value, as it does upstream, where it is assigned later.
-- The variable joins the build directory and the file name with the
-- platform's separator where upstream writes @\/@, so on Windows it is
-- spelled as @NIX_BUILD_TOP@ is.  Structured attributes need no such
-- escape from the environment, and upstream ignores @passAsFile@ there.
--
-- The build's rewrite ('buildRewrite') applies to the shell file's text
-- as upstream applies it, after rendering; the JSON file's strings take it
-- before they are escaped, so a store directory holding backslashes still
-- yields JSON.
attrsDelivery :: BuildConfig -> Derivation -> FilePath -> [(Text, FilePath)] -> Either Text AttrsDelivery
attrsDelivery config drv buildDir outputDirs = case Map.lookup structuredAttrsKey env of
  Nothing -> do
    let (variables, passed) = splitPassAsFile env
        passedPath name = buildDir </> passedFileName name
        pathVars = Map.fromList [(name <> passedFileVarSuffix, T.pack (passedPath name)) | name <- Map.keys passed]
    decoded <- Map.traverseWithKey decodeVariable variables
    Right
      AttrsDelivery
        { adEnv = unionEnvs [Map.withoutKeys outputEnv (Map.keysSet passed), rewriteEnv rewrite (unionEnvs [decoded, pathVars])],
          adFiles = [(passedPath name, passedContents name value) | (name, value) <- Map.toList passed],
          -- Read from the whole environment, as upstream's getStringsAttr
          -- reads it, so passing the attribute as a file does not drop it.
          adImpureEnvVars = T.words <$> decodeVariable envImpureEnvVars (Map.findWithDefault "" envImpureEnvVars env)
        }
  Just encodedBytes -> do
    encoded <- decodeVariable structuredAttrsKey encodedBytes
    attrs <- first (("cannot process " <> structuredAttrsKey <> " attribute of '" <> formatDrvName drv <> "': ") <>) (decodeStructuredAttrs encoded)
    let prepared = withOutputPlaceholders (map fst outputDirs) attrs
        jsonFile = buildDir </> attrsJsonFileName
        shellFile = buildDir </> attrsShellFileName
    Right
      AttrsDelivery
        { adEnv = Map.fromList [(envNixAttrsJsonFile, T.pack jsonFile), (envNixAttrsShFile, T.pack shellFile)],
          adFiles =
            [ (jsonFile, renderAttrsJson rewrite prepared),
              (shellFile, TE.encodeUtf8 (rewrite (renderAttrsShell prepared)))
            ],
          adImpureEnvVars = fromMaybe [] <$> stringsAttr envImpureEnvVars attrs
        }
  where
    env = drvEnv drv
    rewrite = buildRewrite config outputDirs
    outputEnv = Map.fromList [(name, T.pack path) | (name, path) <- outputDirs]
    -- An output's variable is the path it is written to, which is not
    -- the derivation's value when that output is already valid, so its
    -- file holds the same path.
    passedContents name value =
      maybe (buildRewriteBytes config outputDirs value) (TE.encodeUtf8 . T.pack) (lookup name outputDirs)

-- | Upstream's @passAsFile@: split off the attributes the @passAsFile@
-- attribute names, as files rather than variables, leaving the rest.  A
-- name with no attribute behind it is ignored, and @passAsFile@ itself
-- stays a variable unless it names itself.  Names are compared as bytes,
-- as upstream compares them.
splitPassAsFile :: Map Text BS.ByteString -> (Map Text BS.ByteString, Map Text BS.ByteString)
splitPassAsFile env = (variables, passed)
  where
    named = Set.fromList (attrTokens (Map.findWithDefault "" envPassAsFile env))
    (passed, variables) = Map.partitionWithKey (\name _ -> Set.member (TE.encodeUtf8 name) named) env

-- | Upstream's @tokenizeString@ with its default separators: the
-- non-empty runs between 'tokenSeparators'.
attrTokens :: BS.ByteString -> [BS.ByteString]
attrTokens = filter (not . BS.null) . BS.splitWith (`BS.elem` tokenSeparators)

-- | The file an attribute passed as a file is written to: upstream's
-- @.attr-@ and the SHA-256 of the attribute's name in Nix base-32, so
-- whatever the name holds, the file is one entry of the build directory.
passedFileName :: Text -> FilePath
passedFileName name = T.unpack (passedFilePrefix <> Nix32.encode (sha256Digest (TE.encodeUtf8 name)))

-- | Build the process environment from the derivation's contribution
-- ('adEnv') + standard vars.
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
  Map Text Text
buildEnvironment config derivationEnv builderPath buildDir =
  let -- Standard build variables
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
   in -- Priority: derivation env (output paths first) > standard env
      unionEnvs [derivationEnv, standardEnv]

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

-- | Run the builder process, copying what it writes to @buildLog@, and
-- report how it exited.
--
-- Its standard output and standard error are one pipe ('openLogPipe'),
-- so the log holds both in the order the builder wrote them, as
-- upstream's does.  Its standard input is a pipe this process closes
-- as soon as the builder is spawned, so a read sees end-of-file.
--
-- The child's environment is exactly the build environment plus
-- 'scrubAmbient'\'s allowance - the ambient environment does not flow
-- through.  Filesystem and process isolation remain future work.
--
-- @impureVars@ is the impureEnvVars carve-out: the caller passes the
-- names a fixed-output derivation listed (empty otherwise), and each
-- is copied from the ambient environment - the EMPTY STRING when
-- absent, not omitted, matching upstream's @getEnv(i).value_or("")@.
-- Upstream writes these last in initEnv, so they win even over the
-- derivation's own values.  Names match exactly, as upstream's do.
runBuilder ::
  System.IO.Handle ->
  FilePath ->
  [String] ->
  Map Text Text ->
  [Text] ->
  FilePath ->
  IO (Either BuilderFailure ())
runBuilder buildLog builderPath builderArgs buildEnv impureVars workDir = do
  systemEnv <- System.Environment.getEnvironment
  let systemMap = Map.fromList [(T.pack k, T.pack v) | (k, v) <- systemEnv]
      impureAllowance =
        Map.fromList [(var, Map.findWithDefault "" var systemMap) | var <- impureVars]
      -- Impure carve-out wins, then build env, displacing case variants
      -- on Windows; the scrubbed ambient allowance fills underneath.
      mergedEnv = unionEnvs [impureAllowance, buildEnv, scrubAmbient systemMap]
      envList = [(T.unpack k, T.unpack v) | (k, v) <- Map.toList mergedEnv]
      spawnWith toLog =
        (mkBuilderProcess builderPath builderArgs)
          { Proc.cwd = Just workDir,
            Proc.env = Just envList,
            Proc.std_in = Proc.CreatePipe,
            -- The spawn closes this process's copy of the write end, so
            -- the builder's tree holds the only ones and its exit ends
            -- the log.
            Proc.std_out = Proc.UseHandle toLog,
            Proc.std_err = Proc.UseHandle toLog,
            -- Wrap the builder's whole process tree in a Win32 job with
            -- kill-on-job-close.  Without it, a builder's grandchildren
            -- (bash -> make -> cc, or anything cmd.exe spawns) outlive an
            -- interrupt: terminateProcess reaches only the direct child,
            -- so the tree is orphaned and keeps running.  With the job,
            -- terminateProcess becomes TerminateJobObject and reaps the
            -- tree, and waitForProcess waits for all of it.  Ignored on
            -- POSIX, where terminateProcess signals the builder alone and
            -- leaves its descendants running.
            -- A deliberately daemonizing grandchild changes from leaking
            -- to blocking the build until it exits; that is the correct
            -- trade for a build, which owns everything it spawned.
            Proc.use_process_jobs = True
          }
      closePipe (fromBuilder, toLog) = System.IO.hClose fromBuilder *> System.IO.hClose toLog
  exitCode <-
    bracket openLogPipe closePipe $ \(fromBuilder, toLog) ->
      Proc.withCreateProcess (spawnWith toLog) $ \stdinEnd _ _ process -> do
        for_ stdinEnd System.IO.hClose
        streamBuildLog fromBuilder buildLog
        Proc.waitForProcess process
  pure $ case exitCode of
    ExitSuccess -> Right ()
    ExitFailure code -> Left (BuilderExited code)

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

-- | Run a @builtin:fetchurl@ derivation: try URLs in order, each under the
-- retry policy, and verify the written bytes against the derivation's
-- @outputHash@. Nix's bootstrap fetcher is baked into the binary because
-- nothing can be fetched before a fetcher exists.  Returns the same
-- @Either (exit, msg) ()@ shape as 'runBuilder', so the shared
-- output-registration path is reused unchanged.
runBuiltinFetchurl :: FetchRetryPolicy -> Derivation -> [(Text, FilePath)] -> IO (Either (Int, Text) ())
runBuiltinFetchurl policy drv outputDirs =
  case (fetchUrlsFromEnv (drvEnv drv), lookup envOut outputDirs, fixedOutput) of
    (Left err, _, _) -> pure (Left (1, "builtin:fetchurl: " <> err))
    (_, Nothing, _) -> pure (Left (1, "builtin:fetchurl: derivation defines no 'out' output"))
    (_, _, Nothing) -> pure (Left (1, "builtin:fetchurl: 'out' output has no fixed-output hash"))
    (Right urls@(firstUrl :| _), Just outPath, Just out)
      -- Invalid specifications reject before any network traffic.
      | recursive -> pure (Left (1, recursiveUnsupportedMessage))
      | Nothing <- hexToBytes (doHash out) ->
          pure (Left (1, malformedExpectedHash firstUrl))
      | otherwise -> case hashInitWithAlgo algo of
          Nothing ->
            pure (Left (1, "builtin:fetchurl: unsupported hash algorithm '" <> algo <> "'"))
          Just ctx -> do
            result <- tryFetchUrlsWith (fetchWithRetry outPath out ctx) urls
            pure $ case result of
              Left err -> Left (1, err)
              Right value -> Right value
      where
        (recursive, algo) = splitHashMode (doHashAlgo out)
  where
    -- builtin:fetchurl derivations are always fixed-output; the expected hash
    -- lives in the canonical output spec (doHashAlgo + doHash), not the env.
    fixedOutput = case drvOutputs drv of
      (out : _) | not (T.null (doHashAlgo out)) -> Just out
      _ -> Nothing
    fetchWithRetry outPath out ctx url =
      first attemptFailureMessage <$> retryTransient policy ioRetryEffects (fetchAndVerify outPath out ctx url)
    -- A download that completed and verifies wrong is the server's bytes,
    -- not a hiccup: upstream checks the hash after the transfer layer has
    -- given up retrying, so a mismatch is never retried.
    fetchAndVerify outPath out ctx url = do
      downloaded <- downloadUrlTo url outPath ctx
      pure $ case downloaded of
        Left failure -> Left (prefixFailure failure)
        Right digest -> first (FatalFailure . snd) (verifyFetchedDigest url out digest)
    prefixFailure failure = case failure of
      TransientFailure msg -> TransientFailure ("builtin:fetchurl: " <> msg)
      FatalFailure msg -> FatalFailure ("builtin:fetchurl: " <> msg)

-- | Nova's mirror extension uses the ASCII-whitespace-separated @urls@ field.
-- An absent field preserves the legacy single @url@ interface. An explicit
-- empty or malformed list is an error, rather than a silent fallback.
fetchUrlsFromEnv :: Map Text BS.ByteString -> Either Text (NonEmpty Text)
fetchUrlsFromEnv env = case Map.lookup envUrls env of
  Just bytes -> do
    text <- decode envUrls bytes
    maybe (Left "'urls' is empty") Right (NE.nonEmpty (filter (not . T.null) (T.split separator text)))
  Nothing -> case Map.lookup envUrl env of
    Nothing -> Left "derivation has no 'url' or 'urls'"
    Just bytes -> do
      url <- decode envUrl bytes
      if T.null url then Left "'url' is empty" else Right (url :| [])
  where
    -- Match the fetcher expression's POSIX space class, preserving non-ASCII
    -- URL characters that 'T.words' would otherwise split into new candidates.
    separator char = isAscii char && isSpace char
    decode field = first (const ("'" <> field <> "' contains invalid UTF-8")) . TE.decodeUtf8'

-- | Ordered fallback policy, independent of HTTP and files. Only the first
-- successful /verified/ attempt wins; report every failure if none wins.
-- Exceptions propagate, so cancellation cannot start another download.
tryFetchUrlsWith :: (Monad m) => (Text -> m (Either Text a)) -> NonEmpty Text -> m (Either Text a)
tryFetchUrlsWith attempt = go []
  where
    go !failures (url :| rest) = do
      result <- attempt url
      case result of
        Right value -> pure (Right value)
        Left err -> case NE.nonEmpty rest of
          Nothing -> pure (Left (T.intercalate "\n" (reverse (err : failures))))
          Just urls -> go (err : failures) urls

-- ---------------------------------------------------------------------------
-- Download
-- ---------------------------------------------------------------------------

-- | Download a URL to a file using nova-nix's own linked HTTP client
-- (the same 'Network.HTTP.Client' the substituter uses) - no external
-- @curl@, which is what makes this a genuine builtin.  The body streams
-- to disk through the incremental hash chunk by chunk, so memory stays
-- at chunk size no matter the download's size, and the returned digest
-- is of exactly the written bytes. A failure of the transfer or of the
-- write becomes a 'Left' carrying its retry class
-- ('transferFailureHandlers'); cancellation propagates through the shared
-- build cleanup. Every attempt opens the output in WriteMode and starts
-- from the original hash context, so a retry never resumes a torn
-- transfer: upstream resumes from the written offset when the server
-- accepts ranges and otherwise gives up, which this restart subsumes.
downloadUrlTo :: Text -> FilePath -> IncrementalHash -> IO (Either AttemptFailure BS.ByteString)
downloadUrlTo url outPath ctx0 =
  fetch `catches` transferFailureHandlers url
  where
    fetch :: IO (Either AttemptFailure BS.ByteString)
    fetch = do
      manager <- HTTPS.getGlobalManager
      request <- withUserAgent <$> HTTP.parseRequest (T.unpack url)
      withTransfer request manager $ \response -> do
        let status = HTTP.responseStatus response
        if HTTP.statusCode status /= httpStatusOk
          then pure (Left (fetchStatusFailure url status))
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

-- | After a successful build, register every output in the store, in
-- upstream's two passes (registerOutputs, local-derivation-goal.cc:2288-2751
-- at 2.24.9).  Every output is scanned first.  Then the outputs are ordered
-- so that each follows the siblings it references ('orderByReferences'),
-- and placed in that order.  Outputs that reference each other in a cycle
-- fail the build before any is placed, so none is registered.
--
-- The placed outputs register together in one transaction, so a
-- cross-output reference finds its referent's row (see 'registerPaths').
registerOutputs ::
  BuildConfig ->
  Store ->
  Derivation ->
  StorePath ->
  Set StorePath ->
  [OutputPlan] ->
  IO (Either BuildStop StorePath)
registerOutputs config store drv drvPath inputPaths plans = runExceptT $ do
  scanned <- withExceptT scanStop (traverse (scanOutput config store scan) (zip (drvOutputs drv) plans))
  ordered <- liftEither (orderScanned drvPath (siblingsReferenced (drvOutputs drv)) scanned)
  regs <- withExceptT scanStop (traverse (ExceptT . placeOutput store deriver . snd) ordered)
  liftIO (registerPaths (stDB store) (catMaybes regs))
  case drvOutputs drv of
    (firstOut : _) -> pure (doPath firstOut)
    [] -> throwError (BuildStop noOutputsMessage 1)
  where
    scan = outputScan drv inputPaths plans
    -- Outputs register with no deriver yet, so 'queryDeriver' answers
    -- Nothing for a built path, where upstream records the @.drv@
    -- (local-derivation-goal.cc:2738).
    deriver = Nothing

-- | After a successful rebuild under 'BuildCheck', compare every output
-- with the NAR hash the store recorded for it, the hashes in the order of
-- the derivation's outputs: upstream's @registerOutputs@ under @bmCheck@
-- (local-derivation-goal.cc at 2.24.9).  Every output is scanned and the
-- outputs ordered as a build's are, so a reference cycle between them
-- fails a check as it fails a build (:2317, :2381-2411).  Each is then
-- compared in that order, an output after the siblings it references, and
-- the first that differs ends the comparison, where upstream's loop throws
-- (:2415, :2694-2715).
--
-- Nothing is registered and no output is moved.  Upstream marks a path it
-- has checked as ultimately trusted and signs it (:2717-2722); the store
-- database here records neither.
compareOutputs ::
  BuildConfig ->
  Store ->
  Derivation ->
  StorePath ->
  [Text] ->
  Set StorePath ->
  [OutputPlan] ->
  IO (Either BuildStop CheckVerdict)
compareOutputs config store drv drvPath recordedHashes inputPaths plans = runExceptT $ do
  built <- withExceptT scanStop (traverse rebuilt (zip3 (drvOutputs drv) plans recordedHashes))
  ordered <- liftEither (orderScanned drvPath (siblings . boReferences . fst) built)
  ExceptT (firstDifference (volumeCaseHack (stCaseSensitivity store)) (map snd ordered))
  where
    scan = outputScan drv inputPaths plans
    siblings = siblingNames (drvOutputs drv)
    rebuilt (output, plan, recorded) = do
      outDir <- writtenOutput config (output, plan)
      scanned <- liftIO (scanBuilt scan output outDir)
      pure (doName output, (scanned, recorded))

-- | The stop a failed scan or placement reports.
scanStop :: Text -> BuildStop
scanStop message = BuildStop message 1

-- | Order scanned outputs so that each follows the siblings it references
-- ('orderByReferences'), or stop the build at a cycle among them.
orderScanned :: StorePath -> (v -> Set Text) -> [(Text, v)] -> Either BuildStop [(Text, v)]
orderScanned drvPath references scanned =
  first (scanStop . outputCycleMessage drvPath) (orderByReferences references (Map.fromList scanned))

-- | Upstream's failure for outputs that reference each other in a cycle
-- (local-derivation-goal.cc:2406-2408 at 2.24.9), naming the @.drv@ and
-- the reference that closes the cycle.
outputCycleMessage :: StorePath -> ReferenceCycle Text -> Text
outputCycleMessage drvPath (ReferenceCycle node referrer) =
  "cycle detected in build of '"
    <> storePathToText defaultStoreDir drvPath
    <> "' in the references of output '"
    <> node
    <> "' from output '"
    <> referrer
    <> "'"

-- | What the scan of a build's outputs rewrites and looks for.
data OutputScan = OutputScan
  { -- | Each redirected output's scratch hash and the final one it
    -- becomes.  An output may record a sibling, or itself, whose build
    -- wrote to a scratch path that is about to stop existing.  Upstream
    -- scans first and translates its hits; rewriting first lets the scan
    -- see final hashes and needs no translation, and the references come
    -- out the same.
    osScratchRewrites :: ![(Text, Text)],
    -- | Candidates for reference scanning: the input closure and this
    -- derivation's own outputs, upstream's @referenceablePaths@
    -- (local-derivation-goal.cc:2275-2278 at 2.24.9).
    osCandidates :: ![StorePath]
  }

-- | The scan for a derivation's outputs, given the closure of its inputs
-- and where each output was written.
outputScan :: Derivation -> Set StorePath -> [OutputPlan] -> OutputScan
outputScan drv inputPaths plans =
  OutputScan
    { osScratchRewrites = [(spHash (opScratch p), spHash (opFinal p)) | p <- plans, opValid p],
      osCandidates = Set.toList inputPaths ++ map doPath (drvOutputs drv)
    }

-- | One output after the scan and before it is placed: upstream's
-- @AlreadyRegistered@ and @PerhapsNeedToRegister@.
data ScannedOutput
  = -- | Registered before the build, and left as it is.
    AlreadyRegistered
  | -- | Built here.
    NeedsRegistering !BuiltOutput

-- | An output the builder wrote, after the scan.
data BuiltOutput = BuiltOutput
  { boOutput :: !DerivationOutput,
    -- | Where the builder wrote it.
    boDir :: !FilePath,
    -- | The references the scan found in it.
    boReferences :: ![StorePath]
  }

-- | Where the builder wrote an output, which has to be there (a file or a
-- directory).
writtenOutput :: BuildConfig -> (DerivationOutput, OutputPlan) -> ExceptT Text IO FilePath
writtenOutput config (_, plan) = do
  let outDir = storePathToFilePath (bcStoreDir config) (opScratch plan)
  exists <- liftIO (doesPathExist outDir)
  unless exists (throwError ("output missing: " <> T.pack outDir))
  pure outDir

-- | Scan one output of a build, keyed by its name: skip it if it is
-- already valid in the DB, otherwise scan what the builder wrote
-- ('scanBuilt').
scanOutput ::
  BuildConfig ->
  Store ->
  OutputScan ->
  (DerivationOutput, OutputPlan) ->
  ExceptT Text IO (Text, ScannedOutput)
scanOutput config store scan planned@(output, _) = do
  outDir <- writtenOutput config planned
  -- Validity is a DB fact, not mere disk presence: an output left on disk
  -- by an interrupted run is NOT valid, and build outputs are not
  -- content-addressed, so a leftover cannot be verified against its path.
  -- 'runPlannedBuild' removed any leftover before the builder ran, so the
  -- tree here is this build's (upstream's delete-then-move).
  valid <- liftIO (isValid store (doPath output))
  scanned <- if valid then pure AlreadyRegistered else NeedsRegistering <$> liftIO (scanBuilt scan output outDir)
  pure (doName output, scanned)

-- | Rewrite scratch hashes in an output the builder wrote, then scan it
-- for references to the input closure and this derivation's outputs.
--
-- One scan, not two: the candidates already carry this derivation's own
-- outputs, and after the rewrite the tree names them by their final
-- hashes, so a self- or cross-output reference is an ordinary hit.
scanBuilt :: OutputScan -> DerivationOutput -> FilePath -> IO BuiltOutput
scanBuilt scan output outDir = do
  rewriteStoreHashes (osScratchRewrites scan) outDir
  refs <- dedupStorePaths <$> scanReferences (osCandidates scan) outDir
  pure (BuiltOutput output outDir refs)

-- | The names of the sibling outputs among an output's references.
siblingNames :: [DerivationOutput] -> [StorePath] -> Set Text
siblingNames outputs = Set.fromList . mapMaybe (`Map.lookup` nameByPath)
  where
    nameByPath = Map.fromList [(doPath o, doName o) | o <- outputs]

-- | The names of the sibling outputs an output's scan found.  An output
-- registered before the build references none: upstream uses the
-- registered copy and treats it as a leaf.
siblingsReferenced :: [DerivationOutput] -> ScannedOutput -> Set Text
siblingsReferenced outputs = referencedBy
  where
    siblings = siblingNames outputs
    referencedBy AlreadyRegistered = Set.empty
    referencedBy (NeedsRegistering built) = siblings (boReferences built)

-- | Compare rebuilt outputs, each with the NAR hash the store recorded for
-- it, in order, up to the first that differs or cannot be compared.
firstDifference :: NAR.CaseHack -> [(BuiltOutput, Text)] -> IO (Either BuildStop CheckVerdict)
firstDifference _ [] = pure (Right OutputsMatch)
firstDifference caseHack (rebuilt : rest) = do
  compared <- compareOutput caseHack rebuilt
  case compared of
    Right OutputsMatch -> firstDifference caseHack rest
    ended -> pure ended

-- | Compare one rebuilt output with the NAR hash the store recorded for
-- the valid one, which is all upstream compares (@newInfo.narHash !=
-- oldInfo.narHash@, local-derivation-goal.cc:2698 at 2.24.9).  The rebuilt
-- tree's hash is 'ExecBit.narHashOfPath', which streams the archive that
-- registration serialises whole ('Nix.Store.registrationFor'), under the
-- same case-hack mode and source of the executable bit.
--
-- A fixed output is held to its declared hash first, as a build holds it:
-- upstream raises that mismatch for a checked fixed output and never
-- reaches the comparison (:2612-2621, :2696, :2757).
compareOutput :: NAR.CaseHack -> (BuiltOutput, Text) -> IO (Either BuildStop CheckVerdict)
compareOutput caseHack (built, recorded) = do
  fixedCheck <- verifyFixedOutput caseHack (boOutput built) (boDir built)
  case fixedCheck of
    Left err -> pure (Left (scanStop err))
    Right () -> do
      rebuilt <- Hash.formatNixHash <$> ExecBit.narHashOfPath caseHack (boDir built)
      pure (Right (if narHashMatches recorded rebuilt then OutputsMatch else OutputDiffers (doPath (boOutput built))))

-- | Place a scanned output in the store and check a fixed output's
-- content, returning its 'PathRegistration'; @Nothing@ for an output
-- registered before the build.
placeOutput :: Store -> Maybe Text -> ScannedOutput -> IO (Either Text (Maybe PathRegistration))
placeOutput store deriver scanned = case scanned of
  AlreadyRegistered -> pure (Right Nothing)
  NeedsRegistering (BuiltOutput output outDir refs) -> do
    -- The builder wrote straight into the store path, so outDir IS the
    -- target and placing it moves nothing.
    let targetPath = storePathToFilePath (stDir store) (doPath output)
    reg <- placeInStore store outDir (doPath output) deriver refs
    fixedCheck <- verifyFixedOutput (volumeCaseHack (stCaseSensitivity store)) output targetPath
    case fixedCheck of
      Left err -> do
        -- Wrong bytes for a declared content address: remove the placed
        -- tree so a later run cannot meet it on disk.  'removePathForcibly'
        -- clears the read-only marks placing set.
        removePathForcibly targetPath
        pure (Left err)
      Right () -> pure (Right (Just reg))

-- | A reference cycle, named by the reference that closes it: the node
-- reached again while it is still on the walk's path, and the node whose
-- reference reached it.
data ReferenceCycle k = ReferenceCycle
  { cycleNode :: !k,
    cycleReferrer :: !k
  }
  deriving (Eq, Show)

-- | Where the walk in 'orderByReferences' is: the nodes it has entered,
-- and the ones it has finished, the last finished first.
data Walk k v = Walk
  { walkEntered :: !(Set k),
    walkFinished :: ![(k, v)]
  }

-- | Order nodes so that each follows every node it references, or report
-- a cycle among them: upstream's @topoSort@ (src/libutil/topo-sort.hh at
-- 2.24.9) with the reversal its @registerOutputs@ applies to the result
-- (local-derivation-goal.cc:2411).
--
-- The walk is upstream's depth-first one, so the order and the cycle it
-- reports are upstream's too: it starts from each node in ascending
-- order, follows each node's references in ascending order, ignores a
-- self-reference and a reference to a node not in the map, and lists a
-- node once everything it references is listed.  The first reference that
-- leads back to a node still on the path is the cycle reported.
orderByReferences :: (Ord k) => (v -> Set k) -> Map k v -> Either (ReferenceCycle k) [(k, v)]
orderByReferences references nodes =
  reverse . walkFinished <$> foldM (visit Set.empty) (Walk Set.empty []) (Map.toAscList nodes)
  where
    visit onPath walk node@(key, value)
      | Set.member key (walkEntered walk) = Right walk
      | otherwise = do
          let referenced =
                [ (child, childValue)
                | child <- Set.toAscList (references value),
                  child /= key,
                  Just childValue <- [Map.lookup child nodes]
                ]
          finished <-
            foldM
              (follow (Set.insert key onPath) key)
              walk {walkEntered = Set.insert key (walkEntered walk)}
              referenced
          pure finished {walkFinished = node : walkFinished finished}
    follow onPath referrer walk child@(key, _)
      | Set.member key onPath = Left (ReferenceCycle key referrer)
      | otherwise = visit onPath walk child

-- | Re-check a placed fixed-output output against its declared hash.
-- A fixed-output derivation declares both its store path and, via the
-- output spec (@doHashAlgo@ + @doHash@), the exact content the path
-- must carry - but the builder process writes the actual bytes, so the
-- placed result must reproduce the declared digest before it may
-- register as valid (upstream: the fixed-output check in its
-- registerOutputs).  Flat mode hashes the output file's bytes;
-- recursive (@r:@) mode hashes the canonical NAR of the placed tree.
-- Non-fixed outputs (empty @doHashAlgo@) pass unchecked.  The tree is
-- read back under the store volume's case-hack mode.
verifyFixedOutput :: NAR.CaseHack -> DerivationOutput -> FilePath -> IO (Either Text ())
verifyFixedOutput caseHack out placedPath
  | T.null (doHashAlgo out) = pure (Right ())
  | recursive = do
      -- Re-serialises the placed tree: the registration's NAR hash is
      -- always sha256, while the declared algorithm may be any.
      entry <- ExecBit.serialiseFromPath caseHack placedPath
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

-- ---------------------------------------------------------------------------
-- Dependency-aware build orchestration
-- ---------------------------------------------------------------------------

-- | Realise a derivation and what it needs, root first ("Nix.Realise").
--
-- 1. Build the dependency graph by reading .drv files from the store.
-- 2. Refuse a cycle.
-- 3. Try each derivation's outputs before its inputs, so a substituted
--    derivation needs only its runtime closure.  What no cache supplies
--    builds locally, leaves first, and so does what a cache fails to
--    deliver when 'bcFallback' allows it, and a derivation that forbids
--    substitutes unless 'bcSubstitutes' overrides it.
--
-- Under 'BuildCheck' ('bcMode') the root is not realised but checked: an
-- output of its that is not valid is substituted, its inputs are realised
-- as above, and it is built again beside its valid outputs and compared
-- with them ('checkDerivation').
--
-- Returns 'BuildSuccess' with the root output path on success, or
-- 'BuildFailure' with the status the build exits with
-- ('realiseFailureStatus'): 104 if a check found an output different, and
-- 1 if a derivation fails to build, a substitute fails under 'NoFallback',
-- a cache cannot be asked, a derivation's @allowSubstitutes@ cannot be
-- read, or a check has no valid output to compare with.
buildWithDeps :: BuildConfig -> Store -> Derivation -> StorePath -> IO BuildResult
buildWithDeps config store rootDrv rootDrvPath = do
  graph <- buildDepGraph (readDrvFromStore config) rootDrv rootDrvPath
  case graph of
    Left err -> pure (BuildFailure ("dependency graph error: " <> err) 1)
    Right depGraph ->
      case topoSort depGraph of
        TopoCycle _ ->
          pure (BuildFailure "dependency cycle detected" 1)
        TopoSorted _ -> do
          let settings = RealiseSettings {rsFallback = bcFallback config, rsSubstitutes = bcSubstitutes config, rsMode = bcMode config}
          result <- realise settings (realiseEffects config store) depGraph rootDrvPath
          case result of
            Left failure -> pure (BuildFailure (realiseFailureMessage (bcStoreDir config) failure) (realiseFailureStatus failure))
            Right () ->
              case drvOutputs rootDrv of
                (firstOut : _) -> pure (BuildSuccess (doPath firstOut))
                [] -> pure (BuildFailure noOutputsMessage 1)

-- | Read and parse a .drv file from the store, as bytes (no locale
-- decode, no newline translation).
readDrvFromStore :: BuildConfig -> StorePath -> IO (Either Text Derivation)
readDrvFromStore config sp = do
  let drvFilePath = storePathToFilePath (bcStoreDir config) sp
  content <- try (BS.readFile drvFilePath)
  pure $ case content of
    Left (_ :: IOException) -> Left ("cannot read .drv file: " <> T.pack drvFilePath)
    Right bytes -> fromATerm bytes

-- ---------------------------------------------------------------------------
-- Realisation effects
-- ---------------------------------------------------------------------------

-- | The store, caches and builder that "Nix.Realise" drives.
realiseEffects :: BuildConfig -> Store -> RealiseEffects IO
realiseEffects config store =
  RealiseEffects
    { reSubst =
        SubstEffects
          { seValid = isValid store,
            seReferences = queryReferencesWith (bcFetchRetry config) ioRetryEffects (bcFallback config) (bcCaches config),
            seFetch = substitutePaths config store
          },
      -- Every output, not just the first: a derivation whose first output is
      -- valid and whose second was deleted would otherwise be logged cached
      -- and skipped, and the missing output would stay missing.  Upstream
      -- skips a build only when all of them are valid.
      reAllValid = fmap isJust . allOutputsValid store,
      reBuild = buildReporting config store,
      reCheck = checkDerivation config store,
      reReport = logRealised
    }

-- | Build one derivation, naming it in any failure.
buildReporting :: BuildConfig -> Store -> Derivation -> StorePath -> IO (Either Text ())
buildReporting config store drv drvPath =
  bimap (stopReport drv) (const ()) <$> buildOutputs config store drv drvPath

-- | A stopped build as the realisation reports it, naming the derivation.
stopReport :: Derivation -> BuildStop -> Text
stopReport drv (BuildStop message code) =
  "building " <> formatDrvName drv <> " failed: exit " <> T.pack (show code) <> ": " <> message

-- | Format a status tag for display.
statusTag :: Realised -> Text
statusTag AlreadyValid = "[cached]"
statusTag Substituted = "[subst] "
statusTag Built = "[build] "
statusTag Checked = "[check] "

-- | Log how a dependency was realised to stderr.
logRealised :: Realised -> Derivation -> IO ()
logRealised status drv =
  TIO.hPutStrLn System.IO.stderr ("  " <> statusTag status <> " " <> formatDrvName drv)

-- | Try to substitute paths from binary caches, every one of them or none
-- ('FetchOutcome').
--
-- Registration is all-or-nothing and batched: every path is verified and
-- unpacked first, then recorded in one 'registerPaths' transaction, so
-- reference edges between the outputs of one derivation land after both
-- endpoints' rows exist and a partial substitution registers nothing (the
-- subsequent build starts from unregistered outputs and 'runPlannedBuild'
-- replaces the leftover unpacked trees).
--
-- Each substituted path arrives with its path lock STILL HELD (see
-- 'Nix.Substituter.SubstSuccess'), and the exclusion must survive until
-- the registration transaction commits - released earlier, another
-- process could meet the unpacked-but-unregistered window and delete
-- the tree the row is about to describe.  Every held lock is released
-- here on every exit path (finally), including failures that never
-- reach registration.
--
-- A registration the database REFUSES - its unregistered-referent guard,
-- reachable only through cache-declared references this store has never
-- seen - is a failed substitution like any other: the transaction
-- recorded nothing, so report it ('FetchFailed').  Upstream's copy of the
-- path fails the same way when a reference is not valid.
substitutePaths :: BuildConfig -> Store -> [StorePath] -> IO FetchOutcome
substitutePaths config store paths
  | null (bcCaches config) = pure FetchMissed
  | otherwise = do
      heldLocks <- newIORef []
      registerSubstituted heldLocks
        `finally` (readIORef heldLocks >>= mapM_ releasePathLock)
  where
    registerSubstituted :: IORef [PathLock] -> IO FetchOutcome
    registerSubstituted heldLocks = do
      results <- mapM (substituteOne heldLocks) (lockOrder paths)
      case batchRegistrations results of
        Left outcome -> pure outcome
        Right rows -> do
          registered <- try (registerPaths (stDB store) rows)
          case registered of
            Right () -> pure Fetched
            Left (e :: IOException) -> do
              TIO.hPutStrLn System.IO.stderr ("  [subst] registration refused: " <> T.pack (displayException e))
              pure FetchFailed
    -- Record a returned lock the moment it exists, so the enclosing
    -- finally owns it even when a later path's attempt fails or throws.
    substituteOne heldLocks sp = do
      result <- trySubstituteWith (bcFetchRetry config) ioRetryEffects store (bcCaches config) sp
      case result of
        SubstSuccess _ lock -> atomicModifyIORef' heldLocks (\locks -> (lock : locks, ()))
        SubstError err -> TIO.hPutStrLn System.IO.stderr ("  [subst] " <> spName sp <> ": " <> err)
        _ -> pure ()
      pure result

-- | What one batch's results come to: the rows to register when every path
-- arrived, or why not.  One failed path fails the batch whatever the others
-- did, as one failed output fails upstream's derivation goal.
batchRegistrations :: [SubstResult] -> Either FetchOutcome [PathRegistration]
batchRegistrations results
  | any failed results = Left FetchFailed
  | otherwise = maybe (Left FetchMissed) (Right . catMaybes) (traverse arrived results)
  where
    failed (SubstError _) = True
    failed _ = False
    -- An already-valid path counts as arrived but contributes no
    -- registration row (and holds no lock).
    arrived (SubstSuccess reg _) = Just (Just reg)
    arrived SubstAlreadyValid = Just Nothing
    arrived SubstNotFound = Nothing
    arrived (SubstError _) = Nothing

-- | Format a derivation name for status output (display-only, so the
-- byte-string env value decodes lossily).
formatDrvName :: Derivation -> Text
formatDrvName drv =
  case Map.lookup "name" (drvEnv drv) of
    Just n -> TE.decodeUtf8With lenientDecode n
    Nothing -> case drvOutputs drv of
      (out : _) -> spName (doPath out)
      [] -> "<unknown>"
