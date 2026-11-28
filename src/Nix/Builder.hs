{-# LANGUAGE ScopedTypeVariables #-}

-- | Derivation builder - executes build recipes.
--
-- == The build process
--
-- When Nix needs to build a derivation (cache miss), it:
--
-- 1. Creates a directory for the build under the store's state directory
--    ("Nix.Builder.BuildDir")
-- 2. Sets up the environment: the derivation's variables over the
--    standard ones, with a PATH of the builder's directory, its sibling
--    @usr\/bin@ and the system directories ('buildPath').  A sandboxed
--    build ("Nix.Builder.Sandbox") reaches, of the store, only its input
--    closure, the closure of its @sandbox-paths@, its outputs and its
--    build directory, and the host's network only when it is fixed-output
--    (a macOS one may also ask for local networking); an unsandboxed one
--    reaches the whole filesystem and the network.
-- 3. Runs the derivation's builder with its arguments (for a nixpkgs
--    stdenv, @bash -e@ on scripts that source the stdenv's @setup@)
-- 4. The build script runs @genericBuild@, which calls @unpackPhase@,
--    @patchPhase@, @configurePhase@, @buildPhase@, @installPhase@,
--    @fixupPhase@, etc.
-- 5. Builder writes output to @$out@ (the output store path, or a scratch
--    path for an output already valid; a Linux sandboxed build's is that
--    path beneath its chroot)
-- 6. Nix scans the output for references to other store paths
-- 7. Output is placed in the store and registered ('Nix.Store.placeInStore':
--    a Linux sandboxed build's moves out of its chroot, any other build's
--    is there already)
--
-- == Checking
--
-- Under 'BuildCheck', upstream's @nix-build --check@, 'buildWithDeps'
-- builds its derivation again beside the valid outputs and compares what
-- comes out with them ('checkDerivation').  That rebuild registers nothing
-- and leaves the outputs as they are, and under 'KeepFailed' keeps the
-- rebuild of an output that differs beside it.  What comes before it adds
-- to the store as any build does: an output that is not valid is
-- substituted first, and the inputs are substituted or built.
--
-- == On Windows
--
-- The key difference is process creation.  Upstream on Linux uses
-- @fork\/exec@ with namespace isolation.  nova-nix spawns builders through
-- "System.Process", which reaches @CreateProcess@ on Windows with no POSIX
-- layer, and runs them as the invoking user, without filesystem or network
-- isolation.
--
-- The builder's process tree runs inside a Win32 job object
-- ('Proc.use_process_jobs'), so the build waits until every process in
-- the tree has exited, and an interrupt terminates the whole job instead
-- of orphaning grandchildren.
module Nix.Builder
  ( -- * Build execution
    BuildResult (..),
    BuildStop (..),
    buildDerivation,
    checkDerivation,
    buildWithDeps,
    buildRequests,
    realiseWithoutWriting,
    checkStored,
    substitutePaths,
    substituteFromCaches,

    -- * Build configuration
    BuildConfig (..),
    SubstitutionAccess (..),
    defaultBuildConfig,

    -- * Pure pieces (exported for tests)
    AttrsDelivery (..),
    attrsDelivery,
    BuildPlace (..),
    BuildLayout (..),
    BuilderInvocation (..),
    builderInvocation,
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

import Control.Exception (IOException, bracket, displayException, finally, onException, try, uninterruptibleMask_)
import Control.Monad (filterM, foldM, unless, when)
import Control.Monad.Except (ExceptT (..), liftEither, runExceptT, throwError, withExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (StateT, evalStateT)
import Data.Bifunctor (bimap, first)
import qualified Data.ByteString as BS
import Data.Char (isAscii, isSpace, toLower, toUpper)
import Data.Foldable (for_)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (sortOn, uncons)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe, maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import Data.Word (Word32)
import qualified Network.HTTP.Client as HTTP
import Nix.Builder.BuildDir (BuildEnding (..), FailedBuilds (..), WorkDir (..), sandboxedBuildDir, withBuildDir)
import Nix.Builder.Buildenv (builtinBuildenvBuilder, runBuiltinBuildenv)
import Nix.Builder.Log (openLogPipe, streamBuildLog)
import Nix.Builder.Sandbox (DarwinOptions (..), DarwinPathInputs (..), DarwinPaths (..), DarwinPlan (..), DarwinProfile (..), HelperOutcome (..), Network, PathPresence (..), PlanInputs (..), SandboxBackend (..), SandboxConfig (..), SandboxFallback (..), SandboxPath (..), SandboxUse (..), backendFallback, chrootSkeleton, darwinOptions, darwinParameters, darwinPathsInSandbox, darwinProfile, darwinSetupFailureMessage, darwinStageName, defaultSandboxConfig, helperOutcome, hostSandboxBackend, renderDarwinPlan, renderPlan, rootRefusal, sandboxHome, sandboxPlan, sandboxUse, setupFailureMessage, setupStageName, unavailableMessage)
import Nix.Builder.Sandbox.Spawn (HelperRun (..), bytesFilePath, filePathBytes, processIsRoot, profileFilters, resolvedPath, runHelper, sandboxAvailability, sealChroot, warnFallback, withChroot)
import Nix.Builder.Unpack (UnpackLimits, builtinUnpackBuilder, defaultUnpackLimits, runBuiltinUnpack)
import Nix.DependencyGraph (DepGraph, TopoResult (..), buildDepGraph, buildDepGraphFrom, topoSort)
import Nix.Derivation (Derivation (..), DerivationOutput (..), Platform, currentPlatform, extraPlatforms, fixedOutput, fromATerm, platformToText)
import Nix.Derivation.StructuredAttrs (decodeStructuredAttrs, renderAttrsJson, renderAttrsShell, stringsAttr, structuredAttrsKey, withOutputPlaceholders)
import Nix.Hash (IncrementalHash, bytesToHexText, hashFinalizeBytes, hashInitWithAlgo, hashPlaceholder, hashUpdateChunk, hexToBytes, makeStorePath, rawHashWithAlgo, sha256Digest)
import Nix.Http (AttemptFailure (..), FetchRetryPolicy, attemptFailureMessage, catchSync, defaultFetchRetryPolicy, downloadUrlWith, ioRetryEffects, retryTransient)
import Nix.Logger (Logger (..))
import Nix.Push (narHashMatches)
import Nix.Realise (BuildMode (..), CheckVerdict (..), ClosureOutcome, DifferingRebuild (..), Fallback (..), FetchOutcome (..), Offer (QueryFailed), RealiseEffects (..), RealiseSettings (..), Realised (..), Registration (..), SubstEffects (..), SubstitutesSetting (..), Undelivered (..), checkedOutputPath, noWriteAccessMessage, realise, realiseFailureMessage, realiseFailureStatus, realiseRequests, requestsFailureMessage, requestsFailureStatus, substituteClosure)
import Nix.Store (ArchiveEnd (..), CaseSensitivity, PathInfo (..), PathLock, PathRegistration, Store (..), UpstreamHazard (..), acquirePathLock, canonicalizeMetadata, finishNarUnpack, freshCopyPath, isValid, moveOutput, newNarUnpackSink, placeInStore, queryPathInfo, queryReferences, registerPaths, releasePathLock, replaceWithCopy, rewriteBytes, rewriteStoreHashes, scanReferences, sinkNarStream, upstreamManagedRefusal, volumeCaseHack)
import qualified Nix.Store.ExecBit as ExecBit
import Nix.Store.Metadata (foreignOwnedNode, isPlainFile, writableByOthers)
import Nix.Store.Path (BadStorePath, StoreDir (..), StorePath (spHash, spName), defaultStoreDir, defaultStoreDirText, parseStorePath, parseStorePathPrefix, storePathToFilePath, storePathToText, unStoreDir)
import Nix.Store.Symlink (WalkNode (..), classifyWalkNode)
import Nix.Substituter (CacheConfig, CacheMemo, SubstResult (..), Substitute, emptyCacheMemo, fetchSubstituteWith, narStreamFailure, queryNextOfferWith, queryReferencesWith, substitutePath, withXzAttempt)
import qualified NovaCache.Base32 as Nix32
import qualified NovaCache.Hash as Hash
import qualified NovaCache.NAR as NAR
import qualified NovaCache.Xz as Xz
import System.Directory (removePathForcibly)
import qualified System.Environment
import System.Exit (ExitCode (..))
import System.FilePath (isAbsolute, isPathSeparator, takeDirectory, takeFileName, (</>))
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

-- | Environment variable for the cores a builder may use ('bcCores').
envNixBuildCores :: Text
envNixBuildCores = "NIX_BUILD_CORES"

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

-- | A Unix build's @PATH@ unless the derivation sets one: upstream's
-- value, which names no directory there is, so the builder finds no
-- program its derivation does not name, and a shell does not fall back
-- to a search path of its own as it would were @PATH@ unset.
pathNotSet :: Text
pathNotSet = "/path-not-set"

-- | The system's directories a Windows build's @PATH@ ends with, where
-- @cmd.exe@ and the programs a job needs are.
windowsSystemDirs :: [FilePath]
windowsSystemDirs = ["C:\\Windows\\System32", "C:\\Windows"]

-- | The magic builder string for the built-in URL fetcher.  A derivation with
-- this builder is not executed as a process - the Builder downloads its @url@
-- and verifies it against @outputHash@ (see 'runBuiltinFetchurl').
-- Bytes, matching the 'drvBuilder' field it is compared against.
builtinFetchurlBuilder :: BS.ByteString
builtinFetchurlBuilder = "builtin:fetchurl"

-- | What every builtin builder's name starts with, which is how upstream
-- tells a builtin from a program.
builtinPrefix :: BS.ByteString
builtinPrefix = "builtin:"

-- | Derivation environment keys read by @builtin:fetchurl@.
envUrl, envUrls, envOut, envUnpack, envExecutable :: Text
envUrl = "url"
envUrls = "urls"
envOut = "out"
envUnpack = "unpack"
envExecutable = "executable"

-- | The value a derivation attribute set to @true@ has in the
-- environment, and the one upstream's @builtin:fetchurl@ compares
-- @unpack@ and @executable@ with (@builtinFetchurl@ in fetchurl.cc).
envTrue :: BS.ByteString
envTrue = "1"

-- | The URL suffix that marks the NAR @builtin:fetchurl@ restores as
-- xz-compressed (@builtinFetchurl@ in fetchurl.cc).
xzUrlSuffix :: Text
xzUrlSuffix = ".xz"

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
  { -- | Store directory: where outputs go, and under whose state
    -- directory a build runs ("Nix.Builder.BuildDir").
    bcStoreDir :: !StoreDir,
    -- | Binary caches to try before building (checked in priority order).
    bcCaches :: ![CacheConfig],
    -- | What a failed substitute leads to: upstream's @fallback@.
    bcFallback :: !Fallback,
    -- | Whether a derivation's @allowSubstitutes@ attribute is heeded:
    -- upstream's @always-allow-substitutes@.
    bcSubstitutes :: !SubstitutesSetting,
    -- | Whether a substitute may be fetched and written to the store.
    bcSubstitution :: !SubstitutionAccess,
    -- | What 'buildWithDeps' does with the derivation it is given:
    -- upstream's build mode, which @nix-build --check@ sets.  Nothing
    -- else reads it, and 'buildDerivation' never checks.
    bcMode :: !BuildMode,
    -- | What a builder's @NIX_BUILD_CORES@ says: upstream's @cores@, 0
    -- meaning every core.  Where a derivation's attributes reach the
    -- environment, which they do not under @__structuredAttrs@, an
    -- attribute of that name wins, as upstream sets the variable before it
    -- copies the derivation's environment (@LocalDerivationGoal::initEnv@).
    bcCores :: !Word32,
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
    -- | Whether, and how, a spawned builder runs in a sandbox
    -- ("Nix.Builder.Sandbox").
    bcSandbox :: !SandboxConfig,
    -- | What a build that does not succeed leaves, upstream's
    -- @keep-failed@: its directory ("Nix.Builder.BuildDir"), unless its
    -- builder is a builtin, and for a check that found an output
    -- different, the rebuild of that output ('checkDerivation').
    bcFailedBuilds :: !FailedBuilds
  }
  deriving (Eq, Show)

-- | Whether a build may substitute what it needs.
data SubstitutionAccess
  = -- | It fetches a substitute from 'bcCaches' and registers what
    -- arrives.
    SubstituteFromCaches
  | -- | It refuses each substitution before any cache is asked, as
    -- upstream's substitution goal refuses one in a read-only evaluation
    -- ('Nix.Realise.noWriteAccessMessage').  An output that may not be
    -- substituted is still built, as upstream builds one there: nothing
    -- on its build path checks for read-only mode
    -- (@DerivationGoal::haveDerivation@ in derivation-goal.cc).
    RefuseSubstitution
  deriving (Eq, Show)

-- | Default build configuration.  A builder is told of one core
-- ('baselineCores'), where upstream's default is the CPU limit of the
-- process's cgroup on Linux, otherwise the machine's count, which takes
-- IO to find ("Nix.Builder.Cores"), and runs unsandboxed
-- ('defaultSandboxConfig'), where upstream sandboxes on Linux, since
-- finding the sandbox's helper takes IO too.  A build that fails keeps
-- nothing, as upstream's @keep-failed@ defaults off.
defaultBuildConfig :: StoreDir -> BuildConfig
defaultBuildConfig dir =
  BuildConfig
    { bcStoreDir = dir,
      bcCaches = [],
      bcFallback = NoFallback,
      bcSubstitutes = HeedAllowSubstitutes,
      bcSubstitution = SubstituteFromCaches,
      bcMode = BuildNormal,
      bcCores = baselineCores,
      bcExecWrappers = Map.empty,
      bcUnpackLimits = defaultUnpackLimits,
      bcFetchRetry = defaultFetchRetryPolicy,
      bcSandbox = defaultSandboxConfig,
      bcFailedBuilds = RemoveFailed
    }

-- | The cores 'defaultBuildConfig' tells a builder of: one, which is what
-- nixpkgs' setup.sh takes an unset @NIX_BUILD_CORES@ to mean.
baselineCores :: Word32
baselineCores = 1

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
-- 2. Decide how the builder runs, refusing a sandbox that cannot be had
-- 3. Create the build directory ('withBuildDir')
-- 4. Set up environment from derivation + standard vars
-- 5. Run the builder process, which writes each output at its store path,
--    or at a scratch path for one already valid; a Linux sandboxed build
--    writes them beneath its chroot until they are registered
-- 6. On success: scan the outputs for references and register them,
--    failing on a reference cycle between them ('registerOutputs')
-- 7. On failure: clean up and report error, keeping the build directory
--    under 'KeepFailed' ('failedBuildsFor')
-- 8. Exception safety: synchronous exceptions convert to BuildFailure;
--    asynchronous exceptions propagate ('catchSync') - an interrupt
--    must abort the build, never be reported as a build failure.
--
-- A store upstream Nix's database may also manage fails the build before
-- any of this ("Nix.Store.Upstream").
buildDerivation :: BuildConfig -> Store -> Derivation -> StorePath -> IO BuildResult
buildDerivation config store drv drvPath =
  either stoppedResult BuildSuccess <$> buildOutputs config store drv drvPath

-- | Why a build stopped short of what it was for: the message and exit
-- code a 'BuildFailure' reports.
data BuildStop = BuildStop !Text !Int
  deriving (Eq, Show)

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
    build = buildUnderLock config store drvPath drv RedirectValid (const BuildSucceeded) (registerOutputs store drv drvPath)

-- | Check a derivation, given its @.drv@ path: build it again beside its
-- valid outputs and compare what comes out with what the store holds.
-- This is the build upstream runs under @bmCheck@, without what comes
-- before it: "Nix.Realise" substitutes an output that is not valid and
-- realises the inputs, and 'buildWithDeps' under 'BuildCheck' does both.
--
-- The rebuild registers nothing and leaves the outputs as they are.  It
-- keeps nothing it wrote, but under 'KeepFailed' the rebuild of the first
-- output that differs, which it moves to the output's path with @.check@
-- after it ('checkedOutputPath'), removing what was there, as upstream
-- does.  The verdict says which ('DifferingRebuild').  'Left' is why
-- there was no comparison to answer with, naming the derivation as
-- 'buildWithDeps' names one that failed to build: the rebuild failed (an
-- input missing, its builder failing, or what it wrote refused: an output
-- missing or open to other users, a cycle between outputs, a fixed
-- output's wrong hash); the derivation has no output, or one is not
-- valid, so there is nothing to compare a rebuild with; or the store is
-- one upstream Nix's database may also manage, refused before anything
-- is touched ("Nix.Store.Upstream").
checkDerivation :: BuildConfig -> Store -> Derivation -> StorePath -> IO (Either Text CheckVerdict)
checkDerivation config store drv drvPath =
  first (stopReport drv) <$> checkOutputs config store drv drvPath

-- | 'checkDerivation', under the outputs' path locks, which upstream holds
-- through a check as through any build (@DerivationGoal::tryToBuild@ at
-- 2.24.9).  The builder writes every output to a scratch path
-- ('RedirectAll'), so the store's copies stay as they are, and what it
-- wrote is gone when this returns, but for a rebuild 'KeepFailed' keeps.
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
          buildUnderLock config store drvPath drv RedirectAll verdictEnding (compareOutputs (bcFailedBuilds config) store drv drvPath (map piNarHash registrations))

-- | How a check ended: one that found an output different failed, as
-- upstream's does, so it keeps the build's directory under @keep-failed@
-- as any failure does.
verdictEnding :: CheckVerdict -> BuildEnding
verdictEnding verdict = case verdict of
  OutputsMatch -> BuildSucceeded
  OutputDiffers _ _ -> BuildFailed

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
-- concurrent builds of one derivation would write one output path,
-- interleave their writes into a single tree, and each register it as
-- valid.  Nothing would fail; the registered output would simply not be
-- what either build produced.  The first output's lock is also what
-- keeps a build directory's name to one build at a time
-- ("Nix.Builder.BuildDir").
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
--
-- A store upstream Nix's database may also manage is refused before any
-- lock is taken ("Nix.Store.Upstream"): a build clears each output path
-- with no row here, and a path upstream registered has none.
withOutputLocks :: BuildConfig -> Store -> Derivation -> (Maybe StorePath -> IO (Either BuildStop a)) -> IO (Either BuildStop a)
withOutputLocks config store drv act = do
  refusal <- upstreamManagedRefusal ReplacesUpstreamPaths (bcStoreDir config)
  maybe locked (\reason -> pure (Left (BuildStop reason 1))) refusal
  where
    locked = do
      heldLocks <- newIORef []
      let takeLock sp = do
            lock <- acquirePathLock (stLogger store) (bcStoreDir config) sp
            atomicModifyIORef' heldLocks (\locks -> (lock : locks, ()))
      ( do
          mapM_ takeLock (lockOrder id (map doPath (drvOutputs drv)))
          finishedElsewhere <- allOutputsValid store drv
          act finishedElsewhere
        )
        `finally` (readIORef heldLocks >>= mapM_ releasePathLock)

-- | The order a process takes more than one path lock in.  Substitution
-- holds each lock until its batch registers, and a build until it finishes,
-- so both have to use this order: the blocking lock has no timeout, and two
-- processes taking the same paths in different orders would wait forever.
lockOrder :: (a -> StorePath) -> [a] -> [a]
lockOrder = sortOn

-- | The derivation's first output path when EVERY output is already
-- registered, otherwise 'Nothing'.
--
-- Every output, never just the first: a partially valid multi-output
-- derivation still has to build, and treating it as finished would leave
-- the missing outputs missing.  This is the re-check under the output
-- locks.  A build reaches it once some output a request wanted was not
-- valid, and upstream's re-check there asks about the wanted outputs
-- alone (@DerivationGoal::tryToBuild@ at 2.24.9), so where another
-- process made those valid during the wait, upstream adopts the
-- derivation and this builds it.
allOutputsValid :: Store -> Derivation -> IO (Maybe StorePath)
allOutputsValid store drv = case map doPath (drvOutputs drv) of
  [] -> pure Nothing
  outputPaths@(firstOutput : _) -> do
    validity <- mapM (isValid store) outputPaths
    pure (if and validity then Just firstOutput else Nothing)

-- | What a build does with the outputs its builder wrote, given the
-- closure of its inputs, where each output was written, and where each is
-- on disk once its builder has ended: registers them ('registerOutputs'),
-- or compares them with the store's ('compareOutputs').
type Finish a = Set StorePath -> [OutputPlan] -> Written -> Commit -> IO (Either BuildStop a)

-- | How a build runs the step that registers its outputs: with the record
-- that they are registered, which keeps the cleanup of an interrupted
-- build from removing them, made in the same step, which no asynchronous
-- exception can break into.
type Commit = IO () -> IO ()

-- | What a build's builder left: where each output is on disk once it has
-- ended, at its scratch path in the store, or for a Linux sandboxed build
-- at that path in the chroot, where it stays until it is registered; and
-- whether the builder left an output writable by its group or anyone else
-- ('writableByOthers'), read for a Linux sandboxed build before the
-- chroot's seal changes the mode ('sealChroot').
data Written = Written
  { writtenPath :: OutputPlan -> FilePath,
    writtenOpen :: OutputPlan -> IO Bool
  }

-- | Which outputs the builder is handed a scratch path for
-- ('outputPlan').
data Redirected
  = -- | Those that are valid, which a build has to leave as they are.
    RedirectValid
  | -- | Every one.  A check's outputs are all valid: 'checkOutputs' has
    -- their registrations in hand, under the locks this runs under.
    RedirectAll

-- | The build itself, of the derivation at a @.drv@ path, with every
-- output's lock already held, given how its result says it ended, which
-- decides whether a build directory is kept ("Nix.Builder.BuildDir").
buildUnderLock :: BuildConfig -> Store -> StorePath -> Derivation -> Redirected -> (a -> BuildEnding) -> Finish a -> IO (Either BuildStop a)
buildUnderLock config store drvPath drv redirected succeeded finish = runExceptT $ do
  -- 1. Validate inputs (sources + input-derivation outputs) exist, and
  --    take their closure: what an output may reference besides the
  --    derivation's own outputs.
  inputPaths <- withExceptT stopped (ExceptT (validateInputs config store drv >>= either (pure . Left) (inputClosure store)))
  -- The build directory is named by the first output, whose lock keeps
  -- it to one build at a time.  A derivation with no output has no lock
  -- to name one by, and its build fails whatever its builder does
  -- ('registerOutputs'), so it fails here, before a directory is made or
  -- a builder runs.
  firstOutput <- case drvOutputs drv of
    [] -> throwError (stopped noOutputsMessage)
    output : _ -> pure output
  -- 2. Compute output paths (but do NOT pre-create them - the builder is
  --    responsible for creating $out, $dev, etc.).
  --
  --    $out is the final store path, as upstream's is: a temporary path
  --    recorded by the build (a compiler driver, a libtool archive) would
  --    outlive the directory it named.
  outputValidity <- liftIO $ case redirected of
    RedirectValid -> mapM (isValid store . doPath) (drvOutputs drv)
    RedirectAll -> pure (map (const True) (drvOutputs drv))
  plans <- liftEither (first (\nameErr -> stopped ("output name is not a store path name: " <> T.pack (show nameErr))) (traverse outputPlan (zip (drvOutputs drv) outputValidity)))
  -- 3. Decide how the builder runs, which says where in the build
  --    directory it works, refusing a sandbox that cannot be had before
  --    a directory is made, as upstream refuses one before it makes its
  --    own.
  let context = BuildContext store drvPath inputPaths
  run <- withExceptT stopped (ExceptT (builderRunFor config context drv))
  -- 4. Create the build directory, empty, and remove it however the
  --    build ends, unless it failed and keeps it ('failedBuildsFor').
  ExceptT $
    withBuildDir (stLogger store) (bcStoreDir config) (doPath firstOutput) (runWorkDir run) (failedBuildsFor config drv) (either (const BuildFailed) succeeded) $ \buildDir ->
      runPlannedBuild config context drv run buildDir plans (finish inputPaths plans)
  where
    stopped reason = BuildStop reason 1

-- | What a build that does not succeed leaves of its directory: what
-- 'bcFailedBuilds' says, unless its builder is a builtin, of which
-- upstream keeps nothing, since one might hold privileged files, such as
-- a copy of netrc.  A builtin is a builder named with upstream's prefix
-- ('builtinPrefix'), whether or not nova-nix runs it.
failedBuildsFor :: BuildConfig -> Derivation -> FailedBuilds
failedBuildsFor config drv
  | builtinPrefix `BS.isPrefixOf` drvBuilder drv = RemoveFailed
  | otherwise = bcFailedBuilds config

-- | How a derivation's builder runs, decided before its build directory
-- is made.
data BuilderRun
  = -- | In this process, a builtin, given where each output, by name, is
    -- on disk.
    RunsBuiltin !([(Text, FilePath)] -> IO (Either (Int, Text) ()))
  | -- | Spawned, where 'placementFor' put it.
    RunsSpawned !Placement

-- | Where a spawned builder runs.
data Placement
  = -- | Where its directory and outputs are, with no sandbox.
    PlacedUnsandboxed
  | -- | In the Linux sandbox, through this helper, on this network.
    PlacedInLinuxSandbox !FilePath !Network
  | -- | Under macOS's sandbox profile, through this helper, on this
    -- network.
    PlacedInDarwinSandbox !FilePath !Network

-- | The builtins nova-nix runs in this process, and every other builder
-- spawned where 'placementFor' puts it.  'Left' is why it cannot run.
-- @builtin:fetchurl@ restores a NAR's names under the store volume's case
-- sensitivity.
builderRunFor :: BuildConfig -> BuildContext -> Derivation -> IO (Either Text BuilderRun)
builderRunFor config context drv = case drvBuilder drv of
  b
    | b == builtinFetchurlBuilder -> builtin (runBuiltinFetchurl (buildLogger context) (bcFetchRetry config) (stCaseSensitivity (bxStore context)) drv)
    | b == builtinUnpackBuilder -> builtin (runBuiltinUnpack (bcStoreDir config) (bcUnpackLimits config) drv)
    | b == builtinBuildenvBuilder -> builtin (runBuiltinBuildenv (buildLogger context) (bcStoreDir config) drv)
  _ -> fmap RunsSpawned <$> placementFor config context drv
  where
    builtin = pure . Right . RunsBuiltin

-- | Where in its build directory a builder works: a builder in the Linux
-- sandbox in the directory inside it that the sandbox shows at
-- @sandbox-build-dir@ ('Nix.Builder.BuildDir.sandboxedBuildDir'), as
-- upstream's Linux sandbox does, and every other in the directory itself.
runWorkDir :: BuilderRun -> WorkDir
runWorkDir run = case run of
  RunsSpawned (PlacedInLinuxSandbox _ _) -> WorksInSandboxedBuildDir
  RunsSpawned (PlacedInDarwinSandbox _ _) -> WorksInBuildDir
  RunsSpawned PlacedUnsandboxed -> WorksInBuildDir
  RunsBuiltin _ -> WorksInBuildDir

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
-- outside a Linux chroot (@startBuilder@ in local-derivation-goal.cc at
-- 2.24.9, @startBuild@ in unix-derivation-builder.cc at 2.36.0).
-- nova-nix's sandbox writes one to a scratch path too, where upstream's
-- chroot leaves the output's own path free (@needsHashRewrite@, in
-- local-derivation-goal.cc at 2.24.9 and chroot-derivation-builder.hh at
-- 2.36.0).
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
  = -- | The derivation targets this machine's platform, or one it executes
    -- as well ('extraPlatforms'); spawn it directly.
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

-- | What a build knows besides its configuration and derivation: the
-- store, the @.drv@ path, and the closure of its inputs.
data BuildContext = BuildContext
  { bxStore :: !Store,
    bxDrvPath :: !StorePath,
    bxInputs :: !(Set StorePath)
  }

-- | Where a build reports: its store's logger.
buildLogger :: BuildContext -> Logger
buildLogger = stLogger . bxStore

-- | The build, once every output knows where it is written and where it
-- belongs and the builder where it runs: run the builder, then @finish@
-- with where each output it wrote is now.
runPlannedBuild :: BuildConfig -> BuildContext -> Derivation -> BuilderRun -> FilePath -> [OutputPlan] -> (Written -> Commit -> IO (Either BuildStop a)) -> IO (Either BuildStop a)
runPlannedBuild config context drv run buildDir plans finish = do
  let scratchDir = scratchPathOf config
      outputDirs = [(opName p, scratchDir p) | p <- plans]
      -- Every scratch path is the build's to use: the ones that are final
      -- paths may hold a tree an interrupted run left behind, and the
      -- fallbacks may hold one an interrupted run failed to discard, as
      -- the name beside one may hold a fresh copy one left half made.
      clearScratch = for_ plans $ \p -> removePathForcibly (scratchDir p) *> removePathForcibly (freshCopyPath (scratchDir p))
  clearScratch
  -- The cleanup contract: outputs are written at their store paths, not
  -- in the build directory, so an interrupt or a throw between the
  -- builder starting and registration committing would otherwise strand
  -- an unregistered tree at a real store path.  Once @finish@ has
  -- succeeded there is nothing left to take back, and the flag says so: a
  -- build's registrations are committed, and a check wrote to scratch
  -- paths alone.
  committed <- newIORef False
  let cleanupUnlessCommitted = do
        done <- readIORef committed
        unless done clearScratch
      commit registration = uninterruptibleMask_ (registration *> writeIORef committed True)
      -- 6. Success: validate that the builder created all expected
      --    outputs, wherever they are now.  Outputs may be files or
      --    directories - both are valid (real Nix allows $out to be a
      --    single file, a directory tree, or a symlink).
      afterBuilder written = do
        missing <- filterM (nothingAt . writtenPath written) plans
        case missing of
          _ : _ -> do
            let names = T.intercalate ", " (map opName missing)
            -- The builder may have produced some outputs before omitting
            -- another.  None were registered, so take those orphaned
            -- trees back out of the store.
            clearScratch
            pure (Left (BuildStop ("builder succeeded but outputs missing: " <> names) 1))
          [] -> do
            finished <- finish written commit
            case finished of
              Right _ -> do
                writeIORef committed True
                -- Only the fallbacks go; the registered trees stay.
                -- After a build they are the redirected copies of outputs
                -- that were already valid, which upstream discards
                -- unread.  After a check they are every output the
                -- builder wrote, compared by now and not kept, whatever
                -- the comparison found, but a rebuild that differs, which
                -- under 'KeepFailed' is at its @.check@ path by now.  A
                -- Linux sandboxed build's are still in its chroot, which
                -- goes with them.
                for_ [p | p <- plans, opValid p] (removePathForcibly . scratchDir)
              Left _ -> clearScratch
            pure finished
      inStore = Written scratchDir (writableByOthers . scratchDir)
  ( do
      -- 5. Run the builder.  A builtin builder runs in this process and
      --    writes the outputs where they are on disk; a spawned one runs
      --    where 'placementFor' put it, and what follows it runs while
      --    its outputs are where it left them.
      ran <- case run of
        RunsBuiltin builtin -> builtin outputDirs >>= either (pure . Left . uncurry BuilderRefused) (\() -> Right <$> afterBuilder inStore)
        RunsSpawned placement -> runSpawnedBuilder config context drv placement buildDir plans afterBuilder
      case ran of
        Left failure -> do
          -- Whatever the builder wrote is in the store now, so failure has
          -- to take it back out: an unregistered tree there is invisible to
          -- the database and would only confuse the next run.
          clearScratch
          pure (Left (builderStop failure))
        Right finished -> pure finished
    )
    `onException` cleanupUnlessCommitted

-- | Where the builder writes an output, on disk.
scratchPathOf :: BuildConfig -> OutputPlan -> FilePath
scratchPathOf config p = storePathToFilePath (bcStoreDir config) (opScratch p)

-- | Where a derivation's spawned builder runs: in the sandbox when the
-- settings and the derivation ask for it ('sandboxUse') and this system
-- can make one, otherwise where its directory and outputs are.  A Linux
-- system that cannot make the sandbox runs the build unsandboxed under
-- @sandbox-fallback@, after one warning a process ('warnFallback'), and
-- refuses it otherwise, as upstream does; on macOS it refuses the build
-- whatever @sandbox-fallback@ says ('backendFallback'), as upstream does
-- too.  'Left' is the refusal, in upstream's words ("Nix.Builder.Sandbox").
placementFor :: BuildConfig -> BuildContext -> Derivation -> IO (Either Text Placement)
placementFor config context drv =
  case sandboxUse hostSandboxBackend (scMode sandbox) (bxDrvPath context) drv of
    Left reason -> pure (Left reason)
    Right Unsandboxed -> pure (Right PlacedUnsandboxed)
    Right (Sandboxed network) -> do
      root <- processIsRoot
      -- Before anything is made, and whatever sandbox-fallback says.
      if root
        then pure (Left rootRefusal)
        else do
          available <- sandboxAvailability (scHelper sandbox)
          case (available, backendFallback hostSandboxBackend (scFallback sandbox)) of
            (Right (LinuxBackend, helper), _) -> pure (Right (PlacedInLinuxSandbox helper network))
            (Right (DarwinBackend, helper), _) -> pure (Right (PlacedInDarwinSandbox helper network))
            (Left cause, FallBackUnsandboxed) -> Right PlacedUnsandboxed <$ warnFallback (buildLogger context) cause
            (Left cause, RefuseUnsandboxed) -> pure (Left (unavailableMessage hostSandboxBackend cause))
  where
    sandbox = bcSandbox config

-- | Run a derivation's spawned builder where 'placementFor' put it.  Once
-- the builder has succeeded, @continue@ runs with where its outputs are.
runSpawnedBuilder :: BuildConfig -> BuildContext -> Derivation -> Placement -> FilePath -> [OutputPlan] -> (Written -> IO r) -> IO (Either BuilderFailure r)
runSpawnedBuilder config context drv placement buildDir plans continue = case placement of
  PlacedUnsandboxed -> unsandboxed
  PlacedInLinuxSandbox helper network -> runSandboxedBuild config context drv buildDir plans helper network continue
  PlacedInDarwinSandbox helper network -> runDarwinSandboxedBuild config context drv buildDir plans helper network continue
  where
    -- The builder runs where its directory and outputs are, so it is told
    -- of each where it is on disk.
    layout = BuildLayout (inPlace buildDir) [(opName p, inPlace (scratchPathOf config p)) | p <- plans] buildDir
    unsandboxed = case builderInvocation config drv layout of
      Left reason -> pure (Left (BuilderRefused 1 reason))
      Right invocation -> do
        for_ (invFiles invocation) (uncurry BS.writeFile)
        ran <- runBuilder (buildLogger context) invocation
        traverse (\() -> continue (Written (scratchPathOf config) (writableByOthers . scratchPathOf config))) ran

-- | Run a builder in the sandbox, in a chroot beside its build directory
-- ("Nix.Builder.Sandbox.Spawn") through the helper, then @continue@ with
-- each output where the builder wrote it, in the chroot, which is
-- removed once @continue@ ends: an output leaves the chroot only when it
-- is registered, as upstream's leaves its chroot last (@registerOutputs@ in
-- local-derivation-goal.cc at 2.24.9), so a check or an output that was
-- already valid never enters the store.  The builder sees its build directory
-- at @sandbox-build-dir@ and each output at its own path, which on disk is
-- that path beneath the chroot's root, upstream's @toRealPathChroot@
-- (@registerOutputs@ in local-derivation-goal.cc at 2.24.9).  The chroot is
-- in the store's state directory, so the move into the store is a rename
-- ('Nix.Store.placeInStore').  The directory the builder sees is one inside
-- the build directory, made with it ('sandboxedBuildDir').  An
-- @--exec-wrapper@ launcher is bound into the chroot as a @sandbox-paths@
-- entry naming itself.
runSandboxedBuild :: BuildConfig -> BuildContext -> Derivation -> FilePath -> [OutputPlan] -> FilePath -> Network -> (Written -> IO r) -> IO (Either BuilderFailure r)
runSandboxedBuild config context drv outerBuildDir plans helper network continue = do
  paths <- buildSandboxPaths config drv
  closure <- sandboxPathsClosure config context paths
  case closure of
    Left reason -> pure (Left (BuilderSandboxFailed reason))
    Right closurePaths -> do
      let buildDir = sandboxedBuildDir outerBuildDir
      storeBytes <- filePathBytes (unStoreDir (bcStoreDir config))
      buildDirBytes <- filePathBytes buildDir
      seenBuildDir <- bytesFilePath (scBuildDir sandbox)
      inputBytes <- traverse (filePathBytes . storeFile) (Set.toAscList (bxInputs context))
      closureBytes <- traverse (filePathBytes . storeFile) closurePaths
      -- Each output's final path and the path it is written to are kept
      -- out of the chroot, as upstream keeps every known output path out
      -- (@startBuilder@ in local-derivation-goal.cc at 2.24.9).
      outputBytes <- traverse filePathBytes (Set.toAscList (Set.fromList (concat [[storeFile (opFinal p), scratchPathOf config p] | p <- plans])))
      chrooted <- withChroot (buildLogger context) outerBuildDir (chrootSkeleton storeBytes (scBuildDir sandbox) network paths) $ \root -> do
        rootBytes <- filePathBytes root
        let seenLayout = BuildLayout (BuildPlace seenBuildDir buildDir) [(opName p, BuildPlace (scratchPathOf config p) (root <> scratchPathOf config p)) | p <- plans] sandboxHome
            planInputs environment =
              PlanInputs
                { piRoot = rootBytes,
                  piStoreDir = storeBytes,
                  piBuildDirSeen = scBuildDir sandbox,
                  piBuildDirOnDisk = buildDirBytes,
                  piInputs = inputBytes,
                  piSandboxPaths = paths,
                  piSandboxClosure = closureBytes,
                  piOutputs = outputBytes,
                  piEnvironment = environment,
                  piNetwork = network,
                  piCaFile = scCaFile sandbox,
                  piDevShmSize = scDevShmSize sandbox,
                  piDropGroups = scDropGroups sandbox
                }
        case builderInvocation config drv seenLayout of
          Left reason -> pure (Left (BuilderRefused 1 reason))
          Right invocation -> do
            environment <- builderEnvironment invocation
            case sandboxPlan (planInputs (map TE.encodeUtf8 environment)) of
              Left reason -> pure (Left (BuilderSandboxFailed reason))
              Right plan -> do
                for_ (invFiles invocation) (uncurry BS.writeFile)
                ran <- runSandboxedBuilder (buildLogger context) helper (renderPlan plan) (helperOutcome setupStageName . setupFailureMessage plan) invocation
                case ran of
                  Left failure -> pure (Left failure)
                  Right () -> do
                    -- No process is left in the build's PID namespace;
                    -- one outside it, handed a descriptor by a
                    -- fixed-output builder, may be.  The chroot is closed
                    -- to every other user before anything reads an output
                    -- there ('sealChroot').
                    sealed <- sealChroot root (unStoreDir (bcStoreDir config))
                    case sealed of
                      Left reason -> pure (Left (BuilderSandboxFailed reason))
                      Right open -> Right <$> continue (Written (\p -> root <> scratchPathOf config p) (pure . (`Set.member` open) . takeFileName . scratchPathOf config))
      pure (either (Left . BuilderSandboxFailed) id chrooted)
  where
    sandbox = bcSandbox config
    storeFile = storePathToFilePath (bcStoreDir config)

-- | A sandboxed build's @sandbox-paths@ entries: the setting's, and an
-- @--exec-wrapper@ launcher the build runs through, as an entry naming
-- itself.
buildSandboxPaths :: BuildConfig -> Derivation -> IO [SandboxPath]
buildSandboxPaths config drv = do
  launcherPaths <- case execWrapperFor config drv of
    SpawnThrough launcher -> (\bytes -> [SandboxPath bytes bytes RequiredPath]) <$> filePathBytes launcher
    _ -> pure []
  pure (scPaths (bcSandbox config) ++ launcherPaths)

-- | The closures of the @sandbox-paths@ entries whose source is in the
-- store, which the sandbox holds beside them
-- (@LocalDerivationGoal::startBuilder@ at 2.24.9,
-- @DerivationBuildingGoal::buildLocally@ at 2.36.0).  A source that names no
-- store path adds nothing.
sandboxPathsClosure :: BuildConfig -> BuildContext -> [SandboxPath] -> IO (Either Text [StorePath])
sandboxPathsClosure config context paths = do
  let inStore = [sp | entry <- paths, Right source <- [TE.decodeUtf8' (spSource entry)], Just sp <- [parseStorePathPrefix (bcStoreDir config) source]]
  fmap Set.toAscList <$> inputClosure (bxStore context) inStore

-- | Run a builder under the macOS sandbox's profile, through the helper,
-- then @continue@ with each output where the builder wrote it.  macOS's
-- sandbox has no chroot: the builder sees its build directory and its
-- outputs where they are on disk, as an unsandboxed one does and as
-- upstream's does (@tmpDirInSandbox@ in unix-derivation-builder-impl.hh
-- at 2.36.0), and is told upstream's @HOME@.  The profile
-- ('darwinProfile') names the store directory and the build directory
-- with their links resolved here, since macOS's sandbox matches a file by
-- its resolved path.  A derivation's impure host paths, and the refusals
-- of a path the profile cannot name, are upstream's
-- ('darwinPathsInSandbox', 'profileFilters').
runDarwinSandboxedBuild :: BuildConfig -> BuildContext -> Derivation -> FilePath -> [OutputPlan] -> FilePath -> Network -> (Written -> IO r) -> IO (Either BuilderFailure r)
runDarwinSandboxedBuild config context drv buildDir plans helper network continue = runExceptT $ do
  options <- liftEither (first BuilderSandboxFailed (darwinOptions (bxDrvPath context) drv))
  paths <- liftIO (buildSandboxPaths config drv)
  closurePaths <- ExceptT (first BuilderSandboxFailed <$> sandboxPathsClosure config context paths)
  storeBytes <- liftIO (filePathBytes (unStoreDir (bcStoreDir config)))
  realStore <- liftIO (resolvedPath storeBytes)
  buildBytes <- liftIO (filePathBytes buildDir)
  realBuild <- liftIO (resolvedPath buildBytes)
  realStoreDir <- liftIO (StoreDir <$> bytesFilePath realStore)
  let resolved = filePathBytes . storePathToFilePath realStoreDir
  inputBytes <- liftIO (traverse resolved (Set.toAscList (bxInputs context)))
  closureBytes <- liftIO (traverse resolved closurePaths)
  outputBytes <- liftIO (traverse (resolved . opScratch) plans)
  darwinPaths <-
    liftEither . first BuilderSandboxFailed $
      darwinPathsInSandbox
        DarwinPathInputs
          { dpiDrvPath = bxDrvPath context,
            dpiStoreDir = storeBytes,
            dpiRealStoreDir = realStore,
            dpiBuildDir = buildBytes,
            dpiRealBuildDir = realBuild,
            dpiSandboxPaths = paths,
            dpiSandboxClosure = closureBytes,
            dpiImpureHostDeps = doImpureHostDeps options,
            dpiAllowedImpureHostDeps = scAllowedImpureHostDeps (bcSandbox config),
            dpiInputs = inputBytes
          }
  filters <- ExceptT (first BuilderSandboxFailed <$> profileFilters (dpPaths darwinPaths))
  let layout = BuildLayout (inPlace buildDir) [(opName p, inPlace (scratchPathOf config p)) | p <- plans] sandboxHome
      profile =
        darwinProfile
          DarwinProfile
            { dprViolationLog = scViolationLog (bcSandbox config),
              dprNetwork = network,
              dprStoreDir = realStore,
              dprOutputs = outputBytes,
              dprPaths = filters,
              dprAncestry = dpAncestry darwinPaths,
              dprExtra = doExtraProfile options
            }
  invocation <- liftEither (first (BuilderRefused 1) (builderInvocation config drv layout))
  environment <- liftIO (builderEnvironment invocation)
  let plan = DarwinPlan realBuild profile (darwinParameters realBuild (doLocalNetworking options)) (map TE.encodeUtf8 environment)
  liftIO (for_ (invFiles invocation) (uncurry BS.writeFile))
  ExceptT (runSandboxedBuilder (buildLogger context) helper (renderDarwinPlan plan) (helperOutcome darwinStageName . darwinSetupFailureMessage plan) invocation)
  liftIO (continue (Written (scratchPathOf config) (writableByOthers . scratchPathOf config)))

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
  | -- | The sandbox could not be made, for the reason given, in upstream's
    -- words ("Nix.Builder.Sandbox").
    BuilderSandboxFailed !Text

-- | The stop a failed builder step reports.
builderStop :: BuilderFailure -> BuildStop
builderStop failure = case failure of
  BuilderExited code -> BuildStop "builder failed" code
  BuilderRefused code reason -> BuildStop ("builder failed: " <> reason) code
  BuilderSandboxFailed reason -> BuildStop reason 1

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
-- of the input sources (@DerivationGoal::inputsRealised@ in
-- derivation-goal.cc at 2.24.9, @DerivationBuildingGoal::tryToBuild@ in
-- derivation-building-goal.cc at 2.36.0).  An output that embeds a
-- dependency of a dependency must record it, or a collection deletes it
-- while the output still uses it.  As in upstream's, an input derivation's
-- @.drv@ is in it only when an input source reaches it.
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
-- Environment
-- ---------------------------------------------------------------------------

-- | A path a build uses: where its builder is told it is, and where it is
-- on disk, which is where this process writes and reads it.  The two
-- differ when the builder's view of the filesystem is not this
-- process's.
data BuildPlace = BuildPlace
  { -- | The path as the builder is told of it.
    placeSeen :: !FilePath,
    -- | The path on disk.
    placeOnDisk :: !FilePath
  }
  deriving (Eq, Show)

-- | A path the builder is told of where it is on disk.
inPlace :: FilePath -> BuildPlace
inPlace path = BuildPlace path path

-- | Where a build's directory and its outputs are.
data BuildLayout = BuildLayout
  { -- | The build directory, upstream's @tmpDir@ on disk and
    -- @tmpDirInSandbox@ as the builder sees it
    -- (@initTmpDir@ in local-derivation-goal.cc at 2.24.9,
    -- @tmpDirInSandbox@ in chroot-derivation-builder.cc at 2.36.0).
    layoutBuildDir :: !BuildPlace,
    -- | Each output by name, at the path the builder writes it to.
    layoutOutputs :: ![(Text, BuildPlace)],
    -- | The @HOME@ the builder is told of: its build directory when it
    -- runs unsandboxed, and upstream's @\/homeless-shelter@ in the
    -- sandbox ('Nix.Builder.Sandbox.sandboxHome').
    layoutHome :: !FilePath
  }
  deriving (Eq, Show)

-- | How a spawned builder is started.
data BuilderInvocation = BuilderInvocation
  { -- | The program spawned: the builder, or the launcher that runs it.
    invProgram :: !FilePath,
    -- | Its arguments.
    invArgs :: ![String],
    -- | The build's environment, before the ambient variables a builder
    -- may inherit ('scrubAmbient').
    invEnv :: !(Map Text Text),
    -- | The ambient variables copied into it, the @impureEnvVars@
    -- carve-out.
    invImpureVars :: ![Text],
    -- | The files written before it starts, at their paths on disk.
    invFiles :: ![(FilePath, BS.ByteString)],
    -- | The directory it is started in, on disk.
    invWorkDir :: !FilePath
  }
  deriving (Eq, Show)

-- | How a derivation's builder is started, given where the build's
-- directory and outputs are.  What the builder is told of, its arguments
-- and environment, names the build directory and each output as the
-- builder sees it, as upstream's environment names @tmpDirInSandbox@
-- (@initTmpDir@ and @writeStructuredAttrs@ in local-derivation-goal.cc at
-- 2.24.9, @initEnv@ in unix-derivation-builder.cc and
-- @DesugaredEnv::create@ in derivation-env-desugar.cc at 2.36.0); the
-- files it finds are written, and it is started, where the directory is
-- on disk.
--
-- Derivation strings are bytes, and spawning a process needs text, so
-- invalid UTF-8 in the builder, an argument or a variable refuses the
-- build rather than spawning a mangled one.  The builder and each
-- argument end at their first NUL, as exec takes them ('asExecString').
-- An attribute that reaches the builder as a file stays bytes
-- ('attrsDelivery').
builderInvocation :: BuildConfig -> Derivation -> BuildLayout -> Either Text BuilderInvocation
builderInvocation config drv layout = case (execWrapperFor config drv, decodeBuilderStrings drv) of
  (SpawnUnsupported system, _) ->
    Left ("no way to run a " <> system <> " builder on this machine; pass --exec-wrapper " <> system <> "=PATH to name a launcher for it")
  (_, Left errMsg) -> Left errMsg
  (spawn, Right (builderText, argTexts)) -> do
    let builderPath = T.unpack (asExecString (onStore config builderText))
        builderArgs = map (T.unpack . asExecString . buildRewrite config seenOutputs) argTexts
        -- Env still names the real builder - a launcher only
        -- changes what's spawned, not what the derivation says.
        (spawnPath, spawnArgs) = case spawn of
          SpawnThrough launcher -> (launcher, builderPath : builderArgs)
          _ -> (builderPath, builderArgs)
        -- Only a fixed-output derivation's impureEnvVars reach
        -- the spawn.  Upstream passes them to a derivation whose
        -- type is not sandboxed (initEnv in local-derivation-goal.cc
        -- at 2.24.9, unix-derivation-builder.cc at 2.36.0): a
        -- fixed-output one, or an impure one
        -- (DerivationType::isSandboxed and BasicDerivation::type in
        -- derivations.cc at 2.24.9, derivation::Type::isSandboxed and
        -- derivation::type there at 2.36.0), which takes the
        -- impure-derivations experimental feature nova-nix does
        -- not implement.
        impureVars delivery = case fixedOutput drv of
          Just _ -> adImpureEnvVars delivery
          Nothing -> Right []
    delivery <- attrsDelivery config drv (placeSeen buildDir) seenOutputs
    passed <- impureVars delivery
    Right
      BuilderInvocation
        { invProgram = spawnPath,
          invArgs = spawnArgs,
          invEnv = buildEnvironment config (adEnv delivery) builderPath (placeSeen buildDir) (layoutHome layout),
          invImpureVars = passed,
          invFiles = [(placeOnDisk buildDir </> name, bytes) | (name, bytes) <- adFiles delivery],
          invWorkDir = placeOnDisk buildDir
        }
  where
    buildDir = layoutBuildDir layout
    seenOutputs = [(name, placeSeen place) | (name, place) <- layoutOutputs layout]

-- | Replace each output's @builtins.placeholder@ sentinel with its
-- build-time path (the same value @$out@ carries).  The args, the
-- environment and the files attributes reach the builder as
-- ('attrsDelivery') are rewritten; the builder path is left alone, as
-- upstream Nix leaves it.
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
-- values.  Upstream rewrites the whole @name=value@ string (@runChild@ in
-- local-derivation-goal.cc at 2.24.9, unix-derivation-builder.cc at
-- 2.36.0), so a placeholder in a dynamic attribute name is substituted
-- too; rewriting only the values would hand the builder a variable whose
-- name still held the sentinel.
rewriteEnv :: (Text -> Text) -> Map Text Text -> Map Text Text
rewriteEnv rewrite = Map.fromList . map rewritePair . Map.toList
  where
    rewritePair (name, value) = (rewrite name, rewrite value)

-- | What a builder receives of its derivation beyond the builder path and
-- arguments.
data AttrsDelivery = AttrsDelivery
  { -- | Variables the derivation contributes to the environment.
    adEnv :: !(Map Text Text),
    -- | Files the builder finds in its build directory when it starts,
    -- by name in that directory.
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
--
-- The build directory and the outputs are given where the builder sees
-- them, and the files are named in that directory, so they can be
-- written wherever it is on disk.
attrsDelivery :: BuildConfig -> Derivation -> FilePath -> [(Text, FilePath)] -> Either Text AttrsDelivery
attrsDelivery config drv buildDir outputDirs = case Map.lookup structuredAttrsKey env of
  Nothing -> do
    let (variables, passed) = splitPassAsFile env
        pathVars = Map.fromList [(name <> passedFileVarSuffix, T.pack (buildDir </> passedFileName name)) | name <- Map.keys passed]
    decoded <- Map.traverseWithKey decodeVariable variables
    Right
      AttrsDelivery
        { adEnv = unionEnvs [Map.withoutKeys outputEnv (Map.keysSet passed), rewriteEnv rewrite (unionEnvs [decoded, pathVars])],
          adFiles = [(passedFileName name, passedContents name value) | (name, value) <- Map.toList passed],
          -- Read from the whole environment, as upstream's getStringsAttr
          -- reads it, so passing the attribute as a file does not drop it.
          adImpureEnvVars = T.words <$> decodeVariable envImpureEnvVars (Map.findWithDefault "" envImpureEnvVars env)
        }
  Just encodedBytes -> do
    encoded <- decodeVariable structuredAttrsKey encodedBytes
    attrs <- first (("cannot process " <> structuredAttrsKey <> " attribute of '" <> formatDrvName drv <> "': ") <>) (decodeStructuredAttrs encoded)
    let prepared = withOutputPlaceholders (map fst outputDirs) attrs
    Right
      AttrsDelivery
        { adEnv =
            Map.fromList
              [ (envNixAttrsJsonFile, T.pack (buildDir </> attrsJsonFileName)),
                (envNixAttrsShFile, T.pack (buildDir </> attrsShellFileName))
              ],
          adFiles =
            [ (attrsJsonFileName, renderAttrsJson rewrite prepared),
              (attrsShellFileName, TE.encodeUtf8 (rewrite (renderAttrsShell prepared)))
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
-- ('adEnv') + standard vars, naming the build directory as the builder
-- sees it and its home.  Its PATH is the builder's 'buildPath', which a
-- PATH the derivation sets displaces.
buildEnvironment ::
  BuildConfig ->
  Map Text Text ->
  FilePath ->
  FilePath ->
  FilePath ->
  Map Text Text
buildEnvironment config derivationEnv builderPath buildDir home =
  let -- Standard build variables
      standardEnv =
        Map.fromList
          [ (envNixBuildTop, T.pack buildDir),
            (envTmpDir, T.pack buildDir),
            (envTempDir, T.pack buildDir),
            (envTmp, T.pack buildDir),
            (envTemp, T.pack buildDir),
            (homeEnvVar, T.pack home),
            (envNixStore, T.pack (unStoreDir (bcStoreDir config))),
            (envNixBuildCores, T.pack (show (bcCores config))),
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

-- | Construct the build PATH from the builder's location.  On Unix it is
-- 'pathNotSet', as upstream Nix's is.  On Windows it includes the
-- builder's directory, its sibling @usr\/bin@ (for MSYS2 coreutils
-- bundled with Git for Windows), and 'windowsSystemDirs', which follow it
-- even when the builder's directory is a store path.
--
-- A builder whose directory depends on the working directory, a bare
-- name or a relative path such as @./tools/bash@, adds no directory: the
-- working directory is the build directory, so a file dropped there
-- would resolve ahead of every tool.
buildPath :: FilePath -> Text
buildPath builderPath
  | isWindows = T.intercalate ";" (map T.pack (builderDirs ++ windowsSystemDirs))
  | otherwise = pathNotSet
  where
    builderDir = takeDirectory builderPath
    builderDirs
      | independentOfWorkDir builderDir = [builderDir, takeDirectory builderDir </> "usr" </> "bin"]
      | otherwise = []

-- | Whether a directory names one place whatever the working directory:
-- absolute, or, on Windows, rooted on the working directory's drive.
independentOfWorkDir :: FilePath -> Bool
independentOfWorkDir dir = isAbsolute dir || maybe False (isPathSeparator . fst) (uncons dir)

-- | The home directory environment variable name (platform-dependent).
homeEnvVar :: Text
homeEnvVar =
  if System.Info.os == "mingw32"
    then "USERPROFILE"
    else "HOME"

-- ---------------------------------------------------------------------------
-- Process execution
-- ---------------------------------------------------------------------------

-- | Run the builder process, handing what it writes to the logger a line
-- at a time ('streamBuildLog'), and report how it exited.
--
-- Its standard output and standard error are one pipe ('openLogPipe'),
-- so the log holds both in the order the builder wrote them, as
-- upstream's does.  Its standard input is a pipe this process closes
-- as soon as the builder is spawned, so a read sees end-of-file.
--
-- The child's environment is exactly the build environment plus
-- 'scrubAmbient'\'s allowance - the ambient environment does not flow
-- through ('builderProcessEnv').  It runs unsandboxed, with the
-- filesystem and the network in reach; 'runSandboxedBuilder' runs a
-- sandboxed one.
--
-- 'invImpureVars' is the impureEnvVars carve-out: the names a
-- fixed-output derivation listed (empty otherwise), each copied from
-- the ambient environment - the EMPTY STRING when
-- absent, not omitted, matching upstream's @getEnv(i).value_or("")@.
-- Upstream writes these last in initEnv, so they win even over the
-- derivation's own values.  Names match exactly, as upstream's do.
runBuilder :: Logger -> BuilderInvocation -> IO (Either BuilderFailure ())
runBuilder logger invocation = do
  envList <- builderProcessEnv invocation
  let spawnWith toLog =
        (mkBuilderProcess (invProgram invocation) (invArgs invocation))
          { Proc.cwd = Just (invWorkDir invocation),
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
        streamBuildLog fromBuilder logger
        Proc.waitForProcess process
  pure $ case exitCode of
    ExitSuccess -> Right ()
    ExitFailure code -> Left (BuilderExited code)

-- | A builder's string as exec takes it, a C string: its text up to the
-- first NUL, as upstream's execve takes each argument and each
-- @NAME=VALUE@ entry it builds (stringsToCharPtrs in util.cc at 2.24.9 and
-- 2.34.9, and @runChild@ in local-derivation-goal.cc at 2.24.9).  Cut
-- here, a NUL reaches neither the process library, which refuses one in an
-- environment entry and on Windows would end an entry or the command line
-- at it, nor the sandbox helper's plan.
asExecString :: Text -> Text
asExecString = T.takeWhile (/= '\0')

-- | The environment a builder process gets, as the entries of its exec:
-- the invocation's, the @impureEnvVars@ carve-out over it, and
-- 'scrubAmbient'\'s allowance under it ('runBuilder'), each @NAME=VALUE@
-- ending at its first NUL ('asExecString').
builderEnvironment :: BuilderInvocation -> IO [Text]
builderEnvironment invocation = do
  systemEnv <- System.Environment.getEnvironment
  let systemMap = Map.fromList [(T.pack k, T.pack v) | (k, v) <- systemEnv]
      impureAllowance =
        Map.fromList [(var, Map.findWithDefault "" var systemMap) | var <- invImpureVars invocation]
  -- Impure carve-out wins, then build env, displacing case variants on
  -- Windows; the scrubbed ambient allowance fills underneath.
  pure [asExecString (name <> "=" <> value) | (name, value) <- Map.toList (unionEnvs [impureAllowance, invEnv invocation, scrubAmbient systemMap])]

-- | 'builderEnvironment' as the process library takes one: a name and a
-- value, which it joins with @=@.  An entry with no @=@, which a name
-- holding a NUL leaves, is one it cannot pass, and is left out; upstream
-- passes it, and no getenv finds a variable in it.  The sandbox helper
-- passes every entry as it is.
builderProcessEnv :: BuilderInvocation -> IO [(String, String)]
builderProcessEnv invocation = mapMaybe asPair <$> builderEnvironment invocation
  where
    asPair entry = case T.breakOn "=" entry of
      (name, rest) | Just (_, value) <- T.uncons rest -> Just (T.unpack name, T.unpack value)
      _ -> Nothing

-- | Run a builder through the sandbox helper ('runHelper'), given the
-- logger its lines go to, the plan as the helper reads it, with the
-- builder's environment in it ('builderEnvironment'), and how a run is
-- read for the program it executes ('helperOutcome').
runSandboxedBuilder :: Logger -> FilePath -> BS.ByteString -> (BS.ByteString -> Either Text () -> ExitCode -> BS.ByteString -> HelperOutcome) -> BuilderInvocation -> IO (Either BuilderFailure ())
runSandboxedBuilder logger helper plan outcomeOf invocation = do
  program <- filePathBytes (invProgram invocation)
  run <- runHelper logger helper plan (invWorkDir invocation) (invProgram invocation : invArgs invocation)
  pure $ case outcomeOf program (hrDelivered run) (hrExit run) (hrStatus run) of
    SetupIncomplete reason -> Left (BuilderSandboxFailed reason)
    BuilderRan ExitSuccess -> Right ()
    BuilderRan (ExitFailure code) -> Left (BuilderExited code)

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
-- retry policy, make the output from what each serves, and verify it
-- against the derivation's @outputHash@.  Nix's bootstrap fetcher is
-- baked into the binary because nothing can be fetched before a fetcher
-- exists.  Returns @Either (exit, msg) ()@, which the build reports as a
-- refused builder, so the shared output-registration path is reused
-- unchanged.
--
-- The output is made as upstream's builtin makes it (@builtinFetchurl@ in
-- fetchurl.cc): the download is the output file, or, when @unpack@ is
-- true, a NAR restored as the output ('Unpacking'); then, when
-- @executable@ is true, the output is made executable ('Marking').  The
-- hash it is held to is the one the derivation declares, in the mode it
-- declares whatever made the output, as upstream's @registerOutputs@
-- holds it ('fixedOutputDigest'): a flat hash needs a regular file its
-- owner may not execute and is of its bytes, and a recursive one is of
-- the output's NAR.  @\<nix\/fetchurl.nix\>@ declares a recursive hash
-- for either attribute.  The store's volume decides how a NAR's names
-- are written and read back.
runBuiltinFetchurl :: Logger -> FetchRetryPolicy -> CaseSensitivity -> Derivation -> [(Text, FilePath)] -> IO (Either (Int, Text) ())
runBuiltinFetchurl logger policy sensitivity drv outputDirs =
  case (fetchUrlsFromEnv env, lookup envOut outputDirs, hashedOutput) of
    (Left err, _, _) -> pure (Left (1, "builtin:fetchurl: " <> err))
    (_, Nothing, _) -> pure (Left (1, "builtin:fetchurl: derivation defines no 'out' output"))
    (_, _, Nothing) -> pure (Left (1, "builtin:fetchurl: 'out' output has no fixed-output hash"))
    (Right urls@(firstUrl :| _), Just outPath, Just out)
      -- Invalid specifications reject before any network traffic.
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
        algo = snd (splitHashMode (doHashAlgo out))
  where
    env = drvEnv drv
    unpacking
      | attrIsTrue envUnpack env = RestoreNar
      | otherwise = KeepDownload
    marking
      | attrIsTrue envExecutable env = MarkExecutable
      | otherwise = LeaveMode
    -- builtin:fetchurl derivations are always fixed-output; the expected hash
    -- lives in the canonical output spec (doHashAlgo + doHash), not the env.
    hashedOutput = case drvOutputs drv of
      (out : _) | not (T.null (doHashAlgo out)) -> Just out
      _ -> Nothing
    fetchWithRetry outPath out ctx url =
      first attemptFailureMessage <$> retryTransient policy (ioRetryEffects logger) (fetchAndVerify outPath out ctx url)
    -- Every attempt makes the output from nothing (a restore creates each
    -- node exclusively, so what an earlier attempt left would refuse it)
    -- and hashes from the original context, so a retry never resumes a
    -- torn transfer: upstream resumes from the written offset when the
    -- server accepts ranges and otherwise gives up, which this restart
    -- subsumes.  An output that cannot be marked or that verifies wrong
    -- is the server's bytes, not a hiccup: upstream checks the hash after
    -- the transfer layer has given up retrying, so a mismatch is never
    -- retried.
    fetchAndVerify outPath out ctx url = runExceptT $ do
      liftIO (removePathForcibly outPath)
      writtenDigest <- withExceptT prefixFailure (ExceptT (downloadUrlWith url (makeOutput outPath ctx url)))
      ExceptT (markOutput outPath)
      withExceptT FatalFailure (ExceptT (verifyOutput outPath out url writtenDigest))
    -- Making the output yields the digest of the bytes written when
    -- they are the output file as it was served.
    makeOutput outPath ctx url body = case unpacking of
      KeepDownload -> Right . Just <$> writeBody outPath ctx body
      RestoreNar -> fmap (const Nothing) <$> restoreNarBody sensitivity outPath (narEncodingOf url) body
    -- The root is read without following a link ('Marking').
    markOutput outPath = case marking of
      LeaveMode -> pure (Right ())
      MarkExecutable -> do
        node <- classifyWalkNode outPath
        case node of
          WalkRegular -> Right () <$ ExecBit.markExecutable outPath
          WalkDirectory -> pure (Right ())
          WalkSymlink -> pure (Left (unmarkable outPath "the output is a symlink"))
          WalkAbsent -> pure (Left (unmarkable outPath "the output does not exist"))
    unmarkable outPath reason =
      FatalFailure ("builtin:fetchurl: making '" <> T.pack outPath <> "' executable: " <> reason)
    -- A flat hash of a download written as it was served is the digest
    -- taken as it streamed to disk; any other output is hashed as it now
    -- is on disk.
    verifyOutput outPath out url writtenDigest = case (splitHashMode (doHashAlgo out), writtenDigest) of
      ((False, _), Just digest) -> pure (first snd (verifyFetchedDigest url out digest))
      _ -> do
        digest <- fixedOutputDigest (volumeCaseHack sensitivity) out outPath
        pure (either (Left . ("builtin:fetchurl: " <>)) (first snd . verifyFetchedDigest url out) digest)
    prefixFailure failure = case failure of
      TransientFailure msg -> TransientFailure ("builtin:fetchurl: " <> msg)
      FatalFailure msg -> FatalFailure ("builtin:fetchurl: " <> msg)

-- | What @builtin:fetchurl@ makes its output from: the download as it
-- is, or the NAR the download holds, restored.  Upstream restores when
-- the derivation's @unpack@ is true (@builtinFetchurl@ in fetchurl.cc).
data Unpacking = KeepDownload | RestoreNar

-- | Whether @builtin:fetchurl@ makes its output executable once it is
-- made: upstream does when the derivation's @executable@ is true, with a
-- @chmod@ to @0755@ that the store's canonical metadata then makes
-- @0555@ (@builtinFetchurl@ in fetchurl.cc).  Here a regular file is marked
-- executable ('ExecBit.markExecutable'), on POSIX by its owner's execute
-- bit, which the canonical metadata makes @0555@ alike.  A directory is
-- left as it is: a NAR records no mode for one, and canonical metadata
-- makes every directory @0555@.
--
-- A symlink root fails the URL, and the next is tried.  Upstream's
-- @chmod@ follows the link, in the build's child process: after the
-- @chroot@ when the build is sandboxed, and as the build user when there
-- are build users (@runChild@ in local-derivation-goal.cc at 2.24.9,
-- unix-derivation-builder.cc at 2.36.0).  There a target outside the
-- build is absent or not the build user's to change, so the @chmod@ fails
-- the build; only a build with neither changes the file the link names.
-- nova-nix runs every builtin as the invoking user with neither, so it
-- does not follow the link.
data Marking = LeaveMode | MarkExecutable

-- | How the NAR a download holds is encoded.
data NarEncoding = PlainNar | XzNar

-- | The encoding of the NAR a URL serves: xz when the URL ends in
-- 'xzUrlSuffix', as upstream decides (@builtinFetchurl@ in fetchurl.cc).
-- Upstream asks it of the derivation's @url@ whatever URL it fetches;
-- here it is asked of each URL tried, which for a derivation
-- @\<nix\/fetchurl.nix\>@ writes is that @url@ alone.
narEncodingOf :: Text -> NarEncoding
narEncodingOf url
  | xzUrlSuffix `T.isSuffixOf` url = XzNar
  | otherwise = PlainNar

-- | Whether a derivation attribute is true as upstream's
-- @builtin:fetchurl@ reads one: its value is exactly 'envTrue', and
-- anything else, an absent attribute included, is false.
attrIsTrue :: Text -> Map Text BS.ByteString -> Bool
attrIsTrue key env = Map.lookup key env == Just envTrue

-- | Nova's mirror extension uses the ASCII-whitespace-separated @urls@ field.
-- An absent field falls back to the single @url@ attribute. An explicit
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

-- | Write a response body to the output file as it streams, through the
-- incremental hash chunk by chunk, so memory stays at chunk size no
-- matter the download's size, and return the digest of exactly the
-- written bytes.
writeBody :: FilePath -> IncrementalHash -> HTTP.BodyReader -> IO BS.ByteString
writeBody outPath ctx0 body =
  System.IO.withBinaryFile outPath System.IO.WriteMode $ \handle ->
    let consume !ctx = do
          chunk <- HTTP.brRead body
          if BS.null chunk
            then pure (hashFinalizeBytes ctx)
            else do
              BS.hPut handle chunk
              consume (hashUpdateChunk ctx chunk)
     in consume ctx0

-- | Restore the NAR a response body holds as the output, through the
-- store's streaming restore, as the substituter restores one: names are
-- spelled for the store's volume, and nothing is held whole.  An xz body
-- is decompressed on the way ('fetchurlXzLimits').  The store refusing
-- what the archive holds is fatal, and a body that ends short of a whole
-- archive is transient ('narStreamFailure').  The archive ends at its
-- root node's close and the body after it is left unread, as upstream's
-- restore stops reading the download there.
restoreNarBody :: CaseSensitivity -> FilePath -> NarEncoding -> HTTP.BodyReader -> IO (Either AttemptFailure ())
restoreNarBody sensitivity outPath encoding body = case encoding of
  PlainNar -> restore (HTTP.brRead body)
  XzNar -> withXzAttempt fetchurlXzLimits (HTTP.brRead body) restore
  where
    restore source = do
      sink <- newNarUnpackSink sensitivity outPath
      streamed <- sinkNarStream AtRootClose sink source
      case streamed of
        Left failure -> pure (Left (narStreamFailure failure))
        Right _ -> first FatalFailure <$> finishNarUnpack sink

-- | The bounds on decompressing an @.xz@ download.  The output has none,
-- as upstream's has none: it streams to disk, never held whole, and the
-- hash the output is held to vouches for it.  The decoder's memory has
-- nova-cache's default bound, the hardening the substituter applies,
-- where upstream's decoder, libarchive's, has none.
fetchurlXzLimits :: Xz.XzLimits
fetchurlXzLimits =
  Xz.XzLimits
    { Xz.xzMaxOutputBytes = maxBound,
      Xz.xzMaxDecoderMemoryBytes = Xz.defaultXzDecoderMemoryBytes
    }

-- | Verify a downloaded body, as the flat output file it makes, against
-- the derivation's fixed-output hash, read from the canonical output
-- spec.  @doHashAlgo@ is @sha256@/@sha512@/... (or @r:sha256@ for
-- recursive); @doHash@ is the base-16 digest eval normalized the user's
-- hash into.  A recursive hash is of the output's NAR, which the body
-- alone does not settle (the builtin may restore it, or mark it
-- executable), so it is refused rather than compared with the body's
-- flat digest.
verifyFetchHash :: Text -> DerivationOutput -> BS.ByteString -> Either (Int, Text) ()
verifyFetchHash url out body
  | recursive = Left (1, "builtin:fetchurl: a recursive hash is of the output's NAR, not of the downloaded bytes")
  | otherwise = case (hexToBytes (doHash out), rawHashWithAlgo algo body) of
      (Nothing, _) -> Left (1, malformedExpectedHash url)
      (_, Nothing) -> Left (1, "builtin:fetchurl: unsupported hash algorithm '" <> algo <> "'")
      (Just _, Just got) -> verifyFetchedDigest url out got
  where
    (recursive, algo) = splitHashMode (doHashAlgo out)

-- | Compare a digest of the output, in its declared algorithm and mode,
-- with the derivation's fixed-output hash, naming the URL it came from:
-- the digest of the bytes as they were written, or of the output on
-- disk ('fixedOutputDigest').  'verifyFetchHash' is its twin over a
-- buffered body.
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

malformedExpectedHash :: Text -> Text
malformedExpectedHash url = "builtin:fetchurl: malformed expected hash for " <> url

-- ---------------------------------------------------------------------------
-- Output registration
-- ---------------------------------------------------------------------------

-- | After a successful build, register every output in the store, in
-- upstream's two passes (registerOutputs in local-derivation-goal.cc at
-- 2.24.9, derivation-builder-impl.cc at 2.36.0).  Every output is scanned
-- first.  Then the outputs are ordered so that each follows the siblings it
-- references ('orderByReferences'), and placed in that order.  Outputs that
-- reference each other in a cycle fail the build before any is placed, so
-- none is registered.
--
-- The placed outputs register together in one transaction, so a
-- cross-output reference finds its referent's row (see 'registerPaths').
registerOutputs ::
  Store ->
  Derivation ->
  StorePath ->
  Set StorePath ->
  [OutputPlan] ->
  Written ->
  Commit ->
  IO (Either BuildStop StorePath)
registerOutputs store drv drvPath inputPaths plans written commit = runExceptT $ do
  scanned <- withExceptT scanStop (traverse (scanOutput store scan written) (zip (drvOutputs drv) plans))
  ordered <- liftEither (orderScanned drvPath (siblingsReferenced (drvOutputs drv)) scanned)
  regs <- withExceptT scanStop (traverse (ExceptT . placeOutput store drvPath deriver . snd) ordered)
  liftIO (commit (registerPaths (stDB store) (catMaybes regs)))
  case drvOutputs drv of
    (firstOut : _) -> pure (doPath firstOut)
    [] -> throwError (BuildStop noOutputsMessage 1)
  where
    scan = outputScan drv inputPaths plans
    -- Every output names the @.drv@ that built it, as upstream's
    -- @newInfo.deriver = drvPath@ does (@registerOutputs@ in
    -- local-derivation-goal.cc at 2.24.9, derivation-builder-impl.cc at
    -- 2.36.0), spelled as the store's own rows spell a path, the form a
    -- substituted path's deriver takes ('Nix.Substituter.parseDeriver').
    deriver = Just (T.pack (storePathToFilePath (stDir store) drvPath))

-- | After a successful rebuild under 'BuildCheck', compare every output
-- with the NAR hash the store recorded for it, the hashes in the order of
-- the derivation's outputs: upstream's @registerOutputs@ under @bmCheck@
-- (local-derivation-goal.cc at 2.24.9).  Every output is scanned and the
-- outputs ordered as a build's are, so a reference cycle between them
-- fails a check as it fails a build (its @wanted@ flag and its @topoSort@
-- of the outputs).  Each is then compared in that order, an output after
-- the siblings it references, and the first that differs ends the
-- comparison, where upstream's loop throws (its @bmCheck@ branch).
--
-- Nothing is registered and no valid output is touched.  The rebuild of
-- the output that differs is discarded with the rest, or under
-- 'KeepFailed' moved to the output's path with @.check@ after it
-- ('checkedOutputPath'), as upstream moves it under @keep-failed@:
-- rewritten and canonical, as it was compared, unregistered, and
-- replacing whatever was at that path.  Upstream marks a path it has
-- checked as ultimately trusted and signs it; the store database here
-- records neither.
compareOutputs ::
  FailedBuilds ->
  Store ->
  Derivation ->
  StorePath ->
  [Text] ->
  Set StorePath ->
  [OutputPlan] ->
  Written ->
  Commit ->
  IO (Either BuildStop CheckVerdict)
compareOutputs failedBuilds store drv drvPath recordedHashes inputPaths plans written _ = runExceptT $ do
  built <- withExceptT scanStop (traverse rebuilt (zip3 (drvOutputs drv) plans recordedHashes))
  ordered <- liftEither (orderScanned drvPath (siblings . boReferences . fst) built)
  differing <- ExceptT (firstDifference (volumeCaseHack (stCaseSensitivity store)) drvPath (map snd ordered))
  liftIO (maybe (pure OutputsMatch) (differingVerdict failedBuilds store) differing)
  where
    scan = outputScan drv inputPaths plans
    siblings = siblingNames (drvOutputs drv)
    rebuilt (output, plan, recorded) = do
      outDir <- writtenOutput (writtenPath written plan)
      open <- liftIO (writtenOpen written plan)
      scanned <- preparedOutput scan output outDir open
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
-- (@registerOutputs@ in local-derivation-goal.cc at 2.24.9,
-- derivation-builder-impl.cc at 2.36.0), naming the @.drv@ and the
-- reference that closes the cycle.
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
    -- (@registerOutputs@ in local-derivation-goal.cc at 2.24.9,
    -- derivation-builder-impl.cc at 2.36.0).
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

-- | Where the builder's output is now, which has to be there (a file, a
-- directory, or a symlink, whatever it names).
writtenOutput :: FilePath -> ExceptT Text IO FilePath
writtenOutput outDir = do
  absent <- liftIO (nothingAt outDir)
  when absent (throwError ("output missing: " <> T.pack outDir))
  pure outDir

-- | Whether nothing is at a path, read without following a link, as
-- upstream reads an output (@maybeLstat@ in @registerOutputs@,
-- local-derivation-goal.cc at 2.24.9, derivation-builder-impl.cc at
-- 2.36.0): one that is a symlink is there whatever it names, a sibling
-- output still in the chroot, or nothing at all.
nothingAt :: FilePath -> IO Bool
nothingAt path = do
  node <- classifyWalkNode path
  pure $ case node of
    WalkAbsent -> True
    WalkRegular -> False
    WalkDirectory -> False
    WalkSymlink -> False

-- | Scan one output of a build, keyed by its name: skip it if it is
-- already valid in the DB, otherwise make ready what the builder wrote
-- ('preparedOutput').
scanOutput ::
  Store ->
  OutputScan ->
  Written ->
  (DerivationOutput, OutputPlan) ->
  ExceptT Text IO (Text, ScannedOutput)
scanOutput store scan written (output, plan) = do
  outDir <- writtenOutput (writtenPath written plan)
  -- Validity is a DB fact, not mere disk presence: an output left on disk
  -- by an interrupted run is NOT valid, and build outputs are not
  -- content-addressed, so a leftover cannot be verified against its path.
  -- 'runPlannedBuild' removed any leftover before the builder ran, so the
  -- tree here is this build's (upstream's delete-then-move).
  valid <- liftIO (isValid store (doPath output))
  scanned <-
    if valid
      then pure AlreadyRegistered
      else do
        open <- liftIO (writtenOpen written plan)
        NeedsRegistering <$> preparedOutput scan output outDir open
  pure (doName output, scanned)

-- | An output the builder wrote, made ready where it is to be placed or
-- compared, in upstream Nix's order: one that lets its group or anyone
-- else write it is refused, since someone other than the build may have
-- changed it, and so is one holding a node another
-- user owns; it is made canonical, so nothing in it is setuid or writable by
-- another user, before it is read or changed; a fixed output is replaced with
-- a fresh copy of itself, made canonical again, so a descriptor the builder
-- passed out names an inode that is not registered; and its hashes are
-- rewritten and its references scanned ('scanBuilt').  Whether others could
-- write the output is read from what the builder left, before a Linux
-- sandboxed build's seal changed its mode.  Its chroot is closed to
-- other users before this ('Nix.Builder.Sandbox.Spawn.sealChroot'); a process
-- of the same user can still change an output while it is read.
preparedOutput :: OutputScan -> DerivationOutput -> FilePath -> Bool -> ExceptT Text IO BuiltOutput
preparedOutput scan output outDir open = do
  when open (throwError ("suspicious ownership or permission on '" <> T.pack outDir <> "' for output '" <> doName output <> "'; rejecting this build output"))
  othersNode <- liftIO (foreignOwnedNode outDir)
  for_ othersNode (\node -> throwError ("invalid ownership on file '" <> T.pack node <> "'"))
  liftIO (canonicalizeMetadata outDir)
  -- The copy is made canonical in its turn, and it is the copy that is
  -- read: its inodes are new, so no descriptor another user holds
  -- reaches what is scanned, hashed and moved into the store.
  unless (T.null (doHashAlgo output)) (liftIO (replaceWithCopy outDir *> canonicalizeMetadata outDir))
  liftIO (scanBuilt scan output outDir)

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
-- it, in order, up to the first that differs, which is answered, or
-- cannot be compared.
firstDifference :: NAR.CaseHack -> StorePath -> [(BuiltOutput, Text)] -> IO (Either BuildStop (Maybe BuiltOutput))
firstDifference _ _ [] = pure (Right Nothing)
firstDifference caseHack drvPath (rebuilt : rest) = do
  compared <- compareOutput caseHack drvPath rebuilt
  case compared of
    Right Nothing -> firstDifference caseHack drvPath rest
    ended -> pure ended

-- | The verdict on a rebuilt output that differs from the valid one, its
-- rebuild kept or discarded as 'compareOutputs' says.
differingVerdict :: FailedBuilds -> Store -> BuiltOutput -> IO CheckVerdict
differingVerdict failedBuilds store built =
  OutputDiffers output <$> case failedBuilds of
    RemoveFailed -> pure RebuildDiscarded
    KeepFailed -> RebuildKept <$ (removePathForcibly kept *> moveOutput (boDir built) kept)
  where
    output = doPath (boOutput built)
    kept = checkedOutputPath (stDir store) output

-- | Compare one rebuilt output with the NAR hash the store recorded for
-- the valid one, which is all upstream compares (@newInfo.narHash !=
-- oldInfo.narHash@ in @registerOutputs@, local-derivation-goal.cc at
-- 2.24.9, derivation-builder-impl.cc at 2.36.0).  The rebuilt tree's hash
-- is 'ExecBit.narHashOfPath', which streams the archive that registration
-- serialises whole ('Nix.Store.registrationFor'), under the same case-hack
-- mode and source of the executable bit.
--
-- A fixed output is held to its declared hash first, as a build holds it:
-- upstream raises that mismatch for a checked fixed output and never
-- reaches the comparison (the @CAFixed@ case, the @bmCheck@ branch and the
-- final rethrow of @delayedException@ in @registerOutputs@ at 2.24.9).
-- 'Right' answers the output when it differs.
compareOutput :: NAR.CaseHack -> StorePath -> (BuiltOutput, Text) -> IO (Either BuildStop (Maybe BuiltOutput))
compareOutput caseHack drvPath (built, recorded) = do
  fixedCheck <- verifyFixedOutput caseHack drvPath (boOutput built) (boReferences built) (boDir built)
  case fixedCheck of
    Left err -> pure (Left (scanStop err))
    Right () -> do
      rebuilt <- Hash.formatNixHash <$> ExecBit.narHashOfPath caseHack (boDir built)
      pure (Right (if narHashMatches recorded rebuilt then Nothing else Just built))

-- | Place a scanned output in the store and check a fixed output's
-- content, returning its 'PathRegistration'; @Nothing@ for an output
-- registered before the build.
placeOutput :: Store -> StorePath -> Maybe Text -> ScannedOutput -> IO (Either Text (Maybe PathRegistration))
placeOutput store drvPath deriver scanned = case scanned of
  AlreadyRegistered -> pure (Right Nothing)
  NeedsRegistering (BuiltOutput output outDir refs) -> do
    -- An unsandboxed or macOS sandboxed builder wrote straight into the
    -- store path, so there outDir IS the target and placing it moves
    -- nothing; a Linux sandboxed one's moves out of the chroot here.
    let targetPath = storePathToFilePath (stDir store) (doPath output)
    reg <- placeInStore store outDir (doPath output) deriver refs
    fixedCheck <- verifyFixedOutput (volumeCaseHack (stCaseSensitivity store)) drvPath output refs targetPath
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
-- (local-derivation-goal.cc at 2.24.9, derivation-builder-impl.cc at
-- 2.36.0).
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
-- registerOutputs).  Flat mode hashes the bytes of an output that must
-- be a regular file its owner may not execute; recursive (@r:@) mode
-- hashes the canonical NAR of the placed tree ('fixedOutputDigest').
-- Non-fixed outputs (empty @doHashAlgo@) pass unchecked.
--
-- A fixed output that references any store path, itself included, is
-- refused once its digest is taken, in upstream's order: an output its
-- hash mode cannot take fails first, and a reference before a hash that
-- does not match (the @CAFixed@ case of @registerOutputs@ in
-- local-derivation-goal.cc at 2.24.9).  Its path comes from its content
-- alone, which a reference would leave incomplete.  Upstream registers such
-- an output under the path its references give it before it fails the build;
-- nova-nix registers nothing, as for a hash that does not match.
verifyFixedOutput :: NAR.CaseHack -> StorePath -> DerivationOutput -> [StorePath] -> FilePath -> IO (Either Text ())
verifyFixedOutput caseHack drvPath out refs placedPath
  | T.null (doHashAlgo out) = pure (Right ())
  | otherwise = (>>= unreferenced) <$> fixedOutputDigest caseHack out placedPath
  where
    unreferenced got
      | null refs = matches got
      | otherwise = Left ("illegal path references in fixed-output derivation '" <> storePathToText defaultStoreDir drvPath <> "'")
    subject = fixedOutputSubject out
    matches got = case hexToBytes (doHash out) of
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

-- | A fixed output's digest on disk, in its declared algorithm and mode:
-- of a flat output's file bytes, or of a recursive output's NAR, read
-- back under the store volume's case-hack mode and through the exec-bit
-- source of truth registration reads ('ExecBit.withNarSource').  Both
-- stream.  The NAR is serialised again rather than taken from the
-- registration, whose NAR hash is always sha256 where the declared
-- algorithm may be any.
--
-- A flat output must first be a regular file its owner may not execute,
-- read without following a symlink ('isPlainFile'), or it fails with
-- upstream's words for any other (@newInfoFromCA@ in @registerOutputs@,
-- local-derivation-goal.cc at 2.24.9, derivation-builder-impl.cc at 2.36.0):
-- a directory, a symlink whose target's bytes would otherwise be hashed, or
-- an executable file, whose mode a flat hash does not record.
fixedOutputDigest :: NAR.CaseHack -> DerivationOutput -> FilePath -> IO (Either Text BS.ByteString)
fixedOutputDigest caseHack out path
  | recursive = maybe unsupported Right <$> narDigestWithAlgo caseHack algo path
  | otherwise = do
      plain <- isPlainFile path
      if plain
        then maybe unsupported Right <$> hashFileWithAlgo algo path
        else
          pure
            ( Left
                ( "output path '"
                    <> T.pack path
                    <> "' should be a non-executable regular file since recursive hashing is not enabled (one of outputHashMode={flat,text} is true)"
                )
            )
  where
    (recursive, algo) = splitHashMode (doHashAlgo out)
    subject = fixedOutputSubject out
    unsupported = Left (subject <> ": unsupported hash algorithm '" <> algo <> "'")

-- | How a fixed output's check names it.
fixedOutputSubject :: DerivationOutput -> Text
fixedOutputSubject out = "fixed-output '" <> spName (doPath out) <> "'"

-- | Stream a file through the incremental hash without materializing it
-- (a fixed-output file is input-sized).  'Nothing' for an unsupported
-- algorithm.
hashFileWithAlgo :: Text -> FilePath -> IO (Maybe BS.ByteString)
hashFileWithAlgo algo path = case hashInitWithAlgo algo of
  Nothing -> pure Nothing
  Just initialCtx -> System.IO.withBinaryFile path System.IO.ReadMode $ \handle ->
    Just <$> digestSource initialCtx (BS.hGet handle fixedOutputHashChunk)

-- | Stream a path's NAR through the incremental hash, the exec-bit
-- source of truth answering each file's flag.  'Nothing' for an
-- unsupported algorithm.
narDigestWithAlgo :: NAR.CaseHack -> Text -> FilePath -> IO (Maybe BS.ByteString)
narDigestWithAlgo caseHack algo path = case hashInitWithAlgo algo of
  Nothing -> pure Nothing
  Just initialCtx -> Just <$> ExecBit.withNarSource caseHack path (digestSource initialCtx)

-- | Fold a chunk source through the incremental hash up to its first
-- empty chunk, which ends it.
digestSource :: IncrementalHash -> IO BS.ByteString -> IO BS.ByteString
digestSource initialCtx pull = consume initialCtx
  where
    consume !ctx = do
      chunk <- pull
      if BS.null chunk
        then pure (hashFinalizeBytes ctx)
        else consume (hashUpdateChunk ctx chunk)

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
--    derivation needs only its runtime closure.  The root's outputs are
--    all wanted, and of each input only those its dependent's @.drv@
--    names.  What no cache supplies builds locally, leaves first, and so
--    does what a cache fails to deliver when 'bcFallback' allows it, and
--    a derivation that forbids substitutes unless 'bcSubstitutes'
--    overrides it.
--
-- Under 'BuildCheck' ('bcMode') the root is not realised but checked: an
-- output of its that is not valid is substituted, its inputs are realised
-- as above, and it is built again beside its valid outputs and compared
-- with them ('checkDerivation').
--
-- Returns 'BuildSuccess' with the root output path on success, or
-- 'BuildFailure' with the status the build exits with
-- ('realiseFailureStatus'): 104 if a check found an output different, and
-- 1 for any other failure: a derivation fails to build, a substitute fails
-- under 'NoFallback', a cache cannot be asked, a derivation's attributes
-- cannot be read, a check has no valid output to compare with, a
-- dependent asks for an output a derivation does not have, or the graph
-- cannot be read or holds a cycle.
--
-- A store upstream Nix's database may also manage is refused, with
-- status 1, before anything is read or any cache asked
-- ("Nix.Store.Upstream").  Each build and substitution refuses it too,
-- but by then the realisation has asked the caches, and it reports a
-- refused substitute as a failed download.
buildWithDeps :: BuildConfig -> Store -> Derivation -> StorePath -> IO BuildResult
buildWithDeps config store rootDrv rootDrvPath = do
  refusal <- upstreamManagedRefusal ReplacesUpstreamPaths (bcStoreDir config)
  maybe realiseRoot (\reason -> pure (BuildFailure reason 1)) refusal
  where
    realiseRoot = do
      graph <- acyclicGraph <$> buildDepGraph (readDrvFromStore config) rootDrv rootDrvPath
      either (pure . stoppedResult) realiseGraph graph
    realiseGraph depGraph = do
      result <- evalStateT (realise (realiseSettings config) (bcMode config) (realiseEffects config store) depGraph rootDrvPath) emptyCacheMemo
      case result of
        Left failure -> pure (BuildFailure (realiseFailureMessage (bcStoreDir config) failure) (realiseFailureStatus failure))
        Right () ->
          case drvOutputs rootDrv of
            (firstOut : _) -> pure (BuildSuccess (doPath firstOut))
            [] -> pure (BuildFailure noOutputsMessage 1)

-- | Realise several derivations the store holds, each for the outputs a
-- request names ("Nix.Realise"'s 'realiseRequests'): each requested
-- @.drv@ and every input it reaches is read from the store, a cycle among
-- them is refused, and the requests are realised with one record of what
-- is realised.  They are realised and never checked, whatever 'bcMode'
-- says; 'checkStored' checks a derivation.  It stops with the message and
-- status the build reports, which for one request for every output of a
-- derivation are those 'buildWithDeps' gives the same failure.
--
-- A store upstream Nix's database may also manage is refused, with
-- status 1, before anything is read or any cache asked
-- ("Nix.Store.Upstream").
buildRequests :: BuildConfig -> Store -> [(StorePath, Set Text)] -> IO (Either BuildStop ())
buildRequests config store requests = do
  refusal <- upstreamManagedRefusal ReplacesUpstreamPaths (bcStoreDir config)
  maybe (realiseRequestsWith config (realiseEffects config store) requests) (\reason -> pure (Left (BuildStop reason 1))) refusal

-- | Realise requests as 'buildRequests' does where nothing may be written:
-- in a store upstream Nix's database also manages, for an evaluation that
-- runs read-only.  A path is valid as the answer given says, every
-- substitution is refused before a cache is asked, as upstream's
-- read-only evaluation refuses it ('Nix.Realise.noWriteAccessMessage'),
-- and an output that has to be built is refused for the reason given.
-- Nothing is written, and nothing is opened but the @.drv@ files it reads
-- and what the validity answer reads, so a refusal of a substitution comes
-- before the refusal of the store.
realiseWithoutWriting :: Logger -> BuildConfig -> (StorePath -> IO Bool) -> Text -> [(StorePath, Set Text)] -> IO (Either BuildStop ())
realiseWithoutWriting logger config valid reason = realiseRequestsWith config effects
  where
    refused = pure . QueryFailed . noWriteAccessMessage
    effects =
      RealiseEffects
        { reSubst =
            SubstEffects
              { seValid = liftIO . valid,
                seReferences = refused,
                seNextOffer = const . refused,
                seFetch = \_ batch -> pure (NotFetched (fmap (\(path, _) -> (path, DeliveryFailed)) batch))
              },
          reBuild = \_ _ _ -> pure (Left reason),
          reCheck = \_ _ -> pure (Left reason),
          reReport = \status drv -> liftIO (logRealised logger status drv)
        }

-- | Read each requested @.drv@ and every input it reaches from the store,
-- refuse a cycle among them, and realise the requests over the effects
-- given, with the message and status a build that stops reports.
realiseRequestsWith :: BuildConfig -> RealiseEffects Substitute Realising -> [(StorePath, Set Text)] -> IO (Either BuildStop ())
realiseRequestsWith config effects requests = do
  graph <- acyclicGraph <$> buildDepGraphFrom (readDrvFromStore config) (map fst requests)
  either (pure . Left) realiseIn graph
  where
    realiseIn depGraph = do
      result <- evalStateT (realiseRequests (realiseSettings config) effects depGraph requests) emptyCacheMemo
      pure (first (\failure -> BuildStop (requestsFailureMessage (bcStoreDir config) failure) (requestsFailureStatus failure)) result)

-- | Check one derivation the store holds, every output of it, as
-- 'buildWithDeps' does under 'BuildCheck', whatever 'bcMode' says: its
-- @.drv@ is read from the store, and a check that passes answers
-- @Right ()@.  A store upstream Nix's database may also manage is
-- refused, with status 1, before anything is read ("Nix.Store.Upstream").
checkStored :: BuildConfig -> Store -> StorePath -> IO (Either BuildStop ())
checkStored config store drvPath = do
  refusal <- upstreamManagedRefusal ReplacesUpstreamPaths (bcStoreDir config)
  maybe (readDrvFromStore config drvPath >>= either (pure . Left . unreadableGraph) check) (\reason -> pure (Left (BuildStop reason 1))) refusal
  where
    check drv = stopOf <$> buildWithDeps config {bcMode = BuildCheck} store drv drvPath
    stopOf (BuildSuccess _) = Right ()
    stopOf (BuildFailure message code) = Left (BuildStop message code)

-- | A graph that was read and holds no cycle, or the stop a build reports
-- for it.
acyclicGraph :: Either Text DepGraph -> Either BuildStop DepGraph
acyclicGraph (Left err) = Left (unreadableGraph err)
acyclicGraph (Right graph) = case topoSort graph of
  TopoCycle _ -> Left (BuildStop "dependency cycle detected" 1)
  TopoSorted _ -> Right graph

-- | The stop a build reports for a @.drv@ it cannot read.
unreadableGraph :: Text -> BuildStop
unreadableGraph err = BuildStop ("dependency graph error: " <> err) 1

-- | The realisation settings a build configuration gives.
realiseSettings :: BuildConfig -> RealiseSettings
realiseSettings config = RealiseSettings {rsFallback = bcFallback config, rsSubstitutes = bcSubstitutes config}

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

-- | A realisation's effects, which carry a t'CacheMemo' through the run,
-- so each cache is opened once and asked about each path once.
type Realising = StateT CacheMemo IO

-- | The store, caches and builder that "Nix.Realise" drives.
realiseEffects :: BuildConfig -> Store -> RealiseEffects Substitute Realising
realiseEffects config store =
  RealiseEffects
    { reSubst = substEffects config store,
      reBuild = \drv drvPath _ -> liftIO (buildReporting config store drv drvPath),
      reCheck = \drv drvPath -> liftIO (checkDerivation config store drv drvPath),
      reReport = \status drv -> liftIO (logRealised (stLogger store) status drv)
    }

-- | The store and caches a substitution uses: 'bcCaches' asked under
-- 'bcFallback' and 'bcFetchRetry', and what arrives registered in the
-- store ('substitutePaths').  Under 'RefuseSubstitution' the first path
-- asked about is refused instead, a failed query, which ends the call
-- whatever 'bcFallback' says, as upstream's refusal is thrown before
-- @fallback@ is read.  A t'Substitute' names the path it was offered
-- for, which is the path "Nix.Realise" pairs it with.
substEffects :: BuildConfig -> Store -> SubstEffects Substitute Realising
substEffects config store =
  SubstEffects
    { seValid = liftIO . isValid store,
      seReferences = case bcSubstitution config of
        SubstituteFromCaches -> queryReferencesWith (stLogger store) (bcFetchRetry config) (ioRetryEffects (stLogger store)) (bcFallback config) (bcCaches config)
        RefuseSubstitution -> pure . QueryFailed . noWriteAccessMessage,
      seNextOffer = const (queryNextOfferWith (stLogger store) (bcFetchRetry config) (ioRetryEffects (stLogger store)) (bcFallback config)),
      seFetch = \registration -> liftIO . substitutePaths config store registration . fmap snd
    }

-- | Substitute paths from 'bcCaches' with everything they reference,
-- references first ('substituteClosure'), as a build substitutes a
-- derivation's outputs, and build nothing.  The paths are one batch: when
-- no cache holds one of them, or one references a path no cache holds,
-- none of them is substituted, though a reference that arrived first
-- stays.  A store
-- upstream Nix's database may also manage refuses each path once its
-- narinfo is read ('fetchSubstituteWith'), and the outcome is a failed one.
substituteFromCaches :: BuildConfig -> Store -> [StorePath] -> IO ClosureOutcome
substituteFromCaches config store paths =
  evalStateT (substituteClosure (substEffects config store) paths) emptyCacheMemo

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

-- | Tell the logger how a dependency was realised.
logRealised :: Logger -> Realised -> Derivation -> IO ()
logRealised logger status drv =
  logMessage logger ("  " <> statusTag status <> " " <> formatDrvName drv)

-- | Substitute paths, each from the cache that offered it, every one of
-- them or none ('FetchOutcome'), or under 'RegisterNone' fetch them and
-- register nothing, to learn which a cache delivers.
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
-- recorded nothing, so every path of the batch counts as not delivered
-- ('DeliveryFailed').  Upstream's copy of the path fails the same way when
-- a reference is not valid.
--
-- Each path comes from the cache that offered it, as the narinfo the
-- reference query read describes it ('fetchSubstituteWith'), and no
-- narinfo is requested again.
substitutePaths :: BuildConfig -> Store -> Registration -> NonEmpty Substitute -> IO FetchOutcome
substitutePaths config store registration offered = do
  heldLocks <- newIORef []
  registerSubstituted heldLocks
    `finally` (readIORef heldLocks >>= mapM_ releasePathLock)
  where
    registerSubstituted :: IORef [PathLock] -> IO FetchOutcome
    registerSubstituted heldLocks = do
      results <- traverse (substituteOne heldLocks) (lockOrder substitutePath (NE.toList offered))
      case (batchRegistrations results, registration) of
        (Left outcome, _) -> pure outcome
        -- The trees stay unpacked and unregistered, as a batch that does
        -- not land leaves them.
        (Right _, RegisterNone) -> pure Fetched
        (Right rows, RegisterAll) -> do
          registered <- try (registerPaths (stDB store) rows)
          case registered of
            Right () -> pure Fetched
            Left (e :: IOException) -> do
              logMessage (stLogger store) ("  [subst] registration refused: " <> T.pack (displayException e))
              pure (NotFetched (fmap (\substitute -> (substitutePath substitute, DeliveryFailed)) offered))
    -- Record a returned lock the moment it exists, so the enclosing
    -- finally owns it even when a later path's attempt fails or throws.
    substituteOne heldLocks substitute = do
      let sp = substitutePath substitute
      result <- fetchSubstituteWith (bcFetchRetry config) (ioRetryEffects (stLogger store)) store substitute
      case result of
        SubstSuccess _ lock -> atomicModifyIORef' heldLocks (\locks -> (lock : locks, ()))
        SubstError err -> logMessage (stLogger store) ("  [subst] " <> spName sp <> ": " <> err)
        _ -> pure ()
      pure (sp, result)

-- | What one batch's results come to: the rows to register when every path
-- arrived, or the paths that did not.  Nothing is registered unless every
-- path arrived, as one failed output fails upstream's derivation goal.
batchRegistrations :: [(StorePath, SubstResult)] -> Either FetchOutcome [PathRegistration]
batchRegistrations results =
  case NE.nonEmpty [(sp, why) | (sp, result) <- results, Just why <- [undelivered result]] of
    Just short -> Left (NotFetched short)
    -- An already-valid path counts as arrived but contributes no
    -- registration row (and holds no lock).
    Nothing -> Right [reg | (_, SubstSuccess reg _) <- results]
  where
    undelivered (SubstSuccess _ _) = Nothing
    undelivered SubstAlreadyValid = Nothing
    undelivered SubstNotFound = Just Gone
    undelivered (SubstError _) = Just DeliveryFailed

-- | Format a derivation name for status output (display-only, so the
-- byte-string env value decodes lossily).
formatDrvName :: Derivation -> Text
formatDrvName drv =
  case Map.lookup "name" (drvEnv drv) of
    Just n -> TE.decodeUtf8With lenientDecode n
    Nothing -> case drvOutputs drv of
      (out : _) -> spName (doPath out)
      [] -> "<unknown>"
