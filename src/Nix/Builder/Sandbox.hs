-- | What the build sandbox is, worked out without IO: its settings, whether
-- a build is sandboxed, which network it gets, the plan the
-- @nova-nix-sandbox@ helper carries out, the chroot it carries it out in
-- on Linux, the profile it applies on macOS, and the helper's report of a
-- step that failed.
--
-- == The sandbox
--
-- On Linux a sandboxed build runs in fresh user, mount, PID, IPC and UTS
-- namespaces, and a network namespace of its own unless it is
-- fixed-output, with a chroot that holds its inputs, as upstream's
-- @LocalDerivationGoal@ builds one (local-derivation-goal.cc at 2.24.9,
-- called LDG here).  On macOS it runs under a profile of macOS's sandbox
-- that lets it read its inputs and write its outputs and build directory,
-- as upstream's @DarwinDerivationBuilder@ applies one
-- (darwin-derivation-builder.cc at 2.36.0, over
-- @UnixDerivationBuilderImpl@ in unix-derivation-builder.cc there).  The
-- work is split three ways: this module decides and plans;
-- "Nix.Builder.Sandbox.Spawn" makes the chroot's skeleton, looks at the
-- paths a profile names, probes the system and runs the helper; and the
-- helper, a C program of its own, carries the plan out.  The helper's core
-- (@sandbox\/nova-nix-sandbox.c@) reads the plan and reports a failed step
-- as it does on every OS, and links the backend for its OS
-- ('SandboxBackend'), which makes the sandbox: on Linux
-- (@sandbox\/linux.c@) the namespaces, the mounts and the root, between
-- the namespaces' creation and the builder's exec, where the process
-- library runs nothing; on macOS (@sandbox\/darwin.c@) the profile,
-- applied in the builder's process just before its exec.
--
-- == The protocol
--
-- The helper is spawned with an empty environment, so its dynamic loader
-- reads none of the variables a derivation sets; the builder's
-- environment travels in the plan, which the helper reads ('renderPlan')
-- on its standard input: a version line, a line holding the length of the
-- rest, then tokens, each its length, a colon, its bytes and a NUL.  The
-- pipe stays open after the plan, and its end kills the build.  On its standard
-- output the helper writes at most one record, @STAGE ERRNO INDEX@, for a
-- step that failed ('parseStatus'); the builder's start closes that
-- output, so nothing there means the builder ran.  The stage names here
-- and in the helper are one list.
module Nix.Builder.Sandbox
  ( -- * Settings
    SandboxMode (..),
    sandboxSettingName,
    parseSandboxMode,
    SandboxFallback (..),
    DropGroups (..),
    SandboxPath (..),
    PathPresence (..),
    parseSandboxPaths,
    ViolationLog (..),
    SandboxConfig (..),
    defaultSandboxConfig,

    -- * The backends
    SandboxBackend (..),
    hostSandboxBackend,
    backendFallback,
    darwinSandboxPaths,
    darwinImpureHostPrefixes,

    -- * The decision
    SandboxUse (..),
    sandboxUse,
    Network (..),
    buildNetwork,
    LocalNetworking (..),
    DarwinOptions (..),
    darwinOptions,

    -- * The plan
    sandboxUid,
    sandboxGid,
    sandboxHome,
    rootRefusal,
    PlanInputs (..),
    SandboxPlan (..),
    Bind (..),
    BindAccess (..),
    SourceResolution (..),
    sandboxPlan,
    planVersionLine,
    renderPlan,

    -- * The macOS profile
    DarwinPathInputs (..),
    DarwinPath (..),
    DarwinPaths (..),
    darwinPathsInSandbox,
    darwinPathTarget,
    PathFound (..),
    ProfileFilter (..),
    profileFilter,
    DarwinProfile (..),
    darwinProfile,

    -- * The macOS plan
    DarwinPlan (..),
    darwinParameters,
    renderDarwinPlan,

    -- * The chroot
    SkeletonEntry (..),
    chrootSkeleton,
    passwdFile,
    groupFile,

    -- * What the helper reports
    SetupStage (..),
    setupStageName,
    DarwinStage (..),
    darwinStageName,
    ProbeStage (..),
    probeStageName,
    StatusRecord (..),
    parseStatus,
    Unavailability (..),
    unavailabilityCause,
    unavailableMessage,
    setupFailureMessage,
    darwinSetupFailureMessage,
    fallbackWarning,
    HelperOutcome (..),
    helperOutcome,
  )
where

import Data.Bool (bool)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Either (partitionEithers)
import Data.Int (Int32)
import Data.List (find, isPrefixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word32)
import Foreign.C.Error (Errno (..), eNOENT, eNOTDIR, ePERM, errnoToIOError)
import GHC.IO.Exception (IOException (ioe_description))
import Nix.Derivation (Derivation, fixedOutput)
import Nix.Realise (derivationAttrs, derivationBoolAttr, derivationStringAttr, derivationStringsAttr)
import Nix.Store.Path (StorePath, defaultStoreDir, storePathToText)
import System.Exit (ExitCode)
import qualified System.Info
import Text.Read (readMaybe)

-- ---------------------------------------------------------------------------
-- Settings
-- ---------------------------------------------------------------------------

-- | Upstream's @sandbox@ setting (@sandboxMode@ in globals.hh at 2.24.9,
-- local-settings.hh from 2.34.9).
data SandboxMode
  = -- | Every build is sandboxed, and @__noChroot@ is an error.
    SandboxEnabled
  | -- | A build is sandboxed unless it is fixed-output or sets
    -- @__noChroot@.
    SandboxRelaxed
  | -- | No build is sandboxed.
    SandboxDisabled
  deriving (Eq, Show)

-- | The setting's own name, which its value errors name whatever alias set
-- it.
sandboxSettingName :: ByteString
sandboxSettingName = "sandbox"

-- | Upstream's @BaseSetting\<SandboxMode\>::parse@ (globals.cc):
-- @true@, @relaxed@ or @false@, and nothing else, not even the other
-- Boolean spellings.
parseSandboxMode :: ByteString -> Either Text SandboxMode
parseSandboxMode value = case value of
  "true" -> Right SandboxEnabled
  "relaxed" -> Right SandboxRelaxed
  "false" -> Right SandboxDisabled
  _ -> Left ("option '" <> TE.decodeUtf8Lenient sandboxSettingName <> "' has invalid value '" <> TE.decodeUtf8Lenient value <> "'")

-- | Upstream's @sandbox-fallback@: what a sandboxed build does on a Linux
-- system that cannot make the namespaces ('backendFallback').
data SandboxFallback
  = -- | Build unsandboxed, upstream's default.
    FallBackUnsandboxed
  | -- | Fail the build.
    RefuseUnsandboxed
  deriving (Eq, Show)

-- | Upstream's @require-drop-supplementary-groups@: whether a build fails
-- when the invoking user's supplementary groups cannot be dropped.
data DropGroups
  = -- | Fail, upstream's default for root.
    RequireDropGroups
  | -- | Keep them and go on, upstream's default for anyone else.
    TolerateKeptGroups
  deriving (Eq, Show)

-- | Whether a path a bind names must exist.
data PathPresence
  = RequiredPath
  | -- | A missing source is passed over, as a @sandbox-paths@ entry ending
    -- in @?@ is.
    OptionalPath
  deriving (Eq, Show)

-- | One @sandbox-paths@ entry: the path in the sandbox and the host path
-- bound there, both bytes as the setting holds them.
data SandboxPath = SandboxPath
  { spTarget :: !ByteString,
    spSource :: !ByteString,
    spPresence :: !PathPresence
  }
  deriving (Eq, Show)

-- | Upstream's reading of @sandbox-paths@ (@startBuilder@ in LDG): in the
-- set's order, an empty entry skipped, a trailing @?@ making the source
-- optional, and @target=source@ split at the first @=@, a bare path binding
-- itself.  A later entry for a target replaces an earlier one
-- ('sandboxPlan').
parseSandboxPaths :: Set ByteString -> [SandboxPath]
parseSandboxPaths = concatMap entry . Set.toAscList
  where
    entry raw = case BS8.unsnoc raw of
      Nothing -> []
      Just (rest, '?') -> placed rest OptionalPath
      Just _ -> placed raw RequiredPath
    placed text presence = case BS8.break (== '=') text of
      (target, rest) | Just (_, source) <- BS8.uncons rest -> [SandboxPath target source presence]
      _ -> [SandboxPath text text presence]

-- | Upstream's @darwin-log-sandbox-violations@: whether macOS's sandbox
-- logs what a build's profile refuses.
data ViolationLog
  = -- | Refusals go unlogged, upstream's default.
    ViolationsUnlogged
  | -- | Refusals go to the system log.
    ViolationsLogged
  deriving (Eq, Show)

-- | How builds are sandboxed: the settings, and where the helper is.
data SandboxConfig = SandboxConfig
  { scMode :: !SandboxMode,
    scFallback :: !SandboxFallback,
    -- | The @sandbox-paths@ entries, read ('parseSandboxPaths').
    scPaths :: ![SandboxPath],
    -- | Upstream's @sandbox-build-dir@: where the builder sees its build
    -- directory on Linux.
    scBuildDir :: !ByteString,
    -- | Upstream's @sandbox-dev-shm-size@: the size of the tmpfs at
    -- @\/dev\/shm@, in mount(8)'s @size@ syntax.
    scDevShmSize :: !ByteString,
    scDropGroups :: !DropGroups,
    -- | Upstream's @allowed-impure-host-deps@: the host paths at or
    -- beneath which a derivation's @__impureHostDeps@ may name one, on
    -- macOS.
    scAllowedImpureHostDeps :: !(Set ByteString),
    scViolationLog :: !ViolationLog,
    -- | Upstream's @ssl-cert-file@: the CA bundle a fixed-output build sees
    -- at @\/etc\/ssl\/certs\/ca-certificates.crt@, when there is one.
    scCaFile :: !(Maybe ByteString),
    -- | Where the @nova-nix-sandbox@ helper is, when it was found.
    scHelper :: !(Maybe FilePath)
  }
  deriving (Eq, Show)

-- | No build sandboxed, and otherwise upstream's defaults where they are
-- the same on every OS: fallback on, no paths, @\/build@, half of memory
-- for @\/dev\/shm@, kept groups tolerated, no impure host paths allowed,
-- refusals unlogged, and no CA bundle or helper, which only IO can find.
defaultSandboxConfig :: SandboxConfig
defaultSandboxConfig =
  SandboxConfig
    { scMode = SandboxDisabled,
      scFallback = FallBackUnsandboxed,
      scPaths = [],
      scBuildDir = "/build",
      scDevShmSize = "50%",
      scDropGroups = TolerateKeptGroups,
      scAllowedImpureHostDeps = Set.empty,
      scViolationLog = ViolationsUnlogged,
      scCaFile = Nothing,
      scHelper = Nothing
    }

-- ---------------------------------------------------------------------------
-- The backends
-- ---------------------------------------------------------------------------

-- | The sandbox an OS makes, as the helper's backend for that OS makes it
-- (@sandbox\/linux.c@, @sandbox\/darwin.c@).  Each holds a build to rules
-- of its own.
data SandboxBackend
  = -- | Namespaces, and a chroot of the build's inputs.
    LinuxBackend
  | -- | A profile macOS's sandbox holds the builder to.
    DarwinBackend
  deriving (Eq, Show)

-- | The backend for the OS nova-nix was built for, where the helper has
-- one.
hostSandboxBackend :: Maybe SandboxBackend
hostSandboxBackend = case System.Info.os of
  "linux" -> Just LinuxBackend
  "darwin" -> Just DarwinBackend
  _ -> Nothing

-- | What a sandboxed build does where the sandbox cannot be made, under
-- the given backend: upstream reads @sandbox-fallback@ on Linux alone, and
-- on macOS a sandbox that cannot be made fails the build
-- (@makeDerivationBuilder@ in unix-derivation-builder.cc at 2.36.0).
backendFallback :: Maybe SandboxBackend -> SandboxFallback -> SandboxFallback
backendFallback backend fallback = case backend of
  Just DarwinBackend -> RefuseUnsandboxed
  _ -> fallback

-- | The @sandbox-paths@ a macOS build sees by default: upstream's macOS
-- defaults (globals.cc at 2.36.0), the system's frameworks, libraries and
-- shells, without its temporary directories @\/private\/tmp@ and
-- @\/private\/var\/tmp@.  Those hold the invoking user's files and
-- sockets, where upstream's build user owns nothing.  Undo when builds run
-- as build users of their own, under a daemon.
darwinSandboxPaths :: Set ByteString
darwinSandboxPaths =
  Set.fromList
    [ "/System/Library/Frameworks",
      "/System/Library/PrivateFrameworks",
      "/bin/sh",
      "/bin/bash",
      "/usr/lib"
    ]

-- | The @allowed-impure-host-deps@ prefixes on macOS by default, upstream's
-- (globals.cc at 2.36.0).  A path beneath @\/dev@ a derivation names this
-- way is refused if it is a terminal ('darwinProfile').
darwinImpureHostPrefixes :: Set ByteString
darwinImpureHostPrefixes = Set.fromList ["/System/Library", "/usr/lib", "/dev", "/bin/sh"]

-- ---------------------------------------------------------------------------
-- The decision
-- ---------------------------------------------------------------------------

-- | Whether a build is sandboxed, and if it is, which network it gets.
data SandboxUse = Sandboxed !Network | Unsandboxed
  deriving (Eq, Show)

-- | The network a sandboxed build gets.
data Network
  = -- | A namespace of its own, holding only a loopback interface.
    PrivateNetwork
  | -- | The host's.
    HostNetwork
  deriving (Eq, Show)

-- | A fixed-output derivation reaches the host's network, as upstream's
-- derivation type leaves it unsandboxed (@DerivationType::isSandboxed@ and
-- @BasicDerivation::type@ in derivations.cc at 2.24.9,
-- @derivation::Type::isSandboxed@ and @derivation::type@ there at 2.36.0);
-- every other build gets a network of its own (@startBuilder@ in LDG).
-- 'fixedOutput' is the rule the @impureEnvVars@ carve-out follows too.
buildNetwork :: Derivation -> Network
buildNetwork drv = case fixedOutput drv of
  Just _ -> HostNetwork
  Nothing -> PrivateNetwork

-- | Upstream's choice of a sandboxed build (@tryLocalBuild@ in LDG,
-- @makeDerivationBuilder@ in unix-derivation-builder.cc at 2.36.0), for
-- the derivation at a @.drv@ path, built where the given backend makes
-- the sandbox.  Under @sandbox = true@ every build is sandboxed, a
-- derivation setting @__noChroot@ is refused, and on macOS so is one
-- setting @__sandboxProfile@; under @relaxed@ one that is fixed-output or
-- sets @__noChroot@ is not sandboxed.
sandboxUse :: Maybe SandboxBackend -> SandboxMode -> StorePath -> Derivation -> Either Text SandboxUse
sandboxUse backend mode drvPath drv = case mode of
  SandboxDisabled -> Right Unsandboxed
  SandboxEnabled -> do
    attrs <- derivationAttrs drvPath drv
    noChroot <- derivationBoolAttr drvPath noChrootAttr False attrs
    profile <- case backend of
      Just DarwinBackend -> derivationStringAttr drvPath sandboxProfileAttr attrs
      _ -> Right Nothing
    case (noChroot, profile) of
      (True, _) -> Left ("derivation '" <> shownDrvPath <> "' has '" <> noChrootAttr <> "' set, but that's not allowed when 'sandbox' is 'true'")
      (False, Just text)
        | not (BS.null text) -> Left ("derivation '" <> shownDrvPath <> "' specifies a sandbox profile, but this is only allowed when 'sandbox' is 'relaxed'")
      _ -> Right (Sandboxed (buildNetwork drv))
  SandboxRelaxed -> do
    noChroot <- noChrootSet
    pure $ case (fixedOutput drv, noChroot) of
      (Nothing, False) -> Sandboxed PrivateNetwork
      _ -> Unsandboxed
  where
    noChrootSet = derivationAttrs drvPath drv >>= derivationBoolAttr drvPath noChrootAttr False
    shownDrvPath = storePathToText defaultStoreDir drvPath

-- | The attribute a derivation asks to build outside the sandbox with.
noChrootAttr :: Text
noChrootAttr = "__noChroot"

-- | The attributes a derivation asks the macOS sandbox for things with
-- (@derivationOptionsFromStructuredAttrs@ in derivation-options.cc at
-- 2.36.0).
sandboxProfileAttr, impureHostDepsAttr, localNetworkingAttr :: Text
sandboxProfileAttr = "__sandboxProfile"
impureHostDepsAttr = "__impureHostDeps"
localNetworkingAttr = "__darwinAllowLocalNetworking"

-- | Whether a macOS build may use the loopback network, which its
-- profile otherwise refuses a build that is not fixed-output.
data LocalNetworking = LocalNetworkingDenied | LocalNetworkingAllowed
  deriving (Eq, Show)

-- | What a derivation asks of the macOS sandbox: upstream's
-- @additionalSandboxProfile@, @impureHostDeps@ and @allowLocalNetworking@
-- (@derivationOptionsFromStructuredAttrs@ in derivation-options.cc at
-- 2.36.0).
data DarwinOptions = DarwinOptions
  { -- | @__sandboxProfile@: profile text the build's profile ends with.
    doExtraProfile :: !ByteString,
    -- | @__impureHostDeps@: host paths the build may read, each beneath
    -- an @allowed-impure-host-deps@ entry.
    doImpureHostDeps :: !(Set ByteString),
    -- | @__darwinAllowLocalNetworking@.
    doLocalNetworking :: !LocalNetworking
  }
  deriving (Eq, Show)

-- | A derivation's t'DarwinOptions', for the derivation at a @.drv@ path:
-- none of them when it sets none.
darwinOptions :: StorePath -> Derivation -> Either Text DarwinOptions
darwinOptions drvPath drv = do
  attrs <- derivationAttrs drvPath drv
  profile <- derivationStringAttr drvPath sandboxProfileAttr attrs
  deps <- derivationStringsAttr drvPath impureHostDepsAttr attrs
  local <- derivationBoolAttr drvPath localNetworkingAttr False attrs
  pure
    DarwinOptions
      { doExtraProfile = fromMaybe "" profile,
        doImpureHostDeps = maybe Set.empty Set.fromList deps,
        doLocalNetworking = bool LocalNetworkingDenied LocalNetworkingAllowed local
      }

-- ---------------------------------------------------------------------------
-- The plan
-- ---------------------------------------------------------------------------

-- | The ids a build runs as in its user namespace, which upstream uses
-- when it has no build users (@sandboxUid@ and @sandboxGid@ in
-- local-derivation-goal.hh at 2.24.9, linux-derivation-builder.cc at
-- 2.36.0).
sandboxUid, sandboxGid :: Word32
sandboxUid = 1000
sandboxGid = 100

-- | The @HOME@ a sandboxed build is told of, upstream's, which does not
-- exist, so a program looking for its settings there finds none
-- (@homeDir@ and @initEnv@ in LDG).
sandboxHome :: FilePath
sandboxHome = "/homeless-shelter"

-- | Why a sandboxed build is refused when nova-nix runs as root.  Upstream
-- builds as root under build users, uids of their own; nova-nix has none
-- yet, so the builder would map to root on the host and own every file
-- root owns there.  A fallback to an unsandboxed build would be no answer
-- to that, so this refusal stands whatever @sandbox-fallback@ says.
rootRefusal :: Text
rootRefusal = "a sandboxed build as root needs build users, which nova-nix does not have yet; run nova-nix as a user other than root, or pass '--no-sandbox' to build without the sandbox"

-- | What a sandboxed build's plan is made from.  Paths are bytes; each
-- input path is where the builder sees it, which is where it is on disk.
data PlanInputs = PlanInputs
  { -- | The chroot's root on disk.
    piRoot :: !ByteString,
    -- | The store directory.
    piStoreDir :: !ByteString,
    -- | The build directory as the builder sees it ('scBuildDir').
    piBuildDirSeen :: !ByteString,
    -- | The build directory on disk.
    piBuildDirOnDisk :: !ByteString,
    -- | The closure of the build's inputs, upstream's @inputPaths@.
    piInputs :: ![ByteString],
    -- | The @sandbox-paths@ entries.
    piSandboxPaths :: ![SandboxPath],
    -- | The closure of the entries whose source is in the store.
    piSandboxClosure :: ![ByteString],
    -- | Each output as the builder sees it.
    piOutputs :: ![ByteString],
    -- | The builder's environment, as @NAME=VALUE@ entries.
    piEnvironment :: ![ByteString],
    piNetwork :: !Network,
    piCaFile :: !(Maybe ByteString),
    piDevShmSize :: !ByteString,
    piDropGroups :: !DropGroups
  }
  deriving (Eq, Show)

-- | Whether the builder may write through a bind.
data BindAccess = ReadOnlyBind | ReadWriteBind
  deriving (Eq, Show)

-- | Whether the helper resolves a source's symlinks before it binds it, as
-- upstream's @canonPath(path, true)@ does for the device nodes and host
-- files it adds (@runChild@ in LDG).
data SourceResolution = SourceAsGiven | SourceResolved
  deriving (Eq, Show)

-- | A host path bound into the chroot.
data Bind = Bind
  { bindTarget :: !ByteString,
    bindSource :: !ByteString,
    bindAccess :: !BindAccess,
    bindPresence :: !PathPresence,
    bindResolution :: !SourceResolution
  }
  deriving (Eq, Show)

-- | What the helper does for one build on Linux.
data SandboxPlan = SandboxPlan
  { planRoot :: !ByteString,
    planBuildDir :: !ByteString,
    planDevShmSize :: !ByteString,
    planNetwork :: !Network,
    planDropGroups :: !DropGroups,
    -- | In the order they are made, which is the targets' sorted order,
    -- as upstream walks its @pathsInChroot@ map, so a directory is bound
    -- before anything bound beneath it.
    planBinds :: ![Bind],
    -- | The builder's environment, as @NAME=VALUE@ entries, which the
    -- helper passes to the builder's @execve@ and never takes as its own.
    planEnvironment :: ![ByteString]
  }
  deriving (Eq, Show)

-- | The device nodes a build sees when nothing binds @\/dev@ whole
-- (@runChild@ in LDG).  @\/dev\/kvm@ joins them upstream only for a store
-- with the @kvm@ system feature, which nova-nix has no setting for.
deviceNodes :: [ByteString]
deviceNodes = ["/dev/full", "/dev/null", "/dev/random", "/dev/tty", "/dev/urandom", "/dev/zero"]

-- | The host files a fixed-output build sees, where they exist
-- (@runChild@ in LDG).
hostNetworkFiles :: [ByteString]
hostNetworkFiles = ["/etc/resolv.conf", "/etc/services", "/etc/hosts"]

-- | Where a fixed-output build sees the CA bundle (@runChild@ in LDG).
caBundleTarget :: ByteString
caBundleTarget = "/etc/ssl/certs/ca-certificates.crt"

-- | The path a build sees its whole @\/dev@ at when an entry binds it.
devDir :: ByteString
devDir = "/dev"

-- | The binds of a sandboxed build, built up as upstream builds
-- @pathsInChroot@ (@startBuilder@ and @runChild@ in LDG): the @sandbox-paths@
-- entries, a later one for a target replacing an earlier; the build
-- directory; the closure of the entries in the store; the input closure;
-- then no output.  Then, without replacing an entry already there, the
-- device nodes unless @\/dev@ is bound whole, and for a fixed-output
-- build the host's network files and the CA bundle, each optional.
--
-- Every host path is bound read-only, the build directory alone writable,
-- where upstream binds them plainly (@doBind@ in LDG): the builder runs as the
-- invoking user, so a writable bind would let a build change a valid store
-- path, the bundled shell later builds run, an @--exec-wrapper@ launcher,
-- or any file of the user's a @sandbox-paths@ entry names.  A bind is
-- read-only through every mount beneath its source, which a recursive bind
-- brings along.  A device node stays usable on a read-only mount.  The
-- one host path the helper binds writable is outside the plan: where the
-- kernel will not mount a devpts of the sandbox's own, it binds the
-- host's @\/dev\/pts@ and @\/dev\/ptmx@ instead, as upstream does
-- (@runChild@ in LDG).
--
-- A build directory that holds the store directory is upstream's error
-- (@startBuilder@ in LDG).  A target that is not absolute, or has a @..@
-- component, is refused, since joined to the chroot's root it names a path
-- outside it; upstream joins its targets as they are (@runChild@ in LDG).
sandboxPlan :: PlanInputs -> Either Text SandboxPlan
sandboxPlan inputs
  | piBuildDirSeen inputs `BS.isPrefixOf` piStoreDir inputs = Left "`sandbox-build-dir` must not contain the storeDir"
  | Just target <- find leavesRoot (piBuildDirSeen inputs : map spTarget (piSandboxPaths inputs)) = Left (outsideSandboxMessage target)
  | otherwise =
      Right
        SandboxPlan
          { planRoot = piRoot inputs,
            planBuildDir = piBuildDirSeen inputs,
            planDevShmSize = piDevShmSize inputs,
            planNetwork = piNetwork inputs,
            planDropGroups = piDropGroups inputs,
            planBinds = [Bind target source access presence resolution | (target, Entry source access presence resolution) <- Map.toAscList withHostPaths],
            planEnvironment = piEnvironment inputs
          }
  where
    configured = foldl' (\m sp -> Map.insert (spTarget sp) (Entry (spSource sp) ReadOnlyBind (spPresence sp) SourceAsGiven) m) Map.empty (piSandboxPaths inputs)
    withBuildDir = Map.insert (piBuildDirSeen inputs) (Entry (piBuildDirOnDisk inputs) ReadWriteBind RequiredPath SourceAsGiven) configured
    withStore = foldl' (\m p -> Map.insert p (Entry p ReadOnlyBind RequiredPath SourceAsGiven) m) withBuildDir (piSandboxClosure inputs ++ piInputs inputs)
    withoutOutputs = foldl' (flip Map.delete) withStore (piOutputs inputs)
    devices
      | Map.member devDir withoutOutputs = []
      | otherwise = [(node, Entry node ReadOnlyBind RequiredPath SourceResolved) | node <- deviceNodes]
    networkFiles = case piNetwork inputs of
      PrivateNetwork -> []
      HostNetwork ->
        [(file, Entry file ReadOnlyBind OptionalPath SourceResolved) | file <- hostNetworkFiles]
          ++ [(caBundleTarget, Entry caFile ReadOnlyBind OptionalPath SourceResolved) | Just caFile <- [piCaFile inputs], not (BS.null caFile)]
    withHostPaths = foldl' (\m (target, entry) -> Map.insertWith keepExisting target entry m) withoutOutputs (devices ++ networkFiles)
    keepExisting _ existing = existing

-- | A bind before it has a target, as the map holds it.
data Entry = Entry !ByteString !BindAccess !PathPresence !SourceResolution

-- | Whether a target names no path in the sandbox: it is not absolute, or
-- has a @..@ component.
leavesRoot :: ByteString -> Bool
leavesRoot target = not ("/" `BS.isPrefixOf` target) || ".." `elem` BS8.split '/' target

-- | The refusal of a sandbox target that names no path in the sandbox:
-- one 'leavesRoot' finds on Linux, or a relative one on macOS.
outsideSandboxMessage :: ByteString -> Text
outsideSandboxMessage target = "the sandbox path '" <> TE.decodeUtf8Lenient target <> "' is not an absolute path without a '..' component, so it names no path in the sandbox"

-- | The first line of a plan, which names the format's version.  The
-- helper refuses any other.
planVersionLine :: ByteString
planVersionLine = "nova-nix-sandbox-plan 4\n"

-- | A plan as the helper reads it, given its tokens: the version line, a
-- line holding the length in bytes of the rest, then each token, its
-- length in decimal, a colon, its bytes and a NUL.  A token's length says
-- where it ends, so no bytes in a field, a NUL among them, can end it
-- early or start another; the helper, which uses each token as a C
-- string, refuses one that holds a NUL.  The plan's length lets the
-- helper read it and leave the pipe open behind it.
framePlan :: [ByteString] -> ByteString
framePlan tokens = planVersionLine <> BS8.pack (show (BS.length body)) <> "\n" <> body
  where
    body = foldMap token tokens
    token bytes = BS8.pack (show (BS.length bytes)) <> ":" <> bytes <> "\0"

-- | A Linux plan as the helper reads it ('framePlan'): the backend, then
-- each key and its values in one fixed order.  A bind is its target, its
-- source and its flags: @-@ for none, else @r@ (read-only), @o@
-- (optional) and @c@ (resolve the source's symlinks).  An @env@ token is
-- one entry of the builder's environment.
renderPlan :: SandboxPlan -> ByteString
renderPlan plan =
  framePlan
    ( concat
        [ ["backend", "linux"],
          ["root", planRoot plan],
          ["build-dir", planBuildDir plan],
          ["dev-shm-size", planDevShmSize plan],
          ["sandbox-uid", BS8.pack (show sandboxUid)],
          ["sandbox-gid", BS8.pack (show sandboxGid)],
          ["network", networkToken (planNetwork plan)],
          ["drop-groups", dropGroupsToken (planDropGroups plan)]
        ]
        ++ concatMap bindTokens (planBinds plan)
        ++ concatMap (\entry -> ["env", entry]) (planEnvironment plan)
    )
  where
    networkToken network = case network of
      PrivateNetwork -> "private"
      HostNetwork -> "host"
    dropGroupsToken groups = case groups of
      RequireDropGroups -> "require"
      TolerateKeptGroups -> "tolerate"
    bindTokens bind = ["bind", bindTarget bind, bindSource bind, bindFlags bind]
    bindFlags bind = case flagLetters bind of
      "" -> "-"
      letters -> letters
    flagLetters bind =
      BS8.pack
        ( ['r' | bindAccess bind == ReadOnlyBind]
            ++ ['o' | bindPresence bind == OptionalPath]
            ++ ['c' | bindResolution bind == SourceResolved]
        )

-- ---------------------------------------------------------------------------
-- The chroot
-- ---------------------------------------------------------------------------

-- | What the chroot holds before the helper binds anything into it, each
-- path as the builder sees it.
data SkeletonEntry
  = -- | A directory, and its mode.
    SkeletonDir !ByteString !Word32
  | -- | A file, and its contents.
    SkeletonFile !ByteString !ByteString
  | -- | A symlink, and its target.
    SkeletonLink !ByteString !ByteString
  deriving (Eq, Show)

-- | Upstream's chroot before the binds (@startBuilder@ in LDG): a
-- writable @\/tmp@, @\/etc@ with the build user's @passwd@ and @group@ and,
-- for a build with a network of its own, @hosts@, or for a fixed-output
-- one @nsswitch.conf@; the store directory, writable by its owner and
-- sticky; @\/proc@; and unless @\/dev@ is bound whole, @\/dev\/shm@,
-- @\/dev\/pts@ and the links into @\/proc\/self\/fd@ (@runChild@ in LDG).  A
-- directory above the store directory is created with the default mode.
-- The helper gives @\/etc@ upstream's mode, 0555, once its binds are in
-- place.
chrootSkeleton :: ByteString -> ByteString -> Network -> [SandboxPath] -> [SkeletonEntry]
chrootSkeleton storeDir buildDirSeen network paths =
  [SkeletonDir "/tmp" stickyWritable, SkeletonDir "/etc" plainDir, SkeletonFile "/etc/passwd" (passwdFile buildDirSeen), SkeletonFile "/etc/group" groupFile]
    ++ networkFile
    ++ [SkeletonDir storeDir stickyGroupWritable, SkeletonDir "/proc" plainDir]
    ++ devEntries
  where
    networkFile = case network of
      PrivateNetwork -> [SkeletonFile "/etc/hosts" "127.0.0.1 localhost\n::1 localhost\n"]
      HostNetwork -> [SkeletonFile "/etc/nsswitch.conf" "hosts: files dns\nservices: files\n"]
    devEntries
      | any ((== devDir) . spTarget) paths = []
      | otherwise =
          [ SkeletonDir devDir plainDir,
            SkeletonDir "/dev/shm" plainDir,
            SkeletonDir "/dev/pts" plainDir,
            SkeletonLink "/dev/fd" "/proc/self/fd",
            SkeletonLink "/dev/stdin" "/proc/self/fd/0",
            SkeletonLink "/dev/stdout" "/proc/self/fd/1",
            SkeletonLink "/dev/stderr" "/proc/self/fd/2"
          ]
    stickyWritable = 0o1777
    stickyGroupWritable = 0o1775
    plainDir = 0o755

-- | Upstream's @\/etc\/passwd@ for a build (@startBuilder@ in LDG), whose
-- home is the build directory as the builder sees it.
passwdFile :: ByteString -> ByteString
passwdFile home =
  BS.concat
    [ "root:x:0:0:Nix build user:",
      home,
      ":/noshell\n",
      "nixbld:x:",
      BS8.pack (show sandboxUid),
      ":",
      BS8.pack (show sandboxGid),
      ":Nix build user:",
      home,
      ":/noshell\n",
      "nobody:x:65534:65534:Nobody:/:/noshell\n"
    ]

-- | Upstream's @\/etc\/group@ for a build (@startBuilder@ in LDG).
groupFile :: ByteString
groupFile = BS.concat ["root:x:0:\n", "nixbld:!:", BS8.pack (show sandboxGid), ":\n", "nogroup:x:65534:\n"]

-- ---------------------------------------------------------------------------
-- The macOS profile
-- ---------------------------------------------------------------------------

-- | A host path a macOS build may read and run, as upstream's
-- @pathsInChroot@ holds one (@DarwinDerivationBuilder::setUser@): where
-- the build sees it, where
-- it is on the host, and whether it may be missing.  The profile names
-- the path the build sees, which must be where it is.
data DarwinPath = DarwinPath
  { darwinTarget :: !ByteString,
    darwinSource :: !ByteString,
    darwinPresence :: !PathPresence
  }
  deriving (Eq, Show)

-- | What a macOS build's paths are made from, each as bytes.  A profile
-- matches a path as the kernel resolves it, so the store directory and
-- the build directory are given resolved as well as as the builder sees
-- them, and the inputs and the closure are given beneath the resolved
-- store directory.
data DarwinPathInputs = DarwinPathInputs
  { -- | The @.drv@ path, which a refused impure path names.
    dpiDrvPath :: !StorePath,
    dpiStoreDir :: !ByteString,
    dpiRealStoreDir :: !ByteString,
    dpiBuildDir :: !ByteString,
    dpiRealBuildDir :: !ByteString,
    -- | The @sandbox-paths@ entries.
    dpiSandboxPaths :: ![SandboxPath],
    -- | The closure of the entries whose source is in the store.
    dpiSandboxClosure :: ![ByteString],
    -- | The derivation's @__impureHostDeps@.
    dpiImpureHostDeps :: !(Set ByteString),
    -- | @allowed-impure-host-deps@.
    dpiAllowedImpureHostDeps :: !(Set ByteString),
    -- | The closure of the build's inputs, upstream's @inputPaths@.
    dpiInputs :: ![ByteString]
  }
  deriving (Eq, Show)

-- | A macOS build's paths: each it may read and run, in the order the
-- profile names them, which is the targets' sorted order, and the
-- directories above them it may read.
data DarwinPaths = DarwinPaths
  { dpPaths :: ![DarwinPath],
    dpAncestry :: ![ByteString]
  }
  deriving (Eq, Show)

-- | The paths of a macOS build, built up as upstream builds them
-- (@DerivationBuildingGoal::buildLocally@,
-- @UnixDerivationBuilderImpl::getPathsInSandbox@ and
-- @DarwinDerivationBuilder::setUser@ at 2.36.0): the @sandbox-paths@
-- entries, a later one for a target
-- replacing an earlier; the closure of the entries in the store; the
-- build directory; each @__impureHostDeps@ path, which may be missing;
-- then the input closure.
--
-- The ancestry is every directory above a path but an input, and the
-- store directory with every directory above it but the root, read
-- before the inputs join, which share it.  The store directory and the
-- build directory count both as the builder sees them and as they
-- resolve, so a build can follow a link on its way to either.
--
-- An impure path is refused, in upstream's words, unless it is an entry
-- of @allowed-impure-host-deps@ or beneath one, each compared once its
-- @.@ and @..@ components are resolved.  A @sandbox-paths@ target that is
-- not absolute is refused, in the words the Linux sandbox refuses it
-- with, where upstream loops forever looking for the root above it.
darwinPathsInSandbox :: DarwinPathInputs -> Either Text DarwinPaths
darwinPathsInSandbox inputs
  | Just target <- find (not . ("/" `BS.isPrefixOf`)) (map spTarget (dpiSandboxPaths inputs)) = Left (outsideSandboxMessage target)
  | otherwise = do
      impure <- traverse allowedImpure (Set.toAscList (dpiImpureHostDeps inputs))
      let configured = foldl' (\m sp -> Map.insert (spTarget sp) (DarwinPath (spTarget sp) (spSource sp) (spPresence sp)) m) Map.empty (dpiSandboxPaths inputs)
          withClosure = foldl' (\m p -> Map.insert p (itself RequiredPath p) m) configured (dpiSandboxClosure inputs)
          withBuildDir = Map.insert (dpiRealBuildDir inputs) (itself RequiredPath (dpiRealBuildDir inputs)) withClosure
          withImpure = foldl' (\m p -> Map.insert p (itself OptionalPath p) m) withBuildDir impure
          withInputs = foldl' (\m p -> Map.insert p (itself RequiredPath p) m) withImpure (dpiInputs inputs)
          ancestry =
            Set.fromList (concatMap pathParents (dpiBuildDir inputs : Map.keys withImpure))
              <> Set.fromList (concatMap selfAndParents [dpiStoreDir inputs, dpiRealStoreDir inputs])
      Right (DarwinPaths (Map.elems withInputs) (Set.toAscList ancestry))
  where
    itself presence path = DarwinPath path path presence
    allowedImpure dep = do
      canonical <- canonicalPath dep
      let search [] = Left ("derivation '" <> storePathToText defaultStoreDir (dpiDrvPath inputs) <> "' requested impure path '" <> TE.decodeUtf8Lenient dep <> "', but it was not in allowed-impure-host-deps")
          search (prefix : rest) = do
            allowed <- canonicalPath prefix
            case dirOrInDir canonical allowed of
              True -> Right dep
              False -> search rest
      search (Set.toAscList (dpiAllowedImpureHostDeps inputs))
    selfAndParents path
      | path == "/" = []
      | otherwise = path : selfAndParents (parentDir path)

-- | Every directory above an absolute path, the root included, as
-- upstream walks @parent_path@ up to it (@DarwinDerivationBuilder::setUser@).
pathParents :: ByteString -> [ByteString]
pathParents path
  | path == "/" = []
  | otherwise = let up = parentDir path in up : pathParents up

-- | @std::filesystem::path::parent_path@ of an absolute path: the path
-- without its last component, or the root above a top-level one.  A path
-- with a trailing slash has an empty last component, so its parent is
-- the path without the slash.
parentDir :: ByteString -> ByteString
parentDir path = case BS8.dropWhileEnd (== '/') (fst (BS8.breakEnd (== '/') path)) of
  "" -> "/"
  dir -> dir

-- | Upstream's @canonPath@ without resolving links
-- (file-system.cc and file-path-impl.hh at 2.36.0): an absolute
-- path with its empty and @.@ components dropped and each @..@ taking
-- the component before it away, the root's staying.
canonicalPath :: ByteString -> Either Text ByteString
canonicalPath path
  | BS.null path = Left "cannot canonicalise an empty path"
  | not ("/" `BS.isPrefixOf` path) = Left ("not an absolute path: \"" <> TE.decodeUtf8Lenient path <> "\"")
  | otherwise = Right ("/" <> BS.intercalate "/" (reverse (foldl' step [] (BS8.split '/' path))))
  where
    step kept component = case component of
      "" -> kept
      "." -> kept
      ".." -> drop 1 kept
      _ -> component : kept

-- | Upstream's @isDirOrInDir@ (file-system.cc at 2.36.0) for two
-- canonical paths: the same path, or one whose components start with all
-- of the other's.
dirOrInDir :: ByteString -> ByteString -> Bool
dirOrInDir path dir = path == dir || (length dirParts < length pathParts && dirParts `isPrefixOf` pathParts)
  where
    pathParts = components path
    dirParts = components dir
    components = filter (not . BS.null) . BS8.split '/'

-- | The one path the profile can name for an entry, its target, which
-- upstream's refuses unless the entry's source is the same path
-- (@DarwinDerivationBuilder::setUser@).
darwinPathTarget :: DarwinPath -> Either Text ByteString
darwinPathTarget path
  | darwinTarget path == darwinSource path = Right (darwinTarget path)
  | otherwise = Left (setupContext <> "can't map " <> quoted (darwinTarget path) <> " to " <> quoted (darwinSource path) <> ": mismatched impure paths not supported on Darwin")
  where
    quoted bytes = "\"" <> TE.decodeUtf8Lenient bytes <> "\""

-- | What @lstat@ found at a path.
data PathFound
  = FoundDirectory
  | -- | A file, a link or anything else that is not a directory.
    FoundOther
  | -- | Nothing could be found, with the @errno@ that said why.
    LstatFailed !Int
  deriving (Eq, Show)

-- | How the profile names a host path: a directory with everything
-- beneath it, or the one node at the path (@DarwinDerivationBuilder::setUser@).
data ProfileFilter = Subpath !ByteString | Literal !ByteString
  deriving (Eq, Show)

-- | What an entry adds to the profile, given what @lstat@ found at its
-- target, as upstream decides it (@DarwinDerivationBuilder::setUser@,
-- with @maybeLstat@ in unix/file-system.cc at 2.36.0): nothing for a
-- missing entry that
-- may be missing; for a missing one that may not, upstream's error, whose
-- quote is never closed as upstream's format leaves it; and for any other
-- failure, @maybeLstat@'s.  A path is missing when @lstat@ says @ENOENT@
-- or @ENOTDIR@.
profileFilter :: DarwinPath -> PathFound -> Either Text (Maybe ProfileFilter)
profileFilter path found = case found of
  FoundDirectory -> Right (Just (Subpath target))
  FoundOther -> Right (Just (Literal target))
  LstatFailed number
    | Errno (fromIntegral number) `notElem` [eNOENT, eNOTDIR] -> Left (setupContext <> "getting status of \"" <> shown <> "\"" <> errnoSuffix number)
    | darwinPresence path == OptionalPath -> Right Nothing
    | otherwise -> Left (setupContext <> "getting attributes of required path '" <> shown <> errnoSuffix number)
  where
    target = darwinTarget path
    shown = TE.decodeUtf8Lenient target

-- | What a macOS build's profile is made from.
data DarwinProfile = DarwinProfile
  { dprViolationLog :: !ViolationLog,
    dprNetwork :: !Network,
    -- | The store directory, resolved.
    dprStoreDir :: !ByteString,
    -- | The paths the outputs are written to, resolved.
    dprOutputs :: ![ByteString],
    -- | The paths the build reads and runs ('darwinPathsInSandbox'), each
    -- as 'profileFilter' names it.
    dprPaths :: ![ProfileFilter],
    dprAncestry :: ![ByteString],
    -- | The derivation's @__sandboxProfile@.
    dprExtra :: !ByteString
  }
  deriving (Eq, Show)

-- | A macOS build's profile, nova-nix's own text written to upstream's
-- rules (@DarwinDerivationBuilder::setUser@ at 2.36.0, with the rules of
-- upstream's @sandbox-defaults.sb@ and @sandbox-network.sb@): everything
-- is denied unless an allow form here allows it, and what is refused is
-- logged only under 'ViolationsLogged'.  The build may write its
-- outputs and its build directory ('profileBase'); a fixed-output build
-- reaches the network ('profileNetwork'); the inputs and every other path
-- ('darwinPathsInSandbox') may be read and run; and each directory of the
-- ancestry may be read, so a path to them resolves.  The terminals are
-- refused after every rule that could allow one ('profileTerminals'), the
-- derivation's own text follows, and the build's mark ('profileMark')
-- comes last.
--
-- macOS compiles the path filters of one operation into one table of at
-- most 64 KiB, however many forms hold them, so the paths in the store
-- are grouped by the first byte of their name, their hash's, each group
-- behind a prefix filter of its own, which gets a table of its own, as
-- upstream groups them; the prefix holds every path of its group, so it
-- allows nothing more.  Every other path is in one form of its own.
--
-- A later rule for an operation overrides an earlier one for it, so
-- writes beneath the store directory are denied after the scratch rule,
-- and the build directory, which nova-nix keeps in the store, and the
-- outputs are allowed again after the denial.  The allowing rule names
-- @file-write*@, since one for the wider @file*@ does not override a
-- denial of @file-write*@.
--
-- Upstream allows writes to the inputs and paths too, under build users
-- who own none of them.  nova-nix's builder runs as the invoking user,
-- who owns the store and the files a @sandbox-paths@ entry may name, so
-- this profile lets the build read and run them alone, as the Linux
-- sandbox binds every host path read-only.  A path is written in a
-- string escaped for the profile's reader ('profileString'), as upstream
-- escapes it (@sbplQuote@).  An allow form is never written with no
-- filter, which would allow its operations everywhere.
darwinProfile :: DarwinProfile -> ByteString
darwinProfile profile =
  BS.concat
    [ "(version 1)\n",
      denyDefault,
      profileBase,
      networkRules,
      storeRules,
      section "The outputs the build writes." "file-read* file-write* process-exec" (map Subpath (dprOutputs profile)),
      section "The host paths the build reads and runs." readAndRun otherPaths,
      bucketForms,
      section "The directories above them, so a path to them resolves." "file-read*" (map Literal (dprAncestry profile)),
      profileTerminals,
      endLine (dprExtra profile),
      profileMark
    ]
  where
    denyDefault = case dprViolationLog profile of
      ViolationsUnlogged -> "(deny default (with no-log))\n"
      ViolationsLogged -> "(deny default)\n"
    networkRules = case dprNetwork profile of
      PrivateNetwork -> ""
      HostNetwork -> profileNetwork
    readAndRun = "file-read* process-exec"
    storePrefix = dprStoreDir profile <> "/"
    -- A store path's bucket is the first byte of its name, its hash's.
    bucketOf pathFilter = BS.uncons =<< BS.stripPrefix storePrefix (filterPath pathFilter)
    (storePaths, otherPaths) = partitionEithers [maybe (Right pathFilter) (\(byte, _) -> Left (byte, pathFilter)) (bucketOf pathFilter) | pathFilter <- dprPaths profile]
    buckets = Map.toAscList (Map.fromListWith (flip (++)) [(byte, [pathFilter]) | (byte, pathFilter) <- storePaths])
    bucketForms = case buckets of
      [] -> ""
      _ -> "; The store paths the build reads and runs, a form for each first\n; character of their hash.\n" <> foldMap bucketForm buckets
    bucketForm (byte, filters) =
      BS.concat
        [ "(allow " <> readAndRun <> "\n",
          "\t(require-all\n",
          "\t\t(prefix " <> profileString (BS.snoc storePrefix byte) <> ")\n",
          "\t\t(require-any\n",
          foldMap (\pathFilter -> "\t\t" <> filterLine pathFilter) filters,
          "\t\t)))\n"
        ]
    storeRules =
      BS8.unlines
        [ "; The store, which no rule above opens to writes, and the build",
          "; directory within it, opened again.",
          "(deny file-write* (subpath " <> profileString (dprStoreDir profile) <> "))",
          "(allow file-read* file-write* process-exec network-outbound network-inbound (subpath (param \"_NIX_BUILD_TOP\")))"
        ]

-- | A comment, then an allow form of the operations over the filters, one
-- to a line, or nothing for no filter.
section :: ByteString -> ByteString -> [ProfileFilter] -> ByteString
section _ _ [] = ""
section text operations filters = "; " <> text <> "\n(allow " <> operations <> "\n" <> foldMap filterLine filters <> ")\n"

-- | The path a filter names.
filterPath :: ProfileFilter -> ByteString
filterPath pathFilter = case pathFilter of
  Subpath path -> path
  Literal path -> path

-- | A filter's line in an allow form.
filterLine :: ProfileFilter -> ByteString
filterLine profileFilterValue = case profileFilterValue of
  Subpath path -> "\t(subpath " <> profileString path <> ")\n"
  Literal path -> "\t(literal " <> profileString path <> ")\n"

-- | A string of the profile's language holding these bytes: a backslash
-- and a double quote are escaped with a backslash, as its reader takes
-- them.
profileString :: ByteString -> ByteString
profileString bytes = "\"" <> BS.concatMap escape bytes <> "\""
  where
    escape byte
      | byte == backslash || byte == doubleQuote = BS.pack [backslash, byte]
      | otherwise = BS.singleton byte
    backslash = 0x5c
    doubleQuote = 0x22

-- | The rules every macOS build's profile holds, after its default
-- refusal (the rules of upstream's @sandbox-defaults.sb@ at 2.36.0).  The
-- parameters are the helper's ('darwinParameters').  @TMPDIR@ is defined
-- as upstream's profile defines it, for a derivation's own text to use.
--
-- Three of upstream's rules are left out, since each reaches the
-- invoking user's own resources, which upstream's build user cannot:
-- writes, programs and Unix sockets in the temporary directory nova-nix
-- runs with, where the user's live sockets and files are
-- ('darwinParameters'); pseudo-terminals, through which a build would
-- read and write the user's terminals ('profileTerminals'); and POSIX
-- shared memory and System V's shared memory and semaphores, through
-- which it would attach the user's own, named or keyed as they are (a
-- PostgreSQL server's System V segment, for one).  POSIX semaphores,
-- which Python's multiprocessing takes, are kept, though a build can open
-- one of the user's by name too.  Undo each when builds run as build
-- users of their own, under a daemon, whose resources are theirs alone.
--
-- Two exposures no rule closes while the builder runs as the invoking
-- user, which build users would: a build may still create System V
-- segments and semaphores, which no rule refuses, though it can neither
-- attach, use nor remove one, so each it creates outlives the build,
-- counts against the system's limit of them (@kern.sysv.shmmni@
-- segments), and may hold a key one of the user's programs wants; and a
-- build may read the command lines of the user's other processes
-- (@KERN_PROCARGS2@), which neither @sysctl-read@ nor
-- @process-info*@ rules reach, where the kernel refuses a build user
-- another user's.
profileBase :: ByteString
profileBase =
  BS8.unlines
    [ "; Scratch space: /tmp itself and the build's own directory, where the",
      "; build may also use Unix sockets.",
      "(define TMPDIR (param \"_GLOBAL_TMP_DIR\"))",
      "(allow file* process-exec network-outbound network-inbound",
      "\t(literal \"/tmp\")",
      "\t(subpath (param \"_NIX_BUILD_TOP\")))",
      "; Processes: the build may start them and signal its own, and may",
      "; make no file setuid or setgid.",
      "(allow process-fork)",
      "(allow signal (target same-sandbox))",
      "(deny file-write-setugid)",
      "; System information, POSIX semaphores, sockets, and user and group",
      "; lookups.",
      "(allow sysctl-read)",
      "(allow ipc-posix-sem)",
      "(allow system-socket)",
      "(allow mach-lookup (global-name \"com.apple.system.opendirectoryd.libinfo\"))",
      "; Devices: the standard ones and DTrace's helper.",
      "(allow file*",
      "\t(literal \"/dev/null\")",
      "\t(literal \"/dev/zero\")",
      "\t(literal \"/dev/random\")",
      "\t(literal \"/dev/urandom\")",
      "\t(literal \"/dev/tty\")",
      "\t(literal \"/dev/stdin\")",
      "\t(literal \"/dev/stdout\")",
      "\t(literal \"/dev/stderr\")",
      "\t(subpath \"/dev/fd\")",
      "\t(literal \"/dev/dtracehelper\"))",
      "; The shell /bin/sh selects.",
      "(allow file* (literal \"/private/var/select/sh\"))",
      "; System files programs read: the system's version, time zones,",
      "; locales, and Rosetta, which runs a program built for the other",
      "; architecture.",
      "(allow file-read*",
      "\t(literal \"/System/Library/CoreServices/SystemVersion.plist\")",
      "\t(literal \"/System/Library/CoreServices/SystemVersionCompat.plist\")",
      "\t(subpath \"/usr/share/zoneinfo\")",
      "\t(subpath \"/usr/share/locale\")",
      "\t(subpath \"/Library/Apple/usr/libexec/oah\")",
      "\t(subpath \"/System/Library/Apple/usr/libexec/oah\")",
      "\t(subpath \"/System/Library/LaunchDaemons/com.apple.oahd.plist\")",
      "\t(subpath \"/Library/Apple/System/Library/LaunchDaemons/com.apple.oahd.plist\"))",
      "; The attributes of /dev, and of /etc and /var, links a path through",
      "; them follows.",
      "(allow file-read-metadata",
      "\t(literal \"/dev\")",
      "\t(literal \"/etc\")",
      "\t(literal \"/var\")",
      "\t(literal \"/private/var/tmp\"))",
      "; Loopback networking, with the name lookups it takes, for a build",
      "; that asks for it.",
      "(if (param \"_ALLOW_LOCAL_NETWORKING\")",
      "    (begin",
      "      (allow network* (remote ip \"localhost:*\"))",
      "      (allow network-inbound (local ip \"*:*\"))",
      "      (allow file-read-metadata",
      "\t(literal \"/etc/resolv.conf\")",
      "\t(literal \"/private/etc/resolv.conf\")",
      "\t(literal \"/etc/hosts\"))",
      "      (allow file-read*",
      "\t(literal \"/private/var/run/resolv.conf\")",
      "\t(literal \"/private/etc/hosts\"))",
      "      (allow network-outbound (remote unix-socket (path-literal \"/private/var/run/mDNSResponder\")))))"
    ]

-- | The build's mark, by which the helper finds the build's processes to
-- end them: a Mach service name the profile allows and one it refuses,
-- each random and made by the helper for this build alone.  No service
-- holds either name, so the first grants nothing.  Under @sandbox = true@
-- nothing else is in the profile, and no process of the build can change
-- its sandbox, so the mark holds.  Under @relaxed@ the rules come after a
-- derivation's own text, and text that would swallow them leaves a
-- profile macOS's sandbox refuses, but that text can redefine the
-- profile's own forms (@param@, @allow@, @deny@) and so drop the mark,
-- leaving its processes to outlive the build; @relaxed@ trusts the
-- derivation as it does one that asks for no sandbox at all.  Undo with
-- the helper's sweep, when builds run as build users of their own, under
-- a daemon.
profileMark :: ByteString
profileMark =
  BS8.unlines
    [ "; The build's mark, which the helper finds its processes by.",
      "(allow mach-lookup (global-name (param \"_NOVA_NIX_BUILD_TOKEN\")))",
      "(deny mach-lookup (global-name (param \"_NOVA_NIX_BUILD_DECOY\")))"
    ]

-- | Text with a newline at its end, unless it is empty or has one, so
-- what follows it starts a line of its own rather than ending a comment.
endLine :: ByteString -> ByteString
endLine text
  | BS.null text || "\n" `BS.isSuffixOf` text = text
  | otherwise = text <> "\n"

-- | The refusal of the terminals, after every rule that could allow one:
-- a terminal's device is the invoking user's, as the console's is, and
-- an @__impureHostDeps@ path @\/dev@, which the profile lets a build read
-- beneath, would let it read what the user types there.  No earlier rule
-- lets a build write beneath @\/dev@ but to its standard devices, and
-- opening @\/dev\/ptmx@ takes writing as well as reading, so the deny's
-- write half, and its line for @\/dev\/ptmx@, refuse nothing today; they
-- keep the terminals refused should an earlier rule widen.
-- @\/dev\/tty@, a process's controlling terminal, stays allowed, since a
-- builder leads a session with none.  Undo when builds run as build users
-- of their own, under a daemon.
profileTerminals :: ByteString
profileTerminals =
  BS8.unlines
    [ "; Terminals, which are the user's.",
      "(deny file-read* file-write*",
      "\t(literal \"/dev/ptmx\")",
      "\t(regex #\"^/dev/pty\")",
      "\t(regex #\"^/dev/tty.\")",
      "\t(literal \"/dev/console\"))"
    ]

-- | The rules a fixed-output build's profile adds (the rules of upstream's
-- @sandbox-network.sb@ at 2.36.0): any network, name lookups, and the
-- certificate checks of trustd.
profileNetwork :: ByteString
profileNetwork =
  BS8.unlines
    [ "; The network, for a fixed-output build, with name lookups and",
      "; certificate checks.",
      "(allow network* (local ip) (remote ip))",
      "(allow file-read-metadata",
      "\t(literal \"/etc/resolv.conf\")",
      "\t(literal \"/private/etc/resolv.conf\"))",
      "(allow file-read* (literal \"/private/var/run/resolv.conf\"))",
      "(allow network-outbound (remote unix-socket (path-literal \"/private/var/run/mDNSResponder\")))",
      "(allow mach-lookup",
      "\t(global-name \"com.apple.SystemConfiguration.DNSConfiguration\")",
      "\t(global-name \"com.apple.trustd\")",
      "\t(global-name \"com.apple.trustd.agent\"))"
    ]

-- ---------------------------------------------------------------------------
-- The macOS plan
-- ---------------------------------------------------------------------------

-- | What the helper does for one build on macOS.
data DarwinPlan = DarwinPlan
  { -- | The build directory, resolved as the profile names it, where the
    -- builder starts.
    darwinPlanBuildDir :: !ByteString,
    darwinPlanProfile :: !ByteString,
    -- | The values of the parameters the profile reads, by name.
    darwinPlanParameters :: ![(ByteString, ByteString)],
    -- | The builder's environment, as @NAME=VALUE@ entries.
    darwinPlanEnvironment :: ![ByteString]
  }
  deriving (Eq, Show)

-- | The parameters a macOS build's profile reads, as upstream names them
-- (@DarwinDerivationBuilder::setUser@), given the build directory,
-- resolved: @_NIX_BUILD_TOP@, the build directory; @_GLOBAL_TMP_DIR@, the
-- build directory too, without trailing slashes, which a subpath filter
-- does not match through; and @_ALLOW_LOCAL_NETWORKING@ for a derivation
-- that asks for it.
--
-- Upstream's @_GLOBAL_TMP_DIR@ is the temporary directory Nix runs with,
-- @TMPDIR@ or @\/tmp@, where its build user owns nothing.  nova-nix's
-- builder runs as the invoking user, whose live Unix sockets and files are
-- there, so a build is given its own directory in its place, which its
-- @TMPDIR@ names already.  Undo when builds run as build users of their
-- own, under a daemon.
darwinParameters :: ByteString -> LocalNetworking -> [(ByteString, ByteString)]
darwinParameters buildDir local =
  [("_NIX_BUILD_TOP", buildDir), ("_GLOBAL_TMP_DIR", BS8.dropWhileEnd (== '/') buildDir)]
    ++ [("_ALLOW_LOCAL_NETWORKING", "1") | local == LocalNetworkingAllowed]

-- | A macOS plan as the helper reads it ('framePlan'): the backend, the
-- build directory and the profile, then each parameter's name and value,
-- then the builder's environment.
renderDarwinPlan :: DarwinPlan -> ByteString
renderDarwinPlan plan =
  framePlan
    ( concat
        [ ["backend", "darwin"],
          ["build-dir", darwinPlanBuildDir plan],
          ["profile", darwinPlanProfile plan]
        ]
        ++ concat [["param", name, value] | (name, value) <- darwinPlanParameters plan]
        ++ concatMap (\entry -> ["env", entry]) (darwinPlanEnvironment plan)
    )

-- ---------------------------------------------------------------------------
-- What the helper reports
-- ---------------------------------------------------------------------------

-- | A step of @nova-nix-sandbox run@ that can fail.
data SetupStage
  = StageStdout
  | StageSigchld
  | StagePlan
  | StageStdin
  | StageSetgroups
  | StageSync
  | StageSignals
  | StageClone
  | StageUidMap
  | StageSetgroupsDeny
  | StageGidMap
  | StageRelease
  | StagePdeathsig
  | StageSetsid
  | StageLoopbackSocket
  | StageLoopback
  | StageHostname
  | StageDomainname
  | StagePrivate
  | StageBindRoot
  | StageBindSource
  | StageBindTarget
  | StageBind
  | StageRemount
  | StageProc
  | StageDevShm
  | StageDevpts
  | StageEtc
  | StageChdirRoot
  | StageRealRoot
  | StagePivot
  | StageChroot
  | StageUmount
  | StageRmdir
  | StageSetgid
  | StageSetuid
  | StageChdir
  | StageKeyring
  | StageNoNewPrivs
  | StageExec
  deriving (Eq, Show, Enum, Bounded)

-- | The token the helper writes for a stage.
setupStageName :: SetupStage -> ByteString
setupStageName stage = case stage of
  StageStdout -> "stdout"
  StageSigchld -> "sigchld"
  StagePlan -> "plan"
  StageStdin -> "stdin"
  StageSetgroups -> "setgroups"
  StageSync -> "sync"
  StageSignals -> "signals"
  StageClone -> "clone"
  StageUidMap -> "uid-map"
  StageSetgroupsDeny -> "setgroups-deny"
  StageGidMap -> "gid-map"
  StageRelease -> "release"
  StagePdeathsig -> "pdeathsig"
  StageSetsid -> "setsid"
  StageLoopbackSocket -> "loopback-socket"
  StageLoopback -> "loopback"
  StageHostname -> "hostname"
  StageDomainname -> "domainname"
  StagePrivate -> "private"
  StageBindRoot -> "bind-root"
  StageBindSource -> "bind-source"
  StageBindTarget -> "bind-target"
  StageBind -> "bind"
  StageRemount -> "remount"
  StageProc -> "proc"
  StageDevShm -> "dev-shm"
  StageDevpts -> "devpts"
  StageEtc -> "etc"
  StageChdirRoot -> "chdir-root"
  StageRealRoot -> "real-root"
  StagePivot -> "pivot"
  StageChroot -> "chroot"
  StageUmount -> "umount"
  StageRmdir -> "rmdir"
  StageSetgid -> "setgid"
  StageSetuid -> "setuid"
  StageChdir -> "chdir"
  StageKeyring -> "keyring"
  StageNoNewPrivs -> "no-new-privs"
  StageExec -> "exec"

-- | A check of @nova-nix-sandbox probe@ that can fail.
data ProbeStage
  = ProbeSigchld
  | ProbeUsernsMissing
  | ProbeUsernsMax
  | ProbeUsernsClone
  | ProbeUserns
  | ProbeAppArmor
  | ProbeMountns
  | ProbeProcRemount
  | -- | macOS's sandbox refused the probe's profile.
    ProbeSandbox
  deriving (Eq, Show, Enum, Bounded)

-- | The token the helper writes for a probe check.
probeStageName :: ProbeStage -> ByteString
probeStageName stage = case stage of
  ProbeSigchld -> "sigchld"
  ProbeUsernsMissing -> "userns-missing"
  ProbeUsernsMax -> "userns-max"
  ProbeUsernsClone -> "userns-clone"
  ProbeUserns -> "userns"
  ProbeAppArmor -> "apparmor"
  ProbeMountns -> "mountns"
  ProbeProcRemount -> "proc-remount"
  ProbeSandbox -> "sandbox"

-- | The helper's report of a step that failed: the step, the @errno@ it
-- failed with (0 for none), and the plan entry or process it concerns.
data StatusRecord stage = StatusRecord
  { recordStage :: !stage,
    recordErrno :: !Int,
    recordIndex :: !(Maybe Int)
  }
  deriving (Eq, Show)

-- | Read what the helper wrote on its status output: nothing, which means
-- it did not fail, or one record.  Anything else is the refusal of a
-- status that cannot be read, since a helper that wrote something it
-- should not have has not run as this module expects.
parseStatus :: (Enum stage, Bounded stage) => (stage -> ByteString) -> ByteString -> Either Text (Maybe (StatusRecord stage))
parseStatus stageName bytes
  | BS.null bytes = Right Nothing
  | otherwise = case BS8.lines bytes of
      [line]
        | "\n" `BS.isSuffixOf` bytes,
          [name, errnoText, indexText] <- BS8.split ' ' line -> do
            stage <- maybe (Left unreadable) Right (lookup name named)
            number <- maybe (Left unreadable) Right (decimal errnoText)
            index <- maybe (Left unreadable) Right (indexOf indexText)
            Right (Just (StatusRecord stage number index))
      _ -> Left unreadable
  where
    named = [(stageName stage, stage) | stage <- [minBound .. maxBound]]
    unreadable = "the sandbox helper wrote a status that cannot be read: '" <> TE.decodeUtf8Lenient bytes <> "'"
    -- A record's numbers are an errno and an index, so one past a 32-bit
    -- int is no record the helper writes, and not read as a wrapped Int.
    decimal text
      | not (BS.null text), BS8.all (`elem` ['0' .. '9']) text = readMaybe (BS8.unpack text) >>= inRecordRange
      | otherwise = Nothing
    inRecordRange :: Integer -> Maybe Int
    inRecordRange number
      | number <= toInteger (maxBound :: Int32) = Just (fromInteger number)
      | otherwise = Nothing
    indexOf text
      | text == "-1" = Just Nothing
      | otherwise = Just <$> decimal text

-- | Why a sandboxed build cannot run in a sandbox on this system.
data Unavailability
  = -- | The platform has no sandbox nova-nix can make.
    PlatformUnsupported
  | -- | The helper was not found.
    HelperNotFound
  | -- | The helper could not be run, for the reason given.
    HelperUnrunnable !Text
  | -- | A check of the probe failed.
    ProbeFailed !(StatusRecord ProbeStage)
  | -- | The probe's report could not be read.
    ProbeUnreadable !Text
  deriving (Eq, Show)

-- | The cause, in a line.  Upstream logs its probe's at debug level
-- (@userNamespacesSupported@ and @mountAndPidNamespacesSupported@ in
-- namespaces.cc at 2.24.9, linux-namespaces.cc at 2.36.0); these are its
-- words.
unavailabilityCause :: Unavailability -> Text
unavailabilityCause cause = case cause of
  PlatformUnsupported -> "sandboxing builds is not supported on this platform"
  HelperNotFound -> "the sandbox helper 'nova-nix-sandbox' was not found beside nova-nix"
  HelperUnrunnable reason -> "the sandbox helper could not be run: " <> reason
  ProbeUnreadable reason -> reason
  ProbeFailed record -> case recordStage record of
    ProbeSigchld -> "the sandbox helper cannot reset SIGCHLD" <> errnoSuffix (recordErrno record)
    ProbeUsernsMissing -> "'/proc/self/ns/user' does not exist; your kernel was likely built without CONFIG_USER_NS=y"
    ProbeUsernsMax -> "user namespaces appear to be disabled; check '/proc/sys/user/max_user_namespaces'"
    ProbeUsernsClone -> "user namespaces appear to be disabled; check '/proc/sys/kernel/unprivileged_userns_clone'"
    ProbeUserns -> "user namespaces do not work on this system" <> errnoSuffix (recordErrno record)
    ProbeAppArmor -> "AppArmor restricts unprivileged user namespaces ('/proc/sys/kernel/apparmor_restrict_unprivileged_userns' is 1), and no AppArmor profile lets nova-nix-sandbox use them"
    ProbeMountns -> "mount namespaces do not work on this system" <> errnoSuffix (recordErrno record)
    ProbeProcRemount -> "PID namespaces do not work on this system: cannot remount /proc"
    ProbeSandbox -> "macOS's sandbox refuses a profile from nova-nix's process, as it refuses one from a process already in a sandbox"

-- | Upstream's refusal of a sandboxed build on a system without the
-- namespaces, with @sandbox-fallback@ off (@tryLocalBuild@ in LDG,
-- @makeDerivationBuilder@ in unix-derivation-builder.cc at 2.36.0), or
-- for a cause upstream has no probe for, that cause.  Under the macOS
-- backend a failed probe is upstream's refusal of a profile macOS's
-- sandbox would not take (@DarwinDerivationBuilder::setUser@), which fails
-- the build whatever @sandbox-fallback@ says ('backendFallback').
unavailableMessage :: Maybe SandboxBackend -> Unavailability -> Text
unavailableMessage backend cause = case (backend, cause) of
  (Just DarwinBackend, ProbeFailed _) -> profileRefused
  (Just DarwinBackend, ProbeUnreadable _) -> profileRefused
  (_, ProbeFailed _) -> namespacesUnsupported
  (_, ProbeUnreadable _) -> namespacesUnsupported
  _ -> ownCause
  where
    profileRefused = setupContext <> "failed to configure sandbox"
    ownCause = unavailabilityCause cause <> "; use '--no-sandbox' to disable sandboxing"
    namespacesUnsupported = "this system does not support the kernel namespaces that are required for sandboxing; use '--no-sandbox' to disable sandboxing"

-- | The one warning a process prints when its builds fall back to running
-- unsandboxed, naming why.  Upstream says nothing there (@tryLocalBuild@ in
-- LDG), but its root daemon rarely falls back, where nova-nix without one
-- falls back on every stock Ubuntu.
fallbackWarning :: Unavailability -> Text
fallbackWarning cause = "warning: building without a sandbox, since " <> unavailabilityCause cause

-- | A failed setup step: the words of the @SysError@ upstream's
-- @startBuilder@ or @runChild@ throws for the step (LDG), with errno's
-- text after them, behind the line of context upstream adds to a failure
-- from the child (@startBuilder@ in LDG).  Upstream prints that context as a
-- trace line and then the error after an @error:@ of its own; here the two
-- are one line.  The plan names the bind an index points at and the host
-- directory bound as the build directory, and the program is the one the
-- helper executes.
setupFailureMessage :: SandboxPlan -> ByteString -> StatusRecord SetupStage -> Text
setupFailureMessage plan program record =
  setupContext <> case recordStage record of
    StageStdout -> sys "cannot dup stderr into stdout"
    StageSigchld -> sys "resetting SIGCHLD"
    StagePlan -> sys "the sandbox helper cannot read its plan"
    StageStdin -> sys "cannot open '/dev/null'"
    StageSetgroups
      | Errno (fromIntegral (recordErrno record)) == ePERM -> "setgroups failed. Set the require-drop-supplementary-groups option to false to skip this step."
      | otherwise -> sys "setgroups failed"
    StageSync -> sys "creating pipe"
    StageSignals -> sys "setting signal handlers"
    StageClone -> "unable to start build process"
    StageUidMap -> sys ("opening file '" <> procFile "uid_map" <> "'")
    StageSetgroupsDeny -> sys ("opening file '" <> procFile "setgroups" <> "'")
    StageGidMap -> sys ("opening file '" <> procFile "gid_map" <> "'")
    StageRelease -> sys "writing to the build process"
    StagePdeathsig -> sys "setting death signal"
    StageSetsid -> sys "creating a new session"
    StageLoopbackSocket -> sys "cannot open IP socket"
    StageLoopback -> sys "cannot set loopback interface flags"
    StageHostname -> sys "cannot set host name"
    StageDomainname -> sys "cannot set domain name"
    StagePrivate -> sys "unable to make '/' private"
    StageBindRoot -> sys ("unable to bind mount '" <> shown (planRoot plan) <> "'")
    StageBindSource -> sys ("getting attributes of path '" <> maybe "" (shown . bindSource) bind <> "'")
    StageBindTarget -> sys ("creating directory '" <> inRoot (maybe "" bindTarget bind) <> "'")
    StageBind -> sys ("bind mount from '" <> maybe "" (shown . bindSource) bind <> "' to '" <> inRoot (maybe "" bindTarget bind) <> "' failed")
    StageRemount -> sys ("cannot remount '" <> inRoot (maybe "" bindTarget bind) <> "' read-only")
    StageProc -> sys "mounting /proc"
    StageDevShm -> sys "mounting /dev/shm"
    StageDevpts -> sys "mounting /dev/pts"
    StageEtc -> sys ("setting permissions on '" <> inRoot "/etc" <> "'")
    StageChdirRoot -> sys ("cannot change directory to '" <> shown (planRoot plan) <> "'")
    StageRealRoot -> sys "cannot create real-root directory"
    StagePivot -> sys ("cannot pivot old root directory onto '" <> inRoot "/real-root" <> "'")
    StageChroot -> sys ("cannot change root directory to '" <> shown (planRoot plan) <> "'")
    StageUmount -> sys "cannot unmount real root filesystem"
    StageRmdir -> sys "cannot remove real-root directory"
    StageSetgid -> sys "setgid failed"
    StageSetuid -> sys "setuid failed"
    StageChdir -> sys ("changing into '" <> shown hostBuildDir <> "'")
    StageKeyring -> sys "unable to join a new session keyring"
    StageNoNewPrivs -> sys "unable to set 'no new privileges'"
    StageExec -> sys ("executing '" <> shown program <> "'")
  where
    sys message = message <> errnoSuffix (recordErrno record)
    bind = recordIndex record >>= \index -> lookup index (zip [0 ..] (planBinds plan))
    -- Upstream names the build directory on the host there (@runChild@ in
    -- LDG).
    hostBuildDir = maybe (planBuildDir plan) bindSource (find ((== planBuildDir plan) . bindTarget) (planBinds plan))
    procFile name = "/proc/" <> maybe "" (T.pack . show) (recordIndex record) <> "/" <> name
    inRoot path = shown (planRoot plan <> path)
    shown = TE.decodeUtf8Lenient

-- | The line of context upstream puts before a failure of the builder's
-- process before its exec (@LocalDerivationGoal::startBuilder@ at 2.24.9,
-- @UnixDerivationBuilderImpl::processSandboxSetupMessages@ at 2.36.0).
setupContext :: Text
setupContext = "while setting up the build environment: "

-- | A step of @nova-nix-sandbox run@ that can fail on macOS.
data DarwinStage
  = DarwinStageStdout
  | DarwinStageSigchld
  | DarwinStagePlan
  | DarwinStageStdin
  | DarwinStageLog
  | DarwinStageLogMode
  | DarwinStageLogWatch
  | DarwinStageOutput
  | DarwinStageSweep
  | DarwinStageSignals
  | DarwinStageFork
  | DarwinStageSetsid
  | DarwinStageChdir
  | DarwinStageSandbox
  | DarwinStageExec
  deriving (Eq, Show, Enum, Bounded)

-- | The token the helper writes for a macOS stage.
darwinStageName :: DarwinStage -> ByteString
darwinStageName stage = case stage of
  DarwinStageStdout -> "stdout"
  DarwinStageSigchld -> "sigchld"
  DarwinStagePlan -> "plan"
  DarwinStageStdin -> "stdin"
  DarwinStageLog -> "log"
  DarwinStageLogMode -> "logmode"
  DarwinStageLogWatch -> "logwatch"
  DarwinStageOutput -> "output"
  DarwinStageSweep -> "sweep"
  DarwinStageSignals -> "signals"
  DarwinStageFork -> "fork"
  DarwinStageSetsid -> "setsid"
  DarwinStageChdir -> "chdir"
  DarwinStageSandbox -> "sandbox"
  DarwinStageExec -> "exec"

-- | A failed macOS setup step, as 'setupFailureMessage' reports a Linux
-- one, in the words of upstream's errors at 2.36.0 (in
-- unix-derivation-builder.cc, darwin-derivation-builder.cc, child.cc,
-- file-descriptor.cc and processes.cc there), given the plan and the
-- program the helper
-- executes.  A profile macOS's sandbox refuses is reported with the words
-- of upstream's line for it, @failed to configure sandbox@
-- (@DarwinDerivationBuilder::setUser@),
-- which the helper writes to the build log with the sandbox's own reason
-- after them.  Upstream reads that line as one of the child's setup
-- messages and reports the end of its pipe instead, while waiting for the
-- build environment to initialize, the line among its previous messages
-- (@UnixDerivationBuilderImpl::processSandboxSetupMessages@).  A failure
-- to watch the build log is not a setup step: the helper reports it once
-- the build has ended, on the status channel, since the log it could not
-- watch cannot carry it.
darwinSetupFailureMessage :: DarwinPlan -> ByteString -> StatusRecord DarwinStage -> Text
darwinSetupFailureMessage plan program record = case recordStage record of
  DarwinStageStdout -> setup (sys "cannot dup stderr into stdout")
  DarwinStageSigchld -> setup (sys "resetting SIGCHLD")
  DarwinStagePlan -> setup (sys "the sandbox helper cannot read its plan")
  DarwinStageStdin -> setup (sys "opening '/dev/null'")
  DarwinStageLog -> setup (sys "preparing the pipe the builder writes its log to")
  DarwinStageLogMode -> setup (sys "making writes to the build log non-blocking")
  DarwinStageLogWatch -> sys "the sandbox helper cannot watch the build log, which must be pollable (a pipe or a regular file, not /dev/null)"
  DarwinStageOutput -> setup (sys "redirecting the builder's output to its log pipe")
  DarwinStageSweep -> setup (sys "the sandbox helper cannot prepare to end the build's processes")
  DarwinStageSignals -> setup (sys "setting signal handlers")
  DarwinStageFork -> setup (sys "unable to fork")
  DarwinStageSetsid -> setup (sys "creating a new session")
  DarwinStageChdir -> setup (sys ("changing into \"" <> TE.decodeUtf8Lenient (darwinPlanBuildDir plan) <> "\""))
  DarwinStageSandbox -> setup "failed to configure sandbox"
  DarwinStageExec -> setup (sys ("executing '" <> TE.decodeUtf8Lenient program <> "'"))
  where
    setup = (setupContext <>)
    sys message = message <> errnoSuffix (recordErrno record)

-- | What the helper's run of a builder came to.
data HelperOutcome
  = -- | The sandbox was made and the builder ran, exiting so.
    BuilderRan !ExitCode
  | -- | The sandbox was not made, for the reason given.
    SetupIncomplete !Text
  deriving (Eq, Show)

-- | Read a run of the helper, given its backend's stage names and its
-- report of a failed step ('setupFailureMessage',
-- 'darwinSetupFailureMessage'), whether the plan reached it, how it
-- exited and what it wrote on its status output.  A record is the step
-- that failed.  With none, the exit is the builder's, which the helper
-- relays, unless the plan never reached it: then the helper ended before
-- it could report anything.
helperOutcome :: (Enum stage, Bounded stage) => (stage -> ByteString) -> (StatusRecord stage -> Text) -> Either Text () -> ExitCode -> ByteString -> HelperOutcome
helperOutcome stageName failureMessage delivered exitCode status = case parseStatus stageName status of
  Left unreadable -> SetupIncomplete unreadable
  Right (Just record) -> SetupIncomplete (failureMessage record)
  Right Nothing -> case delivered of
    Left failure -> SetupIncomplete ("the sandbox helper ended before it read its plan: " <> failure)
    Right () -> BuilderRan exitCode

-- | @": "@ and the system's text for an @errno@, as upstream's @SysError@
-- appends it, or nothing for none.
errnoSuffix :: Int -> Text
errnoSuffix 0 = ""
errnoSuffix number = ": " <> T.pack (ioe_description (errnoToIOError "" (Errno (fromIntegral number)) Nothing Nothing))
