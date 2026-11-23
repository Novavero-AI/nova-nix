-- | What the build sandbox is, worked out without IO: its settings, whether
-- a build is sandboxed, which network it gets, the plan the
-- @nova-nix-sandbox@ helper carries out, the chroot it carries it out in,
-- and the helper's report of a step that failed.
--
-- == The sandbox
--
-- On Linux a sandboxed build runs in fresh user, mount, PID, IPC and UTS
-- namespaces, and a network namespace of its own unless it is
-- fixed-output, with a chroot that holds its inputs, as upstream's
-- @LocalDerivationGoal@ builds one (local-derivation-goal.cc at 2.24.9,
-- cited here as LDG).  The work is split three ways: this module decides
-- and plans; "Nix.Builder.Sandbox.Spawn" makes the chroot's skeleton,
-- probes the system and runs the helper; and the helper, a C program of
-- its own (@sandbox\/nova-nix-sandbox.c@), makes the namespaces, the
-- mounts and the root, which happens between the namespaces' creation and
-- the builder's exec, where the process library runs nothing.
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
    SandboxConfig (..),
    defaultSandboxConfig,

    -- * The decision
    SandboxUse (..),
    sandboxUse,
    Network (..),
    buildNetwork,

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

    -- * The chroot
    SkeletonEntry (..),
    chrootSkeleton,
    passwdFile,
    groupFile,

    -- * What the helper reports
    SetupStage (..),
    setupStageName,
    ProbeStage (..),
    probeStageName,
    StatusRecord (..),
    parseStatus,
    Unavailability (..),
    unavailabilityCause,
    unavailableMessage,
    setupFailureMessage,
    fallbackWarning,
    HelperOutcome (..),
    helperOutcome,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Int (Int32)
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word32)
import Foreign.C.Error (Errno (..), ePERM, errnoToIOError)
import GHC.IO.Exception (IOException (ioe_description))
import Nix.Derivation (Derivation, fixedOutput)
import Nix.Realise (derivationAttrs, derivationBoolAttr)
import Nix.Store.Path (StorePath, defaultStoreDir, storePathToText)
import System.Exit (ExitCode)
import Text.Read (readMaybe)

-- ---------------------------------------------------------------------------
-- Settings
-- ---------------------------------------------------------------------------

-- | Upstream's @sandbox@ setting (globals.hh:597-630 at 2.24.9).
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

-- | Upstream's @BaseSetting\<SandboxMode\>::parse@ (globals.cc:282-288):
-- @true@, @relaxed@ or @false@, and nothing else, not even the other
-- Boolean spellings.
parseSandboxMode :: ByteString -> Either Text SandboxMode
parseSandboxMode value = case value of
  "true" -> Right SandboxEnabled
  "relaxed" -> Right SandboxRelaxed
  "false" -> Right SandboxDisabled
  _ -> Left ("option '" <> TE.decodeUtf8Lenient sandboxSettingName <> "' has invalid value '" <> TE.decodeUtf8Lenient value <> "'")

-- | Upstream's @sandbox-fallback@: what a sandboxed build does on a system
-- that cannot make the namespaces.
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

-- | Upstream's reading of @sandbox-paths@ (LDG 624-636): in the set's
-- order, an empty entry skipped, a trailing @?@ making the source
-- optional, and @target=source@ split at the first @=@, a bare path
-- binding itself.  A later entry for a target replaces an earlier one
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

-- | How builds are sandboxed: the settings, and where the helper is.
data SandboxConfig = SandboxConfig
  { scMode :: !SandboxMode,
    scFallback :: !SandboxFallback,
    -- | The @sandbox-paths@ entries, read ('parseSandboxPaths').
    scPaths :: ![SandboxPath],
    -- | Upstream's @sandbox-build-dir@: where the builder sees its build
    -- directory.
    scBuildDir :: !ByteString,
    -- | Upstream's @sandbox-dev-shm-size@: the size of the tmpfs at
    -- @\/dev\/shm@, in mount(8)'s @size@ syntax.
    scDevShmSize :: !ByteString,
    scDropGroups :: !DropGroups,
    -- | Upstream's @ssl-cert-file@: the CA bundle a fixed-output build sees
    -- at @\/etc\/ssl\/certs\/ca-certificates.crt@, when there is one.
    scCaFile :: !(Maybe ByteString),
    -- | Where the @nova-nix-sandbox@ helper is, when it was found.
    scHelper :: !(Maybe FilePath)
  }
  deriving (Eq, Show)

-- | No build sandboxed, and otherwise upstream's defaults: fallback on, no
-- paths, @\/build@, half of memory for @\/dev\/shm@, kept groups tolerated,
-- and no CA bundle or helper, which only IO can find.
defaultSandboxConfig :: SandboxConfig
defaultSandboxConfig =
  SandboxConfig
    { scMode = SandboxDisabled,
      scFallback = FallBackUnsandboxed,
      scPaths = [],
      scBuildDir = "/build",
      scDevShmSize = "50%",
      scDropGroups = TolerateKeptGroups,
      scCaFile = Nothing,
      scHelper = Nothing
    }

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
-- derivation type leaves it unsandboxed (derivations.cc:99-112 and
-- 734-748 at 2.24.9); every other build gets a network of its own
-- (LDG 937-938).  'fixedOutput' is the rule the @impureEnvVars@ carve-out
-- follows too.
buildNetwork :: Derivation -> Network
buildNetwork drv = case fixedOutput drv of
  Just _ -> HostNetwork
  Nothing -> PrivateNetwork

-- | Upstream's choice of a chroot build (LDG 196-214), for the derivation
-- at a @.drv@ path.  Under @sandbox = true@ every build is sandboxed and a
-- derivation setting @__noChroot@ is refused; under @relaxed@ one that is
-- fixed-output or sets @__noChroot@ is not sandboxed.
sandboxUse :: SandboxMode -> StorePath -> Derivation -> Either Text SandboxUse
sandboxUse mode drvPath drv = case mode of
  SandboxDisabled -> Right Unsandboxed
  SandboxEnabled -> do
    noChroot <- noChrootSet
    case noChroot of
      True -> Left ("derivation '" <> storePathToText defaultStoreDir drvPath <> "' has '" <> noChrootAttr <> "' set, but that's not allowed when 'sandbox' is 'true'")
      False -> Right (Sandboxed (buildNetwork drv))
  SandboxRelaxed -> do
    noChroot <- noChrootSet
    pure $ case (fixedOutput drv, noChroot) of
      (Nothing, False) -> Sandboxed PrivateNetwork
      _ -> Unsandboxed
  where
    noChrootSet = derivationAttrs drvPath drv >>= derivationBoolAttr drvPath noChrootAttr False

-- | The attribute a derivation asks to build outside the sandbox with.
noChrootAttr :: Text
noChrootAttr = "__noChroot"

-- ---------------------------------------------------------------------------
-- The plan
-- ---------------------------------------------------------------------------

-- | The ids a build runs as in its user namespace, which upstream uses
-- when it has no build users (local-derivation-goal.hh:140-141).
sandboxUid, sandboxGid :: Word32
sandboxUid = 1000
sandboxGid = 100

-- | The @HOME@ a sandboxed build is told of, upstream's, which does not
-- exist, so a program looking for its settings there finds none
-- (LDG 105 and 1142-1148).
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
-- files it adds (LDG 1863-1868).
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

-- | What the helper does for one build.
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
-- (LDG 1823-1838).  @\/dev\/kvm@ joins them upstream only for a store
-- with the @kvm@ system feature, which nova-nix has no setting for.
deviceNodes :: [ByteString]
deviceNodes = ["/dev/full", "/dev/null", "/dev/random", "/dev/tty", "/dev/urandom", "/dev/zero"]

-- | The host files a fixed-output build sees, where they exist
-- (LDG 1853-1855).
hostNetworkFiles :: [ByteString]
hostNetworkFiles = ["/etc/resolv.conf", "/etc/services", "/etc/hosts"]

-- | Where a fixed-output build sees the CA bundle (LDG 1857-1860).
caBundleTarget :: ByteString
caBundleTarget = "/etc/ssl/certs/ca-certificates.crt"

-- | The path a build sees its whole @\/dev@ at when an entry binds it.
devDir :: ByteString
devDir = "/dev"

-- | The binds of a sandboxed build, built up as upstream builds
-- @pathsInChroot@ (LDG 618-772 and 1820-1868): the @sandbox-paths@
-- entries, a later one for a target replacing an earlier; the build
-- directory; the closure of the entries in the store; the input closure;
-- then no output.  Then, without replacing an entry already there, the
-- device nodes unless @\/dev@ is bound whole, and for a fixed-output
-- build the host's network files and the CA bundle, each optional.
--
-- Every host path is bound read-only, the build directory alone writable,
-- where upstream binds them plainly (LDG 402-433): the builder runs as the
-- invoking user, so a writable bind would let a build change a valid store
-- path, the bundled shell later builds run, an @--exec-wrapper@ launcher,
-- or any file of the user's a @sandbox-paths@ entry names.  A bind is
-- read-only through every mount beneath its source, which a recursive bind
-- brings along.  A device node stays usable on a read-only mount.  The
-- one host path the helper binds writable is outside the plan: where the
-- kernel will not mount a devpts of the sandbox's own, it binds the
-- host's @\/dev\/pts@ and @\/dev\/ptmx@ instead, as upstream does
-- (LDG 1912-1928).
--
-- A build directory that holds the store directory is upstream's error
-- (LDG 637-640).  A target that is not absolute, or has a @..@ component,
-- is refused, since joined to the chroot's root it names a path outside
-- it; upstream joins its targets as they are (LDG 1887).
sandboxPlan :: PlanInputs -> Either Text SandboxPlan
sandboxPlan inputs
  | piBuildDirSeen inputs `BS.isPrefixOf` piStoreDir inputs = Left "`sandbox-build-dir` must not contain the storeDir"
  | Just target <- find leavesRoot (piBuildDirSeen inputs : map spTarget (piSandboxPaths inputs)) =
      Left ("the sandbox path '" <> TE.decodeUtf8Lenient target <> "' is not an absolute path without a '..' component, so it names no path in the sandbox")
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
    leavesRoot target = not ("/" `BS.isPrefixOf` target) || ".." `elem` BS8.split '/' target
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

-- | The first line of a plan, which names the format's version.  The
-- helper refuses any other.
planVersionLine :: ByteString
planVersionLine = "nova-nix-sandbox-plan 3\n"

-- | A plan as the helper reads it: the version line, a line holding the
-- length in bytes of the rest, then each key and its values in one fixed
-- order, every token its length in decimal, a colon, its bytes and a NUL.
-- A token's length says where it ends, so no bytes in a field, a NUL
-- among them, can end it early or start another; the helper, which uses
-- each token as a C string, refuses one that holds a NUL.  The plan's
-- length lets the helper read it and leave the pipe open behind it.  A
-- bind is its target, its source and its flags: @-@ for none, else @r@
-- (read-only), @o@ (optional) and @c@ (resolve the source's symlinks).
-- An @env@ token is one entry of the builder's environment.
renderPlan :: SandboxPlan -> ByteString
renderPlan plan = planVersionLine <> BS8.pack (show (BS.length tokens)) <> "\n" <> tokens
  where
    tokens =
      foldMap
        token
        ( concat
            [ ["root", planRoot plan],
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
    token bytes = BS8.pack (show (BS.length bytes)) <> ":" <> bytes <> "\0"
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

-- | Upstream's chroot before the binds (LDG 711-748 and 1018-1022): a
-- writable @\/tmp@, @\/etc@ with the build user's @passwd@ and @group@ and,
-- for a build with a network of its own, @hosts@, or for a fixed-output
-- one @nsswitch.conf@; the store directory, writable by its owner and
-- sticky; @\/proc@; and unless @\/dev@ is bound whole, @\/dev\/shm@,
-- @\/dev\/pts@ and the links into @\/proc\/self\/fd@ (LDG 1823-1837).  A
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

-- | Upstream's @\/etc\/passwd@ for a build (LDG 1018-1022), whose home is
-- the build directory as the builder sees it.
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

-- | Upstream's @\/etc\/group@ for a build (LDG 730-733).
groupFile :: ByteString
groupFile = BS.concat ["root:x:0:\n", "nixbld:!:", BS8.pack (show sandboxGid), ":\n", "nogroup:x:65534:\n"]

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
-- (namespaces.cc:21-91 at 2.24.9); these are its words.
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

-- | Upstream's refusal of a sandboxed build on a system without the
-- namespaces, with @sandbox-fallback@ off (LDG 228-229), or for a cause
-- upstream has no probe for, that cause.
unavailableMessage :: Unavailability -> Text
unavailableMessage cause = case cause of
  ProbeFailed _ -> namespacesUnsupported
  ProbeUnreadable _ -> namespacesUnsupported
  _ -> unavailabilityCause cause <> "; use '--no-sandbox' to disable sandboxing"
  where
    namespacesUnsupported = "this system does not support the kernel namespaces that are required for sandboxing; use '--no-sandbox' to disable sandboxing"

-- | The one warning a process prints when its builds fall back to running
-- unsandboxed, naming why.  Upstream says nothing there (LDG 225-231),
-- but its root daemon rarely falls back, where nova-nix without one falls
-- back on every stock Ubuntu.
fallbackWarning :: Unavailability -> Text
fallbackWarning cause = "warning: building without a sandbox, since " <> unavailabilityCause cause

-- | A failed setup step: the words of the @SysError@ upstream's
-- @startBuilder@ or @runChild@ throws for the step (LDG), with errno's
-- text after them, behind the line of context upstream adds to a failure
-- from the child (LDG 1075).  Upstream prints that context as a trace line
-- and then the error after an @error:@ of its own; here the two are one
-- line.  The plan names the bind an index points at and the host
-- directory bound as the build directory, and the program is the one the
-- helper executes.
setupFailureMessage :: SandboxPlan -> ByteString -> StatusRecord SetupStage -> Text
setupFailureMessage plan program record =
  "while setting up the build environment: " <> case recordStage record of
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
    -- Upstream names the build directory on the host there (LDG 1986).
    hostBuildDir = maybe (planBuildDir plan) bindSource (find ((== planBuildDir plan) . bindTarget) (planBinds plan))
    procFile name = "/proc/" <> maybe "" (T.pack . show) (recordIndex record) <> "/" <> name
    inRoot path = shown (planRoot plan <> path)
    shown = TE.decodeUtf8Lenient

-- | What the helper's run of a builder came to.
data HelperOutcome
  = -- | The sandbox was made and the builder ran, exiting so.
    BuilderRan !ExitCode
  | -- | The sandbox was not made, for the reason given.
    SetupIncomplete !Text
  deriving (Eq, Show)

-- | Read a run of the helper, given the plan, the program it was to
-- execute, whether the plan reached it, how it exited and what it wrote
-- on its status output.  A record is the step that failed.  With none, the
-- exit is the builder's, which the helper relays, unless the plan never
-- reached it: then the helper ended before it could report anything.
helperOutcome :: SandboxPlan -> ByteString -> Either Text () -> ExitCode -> ByteString -> HelperOutcome
helperOutcome plan program delivered exitCode status = case parseStatus setupStageName status of
  Left unreadable -> SetupIncomplete unreadable
  Right (Just record) -> SetupIncomplete (setupFailureMessage plan program record)
  Right Nothing -> case delivered of
    Left failure -> SetupIncomplete ("the sandbox helper ended before it read its plan: " <> failure)
    Right () -> BuilderRan exitCode

-- | @": "@ and the system's text for an @errno@, as upstream's @SysError@
-- appends it, or nothing for none.
errnoSuffix :: Int -> Text
errnoSuffix 0 = ""
errnoSuffix number = ": " <> T.pack (ioe_description (errnoToIOError "" (Errno (fromIntegral number)) Nothing Nothing))
