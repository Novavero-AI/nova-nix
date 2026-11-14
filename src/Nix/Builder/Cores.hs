{-# LANGUAGE CPP #-}

-- | The default of upstream's @cores@ setting, which a builder's
-- @NIX_BUILD_CORES@ carries when no source sets it: the CPU limit of the
-- process's cgroup on Linux, otherwise the machine's hardware threads
-- (@Settings::getDefaultCores@, globals.cc:172 at Nix 2.24.9).
--
-- The limit is upstream's @getMaxCPU@ (current-process.cc:28): the
-- @cpu.max@ of the process's cgroup v2 group, under the first @cgroup2@
-- mount, as its quota over its period rounded up.  A file upstream cannot
-- read, or a line it cannot parse, gives 0 there, which is no limit, and
-- no limit here; where upstream's number parsing throws past its catch
-- instead, this module still answers no limit ('cpuMaxCores' says
-- when).  Each file is parsed by a pure function, and
-- the reads are an effect the caller passes in ('ReadKernelFile'), so the
-- whole rule runs in tests over an in-memory map.  The choice to read the
-- files only on Linux, where upstream reads them, is the caller's.
module Nix.Builder.Cores
  ( -- * The default
    defaultCores,
    hardwareConcurrency,
    ReadKernelFile,
    cgroupCoreLimit,

    -- * The files
    cgroup2MountPoint,
    parseCgroups,
    cpuMaxCores,
  )
where

import Control.Monad (guard, mfilter, (<=<))
import Data.Bits (toIntegralSized)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (isDigit, ord)
import Data.Int (Int32)
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Word (Word32)
import Nix.HostPath (hostPathFromBytes)
import System.OsPath (OsPath)
#if defined(mingw32_HOST_OS)
import GHC.Conc (getNumProcessors)
#else
import Foreign.C.Types (CLong (..))
#endif

-- ---------------------------------------------------------------------------
-- The default
-- ---------------------------------------------------------------------------

-- | Upstream's @getDefaultCores@: the cgroup's limit when it is positive,
-- otherwise the hardware thread count, raised to 1 as
-- @std::max(1U, std::thread::hardware_concurrency())@ raises it.
defaultCores :: Maybe Word32 -> Word32 -> Word32
defaultCores limit hardware = case limit of
  Just cores | cores > 0 -> cores
  _ -> max 1 hardware

-- | Upstream's @std::thread::hardware_concurrency@ on this machine, which
-- 'defaultCores' raises to 1.  On Linux and macOS it is the call
-- upstream's C++ runtime makes, @sysconf(_SC_NPROCESSORS_ONLN)@
-- (cbits\/nn_cpus.c).  libc++ 16.0.6, which nixpkgs 24.05 builds upstream
-- with on macOS, makes that call.  libstdc++ calls glibc's @get_nprocs@,
-- and glibc's @sysconf@ answers that name with the same function
-- (sysdeps\/posix\/sysconf.c at glibc 2.39), which counts the CPUs
-- @\/sys\/devices\/system\/cpu\/online@ lists.  The glibc is the
-- host's here and nixpkgs' 2.39 upstream; they agree on every release but
-- 2.34, whose @get_nprocs@ counts the process's affinity mask instead.  A
-- negative answer, @sysconf@'s error, is 0, as both runtimes make it.  On
-- Windows, where upstream has no local builder, it is GHC's
-- @getNumProcessors@ ("GHC.Conc").
hardwareConcurrency :: IO Word32
#if defined(mingw32_HOST_OS)
-- The RTS counts in a uint32_t that getNumProcessors widens to an Int, so
-- narrowing it back loses nothing.
hardwareConcurrency = fromIntegral <$> getNumProcessors
#else
hardwareConcurrency = fromMaybe 0 . toIntegralSized <$> c_nn_online_processors

foreign import ccall unsafe "nn_cpus.h nn_online_processors"
  c_nn_online_processors :: IO CLong
#endif

-- | How 'cgroupCoreLimit' reads a kernel file: its bytes, or 'Nothing'
-- when it cannot be read.
type ReadKernelFile m = OsPath -> m (Maybe ByteString)

-- | Upstream's @getMaxCPU@ over the given reads: the cores the @cpu.max@
-- of the process's cgroup v2 group allows, or 'Nothing' when no step
-- reaches a number.  The group is the one @\/proc\/self\/cgroup@ lists
-- under the empty name ('parseCgroups'), and an empty path is none.  Its
-- @cpu.max@ is read at the first @cgroup2@ mount ('cgroup2MountPoint')
-- joined to the group's path with a @\/@ between, which the path's own
-- leading @\/@ doubles, as upstream joins them.
cgroupCoreLimit :: (Monad m) => ReadKernelFile m -> m (Maybe Word32)
cgroupCoreLimit readKernelFile = do
  mountPoint <- (>>= cgroup2MountPoint) <$> readBytes procMountsPath
  whenFound mountPoint $ \mount -> do
    group <- (>>= unifiedGroup) <$> readBytes procSelfCgroupPath
    whenFound group $ \path ->
      (>>= cpuMaxCores) <$> readBytes (BS.intercalate "/" [mount, path, cpuMaxFileName])
  where
    readBytes = maybe (pure Nothing) readKernelFile . hostPathFromBytes
    unifiedGroup = mfilter (not . BS.null) . Map.lookup unifiedHierarchy <=< parseCgroups
    whenFound found next = maybe (pure Nothing) next found

-- ---------------------------------------------------------------------------
-- The files
-- ---------------------------------------------------------------------------

-- | The mount point of the first @cgroup2@ file system in the bytes of
-- @\/proc\/mounts@, as upstream's @getCgroupFS@ (linux/cgroup.cc at
-- 2.24.9) finds it through glibc's @getmntent@ (misc/mntent_r.c and
-- misc/mntent.c at glibc 2.39).  A line is read only as far as the 4095
-- bytes glibc's buffer holds; a line that is blank or whose first byte
-- after spaces and tabs is @#@ is skipped; and the fields are the runs of
-- bytes other than space and tab.  The mount point is decoded as glibc's
-- @decode_name@ decodes it: @\\040@, @\\011@ and @\\012@ are a space,
-- a tab and a newline, @\\134@ and @\\\\@ are a backslash, and any
-- other backslash stands for itself.  The first four are how the kernel
-- writes those bytes in a mount point (@show_vfsmnt@,
-- fs/proc_namespace.c).  glibc decodes the type too, but no escape
-- decodes to a byte @cgroup2@ holds, so the type compares the same
-- undecoded.
cgroup2MountPoint :: ByteString -> Maybe ByteString
cgroup2MountPoint = listToMaybe . mapMaybe (cgroup2Entry . BS.take mntentLineLimit) . BS8.lines
  where
    cgroup2Entry line = case mountFields line of
      _ : dir : fsType : _ | fsType == cgroup2Type -> Just (decodeMountField dir)
      _ -> Nothing
    mountFields line = case BS8.uncons (BS8.dropWhile isMountBlank line) of
      Just (lead, _) | lead == mntentComment -> []
      _ -> filter (not . BS.null) (BS8.splitWith isMountBlank line)
    isMountBlank c = c == ' ' || c == '\t'

-- | glibc's @decode_name@, whose escapes 'cgroup2MountPoint' lists.  In
-- a device or type the kernel also writes @#@ as @\\043@, which glibc
-- leaves as written.
decodeMountField :: ByteString -> ByteString
decodeMountField = BS.concat . chunks
  where
    chunks field = case BS8.break (== '\\') field of
      (plain, escaped) -> plain : maybe [] (escapeChunks . snd) (BS.uncons escaped)
    escapeChunks rest = case find ((`BS.isPrefixOf` rest) . fst) mountEscapes of
      Just (code, decoded) -> decoded : chunks (BS.drop (BS.length code) rest)
      Nothing -> "\\" : chunks rest

-- | What follows a backslash in an escape glibc decodes, and the byte it
-- stands for.
mountEscapes :: [(ByteString, ByteString)]
mountEscapes = [("040", " "), ("011", "\t"), ("012", "\n"), ("\\", "\\"), ("134", "\\")]

-- | Upstream's @getCgroups@ (linux/cgroup.cc at 2.24.9) over the bytes of
-- @\/proc\/self\/cgroup@: each non-empty line is @ID:CONTROLLERS:PATH@ as
-- its @std::regex@ @([0-9]+):([^:]*):(.*)@ matches it, keyed by the
-- controller list with a @name=@ prefix dropped, and a later line replaces
-- an earlier one of the same name.  A line that does not match makes the
-- whole file 'Nothing', as upstream throws.  The regex's grammar is
-- ECMAScript, whose @.@ matches no carriage return, so a path holding one
-- does not match.
parseCgroups :: ByteString -> Maybe (Map ByteString ByteString)
parseCgroups = fmap Map.fromList . traverse cgroupLine . filter (not . BS.null) . BS8.split '\n'
  where
    cgroupLine line = do
      let (hierarchyId, afterId) = BS8.break (== ':') line
      guard (not (BS.null hierarchyId) && BS8.all isDigit hierarchyId)
      (controllers, afterControllers) <- BS8.break (== ':') <$> dropColon afterId
      path <- dropColon afterControllers
      guard (BS8.notElem '\r' path)
      pure (fromMaybe controllers (BS.stripPrefix namedHierarchyPrefix controllers), path)
    dropColon = fmap snd . mfilter ((== ':') . fst) . BS8.uncons

-- | The cores a cgroup's @cpu.max@ allows, as upstream's @getMaxCPU@
-- reads it: exactly two tokens between spaces and newlines, a quota of
-- @max@ meaning no limit, and otherwise
-- @std::ceil(std::stoi(quota) \/ std::stof(period))@, which is 0 for a
-- quota of 0.  That division is in single precision, and so is this one:
-- its rounding decides the count for a quota a little over a whole number
-- of periods, so 20000001 over 1000000 is 20 there and here, not 21.  The
-- kernel writes both as decimal integers.  A token that is not one, a
-- quota past @int@, and a period of 0 are 'Nothing' here.  Upstream
-- instead reads a token's leading number and ignores the rest
-- (@std::stoi@, @std::stof@), throws an exception its @catch (Error &)@
-- does not catch when there is no number or it is out of range, and
-- divides by zero.
cpuMaxCores :: ByteString -> Maybe Word32
cpuMaxCores contents = case filter (not . BS.null) (BS8.splitWith (`BS8.elem` cpuMaxSeparators) contents) of
  [quota, period] | quota /= unlimitedQuota -> do
    quotaMicros <- decimal quota >>= toIntegralSized :: Maybe Int32
    periodMicros <- mfilter (> 0) (decimal period)
    toIntegralSized (ceiling (fromIntegral quotaMicros / fromInteger periodMicros :: Float) :: Integer)
  _ -> Nothing

-- | A whole run of decimal digits as an 'Integer'; anything else, the
-- empty run included, is 'Nothing'.
decimal :: ByteString -> Maybe Integer
decimal digits
  | not (BS.null digits) && BS8.all isDigit digits = Just (BS8.foldl' step 0 digits)
  | otherwise = Nothing
  where
    step acc digit = acc * 10 + toInteger (ord digit - ord '0')

-- ---------------------------------------------------------------------------
-- Names
-- ---------------------------------------------------------------------------

procMountsPath, procSelfCgroupPath, cpuMaxFileName :: ByteString
procMountsPath = "/proc/mounts"
procSelfCgroupPath = "/proc/self/cgroup"
cpuMaxFileName = "cpu.max"

-- | The file system type of the cgroup v2 hierarchy.
cgroup2Type :: ByteString
cgroup2Type = "cgroup2"

-- | The name 'parseCgroups' gives the cgroup v2 hierarchy, whose line in
-- @\/proc\/self\/cgroup@ lists no controllers.
unifiedHierarchy :: ByteString
unifiedHierarchy = ""

-- | What a named cgroup v1 hierarchy's controller list starts with.
namedHierarchyPrefix :: ByteString
namedHierarchyPrefix = "name="

-- | The quota @cpu.max@ holds for a group with no limit.
unlimitedQuota :: ByteString
unlimitedQuota = "max"

-- | The separators upstream's @tokenizeString@ splits @cpu.max@ at.
cpuMaxSeparators :: ByteString
cpuMaxSeparators = " \n"

-- | The most of a line @getmntent@ reads: its buffer is 4096 bytes, less
-- the NUL @fgets@ ends it with, and the rest of a longer line is dropped.
mntentLineLimit :: Int
mntentLineLimit = 4095

-- | What starts a comment line @getmntent@ skips.
mntentComment :: Char
mntentComment = '#'
