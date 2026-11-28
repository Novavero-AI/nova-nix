{-# LANGUAGE CPP #-}

-- | A store path's metadata, made canonical when the path is added to the
-- store.
--
-- Upstream gives every file of a path it adds one form
-- (@canonicalisePathMetaData@ in posix-fs-canonicalise.cc and
-- @setWriteTime@ in file-system.cc, at 2.24.9): a file or directory whose
-- mode is anything but @0444@ or @0555@ is set to @0444@, or @0555@ when
-- its owner may execute it, which drops the write, setuid, setgid and
-- sticky bits; every node's modification time is set to 1, a second past
-- the epoch, a symlink's own included, through @lutimes@, while its access
-- time keeps its whole seconds; and a node another user owns is given to
-- the process's effective user and group.  A symlink's mode is never
-- changed, and a node that is not a regular file, a directory or a symlink
-- fails the path.  A builder reading the path, and every user on the
-- machine, then sees the same tree whatever wrote it.  The executable flag
-- a NAR records is the owner's execute bit, which the canonical mode
-- keeps, so a path's NAR hash is the same before and after.
--
-- On Linux a node's extended attributes, its ACLs among them, are removed
-- first, as upstream removes them (@canonicalisePathMetaData_@ in
-- posix-fs-canonicalise.cc), but for those upstream's @ignored-acls@ names by
-- default, @security.selinux@, @system.nfs4_acl@ and @security.csm@, which
-- nova-nix does not read as a setting: a default ACL would otherwise leave
-- what is made beneath a directory writable by others whatever its mode says.
-- Upstream also clears a node's file flags on macOS (@lchflags@), which
-- nova-nix does not.  Upstream's remaining check needs build users, which
-- nova-nix does not have: it runs a build as a user from a range and fails
-- one whose output holds a node that user does not own.
--
-- On Windows, where upstream sets no times and has no modes, every file
-- and directory is given the read-only attribute and the same
-- modification time.  A symlink keeps its attributes, and its own time
-- is set as on POSIX.  The attribute keeps a file from being written; on
-- a directory it does not stop an entry being added or removed, which
-- only an access control list could.
--
-- 'metadataChanges' decides what one node needs from what it reads of
-- it, without touching the filesystem, and 'canonicalizeMetadata' walks
-- a tree and makes the changes.  'isPlainFile' reads the form upstream
-- requires of an output with a flat hash.
module Nix.Store.Metadata
  ( -- * Canonicalizing a tree
    canonicalizeMetadata,

    -- * Reading a node
    isPlainFile,
    writableByOthers,
    foreignOwnedNode,
    nodeModificationTime,

    -- * The canonical form
    NodeKind (..),
    Ownership (..),
    NodeMetadata (..),
    MetadataChange (..),
    metadataChanges,
    canonicalPermissions,
    readOnlyPermissions,
    executablePermissions,
    storeModificationTime,
  )
where

import Data.Int (Int64)
import Data.Word (Word32)

#if defined(mingw32_HOST_OS)

import Control.Exception (bracket)
import Data.Bits (testBit, (.|.))
import Data.Foldable (traverse_)
import qualified Nix.Store.ExecBit as ExecBit
import Nix.Store.Symlink (WalkNode (..), classifyWalkNode)
import Nix.Store.Win32Path (win32FilePath)
import qualified System.Directory as Dir
import System.FilePath ((</>))
import System.IO.Error (doesNotExistErrorType, mkIOError)
import System.Win32.File
  ( FileAttributeOrFlag,
    closeHandle,
    createFile,
    fILE_FLAG_BACKUP_SEMANTICS,
    fILE_SHARE_DELETE,
    fILE_SHARE_READ,
    fILE_SHARE_WRITE,
    fILE_WRITE_ATTRIBUTES,
    gENERIC_READ,
    oPEN_EXISTING,
  )
import System.Win32.Time (FILETIME (..), getFileTime, setFileTime)

#else

import Control.Monad (when)
import Data.Bits (complement, testBit, (.&.), (.|.))
import Data.Foldable (traverse_)
import Foreign.C.Types (CTime (..))
#if defined(linux_HOST_OS)
import Control.Monad (unless)
import qualified Data.ByteString as BS
import Data.Foldable (for_)
import Foreign.C.Error (eNODATA, eNOTSUP, getErrno, throwErrnoPath, throwErrnoPathIfMinus1, throwErrnoPathIfMinus1_)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import qualified GHC.Foreign as GHC
import GHC.IO.Encoding (getFileSystemEncoding)
import System.Posix.Types (CSsize (..))
#endif
import System.Directory (listDirectory)
import System.FilePath ((</>))
import System.Posix.Files
  ( FileStatus,
    accessTime,
    fileMode,
    fileOwner,
    fileTypeModes,
    getSymbolicLinkStatus,
    groupWriteMode,
    otherWriteMode,
    isDirectory,
    isRegularFile,
    isSymbolicLink,
    modificationTime,
    ownerExecuteMode,
    ownerReadMode,
    setFileMode,
    setSymbolicLinkOwnerAndGroup,
    setSymbolicLinkTimesHiRes,
  )
import System.Posix.Types (GroupID, UserID)
import System.Posix.User (getEffectiveGroupID, getEffectiveUserID)

#endif

-- ---------------------------------------------------------------------------
-- The canonical form
-- ---------------------------------------------------------------------------

-- | What a node of a tree is, read without following a symlink.
data NodeKind = RegularNode | DirectoryNode | SymlinkNode
  deriving (Eq, Show)

-- | Whether a node belongs to the process's effective user.
data Ownership = OwnedByProcess | OwnedByOther
  deriving (Eq, Show)

-- | What canonicalization reads of one node.
data NodeMetadata = NodeMetadata
  { -- | The node's kind.
    nmKind :: !NodeKind,
    -- | Its mode below the file type: the permission, setuid, setgid
    -- and sticky bits.
    nmPermissions :: !Word32,
    -- | Its modification time, in whole seconds since the epoch.
    nmModificationTime :: !Int64,
    -- | Who owns it.
    nmOwnership :: !Ownership
  }
  deriving (Eq, Show)

-- | One change canonicalization makes to a node, in the order upstream
-- makes them.
data MetadataChange
  = -- | Set the node's mode to these bits.
    SetPermissions !Word32
  | -- | Set the node's modification time to 'storeModificationTime',
    -- keeping its access time's whole seconds.
    SetModificationTime
  | -- | Give the node to the process's effective user and group.
    TakeOwnership
  deriving (Eq, Show)

-- | The changes that make one node canonical, none for a node that
-- already is (@canonicalisePathMetaData_@ and
-- @canonicaliseTimestampAndPermissions@ in posix-fs-canonicalise.cc).
metadataChanges :: NodeMetadata -> [MetadataChange]
metadataChanges node = permissions <> time <> ownership
  where
    permissions = case nmKind node of
      SymlinkNode -> []
      _
        | nmPermissions node `elem` [readOnlyPermissions, executablePermissions] -> []
        | otherwise -> [SetPermissions (canonicalPermissions (nmPermissions node))]
    time = [SetModificationTime | nmModificationTime node /= storeModificationTime]
    ownership = case nmOwnership node of
      OwnedByProcess -> []
      OwnedByOther -> [TakeOwnership]

-- | The canonical mode for a node's current one: 'executablePermissions'
-- when its owner may execute it, 'readOnlyPermissions' otherwise.
canonicalPermissions :: Word32 -> Word32
canonicalPermissions perms
  | testBit perms ownerExecuteBit = executablePermissions
  | otherwise = readOnlyPermissions

-- | A canonical node its owner may not execute: @0444@.
readOnlyPermissions :: Word32
readOnlyPermissions = 0o444

-- | A canonical node its owner may execute: @0555@.
executablePermissions :: Word32
executablePermissions = 0o555

-- | The bit of a mode that lets its owner execute the node (@S_IXUSR@).
ownerExecuteBit :: Int
ownerExecuteBit = 6

-- | Every store node's modification time, in seconds since the epoch:
-- upstream's @mtimeStore@ in posix-fs-canonicalise.cc.
storeModificationTime :: Int64
storeModificationTime = 1

-- ---------------------------------------------------------------------------
-- Canonicalizing a tree
-- ---------------------------------------------------------------------------

-- | Make every node of a tree canonical, the root included, without
-- following a symlink.  On POSIX a node that cannot be read or changed
-- fails with the 'Control.Exception.IOException' the system call raised,
-- and one of a type a store path cannot hold fails too; on Windows a node
-- the walk finds absent, or cannot probe, is passed over.
canonicalizeMetadata :: FilePath -> IO ()

-- | Whether a node is a regular file its owner may not execute, read
-- without following a symlink: what upstream requires of an output with
-- a flat hash, a node whose @lstat@ has @S_ISREG@ and no @S_IXUSR@
-- (@registerOutputs@ in local-derivation-goal.cc at 2.24.9,
-- derivation-builder-impl.cc at 2.36.0).  On Windows, which has no mode, the
-- owner may execute a file that carries the exec stream
-- ("Nix.Store.ExecBit").  A node that is not there fails with the
-- 'Control.Exception.IOException' raised.
isPlainFile :: FilePath -> IO Bool

-- | Whether a node lets its group or anyone else write it, read without
-- following a symlink, which is never writable: upstream's test of a
-- build's output before it makes the output canonical, since such a node
-- may have been changed by someone other than the build
-- (@registerOutputs@ in local-derivation-goal.cc at 2.24.9,
-- derivation-builder-impl.cc at 2.36.0).  Windows has no such mode, and no
-- node there is.
writableByOthers :: FilePath -> IO Bool

-- | The first node of a tree that the process's effective user does not
-- own, read without following a symlink, the root first and a directory
-- before its entries, or nothing when the user owns them all.  Upstream
-- refuses such a node in a build's output, @invalid ownership on file@
-- (@canonicalisePathMetaData_@ in posix-fs-canonicalise.cc), where it builds
-- as a build user: it may be a hard link to a file the output has no business
-- changing, which making the output canonical would change.  A directory the
-- user owns is given read and search for its owner before it is listed, so
-- one a builder closed is read rather than refused.  That is a divergence:
-- upstream makes such a directory canonical first, at 0444 for one its owner
-- may not search, and a single-user build of it, not run as root, then fails
-- to read beneath it; here it ends at 0555, since a process that is not root
-- can read a directory only with search on it.  Windows has no owner to
-- compare, and finds none.
foreignOwnedNode :: FilePath -> IO (Maybe FilePath)

-- | A node's own modification time, in whole seconds since the epoch: a
-- symlink's own rather than its target's, as @lstat@ reads it.  A node
-- that is not there fails with the 'Control.Exception.IOException'
-- raised.
nodeModificationTime :: FilePath -> IO Int64

#if defined(mingw32_HOST_OS)

writableByOthers _ = pure False

foreignOwnedNode _ = pure Nothing

isPlainFile path = do
  node <- classifyWalkNode path
  case node of
    WalkRegular -> not <$> ExecBit.isExecutable path
    WalkDirectory -> pure False
    WalkSymlink -> pure False
    WalkAbsent -> ioError (mkIOError doesNotExistErrorType "isPlainFile" Nothing (Just path))

-- A directory is done after its entries, so nothing done to them comes
-- after its own time is set.
canonicalizeMetadata path = do
  node <- classifyWalkNode path
  case node of
    WalkSymlink -> setStoreTime path
    WalkDirectory -> do
      Dir.listDirectory path >>= traverse_ (canonicalizeMetadata . (path </>))
      sealNode path
    WalkRegular -> sealNode path
    WalkAbsent -> pure ()

-- | Set a file's or directory's time and give it the read-only attribute.
sealNode :: FilePath -> IO ()
sealNode path = do
  setStoreTime path
  perms <- Dir.getPermissions path
  Dir.setPermissions path (Dir.setOwnerWritable False perms)

-- | Set a node's own modification time to 'storeModificationTime'.  The
-- node is opened for its attributes alone, which its read-only attribute
-- and another process's open handle both allow, as a symlink itself
-- rather than its target, and with the flag a directory needs.
setStoreTime :: FilePath -> IO ()
setStoreTime path = do
  target <- win32FilePath path
  bracket (open target) closeHandle $ \handle ->
    setFileTime handle Nothing Nothing (Just storeFileTime)
  where
    open target =
      createFile
        target
        fILE_WRITE_ATTRIBUTES
        (fILE_SHARE_READ .|. fILE_SHARE_WRITE .|. fILE_SHARE_DELETE)
        Nothing
        oPEN_EXISTING
        (fILE_FLAG_BACKUP_SEMANTICS .|. fileFlagOpenReparsePoint)
        Nothing

-- | 'storeModificationTime' as a Windows file time.
storeFileTime :: FILETIME
storeFileTime = FILETIME (fromIntegral (unixEpochTicks + storeModificationTime * ticksPerSecond))

-- The node is opened as 'setStoreTime' opens it, as a symlink itself
-- rather than its target and with the flag a directory needs, for the
-- read access GetFileTime asks of a handle.  A time before the epoch
-- rounds down to its second, as a negative @st_mtime@ does.
nodeModificationTime path = do
  target <- win32FilePath path
  bracket (open target) closeHandle $ \handle -> do
    (_, _, FILETIME ticks) <- getFileTime handle
    pure ((fromIntegral ticks - unixEpochTicks) `div` ticksPerSecond)
  where
    open target =
      createFile
        target
        gENERIC_READ
        (fILE_SHARE_READ .|. fILE_SHARE_WRITE .|. fILE_SHARE_DELETE)
        Nothing
        oPEN_EXISTING
        (fILE_FLAG_BACKUP_SEMANTICS .|. fileFlagOpenReparsePoint)
        Nothing

-- | The Unix epoch as a Windows file time, which counts 100-nanosecond
-- ticks from 1601, and the ticks in a second.
unixEpochTicks, ticksPerSecond :: Int64
unixEpochTicks = 116444736000000000
ticksPerSecond = 10000000

-- | @FILE_FLAG_OPEN_REPARSE_POINT@ (winbase.h), which "System.Win32.File"
-- does not export: open a symlink rather than what it points to.
fileFlagOpenReparsePoint :: FileAttributeOrFlag
fileFlagOpenReparsePoint = 0x00200000

#else

isPlainFile path = do
  status <- getSymbolicLinkStatus path
  pure (isRegularFile status && not (testBit (fileMode status) ownerExecuteBit))

writableByOthers path = do
  status <- getSymbolicLinkStatus path
  pure (not (isSymbolicLink status) && fileMode status .&. (groupWriteMode .|. otherWriteMode) /= 0)

nodeModificationTime path = do
  status <- getSymbolicLinkStatus path
  let CTime seconds = modificationTime status
  pure seconds

foreignOwnedNode root = do
  user <- getEffectiveUserID
  let walk path = getSymbolicLinkStatus path >>= visit path
      visit path status
        | fileOwner status /= user = pure (Just path)
        | isDirectory status = do
            let mode = fileMode status
                openable = mode .|. ownerReadMode .|. ownerExecuteMode
            when (openable /= mode) (setFileMode path openable)
            listDirectory path >>= firstFound . map (walk . (path </>))
        | otherwise = pure Nothing
  walk root
  where
    firstFound [] = pure Nothing
    firstFound (look : rest) = look >>= maybe (firstFound rest) (pure . Just)

-- | Remove a node's extended attributes, read without following a link,
-- but for those upstream ignores by default, as
-- upstream removes them (@canonicalisePathMetaData_@ in
-- posix-fs-canonicalise.cc).  A filesystem that keeps none is no error.
-- Linux only, as upstream's is.
removeExtendedAttributes :: FilePath -> IO ()
#if defined(linux_HOST_OS)
removeExtendedAttributes path = do
  encoding <- getFileSystemEncoding
  GHC.withCString encoding path $ \cpath -> do
    size <- c_llistxattr cpath nullPtr 0
    if size < 0
      then do
        errno <- getErrno
        unless (errno == eNOTSUP || errno == eNODATA) (throwErrnoPath "querying extended attributes" path)
      else when (size > 0) $
        allocaBytes (fromIntegral size) $ \buffer -> do
          listed <- throwErrnoPathIfMinus1 "querying extended attributes" path (c_llistxattr cpath buffer (fromIntegral size))
          names <- BS.split 0 <$> BS.packCStringLen (buffer, fromIntegral listed)
          for_ [name | name <- names, not (BS.null name), name `notElem` keptAttributes] $ \name ->
            BS.useAsCString name $ \cname ->
              throwErrnoPathIfMinus1_ "removing extended attribute" path (c_lremovexattr cpath cname)

-- | The attributes upstream's @ignored-acls@ keeps by default, which even
-- root may be unable to remove (@ignoredAcls@ in globals.hh at 2.24.9,
-- local-settings.hh from 2.34.9).
keptAttributes :: [BS.ByteString]
keptAttributes = ["security.selinux", "system.nfs4_acl", "security.csm"]

foreign import ccall unsafe "sys/xattr.h llistxattr"
  c_llistxattr :: CString -> CString -> CSize -> IO CSsize

foreign import ccall unsafe "sys/xattr.h lremovexattr"
  c_lremovexattr :: CString -> CString -> IO CInt
#else
removeExtendedAttributes _ = pure ()
#endif

-- A directory is changed before its entries are read, as upstream
-- changes it, so one its owner may execute but not read becomes
-- readable first.
canonicalizeMetadata root = do
  user <- getEffectiveUserID
  group <- getEffectiveGroupID
  canonicalizeNode user group root

-- | Make one node canonical, then each entry of a directory.
canonicalizeNode :: UserID -> GroupID -> FilePath -> IO ()
canonicalizeNode user group path = do
  status <- getSymbolicLinkStatus path
  kind <- maybe (ioError (userError ("file '" <> path <> "' has an unsupported type"))) pure (statusKind status)
  removeExtendedAttributes path
  traverse_ (applyChange status) (metadataChanges (statusMetadata user kind status))
  case kind of
    DirectoryNode -> listDirectory path >>= traverse_ (canonicalizeNode user group . (path </>))
    RegularNode -> pure ()
    SymlinkNode -> pure ()
  where
    applyChange _ (SetPermissions perms) = setFileMode path (fromIntegral perms)
    applyChange status SetModificationTime =
      setSymbolicLinkTimesHiRes path (realToFrac (accessTime status)) (fromIntegral storeModificationTime)
    applyChange _ TakeOwnership = setSymbolicLinkOwnerAndGroup path user group

-- | A node's kind from its @lstat@, if it is one a store path may hold.
statusKind :: FileStatus -> Maybe NodeKind
statusKind status
  | isRegularFile status = Just RegularNode
  | isDirectory status = Just DirectoryNode
  | isSymbolicLink status = Just SymlinkNode
  | otherwise = Nothing

-- | What canonicalization reads of a node from its @lstat@.
statusMetadata :: UserID -> NodeKind -> FileStatus -> NodeMetadata
statusMetadata user kind status =
  NodeMetadata
    { nmKind = kind,
      nmPermissions = fromIntegral (fileMode status .&. complement fileTypeModes),
      nmModificationTime = seconds,
      nmOwnership = if fileOwner status == user then OwnedByProcess else OwnedByOther
    }
  where
    CTime seconds = modificationTime status

#endif
