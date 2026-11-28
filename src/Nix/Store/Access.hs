{-# LANGUAGE CPP #-}

-- | Who a directory the store creates is open to, how it is created
-- that way, and when a directory already there may hold a store or lead
-- to one.
--
-- A store's directory and its state directory are readable by every
-- user and changeable only by the user nova-nix runs as and the
-- system's administrators ('StoreAccess', 'StateAccess').  A directory created on the
-- way to a store, @C:\\nix@ when the installer has not made it, is
-- theirs alone on Windows, and on POSIX as open as a store's directory
-- ('ParentAccess'); a build directory is that user's alone ('OwnerAccess').
--
-- == POSIX
--
-- A mode says so: a store's directories and the parents made for them
-- take the default mode narrowed by the umask, as upstream's
-- @createDirs@ makes them (file-system.cc), and a build directory 0700, as
-- upstream's @createTempDir@ makes its build directories.
--
-- == Windows
--
-- A directory created with no security descriptor takes the access
-- control list its parent passes down, and a drive root passes down
-- modify for Authenticated Users, every signed-in user, so a store made
-- that way under the default @C:\\nix\\store@ would let any of them
-- change, replace or delete store paths and the database, and change a
-- build while it runs.  Each directory is created instead with a
-- protected DACL, one that takes nothing from its parent, in the one
-- CreateDirectoryW call that makes it, so it never exists with the list
-- its parent would pass down.  Every grant is inherited by the files and
-- directories made under it (@OICI@), so a store path or a build's tree
-- is covered too.
--
-- A store's directories grant full control to the user nova-nix runs
-- as, SYSTEM and Administrators, and read and execute to Users, naming
-- the user once when it is SYSTEM: the protected DACL install-nova-nix's
-- @install.ps1@ sets on @C:\\nix@, with the same rights, inheritance and
-- principals.  The installer sets it on @C:\\nix@, which the store then
-- inherits; nova-nix sets it on each store directory it creates, since a
-- store can be wherever @--store@ names.  A build directory grants full
-- control to that user alone.  The user is the process token's, as the
-- installer's @WindowsIdentity.GetCurrent().User@ is, and neither sets
-- the owner, which stays the token's default.
--
-- == Build accounts
--
-- Where builds run as build accounts, the local group @nixbld@'s, the
-- account nova-nix runs as is the service that keeps the store, and it
-- plays the part of the user above, as a t'StoreAccounts' value says.
-- The store's directory
-- then grants its entries to itself alone, and the group the right to
-- create files and directories in it and nothing else, so what a build
-- makes there takes its own token's default DACL rather than the store's.
-- Registering a path ('withValidPathAccess') gives each of its nodes the
-- service as owner and a protected DACL that grants the service, SYSTEM
-- and Administrators full control, Users and the group read and execute,
-- and the node's owner read-control alone ('validPathSddl'), so no build
-- changes a valid path, the build account that made it included.  A
-- machine with no such group, a single-user store's, has no build
-- accounts to grant: the store's directory passes its entries down as it
-- always has, and a registered path's DACL grants what it would inherit
-- from it, with the user as its owner.
--
-- A parent created on the way to a store grants full control to the
-- user, SYSTEM and Administrators and nothing to other users, so no
-- other user can rename it and put a store of their own in its place,
-- and nothing it holds is opened to other users unless it says so
-- itself.  The installer accepts such a @C:\\nix@, whose every entry
-- names an account it trusts, and gives it its own DACL, which adds read
-- and execute for Users.
--
-- nova-nix never changes the list of a directory that already exists;
-- the next section says when it uses one.
--
-- == A directory already there
--
-- On Windows a directory already on the way to a store, the store's
-- directory and its state directory among them, is used only when no
-- account but the user nova-nix runs as, SYSTEM and Administrators can
-- change it: one of them owns it, it has a DACL, and no allow entry of
-- any kind grants another account a right that changes it, an
-- inherit-only entry, which grants nothing on the directory itself,
-- included.  That is install-nova-nix's rule for @C:\\nix@ and what it
-- holds.  A drive root lets every signed-in user create a folder in it,
-- so where the installer never ran another user can make @C:\\nix@
-- first, and as its owner give themselves the right to delete what it
-- holds, move the store aside and put a store and database of their own
-- in its place.
--
-- The volume root itself is not checked: its default DACL lets other
-- users create folders in it but not delete or rename one already
-- there, and Windows may own it as TrustedInstaller.  Every directory
-- below it is, wherever @--store@ names the store, so a folder made at a
-- drive root that took the root's DACL, which lets other users change
-- what is made in it, is refused.  A directory is checked before
-- anything is created in it or read from it, since a database another
-- account controls is as dangerous to read as to write.  One nova-nix
-- creates passes by construction.  A link is checked as itself and not
-- followed: one another account made is refused, and where one the user
-- made leads is the user's to choose.
--
-- On POSIX nothing is checked, as upstream checks nothing: there only
-- root can create a directory in @/@.
module Nix.Store.Access
  ( -- * Who a directory is open to
    DirAccess (..),
    StoreAccounts (..),

    -- * Creating a directory
    createDirectoryWith,
    createStoreDirectory,
    createStateDirectory,
    checkStateDirectory,

    -- * A directory already there
    storeDirectories,
    stateDirectories,
    DirSecurity (..),
    AclEntry (..),
    Distrust (..),
    parseSecurityDescriptor,
    securityDistrust,
    descriptorDistrust,
    distrustMessage,

    -- * A registered path
    withValidPathAccess,

    -- * The Windows access control list
    Sid,
    sidFromText,
    sidText,
    accessSddl,
    validPathSddl,
  )
where

import Control.Monad (guard)
import Data.Bits (complement, shiftL, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (isDigit, isHexDigit)
import Data.Foldable (toList)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Word (Word16, Word32, Word8)
import Numeric (showHex)
import qualified System.FilePath.Windows as Windows

#if defined(mingw32_HOST_OS)

import Control.Exception (bracket, finally)
import Control.Monad (unless, void, when)
import Data.Foldable (for_, traverse_)
import Data.Maybe (fromMaybe)
import Foreign.C.String (CString, CWString, peekCString, peekCWString, withCWString)
import Foreign.C.Types (CULong)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (maybePeek)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peek)
import Nix.Store.Win32Path (win32FilePath)
import System.Directory (doesDirectoryExist)
import System.FilePath (takeDirectory)
import System.IO.Error (alreadyExistsErrorType, catchIOError, ioeSetFileName, isAlreadyExistsError, mkIOError, modifyIOError)
import System.Win32.Info (getFullPathName)
import System.Win32.Types (ErrCode, failUnlessSuccess, failWith, localFree)

#else

import System.Directory (createDirectoryIfMissing)
import qualified System.Posix.Directory as Posix
import System.Posix.Files (accessModes, ownerModes)

#endif

-- ---------------------------------------------------------------------------
-- Who a directory is open to
-- ---------------------------------------------------------------------------

-- | Who a directory the store creates is open to.
data DirAccess
  = -- | Readable by every user, changeable by the user nova-nix runs as,
    -- SYSTEM and Administrators, and on Windows a directory in which the
    -- build accounts' group, where the machine has one, may create entries
    -- and do nothing else: a store's directory.
    StoreAccess
  | -- | Readable by every user, changeable by the user nova-nix runs as,
    -- SYSTEM and Administrators, and passing that down to all it holds: a
    -- store's state directory.
    StateAccess
  | -- | On Windows readable and changeable by the user nova-nix runs as,
    -- SYSTEM and Administrators alone, and on POSIX the mode of a store's
    -- directory: a directory created on the way to a store.
    ParentAccess
  | -- | The user nova-nix runs as alone: a build directory.
    OwnerAccess
  deriving (Eq, Show)

-- | The accounts a store's directories and registered paths name on
-- Windows, beside SYSTEM, Administrators and Users.
data StoreAccounts = StoreAccounts
  { -- | The account that keeps the store: the one nova-nix runs as, the
    -- user in a single-user store and the service where builds run as
    -- build accounts.
    saService :: !Sid,
    -- | The build accounts' local group, @nixbld@, where the machine has
    -- one, and 'Nothing' where it does not, as in a single-user store.
    saBuilders :: !(Maybe Sid)
  }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Creating a directory
-- ---------------------------------------------------------------------------

-- | Create a directory open to whom the access says, failing with an
-- 'System.IO.Error.isAlreadyExistsError' exception when anything already
-- answers to the name, a symlink included, which is not followed.  Its
-- parent must exist.  On POSIX its mode is 0777 for 'StoreAccess',
-- 'StateAccess' and 'ParentAccess' and 0700 for 'OwnerAccess', each
-- narrowed by the umask; on Windows it is created with the protected DACL
-- 'accessSddl' describes for the t'StoreAccounts' of this machine.
createDirectoryWith :: DirAccess -> FilePath -> IO ()
#if defined(mingw32_HOST_OS)
createDirectoryWith access path =
  modifyIOError (`ioeSetFileName` path) $ do
    accounts <- storeAccounts
    target <- win32FilePath path
    withCWString target $ \cTarget ->
      withCWString (T.unpack (accessSddl accounts access)) $ \cSddl ->
        failUnlessSuccess "CreateDirectory" (c_nn_dir_create_sddl cTarget cSddl)
#else
createDirectoryWith access path = Posix.createDirectory path $ case access of
  StoreAccess -> accessModes
  StateAccess -> accessModes
  ParentAccess -> accessModes
  OwnerAccess -> ownerModes
#endif

-- | Run an action given the step that registering a path takes for each
-- of its nodes on Windows: the node itself, a link not followed, is
-- given the service as its owner and the protected DACL 'validPathSddl'
-- describes for the t'StoreAccounts' of this machine, which no build
-- account may change.  The DACL is each node's own, not one inherited from the
-- path's root, so the walk that sets it is the caller's, which does not
-- follow a link.  On POSIX the step does nothing, and the walk's own
-- ownership change stands for it.
withValidPathAccess :: ((FilePath -> IO ()) -> IO a) -> IO a
#if defined(mingw32_HOST_OS)
withValidPathAccess use = do
  accounts <- storeAccounts
  withCWString (T.unpack (validPathSddl accounts)) $ \cSddl ->
    alloca $ \descriptorOut -> do
      failUnlessSuccess "ConvertStringSecurityDescriptorToSecurityDescriptorW" (c_nn_descriptor_from_sddl cSddl descriptorOut)
      bracket (peek descriptorOut) (void . localFree) (use . giveValidPathAccess)
#else
withValidPathAccess use = use (const (pure ()))
#endif

-- | Create a store's directory, given its path, where it is missing,
-- with any missing parent of it, as @createDirectoryIfMissing True@
-- does: the directories 'createStateDirectory' creates and checks, less
-- the state directory, for what is written into a store before it is
-- opened.  On POSIX that is all it does.  On Windows the store's
-- directory is open to whom 'StoreAccess' says and each parent to whom
-- 'ParentAccess' says, and one already there is refused as
-- 'createStateDirectory' refuses one.
createStoreDirectory :: FilePath -> IO ()
#if defined(mingw32_HOST_OS)
createStoreDirectory storeDir = do
  accounts <- storeAccounts
  full <- getFullPathName storeDir
  traverse_ (prepareDirectory accounts full) (storeDirectories full)
#else
createStoreDirectory = createDirectoryIfMissing True
#endif

-- | Create a store's state directory, given its path, where it is
-- missing, with the store directory it is in and any missing parent of
-- that, as @createDirectoryIfMissing True@ does.  On POSIX that is all
-- it does: the modes are the default, narrowed by the umask.  On
-- Windows it takes the directories 'stateDirectories' gives, outermost
-- first.  One that is missing is created, open to whom its access says.
-- One already there, or made by another process before this one could
-- make it, is refused unless it can be trusted with a store
-- ('descriptorDistrust'), with a 'userError' naming it and what is
-- wrong ('distrustMessage'), before anything is created in it; then
-- something other than a directory there fails with an
-- 'System.IO.Error.isAlreadyExistsError' exception naming it.
createStateDirectory :: FilePath -> IO ()
#if defined(mingw32_HOST_OS)
createStateDirectory stateDir = do
  accounts <- storeAccounts
  full <- getFullPathName stateDir
  traverse_ (prepareDirectory accounts (takeDirectory full)) (stateDirectories full)
#else
createStateDirectory = createDirectoryIfMissing True
#endif

-- | Refuse a store, given its state directory, before anything is read
-- from it, as 'createStateDirectory' refuses one: on Windows each of the
-- directories it takes that exists must be trusted with a store, and one
-- that does not exist is passed over.  On POSIX it checks nothing.
checkStateDirectory :: FilePath -> IO ()
#if defined(mingw32_HOST_OS)
checkStateDirectory stateDir = do
  accounts <- storeAccounts
  full <- getFullPathName stateDir
  for_ (stateDirectories full) $ \(dir, access) ->
    entrySecurity dir >>= traverse_ (refuseUntrusted accounts access (takeDirectory full) dir)
#else
checkStateDirectory _ = pure ()
#endif

#if defined(mingw32_HOST_OS)

foreign import ccall unsafe "nn_winfs.h nn_token_user_sid"
  c_nn_token_user_sid :: Ptr CWString -> Ptr CString -> IO ErrCode

foreign import ccall unsafe "nn_winfs.h nn_dir_create_sddl"
  c_nn_dir_create_sddl :: CWString -> CWString -> IO ErrCode

foreign import ccall unsafe "nn_winfs.h nn_entry_security"
  c_nn_entry_security :: CWString -> Ptr (Ptr ()) -> Ptr CULong -> Ptr CString -> IO ErrCode

foreign import ccall unsafe "nn_winfs.h nn_local_group_sid"
  c_nn_local_group_sid :: CWString -> Ptr CWString -> Ptr CString -> IO ErrCode

foreign import ccall unsafe "nn_winfs.h nn_descriptor_from_sddl"
  c_nn_descriptor_from_sddl :: CWString -> Ptr (Ptr ()) -> IO ErrCode

foreign import ccall unsafe "nn_winfs.h nn_entry_set_owner_dacl"
  c_nn_entry_set_owner_dacl :: CWString -> Ptr () -> Ptr CString -> IO ErrCode

-- | The accounts this machine's store names: the one this process runs
-- as, and the local group 'buildersGroupName' where there is one.
storeAccounts :: IO StoreAccounts
storeAccounts = StoreAccounts <$> tokenUserSid <*> localGroupSid buildersGroupName

-- | The name of the build accounts' local group, as upstream names its
-- Linux build users' group.
buildersGroupName :: String
buildersGroupName = "nixbld"

-- | The SID of the local group the name names on this machine, or
-- 'Nothing' where it has no local group of that name: none at all, or an
-- account of another kind.  A failure names the call that failed.
localGroupSid :: String -> IO (Maybe Sid)
localGroupSid name =
  withCWString name $ \cName -> alloca $ \sidOut -> alloca $ \failedOut -> do
    code <- c_nn_local_group_sid cName sidOut failedOut
    when (code /= 0) $ do
      call <- maybePeek peekCString =<< peek failedOut
      failWith (fromMaybe "nn_local_group_sid" call) code
    traverse readSid =<< maybePeek pure =<< peek sidOut
  where
    readSid sid = (`finally` localFree sid) $ do
      text <- T.pack <$> peekCWString sid
      maybe (ioError (userError ("the group " <> name <> " has a SID that is not one: " <> show text))) pure (sidFromText text)

-- | Give the node at the path itself, a link not followed, the owner and
-- protected DACL of the security descriptor given.  A failure names the
-- call that failed and the path.
giveValidPathAccess :: Ptr () -> FilePath -> IO ()
giveValidPathAccess descriptor path =
  modifyIOError (`ioeSetFileName` path) $ do
    target <- win32FilePath path
    withCWString target $ \cTarget -> alloca $ \failedOut -> do
      code <- c_nn_entry_set_owner_dacl cTarget descriptor failedOut
      when (code /= 0) $ do
        call <- maybePeek peekCString =<< peek failedOut
        failWith (fromMaybe "nn_entry_set_owner_dacl" call) code

-- | Check a directory on the way to a store, given the machine's
-- accounts and the store's directory, where it exists, and create it
-- with its access where it does not ('createStateDirectory').
prepareDirectory :: StoreAccounts -> FilePath -> (FilePath, DirAccess) -> IO ()
prepareDirectory accounts store (dir, access) =
  entrySecurity dir >>= maybe (createDirectoryWith access dir `catchIOError` raced) keep
  where
    raced err
      | isAlreadyExistsError err = entrySecurity dir >>= maybe (ioError err) keep
      | otherwise = ioError err
    keep descriptor = do
      refuseUntrusted accounts access store dir descriptor
      present <- doesDirectoryExist dir
      unless present (ioError (mkIOError alreadyExistsErrorType "createDirectory" Nothing (Just dir)))

-- | Refuse the store whose directory is given, for the machine's
-- accounts, when the directory on the way to it whose access and
-- security descriptor are given cannot be trusted with it.
refuseUntrusted :: StoreAccounts -> DirAccess -> FilePath -> FilePath -> ByteString -> IO ()
refuseUntrusted accounts access store dir descriptor =
  traverse_ (ioError . userError . distrustMessage dir store) (descriptorDistrust accounts access descriptor)

-- | The security descriptor of the entry at the path itself, a link read
-- as itself and not followed, holding its owner and DACL, or 'Nothing'
-- when nothing has the name.  A failure names the call that failed and
-- the path.
entrySecurity :: FilePath -> IO (Maybe ByteString)
entrySecurity path =
  modifyIOError (`ioeSetFileName` path) $ do
    target <- win32FilePath path
    withCWString target $ \cTarget ->
      alloca $ \descriptorOut -> alloca $ \lengthOut -> alloca $ \failedOut -> do
        code <- c_nn_entry_security cTarget descriptorOut lengthOut failedOut
        case code of
          0 -> bracket (peek descriptorOut) (void . localFree) $ \descriptor -> do
            size <- peek lengthOut
            Just <$> BS.packCStringLen (castPtr descriptor, fromIntegral size)
          _
            | code `elem` [errorFileNotFound, errorPathNotFound] -> pure Nothing
            | otherwise -> do
                call <- maybePeek peekCString =<< peek failedOut
                failWith (fromMaybe "nn_entry_security" call) code

-- | @ERROR_FILE_NOT_FOUND@ and @ERROR_PATH_NOT_FOUND@ (winerror.h), the
-- errors CreateFileW gives when nothing has the name, or a directory on
-- the way to it is missing.
errorFileNotFound, errorPathNotFound :: ErrCode
errorFileNotFound = 2
errorPathNotFound = 3

-- | The user this process runs as, its token's user.  A failure names
-- the call that failed, which the shim reports beside its error.
tokenUserSid :: IO Sid
tokenUserSid =
  alloca $ \sidOut -> alloca $ \failedOut -> do
    code <- c_nn_token_user_sid sidOut failedOut
    when (code /= 0) $ do
      call <- maybePeek peekCString =<< peek failedOut
      failWith (fromMaybe "nn_token_user_sid" call) code
    bracket (peek sidOut) (void . localFree) $ \sid -> do
      text <- T.pack <$> peekCWString sid
      maybe (ioError (userError ("the process token's user SID is not a SID: " <> show text))) pure (sidFromText text)

#endif

-- ---------------------------------------------------------------------------
-- A directory already there
-- ---------------------------------------------------------------------------

-- | The directories down to a store's directory, given its full path on
-- Windows, outermost first, each with the access it is created with
-- where it is missing: the store's directory with 'StoreAccess', and
-- each parent of it with 'ParentAccess'.  The volume root, the drive
-- 'System.FilePath.Windows.splitDrive' gives, is never among them (the
-- module header says why), so a store at a drive root has none.  It
-- follows Windows' path rules on every platform.
storeDirectories :: FilePath -> [(FilePath, DirAccess)]
storeDirectories storeDir = reverse (zip (reverse below) (StoreAccess : repeat ParentAccess))
  where
    (root, rest) = Windows.splitDrive storeDir
    below = map (root Windows.</>) (scanl1 (Windows.</>) (Windows.splitDirectories rest))

-- | The directories 'storeDirectories' gives for the store directory a
-- state directory is in, given the state directory's full path on
-- Windows, then the state directory itself, with 'StateAccess'.
stateDirectories :: FilePath -> [(FilePath, DirAccess)]
stateDirectories stateDir = storeDirectories (Windows.takeDirectory stateDir) <> [(stateDir, StateAccess)]

-- | The owner and DACL of a directory, as its security descriptor holds
-- them.
data DirSecurity = DirSecurity
  { -- | The owner, 'Nothing' when the descriptor names none.
    dsOwner :: !(Maybe Sid),
    -- | The DACL's entries in order, 'Nothing' when there is no DACL or
    -- a null one, either of which lets every account do anything.
    dsDacl :: !(Maybe [AclEntry])
  }
  deriving (Eq, Show)

-- | An entry of a DACL, by what it grants.
data AclEntry
  = -- | An allow entry of any kind and any inheritance, an inherit-only
    -- entry among them: its flags, the inheritance ones among them, the
    -- rights it grants and the account it grants them to.
    AllowEntry !Word8 !Word32 !Sid
  | -- | A deny, audit or alarm entry of any kind, which grants nothing.
    OtherEntry
  | -- | An entry of any other type, given: one winnt.h numbers after the
    -- callback types, such as a mandatory label, which belongs in a
    -- SACL, or one it does not define.
    UnknownEntry !Word8
  deriving (Eq, Show)

-- | Why a directory on the way to a store cannot be trusted with it.
data Distrust
  = -- | Its owner, 'Nothing' for none, is not the user, SYSTEM or
    -- Administrators.
    OwnedBy !(Maybe Sid)
  | -- | It has no DACL, or a null one.
    NoDacl
  | -- | Its DACL holds an 'UnknownEntry'.
    UnreadableEntry
  | -- | An allow entry grants this account, not the user, SYSTEM or
    -- Administrators, a right that changes the directory.
    WritableBy !Sid
  | -- | Its security descriptor does not parse
    -- ('parseSecurityDescriptor').
    UnreadableDescriptor
  deriving (Eq, Show)

-- | Why a directory with the owner and DACL given, of the access given,
-- cannot be trusted with a store for the accounts given, or 'Nothing'
-- when it can: install-nova-nix's rule, which the module header gives,
-- with the service in the user's place.  One grant to another account
-- is trusted, on the store's directory alone: the build accounts'
-- group's, to create entries in it and nothing more, applying to the
-- directory itself and passed to nothing it holds, as 'accessSddl'
-- writes it.  Of the entries that fail the rule, the first is named.
securityDistrust :: StoreAccounts -> DirAccess -> DirSecurity -> Maybe Distrust
securityDistrust accounts access (DirSecurity owner dacl)
  | not (maybe False trusted owner) = Just (OwnedBy owner)
  | otherwise = maybe (Just NoDacl) (listToMaybe . mapMaybe entryDistrust) dacl
  where
    trusted sid = sid `elem` [saService accounts, localSystemSid, administratorsSid]
    entryDistrust entry = case entry of
      AllowEntry flags rights sid
        | rights .&. changeRights == 0 || trusted sid || buildersCreating flags rights sid -> Nothing
        | otherwise -> Just (WritableBy sid)
      OtherEntry -> Nothing
      UnknownEntry _ -> Just UnreadableEntry
    buildersCreating flags rights sid =
      access == StoreAccess
        && Just sid == saBuilders accounts
        && rights .&. complement createOnly == 0
        && flags .&. passingDown == 0

-- | 'securityDistrust' of a security descriptor given as bytes, or
-- 'UnreadableDescriptor' when they are not one.
descriptorDistrust :: StoreAccounts -> DirAccess -> ByteString -> Maybe Distrust
descriptorDistrust accounts access = maybe (Just UnreadableDescriptor) (securityDistrust accounts access) . parseSecurityDescriptor

-- | @OBJECT_INHERIT_ACE@, @CONTAINER_INHERIT_ACE@ and @INHERIT_ONLY_ACE@
-- (winnt.h), the flags of an entry that passes to what a directory holds.
passingDown :: Word8
passingDown = 0x1 .|. 0x2 .|. 0x8

-- | The refusal of a store, given its directory second, for a directory
-- on the way to it, given first, that cannot be trusted with it, in
-- install-nova-nix's words where it has them.
distrustMessage :: FilePath -> FilePath -> Distrust -> String
distrustMessage dir store reason =
  dir <> " " <> why <> ", so the store in " <> store <> " cannot be trusted; move " <> dir <> " aside and let nova-nix or install-nova-nix create it"
  where
    why = case reason of
      OwnedBy owner -> "belongs to " <> maybe "no one" (T.unpack . sidText) owner <> ", not to you, SYSTEM or Administrators"
      NoDacl -> "has no DACL, which lets every account change it"
      UnreadableEntry -> "carries an access entry of a kind nova-nix cannot read"
      WritableBy sid -> "lets " <> T.unpack (sidText sid) <> " change it"
      UnreadableDescriptor -> "has a security descriptor nova-nix cannot read"

-- | The rights that let an account change a directory, install-nova-nix's
-- write rights (winnt.h).  The generic two are among them because an
-- inherit-only entry can hold them in place of the rights they map to
-- once inherited.
changeRights :: Word32
changeRights =
  foldl'
    (.|.)
    0
    [ 0x2, -- FILE_WRITE_DATA: on a directory, adding a file
      0x4, -- FILE_APPEND_DATA: on a directory, adding a subdirectory
      0x10, -- FILE_WRITE_EA
      0x40, -- FILE_DELETE_CHILD
      0x100, -- FILE_WRITE_ATTRIBUTES
      0x10000, -- DELETE
      0x40000, -- WRITE_DAC
      0x80000, -- WRITE_OWNER
      0x10000000, -- GENERIC_ALL
      0x40000000 -- GENERIC_WRITE
    ]

-- | The owner and DACL a self-relative security descriptor holds, the
-- form GetSecurityInfo returns one in, or 'Nothing' when the bytes are
-- not one.  The descriptor starts with its revision, its control flags
-- at byte 2, and the offsets of its owner at 4 and its DACL at 16, an
-- offset of 0 for one it does not hold; the DACL is there only when the
-- flags say so, and a DACL that is there at offset 0 is a null one.
-- Every entry of the DACL is read, as Windows keeps it.
parseSecurityDescriptor :: ByteString -> Maybe DirSecurity
parseSecurityDescriptor bytes = do
  revision <- byteAt 0 bytes
  control <- word16At 2 bytes
  ownerOffset <- word32At 4 bytes
  daclOffset <- word32At 16 bytes
  guard (revision == descriptorRevision && control .&. selfRelative /= 0)
  owner <- partAt ownerOffset sidIn
  dacl <- if control .&. daclPresent == 0 then Just Nothing else partAt daclOffset aclEntriesIn
  pure (DirSecurity owner dacl)
  where
    partAt offset parse
      | offset == 0 = Just Nothing
      | otherwise = Just <$> parse (BS.drop (fromIntegral offset) bytes)

-- | The SID the bytes start with: its revision, 1, the count of its
-- subauthorities, its identifier authority in six bytes, the most
-- significant first, then each subauthority in four, the least
-- significant first.  It is written as ConvertSidToStringSidW writes it,
-- so a SID 'sidFromText' does not accept is no SID here either.
sidIn :: ByteString -> Maybe Sid
sidIn bytes = do
  revision <- byteAt 0 bytes
  count <- byteAt 1 bytes
  guard (revision == sidRevision)
  authority <- foldl' (\value byte -> value * 256 + toInteger byte) 0 <$> traverse (`byteAt` bytes) [2 .. sidHeaderSize - 1]
  subauthorities <- traverse (\index -> word32At (sidHeaderSize + 4 * index) bytes) [0 .. fromIntegral count - 1]
  sidFromText (sidRevisionPrefix <> authorityText authority <> foldMap (("-" <>) . T.pack . show) subauthorities)
  where
    authorityText value
      | value < fieldLimit = T.pack (show value)
      | otherwise = "0x" <> T.justifyRight wideAuthorityDigits '0' (T.pack (showHex value ""))

-- | The entries of the ACL the bytes start with: its header gives its
-- size at byte 2 and its count of entries at 4, and the entries follow
-- it, each led by its type and, at its byte 2, its own size.  An entry
-- that runs past the ACL, or an ACL past the bytes, is not one.
aclEntriesIn :: ByteString -> Maybe [AclEntry]
aclEntriesIn bytes = do
  size <- fromIntegral <$> word16At 2 bytes
  count <- fromIntegral <$> word16At 4 bytes
  guard (size >= aclHeaderSize && BS.length bytes >= size)
  entriesIn count (BS.drop aclHeaderSize (BS.take size bytes))
  where
    entriesIn :: Int -> ByteString -> Maybe [AclEntry]
    entriesIn count rest
      | count <= 0 = Just []
      | otherwise = do
          size <- fromIntegral <$> word16At 2 rest
          guard (size >= aceHeaderSize && BS.length rest >= size)
          entry <- entryIn (BS.take size rest)
          (entry :) <$> entriesIn (count - 1) (BS.drop size rest)

-- | An entry of a DACL, given its bytes: one of a type winnt.h lays out
-- has its access mask after its header and a SID after that, where its
-- layout says.
entryIn :: ByteString -> Maybe AclEntry
entryIn entry = do
  kind <- byteAt 0 entry
  case entryLayout kind of
    Nothing -> Just (UnknownEntry kind)
    Just layout -> do
      flags <- byteAt 1 entry
      rights <- word32At aceHeaderSize entry
      sidStart <- sidOffset layout
      sid <- sidIn (BS.drop sidStart entry)
      Just (if kind `elem` allowTypes then AllowEntry flags rights sid else OtherEntry)
  where
    sidOffset layout = case layout of
      CommonLayout -> Just 8
      CompoundLayout -> Just 12
      -- The flags, after the mask, say which of the two object type
      -- GUIDs come before the SID.
      ObjectLayout -> do
        flags <- word32At 8 entry
        let present flag = flags .&. flag /= 0
        Just (12 + guidSize * length (filter present [aceObjectTypePresent, aceInheritedObjectTypePresent]))

-- | Where an entry's SID starts, by the structure winnt.h lays its type
-- out as.
data EntryLayout
  = -- | @ACCESS_ALLOWED_ACE@: the SID after the mask.
    CommonLayout
  | -- | @ACCESS_ALLOWED_COMPOUND_ACE@: the SID after the mask, the
    -- compound type and a reserved field.
    CompoundLayout
  | -- | @ACCESS_ALLOWED_OBJECT_ACE@: the SID after the mask, the flags
    -- and the object type GUIDs the flags say are there.
    ObjectLayout

-- | The layout of an entry of the type given, for each type winnt.h
-- defines for a DACL; 'Nothing' for any other.
entryLayout :: Word8 -> Maybe EntryLayout
entryLayout kind
  | kind `elem` commonTypes = Just CommonLayout
  | kind == compoundType = Just CompoundLayout
  | kind `elem` objectTypes = Just ObjectLayout
  | otherwise = Nothing

-- | The entry types winnt.h lays out as @ACCESS_ALLOWED_ACE@: allow,
-- deny, audit and alarm (0 to 3), and their callback forms (9, 10, 13
-- and 14).
commonTypes :: [Word8]
commonTypes = [0x0, 0x1, 0x2, 0x3, 0x9, 0xA, 0xD, 0xE]

-- | @ACCESS_ALLOWED_COMPOUND_ACE_TYPE@ (winnt.h).
compoundType :: Word8
compoundType = 0x4

-- | The object entry types (winnt.h): allow, deny, audit and alarm (5 to
-- 8), and their callback forms (11, 12, 15 and 16).
objectTypes :: [Word8]
objectTypes = [0x5, 0x6, 0x7, 0x8, 0xB, 0xC, 0xF, 0x10]

-- | The allow types among them: @ACCESS_ALLOWED_ACE_TYPE@, the compound
-- type, @ACCESS_ALLOWED_OBJECT_ACE_TYPE@ and the callback forms of the
-- first and the last.
allowTypes :: [Word8]
allowTypes = [0x0, compoundType, 0x5, 0x9, 0xB]

-- | @SECURITY_DESCRIPTOR_REVISION@ (winnt.h).
descriptorRevision :: Word8
descriptorRevision = 1

-- | @SE_SELF_RELATIVE@ and @SE_DACL_PRESENT@ (winnt.h), the control flags
-- saying the descriptor holds offsets and that it has a DACL.
selfRelative, daclPresent :: Word16
selfRelative = 0x8000
daclPresent = 0x4

-- | @SID_REVISION@ (winnt.h).
sidRevision :: Word8
sidRevision = 1

-- | The bytes of a SID before its subauthorities: its revision, its
-- count and its identifier authority.
sidHeaderSize :: Int
sidHeaderSize = 8

-- | The size of an ACL's header and of an entry's header (winnt.h's
-- @ACL@ and @ACE_HEADER@).
aclHeaderSize, aceHeaderSize :: Int
aclHeaderSize = 8
aceHeaderSize = 4

-- | The size of a GUID, and @ACE_OBJECT_TYPE_PRESENT@ and
-- @ACE_INHERITED_OBJECT_TYPE_PRESENT@ (winnt.h), the flags saying an
-- object entry holds one for its object type and one for its inherited
-- object type.
guidSize :: Int
guidSize = 16

aceObjectTypePresent, aceInheritedObjectTypePresent :: Word32
aceObjectTypePresent = 0x1
aceInheritedObjectTypePresent = 0x2

-- | The byte, and the 16-bit and 32-bit little-endian words, at an
-- offset into the bytes, or 'Nothing' past their end.
byteAt :: Int -> ByteString -> Maybe Word8
byteAt offset bytes = BS.indexMaybe bytes offset

word16At :: Int -> ByteString -> Maybe Word16
word16At offset bytes = do
  low <- byteAt offset bytes
  high <- byteAt (offset + 1) bytes
  Just (fromIntegral low .|. fromIntegral high `shiftL` 8)

word32At :: Int -> ByteString -> Maybe Word32
word32At offset bytes = do
  low <- word16At offset bytes
  high <- word16At (offset + 2) bytes
  Just (fromIntegral low .|. fromIntegral high `shiftL` 16)

-- ---------------------------------------------------------------------------
-- The Windows access control list
-- ---------------------------------------------------------------------------

-- | A security identifier in the string form ConvertSidToStringSidW
-- writes: @S-1-5-18@ for SYSTEM.  'sidFromText' is the one way to make
-- one, so the text 'accessSddl' writes as an entry's trustee is a SID
-- and nothing more.
newtype Sid = Sid Text
  deriving (Show)

-- | Two SIDs are equal when they name one account.  Only an authority of
-- 2^32 or more has two spellings, its hexadecimal digits in either case,
-- and which case ConvertSidToStringSidW writes them in is undocumented.
instance Eq Sid where
  Sid a == Sid b = T.toLower a == T.toLower b

-- | A SID from its string form, @S-R-I-S...@: @S@, the revision, which is
-- 1, the identifier authority and one to fifteen subauthorities, joined
-- by @-@.  The authority is a decimal below 2^32, or @0x@ and twelve
-- hexadecimal digits for one of 2^32 or more, and each subauthority a
-- decimal below 2^32, as ConvertSidToStringSidW writes them.  A decimal
-- has no leading zero, so a SID whose authority is below 2^32, SYSTEM's
-- among them, has the one spelling 'accessSddl' compares the user's
-- with.  Anything else, any other character included, is 'Nothing'.
sidFromText :: Text -> Maybe Sid
sidFromText text = do
  rest <- T.stripPrefix sidRevisionPrefix text
  case T.splitOn "-" rest of
    authority : subauthorities@(_ : _)
      | validAuthority authority,
        length subauthorities <= maxSubAuthorities,
        all (canonicalDecimalBelow fieldLimit) subauthorities ->
          Just (Sid text)
    _ -> Nothing
  where
    validAuthority authority =
      canonicalDecimalBelow fieldLimit authority || maybe False wideAuthority (T.stripPrefix "0x" authority)
    -- The form ConvertSidToStringSidW takes only for an authority whose
    -- top 16 of 48 bits are not all zero.
    wideAuthority digits =
      T.length digits == wideAuthorityDigits && T.all isHexDigit digits && T.any (/= '0') (T.take wideHighDigits digits)

-- | A SID's string form, as 'sidFromText' took it.
sidText :: Sid -> Text
sidText (Sid text) = text

-- | Whether text is a decimal below the limit, spelled with no sign,
-- space or leading zero.
canonicalDecimalBelow :: Integer -> Text -> Bool
canonicalDecimalBelow limit digits = case T.uncons digits of
  Just ('0', more) -> T.null more
  Just _
    | T.all isDigit digits, Right (value, unread) <- TR.decimal digits -> T.null unread && value < limit
  _ -> False

-- | A SID's string form up to its identifier authority: @S@ and the
-- revision, @SID_REVISION@ (winnt.h).
sidRevisionPrefix :: Text
sidRevisionPrefix = "S-1-"

-- | @SID_MAX_SUB_AUTHORITIES@ (winnt.h).
maxSubAuthorities :: Int
maxSubAuthorities = 15

-- | The bound of a subauthority, a 32-bit value, and of an authority
-- written in decimal.
fieldLimit :: Integer
fieldLimit = 2 ^ (32 :: Int)

-- | The hexadecimal digits of a 48-bit authority, and of its top 16
-- bits.
wideAuthorityDigits, wideHighDigits :: Int
wideAuthorityDigits = 12
wideHighDigits = 4

-- | The security descriptor, in SDDL, that a directory of the given
-- access is created with on Windows, for the given accounts: a protected
-- DACL (@D:P@) of allow entries.  'StateAccess' grants full control to
-- the service, SYSTEM and Administrators and read and execute to Users,
-- in that order, naming the service once when it is SYSTEM itself, each
-- inherited by files and directories below (@OICI@), as install-nova-nix
-- grants them on @C:\\nix@.  'StoreAccess' grants the same where the
-- machine has no build accounts' group, so a single-user store is as it
-- always was.  Where it has one, each of those entries applies to the
-- store's directory alone, so what a build makes there takes its token's
-- own default DACL rather than one passed down, and the group gets the
-- right to create files and directories there and nothing else.
-- 'ParentAccess' grants what 'StateAccess' does less Users' entry, and
-- 'OwnerAccess' grants full control to the service alone.
accessSddl :: StoreAccounts -> DirAccess -> Text
accessSddl accounts access = protectedDacl <> foldMap grantSddl (accessGrants accounts access)

-- | The security descriptor, in SDDL, that each node of a path is given
-- when the path is registered, on Windows: the service as its owner, and
-- a protected DACL of allow entries inherited below that grants full
-- control to the service, SYSTEM and Administrators, read and execute to
-- Users and the build accounts' group where there is one, and read
-- access to its own DACL alone to its owner (@OWNER RIGHTS@), which
-- takes the place of the rights an owner otherwise has whatever the DACL
-- says.  A build account that made a node, and owns it where the
-- service has not taken it, can then change nothing of it.
validPathSddl :: StoreAccounts -> Text
validPathSddl accounts =
  "O:" <> sidText (saService accounts) <> protectedDacl <> foldMap grantSddl grants
  where
    grants =
      map (Grant PassedDown fileAllAccess) (trustedAccounts accounts)
        <> [Grant PassedDown readAndExecute usersSid]
        <> [Grant PassedDown readAndExecute builders | builders <- toList (saBuilders accounts)]
        <> [Grant PassedDown readControl ownerRightsSid]

-- | One allow entry of a DACL nova-nix writes: whether it is passed to
-- what a directory holds, the rights, and the account it grants them to.
data Grant = Grant !Inheritance !Word32 !Sid

-- | Whether an entry is passed to the files and directories below, as
-- well as applying to the directory itself.
data Inheritance = PassedDown | ItselfAlone

-- | Each entry a directory of the given access is created with.
accessGrants :: StoreAccounts -> DirAccess -> [Grant]
accessGrants accounts access = case access of
  StoreAccess -> case saBuilders accounts of
    Nothing -> stateGrants
    Just builders -> map (Grant ItselfAlone fileAllAccess) trusted <> [Grant ItselfAlone readAndExecute usersSid, Grant ItselfAlone createOnly builders]
  StateAccess -> stateGrants
  ParentAccess -> map (Grant PassedDown fileAllAccess) trusted
  OwnerAccess -> [Grant PassedDown fileAllAccess (saService accounts)]
  where
    trusted = trustedAccounts accounts
    stateGrants = map (Grant PassedDown fileAllAccess) trusted <> [Grant PassedDown readAndExecute usersSid]

-- | The accounts the installer trusts with a store, the service in the
-- user's place: the service, named once when it is SYSTEM itself,
-- SYSTEM and Administrators.
trustedAccounts :: StoreAccounts -> [Sid]
trustedAccounts accounts = [service | service /= localSystemSid] <> [localSystemSid, administratorsSid]
  where
    service = saService accounts

-- | An allow entry (@A@) in SDDL, its flags @OICI@ when it is passed
-- down, the installer's @ContainerInherit, ObjectInherit@ with no
-- propagation flags, and none when it applies to the directory itself
-- alone.  Its object and inherited-object type fields stay empty: they
-- apply to directory-service objects, not to files.
grantSddl :: Grant -> Text
grantSddl (Grant inheritance rights sid) = "(A;" <> flags <> ";0x" <> T.pack (showHex rights "") <> ";;;" <> sidText sid <> ")"
  where
    flags = case inheritance of
      PassedDown -> "OICI"
      ItselfAlone -> ""

-- | SDDL's DACL (@D:@), protected (@P@): it takes no entry from the
-- parent, as the installer's @SetAccessRuleProtection($true, $false)@
-- makes its own.
protectedDacl :: Text
protectedDacl = "D:P"

-- | The rights to create files and directories in a directory and
-- nothing else: @FILE_ADD_FILE@, @FILE_ADD_SUBDIRECTORY@,
-- @FILE_TRAVERSE@, @FILE_READ_ATTRIBUTES@ and @SYNCHRONIZE@ (winnt.h).
createOnly :: Word32
createOnly = 0x2 .|. 0x4 .|. 0x20 .|. 0x80 .|. 0x100000

-- | @READ_CONTROL@ (winnt.h): reading a node's security descriptor.
readControl :: Word32
readControl = 0x20000

-- | @FILE_ALL_ACCESS@ (winnt.h), .NET's @FileSystemRights.FullControl@.
fileAllAccess :: Word32
fileAllAccess = 0x1F01FF

-- | @FILE_GENERIC_READ | FILE_GENERIC_EXECUTE@ (winnt.h): .NET's
-- @FileSystemRights.ReadAndExecute@ with the @Synchronize@ right .NET
-- adds to every allow entry it writes.
readAndExecute :: Word32
readAndExecute = 0x1200A9

-- | SYSTEM, the installer's @LocalSystemSid@.
localSystemSid :: Sid
localSystemSid = Sid "S-1-5-18"

-- | The local Administrators group, the installer's
-- @BuiltinAdministratorsSid@.
administratorsSid :: Sid
administratorsSid = Sid "S-1-5-32-544"

-- | The local Users group, the installer's @BuiltinUsersSid@.
usersSid :: Sid
usersSid = Sid "S-1-5-32-545"

-- | @OWNER RIGHTS@, the SID whose entry stands for an object's owner in
-- place of the rights its owner otherwise holds.
ownerRightsSid :: Sid
ownerRightsSid = Sid "S-1-3-4"
