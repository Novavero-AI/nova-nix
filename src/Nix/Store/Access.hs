{-# LANGUAGE CPP #-}

-- | Who a directory the store creates is open to, and how it is created
-- that way.
--
-- A store's directory and its state directory are readable by every
-- user and changeable only by the user nova-nix runs as and the
-- system's administrators ('StoreAccess').  A directory created on the
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
-- A parent created on the way to a store grants full control to the
-- user, SYSTEM and Administrators and nothing to other users, so no
-- other user can rename it and put a store of their own in its place,
-- and nothing it holds is opened to other users unless it says so
-- itself.  The installer accepts such a @C:\\nix@, whose every entry
-- names an account it trusts, and gives it its own DACL, which adds read
-- and execute for Users.
--
-- A directory that already exists keeps the list it has.
module Nix.Store.Access
  ( -- * Who a directory is open to
    DirAccess (..),

    -- * Creating a directory
    createDirectoryWith,
    createStateDirectory,

    -- * The Windows access control list
    Sid,
    sidFromText,
    sidText,
    accessSddl,
  )
where

import Data.Char (isDigit, isHexDigit)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Word (Word32)
import Numeric (showHex)

#if defined(mingw32_HOST_OS)

import Control.Exception (bracket)
import Control.Monad (unless, void, when)
import Data.Maybe (fromMaybe)
import Foreign.C.String (CString, CWString, peekCString, peekCWString, withCWString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (maybePeek)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import Nix.Store.Win32Path (win32FilePath)
import System.Directory (doesDirectoryExist)
import System.FilePath (takeDirectory)
import System.IO.Error (catchIOError, ioeSetFileName, isAlreadyExistsError, isDoesNotExistError, isPermissionError, modifyIOError)
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
    -- SYSTEM and Administrators: a store's directory and its state
    -- directory.
    StoreAccess
  | -- | On Windows readable and changeable by the user nova-nix runs as,
    -- SYSTEM and Administrators alone, and on POSIX the mode of a store's
    -- directory: a directory created on the way to a store.
    ParentAccess
  | -- | The user nova-nix runs as alone: a build directory.
    OwnerAccess
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Creating a directory
-- ---------------------------------------------------------------------------

-- | Create a directory open to whom the access says, failing with an
-- 'System.IO.Error.isAlreadyExistsError' exception when anything already
-- answers to the name, a symlink included, which is not followed.  Its
-- parent must exist.  On POSIX its mode is 0777 for 'StoreAccess' and
-- 'ParentAccess' and 0700 for 'OwnerAccess', each narrowed by the
-- umask; on Windows it is created with the protected DACL 'accessSddl'
-- describes for the user the process runs as.
createDirectoryWith :: DirAccess -> FilePath -> IO ()
#if defined(mingw32_HOST_OS)
createDirectoryWith access path =
  modifyIOError (`ioeSetFileName` path) $ do
    user <- tokenUserSid
    target <- win32FilePath path
    withCWString target $ \cTarget ->
      withCWString (T.unpack (accessSddl user access)) $ \cSddl ->
        failUnlessSuccess "CreateDirectory" (c_nn_dir_create_sddl cTarget cSddl)
#else
createDirectoryWith access path = Posix.createDirectory path $ case access of
  StoreAccess -> accessModes
  ParentAccess -> accessModes
  OwnerAccess -> ownerModes
#endif

-- | Create a store's state directory, given its path, where it is
-- missing, with the store directory it is in and any missing parent of
-- that, as @createDirectoryIfMissing True@ does.  On POSIX that is all
-- it does: the modes are the default, narrowed by the umask.  On
-- Windows the store directory and the state directory, where this
-- creates them, are open to whom 'StoreAccess' says, and each parent of
-- the store directory this creates to whom 'ParentAccess' says.  A
-- directory already there keeps the access it has.
createStateDirectory :: FilePath -> IO ()
#if defined(mingw32_HOST_OS)
createStateDirectory stateDir = do
  createDirectoryIfMissingWith StoreAccess (takeDirectory stateDir)
  createDirectoryIfMissingWith StoreAccess stateDir
#else
createStateDirectory = createDirectoryIfMissing True
#endif

#if defined(mingw32_HOST_OS)

foreign import ccall unsafe "nn_winfs.h nn_token_user_sid"
  c_nn_token_user_sid :: Ptr CWString -> Ptr CString -> IO ErrCode

foreign import ccall unsafe "nn_winfs.h nn_dir_create_sddl"
  c_nn_dir_create_sddl :: CWString -> CWString -> IO ErrCode

-- | 'createDirectoryWith', keeping a directory already at the path as
-- it is, and creating each missing parent first with 'ParentAccess', as
-- @createDirectoryIfMissing True@ creates parents: only once the
-- directory's own creation finds no parent, so nothing is created above
-- a parent that exists.  A refusal for want of permission is kept too
-- when a directory answers to the name, as @createDirectoryIfMissing@
-- keeps one: Windows can refuse so to create a directory that exists,
-- the current one at a drive root among them.
createDirectoryIfMissingWith :: DirAccess -> FilePath -> IO ()
createDirectoryIfMissingWith access path = attempt `catchIOError` missingParent
  where
    attempt = createDirectoryWith access path `catchIOError` keepDirectory
    -- A root is its own parent, and the attempt's failure stands there.
    parent = takeDirectory path
    missingParent err
      | isDoesNotExistError err && parent /= path = do
          createDirectoryIfMissingWith ParentAccess parent
          attempt
      | otherwise = ioError err
    keepDirectory err
      | isAlreadyExistsError err || isPermissionError err = do
          present <- doesDirectoryExist path
          unless present (ioError err)
      | otherwise = ioError err

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
-- The Windows access control list
-- ---------------------------------------------------------------------------

-- | A security identifier in the string form ConvertSidToStringSidW
-- writes: @S-1-5-18@ for SYSTEM.  'sidFromText' is the one way to make
-- one, so the text 'accessSddl' writes as an entry's trustee is a SID
-- and nothing more.
newtype Sid = Sid Text
  deriving (Eq, Show)

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
-- access is created with on Windows, for the given user: a protected
-- DACL (@D:P@) of allow entries, each inherited by files and
-- directories below (@OICI@).  'StoreAccess' grants full control to the
-- user, SYSTEM and Administrators and read and execute to Users, in
-- that order, naming the user once when it is SYSTEM itself, as
-- install-nova-nix grants them on @C:\\nix@.  'ParentAccess' grants the
-- same less Users' entry, and 'OwnerAccess' grants full control to the
-- user alone.
accessSddl :: Sid -> DirAccess -> Text
accessSddl user access = protectedDacl <> foldMap entry (accessGrants user access)
  where
    -- An entry's object and inherited-object type fields stay empty:
    -- they apply to directory-service objects, not to files.
    entry (Sid sid, rights) = "(" <> allowInheritedBelow <> ";0x" <> T.pack (showHex rights "") <> ";;;" <> sid <> ")"

-- | Each principal a directory of the given access grants rights to,
-- with those rights.
accessGrants :: Sid -> DirAccess -> [(Sid, Word32)]
accessGrants user access = case access of
  StoreAccess -> trusted <> [(usersSid, readAndExecute)]
  ParentAccess -> trusted
  OwnerAccess -> [(user, fileAllAccess)]
  where
    -- The accounts the installer trusts with a store, each with full
    -- control.
    trusted =
      [(user, fileAllAccess) | user /= localSystemSid]
        <> [(localSystemSid, fileAllAccess), (administratorsSid, fileAllAccess)]

-- | SDDL's DACL (@D:@), protected (@P@): it takes no entry from the
-- parent, as the installer's @SetAccessRuleProtection($true, $false)@
-- makes its own.
protectedDacl :: Text
protectedDacl = "D:P"

-- | SDDL's allow entry (@A@), inherited by files (@OI@) and directories
-- (@CI@) below and applying to the directory itself: the installer's
-- @ContainerInherit, ObjectInherit@ with no propagation flags.
allowInheritedBelow :: Text
allowInheritedBelow = "A;OICI"

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
