{-# LANGUAGE CPP #-}

-- | A directory's DACL as Windows reads it back, for the store access
-- tests: whether it is protected and each of its entries, through
-- GetNamedSecurityInfoW (test/cbits/nn_test_dacl.c), and the user the
-- process runs as.  A POSIX directory has no DACL, so both answer
-- 'Nothing' there.
module DirDacl
  ( Dacl (..),
    DaclEntry (..),
    directoryDacl,
    processUserSid,
  )
where

import Data.Text (Text)
import Data.Word (Word32, Word8)

#if defined(mingw32_HOST_OS)
import Control.Exception (finally)
import Control.Monad (when)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Foreign.C.String (CString, CWString, peekCString, peekCWString, withCWString)
import Foreign.C.Types (CInt (..), CLong (..), CULong (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Marshal.Utils (maybePeek)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
#endif

-- | One entry of a DACL: its type (0 allows, 1 denies), its flags (the
-- inheritance bits among them), its access mask, and the SID it names,
-- which only an allow or deny entry is read for.
data DaclEntry = DaclEntry
  { deType :: !Word8,
    deFlags :: !Word8,
    deMask :: !Word32,
    deSid :: !(Maybe Text)
  }
  deriving (Eq, Ord, Show)

-- | A directory's DACL: whether it is protected, taking no entry from
-- its parent, and its entries in order.
data Dacl = Dacl
  { daclProtected :: !Bool,
    daclEntries :: ![DaclEntry]
  }
  deriving (Eq, Show)

-- | The DACL of the directory at the path, or 'Nothing' off Windows.  A
-- failure to read it, or a directory with no DACL to read, throws.
directoryDacl :: FilePath -> IO (Maybe Dacl)

-- | The SID of the user this process runs as, as nova-nix reads it for
-- the grants it writes, or 'Nothing' off Windows.
processUserSid :: IO (Maybe Text)

#if defined(mingw32_HOST_OS)

foreign import ccall unsafe "nn_test_dacl_read"
  c_nn_test_dacl_read :: CWString -> Ptr (Ptr ()) -> IO CULong

foreign import ccall unsafe "nn_test_dacl_protected"
  c_nn_test_dacl_protected :: Ptr () -> IO CInt

foreign import ccall unsafe "nn_test_dacl_count"
  c_nn_test_dacl_count :: Ptr () -> IO CLong

foreign import ccall unsafe "nn_test_dacl_entry"
  c_nn_test_dacl_entry :: Ptr () -> CULong -> Ptr CULong -> Ptr CWString -> IO CULong

foreign import ccall unsafe "nn_token_user_sid"
  c_nn_token_user_sid :: Ptr CWString -> Ptr CString -> IO CULong

foreign import ccall unsafe "LocalFree"
  c_LocalFree :: Ptr a -> IO (Ptr a)

directoryDacl path =
  withCWString path $ \cPath -> alloca $ \out -> do
    succeeded "GetNamedSecurityInfoW" =<< c_nn_test_dacl_read cPath out
    descriptor <- peek out
    (Just <$> readDacl descriptor) `finally` c_LocalFree descriptor

processUserSid =
  alloca $ \sidOut -> alloca $ \failedOut -> do
    code <- c_nn_token_user_sid sidOut failedOut
    call <- maybePeek peekCString =<< peek failedOut
    succeeded (fromMaybe "nn_token_user_sid" call) code
    Just <$> (takeWideString =<< peek sidOut)

-- | The protection and entries of a security descriptor's DACL.
readDacl :: Ptr () -> IO Dacl
readDacl descriptor = do
  protected <- c_nn_test_dacl_protected descriptor
  count <- c_nn_test_dacl_count descriptor
  when (protected < 0) (ioError (userError "GetSecurityDescriptorControl failed"))
  when (count < 0) (ioError (userError "the directory has no DACL to read"))
  Dacl (protected == 1) <$> traverse (readEntry descriptor . fromIntegral) [0 .. count - 1]

-- | One entry of a security descriptor's DACL.
readEntry :: Ptr () -> CULong -> IO DaclEntry
readEntry descriptor index =
  allocaArray entryFieldCount $ \fields -> alloca $ \sidOut -> do
    succeeded "GetAce" =<< c_nn_test_dacl_entry descriptor index fields sidOut
    values <- peekArray entryFieldCount fields
    sid <- peek sidOut
    named <- maybePeek takeWideString sid
    case values of
      [kind, flags, mask] -> pure (DaclEntry (fromIntegral kind) (fromIntegral flags) (fromIntegral mask) named)
      _ -> ioError (userError "nn_test_dacl_entry wrote the wrong number of fields")

-- | The fields nn_test_dacl_entry writes: type, flags and mask.
entryFieldCount :: Int
entryFieldCount = 3

-- | A string the Win32 API allocated, read and then released.
takeWideString :: CWString -> IO Text
takeWideString str = (T.pack <$> peekCWString str) `finally` c_LocalFree str

-- | Throw unless a Win32 call's error code is 0.
succeeded :: String -> CULong -> IO ()
succeeded call code =
  when (code /= 0) (ioError (userError (call <> " failed with Win32 error " <> show code)))

#else

directoryDacl _ = pure Nothing

processUserSid = pure Nothing

#endif
