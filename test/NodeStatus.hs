{-# LANGUAGE CPP #-}

-- | A node's mode and modification time as @lstat@ reports them, for the
-- store metadata tests: "System.Directory" reads only the owner's
-- permissions, and follows a symlink for its time.  Windows has no mode,
-- so it answers 'Nothing' there.
module NodeStatus
  ( NodeStatus (..),
    nodeStatus,
  )
where

import Data.Int (Int64)
import Data.Word (Word32)

#if !defined(mingw32_HOST_OS)
import Data.Bits (complement, (.&.))
import Foreign.C.Types (CTime (..))
import System.Posix.Files (fileMode, fileTypeModes, getSymbolicLinkStatus, modificationTime)
#endif

-- | A node's mode below its file type, and its modification time in
-- whole seconds since the epoch.
data NodeStatus = NodeStatus
  { nsPermissions :: !Word32,
    nsModificationTime :: !Int64
  }
  deriving (Eq, Show)

-- | The status of a node, of a symlink itself rather than its target.
nodeStatus :: FilePath -> IO (Maybe NodeStatus)
#if defined(mingw32_HOST_OS)
nodeStatus _ = pure Nothing
#else
nodeStatus path = do
  status <- getSymbolicLinkStatus path
  let CTime seconds = modificationTime status
  pure (Just (NodeStatus (fromIntegral (fileMode status .&. complement fileTypeModes)) seconds))
#endif
