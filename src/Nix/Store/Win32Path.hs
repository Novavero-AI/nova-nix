-- | A path as the store's direct Win32 calls hand it to CreateFileW.
-- Built on Windows only: the POSIX store opens through @unix@, which
-- takes a path as it is.
module Nix.Store.Win32Path
  ( win32FilePath,
  )
where

import Data.List (isPrefixOf)
import System.Win32.Info (getFullPathName)

-- | The Win32 file-namespace spelling of a path (@\\\\?\\@), which
-- CreateFileW takes past MAX_PATH.  @base@ converts every path it
-- opens to this form, under either I\/O manager
-- (@__hs_create_device_name@ in GHC's utils\/fs\/fs.c), so a raw
-- CreateFileW without it would refuse long paths @base@ accepts.  The
-- namespace form is taken literally, so separators, @.@ and @..@ are
-- resolved first by GetFullPathNameW, as that conversion does.
win32FilePath :: FilePath -> IO FilePath
win32FilePath path
  | any (`isPrefixOf` path) namespacePrefixes = pure path
  | otherwise = toNamespace <$> getFullPathName path
  where
    namespacePrefixes = [fileNamespace, "\\\\.\\", "\\??\\"]
    toNamespace full = case full of
      '\\' : '\\' : share -> fileNamespace <> "UNC\\" <> share
      _ -> fileNamespace <> full
    fileNamespace = "\\\\?\\"
