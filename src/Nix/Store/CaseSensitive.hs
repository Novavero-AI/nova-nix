{-# LANGUAGE CPP #-}

-- | Per-directory case sensitivity: the capability probe behind
-- true-name NAR materialization on a folding filesystem.
--
-- Windows NTFS exposes case sensitivity as a per-directory flag (the
-- mechanism WSL uses to host Linux trees), so an unpacker can hold
-- case-variant siblings under their REAL names instead of mangling
-- them.  Other platforms report unsupported - APFS fixes sensitivity
-- per volume at creation, Linux needs nothing - and the caller falls
-- back to the case-hack.
module Nix.Store.CaseSensitive (trySetCaseSensitiveDir) where

#ifdef mingw32_HOST_OS

import Foreign.C.String (CWString, withCWString)
import Foreign.C.Types (CInt (..))

foreign import ccall unsafe "nn_winfs.h nn_dir_set_case_sensitive"
  c_nn_dir_set_case_sensitive :: CWString -> IO CInt

-- | Enable case-sensitive naming on an EMPTY directory the caller just
-- created.  True means the directory now holds case-variant sibling
-- names as distinct files; False (non-NTFS volume, policy) means fall
-- back to the case-hack.
trySetCaseSensitiveDir :: FilePath -> IO Bool
trySetCaseSensitiveDir path =
  withCWString path (fmap (/= 0) . c_nn_dir_set_case_sensitive)

#else

-- | Unsupported off Windows: sensitivity is a volume-level property on
-- macOS (chosen at store-volume creation) and inherent on Linux.
trySetCaseSensitiveDir :: FilePath -> IO Bool
trySetCaseSensitiveDir _ = pure False

#endif
