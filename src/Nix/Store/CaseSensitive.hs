{-# LANGUAGE CPP #-}

-- | Case sensitivity of the store filesystem: the runtime probe behind
-- the store's sibling-name handling, and the per-directory capability
-- behind true-name NAR materialization on NTFS.
--
-- A folding filesystem lands two sibling names differing only by case
-- on one file, so an unpacker there has to keep them apart on disk
-- (upstream's case-hack suffix) or make the directory sensitive (the
-- NTFS per-directory flag, the mechanism WSL uses to host Linux trees).
-- Which of those a store needs is a property of the volume holding it,
-- not of the operating system: APFS is formatted either way at volume
-- creation, so the store probes its own directory when it opens and
-- carries the answer, rather than deciding at compile time by OS name.
module Nix.Store.CaseSensitive
  ( CaseSensitivity (..),
    probeCaseSensitivity,
    trySetCaseSensitiveDir,
  )
where

#if defined(darwin_HOST_OS)

import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..))
import System.Posix.Internals (withFilePath)

#elif defined(mingw32_HOST_OS)

import Foreign.C.String (CWString, withCWString)
import Foreign.C.Types (CInt (..))

#endif

-- | How the filesystem under a store compares sibling names.
data CaseSensitivity
  = -- | Names differing only by case are distinct entries.
    CaseSensitive
  | -- | Names differing only by case resolve to one entry.
    CaseInsensitive
  deriving (Eq, Show)

#if defined(darwin_HOST_OS)

foreign import ccall unsafe "nn_darwinfs.h nn_path_case_sensitive"
  c_nn_path_case_sensitive :: CString -> IO CInt

-- | The case sensitivity of the volume holding an EXISTING path, from
-- @pathconf(_PC_CASE_SENSITIVE)@: the default APFS format folds, a
-- volume formatted "Case-sensitive APFS" does not.  No answer (the
-- path is missing, or the filesystem does not implement the query)
-- reads as folding, which is upstream's Darwin assumption and the safe
-- side: the case-hack on a sensitive volume only spells names oddly,
-- while true names on a folding volume lose files.
probeCaseSensitivity :: FilePath -> IO CaseSensitivity
probeCaseSensitivity path =
  withFilePath path (fmap classify . c_nn_path_case_sensitive)
  where
    classify answer
      | answer == probeAnswerSensitive = CaseSensitive
      | otherwise = CaseInsensitive

-- | The shim's answer for a sensitive volume (@nn_darwinfs.h@): 1, with
-- 0 for a folding one and -1 for no answer.
probeAnswerSensitive :: CInt
probeAnswerSensitive = 1

#elif defined(mingw32_HOST_OS)

-- | Win32 resolves names case-insensitively unless a directory carries
-- the NTFS per-directory flag, which the store sets itself where a
-- tree needs it ('trySetCaseSensitiveDir'), so the volume-level answer
-- is the Win32 default.
probeCaseSensitivity :: FilePath -> IO CaseSensitivity
probeCaseSensitivity _ = pure CaseInsensitive

#else

-- | Linux has no @pathconf@ name for case sensitivity and its
-- filesystems are case-sensitive by convention, the same assumption
-- upstream makes there (its case-hack is off outside Darwin).
probeCaseSensitivity :: FilePath -> IO CaseSensitivity
probeCaseSensitivity _ = pure CaseSensitive

#endif

#ifdef mingw32_HOST_OS

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
-- macOS (chosen at volume creation, and probed above) and inherent on
-- Linux.
trySetCaseSensitiveDir :: FilePath -> IO Bool
trySetCaseSensitiveDir _ = pure False

#endif
