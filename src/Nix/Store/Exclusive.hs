{-# LANGUAGE CPP #-}

-- | Creating store entries that must not exist yet, and upstream's
-- words when one does.
--
-- A materializer that creates or truncates (@WriteMode@) trusts its own
-- bookkeeping to know that a name is free, and on a case-folding volume
-- that bookkeeping is an approximation of the volume's fold table: a
-- sibling it keeps apart but the volume folds lands on the earlier
-- file and replaces it.  An exclusive create asks the volume itself,
-- which is the only party that knows, and refuses instead.  Upstream's
-- restore opens every regular file this way (@O_CREAT | O_EXCL@ in
-- @RestoreSink::createRegularFile@, fs-sink.cc at 2.24.9, and
-- @CREATE_NEW@ in its Windows branch by 2.33.2).
--
-- @base@ has no exclusive open, so the platform's own call is made
-- through @unix@ or @Win32@ and the descriptor is handed to the I\/O
-- library as an ordinary 'Handle'.  The path is an 'OsPath', so a POSIX
-- name reaches @open@ as the bytes it was spelled from, and an error
-- names the path as base names one it opened.
module Nix.Store.Exclusive
  ( -- * Creation
    openNewBinaryFile,

    -- * Upstream's refusals
    Occupant (..),
    fileTakenMessage,
    directoryTakenMessage,
    symlinkTakenMessage,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import System.OsPath (OsPath, decodeFS)

#if defined(mingw32_HOST_OS)

import Control.Exception (bracketOnError)
import Data.Bits ((.|.))
import Data.List (isPrefixOf)
import GHC.IO.SubSystem (IoSubSystem (..), ioSubSystem)
import System.IO (Handle)
import System.IO.Error (ioeSetFileName, modifyIOError)
import System.Win32.File
  ( cREATE_NEW,
    closeHandle,
    createFile,
    fILE_ATTRIBUTE_NORMAL,
    fILE_FLAG_OVERLAPPED,
    fILE_SHARE_READ,
    fILE_SHARE_WRITE,
    gENERIC_READ,
    gENERIC_WRITE,
  )
import System.Win32.Info (getFullPathName)
import System.Win32.Types (hANDLEToHandle)

#else

import Control.Exception (bracketOnError)
import qualified Data.ByteString.Char8 as BS8
import GHC.IO.Handle.FD (fdToHandle')
import System.IO (Handle, IOMode (WriteMode))
import System.IO.Error (ioeSetFileName, modifyIOError)
import qualified System.OsPath as OP
import System.Posix.Files (stdFileMode)
import System.Posix.IO
  ( OpenFileFlags (cloexec, creat, exclusive),
    OpenMode (WriteOnly),
    closeFd,
    defaultFileFlags,
  )
import System.Posix.IO.ByteString (openFd)
import System.Posix.Types (Fd (..))

#endif

-- ---------------------------------------------------------------------------
-- Creation
-- ---------------------------------------------------------------------------

#if defined(mingw32_HOST_OS)

-- | Create a regular file and open it for binary writing, failing with
-- an 'System.IO.Error.isAlreadyExistsError' exception when any entry
-- already answers to the name, including one the volume's fold table
-- matches to it.  Access and sharing are upstream's restore on Windows
-- (2.33.2), and what @base@ requests for a write under its
-- descriptor-based I\/O manager; the overlapped flag is what the
-- native I\/O manager needs from a handle it adopts.
openNewBinaryFile :: OsPath -> IO Handle
openNewBinaryFile osPath = do
  -- A Windows 'OsPath' is UTF-16, so decoding it is exact.
  path <- decodeFS osPath
  modifyIOError (`ioeSetFileName` path) $ do
    target <- win32FilePath path
    bracketOnError
      ( createFile
          target
          (gENERIC_READ .|. gENERIC_WRITE)
          (fILE_SHARE_READ .|. fILE_SHARE_WRITE)
          Nothing
          cREATE_NEW
          attributes
          Nothing
      )
      closeHandle
      hANDLEToHandle
  where
    attributes = case ioSubSystem of
      IoPOSIX -> fILE_ATTRIBUTE_NORMAL
      IoNative -> fILE_ATTRIBUTE_NORMAL .|. fILE_FLAG_OVERLAPPED

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

#else

-- | Create a regular file and open it for binary writing, failing with
-- an 'System.IO.Error.isAlreadyExistsError' exception when any entry
-- already answers to the name, including one the volume's fold table
-- matches to it.  The mode is @base@'s for a new file (0666, narrowed
-- by the umask), and the handle is named by the path, as @base@ names
-- the handles it opens.  Close-on-exec keeps a concurrently spawned
-- builder from inheriting the descriptor, as upstream's restore does.
openNewBinaryFile :: OsPath -> IO Handle
openNewBinaryFile path = do
  shown <- decodeFS path
  modifyIOError (`ioeSetFileName` shown) $
    bracketOnError
      (openFd (rawPath path) WriteOnly newFileFlags)
      closeFd
      (\(Fd fd) -> fdToHandle' fd Nothing False shown WriteMode True)
  where
    -- A POSIX path unit is a byte, and 'OP.toChar' reads it as the
    -- character of that code, so the round trip through 'BS8' is exact.
    rawPath = BS8.pack . map OP.toChar . OP.unpack
    newFileFlags =
      defaultFileFlags
        { creat = Just stdFileMode,
          exclusive = True,
          cloexec = True
        }

#endif

-- ---------------------------------------------------------------------------
-- Upstream's refusals
-- ---------------------------------------------------------------------------

-- | What already answers to the name a directory was to take.
-- Upstream words the two apart: @create_directory@ reports an existing
-- directory by returning false and anything else by throwing.
data Occupant
  = OccupiedByDirectory
  | OccupiedByOther
  deriving (Eq, Show)

-- | Upstream's refusal of a regular file whose name is taken
-- (@RestoreSink::createRegularFile@, fs-sink.cc at 2.24.9).  The path
-- reaches its formatter as a @std::filesystem::path@, whose stream
-- insertion goes through @std::quoted@: hence the double quotes inside
-- the single ones.
fileTakenMessage :: FilePath -> Text
fileTakenMessage path = "creating file '" <> cxxQuoted path <> "': " <> existsReason

-- 2.24.9 printed the C++ library's filesystem_error text for the two
-- refusals below, which is implementation-defined and differs by
-- platform; 2.25 wrapped it in the messages 2.33.2 prints, which are
-- followed.

-- | Upstream's refusal of a directory whose name is taken: its own
-- error over an existing directory (@RestoreSink::createDirectory@,
-- fs-sink.cc at 2.24.9), and 2.33.2's over anything else.
directoryTakenMessage :: Occupant -> FilePath -> Text
directoryTakenMessage occupant path = case occupant of
  OccupiedByDirectory -> "path '" <> T.pack path <> "' already exists"
  OccupiedByOther -> "creating directory '" <> T.pack path <> "': " <> existsReason

-- | Upstream's refusal of a link whose name is taken, given the link's
-- path and its target (@RestoreSink::createSymlink@, fs-sink.cc at
-- 2.33.2).
symlinkTakenMessage :: FilePath -> FilePath -> Text
symlinkTakenMessage linkPath target =
  "creating symlink from '" <> T.pack linkPath <> "' -> '" <> T.pack target <> "': " <> existsReason

-- | @strerror(EEXIST)@, the reason the refusals here end with.
existsReason :: Text
existsReason = "File exists"

-- | @std::quoted@ over a string: double quotes around it, and a double
-- quote or backslash inside escaped with a backslash.
cxxQuoted :: FilePath -> Text
cxxQuoted path = "\"" <> T.concatMap escape (T.pack path) <> "\""
  where
    escape c
      | c == '"' || c == '\\' = T.pack ['\\', c]
      | otherwise = T.singleton c
