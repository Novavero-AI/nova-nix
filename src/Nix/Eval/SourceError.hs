-- | The messages upstream gives a failed read of a source path.
--
-- Nix 2.24.9 reads source paths through @PosixSourceAccessor@, which
-- names the access that failed and the path, then appends @strerror@ as
-- every @SysError@ does.  GHC raises the same failures as 'IOException's
-- whose text names the Haskell function instead, so each access the
-- evaluator makes is tagged and its failure reworded here.
module Nix.Eval.SourceError
  ( SourceAccess (..),
    sourceErrorMessage,
  )
where

import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Foreign.C.Error (Errno (..), eISDIR, eNOTDIR)
import GHC.IO.Exception (IOErrorType (InappropriateType), IOException (..))
import System.IO.Error (isDoesNotExistError)

-- | The access to a source path that failed.
data SourceAccess
  = -- | An lstat.  An absent path (@ENOENT@ or @ENOTDIR@, which
    -- @maybeLstat@ in file-system.cc treats alike) is
    -- @SourceAccessor::lstat@'s @path '%s' does not exist@, anything else
    -- @maybeLstat@'s @getting status of '%s'@.
    StatPath
  | -- | Opening a file and reading it (@PosixSourceAccessor::readFile@):
    -- @opening file '%s'@, or @reading from file '%s'@ for a directory,
    -- which upstream opens and then fails to read.
    ReadFile
  | -- | Listing a directory (@PosixSourceAccessor::readDirectory@):
    -- @reading directory %s@, the path unquoted.
    ReadDirectory
  deriving (Eq, Show)

-- | Upstream's message for an access to a path that raised an exception.
sourceErrorMessage :: SourceAccess -> Text -> IOException -> Text
sourceErrorMessage access path err = case access of
  StatPath
    | isDoesNotExistError err || errnoIs eNOTDIR -> "path '" <> path <> "' does not exist"
    | otherwise -> "getting status of '" <> path <> "': " <> reason
  ReadFile
    | isDirectory -> "reading from file '" <> path <> "': " <> isDirectoryReason
    | otherwise -> "opening file '" <> path <> "': " <> reason
  ReadDirectory -> "reading directory " <> path <> ": " <> reason
  where
    errnoIs code = fmap Errno (ioe_errno err) == Just code
    -- An errno-derived IOException carries strerror as its description.
    reason = T.pack (ioe_description err)
    -- GHC's openFile refuses a directory itself, with no errno and its own
    -- lower-case wording; upstream's open succeeds and the read fails
    -- with EISDIR.
    isDirectory =
      errnoIs eISDIR || (ioe_type err == InappropriateType && isNothing (ioe_errno err) && ioe_description err == ghcIsDirectory)
    ghcIsDirectory = "is a directory"
    isDirectoryReason = "Is a directory"
