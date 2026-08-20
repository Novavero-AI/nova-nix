-- | The executable bit, on a platform that does not have one.
--
-- Windows' 'System.Directory.getPermissions' answers from the file
-- extension, not a real bit, so a NAR round-trip through it loses
-- executability and the substituter rejects a mismatched hash. The bit is
-- instead stored in an NTFS alternate data stream ('execStreamName') whose
-- presence is the bit: set before a store path is sealed read-only, and
-- carried across copies explicitly since 'Dir.copyFile' drops it. On Unix
-- this all collapses to the mode bit.
module Nix.Store.ExecBit
  ( isExecutable,
    markExecutable,
    copyExecMark,
    serialiseFromPath,
    narHashOfPath,
    execStreamName,
  )
where

import Control.Exception (bracket_)
import Control.Monad (unless, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8Lenient)
import qualified NovaCache.Hash as Hash
import qualified NovaCache.NAR as NAR
import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified System.Info

-- | The alternate data stream whose presence marks a file executable.
-- Named for this project so it cannot collide with @Zone.Identifier@ or
-- another tool's stream.
execStreamName :: String
execStreamName = "nova.exec"

-- | Whether the exec bit needs the stream representation.  A plain
-- comparison rather than CPP: the same binary is not built for both, but
-- keeping one code path means the Unix branch is type-checked on Windows
-- and vice versa.
usesStream :: Bool
usesStream = System.Info.os == "mingw32"

-- | The stream that marks a file, addressable through ordinary file IO.
execStreamPath :: FilePath -> FilePath
execStreamPath path = path ++ ":" ++ execStreamName

-- | Whether a regular file is executable, by whichever representation the
-- platform keeps it in.
isExecutable :: FilePath -> IO Bool
isExecutable path
  | usesStream = Dir.doesFileExist (execStreamPath path)
  | otherwise = Dir.executable <$> Dir.getPermissions path

-- | Mark a regular file executable.
--
-- A stream cannot be created on a read-only file, and a copy out of a
-- sealed store path is read-only by the time it exists: @directory@'s
-- 'Dir.copyFile' is @atomicCopyFileContents@ with @copyPermissions@ from
-- the source as its post-action, and that post-action runs on the
-- replacement file just before it is renamed into place, so on Windows the
-- destination carries the source's @FILE_ATTRIBUTE_READONLY@ from the
-- moment it appears.  Marking before the copy is not an option either --
-- the rename would replace whatever stream had been written.  So the write
-- is bracketed by clearing and restoring the attribute, which leaves an
-- already-writable file exactly as it was, and every caller gets this
-- rather than each remembering the order.
markExecutable :: FilePath -> IO ()
markExecutable path
  | usesStream = do
      perms <- Dir.getPermissions path
      let writable = Dir.writable perms
      bracket_
        (unless writable (Dir.setPermissions path (Dir.setOwnerWritable True perms)))
        (unless writable (Dir.setPermissions path perms))
        (BS8.writeFile (execStreamPath path) (BS8.pack "1"))
  | otherwise = do
      perms <- Dir.getPermissions path
      Dir.setPermissions path (Dir.setOwnerExecutable True perms)

-- | Carry a file's exec mark from one path to another.  'Dir.copyFile'
-- copies the unnamed stream only, so without this a copy silently
-- de-executables its result.
copyExecMark :: FilePath -> FilePath -> IO ()
copyExecMark from to = do
  exec <- isExecutable from
  when exec (markExecutable to)

-- | 'NovaCache.NAR.serialiseFromPath', with every regular file's executable
-- flag taken from 'isExecutable' rather than from the file's permissions.
--
-- The walk is nova-cache's own -- name safety, case-hack folding and entry
-- ordering are subtle and belong there -- and this only rewrites the flags
-- it produced.  On Unix those flags are already right, so the tree is
-- returned untouched.
serialiseFromPath :: FilePath -> IO NAR.NarEntry
serialiseFromPath path = do
  entry <- NAR.serialiseFromPath path
  if usesStream then correct path entry else pure entry
  where
    correct p (NAR.NarRegular _ contents) = do
      exec <- isExecutable p
      pure (NAR.NarRegular exec contents)
    correct p (NAR.NarDirectory entries) = do
      -- An entry's name is not always the name on disk.  nova-cache strips
      -- upstream's case-hack suffix from a folded sibling, so the entry
      -- reads @foo@ while the directory holds @foo~nix~case~hack~1@, and
      -- this walk runs only where that stripping is enabled.  The
      -- directory is listed once and each entry name resolved back through
      -- the same stripping, so the stat lands on the file that exists.
      diskNames <- Dir.listDirectory p
      let onDisk = Map.fromList [(stripCaseHack (T.pack n), n) | n <- diskNames]
          -- Decoded as UTF-8 (not BS8.unpack, which truncates each byte to
          -- a Char and mangles multi-byte names) since that is what a store
          -- path name always is.
          childName n =
            let entryName = decodeUtf8Lenient n
             in Map.findWithDefault (T.unpack entryName) entryName onDisk
      NAR.NarDirectory <$> mapM (\(n, e) -> (,) n <$> correct (p </> childName n) e) entries
    correct _ e@(NAR.NarSymlink _) = pure e

-- | The name a case-hacked on-disk name serialises under: everything
-- before nova-cache's @caseHackSuffix@, or the name itself when the
-- suffix is absent.  Mirrors @unhackedDirNames@'s @resolve@.
stripCaseHack :: T.Text -> T.Text
stripCaseHack = fst . T.breakOn (decodeUtf8Lenient NAR.caseHackSuffix)

-- | The NAR hash of a path, read through the same exec-bit source of truth
-- 'serialiseFromPath' writes.
--
-- On Unix the flags nova-cache's own walk reads are already right, so the
-- hash streams: 'NAR.withNarSource' pulls the archive in chunks and the
-- digest folds over them, and no file's contents are ever held.  On Windows
-- the bit lives in a stream that walk cannot see, so the tree goes through
-- 'serialiseFromPath' instead -- the same trade-off that function already
-- makes, and the only way a sink writing the mark and a verifier reading it
-- back agree about the same file.
narHashOfPath :: FilePath -> IO Hash.NixHash
narHashOfPath path
  | usesStream = NAR.narHash <$> serialiseFromPath path
  | otherwise = NAR.withNarSource NAR.defaultCaseHack path $ \pull ->
      let go !ctx = do
            chunk <- pull
            if BS.null chunk
              then pure (Hash.hashFinalize ctx)
              else go (Hash.hashUpdate ctx chunk)
       in go Hash.hashInit
