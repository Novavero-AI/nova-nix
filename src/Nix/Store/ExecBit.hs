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
import qualified NovaCache.Hash as Hash
import qualified NovaCache.NAR as NAR
import qualified System.Directory as Dir
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

-- | nova-cache serialise options wiring 'isExecutable' in as the
-- exec-bit source of truth: the walk asks the resolver per regular
-- file, with the on-disk (case-hack-suffixed) name, which is exactly
-- the file 'isExecutable' stats.  On Unix 'isExecutable' reads the
-- owner-execute permission, the same answer the default resolver
-- gives, so one options value serves both platforms.
streamOptions :: NAR.SerialiseOptions
streamOptions = NAR.defaultSerialiseOptions {NAR.soExecBit = isExecutable}

-- | 'NovaCache.NAR.serialiseFromPath', with every regular file's
-- executable flag taken from 'isExecutable' rather than from the
-- file's permissions.  The walk is nova-cache's own -- name safety,
-- case-hack folding and entry ordering are subtle and belong there --
-- and the resolver hook (nova-cache 0.11.1) answers the flag during
-- the walk, replacing the post-hoc rewrite this module used to do.
serialiseFromPath :: FilePath -> IO NAR.NarEntry
serialiseFromPath = NAR.serialiseFromPathOpts streamOptions

-- | The NAR hash of a path, read through the same exec-bit source of
-- truth 'serialiseFromPath' writes.  Streams on every platform:
-- 'NAR.withNarSourceOpts' pulls the archive in chunks with the ADS
-- resolver answering the flags, and the digest folds over them, so no
-- file's contents are ever held whole.  Before the resolver hook,
-- Windows had to materialize the tree in memory to rewrite the flags.
narHashOfPath :: FilePath -> IO Hash.NixHash
narHashOfPath path =
  NAR.withNarSourceOpts streamOptions path $ \pull ->
    let go !ctx = do
          chunk <- pull
          if BS.null chunk
            then pure (Hash.hashFinalize ctx)
            else go (Hash.hashUpdate ctx chunk)
     in go Hash.hashInit
