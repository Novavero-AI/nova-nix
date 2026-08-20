-- | Per-store-path locking: mutual exclusion across delete, materialize,
-- and register.
--
-- == Why store paths lock
--
-- Two processes sharing a store can interleave substitution of one path:
-- process A removes the stale destination, process B materializes and
-- registers, then A materializes over B's tree - a valid database row
-- pointing at deleted or torn bytes.  Upstream C++ Nix
-- (@src\/libstore\/pathlocks.cc@, and the substitution path in
-- @local-store.cc@) prevents this with an exclusive lock on a
-- @\<store-path\>.lock@ file held across the whole
-- delete-materialize-register sequence, re-checking validity under the
-- lock so a waiter adopts the winner's finished work instead of redoing
-- it.
--
-- == Why filelock, not base's file locks
--
-- The lock is taken on a raw descriptor through the @filelock@
-- library (@flock@ on POSIX, @LockFileEx@ on Windows; CC0-licensed,
-- so nothing encumbers this package's Apache-2.0 distribution).
-- Base's 'GHC.IO.Handle.Lock' cannot express this lock: GHC's handle
-- registry forbids a second in-process writable handle on one file,
-- while Linux's open-file-description locks refuse an exclusive lock
-- on a read-only descriptor, so any 'System.IO.Handle' design must
-- pick between spurious in-process open failures and @EBADF@ at lock
-- time.  @filelock@'s descriptor lives outside the registry, opened
-- write-access and created atomically; the @flock@ lock attaches to
-- the open description, so it excludes other holders in this process
-- and any other, and releases when the description closes, which also
-- covers a crashed holder.  @flock@'s guarantee is for local
-- filesystems - the store's single-machine model.
--
-- == Lock files persist
--
-- Upstream deletes a lock file once its holder finishes, which opens the
-- deleted-lock-file hazard: a waiter blocked on the old file can acquire
-- it just after deletion and then hold a lock no later process can see;
-- upstream closes the hazard by writing a marker byte before deleting
-- and having every acquirer re-check the file it locked.  Here lock
-- files are never deleted: the file a waiter blocked on is always the
-- file the next holder locks, the marker dance disappears, and Windows -
-- where deleting a file another process holds open fails anyway - needs
-- no separate path.  The cost is one empty @\<store-path\>.lock@ per
-- substituted path left beside it in the store directory; the files are
-- inert debris, invisible to path queries (only exact store-path
-- basenames resolve).
module Nix.Store.Lock
  ( -- * Held locks
    PathLock,
    acquirePathLock,
    tryAcquirePathLock,
    releasePathLock,
    withPathLock,

    -- * Naming
    pathLockFilePath,
    lockFileSuffix,
  )
where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar)
import Control.Exception (bracket)
import Nix.Store.Path (StoreDir, StorePath, storePathToFilePath)
import System.FileLock (FileLock, SharedExclusive (Exclusive), lockFile, tryLockFile, unlockFile)
import System.IO (hPutStrLn, stderr)

-- | An exclusive lock held on one store path: the lock file's path and
-- the descriptor whose OS lock is the exclusion.  The descriptor sits
-- behind an 'MVar' so release is idempotent and thread-safe: a bracket
-- and an explicit release can both fire on one lock without a
-- double-close.
data PathLock = PathLock !FilePath !(MVar (Maybe FileLock))

-- | Identity is the lock file; the descriptor is process-local plumbing.
instance Eq PathLock where
  PathLock leftFile _ == PathLock rightFile _ = leftFile == rightFile

instance Show PathLock where
  show (PathLock lockedFile _) = "PathLock " <> show lockedFile

-- | Upstream's lock-file naming convention: the lock for a store path
-- lives beside it, under the path's own name plus this suffix.
lockFileSuffix :: FilePath
lockFileSuffix = ".lock"

-- | The lock file guarding one store path.
pathLockFilePath :: StoreDir -> StorePath -> FilePath
pathLockFilePath dir sp = storePathToFilePath dir sp <> lockFileSuffix

-- | Take the exclusive lock on a store path, blocking until granted.
-- Blocking is upstream's behavior on a busy path lock, announced the
-- same way ('waitingForLockMessage') so a stalled substitution names
-- what it is waiting for.  The non-blocking probe runs first, so the
-- contended case announces itself before the wait begins; the lock
-- file is created if absent, atomically at the open.
acquirePathLock :: StoreDir -> StorePath -> IO PathLock
acquirePathLock dir sp = do
  let lockPath = pathLockFilePath dir sp
  probe <- tryLockFile lockPath Exclusive
  held <- case probe of
    Just granted -> pure granted
    Nothing -> do
      hPutStrLn stderr (waitingForLockMessage lockPath)
      lockFile lockPath Exclusive
  heldRef <- newMVar (Just held)
  pure (PathLock lockPath heldRef)

-- | Upstream's log line for a busy path lock.
waitingForLockMessage :: FilePath -> String
waitingForLockMessage lockPath = "waiting for lock on '" <> lockPath <> "'..."

-- | Take the exclusive lock only if it is free: 'Nothing' when another
-- holder - this process's or another's - already has it.
tryAcquirePathLock :: StoreDir -> StorePath -> IO (Maybe PathLock)
tryAcquirePathLock dir sp = do
  let lockPath = pathLockFilePath dir sp
  probe <- tryLockFile lockPath Exclusive
  case probe of
    Nothing -> pure Nothing
    Just granted -> do
      heldRef <- newMVar (Just granted)
      pure (Just (PathLock lockPath heldRef))

-- | Release a held lock.  Closing the descriptor releases the OS lock
-- under every backend; the lock file stays - never deleted, see the
-- module header.  Idempotent: a second release finds the descriptor
-- already surrendered and does nothing.
releasePathLock :: PathLock -> IO ()
releasePathLock (PathLock _ heldRef) = modifyMVar_ heldRef surrender
  where
    surrender Nothing = pure Nothing
    surrender (Just held) = do
      unlockFile held
      pure Nothing

-- | Run an action holding a path's lock, released on every exit.
withPathLock :: StoreDir -> StorePath -> (PathLock -> IO a) -> IO a
withPathLock dir sp = bracket (acquirePathLock dir sp) releasePathLock
