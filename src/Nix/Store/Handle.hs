-- | The open-store handle: the store directory, its database, and the
-- handle's lease on the store-wide garbage-collector lock.
--
-- == The lease
--
-- Every open t'Store' holds @.nova-nix\/gc.lock@ shared from 'openStore'
-- to 'closeStore'.  A collector needs that lock exclusively, so a
-- collection runs only while no other process has the store open, and a
-- process opening the store while one runs waits for it to finish.
-- 'withCollectorLock' is the one place a handle trades its shared lease
-- for the exclusive lock and back; "Nix.Store.GC" states why this is the
-- protocol and what the per-path locks cannot express.
--
-- The lease is this handle's own descriptor, and the OS treats each
-- descriptor as a separate holder: a process that opens two handles on
-- one store directory and collects through either waits forever on its
-- own other lease.  One handle per store per process is the rule.
--
-- == The directory's spelling
--
-- The database keys every row by the store directory's spelling
-- ("Nix.Store.DB"), so 'openStore' keys the handle by one canonical
-- spelling ('canonicalStoreDir') whatever the caller typed: relative or
-- absolute, with or without a trailing separator, through @.@ and
-- @..@.  Symlinks are left unresolved, as upstream's @absPath@ leaves
-- them on a @--store@ path: a store that has always been opened through
-- a symlinked spelling has its rows keyed by that spelling, and
-- resolving it would make every one of them foreign.
module Nix.Store.Handle
  ( -- * Handle
    Store (..),
    openStore,
    closeStore,
    canonicalStoreDir,

    -- * The collector lock
    withCollectorLock,
    gcLockFileName,
    gcLockFilePath,
  )
where

import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar, withMVar)
import Control.Exception (SomeException, bracketOnError, mask, onException, throwIO, try)
import Data.Text (Text)
import qualified Data.Text as T
import Nix.Eval.CanonPath (canonPath)
import Nix.Logger (Logger)
import Nix.Store.CaseSensitive (CaseSensitivity, probeCaseSensitivity)
import Nix.Store.DB (StoreDB, closeStoreDB, createStoreDirectories, metaDirName, openStoreDB)
import Nix.Store.Lock (LockMode (..), PathLock, acquireLockFileWith, releasePathLock, withLockFileWith)
import Nix.Store.Path (StoreDir (..))
import System.Directory (makeAbsolute)
import System.FilePath ((</>))

-- | An open store with database and configuration.  'stDir' is the
-- canonical spelling ('canonicalStoreDir'); 'stGcLease' is the handle's
-- shared hold on the collector lock, which 'withCollectorLock' swaps for
-- the exclusive lock and back; after a failed renewal the handle holds
-- none until it is closed.
data Store = Store
  { stDir :: !StoreDir,
    stDB :: !StoreDB,
    stGcLease :: !(MVar PathLock),
    -- | How the volume holding the store compares sibling names, probed
    -- at open ('probeCaseSensitivity') and consulted by every NAR
    -- materialization into the store.
    stCaseSensitivity :: !CaseSensitivity,
    -- | Where an operation through the handle reports what it is doing:
    -- a wait on a lock, a collection's progress, a substitution's
    -- messages, a build's messages and the lines its builder writes.
    stLogger :: !Logger
  }

-- | Upstream's name for the store-wide garbage-collector lock file.
gcLockFileName :: FilePath
gcLockFileName = "gc.lock"

-- | The collector lock file, beside the database under the metadata
-- directory.
gcLockFilePath :: StoreDir -> FilePath
gcLockFilePath dir = unStoreDir dir </> metaDirName </> gcLockFileName

-- | The spelling a store directory is keyed by: absolute, with @.@ and
-- @..@ collapsed, separators normalised and no trailing separator,
-- symlinks unresolved.  That is upstream's @absPath@ of a @--store@
-- path; the module header says why symlinks stay.
canonicalStoreDir :: StoreDir -> IO StoreDir
canonicalStoreDir (StoreDir dir) = StoreDir . T.unpack . canonPath . T.pack <$> makeAbsolute dir

-- | Open a Nix store at the given directory, keyed by its canonical
-- spelling.  Creates the store directory and database if they don't
-- exist, the directories as 'createStoreDirectories' does.  The shared
-- lease is taken before the database is touched, so an opener arriving
-- during a collection reads nothing until the collection is over.  It
-- waits with the collector's own line: upstream's opener never waits
-- here (it hands its roots to the running collector through a socket
-- instead), so there is no upstream line for the wait, and the
-- collector's names what is being waited for.  A step that fails
-- once the lease is held releases what was taken before it, the database
-- and the lease, so a failed open holds nothing that a later collection
-- would wait on.  The line goes to the logger given, which the handle
-- keeps for its operations ('stLogger').
openStore :: Logger -> StoreDir -> IO Store
openStore logger rawDir = do
  dir <- canonicalStoreDir rawDir
  let lockPath = gcLockFilePath dir
  createStoreDirectories dir
  bracketOnError (acquireLockFileWith logger LockShared bigGcLockMessage lockPath) releasePathLock $ \lease ->
    bracketOnError (openStoreDB dir) closeStoreDB $ \db -> do
      -- After the directory exists: the probe answers for a path on disk,
      -- and a fresh store's directory sits on the volume it will live on.
      sensitivity <- probeCaseSensitivity (unStoreDir dir)
      leaseVar <- newMVar lease
      pure Store {stDir = dir, stDB = db, stGcLease = leaseVar, stCaseSensitivity = sensitivity, stLogger = logger}

-- | Close the store: the database first, then the lease, so the lock
-- outlives every write this handle made.  Waits for an in-progress
-- 'withCollectorLock' on another thread rather than racing its swap.
closeStore :: Store -> IO ()
closeStore store = do
  closeStoreDB (stDB store)
  withMVar (stGcLease store) releasePathLock

-- | Upstream's announcement when the collector lock is busy.
bigGcLockMessage :: Text
bigGcLockMessage = "waiting for the big garbage collector lock..."

-- | Run an action holding the collector lock exclusively.  The handle's
-- shared lease is released first (the OS would otherwise count it as a
-- competing holder), the exclusive lock is taken, blocking with
-- upstream's message while other handles are open, and a fresh shared
-- lease is taken back before the handle is returned to use, on every
-- exit.  Asynchronous exceptions are masked around the swap so the
-- handle is never left without a lease; the action itself runs
-- unmasked.  If the renewal itself fails (the lock file cannot be
-- opened again), the surrendered lease goes back instead, so the handle
-- stays closable: 'closeStore' releases it as a no-op, and the failure
-- reaches the caller, whose handle holds no lease until it is closed.
-- The message goes to the handle's logger.
withCollectorLock :: Store -> IO a -> IO a
withCollectorLock store act = mask $ \restore -> do
  lease <- takeMVar (stGcLease store)
  releasePathLock lease
  outcome <- try (restore (withLockFileWith (stLogger store) LockExclusive bigGcLockMessage lockPath (const act)))
  renewed <- acquireLockFileWith (stLogger store) LockShared bigGcLockMessage lockPath `onException` putMVar (stGcLease store) lease
  putMVar (stGcLease store) renewed
  either (\e -> throwIO (e :: SomeException)) pure outcome
  where
    lockPath = gcLockFilePath (stDir store)
