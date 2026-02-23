-- | The Nix store: content-addressed, immutable package storage.
--
-- == What the store actually does
--
-- When Nix builds a package, the output goes into the store:
--
-- 1. Builder runs in a temp directory, produces output files
-- 2. Output is scanned for references to other store paths
-- 3. Output is moved to @\/nix\/store\/\<hash\>-\<name\>@
-- 4. Path is registered in the SQLite DB with its references
-- 5. Directory permissions set to read-only (immutability)
--
-- When Nix SUBSTITUTES (downloads from a binary cache):
--
-- 1. Fetch @\<hash\>.narinfo@ from cache — contains NAR hash, size, refs
-- 2. Fetch the @.nar.xz@ file
-- 3. Verify file hash matches narinfo
-- 4. Decompress and unpack NAR into store path
-- 5. Verify NAR hash matches narinfo
-- 6. Register path in DB with references from narinfo
--
-- Both paths end the same way: a registered, immutable store path.
--
-- == Garbage collection
--
-- A GC root is an explicit "keep this" marker (e.g. the current system
-- profile, per-user profiles, result symlinks from @nix-build@).
-- GC walks all roots, follows references transitively, and deletes
-- everything not reachable.  Since paths are immutable and reference
-- tracking is exact, GC is safe — it never deletes something in use.
module Nix.Store
  ( -- * Store operations
    Store,
    openStore,
    closeStore,

    -- * Queries
    isValid,
    pathExists,

    -- * Re-exports
    module Nix.Store.Path,
  )
where

import Nix.Store.DB (StoreDB, closeStoreDB, isValidPath, openStoreDB)
import Nix.Store.Path
import System.Directory (doesDirectoryExist)

-- | An open store with database and configuration.
data Store = Store
  { stDir :: !StoreDir,
    stDB :: !StoreDB
  }

-- | Open a Nix store at the given directory.
-- Creates the store directory and database if they don't exist.
openStore :: StoreDir -> IO Store
openStore dir = do
  db <- openStoreDB dir
  pure Store {stDir = dir, stDB = db}

-- | Close the store (flushes the database).
closeStore :: Store -> IO ()
closeStore = closeStoreDB . stDB

-- | Check if a store path is registered as valid in the database.
isValid :: Store -> StorePath -> IO Bool
isValid = isValidPath . stDB

-- | Check if a store path directory exists on disk (regardless of DB).
pathExists :: Store -> StorePath -> IO Bool
pathExists store sp = doesDirectoryExist (storePathToFilePath (stDir store) sp)
