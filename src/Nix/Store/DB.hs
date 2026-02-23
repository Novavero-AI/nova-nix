-- | SQLite database for store path registration.
--
-- == Why a database?
--
-- The store directory is just files on disk.  But Nix needs to track
-- metadata about each path that isn't in the filesystem:
--
-- * __References__: which other store paths does this path depend on?
--   (Needed for garbage collection — can't delete a path that others
--   reference.)
-- * __Registrant__: who put this path here? (Substituted from cache?
--   Built locally?)
-- * __Deriver__: which .drv file produced this output? (For @nix-store -q
--   --deriver@.)
-- * __NAR hash__: SHA-256 of the path's NAR serialization. (For integrity
--   verification.)
-- * __NAR size__: byte count of the NAR. (For disk usage reporting.)
-- * __Validity__: has this path been verified? (A path can exist on disk
--   but be invalid if the build was interrupted.)
--
-- C++ Nix uses SQLite for this.  So do we.  The database lives at
-- @\/nix\/store\/.nova-nix\/db.sqlite@ (or @C:\\nix\\store\\.nova-nix\\db.sqlite@).
module Nix.Store.DB
  ( -- * Database handle
    StoreDB,

    -- * Lifecycle
    openStoreDB,
    closeStoreDB,

    -- * Registration
    registerPath,
    isValidPath,
    queryReferences,
  )
where

import Data.Text (Text)
import Nix.Store.Path (StoreDir (..), StorePath)

-- | Opaque handle to the store database.
-- Wraps a SQLite connection.
-- TODO: add Connection field when sqlite-simple is wired
newtype StoreDB = StoreDB
  { sdbDir :: StoreDir
  }

-- | Open (or create) the store database.
openStoreDB :: StoreDir -> IO StoreDB
openStoreDB dir = do
  -- TODO: open SQLite at storeDir/.nova-nix/db.sqlite
  -- TODO: run migrations (create tables if not exist)
  pure StoreDB {sdbDir = dir}

-- | Close the store database.
closeStoreDB :: StoreDB -> IO ()
closeStoreDB _db = pure () -- TODO: close SQLite connection

-- | Register a store path as valid with its references.
registerPath :: StoreDB -> StorePath -> [StorePath] -> IO ()
registerPath _db _path _refs = pure () -- TODO: INSERT into ValidPaths + Refs

-- | Check if a store path is registered as valid.
isValidPath :: StoreDB -> StorePath -> IO Bool
isValidPath _db _path = pure False -- TODO: SELECT from ValidPaths

-- | Query the references of a registered store path.
queryReferences :: StoreDB -> StorePath -> IO [Text]
queryReferences _db _path = pure [] -- TODO: SELECT from Refs
