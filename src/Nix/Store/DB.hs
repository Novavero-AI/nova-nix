-- | SQLite database for store path registration.
--
-- == Why a database?
--
-- The store directory is just files on disk.  But Nix needs to track
-- metadata about each path that isn't in the filesystem:
--
-- * __References__: which other store paths does this path depend on?
--   (Needed for garbage collection - can't delete a path that others
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

    -- * Types
    PathRegistration (..),
    PathInfo (..),

    -- * Lifecycle
    openStoreDB,
    closeStoreDB,

    -- * Registration
    registerPath,
    registerPaths,
    isValidPath,
    queryReferences,
    queryDeriver,
    queryPathInfo,
    queryAllValidPaths,

    -- * Unregistration
    UnregisterResult (..),
    unregisterPathRow,

    -- * Constants
    metaDirName,
    dbFileName,
  )
where

import Control.Exception (throwIO)
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple
  ( Connection,
    Only (..),
    Query (..),
    close,
    execute,
    execute_,
    open,
    query,
    withTransaction,
  )
import Nix.Store.Path (StoreDir (..), StorePath, storePathToFilePath)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

-- ---------------------------------------------------------------------------
-- Named constants
-- ---------------------------------------------------------------------------

-- | Subdirectory under the store for metadata.
metaDirName :: FilePath
metaDirName = ".nova-nix"

-- | SQLite database filename.
dbFileName :: FilePath
dbFileName = "db.sqlite"

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Opaque handle to the store database.
-- Wraps a SQLite connection and the store directory.
data StoreDB = StoreDB
  { sdbDir :: !StoreDir,
    sdbConn :: !Connection
  }

-- | Information needed to register a store path.
data PathRegistration = PathRegistration
  { prPath :: !StorePath,
    prNarHash :: !Text,
    prNarSize :: !Int,
    prDeriver :: !(Maybe Text),
    prReferences :: ![StorePath]
  }
  deriving (Eq, Show)

-- | Stored information about a registered path.
data PathInfo = PathInfo
  { piPath :: !Text,
    piNarHash :: !Text,
    piNarSize :: !Int,
    piDeriver :: !(Maybe Text),
    piRegTime :: !Int
  }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- SQL statements
-- ---------------------------------------------------------------------------

-- | Create the ValidPaths table.
createValidPathsSQL :: String
createValidPathsSQL =
  "CREATE TABLE IF NOT EXISTS ValidPaths (\
  \  id               INTEGER PRIMARY KEY AUTOINCREMENT,\
  \  path             TEXT UNIQUE NOT NULL,\
  \  hash             TEXT NOT NULL,\
  \  registrationTime INTEGER NOT NULL,\
  \  deriver          TEXT,\
  \  narSize          INTEGER NOT NULL\
  \)"

-- | Create the Refs table.
createRefsSQL :: String
createRefsSQL =
  "CREATE TABLE IF NOT EXISTS Refs (\
  \  referrer  INTEGER NOT NULL REFERENCES ValidPaths(id),\
  \  reference INTEGER NOT NULL REFERENCES ValidPaths(id),\
  \  PRIMARY KEY (referrer, reference)\
  \)"

-- ---------------------------------------------------------------------------
-- Lifecycle
-- ---------------------------------------------------------------------------

-- | Open (or create) the store database.
-- Creates the store directory, metadata subdirectory, and database
-- tables if they don't exist.  Enables WAL mode for concurrency and
-- foreign-key enforcement for referential integrity.
openStoreDB :: StoreDir -> IO StoreDB
openStoreDB dir = do
  let storeRoot = unStoreDir dir
      metaDir = storeRoot </> metaDirName
      dbPath = metaDir </> dbFileName
  createDirectoryIfMissing True metaDir
  conn <- open dbPath
  execute_ conn "PRAGMA journal_mode=WAL"
  -- SQLite leaves foreign keys OFF per connection; without this the Refs
  -- REFERENCES clauses are inert, and deleting a ValidPaths row (path
  -- deletion, garbage collection) would leave dangling Refs edges that
  -- closure JOINs silently under-report.
  execute_ conn "PRAGMA foreign_keys=ON"
  execute_ conn (fromString createValidPathsSQL)
  execute_ conn (fromString createRefsSQL)
  pure StoreDB {sdbDir = dir, sdbConn = conn}
  where
    fromString = Query . T.pack

-- | Close the store database.
closeStoreDB :: StoreDB -> IO ()
closeStoreDB db = close (sdbConn db)

-- ---------------------------------------------------------------------------
-- Registration
-- ---------------------------------------------------------------------------

-- | Register a single store path as valid with its metadata and references.
-- A convenience wrapper over 'registerPaths' for one path.
registerPath :: StoreDB -> PathRegistration -> IO ()
registerPath db reg = registerPaths db [reg]

-- | Register several store paths as valid in one transaction.
--
-- ALL path rows are inserted BEFORE any reference edge, so references among the
-- paths in this batch - e.g. intra-derivation cross-output references - are
-- never dropped.  (Registering one path at a time loses an edge whenever a
-- referrer is registered before its referent.)
--
-- Re-registering an existing path refreshes its metadata (NAR hash, size,
-- deriver) via @ON CONFLICT DO UPDATE@, so a path first registered with a
-- placeholder hash is corrected on a later real registration.
registerPaths :: StoreDB -> [PathRegistration] -> IO ()
registerPaths db regs = withTransaction (sdbConn db) $ do
  mapM_ (insertPathRow db) regs
  mapM_ (insertPathRefs db) regs

-- | Insert (or refresh) a single ValidPaths row.
insertPathRow :: StoreDB -> PathRegistration -> IO ()
insertPathRow db reg = do
  -- DB rows key store paths in PLATFORM spelling (storePathToFilePath):
  -- the database is host-local state describing this host's store tree,
  -- every writer and reader in this module uses the same spelling, and
  -- the store dir itself is host configuration.  Identity artifacts
  -- (drv ATerm, narinfo, eval-visible store-path strings) spell
  -- canonically; the DB is deliberately not one of them.
  let pathText = T.pack (storePathToFilePath (sdbDir db) (prPath reg))
  execute
    (sdbConn db)
    "INSERT INTO ValidPaths (path, hash, registrationTime, deriver, narSize) \
    \VALUES (?, ?, strftime('%s','now'), ?, ?) \
    \ON CONFLICT(path) DO UPDATE SET hash = excluded.hash, narSize = excluded.narSize, deriver = excluded.deriver"
    (pathText, prNarHash reg, prDeriver reg, prNarSize reg)

-- | Insert the reference edges for a path whose row already exists.
--
-- Re-registration REPLACES the edge set: the metadata-refresh contract
-- (see 'registerPaths') applies to references too, and keeping the union
-- of old and new edges would over-report - 'queryReferences' feeds pushed
-- narinfos, which would advertise references the path no longer has.
--
-- A reference to an unregistered path is an error, not a skip: silently
-- dropping the edge under-reports the same narinfos and hands a future GC
-- permission to delete a live dependency.  Referents must be registered
-- first or in the same 'registerPaths' batch (path rows are all inserted
-- before any edge).
insertPathRefs :: StoreDB -> PathRegistration -> IO ()
insertPathRefs db reg = do
  let conn = sdbConn db
      pathText = T.pack (storePathToFilePath (sdbDir db) (prPath reg))
  referrerRows <- query conn "SELECT id FROM ValidPaths WHERE path = ?" (Only pathText) :: IO [Only Int]
  case referrerRows of
    (Only referrerId : _) -> do
      execute conn "DELETE FROM Refs WHERE referrer = ?" (Only referrerId)
      mapM_ (insertRef conn referrerId) (prReferences reg)
    [] -> pure () -- Should not happen: the row was just inserted above.
  where
    insertRef conn referrerId refPath = do
      let refPathText = T.pack (storePathToFilePath (sdbDir db) refPath)
      refRows <- query conn "SELECT id FROM ValidPaths WHERE path = ?" (Only refPathText) :: IO [Only Int]
      case refRows of
        (Only refId : _) ->
          execute conn "INSERT OR IGNORE INTO Refs (referrer, reference) VALUES (?, ?)" (referrerId, refId)
        [] ->
          throwIO
            ( userError
                ( "registerPaths: "
                    <> storePathToFilePath (sdbDir db) (prPath reg)
                    <> " references unregistered path "
                    <> T.unpack refPathText
                )
            )

-- ---------------------------------------------------------------------------
-- Queries
-- ---------------------------------------------------------------------------

-- | Check if a store path is registered as valid.
isValidPath :: StoreDB -> StorePath -> IO Bool
isValidPath db sp = do
  let pathText = T.pack (storePathToFilePath (sdbDir db) sp)
  rows <- query (sdbConn db) "SELECT 1 FROM ValidPaths WHERE path = ? LIMIT 1" (Only pathText) :: IO [Only Int]
  pure (not (null rows))

-- | Query the references of a registered store path.
-- Returns the full path strings of referenced store paths.
--
-- Reads return the stored text without re-parsing: every row was
-- written from a validated 'StorePath' inside this module's
-- transactions, so this is trust-on-read of host-local state the
-- module itself wrote (the delete path documents the same stance for
-- its raw-basename key).  Callers that need a 'StorePath' back parse
-- at their own boundary.
queryReferences :: StoreDB -> StorePath -> IO [Text]
queryReferences db sp = do
  let pathText = T.pack (storePathToFilePath (sdbDir db) sp)
  rows <-
    query
      (sdbConn db)
      "SELECT vp2.path FROM Refs r \
      \JOIN ValidPaths vp1 ON r.referrer = vp1.id \
      \JOIN ValidPaths vp2 ON r.reference = vp2.id \
      \WHERE vp1.path = ?"
      (Only pathText) ::
      IO [Only Text]
  pure [p | Only p <- rows]

-- | Query every registered valid path, as full path text in registration
-- order of the table (sorted for determinism).
queryAllValidPaths :: StoreDB -> IO [Text]
queryAllValidPaths db = do
  rows <- query (sdbConn db) "SELECT path FROM ValidPaths ORDER BY path" () :: IO [Only Text]
  pure [p | Only p <- rows]

-- | Query the deriver of a registered store path.
queryDeriver :: StoreDB -> StorePath -> IO (Maybe Text)
queryDeriver db sp = do
  let pathText = T.pack (storePathToFilePath (sdbDir db) sp)
  rows <- query (sdbConn db) "SELECT deriver FROM ValidPaths WHERE path = ?" (Only pathText) :: IO [Only (Maybe Text)]
  case rows of
    (Only deriver : _) -> pure deriver
    [] -> pure Nothing

-- ---------------------------------------------------------------------------
-- Unregistration
-- ---------------------------------------------------------------------------

-- | Outcome of 'unregisterPathRow'.
data UnregisterResult
  = -- | The row and its outgoing reference edges were removed.
    RowUnregistered
  | -- | No row carries this path text.
    RowAbsent
  | -- | Other valid paths still reference this one (their path texts,
    -- sorted); nothing was changed.
    RowReferenced ![Text]
  deriving (Eq, Show)

-- | Remove a path's ValidPaths row and outgoing Refs edges, keyed by the
-- EXACT stored path text.  Deliberately not keyed by 'StorePath': the
-- rows this exists to clean up include ones whose names the current
-- validator rejects, and those cannot round-trip through a parse.
--
-- Refuses while any OTHER valid path references this one; a
-- self-reference does not block.  Lookup, referrer check, and deletion
-- run in one transaction, so a registration cannot interleave between
-- the check and the delete.
unregisterPathRow :: StoreDB -> Text -> IO UnregisterResult
unregisterPathRow db pathText = withTransaction conn $ do
  idRows <- query conn "SELECT id FROM ValidPaths WHERE path = ?" (Only pathText) :: IO [Only Int]
  case idRows of
    [] -> pure RowAbsent
    (Only pathId : _) -> do
      referrerRows <-
        query
          conn
          "SELECT vp.path FROM Refs r \
          \JOIN ValidPaths vp ON r.referrer = vp.id \
          \WHERE r.reference = ? AND r.referrer != ? \
          \ORDER BY vp.path"
          (pathId, pathId) ::
          IO [Only Text]
      case [p | Only p <- referrerRows] of
        referrers@(_ : _) -> pure (RowReferenced referrers)
        [] -> do
          execute conn "DELETE FROM Refs WHERE referrer = ?" (Only pathId)
          execute conn "DELETE FROM ValidPaths WHERE id = ?" (Only pathId)
          pure RowUnregistered
  where
    conn = sdbConn db

-- | Query full path info for a registered store path.
queryPathInfo :: StoreDB -> StorePath -> IO (Maybe PathInfo)
queryPathInfo db sp = do
  let pathText = T.pack (storePathToFilePath (sdbDir db) sp)
  rows <-
    query
      (sdbConn db)
      "SELECT path, hash, narSize, deriver, registrationTime FROM ValidPaths WHERE path = ?"
      (Only pathText) ::
      IO [(Text, Text, Int, Maybe Text, Int)]
  case rows of
    ((pth, hsh, sz, drv, regTime) : _) ->
      pure $
        Just
          PathInfo
            { piPath = pth,
              piNarHash = hsh,
              piNarSize = sz,
              piDeriver = drv,
              piRegTime = regTime
            }
    [] -> pure Nothing
