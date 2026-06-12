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

    -- * Constants
    metaDirName,
    dbFileName,
  )
where

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
import Nix.Store.Path (StoreDir (..), StorePath (..), storePathToFilePath)
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
-- tables if they don't exist.  Enables WAL mode for concurrency.
openStoreDB :: StoreDir -> IO StoreDB
openStoreDB dir = do
  let storeRoot = unStoreDir dir
      metaDir = storeRoot </> metaDirName
      dbPath = metaDir </> dbFileName
  createDirectoryIfMissing True metaDir
  conn <- open dbPath
  execute_ conn "PRAGMA journal_mode=WAL"
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
  let pathText = T.pack (storePathToFilePath (sdbDir db) (prPath reg))
  execute
    (sdbConn db)
    "INSERT INTO ValidPaths (path, hash, registrationTime, deriver, narSize) \
    \VALUES (?, ?, strftime('%s','now'), ?, ?) \
    \ON CONFLICT(path) DO UPDATE SET hash = excluded.hash, narSize = excluded.narSize, deriver = excluded.deriver"
    (pathText, prNarHash reg, prDeriver reg, prNarSize reg)

-- | Insert the reference edges for a path whose row already exists.
insertPathRefs :: StoreDB -> PathRegistration -> IO ()
insertPathRefs db reg = do
  let conn = sdbConn db
      pathText = T.pack (storePathToFilePath (sdbDir db) (prPath reg))
  referrerRows <- query conn "SELECT id FROM ValidPaths WHERE path = ?" (Only pathText) :: IO [Only Int]
  case referrerRows of
    (Only referrerId : _) -> mapM_ (insertRef conn referrerId) (prReferences reg)
    [] -> pure () -- Should not happen: the row was just inserted above.
  where
    insertRef conn referrerId refPath = do
      let refPathText = T.pack (storePathToFilePath (sdbDir db) refPath)
      refRows <- query conn "SELECT id FROM ValidPaths WHERE path = ?" (Only refPathText) :: IO [Only Int]
      case refRows of
        (Only refId : _) ->
          execute conn "INSERT OR IGNORE INTO Refs (referrer, reference) VALUES (?, ?)" (referrerId, refId)
        [] -> pure () -- Reference not in this batch or the store yet - skip.

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
