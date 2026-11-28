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
-- * __Registration time__: when the path was registered.
-- * __Deriver__: which .drv file produced this output? (For @nix-store -q
--   --deriver@.)
-- * __NAR hash__: SHA-256 of the path's NAR serialization. (For integrity
--   verification.)
-- * __NAR size__: byte count of the NAR. (For disk usage reporting.)
-- * __Validity__: has this path been verified? (A path can exist on disk
--   but be invalid if the build was interrupted.)
-- * __Trust__: whether this store built the path, the signatures it
--   carries, and the content address its name was made from, if it was.
--
-- C++ Nix uses SQLite for this.  So do we.  The database lives at
-- @\/nix\/store\/.nova-nix\/db.sqlite@ (or @C:\\nix\\store\\.nova-nix\\db.sqlite@).
--
-- == The schema
--
-- The tables and columns are upstream's, under upstream's names, and the
-- trust columns hold what upstream's hold: @ultimate@ 1 for a path this
-- store built and NULL otherwise, @sigs@ the signatures spelled as a
-- narinfo spells them and separated by spaces, or NULL for none, and
-- @ca@ the content address as upstream renders it, or NULL for none.
-- @DerivationOutputs@ lists each valid derivation's outputs, so the
-- derivations that make a path are found without reading a @.drv@.  A
-- database an earlier nova-nix made has neither the trust columns nor
-- that table, and gains both when it is opened for writing
-- ('openStoreDB'), the table filled from the derivations it holds.
-- Unlike upstream's, a row names its path in the platform's spelling of
-- the store directory, the NAR hash is spelled in nix-base32, @narSize@
-- is never NULL, and @Refs@ has neither upstream's delete actions nor its
-- trigger, since this module removes a path's edges itself.
module Nix.Store.DB
  ( -- * Database handle
    StoreDB,

    -- * Types
    PathRegistration (..),
    PathInfo (..),
    Ultimate (..),

    -- * Lifecycle
    openStoreDB,
    openStoreDBReadOnly,
    storeTablesExist,
    closeStoreDB,
    createStoreDirectories,

    -- * Registration
    registerPath,
    registerPaths,
    isValidPath,
    queryReferences,
    queryAllReferences,
    queryDeriver,
    queryPathInfo,
    queryAllValidPaths,
    queryReferrers,
    queryValidDerivers,
    queryPathFromHashPart,

    -- * Unregistration
    UnregisterResult (..),
    unregisterPathRow,
    unregisterPathRows,

    -- * Constants
    metaDirName,
    dbFileName,
  )
where

import Control.Exception (IOException, bracketOnError, throwIO, try)
import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Foldable (for_, traverse_)
import Data.IORef (newIORef)
import Data.List (sortOn)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.SQLite.Simple
  ( Connection (..),
    Only (..),
    Query (..),
    SQLData (..),
    close,
    execute,
    executeMany,
    execute_,
    open,
    query,
    query_,
    withImmediateTransaction,
    withTransaction,
  )
import qualified Database.SQLite3 as SQLite3
import Nix.Daemon.Protocol (ContentAddress, Signature, parseContentAddress, parseSignature, renderContentAddress, renderSignature)
import Nix.Derivation (DerivationOutput (..), drvExtension, drvOutputs, fromATerm)
import Nix.Store.Access (checkStateDirectory, createStateDirectory)
import Nix.Store.Path (StoreDir (..), StorePath, defaultStoreDir, spName, storePathHashLen, storePathToFilePath, storePathToText)
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
    prReferences :: ![StorePath],
    -- | Whether this store built the path.
    prUltimate :: !Ultimate,
    -- | The signatures the path carries.
    prSignatures :: !(Set Signature),
    -- | The content address the path's name was made from, if it was.
    prContentAddress :: !(Maybe ContentAddress)
  }
  deriving (Eq, Show)

-- | Stored information about a registered path.
data PathInfo = PathInfo
  { piPath :: !Text,
    piNarHash :: !Text,
    piNarSize :: !Int,
    piDeriver :: !(Maybe Text),
    piRegTime :: !Int,
    -- | Whether this store built the path.
    piUltimate :: !Ultimate,
    -- | The signatures the path carries.
    piSignatures :: !(Set Signature),
    -- | The content address the path's name was made from, if it was.
    piContentAddress :: !(Maybe ContentAddress)
  }
  deriving (Eq, Show)

-- | Whether a path is ultimately trusted, upstream's @ultimate@: a
-- derivation output this store built is, and a path that reached it any
-- other way is not.
data Ultimate
  = Ultimate
  | NotUltimate
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
  \  narSize          INTEGER NOT NULL,\
  \  ultimate         INTEGER,\
  \  sigs             TEXT,\
  \  ca               TEXT\
  \)"

-- | The trust columns, with their types, which a ValidPaths table an
-- earlier nova-nix made lacks.
trustColumns :: [(Text, Text)]
trustColumns = [("ultimate", "INTEGER"), ("sigs", "TEXT"), ("ca", "TEXT")]

-- | The table of each valid derivation's outputs, upstream's: the
-- derivation's row, the output's name, and its path.  A derivation's
-- outputs go with its row.
derivationOutputsTable :: Text
derivationOutputsTable = "DerivationOutputs"

-- | Create the DerivationOutputs table and the index that finds a path's
-- derivations.
createDerivationOutputsSQL :: [Query]
createDerivationOutputsSQL =
  [ "CREATE TABLE IF NOT EXISTS DerivationOutputs (\
    \  drv  INTEGER NOT NULL,\
    \  id   TEXT NOT NULL,\
    \  path TEXT NOT NULL,\
    \  PRIMARY KEY (drv, id),\
    \  FOREIGN KEY (drv) REFERENCES ValidPaths(id) ON DELETE CASCADE\
    \)",
    "CREATE INDEX IF NOT EXISTS IndexDerivationOutputs ON DerivationOutputs(path)"
  ]

-- | The tables a store's database holds, as 'createValidPathsSQL' and
-- 'createRefsSQL' make them.
storeTables :: Set.Set Text
storeTables = Set.fromList ["ValidPaths", "Refs"]

-- | Create the Refs table.
createRefsSQL :: String
createRefsSQL =
  "CREATE TABLE IF NOT EXISTS Refs (\
  \  referrer  INTEGER NOT NULL REFERENCES ValidPaths(id),\
  \  reference INTEGER NOT NULL REFERENCES ValidPaths(id),\
  \  PRIMARY KEY (referrer, reference)\
  \)"

-- | Upstream's index that finds a path's referrers.
createReferenceIndexSQL :: Query
createReferenceIndexSQL = "CREATE INDEX IF NOT EXISTS IndexReference ON Refs(reference)"

-- ---------------------------------------------------------------------------
-- Lifecycle
-- ---------------------------------------------------------------------------

-- | Open (or create) the store database.
-- Creates the store directory and metadata subdirectory
-- ('createStoreDirectories') and the database tables if they don't
-- exist.  Enables WAL mode for concurrency and foreign-key enforcement
-- for referential integrity.  A statement that fails closes the
-- connection it ran on.
openStoreDB :: StoreDir -> IO StoreDB
openStoreDB dir = do
  createStoreDirectories dir
  bracketOnError (open dbPath) close (configureDB dir)
  where
    dbPath = unStoreDir dir </> metaDirName </> dbFileName

-- | Create the store directory and its metadata subdirectory where
-- they are missing, with any missing parent of the store directory, as
-- 'createStateDirectory' creates them: on Windows the two are open to
-- whom 'Nix.Store.Access.StoreAccess' says, and a parent this creates
-- to whom 'Nix.Store.Access.ParentAccess' says.  A directory already
-- there keeps the access it has, and on Windows one another account
-- could change is refused before anything is created in it.
createStoreDirectories :: StoreDir -> IO ()
createStoreDirectories dir = createStateDirectory (unStoreDir dir </> metaDirName)

-- | Set a connection to a store's database up: its pragmas and its
-- tables, bringing a database an earlier nova-nix made up to the schema
-- ('migrateSchema').
configureDB :: StoreDir -> Connection -> IO StoreDB
configureDB dir conn = do
  execute_ conn "PRAGMA journal_mode=WAL"
  waitWhileBusy conn
  -- SQLite leaves foreign keys OFF per connection; without this the Refs
  -- REFERENCES clauses are inert, and deleting a ValidPaths row (path
  -- deletion, garbage collection) would leave dangling Refs edges that
  -- closure JOINs silently under-report.
  execute_ conn "PRAGMA foreign_keys=ON"
  execute_ conn (fromString createValidPathsSQL)
  execute_ conn (fromString createRefsSQL)
  execute_ conn createReferenceIndexSQL
  let db = StoreDB {sdbDir = dir, sdbConn = conn}
  migrateSchema db
  pure db
  where
    fromString = Query . T.pack

-- | Add what a database an earlier nova-nix made lacks: the trust columns,
-- NULL in every row it holds, so each path reads back as not ultimate,
-- unsigned and with no content address, and the DerivationOutputs table,
-- filled from each valid derivation's file.  A derivation whose file
-- cannot be read or parsed has no outputs listed, as upstream would not
-- have registered it.  The upgrade runs in a write transaction and looks
-- again inside it, so of two processes opening the database at once one
-- upgrades it and the other finds nothing left to do; a database that
-- needs nothing takes no write lock.
migrateSchema :: StoreDB -> IO ()
migrateSchema db = do
  outdated <- schemaOutdated conn
  when outdated (withImmediateTransaction conn upgrade)
  where
    conn = sdbConn db
    upgrade = do
      columns <- validPathsColumns conn
      for_ trustColumns $ \(column, kind) ->
        unless (Set.member column columns) $
          execute_ conn (Query ("ALTER TABLE ValidPaths ADD COLUMN " <> column <> " " <> kind))
      tables <- databaseTables conn
      unless (Set.member derivationOutputsTable tables) $ do
        traverse_ (execute_ conn) createDerivationOutputsSQL
        derivations <- query_ conn "SELECT path FROM ValidPaths WHERE path LIKE '%.drv'" :: IO [Only Text]
        for_ derivations $ \(Only pathText) -> do
          contents <- try (BS.readFile (T.unpack pathText)) :: IO (Either IOException ByteString)
          case fromATerm <$> contents of
            Right (Right drv) -> insertDerivationOutputs db pathText (drvOutputs drv)
            _ -> pure ()

-- | Whether a database lacks a trust column or the DerivationOutputs
-- table.
schemaOutdated :: Connection -> IO Bool
schemaOutdated conn = do
  columns <- validPathsColumns conn
  tables <- databaseTables conn
  pure (not (all ((`Set.member` columns) . fst) trustColumns && Set.member derivationOutputsTable tables))

-- | The columns of a database's ValidPaths table.
validPathsColumns :: Connection -> IO (Set Text)
validPathsColumns conn = Set.fromList . map fromOnly <$> query_ conn "SELECT name FROM pragma_table_info('ValidPaths')"

-- | The tables a database holds.
databaseTables :: Connection -> IO (Set Text)
databaseTables conn = Set.fromList . map fromOnly <$> query_ conn "SELECT name FROM sqlite_master WHERE type = 'table'"

-- | Have a connection wait up to an hour for a peer's lock, as upstream's
-- does (sqlite3_busy_timeout(db, 60 * 60 * 1000) in the @SQLite@ constructor,
-- sqlite.cc), where SQLite's default timeout of zero fails at once with
-- ErrorBusy.  The pragma reaches the same API, and writes nothing.
waitWhileBusy :: Connection -> IO ()
waitWhileBusy conn = execute_ conn "PRAGMA busy_timeout=3600000"

-- | Open the database of a store that has one, for reading only.  The
-- connection is opened read-only and wrapped in sqlite-simple's
-- t'Connection', whose two fields the cabal file's bound on sqlite-simple
-- pins.  It runs no pragma that writes and no table statement, so the
-- database file is never written and nothing is created around it but
-- what SQLite itself needs.  For a database in WAL
-- mode, which a store's is, that is a wal-index: SQLite creates the
-- @-wal@ and @-shm@ files beside one that has none, and leaves them when
-- the connection closes, since a read-only connection cannot checkpoint;
-- the next connection that writes removes them as it closes.  Only an
-- immutable open creates neither, and it reads with no locks, so a
-- concurrent writer can give it wrong answers or corruption errors;
-- upstream opens immutably only a store it is told is read-only
-- (@LocalStore::openDB@ in local-store.cc).  Before the database is
-- opened, on Windows the directories on the way to it are checked as
-- 'checkStateDirectory' checks them, and a store another account could
-- change is refused.
openStoreDBReadOnly :: StoreDir -> IO StoreDB
openStoreDBReadOnly dir = do
  checkStateDirectory (unStoreDir dir </> metaDirName)
  bracketOnError opened close (\conn -> StoreDB dir conn <$ waitWhileBusy conn)
  where
    dbPath = unStoreDir dir </> metaDirName </> dbFileName
    opened = Connection <$> SQLite3.open2 (T.pack dbPath) [SQLite3.SQLOpenReadOnly] SQLite3.SQLVFSDefault <*> newIORef 0

-- | Whether a database holds the store's tables ('storeTables').  One
-- whose first open was cut short, or that another process is opening for
-- the first time, holds none of them yet.
storeTablesExist :: StoreDB -> IO Bool
storeTablesExist db = (storeTables `Set.isSubsetOf`) <$> databaseTables (sdbConn db)

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
-- paths in this batch - e.g. intra-derivation cross-output references -
-- resolve whatever the batch order.  (Registering one path at a time would
-- refuse a referrer registered before its referent.)
--
-- Re-registering an existing path refreshes its metadata (NAR hash, size,
-- deriver) via @ON CONFLICT DO UPDATE@, so a path first registered with a
-- placeholder hash is corrected on a later real registration.  A trust
-- field the new registration states replaces the recorded one, and one it
-- leaves absent (not ultimate, no signatures, no content address) keeps
-- what was recorded, so writing a derivation again does not drop the
-- signatures it was substituted with.
--
-- A derivation's outputs are listed in DerivationOutputs from its file in
-- the store, as upstream lists them from the file when it registers one,
-- so the file must be in place: one that cannot be read or parsed refuses
-- the batch with upstream's message, @error parsing derivation ...@.
--
-- A reference to a path neither registered nor in the batch refuses the
-- whole batch, recording nothing, with an 'IOError' carrying upstream's
-- message, @path \'\/nix\/store\/\<hash\>-\<name\>\' is not valid@
-- (@queryValidPathId@, local-store.cc at 2.24.9).  The path is spelled
-- under the logical store directory, as upstream's @printStorePath@
-- spells it, wherever the store's files are kept: a relocated store
-- still has @\/nix\/store@ as its @storeDir@ and only its @realStoreDir@
-- moves (local-fs-store.hh).  The edges go in upstream's order, the batch
-- by path and each path's references as a sorted set, so of several
-- unregistered references the one named is the one upstream names.  The
-- sort is stable, so of two registrations of one path in a batch the
-- later's edges still win.
registerPaths :: StoreDB -> [PathRegistration] -> IO ()
registerPaths db regs = withTransaction (sdbConn db) $ do
  mapM_ (insertPathRow db) regs
  mapM_ (insertPathRefs db) (sortOn prPath regs)
  mapM_ (registerDerivationOutputs db . prPath) (filter (isDerivation . prPath) regs)

-- | Whether a store path names a derivation, by its name, as upstream
-- tells one.
isDerivation :: StorePath -> Bool
isDerivation sp = drvExtension `T.isSuffixOf` spName sp

-- | List a registered derivation's outputs, read from its file.
registerDerivationOutputs :: StoreDB -> StorePath -> IO ()
registerDerivationOutputs db sp = do
  contents <- BS.readFile file
  case fromATerm contents of
    Left err -> throwIO (userError ("error parsing derivation '" <> T.unpack (storePathToText defaultStoreDir sp) <> "': " <> T.unpack err))
    Right drv -> insertDerivationOutputs db (T.pack file) (drvOutputs drv)
  where
    file = storePathToFilePath (sdbDir db) sp

-- | Record a derivation's outputs against its row, the derivation and its
-- outputs' paths spelled as rows spell them.
insertDerivationOutputs :: StoreDB -> Text -> [DerivationOutput] -> IO ()
insertDerivationOutputs db drvText outputs =
  for_ outputs $ \out ->
    execute
      (sdbConn db)
      "INSERT OR REPLACE INTO DerivationOutputs (drv, id, path) \
      \VALUES ((SELECT id FROM ValidPaths WHERE path = ?), ?, ?)"
      (drvText, doName out, T.pack (storePathToFilePath (sdbDir db) (doPath out)))

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
    "INSERT INTO ValidPaths (path, hash, registrationTime, deriver, narSize, ultimate, sigs, ca) \
    \VALUES (?, ?, strftime('%s','now'), ?, ?, ?, ?, ?) \
    \ON CONFLICT(path) DO UPDATE SET hash = excluded.hash, narSize = excluded.narSize, deriver = excluded.deriver, \
    \ultimate = COALESCE(excluded.ultimate, ultimate), sigs = COALESCE(excluded.sigs, sigs), ca = COALESCE(excluded.ca, ca)"
    ( pathText,
      prNarHash reg,
      prDeriver reg,
      prNarSize reg,
      ultimateField (prUltimate reg),
      signaturesField (prSignatures reg),
      SQLText . renderContentAddress <$> prContentAddress reg
    )

-- | The @ultimate@ column of a row: 1 for an ultimately trusted path and
-- NULL otherwise, as upstream writes it.
ultimateField :: Ultimate -> SQLData
ultimateField Ultimate = SQLInteger 1
ultimateField NotUltimate = SQLNull

-- | A row's @ultimate@ column as upstream reads it: 1 is ultimately
-- trusted, and anything else, NULL included, is not.
ultimateFromField :: Maybe Int -> Ultimate
ultimateFromField (Just 1) = Ultimate
ultimateFromField _ = NotUltimate

-- | The @sigs@ column of a row: the signatures as a narinfo spells them,
-- in order and separated by spaces, or NULL for none, as upstream writes
-- them.  The column is text, and a key name that is not UTF-8, which no
-- key nova-nix reads has, is kept as the bytes it is.
signaturesField :: Set Signature -> SQLData
signaturesField sigs
  | Set.null sigs = SQLNull
  | otherwise = either (const (SQLBlob spelled)) SQLText (TE.decodeUtf8' spelled)
  where
    spelled = BS.intercalate " " (map renderSignature (Set.toAscList sigs))

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
      mapM_ (insertRef conn referrerId) (Set.toAscList (Set.fromList (prReferences reg)))
    [] -> pure () -- Should not happen: the row was just inserted above.
  where
    insertRef conn referrerId refPath = do
      let refPathText = T.pack (storePathToFilePath (sdbDir db) refPath)
      refRows <- query conn "SELECT id FROM ValidPaths WHERE path = ?" (Only refPathText) :: IO [Only Int]
      case refRows of
        (Only refId : _) ->
          execute conn "INSERT OR IGNORE INTO Refs (referrer, reference) VALUES (?, ?)" (referrerId, refId)
        [] ->
          throwIO (userError ("path '" <> T.unpack (storePathToText defaultStoreDir refPath) <> "' is not valid"))

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

-- | Query every registered valid path, as full path text, sorted by that
-- text for determinism.
queryAllValidPaths :: StoreDB -> IO [Text]
queryAllValidPaths db = do
  rows <- query (sdbConn db) "SELECT path FROM ValidPaths ORDER BY path" () :: IO [Only Text]
  pure [p | Only p <- rows]

-- | Every reference edge in the store as @(referrer, reference)@ path
-- text, in one query: the collector's reachability walk is over the
-- whole graph, and one round trip beats one per valid path.
queryAllReferences :: StoreDB -> IO [(Text, Text)]
queryAllReferences db =
  query
    (sdbConn db)
    "SELECT vp1.path, vp2.path FROM Refs r \
    \JOIN ValidPaths vp1 ON r.referrer = vp1.id \
    \JOIN ValidPaths vp2 ON r.reference = vp2.id \
    \ORDER BY vp1.path, vp2.path"
    ()

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

-- | Remove a SET of rows and every edge leaving them, in one
-- transaction, keyed by exact stored path text like 'unregisterPathRow'.
-- Exists for the collector: dead paths reference each other freely
-- (cross-output references run both ways between the outputs of one
-- derivation), and no order of one-row deletes gets through a cycle,
-- while deleting every dead referrer's edges before any dead row makes
-- the order irrelevant.
--
-- Refuses, changing nothing, when a path OUTSIDE the set still
-- references a path inside it: by construction a dead path's referrers
-- are all dead, so an outside referrer means the set is not the dead set
-- of this database, and dropping its edge would under-report that
-- referrer's references.  The outside referrers are returned as
-- 'RowReferenced'; 'RowAbsent' is never returned, since an absent row in
-- the set simply has nothing to remove.
--
-- The set goes through a temporary table rather than an @IN (?, ?, ...)@
-- list: SQLite caps bound variables per statement, and a dead set at
-- nixpkgs scale is larger than that cap.
unregisterPathRows :: StoreDB -> [Text] -> IO UnregisterResult
unregisterPathRows db paths = withTransaction conn $ do
  execute_ conn "CREATE TEMP TABLE IF NOT EXISTS DeadPaths (path TEXT PRIMARY KEY)"
  execute_ conn "DELETE FROM DeadPaths"
  executeMany conn "INSERT OR IGNORE INTO DeadPaths (path) VALUES (?)" (map Only paths)
  outsideReferrers <-
    query
      conn
      "SELECT DISTINCT vp1.path FROM Refs r \
      \JOIN ValidPaths vp1 ON r.referrer = vp1.id \
      \JOIN ValidPaths vp2 ON r.reference = vp2.id \
      \WHERE vp2.path IN (SELECT path FROM DeadPaths) \
      \AND vp1.path NOT IN (SELECT path FROM DeadPaths) \
      \ORDER BY vp1.path"
      () ::
      IO [Only Text]
  result <- case [p | Only p <- outsideReferrers] of
    referrers@(_ : _) -> pure (RowReferenced referrers)
    [] -> do
      execute_ conn "DELETE FROM Refs WHERE referrer IN (SELECT id FROM ValidPaths WHERE path IN (SELECT path FROM DeadPaths))"
      execute_ conn "DELETE FROM ValidPaths WHERE path IN (SELECT path FROM DeadPaths)"
      pure RowUnregistered
  execute_ conn "DELETE FROM DeadPaths"
  pure result
  where
    conn = sdbConn db

-- | Query full path info for a registered store path.  A trust column a
-- row holds that does not read back, which only an edit outside nova-nix
-- can leave, fails the query, as upstream's fails on a hash it cannot
-- read, @invalid-path entry for ...@.
queryPathInfo :: StoreDB -> StorePath -> IO (Maybe PathInfo)
queryPathInfo db sp = do
  let pathText = T.pack (storePathToFilePath (sdbDir db) sp)
  rows <-
    query
      (sdbConn db)
      "SELECT path, hash, narSize, deriver, registrationTime, ultimate, sigs, ca FROM ValidPaths WHERE path = ?"
      (Only pathText) ::
      IO [(Text, Text, Int, Maybe Text, Int, Maybe Int, SQLData, Maybe Text)]
  case rows of
    ((pth, hsh, sz, drv, regTime, ultimate, sigs, ca) : _) ->
      either (throwIO . userError . T.unpack . invalidEntry) (pure . Just) $ do
        signatures <- signaturesFromField sigs
        address <- traverse parseContentAddress ca
        pure
          PathInfo
            { piPath = pth,
              piNarHash = hsh,
              piNarSize = sz,
              piDeriver = drv,
              piRegTime = regTime,
              piUltimate = ultimateFromField ultimate,
              piSignatures = signatures,
              piContentAddress = address
            }
    [] -> pure Nothing
  where
    invalidEntry reason = "invalid-path entry for '" <> storePathToText defaultStoreDir sp <> "': " <> reason

-- | The signatures a @sigs@ column holds, separated by spaces, as
-- upstream reads them: none for NULL, the column's bytes whether SQLite
-- holds it as text or as a blob.
signaturesFromField :: SQLData -> Either Text (Set Signature)
signaturesFromField field = case field of
  SQLNull -> Right Set.empty
  SQLText spelled -> fromBytes (TE.encodeUtf8 spelled)
  SQLBlob spelled -> fromBytes spelled
  _ -> Left "the signatures column holds neither text nor bytes"
  where
    fromBytes = fmap Set.fromList . traverse parseSignature . filter (not . BS.null) . BS.split space
    space = 0x20

-- | The valid paths that reference a path, as full path text, sorted: a
-- path that references itself is among its own referrers, as upstream
-- lists it.
queryReferrers :: StoreDB -> StorePath -> IO [Text]
queryReferrers db sp = do
  let pathText = T.pack (storePathToFilePath (sdbDir db) sp)
  rows <-
    query
      (sdbConn db)
      "SELECT vp.path FROM Refs r \
      \JOIN ValidPaths vp ON r.referrer = vp.id \
      \WHERE r.reference = (SELECT id FROM ValidPaths WHERE path = ?) \
      \ORDER BY vp.path"
      (Only pathText) ::
      IO [Only Text]
  pure [p | Only p <- rows]

-- | The valid derivations with an output at a path, as full path text,
-- sorted: upstream's @queryValidDerivers@, which reads them from
-- DerivationOutputs.  The path need not be valid itself.
queryValidDerivers :: StoreDB -> StorePath -> IO [Text]
queryValidDerivers db sp = do
  let pathText = T.pack (storePathToFilePath (sdbDir db) sp)
  rows <-
    query
      (sdbConn db)
      "SELECT DISTINCT vp.path FROM DerivationOutputs d \
      \JOIN ValidPaths vp ON d.drv = vp.id \
      \WHERE d.path = ? \
      \ORDER BY vp.path"
      (Only pathText) ::
      IO [Only Text]
  pure [p | Only p <- rows]

-- | The valid path whose hash part is the one given, as full path text,
-- or why the hash part is refused: upstream's @queryPathFromHashPart@,
-- which takes the first valid path at or after the store directory and
-- the hash part and answers it when it begins with them.  A hash part
-- that is not a store path hash's length is refused in upstream's words.
queryPathFromHashPart :: StoreDB -> Text -> IO (Either Text (Maybe Text))
queryPathFromHashPart db hashPart
  | T.length hashPart /= storePathHashLen = pure (Left "invalid hash part")
  | otherwise = do
      rows <- query (sdbConn db) "SELECT path FROM ValidPaths WHERE path >= ? ORDER BY path LIMIT 1" (Only prefix) :: IO [Only Text]
      pure
        ( Right
            ( case rows of
                (Only found : _) | prefix `T.isPrefixOf` found -> Just found
                _ -> Nothing
            )
        )
  where
    prefix = T.pack (unStoreDir (sdbDir db) </> T.unpack hashPart)
