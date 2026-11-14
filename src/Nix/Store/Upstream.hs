-- | A store directory upstream Nix also manages.
--
-- Upstream registers its paths in its own database and keeps its roots
-- under its own state directory, so in a directory upstream also uses,
-- a path upstream registered has no row in this store's database.  Every
-- operation that takes a tree with no row for its own debris would take
-- such a path for one: the collector and the checked delete would delete
-- it ("Nix.Store.GC"), and a build, a substitution, the restore of
-- evaluation's sources and an evaluation's own store write would clear
-- it and write their own tree in its place.  An evaluation's write
-- clears only a tree that does not reproduce its address, but which
-- tree that is depends on how the names on disk are read, and upstream
-- may be writing the path at that moment.  Each of these operations
-- refuses, before it touches anything, a store whose
-- directory, as spelled or as its links resolve, has upstream's database
-- where upstream's local store keeps it ('upstreamDatabasePath'):
-- @\/nix\/var\/nix\/db\/db.sqlite@ for @\/nix\/store@.  They refuse, too,
-- when that path cannot be examined.  An upstream whose state directory
-- is moved (@NIX_STATE_DIR@, or a build configured with another one) and
-- a store reached through a bind mount are not recognised.
--
-- The look runs as each such operation starts, not when the store is
-- opened or an evaluation begins: a handle on such a store still lists
-- its roots and pushes its paths, and an evaluation that writes nothing
-- still evaluates, none of which changes a path in it.
module Nix.Store.Upstream
  ( UpstreamHazard (..),
    upstreamManagedRefusal,
    upstreamDatabasePath,
  )
where

import Control.Exception (IOException, displayException, try)
import Data.List (isSuffixOf)
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Nix.Store.Path (StoreDir (..))
import System.Directory (canonicalizePath, getPermissions)
import System.FilePath (dropTrailingPathSeparator, splitDirectories, takeDirectory, (</>))
import System.IO.Error (isDoesNotExistError)

-- ---------------------------------------------------------------------------
-- Layout
-- ---------------------------------------------------------------------------

-- | The database upstream Nix would keep for a store in this directory,
-- when the directory is one upstream's local store uses.  That store is
-- @\/nix\/store@, with its state in @nixStateDir@, @\/nix\/var\/nix@
-- unless @NIX_STATE_DIR@ or the build moves it; or, opened with a root,
-- @\<root\>\/nix\/store@ with its state in @\<root\>\/nix\/var\/nix@
-- (@LocalFSStoreConfig@).  The database is @db\/db.sqlite@ in the state
-- directory (@LocalStore@).
upstreamDatabasePath :: FilePath -> Maybe FilePath
upstreamDatabasePath dir
  | upstreamStoreDirTail `isSuffixOf` splitDirectories trimmed = Just (takeDirectory trimmed </> upstreamDatabaseRelPath)
  | otherwise = Nothing
  where
    trimmed = dropTrailingPathSeparator dir

-- | The last two components of every directory upstream's local store
-- keeps a store in.
upstreamStoreDirTail :: [FilePath]
upstreamStoreDirTail = ["nix", "store"]

-- | Upstream's database, relative to the directory that holds its store.
upstreamDatabaseRelPath :: FilePath
upstreamDatabaseRelPath = "var" </> "nix" </> "db" </> "db.sqlite"

-- ---------------------------------------------------------------------------
-- The refusal
-- ---------------------------------------------------------------------------

-- | What a refused operation would do to the paths upstream registers,
-- which its refusal names.
data UpstreamHazard
  = -- | Delete them as unregistered trees: a collection or a checked
    -- delete, which read this store's roots and not upstream's.
    DeletesUpstreamPaths
  | -- | Clear them as unregistered leftovers and write its own trees in
    -- their place: a build, a substitution, the restore of evaluation's
    -- sources or an evaluation's own store write.
    ReplacesUpstreamPaths
  deriving (Eq, Show)

-- | Why an operation on this store directory is refused, or 'Nothing'
-- when it may go ahead: upstream's database exists for the directory as
-- spelled or as its links resolve ('upstreamDatabasePath'), or such a
-- path cannot be examined.  The look reads nothing of the store itself,
-- so a caller runs it before taking any lock.
upstreamManagedRefusal :: UpstreamHazard -> StoreDir -> IO (Maybe Text)
upstreamManagedRefusal hazard (StoreDir dir) = do
  resolved <- canonicalizePath dir
  found <- traverse probe (Set.toList (Set.fromList (mapMaybe upstreamDatabasePath [dir, resolved])))
  pure (listToMaybe (catMaybes found))
  where
    -- On POSIX getPermissions stats the path, and on Windows it opens it
    -- with no access rights, so a database this user cannot read refuses
    -- the store, as present or as unexaminable.
    probe db = do
      probed <- try (getPermissions db)
      pure $ case probed of
        Left err
          | isDoesNotExistError err -> Nothing
          | otherwise -> Just (upstreamUnknownMessage hazard dir db err)
        Right _ -> Just (upstreamManagedMessage hazard dir db)

-- ---------------------------------------------------------------------------
-- Messages
-- ---------------------------------------------------------------------------

-- | The refusal of a store upstream Nix's database also manages.
upstreamManagedMessage :: UpstreamHazard -> FilePath -> FilePath -> Text
upstreamManagedMessage hazard dir db =
  "upstream Nix's database '"
    <> T.pack db
    <> "' also manages the store '"
    <> T.pack dir
    <> "': the paths it registers have no row in this store's database"
    <> unreadRoots hazard
    <> ", so nova-nix would "
    <> hazardVerb hazard
    <> " those paths as unregistered"
  where
    unreadRoots DeletesUpstreamPaths = " and the roots it keeps are not read"
    unreadRoots ReplacesUpstreamPaths = ""

-- | The refusal of a store when upstream Nix's database cannot be looked
-- for.
upstreamUnknownMessage :: UpstreamHazard -> FilePath -> FilePath -> IOException -> Text
upstreamUnknownMessage hazard dir db err =
  "cannot tell whether upstream Nix's database '"
    <> T.pack db
    <> "' manages the store '"
    <> T.pack dir
    <> "' ("
    <> T.pack (displayException err)
    <> "): if it does, nova-nix would "
    <> hazardVerb hazard
    <> " the paths it registers as unregistered"

-- | What the refused operation would do to upstream's paths, as the
-- refusal words it.
hazardVerb :: UpstreamHazard -> Text
hazardVerb DeletesUpstreamPaths = "delete"
hazardVerb ReplacesUpstreamPaths = "replace"
