{-# LANGUAGE ScopedTypeVariables #-}

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
    Store (..),
    openStore,
    closeStore,

    -- * Queries
    isValid,
    pathExists,

    -- * Store operations
    addToStore,
    scanReferences,
    setReadOnly,
    writeDrv,

    -- * Re-exports
    module Nix.Store.Path,
    module Nix.Store.DB,
  )
where

import Control.Exception (IOException, catch)
import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Nix.Derivation (Derivation, toATerm)
import Nix.Store.DB
import Nix.Store.Path
import System.Directory
  ( copyFile,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
    removeDirectoryRecursive,
    renamePath,
    setPermissions,
  )
import qualified System.Directory as Dir
import System.FilePath ((</>))

-- | An open store with database and configuration.
data Store = Store
  { stDir :: !StoreDir,
    stDB :: !StoreDB
  }

-- | Open a Nix store at the given directory.
-- Creates the store directory and database if they don't exist.
openStore :: StoreDir -> IO Store
openStore dir = do
  createDirectoryIfMissing True (unStoreDir dir)
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

-- ---------------------------------------------------------------------------
-- Store operations
-- ---------------------------------------------------------------------------

-- | Move an output directory to the store path, set read-only, and register.
--
-- If @renameDirectory@ fails (cross-device move), falls back to
-- recursive copy + remove.
addToStore ::
  Store ->
  FilePath ->
  StorePath ->
  Maybe Text ->
  [StorePath] ->
  IO ()
addToStore store srcDir sp deriver refs = do
  let destPath = storePathToFilePath (stDir store) sp
  -- Move (or copy) source to store
  moveDirectory srcDir destPath
  -- Set read-only permissions
  setReadOnly destPath
  -- Register in database
  registerPath
    (stDB store)
    PathRegistration
      { prPath = sp,
        prNarHash = "sha256:0000000000000000000000000000000000000000000000000000", -- NAR hashing deferred until nova-cache integration
        prNarSize = 0, -- Computed size deferred until NAR serialization is wired in
        prDeriver = deriver,
        prReferences = refs
      }

-- | Cross-device safe directory move.
-- Tries 'renamePath' first; on IOException falls back to copy + remove.
moveDirectory :: FilePath -> FilePath -> IO ()
moveDirectory src dest =
  renamePath src dest `catch` \(_ :: IOException) -> do
    copyDirectoryRecursive src dest
    removeDirectoryRecursive src

-- | Recursively copy a directory tree.
copyDirectoryRecursive :: FilePath -> FilePath -> IO ()
copyDirectoryRecursive src dest = do
  createDirectoryIfMissing True dest
  entries <- listDirectory src
  mapM_ (copyEntry src dest) entries
  where
    copyEntry srcDir destDir name = do
      let srcPath = srcDir </> name
          destPath = destDir </> name
      isDir <- doesDirectoryExist srcPath
      if isDir
        then copyDirectoryRecursive srcPath destPath
        else copyFile srcPath destPath

-- | Byte-scan all files under a directory for store path references.
--
-- Builds a 'Set' of candidate hash strings from the given store paths.
-- Walks all regular files, reads each as ByteString, searches for the
-- store dir prefix followed by a 32-character hash.  Returns matching
-- store paths from the candidate set.
scanReferences :: StoreDir -> [StorePath] -> FilePath -> IO [StorePath]
scanReferences storeDir candidates dir = do
  let candidateSet = Set.fromList [(spHash sp, sp) | sp <- candidates]
      prefixBytes = TE.encodeUtf8 (T.pack (unStoreDir storeDir <> "/"))
      prefixLen = BS.length prefixBytes
      hashLen = 32
  files <- collectRegularFiles dir
  foundHashes <- foldlIO Set.empty files $ \acc filePath -> do
    contents <- BS.readFile filePath
    pure (scanBytes prefixBytes prefixLen hashLen contents acc)
  pure [sp | (h, sp) <- Set.toList candidateSet, Set.member h foundHashes]

-- | Collect all regular files under a directory, recursively.
collectRegularFiles :: FilePath -> IO [FilePath]
collectRegularFiles dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure []
    else do
      entries <- listDirectory dir
      concat <$> mapM (classifyAndCollect dir) entries
  where
    classifyAndCollect parent name = do
      let fullPath = parent </> name
      isDir <- doesDirectoryExist fullPath
      if isDir
        then collectRegularFiles fullPath
        else do
          isFile <- doesFileExist fullPath
          pure [fullPath | isFile]

-- | Scan a ByteString for store path prefix occurrences, extract hashes.
scanBytes :: BS.ByteString -> Int -> Int -> BS.ByteString -> Set Text -> Set Text
scanBytes prefix prefixLen hashLen bs =
  go 0
  where
    bsLen = BS.length bs
    go !idx !found
      | idx + prefixLen + hashLen > bsLen = found
      | BS.isPrefixOf prefix (BS.drop idx bs) =
          let hashBytes = BS.take hashLen (BS.drop (idx + prefixLen) bs)
           in case TE.decodeUtf8' hashBytes of
                Right hashText -> go (idx + prefixLen + hashLen) (Set.insert hashText found)
                Left _ -> go (idx + 1) found
      | otherwise = go (idx + 1) found

-- | Strict left fold over a list in IO.
foldlIO :: a -> [b] -> (a -> b -> IO a) -> IO a
foldlIO z [] _ = pure z
foldlIO z (x : xs) f = do
  acc <- f z x
  foldlIO acc xs f

-- | Recursively set a directory and all its contents to read-only.
setReadOnly :: FilePath -> IO ()
setReadOnly path = do
  isDir <- doesDirectoryExist path
  if isDir
    then do
      entries <- listDirectory path
      mapM_ (setReadOnly . (path </>)) entries
      perms <- Dir.getPermissions path
      Dir.setPermissions path (Dir.setOwnerWritable False perms)
    else do
      isFile <- doesFileExist path
      when isFile $ do
        perms <- Dir.getPermissions path
        setPermissions path (Dir.setOwnerWritable False perms)

-- | Serialize a derivation to ATerm and write it to the store at the given path.
writeDrv :: Store -> Derivation -> StorePath -> IO ()
writeDrv store drv sp = do
  let destPath = storePathToFilePath (stDir store) sp
      aterm = toATerm drv
  createDirectoryIfMissing True (unStoreDir (stDir store))
  writeFile destPath (T.unpack aterm)
