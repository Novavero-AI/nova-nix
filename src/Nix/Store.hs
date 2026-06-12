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
-- 1. Fetch @\<hash\>.narinfo@ from cache - contains NAR hash, size, refs
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
-- tracking is exact, GC is safe - it never deletes something in use.
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
    placeInStore,
    registrationFor,
    scanReferences,
    scanTempReferences,
    setReadOnly,
    writeDrv,
    writeDrvAterm,

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
import qualified Data.Text.IO as TIO
import Nix.Derivation (Derivation, toATerm)
import Nix.Store.DB
import Nix.Store.Path
import qualified NovaCache.Hash as Hash
import qualified NovaCache.NAR as NAR
import System.Directory
  ( copyFile,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    doesPathExist,
    listDirectory,
    removeDirectoryRecursive,
    removeFile,
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

-- | Check if a store path exists on disk (file or directory, regardless of DB).
pathExists :: Store -> StorePath -> IO Bool
pathExists store sp = doesPathExist (storePathToFilePath (stDir store) sp)

-- ---------------------------------------------------------------------------
-- Store operations
-- ---------------------------------------------------------------------------

-- | Move a build output (file or directory) to the store path, set read-only,
-- and register.
--
-- If @renamePath@ fails (cross-device move), falls back to copy + remove.
addToStore ::
  Store ->
  FilePath ->
  StorePath ->
  Maybe Text ->
  [StorePath] ->
  IO ()
addToStore store srcPath sp deriver refs = do
  reg <- placeInStore store srcPath sp deriver refs
  registerPath (stDB store) reg

-- | Move a build output into the store (read-only) and compute its
-- registration (NAR hash, size, references) WITHOUT writing to the database.
--
-- Splitting placement from registration lets a multi-output build place every
-- output first and then register them together, so intra-derivation
-- cross-output references are preserved (see 'registerPaths').
placeInStore ::
  Store ->
  FilePath ->
  StorePath ->
  Maybe Text ->
  [StorePath] ->
  IO PathRegistration
placeInStore store srcPath sp deriver refs = do
  let destPath = storePathToFilePath (stDir store) sp
  -- Move (or copy) source to store
  moveOutput srcPath destPath
  -- Set read-only permissions
  setReadOnly destPath
  registrationFor store sp deriver refs

-- | Compute the registration metadata for a store path already present on
-- disk, without moving anything.  Used to (re-)register an output that an
-- interrupted build left in place but never recorded in the database.
registrationFor :: Store -> StorePath -> Maybe Text -> [StorePath] -> IO PathRegistration
registrationFor store sp deriver refs = do
  let destPath = storePathToFilePath (stDir store) sp
  -- Compute the NAR hash and size of the final store contents.  The NAR
  -- serialization is canonical (entries sorted, 8-byte padding), so this is
  -- exactly the NarHash/NarSize a binary cache reports for the path.
  narEntry <- NAR.serialiseFromPath destPath
  let narBytes = NAR.serialise narEntry
  pure
    PathRegistration
      { prPath = sp,
        prNarHash = Hash.formatNixHash (Hash.hashBytes narBytes),
        prNarSize = BS.length narBytes,
        prDeriver = deriver,
        prReferences = refs
      }

-- | Cross-device safe move for files or directories.
-- Tries 'renamePath' first; on IOException falls back to copy + remove.
moveOutput :: FilePath -> FilePath -> IO ()
moveOutput src dest =
  renamePath src dest `catch` \(_ :: IOException) -> do
    isDir <- doesDirectoryExist src
    if isDir
      then do
        copyDirectoryRecursive src dest
        removeDirectoryRecursive src
      else do
        copyFile src dest
        removeFile src

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
      storeDirStr = unStoreDir storeDir
      -- Scan for both forward-slash and backslash store prefixes.  On Windows,
      -- binaries may contain either separator style.  Both separators are a
      -- single ASCII byte, so the two prefixes share a length.
      prefixFwd = TE.encodeUtf8 (T.pack (storeDirStr <> "/"))
      prefixBwd = TE.encodeUtf8 (T.pack (storeDirStr <> "\\"))
      prefixLen = BS.length prefixFwd
  files <- collectRegularFiles dir
  foundHashes <- foldlIO Set.empty files $ \acc filePath -> do
    contents <- BS.readFile filePath
    -- Scan with forward-slash prefix, then backslash (for Windows)
    let acc1 = scanBytes prefixFwd prefixLen storePathHashLen contents acc
    pure (scanBytes prefixBwd prefixLen storePathHashLen contents acc1)
  pure [sp | (h, sp) <- Set.toList candidateSet, Set.member h foundHashes]

-- | Scan an output for references to build-temp output locations.
--
-- The builder runs under a temp directory, so an output that embeds its own or
-- a sibling output's path embeds the TEMP path - which 'scanReferences' (store
-- prefix only) cannot see.  Given @(tempDir, storePath)@ for every output of
-- the derivation, returns the store paths whose temp location is referenced
-- from the scanned output, capturing self- and cross-output references.
--
-- This records the dependency edge; it does not rewrite the embedded bytes
-- (self-reference hash rewriting is a separate, future concern).
scanTempReferences :: [(FilePath, StorePath)] -> FilePath -> IO [StorePath]
scanTempReferences tempPairs dir = do
  let needles = [(TE.encodeUtf8 (T.pack tempDir), sp) | (tempDir, sp) <- tempPairs]
  files <- collectRegularFiles dir
  foundHashes <- foldlIO Set.empty files $ \acc filePath -> do
    contents <- BS.readFile filePath
    pure (Set.union acc (Set.fromList [spHash sp | (needle, sp) <- needles, needle `BS.isInfixOf` contents]))
  pure [sp | (_, sp) <- tempPairs, Set.member (spHash sp) foundHashes]

-- | Collect all regular files under a path, recursively.
-- If the path is itself a regular file, returns it directly.
collectRegularFiles :: FilePath -> IO [FilePath]
collectRegularFiles path = do
  isDir <- doesDirectoryExist path
  if isDir
    then do
      entries <- listDirectory path
      concat <$> mapM (classifyAndCollect path) entries
    else do
      isFile <- doesFileExist path
      pure [path | isFile]
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

-- | Recursively mark a store path and its contents read-only after a build.
--
-- On Windows the directory read-only attribute does not prevent adding or
-- removing entries - only the per-file read-only attribute protects a file.
-- Immutability here is therefore enforced at FILE granularity (every file is
-- made read-only); hardening the directory itself against entry changes would
-- require ACLs and is deferred.
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

-- | Write an already-serialized derivation ATerm to its store path.  Used to
-- materialize the input @.drv@ closure (root plus every transitive input)
-- before a dependency-aware build: evaluation computes these ATerms but does
-- no store IO, so the build driver writes them here.
writeDrvAterm :: Store -> StorePath -> Text -> IO ()
writeDrvAterm store sp aterm = do
  let destPath = storePathToFilePath (stDir store) sp
  createDirectoryIfMissing True (unStoreDir (stDir store))
  TIO.writeFile destPath aterm

-- | Serialize a derivation to ATerm and write it to the store at the given path.
writeDrv :: Store -> Derivation -> StorePath -> IO ()
writeDrv store drv sp = writeDrvAterm store sp (toATerm drv)
