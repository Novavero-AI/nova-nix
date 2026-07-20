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
    copyPathInto,
    placeInStore,
    registrationFor,
    materializeEvalSources,
    scanReferences,
    scanTempReferences,
    setReadOnly,
    unpackNarEntry,
    writeDrv,
    writeDrvAterm,
    writeDrvClosure,

    -- * NAR entry-name safety
    isSafeNarName,

    -- * Link ordering (exposed for testing)
    orderLinks,

    -- * Re-exports
    module Nix.Store.Path,
    module Nix.Store.DB,
  )
where

import Control.Exception (IOException, SomeException, catch, throwIO, try)
import Control.Monad (unless, when)
import qualified Data.ByteString as BS
import Data.Char (isDigit, toUpper)
import Data.List (foldl', inits)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Nix.Derivation (Derivation (..), fromATerm, toATerm)
import Nix.Hash (makeFixedOutputPath, sha256Digest)
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
    renamePath,
    setPermissions,
  )
import qualified System.Directory as Dir
import System.FilePath (splitDirectories, takeDirectory, (</>))
import qualified System.Info

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
-- disk, without moving anything.  Used by 'placeInStore' after its move,
-- and by 'materializeEvalSources' to register a verified adopted tree.
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
-- The fallback copies via 'copyPathInto', which preserves symlinks as
-- symlinks: a dereferencing copy here would make the stored bytes - and
-- so the NAR hash a cache signs - depend on whether the source and the
-- store share a volume.
moveOutput :: FilePath -> FilePath -> IO ()
moveOutput src dest =
  renamePath src dest `catch` \(_ :: IOException) -> do
    copyPathInto src dest
    Dir.removePathForcibly src

-- | Byte-scan a tree for store path references.
--
-- Searches each scan unit ('collectScanUnits': regular file bytes and
-- symlink target strings) for each candidate's bare 32-character hash -
-- the same needle upstream Nix scans for.  Matching the hash rather
-- than a store-dir-prefixed path keeps the scan independent of the
-- spelling the builder embedded (canonical @\/nix\/store\/...@,
-- @C:\\nix\\store\\...@, MSYS2 forms): eval injects canonical
-- forward-slash text into builder environments, which a
-- platform-store-dir prefix never matches on Windows.
scanReferences :: [StorePath] -> FilePath -> IO [StorePath]
scanReferences candidates dir = do
  let candidateSet = Set.fromList [(spHash sp, sp) | sp <- candidates]
      needles = [(TE.encodeUtf8 h, h) | (h, _) <- Set.toList candidateSet]
  units <- collectScanUnits dir
  foundHashes <- foldlIO Set.empty units $ \acc unit -> do
    contents <- scanUnitBytes unit
    pure (Set.union acc (Set.fromList [h | (needle, h) <- needles, needle `BS.isInfixOf` contents]))
  pure [sp | (h, sp) <- Set.toList candidateSet, Set.member h foundHashes]

-- | Scan an output for references to build-temp output locations.
--
-- The builder runs under a temp directory, so an output that embeds its own or
-- a sibling output's path embeds the TEMP path - which 'scanReferences' does
-- not look for.  Given @(tempDir, storePath)@ for every output of
-- the derivation, returns the store paths whose temp location is referenced
-- from the scanned output, capturing self- and cross-output references.
--
-- This records the dependency edge; it does not rewrite the embedded bytes
-- (self-reference hash rewriting is a separate, future concern).
scanTempReferences :: [(FilePath, StorePath)] -> FilePath -> IO [StorePath]
scanTempReferences tempPairs dir = do
  let needles = [(TE.encodeUtf8 (T.pack tempDir), sp) | (tempDir, sp) <- tempPairs]
  units <- collectScanUnits dir
  foundHashes <- foldlIO Set.empty units $ \acc unit -> do
    contents <- scanUnitBytes unit
    pure (Set.union acc (Set.fromList [spHash sp | (needle, sp) <- needles, needle `BS.isInfixOf` contents]))
  pure [sp | (_, sp) <- tempPairs, Set.member (spHash sp) foundHashes]

-- | A node kind for store walks, classified WITHOUT following symlinks:
-- the link test runs first because 'doesDirectoryExist' and
-- 'doesFileExist' follow links and would report a link as its target.
-- A dangling link still classifies as 'WalkSymlink'; a probe failure
-- classifies as 'WalkAbsent' rather than throwing mid-walk.
data WalkNode = WalkSymlink | WalkDirectory | WalkRegular | WalkAbsent

-- | Classify one path for a store walk.  The walks in this module
-- dispatch on this (or, in 'copyPathInto', run the same link-first
-- probe order) so no store walk follows a symlink: following one reads
-- or mutates content outside the tree being walked, and does not
-- terminate on a link cycle.
classifyWalkNode :: FilePath -> IO WalkNode
classifyWalkNode path = do
  isLink <- Dir.pathIsSymbolicLink path `catch` \(_ :: IOException) -> pure False
  if isLink
    then pure WalkSymlink
    else do
      isDir <- doesDirectoryExist path
      if isDir
        then pure WalkDirectory
        else do
          isFile <- doesFileExist path
          pure (if isFile then WalkRegular else WalkAbsent)

-- | One scannable unit of a walked tree: a regular file's bytes read
-- from disk, or a symlink's target string.  The NAR serialization
-- carries both, so reference scanning covers both - a link into a
-- dependency (@bin\/tool -> \/nix\/store\/\<hash\>-dep\/tool@)
-- references the dependency even when no file byte does.
data ScanUnit = ScanFile !FilePath | ScanLinkTarget !BS.ByteString

-- | Collect the scannable units under a path: regular files and symlink
-- targets, links never followed.  A path that is itself a regular file
-- or link is its own single unit.
collectScanUnits :: FilePath -> IO [ScanUnit]
collectScanUnits path = do
  node <- classifyWalkNode path
  case node of
    WalkSymlink -> do
      target <- Dir.getSymbolicLinkTarget path
      pure [ScanLinkTarget (TE.encodeUtf8 (T.pack target))]
    WalkDirectory -> do
      entries <- listDirectory path
      concat <$> mapM (collectScanUnits . (path </>)) entries
    WalkRegular -> pure [ScanFile path]
    WalkAbsent -> pure []

-- | The bytes a 'ScanUnit' contributes to the scan.
scanUnitBytes :: ScanUnit -> IO BS.ByteString
scanUnitBytes (ScanFile path) = BS.readFile path
scanUnitBytes (ScanLinkTarget target) = pure target

-- | Strict left fold over a list in IO.  The accumulator is forced to
-- WHNF each step - without it, the scan retains every scanned file's
-- bytes in Set.union thunks until the end (peak memory ~ total tree
-- size).  Set's spine-strict nodes make WHNF force the whole union.
foldlIO :: a -> [b] -> (a -> b -> IO a) -> IO a
foldlIO z [] _ = pure z
foldlIO z (x : xs) f = do
  !acc <- f z x
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
  node <- classifyWalkNode path
  case node of
    -- A symlink is a leaf: descending would mark content outside the
    -- tree (or loop on a link cycle), and a permission change applied
    -- to the link resolves through to its target.
    WalkSymlink -> pure ()
    WalkDirectory -> do
      entries <- listDirectory path
      mapM_ (setReadOnly . (path </>)) entries
      perms <- Dir.getPermissions path
      Dir.setPermissions path (Dir.setOwnerWritable False perms)
    WalkRegular -> do
      perms <- Dir.getPermissions path
      setPermissions path (Dir.setOwnerWritable False perms)
    WalkAbsent -> pure ()

-- | Write an already-serialized derivation ATerm to its store path.  Used to
-- materialize the input @.drv@ closure (root plus every transitive input)
-- before a dependency-aware build: evaluation computes these ATerms but does
-- no store IO, so the build driver writes them here.
writeDrvAterm :: Store -> StorePath -> BS.ByteString -> IO ()
writeDrvAterm store sp aterm = do
  let destPath = storePathToFilePath (stDir store) sp
  createDirectoryIfMissing True (unStoreDir (stDir store))
  -- Raw bytes, not text-mode IO: the path was computed from exactly these
  -- bytes, and a locale-dependent or newline-translating write would store
  -- bytes that no longer match their content address.
  BS.writeFile destPath aterm

-- | Serialize a derivation to ATerm, write it to the store, and register
-- it: a @.drv@ is a store object like any other, so it gets a ValidPaths
-- row with its NAR hash and its references.  Reference scans may name a
-- @.drv@ (an output that embeds an input drv hash), and an unregistered
-- referent fails the whole registration batch.
writeDrv :: Store -> Derivation -> StorePath -> IO ()
writeDrv store drv sp = do
  writeDrvAterm store sp (toATerm drv)
  reg <- registrationFor store sp Nothing (drvReferences drv)
  registerPath (stDB store) reg

-- | A @.drv@'s references: its input sources and input @.drv@ paths -
-- the same set upstream records when writing a derivation to the store.
drvReferences :: Derivation -> [StorePath]
drvReferences drv = drvInputSrcs drv ++ Map.keys (drvInputDrvs drv)

-- | Write every recorded @.drv@ ATerm (keyed by its store-path text) to
-- the store and register the whole closure in one batch: rows all land
-- before edges ('registerPaths'), so references between the closure's
-- own @.drv@ files resolve regardless of map order.  Input SOURCES must
-- already be registered - the build driver runs 'materializeEvalSources'
-- first.
--
-- Keys come from evaluation via 'storePathToText' so they always parse;
-- an unparseable key is skipped defensively.  The ATerm bytes were
-- rendered by evaluation, so a re-parse failure is an invariant break
-- and throws rather than registering a recipe with dropped references.
writeDrvClosure :: Store -> Map Text BS.ByteString -> IO ()
writeDrvClosure store closure = do
  regs <- mapM writeOne (Map.toList closure)
  registerPaths (stDB store) (catMaybes regs)
  where
    writeOne (pathText, aterm) =
      case parseStorePath defaultStoreDir pathText of
        Nothing -> pure Nothing
        Just sp -> do
          writeDrvAterm store sp aterm
          case fromATerm aterm of
            Right drv -> Just <$> registrationFor store sp Nothing (drvReferences drv)
            Left err ->
              throwIO
                ( userError
                    ( "writeDrvClosure: recorded ATerm for "
                        <> T.unpack pathText
                        <> " does not re-parse: "
                        <> T.unpack err
                    )
                )

-- ---------------------------------------------------------------------------
-- NAR unpacking
-- ---------------------------------------------------------------------------

-- | Unpack a NarEntry tree to a filesystem destination.  Returns @Left@ on an
-- unsafe entry name (path traversal); these can come from untrusted cache
-- data, so a typed failure is used instead of a partial 'error'.
--
-- Regular files and directories are written in one pass; symlinks are
-- created in a second pass, after their targets are materialized.  Windows
-- symlinks are typed (file vs directory) and the NAR format does not record
-- the target's kind, so the only reliable way to pick the flavor is to look
-- at the target on disk - which may sort after the link within the tree.
unpackNarEntry :: FilePath -> NAR.NarEntry -> IO (Either Text ())
unpackNarEntry path entry = do
  walked <- unpackTree path entry
  case walked of
    Left err -> pure (Left err)
    Right links -> createSymlinks links

-- | First unpack pass: write regular files and directories, recording
-- symlinks as (link path, target) for the second pass.
unpackTree :: FilePath -> NAR.NarEntry -> IO (Either Text [(FilePath, Text)])
unpackTree path entry = case entry of
  NAR.NarRegular isExec contents -> do
    createDirectoryIfMissing True (takeDirectory path)
    BS.writeFile path contents
    when isExec $ do
      perms <- Dir.getPermissions path
      setPermissions path (Dir.setOwnerExecutable True perms)
    pure (Right [])
  NAR.NarSymlink target -> pure (Right [(path, target)])
  NAR.NarDirectory entries -> do
    createDirectoryIfMissing True path
    unpackChildren path entries

-- | The on-disk identity a NAR entry name occupies on this platform's
-- store filesystem.  Windows (NTFS\/Win32) compares names
-- case-insensitively and strips trailing dots and spaces; the default
-- macOS APFS volume folds case; Linux preserves names byte-for-byte.
-- Two sibling entries sharing a key land on ONE file, the second
-- silently overwriting the first.  The fold is per-character uppercase:
-- a corruption backstop for the collisions real trees carry
-- (@Makefile@\/@makefile@), not a full model of filesystem Unicode
-- folding.
onDiskNameKey :: Text -> Text
onDiskNameKey = case System.Info.os of
  "mingw32" -> T.map toUpper . T.dropWhileEnd (\c -> c == '.' || c == ' ')
  "darwin" -> T.map toUpper
  _ -> id

-- | The first pair of sibling names folding to the same on-disk file,
-- if any: (earlier entry, colliding later entry).
firstNameCollision :: [Text] -> Maybe (Text, Text)
firstNameCollision = go Map.empty
  where
    go !_ [] = Nothing
    go !seen (name : rest) =
      let key = onDiskNameKey name
       in case Map.lookup key seen of
            Just earlier -> Just (earlier, name)
            Nothing -> go (Map.insert key name seen) rest

-- | Unpack directory children, short-circuiting with a typed failure on the
-- first unsafe entry name rather than crashing on untrusted input.
--
-- The sibling list is checked for on-disk name collisions BEFORE any
-- child is written: a folding filesystem lands two distinct NAR names
-- on one file, and the silent merge would materialize a tree that no
-- longer matches the NAR hash it is about to be registered under.
-- (Upstream works around such trees with its case-hack name mangling;
-- until that lands here, the collision is a loud failure, never a
-- corrupt store path.)
unpackChildren :: FilePath -> [(Text, NAR.NarEntry)] -> IO (Either Text [(FilePath, Text)])
unpackChildren path entries = case firstNameCollision (map fst entries) of
  Just (earlier, later) ->
    pure
      ( Left
          ( "NAR sibling entries '"
              <> earlier
              <> "' and '"
              <> later
              <> "' fold to the same on-disk name on this filesystem"
          )
      )
  Nothing -> go entries
  where
    go [] = pure (Right [])
    go ((name, child) : rest)
      | not (isSafeNarName name) =
          pure (Left ("unsafe NAR directory entry name: " <> name))
      | otherwise = do
          result <- unpackTree (path </> T.unpack name) child
          case result of
            Left err -> pure (Left err)
            Right links -> do
              restResult <- go rest
              case restResult of
                Left err -> pure (Left err)
                Right moreLinks -> pure (Right (links <> moreLinks))

-- | Second unpack pass: create the recorded symlinks in dependency
-- order - each link is created after every pending link its target
-- resolves at or through - so the Windows link flavor (file vs
-- directory) is read off the real target with one probe per link.
-- (The previous ready-set rounds re-stat'd every remaining link per
-- round: quadratic filesystem stats on a link chain.)  Links on a
-- dependency cycle have no knowable kind and default to file links,
-- exactly as dangling links always have.
createSymlinks :: [(FilePath, Text)] -> IO (Either Text ())
createSymlinks pending = createAll (orderLinks pending)
  where
    createAll [] = pure (Right ())
    createAll (link : rest) = do
      made <- uncurry createSymlink link
      case made of
        Left err -> pure (Left err)
        Right () -> createAll rest

-- | Order pending links so each follows every pending link its target
-- path resolves at or through: the target itself, or a link standing on
-- one of the target's ancestor directories.  Purely textual over the
-- same @takeDirectory linkPath \</\> target@ resolution 'createSymlink'
-- probes - no filesystem access.  Kahn's ordering, deterministic:
-- ready links leave in input order, and cycle members keep input order
-- at the end.  Exported for testing (the ordering property is pure).
orderLinks :: [(FilePath, Text)] -> [(FilePath, Text)]
orderLinks pending =
  let indexed = zip [0 :: Int ..] pending
      linkByIndex = Map.fromList indexed
      indexByKey =
        Map.fromList [(normalisedComponents linkPath, i) | (i, (linkPath, _)) <- indexed]
      -- The pending links this link's resolved target lands on or
      -- passes through (every nonempty component prefix).
      depsOf (linkPath, target) =
        let resolved = normalisedComponents (takeDirectory linkPath </> T.unpack target)
         in Set.fromList
              [j | prefix <- drop 1 (inits resolved), Just j <- [Map.lookup prefix indexByKey]]
      dependsOn = Map.fromList [(i, depsOf link) | (i, link) <- indexed]
      dependents =
        Map.fromListWith
          (flip (++))
          [(dep, [i]) | (i, deps) <- Map.toList dependsOn, dep <- Set.toList deps]
      initialCounts = Map.map Set.size dependsOn
      initialReady = [i | (i, count) <- Map.toList initialCounts, count == 0]
      -- Kahn's ordering with a two-list queue (amortized O(1) pops).
      run !emittedRev !counts front back = case front of
        [] -> case back of
          [] -> reverse emittedRev
          _ -> run emittedRev counts (reverse back) []
        (i : rest) ->
          let (updatedCounts, readied) = release counts (Map.findWithDefault [] i dependents)
           in run (i : emittedRev) updatedCounts rest (readied ++ back)
      release !counts deps = case deps of
        [] -> (counts, [])
        (d : more) ->
          let updated = Map.adjust (subtract 1) d counts
              (finalCounts, readied) = release updated more
           in (finalCounts, [d | Map.lookup d updated == Just 0] ++ readied)
      emittedOrder = run [] initialCounts initialReady []
      emittedSet = Set.fromList emittedOrder
      cycleRemainder = [link | (i, link) <- indexed, not (Set.member i emittedSet)]
   in [link | i <- emittedOrder, Just link <- [Map.lookup i linkByIndex]] ++ cycleRemainder

-- | Path components with @.@ dropped and @..@ collapsed textually - the
-- spelling-insensitive key that matches a link target against pending
-- link paths ('splitDirectories' accepts both separator spellings).  A
-- @..@ with nothing left to pop stays, matching no real path.
normalisedComponents :: FilePath -> [FilePath]
normalisedComponents path = reverse (foldl' step [] (splitDirectories path))
  where
    step stack comp
      | comp == "." = stack
      | comp == ".." = case stack of
          (top : rest) | top /= ".." -> rest
          _ -> comp : stack
      | otherwise = comp : stack

-- | Create one symlink, choosing the Windows flavor from the target's kind.
-- A creation failure is loud: the old fallback of writing the target text
-- as a regular file registered a tree whose NAR hash differed from the
-- signed narinfo's - silent store corruption that a later push refuses to
-- publish.  Failing lets the caller fall back to a local build.
createSymlink :: FilePath -> Text -> IO (Either Text ())
createSymlink linkPath target = do
  createDirectoryIfMissing True (takeDirectory linkPath)
  let targetStr = T.unpack target
  targetIsDir <- Dir.doesDirectoryExist (takeDirectory linkPath </> targetStr)
  result <-
    try $
      if targetIsDir
        then Dir.createDirectoryLink targetStr linkPath
        else Dir.createFileLink targetStr linkPath
  case result of
    Right () -> pure (Right ())
    Left (e :: SomeException) ->
      pure
        ( Left
            ( "cannot create symlink "
                <> T.pack linkPath
                <> " -> "
                <> target
                <> ": "
                <> T.pack (show e)
                <> " (on Windows this needs Developer Mode or elevation)"
            )
        )

-- | Whether a NAR directory entry name is safe to materialize on every
-- platform the store targets.  Two rejection classes:
--
-- 1. Path escapes: empty, @.@, @..@, a separator, a NUL (truncates the
--    name in any NUL-terminated API downstream), or a @:@ - a
--    drive-prefixed name like @C:evil@ makes 'System.FilePath.</>'
--    discard the store prefix entirely.
--
-- 2. Names the Win32 path layer silently REWRITES rather than refuses,
--    landing the bytes somewhere other than the named entry so the
--    on-disk tree no longer reproduces the NAR hash that named it: an
--    alternate-data-stream @:@ diverts the contents into a stream of
--    another file, a reserved device stem (CON, PRN, AUX, NUL,
--    COM0-COM9, LPT0-LPT9, plus the superscript-digit forms) addresses
--    the device instead of a file, and a trailing dot or space is
--    stripped on create, folding distinct NAR names onto one on-disk
--    name.
--
-- Characters Windows merely REFUSES (@\"@, @*@, @<@, @>@, @|@) stay
-- allowed: the create call fails loudly and the unpack loop surfaces
-- the failure, which cannot misplace or corrupt anything.
isSafeNarName :: Text -> Bool
isSafeNarName name =
  not (T.null name)
    && name /= ".."
    && name /= "."
    && not (T.any escapesTree name)
    && not (trailingRewritten name)
    && not (reservedDeviceStem name)
  where
    escapesTree c = c == '/' || c == '\\' || c == ':' || c == '\0'
    trailingRewritten n = case T.unsnoc n of
      Just (_, end) -> end == '.' || end == ' '
      Nothing -> False
    -- Win32 device parsing takes the name up to the first dot as the
    -- stem and ignores trailing spaces there ("NUL .txt" still
    -- addresses NUL), so the stem is space-trimmed before comparison.
    reservedDeviceStem n =
      let stem = T.toUpper (T.dropWhileEnd (== ' ') (T.takeWhile (/= '.') n))
       in stem == "CON"
            || stem == "PRN"
            || stem == "AUX"
            || stem == "NUL"
            || numberedDeviceStem stem
    numberedDeviceStem stem = case T.unpack stem of
      [a, b, c, digit] -> ([a, b, c] == "COM" || [a, b, c] == "LPT") && deviceDigit digit
      _ -> False
    -- Digits 0-9 plus the superscript forms ('\185' '\178' '\179') the
    -- platform also reserves.
    deviceDigit c = isDigit c || c == '\185' || c == '\178' || c == '\179'

-- ---------------------------------------------------------------------------
-- Eval source materialization
-- ---------------------------------------------------------------------------

-- | Copy eval-coerced source paths into the store and register them.  The
-- evaluator's source-path cache maps each coerced filesystem path to its
-- @source@ fixed-output store path (text only - eval performs no store
-- writes).  Each entry not already valid is copied in, made read-only, and
-- registered with its real NAR hash.  A copied source carries no references.
materializeEvalSources :: Store -> Map Text Text -> IO ()
materializeEvalSources store sourceCache = mapM_ adopt (Map.toList sourceCache)
  where
    -- Each source registers IMMEDIATELY after its copy (sources carry no
    -- cross-references, so there is nothing to batch).  A tree already on
    -- disk is adopted only after verification: its NAR digest must
    -- reproduce the store path being registered - an interrupted earlier
    -- copy leaves a partial tree, and registering it as-is validates
    -- content that does not match its address.  A verified adoption never
    -- touches the files (re-copying onto a read-only tree fails on
    -- Windows and would wedge the store permanently); a failed one clears
    -- and re-copies.  Mirrors the builder's own prepareOutput recovery.
    adopt (rawPath, spText) =
      case parseStorePath defaultStoreDir spText of
        Nothing -> pure ()
        Just sp -> do
          valid <- isValid store sp
          unless valid $ do
            let dest = storePathToFilePath (stDir store) sp
            onDisk <- doesPathExist dest
            adoptable <- if onDisk then adoptedTreeMatches dest sp else pure False
            unless adoptable $ do
              when onDisk (Dir.removePathForcibly dest)
              copyPathInto (T.unpack rawPath) dest
              setReadOnly dest
            reg <- registrationFor store sp Nothing []
            registerPath (stDB store) reg

-- | Whether an on-disk tree reproduces the source store path it is about to
-- be registered under: its recursive NAR digest and the path's own name must
-- derive exactly this path.  An unreadable tree counts as a mismatch.
adoptedTreeMatches :: FilePath -> StorePath -> IO Bool
adoptedTreeMatches dest sp = do
  result <- try (NAR.serialiseFromPath dest)
  pure $ case result of
    Left (_ :: SomeException) -> False
    Right entry ->
      makeFixedOutputPath (spName sp) "sha256" "recursive" (sha256Digest (NAR.serialise entry)) == Right sp

-- | Recursively copy a file or directory tree to a destination path.
-- A symlink is replicated as a symlink: the store path's name came from a
-- NAR hash that ENCODES the link entry, so dereferencing it here would
-- store content that no longer matches its own address (and a
-- self-referential link would recurse forever).  On Windows without
-- symlink privilege the link creation fails loudly rather than silently
-- corrupting the content address.
copyPathInto :: FilePath -> FilePath -> IO ()
copyPathInto src dest = do
  isLink <- Dir.pathIsSymbolicLink src
  if isLink
    then do
      target <- Dir.getSymbolicLinkTarget src
      linkedDir <- doesDirectoryExist src
      if linkedDir
        then Dir.createDirectoryLink target dest
        else Dir.createFileLink target dest
    else do
      isDir <- doesDirectoryExist src
      if isDir
        then do
          createDirectoryIfMissing True dest
          names <- listDirectory src
          mapM_ (\name -> copyPathInto (src </> name) (dest </> name)) names
        else copyFile src dest
