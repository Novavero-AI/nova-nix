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
-- A GC root is an explicit "keep this" marker: the out-link
-- @build --out-link@ creates, or a file the operator drops under the
-- store's @gcroots@ directory.  The collector walks every root, follows
-- references transitively, and deletes everything not reachable; a
-- single-path delete refuses a path that walk reaches.  "Nix.Store.GC"
-- holds the roots model, the walk, the sweep, and the lock that keeps a
-- collection and a concurrent writer apart; "Nix.Store.Handle" holds the
-- open handle and its lease on that lock.
module Nix.Store
  ( -- * Queries
    isValid,
    pathExists,

    -- * Deletion
    DeleteOutcome (..),
    deleteStorePathRaw,
    deleteStorePathChecked,
    resolveDeleteTarget,

    -- * Store operations
    addToStore,
    copyPathInto,
    placeInStore,
    registrationFor,
    materializeEvalSources,
    materializeEvalStoreWrites,
    scanReferences,
    scanTempReferences,
    setReadOnly,
    unpackNarEntry,
    writeDrv,
    writeDrvAterm,
    writeDrvClosure,

    -- * Streaming NAR unpacking
    NarUnpackSink,
    newNarUnpackSink,
    sinkNarEvent,
    finishNarUnpack,
    abortNarUnpack,
    NarStreamFailure (..),
    sinkNarStream,

    -- * Link ordering (exposed for testing)
    orderLinks,

    -- * Case-hack naming (exposed for testing)
    onDiskNameKey,
    caseHackDiskNames,

    -- * Re-exports
    module Nix.Store.Path,
    module Nix.Store.DB,
    module Nix.Store.Lock,
    module Nix.Store.CaseSensitive,
    module Nix.Store.Handle,
    module Nix.Store.GC,
  )
where

import Control.Exception (IOException, SomeException, bracket, catch, onException, throwIO, try, tryJust)
import Control.Monad (guard, join, unless, when)
import Data.Bool (bool)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (toUpper)
import Data.Foldable (traverse_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (inits)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Nix.Derivation (Derivation (..), fromATerm, toATerm)
import Nix.Hash (makeFixedOutputPath, makeTextPath, sha256Digest)
import Nix.Store.CaseSensitive (CaseSensitivity (..), processCaseHack, trySetCaseSensitiveDir, volumeCaseHack)
import Nix.Store.DB
import Nix.Store.EntryName (checkEntryName, entryNamePath, hostNameRules, linkTargetPath, shownBytes)
import Nix.Store.Exclusive (Occupant (..), directoryTakenMessage, fileTakenMessage, openNewBinaryFile, symlinkTakenMessage)
import qualified Nix.Store.ExecBit as ExecBit
import Nix.Store.GC
import Nix.Store.Handle
import Nix.Store.Lock
import Nix.Store.Path
import Nix.Store.Symlink (WalkNode (..), classifyWalkNode, createSymlinkOfKind)
import qualified NovaCache.Hash as Hash
import qualified NovaCache.NAR as NAR
import qualified NovaCache.NAR.Stream as Stream
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesPathExist,
    listDirectory,
    renamePath,
    setPermissions,
  )
import qualified System.Directory as Dir
import qualified System.Directory.OsPath as OsDir
import System.FilePath ((</>))
import System.IO (Handle, IOMode (ReadMode), hClose, withBinaryFile)
import System.IO.Error (isAlreadyExistsError)
import System.OsPath (OsPath)
import qualified System.OsPath as OP

-- | Check if a store path is registered as valid in the database.
isValid :: Store -> StorePath -> IO Bool
isValid = isValidPath . stDB

-- | Check if a store path exists on disk (file or directory, regardless of DB).
pathExists :: Store -> StorePath -> IO Bool
pathExists store sp = doesPathExist (storePathToFilePath (stDir store) sp)

-- ---------------------------------------------------------------------------
-- Deletion
-- ---------------------------------------------------------------------------

-- | What 'deleteStorePathRaw' removed.
data DeleteOutcome = DeleteOutcome
  { -- | A ValidPaths row (with its outgoing reference edges) was removed.
    doRowRemoved :: !Bool,
    -- | An on-disk tree was removed.
    doTreeRemoved :: !Bool
  }
  deriving (Eq, Show)

-- | Resolve a @store delete@ argument to the basename it names: a bare
-- @hash-name@ basename, or a full path spelled under the opened store's
-- directory, the platform store, or the canonical store.  The basename is
-- NOT validated as a store path - the entries deletion exists to remove
-- are the ones current name rules reject - so only traversal shapes are
-- refused: separators (excluded by construction), dot-leading names (the
-- store's metadata directory lives at a dot name), and colons (an NTFS
-- alternate-data-stream spelling).  Lock files are refused too, but not
-- here: names like @flake.lock@ are legal store-path names, so telling a
-- lock file from a store object needs the registration rows, and that
-- decision lives in 'deleteStorePathRaw'.
resolveDeleteTarget :: StoreDir -> Text -> Either Text Text
resolveDeleteTarget storeDir raw
  | T.null basename = Left (raw <> ": empty store path name")
  | T.isPrefixOf "." basename =
      Left (basename <> ": dot-leading names are not store paths")
  | T.any (== ':') basename =
      Left (basename <> ": ':' is not valid in a store path name")
  | not dirOk = Left (raw <> ": not a path under this store")
  | otherwise = Right basename
  where
    basename = T.takeWhileEnd (\c -> c /= '/' && c /= '\\') raw
    dirPart =
      normalizeSeps
        (T.dropWhileEnd (\c -> c == '/' || c == '\\') (T.dropEnd (T.length basename) raw))
    dirOk =
      T.null dirPart
        || dirPart
          `elem` [ normalizeSeps (T.pack (unStoreDir storeDir)),
                   normalizeSeps (T.pack (unStoreDir platformStoreDir)),
                   normalizeSeps (T.pack (unStoreDir defaultStoreDir))
                 ]
    normalizeSeps = T.map (\c -> if c == '\\' then '/' else c)

-- | Delete one store entry by basename: the registration row (refused
-- while a root keeps it alive, or while other valid paths reference it)
-- and the on-disk tree.  One checked delete under its own collector
-- lock and live set; a batch shares both through 'withLiveSet' and
-- 'deleteStorePathChecked'.
deleteStorePathRaw :: Store -> Text -> IO (Either Text DeleteOutcome)
deleteStorePathRaw store basename =
  join <$> withLiveSet store (\live -> deleteStorePathChecked store live basename)

-- | The checked delete of one entry, given the live set, which also
-- stands for the collector lock being held.  Row first, tree second: an
-- orphan tree left by a failed removal is inert debris, while a
-- still-registered row whose tree is gone would be adopted as valid by
-- existence checks.  A row without a tree and a tree without a row both
-- delete (the repair cases); only a target with neither is an error.
--
-- Liveness is upstream's rule for a specific delete: a path reachable
-- from a root is refused with its words.  The referrer refusal that
-- follows is this tool's own in wording and in shape.  Upstream refuses
-- a path with an unrooted referrer with the same still-alive line
-- unless that referrer is named in the same invocation (its
-- @pathsToDelete@ set in 2.24.9's @deleteReferrersClosure@), in which
-- case the named set deletes referrer-first; this delete removes
-- exactly the path named and lists the referrers that stand in the way,
-- so a chain deletes leaf-first in argument order.  Inside the collector
-- lock the sequence also holds the target's per-path lock - the same
-- file a substituter of the path locks, as upstream's deletePath does -
-- so a holder that is not a store handle (nothing in this tool, but
-- nothing forbids one) still cannot have its delete-materialize-register
-- torn apart by this delete.
deleteStorePathChecked :: Store -> LiveSet -> Text -> IO (Either Text DeleteOutcome)
deleteStorePathChecked store live basename
  | isLive live (T.pack target) = pure (Left (stillAliveMessage (T.pack target)))
  | otherwise = deleteUnderPathLock
  where
    target = unStoreDir (stDir store) </> T.unpack basename
    deleteUnderPathLock = withLockFile (target <> lockFileSuffix) $ \_ -> do
      -- A holder never deletes a lock file (the 'Nix.Store.Lock'
      -- header), and neither does this delete.  A target is one when
      -- stripping the suffix leaves a well-formed store basename AND no
      -- registration row bears the full name: names like @flake.lock@
      -- are legal store-path names, so a registered object of this
      -- exact name deletes normally, and only the rows can tell the two
      -- apart.  The check precedes the row removal below, which is
      -- destructive.
      registered <- case parseStorePathBaseName basename of
        Just sp -> isValidPath (stDB store) sp
        Nothing -> pure False
      case (registered, lockedPathOf basename) of
        (False, Just guardedPath) ->
          pure
            ( Left
                ( basename
                    <> ": names the lock file of "
                    <> guardedPath
                    <> "; lock files coordinate concurrent store access"
                    <> " and are never deleted here (store gc removes it,"
                    <> " and any unregistered store object of this exact"
                    <> " name, once "
                    <> guardedPath
                    <> " is not live)"
                )
            )
        _ -> deleteRowAndTree
    deleteRowAndTree = do
      rowResult <- unregisterPathRow (stDB store) (T.pack target)
      case rowResult of
        RowReferenced referrers ->
          pure
            ( Left
                ( basename
                    <> " is referenced by:"
                    <> T.concat ["\n  " <> r | r <- referrers]
                )
            )
        _ -> do
          -- 'doesPathExist' follows links, so a dangling top-level symlink
          -- would read as absent; links are leaves here (as in store walks),
          -- and leftover link debris must still be removable.
          treeExisted <- do
            onDisk <- doesPathExist target
            if onDisk
              then pure True
              else Dir.pathIsSymbolicLink target `catch` \(_ :: IOException) -> pure False
          when treeExisted (Dir.removePathForcibly target)
          let rowRemoved = rowResult == RowUnregistered
          if rowRemoved || treeExisted
            then pure (Right DeleteOutcome {doRowRemoved = rowRemoved, doTreeRemoved = treeExisted})
            else pure (Left (basename <> ": not in this store (no registration row, no tree on disk)"))

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
  moveOutput srcPath destPath
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
  narEntry <- ExecBit.serialiseFromPath (volumeCaseHack (stCaseSensitivity store)) destPath
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
moveOutput src dest
  -- A build writes into its own output path, so the usual case is a move
  -- onto itself.  POSIX rename(a,a) is a benign no-op, but MoveFileEx is
  -- documented as unusable when either name is a directory, and the
  -- IOException fallback below would then copy a tree into itself and
  -- delete the result.  Answering here settles both platforms.
  | src == dest = pure ()
  | otherwise =
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
-- Exclusive creation
-- ---------------------------------------------------------------------------

-- Every regular file and directory a tree materializes is created
-- exclusively: a name already taken refuses the write instead of
-- landing on the earlier entry.  'onDiskNameKey' predicts which
-- siblings a folding volume merges, but only the volume knows its own
-- fold table, so the create is what catches a pair the key misses.
-- Upstream's restore creates the same way (@RestoreSink@ in
-- fs-sink.cc at 2.24.9: @O_CREAT | O_EXCL@ for a regular file, a
-- refused @create_directory@ for a directory), and the refusals carry
-- its words ("Nix.Store.Exclusive").  Symlinks need nothing extra:
-- @symlink(2)@ and CreateSymbolicLinkW never replace an existing name,
-- and 'createSymlinkOfKind' words that refusal as upstream does.

-- | Run a create that must find its name free, its refusal becoming the
-- failure the given action describes.  Only the refusal becomes a
-- value: it is a property of the tree being written, where any other
-- I\/O failure (permissions, a full disk) is one of the machine and
-- stays an exception.
refuseTaken :: IO Text -> IO a -> IO (Either Text a)
refuseTaken describe create = do
  attempt <- tryJust (guard . isAlreadyExistsError) create
  either (const (Left <$> describe)) (pure . Right) attempt

-- | Create a regular file of a materialized tree exclusively and write
-- it through the handle, closed on the way out.
withNewTreeFile :: OsPath -> (Handle -> IO a) -> IO (Either Text a)
withNewTreeFile path write =
  bracket (openNewTreeFile path) (traverse_ hClose) (traverse write)

-- | Open a regular file of a materialized tree, created exclusively.
openNewTreeFile :: OsPath -> IO (Either Text Handle)
openNewTreeFile path = refuseTaken (fileTakenMessage <$> OP.decodeFS path) (openNewBinaryFile path)

-- | Create a directory of a materialized tree exclusively.  Its parent
-- is created first if missing, which only the root can need.  A
-- refusal asks what holds the name, as @create_directory@ does before
-- it chooses between returning false and throwing.
createTreeDirectory :: OsPath -> IO (Either Text ())
createTreeDirectory path = do
  OsDir.createDirectoryIfMissing True (OP.takeDirectory path)
  refuseTaken describe (OsDir.createDirectory path)
  where
    describe = do
      occupant <- bool OccupiedByOther OccupiedByDirectory <$> OsDir.doesDirectoryExist path
      directoryTakenMessage occupant <$> OP.decodeFS path

-- ---------------------------------------------------------------------------
-- NAR unpacking
-- ---------------------------------------------------------------------------

-- | Unpack a NarEntry tree to a filesystem destination.  Returns @Left@
-- on an entry name or link target the host cannot hold
-- ("Nix.Store.EntryName"); these can come from untrusted cache data, so
-- a typed failure is used instead of a partial 'error'.
--
-- Names and targets reach the filesystem as the bytes the NAR carries,
-- as upstream's restore writes them on POSIX: the tree is walked in
-- 'OsPath's, a POSIX path being bytes.  Only the destination enters as
-- a 'FilePath', spelled as every 'FilePath' API spells it.
--
-- The sensitivity is the destination volume's ('stCaseSensitivity' for
-- a store path) and decides whether case-variant siblings need the
-- case-hack ('onDiskNameKey').
--
-- Regular files and directories are written in one pass; symlinks are
-- created in a second pass, after their targets are materialized.  Windows
-- symlinks are typed (file vs directory) and the NAR format does not record
-- the target's kind, so the only reliable way to pick the flavor is to look
-- at the target on disk - which may sort after the link within the tree.
unpackNarEntry :: CaseSensitivity -> FilePath -> NAR.NarEntry -> IO (Either Text ())
unpackNarEntry sensitivity path entry = do
  root <- OP.encodeFS path
  walked <- unpackTree sensitivity root entry
  case walked of
    Left err -> pure (Left err)
    Right links -> createSymlinks links

-- | First unpack pass: write regular files and directories, recording
-- symlinks as (link path, target) for the second pass.
unpackTree :: CaseSensitivity -> OsPath -> NAR.NarEntry -> IO (Either Text [(OsPath, OsPath)])
unpackTree sensitivity path entry = case entry of
  NAR.NarRegular isExec contents -> do
    OsDir.createDirectoryIfMissing True (OP.takeDirectory path)
    written <- withNewTreeFile path (`BS.hPut` contents)
    case written of
      Left err -> pure (Left err)
      Right () -> do
        when isExec (ExecBit.markExecutableOsPath path)
        pure (Right [])
  NAR.NarSymlink target -> fmap (\targetPath -> [(path, targetPath)]) <$> linkTargetPath target
  NAR.NarDirectory entries -> do
    created <- createTreeDirectory path
    case created of
      Left err -> pure (Left err)
      Right () -> unpackChildren sensitivity path entries

-- | The on-disk identity a NAR entry name occupies on a volume of the
-- given case sensitivity.  Two sibling entries sharing a key name ONE
-- file on that volume, so the unpack keeps them apart
-- (@unpackNamedChildren@).  A folding volume (the default APFS format,
-- NTFS through Win32) keys a name by its uppercase; a sensitive one
-- (Linux, a case-sensitive APFS volume) keys it byte-for-byte.  The
-- fold is per-character uppercase, which keeps apart the collisions
-- real trees carry (@Makefile@\/@makefile@) but not every pair the
-- volume folds: APFS reads U+00DF and U+1E9E as one name while
-- 'toUpper' leaves U+00DF unchanged, so that pair takes two keys and
-- the exclusive create refuses the second at the write (#234), as
-- upstream's restore does.  The reverse miss (U+0131 uppercases to
-- @I@, which APFS keeps distinct) only spells a name with a suffix it
-- did not need.  A name that is not UTF-8 has no characters to fold
-- and keys as its bytes; no host whose volumes fold admits one
-- ("Nix.Store.EntryName"), and the create would catch what the key
-- missed.  Win32 strips a trailing dot or space on create, which would
-- merge names this key keeps apart, but Windows refuses such names
-- before any is keyed.
--
-- Upstream decides this per process, not per volume: its
-- @use-case-hack@ setting defaults to on for Darwin and off elsewhere
-- (archive.cc at 2.24.9), so a case-sensitive macOS volume still has
-- @makefile@ restored as @makefile~nix~case~hack~1@ (observed from
-- @nix-store --restore@ 2.33.2).  The hack exists to work around
-- folding, and a volume that does not fold needs none, so nova-nix
-- keys by the probed answer and materializes true names there.  The
-- divergence is in on-disk spelling only: either tree re-serialises
-- to the same NAR.
onDiskNameKey :: CaseSensitivity -> BS.ByteString -> BS.ByteString
onDiskNameKey sensitivity name = case sensitivity of
  CaseSensitive -> name
  CaseInsensitive -> either (const name) (TE.encodeUtf8 . T.map toUpper) (TE.decodeUtf8' name)

-- | The first pair of sibling names folding to the same on-disk file,
-- if any: (earlier entry, colliding later entry).
firstNameCollision :: CaseSensitivity -> [BS.ByteString] -> Maybe (BS.ByteString, BS.ByteString)
firstNameCollision sensitivity = go Map.empty
  where
    go !_ [] = Nothing
    go !seen (name : rest) =
      let key = onDiskNameKey sensitivity name
       in case Map.lookup key seen of
            Just earlier -> Just (earlier, name)
            Nothing -> go (Map.insert key name seen) rest

-- | Whether a tree on a volume of the given case sensitivity is read
-- back with the case-hack suffix stripped ('volumeCaseHack').  Where
-- it is, an INCOMING entry name carrying the suffix must be rejected:
-- materialized verbatim it would re-serialise under a different name
-- and fail its own hash recheck.  Upstream's restore accepts such a
-- name and its dump then strips the suffix (archive.cc at 2.24.9), so
-- the tree fails there as well, only later; refusing at the write
-- boundary surfaces the same failure before anything is materialized.
-- A volume that keeps names apart reads every name as spelled, so the
-- name materializes and round-trips there.
volumeStripsCaseHack :: CaseSensitivity -> Bool
volumeStripsCaseHack sensitivity = volumeCaseHack sensitivity == NAR.CaseHackEnabled

-- | Refuse an entry name before any sibling is written: one the host
-- cannot hold ('checkEntryName'), or one carrying the case-hack suffix
-- where the destination volume's serialiser would strip it.
admitEntryName :: CaseSensitivity -> BS.ByteString -> Either Text ()
admitEntryName sensitivity name = do
  checkEntryName hostNameRules name
  when (volumeStripsCaseHack sensitivity && NAR.caseHackSuffix `BS.isInfixOf` name) $
    Left ("NAR entry name contains the case-hack suffix: " <> shownBytes name)

-- | The case-hacked disk name for the given occurrence of a key.
caseHackName :: BS.ByteString -> Int -> BS.ByteString
caseHackName name occurrence = name <> NAR.caseHackSuffix <> BS8.pack (show occurrence)

-- | Disk names for a sibling list on a volume of the given case
-- sensitivity, WITHOUT per-directory case sensitivity: upstream's
-- case-hack where names fold, every name as spelled where they do not.
-- The first occurrence of each key keeps its spelling; every later
-- name with the same key gains the reversible suffix and a per-name
-- counter, which the serialiser strips on the way back out
-- ('volumeCaseHack').  Order is preserved; result pairs are (NAR
-- name, on-disk name).
caseHackDiskNames :: CaseSensitivity -> [BS.ByteString] -> [(BS.ByteString, BS.ByteString)]
caseHackDiskNames sensitivity = reverse . snd . foldl' step (Map.empty, [])
  where
    step (!seen, !acc) name =
      let key = onDiskNameKey sensitivity name
       in case Map.lookup key seen of
            Nothing -> (Map.insert key (0 :: Int) seen, (name, name) : acc)
            Just occurrences ->
              let next = occurrences + 1
               in (Map.insert key next seen, (name, caseHackName name next) : acc)

-- | Unpack directory children.  Entry names arrive as the raw bytes
-- the NAR carries, and every sibling's name is admitted
-- ('admitEntryName') before the first is written, so a name the host
-- cannot hold leaves no partial directory behind it.
unpackChildren :: CaseSensitivity -> OsPath -> [(BS.ByteString, NAR.NarEntry)] -> IO (Either Text [(OsPath, OsPath)])
unpackChildren sensitivity path entries = case traverse_ (admitEntryName sensitivity . fst) entries of
  Left err -> pure (Left err)
  Right () -> unpackNamedChildren sensitivity path entries

-- | Unpack admitted directory children.
--
-- On a volume the probe reports sensitive (Linux, case-sensitive APFS)
-- no two sibling names share an on-disk identity and every entry
-- materializes as spelled.  Sibling names folding to one on-disk name
-- (NTFS, default APFS) take the TRUE-NAME path when the platform
-- provides one: the just-created empty directory gains NTFS
-- per-directory case sensitivity and the tree materializes under its
-- real names.  Where the flag is unavailable (a non-NTFS store volume,
-- a folding APFS volume) the collision falls back to upstream's
-- case-hack renaming, which the volume's serialiser reverses.  Either
-- way a registered path re-serialises to its NAR byte-for-byte - the
-- substituter's on-disk recheck verifies it.  The name spelled on disk
-- is the one the host is asked to hold, suffix included.
unpackNamedChildren :: CaseSensitivity -> OsPath -> [(BS.ByteString, NAR.NarEntry)] -> IO (Either Text [(OsPath, OsPath)])
unpackNamedChildren sensitivity path entries = do
  diskNames <- resolveDiskNames
  walkChildren (zip diskNames (map snd entries))
  where
    names = map fst entries
    resolveDiskNames = case firstNameCollision sensitivity names of
      Nothing -> pure names
      Just _ -> do
        trueNames <- trySetCaseSensitiveDir =<< OP.decodeFS path
        pure
          ( if trueNames
              then names
              else map snd (caseHackDiskNames sensitivity names)
          )
    walkChildren [] = pure (Right [])
    walkChildren ((diskName, child) : rest) = do
      spelled <- entryNamePath diskName
      case spelled of
        Left err -> pure (Left err)
        Right component -> do
          result <- unpackTree sensitivity (path OP.</> component) child
          case result of
            Left err -> pure (Left err)
            Right links -> do
              restResult <- walkChildren rest
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
createSymlinks :: [(OsPath, OsPath)] -> IO (Either Text ())
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
orderLinks :: [(OsPath, OsPath)] -> [(OsPath, OsPath)]
orderLinks pending =
  let indexed = zip [0 :: Int ..] pending
      linkByIndex = Map.fromList indexed
      indexByKey =
        Map.fromList [(normalisedComponents linkPath, i) | (i, (linkPath, _)) <- indexed]
      -- The pending links this link's resolved target lands on or
      -- passes through (every nonempty component prefix).
      depsOf (linkPath, target) =
        let resolved = normalisedComponents (OP.takeDirectory linkPath OP.</> target)
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
-- link paths ('OP.splitDirectories' accepts both separator spellings
-- on Windows).  A @..@ with nothing left to pop stays, matching no real
-- path.
normalisedComponents :: OsPath -> [OsPath]
normalisedComponents path = reverse (foldl' step [] (OP.splitDirectories path))
  where
    step stack comp
      | comp == currentDirectory = stack
      | comp == parentDirectory = case stack of
          (top : rest) | top /= parentDirectory -> rest
          _ -> comp : stack
      | otherwise = comp : stack
    currentDirectory = OP.pack [dot]
    parentDirectory = OP.pack [dot, dot]
    dot = OP.unsafeFromChar '.'

-- | Create one unpacked symlink, parents first, with the Windows flavor
-- read off the target ('createSymlinkOfKind').  A creation failure is
-- loud, and failing lets the caller fall back to a local build.
createSymlink :: OsPath -> OsPath -> IO (Either Text ())
createSymlink linkPath target = do
  OsDir.createDirectoryIfMissing True (OP.takeDirectory linkPath)
  createSymlinkOfKind linkPath target

-- ---------------------------------------------------------------------------
-- Streaming NAR unpacking
-- ---------------------------------------------------------------------------

-- | One open directory in the streaming unpack: its on-disk path and
-- the sibling names materialized so far, keyed by their on-disk
-- identity ('onDiskNameKey') for collision handling.
data UnpackFrame = UnpackFrame
  { ufPath :: !OsPath,
    ufSeen :: !(Map BS.ByteString Int)
  }

-- | Mutable state behind a 'NarUnpackSink' - the same deliberate IO
-- boundary as the store's other materializers.  'nusTargets' is the
-- stack of on-disk paths the NEXT node materializes at: the
-- destination at the root, plus one pushed per open directory entry.
data NarUnpackState = NarUnpackState
  { -- | The destination volume's case sensitivity, keying sibling
    -- names exactly as the strict path does.
    nusCaseSensitivity :: !CaseSensitivity,
    nusFrames :: ![UnpackFrame],
    nusTargets :: ![OsPath],
    nusOpen :: !(Maybe (Handle, OsPath, Bool)),
    nusLinks :: ![(OsPath, OsPath)]
  }

-- | A push sink materializing 'Stream.NarEvent's under a destination
-- path as they arrive, so a substituted NAR unpacks in the same pass
-- that downloads it.  Semantics mirror 'unpackNarEntry' - the same
-- name bytes and host checks, executable bit, and second-pass
-- symlink creation - with one divergence: sibling names colliding on
-- a folding volume always take upstream's case-hack renaming, never
-- the NTFS true-name path, because per-directory case sensitivity can
-- only be enabled on an EMPTY directory and a stream cannot know a
-- directory's siblings before materializing the first.  On a volume
-- the probe reports sensitive nothing collides, as in the strict path.
-- Upstream's own streaming restore behaves identically, and the
-- substituter's on-disk recheck proves the tree re-serialises to its
-- NAR either way.
newtype NarUnpackSink = NarUnpackSink (IORef NarUnpackState)

-- | A sink for one NAR unpack under the given destination path, on a
-- volume of the given case sensitivity.
newNarUnpackSink :: CaseSensitivity -> FilePath -> IO NarUnpackSink
newNarUnpackSink sensitivity destPath = do
  root <- OP.encodeFS destPath
  NarUnpackSink <$> newIORef (NarUnpackState sensitivity [] [root] Nothing [])

-- | Feed one event.  On 'Left' the partial tree stays for the caller
-- to remove - 'abortNarUnpack' first, so no handle stays open on it.
sinkNarEvent :: NarUnpackSink -> Stream.NarEvent -> IO (Either Text ())
sinkNarEvent (NarUnpackSink ref) event = do
  narState <- readIORef ref
  outcome <- applyNarEvent narState event
  case outcome of
    Left err -> pure (Left err)
    Right updated -> do
      writeIORef ref updated
      pure (Right ())

-- | One event's filesystem effects plus the state that follows it.
-- The stream machine already proved grammar well-formedness, so the
-- mismatch arms guard sink-state desync, not archive syntax.
applyNarEvent :: NarUnpackState -> Stream.NarEvent -> IO (Either Text NarUnpackState)
applyNarEvent narState event = case event of
  Stream.EventRegularBegin isExec _declaredSize -> withNodeTarget narState $ \path -> do
    OsDir.createDirectoryIfMissing True (OP.takeDirectory path)
    opened <- openNewTreeFile path
    pure (fmap (\fileHandle -> narState {nusOpen = Just (fileHandle, path, isExec)}) opened)
  Stream.EventRegularChunk slice -> case nusOpen narState of
    Nothing -> pure (Left "NAR stream sink: file contents outside an open file")
    Just (fileHandle, _, _) -> do
      BS.hPut fileHandle slice
      pure (Right narState)
  Stream.EventRegularEnd -> case nusOpen narState of
    Nothing -> pure (Left "NAR stream sink: file close without an open file")
    Just (fileHandle, path, isExec) -> do
      hClose fileHandle
      when isExec (ExecBit.markExecutableOsPath path)
      pure (Right narState {nusOpen = Nothing})
  Stream.EventSymlink targetBytes -> withNodeTarget narState $ \path ->
    fmap (\target -> narState {nusLinks = (path, target) : nusLinks narState})
      <$> linkTargetPath targetBytes
  Stream.EventDirectoryBegin -> withNodeTarget narState $ \path -> do
    created <- createTreeDirectory path
    pure (narState {nusFrames = UnpackFrame path Map.empty : nusFrames narState} <$ created)
  Stream.EventEntryBegin name -> case nusFrames narState of
    [] -> pure (Left "NAR stream sink: entry outside a directory")
    (frame : outer) -> case admitEntryName (nusCaseSensitivity narState) name of
      Left err -> pure (Left err)
      Right () -> do
        -- Sequential case-hack: the disk name of entry N depends only
        -- on the siblings before it, the same sequence
        -- 'caseHackDiskNames' folds over a whole list.
        let key = onDiskNameKey (nusCaseSensitivity narState) name
            (diskName, occurrences) = case Map.lookup key (ufSeen frame) of
              Nothing -> (name, 0)
              Just seen -> (caseHackName name (seen + 1), seen + 1)
            updatedFrame = frame {ufSeen = Map.insert key occurrences (ufSeen frame)}
        spelled <- entryNamePath diskName
        pure $
          fmap
            ( \component ->
                narState
                  { nusFrames = updatedFrame : outer,
                    nusTargets = (ufPath frame OP.</> component) : nusTargets narState
                  }
            )
            spelled
  Stream.EventEntryEnd -> case nusTargets narState of
    -- The root destination never pops; only entry-pushed paths do.
    (_ : rest@(_ : _)) -> pure (Right narState {nusTargets = rest})
    _ -> pure (Left "NAR stream sink: entry close without an open entry")
  Stream.EventDirectoryEnd -> case nusFrames narState of
    [] -> pure (Left "NAR stream sink: directory close without an open directory")
    (_ : outer) -> pure (Right narState {nusFrames = outer})

-- | Run an action on the path the next node materializes at.
withNodeTarget :: NarUnpackState -> (OsPath -> IO (Either Text NarUnpackState)) -> IO (Either Text NarUnpackState)
withNodeTarget narState act = case nusTargets narState of
  (path : _) -> act path
  [] -> pure (Left "NAR stream sink: node with no destination")

-- | Finish after 'Stream.NarDone': every node must be closed, then
-- the recorded symlinks are created - the same dependency ordering
-- and flavor probing as the strict path's second pass.
finishNarUnpack :: NarUnpackSink -> IO (Either Text ())
finishNarUnpack (NarUnpackSink ref) = do
  narState <- readIORef ref
  case (nusOpen narState, nusFrames narState) of
    (Just _, _) -> pure (Left "NAR stream sink: stream ended inside a file")
    (Nothing, _ : _) -> pure (Left "NAR stream sink: stream ended inside a directory")
    (Nothing, []) -> createSymlinks (reverse (nusLinks narState))

-- | Close any open handle so the caller can remove the partial tree;
-- Windows will not delete a file a handle still holds open.
abortNarUnpack :: NarUnpackSink -> IO ()
abortNarUnpack (NarUnpackSink ref) = do
  narState <- readIORef ref
  case nusOpen narState of
    Nothing -> pure ()
    Just (fileHandle, _, _) ->
      hClose fileHandle `catch` \(_ :: IOException) -> pure ()
  writeIORef ref narState {nusOpen = Nothing}

-- | Why a streamed archive stopped short of its end.
data NarStreamFailure
  = -- | The store refused what the archive holds (a name, a taken
    -- path): a property of the archive, not of how it arrived.
    NarStreamRefused !Text
  | -- | The bytes do not parse as a NAR.  A truncated or torn transfer
    -- reads the same as a malformed archive.
    NarStreamMalformed !Text
  deriving (Eq, Show)

-- | Feed a chunk source through the streaming NAR parser into a sink,
-- hashing and counting exactly the bytes the parser consumes, and
-- return the archive's SHA-256 and size once its root node closes.
-- The recorded symlinks are not created yet ('finishNarUnpack'), so a
-- caller checks the digest before any link exists.  On a failure the
-- sink is already aborted.
sinkNarStream :: NarUnpackSink -> IO BS.ByteString -> IO (Either NarStreamFailure (Hash.NixHash, Int))
sinkNarStream sink source = go Hash.hashInit 0 Stream.narStream `onException` abortNarUnpack sink
  where
    go !ctx !count step = case step of
      Stream.NarAwait continue -> do
        chunk <- source
        go (Hash.hashUpdate ctx chunk) (count + BS.length chunk) (continue chunk)
      Stream.NarYield event next -> do
        sunk <- sinkNarEvent sink event
        either (stopWith . NarStreamRefused) (const (go ctx count next)) sunk
      Stream.NarFail msg -> stopWith (NarStreamMalformed (T.pack msg))
      Stream.NarDone -> pure (Right (Hash.hashFinalize ctx, count))
    stopWith failure = Left failure <$ abortNarUnpack sink

-- ---------------------------------------------------------------------------
-- Eval source materialization
-- ---------------------------------------------------------------------------

-- | Restore eval-coerced source paths into the store and register them.
-- The evaluator's source-path cache maps each coerced filesystem path to
-- its @source@ fixed-output store path (text only - eval performs no
-- store writes).  Each entry not already valid is restored from a dump
-- of its source ('restoreSource'), made read-only, and registered.  A
-- source carries no references.
materializeEvalSources :: Store -> Map Text Text -> IO ()
materializeEvalSources store sourceCache = mapM_ adopt (Map.toList sourceCache)
  where
    -- Each source registers IMMEDIATELY after its restore (sources carry
    -- no cross-references, so there is nothing to batch).  A tree already
    -- on disk is adopted only after verification: its NAR digest must
    -- reproduce the store path being registered - an interrupted earlier
    -- restore leaves a partial tree, and registering it as-is validates
    -- content that does not match its address.  A verified adoption never
    -- touches the files (rewriting a read-only tree fails on Windows and
    -- would wedge the store permanently); a failed one clears and
    -- restores.  Mirrors the builder's own prepareOutput recovery.
    -- The whole check-then-act runs under the path's cross-process
    -- lock, validity re-checked once held: without it, two processes
    -- materializing the same source raced isValid, and the loser's
    -- removePathForcibly deleted the tree the winner had just
    -- registered.  Same protocol as the builder's withOutputLocks.
    adopt (rawPath, spText) =
      case parseStorePath defaultStoreDir spText of
        Nothing -> pure ()
        Just sp -> withPathLock (stDir store) sp $ \_ -> do
          valid <- isValid store sp
          unless valid $ do
            let dest = storePathToFilePath (stDir store) sp
            onDisk <- doesPathExist dest
            adoptable <- if onDisk then adoptedTreeMatches (stCaseSensitivity store) dest sp else pure False
            reg <-
              if adoptable
                then registrationFor store sp Nothing []
                else do
                  when onDisk (Dir.removePathForcibly dest)
                  restoreSource store (T.unpack rawPath) sp >>= either (refuse spText) pure
            registerPath (stDB store) reg
    refuse spText reason =
      throwIO (userError (T.unpack ("refusing to register " <> spText <> ": " <> reason)))

-- | Restore a source tree at the store path evaluation named it by,
-- from a dump of the source, as upstream's @addToStoreFromDump@
-- restores the dump it hashed (@restorePath@, local-store.cc at
-- 2.24.9) rather than copying files: the source's NAR streams out of
-- the walk, through the hash and into an unpack sink in one pass, so a
-- sibling pair the store's volume folds takes the case-hack on the way
-- in, and no file's contents are ever held whole.  The source is read
-- as evaluation read it to name the path, under the process's
-- case-hack setting, and the restored tree as a store path, under its
-- volume's.
--
-- Evaluation hashed the source earlier, so the dump is held to that
-- address before any link is created: a source that changed in between
-- is refused, never registered under a hash its bytes no longer have.
-- The restored tree is then rechecked on disk, as the substituter
-- rechecks an unpacked one, since the volume decides the final
-- spelling of every name.  Any failure removes the tree.
restoreSource :: Store -> FilePath -> StorePath -> IO (Either Text PathRegistration)
restoreSource store src sp = do
  outcome <- restore `onException` Dir.removePathForcibly dest
  either (\reason -> Left reason <$ Dir.removePathForcibly dest) (pure . Right) outcome
  where
    dest = storePathToFilePath (stDir store) sp
    restore = do
      sink <- newNarUnpackSink (stCaseSensitivity store) dest
      streamed <- ExecBit.withNarSource processCaseHack src (sinkNarStream sink)
      case streamed of
        Left (NarStreamRefused reason) -> pure (Left reason)
        Left (NarStreamMalformed reason) -> pure (Left ("the dump of " <> T.pack src <> " does not parse: " <> reason))
        Right (digest, size)
          | not (recursiveDigestNames sp digest) -> do
              abortNarUnpack sink
              pure (Left (T.pack src <> " changed after evaluation hashed it, so it no longer reproduces its store path; re-evaluate"))
          | otherwise -> do
              finished <- finishNarUnpack sink
              either (pure . Left) (const (sealAndRecheck digest size)) finished
    sealAndRecheck digest size = do
      setReadOnly dest
      onDisk <- ExecBit.narHashOfPath (volumeCaseHack (stCaseSensitivity store)) dest
      pure $
        if onDisk /= digest
          then Left "the restored tree does not reproduce its store path on disk"
          else
            Right
              PathRegistration
                { prPath = sp,
                  prNarHash = Hash.formatNixHash digest,
                  prNarSize = size,
                  prDeriver = Nothing,
                  prReferences = []
                }

-- | Whether a recursive NAR digest and a store path's own name derive
-- exactly that path, as a @source@ path's address is derived.
recursiveDigestNames :: StorePath -> Hash.NixHash -> Bool
recursiveDigestNames sp (Hash.NixHash digest) =
  makeFixedOutputPath (spName sp) "sha256" "recursive" digest == Right sp

-- | Register the store objects evaluation wrote: makes each read-only
-- and records it in the DB.  Batched so a write referring to another
-- resolves.  Covers every eval-time writer, not only @builtins.toFile@:
-- an unregistered write reaches @drvInputSrcs@ and fails the build.
materializeEvalStoreWrites :: Store -> Map Text ([StorePath], StoreWriteMode) -> IO ()
materializeEvalStoreWrites store storeWrites = do
  regs <- catMaybes <$> mapM prepare (Map.toList storeWrites)
  unless (null regs) (registerPaths (stDB store) regs)
  where
    -- Checked and sealed under the path's lock, validity re-checked
    -- once held, so a peer mid-producing the same path is waited out
    -- rather than torn-read (and refused).  The lock releases before
    -- the batched registration: registration is an upsert over content
    -- both holders verified reproduces the same path, so the winner
    -- and loser record the same row.
    prepare (spText, (refs, mode)) =
      case parseStorePath defaultStoreDir spText of
        Nothing -> pure Nothing
        Just sp -> withPathLock (stDir store) sp $ \_ -> do
          valid <- isValid store sp
          if valid
            then pure Nothing
            else do
              let dest = storePathToFilePath (stDir store) sp
              onDisk <- doesPathExist dest
              -- Absent means removed since eval; nothing to register.
              if not onDisk
                then pure Nothing
                else do
                  -- The writers verified (or rewrote) this content at
                  -- write time; a mismatch here means the tree changed
                  -- between evaluation and registration, and registering
                  -- it would record a hash its bytes do not have, then
                  -- seal the lie read-only.  Refuse loudly instead.
                  reproduces <- writeReproducesPath (stCaseSensitivity store) dest sp refs mode
                  unless reproduces $
                    throwIO
                      ( userError
                          ( "refusing to register "
                              <> T.unpack spText
                              <> ": the on-disk content does not reproduce its store path; "
                              <> "delete the path and re-evaluate"
                          )
                      )
                  setReadOnly dest
                  Just <$> registrationFor store sp Nothing refs

-- | Whether on-disk content re-derives exactly the store path it is
-- about to be registered under, under the scheme that named the write.
-- An unreadable destination counts as a mismatch.
writeReproducesPath :: CaseSensitivity -> FilePath -> StorePath -> [StorePath] -> StoreWriteMode -> IO Bool
writeReproducesPath sensitivity dest sp refs mode = case mode of
  WriteRecursive -> adoptedTreeMatches sensitivity dest sp
  WriteFlat ->
    withFileBytes (\bytes -> makeFixedOutputPath (spName sp) "sha256" "flat" (sha256Digest bytes) == Right sp)
  WriteText ->
    withFileBytes (\bytes -> makeTextPath (spName sp) (sha256Digest bytes) refs == Right sp)
  where
    withFileBytes check = do
      result <- try (BS.readFile dest) :: IO (Either IOException BS.ByteString)
      pure (either (const False) check result)

-- | Whether an on-disk tree reproduces the source store path it is about to
-- be registered under: its recursive NAR digest and the path's own name must
-- derive exactly this path.  An unreadable tree counts as a mismatch.  The
-- sensitivity is the store volume's, which decides the case-hack mode the
-- tree is read back under.
adoptedTreeMatches :: CaseSensitivity -> FilePath -> StorePath -> IO Bool
adoptedTreeMatches sensitivity dest sp = do
  result <- try (ExecBit.serialiseFromPath (volumeCaseHack sensitivity) dest)
  pure $ case result of
    Left (_ :: SomeException) -> False
    Right entry -> recursiveDigestNames sp (NAR.narHash entry)

-- | Recursively copy a file or directory tree to a destination path.
-- A symlink is replicated as a symlink: the store path's name came from a
-- NAR hash that ENCODES the link entry, so dereferencing it here would
-- store content that no longer matches its own address (and a
-- self-referential link would recurse forever).  On Windows without
-- symlink privilege the link creation fails loudly rather than silently
-- corrupting the content address.
--
-- Every entry is created exclusively, like an unpacked tree's:
-- siblings distinct on the source volume can fold together on the
-- destination's, and the copy refuses the second rather than landing
-- it on the first, raising the refusal in upstream's wording.
copyPathInto :: FilePath -> FilePath -> IO ()
copyPathInto src dest = do
  isLink <- Dir.pathIsSymbolicLink src
  if isLink
    then do
      target <- Dir.getSymbolicLinkTarget src
      linkedDir <- doesDirectoryExist src
      let createLink = if linkedDir then Dir.createDirectoryLink else Dir.createFileLink
      refuseTaken (pure (symlinkTakenMessage dest target)) (createLink target dest) >>= raiseRefusal
    else do
      isDir <- doesDirectoryExist src
      destPath <- OP.encodeFS dest
      if isDir
        then do
          createTreeDirectory destPath >>= raiseRefusal
          names <- listDirectory src
          mapM_ (\name -> copyPathInto (src </> name) (dest </> name)) names
        else do
          copied <- withNewTreeFile destPath (\to -> withBinaryFile src ReadMode (`copyHandleBytes` to))
          raiseRefusal copied
          -- Best effort: of the permissions, the NAR records only the
          -- exec mark, which is carried on its own below (the unnamed
          -- stream alone does not hold it on Windows).
          Dir.copyPermissions src dest `catch` \(_ :: IOException) -> pure ()
          ExecBit.copyExecMark src dest
  where
    raiseRefusal = either (throwIO . userError . T.unpack) pure

-- | Copy a readable handle's remaining bytes into a writable one, a
-- bounded chunk at a time.
copyHandleBytes :: Handle -> Handle -> IO ()
copyHandleBytes from to = go
  where
    go = do
      chunk <- BS.hGetSome from copyChunkBytes
      unless (BS.null chunk) $ do
        BS.hPut to chunk
        go

-- | The read size of 'copyHandleBytes': the block @directory@'s own
-- file copy reads, which is coreutils' @cp@ size.
copyChunkBytes :: Int
copyChunkBytes = 128 * 1024
