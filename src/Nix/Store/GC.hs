-- | Garbage collection: roots, the reachability walk, and the sweep.
--
-- == Roots
--
-- A root is a store path the collector keeps, together with everything
-- it references transitively.  Roots live under the store's metadata
-- directory, @\<store\>\/.nova-nix\/gcroots@.  Upstream keeps them under
-- its state directory (@\/nix\/var\/nix\/gcroots@); this store has no
-- state directory apart from @.nova-nix@, and roots beside the database
-- mean a store copied or moved as one directory keeps them.  Two kinds:
--
-- * __Permanent roots__ are entries anywhere under @gcroots@ except
--   @auto@: a symlink whose target lies in the store roots the store
--   path it lies in (the path itself or anything beneath it, upstream's
--   @toStorePath@), and a regular file named @\<hash\>-\<name\>@ roots
--   the path of that name.  Both are upstream's own forms (its
--   @findRoots@ reads a symlink's target and takes a regular file's
--   basename), and the file form is the one that works everywhere,
--   since a symlink on Windows needs Developer Mode.  Nothing in
--   nova-nix creates a permanent root; they are for the operator.
--
-- * __Indirect roots__ are what @build --out-link PATH@ registers
--   ('addOutLinkRoot', upstream's @addPermRoot@): the out-link
--   @PATH -> \<store path\>@ where the user asked for it, plus a record
--   @gcroots\/auto\/\<sha256 of PATH\>@ holding PATH's absolute
--   spelling.  Upstream's record is a symlink to the out-link; here it
--   is a regular file with the same content, because the record must be
--   writable on every platform the store runs on.  The out-link itself
--   is a native symlink everywhere, with the Developer Mode requirement
--   the README states for symlinks.  A record whose out-link no longer
--   exists is stale and removed by the next walk, as upstream removes
--   its stale @auto@ links; one whose out-link exists but no longer
--   points into the store is simply not a root.
--
-- Upstream also finds runtime roots (open files and mapped executables
-- of running processes, through @\/proc@ on Linux and @lsof@ elsewhere)
-- and per-process temporary roots.  Neither exists here: the lock below
-- covers builds in flight, and a program running out of an unrooted
-- store path is not protected.  Upstream's @keep-derivations@ and
-- @keep-outputs@ are not implemented either: the builder registers
-- outputs with no deriver, so a @.drv@ is kept only while something
-- references it, and an unrooted @.drv@ and the sources only it
-- references are collected.
--
-- == Reachability and the sweep
--
-- 'reachableFrom' is the pure walk over the references map: a root is
-- live, and so is everything a live path references.  Every valid path
-- outside that set is dead.  'sweepPlan', also pure, turns the valid
-- set, the live set and the store directory's entries into what to
-- remove: the dead rows, and every directory entry that is neither live
-- nor the metadata directory, which is what upstream's sweep removes
-- too (a dead path's tree, a store-shaped tree with no row from a build
-- interrupted before registration, junk, and a dead path's lock file).
-- A live path's lock file stays.  Upstream's holders delete their lock
-- files on release, so a leftover one is crash debris its sweep
-- reclaims; here holders never delete ("Nix.Store.Lock"), a live
-- path's lock file is its normal state, and only the path's death makes
-- the file debris.  The collector may delete lock files at all because
-- under the exclusive lock no process has the store open, and every
-- per-path lock in this tool is taken by a process holding a store
-- handle, so no holder or waiter exists for a deleted file to strand.
--
-- 'collectGarbage' is the IO boundary: dead rows go in one database
-- transaction ('unregisterPathRows'), then trees one by one.  Rows
-- before trees for the reason 'Nix.Store.deleteStorePathRaw' gives: a
-- tree without a row is inert debris, a row without a tree is a lie
-- existence checks believe.  A dead row with no tree is unregistered
-- without a line or a count: upstream's sweep walks the directory and
-- never meets it, and what is announced and counted here is what
-- upstream announces and counts, the directory entries removed.
--
-- == The store directory's spelling
--
-- The database keys every row by the store directory's spelling at
-- registration ("Nix.Store.DB"), and the collector compares that text
-- with paths derived from the handle's directory.
-- 'Nix.Store.Handle.openStore' canonicalises the spelling, so one
-- directory named two ways keys the same rows; a store whose rows were
-- registered under another spelling (a symlinked parent, a store moved
-- and opened at its new path) would make every row look dead, so the
-- collector and the checked delete refuse when any row lies outside the
-- opened directory ('validPathsUnder') rather than sweep a rooted
-- closure over a text mismatch.
--
-- == The lock
--
-- Upstream serialises collection against writers with a store-wide
-- @gc.lock@, held exclusively by the collector and shared by every
-- writer, plus a socket through which a writer registers roots with a
-- running collector.  nova-nix keeps the lock and drops the socket:
-- every open 'Store' handle holds the shared lock from 'openStore' to
-- 'closeStore' ("Nix.Store.Handle"), and 'collectGarbage' (and
-- 'withLiveSet', which the checked delete runs under) trades the
-- handle's shared lease for the exclusive lock for the duration.  A
-- collection therefore runs only while no other process has the store
-- open, and a process opening the store while one runs waits for it to
-- finish.  That is coarser than the per-path locks the builder and
-- substituter hold, and deliberately so: a per-path lock covers a path
-- while it is being produced, but not the window between a dependency's
-- registration and the build that consumes it, nor the writers that
-- register without one (the @.drv@ closure, the eval-time store
-- writes).  Under the exclusive lock no writer is active, so the
-- collector takes no per-path lock and creates no lock file.
-- 'findRoots' alone needs only the handle's shared lease: a listing
-- changes nothing another handle reads, and upstream's @--print-roots@
-- takes no collector lock either.
module Nix.Store.GC
  ( -- * Roots
    GcRoot (..),
    findRoots,
    addOutLinkRoot,
    gcRootsDir,
    autoRootsDir,
    gcRootsDirName,
    autoRootsDirName,
    indirectRootRecordName,

    -- * Reachability (pure)
    referencesMap,
    reachableFrom,

    -- * The live set
    LiveSet,
    withLiveSet,
    isLive,

    -- * Collection
    GcResults (..),
    collectGarbage,
    SweepPlan (..),
    sweepPlan,
    showFreedBytes,
    gcSummaryLine,

    -- * Messages
    findingRootsMessage,
    stillAliveMessage,
  )
where

import Control.Exception (IOException, catch, throwIO, try)
import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.List (isPrefixOf, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, isJust, listToMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.IO.Exception (IOErrorType (InappropriateType))
import Nix.Eval.CanonPath (canonPath)
import Nix.Hash (sha256Digest)
import Nix.Store.DB (UnregisterResult (..), isValidPath, metaDirName, queryAllReferences, queryAllValidPaths, unregisterPathRows)
import Nix.Store.Handle (Store (..), withCollectorLock)
import Nix.Store.Lock (lockedPathOf)
import Nix.Store.Path (StoreDir (..), StorePath, defaultStoreDir, parseStorePathBaseName, parseStorePathPrefix, platformStoreDir, storePathToFilePath)
import Nix.Store.Symlink (WalkNode (..), classifyWalkNode, createSymlinkOfKind)
import qualified NovaCache.Base32 as Base32
import System.Directory (createDirectoryIfMissing, getFileSize, getSymbolicLinkTarget, listDirectory, makeAbsolute, removePathForcibly, renamePath)
import System.FilePath (normalise, splitDirectories, takeDirectory, takeFileName, (</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Error (ioeGetErrorType, isDoesNotExistError, isPermissionError)
import Text.Printf (printf)

-- ---------------------------------------------------------------------------
-- Layout
-- ---------------------------------------------------------------------------

-- | Upstream's name for the roots directory.
gcRootsDirName :: FilePath
gcRootsDirName = "gcroots"

-- | Upstream's name for the indirect-root records under 'gcRootsDirName'.
autoRootsDirName :: FilePath
autoRootsDirName = "auto"

-- | The roots directory of a store, under its metadata directory.
gcRootsDir :: StoreDir -> FilePath
gcRootsDir dir = unStoreDir dir </> metaDirName </> gcRootsDirName

-- | The indirect-root records directory of a store.
autoRootsDir :: StoreDir -> FilePath
autoRootsDir dir = gcRootsDir dir </> autoRootsDirName

-- | The record name for an out-link: nix-base32 of the SHA-256 of the
-- link's absolute spelling, so one out-link has one record however
-- often it is re-registered.  Upstream hashes with SHA-1; the digest is
-- an internal key and the store has no SHA-1 primitive to spend on it.
indirectRootRecordName :: FilePath -> FilePath
indirectRootRecordName link = T.unpack (Base32.encode (sha256Digest (TE.encodeUtf8 (T.pack link))))

-- | Where a record is written before it is renamed into @auto@: under
-- the metadata directory, which the roots walk never enters, so a
-- listing under the shared lease sees a record whole or not at all.
recordStagingPath :: StoreDir -> FilePath -> FilePath
recordStagingPath dir link = unStoreDir dir </> metaDirName </> indirectRootRecordName link <> stagingSuffix

-- | The suffix of a record still being written.
stagingSuffix :: FilePath
stagingSuffix = ".tmp"

-- ---------------------------------------------------------------------------
-- Messages
-- ---------------------------------------------------------------------------

-- | Upstream's first progress line of a collection or a checked delete.
findingRootsMessage :: String
findingRootsMessage = "finding garbage collector roots..."

-- | Upstream's progress line before the sweep.
deletingGarbageMessage :: String
deletingGarbageMessage = "deleting garbage..."

-- | Upstream's per-path line of the sweep.
deletingPathMessage :: Text -> String
deletingPathMessage path = "deleting '" <> T.unpack path <> "'"

-- | Upstream's line for an indirect-root record whose out-link is gone.
removingStaleLinkMessage :: FilePath -> FilePath -> String
removingStaleLinkMessage record outLink =
  "removing stale link from '" <> record <> "' to '" <> outLink <> "'"

-- | Upstream's line for a symlink root naming a path the database does
-- not hold, with the target as the link spells it.
skippingInvalidRootMessage :: FilePath -> FilePath -> String
skippingInvalidRootMessage link target =
  "skipping invalid root from '" <> link <> "' to '" <> target <> "'"

-- | Upstream's line for a roots-directory entry it cannot read.
unreadableRootMessage :: FilePath -> String
unreadableRootMessage path = "cannot read potential root '" <> path <> "'"

-- | Upstream's refusal to delete a path a root keeps alive.  Upstream's
-- hint names @nix-store --query --roots@ and @--query --referrers@;
-- this tool has @store gc --print-roots@ for the first and lists
-- referrers in its own refusal for the second, so the hint names what
-- exists.
stillAliveMessage :: Text -> Text
stillAliveMessage path =
  "Cannot delete path '"
    <> path
    <> "' since it is still alive. To find out why, use: nova-nix store gc --print-roots"

-- | Upstream's refusal of an out-link inside the store, with this tool's
-- command in the parenthetical.
rootInsideStoreMessage :: FilePath -> Text
rootInsideStoreMessage link =
  "creating a garbage collector root ("
    <> T.pack link
    <> ") in the Nix store is forbidden (are you running nova-nix build inside the store?)"

-- | Upstream's refusal to clobber something that is not a store link.
alreadyExistsMessage :: FilePath -> Text
alreadyExistsMessage link = "cannot create symlink '" <> T.pack link <> "'; already exists"

-- | The refusal of a collection or a checked delete over a database
-- keyed under another spelling of the store directory.
storeSpellingMismatchMessage :: FilePath -> Text -> Text
storeSpellingMismatchMessage dir row =
  "the store was opened as '"
    <> T.pack dir
    <> "' but its database holds '"
    <> row
    <> "': the paths were registered under another spelling of the directory, and a collection would sweep every such path as dead"

-- ---------------------------------------------------------------------------
-- Roots
-- ---------------------------------------------------------------------------

-- | One root: the link (or record) that names it, for reporting, and the
-- store path it keeps, spelled as the database spells paths.  Ordered
-- by link, then path: the order @--print-roots@ lists them in.
data GcRoot = GcRoot
  { grLink :: !FilePath,
    grPath :: !Text
  }
  deriving (Eq, Ord, Show)

-- | A root candidate before the validity check: where it was found, the
-- parsed path, and the target text to report if the database does not
-- hold the path.  Upstream reports a symlink's invalid target with the
-- target as written and drops a regular file's silently, so only the
-- symlink forms carry a report.
data RootCandidate = RootCandidate
  { rcLink :: !FilePath,
    rcPath :: !StorePath,
    rcInvalidReport :: !(Maybe FilePath)
  }

-- | Every root the store has, valid paths only, each link once and
-- sorted as @--print-roots@ prints them.  A symlink root naming a path
-- the database does not hold is reported and dropped, a regular-file
-- root naming one is dropped silently, both as upstream does.  Stale
-- indirect-root records are removed on the way, and an entry that
-- cannot be read is reported and skipped.  The handle's shared lease is
-- enough: a listing changes nothing another handle reads, and a record
-- is published by rename, so a concurrent 'addOutLinkRoot' is seen
-- whole or not at all.  The collector runs this under the exclusive
-- lock as the first step of a collection.
findRoots :: Store -> IO [GcRoot]
findRoots store = do
  candidates <- walkRoots (stDir store) (gcRootsDir (stDir store))
  Set.toAscList . Set.fromList . catMaybes <$> mapM validOnly candidates
  where
    validOnly candidate = do
      valid <- isValidPath (stDB store) (rcPath candidate)
      if valid
        then pure (Just (GcRoot (rcLink candidate) (T.pack (storePathToFilePath (stDir store) (rcPath candidate)))))
        else do
          mapM_ (hPutStrLn stderr . skippingInvalidRootMessage (rcLink candidate)) (rcInvalidReport candidate)
          pure Nothing

-- | Walk the roots directory.  A directory recurses; a symlink into the
-- store is a permanent root, and any other symlink is followed one hop
-- as upstream follows it; a regular file is an indirect-root record
-- under @auto@ and a basename-named permanent root elsewhere.  An entry
-- that cannot be read (permission denied, vanished, not a directory
-- after all) is reported with upstream's line and contributes nothing;
-- any other failure propagates, as upstream's does.
walkRoots :: StoreDir -> FilePath -> IO [RootCandidate]
walkRoots storeDir path = walk `catch` unreadable
  where
    walk = do
      node <- classifyWalkNode path
      case node of
        WalkDirectory -> do
          entries <- listDirectory path
          concat <$> mapM (walkRoots storeDir . (path </>)) (sort entries)
        WalkSymlink -> do
          target <- getSymbolicLinkTarget path
          case parseStoreTarget storeDir target of
            Just sp -> pure [RootCandidate path sp (Just target)]
            Nothing -> followIndirect storeDir path (takeDirectory path </> target)
        WalkRegular
          | isUnderDir (autoRootsDir storeDir) path -> do
              recorded <- BS.readFile path
              followIndirect storeDir path (T.unpack (TE.decodeUtf8Lenient recorded))
          | otherwise ->
              pure [RootCandidate path sp Nothing | Just sp <- [parseStorePathBaseName (T.pack (takeFileName path))]]
        WalkAbsent -> pure []
    unreadable :: IOException -> IO [RootCandidate]
    unreadable e
      | isPermissionError e || isDoesNotExistError e || ioeGetErrorType e == InappropriateType = do
          hPutStrLn stderr (unreadableRootMessage path)
          pure []
      | otherwise = throwIO e

-- | The second hop of an indirect root: the out-link the record names.
-- A symlink into the store is the root (reported under the out-link's
-- own path, which is what the user created); an absent out-link makes
-- a record under @auto@ stale, and it is removed; anything else is not
-- a root.
followIndirect :: StoreDir -> FilePath -> FilePath -> IO [RootCandidate]
followIndirect storeDir record outLink = do
  node <- classifyWalkNode outLink
  case node of
    WalkSymlink -> do
      target <- getSymbolicLinkTarget outLink
      pure [RootCandidate outLink sp (Just target) | Just sp <- [parseStoreTarget storeDir target]]
    WalkAbsent
      | isUnderDir (autoRootsDir storeDir) record -> do
          hPutStrLn stderr (removingStaleLinkMessage record outLink)
          removePathForcibly record
          pure []
    _ -> pure []

-- | The store path a link target names: upstream's @toStorePath@ after
-- its @isInStore@, so a target that is a store path, a trailing-separator
-- spelling of one, or anything beneath one names that path.  Tried under
-- the opened store, the platform store and the canonical store: the
-- three spellings a link written by this tool or by hand can carry.
parseStoreTarget :: StoreDir -> FilePath -> Maybe StorePath
parseStoreTarget storeDir target =
  listToMaybe (mapMaybe (`parseStorePathPrefix` targetText) [storeDir, platformStoreDir, defaultStoreDir])
  where
    targetText = T.pack target

-- | Whether a path is the directory or lies beneath it, by normalised
-- components: upstream's @isInDir@, without the separator games.
isUnderDir :: FilePath -> FilePath -> Bool
isUnderDir dir path = splitDirectories (normalise dir) `isPrefixOf` splitDirectories (normalise path)

-- | Register an out-link as an indirect root: upstream's @addPermRoot@.
-- The link's spelling is made absolute and collapsed without resolving
-- symlinks (upstream's @canonPath@), so a @..@ cannot spell a link into
-- the store past the refusal, and the store directory it is compared
-- with is the handle's canonical one.  A link inside the store is
-- refused, as is clobbering anything that is not already a symlink into
-- the store; an existing store link is replaced.  The link's parent
-- directories are created, as upstream's @makeSymlink@ creates them.
-- The link is created first and the record second, so a failed link
-- (Windows without Developer Mode) leaves no record naming it; the
-- record is written under the metadata directory and renamed into
-- @auto@, so a listing under the shared lease never reads a torn one,
-- and a record already holding the link is left alone, since its
-- content is a function of its name.  Upstream replaces the link
-- atomically through a temporary name and a rename; a rename cannot
-- replace a directory symlink on Windows, so the old link is removed
-- and the new one created, one code path for every platform, and the
-- link is briefly absent rather than briefly two things.
addOutLinkRoot :: Store -> FilePath -> StorePath -> IO (Either Text FilePath)
addOutLinkRoot store rawLink sp = do
  link <- T.unpack . canonPath . T.pack <$> makeAbsolute rawLink
  if isUnderDir (unStoreDir (stDir store)) link
    then pure (Left (rootInsideStoreMessage link))
    else do
      node <- classifyWalkNode link
      replaceable <- case node of
        WalkAbsent -> pure True
        WalkSymlink -> isJust . parseStoreTarget (stDir store) <$> getSymbolicLinkTarget link
        _ -> pure False
      if not replaceable
        then pure (Left (alreadyExistsMessage link))
        else do
          when (node == WalkSymlink) (removePathForcibly link)
          createDirectoryIfMissing True (takeDirectory link)
          created <- createSymlinkOfKind link (storePathToFilePath (stDir store) sp)
          case created of
            Left err -> pure (Left err)
            Right () -> do
              publishRecord (stDir store) link
              pure (Right link)

-- | Write an out-link's record into @auto@ by rename, unless a record
-- with that content is already there.
publishRecord :: StoreDir -> FilePath -> IO ()
publishRecord dir link = do
  let record = autoRootsDir dir </> indirectRootRecordName link
      staging = recordStagingPath dir link
      content = TE.encodeUtf8 (T.pack link)
  createDirectoryIfMissing True (autoRootsDir dir)
  existing <- try (BS.readFile record) :: IO (Either IOException BS.ByteString)
  when (existing /= Right content) $ do
    BS.writeFile staging content
    renamePath staging record

-- ---------------------------------------------------------------------------
-- Reachability
-- ---------------------------------------------------------------------------

-- | The references map from the edge list 'queryAllReferences' returns.
referencesMap :: [(Text, Text)] -> Map Text [Text]
referencesMap edges = Map.fromListWith (flip (++)) [(referrer, [reference]) | (referrer, reference) <- edges]

-- | Every path reachable from the roots over the references map: the
-- roots themselves and, transitively, everything they reference.  A
-- root or a reference the map does not know is still reachable (it is
-- named), just a dead end; cycles terminate on the visited set.
reachableFrom :: Map Text [Text] -> [Text] -> Set Text
reachableFrom refs = go Set.empty
  where
    go !seen [] = seen
    go !seen (path : pending)
      | Set.member path seen = go seen pending
      | otherwise = go (Set.insert path seen) (Map.findWithDefault [] path refs ++ pending)

-- ---------------------------------------------------------------------------
-- The live set
-- ---------------------------------------------------------------------------

-- | The live set at one moment under the exclusive collector lock: the
-- roots' closure over the references map.  Only 'withLiveSet' makes
-- one, so a checked delete holding it holds the lock and a set the
-- delete can act on.
newtype LiveSet = LiveSet (Set Text)

-- | Whether the live set holds a path, spelled as the database spells it.
isLive :: LiveSet -> Text -> Bool
isLive (LiveSet live) path = Set.member path live

-- | Run a checked operation under the exclusive collector lock with the
-- live set as it stands, after upstream's roots line.  Refused, with
-- the action not run, when the database holds a path outside the opened
-- directory ('validPathsUnder').
withLiveSet :: Store -> (LiveSet -> IO a) -> IO (Either Text a)
withLiveSet store act = withCollectorLock store $ do
  checked <- validPathsUnder store
  case checked of
    Left err -> pure (Left err)
    Right _ -> do
      hPutStrLn stderr findingRootsMessage
      live <- liveSetOf store
      Right <$> act live

-- | The roots' closure as it stands; the caller holds the collector lock.
liveSetOf :: Store -> IO LiveSet
liveSetOf store = do
  roots <- findRoots store
  refs <- referencesMap <$> queryAllReferences (stDB store)
  pure (LiveSet (reachableFrom refs (map grPath roots)))

-- | Every valid path as the database spells it, refused when one lies
-- outside the opened directory: the collector and the checked delete
-- compare row text with paths derived from the handle's directory, and
-- a row registered under another spelling of the directory would
-- compare as dead and be swept with its tree.
-- 'Nix.Store.Handle.openStore' canonicalises the spelling, so this fires
-- only for a store whose rows and directory genuinely disagree.
validPathsUnder :: Store -> IO (Either Text (Set Text))
validPathsUnder store = do
  valid <- queryAllValidPaths (stDB store)
  pure $ case filter (not . isUnderDir dir . T.unpack) valid of
    [] -> Right (Set.fromList valid)
    (stray : _) -> Left (storeSpellingMismatchMessage dir stray)
  where
    dir = unStoreDir (stDir store)

-- ---------------------------------------------------------------------------
-- Collection
-- ---------------------------------------------------------------------------

-- | What a collection removed: every store directory entry deleted,
-- which is what upstream announces and counts, and the bytes those
-- trees held.  A dead row with no tree is unregistered without either.
data GcResults = GcResults
  { gcrDeleted :: ![Text],
    gcrBytesFreed :: !Integer
  }
  deriving (Eq, Show)

-- | What a sweep removes, each list sorted.
data SweepPlan = SweepPlan
  { -- | Every valid path outside the live set, to unregister in one
    -- transaction whether or not its tree exists.
    spDeadRows :: ![Text],
    -- | Every store directory entry to delete: a dead path's tree, a
    -- store-shaped tree with no row (a build interrupted before
    -- registration), junk, and the lock file of a path that is not
    -- live.  The metadata directory, every live path and a live path's
    -- lock file stay.
    spTrees :: ![Text]
  }
  deriving (Eq, Show)

-- | The sweep plan over the valid set, the live set and the store
-- directory's entries.  The lock-file test is the one
-- 'Nix.Store.deleteStorePathRaw' makes: a lock-shaped name with no row
-- is a lock file, since only the rows can tell one from a store object
-- that happens to be called @flake.lock@, so a dead row's tree goes
-- whatever its name, and an unregistered lock-shaped entry stays only
-- while the path it guards is live.
sweepPlan :: StoreDir -> Set Text -> Set Text -> [Text] -> SweepPlan
sweepPlan storeDir valid live entries =
  SweepPlan
    { spDeadRows = Set.toAscList (Set.difference valid live),
      spTrees = Set.toAscList (Set.fromList [fullPath entry | entry <- entries, swept entry])
    }
  where
    fullPath entry = T.pack (unStoreDir storeDir </> T.unpack entry)
    swept entry =
      entry /= T.pack metaDirName
        && not (Set.member (fullPath entry) live)
        && (Set.member (fullPath entry) valid || not (guardsLivePath entry))
    guardsLivePath entry = maybe False (\guarded -> Set.member (fullPath guarded) live) (lockedPathOf entry)

-- | Collect garbage: find the roots, walk the references, remove every
-- dead row in one transaction, then every dead or unregistered tree.
-- Holds the collector lock throughout, and is refused over a database
-- keyed under another spelling of the directory ('validPathsUnder').
-- The progress lines are upstream's, on stderr; the summary is the
-- caller's to print ('gcSummaryLine').
collectGarbage :: Store -> IO (Either Text GcResults)
collectGarbage store = withCollectorLock store $ do
  checked <- validPathsUnder store
  case checked of
    Left err -> pure (Left err)
    Right valid -> Right <$> sweep valid
  where
    sweep valid = do
      hPutStrLn stderr findingRootsMessage
      LiveSet live <- liveSetOf store
      entries <- map T.pack <$> listDirectory (unStoreDir (stDir store))
      let plan = sweepPlan (stDir store) valid live entries
      hPutStrLn stderr deletingGarbageMessage
      unregistered <- unregisterPathRows (stDB store) (spDeadRows plan)
      case unregistered of
        RowReferenced referrers ->
          -- Unreachable under the exclusive lock: a referrer outside the
          -- dead set would have made its reference live.  Loud, not silent.
          throwIO (userError ("collectGarbage: live paths reference dead ones: " <> show referrers))
        _ -> pure ()
      freed <- foldlIO 0 (spTrees plan) deleteTree
      pure GcResults {gcrDeleted = spTrees plan, gcrBytesFreed = freed}
    deleteTree !acc path = do
      hPutStrLn stderr (deletingPathMessage path)
      let target = T.unpack path
      node <- classifyWalkNode target
      case node of
        WalkAbsent -> pure acc
        _ -> do
          bytes <- treeBytes target
          removePathForcibly target
          pure (acc + bytes)

-- | The bytes a tree holds, links as leaves: a regular file's size, a
-- symlink's target length (upstream counts a link's @st_size@, which is
-- that), nothing for a directory itself.  Upstream skips files with
-- three or more hard links as shared with its @.links@ optimisation;
-- this store has no such optimisation, so every file counts.
treeBytes :: FilePath -> IO Integer
treeBytes path = do
  node <- classifyWalkNode path
  case node of
    WalkRegular -> getFileSize path
    WalkSymlink -> fromIntegral . BS.length . TE.encodeUtf8 . T.pack <$> getSymbolicLinkTarget path
    WalkDirectory -> do
      entries <- listDirectory path
      foldlIO 0 entries (\acc entry -> (acc +) <$> treeBytes (path </> entry))
    WalkAbsent -> pure 0

-- | Strict left fold over a list in IO: the accumulator is forced each
-- step so a sweep over a large store builds no thunk chain.
foldlIO :: a -> [b] -> (a -> b -> IO a) -> IO a
foldlIO z [] _ = pure z
foldlIO z (x : xs) f = do
  !acc <- f z x
  foldlIO acc xs f

-- | Upstream's @showBytes@ at 2.24.9: mebibytes to two places.
showFreedBytes :: Integer -> Text
showFreedBytes bytes = T.pack (printf "%.2f MiB" (fromIntegral bytes / (1024 * 1024) :: Double))

-- | Upstream's summary line (@PrintFreed@), plural as it prints it
-- whatever the count.
gcSummaryLine :: GcResults -> Text
gcSummaryLine results =
  T.pack (show (length (gcrDeleted results)))
    <> " store paths deleted, "
    <> showFreedBytes (gcrBytesFreed results)
    <> " freed"
