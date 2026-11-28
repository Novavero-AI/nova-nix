{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- | A downloaded archive unpacked into a tree, as upstream's tarball
-- fetcher unpacks one: decompressed by what its first bytes announce,
-- read as libarchive reads tar ("Nix.Fetch.Tar"), and its entries placed
-- by the rules of upstream's tar sink (merkle-tar-adapter.cc at 2.36.0).
--
-- The rules are pure: an entry's path is read as upstream's @CanonPath@
-- reads it, the last entry at a path wins, a non-empty directory cannot be
-- replaced, and a hard link names what its target is at that point in the
-- archive.  The edge decompresses, drives the reader, and writes each
-- file's bytes to a blob in a scratch directory as they arrive, so no
-- file is held whole.
module Nix.Fetch.Archive
  ( -- * Failures
    FetchFailure (..),

    -- * The tar sink
    ArchiveEntry (..),
    ArchiveNode (..),
    ArchiveSink,
    emptySink,
    sinkEntry,
    UnpackedArchive (..),
    finishSink,

    -- * Unpacking
    readArchive,
    drainFile,
    unpackArchive,
  )
where

import Control.Monad.Except (ExceptT, runExceptT, throwError, withExceptT)
import Control.Monad.Writer.Strict (Writer, runWriter, tell)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64, Word8)
import Nix.Compression.Stream (compressionMagicBytes, compressionOf, decodingFailure, withDecompressed)
import Nix.Fetch.Tar (FileStep (..), TarEntry (..), TarFailure (..), TarNode (..), TarStep (..), tarReader)
import Nix.Fetch.Tree (Blob (..), Tree (..), dereferenceSingleton)
import Nix.Fetch.Url (bytesToTextLossy)
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), withBinaryFile)

-- ---------------------------------------------------------------------------
-- Failures
-- ---------------------------------------------------------------------------

-- | Why a fetch failed: upstream's message, and the lines of context
-- upstream gives it, outermost first.
data FetchFailure = FetchFailure
  { fetchTraces :: ![Text],
    fetchMessage :: !Text
  }
  deriving (Eq, Show)

-- | Placing entries, with the warnings it gives in order.
type Sinking = ExceptT FetchFailure (Writer [Text])

refuseWith :: Text -> Sinking a
refuseWith = throwError . FetchFailure []

-- ---------------------------------------------------------------------------
-- Paths
-- ---------------------------------------------------------------------------

-- | A path in an archive as upstream's @CanonPath@ holds it: its
-- components, with empty ones and @.@ dropped and @..@ taking the one
-- before it away.  The order is @CanonPath@'s, which sorts a separator
-- before every byte: component by component.
newtype ArchivePath = ArchivePath [ByteString]
  deriving (Eq, Ord, Show)

-- | An entry's path, read as @CanonPath@ reads one under the root.
archivePath :: ByteString -> ArchivePath
archivePath raw = ArchivePath (reverse (foldl' step [] (BS.split slash raw)))
  where
    step kept component
      | BS.null component || component == "." = kept
      | component == ".." = drop 1 kept
      | otherwise = component : kept

-- | A path as upstream prints one: absolute, the root as @\/@.
renderArchivePath :: ArchivePath -> Text
renderArchivePath (ArchivePath components) = "/" <> T.intercalate "/" (map bytesToTextLossy components)

slash :: Word8
slash = 0x2f

-- ---------------------------------------------------------------------------
-- The tar sink
-- ---------------------------------------------------------------------------

-- | An entry as the sink takes it: its path as the archive names it, its
-- modification time and what it is.
data ArchiveEntry blob = ArchiveEntry
  { entryPath :: !ByteString,
    entryMtime :: !Int64,
    entryNode :: !(ArchiveNode blob)
  }
  deriving (Eq, Show)

-- | What an entry is.
data ArchiveNode blob
  = ArchiveFile !Bool !blob
  | ArchiveDirectory
  | ArchiveSymlink !ByteString
  | ArchiveHardLink !ByteString
  | -- | A device or a FIFO, by its file type.
    ArchiveSpecial !Int
  deriving (Eq, Show)

-- | The tree so far, the paths hard links have made one file, and the
-- newest modification time.
data ArchiveSink blob = ArchiveSink
  { sinkRoot :: !(Tree blob),
    -- | The group each linked path is in.
    sinkGroupOf :: !(Map ArchivePath Int),
    -- | The paths of each group.
    sinkGroups :: !(Map Int (Set ArchivePath)),
    sinkNextGroup :: !Int,
    sinkLastModified :: !Int64
  }

-- | Nothing placed yet: an empty root directory.
emptySink :: ArchiveSink blob
emptySink = ArchiveSink (TreeDirectory Map.empty) Map.empty Map.empty 0 0

-- | Place an entry, as @unpackTarfileToSink@ hands one to the tar sink.
-- Every entry counts toward the newest modification time, whatever it is.
sinkEntry :: ArchiveSink blob -> ArchiveEntry blob -> (Either FetchFailure (ArchiveSink blob), [Text])
sinkEntry sink entry = runWriter (runExceptT (place timed))
  where
    timed = sink {sinkLastModified = max (sinkLastModified sink) (entryMtime entry)}
    path = archivePath (entryPath entry)
    place current = case entryNode entry of
      ArchiveFile executable blob -> addNode path (TreeFile executable blob) current
      ArchiveDirectory -> addNode path (TreeDirectory Map.empty) current
      ArchiveSymlink target -> addNode path (TreeSymlink target) current
      ArchiveHardLink target -> hardLink path (archivePath target) current
      ArchiveSpecial fileType ->
        refuseWith ("file '" <> bytesToTextLossy (entryPath entry) <> "' in tarball has unsupported file type " <> T.pack (show fileType))

-- | What placing a node did to what was at its path.
data Placed = Added | Replaced | Unchanged

-- | Put a node at a path, making its parents directories as needed: the
-- last entry wins, a directory onto a directory changes nothing, and a
-- non-empty directory cannot be replaced.  Replacing a path a hard link
-- made one file with others unlinks it from them, with upstream's
-- warning.
addNode :: ArchivePath -> Tree blob -> ArchiveSink blob -> Sinking (ArchiveSink blob)
addNode path@(ArchivePath components) child sink = do
  (root, placed) <- either refuseWith pure (placeAt components (sinkRoot sink))
  let updated = sink {sinkRoot = root}
  case placed of
    Replaced -> unlinked path updated
    Added -> pure updated
    Unchanged -> pure updated
  where
    shown = renderArchivePath path
    notDirectory = "parent of '" <> shown <> "' is not a directory"
    placeAt remaining current = case (remaining, current) of
      ([], _) -> overwrite current
      ([name], TreeDirectory entries) -> case Map.lookup name entries of
        Nothing -> Right (TreeDirectory (Map.insert name child entries), Added)
        Just existing -> (\(node, placed) -> (TreeDirectory (Map.insert name node entries), placed)) <$> overwrite existing
      (name : deeper, TreeDirectory entries) ->
        (\(node, placed) -> (TreeDirectory (Map.insert name node entries), placed))
          <$> placeAt deeper (Map.findWithDefault (TreeDirectory Map.empty) name entries)
      (_, _) -> Left notDirectory
    overwrite existing = case (existing, child) of
      (TreeDirectory _, TreeDirectory _) -> Right (existing, Unchanged)
      (TreeDirectory entries, _)
        | not (Map.null entries) -> Left ("cannot create '" <> shown <> "', conflicting non-empty directory")
      _ -> Right (child, Replaced)

-- | Unlink a replaced path from the paths a hard link made the same file,
-- warning as upstream does; a path left alone in its group is linked to
-- nothing any more.
unlinked :: ArchivePath -> ArchiveSink blob -> Sinking (ArchiveSink blob)
unlinked path sink = case Map.lookup path (sinkGroupOf sink) of
  Nothing -> pure sink
  Just group -> do
    let others = Set.delete path (Map.findWithDefault Set.empty group (sinkGroups sink))
        groupOf = Map.delete path (sinkGroupOf sink)
    tell
      [ renderQuoted path
          <> " is replaced, though a hard link makes it the same file as "
          <> T.intercalate ", " (map renderQuoted (Set.toAscList others))
          <> ". Older versions of Nix resolved hard links differently, so this archive unpacks to a different store path than it used to."
      ]
    pure $
      if Set.size others < 2
        then sink {sinkGroupOf = foldr Map.delete groupOf (Set.toList others), sinkGroups = Map.delete group (sinkGroups sink)}
        else sink {sinkGroupOf = groupOf, sinkGroups = Map.insert group others (sinkGroups sink)}
  where
    renderQuoted p = "'" <> renderArchivePath p <> "'"

-- | Make a path a hard link to what its target is now, a file or a
-- symlink, and the two one file from then on.
hardLink :: ArchivePath -> ArchivePath -> ArchiveSink blob -> Sinking (ArchiveSink blob)
hardLink path target sink =
  withExceptT traced $ do
    node <- case lookupPath target (sinkRoot sink) of
      Nothing -> refuseWith "target does not exist"
      Just (TreeDirectory _) -> refuseWith "target is a directory"
      Just found -> pure found
    placed <- addNode path node sink
    let (group, withTarget) = case Map.lookup target (sinkGroupOf placed) of
          Just existing -> (existing, placed)
          Nothing ->
            ( sinkNextGroup placed,
              placed
                { sinkGroupOf = Map.insert target (sinkNextGroup placed) (sinkGroupOf placed),
                  sinkGroups = Map.insert (sinkNextGroup placed) (Set.singleton target) (sinkGroups placed),
                  sinkNextGroup = sinkNextGroup placed + 1
                }
            )
    pure
      withTarget
        { sinkGroupOf = Map.insert path group (sinkGroupOf withTarget),
          sinkGroups = Map.adjust (Set.insert path) group (sinkGroups withTarget)
        }
  where
    traced (FetchFailure traces message) =
      FetchFailure (("while creating a hard link from '" <> renderArchivePath path <> "' to '" <> renderArchivePath target <> "'") : traces) message

-- | The node at a path, if there is one: none when something on the way
-- is not a directory.
lookupPath :: ArchivePath -> Tree blob -> Maybe (Tree blob)
lookupPath (ArchivePath components) = go components
  where
    go remaining current = case (remaining, current) of
      ([], _) -> Just current
      (name : deeper, TreeDirectory entries) -> Map.lookup name entries >>= go deeper
      (_, _) -> Nothing

-- | An archive unpacked: its tree, the lone top-level directory taken as
-- the root when there is one, and the newest modification time of any
-- entry, or zero.
data UnpackedArchive blob = UnpackedArchive
  { unpackedTree :: !(Tree blob),
    unpackedLastModified :: !Int64
  }
  deriving (Eq, Show)

-- | The archive the sink holds.  A root an entry replaced with something
-- other than a directory is refused: upstream's tarball cache reads the
-- root as a Git tree, and fails on any other object.
finishSink :: ArchiveSink blob -> Either FetchFailure (UnpackedArchive blob)
finishSink sink = case sinkRoot sink of
  root@(TreeDirectory _) -> Right (UnpackedArchive (dereferenceSingleton root) (sinkLastModified sink))
  _ -> Left (FetchFailure [] "the archive's root is not a directory")

-- ---------------------------------------------------------------------------
-- Unpacking
-- ---------------------------------------------------------------------------

-- | The first bytes of a zip archive's local file header, which
-- libarchive reads and this does not yet.
zipMagic :: ByteString
zipMagic = "PK\x03\x04"

-- | Unpack the archive a source gives, writing each file's bytes to a
-- blob under the scratch directory, numbered in archive order, as they
-- arrive, and give its tree with the warnings unpacking gave, in order.
-- A corrupt compressed stream, an archive libarchive would not read and
-- an entry the sink refuses are failures; anything else the source
-- throws, a failed download among them, is left to the caller.
unpackArchive :: FilePath -> IO ByteString -> IO ([Text], Either FetchFailure (UnpackedArchive Blob))
unpackArchive scratch source = do
  (lead, replayed) <- peek compressionMagicBytes source
  if zipMagic `BS.isPrefixOf` lead
    then pure ([], Left (FetchFailure [] "zip archives are not supported"))
    else withDecompressed (compressionOf lead) replayed $ \decompressed ->
      let pull = decodingFailure decompressed
          store count size contents = do
            let blob = Blob (scratch </> show count) size
            fmap (blob,) <$> withBinaryFile (blobFile blob) WriteMode (\handle -> drainFile pull (BS.hPut handle) contents)
       in readArchive pull store

-- | Read an archive's entries into the sink, as @unpackTarfileToSink@
-- hands them to upstream's: chunks come from the pull, which gives a
-- decoder's refusal as a value, and each regular file's contents go to
-- the blob the store makes of them, given the file's number in archive
-- order and its size.  The warnings come in the order the archive gives
-- them, a member's own before what the sink says of it.
readArchive ::
  (Monad m) =>
  m (Either Text ByteString) ->
  (Int -> Word64 -> FileStep -> m (Either FetchFailure (blob, TarStep))) ->
  m ([Text], Either FetchFailure (UnpackedArchive blob))
readArchive pull store = drive [] emptySink 0 tarReader
  where
    -- Warnings are held newest first.
    drive warnings sink count step = case step of
      TarAwait continue -> pull >>= either (stopped warnings . unreadable) (drive warnings sink count . continue)
      TarDone -> pure (reverse warnings, finishSink sink)
      TarFail failure -> stopped warnings (tarFailure failure)
      TarOther entry node next -> placing warnings sink count entry (otherNode node) next
      TarFile entry size contents ->
        store count size contents
          >>= either (stopped warnings) (\(blob, next) -> placing warnings sink (count + 1) entry (ArchiveFile (tarExecutable entry) blob) next)
    placing warnings sink count entry node next =
      let (placed, sinkWarnings) = sinkEntry sink (ArchiveEntry (tarPath entry) (tarMtime entry) node)
          given = reverse sinkWarnings ++ maybe [] (pure . memberWarning entry) (tarWarning entry) ++ warnings
       in either (stopped given) (\updated -> drive given updated count next) placed
    stopped warnings failure = pure (reverse warnings, Left failure)
    memberWarning entry reason = "getting archive member '" <> bytesToTextLossy (tarPath entry) <> "': " <> reason
    otherNode node = case node of
      TarDirectory -> ArchiveDirectory
      TarSymlink target -> ArchiveSymlink target
      TarHardLink target -> ArchiveHardLink target
      TarSpecial fileType -> ArchiveSpecial fileType

-- | Hand a file's contents to the sink as they arrive, and give the
-- archive after them.
drainFile :: (Monad m) => m (Either Text ByteString) -> (ByteString -> m ()) -> FileStep -> m (Either FetchFailure TarStep)
drainFile pull sink step = case step of
  FileAwait continue -> pull >>= either (pure . Left . unreadable) (drainFile pull sink . continue)
  FileChunk bytes rest -> sink bytes >> drainFile pull sink rest
  FileEnd next -> pure (Right next)
  FileFail failure -> pure (Left (tarFailure failure))

-- | A compressed stream the decoder refused, as a fetch failure.
unreadable :: Text -> FetchFailure
unreadable reason = FetchFailure [] ("cannot read the archive: " <> reason)

-- | A reader's failure as a fetch failure.
tarFailure :: TarFailure -> FetchFailure
tarFailure = FetchFailure [] . tarFailureMessage

-- | A reader's failure as upstream reports it: a header that failed before
-- the entry had a name as a member it cannot name, one after as an
-- archive it failed to extract, and a body as a file it cannot read.
tarFailureMessage :: TarFailure -> Text
tarFailureMessage failure = case failure of
  TarHeaderFailure Nothing reason -> "cannot get archive member name: " <> reason
  TarHeaderFailure (Just _) reason -> "failed to extract archive (" <> reason <> ")"
  TarDataFailure reason -> "cannot read file from tarball: " <> reason

-- | Read at least @n@ bytes from a source, or all it has when it has
-- fewer, and give them with a source that gives them again first.
peek :: Int -> IO ByteString -> IO (ByteString, IO ByteString)
peek n source = do
  lead <- gather []
  pending <- newIORef (Just lead)
  let replayed =
        readIORef pending >>= \case
          Just bytes -> writeIORef pending Nothing >> if BS.null bytes then source else pure bytes
          Nothing -> source
  pure (lead, replayed)
  where
    gather held = do
      let have = BS.concat (reverse held)
      if BS.length have >= n
        then pure have
        else do
          chunk <- source
          if BS.null chunk then pure have else gather (chunk : held)
