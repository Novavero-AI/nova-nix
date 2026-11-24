{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}

-- | The tree a user environment is: every package's files linked into one
-- directory, as upstream's @buildProfile@ makes it
-- (builtins\/buildenv.cc), the output of
-- @builtin:buildenv@ ("Nix.Builder.Buildenv").
--
-- Packages are taken in order of priority, where a lower number wins,
-- then of path, each path once.  Each package's
-- @nix-support\/propagated-user-env-packages@ queues the packages it
-- names, which are taken after every package given, at priority 1000 and
-- up, one number per path in path order.  In each directory an entry is
-- linked in where nothing stands.  A directory where a link to a directory
-- stands turns the link into a directory of the tree's own, holding what
-- the link led to at the link's priority and then the new entries at
-- theirs.  A file where a link stands replaces it when its priority number
-- is lower, leaves it when higher, and conflicts with it when equal.  A
-- name starting with @.@, an entry whose link leads nowhere, and the files
-- upstream keeps out of profiles are skipped.
--
-- Reading the packages is the only effect, and it reaches the core
-- through a t'TreeReader', so the core runs over an in-memory store in
-- tests.  The tree comes back as a value: 'storeTreeReader' reads the
-- packages in a store on disk, and 'writeTree' makes the tree there.
--
-- Upstream lists a directory in @readdir@ order and this lists it sorted.
-- The tree does not depend on the order, but when several entries
-- conflict, which pair the failure names can.
module Nix.Profile.Tree
  ( -- * Packages
    Package (..),

    -- * Reading the packages
    TreeReader (..),
    Listing (..),
    EntryKind (..),
    ReadFailure (..),
    storeTreeReader,
    storePackagePath,
    propagatedTokens,

    -- * The tree
    Tree,
    Node (..),
    Built (..),
    Warning (..),
    BuildEnvFailure (..),
    buildProfile,
    warningText,
    buildEnvFailureText,

    -- * Writing the tree
    writeTree,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (try)
import Control.Monad (foldM)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Control.Monad.State.Strict (StateT, modify', runStateT)
import Control.Monad.Trans (lift)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Foldable (traverse_)
import Data.List (sort, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Foreign.C.Error (Errno (..), eNOTDIR)
import GHC.IO.Exception (IOException (ioe_description, ioe_errno))
import Nix.HostPath (hostPathText)
import Nix.Store.Path (StoreDir, defaultStoreDir, parseStorePath, storePathToFilePath)
import Nix.Store.Symlink (createSymlinkOfKind)
import qualified System.Directory.OsPath as OsDir
import System.IO.Error (isDoesNotExistError)
import System.OsPath (OsPath, (</>))
import qualified System.OsPath as OP
#if defined(mingw32_HOST_OS)
import Control.Exception (bracket)
import Data.Bits ((.&.), (.|.))
import Nix.Store.Win32Path (win32FilePath)
import System.Win32.File
  ( BY_HANDLE_FILE_INFORMATION (..),
    closeHandle,
    createFile,
    fILE_ATTRIBUTE_DIRECTORY,
    fILE_FLAG_BACKUP_SEMANTICS,
    fILE_SHARE_DELETE,
    fILE_SHARE_READ,
    fILE_SHARE_WRITE,
    getFileInformationByHandle,
    oPEN_EXISTING,
  )
#else
import System.Posix.Files.ByteString (getFileStatus, isDirectory)
#endif

-- ---------------------------------------------------------------------------
-- Packages
-- ---------------------------------------------------------------------------

-- | A package to link into the tree.
data Package = Package
  { -- | Where the package is, as the t'TreeReader' reads it and as the
    -- tree's links name what is inside it.
    pkgPath :: !OsPath,
    -- | Its priority: of two files at one path, the one with the lower
    -- number stays.
    pkgPriority :: !Int
  }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Reading the packages
-- ---------------------------------------------------------------------------

-- | How the core reads the packages.  Each call follows links, as the
-- @stat@ and @opendir@ upstream calls do, and fails where upstream throws.
data TreeReader m = TreeReader
  { -- | The names in a directory, or 'NotADirectory' when what is at the
    -- path is not one.
    trEntries :: OsPath -> m (Either ReadFailure Listing),
    -- | What is at a path, or 'Nothing' when nothing is: upstream's
    -- @maybeStat@, which reads @ENOENT@ and @ENOTDIR@ as absence and
    -- fails on anything else.
    trKind :: OsPath -> m (Either ReadFailure (Maybe EntryKind)),
    -- | The path with every link on it resolved: upstream's
    -- @canonPath(path, true)@.
    trResolve :: OsPath -> m (Either ReadFailure OsPath),
    -- | The packages a package's @nix-support\/propagated-user-env-packages@
    -- names, in its order, or none when it has no such file.
    trPropagated :: OsPath -> m (Either ReadFailure [OsPath])
  }

-- | What listing a directory finds.
data Listing
  = -- | The directory's names, in any order.
    Entries ![OsPath]
  | -- | Something other than a directory is at the path.
    NotADirectory
  deriving (Eq, Show)

-- | What a path holds once its links are followed: upstream tells a
-- directory from everything else.
data EntryKind = DirectoryEntry | NonDirectoryEntry
  deriving (Eq, Show)

-- | Why a read failed, in words a build failure prints.
newtype ReadFailure = ReadFailure Text
  deriving (Eq, Show)

-- | A t'TreeReader' for packages in a store on disk, which names a
-- propagated package as 'storePackagePath' does.
storeTreeReader :: StoreDir -> TreeReader IO
storeTreeReader storeDir =
  TreeReader
    { trEntries = listEntries,
      trKind = entryKind,
      trResolve = \path -> first (readFailure "resolving the links in" path) <$> tryIO (OsDir.canonicalizePath path),
      trPropagated = propagatedPackages storeDir
    }

-- | The path a package's text names on disk: a store path, in the
-- canonical @\/nix\/store@ spelling a derivation holds or in the store's
-- own, which a builder writes, rendered through the store directory.
-- 'Nothing' for anything else.  Store paths are ASCII, so bytes that are
-- not UTF-8 name none.
storePackagePath :: StoreDir -> ByteString -> IO (Maybe OsPath)
storePackagePath storeDir bytes = case TE.decodeUtf8' bytes of
  Right text
    | Just sp <- parseStorePath defaultStoreDir text <|> parseStorePath storeDir text ->
        Just <$> OP.encodeFS (storePathToFilePath storeDir sp)
  _ -> pure Nothing

-- | The tokens of a @propagated-user-env-packages@ file: what lies between
-- spaces and newlines, with the empty ones dropped, as upstream's
-- @tokenizeString(contents, " \\n")@ gives them (@buildProfile@ in
-- buildenv.cc).
propagatedTokens :: ByteString -> [ByteString]
propagatedTokens = filter (not . BS.null) . BS8.splitWith (\c -> c == ' ' || c == '\n')

-- | List a directory.  A listing that fails where something other than a
-- directory is at the path is upstream's @ENOTDIR@, which it warns of
-- rather than fails on.
listEntries :: OsPath -> IO (Either ReadFailure Listing)
listEntries dir = do
  listed <- tryIO (OsDir.listDirectory dir)
  case listed of
    Right names -> pure (Right (Entries names))
    Left failure -> do
      kind <- entryKind dir
      pure $ case kind of
        Right (Just NonDirectoryEntry) -> Right NotADirectory
        _ -> Left (readFailure "cannot read directory" dir failure)

-- | What is at a path, following links.
entryKind :: OsPath -> IO (Either ReadFailure (Maybe EntryKind))
entryKind path = do
  probed <- tryIO (followedKind path)
  pure $ case probed of
    Right kind -> Right (Just kind)
    Left failure
      | absent failure -> Right Nothing
      | otherwise -> Left (readFailure "getting status of" path failure)

-- | 'trPropagated' for a package in the store: its file read, split into
-- 'propagatedTokens', and each token named through 'storePackagePath'.
propagatedPackages :: StoreDir -> OsPath -> IO (Either ReadFailure [OsPath])
propagatedPackages storeDir pkg = do
  -- The path is a store path's, which came from a 'FilePath', and the
  -- fixed names below it, so spelling it as one again is exact.
  contents <- tryIO (OP.decodeFS file >>= BS.readFile)
  case contents of
    Left failure
      | absent failure -> pure (Right [])
      | otherwise -> pure (Left (readFailure "reading file" file failure))
    Right bytes -> sequenceA <$> traverse named (propagatedTokens bytes)
  where
    file = pkg </> asciiPath nixSupportName </> asciiPath propagatedPackagesName
    named token = maybe (Left (notAStorePath token)) Right <$> storePackagePath storeDir token
    notAStorePath token =
      ReadFailure (quoted file <> " names '" <> TE.decodeUtf8Lenient token <> "', which is not a store path")

-- | Whether a failure says nothing is at the path: @ENOENT@ or @ENOTDIR@,
-- which upstream's @maybeStat@ and its read of the propagated packages
-- take as absence.  On Windows a failure carries the C runtime's error
-- number for the Win32 one, so the same test holds.
absent :: IOException -> Bool
absent failure = isDoesNotExistError failure || fmap Errno (ioe_errno failure) == Just eNOTDIR

-- | A failed read in upstream's form: what was being done, the path, and
-- the system's reason.
readFailure :: Text -> OsPath -> IOException -> ReadFailure
readFailure doing path failure =
  ReadFailure (doing <> " " <> quoted path <> ": " <> T.pack (ioe_description failure))

-- | 'try' for the one exception file system calls throw.
tryIO :: IO a -> IO (Either IOException a)
tryIO = try

-- | What is at a path once its links are followed, failing as @stat@
-- does.
followedKind :: OsPath -> IO EntryKind
#if defined(mingw32_HOST_OS)
-- @directory@'s own probe: opening the path without
-- @FILE_FLAG_OPEN_REPARSE_POINT@ follows a link to what it names, and
-- backup semantics let the open reach a directory.
followedKind path = do
  target <- OP.decodeFS path >>= win32FilePath
  info <-
    bracket
      (createFile target 0 (fILE_SHARE_READ .|. fILE_SHARE_WRITE .|. fILE_SHARE_DELETE) Nothing oPEN_EXISTING fILE_FLAG_BACKUP_SEMANTICS Nothing)
      closeHandle
      getFileInformationByHandle
  pure $
    if bhfiFileAttributes info .&. fILE_ATTRIBUTE_DIRECTORY /= 0
      then DirectoryEntry
      else NonDirectoryEntry
#else
followedKind path = do
  status <- getFileStatus (BS8.pack (map OP.toChar (OP.unpack path)))
  pure (if isDirectory status then DirectoryEntry else NonDirectoryEntry)
#endif

-- ---------------------------------------------------------------------------
-- The tree
-- ---------------------------------------------------------------------------

-- | A directory of the tree: each entry by its name.
type Tree = Map OsPath Node

-- | One entry of the tree.
data Node
  = -- | A link to a path, with the priority it was linked at.
    Link !OsPath !Int
  | -- | A directory of the tree's own, made where entries from more than
    -- one place meet.
    Directory !Tree
  deriving (Eq, Show)

-- | What building a tree gives: the warnings upstream prints on the way,
-- in order, whether or not the build fails, and the tree or why there is
-- none.
data Built = Built
  { builtWarnings :: ![Warning],
    builtTree :: !(Either BuildEnvFailure Tree)
  }
  deriving (Eq, Show)

-- | Something left out of the tree, with a warning.
data Warning
  = -- | A package that is not a directory (@createLinks@ in buildenv.cc).
    NotIncluded !OsPath
  | -- | An entry whose link leads nowhere, by the path it would have had
    -- in the tree (@createLinks@ in buildenv.cc).
    DanglingLink !OsPath
  deriving (Eq, Show)

-- | Why there is no tree.
data BuildEnvFailure
  = -- | Two files at one path with the same priority: what the link there
    -- already names, the file that came second, and their priority
    -- (@BuildEnvFileConflictError@ in buildenv.hh).
    FileConflict !OsPath !OsPath !Int
  | -- | A directory where a link to something other than a directory
    -- stands: the directory and what the link resolves to
    -- (@createLinks@ in buildenv.cc).
    LinkToNonDirectory !OsPath !OsPath
  | -- | A file where a directory of the tree's own stands: the file and the
    -- directory's path in the tree (@createLinks@ in buildenv.cc).
    FileOverDirectory !OsPath !OsPath
  | -- | A package could not be read.
    ReadFailed !ReadFailure
  deriving (Eq, Show)

-- | The first priority a propagated package takes (@buildProfile@ in
-- buildenv.cc).
firstPropagatedPriority :: Int
firstPropagatedPriority = 1000

-- | The names upstream keeps out of profiles wherever they are, as useless
-- or as files every package of a kind brings (@createLinks@ in
-- buildenv.cc).
leftOutNames :: [OsPath]
leftOutNames =
  map
    asciiPath
    [ "propagated-build-inputs",
      nixSupportName,
      "perllocal.pod",
      "log",
      "manifest.nix",
      "manifest.json"
    ]

-- | The directory a package's support files are in, skipped in the tree.
nixSupportName :: String
nixSupportName = "nix-support"

-- | The file in 'nixSupportName' naming the packages a package propagates.
propagatedPackagesName :: String
propagatedPackagesName = "propagated-user-env-packages"

-- | Build the tree for packages, its links named under the directory
-- @out@ in warnings and failures.
buildProfile :: (Monad m) => TreeReader m -> OsPath -> [Package] -> m Built
buildProfile reader out packages = do
  (tree, warnings) <- runStateT (runExceptT linkAll) []
  pure (Built (reverse warnings) tree)
  where
    linkAll = do
      given <- foldM (\progress p -> addPackage reader out progress (pkgPath p) (pkgPriority p)) noProgress (sortOn order packages)
      propagate reader out firstPropagatedPriority given
    order p = (pkgPriority p, pathKey (pkgPath p))
    noProgress = Progress Map.empty Set.empty Map.empty

-- | The build in progress: a failure ends it, and its warnings are kept
-- newest first.
type Building m = ExceptT BuildEnvFailure (StateT [Warning] m)

-- | What the packages added so far have made.
data Progress = Progress
  { progressTree :: !Tree,
    -- | Each package path added, by its 'pathKey'.
    progressDone :: !(Set [OsPath]),
    -- | The propagated packages waiting to be added, in path order.
    progressQueued :: !(Map [OsPath] OsPath)
  }

-- | A path as upstream orders and compares the paths of packages, as a
-- @std::filesystem::path@: component by component.
pathKey :: OsPath -> [OsPath]
pathKey = OP.splitDirectories

-- | Add a package not added already: link its entries in, then queue the
-- packages it propagates that are not added (@buildProfile@ in
-- buildenv.cc).
addPackage :: (Monad m) => TreeReader m -> OsPath -> Progress -> OsPath -> Int -> Building m Progress
addPackage reader out progress path priority
  | Set.member key (progressDone progress) = pure progress
  | otherwise = do
      tree <- linkInto reader path out priority (progressTree progress)
      propagated <- readStep (trPropagated reader path)
      let done = Set.insert key (progressDone progress)
          queued = foldl' (queue done) (progressQueued progress) propagated
      pure (Progress tree done queued)
  where
    key = pathKey path
    queue done waiting next
      | Set.member (pathKey next) done = waiting
      | otherwise = Map.insertWith (\_ earlier -> earlier) (pathKey next) next waiting

-- | Add the queued propagated packages, round by round, each at the next
-- priority in path order.  A package added since it was queued is passed
-- over but still takes its number, as upstream's counter counts it
-- (@buildProfile@ in buildenv.cc).
propagate :: (Monad m) => TreeReader m -> OsPath -> Int -> Progress -> Building m Tree
propagate reader out priority progress
  | Map.null (progressQueued progress) = pure (progressTree progress)
  | otherwise = do
      (next, nextPriority) <- foldM step (progress {progressQueued = Map.empty}, priority) (Map.elems (progressQueued progress))
      propagate reader out nextPriority next
  where
    step (current, !numbered) path = (,numbered + 1) <$> addPackage reader out current path numbered

-- | Link the entries of the directory @srcDir@ into the tree's directory
-- @tree@, which is at @dstDir@ (upstream's @createLinks@ in
-- buildenv.cc).
linkInto :: (Monad m) => TreeReader m -> OsPath -> OsPath -> Int -> Tree -> Building m Tree
linkInto reader srcDir dstDir priority tree = do
  listing <- readStep (trEntries reader srcDir)
  case listing of
    NotADirectory -> tree <$ warn (NotIncluded srcDir)
    Entries names -> foldM (linkEntry reader srcDir dstDir priority) tree (sort names)

-- | Link one entry of @srcDir@ into @tree@.
linkEntry :: (Monad m) => TreeReader m -> OsPath -> OsPath -> Int -> Tree -> OsPath -> Building m Tree
linkEntry reader srcDir dstDir priority tree name
  | isHidden name = pure tree
  | otherwise = do
      kind <- readStep (trKind reader srcFile)
      case kind of
        Nothing -> tree <$ warn (DanglingLink dstFile)
        Just _ | isLeftOut srcDir name -> pure tree
        Just DirectoryEntry -> directory
        Just NonDirectoryEntry -> file
  where
    srcFile = srcDir </> name
    dstFile = dstDir </> name
    linked = Map.insert name (Link srcFile priority) tree
    merged sub = Map.insert name (Directory sub) tree
    directory = case Map.lookup name tree of
      Just (Directory sub) -> merged <$> linkInto reader srcFile dstFile priority sub
      Just (Link target linkPriority) -> do
        resolved <- readStep (trResolve reader target)
        resolvedKind <- readStep (trKind reader resolved)
        case resolvedKind of
          Just DirectoryEntry -> do
            expanded <- linkInto reader resolved dstFile linkPriority Map.empty
            merged <$> linkInto reader srcFile dstFile priority expanded
          _ -> throwError (LinkToNonDirectory srcFile resolved)
      Nothing -> pure linked
    file = case Map.lookup name tree of
      Just (Link target linkPriority)
        | linkPriority == priority -> throwError (FileConflict target srcFile priority)
        | linkPriority < priority -> pure tree
        | otherwise -> pure linked
      Just (Directory _) -> throwError (FileOverDirectory srcFile dstFile)
      Nothing -> pure linked

-- | Whether a name is one a shell glob leaves out, which upstream skips.
isHidden :: OsPath -> Bool
isHidden name = case OP.unpack name of
  lead : _ -> OP.toChar lead == '.'
  [] -> False

-- | Whether upstream keeps an entry of @srcDir@ out of profiles: its path
-- ends in one of 'leftOutNames', or in @info\/dir@ (@createLinks@ in
-- buildenv.cc).
isLeftOut :: OsPath -> OsPath -> Bool
isLeftOut srcDir name =
  name `elem` leftOutNames
    || (name == asciiPath "dir" && OP.takeFileName srcDir == asciiPath "info")

-- | Run one read of the t'TreeReader', its failure ending the build.
readStep :: (Monad m) => m (Either ReadFailure a) -> Building m a
readStep action = ExceptT (first ReadFailed <$> lift action)

-- | Record a warning.
warn :: (Monad m) => Warning -> Building m ()
warn warning = modify' (warning :)

-- | A warning in upstream's words.
warningText :: Warning -> Text
warningText warning = case warning of
  NotIncluded path -> "not including " <> quoted path <> " in the user environment because it's not a directory"
  DanglingLink path -> "skipping dangling symlink '" <> hostPathText path <> "'"

-- | A failure in upstream's words.
buildEnvFailureText :: BuildEnvFailure -> Text
buildEnvFailureText failure = case failure of
  FileConflict existing incoming _ ->
    "Unable to build profile. There is a conflict for the following files:\n\n  "
      <> quoted existing
      <> "\n  "
      <> quoted incoming
  LinkToNonDirectory dir target -> "collision between " <> quoted dir <> " and non-directory " <> quoted target
  FileOverDirectory file dir -> "collision between non-directory '" <> hostPathText file <> "' and directory '" <> hostPathText dir <> "'"
  ReadFailed (ReadFailure reason) -> reason

-- | A path as upstream's @PathFmt@ prints it, in double quotes
-- (its @operator\<\<@ in fmt.hh).
quoted :: OsPath -> Text
quoted path = "\"" <> hostPathText path <> "\""

-- | An 'OsPath' spelled from ASCII, which 'OP.unsafeFromChar' narrows to
-- one path unit per character without loss.
asciiPath :: String -> OsPath
asciiPath = OP.pack . map OP.unsafeFromChar

-- ---------------------------------------------------------------------------
-- Writing the tree
-- ---------------------------------------------------------------------------

-- | Make the tree's entries in the directory @dir@, which exists.  Each
-- link's Windows flavour is read off its target ('createSymlinkOfKind').
writeTree :: OsPath -> Tree -> IO (Either Text ())
writeTree dir tree = runExceptT (traverse_ place (Map.toList tree))
  where
    place (name, node) =
      let path = dir </> name
       in case node of
            Link target _ -> ExceptT (createSymlinkOfKind path target)
            Directory sub -> do
              ExceptT (first (creating path) <$> tryIO (OsDir.createDirectory path))
              ExceptT (writeTree path sub)
    creating path failure = "creating directory '" <> hostPathText path <> "': " <> T.pack (ioe_description failure)
