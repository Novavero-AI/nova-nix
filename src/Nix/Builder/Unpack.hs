{-# LANGUAGE ScopedTypeVariables #-}

-- | __builtin:unpack__ - in-process archive extraction for the bootstrap seed.
--
-- == Why this is a builtin
--
-- The Windows stdenv bootstrap fetches pre-built toolchain tarballs (MSYS2
-- @.pkg.tar.zst@ packages) into the store before any toolchain exists there.
-- Nothing in the store can unpack them - the unpacker is the thing being
-- bootstrapped - and ambient @tar.exe@ is unpinned and may lack zstd support.
-- So, like 'Nix.Builder.runBuiltinFetchurl', extraction runs in-process:
-- a derivation whose @builder@ is @builtin:unpack@ is handled by this module
-- instead of being spawned as a subprocess.
--
-- == Derivation interface
--
-- * @srcs@ - whitespace-separated archive store paths, extracted in order
--   into the single @out@ output.  MSYS2 packages share a top-level
--   @mingw64\/@ prefix, so extracting a package set into one output yields a
--   working toolchain root directly (no separate union step).
-- * @out@ - the merged tree.
--
-- == Determinism
--
-- Extraction is a pure function of the archive bytes: entries are written in
-- archive order, a file appearing in two archives is an error (pacman
-- enforces the same no-conflict invariant), and pacman's per-package
-- metadata entries (@.PKGINFO@, @.MTREE@, ...) are skipped - they are not part
-- of the installed tree and would otherwise collide across packages.
--
-- Symlink and hardlink entries are materialized by __copying__ their target:
-- symlinks on Windows require elevation or Developer Mode, so a link in the
-- store would make the output machine-dependent.  Copying is deterministic
-- everywhere at a small size cost.
module Nix.Builder.Unpack
  ( -- * Builder name
    builtinUnpackBuilder,

    -- * Derivation environment keys
    envSrcs,

    -- * Extraction budget
    UnpackLimits (..),
    defaultUnpackLimits,

    -- * Running
    runBuiltinUnpack,

    -- * Path validation (exposed for testing)
    entryComponents,
    resolveLinkTarget,
  )
where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as TarEntry
import qualified Codec.Compression.Zstd.Lazy as Zstd
import Control.Exception (SomeException, try)
import Control.Monad (when)
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (toLower)
import Data.Int (Int64)
import Data.List (isPrefixOf, isSuffixOf)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Nix.Derivation (Derivation (..))
import Nix.Store.Path (StoreDir, defaultStoreDir, parseStorePath, storePathToFilePath)
import System.Directory
  ( copyFile,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    doesPathExist,
    getFileSize,
    getPermissions,
    listDirectory,
    setOwnerExecutable,
    setPermissions,
  )
import System.FilePath (isAbsolute, isPathSeparator, joinPath, splitDirectories, takeDirectory, (</>))
import qualified System.Info

-- ---------------------------------------------------------------------------
-- Named constants
-- ---------------------------------------------------------------------------

-- | The magic builder string for the built-in archive extractor.  A
-- derivation with this builder is not executed as a process - the Builder
-- extracts its @srcs@ archives into @$out@ (see 'runBuiltinUnpack').
-- Bytes, matching the 'drvBuilder' field it is compared against.
builtinUnpackBuilder :: BS.ByteString
builtinUnpackBuilder = "builtin:unpack"

-- | Derivation environment key holding the whitespace-separated archive
-- store paths to extract.  Store paths never contain whitespace (store-name
-- validation rejects it), so splitting on words is unambiguous.
envSrcs :: Text
envSrcs = "srcs"

-- | Name of the output the merged tree is extracted into.
unpackOutputName :: Text
unpackOutputName = "out"

-- | Extraction budget for one @builtin:unpack@ build, across every
-- archive in @srcs@: total bytes materialized and total filesystem
-- entries created.  Archive bytes decompress and expand with no
-- relation to their compressed size, so extraction without a budget
-- lets a small input produce an output with no upper bound.
data UnpackLimits = UnpackLimits
  { ulMaxBytes :: !Int64,
    ulMaxEntries :: !Int
  }
  deriving (Eq, Show)

-- | Default extraction budget: 4 GiB and one million entries.  The
-- MSYS2 toolchain seed decompresses to well under half of either, and
-- 4 GiB matches the largest NAR nova-cache's server accepts.
defaultUnpackLimits :: UnpackLimits
defaultUnpackLimits =
  UnpackLimits
    { ulMaxBytes = 4 * 1024 * 1024 * 1024,
      ulMaxEntries = 1000000
    }

-- | Charge one created entry plus its bytes against the remaining
-- budget; a breached budget is a loud extraction failure.
chargeBudget :: Text -> Int64 -> UnpackLimits -> Either Text UnpackLimits
chargeBudget label bytes (UnpackLimits remainingBytes remainingEntries)
  | remainingEntries < 1 =
      Left ("extraction exceeds the entry budget at: " <> label)
  | bytes > remainingBytes =
      Left ("extraction exceeds the size budget at: " <> label)
  | otherwise = Right (UnpackLimits (remainingBytes - bytes) (remainingEntries - 1))

-- | Owner-execute bit of a tar header's mode field (octal @0o100@).
ownerExecuteMode :: TarEntry.Permissions
ownerExecuteMode = 0o100

-- | Pax per-file extended header ('x') and global header ('g') type codes.
-- 'Tar.decodeLongNames' already applies pax path\/linkpath overrides; any
-- header entries still present carry only metadata we do not record (mtime,
-- xattrs), so they are skipped rather than rejected.
paxPerFileCode, paxGlobalCode :: Char
paxPerFileCode = 'x'
paxGlobalCode = 'g'

-- | True on a Windows host.  NTFS has no executable bit (PATHEXT decides),
-- so tar mode bits are only materialized on Unix.
isWindowsHost :: Bool
isWindowsHost = System.Info.os == "mingw32"

-- ---------------------------------------------------------------------------
-- Entry types after long-name decoding
-- ---------------------------------------------------------------------------

-- | Entries as produced by 'Tar.decodeLongNames': GNU\/pax long names are
-- resolved, so paths and link targets are plain 'FilePath's.
type DecodedEntries =
  Tar.GenEntries BL.ByteString FilePath FilePath DecodeError

-- | A single decoded entry.
type DecodedEntry = Tar.GenEntry BL.ByteString FilePath FilePath

-- | Format errors from 'Tar.read' or long-name decoding.
type DecodeError = Either Tar.FormatError Tar.DecodeLongNamesError

-- ---------------------------------------------------------------------------
-- Entry point
-- ---------------------------------------------------------------------------

-- | Run a @builtin:unpack@ derivation: extract every archive listed in its
-- @srcs@ env into the @out@ output directory.  Returns the same
-- @Either (exit, msg) ()@ shape as the process runner, so the shared
-- output-validation and registration path in "Nix.Builder" is reused
-- unchanged.
--
-- @srcs@ carries eval's canonical @\/nix\/store@ spelling, and the physical
-- root may live elsewhere - @C:\\nix\\store@ on Windows, where a bare
-- \/-rooted path resolves against the current DRIVE - so each entry is
-- parsed as a store path and rendered through the store dir rather than
-- opened verbatim (#101).  On Unix the rendering is the identity.
runBuiltinUnpack :: StoreDir -> UnpackLimits -> Derivation -> [(Text, FilePath)] -> IO (Either (Int, Text) ())
runBuiltinUnpack storeDir limits drv outputDirs =
  case (Map.lookup envSrcs (drvEnv drv), lookup unpackOutputName outputDirs) of
    (Nothing, _) -> failure "derivation has no 'srcs'"
    (Just srcsBytes, mOutDir) -> case TE.decodeUtf8' srcsBytes of
      -- Store paths are ASCII; a non-UTF-8 srcs value cannot name any.
      Left _ -> failure "'srcs' contains invalid UTF-8"
      Right srcs
        | null (T.words srcs) -> failure "'srcs' is empty"
        | otherwise -> case traverse resolveSrc (T.words srcs) of
            Left bad -> failure ("'srcs' entry is not a store path: " <> bad)
            Right archives -> case mOutDir of
              Nothing -> failure "derivation defines no 'out' output"
              Just outDir -> do
                createDirectoryIfMissing True outDir
                result <- unpackAll outDir limits archives
                pure $ case result of
                  Left msg -> Left (1, "builtin:unpack: " <> msg)
                  Right () -> Right ()
  where
    failure msg = pure (Left (1, "builtin:unpack: " <> msg))
    resolveSrc word = case parseStorePath defaultStoreDir word of
      Just sp -> Right (storePathToFilePath storeDir sp)
      Nothing -> Left word

-- | Extract archives in order, stopping at the first failure.  One
-- budget spans all of them: the caps bound the BUILD's output, not any
-- single archive.
unpackAll :: FilePath -> UnpackLimits -> [FilePath] -> IO (Either Text ())
unpackAll _ _ [] = pure (Right ())
unpackAll outDir budget (archive : rest) = do
  result <- unpackArchive outDir budget archive
  case result of
    Left err -> pure (Left err)
    Right remaining -> unpackAll outDir remaining rest

-- | Unpack one archive into @outDir@, returning the budget left for the
-- archives after it.  The decompressor is chosen by file extension; the
-- tar stream is decoded (GNU + pax long names) and extracted entry by
-- entry.  Decompression errors surface lazily mid-stream, so the whole
-- extraction is exception-wrapped into a clean failure.
unpackArchive :: FilePath -> UnpackLimits -> FilePath -> IO (Either Text UnpackLimits)
unpackArchive outDir budget archivePath = do
  attempt <- try run
  pure $ case attempt of
    Left (e :: SomeException) ->
      Left (T.pack archivePath <> ": " <> T.pack (show e))
    Right result -> result
  where
    run = case decoderFor archivePath of
      Nothing -> pure (Left ("unsupported archive format: " <> T.pack archivePath))
      Just decoder -> do
        raw <- BL.readFile archivePath
        extractEntries outDir budget (Tar.decodeLongNames (Tar.read (decoder raw)))

-- | Choose a decompressor from the archive file name.  @.tar.zst@ (MSYS2
-- packages) and plain @.tar@ are supported.  MSYS2's zstd frames do not
-- pledge a content size, so the lazy (streaming) zstd decoder is required -
-- the strict single-shot API rejects them.
decoderFor :: FilePath -> Maybe (BL.ByteString -> BL.ByteString)
decoderFor path
  | ".tar.zst" `isSuffixOf` lowered = Just Zstd.decompress
  | ".tar" `isSuffixOf` lowered = Just id
  | otherwise = Nothing
  where
    lowered = map toLower path

-- ---------------------------------------------------------------------------
-- Entry extraction
-- ---------------------------------------------------------------------------

-- | Walk the entry stream, extracting each entry under @outDir@ and
-- threading the remaining extraction budget.
extractEntries :: FilePath -> UnpackLimits -> DecodedEntries -> IO (Either Text UnpackLimits)
extractEntries outDir = go
  where
    go budget stream = case stream of
      Tar.Done -> pure (Right budget)
      Tar.Fail err -> pure (Left ("malformed archive: " <> T.pack (show err)))
      Tar.Next entry rest -> do
        result <- extractEntry outDir budget entry
        case result of
          Left err -> pure (Left err)
          Right remaining -> go remaining rest

-- | Extract a single entry, after validating its path stays inside the
-- archive root and skipping pacman package metadata.
extractEntry :: FilePath -> UnpackLimits -> DecodedEntry -> IO (Either Text UnpackLimits)
extractEntry outDir budget entry =
  case entryComponents (TarEntry.entryTarPath entry) of
    Left err -> pure (Left err)
    -- The archive root itself (an entry for "." or "./").
    Right [] -> pure (Right budget)
    Right comps
      | isPackageMetadata comps -> pure (Right budget)
      | otherwise -> extractContent outDir budget comps entry

-- | Extract a path-validated entry's content, charging every created
-- entry and its bytes against the budget.  @comps@ is non-empty (the
-- empty case is consumed by 'extractEntry').
extractContent :: FilePath -> UnpackLimits -> [FilePath] -> DecodedEntry -> IO (Either Text UnpackLimits)
extractContent outDir budget comps entry =
  case TarEntry.entryContent entry of
    Tar.Directory ->
      case chargeBudget pathText 0 budget of
        Left err -> pure (Left err)
        Right remaining -> do
          createDirectoryIfMissing True dest
          pure (Right remaining)
    Tar.NormalFile bytes size ->
      -- The header size is charged before the lazy content is realized,
      -- so a breach rejects the entry rather than materializing it.
      case chargeBudget pathText size budget of
        Left err -> pure (Left err)
        Right remaining -> do
          fresh <- freshDestination dest
          case fresh of
            Left err -> pure (Left err)
            Right () -> do
              createDirectoryIfMissing True (takeDirectory dest)
              BL.writeFile dest bytes
              when (executableEntry entry && not isWindowsHost) (markExecutable dest)
              pure (Right remaining)
    -- A symlink target is relative to the link's own directory.
    Tar.SymbolicLink target ->
      copyLinkTarget "symlink" (resolveLinkTarget parentComps target)
    -- A hardlink target is relative to the archive root.
    Tar.HardLink target ->
      copyLinkTarget "hardlink" (entryComponents target)
    Tar.OtherEntryType code _ _
      | code == paxPerFileCode || code == paxGlobalCode -> pure (Right budget)
      | otherwise ->
          pure (Left ("unsupported tar entry type '" <> T.singleton code <> "': " <> pathText))
    Tar.CharacterDevice _ _ -> unsupportedSpecial "character device"
    Tar.BlockDevice _ _ -> unsupportedSpecial "block device"
    Tar.NamedPipe -> unsupportedSpecial "named pipe"
  where
    dest = outDir </> joinPath comps
    parentComps = take (length comps - 1) comps
    pathText = T.pack (TarEntry.entryTarPath entry)
    unsupportedSpecial kind =
      pure (Left ("unsupported " <> kind <> " entry: " <> pathText))
    copyLinkTarget kind resolved = case resolved of
      Left err -> pure (Left err)
      Right targetComps -> do
        let targetPath = outDir </> joinPath targetComps
        isFile <- doesFileExist targetPath
        isDir <- doesDirectoryExist targetPath
        copyTarget kind targetPath isFile isDir
    copyTarget kind targetPath isFile isDir
      | isFile = do
          fresh <- freshDestination dest
          case fresh of
            Left err -> pure (Left err)
            Right () -> do
              size <- getFileSize targetPath
              case chargeBudget pathText (fromIntegral size) budget of
                Left err -> pure (Left err)
                Right remaining -> do
                  createDirectoryIfMissing True (takeDirectory dest)
                  copyFile targetPath dest
                  pure (Right remaining)
      | isDir = do
          -- The same collision guard as regular entries: without it a
          -- directory link copy silently merges into content another
          -- archive extracted, making the result entry-order-dependent.
          fresh <- freshDestination dest
          case fresh of
            Left err -> pure (Left err)
            Right () -> case chargeBudget pathText 0 budget of
              Left err -> pure (Left err)
              Right remaining -> copyTree remaining targetPath dest
      | otherwise =
          pure
            ( Left
                ( kind
                    <> " target not present (links must follow their targets in the archive): "
                    <> pathText
                    <> " -> "
                    <> T.pack targetPath
                )
            )

-- ---------------------------------------------------------------------------
-- Path validation
-- ---------------------------------------------------------------------------

-- | Split a tar entry path into validated components: relative, no @..@, no
-- @:@ (drive letters, NTFS alternate data streams).  Tar paths use @/@;
-- 'splitDirectories' accepts both separators on Windows.  @.@ components are
-- dropped, so @./mingw64@ and @mingw64@ agree.
entryComponents :: FilePath -> Either Text [FilePath]
entryComponents raw
  | isAbsolute raw = Left ("absolute entry path: " <> T.pack raw)
  | startsAtRoot comps = Left ("rooted entry path: " <> T.pack raw)
  | ".." `elem` comps = Left ("entry path escapes archive root: " <> T.pack raw)
  | any (elem ':') comps = Left ("entry path contains ':': " <> T.pack raw)
  | otherwise = Right comps
  where
    comps = filter (/= ".") (splitDirectories raw)

-- | Resolve a symlink target (relative to the link's parent directory)
-- against the archive root.  @..@ components are allowed but must not climb
-- above the root.
resolveLinkTarget :: [FilePath] -> FilePath -> Either Text [FilePath]
resolveLinkTarget parentComps target
  | isAbsolute target = Left ("absolute symlink target: " <> T.pack target)
  | startsAtRoot targetComps = Left ("rooted symlink target: " <> T.pack target)
  | otherwise = walk (reverse parentComps) targetComps
  where
    targetComps = filter (/= ".") (splitDirectories target)
    walk stack remaining = case (stack, remaining) of
      (_, []) -> Right (reverse stack)
      ([], ".." : _) ->
        Left ("symlink target escapes archive root: " <> T.pack target)
      (_ : popped, ".." : rest) -> walk popped rest
      (_, comp : rest)
        | ':' `elem` comp -> Left ("symlink target contains ':': " <> T.pack target)
        | otherwise -> walk (comp : stack) rest

-- | True when the first path component is a bare separator, i.e. a path rooted
-- at the current drive rather than a named location (a leading @/@ or @\\@ with
-- no drive letter).  On Windows 'isAbsolute' returns 'False' for such paths -
-- it wants a drive letter or a UNC prefix - yet 'System.FilePath.combine' still
-- discards the output directory when the right operand begins with a separator,
-- so joining one onto @outDir@ drops @outDir@ and the write lands at the drive
-- root.  'isPathSeparator' follows the host convention, so on POSIX a leading
-- @\\@ is an ordinary filename character and correctly is not treated as rooted.
startsAtRoot :: [FilePath] -> Bool
startsAtRoot comps = case comps of
  (comp : _) -> not (null comp) && all isPathSeparator comp
  [] -> False

-- | pacman package metadata at the archive root (@.PKGINFO@, @.BUILDINFO@,
-- @.MTREE@, @.INSTALL@): describes the package to pacman, is not part of the
-- installed tree, and collides across packages when several archives merge
-- into one output - so root-level dotfile entries are skipped.
isPackageMetadata :: [FilePath] -> Bool
isPackageMetadata comps = case comps of
  [name] -> "." `isPrefixOf` name
  _ -> False

-- ---------------------------------------------------------------------------
-- Filesystem helpers
-- ---------------------------------------------------------------------------

-- | Guard that nothing was already extracted at @dest@.  Two archives in one
-- seed providing the same file is a packaging error worth failing loudly on
-- (pacman enforces the same no-conflict invariant between its packages).
freshDestination :: FilePath -> IO (Either Text ())
freshDestination dest = do
  exists <- doesPathExist dest
  pure $
    if exists
      then Left ("file collision between archives: " <> T.pack dest)
      else Right ()

-- | True when the entry's tar mode has the owner-execute bit.
executableEntry :: DecodedEntry -> Bool
executableEntry entry = TarEntry.entryPermissions entry .&. ownerExecuteMode /= 0

-- | Materialize the executable bit on Unix hosts.
markExecutable :: FilePath -> IO ()
markExecutable path = do
  perms <- getPermissions path
  setPermissions path (setOwnerExecutable True perms)

-- | Recursively copy a directory tree (used to materialize directory
-- symlinks, which cannot be store-portable links on Windows), charging
-- every created entry and byte against the budget: each tree copy
-- duplicates content already extracted and charged once, so uncharged
-- copies would let K link entries multiply the output roughly 2^K-fold.
copyTree :: UnpackLimits -> FilePath -> FilePath -> IO (Either Text UnpackLimits)
copyTree budget0 src dest = do
  createDirectoryIfMissing True dest
  names <- listDirectory src
  go budget0 names
  where
    go budget [] = pure (Right budget)
    go budget (name : rest) = do
      let from = src </> name
          to = dest </> name
      isDir <- doesDirectoryExist from
      copied <-
        if isDir
          then case chargeBudget (T.pack to) 0 budget of
            Left err -> pure (Left err)
            Right remaining -> copyTree remaining from to
          else do
            size <- getFileSize from
            case chargeBudget (T.pack to) (fromIntegral size) budget of
              Left err -> pure (Left err)
              Right remaining -> do
                copyFile from to
                pure (Right remaining)
      case copied of
        Left err -> pure (Left err)
        Right remaining -> go remaining rest
