{-# LANGUAGE ScopedTypeVariables #-}

-- | A binary cache in a local directory, as upstream's @file:\/\/@ store
-- reads and writes one: the URL that names it, the @nix-cache-info@ at its
-- root, the directories it holds, and the files a narinfo may name in it.
-- A push writes such a cache ("Nix.Push.Directory") and a substitution
-- reads one ("Nix.Substituter"), and both open it the same way first, as
-- upstream opens its store the same way for either.
--
-- == Opening
--
-- Opening a cache makes its @nar@, @build-trace-v2@ and @log@ directories,
-- writes a @nix-cache-info@ holding @StoreDir: \/nix\/store@ alone where
-- the directory has none, and reads the one it has ('openCacheDirectory').
-- Upstream's store makes those directories whenever it opens a cache, to
-- read it as well as to write, and making a directory that is there needs
-- no write access.  A reader that cannot make a missing one drops the
-- cache with a warning, so a push makes them all for a reader without
-- write access to the directory, and also the @realisations@ directory
-- that older upstream releases name in place of @build-trace-v2@
-- ('realisationsDirName'), for a reader of either.
module Nix.CacheDirectory
  ( -- * Naming a cache
    fileUrlDirectory,
    directoryUrl,

    -- * Opening a cache
    CacheInfo (..),
    CacheOpenFailure (..),
    cacheOpenFailureMessage,
    openCacheDirectory,

    -- * Files in a cache
    realisationsDirName,
    cacheFilePath,
    writeCacheFile,

    -- * Pure pieces (exported for tests)
    cacheInfoFileName,
    cacheInfoText,
    readCacheInfo,
    parsePriority,
  )
where

import Control.Exception (IOException, bracketOnError, catch, displayException, handle, tryJust)
import Control.Monad (foldM, guard)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Int (Int32)
import Data.List (stripPrefix)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Nix.Push (narDirSegment)
import Nix.Store.Path (defaultStoreDirText)
import System.Directory (createDirectoryIfMissing, removeFile, renameFile)
import System.FilePath (dropTrailingPathSeparator, hasDrive, isAbsolute, isPathSeparator, splitDirectories, takeDirectory, takeFileName, (</>))
import System.IO (Handle, hClose, openBinaryTempFileWithDefaultPermissions)
import System.IO.Error (isDoesNotExistError)
import Text.Read (readMaybe)

-- ---------------------------------------------------------------------------
-- Naming a cache
-- ---------------------------------------------------------------------------

-- | The scheme of a URL that names a directory.
fileScheme :: String
fileScheme = "file://"

-- | The directory a @file:\/\/@ URL names, or why it names none, and
-- 'Nothing' for a URL of another scheme.  The URL names an absolute path
-- after an empty host, the form upstream's grammar gives a @file:\/\/@ URL,
-- which refuses any host; upstream reads @file:\/\/@ alone as the root
-- directory.  A URL naming a filesystem root, that one or @file:\/\/\/@ or a
-- drive's root, is refused here, as a cache in a root is a variable that
-- expanded to nothing.  A query, a fragment or a percent-escape is refused
-- rather than read: upstream takes the query as the store's settings and
-- decodes escapes, so each would name another cache there.  The refusal
-- ends with the given words, which say how the caller takes what a query
-- would set.  A URL without them that upstream parses names the same
-- directory in both.  A Windows drive path is written with a slash before
-- the drive, @file:\/\/\/C:\/cache@ (RFC 8089, appendix E.2), so where that
-- names no absolute path and the drive path does, on Windows, the slash is
-- dropped.
fileUrlDirectory :: Text -> String -> Maybe (Either Text FilePath)
fileUrlDirectory settingsHint url = named <$> stripPrefix fileScheme url
  where
    shown = T.pack url
    named path
      | any (`elem` unreadUrlCharacters) path =
          Left ("a file:// URL with a query, a fragment or a percent-escape is not read here; name the directory as it is spelled, and " <> settingsHint <> ": " <> shown)
      | otherwise = case absoluteAfterHost path of
          Nothing -> Left ("a file:// URL names an absolute path after an empty host, as file:///srv/cache does, or file:///C:/cache on Windows: " <> shown)
          Just found
            | isRoot found -> Left ("a file:// URL names a directory below a filesystem root, as a variable that expanded to nothing leaves the root: " <> shown)
            | otherwise -> Right found
    isRoot candidate = let trimmed = dropTrailingPathSeparator candidate in takeDirectory trimmed == trimmed
    -- The empty host is the slash that begins the path; without it the
    -- URL names a host, even where what follows is a drive path.
    absoluteAfterHost path = case path of
      '/' : drivePath@(letter : ':' : _)
        | isAsciiUpper letter || isAsciiLower letter,
          isAbsolute drivePath ->
            Just drivePath
      '/' : _ | isAbsolute path -> Just path
      _ -> Nothing

-- | The characters that begin a URL's query, fragment and percent-escapes.
unreadUrlCharacters :: String
unreadUrlCharacters = "?#%"

-- | The @file:\/\/@ URL of a directory 'fileUrlDirectory' accepts, spelled
-- as upstream spells one: with a slash before a Windows drive, and each
-- path separator a slash.
directoryUrl :: FilePath -> Text
directoryUrl dir = T.pack (fileScheme <> driveSlash <> map slashed dir)
  where
    driveSlash = case dir of
      letter : ':' : _ | isAsciiUpper letter || isAsciiLower letter -> "/"
      _ -> ""
    slashed c
      | isPathSeparator c = '/'
      | otherwise = c

-- ---------------------------------------------------------------------------
-- Opening a cache
-- ---------------------------------------------------------------------------

-- | What a cache's @nix-cache-info@ says that nova-nix reads.
newtype CacheInfo = CacheInfo
  { -- | Where a substitution asks the cache among the others, lower
    -- first, when the cache states it.
    ciPriority :: Maybe Int
  }
  deriving (Eq, Show)

-- | Why a cache could not be opened, told apart as upstream tells them
-- apart: a substitution passes over a cache it cannot open, with a
-- warning, while a @Priority@ upstream cannot read as a number fails the
-- command that opens the cache.
data CacheOpenFailure
  = -- | Its directories or its @nix-cache-info@ could not be made or
    -- read, or it is for another store directory.
    CacheUnopenable !Text
  | -- | Its @nix-cache-info@ states a @Priority@ that is not a number,
    -- said in nova-nix's words: upstream's command fails with only the
    -- C++ library's text for the failed conversion.
    CachePriorityInvalid !Text
  deriving (Eq, Show)

-- | What a t'CacheOpenFailure' says.
cacheOpenFailureMessage :: CacheOpenFailure -> Text
cacheOpenFailureMessage failure = case failure of
  CacheUnopenable message -> message
  CachePriorityInvalid message -> message

-- | Open the cache in a directory, named by its URL in what this says:
-- make its directories, write its @nix-cache-info@ where it has none, and
-- read the one it has ('readCacheInfo').  A directory or file that cannot
-- be made or read is a 'CacheUnopenable', which names the cache.
openCacheDirectory :: Text -> FilePath -> IO (Either CacheOpenFailure CacheInfo)
openCacheDirectory url dir = handle unopenable $ do
  mapM_ (createDirectoryIfMissing True . (dir </>)) cacheDirectories
  existing <- tryJust (guard . isDoesNotExistError) (BS.readFile file)
  case existing of
    Left () -> Right (CacheInfo Nothing) <$ writeCacheFile file cacheInfoText
    Right contents -> pure (readCacheInfo url contents)
  where
    file = dir </> cacheInfoFileName
    unopenable (err :: IOException) =
      pure (Left (CacheUnopenable ("cannot open binary cache '" <> url <> "': " <> T.pack (displayException err))))

-- | The directories upstream's @file:\/\/@ store makes in every cache it
-- opens: the NARs', and those a push leaves empty, the realisations
-- directory under the name the newest upstream release gives it.
cacheDirectories :: [FilePath]
cacheDirectories = [T.unpack narDirSegment, buildTraceDirName, logDirName]

-- | The directory of content-addressed derivations' realisations, as
-- older upstream releases name it, which a push makes beside
-- @build-trace-v2@ so a reader of an older release that cannot write
-- opens the cache.  A substitution makes only the newer name, as the
-- newest release does, so it opens a cache without this one that it
-- cannot write to.
realisationsDirName :: FilePath
realisationsDirName = "realisations"

-- | The directory of content-addressed derivations' realisations, as the
-- newest upstream release names it.
buildTraceDirName :: FilePath
buildTraceDirName = "build-trace-v2"

-- | The directory of build logs.
logDirName :: FilePath
logDirName = "log"

-- ---------------------------------------------------------------------------
-- nix-cache-info
-- ---------------------------------------------------------------------------

-- | The file at a binary cache's root that names the store directory its
-- paths are in.
cacheInfoFileName :: FilePath
cacheInfoFileName = "nix-cache-info"

-- | The field of @nix-cache-info@ naming the store directory.
storeDirField :: BS.ByteString
storeDirField = "StoreDir"

-- | The field of @nix-cache-info@ giving the cache's priority.
priorityField :: BS.ByteString
priorityField = "Priority"

-- | What opening a directory writes as @nix-cache-info@ where it has none:
-- the store directory alone, as upstream writes it.  The narinfos name
-- paths in the canonical store directory whatever directory the local
-- store is in, so this is @\/nix\/store@ on every host.
cacheInfoText :: BS.ByteString
cacheInfoText = storeDirField <> ": " <> TE.encodeUtf8 defaultStoreDirText <> "\n"

-- | Read a @nix-cache-info@ as upstream reads it, the cache named by its
-- URL in what a refusal says: each line with a colon is a field, its name
-- what comes before the first colon and its value the rest trimmed of
-- spaces, tabs, carriage returns and newlines, and the fields are read in
-- order, so the first one refused is the refusal.  A @StoreDir@ other than
-- @\/nix\/store@ refuses the cache in upstream's words.  The last
-- @Priority@ is the cache's priority, read as 'parsePriority' reads it,
-- and one that does not read is a 'CachePriorityInvalid'.  Upstream's
-- @WantMassQuery@ is not read: it says whether to ask the cache about many
-- paths at once, which nova-nix never does, and other fields upstream does
-- not read either.
readCacheInfo :: Text -> BS.ByteString -> Either CacheOpenFailure CacheInfo
readCacheInfo url contents = foldM field (CacheInfo Nothing) fields
  where
    fields =
      [ (name, trim (BS.drop 1 rest))
      | line <- BC.split '\n' contents,
        let (name, rest) = BC.break (== ':') line,
        not (BS.null rest)
      ]
    field info (name, value)
      | name == storeDirField && value /= canonical =
          Left
            ( CacheUnopenable
                ( "binary cache '"
                    <> url
                    <> "' is for Nix stores with prefix '"
                    <> lenient value
                    <> "', not '"
                    <> defaultStoreDirText
                    <> "'"
                )
            )
      | name == priorityField = case parsePriority value of
          Just priority -> Right info {ciPriority = Just priority}
          Nothing -> Left (CachePriorityInvalid ("binary cache '" <> url <> "' gives the Priority '" <> lenient value <> "' in its nix-cache-info, which is not a 32-bit integer"))
      | otherwise = Right info
    canonical = TE.encodeUtf8 defaultStoreDirText
    lenient = TE.decodeUtf8With TE.lenientDecode
    trim = BC.dropWhile isTrimmed . BC.dropWhileEnd isTrimmed
    isTrimmed c = c `elem` (" \t\r\n" :: String)

-- | A @Priority@ value as upstream reads it, which is how C reads an
-- @int@ from text: after any white space, an optional sign and at least
-- one decimal digit, with whatever follows them ignored, within the range
-- of a 32-bit @int@.
parsePriority :: BS.ByteString -> Maybe Int
parsePriority value = do
  let unspaced = BC.dropWhile (`elem` cWhiteSpace) value
      (signed, unsigned) = case BC.uncons unspaced of
        Just ('-', rest) -> (negate, rest)
        Just ('+', rest) -> (id, rest)
        _ -> (id, unspaced)
  magnitude <- readMaybe (BC.unpack (BC.takeWhile isDigit unsigned)) :: Maybe Integer
  let priority = signed magnitude
  guard (priority >= toInteger (minBound :: Int32) && priority <= toInteger (maxBound :: Int32))
  pure (fromInteger priority)

-- | The characters C counts as white space before a number.
cWhiteSpace :: String
cWhiteSpace = " \t\n\v\f\r"

-- ---------------------------------------------------------------------------
-- Files in a cache
-- ---------------------------------------------------------------------------

-- | The path of a file a narinfo names in the cache in a directory, held
-- below the directory as upstream holds it: a name that is empty, absolute
-- or holds a @.@ or @..@ segment is refused in upstream's words.  A name
-- with a drive, or one beginning with a separator, is refused as absolute
-- on Windows too, where upstream would read some of them below the cache.
cacheFilePath :: FilePath -> Text -> Either Text FilePath
cacheFilePath dir name
  | null relative = Left "local binary cache path must not be empty"
  | hasDrive relative || any isPathSeparator (take 1 relative) =
      Left ("local binary cache path '" <> name <> "' must not be absolute")
  | any (`elem` [".", ".."]) (splitDirectories relative) =
      Left ("local binary cache path '" <> name <> "' must not contain '..' or '.' segments")
  | otherwise = Right (dir </> relative)
  where
    relative = T.unpack name

-- | Write a file of a cache: whole, into a new file beside it, which is
-- then renamed onto it, as upstream writes one, so a reader never meets a
-- torn file.  The new file has the permissions the umask leaves, as
-- upstream's write creates one, where @openBinaryTempFile@ would leave it
-- readable by its owner alone and so unreadable to a web server running as
-- another user.  Its name starts with a dot, which static servers commonly
-- hide.
writeCacheFile :: FilePath -> BS.ByteString -> IO ()
writeCacheFile target bytes =
  bracketOnError
    (openBinaryTempFileWithDefaultPermissions (takeDirectory target) ('.' : takeFileName target <> tempSuffix))
    discardTemp
    ( \(temp, out) -> do
        BS.hPut out bytes
        hClose out
        renameFile temp target
    )

-- | The extension of a file being written, before its rename.
tempSuffix :: String
tempSuffix = ".tmp"

-- | Close and remove a file whose write failed.  An error doing so is
-- dropped, so it never replaces the failure that brought it here.
discardTemp :: (FilePath, Handle) -> IO ()
discardTemp (temp, opened) = do
  ignoringIOErrors (hClose opened)
  ignoringIOErrors (removeFile temp)
  where
    ignoringIOErrors action = action `catch` \(_ :: IOException) -> pure ()
