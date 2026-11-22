{-# LANGUAGE ScopedTypeVariables #-}

-- | Push store paths to a binary cache in a local directory, as upstream's
-- @nix copy --to file:\/\/\/DIR@ writes one, signing each narinfo with a
-- secret key read here rather than leaving the signing to a server.
--
-- == Layout
--
-- The directory gets the layout upstream's @LocalBinaryCacheStore@ writes
-- at Nix 2.24.9 and its HTTP substituter reads, so any static web server
-- or file share can serve it:
--
-- * @nix-cache-info@, holding @StoreDir: \/nix\/store@ alone, written when
--   the directory has none, and refused when it names another store
--   directory (@BinaryCacheStore::init@, binary-cache-store.cc:40-64).  An
--   upstream HTTP substituter reads no cache without one
--   (http-binary-cache-store.cc:70-84).
-- * @\<hash\>.narinfo@ for each path (binary-cache-store.cc:106-109), its
--   fields in the order @NarInfo::to_string@ writes them
--   (nar-info.cc:101-127).
-- * The NAR at the narinfo's @URL@, @nar\/\<filehash\>.nar@, or @.nar.zst@
--   compressed with zstd, named by the nix-base32 hash of the file
--   (binary-cache-store.cc:163-170).
-- * The empty @realisations@, @build-trace-v2@ and @log@ directories
--   beside @nar@, where content-addressed derivations and
--   @nix store copy-log@ write.  A push writes into none of them, but
--   upstream's @file:\/\/@ store creates @nar@, its realisations
--   directory and @log@ whenever it opens a cache, to read it as well as
--   to write (local-binary-cache-store.cc:107-115, run by
--   store-api.cc:1334), and creating a directory that is there needs no
--   write access (file-system.cc:427-434).  A reader that cannot create a
--   missing one drops the cache with only a warning
--   (store-api.cc:1345-1352), so a push creates them for a reader without
--   write access to the directory.  The realisations directory is
--   @realisations@ through Nix 2.34.9 and @build-trace-v2@ from 2.35.4, so
--   a push makes both, for a reader of either.
--
-- Each narinfo carries an Ed25519 signature by the secret key, as
-- upstream's binary cache store signs every narinfo it writes with the
-- key its @secret-key@ setting names (binary-cache-store.hh:32-33,
-- binary-cache-store.cc:31-33 and 271): over upstream's fingerprint
-- (@ValidPathInfo::fingerprint@, path-info.cc:25-35), as @name:base64@
-- (local-keys.cc:50-57).  nova-cache computes both.  Its verification of
-- that fingerprint accepts cache.nixos.org's signatures, and Ed25519
-- signing is deterministic, so with a key whose second half is the
-- public key of its seed, the only keys 'loadSecretKeyFile' accepts, a
-- signature here is the one upstream's signer makes.
--
-- == Writes
--
-- A path whose narinfo is in the directory is skipped, as upstream takes
-- it to be in the cache (binary-cache-store.cc:371-377), and a NAR whose
-- file is there is not written again (binary-cache-store.cc:258-264).
-- Every NAR is written before any narinfo, and the narinfos in the
-- closure's order, as "Nix.Push" uploads them, so no narinfo names a NAR
-- or a reference that is not there yet.  Each file is written whole
-- beside its name and renamed onto it, as upstream's @upsertFile@ writes
-- it (local-binary-cache-store.cc:58-70), so a reader never meets a torn
-- one.
module Nix.Push.Directory
  ( -- * Where a push writes
    PushTarget (..),
    pushTarget,

    -- * Pushing to a directory
    DirectoryPush (..),
    pushToDirectory,
    loadSecretKeyFile,
    ed25519SeedBytes,

    -- * Pure pieces (exported for tests)
    cacheInfoFileName,
    cacheInfoText,
    checkCacheInfo,
    signNarInfo,
  )
where

import Control.Exception (IOException, bracketOnError, catch, displayException, tryJust)
import Control.Monad (filterM, forM, forM_, guard, unless)
import Control.Monad.Except (ExceptT (..), liftEither, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char (isAsciiLower, isAsciiUpper)
import Data.List (find, isPrefixOf, stripPrefix)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Nix.Http (catchSync)
import Nix.Push (PushArtifact (..), PushCompression, PushSummary (..), computeClosure, loadKeyFile, logNar, logPushed, logSkipped, narDirSegment, narInfoFileName, preparePath)
import Nix.Store (Store)
import Nix.Store.Path (StorePath, defaultStoreDirText)
import NovaCache.NarInfo (NarInfo (..), renderNarInfo)
import NovaCache.Signing (SecretKey)
import qualified NovaCache.Signing as Signing
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile, renameFile)
import System.FilePath (dropTrailingPathSeparator, isAbsolute, takeDirectory, takeFileName, (</>))
import System.IO (Handle, hClose, openBinaryTempFileWithDefaultPermissions)
import System.IO.Error (isDoesNotExistError)

-- ---------------------------------------------------------------------------
-- Where a push writes
-- ---------------------------------------------------------------------------

-- | Where a push writes, with the file holding the key it needs there.
data PushTarget
  = -- | A nova-cache server, by its base URL with no trailing slash, and
    -- the file holding its API key, if it takes one.
    ServerTarget !Text !(Maybe FilePath)
  | -- | A directory, and the file holding the secret key that signs its
    -- narinfos.
    DirectoryTarget !FilePath !FilePath
  deriving (Eq, Show)

-- | The scheme of a URL that names a directory.
fileScheme :: String
fileScheme = "file://"

-- | The schemes of a nova-cache server's URL.
serverSchemes :: [String]
serverSchemes = ["http://", "https://"]

-- | What @push --cache URL@ writes to, given the files @--key-file@ and
-- @--secret-key-file@ name.  An @http:\/\/@ or @https:\/\/@ URL is a
-- nova-cache server, which signs what it receives, so it takes no secret
-- key.  A @file:\/\/@ URL is a directory, which takes no API key and needs
-- the secret key: nova-nix's substituter refuses a narinfo with no
-- signature, and so does upstream's unless @require-sigs@ is off or the
-- cache is marked trusted.
pushTarget :: String -> Maybe FilePath -> Maybe FilePath -> Either Text PushTarget
pushTarget url apiKeyFile secretKeyFile
  | Just path <- stripPrefix fileScheme url = do
      dir <- fileUrlDirectory path
      case (apiKeyFile, secretKeyFile) of
        (Just _, _) -> Left "--key-file is a nova-cache server's API key; a file:// cache is signed with --secret-key-file"
        (Nothing, Nothing) -> Left "a file:// cache needs --secret-key-file, the secret key that signs its narinfos"
        (Nothing, Just keyFile) -> Right (DirectoryTarget dir keyFile)
  | any (`isPrefixOf` url) serverSchemes = case secretKeyFile of
      Just _ -> Left "--secret-key-file signs a file:// cache; a nova-cache server signs what it receives"
      Nothing -> Right (ServerTarget (T.dropWhileEnd (== '/') (T.pack url)) apiKeyFile)
  | otherwise = Left ("--cache takes the http:// or https:// URL of a nova-cache server, or a file:// URL of a directory, not " <> T.pack url)

-- | The directory named by what follows @file:\/\/@ in a URL: an empty
-- host and an absolute path, the form upstream's grammar gives a
-- @file:\/\/@ URL (url.cc:15-20), which refuses any host (url.cc:35-37).
-- Upstream reads @file:\/\/@ alone as the root directory (url.cc:39-40).
-- A URL naming a filesystem root, that one or @file:\/\/\/@ or a drive's
-- root, is refused here, as a cache in a root is a variable that expanded
-- to nothing.  A query, a fragment or a percent-escape is refused rather
-- than read: upstream takes the query as the store's settings and decodes
-- escapes (url.cc:42-50, store-reference.cc:48-51), so each would name
-- another cache there.  A URL without them that upstream parses names
-- the same directory in both.  A Windows drive path is written with a
-- slash before the drive, @file:\/\/\/C:\/cache@ (RFC 8089, appendix
-- E.2), so where that names no absolute path and the drive path does, on
-- Windows, the slash is dropped.
fileUrlDirectory :: String -> Either Text FilePath
fileUrlDirectory path
  | any (`elem` unreadUrlCharacters) path =
      Left ("a file:// URL with a query, a fragment or a percent-escape is not read here; name the directory as it is spelled, and sign and compress with --secret-key-file and --compression: " <> shown)
  | otherwise = case dir of
      Nothing -> Left ("a file:// URL names an absolute path after an empty host, as file:///srv/cache does, or file:///C:/cache on Windows: " <> shown)
      Just found
        | isRoot found -> Left ("a file:// URL names a directory below a filesystem root, as a variable that expanded to nothing leaves the root: " <> shown)
        | otherwise -> Right found
  where
    shown = T.pack (fileScheme <> path)
    isRoot candidate = let trimmed = dropTrailingPathSeparator candidate in takeDirectory trimmed == trimmed
    -- The empty host is the slash that begins the path; without it the
    -- URL names a host, even where what follows is a drive path.
    dir = case path of
      '/' : drivePath@(letter : ':' : _)
        | isAsciiUpper letter || isAsciiLower letter,
          isAbsolute drivePath ->
            Just drivePath
      '/' : _ | isAbsolute path -> Just path
      _ -> Nothing

-- | The characters that begin a URL's query, fragment and percent-escapes.
unreadUrlCharacters :: String
unreadUrlCharacters = "?#%"

-- ---------------------------------------------------------------------------
-- Pushing to a directory
-- ---------------------------------------------------------------------------

-- | Where and how a push writes a directory cache.
data DirectoryPush = DirectoryPush
  { -- | The cache's directory, created if it is not there.
    dpDirectory :: !FilePath,
    -- | The key that signs every narinfo the push writes.
    dpSecretKey :: !SecretKey,
    -- | How each NAR is packaged, as for a server.
    dpCompression :: !PushCompression
  }
  deriving (Eq, Show)

-- | Write the closure of the given paths to the directory cache.  A path
-- the cache holds is skipped.
--
-- Any failure stops the push with an error.  What it leaves is NARs no
-- narinfo names, which no reader asks for, and narinfos for some of the
-- paths, written deps first, so each names a NAR and references that are
-- there.  An interrupt is not a failure: it stops the push and
-- propagates.
pushToDirectory :: DirectoryPush -> Store -> [StorePath] -> IO (Either Text PushSummary)
pushToDirectory cfg store roots =
  -- Every synchronous exception, as for a server: the database's, the
  -- serialiser's, and the directory's IO errors are each a failed push.
  runExceptT run `catchSync` \e -> pure (Left (T.pack (displayException e)))
  where
    inCache relative = dpDirectory cfg </> T.unpack relative
    run = do
      liftIO (mapM_ (createDirectoryIfMissing True . (dpDirectory cfg </>)) cacheDirectories)
      ExceptT (prepareCacheInfo (dpDirectory cfg))
      closure <- ExceptT (computeClosure store roots)
      missing <- liftIO (filterM (fmap not . doesFileExist . inCache . narInfoFileName) closure)
      let skipped = length closure - length missing
      logSkipped skipped
      -- NARs first, one at a time.  Each path's narinfo is signed and
      -- rendered as its NAR is written, and only the rendered bytes are
      -- kept, forced, so what the second phase holds reaches no NAR.
      written <- forM missing $ \sp -> do
        (artifact, narInfo) <- preparePath (dpCompression cfg) store sp
        logNar sp artifact
        liftIO (writeUnlessPresent (inCache (niUrl narInfo)) (paBytes artifact))
        signed <- liftEither (signNarInfo (dpSecretKey cfg) narInfo)
        let !rendered = TE.encodeUtf8 (renderNarInfo signed)
        pure (sp, rendered)
      -- Then the narinfos, in the closure's deps-first order.
      forM_ written $ \(sp, rendered) -> do
        liftIO (writeCacheFile (inCache (narInfoFileName sp)) rendered)
        logPushed sp
      pure PushSummary {psPushed = length written, psSkipped = skipped}

-- | The directories upstream's @file:\/\/@ store creates in every cache it
-- opens (local-binary-cache-store.cc:109-113): the NARs', and those a
-- push leaves empty, the realisations directory under both of its names.
cacheDirectories :: [FilePath]
cacheDirectories = [T.unpack narDirSegment, realisationsDirName, buildTraceDirName, logDirName]

-- | The directory of content-addressed derivations' realisations,
-- upstream's @realisationsPrefix@ through Nix 2.34.9
-- (binary-cache-store.hh:65 at 2.24.9,
-- include/nix/store/binary-cache-store.hh:95 at 2.34.9).
realisationsDirName :: FilePath
realisationsDirName = "realisations"

-- | The same directory as upstream's @realisationsPrefix@ names it from
-- Nix 2.35.4 (include/nix/store/binary-cache-store.hh:118 at 2.35.4).
buildTraceDirName :: FilePath
buildTraceDirName = "build-trace-v2"

-- | The directory of build logs (local-binary-cache-store.cc:113).
logDirName :: FilePath
logDirName = "log"

-- | Read a secret signing key from a file, in the form
-- @nix-store --generate-binary-cache-key@ writes: @name:base64@, the
-- base64 of the 64-byte Ed25519 secret key, its seed and then the seed's
-- public key.  A key whose second half is not its seed's public key is
-- refused, before anything is written: upstream's signer, libsodium's,
-- signs with that half as the public key, so its signatures verify under
-- no key, where nova-cache's, which derives the public key from the
-- seed, would verify under one the key file does not hold.  No message
-- carries the key's bytes.
loadSecretKeyFile :: FilePath -> IO (Either Text SecretKey)
loadSecretKeyFile path = (>>= parse) <$> loadKeyFile path
  where
    parse text = do
      key <- first refused (Signing.parseSecretKey text)
      derived <- first refused (Signing.toPublicKey key)
      key <$ unless (Signing.pkBytes derived == BS.drop ed25519SeedBytes (Signing.skBytes key)) (Left (refused "its second half is not the public key of its seed"))
    refused err = "secret key file " <> T.pack path <> ": " <> T.pack err

-- | The bytes of an Ed25519 seed, the first half of a secret key: the
-- @ed25519SeedSize@ of nova-cache 0.13.0.0 (NovaCache/Signing.hs:77-78),
-- which it does not export.
ed25519SeedBytes :: Int
ed25519SeedBytes = 32

-- | The narinfo with the key's signature added to those it carries, which
-- upstream keeps as a sorted set (path-info.cc:38-41).
signNarInfo :: SecretKey -> NarInfo -> Either Text NarInfo
signNarInfo key narInfo = do
  sig <- first (\err -> "cannot sign the narinfo of " <> niStorePath narInfo <> ": " <> T.pack err) (Signing.sign key narInfo)
  pure narInfo {niSigs = Set.toAscList (Set.insert sig (Set.fromList (niSigs narInfo)))}

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

-- | What a push writes as @nix-cache-info@ in a directory that has none:
-- the store directory alone, as upstream writes it
-- (binary-cache-store.cc:46).  The narinfos name paths in the canonical
-- store directory whatever directory the local store is in, so this is
-- @\/nix\/store@ on every host.
cacheInfoText :: BS.ByteString
cacheInfoText = storeDirField <> ": " <> TE.encodeUtf8 defaultStoreDirText <> "\n"

-- | Refuse a @nix-cache-info@ that names a store directory other than
-- @\/nix\/store@, as upstream refuses it, in its words
-- (binary-cache-store.cc:48-63): each line with a colon is a field, its
-- name what comes before the first colon and its value the rest trimmed of
-- spaces, tabs, carriage returns and newlines.  Other fields are not
-- read; a push writes nothing they describe.
checkCacheInfo :: FilePath -> BS.ByteString -> Either Text ()
checkCacheInfo dir contents = case find (/= canonical) storeDirs of
  Nothing -> Right ()
  Just other ->
    Left
      ( "binary cache '"
          <> T.pack (fileScheme <> dir)
          <> "' is for Nix stores with prefix '"
          <> TE.decodeUtf8With TE.lenientDecode other
          <> "', not '"
          <> defaultStoreDirText
          <> "'"
      )
  where
    canonical = TE.encodeUtf8 defaultStoreDirText
    storeDirs =
      [ trim (BS.drop 1 rest)
      | line <- BC.split '\n' contents,
        let (name, rest) = BC.break (== ':') line,
        not (BS.null rest),
        name == storeDirField
      ]
    trim = BC.dropWhile isTrimmed . BC.dropWhileEnd isTrimmed
    isTrimmed c = c `elem` (" \t\r\n" :: String)

-- | Write @nix-cache-info@ into a directory that has none, or check the
-- one it has.
prepareCacheInfo :: FilePath -> IO (Either Text ())
prepareCacheInfo dir = do
  existing <- tryJust (guard . isDoesNotExistError) (BS.readFile file)
  case existing of
    Left () -> Right () <$ writeCacheFile file cacheInfoText
    Right contents -> pure (checkCacheInfo dir contents)
  where
    file = dir </> cacheInfoFileName

-- ---------------------------------------------------------------------------
-- Writing files
-- ---------------------------------------------------------------------------

-- | Write a file of the cache unless one is there under its name.
writeUnlessPresent :: FilePath -> BS.ByteString -> IO ()
writeUnlessPresent target bytes = do
  present <- doesFileExist target
  unless present (writeCacheFile target bytes)

-- | Write a file of the cache: whole, into a new file beside it, which is
-- then renamed onto it.  The new file has the permissions the umask
-- leaves, as upstream's @writeFile@ creates one (0666, file-system.hh:134
-- at 2.24.9), where @openBinaryTempFile@ would leave it
-- readable by its owner alone and so unreadable to a web server running
-- as another user.  Its name starts with a dot, which static servers
-- commonly hide.
writeCacheFile :: FilePath -> BS.ByteString -> IO ()
writeCacheFile target bytes =
  bracketOnError
    (openBinaryTempFileWithDefaultPermissions (takeDirectory target) ('.' : takeFileName target <> tempSuffix))
    discardTemp
    ( \(temp, handle) -> do
        BS.hPut handle bytes
        hClose handle
        renameFile temp target
    )

-- | The extension of a file being written, before its rename.
tempSuffix :: String
tempSuffix = ".tmp"

-- | Close and remove a file whose write failed.  An error doing so is
-- dropped, so it never replaces the failure that brought it here.
discardTemp :: (FilePath, Handle) -> IO ()
discardTemp (temp, handle) = do
  ignoringIOErrors (hClose handle)
  ignoringIOErrors (removeFile temp)
  where
    ignoringIOErrors action = action `catch` \(_ :: IOException) -> pure ()
