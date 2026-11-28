{-# LANGUAGE ScopedTypeVariables #-}

-- | Push store paths to a binary cache in a local directory, as upstream's
-- @nix copy --to file:\/\/\/DIR@ writes one, signing each narinfo with a
-- secret key read here rather than leaving the signing to a server.
--
-- == Layout
--
-- The directory gets the layout upstream's @file:\/\/@ store writes and its
-- HTTP substituter reads, so any static web server or file share can serve
-- it, and nova-nix's substituter reads it through a @file:\/\/@ URL too.
-- The push opens it first as either reader opens it ("Nix.CacheDirectory"),
-- which makes its directories and its @nix-cache-info@, then makes the
-- realisations directory under its older name too, and then writes:
--
-- * @\<hash\>.narinfo@ for each path, its fields in the order upstream
--   writes them.
-- * The NAR at the narinfo's @URL@, @nar\/\<filehash\>.nar@, or @.nar.zst@
--   compressed with zstd, named by the nix-base32 hash of the file, as
--   upstream names it.
--
-- Each narinfo carries an Ed25519 signature by the secret key, as
-- upstream's binary cache store signs every narinfo it writes with the key
-- its @secret-key@ setting names: over upstream's fingerprint of the path,
-- as @name:base64@.  nova-cache computes both.  Its verification of that
-- fingerprint accepts cache.nixos.org's signatures, and Ed25519 signing is
-- deterministic, so with a key whose second half is the public key of its
-- seed, the only keys 'loadSecretKeyFile' accepts, a signature here is the
-- one upstream's signer makes.
--
-- == Writes
--
-- A path whose narinfo is in the directory is skipped, as upstream takes
-- it to be in the cache, and a NAR whose file is there is not written
-- again.  Every NAR is written before any narinfo, and the narinfos in the
-- closure's order, as "Nix.Push" uploads them, so no narinfo names a NAR or
-- a reference that is not there yet.  Each file is written whole beside its
-- name and renamed onto it ('writeCacheFile'), so a reader never meets a
-- torn one.
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
    signNarInfo,
  )
where

import Control.Exception (displayException)
import Control.Monad (filterM, forM, forM_, unless)
import Control.Monad.Except (ExceptT (..), liftEither, runExceptT, withExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.List (isPrefixOf)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Nix.CacheDirectory (cacheOpenFailureMessage, directoryUrl, fileUrlDirectory, openCacheDirectory, realisationsDirName, writeCacheFile)
import Nix.Http (catchSync)
import Nix.Push (PushArtifact (..), PushCompression, PushSummary (..), computeClosure, loadKeyFile, logNar, logPushed, logSkipped, narInfoFileName, preparePath)
import Nix.Store (Store (..))
import Nix.Store.Path (StorePath)
import NovaCache.NarInfo (NarInfo (..), renderNarInfo)
import NovaCache.Signing (SecretKey)
import qualified NovaCache.Signing as Signing
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))

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

-- | The schemes of a nova-cache server's URL.
serverSchemes :: [String]
serverSchemes = ["http://", "https://"]

-- | What @push --cache URL@ writes to, given the files @--key-file@ and
-- @--secret-key-file@ name.  An @http:\/\/@ or @https:\/\/@ URL is a
-- nova-cache server, which signs what it receives, so it takes no secret
-- key.  A @file:\/\/@ URL is a directory ('fileUrlDirectory'), which takes
-- no API key and needs the secret key: nova-nix's substituter refuses a
-- narinfo with no signature, and so does upstream's unless @require-sigs@
-- is off or the cache is marked trusted.
pushTarget :: String -> Maybe FilePath -> Maybe FilePath -> Either Text PushTarget
pushTarget url apiKeyFile secretKeyFile
  | Just named <- fileUrlDirectory "sign and compress with --secret-key-file and --compression" url = do
      dir <- named
      case (apiKeyFile, secretKeyFile) of
        (Just _, _) -> Left "--key-file is a nova-cache server's API key; a file:// cache is signed with --secret-key-file"
        (Nothing, Nothing) -> Left "a file:// cache needs --secret-key-file, the secret key that signs its narinfos"
        (Nothing, Just keyFile) -> Right (DirectoryTarget dir keyFile)
  | any (`isPrefixOf` url) serverSchemes = case secretKeyFile of
      Just _ -> Left "--secret-key-file signs a file:// cache; a nova-cache server signs what it receives"
      Nothing -> Right (ServerTarget (T.dropWhileEnd (== '/') (T.pack url)) apiKeyFile)
  | otherwise = Left ("--cache takes the http:// or https:// URL of a nova-cache server, or a file:// URL of a directory, not " <> T.pack url)

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
-- propagates.  Its progress lines go to the store's logger.
pushToDirectory :: DirectoryPush -> Store -> [StorePath] -> IO (Either Text PushSummary)
pushToDirectory cfg store roots =
  -- Every synchronous exception, as for a server: the database's, the
  -- serialiser's, and the directory's IO errors are each a failed push.
  runExceptT run `catchSync` \e -> pure (Left (T.pack (displayException e)))
  where
    inCache relative = dpDirectory cfg </> T.unpack relative
    run = do
      _ <- withExceptT cacheOpenFailureMessage (ExceptT (openCacheDirectory (directoryUrl (dpDirectory cfg)) (dpDirectory cfg)))
      liftIO (createDirectoryIfMissing True (dpDirectory cfg </> realisationsDirName))
      closure <- ExceptT (computeClosure store roots)
      missing <- liftIO (filterM (fmap not . doesFileExist . inCache . narInfoFileName) closure)
      let skipped = length closure - length missing
      logSkipped (stLogger store) skipped
      -- NARs first, one at a time.  Each path's narinfo is signed and
      -- rendered as its NAR is written, and only the rendered bytes are
      -- kept, forced, so what the second phase holds reaches no NAR.
      written <- forM missing $ \sp -> do
        (artifact, narInfo) <- preparePath (dpCompression cfg) store sp
        logNar (stLogger store) sp artifact
        liftIO (writeUnlessPresent (inCache (niUrl narInfo)) (paBytes artifact))
        signed <- liftEither (signNarInfo (dpSecretKey cfg) narInfo)
        let !rendered = TE.encodeUtf8 (renderNarInfo signed)
        pure (sp, rendered)
      -- Then the narinfos, in the closure's deps-first order.
      forM_ written $ \(sp, rendered) -> do
        liftIO (writeCacheFile (inCache (narInfoFileName sp)) rendered)
        logPushed (stLogger store) sp
      pure PushSummary {psPushed = length written, psSkipped = skipped}

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
-- @ed25519SeedSize@ of nova-cache 0.13.0.0's @NovaCache.Signing@, which it
-- does not export.
ed25519SeedBytes :: Int
ed25519SeedBytes = 32

-- | The narinfo with the key's signature added to those it carries, which
-- upstream keeps as a sorted set (@ValidPathInfo::sign@ in path-info.cc).
signNarInfo :: SecretKey -> NarInfo -> Either Text NarInfo
signNarInfo key narInfo = do
  sig <- first (\err -> "cannot sign the narinfo of " <> niStorePath narInfo <> ": " <> T.pack err) (Signing.sign key narInfo)
  pure narInfo {niSigs = Set.toAscList (Set.insert sig (Set.fromList (niSigs narInfo)))}

-- ---------------------------------------------------------------------------
-- Writing files
-- ---------------------------------------------------------------------------

-- | Write a file of the cache unless one is there under its name.
writeUnlessPresent :: FilePath -> BS.ByteString -> IO ()
writeUnlessPresent target bytes = do
  present <- doesFileExist target
  unless present (writeCacheFile target bytes)
