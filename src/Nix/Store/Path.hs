-- | Nix store path types and computation.
--
-- == The Nix store model
--
-- The Nix store is a flat directory of immutable, content-addressed
-- packages.  Every entry looks like:
--
-- @\/nix\/store\/\<hash\>-\<name\>@
--
-- On Windows, this becomes:
--
-- @C:\\nix\\store\\\<hash\>-\<name\>@
--
-- The store is IMMUTABLE.  Once a path is registered, it never changes.
-- This is enforced by making the store directory read-only after builds.
-- This immutability is what enables:
--
-- * Atomic upgrades (install new version, switch symlink, done)
-- * Rollbacks (old version still in store, just switch symlink back)
-- * Concurrent installs (no file conflicts — different hashes = different dirs)
-- * Garbage collection (delete unreferenced paths, everything else stays)
-- * Binary substitution (if hash matches, the build output is identical)
--
-- == References
--
-- A store path can REFERENCE other store paths.  For example, a compiled
-- binary references its shared libraries, its interpreter, etc.  Nix
-- scans the output for store path strings to discover these references
-- automatically.  The reference graph is what the garbage collector
-- follows — anything reachable from a GC root is kept.
module Nix.Store.Path
  ( -- * Store directory
    StoreDir (..),
    defaultStoreDir,
    defaultStoreDirText,
    platformStoreDirText,
    windowsStoreDir,

    -- * Store paths
    StorePath (..),
    storePathToFilePath,
    storePathToText,
    parseStorePath,

    -- * Constants
    storePathHashLen,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath ((</>))
import qualified System.Info

-- | The base directory of the Nix store.
newtype StoreDir = StoreDir {unStoreDir :: FilePath}
  deriving (Eq, Show)

-- | Default store directory on Unix: @\/nix\/store@.
defaultStoreDir :: StoreDir
defaultStoreDir = StoreDir "/nix/store"

-- | Default store directory as 'Text', for use in the evaluator.
defaultStoreDirText :: Text
defaultStoreDirText = T.pack (unStoreDir defaultStoreDir)

-- | Platform-appropriate store directory as 'Text'.
-- Returns @C:\\nix\\store@ on Windows, @\/nix\/store@ on Unix.
-- Used for user-facing values like @builtins.storeDir@.
platformStoreDirText :: Text
platformStoreDirText = T.pack (unStoreDir platformStoreDir)
  where
    platformStoreDir = case System.Info.os of
      "mingw32" -> windowsStoreDir
      _ -> defaultStoreDir

-- | Default store directory on Windows: @C:\\nix\\store@.
windowsStoreDir :: StoreDir
windowsStoreDir = StoreDir "C:\\nix\\store"

-- | A parsed store path: the hash and name components.
data StorePath = StorePath
  { -- | The 32-character Nix base-32 hash.
    spHash :: !Text,
    -- | The human-readable name (e.g. @hello-2.12.1@).
    spName :: !Text
  }
  deriving (Eq, Ord, Show)

-- | Convert a 'StorePath' to a full filesystem path under a 'StoreDir'.
storePathToFilePath :: StoreDir -> StorePath -> FilePath
storePathToFilePath (StoreDir dir) sp =
  dir </> T.unpack (spHash sp <> "-" <> spName sp)

-- | Convert a 'StorePath' to canonical 'Text' with forward slashes.
-- Unlike 'storePathToFilePath', this always uses @\/@ as the separator,
-- making it safe for ATerm serialization and cross-platform round-trips.
storePathToText :: StoreDir -> StorePath -> Text
storePathToText (StoreDir dir) sp =
  T.pack dir <> "/" <> spHash sp <> "-" <> spName sp

-- | Length of the Nix base-32 hash component in store paths (32 chars).
storePathHashLen :: Int
storePathHashLen = 32

-- | Parse a full store path string like @\/nix\/store\/abc...-name@ into
-- a 'StorePath'.  Returns 'Nothing' if the path doesn't match the
-- expected format: store dir prefix + separator + 32-char hash + dash + name.
-- Accepts both @\/@ and @\\@ as the separator after the store dir,
-- so paths round-trip correctly regardless of which OS serialized them.
parseStorePath :: StoreDir -> Text -> Maybe StorePath
parseStorePath (StoreDir dir) path =
  let dirText = T.pack dir
      tryWithSep sep = T.stripPrefix (dirText <> sep) path >>= parseRest
   in case tryWithSep "/" of
        Just sp -> Just sp
        Nothing -> tryWithSep "\\"
  where
    parseRest rest
      | T.length rest < storePathHashLen + 2 = Nothing
      | otherwise =
          let hashPart = T.take storePathHashLen rest
              afterHash = T.drop storePathHashLen rest
           in case T.uncons afterHash of
                Just ('-', name)
                  | not (T.null name) -> Just (StorePath hashPart name)
                _ -> Nothing
