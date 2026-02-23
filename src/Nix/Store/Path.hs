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
    windowsStoreDir,

    -- * Store paths
    StorePath (..),
    storePathToFilePath,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath ((</>))

-- | The base directory of the Nix store.
newtype StoreDir = StoreDir {unStoreDir :: FilePath}
  deriving (Eq, Show)

-- | Default store directory on Unix: @\/nix\/store@.
defaultStoreDir :: StoreDir
defaultStoreDir = StoreDir "/nix/store"

-- | Default store directory as 'Text', for use in the evaluator.
defaultStoreDirText :: Text
defaultStoreDirText = T.pack (unStoreDir defaultStoreDir)

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
