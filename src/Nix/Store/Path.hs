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
-- * Concurrent installs (no file conflicts - different hashes = different dirs)
-- * Garbage collection (delete unreferenced paths, everything else stays)
-- * Binary substitution (if hash matches, the build output is identical)
--
-- == References
--
-- A store path can REFERENCE other store paths.  For example, a compiled
-- binary references its shared libraries, its interpreter, etc.  Nix
-- scans the output for store path strings to discover these references
-- automatically.  The reference graph is what the garbage collector
-- follows - anything reachable from a GC root is kept.
module Nix.Store.Path
  ( -- * Store directory
    StoreDir (..),
    defaultStoreDir,
    defaultStoreDirText,
    platformStoreDir,
    platformStoreDirText,
    windowsStoreDir,

    -- * Store paths
    StorePath (spHash, spName),
    storePathToFilePath,
    storePathToText,
    isCanonicalStoreText,
    storeTextToFilePath,
    parseStorePath,
    parseStorePathBaseName,

    -- * Name validation
    StorePathNameError (..),
    StorePathNameReason (..),
    checkStorePathName,
    validStorePathName,
    storePathNameErrorText,
    storePathNameReasonText,

    -- * Constants
    storePathHashLen,
  )
where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Nix.Store.Path.Internal (StorePath (..))
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

-- | Platform-appropriate store directory.
-- Returns @C:\\nix\\store@ on Windows, @\/nix\/store@ on Unix.
-- Use this for filesystem operations and operator-facing output.
-- Use 'defaultStoreDir' for identity - hashing, ATerm text, and every
-- eval-visible store-path string (including @builtins.storeDir@).
platformStoreDir :: StoreDir
platformStoreDir = case System.Info.os of
  "mingw32" -> windowsStoreDir
  _ -> defaultStoreDir

-- | Platform-appropriate store directory as 'Text'.
-- Returns @C:\\nix\\store@ on Windows, @\/nix\/store@ on Unix.
platformStoreDirText :: Text
platformStoreDirText = T.pack (unStoreDir platformStoreDir)

-- | Default store directory on Windows: @C:\\nix\\store@.
windowsStoreDir :: StoreDir
windowsStoreDir = StoreDir "C:\\nix\\store"

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

-- | Whether path text lies under the canonical store dir: exactly
-- @\/nix\/store@, or @\/nix\/store@ followed by a separator of either
-- spelling.  This is the spelling writers put into eval-visible values.
isCanonicalStoreText :: Text -> Bool
isCanonicalStoreText txt = case T.stripPrefix defaultStoreDirText txt of
  Just rest -> case T.uncons rest of
    Nothing -> True
    Just (c, _) -> c == '/' || c == '\\'
  Nothing -> False

-- | Resolve path-value text to the filesystem location it names: text
-- under the canonical store dir resolves into the platform store dir,
-- and every other path is taken as written.  Writers emit the canonical
-- spelling into eval values, so every reader that performs IO on a path
-- value must resolve through this - on Windows the rooted @\/nix@ prefix
-- would otherwise resolve against the working drive.  On Unix the two
-- dirs coincide and this is 'T.unpack'.
storeTextToFilePath :: Text -> FilePath
storeTextToFilePath txt
  | isCanonicalStoreText txt =
      unStoreDir platformStoreDir <> T.unpack (T.drop (T.length defaultStoreDirText) txt)
  | otherwise = T.unpack txt

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
      tryWithSep sep = T.stripPrefix (dirText <> sep) path >>= parseStorePathBaseName
   in case tryWithSep "/" of
        Just sp -> Just sp
        Nothing -> tryWithSep "\\"

-- | Parse a store path basename like @abc...-name@ - the form narinfo
-- @References@ and @Deriver@ fields carry on the wire - into a
-- 'StorePath': 32 nix-base32 hash chars, dash, valid non-empty name.
--
-- Both components are charset-checked, as upstream does at its parse
-- boundary: parsed text can come from a cache, and an unchecked
-- component (separators, dots, drive colons in the hash or name slot)
-- would later become a filesystem path via 'storePathToFilePath' that
-- escapes the store root.
parseStorePathBaseName :: Text -> Maybe StorePath
parseStorePathBaseName basename
  | T.length basename < storePathHashLen + 2 = Nothing
  | otherwise =
      let hashPart = T.take storePathHashLen basename
          afterHash = T.drop storePathHashLen basename
       in case T.uncons afterHash of
            Just ('-', name)
              | T.all isNixBase32Char hashPart && validStorePathName name ->
                  Just (StorePath hashPart name)
            _ -> Nothing

-- | The nix-base32 alphabet: @0-9a-z@ without @e o u t@ (chosen upstream
-- to avoid accidental words).  Hash components may contain nothing else.
isNixBase32Char :: Char -> Bool
isNixBase32Char c =
  isDigit c
    || (isAsciiLower c && c /= 'e' && c /= 'o' && c /= 'u' && c /= 't')

-- | A rejected store-path name: the name itself plus the first rule it
-- broke, so every boundary reports the same diagnosis.
data StorePathNameError = StorePathNameError
  { -- | The rejected name.
    spneName :: !Text,
    -- | The rule it broke.
    spneReason :: !StorePathNameReason
  }
  deriving (Eq, Show)

-- | The store-path name rules, one constructor per rule.
data StorePathNameReason
  = -- | The name is empty.
    NameEmpty
  | -- | The name exceeds 'maxStorePathNameLen'; carries the actual length.
    NameTooLong !Int
  | -- | The name contains a character outside @[A-Za-z0-9+._?=-]@.
    NameIllegalChar !Char
  | -- | The first dash-separated component is the carried @.@ or @..@,
    -- which would name a dot segment on disk.
    NameDotSegment !Text
  deriving (Eq, Show)

-- | Upstream's store path name rules (its checkName boundary):
-- 1-211 characters from @[A-Za-z0-9+._?=-]@, and the first dash-separated
-- component may not be @.@ or @..@.  One rule rejects the traversal names
-- and their @.-@ / @..-@ prefixed forms alike, while other dot-leading
-- names (@.config-1.0@) stay valid.
--
-- Enforced at BOTH boundaries: parse ('parseStorePathBaseName') and
-- construction (the @makeStorePath@ family in "Nix.Hash"), so an unclean
-- name cannot become a 'StorePath' from either side - in particular, no
-- write sink can be handed a path that resolves outside the store root.
checkStorePathName :: Text -> Either StorePathNameError ()
checkStorePathName name
  | T.null name = broke NameEmpty
  | T.length name > maxStorePathNameLen = broke (NameTooLong (T.length name))
  | firstDashComponent == "." || firstDashComponent == ".." = broke (NameDotSegment firstDashComponent)
  | Just c <- T.find (not . isStorePathNameChar) name = broke (NameIllegalChar c)
  | otherwise = Right ()
  where
    broke = Left . StorePathNameError name
    firstDashComponent = T.takeWhile (/= '-') name
    isStorePathNameChar c =
      isAsciiLower c
        || isAsciiUpper c
        || isDigit c
        || c == '+'
        || c == '.'
        || c == '_'
        || c == '?'
        || c == '='
        || c == '-'

-- | Boolean form of 'checkStorePathName', for the parse boundary.
validStorePathName :: Text -> Bool
validStorePathName = either (const False) (const True) . checkStorePathName

-- | Render a rejection as @invalid store path name '<name>': <rule>@.
storePathNameErrorText :: StorePathNameError -> Text
storePathNameErrorText (StorePathNameError name reason) =
  "invalid store path name '" <> name <> "': " <> storePathNameReasonText reason

-- | Render just the broken rule, for callers that frame the name
-- themselves (e.g. @invalid derivation output name@).
storePathNameReasonText :: StorePathNameReason -> Text
storePathNameReasonText reason = case reason of
  NameEmpty -> "the name is empty"
  NameTooLong len ->
    "the name is "
      <> T.pack (show len)
      <> " characters, above the "
      <> T.pack (show maxStorePathNameLen)
      <> " maximum"
  NameIllegalChar c -> "contains the illegal character " <> T.pack (show c)
  NameDotSegment seg -> "the first dash-separated component may not be '" <> seg <> "'"

-- | Upstream's maximum store path name length.
maxStorePathNameLen :: Int
maxStorePathNameLen = 211
