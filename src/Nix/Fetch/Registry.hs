-- | Flake registries, as upstream 2.36.0 reads and consults them
-- (registry.cc): the entries of a registry file, the registries in the
-- order a lookup asks them, and the lookup that resolves an indirect
-- input, such as the @nixpkgs@ in @nixpkgs#hello@, to the input an entry
-- maps it to.
--
-- Reading a registry and looking an input up are pure.  The edge reads
-- each registry's file, or downloads the global one, and hands the bytes
-- over ('readRegistries').
module Nix.Fetch.Registry
  ( -- * Registries
    RegistryType (..),
    RegistryEntry (..),
    Registry (..),
    parseRegistry,

    -- * Lookups
    RegistryUse (..),
    lookupInRegistries,
    resolveFlakeRef,

    -- * Listing
    registryListLines,

    -- * Reading
    GlobalRegistry (..),
    globalRegistryFrom,
    RegistryLocations (..),
    registryFileName,
    readRegistries,
  )
where

import Control.Exception (IOException, displayException, try)
import Control.Monad (unless, when)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Either (fromRight)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Nix.Eval.Policy (PathRules)
import Nix.Fetch.Attrs (AttrValue (..), Attrs, attrsToQuery, maybeStrAttr)
import Nix.Fetch.Download (downloadBytes, urlSource)
import Nix.Fetch.Input (Input (..), InputContext (..), applyOverrides, inputContains, inputFromAttrs, inputIsDirect, inputRef, inputRevision, inputToUrlString, sameInput)
import Nix.Fetch.Url (Checked, bytesToTextLossy, refuse, runChecked)
import Nix.Flake.Ref (FlakeRef (..))
import Nix.Http (FetchRetryPolicy)
import Nix.Json (Json (..), parseJson)
import Nix.Logger (Logger)
import System.Directory (canonicalizePath, doesPathExist)
import System.FilePath (isAbsolute)
import System.OsPath (OsPath)
import qualified System.OsPath as OP

-- ---------------------------------------------------------------------------
-- Registries
-- ---------------------------------------------------------------------------

-- | Where a registry comes from, in the order a lookup asks them.
data RegistryType
  = -- | The entries @--override-flake@ adds for one run.
    FlagRegistry
  | -- | The user's @registry.json@.
    UserRegistry
  | -- | The machine's @registry.json@.
    SystemRegistry
  | -- | The registry @flake-registry@ names.
    GlobalRegistry
  deriving (Eq, Show)

-- | An entry: the input it maps, the one it maps it to, the attributes
-- the reference takes beside that input (a @dir@), and whether only an
-- input equal to its own matches, rather than one that may also carry a
-- @ref@ and a revision.
data RegistryEntry = RegistryEntry
  { entryFrom :: !Input,
    entryTo :: !Input,
    entryExtraAttrs :: !Attrs,
    entryExact :: !Bool
  }
  deriving (Eq, Show)

-- | A registry's entries, in the order its file lists them.
data Registry = Registry
  { registryType :: !RegistryType,
    registryEntries :: ![RegistryEntry]
  }
  deriving (Eq, Show)

-- | Why reading a registry stopped: a value of the wrong JSON shape, which
-- upstream's JSON library reports, or something upstream reports itself.
data ReadStop = JsonStop !Text | NixStop !Text

-- | A registry file's entries, as upstream's @Registry::read@ reads them,
-- and the warning reading gives.  The file is version 2, an object whose
-- @flakes@ lists entries of @from@ and @to@ attributes and an optional
-- @exact@; a @dir@ among the @to@ attributes is the reference's, not the
-- input's.  A file that fails to read leaves the entries before the
-- failure, with upstream's warning naming the file: @cannot parse@ for
-- JSON of the wrong shape, and @cannot read@ for any other failure.
parseRegistry :: InputContext -> RegistryType -> Text -> ByteString -> (Registry, [Text])
parseRegistry ctx kind shown bytes = case readEntries of
  (entries, Nothing) -> (Registry kind (reverse entries), [])
  (entries, Just stop) -> (Registry kind (reverse entries), [stopWarning stop])
  where
    stopWarning stop = case stop of
      JsonStop reason -> "cannot parse flake registry '" <> shown <> "': " <> reason
      NixStop reason -> "cannot read flake registry '" <> shown <> "': " <> reason
    readEntries = case TE.decodeUtf8' bytes of
      Left _ -> ([], Just (JsonStop "the file is not UTF-8"))
      Right text -> case parseJson text of
        Left reason -> ([], Just (JsonStop reason))
        Right json -> case versionOf json of
          Left stop -> ([], Just stop)
          Right version
            | version /= registryVersion -> ([], Just (NixStop ("flake registry '" <> shown <> "' has unsupported version " <> T.pack (show version))))
            | otherwise -> case flakesOf json of
                Left stop -> ([], Just stop)
                Right items -> collect [] items
    collect done items = case items of
      [] -> (done, Nothing)
      item : rest -> case entryOf ctx item of
        Left stop -> (done, Just stop)
        Right entry -> collect (entry : done) rest

-- | The registry file format version upstream reads.
registryVersion :: Integer
registryVersion = 2

-- | The file's @version@, as nlohmann's @value("version", 0)@ reads it: a
-- number, 0 when there is none, from an object.
versionOf :: Json -> Either ReadStop Integer
versionOf json = case json of
  JsonObject members -> case Map.lookup "version" members of
    Nothing -> Right 0
    Just (JsonInt n) -> Right n
    Just (JsonFloat d) -> Right (truncate d)
    Just other -> Left (JsonStop (typeMustBe "number" other))
  other -> Left (JsonStop ("[json.exception.type_error.306] cannot use value() with " <> jsonTypeName other))

-- | The values @flakes@ holds, as a range over nlohmann's value of it
-- gives them: none for a missing or null one, an array's elements, an
-- object's values in name order, and any other value itself.
flakesOf :: Json -> Either ReadStop [Json]
flakesOf json = case json of
  JsonObject members -> Right $ case Map.lookup "flakes" members of
    Nothing -> []
    Just JsonNull -> []
    Just (JsonArray items) -> items
    Just (JsonObject entries) -> Map.elems entries
    Just other -> [other]
  _ -> Right []

-- | One entry, read in upstream's order: the @to@ attributes, the @from@
-- input, the @to@ input without its @dir@, then @exact@.
entryOf :: InputContext -> Json -> Either ReadStop RegistryEntry
entryOf ctx item = do
  members <- case item of
    JsonObject members -> Right members
    JsonNull -> Right Map.empty
    other -> Left (JsonStop ("[json.exception.type_error.305] cannot use operator[] with a string argument with " <> jsonTypeName other))
  let field name = fromMaybe JsonNull (Map.lookup name members)
  toAttrs <- attrsOf (field "to")
  from <- checked (attrsOf (field "from") >>= Right . inputFromAttrs ctx)
  to <- checked (Right (inputFromAttrs ctx (Map.delete dirKey toAttrs)))
  exact <- case Map.lookup "exact" members of
    Nothing -> Right False
    Just (JsonBool b) -> Right b
    Just other -> Left (JsonStop (typeMustBe "boolean" other))
  pure (RegistryEntry from to (maybe Map.empty (Map.singleton dirKey) (Map.lookup dirKey toAttrs)) exact)
  where
    checked = (>>= first NixStop . fst . runChecked)

-- | Attributes from JSON, as upstream's @jsonToAttrs@ reads them: each
-- item a number, read as an unsigned 64-bit integer, a string or a
-- Boolean.  An array's items are keyed by their index, and a value that is
-- neither an array nor an object is one item keyed by the empty string, as
-- nlohmann's @items()@ gives them.
attrsOf :: Json -> Either ReadStop Attrs
attrsOf json = Map.fromList <$> traverse attr (itemsOf json)
  where
    itemsOf value = case value of
      JsonObject members -> Map.toList members
      JsonArray elements -> zip (map (T.pack . show) [0 :: Int ..]) elements
      JsonNull -> []
      other -> [("", other)]
    attr (name, value) = case value of
      JsonInt n -> Right (name, AttrInt (fromInteger n))
      JsonFloat d -> Right (name, AttrInt (truncate d))
      JsonString s -> Right (name, AttrString (TE.encodeUtf8 s))
      JsonBool b -> Right (name, AttrBool b)
      _ -> Left (NixStop "unsupported input attribute type in lock file")

-- | nlohmann's refusal of a value it cannot convert.
typeMustBe :: Text -> Json -> Text
typeMustBe wanted value = "[json.exception.type_error.302] type must be " <> wanted <> ", but is " <> jsonTypeName value

-- | A JSON value's type, as nlohmann names it.
jsonTypeName :: Json -> Text
jsonTypeName value = case value of
  JsonNull -> "null"
  JsonBool _ -> "boolean"
  JsonInt _ -> "number"
  JsonFloat _ -> "number"
  JsonString _ -> "string"
  JsonArray _ -> "array"
  JsonObject _ -> "object"

-- | The attribute of a reference's subdirectory.
dirKey :: Text
dirKey = "dir"

-- ---------------------------------------------------------------------------
-- Lookups
-- ---------------------------------------------------------------------------

-- | Which registries a lookup asks: all of them, or only the flag and
-- global registries, as upstream's @UseRegistries::Limited@ does for a
-- lock file's inputs.
data RegistryUse = AllRegistries | LimitedRegistries
  deriving (Eq, Show)

-- | How many entries a lookup follows before it calls the registries a
-- cycle.
lookupBound :: Int
lookupBound = 100

-- | Resolve an input against the registries, as upstream's
-- @lookupInRegistries@ does: the first entry, registry by registry and in
-- each in file order, that matches the input replaces it, and the lookup
-- starts again from the first registry, until none matches.  An exact
-- entry matches only an input equal to its own.  Another matches an input
-- that is its own or its own with a @ref@ and a revision, and passes on
-- each of those the entry does not set itself.  The attributes of the
-- last entry followed come back with the input.  An input still indirect
-- at the end is not in the registries.
lookupInRegistries :: PathRules -> RegistryUse -> [Registry] -> Input -> Checked (Input, Attrs)
lookupInRegistries rules use registries = go 1 Map.empty
  where
    asked = [registry | registry <- registries, use == AllRegistries || registryType registry `elem` [FlagRegistry, GlobalRegistry]]
    go :: Int -> Attrs -> Input -> Checked (Input, Attrs)
    go n extra input = do
      when (n > lookupBound) $ shown input >>= \text -> refuse ("cycle detected in flake registry for '" <> text <> "'")
      case [entry | registry <- asked, entry <- registryEntries registry, matches entry input] of
        entry : _
          | entryExact entry -> go (n + 1) (entryExtraAttrs entry) (entryTo entry)
          | otherwise -> do
              ref <- carried inputRef entry input
              rev <- carried inputRevision entry input
              resolved <- applyOverrides rules ref rev (entryTo entry)
              go (n + 1) (entryExtraAttrs entry) resolved
        [] -> do
          unless (inputIsDirect input) $ shown input >>= \text -> refuse ("cannot find flake '" <> text <> "' in the flake registries")
          pure (input, extra)
    matches entry input
      | entryExact entry = sameInput (entryFrom entry) input
      | otherwise = inputContains (entryFrom entry) input
    carried get entry input = do
      own <- get (entryFrom entry)
      given <- get input
      pure (maybe given (const Nothing) own)
    shown = fmap bytesToTextLossy . inputToUrlString rules []

-- | A flake reference resolved against the registries, as upstream's
-- @FlakeRef::resolve@ resolves one: its input looked up, and its
-- subdirectory the @dir@ the entry gives, or its own.
resolveFlakeRef :: PathRules -> RegistryUse -> [Registry] -> FlakeRef -> Checked FlakeRef
resolveFlakeRef rules use registries (FlakeRef input subdir) = do
  (resolved, extra) <- lookupInRegistries rules use registries input
  dir <- maybeStrAttr dirKey extra
  pure (FlakeRef resolved (fromMaybe subdir dir))

-- ---------------------------------------------------------------------------
-- Listing
-- ---------------------------------------------------------------------------

-- | What @registry list@ prints of the registries: a line for each entry,
-- its registry's name padded to six columns, then the URLs of its input and
-- of the one it maps it to, with the entry's attributes as parameters.
registryListLines :: PathRules -> [Registry] -> Checked [ByteString]
registryListLines rules registries =
  sequence
    [ line (registryType registry) entry
    | registry <- registries,
      entry <- registryEntries registry
    ]
  where
    line kind entry = do
      from <- inputToUrlString rules [] (entryFrom entry)
      to <- inputToUrlString rules (Map.toList (attrsToQuery (entryExtraAttrs entry))) (entryTo entry)
      pure (BS.intercalate " " [label kind, from, to])
    label kind = case kind of
      FlagRegistry -> "flags "
      UserRegistry -> "user  "
      SystemRegistry -> "system"
      GlobalRegistry -> "global"

-- ---------------------------------------------------------------------------
-- Reading
-- ---------------------------------------------------------------------------

-- | Where the global registry is, as the @flake-registry@ setting names
-- it: nowhere when the setting is empty, a file when it is an absolute
-- path, and otherwise a URL to download.
data GlobalRegistry
  = NoGlobalRegistry
  | GlobalRegistryFile !FilePath
  | GlobalRegistryUrl !Text
  deriving (Eq, Show)

-- | The global registry the setting's value names.
globalRegistryFrom :: ByteString -> GlobalRegistry
globalRegistryFrom value
  | BS.null value = NoGlobalRegistry
  | isAbsolute (BC.unpack value) = GlobalRegistryFile (BC.unpack value)
  | otherwise = GlobalRegistryUrl (bytesToTextLossy value)

-- | The name of the user's and the machine's registry files, in the
-- directories their @nix.conf@ files are in.
registryFileName :: OsPath
registryFileName = OP.pack (map OP.unsafeFromChar "registry.json")

-- | Where the user's and the machine's registries are, and the global one.
data RegistryLocations = RegistryLocations
  { userRegistryFile :: !FilePath,
    systemRegistryFile :: !(Maybe FilePath),
    globalRegistry :: !GlobalRegistry
  }
  deriving (Eq, Show)

-- | The registries a lookup asks, in its order, and the warnings reading
-- them gave, or why the global one could not be downloaded.  A file that
-- is not there is an empty registry, and a file is named in a warning by
-- its path with its symlinks resolved, as upstream names it.  There is no
-- flag registry until a flag adds one.
readRegistries :: Logger -> FetchRetryPolicy -> InputContext -> RegistryLocations -> IO (Either Text ([Registry], [Text]))
readRegistries logger policy ctx locations = do
  user <- fromFile UserRegistry (Just (userRegistryFile locations))
  system <- fromFile SystemRegistry (systemRegistryFile locations)
  global <- case globalRegistry locations of
    NoGlobalRegistry -> pure (Right (Registry GlobalRegistry [], []))
    GlobalRegistryFile path -> Right <$> fromFile GlobalRegistry (Just path)
    GlobalRegistryUrl url -> fmap (parseRegistry ctx GlobalRegistry url) <$> downloadBytes logger policy url (urlSource url)
  pure $ do
    (globalEntries, globalWarnings) <- global
    let (registries, warnings) = unzip [user, system]
    pure (registries ++ [globalEntries], concat warnings ++ globalWarnings)
  where
    fromFile kind = maybe (pure (Registry kind [], [])) $ \path -> do
      there <- doesPathExist path
      if not there
        then pure (Registry kind [], [])
        else do
          resolved <- fromRight path <$> (try (canonicalizePath path) :: IO (Either IOException FilePath))
          contents <- try (BS.readFile resolved) :: IO (Either IOException ByteString)
          pure $ case contents of
            Left err -> (Registry kind [], ["cannot read flake registry '" <> T.pack resolved <> "': " <> T.pack (displayException err)])
            Right bytes -> parseRegistry ctx kind (T.pack resolved) bytes
