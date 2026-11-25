-- | A profile's manifest, the @manifest.json@ every generation holds:
-- each element of the profile by its name, with the store paths it adds,
-- whether it is active and its priority, as upstream's @nix profile@
-- keeps it (@ProfileManifest@ in src\/nix\/profile.cc).
--
-- Versions 1 to 3 are read and version 3 is written, through
-- 'renderJson', which is nlohmann's @dump()@: members in name order,
-- each element's paths in store-path order, in the canonical
-- @\/nix\/store@ spelling.  An element a flake made keeps the fields it
-- was read with, where upstream parses its flake references and renders
-- them again.
--
-- Everything here is pure: reading the file, building the tree and
-- making the generation are "Nix.Profile.Generation"'s.
module Nix.Profile.Manifest
  ( -- * Manifests
    Manifest (..),
    Element (..),
    FlakeSource (..),
    emptyManifest,
    defaultPriority,

    -- * Reading and writing
    ManifestError (..),
    parseManifest,
    manifestJson,
    renderManifest,
    manifestErrorText,

    -- * Names
    elementIdentifier,
    flakeAttribute,
    drvNameName,
    addElement,

    -- * What a generation is made of
    manifestPackages,
    manifestReferences,

    -- * Selecting elements
    Selector (..),
    Matcher (..),
    regexMatcher,
    matcherTitle,
    Selection (..),
    selectElements,

    -- * Conflicts
    ConflictSide (..),
    Conflict (..),
    attributeConflict,
  )
where

import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (isAsciiLower, isAsciiUpper)
import Data.Foldable (toList)
import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Nix.Builder.Buildenv (Activity (..))
import Nix.Daemon.Protocol (OutputName (..), OutputsSpec (AllOutputs), namedOutputs, outputsSpecNames)
import Nix.HostPath (hostPathText)
import Nix.Json (Json (..), parseJson, renderJson)
import Nix.Store.Path (StoreDir, StorePath (spName), defaultStoreDir, parseStorePath, storePathBasename, storePathToFilePath, storePathToText)
import System.OsPath (OsPath)
import qualified Text.Regex.TDFA as RE
import Text.Regex.TDFA.ByteString ()

-- ---------------------------------------------------------------------------
-- Manifests
-- ---------------------------------------------------------------------------

-- | What a profile holds: its elements by name, in name order.
newtype Manifest = Manifest {manifestElements :: Map Text Element}
  deriving (Eq, Show)

-- | One thing a profile holds.
data Element = Element
  { -- | The store paths it adds to the profile's tree.
    elementPaths :: !(Set StorePath),
    -- | Whether its paths go into the tree.  An inactive element's paths
    -- are still the generation's references.
    elementActivity :: !Activity,
    -- | Its priority: of two files at one path, the one whose element has
    -- the lower number stays.
    elementPriority :: !Int,
    -- | The flake it came from, for an element a flake made.
    elementSource :: !(Maybe FlakeSource)
  }
  deriving (Eq, Show)

-- | Where an element a flake made came from, as the manifest records it.
data FlakeSource = FlakeSource
  { -- | The flake reference as it was given.
    sourceOriginalUrl :: !Text,
    -- | The flake reference it was locked to.
    sourceUrl :: !Text,
    -- | The attribute it was taken from.
    sourceAttrPath :: !Text,
    -- | The outputs it was asked for: 'Nothing' for the default ones.
    sourceOutputs :: !(Maybe OutputsSpec)
  }
  deriving (Eq, Show)

-- | A profile with nothing in it, which a profile with no manifest is.
emptyManifest :: Manifest
emptyManifest = Manifest Map.empty

-- | The priority an element has when nothing gives it another
-- (@defaultPriority@ in profile.cc).
defaultPriority :: Int
defaultPriority = 5

-- | The version 'renderManifest' writes (@ProfileManifest::toJSON@ in
-- profile.cc).
writtenVersion :: Integer
writtenVersion = 3

-- ---------------------------------------------------------------------------
-- Reading and writing
-- ---------------------------------------------------------------------------

-- | Why a manifest could not be read.
data ManifestError
  = -- | The file is not UTF-8 JSON or not a JSON object, or its @version@
    -- is not a number or its @elements@ neither an object nor an array.
    ManifestNotJson !Text
  | -- | A version other than 1, 2 and 3 (the @ProfileManifest@ constructor
    -- in profile.cc).
    UnsupportedVersion !Integer
  | -- | An element, by the key or index it was read under, that is not
    -- what upstream reads, and why.
    MalformedElement !Text !Text
  | -- | An element of a version 1 or 2 manifest that a flake made, which
    -- upstream names from its flake reference's URL (the @ProfileManifest@
    -- constructor in profile.cc) and which is not read here, by its index.
    UnnamedFlakeElement !Text
  deriving (Eq, Show)

-- | The members a manifest's version names a flake element's references
-- under: version 1's, then those of versions 2 and 3 (the
-- @ProfileManifest@ constructor in profile.cc).
data UrlKeys = UrlKeys !Text !Text

-- | Read a manifest's bytes, as upstream's @ProfileManifest@ constructor
-- in profile.cc reads @manifest.json@: the version, 0 when it is
-- missing, then each element, by its key in an object or its index in
-- the array versions 1 and 2 hold.  A missing @elements@ or @storePaths@
-- is empty, @active@ is required, and @priority@ defaults to
-- 'defaultPriority'.  An element whose @url@ is not empty came from a
-- flake.
parseManifest :: ByteString -> Either ManifestError Manifest
parseManifest bytes = do
  text <- first (const (ManifestNotJson "it is not UTF-8")) (TE.decodeUtf8' bytes)
  json <- first ManifestNotJson (parseJson text)
  members <- case json of
    JsonObject members -> Right members
    _ -> Left (ManifestNotJson "it is not a JSON object")
  version <- case Map.lookup "version" members of
    Nothing -> Right 0
    Just (JsonInt n) -> Right n
    Just _ -> Left (ManifestNotJson "its version is not a number")
  keys <- case version of
    1 -> Right (UrlKeys "uri" "originalUri")
    2 -> Right (UrlKeys "url" "originalUrl")
    3 -> Right (UrlKeys "url" "originalUrl")
    other -> Left (UnsupportedVersion other)
  case Map.lookup "elements" members of
    Nothing -> Right emptyManifest
    Just JsonNull -> Right emptyManifest
    Just (JsonObject elements) -> foldM (keyed keys) emptyManifest (Map.toAscList elements)
    Just (JsonArray elements) -> foldM (indexed keys) emptyManifest (zip [0 :: Int ..] elements)
    Just _ -> Left (ManifestNotJson "its elements are neither an object nor an array")
  where
    keyed keys manifest (name, value) = (\element -> addElement name element manifest) <$> parseElement keys name value
    indexed keys manifest (index, value) = do
      let label = T.pack (show index)
      element <- parseElement keys label value
      case elementSource element of
        Just _ -> Left (UnnamedFlakeElement label)
        Nothing -> Right (addElement (elementIdentifier element) element manifest)

-- | One element of a manifest.
parseElement :: UrlKeys -> Text -> Json -> Either ManifestError Element
parseElement (UrlKeys urlKey originalUrlKey) label value = case value of
  JsonObject fields -> do
    paths <- case Map.lookup "storePaths" fields of
      Nothing -> Right Set.empty
      Just JsonNull -> Right Set.empty
      Just (JsonArray items) -> Set.fromList <$> traverse storePath items
      Just _ -> malformed "its storePaths are not an array"
    activity <- case Map.lookup "active" fields of
      Just (JsonBool True) -> Right Active
      Just (JsonBool False) -> Right Inactive
      _ -> malformed "its active is not a Boolean"
    priority <- maybe (Right defaultPriority) priorityOf (Map.lookup "priority" fields)
    source <- case Map.lookup urlKey fields of
      Nothing -> Right Nothing
      Just (JsonString "") -> Right Nothing
      Just (JsonString url) -> Just <$> flakeSource fields url
      Just _ -> malformed ("its " <> urlKey <> " is not a string")
    Right (Element paths activity priority source)
  _ -> malformed "it is not an object"
  where
    malformed = Left . MalformedElement label
    storePath item = case item of
      JsonString text -> maybe (malformed ("path '" <> text <> "' is not in the Nix store")) Right (parseStorePath defaultStoreDir text)
      _ -> malformed "a store path is not a string"
    priorityOf priority = case priority of
      JsonInt n
        | n >= toInteger (minBound :: Int32) && n <= toInteger (maxBound :: Int32) -> Right (fromInteger n)
      _ -> malformed "its priority is not an int"
    flakeSource fields url = do
      original <- stringField fields originalUrlKey
      attrPath <- stringField fields "attrPath"
      outputs <- case Map.lookup "outputs" fields of
        Nothing -> Right Nothing
        Just JsonNull -> Right Nothing
        Just (JsonArray [JsonString "*"]) -> Right (Just AllOutputs)
        Just (JsonArray names) -> do
          spelled <- traverse outputName names
          maybe (malformed "its outputs name none") (Right . Just) (namedOutputs (Set.fromList spelled))
        Just _ -> malformed "its outputs are not an array"
      Right (FlakeSource original url attrPath outputs)
    stringField fields key = case Map.lookup key fields of
      Just (JsonString text) -> Right text
      _ -> malformed ("its " <> key <> " is not a string")
    outputName name = case name of
      JsonString text -> Right (OutputName text)
      _ -> malformed "an output name is not a string"

-- | A manifest as version 3 holds it (@ProfileManifest::toJSON@ in
-- profile.cc).
manifestJson :: Manifest -> Json
manifestJson (Manifest elements) =
  JsonObject
    ( Map.fromList
        [ ("elements", JsonObject (Map.map elementJson elements)),
          ("version", JsonInt writtenVersion)
        ]
    )
  where
    elementJson element =
      JsonObject
        ( Map.fromList
            ( [ ("active", JsonBool (elementActivity element == Active)),
                ("priority", JsonInt (toInteger (elementPriority element))),
                ("storePaths", JsonArray [JsonString (storePathToText defaultStoreDir path) | path <- Set.toAscList (elementPaths element)])
              ]
                ++ maybe [] sourceFields (elementSource element)
            )
        )
    sourceFields source =
      [ ("originalUrl", JsonString (sourceOriginalUrl source)),
        ("url", JsonString (sourceUrl source)),
        ("attrPath", JsonString (sourceAttrPath source)),
        ("outputs", outputsJson (sourceOutputs source))
      ]
    outputsJson = maybe JsonNull (JsonArray . map JsonString . outputsSpelled)

-- | The bytes of @manifest.json@: 'manifestJson' as nlohmann's @dump()@
-- writes it, with no newline after (@ProfileManifest::build@ in
-- profile.cc).
renderManifest :: Manifest -> ByteString
renderManifest = renderJson . manifestJson

-- | A failure as a message naming the manifest's path.
manifestErrorText :: OsPath -> ManifestError -> Text
manifestErrorText path failure = case failure of
  ManifestNotJson reason -> "profile manifest " <> quoted <> " cannot be read: " <> reason
  UnsupportedVersion version -> "profile manifest " <> quoted <> " has unsupported version " <> T.pack (show version)
  MalformedElement label reason -> "profile manifest " <> quoted <> " has an element " <> label <> " that cannot be read: " <> reason
  UnnamedFlakeElement label ->
    "profile manifest " <> quoted <> " has an element " <> label <> " that a flake made, which upstream names from its URL and nova-nix cannot"
  where
    quoted = "\"" <> hostPathText path <> "\""

-- ---------------------------------------------------------------------------
-- Names
-- ---------------------------------------------------------------------------

-- | What identifies an element in messages and in what @--regex@ matches
-- (@ProfileElement::identifier@ in profile.cc): for an element a flake made,
-- its flake reference, @#@ and its attribute, then its outputs after @^@;
-- otherwise the name part of each path's name ('drvNameName'), each once, in
-- byte order, joined by a comma and a space, an empty one left out.
elementIdentifier :: Element -> Text
elementIdentifier element = case elementSource element of
  Just source -> sourceOriginalUrl source <> "#" <> flakeAttribute source
  Nothing -> T.intercalate ", " (filter (not . T.null) (Set.toAscList (Set.map (drvNameName . spName) (elementPaths element))))

-- | An element's flake attribute and the outputs it was asked for, after
-- @^@ when it was asked for some (@ProfileElementSource::to_string@ in
-- profile.cc): what @profile list@ prints as its flake attribute.
flakeAttribute :: FlakeSource -> Text
flakeAttribute source = sourceAttrPath source <> maybe "" (("^" <>) . T.intercalate "," . outputsSpelled) (sourceOutputs source)

-- | Outputs as upstream spells them: @*@ for every output, else the
-- names in order.
outputsSpelled :: OutputsSpec -> [Text]
outputsSpelled spec = maybe ["*"] (\names -> [name | OutputName name <- Set.toAscList names]) (outputsSpecNames spec)

-- | The name part of a derivation name: everything before the first dash
-- that something other than a letter follows, as upstream's @DrvName@
-- constructor in names.cc splits one; the whole name when there is no
-- such dash.
drvNameName :: Text -> Text
drvNameName name = go 0 name
  where
    go !taken rest = case T.uncons rest of
      Nothing -> name
      Just ('-', after)
        | Just (next, _) <- T.uncons after,
          not (isAsciiLetter next) ->
            T.take taken name
      Just (_, after) -> go (taken + 1) after
    isAsciiLetter c = isAsciiLower c || isAsciiUpper c

-- | Add an element under a name, or under the name with @-1@, @-2@ and
-- on appended when it is taken (@ProfileManifest::addElement@ in
-- profile.cc).
addElement :: Text -> Element -> Manifest -> Manifest
addElement candidate element (Manifest elements) = Manifest (Map.insert (free candidates) element elements)
  where
    candidates = candidate : [candidate <> "-" <> T.pack (show n) | n <- [1 :: Int ..]]
    free names = case dropWhile (`Map.member` elements) names of
      name : _ -> name
      [] -> candidate

-- ---------------------------------------------------------------------------
-- What a generation is made of
-- ---------------------------------------------------------------------------

-- | Each active element's paths with its priority, the packages of the
-- generation's tree (@ProfileManifest::build@ in profile.cc).
manifestPackages :: Manifest -> [(StorePath, Int)]
manifestPackages (Manifest elements) =
  [(path, elementPriority element) | element <- Map.elems elements, elementActivity element == Active, path <- Set.toAscList (elementPaths element)]

-- | Every element's paths, the inactive ones' too: the generation's
-- references (@ProfileManifest::build@ in profile.cc).
manifestReferences :: Manifest -> Set StorePath
manifestReferences (Manifest elements) = foldMap elementPaths elements

-- ---------------------------------------------------------------------------
-- Selecting elements
-- ---------------------------------------------------------------------------

-- | What selects elements, as @profile remove@ names them
-- (@Matcher@ and its four kinds in profile.cc).  Upstream refuses none at
-- all and @--all@ beside anything
-- (@MixProfileElementMatchers::getMatchingElementNames@); neither is a
-- selector here.
data Selector
  = -- | @--all@: every element.
    SelectAll
  | -- | The elements any of these select.
    SelectSome !(NonEmpty Matcher)

-- | One way to select elements.
data Matcher
  = -- | The element with this name.
    MatchName !Text
  | -- | Every element holding this path.
    MatchStorePath !StorePath
  | -- | Every element whose identifier the pattern matches as a whole,
    -- with the pattern as it was given.
    MatchRegex !Text !RE.Regex

-- | A @--regex@ pattern: a POSIX extended regular expression that
-- ignores case, as upstream compiles one (@std::regex::extended |
-- std::regex::icase@ in @RegexMatcher@, profile.cc), over the pattern's
-- bytes, as @builtins.match@ compiles one.  'Nothing' when it does not
-- compile.
regexMatcher :: Text -> Maybe Matcher
regexMatcher given = MatchRegex given <$> RE.makeRegexOptsM options RE.defaultExecOpt (TE.encodeUtf8 given)
  where
    options = RE.defaultCompOpt {RE.multiline = False, RE.caseSensitive = False}

-- | How a warning names a matcher (each matcher's @getTitle@ in
-- profile.cc).
matcherTitle :: Matcher -> Text
matcherTitle matcher = case matcher of
  MatchName name -> "Package name '" <> name <> "'"
  MatchStorePath path -> "Store path '" <> storePathBasename path <> "'"
  MatchRegex given _ -> "Regex '" <> given <> "'"

-- | Whether a matcher selects an element, by its name.
matches :: Matcher -> Text -> Element -> Bool
matches matcher name element = case matcher of
  MatchName wanted -> name == wanted
  MatchStorePath path -> Set.member path (elementPaths element)
  MatchRegex _ regex -> wholeMatch regex (TE.encodeUtf8 (elementIdentifier element))

-- | Whether a pattern matches the whole of a subject, as @std::regex_match@
-- asks: the leftmost-longest match spans it exactly when a match of the
-- whole exists.
wholeMatch :: RE.Regex -> ByteString -> Bool
wholeMatch regex subject = case RE.matchOnceText regex subject of
  Just (before, _, after) -> BS.null before && BS.null after
  Nothing -> False

-- | What matchers select.
data Selection = Selection
  { -- | The names selected, in name order.
    selectedNames :: !(Set Text),
    -- | Upstream's warnings, in order.
    selectionWarnings :: ![Text]
  }
  deriving (Eq, Show)

-- | The elements a selector selects
-- (@MixProfileElementMatchers::getMatchingElementNames@ in profile.cc): a
-- profile with no elements selects nothing, with upstream's warning, and a
-- matcher that selects nothing is warned of.
selectElements :: Selector -> Manifest -> Selection
selectElements selector (Manifest elements)
  | Map.null elements = Selection Set.empty ["There are no packages in the profile."]
  | otherwise = case selector of
      SelectAll -> Selection (Map.keysSet elements) []
      SelectSome matchers -> foldl' select (Selection Set.empty []) matchers
  where
    select (Selection names warnings) matcher =
      case Map.keys (Map.filterWithKey (matches matcher) elements) of
        [] -> Selection names (warnings ++ [matcherTitle matcher <> " does not match any packages in the profile."])
        found -> Selection (Set.union names (Set.fromList found)) warnings

-- ---------------------------------------------------------------------------
-- Conflicts
-- ---------------------------------------------------------------------------

-- | The element a conflicting file came from: the file, the element's
-- name, and its paths as the store spells them, which @profile add@ takes.
data ConflictSide = ConflictSide !OsPath !Text !(Set Text)
  deriving (Eq, Show)

-- | Two files of equal priority at one path of the tree, each with the
-- element it came from, and their priority.
data Conflict = Conflict
  { conflictExisting :: !ConflictSide,
    conflictNew :: !ConflictSide,
    conflictPriority :: !Int
  }
  deriving (Eq, Show)

-- | The elements two conflicting files came from, as upstream finds them
-- (@CmdProfileAdd::run@ in profile.cc): searching the elements in name order,
-- the first holding a path either file lies in is the existing one, and
-- searching them the other way, the first is the new one.  'Nothing' when no
-- element holds either, where upstream reports the tree's own conflict.  The
-- files are the tree's: one it links directly is under the store directory as
-- the store spells it, the first directory given, and one inside a directory
-- it expanded under the second, the store directory resolved through its
-- links as the tree resolves a link it expands
-- ('Nix.Profile.Tree.trResolve').  A path is looked for under both, so a
-- store reached through a linked directory names the right elements; an
-- element's paths are shown in the store's spelling.
attributeConflict :: StoreDir -> StoreDir -> Manifest -> OsPath -> OsPath -> Int -> Maybe Conflict
attributeConflict storeDir resolvedDir (Manifest elements) fileA fileB priority =
  Conflict <$> side (Map.toAscList elements) <*> side (Map.toDescList elements) <*> pure priority
  where
    side ordered = case [found | (name, element) <- ordered, path <- Set.toAscList (elementPaths element), found <- toList (holding name element path)] of
      found : _ -> Just found
      [] -> Nothing
    holding name element path
      | path `under` fileA = Just (ConflictSide fileA name (installables element))
      | path `under` fileB = Just (ConflictSide fileB name (installables element))
      | otherwise = Nothing
    under path file = any (\dir -> spelledIn dir path `T.isPrefixOf` hostPathText file) [storeDir, resolvedDir]
    installables element = Set.map (spelledIn storeDir) (elementPaths element)
    spelledIn dir = T.pack . storePathToFilePath dir
