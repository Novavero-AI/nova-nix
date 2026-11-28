{-# LANGUAGE LambdaCase #-}

-- | Fetcher inputs as upstream's input schemes read and write them: from
-- a URL, from attributes, and back to a URL (fetchers.cc, git.cc,
-- github.cc, mercurial.cc, indirect.cc, path.cc and tarball.cc at
-- 2.36.0).  Nothing here fetches.
--
-- Upstream keeps its schemes in a map by name and asks each in turn
-- whether a URL is its own, so the first scheme in name order to claim
-- one parses it.  A scheme behind an experimental feature refuses a URL
-- it has claimed, or attributes naming it, while the feature is off.
module Nix.Fetch.Input
  ( -- * Inputs
    Scheme (..),
    schemeName,
    Input (..),
    InputContext (..),
    TreeRequirement (..),

    -- * Reading and writing
    inputFromUrl,
    inputFromAttrs,
    inputToUrl,
    inputToUrlString,

    -- * Git names
    isLegalRefName,
  )
where

import Control.Monad (forM_, unless, void, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isHexDigit)
import Data.Foldable (foldlM)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word64)
import Nix.Eval.Policy (PathRules)
import Nix.ExperimentalFeature (ExperimentalFeature, FeatureSet, flakesFeature, requireFeature, verifiedFetchesFeature)
import Nix.Fetch.Attrs (AttrValue (..), Attrs, attrsJsonText, attrsToQuery, maybeBoolAttr, maybeIntAttr, maybeStrAttr, strAttr)
import Nix.Fetch.Url (Checked, Leniency (..), ParsedUrl (..), UrlScheme (..), authHost, bytesToTextLossy, fixGitUrl, parseUrl, parseUrlScheme, pathToUrlPath, refuse, renderAuthority, renderUrl, urlPathSegments, urlPathToPath)
import Nix.Hash (bytesToHexText, parseAnyHash, parseNarHash)
import Nix.Json (Json (..), parseJson, renderJson)
import qualified NovaCache.Base64 as B64

-- ---------------------------------------------------------------------------
-- Inputs
-- ---------------------------------------------------------------------------

-- | An input scheme, in the order of upstream's map of them: by name.
data Scheme
  = SchemeFile
  | SchemeGit
  | SchemeGitHub
  | SchemeGitLab
  | SchemeMercurial
  | SchemeIndirect
  | SchemePath
  | SchemeSourceHut
  | SchemeTarball
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | A scheme's name, the @type@ attribute of its inputs.
schemeName :: Scheme -> Text
schemeName scheme = case scheme of
  SchemeFile -> "file"
  SchemeGit -> "git"
  SchemeGitHub -> "github"
  SchemeGitLab -> "gitlab"
  SchemeMercurial -> "hg"
  SchemeIndirect -> "indirect"
  SchemePath -> "path"
  SchemeSourceHut -> "sourcehut"
  SchemeTarball -> "tarball"

-- | An input: its scheme, when its @type@ names one upstream has, and its
-- attributes, the @type@ among them.
data Input = Input
  { inputScheme :: !(Maybe Scheme),
    inputAttrs :: !Attrs
  }
  deriving (Eq, Show)

-- | What reading an input depends on: the features that are on, and the
-- host's path rules, which a @path:@ input's path and a Git URL's
-- absolute path are read under.
data InputContext = InputContext
  { icFeatures :: !FeatureSet,
    icPathRules :: !PathRules
  }

-- | Whether a URL must name a tree, as a flake reference must, or may
-- name a plain file.
data TreeRequirement = RequireTree | AllowFile
  deriving (Eq, Show)

-- | The feature a scheme needs.
schemeFeature :: Scheme -> Maybe ExperimentalFeature
schemeFeature scheme = case scheme of
  SchemeGitHub -> Just flakesFeature
  SchemeGitLab -> Just flakesFeature
  SchemeSourceHut -> Just flakesFeature
  SchemeIndirect -> Just flakesFeature
  SchemePath -> Just flakesFeature
  SchemeFile -> Nothing
  SchemeGit -> Nothing
  SchemeMercurial -> Nothing
  SchemeTarball -> Nothing

requireSchemeFeature :: InputContext -> Scheme -> Checked ()
requireSchemeFeature ctx scheme = forM_ (schemeFeature scheme) (\feature -> either refuse pure (requireFeature feature (icFeatures ctx)))

-- ---------------------------------------------------------------------------
-- From a URL
-- ---------------------------------------------------------------------------

-- | An input from a URL, as upstream's @Input::fromURL@ reads one: the
-- first scheme in name order that claims the URL parses it, needing its
-- feature only once it has, and the attributes every input has are then
-- checked.  A URL no scheme claims is refused.
inputFromUrl :: InputContext -> TreeRequirement -> ParsedUrl -> Checked Input
inputFromUrl ctx tree url = go [minBound .. maxBound]
  where
    go schemes = case schemes of
      scheme : rest ->
        schemeFromUrl ctx tree scheme url >>= \case
          Nothing -> go rest
          Just attrs -> do
            requireSchemeFeature ctx scheme
            let input = Input (Just scheme) attrs
            input <$ fixupInput input
      []
        | UrlScheme (Just "file") "git" <- parseUrlScheme (urlScheme url) ->
            refuse ("input " <> quotedUrl url <> " is unsupported; did you mean 'git+file' instead of 'file+git'?")
        | otherwise -> refuse ("input " <> quotedUrl url <> " is unsupported")

-- | A URL quoted for a message.
quotedUrl :: ParsedUrl -> Text
quotedUrl url = "'" <> bytesToTextLossy (renderUrl url) <> "'"

-- | A scheme's reading of a URL, or 'Nothing' when it is not the
-- scheme's.
schemeFromUrl :: InputContext -> TreeRequirement -> Scheme -> ParsedUrl -> Checked (Maybe Attrs)
schemeFromUrl ctx tree scheme url = case scheme of
  SchemeFile -> pure (curlFromUrl scheme tree url)
  SchemeTarball -> pure (curlFromUrl scheme tree url)
  SchemeGit -> traverse (gitFromAttrs ctx) (gitUrlAttrs url)
  SchemeGitHub -> forgeFromUrl scheme url
  SchemeGitLab -> forgeFromUrl scheme url
  SchemeSourceHut -> forgeFromUrl scheme url
  SchemeMercurial -> traverse mercurialFromAttrs (mercurialUrlAttrs url)
  SchemeIndirect -> indirectFromUrl url
  SchemePath -> pathFromUrl ctx url

-- | A string attribute.
str :: ByteString -> AttrValue
str = AttrString

-- | The @type@ attribute of a scheme's inputs.
typeAttr :: Scheme -> (Text, AttrValue)
typeAttr scheme = ("type", str (TE.encodeUtf8 (schemeName scheme)))

-- | Upstream's @string2Int@ for an unsigned 64-bit number: decimal digits
-- with an optional leading @+@, in range.
unsignedValue :: ByteString -> Maybe Word64
unsignedValue raw = case BC.readInteger digits of
  Just (n, rest)
    | BS.null rest,
      not (BS.null digits),
      BC.all isDigit digits,
      n <= toInteger (maxBound :: Word64) ->
        Just (fromInteger n)
  _ -> Nothing
  where
    digits = fromMaybe raw (BC.stripPrefix "+" raw)

-- | The file and tarball schemes' reading of a URL: one fetched over
-- @file@, @http@ or @https@, with the scheme named as its application or,
-- with none named, a tarball by its extension or because a tree is
-- required, a file otherwise.  The query's @narHash@, @rev@, @revCount@
-- and @lastModified@ become attributes, and every name the scheme allows
-- is dropped from the URL.
curlFromUrl :: Scheme -> TreeRequirement -> ParsedUrl -> Maybe Attrs
curlFromUrl scheme tree url
  | not valid = Nothing
  | otherwise =
      Just
        $ Map.fromList
        $ [typeAttr scheme, ("url", str (renderUrl fetchedUrl))]
          ++ [("narHash", str v) | Just v <- [Map.lookup "narHash" query]]
          ++ [("rev", str v) | Just v <- [Map.lookup "rev" query]]
          ++ [("revCount", AttrInt n) | Just n <- [Map.lookup "revCount" query >>= unsignedValue]]
          ++ [("lastModified", AttrInt n) | Just n <- [Map.lookup "lastModified" query >>= unsignedValue]]
  where
    UrlScheme application transport = parseUrlScheme (urlScheme url)
    query = urlQuery url
    valid =
      transport `elem` ["file", "http", "https"] && case application of
        Just named -> named == TE.encodeUtf8 (schemeName scheme)
        Nothing -> case scheme of
          SchemeFile -> tree == AllowFile && not hasTarballExtension
          _ -> tree == RequireTree || hasTarballExtension
    hasTarballExtension = case reverse (urlPath url) of
      final : _ -> any (`BS.isSuffixOf` final) [".zip", ".tar", ".tgz", ".tar.gz", ".tar.xz", ".tar.bz2", ".tar.zst"]
      [] -> False
    fetchedUrl = url {urlScheme = transport, urlQuery = foldr (Map.delete . TE.encodeUtf8) query curlAttrNames}

-- | The attributes the file and tarball schemes allow.
curlAttrNames :: [Text]
curlAttrNames = ["url", "narHash", "name", "unpack", "rev", "revCount", "lastModified"]

-- | The git scheme's attributes from a URL whose scheme is @git@ or has
-- @git@ as its application: @rev@, @ref@ and the key attributes as
-- strings, the flags as true when their value is @1@, and every other
-- parameter left in the URL.
gitUrlAttrs :: ParsedUrl -> Maybe Attrs
gitUrlAttrs url
  | urlScheme url /= "git" && utApplication (parseUrlScheme (urlScheme url)) /= Just "git" = Nothing
  | otherwise =
      Just . Map.fromList $
        [typeAttr SchemeGit, ("url", str (renderUrl url {urlQuery = kept}))]
          ++ [(name, str value) | name <- gitStringNames, Just value <- [lookupParam name]]
          ++ [(name, AttrBool (value == "1")) | name <- gitFlagNames, Just value <- [lookupParam name]]
  where
    lookupParam name = Map.lookup (TE.encodeUtf8 name) (urlQuery url)
    kept = foldr (Map.delete . TE.encodeUtf8) (urlQuery url) (gitStringNames ++ gitFlagNames)

-- | The git scheme's string parameters.
gitStringNames :: [Text]
gitStringNames = ["rev", "ref", "keytype", "publicKey", "publicKeys"]

-- | The git scheme's flags.
gitFlagNames :: [Text]
gitFlagNames = ["shallow", "submodules", "lfs", "exportIgnore", "allRefs", "verifyCommit"]

-- | The git scheme's check of its attributes, as its @inputFromAttrs@
-- makes it: the key attributes need @verified-fetches@, a @ref@ must be a
-- legal name, the flags must be Booleans, and the URL is made a Git URL
-- ('fixGitUrl').
gitFromAttrs :: InputContext -> Attrs -> Checked Attrs
gitFromAttrs ctx attrs = do
  when (any (`Map.member` attrs) ["verifyCommit", "keytype", "publicKey", "publicKeys"]) $
    either refuse pure (requireFeature verifiedFetchesFeature (icFeatures ctx))
  _ <- maybeBoolAttr "verifyCommit" attrs
  maybeStrAttr "ref" attrs >>= mapM_ (\ref -> unless (isLegalRefName ref) (refuse ("invalid Git branch/tag name '" <> bytesToTextLossy ref <> "'")))
  url <- strAttr "url" attrs >>= fixGitUrl (icPathRules ctx)
  let fixed = Map.insert "url" (str (renderUrl url)) attrs
  mapM_ (`maybeBoolAttr` fixed) ["shallow", "submodules", "allRefs"]
  pure fixed

-- | The forge schemes' reading of a URL: @OWNER\/REPO@ and an optional
-- revision or ref, the empty path segments skipped; then the @rev@,
-- @ref@, @host@ and @narHash@ parameters, any other refused.
forgeFromUrl :: Scheme -> ParsedUrl -> Checked (Maybe Attrs)
forgeFromUrl scheme url
  | urlScheme url /= TE.encodeUtf8 (schemeName scheme) = pure Nothing
  | otherwise = case urlPathSegments url of
      owner : repo : rest -> do
        let fromPath = case rest of
              [] -> Map.empty
              [single] | isRev single -> Map.singleton "rev" (str single)
              _ -> Map.singleton "ref" (str (BS.intercalate "/" rest))
        withQuery <- foldlM addParam fromPath (Map.toList (urlQuery url))
        Just <$> forgeFromAttrs (Map.union (Map.fromList [typeAttr scheme, ("owner", str owner), ("repo", str repo)]) withQuery)
      _ -> refuse ("URL " <> quotedUrl url <> " is invalid")
  where
    addParam acc (name, value)
      | name == "rev" = once "rev" "contains multiple commit hashes"
      | name == "ref" = once "ref" "contains multiple branch/tag names"
      | name == "host" = pure (Map.insert "host" (str value) acc)
      | name == "narHash" = pure (Map.insert "narHash" (str value) acc)
      | otherwise = refuse ("URL " <> quotedUrl url <> " contains unknown parameter '" <> bytesToTextLossy name <> "'")
      where
        once key problem
          | Map.member key acc = refuse ("URL " <> quotedUrl url <> " " <> problem)
          | otherwise = pure (Map.insert key (str value) acc)

-- | The forge schemes' check of their attributes: an owner and a repo, a
-- revision or a ref and not both, a revision that is a SHA-1, a legal
-- ref, and a host of letters, digits, dots and dashes.
forgeFromAttrs :: Attrs -> Checked Attrs
forgeFromAttrs attrs = do
  _ <- strAttr "owner" attrs
  _ <- strAttr "repo" attrs
  ref <- maybeStrAttr "ref" attrs
  rev <- maybeStrAttr "rev" attrs
  case (ref, rev) of
    (Just r, Just v) ->
      refuse ("input " <> attrsJsonText attrs <> " contains both a commit hash ('" <> bytesToTextLossy v <> "') and a branch/tag name ('" <> bytesToTextLossy r <> "')")
    _ -> pure ()
  mapM_ (parseHashAs (Just sha1Algo)) rev
  forM_ ref $ \r -> unless (isLegalRefName r) (refuse ("input " <> attrsJsonText attrs <> " contains an invalid branch/tag name"))
  maybeStrAttr "host" attrs >>= mapM_ (\host -> unless (BC.all isHostChar host) (refuse ("input " <> attrsJsonText attrs <> " contains an invalid instance host")))
  pure attrs
  where
    isHostChar c = isAsciiLower c || isAsciiUpper c || isDigit c || c == '.' || c == '-'

-- | The Mercurial scheme's attributes from a @hg+http@, @hg+https@,
-- @hg+ssh@ or @hg+file@ URL: @rev@ and @ref@, every other parameter left
-- in the URL, whose scheme loses its @hg+@.
mercurialUrlAttrs :: ParsedUrl -> Maybe Attrs
mercurialUrlAttrs url
  | urlScheme url `notElem` ["hg+http", "hg+https", "hg+ssh", "hg+file"] = Nothing
  | otherwise =
      Just
        $ Map.fromList
        $ [typeAttr SchemeMercurial, ("url", str (renderUrl url {urlScheme = BS.drop 3 (urlScheme url), urlQuery = kept}))]
          ++ [(name, str value) | name <- ["rev", "ref"], Just value <- [Map.lookup (TE.encodeUtf8 name) (urlQuery url)]]
  where
    kept = Map.filterWithKey (\name _ -> name `notElem` ["rev", "ref"]) (urlQuery url)

-- | The Mercurial scheme's check of its attributes: a URL that parses,
-- and a ref of the characters a Git ref may start and go on with.
mercurialFromAttrs :: Attrs -> Checked Attrs
mercurialFromAttrs attrs = do
  _ <- strAttr "url" attrs >>= parseUrl Strict
  maybeStrAttr "ref" attrs >>= mapM_ (\ref -> unless (matchesRefRegex ref) (refuse ("invalid Mercurial branch/tag name '" <> bytesToTextLossy ref <> "'")))
  pure attrs

-- | The indirect scheme's reading of a @flake:@ URL: an id, then a
-- revision or a ref, or a ref and a revision, the empty path segments
-- skipped.  Its query is not read.
indirectFromUrl :: ParsedUrl -> Checked (Maybe Attrs)
indirectFromUrl url
  | urlScheme url /= "flake" = pure Nothing
  | otherwise = do
      (flakeId, ref, rev) <- case urlPathSegments url of
        [flakeId] -> pure (flakeId, Nothing, Nothing)
        [flakeId, second]
          | isRev second -> (\digest -> (flakeId, Nothing, Just digest)) <$> parseHashAs (Just sha1Algo) second
          | isLegalRefName second -> pure (flakeId, Just second, Nothing)
          | otherwise -> refuse ("in flake URL " <> quotedUrl url <> ", '" <> bytesToTextLossy second <> "' is not a commit hash or branch/tag name")
        [flakeId, second, third] -> do
          unless (isLegalRefName second) $ refuse ("in flake URL " <> quotedUrl url <> ", '" <> bytesToTextLossy second <> "' is not a branch/tag name")
          unless (isRev third) $ refuse ("in flake URL " <> quotedUrl url <> ", '" <> bytesToTextLossy third <> "' is not a commit hash")
          (\digest -> (flakeId, Just second, Just digest)) <$> parseHashAs (Just sha1Algo) third
        _ -> refuse ("GitHub URL " <> quotedUrl url <> " is invalid")
      unless (isFlakeId flakeId) $ refuse ("'" <> bytesToTextLossy flakeId <> "' is not a valid flake ID")
      pure . Just . Map.fromList $
        [typeAttr SchemeIndirect, ("id", str flakeId)]
          ++ [("rev", str (TE.encodeUtf8 (bytesToHexText digest))) | Just digest <- [rev]]
          ++ [("ref", str r) | Just r <- [ref]]

-- | The path scheme's reading of a @path:@ URL: one with no host, its path
-- the URL's, and its query's @rev@, @narHash@, @revCount@ and
-- @lastModified@ as attributes, the counts unsigned; any other parameter
-- is refused.
pathFromUrl :: InputContext -> ParsedUrl -> Checked (Maybe Attrs)
pathFromUrl ctx url
  | urlScheme url /= "path" = pure Nothing
  | otherwise = do
      case urlAuthority url of
        Just authority
          | not (BS.null (authHost authority)) ->
              refuse ("path URL " <> quotedUrl url <> " should not have an authority ('" <> bytesToTextLossy (renderAuthority authority) <> "')")
        _ -> pure ()
      path <- urlPathToPath (icPathRules ctx) (urlPath url)
      params <- traverse param (Map.toList (urlQuery url))
      pure (Just (Map.fromList ([typeAttr SchemePath, ("path", str path)] ++ params)))
  where
    param (name, value)
      | name == "rev" = pure ("rev", str value)
      | name == "narHash" = pure ("narHash", str value)
      | name == "revCount" = count "revCount"
      | name == "lastModified" = count "lastModified"
      | otherwise = refuse ("path URL " <> quotedUrl url <> " has unsupported parameter '" <> bytesToTextLossy name <> "'")
      where
        count key = case unsignedValue value of
          Just n -> pure (key, AttrInt n)
          Nothing -> refuse ("path URL " <> quotedUrl url <> " has invalid parameter '" <> bytesToTextLossy name <> "'")

-- ---------------------------------------------------------------------------
-- From attributes
-- ---------------------------------------------------------------------------

-- | An input from attributes, as upstream's @Input::fromAttrs@ reads
-- them: a @type@ is required; one naming no scheme gives an input no
-- scheme reads, which shows as unsupported; one naming a scheme needs its
-- feature, admits only the attributes the scheme allows, and is checked
-- as the scheme checks its own.  The attributes every input has are
-- checked last.
inputFromAttrs :: InputContext -> Attrs -> Checked Input
inputFromAttrs ctx attrs = do
  typeName <- case Map.lookup "type" attrs of
    Just (AttrString name) -> pure name
    Just _ -> refuse ("input attribute 'type' is not a string " <> attrsJsonText attrs)
    Nothing -> refuse "'type' attribute to specify input scheme is required but not provided"
  input <- case [scheme | scheme <- [minBound .. maxBound], TE.encodeUtf8 (schemeName scheme) == typeName] of
    scheme : _ -> do
      requireSchemeFeature ctx scheme
      forM_ (Map.keys attrs) $ \name ->
        unless (name `elem` ["type", "__final"] || name `elem` allowedAttrs scheme) $
          refuse ("input attribute '" <> name <> "' not supported by scheme '" <> schemeName scheme <> "'")
      Input (Just scheme) <$> schemeFromAttrs ctx scheme attrs
    [] -> pure (Input Nothing attrs)
  input <$ fixupInput input

-- | The attributes a scheme allows beyond @type@.
allowedAttrs :: Scheme -> [Text]
allowedAttrs scheme = case scheme of
  SchemeFile -> curlAllowed
  SchemeTarball -> curlAllowed
  SchemeGit ->
    [ "url",
      "ref",
      "rev",
      "shallow",
      "submodules",
      "lfs",
      "exportIgnore",
      "lastModified",
      "revCount",
      "narHash",
      "allRefs",
      "name",
      "dirtyRev",
      "dirtyShortRev",
      "verifyCommit",
      "keytype",
      "publicKey",
      "publicKeys"
    ]
  SchemeGitHub -> forgeAllowed
  SchemeGitLab -> forgeAllowed
  SchemeSourceHut -> forgeAllowed
  SchemeMercurial -> ["url", "ref", "rev", "revCount", "narHash", "name"]
  SchemeIndirect -> ["id", "ref", "rev", "narHash"]
  SchemePath -> ["path", "rev", "revCount", "lastModified", "narHash"]
  where
    curlAllowed = curlAttrNames
    forgeAllowed = ["owner", "repo", "ref", "rev", "narHash", "lastModified", "host", "treeHash"]

-- | A scheme's own check of its attributes.
schemeFromAttrs :: InputContext -> Scheme -> Attrs -> Checked Attrs
schemeFromAttrs ctx scheme attrs = case scheme of
  SchemeFile -> pure attrs
  SchemeTarball -> pure attrs
  SchemeGit -> gitFromAttrs ctx attrs
  SchemeGitHub -> forgeFromAttrs attrs
  SchemeGitLab -> forgeFromAttrs attrs
  SchemeSourceHut -> forgeFromAttrs attrs
  SchemeMercurial -> mercurialFromAttrs attrs
  SchemeIndirect -> do
    flakeId <- strAttr "id" attrs
    unless (isFlakeId flakeId) $ refuse ("'" <> bytesToTextLossy flakeId <> "' is not a valid flake ID")
    pure attrs
  SchemePath -> attrs <$ strAttr "path" attrs

-- | The checks every input gets once read (@fixupInput@ in fetchers.cc):
-- a string @type@ and @ref@, and integer @revCount@ and @lastModified@.
fixupInput :: Input -> Checked ()
fixupInput (Input _ attrs) = do
  _ <- strAttr "type" attrs
  _ <- maybeStrAttr "ref" attrs
  _ <- maybeIntAttr "revCount" attrs
  void (maybeIntAttr "lastModified" attrs)

-- ---------------------------------------------------------------------------
-- To a URL
-- ---------------------------------------------------------------------------

-- | An input's URL, as its scheme's @toURL@ writes it.  An input no scheme
-- reads has none.
inputToUrl :: PathRules -> Input -> Checked ParsedUrl
inputToUrl rules (Input scheme attrs) = case scheme of
  Nothing -> refuse ("cannot show unsupported input '" <> attrsJsonText attrs <> "'")
  Just SchemeFile -> curlToUrl
  Just SchemeTarball -> curlToUrl
  Just SchemeGit -> do
    url <- strAttr "url" attrs >>= parseUrl Strict
    rev <- inputRev attrs
    ref <- maybeStrAttr "ref" attrs
    flags <- traverse (\name -> (,) name <$> maybeBoolAttr name attrs) ["shallow", "lfs", "submodules", "exportIgnore", "verifyCommit"]
    keys <- publicKeys attrs
    let keyParams = case keys of
          [PublicKey keyType key] -> [("keytype", keyType), ("publicKey", key)]
          [] -> []
          _ -> [("publicKeys", renderJson (JsonArray (map publicKeyJson keys)))]
        params =
          [("rev", gitRev digest) | Just digest <- [rev]]
            ++ [("ref", r) | Just r <- [ref]]
            ++ [(TE.encodeUtf8 name, "1") | (name, Just True) <- flags]
            ++ keyParams
    pure
      url
        { urlScheme = if urlScheme url == "git" then urlScheme url else "git+" <> urlScheme url,
          urlQuery = Map.union (Map.fromList params) (urlQuery url)
        }
  Just SchemeGitHub -> forgeToUrl SchemeGitHub
  Just SchemeGitLab -> forgeToUrl SchemeGitLab
  Just SchemeSourceHut -> forgeToUrl SchemeSourceHut
  Just SchemeMercurial -> do
    url <- strAttr "url" attrs >>= parseUrl Strict
    rev <- inputRev attrs
    ref <- maybeStrAttr "ref" attrs
    let params = [("rev", gitRev digest) | Just digest <- [rev]] ++ [("ref", r) | Just r <- [ref]]
    pure url {urlScheme = "hg+" <> urlScheme url, urlQuery = Map.union (Map.fromList params) (urlQuery url)}
  Just SchemeIndirect -> do
    flakeId <- strAttr "id" attrs
    ref <- maybeStrAttr "ref" attrs
    rev <- inputRev attrs
    pure (ParsedUrl "flake" Nothing ([flakeId] ++ maybe [] pure ref ++ [gitRev digest | Just digest <- [rev]]) Map.empty BS.empty)
  Just SchemePath -> do
    path <- strAttr "path" attrs
    let query = foldr Map.delete (attrsToQuery attrs) ["path", "type", "__final"]
    pure (ParsedUrl "path" Nothing (pathToUrlPath rules path) query BS.empty)
  where
    curlToUrl = do
      url <- strAttr "url" attrs >>= parseUrl Strict
      narHash <- inputNarHash attrs
      pure url {urlQuery = maybe id (Map.insert "narHash") narHash (urlQuery url)}
    forgeToUrl forge = do
      owner <- strAttr "owner" attrs
      repo <- strAttr "repo" attrs
      ref <- maybeStrAttr "ref" attrs
      rev <- inputRev attrs
      narHash <- inputNarHash attrs
      host <- maybeStrAttr "host" attrs
      let query = Map.fromList ([("narHash", h) | Just h <- [narHash]] ++ [("host", h) | Just h <- [host]])
      pure (ParsedUrl (TE.encodeUtf8 (schemeName forge)) Nothing ([owner, repo] ++ maybe [] pure ref ++ [gitRev digest | Just digest <- [rev]]) query BS.empty)

-- | An input's URL with extra parameters, a name the URL has already
-- keeping its own value, as upstream's @Input::toURLString@ adds them.
inputToUrlString :: PathRules -> [(ByteString, ByteString)] -> Input -> Checked ByteString
inputToUrlString rules extra input = do
  url <- inputToUrl rules input
  pure (renderUrl url {urlQuery = Map.union (urlQuery url) (Map.fromList extra)})

-- | The input's revision, as upstream's @Input::getRev@ reads it: a hash
-- with its algorithm named, else a SHA-1.
inputRev :: Attrs -> Checked (Maybe ByteString)
inputRev attrs =
  maybeStrAttr "rev" attrs >>= traverse (\rev -> either (const (parseHashAs (Just sha1Algo) rev)) (pure . snd) (parseAnyHash Nothing (bytesToTextLossy rev)))

-- | The input's NAR hash in SRI, as upstream's @Input::getNarHash@ reads
-- it and writes it back: an SRI SHA-256, or the empty string for a
-- digest of zeros.
inputNarHash :: Attrs -> Checked (Maybe ByteString)
inputNarHash attrs =
  maybeStrAttr "narHash" attrs
    >>= traverse (either refuse (pure . sri) . parseNarHash . bytesToTextLossy)
  where
    sri digest = "sha256-" <> TE.encodeUtf8 (B64.encode digest)

-- | A digest in lower-case hex, as a Git revision is written.
gitRev :: ByteString -> ByteString
gitRev = TE.encodeUtf8 . bytesToHexText

-- | SHA-1, the algorithm of a Git revision.
sha1Algo :: Text
sha1Algo = "sha1"

-- | A hash read as upstream's @Hash::parseAny@ reads one, its digest.
parseHashAs :: Maybe Text -> ByteString -> Checked ByteString
parseHashAs algo hash = either refuse (pure . snd) (parseAnyHash algo (bytesToTextLossy hash))

-- | A Git input's signing key: its type and the key.
data PublicKey = PublicKey !ByteString !ByteString

-- | The type of a key that names none.
defaultKeyType :: ByteString
defaultKeyType = "ssh-ed25519"

publicKeyJson :: PublicKey -> Json
publicKeyJson (PublicKey keyType key) = JsonObject (Map.fromList [("key", JsonString (bytesToTextLossy key)), ("type", JsonString (bytesToTextLossy keyType))])

-- | A Git input's keys, as upstream's @getPublicKeys@ reads them: those
-- @publicKeys@ lists as JSON, then the one @publicKey@ names, of the type
-- @keytype@ names or @ssh-ed25519@.
publicKeys :: Attrs -> Checked [PublicKey]
publicKeys attrs = do
  listed <-
    maybeStrAttr "publicKeys" attrs >>= \case
      Nothing -> pure []
      Just text -> case parseJson (bytesToTextLossy text) of
        Left err -> refuse err
        Right (JsonArray items) -> traverse keyOf items
        Right other -> refuse (expectedType "array" other)
  single <-
    maybeStrAttr "publicKey" attrs >>= \case
      Nothing -> pure []
      Just key -> (\keyType -> [PublicKey (fromMaybe defaultKeyType keyType) key]) <$> maybeStrAttr "keytype" attrs
  pure (listed ++ single)
  where
    keyOf item = case item of
      JsonObject fields -> do
        keyType <- case Map.lookup "type" fields of
          Nothing -> pure defaultKeyType
          Just (JsonString t) -> pure (TE.encodeUtf8 t)
          Just other -> refuse (expectedType "string" other)
        key <- case Map.lookup "key" fields of
          Just (JsonString k) -> pure (TE.encodeUtf8 k)
          Just other -> refuse (expectedType "string" other)
          Nothing -> refuse ("Expected JSON object to contain key 'key' but it doesn't: " <> bytesToTextLossy (renderJson item))
        pure (PublicKey keyType key)
      other -> refuse (expectedType "object" other)
    expectedType want value = "Expected JSON value to be of type '" <> want <> "' but it is of type '" <> jsonTypeName value <> "': " <> bytesToTextLossy (renderJson value)
    jsonTypeName value = case value of
      JsonNull -> "null"
      JsonBool _ -> "boolean"
      JsonInt _ -> "number"
      JsonFloat _ -> "number"
      JsonString _ -> "string"
      JsonArray _ -> "array"
      JsonObject _ -> "object"

-- ---------------------------------------------------------------------------
-- Names
-- ---------------------------------------------------------------------------

-- | Whether a string is a full Git revision: 40 hex digits.
isRev :: ByteString -> Bool
isRev s = BS.length s == revLength && BC.all isHexDigit s

-- | Hex digits in a SHA-1.
revLength :: Int
revLength = 40

-- | Whether a string is a flake ID: a letter, then letters, digits, @_@
-- and @-@.
isFlakeId :: ByteString -> Bool
isFlakeId s = case BC.uncons s of
  Just (first, rest) -> (isAsciiLower first || isAsciiUpper first) && BC.all (\c -> isAlphaNum c || c == '_' || c == '-') rest
  Nothing -> False

isAlphaNum :: Char -> Bool
isAlphaNum c = isAsciiLower c || isAsciiUpper c || isDigit c

-- | Whether a string matches upstream's @refRegexS@: a letter, a digit
-- or @\@@, then letters, digits and @_.\/\@+-@.
matchesRefRegex :: ByteString -> Bool
matchesRefRegex s = case BC.uncons s of
  Just (first, rest) -> (isAlphaNum first || first == '@') && BC.all (\c -> isAlphaNum c || c `elem` ("_./@+-" :: String)) rest
  Nothing -> False

-- | Whether a name is one Git accepts for a ref, a branch or a tag, as
-- upstream's @isLegalRefName@ asks libgit2: not @\@@ alone and holding no
-- DEL, then valid as a ref name of one or more levels (git-check-ref-format):
-- no empty level, no level starting with a dot or ending in @.lock@, no
-- @..@ or @\@{@, no control character, space or any of @~^:?*[\\@, and
-- not ending in a dot.
isLegalRefName :: ByteString -> Bool
isLegalRefName name =
  name /= "@"
    && not (BC.elem '\DEL' name)
    && not (BS.null name)
    && not (".." `BS.isInfixOf` name)
    && not ("@{" `BS.isInfixOf` name)
    && not ("." `BS.isSuffixOf` name)
    && BC.all allowedChar name
    && all validLevel (BC.split '/' name)
  where
    allowedChar c = c > ' ' && c /= '\DEL' && c `notElem` ("~^:?*[\\" :: String)
    validLevel level =
      not (BS.null level)
        && not ("." `BS.isPrefixOf` level)
        && not (".lock" `BS.isSuffixOf` level)
