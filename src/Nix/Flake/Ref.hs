-- | Flake references as upstream reads and writes them (flakeref.cc at
-- 2.36.0): an input and the subdirectory of its tree that holds the
-- flake, read from a URL-like string with an optional @#fragment@, from
-- attributes, and back to a string.
--
-- A string is read in upstream's order: first as a flake ID
-- (@nixpkgs\/branch#attr@), then as a URL, then as a path.  The path form
-- here is the one with no base directory, as @builtins.parseFlakeRef@
-- reads it, where a path must be absolute.
module Nix.Flake.Ref
  ( -- * References
    FlakeRef (..),

    -- * Reading and writing
    parseFlakeRefWithFragment,
    parseFlakeRef,
    flakeRefFromAttrs,
    flakeRefAttrs,
    flakeRefToString,
  )
where

import Control.Monad (guard)
import Control.Monad.Writer.Strict (tell)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isHexDigit)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Nix.Fetch.Attrs (AttrValue (..), Attrs, maybeStrAttr)
import Nix.Fetch.Input (Input (..), InputContext (..), TreeRequirement (..), inputFromAttrs, inputFromUrl, inputToUrlString)
import Nix.Fetch.Url (Checked, Leniency (..), ParsedUrl (..), bytesToTextLossy, decodeQuery, emptyAuthority, isAbsoluteHostPath, parseUrl, pathToUrlPath, percentDecode, refuse, runChecked)

-- | A flake reference: the input that fetches the flake's tree, and the
-- directory in it that holds @flake.nix@, empty for its root.
data FlakeRef = FlakeRef
  { flakeInput :: !Input,
    flakeSubdir :: !ByteString
  }
  deriving (Eq, Show)

-- | Read a flake reference and its fragment, as upstream's
-- @parseFlakeRefWithFragment@ reads one given no base directory: as a
-- flake ID, else as a URL, else as an absolute path.  A string that is
-- not a URL by the grammar is read as a path; one that is a URL but no
-- input is refused as the input refuses it.
parseFlakeRefWithFragment :: InputContext -> ByteString -> Checked (FlakeRef, ByteString)
parseFlakeRefWithFragment ctx ref = case flakeIdParts ref of
  Just (idPart, fragment) -> do
    input <- inputFromUrl ctx RequireTree (ParsedUrl "flake" Nothing (BC.split '/' idPart) Map.empty BS.empty)
    (,) (FlakeRef input BS.empty) <$> percentDecode fragment
  Nothing -> do
    let (asUrl, warnings) = runChecked (parseUrl Lenient ref)
    tell warnings
    either (const (pathFlakeRef ctx ref)) (fromParsedUrl ctx) asUrl

-- | Read a flake reference that has no fragment.
parseFlakeRef :: InputContext -> ByteString -> Checked FlakeRef
parseFlakeRef ctx ref = do
  (flakeRef, fragment) <- parseFlakeRefWithFragment ctx ref
  if BS.null fragment
    then pure flakeRef
    else refuse ("unexpected fragment '" <> bytesToTextLossy fragment <> "' in flake reference '" <> bytesToTextLossy ref <> "'")

-- | A flake reference from a URL: its @dir@ parameter the subdirectory,
-- and its fragment returned beside it.
fromParsedUrl :: InputContext -> ParsedUrl -> Checked (FlakeRef, ByteString)
fromParsedUrl ctx url = do
  let subdir = Map.findWithDefault BS.empty dirParam (urlQuery url)
  input <- inputFromUrl ctx RequireTree url {urlQuery = Map.delete dirParam (urlQuery url), urlFragment = BS.empty}
  pure (FlakeRef input subdir, urlFragment url)

-- | A path flake reference, as upstream reads one with no base
-- directory: the path up to the first @?@ or @#@, the query up to the
-- next @#@, read leniently, and the fragment, decoded.  The path must be
-- absolute.
pathFlakeRef :: InputContext -> ByteString -> Checked (FlakeRef, ByteString)
pathFlakeRef ctx ref = do
  let (path, afterPath) = BC.break (`elem` ("?#" :: String)) ref
      (queryPart, fragmentPart) = case BC.stripPrefix "?" afterPath of
        Just afterMark -> BC.break (== '#') afterMark
        Nothing -> (BS.empty, afterPath)
      fragment = BS.drop 1 fragmentPart
  -- The fragment is @.*@ in upstream's pattern, which matches no line
  -- break.
  if BC.any (`elem` ("\n\r" :: String)) fragment
    then refuse ("invalid flakeref '" <> bytesToTextLossy ref <> "'")
    else do
      query <- decodeQuery Lenient queryPart
      decodedFragment <- percentDecode fragment
      if isAbsoluteHostPath (icPathRules ctx) path
        then fromParsedUrl ctx (ParsedUrl "path" (Just emptyAuthority) (pathToUrlPath (icPathRules ctx) path) query decodedFragment)
        else refuse ("flake reference '" <> bytesToTextLossy ref <> "' is not an absolute path")

-- | A flake ID reference's ID and ref or revision, and its fragment
-- still encoded, when the whole string is one: upstream's pattern of a
-- flake ID, then a @\/@ and a ref or revision, then a @#@ and a fragment.
-- A ref's characters include @\/@, so a ref followed by a revision reads
-- as one ref here as in upstream's pattern; the input splits them.
flakeIdParts :: ByteString -> Maybe (ByteString, ByteString)
flakeIdParts ref = do
  let (idPart, fragmentPart) = BC.break (== '#') ref
      (flakeId, afterId) = BC.break (== '/') idPart
  guard (isFlakeIdText flakeId)
  guard (BS.null afterId || maybe False matchesRef (BC.stripPrefix "/" afterId))
  fragment <- case BC.stripPrefix "#" fragmentPart of
    Nothing -> Just BS.empty
    Just encoded | validFragment encoded -> Just encoded
    _ -> Nothing
  pure (idPart, fragment)
  where
    isFlakeIdText s = case BC.uncons s of
      Just (first, rest) -> isLetter first && BC.all (\c -> isLetter c || isDigit c || c == '_' || c == '-') rest
      Nothing -> False
    matchesRef s = case BC.uncons s of
      Just (first, rest) -> (isLetter first || isDigit first || first == '@') && BC.all (\c -> isLetter c || isDigit c || c `elem` ("_./@+-" :: String)) rest
      Nothing -> False
    isLetter c = isAsciiLower c || isAsciiUpper c
    validFragment s = case BC.uncons s of
      Nothing -> True
      Just ('%', rest) -> case BC.unpack (BC.take 2 rest) of
        [hi, lo] | isHexDigit hi && isHexDigit lo -> validFragment (BS.drop 2 rest)
        _ -> False
      Just (c, rest) -> fragmentChar c && validFragment rest
    fragmentChar c =
      isLetter c || isDigit c || c `elem` ("-._~" :: String) || c `elem` ("!$&'\"()*+,;=" :: String) || c `elem` (":@/? ^" :: String)

-- | A flake reference from attributes, as upstream's @FlakeRef::fromAttrs@
-- reads them: @dir@ the subdirectory, the rest the input's.
flakeRefFromAttrs :: InputContext -> Attrs -> Checked FlakeRef
flakeRefFromAttrs ctx attrs = do
  subdir <- maybeStrAttr dirKey attrs
  input <- inputFromAttrs ctx (Map.delete dirKey attrs)
  pure (FlakeRef input (fromMaybe BS.empty subdir))

-- | A flake reference's attributes, the input's with @dir@ beside them
-- for a subdirectory, an attribute the input has already keeping its
-- value.
flakeRefAttrs :: FlakeRef -> Attrs
flakeRefAttrs (FlakeRef input subdir)
  | BS.null subdir = inputAttrs input
  | otherwise = Map.insertWith (\_ held -> held) dirKey (AttrString subdir) (inputAttrs input)

-- | A flake reference as its URL, with @dir@ for a subdirectory.
flakeRefToString :: InputContext -> FlakeRef -> Checked ByteString
flakeRefToString ctx (FlakeRef input subdir) =
  inputToUrlString (icPathRules ctx) [(dirParam, subdir) | not (BS.null subdir)] input

-- | The attribute naming a flake's subdirectory.
dirKey :: Text
dirKey = "dir"

-- | The URL parameter naming a flake's subdirectory.
dirParam :: ByteString
dirParam = "dir"
