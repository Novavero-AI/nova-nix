-- | __builtin:buildenv__, upstream's builder of a user environment
-- (@builtinBuildenv@ in builtins\/buildenv.cc): the builder of the
-- derivation @nix-env@ evaluates from its @buildenv.nix@.  Its output is
-- the tree ("Nix.Profile.Tree") of the packages its @derivations@
-- attribute names, with @manifest.nix@ linked to its @manifest@.
--
-- Like @builtin:fetchurl@ and @builtin:unpack@, it runs in this process.
-- The paths in @derivations@ and @manifest@ are in eval's canonical
-- @\/nix\/store@ spelling, and each is read and linked as the store spells
-- it on disk ('storePackagePath'), as @builtin:unpack@ renders @srcs@.
module Nix.Builder.Buildenv
  ( -- * Builder name
    builtinBuildenvBuilder,

    -- * The derivations attribute
    Element (..),
    Activity (..),
    DerivationsError (..),
    parseDerivations,
    elementPackages,
    derivationsErrorText,

    -- * Running
    runBuiltinBuildenv,
  )
where

import Control.Monad (guard)
import Control.Monad.Except (ExceptT (..), liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (isDigit)
import Data.Foldable (traverse_)
import Data.Int (Int32)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Nix.Derivation (Derivation (..))
import Nix.Logger (Logger, logWarning)
import Nix.Profile.Tree (Built (..), Package (..), buildEnvFailureText, buildProfile, storePackagePath, storeTreeReader, warningText, writeTree)
import Nix.Store.Path (StoreDir)
import Nix.Store.Symlink (createSymlinkOfKind)
import System.Directory (createDirectoryIfMissing)
import System.OsPath (OsPath, (</>))
import qualified System.OsPath as OP
import Text.Read (readMaybe)

-- ---------------------------------------------------------------------------
-- Named constants
-- ---------------------------------------------------------------------------

-- | The builder string a derivation names this builtin by.  Bytes, as the
-- 'drvBuilder' field it is compared with is.
builtinBuildenvBuilder :: ByteString
builtinBuildenvBuilder = "builtin:buildenv"

-- | The attribute naming the packages.
derivationsKey :: Text
derivationsKey = "derivations"

-- | The attribute naming the file @manifest.nix@ links to.
manifestKey :: Text
manifestKey = "manifest"

-- | The output the tree is made in.
outputName :: Text
outputName = "out"

-- | The name of the link to the manifest, in the output.  ASCII, which
-- 'OP.unsafeFromChar' narrows to one path unit per character without
-- loss.
manifestLinkName :: OsPath
manifestLinkName = OP.pack (map OP.unsafeFromChar "manifest.nix")

-- | The one activity token that makes an element inactive.
inactiveToken :: ByteString
inactiveToken = "false"

-- | The characters C's @isspace@ takes as space in the C locale, which
-- @stoi@ and @stoul@ skip before a number.
cSpaces :: String
cSpaces = " \t\n\v\f\r"

-- ---------------------------------------------------------------------------
-- The derivations attribute
-- ---------------------------------------------------------------------------

-- | Whether an element's packages go into the tree.
data Activity = Active | Inactive
  deriving (Eq, Show)

-- | One element of the @derivations@ attribute, which @buildenv.nix@ makes
-- of each package as its activity, its priority, its output count and its
-- output paths.
data Element = Element
  { -- | 'Inactive' when the token is @false@, and 'Active' for any other.
    elementActivity :: !Activity,
    -- | The priority, as @stoi@ reads it.
    elementPriority :: !Int,
    -- | The output paths, as the attribute spells them.
    elementPaths :: ![ByteString]
  }
  deriving (Eq, Show)

-- | Why the @derivations@ attribute names no elements.  Upstream trusts
-- the attribute's shape, so where it reads past the last token its
-- behaviour is undefined, and where @stoi@ or @stoul@ throws the build
-- fails with no more than the function's name.
data DerivationsError
  = -- | The attribute ends before an element's priority, its output count
    -- or its last path.
    EndsInsideElement
  | -- | A priority that is not an int: no digit leads it, or it is out of
    -- range.
    BadPriority !ByteString
  | -- | An output count that is not a count: no digit leads it, or it is
    -- negative.
    BadOutputCount !ByteString
  deriving (Eq, Show)

-- | Read the @derivations@ attribute: whitespace-separated tokens with the
-- empty ones dropped, as upstream's @tokenizeString@ gives them, in groups
-- of an activity, a priority, an output count and that many paths
-- (@builtinBuildenv@ in buildenv.cc).
parseDerivations :: ByteString -> Either DerivationsError [Element]
parseDerivations = elementsOf . filter (not . BS.null) . BS8.splitWith (`elem` tokenSeparators)
  where
    -- tokenizeString's default separators (its declaration in strings.hh).
    tokenSeparators = " \t\n\r" :: String

-- | The elements a token list holds, in order.
elementsOf :: [ByteString] -> Either DerivationsError [Element]
elementsOf tokens = case tokens of
  [] -> Right []
  activity : rest -> do
    (priorityToken, afterPriority) <- nextToken rest
    priority <- maybe (Left (BadPriority priorityToken)) Right (stoi priorityToken)
    (countToken, afterCount) <- nextToken afterPriority
    count <- maybe (Left (BadOutputCount countToken)) Right (stoulCount countToken)
    (paths, after) <- takeExactly count afterCount
    (Element (activityOf activity) priority paths :) <$> elementsOf after
  where
    nextToken remaining = case remaining of
      token : after -> Right (token, after)
      [] -> Left EndsInsideElement
    activityOf token
      | token == inactiveToken = Inactive
      | otherwise = Active

-- | The first @count@ tokens and the rest, when there are that many.
takeExactly :: Integer -> [ByteString] -> Either DerivationsError ([ByteString], [ByteString])
takeExactly count tokens
  | count > toInteger (maxBound :: Int) = Left EndsInsideElement
  | length taken == wanted = Right (taken, after)
  | otherwise = Left EndsInsideElement
  where
    wanted = fromInteger count
    (taken, after) = splitAt wanted tokens

-- | The int @stoi@ reads from a token: 'leadingInteger', within an int's
-- range.  Whatever follows the digits is ignored, so a float priority,
-- which @buildenv.nix@ writes as @5.500000@, reads as 5.
stoi :: ByteString -> Maybe Int
stoi token = do
  value <- leadingInteger token
  guard (value >= toInteger (minBound :: Int32) && value <= toInteger (maxBound :: Int32))
  pure (fromInteger value)

-- | The count @stoul@ reads from a token: 'leadingInteger', where @-0@ is
-- zero and any other negative number is refused, since @stoul@ wraps it
-- to a count larger than any attribute holds.
stoulCount :: ByteString -> Maybe Integer
stoulCount token = do
  value <- leadingInteger token
  guard (value >= 0)
  pure value

-- | The decimal number at the start of a token, as @strtol@ reads one:
-- C whitespace, then a sign, then at least one digit.
leadingInteger :: ByteString -> Maybe Integer
leadingInteger token = case BS8.uncons unspaced of
  Just ('-', rest) -> negate <$> digits rest
  Just ('+', rest) -> digits rest
  _ -> digits unspaced
  where
    unspaced = BS8.dropWhile (`elem` cSpaces) token
    digits = readMaybe . BS8.unpack . BS8.takeWhile isDigit

-- | Each path of each active element, with the element's priority, in
-- order: upstream's builder passes an inactive one to @buildProfile@,
-- which never reads it (buildenv.cc).
elementPackages :: [Element] -> [(ByteString, Int)]
elementPackages elements = [(path, priority) | Element Active priority paths <- elements, path <- paths]

-- | A @derivations@ failure in words.
derivationsErrorText :: DerivationsError -> Text
derivationsErrorText failure = case failure of
  EndsInsideElement -> "the 'derivations' attribute ends inside an element"
  BadPriority token -> "the 'derivations' attribute gives '" <> shown token <> "' as a priority, which is not an int"
  BadOutputCount token -> "the 'derivations' attribute gives '" <> shown token <> "' as an output count, which is not a count"
  where
    shown = TE.decodeUtf8Lenient

-- ---------------------------------------------------------------------------
-- Running
-- ---------------------------------------------------------------------------

-- | Run a @builtin:buildenv@ derivation: make its @out@ output the tree of
-- the packages @derivations@ names, giving the logger each warning the
-- tree gives, and link @manifest.nix@ in it to @manifest@.  Upstream reads
-- @manifest@ once the tree is made, so a tree that fails is the failure
-- reported when both do.  Returns a failure's exit code and message, which
-- "Nix.Builder" reports as a refused builder, so the shared validation and
-- registration there follow.
runBuiltinBuildenv :: Logger -> StoreDir -> Derivation -> [(Text, FilePath)] -> IO (Either (Int, Text) ())
runBuiltinBuildenv logger storeDir drv outputDirs = first failed <$> runExceptT build
  where
    failed reason = (1, "builtin:buildenv: " <> reason)
    build = do
      outDir <- maybe (throwError "derivation defines no 'out' output") pure (lookup outputName outputDirs)
      liftIO (createDirectoryIfMissing True outDir)
      elements <- attribute derivationsKey >>= liftEither . first derivationsErrorText . parseDerivations
      packages <- traverse (\(path, priority) -> (`Package` priority) <$> named derivationsKey path) (elementPackages elements)
      out <- liftIO (OP.encodeFS outDir)
      Built warnings tree <- liftIO (buildProfile (storeTreeReader storeDir) out packages)
      liftIO (traverse_ (logWarning logger . warningText) warnings)
      linked <- liftEither (first buildEnvFailureText tree)
      ExceptT (writeTree out linked)
      manifest <- attribute manifestKey >>= named manifestKey
      ExceptT (createSymlinkOfKind (out </> manifestLinkName) manifest)
    attribute :: Text -> ExceptT Text IO ByteString
    attribute key = maybe (throwError ("attribute '" <> key <> "' missing")) pure (Map.lookup key (drvEnv drv))
    named :: Text -> ByteString -> ExceptT Text IO OsPath
    named key path =
      liftIO (storePackagePath storeDir path)
        >>= maybe (throwError ("the '" <> key <> "' attribute names '" <> TE.decodeUtf8Lenient path <> "', which is not a store path")) pure
