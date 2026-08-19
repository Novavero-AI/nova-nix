-- | Nix derivation representation and serialization.
--
-- == What is a derivation?
--
-- A derivation (.drv file) is a BUILD RECIPE.  It is the output of
-- evaluating a Nix expression.  When you write:
--
-- @
-- stdenv.mkDerivation {
--   pname = "hello";
--   version = "2.12.1";
--   src = fetchurl { ... };
--   buildInputs = [ zlib ];
-- }
-- @
--
-- The evaluator reduces this to a @.drv@ file that says:
--
-- @
-- Derive(
--   [("out", "\/nix\/store\/abc...-hello-2.12.1", "", "sha256")],
--   [("\/nix\/store\/def...-zlib-1.3.1.drv", ["out"]),
--    ("\/nix\/store\/ghi...-bash-5.2.drv", ["out"])],
--   ["\/nix\/store\/jkl...-hello-2.12.1.tar.gz"],
--   "x86_64-linux",
--   "\/nix\/store\/mno...-bash-5.2\/bin\/bash",
--   ["\/nix\/store\/pqr...-stdenv\/setup"],
--   [("buildInputs", "\/nix\/store\/def...-zlib-1.3.1"),
--    ("builder", "\/nix\/store\/mno...-bash-5.2\/bin\/bash"),
--    ("name", "hello-2.12.1"),
--    ("out", "\/nix\/store\/abc...-hello-2.12.1"),
--    ("src", "\/nix\/store\/jkl...-hello-2.12.1.tar.gz"),
--    ("system", "x86_64-linux")]
-- )
-- @
--
-- That's ATerm format.  Every derivation is:
--
-- 1. __Outputs__: what store paths will be produced (usually just "out")
-- 2. __Input derivations__: other .drv files this depends on (and which
--    of their outputs we need)
-- 3. __Input sources__: non-derivation store paths (source tarballs, patches)
-- 4. __Platform__: what system to build on ("x86_64-linux", "x86_64-windows")
-- 5. __Builder__: the executable to run (usually bash)
-- 6. __Args__: command-line arguments to the builder
-- 7. __Environment__: environment variables for the build
--
-- The HASH of this .drv file (after ATerm serialization) determines the
-- output store path.  Change any input yields a different hash and a different output
-- path.  This is how Nix achieves reproducibility.
module Nix.Derivation
  ( -- * Derivation type
    Derivation (..),
    DerivationOutput (..),

    -- * ATerm serialization
    toATerm,
    toATermForHash,
    fromATerm,

    -- * Platform
    Platform (..),
    currentPlatform,
    platformToText,
    textToPlatform,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import Data.Function (on)
import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import Nix.Store.Path (StorePath)
import qualified Nix.Store.Path as SP
import qualified System.Info as SI

-- | Target platform for a derivation.
data Platform
  = X86_64_Linux
  | X86_64_Darwin
  | Aarch64_Darwin
  | X86_64_Windows
  | Aarch64_Linux
  | OtherPlatform !Text
  deriving (Eq, Ord, Show)

-- | Detect the current platform using 'System.Info'.
-- GHC bakes @os@ and @arch@ into the compiled binary at compile time,
-- so this is equivalent to CPP ifdefs but ormolu-compatible.
currentPlatform :: Platform
currentPlatform = case (SI.arch, SI.os) of
  ("x86_64", "mingw32") -> X86_64_Windows
  ("x86_64", "darwin") -> X86_64_Darwin
  ("aarch64", "darwin") -> Aarch64_Darwin
  ("aarch64", "linux") -> Aarch64_Linux
  ("x86_64", "linux") -> X86_64_Linux
  (arch, os) -> OtherPlatform (packPlatform arch os)

-- | Format an arch-os pair as a Nix platform string.
packPlatform :: String -> String -> Text
packPlatform arch os =
  let archText = case arch of
        "x86_64" -> "x86_64"
        "aarch64" -> "aarch64"
        other -> other
      osText = case os of
        "mingw32" -> "windows"
        "darwin" -> "darwin"
        "linux" -> "linux"
        other -> other
   in mconcat [T.pack archText, "-", T.pack osText]

-- | Convert a 'Platform' to its Nix text representation.
platformToText :: Platform -> Text
platformToText X86_64_Linux = "x86_64-linux"
platformToText X86_64_Darwin = "x86_64-darwin"
platformToText Aarch64_Darwin = "aarch64-darwin"
platformToText X86_64_Windows = "x86_64-windows"
platformToText Aarch64_Linux = "aarch64-linux"
platformToText (OtherPlatform t) = t

-- | Parse a Nix platform string into a 'Platform'.
textToPlatform :: Text -> Platform
textToPlatform t = case t of
  "x86_64-linux" -> X86_64_Linux
  "x86_64-darwin" -> X86_64_Darwin
  "aarch64-darwin" -> Aarch64_Darwin
  "x86_64-windows" -> X86_64_Windows
  "aarch64-linux" -> Aarch64_Linux
  other -> OtherPlatform other

-- | A single output of a derivation.
data DerivationOutput = DerivationOutput
  { -- | Output name (usually "out", sometimes "dev", "lib", "doc").
    doName :: !Text,
    -- | The store path this output will be placed at.
    doPath :: !StorePath,
    -- | Hash algorithm (empty for input-addressed, "sha256" for fixed).
    doHashAlgo :: !Text,
    -- | Expected hash (empty for input-addressed, actual hash for fixed).
    doHash :: !Text
  }
  deriving (Eq, Show)

-- | A complete derivation - everything needed to build a package.
--
-- The builder, args, and env VALUES are byte strings: they are coerced
-- Nix strings, which carry arbitrary bytes, and they flow byte-exact
-- into the ATerm (and therefore the @.drv@ hash and every output path).
-- Env KEYS are attr names, which nova-nix constrains to valid UTF-8
-- 'Text'; for valid UTF-8, Text ordering equals byte ordering, so the
-- sorted env section matches upstream's bytewise @std::map@ order.
data Derivation = Derivation
  { -- | What this derivation produces.
    drvOutputs :: ![DerivationOutput],
    -- | Other derivations this depends on, and which outputs we need.
    drvInputDrvs :: !(Map StorePath [Text]),
    -- | Non-derivation store paths used as inputs (sources, patches).
    drvInputSrcs :: ![StorePath],
    -- | Target platform: "x86_64-linux", "x86_64-windows", etc.
    drvPlatform :: !Platform,
    -- | The builder executable (store path to bash, or other).
    drvBuilder :: !ByteString,
    -- | Arguments to the builder.
    drvArgs :: ![ByteString],
    -- | Environment variables for the build.
    drvEnv :: !(Map Text ByteString)
  }
  deriving (Eq, Show)

-- | Serialize a derivation to ATerm format (the .drv file format).
-- This serialization is what gets hashed to compute the store path,
-- so the result is BYTES: env values, args, and the builder land in it
-- exactly as coerced, with no encoding step in between.
--
-- Format: @Derive([outputs],[inputDrvs],[inputSrcs],platform,builder,[args],[env])@
toATerm :: Derivation -> ByteString
toATerm = toATermForHash False Nothing

-- | Serialize a derivation for HASHING, with the two knobs the Nix
-- derivation-hash algorithm needs:
--
--   * @maskOutputs@: render every output path as the empty string.  Used
--     when computing a derivation's own output paths (not yet known) via
--     @hashDerivationModulo@.
--   * @inputSubst@: when @Just subs@, render the input-derivations section
--     from @subs@ - pairs of @(moduloHashHex, outputNames)@ - instead of
--     from 'drvInputDrvs'.  Each input derivation's store path is replaced
--     by its own modulo hash, so two derivations differing only in an
--     input's path spelling (not its content) hash identically.
--
-- @toATermForHash False Nothing@ is exactly 'toATerm'.
toATermForHash :: Bool -> Maybe [(Text, [Text])] -> Derivation -> ByteString
toATermForHash maskOutputs inputSubst drv =
  LBS.toStrict
    $ B.toLazyByteString
    $ "Derive("
      <> atermOutputsWith maskOutputs (drvOutputs drv)
      <> ","
      <> inputDrvsSection
      <> ","
      <> atermInputSrcs (drvInputSrcs drv)
      <> ","
      <> atermStringT (platformToText (drvPlatform drv))
      <> ","
      <> atermString (drvBuilder drv)
      <> ","
      <> atermList (map atermString (drvArgs drv))
      <> ","
      <> atermEnv (drvEnv drv)
      <> ")"
  where
    inputDrvsSection = case inputSubst of
      Nothing -> atermInputDrvs (drvInputDrvs drv)
      Just subs -> atermInputDrvsSubst subs

-- | Serialize outputs: @[(name,path,hashAlgo,hash)]@, sorted by output name.
-- When @maskOutputs@ is set, every path is rendered as @\"\"@ (used by the
-- masked modulo hash, where output paths aren't known yet).
atermOutputsWith :: Bool -> [DerivationOutput] -> B.Builder
atermOutputsWith maskOutputs outs =
  let sorted = sortBy (compare `on` doName) outs
   in atermList (map render sorted)
  where
    render out =
      "("
        <> atermStringT (doName out)
        <> ","
        <> atermStringT (if maskOutputs then "" else SP.storePathToText SP.defaultStoreDir (doPath out))
        <> ","
        <> atermStringT (doHashAlgo out)
        <> ","
        <> atermStringT (doHash out)
        <> ")"

-- | Input-derivations serializer for the modulo substitution: keys are the
-- modulo-hash hex strings.  Entries that share a key are merged, unioning
-- their output-name sets, so the section carries one entry per distinct modulo
-- hash - mirroring upstream @hashDerivationModulo@'s @std::map<Hash, StringSet>@,
-- where two inputs that collapse to the same modulo hash (e.g. two fetches
-- differing only in URL) become a single entry.  'Set.union' makes the merge
-- order-independent; 'Map.toAscList' and 'Set.toAscList' fix the ascending
-- hex-key and output-name order (hex is lowercase, so this matches the bytewise
-- order of upstream's @std::map<std::string>@).
atermInputDrvsSubst :: [(Text, [Text])] -> B.Builder
atermInputDrvsSubst subs =
  let merged = Map.fromListWith Set.union [(key, Set.fromList outs) | (key, outs) <- subs]
   in atermList (map render (Map.toAscList merged))
  where
    render (key, outs) = "(" <> atermStringT key <> "," <> atermList (map atermStringT (Set.toAscList outs)) <> ")"

-- | Sort and deduplicate output names (matches C++ @std::set<string>@).
sortNubText :: [Text] -> [Text]
sortNubText = Set.toAscList . Set.fromList

-- | Serialize input derivations: @[(drvPath,[outName1,outName2])]@
-- Sorted by store path for determinism.
atermInputDrvs :: Map StorePath [Text] -> B.Builder
atermInputDrvs drvs =
  let sorted = Map.toAscList drvs
   in atermList (map atermInputDrv sorted)

atermInputDrv :: (StorePath, [Text]) -> B.Builder
atermInputDrv (sp, outs) =
  "("
    <> atermStringT (SP.storePathToText SP.defaultStoreDir sp)
    <> ","
    <> atermList (map atermStringT (sortNubText outs))
    <> ")"

-- | Serialize input sources: @[path1,path2,...]@
-- Sorted for deterministic ATerm hashing.
atermInputSrcs :: [StorePath] -> B.Builder
atermInputSrcs srcs =
  let sorted = sortBy (compare `on` SP.storePathToText SP.defaultStoreDir) srcs
   in atermList (map (atermStringT . SP.storePathToText SP.defaultStoreDir) sorted)

-- | Bracketed, comma-separated section.
atermList :: [B.Builder] -> B.Builder
atermList items = "[" <> mconcat (intersperseComma items) <> "]"
  where
    intersperseComma [] = []
    intersperseComma [only] = [only]
    intersperseComma (item : rest) = item : "," : intersperseComma rest

-- | Serialize environment: @[(key,value)]@ sorted by key.  Keys are
-- valid-UTF-8 Text, so 'Map.toAscList' order equals upstream's bytewise
-- order; values are raw bytes.
atermEnv :: Map Text ByteString -> B.Builder
atermEnv env =
  let sorted = Map.toAscList env
   in atermList (map atermEnvPair sorted)

atermEnvPair :: (Text, ByteString) -> B.Builder
atermEnvPair (key, val) =
  "(" <> atermStringT key <> "," <> atermString val <> ")"

-- | ATerm string: double-quoted with standard escaping, byte-level.
-- Every escapable is a single ASCII byte, so escaping the byte stream is
-- exactly upstream's per-char escaping over its byte strings.
atermString :: ByteString -> B.Builder
atermString s = "\"" <> escapeATermBytes s <> "\""

-- | ATerm string from Text (names, store paths, hex keys): UTF-8 bytes,
-- then the shared byte-level escaping.
atermStringT :: Text -> B.Builder
atermStringT = atermString . TE.encodeUtf8

-- | Escape the five ATerm specials; all other bytes pass through verbatim.
-- Scans for the next special with 'BC.break' so clean spans copy in bulk.
escapeATermBytes :: ByteString -> B.Builder
escapeATermBytes s =
  let (plain, rest) = BC.break atermSpecial s
   in case BC.uncons rest of
        Nothing -> B.byteString plain
        Just (c, remaining) ->
          B.byteString plain <> escapeOne c <> escapeATermBytes remaining
  where
    atermSpecial c = c == '\\' || c == '"' || c == '\n' || c == '\r' || c == '\t'
    escapeOne '\\' = "\\\\"
    escapeOne '"' = "\\\""
    escapeOne '\n' = "\\n"
    escapeOne '\r' = "\\r"
    escapeOne '\t' = "\\t"
    -- Unreachable: 'BC.break atermSpecial' only stops at the five specials.
    escapeOne c = B.charUtf8 c

-- ---------------------------------------------------------------------------
-- ATerm parser (hand-rolled recursive descent)
-- ---------------------------------------------------------------------------

-- | Parser state: remaining input bytes.  The @.drv@ on disk is a byte
-- string; only Text-shaped fields (names, paths, keys) decode, and only
-- after unescaping.
newtype Parser a = Parser {runParser :: ByteString -> Either Text (a, ByteString)}

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> case p input of
    Left err -> Left err
    Right (val, rest) -> Right (f val, rest)

instance Applicative Parser where
  pure val = Parser $ \input -> Right (val, input)
  Parser pf <*> Parser pa = Parser $ \input -> case pf input of
    Left err -> Left err
    Right (f, rest) -> case pa rest of
      Left err -> Left err
      Right (a, rest2) -> Right (f a, rest2)

instance Monad Parser where
  Parser pa >>= f = Parser $ \input -> case pa input of
    Left err -> Left err
    Right (a, rest) -> runParser (f a) rest

parserFail :: Text -> Parser a
parserFail msg = Parser $ \_ -> Left msg

-- | Lossy decode for parse-error snippets only.
snippet :: ByteString -> Text
snippet = TE.decodeUtf8With lenientDecode

-- | Consume a specific character.
pChar :: Char -> Parser ()
pChar expected = Parser $ \input ->
  case BC.uncons input of
    Just (c, rest) | c == expected -> Right ((), rest)
    Just (c, _) -> Left ("expected '" <> T.singleton expected <> "' but got '" <> T.singleton c <> "'")
    Nothing -> Left ("expected '" <> T.singleton expected <> "' but got end of input")

-- | Consume a specific string prefix.
pString :: ByteString -> Parser ()
pString prefix = Parser $ \input ->
  case BS.stripPrefix prefix input of
    Just rest -> Right ((), rest)
    Nothing -> Left ("expected \"" <> snippet prefix <> "\" at: " <> snippet (BS.take 20 input))

-- | Parse a quoted ATerm string with escape handling; the content is the
-- raw unescaped bytes.
pQuotedString :: Parser ByteString
pQuotedString = do
  pChar '"'
  content <- pStringContent
  pChar '"'
  pure content

-- | Like 'pQuotedString' for Text-shaped fields (names, store paths, env
-- keys, platform): strict UTF-8 decode after unescaping, so invalid bytes
-- in a field nova-nix represents as Text are a parse error, never mojibake.
pQuotedText :: Parser Text
pQuotedText = do
  content <- pQuotedString
  case TE.decodeUtf8' content of
    Right t -> pure t
    Left _ -> parserFail ("invalid UTF-8 in ATerm string: " <> snippet (BS.take 40 content))

-- | Parse the contents of a quoted string (up to unescaped quote).
-- Scans to the next special byte with 'BC.break' so clean spans are
-- copied in bulk, accumulating reversed chunks (O(n) total).
pStringContent :: Parser ByteString
pStringContent = Parser $ \input -> go input []
  where
    go remaining acc =
      let (plain, rest) = BC.break (\c -> c == '"' || c == '\\') remaining
       in case BC.uncons rest of
            Nothing -> Left "unterminated string"
            Just ('"', _) -> Right (BS.concat (reverse (plain : acc)), rest)
            Just ('\\', afterSlash) -> case BC.uncons afterSlash of
              Nothing -> Left "unterminated escape"
              Just ('\\', rest2) -> go rest2 ("\\" : plain : acc)
              Just ('"', rest2) -> go rest2 ("\"" : plain : acc)
              Just ('n', rest2) -> go rest2 ("\n" : plain : acc)
              Just ('r', rest2) -> go rest2 ("\r" : plain : acc)
              Just ('t', rest2) -> go rest2 ("\t" : plain : acc)
              -- A non-standard escape keeps the byte and drops the
              -- backslash, as upstream's .drv string parser does.
              Just (c, rest2) -> go rest2 (BC.singleton c : plain : acc)
            -- Unreachable: BC.break stops only at '"' or '\\'.
            Just (c, _) -> Left ("pStringContent: impossible break byte '" <> T.singleton c <> "'")

-- | Parse a comma-separated list enclosed in brackets: @[item,item,...]@
pList :: Parser a -> Parser [a]
pList pItem = do
  pChar '['
  items <- pSepBy pItem (pChar ',')
  pChar ']'
  pure items

-- | Parse zero or more items separated by a delimiter.
pSepBy :: Parser a -> Parser () -> Parser [a]
pSepBy pItem pSep = Parser $ \input ->
  case runParser pItem input of
    Left _ -> Right ([], input) -- empty list
    Right (first, rest) -> goMore rest [first]
  where
    goMore remaining acc =
      case runParser pSep remaining of
        Left _ -> Right (reverse acc, remaining)
        Right ((), afterSep) ->
          case runParser pItem afterSep of
            Left err -> Left err
            Right (item, rest2) -> goMore rest2 (item : acc)

-- | Parse a single output tuple: @(name, path, hashAlgo, hash)@.
-- The output name is validated with the store-name rules at this parse
-- boundary: it later becomes a filesystem component under the build
-- dir and an environment variable name, and a @.drv@ read from disk
-- is input, not trusted state.
pOutput :: Parser DerivationOutput
pOutput = do
  pChar '('
  name <- pQuotedText
  pChar ','
  pathStr <- pQuotedText
  pChar ','
  hashAlgo <- pQuotedText
  pChar ','
  hashVal <- pQuotedText
  pChar ')'
  case SP.checkStorePathName name of
    Left err -> parserFail ("invalid output name: " <> SP.storePathNameErrorText err)
    Right () -> case parseStorePathFromATerm pathStr of
      Just sp -> pure (DerivationOutput name sp hashAlgo hashVal)
      Nothing -> parserFail ("invalid output store path: " <> pathStr)

-- | Parse a store path from an ATerm string.
-- Tries defaultStoreDir first, then Windows store dir.
parseStorePathFromATerm :: Text -> Maybe StorePath
parseStorePathFromATerm pathStr =
  case SP.parseStorePath SP.defaultStoreDir pathStr of
    Just sp -> Just sp
    Nothing -> SP.parseStorePath (SP.StoreDir "C:\\nix\\store") pathStr

-- | Parse an input derivation tuple: @(drvPath, [outName1, outName2])@.
pInputDrv :: Parser (StorePath, [Text])
pInputDrv = do
  pChar '('
  pathStr <- pQuotedText
  pChar ','
  outs <- pList pQuotedText
  pChar ')'
  case parseStorePathFromATerm pathStr of
    Just sp -> pure (sp, outs)
    Nothing -> parserFail ("invalid input drv store path: " <> pathStr)

-- | Parse an input source (quoted store path string).
pInputSrc :: Parser StorePath
pInputSrc = do
  pathStr <- pQuotedText
  case parseStorePathFromATerm pathStr of
    Just sp -> pure sp
    Nothing -> parserFail ("invalid input src store path: " <> pathStr)

-- | Parse an environment pair: @(key, value)@.  The key is an attr name
-- (Text); the value keeps its raw bytes.
pEnvPair :: Parser (Text, ByteString)
pEnvPair = do
  pChar '('
  key <- pQuotedText
  pChar ','
  val <- pQuotedString
  pChar ')'
  pure (key, val)

-- | Parse a full derivation from its ATerm bytes.
-- Format: @Derive([outputs],[inputDrvs],[inputSrcs],platform,builder,[args],[env])@
--
-- Total function: returns @Left@ on any malformed input.
fromATerm :: ByteString -> Either Text Derivation
fromATerm input = case runParser pDerivation input of
  Left err -> Left ("ATerm parse error: " <> err)
  Right (drv, remaining)
    | BS.null remaining -> Right drv
    | otherwise -> Left ("ATerm parse error: unexpected trailing: " <> snippet (BS.take 40 remaining))

pDerivation :: Parser Derivation
pDerivation = do
  pString "Derive("
  outputs <- pList pOutput
  pChar ','
  inputDrvsList <- pList pInputDrv
  pChar ','
  inputSrcs <- pList pInputSrc
  pChar ','
  platformStr <- pQuotedText
  pChar ','
  builder <- pQuotedString
  pChar ','
  args <- pList pQuotedString
  pChar ','
  envPairs <- pList pEnvPair
  pChar ')'
  let inputDrvs = Map.fromListWith (++) inputDrvsList
      env = Map.fromList envPairs
      platform = textToPlatform platformStr
  pure
    Derivation
      { drvOutputs = outputs,
        drvInputDrvs = inputDrvs,
        drvInputSrcs = inputSrcs,
        drvPlatform = platform,
        drvBuilder = builder,
        drvArgs = args,
        drvEnv = env
      }
