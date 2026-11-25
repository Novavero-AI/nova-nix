-- | The build environment @nix develop@ replays, and the bash script that
-- replays it, as upstream Nix 2.35.4 makes them (@src\/nix\/develop.cc@).
--
-- A capture builds a copy of the target's derivation whose builder runs
-- upstream's @get-env.sh@: it sources the stdenv's setup, then writes
-- the variables and functions bash holds, less bash's own, as JSON.  That
-- document is a t'BuildEnvironment', read by 'parseBuildEnvironment' and written by
-- 'buildEnvironmentJson'.  'environmentScript' turns one into the script
-- @print-dev-env@ prints, which sets each variable and function again
-- with each output's path moved under the working directory, and
-- 'renderRcfile' wraps that script for the shell @develop@ starts.
--
-- Everything here is pure: the caller reads the capture, writes the
-- files, and starts the shell.  A path is the bytes nova-nix holds for it
-- ("Nix.HostPath"), and the scripts are bytes, since a path need not be
-- UTF-8.
module Nix.Develop.Environment
  ( -- * The captured environment
    BuildEnvironment (..),
    Variable (..),
    AttrsFiles (..),
    parseBuildEnvironment,
    buildEnvironmentJson,

    -- * The environment script
    ignoreVars,
    escapeShellArgAlways,
    ScriptDirectories (..),
    EnvironmentScript (..),
    ScriptFile (..),
    environmentScript,
    appendPath,

    -- * The rcfile
    Shell (..),
    ShellMode (..),
    Prompt (..),
    renderRcfile,
  )
where

import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (toList)
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Nix.Json (Json (..), parseJson)

-- ---------------------------------------------------------------------------
-- The captured environment
-- ---------------------------------------------------------------------------

-- | What @get-env.sh@ captured from a builder's bash: upstream's
-- @BuildEnvironment@ in develop.cc.
data BuildEnvironment = BuildEnvironment
  { -- | Each variable, by name.
    beVariables :: !(Map Text Variable),
    -- | Each function's body, by name: what bash's @type@ prints between
    -- the function's outer braces.
    beFunctions :: !(Map Text Text),
    -- | The structured attributes' files the builder read, when the
    -- derivation has structured attributes.
    beAttrsFiles :: !(Maybe AttrsFiles)
  }
  deriving (Eq, Show)

-- | A variable's value, of one of the four kinds @get-env.sh@ writes.
data Variable
  = -- | A string bash does not export.
    ShellString !Text
  | -- | A string bash exports.
    ExportedString !Text
  | -- | An indexed array.
    IndexedArray ![Text]
  | -- | An associative array, by key.
    AssociativeArray !(Map Text Text)
  deriving (Eq, Show)

-- | The contents of the two files a structured-attributes builder reads,
-- as @get-env.sh@ read them, which drops their trailing newlines.
data AttrsFiles = AttrsFiles
  { -- | @.attrs.sh@.
    attrsSh :: !Text,
    -- | @.attrs.json@.
    attrsJson :: !Text
  }
  deriving (Eq, Show)

-- | The document's members, as @get-env.sh@ and upstream's @toJSON@ name
-- them.
variablesMember, functionsMember, attrsFilesMember, attrsShMember, attrsJsonMember, typeMember, valueMember :: Text
variablesMember = "variables"
functionsMember = "bashFunctions"
attrsFilesMember = "structuredAttrs"
attrsShMember = ".attrs.sh"
attrsJsonMember = ".attrs.json"
typeMember = "type"
valueMember = "value"

-- | The name each 'Variable' kind has in the document.
shellStringType, exportedStringType, indexedArrayType, associativeArrayType :: Text
shellStringType = "var"
exportedStringType = "exported"
indexedArrayType = "array"
associativeArrayType = "associative"

-- | Read a captured environment as upstream's @BuildEnvironment::parseJSON@
-- does (with @BuildEnvironment::fromJSON@, in develop.cc): the document as
-- nlohmann parses it, which refuses one that is not UTF-8, then each
-- variable, each function and the structured attributes' files.  A variable
-- of a kind other than the four is dropped, as upstream drops it:
-- @get-env.sh@ writes @\"unknown\"@ and no value for a readonly, integer or
-- reference variable.
--
-- Upstream takes the document's shape on trust: a member it reads that is
-- missing is undefined behaviour in nlohmann's lookup, and one of the
-- wrong JSON type an exception.  Each is a 'Left' here, naming the member.
parseBuildEnvironment :: ByteString -> Either Text BuildEnvironment
parseBuildEnvironment bytes = do
  text <- first (const "the build environment is not UTF-8") (TE.decodeUtf8' bytes)
  top <- parseJson text >>= jsonObject document
  variables <- member document variablesMember top >>= jsonObject (quote variablesMember) >>= Map.traverseMaybeWithKey readVariable
  functions <- member document functionsMember top >>= jsonObject (quote functionsMember) >>= Map.traverseWithKey (jsonString . ("function " <>) . quote)
  attrsFiles <- traverse readAttrsFiles (Map.lookup attrsFilesMember top)
  pure (BuildEnvironment variables functions attrsFiles)
  where
    document = "the build environment"
    readAttrsFiles json = do
      files <- jsonObject (quote attrsFilesMember) json
      AttrsFiles <$> attrsFile attrsShMember files <*> attrsFile attrsJsonMember files
    attrsFile name files = member (quote attrsFilesMember) name files >>= jsonString (quote name)

-- | One variable of the document, or 'Nothing' for a kind it drops.
readVariable :: Text -> Json -> Either Text (Maybe Variable)
readVariable name json = do
  info <- jsonObject what json
  kind <- member what typeMember info >>= jsonString ("the type of " <> what)
  readKind kind info
  where
    what = "variable " <> quote name
    value = member what valueMember
    valueWhat = "the value of " <> what
    readKind kind info
      | kind == shellStringType = Just . ShellString <$> (value info >>= jsonString valueWhat)
      | kind == exportedStringType = Just . ExportedString <$> (value info >>= jsonString valueWhat)
      | kind == indexedArrayType = Just . IndexedArray <$> (value info >>= jsonArray valueWhat >>= traverse (jsonString valueWhat))
      | kind == associativeArrayType = Just . AssociativeArray <$> (value info >>= jsonObject valueWhat >>= traverse (jsonString valueWhat))
      | otherwise = Right Nothing

-- | A name in single quotes, for an error message.
quote :: Text -> Text
quote name = "'" <> name <> "'"

-- | A member of an object, which @what@ names in the error.
member :: Text -> Text -> Map Text Json -> Either Text Json
member what name members = maybe (Left (what <> " has no " <> quote name)) Right (Map.lookup name members)

jsonObject :: Text -> Json -> Either Text (Map Text Json)
jsonObject _ (JsonObject members) = Right members
jsonObject what _ = Left (what <> " is not a JSON object")

jsonArray :: Text -> Json -> Either Text [Json]
jsonArray _ (JsonArray items) = Right items
jsonArray what _ = Left (what <> " is not a JSON array")

jsonString :: Text -> Json -> Either Text Text
jsonString _ (JsonString s) = Right s
jsonString what _ = Left (what <> " is not a JSON string")

-- | A captured environment as upstream's @BuildEnvironment::toJSON@ writes
-- it (develop.cc), which 'parseBuildEnvironment' reads back as the
-- same environment.
buildEnvironmentJson :: BuildEnvironment -> Json
buildEnvironmentJson env =
  JsonObject . Map.fromList $
    [ (variablesMember, JsonObject (Map.map variableJson (beVariables env))),
      (functionsMember, JsonObject (Map.map JsonString (beFunctions env)))
    ]
      <> [(attrsFilesMember, attrsFilesJson files) | Just files <- [beAttrsFiles env]]
  where
    variableJson variable = case variable of
      ShellString s -> typed shellStringType (JsonString s)
      ExportedString s -> typed exportedStringType (JsonString s)
      IndexedArray items -> typed indexedArrayType (JsonArray (map JsonString items))
      AssociativeArray entries -> typed associativeArrayType (JsonObject (Map.map JsonString entries))
    typed kind value = JsonObject (Map.fromList [(typeMember, JsonString kind), (valueMember, value)])
    attrsFilesJson files =
      JsonObject (Map.fromList [(attrsShMember, JsonString (attrsSh files)), (attrsJsonMember, JsonString (attrsJson files))])

-- ---------------------------------------------------------------------------
-- The environment script
-- ---------------------------------------------------------------------------

-- | The variables the script leaves as the shell has them: upstream's
-- @ignoreVars@ (a member of @Common@ in develop.cc), the build's home,
-- terminal, log descriptor and temporary directories among them.
ignoreVars :: Set Text
ignoreVars =
  Set.fromList
    [ "BASHOPTS",
      "HOME",
      "NIX_BUILD_TOP",
      "NIX_ENFORCE_PURITY",
      "NIX_LOG_FD",
      "NIX_REMOTE",
      "PPID",
      "SHELLOPTS",
      "SSL_CERT_FILE",
      "TEMP",
      "TEMPDIR",
      "TERM",
      "TMP",
      "TMPDIR",
      "TZ",
      "UID"
    ]

-- | Upstream's @escapeShellArgAlways@ (@src\/libutil\/util.cc@):
-- the bytes in single quotes, each single quote among them closed, escaped
-- and reopened.  A POSIX shell reads the word back as the same bytes when
-- they hold no NUL.
escapeShellArgAlways :: ByteString -> ByteString
escapeShellArgAlways s = BL.toStrict (BB.toLazyByteString (quotedWord s))

-- | 'escapeShellArgAlways' as a builder.
quotedWord :: ByteString -> BB.Builder
quotedWord s = "'" <> mconcat (intersperse "'\\''" (map BB.byteString (BS.split singleQuote s))) <> "'"
  where
    singleQuote = 0x27

-- | Where the script puts what it moves out of the build, each the bytes
-- of an absolute path.
data ScriptDirectories = ScriptDirectories
  { -- | The directory the shell runs in.  Each output's path becomes
    -- @outputs\/NAME@ under it, upstream's default @outputsDir@
    -- (@Common::makeRcScript@ in develop.cc).
    workingDirectory :: !ByteString,
    -- | A directory the caller made for the structured attributes' files.
    temporaryDirectory :: !ByteString
  }
  deriving (Eq, Show)

-- | The script @print-dev-env@ prints, and the files it names.
data EnvironmentScript = EnvironmentScript
  { -- | The script.
    scriptText :: !ByteString,
    -- | The structured attributes' files, @.attrs.sh@ then @.attrs.json@,
    -- in the temporary directory, which the caller writes before a shell
    -- reads the script; none without structured attributes.
    scriptFiles :: ![ScriptFile]
  }
  deriving (Eq, Show)

-- | A file a script names, with what the caller writes to it.
data ScriptFile = ScriptFile
  { -- | The bytes of the file's path.
    scriptFilePath :: !ByteString,
    -- | What the file holds.
    scriptFileContents :: !ByteString
  }
  deriving (Eq, Show)

-- | The variables a script saves before setting the environment's and
-- appends again after (@savedVars@ in @Common::makeRcScript@, develop.cc):
-- enough of the user's own for the shell to stay usable.
savedVariables :: [BB.Builder]
savedVariables = ["PATH", "XDG_DATA_DIRS"]

-- | The variables a script points at the fresh directory it makes in
-- place of the build's (@Common::makeRcScript@ in develop.cc),
-- @NIX_BUILD_TOP@ first.
temporaryDirectoryVariables :: [BB.Builder]
temporaryDirectoryVariables = ["TMP", "TMPDIR", "TEMP", "TEMPDIR"]

-- | Upstream's @makeRcScript@ (develop.cc), as @develop@ and
-- @print-dev-env@ both run it, with the outputs moved under the working
-- directory:
--
-- * @shellHook@ unset, and @PATH@ and @XDG_DATA_DIRS@ saved;
-- * each variable not in 'ignoreVars', in name order: a string quoted by
--   'escapeShellArgAlways' and exported when it was, an array declared
--   with each element, or each key and value, quoted and followed by a
--   space;
-- * each function, in name order;
-- * the saved values appended to the environment's;
-- * @NIX_BUILD_TOP@ and the temporary-directory variables set to a fresh
--   @mktemp -d -t nix-shell.XXXXXX@ directory;
-- * @shellHook@ evaluated.
--
-- Then each output's captured path is replaced by its directory under the
-- working directory, and under structured attributes each file
-- variable's value by its file in the temporary directory.  Without
-- structured attributes the outputs are the words of an @outputs@ string
-- (or an array's elements, or an associative array's keys), each with a
-- string variable holding its path; with them, @outputs@ maps each name
-- to its path.  A path two outputs share goes to the first's directory.
-- Each replacement runs over the whole script, in the byte order of the
-- paths, and after replacing an occurrence the search resumes where the
-- replacement starts, as upstream's @rewriteStrings@ does
-- (@src\/libutil\/util.cc@), so a match the replacement completes with the
-- text after it is replaced too.
--
-- 'Left' in upstream's words for an environment with no @outputs@, an
-- output variable that is not a string, and structured attributes whose
-- @outputs@ is not an associative array.  Upstream asserts that each
-- output and file variable is set, and its replacement never ends when the
-- text replacing a path holds it; each is a 'Left' here too.
environmentScript :: ScriptDirectories -> BuildEnvironment -> Either Text EnvironmentScript
environmentScript directories env = do
  outputs <- maybe (Left "derivation does not have an 'outputs' attribute") Right (Map.lookup outputsVariable variables)
  outputRewrites <- case beAttrsFiles env of
    Just _ -> do
      paths <- associativeValue outputs
      pure [(TE.encodeUtf8 path, outputDirectory name) | (name, path) <- Map.toAscList paths]
    Nothing -> traverse outputRewrite (outputNames outputs)
  files <- maybe (Right []) attrsFileRewrites (beAttrsFiles env)
  -- Upstream inserts each rewrite into a map, which keeps the first for a
  -- path given twice.
  let rewrites = Map.fromListWith (\_ earlier -> earlier) (outputRewrites <> map snd files)
  text <- rewriteStrings rewrites (BL.toStrict (BB.toLazyByteString (scriptBody env)))
  pure (EnvironmentScript text (map fst files))
  where
    variables = beVariables env
    outputDirectory name = appendPath (appendPath (workingDirectory directories) outputsDirectoryName) (TE.encodeUtf8 name)
    outputRewrite name = do
      path <- setVariable name >>= stringValue
      pure (TE.encodeUtf8 path, outputDirectory name)
    attrsFileRewrites files =
      sequence
        [ attrsFile attrsShFileVariable attrsShMember (attrsSh files),
          attrsFile attrsJsonFileVariable attrsJsonMember (attrsJson files)
        ]
    attrsFile variable fileName contents = do
      path <- setVariable variable >>= stringValue
      let file = appendPath (temporaryDirectory directories) (TE.encodeUtf8 fileName)
      pure (ScriptFile file (TE.encodeUtf8 contents), (TE.encodeUtf8 path, file))
    setVariable name = maybe (Left ("the build environment has no variable " <> quote name)) Right (Map.lookup name variables)

-- | The variable naming a derivation's outputs.
outputsVariable :: Text
outputsVariable = "outputs"

-- | The directory under the working directory that holds the outputs.
outputsDirectoryName :: ByteString
outputsDirectoryName = "outputs"

-- | The variables naming the structured attributes' files in the build.
attrsShFileVariable, attrsJsonFileVariable :: Text
attrsShFileVariable = "NIX_ATTRS_SH_FILE"
attrsJsonFileVariable = "NIX_ATTRS_JSON_FILE"

-- | Upstream's @getString@: the value of a string variable, exported or
-- not.
stringValue :: Variable -> Either Text Text
stringValue variable = case variable of
  ShellString s -> Right s
  ExportedString s -> Right s
  _ -> Left "bash variable is not a string"

-- | Upstream's @getAssociative@.
associativeValue :: Variable -> Either Text (Map Text Text)
associativeValue variable = case variable of
  AssociativeArray entries -> Right entries
  _ -> Left "bash variable is not an associative array"

-- | Upstream's @getStrings@: a string's words, split on space, tab,
-- newline and carriage return, an array's elements, or an associative
-- array's keys.
outputNames :: Variable -> [Text]
outputNames variable = case variable of
  ShellString s -> wordsOf s
  ExportedString s -> wordsOf s
  IndexedArray items -> items
  AssociativeArray entries -> Map.keys entries
  where
    wordsOf = filter (not . T.null) . T.split (`elem` [' ', '\t', '\n', '\r'])

-- | @std::filesystem::path@'s @/@ on POSIX: a name with a root replaces the
-- directory, and otherwise follows it after a separator unless the
-- directory is empty or ends in one.
appendPath :: ByteString -> ByteString -> ByteString
appendPath directory name
  | separator `BS.isPrefixOf` name = name
  | BS.null directory || separator `BS.isSuffixOf` directory = directory <> name
  | otherwise = directory <> separator <> name
  where
    separator = "/"

-- | The script before its paths are replaced.
scriptBody :: BuildEnvironment -> BB.Builder
scriptBody env =
  "unset shellHook\n"
    <> foldMap saveVariable savedVariables
    <> Map.foldMapWithKey variableLines (Map.withoutKeys (beVariables env) ignoreVars)
    <> Map.foldMapWithKey functionLines (beFunctions env)
    <> foldMap restoreVariable savedVariables
    <> "export NIX_BUILD_TOP=\"$(mktemp -d -t nix-shell.XXXXXX)\"\n"
    <> foldMap (\name -> "export " <> name <> "=\"$NIX_BUILD_TOP\"\n") temporaryDirectoryVariables
    <> "eval \"${shellHook:-}\"\n"
  where
    saveVariable name = name <> "=${" <> name <> ":-}\n" <> "nix_saved_" <> name <> "=\"$" <> name <> "\"\n"
    restoreVariable name = name <> "=\"$" <> name <> "${nix_saved_" <> name <> ":+:$nix_saved_" <> name <> "}\"\n"

-- | Upstream's @toBash@ for one variable (@BuildEnvironment::toBash@ in
-- develop.cc).
variableLines :: Text -> Variable -> BB.Builder
variableLines name variable = case variable of
  ShellString s -> assignment s
  ExportedString s -> assignment s <> "export " <> nameBytes <> "\n"
  IndexedArray items -> "declare -a " <> nameBytes <> "=(" <> foldMap (\item -> quoted item <> " ") items <> ")\n"
  AssociativeArray entries ->
    "declare -A " <> nameBytes <> "=(" <> Map.foldMapWithKey (\key value -> "[" <> quoted key <> "]=" <> quoted value <> " ") entries <> ")\n"
  where
    nameBytes = TE.encodeUtf8Builder name
    assignment s = nameBytes <> "=" <> quoted s <> "\n"
    quoted = quotedWord . TE.encodeUtf8

-- | Upstream's @toBash@ for one function (@BuildEnvironment::toBash@ in
-- develop.cc).
functionLines :: Text -> Text -> BB.Builder
functionLines name body = TE.encodeUtf8Builder name <> " ()\n{\n" <> TE.encodeUtf8Builder body <> "}\n"

-- | Upstream's @rewriteStrings@ (util.cc): each rewrite in the byte
-- order of the text it replaces, replacing every occurrence, the search
-- resuming where each replacement starts.  When a replacement holds the
-- text it replaces, and the script holds that text, upstream's loop never
-- ends; that is a 'Left' here.
rewriteStrings :: Map ByteString ByteString -> ByteString -> Either Text ByteString
rewriteStrings rewrites script = foldM rewrite script (Map.toAscList rewrites)
  where
    rewrite text (from, to)
      | from == to = Right text
      | from `BS.isInfixOf` to && from `BS.isInfixOf` text =
          Left ("replacing '" <> shown from <> "' with '" <> shown to <> "', which holds it, would not end")
      | otherwise = Right (replaceAll from to text)
    -- For an error message only.
    shown = TE.decodeUtf8Lenient

-- | Every occurrence of @from@ replaced by @to@, the search resuming at the
-- start of each replacement.  @to@ does not hold @from@, so the loop ends:
-- a match starting in a replacement runs past it into text not yet
-- searched, or, when @to@ is the shorter, the text left shrinks.
replaceAll :: ByteString -> ByteString -> ByteString -> ByteString
replaceAll from to = go []
  where
    go done rest = case BS.breakSubstring from rest of
      (before, after)
        | BS.null after -> BS.concat (reverse (before : done))
        | otherwise -> go (before : done) (to <> BS.drop (BS.length from) after)

-- ---------------------------------------------------------------------------
-- The rcfile
-- ---------------------------------------------------------------------------

-- | The shell an rcfile is written for.
data Shell = Shell
  { -- | Where the rcfile is: it removes itself once it has set the
    -- environment.
    shellRcfile :: !ByteString,
    -- | What the shell does once the environment is set.
    shellMode :: !ShellMode,
    -- | What @SHELL@ is set to: the shell the caller starts.
    shellProgram :: !ByteString
  }
  deriving (Eq, Show)

-- | What the shell does once the environment is set.
data ShellMode
  = -- | Reads commands, with this prompt.
    Interactive !Prompt
  | -- | Replaces itself with this command and its arguments: @--command@.
    RunCommand !(NonEmpty ByteString)
  | -- | Runs the phase of this name, such as @build@: @--phase@.
    RunPhase !ByteString
  deriving (Eq, Show)

-- | The @bash-prompt@ settings (@DevelopSettings@ in develop.cc), each empty
-- when unset, as upstream defaults them.  An empty one writes nothing.
data Prompt = Prompt
  { -- | @bash-prompt@: the prompt in place of the user's.
    bashPrompt :: !ByteString,
    -- | @bash-prompt-prefix@: text before the prompt.
    bashPromptPrefix :: !ByteString,
    -- | @bash-prompt-suffix@: text after it.
    bashPromptSuffix :: !ByteString
  }
  deriving (Eq, Show)

-- | The rcfile @develop@ starts bash with, as upstream's @CmdDevelop::run@
-- writes it (develop.cc): the environment script, then a line
-- removing the rcfile, then by mode
--
-- * 'RunPhase': @foundMakefile=1@ and the phase run through @runPhase@
--   when the environment defines it, otherwise through @runHook@;
-- * 'RunCommand': @exec@ and each word quoted by 'escapeShellArgAlways';
-- * 'Interactive': before the script, a line sourcing @~\/.bashrc@ when
--   @PS1@ is set and one turning aliases off; after the removal, an empty
--   line, one turning aliases back on, and a line for each prompt setting
--   that is not empty;
--
-- and last @SHELL@ set to the shell.  The rcfile path, phase and shell go
-- in as they are, unescaped (the path between single quotes, the shell
-- between double quotes, the phase bare), as upstream writes them.
--
-- Upstream also writes @set -x@ at debug verbosity, and after @SHELL@ a
-- @PATH@ line naming @nixpkgs#bashInteractive@'s directory when it finds
-- that package.  nova-nix has neither the verbosity nor the lookup, so
-- neither line is written.
renderRcfile :: Shell -> EnvironmentScript -> ByteString
renderRcfile shell script = BL.toStrict . BB.toLazyByteString $ case shellMode shell of
  RunPhase phase ->
    body
      <> "foundMakefile=1\n"
      <> "if declare -f runPhase >/dev/null; then\n"
      <> ("  runPhase " <> BB.byteString phase <> "Phase\n")
      <> "else\n"
      <> ("  runHook " <> BB.byteString phase <> "Phase\n")
      <> "fi\n"
      <> shellLine
  RunCommand command ->
    body <> "exec " <> mconcat (intersperse " " (map quotedWord (toList command))) <> "\n" <> shellLine
  Interactive prompt ->
    "[ -n \"$PS1\" ] && [ -e ~/.bashrc ] && source ~/.bashrc;\nshopt -u expand_aliases\n"
      <> body
      <> "\nshopt -s expand_aliases\n"
      <> promptLine (bashPrompt prompt) (\p -> "PS1=" <> p <> ";\n")
      <> promptLine (bashPromptPrefix prompt) (\p -> "PS1=" <> p <> "\"$PS1\";\n")
      <> promptLine (bashPromptSuffix prompt) (\p -> "PS1+=" <> p <> ";\n")
      <> shellLine
  where
    body = BB.byteString (scriptText script) <> "command rm -f '" <> BB.byteString (shellRcfile shell) <> "'\n"
    shellLine = "SHELL=\"" <> BB.byteString (shellProgram shell) <> "\"\n"
    promptLine setting assignment
      | BS.null setting = mempty
      | otherwise = "[ -n \"$PS1\" ] && " <> assignment (quotedWord setting)
