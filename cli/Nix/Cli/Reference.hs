-- | The command line's reference: every command and flag, the nix.conf
-- settings the CLI reads, and the two renderings of them.
--
-- This module is the one place the CLI is documented.  @--help@ prints
-- 'helpLines', and @docs\/cli.json@ holds 'referenceJson', which the
-- project's site renders as its command-line reference; the test suite
-- fails while the committed file differs from it.  "Nix.Cli.Args" is the
-- grammar these entries describe, and the suite checks each flag against
-- it: every command an entry lists accepts the flag, and the parser asks
-- for a value exactly when the entry names an argument.
--
-- The commands and flags are enumerations, and their entries are total
-- functions of them, so a command or flag without documentation fails the
-- warning-clean build.  An example that shows output shows what the test
-- suite gets running it.  A summary or description marks code between
-- backticks, as Markdown does: @--help@ prints it without them, and the
-- site sets it as code.
module Nix.Cli.Reference
  ( -- * Commands
    CommandName (..),
    CommandEntry (..),
    Form (..),
    Example (..),
    codeMark,
    programName,
    commandNames,
    commandWords,
    commandInvocation,
    globalForm,
    command,
    exampleCommandLine,
    shellQuote,

    -- * Flags
    FlagName (..),
    FlagEntry (..),
    FlagScope (..),
    flagNames,
    flag,
    flagSpellings,
    globalFlags,

    -- * Configuration
    Configuration (..),
    Setting (..),
    configuration,

    -- * Hosts
    Host (..),
    currentHost,
    SystemConfDirSource (..),
    systemConfDirSource,
    systemConfDirName,

    -- * Renderings
    helpLines,
    helpWidth,
    wrapWords,
    referenceJson,
    renderIndentedJson,
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Foldable (toList)
import Data.List (intersperse, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Nix.Config as Config
import Nix.Eval.CallDepth (defaultMaxCallDepth)
import Nix.HostPath (hostPathText)
import Nix.Json (Json (..), renderJson)
import Nix.Push (pushCompressionValues)
import qualified System.FilePath.Posix as Posix
import qualified System.FilePath.Windows as Windows
import qualified System.Info as SI

-- ---------------------------------------------------------------------------
-- Commands
-- ---------------------------------------------------------------------------

-- | The commands, in the order the reference lists them.  The global
-- form, @nova-nix@ with no command, is 'globalForm'.
data CommandName
  = EvalCommand
  | BuildCommand
  | PushCommand
  | StoreDeleteCommand
  | StoreGcCommand
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | What the reference says about a command, or about the global form.
data CommandEntry = CommandEntry
  { -- | The ways to invoke it, each with what it does in a line.
    commandForms :: !(NonEmpty Form),
    -- | What it does, in a paragraph.
    commandDescription :: !Text,
    -- | Command lines that use it, each with what it is for.
    commandExamples :: !(NonEmpty Example)
  }
  deriving (Eq, Show)

-- | One way to invoke a command.
data Form = Form
  { -- | What follows the command's words: its operands and any flag it
    -- cannot do without.  Empty for a command that takes neither.
    formArguments :: !Text,
    -- | What the form does, in a line.
    formSummary :: !Text
  }
  deriving (Eq, Show)

-- | A command line and what it is for.
data Example = Example
  { -- | What the command line is for, in a sentence.
    exampleDescription :: !Text,
    -- | The arguments after the program's name.
    exampleArguments :: !(NonEmpty Text),
    -- | What it prints on stdout, line by line.  Empty for an example
    -- whose output depends on the machine or the release, or that needs
    -- what the test suite does not have; the suite runs every example
    -- that shows output and compares.
    exampleOutput :: ![Text]
  }
  deriving (Eq, Show)

-- | What opens and closes a code span in the reference's text, as in
-- Markdown.
codeMark :: Char
codeMark = '`'

-- | Text set as code.
codeSpan :: Text -> Text
codeSpan text = T.singleton codeMark <> text <> T.singleton codeMark

-- | The program's name, as every invocation starts.
programName :: Text
programName = "nova-nix"

-- | Every command, in order.
commandNames :: [CommandName]
commandNames = [minBound .. maxBound]

-- | The words that select a command after the program's name.
commandWords :: CommandName -> [Text]
commandWords name = case name of
  EvalCommand -> ["eval"]
  BuildCommand -> ["build"]
  PushCommand -> ["push"]
  StoreDeleteCommand -> ["store", "delete"]
  StoreGcCommand -> ["store", "gc"]

-- | A command as it is invoked, @nova-nix store gc@.
commandInvocation :: CommandName -> Text
commandInvocation = T.unwords . (programName :) . commandWords

-- | A form as it is typed after the given words.
formUsage :: [Text] -> Form -> Text
formUsage commandPart form = T.unwords (commandPart ++ filter (not . T.null) [formArguments form])

-- | An example as it is typed at a POSIX shell.
exampleCommandLine :: Example -> Text
exampleCommandLine = T.unwords . (programName :) . map shellQuote . toList . exampleArguments

-- | An argument as a POSIX shell reads it back unchanged: as it is when
-- every character is one no shell treats specially, otherwise in single
-- quotes, each single quote in it closing the quotes, escaped, and opening
-- them again.  A leading @=@ is quoted too, since zsh expands @=name@ to
-- the command's path.
shellQuote :: Text -> Text
shellQuote argument
  | not (T.null argument) && T.all plain argument && not ("=" `T.isPrefixOf` argument) = argument
  | otherwise = "'" <> T.replace "'" "'\\''" argument <> "'"
  where
    plain c = isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` ("_@%+=:,./-" :: String)

-- | @nova-nix@ with no command.
globalForm :: CommandEntry
globalForm =
  CommandEntry
    { commandForms = Form "COMMAND [FLAG...]" "Run one command, with the flags it takes" :| [],
      commandDescription =
        "nova-nix evaluates Nix expressions, builds derivations, pushes store paths to a binary cache and maintains its store, one command per invocation, and reads its settings from `nix.conf` as upstream Nix does. Each command takes the flags listed with it, after the command's name. A command exits with status 0 when it succeeds, and with status 1 when it fails, after saying why on stderr; `build --check` alone has another status, 104, for an output that differs.",
      commandExamples =
        Example "Print the release this binary was built from." ("--version" :| []) []
          :| [Example "Print the commands, flags and settings." ("--help" :| []) []]
    }

-- | What the reference says about each command.
command :: CommandName -> CommandEntry
command name = case name of
  EvalCommand ->
    CommandEntry
      { commandForms =
          Form "FILE.nix" "Evaluate a .nix file, print result"
            :| [Form "--expr 'EXPR'" "Evaluate an inline expression"],
        commandDescription =
          "Evaluates the expression in `FILE.nix`, or the one `--expr` gives, and prints its value as `nix-instantiate --eval` prints it. A part of the value the evaluation never needed prints as `<CODE>`; with `--strict` the whole value is evaluated first. A relative path resolves against the file's directory, or for `--expr` against the working directory. A `<name>` path is looked up in the entries `--nix-path` adds, then `NIX_PATH`'s, then the `<nix/*>` expressions nova-nix ships with.",
        commandExamples =
          Example "Evaluate an expression." ("eval" :| ["--expr", "1 + 2"]) ["3"]
            :| [ Example
                   "Print an attribute set as far as evaluating it forced it."
                   ("eval" :| ["--expr", "{ a = 1 + 1; }"])
                   ["{ a = <CODE>; }"],
                 Example
                   "Evaluate the whole value before printing it."
                   ("eval" :| ["--strict", "--expr", "{ a = 1 + 1; }"])
                   ["{ a = 2; }"],
                 Example
                   "Print the `.drv` a derivation is written to the store as, to compare it with another evaluator's."
                   ("eval" :| ["--aterm", "--expr", "derivation { name = \"hello\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; }"])
                   ["Derive([(\"out\",\"/nix/store/pnwh4xsfs4j508bs9iw6bpkyc4zw6ryx-hello\",\"\",\"\")],[],[],\"x86_64-linux\",\"/bin/sh\",[],[(\"builder\",\"/bin/sh\"),(\"name\",\"hello\"),(\"out\",\"/nix/store/pnwh4xsfs4j508bs9iw6bpkyc4zw6ryx-hello\"),(\"system\",\"x86_64-linux\")])"],
                 Example
                   "Evaluate an expression from a nixpkgs checkout in the working directory."
                   ("eval" :| ["--nix-path", "nixpkgs=./nixpkgs", "--expr", "(import <nixpkgs/lib>).version"])
                   []
               ]
      }
  BuildCommand ->
    CommandEntry
      { commandForms =
          Form "FILE.nix" "Build a derivation from a .nix file"
            :| [Form "--expr 'EXPR'" "Build a derivation from an inline expression"],
        commandDescription =
          "Evaluates `FILE.nix`, or the expression `--expr` gives, selects the attribute `-A` names when it is given, builds the derivation that results and every derivation it depends on, and prints the path of the output its `outputName` names, the first of its outputs unless the expression selects another, as `nix-build` does. A derivation whose outputs a substituter holds is downloaded instead of built, from the substituters `nix.conf` configures and the one `--substituter` adds, and only a path signed by a trusted key is accepted. The outputs of a derivation whose `allowSubstitutes` attribute is false are built instead, unless `always-allow-substitutes` is on. A substituter that fails stops the build unless `fallback` is on. With `--check` the derivation is checked as `nix-build --check` checks one: an output of its that is not valid is first downloaded from a substituter that holds it, the derivations it depends on are built or downloaded as ever, and then it is built again beside its outputs, never downloaded, and each output is compared with the one in the store. The rebuild changes nothing in the store and what it wrote is discarded; the downloads and builds before it add to the store as in any build. An output that comes out different fails the command with status 104, so a path a substituter supplied can be held to a build. With `--out-link` the output is linked from `PATH` and kept from garbage collection; without it nothing is linked, where `nix-build` links `./result`.",
        commandExamples =
          Example
            "Build GNU Hello for Windows from the directory a release unpacks to, with the public cache as a substituter."
            ("build" :| ["pkgs/windows/hello.nix", "--substituter", "https://cache.novavero.ai", "--trusted-key", "cache.novavero.ai-1:9gQ7tLWMM+2tdC9H5sKMJltDIPfD7X2GWlZe8Aa8hHQ="])
            []
            :| [ Example
                   "Build an attribute of a file and link the output from `./result`, a root the garbage collector keeps."
                   ("build" :| ["pkgs/windows/default.nix", "-A", "hello", "-o", "result"])
                   [],
                 Example
                   "Build a derivation written inline, on Linux or macOS."
                   ("build" :| ["--expr", "derivation { name = \"greeting\"; system = builtins.currentSystem; builder = \"/bin/sh\"; args = [ \"-c\" \"echo hello > $out\" ]; }"])
                   [],
                 Example
                   "Check GNU Hello with the public cache as a substituter: Hello and what it is built from are downloaded where the cache holds them and built where it does not, then Hello is built again, and the command fails if its output comes out different."
                   ("build" :| ["pkgs/windows/hello.nix", "--check", "--substituter", "https://cache.novavero.ai", "--trusted-key", "cache.novavero.ai-1:9gQ7tLWMM+2tdC9H5sKMJltDIPfD7X2GWlZe8Aa8hHQ="])
                   []
               ]
      }
  PushCommand ->
    CommandEntry
      { commandForms =
          Form "--cache URL PATH..." "Push store paths (and their closures) to a binary cache"
            :| [Form "--cache URL --all" "Push every valid path except derivations, and their closures"],
        commandDescription =
          "Uploads the closure of each `PATH`, or with `--all` every valid path except derivations, to the nova-cache server at `URL`, which signs what it receives. A `PATH` is a store path, in full or as its base name; a derivation is refused, since a binary cache serves outputs. Paths the cache already holds are skipped, every NAR is uploaded before any narinfo announces it, and a narinfo is read back to check that the server signed it. It prints how many paths it pushed and how many the cache already held.",
        commandExamples =
          Example
            "Push every path in the store except derivations, compressed with zstd, to a cache that takes the API key in `cache-api-key`."
            ("push" :| ["--cache", "https://cache.example.org", "--key-file", "cache-api-key", "--compression", "zstd", "--all"])
            []
            :| []
      }
  StoreDeleteCommand ->
    CommandEntry
      { commandForms = Form "PATH..." "Remove store paths, refused while a root or another valid path keeps them" :| [],
        commandDescription =
          "Deletes each `PATH`'s registration and its tree on disk, in the order given, and prints each one it deleted. A `PATH` is a store path, in full or as its base name, and an entry with only a registration or only a tree is deleted too. A path a root keeps, or that another valid path references, is refused, and the first refusal stops the command, so a path is named before the paths it references.",
        commandExamples =
          Example
            "Delete a build's output, here GNU Hello's, so that the next build makes it again; `HASH` stands for the hash in its path."
            ("store" :| ["delete", "/nix/store/HASH-hello"])
            []
            :| []
      }
  StoreGcCommand ->
    CommandEntry
      { commandForms = Form "" "Remove every store path not reachable from a root" :| [],
        commandDescription =
          "Deletes every store path no root keeps, directly or through the paths that reference it, and prints how many it deleted and the space it freed, as `nix-store --gc` does. The roots are the out-links `build --out-link` registered and the links and files under the store's `.nova-nix/gcroots` directory; a path a running program has open is not a root, and a rooted output does not keep the `.drv` that built it. With `--print-roots` it lists the roots and deletes nothing.",
        commandExamples =
          Example "Delete everything no root keeps." ("store" :| ["gc"]) []
            :| [Example "List the roots, each as the link that names it and the path it keeps." ("store" :| ["gc", "--print-roots"]) []]
      }

-- ---------------------------------------------------------------------------
-- Flags
-- ---------------------------------------------------------------------------

-- | The flags, in the order the JSON and each command's list give them;
-- @--help@ groups them by the commands they apply to.
data FlagName
  = ExprFlag
  | AttrFlag
  | OutLinkFlag
  | StrictFlag
  | AtermFlag
  | NixPathFlag
  | RestrictEvalFlag
  | PureEvalFlag
  | TraceVerboseFlag
  | SubstituterFlag
  | TrustedKeyFlag
  | FallbackFlag
  | CheckFlag
  | ExecWrapperFlag
  | CacheFlag
  | AllFlag
  | KeyFileFlag
  | CompressionFlag
  | PrintRootsFlag
  | StoreFlag
  | HelpFlag
  | VersionFlag
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | What the reference says about a flag.
data FlagEntry = FlagEntry
  { -- | The long spelling, without its leading @--@.
    flagLong :: !Text,
    -- | The one-letter spelling, without its leading @-@, where there is
    -- one.
    flagShort :: !(Maybe Char),
    -- | The name of the value it takes, where it takes one.
    flagArgument :: !(Maybe Text),
    -- | What it does, in a line.
    flagDescription :: !Text,
    -- | Where it applies.
    flagScope :: !FlagScope
  }
  deriving (Eq, Show)

-- | Where a flag applies.
data FlagScope
  = -- | To the global form, which runs no command.
    GlobalFlag
  | -- | To each of these commands.
    CommandFlag !(NonEmpty CommandName)
  deriving (Eq, Show)

-- | Every flag, in order.
flagNames :: [FlagName]
flagNames = [minBound .. maxBound]

-- | A flag's spellings: the one-letter one first, where there is one.
flagSpellings :: FlagEntry -> NonEmpty Text
flagSpellings entry = case flagShort entry of
  Just letter -> T.pack ['-', letter] :| [longSpelling entry]
  Nothing -> longSpelling entry :| []

-- | A flag's long spelling, which names it.
longSpelling :: FlagEntry -> Text
longSpelling entry = "--" <> flagLong entry

-- | The flags of the global form, in order.
globalFlags :: [FlagName]
globalFlags = filter ((== GlobalFlag) . flagScope . flag) flagNames

-- | What the reference says about each flag.
flag :: FlagName -> FlagEntry
flag name = case name of
  ExprFlag ->
    long "expr" (Just "EXPR") "Evaluate `EXPR` in place of a file; a relative path in it resolves against the working directory" evalAndBuild
  AttrFlag ->
    short 'A' "attr" (Just "ATTRPATH") "Select a dotted attribute path (`a.b.c`)" buildOnly
  OutLinkFlag ->
    short 'o' "out-link" (Just "PATH") "Create `PATH` as a symlink to the result and register it as a GC root" buildOnly
  StrictFlag ->
    long "strict" Nothing "Deep-force all thunks before printing (warning: OOM on large results)" evalOnly
  AtermFlag ->
    long "aterm" Nothing "With `--expr`, print the derivation's `.drv` ATerm" evalOnly
  NixPathFlag ->
    long "nix-path" (Just "NAME=PATH") "Add search path (repeatable, merged with `NIX_PATH`)" evalAndBuild
  RestrictEvalFlag ->
    long "restrict-eval" Nothing "Confine reads to the search path roots and what the evaluation itself writes; fetch only `allowed-uris`" evalAndBuild
  PureEvalFlag ->
    long "pure-eval" Nothing "Confine reads as `--restrict-eval` does, with no search path but the bundled `<nix/*>`, no `currentTime`, `currentSystem` or `storePath`, and pinned fetches only (`allowed-uris` applies only under `--restrict-eval`)" evalAndBuild
  TraceVerboseFlag ->
    long "trace-verbose" Nothing "Make `builtins.traceVerbose` behave as `builtins.trace`; without it, the first argument is never forced" evalAndBuild
  SubstituterFlag ->
    long "substituter" (Just "URL") "Try this binary cache before building" buildOnly
  TrustedKeyFlag ->
    long "trusted-key" (Just "KEY") "Trust signatures by this public key (`NAME:BASE64`) from any substituter" buildOnly
  FallbackFlag ->
    long "fallback" Nothing "Build from source if substitution fails" buildOnly
  CheckFlag ->
    long "check" Nothing "Build the derivation again beside its valid outputs, and fail with status 104 if an output comes out different" buildOnly
  ExecWrapperFlag ->
    long "exec-wrapper" (Just "S=PATH") "Run system `S`'s derivations through `PATH` (repeatable), e.g. `x86_64-windows=/usr/bin/wine`" buildOnly
  CacheFlag ->
    long "cache" (Just "URL") "The nova-cache server to push to (required)" pushOnly
  AllFlag ->
    long "all" Nothing "Select every valid path except derivations, in place of `PATH...`" pushOnly
  KeyFileFlag ->
    long "key-file" (Just "PATH") "File holding the cache API key" pushOnly
  CompressionFlag ->
    long "compression" (Just "KIND") ("Artifact packaging (" <> pushCompressionValues <> "; default none)") pushOnly
  PrintRootsFlag ->
    long "print-roots" Nothing "List the roots as `LINK -> PATH` and delete nothing" (CommandFlag (StoreGcCommand :| []))
  StoreFlag ->
    long "store" (Just "DIR") "Use `DIR` as the store (default: the platform store)" (CommandFlag (EvalCommand :| [BuildCommand, PushCommand, StoreDeleteCommand, StoreGcCommand]))
  HelpFlag ->
    long "help" Nothing "Print the commands, flags and settings, and exit" GlobalFlag
  VersionFlag ->
    long "version" Nothing "Print the version and exit" GlobalFlag
  where
    long longName = FlagEntry longName Nothing
    short letter longName = FlagEntry longName (Just letter)
    evalOnly = CommandFlag (EvalCommand :| [])
    buildOnly = CommandFlag (BuildCommand :| [])
    pushOnly = CommandFlag (PushCommand :| [])
    evalAndBuild = CommandFlag (EvalCommand :| [BuildCommand])

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | The nix.conf settings the CLI reads, and where it reads them.
data Configuration = Configuration
  { configSettings :: !(NonEmpty Setting),
    -- | Where the settings come from, in a paragraph.
    configDescription :: !Text
  }
  deriving (Eq, Show)

-- | A nix.conf setting.
data Setting = Setting
  { -- | Its name in @nix.conf@.
    settingName :: !Text,
    -- | What it does, in a line.
    settingDescription :: !Text,
    -- | The flag that adds to it on one invocation, where there is one.
    settingFlag :: !(Maybe FlagName)
  }
  deriving (Eq, Show)

-- | The settings, and where they are read.
configuration :: Configuration
configuration =
  Configuration
    { configSettings =
        Setting "substituters" "Binary caches a build substitutes from" (Just SubstituterFlag)
          :| [ Setting "trusted-public-keys" "Public keys, one of whose signatures a substituted path must carry" (Just TrustedKeyFlag),
               Setting "fallback" "Build from source when a substitute fails" (Just FallbackFlag),
               Setting "always-allow-substitutes" "Substitute a derivation whose `allowSubstitutes` attribute is false" Nothing,
               Setting "max-call-depth" ("How many function calls may be active around a new one before evaluation refuses it (default " <> T.pack (show defaultMaxCallDepth) <> ")") Nothing,
               Setting "restrict-eval" "Turn on restricted evaluation" (Just RestrictEvalFlag),
               Setting "pure-eval" "Turn on pure evaluation" (Just PureEvalFlag),
               Setting "allowed-uris" "URI prefixes a fetch may reach under `restrict-eval`" Nothing,
               Setting "trace-verbose" "Make `builtins.traceVerbose` trace" (Just TraceVerboseFlag)
             ],
      configDescription =
        "`nix.conf` is read in upstream's order: `$NIX_CONF_DIR/nix.conf`, where `NIX_CONF_DIR` defaults to "
          <> codeSpan (systemConfDirName PosixHost)
          <> ", or "
          <> codeSpan (systemConfDirName WindowsHost)
          <> " on Windows; the XDG `nix/nix.conf` files or `$NIX_USER_CONF_FILES`; then `$NIX_CONFIG`. A later source replaces an earlier one's value, an `extra-` prefix on a list setting adds to it instead, and a flag adds to whatever they configure."
    }

-- ---------------------------------------------------------------------------
-- Hosts
-- ---------------------------------------------------------------------------

-- | The two families of host whose conventions differ in what the CLI
-- reads.
data Host = PosixHost | WindowsHost
  deriving (Eq, Show)

-- | The host this binary was built for.
currentHost :: Host
currentHost = case SI.os of
  "mingw32" -> WindowsHost
  _ -> PosixHost

-- | Where a host's machine-wide config directory comes from, with @nix@
-- beneath it either way: a fixed path (upstream's @sysconfdir@ on Unix) or
-- an environment variable naming it (@%ProgramData%@ on Windows).  One
-- value serves both the lookup and the reference's text, so the two
-- cannot disagree.
data SystemConfDirSource = FixedConfDir !FilePath | EnvConfDir !BS.ByteString
  deriving (Eq, Show)

-- | A host's machine-wide config directory.
systemConfDirSource :: Host -> SystemConfDirSource
systemConfDirSource host = case host of
  PosixHost -> FixedConfDir posixSysconfDir
  WindowsHost -> EnvConfDir programDataVar

-- | Upstream's @sysconfdir@ on Unix, which libstore's meson.build forces
-- absolute, so @\/etc\/nix@ is the compiled-in system directory.
posixSysconfDir :: FilePath
posixSysconfDir = "/etc"

-- | Windows' all-users application data directory, the machine-wide
-- config directory there.
programDataVar :: BS.ByteString
programDataVar = "ProgramData"

-- | The system config directory as the reference names it: the path on
-- POSIX, the variable on Windows, each joined with that host's separator
-- whichever host renders it.
systemConfDirName :: Host -> Text
systemConfDirName host = T.pack (base `joined` T.unpack (hostPathText Config.nixConfDirName))
  where
    base = case systemConfDirSource host of
      FixedConfDir dir -> dir
      EnvConfDir var -> "%" <> BC.unpack var <> "%"
    joined = case host of
      PosixHost -> (Posix.</>)
      WindowsHost -> (Windows.</>)

-- ---------------------------------------------------------------------------
-- Help
-- ---------------------------------------------------------------------------

-- | The width @--help@ wraps its text to.
helpWidth :: Int
helpWidth = 80

-- | How far an entry is indented.
helpIndent :: Int
helpIndent = 2

-- | The least space between an entry and its description.
helpGap :: Int
helpGap = 2

-- | What @--help@ prints: the usage line, each command's forms, the flags
-- grouped by the commands they apply to, the settings and where they are
-- read, then the global form's flags.
helpLines :: [Text]
helpLines =
  usageLines
    ++ section "Commands:" commandRows
    ++ concatMap (uncurry section) flagGroups
    ++ section "Settings read from nix.conf:" settingRows
    ++ [""]
    ++ map (indentBy helpIndent) (wrapWords (helpWidth - helpIndent) (unmarked (configDescription configuration)))
    ++ [""]
    ++ concatMap (row . flagRow) globalFlags
  where
    usagePrefix = "Usage: "
    usageLines = case fmap (formUsage [programName]) (commandForms globalForm) of
      first :| rest -> (usagePrefix <> first) : map (indentBy (T.length usagePrefix)) rest
    section header rows = "" : header : concatMap row rows
    commandRows =
      [ (formUsage (commandWords name) form, unmarked (formSummary form))
      | name <- commandNames,
        form <- toList (commandForms (command name))
      ]
    flagGroups =
      [ ("Flags for " <> describeCommands names <> ":", map flagRow members)
      | (names, members) <- sortOn groupOrder (Map.toList (Map.fromListWith (flip (++)) grouped))
      ]
    grouped = [(Set.fromList (toList names), [name]) | name <- flagNames, CommandFlag names <- [flagScope (flag name)]]
    groupOrder (names, _) = (Set.lookupMin names, Down (Set.size names))
    settingRows =
      [ (settingName setting, unmarked (settingDescription setting) <> maybe "" (\name -> " (" <> longSpelling (flag name) <> ")") (settingFlag setting))
      | setting <- toList (configSettings configuration)
      ]
    entries = map fst (commandRows ++ concatMap snd flagGroups ++ settingRows ++ map flagRow globalFlags)
    column = helpIndent + foldl' max 0 (map T.length entries) + helpGap
    row (entry, description) = case wrapWords (helpWidth - column) description of
      [] -> [indentBy helpIndent entry]
      first : rest ->
        (indentBy helpIndent (T.justifyLeft (column - helpIndent) ' ' entry) <> first)
          : map (indentBy column) rest

-- | A flag's entry in @--help@: its spellings and its argument, and its
-- description.
flagRow :: FlagName -> (Text, Text)
flagRow name =
  (T.intercalate ", " (toList (flagSpellings entry)) <> maybe "" (" " <>) (flagArgument entry), unmarked (flagDescription entry))
  where
    entry = flag name

-- | Marked text as @--help@ prints it, without its code marks.
unmarked :: Text -> Text
unmarked = T.filter (/= codeMark)

-- | Commands named in a sentence: each by its words, @every command@ for
-- all of them.
describeCommands :: Set.Set CommandName -> Text
describeCommands names
  | names == Set.fromList commandNames = "every command"
  | otherwise = joinWithAnd (map (T.unwords . commandWords) (Set.toAscList names))

-- | Items in a sentence: @a@, @a and b@, @a, b and c@.
joinWithAnd :: [Text] -> Text
joinWithAnd items = case reverse items of
  [] -> ""
  [only] -> only
  final : before -> T.intercalate ", " (reverse before) <> " and " <> final

-- | Text after @width@ spaces.
indentBy :: Int -> Text -> Text
indentBy width = (T.replicate width " " <>)

-- | Text broken into lines of at most @width@ characters, at spaces, each
-- line as full as it can be.  A word longer than the width is a line of
-- its own.
wrapWords :: Int -> Text -> [Text]
wrapWords width text = case T.words text of
  [] -> []
  first : rest -> go first rest
  where
    go line [] = [line]
    go line (word : more)
      | T.length line + 1 + T.length word <= width = go (line <> " " <> word) more
      | otherwise = line : go word more

-- ---------------------------------------------------------------------------
-- JSON
-- ---------------------------------------------------------------------------

-- | The reference as JSON, the bytes of @docs\/cli.json@: the global form,
-- the commands, the flags and the configuration, each command and flag in
-- order, and a command named as it is invoked.
referenceJson :: BS.ByteString
referenceJson =
  renderIndentedJson $
    object
      [ ("program", JsonString programName),
        ("global", commandJson programName [programName] globalForm),
        ("commands", JsonArray [commandJson (commandInvocation name) (programName : commandWords name) (command name) | name <- commandNames]),
        ("flags", JsonArray (map flagJson flagNames)),
        ("configuration", configurationJson)
      ]
  where
    commandJson invocation commandPart entry =
      object
        [ ("name", JsonString invocation),
          ("synopsis", JsonArray [object [("usage", JsonString (formUsage commandPart form)), ("summary", JsonString (formSummary form))] | form <- toList (commandForms entry)]),
          ("description", JsonString (commandDescription entry)),
          ("examples", JsonArray (map exampleJson (toList (commandExamples entry))))
        ]
    exampleJson example =
      object
        [ ("description", JsonString (exampleDescription example)),
          ("arguments", strings (toList (exampleArguments example))),
          ("command", JsonString (exampleCommandLine example)),
          ("output", strings (exampleOutput example))
        ]
    flagJson name =
      let entry = flag name
       in object
            [ ("name", JsonString (longSpelling entry)),
              ("spellings", strings (toList (flagSpellings entry))),
              ("argument", maybe JsonNull JsonString (flagArgument entry)),
              ("description", JsonString (flagDescription entry)),
              ("commands", strings (scopeInvocations (flagScope entry)))
            ]
    scopeInvocations GlobalFlag = [programName]
    scopeInvocations (CommandFlag names) = map commandInvocation (toList names)
    configurationJson =
      object
        [ ("description", JsonString (configDescription configuration)),
          ( "settings",
            JsonArray
              [ object
                  [ ("name", JsonString (settingName setting)),
                    ("description", JsonString (settingDescription setting)),
                    ("flag", maybe JsonNull (JsonString . longSpelling . flag) (settingFlag setting))
                  ]
              | setting <- toList (configSettings configuration)
              ]
          )
        ]
    object = JsonObject . Map.fromList
    strings = JsonArray . map JsonString

-- | JSON laid out one member or element per line, indented two spaces a
-- level, with a final newline: the layout of JavaScript's
-- @JSON.stringify(value, null, 2)@, so a change to one entry is a change
-- to its lines alone.  Each value is written as 'renderJson' writes it.
renderIndentedJson :: Json -> BS.ByteString
renderIndentedJson value = BL.toStrict (BB.toLazyByteString (layout 0 value <> BB.char7 '\n'))
  where
    layout :: Int -> Json -> BB.Builder
    layout depth json = case json of
      JsonArray items@(_ : _) -> nested depth '[' ']' (map (layout (depth + 1)) items)
      JsonObject members
        | not (Map.null members) ->
            nested depth '{' '}' [BB.byteString (renderJson (JsonString key)) <> ": " <> layout (depth + 1) member | (key, member) <- Map.toAscList members]
      leaf -> BB.byteString (renderJson leaf)
    nested depth open close items =
      BB.char7 open
        <> mconcat (intersperse (BB.char7 ',') (map (\item -> newline (depth + 1) <> item) items))
        <> newline depth
        <> BB.char7 close
    newline depth = BB.char7 '\n' <> BB.byteString (BC.replicate (2 * depth) ' ')
