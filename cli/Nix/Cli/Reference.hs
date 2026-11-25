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
  | DevelopCommand
  | PrintDevEnvCommand
  | TrustCommand
  | PushCommand
  | StoreDeleteCommand
  | StoreGcCommand
  | ProfileAddCommand
  | ProfileRemoveCommand
  | ProfileListCommand
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
  DevelopCommand -> ["develop"]
  PrintDevEnvCommand -> ["print-dev-env"]
  TrustCommand -> ["trust"]
  PushCommand -> ["push"]
  StoreDeleteCommand -> ["store", "delete"]
  StoreGcCommand -> ["store", "gc"]
  ProfileAddCommand -> ["profile", "add"]
  ProfileRemoveCommand -> ["profile", "remove"]
  ProfileListCommand -> ["profile", "list"]

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
        "nova-nix evaluates Nix expressions, builds derivations, starts shells with a derivation's build environment, reports what a derivation's closure relies on, pushes store paths to a binary cache, keeps profiles of packages and maintains its store, one command per invocation, and reads its settings from `nix.conf` as upstream Nix does. Each command takes the flags listed with it, after the command's name. A command exits with status 0 when it succeeds, and with status 1 when it fails, after saying why on stderr; `build --check` has another status, 104, for an output that differs, and `develop`, once its shell has started, exits with the shell's.",
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
          Form "PATH" "Evaluate a .nix file, or a directory's default.nix, print result"
            :| [ Form "'<NAME>'" "Evaluate the file the search path finds for NAME",
                 Form "--expr 'EXPR'" "Evaluate an inline expression"
               ],
        commandDescription =
          "Evaluates the expression in " <> targetSource <> ", and prints its value as `nix-instantiate --eval` prints it. With an argument from `--arg` or `--argstr`, the value is called first, as `nix-instantiate --eval` calls it: " <> callRule <> " Without one it is not called, and a function prints as `<LAMBDA>`; with `--aterm` it is called either way, as `nix-instantiate` calls it before it writes a derivation. A part of the value the evaluation never needed prints as `<CODE>`; with `--strict` the whole value is evaluated first. A relative path resolves against the file's directory, or for `--expr` and an `--arg` expression against the working directory. A `<name>` path, and a `<NAME>` argument, is looked up in the entries `--nix-path` adds, then `NIX_PATH`'s, then the `<nix/*>` expressions nova-nix ships with. An evaluation that writes to the store, as `builtins.toFile`, `builtins.path` and the fetchers do, stops with an error at its first write to a store directory that upstream Nix's database also manages, such as `/nix/store` where upstream Nix is installed; one that writes nothing is not refused. With `NOVA_NIX_C_STATS` set to a file, an evaluation that succeeds writes to it a census of the memory its C data layer holds: the objects and bytes of each kind allocated, those its result, the root environment, the import cache and the search path still reach, and what finding them cost. With `NOVA_NIX_REGION_STATS` set to a file, it writes a census of regions: at the end of each force of a thunk, the bytes the force allocated and the bytes of them still reachable, and the most bytes collectors freeing at those points would have held.",
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
                   "Call a function with an argument, its default filling the other."
                   ("eval" :| ["--arg", "x", "2", "--expr", "{ x, y ? 3 }: x * y"])
                   ["6"],
                 Example
                   "Call a function with a string argument."
                   ("eval" :| ["--argstr", "name", "world", "--expr", "{ name }: \"hello \" + name"])
                   ["\"hello world\""],
                 Example
                   "Evaluate an expression from a nixpkgs checkout in the working directory."
                   ("eval" :| ["--nix-path", "nixpkgs=./nixpkgs", "--expr", "(import <nixpkgs/lib>).version"])
                   []
               ]
      }
  BuildCommand ->
    CommandEntry
      { commandForms =
          Form "PATH" "Build a derivation from a .nix file, or a directory's default.nix"
            :| [ Form "'<NAME>'" "Build a derivation from the file the search path finds for NAME",
                 Form "--expr 'EXPR'" "Build a derivation from an inline expression"
               ],
        commandDescription =
          "Evaluates " <> targetSource <> ", selects the attribute `-A` names when it is given, builds the derivation that results and every derivation it depends on, and prints the path of the output its `outputName` names, the first of its outputs unless the expression selects another, as `nix-build` does. " <> targetCalls <> " A derivation whose outputs a substituter holds is downloaded instead of built, from the substituters `nix.conf` configures and the one `--substituter` adds, and only a path signed by a trusted key is accepted. The outputs of a derivation whose `allowSubstitutes` attribute is false are built instead, unless `always-allow-substitutes` is on. A substituter that fails stops the build unless `fallback` is on. With `--check` the derivation is checked as `nix-build --check` checks one: an output of its that is not valid is first downloaded from a substituter that holds it, the derivations it depends on are built or downloaded as ever, and then it is built again beside its outputs, never downloaded, and each output is compared with the one in the store. The rebuild changes nothing in the store and what it wrote is discarded; the downloads and builds before it add to the store as in any build. An output that comes out different fails the command with status 104, so a path a substituter supplied can be held to a build. With `keep-failed` on, a build that fails keeps its directory, under the store's `.nova-nix/builds`, until the derivation is next built, and names the directory its builder worked in on stderr, unless its builder is a builtin; and a check that finds an output different keeps that output's rebuild beside it as `PATH.check`, which nothing registers and the garbage collector removes, and names it in its error. The output is linked from `./result`, or from the path `--out-link` gives, with `-NAME` appended for an output `NAME` other than `out`, as `nix-build` links it, and the link is a root the garbage collector keeps; it replaces a link already there that points into the store, and anything else there fails the command. With `--no-out-link` nothing is linked. Where `sandbox` is on, as it is by default on Linux, a builder runs in a sandbox: it sees its inputs, the paths `sandbox-paths` names and its build directory, which on Linux it sees at `sandbox-build-dir`, and unless its derivation is fixed-output it reaches no network, which on Linux leaves it a loopback of its own; a system that cannot make the sandbox builds without it after a warning, or with `sandbox-fallback` off fails the build. A store directory that upstream Nix's database also manages, such as `/nix/store` where upstream Nix is installed, is refused before anything is evaluated.",
        commandExamples =
          Example
            "Build GNU Hello for Windows from the directory a release unpacks to, with the public cache as a substituter."
            ("build" :| ["pkgs/windows/hello.nix", "--substituter", "https://cache.novavero.ai", "--trusted-key", "cache.novavero.ai-1:9gQ7tLWMM+2tdC9H5sKMJltDIPfD7X2GWlZe8Aa8hHQ="])
            []
            :| [ Example
                   "Build an attribute of a file, linking the output from `./result`, a root the garbage collector keeps."
                   ("build" :| ["pkgs/windows/default.nix", "-A", "hello"])
                   [],
                 Example
                   "Build GNU Hello for Windows and print its path with no link, as a script that reads the path does."
                   ("build" :| ["pkgs/windows/hello.nix", "--no-out-link"])
                   [],
                 Example
                   "Build GNU Hello from a nixpkgs checkout in the working directory: nixpkgs is a function, called with its defaults before `hello` is selected from it."
                   ("build" :| ["--nix-path", "nixpkgs=./nixpkgs", "<nixpkgs>", "-A", "hello"])
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
  DevelopCommand ->
    CommandEntry
      { commandForms =
          Form "PATH" "Start a shell with the build environment of a derivation in a .nix file, or a directory's default.nix"
            :| [ Form "'<NAME>'" "Start a shell with the build environment of a derivation in the file the search path finds for NAME",
                 Form "--expr 'EXPR'" "Start a shell with the build environment of an inline derivation"
               ],
        commandDescription =
          "Evaluates " <> targetSource <> ", selects the attribute `-A` names when it is given, and starts `bash` with the environment the builder of the derivation that results has once its stdenv's setup has run, as `nix develop` does. " <> targetCalls <> " " <> captureRule <> " The shell sets each variable and function again, but for those that belong to the shell running it, such as `HOME`, `TERM` and the temporary directories, with each output's path replaced by `outputs/NAME` under the working directory. It then puts the caller's `PATH` and `XDG_DATA_DIRS` after the captured ones, points `NIX_BUILD_TOP` and the temporary-directory variables at a fresh directory, and runs `shellHook`. Under structured attributes it reads `.attrs.sh` and `.attrs.json` from a temporary directory, which outlives the shell. An interactive shell reads `~/.bashrc` first and takes its prompt from the `bash-prompt` settings. With `--command` the command runs in place of an interactive shell, and with `--phase`, or a flag naming a phase, the phase runs through the setup's `runPhase`, or through `runHook` where it has none. The shell is `bash` found on the `PATH` of the environment it starts with, `/bin/bash` on macOS unless that `PATH` names another, with `SHELL` set to `bash` and `NIX_GCROOT` to the copy's output, which the garbage collector does not yet keep while the shell runs. That environment is the caller's, changed by `--ignore-env`, `--keep-env-var`, `--unset-env-var` and `--set-env-var`. nova-nix replaces itself with the shell, so the command exits with the shell's status. " <> developRefusals,
        commandExamples =
          Example
            "Start a shell with GNU Hello's build environment, from a nixpkgs checkout in the working directory."
            ("develop" :| ["--nix-path", "nixpkgs=./nixpkgs", "<nixpkgs>", "-A", "hello"])
            []
            :| [ Example
                   "Run Hello's configure phase in its unpacked source, as its builder would."
                   ("develop" :| ["--nix-path", "nixpkgs=./nixpkgs", "<nixpkgs>", "-A", "hello", "--configure"])
                   [],
                 Example
                   "Run a command in Hello's build environment: here, print `$out`, which names `outputs/out` under the working directory."
                   ("develop" :| ["--nix-path", "nixpkgs=./nixpkgs", "<nixpkgs>", "-A", "hello", "--command", "sh", "-c", "echo $out"])
                   []
               ]
      }
  PrintDevEnvCommand ->
    CommandEntry
      { commandForms =
          Form "PATH" "Print the build environment of a derivation in a .nix file, or a directory's default.nix"
            :| [ Form "'<NAME>'" "Print the build environment of a derivation in the file the search path finds for NAME",
                 Form "--expr 'EXPR'" "Print the build environment of an inline derivation"
               ],
        commandDescription =
          "Evaluates " <> targetSource <> ", selects the attribute `-A` names when it is given, and prints the script that `develop`'s shell runs to set the build environment of the derivation that results, which a running `bash` can source, as `nix print-dev-env` does. " <> targetCalls <> " " <> captureRule <> " Under structured attributes the script names `.attrs.sh` and `.attrs.json` in a temporary directory, which is removed when the command exits, as upstream's is. With `--json` it prints the captured environment instead, as one JSON document of the variables, the functions and, under structured attributes, the two files the builder read, laid out over several lines when stdout is a terminal and on one otherwise, unless `--pretty` or `--no-pretty` says which. " <> developRefusals,
        commandExamples =
          Example
            "Print the script that sets GNU Hello's build environment, from a nixpkgs checkout in the working directory."
            ("print-dev-env" :| ["--nix-path", "nixpkgs=./nixpkgs", "<nixpkgs>", "-A", "hello"])
            []
            :| [ Example
                   "Print that environment as JSON."
                   ("print-dev-env" :| ["--nix-path", "nixpkgs=./nixpkgs", "<nixpkgs>", "-A", "hello", "--json"])
                   []
               ]
      }
  TrustCommand ->
    CommandEntry
      { commandForms =
          Form "PATH" "Report what the closure of a derivation in a .nix file, or a directory's default.nix, relies on"
            :| [ Form "'<NAME>'" "Report what the closure of a derivation in the file the search path finds for NAME relies on",
                 Form "--expr 'EXPR'" "Report what an inline derivation's closure relies on"
               ],
        commandDescription =
          "Evaluates " <> targetSource <> ", selects the attribute `-A` names when it is given, and reports what the closure of the derivation that results relies on. " <> targetCalls <> " A closure bottoms out in fixed-output derivations, whose outputs are held to the hash their recipe states, whether a builtin fetches them or the closure builds them. First come those a build runs as builders, where a derivation's builder is inside one's output, or inside the tree a builtin such as `builtin:unpack` makes from it and any others; then those whose files hold prebuilt code, each with the files found; then those not fully read, each with what was not. Each says whether a builtin fetched it or the closure built it, and names any other with the same hash. Then come how many hold no prebuilt code, the builders the closure does not contain, such as a program on the host, the builders in its input sources, the builtins nova-nix runs itself, and how many input sources its derivations take, store paths none of them builds. A fixed-output derivation whose builder is a builtin and which has no inputs is fetched first, or downloaded from a substituter, so that its bytes can be read; any other is only downloaded, with the paths it references, from a substituter that holds it, since making it would run its builder, and is read only when one does or the store already holds it. A fetch that fails stops the command, and so does a failed download of one that no builtin fetches. The scan finds prebuilt code by its header: a PE image, an ELF or Mach-O file, a COFF object, or an ar archive, whose members it does not read. It looks in a fetched file, in each file of a fetched tree, and in each file of a tar archive, plain or compressed with gzip, bzip2, xz or zstd, that one of those is; it does not open an archive inside another, and it reports a file in another archive format it knows, such as zip, as not read. It reports what the closure relies on, not whether any of it is safe. A store directory that upstream Nix's database also manages, such as `/nix/store` where upstream Nix is installed, is refused before anything is fetched or read.",
        commandExamples =
          Example
            "Report what GNU Hello for Windows relies on, from the directory a release unpacks to, with the public cache as a substituter."
            ("trust" :| ["pkgs/windows/hello.nix", "--substituter", "https://cache.novavero.ai", "--trusted-key", "cache.novavero.ai-1:9gQ7tLWMM+2tdC9H5sKMJltDIPfD7X2GWlZe8Aa8hHQ="])
            []
            :| [ Example
                   "Report what a derivation written inline relies on: its builder, `/bin/sh`, is a program on the host."
                   ("trust" :| ["--expr", "derivation { name = \"greeting\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; args = [ \"-c\" \"echo hello > $out\" ]; }"])
                   [ "Fixed-output inputs run as builders: 0",
                     "Fixed-output inputs holding prebuilt code: 0",
                     "Fixed-output inputs not fully read: 0",
                     "Fixed-output inputs holding no prebuilt code: 0",
                     "Builders outside the closure: 1",
                     "  /bin/sh, for 1 derivation",
                     "Builders in input sources: 0",
                     "Builtin builders: 0",
                     "Input sources: 0",
                     "This reports what the closure relies on, not whether any of it is safe."
                   ]
               ]
      }
  PushCommand ->
    CommandEntry
      { commandForms =
          Form "--cache URL PATH..." "Push store paths (and their closures) to a binary cache"
            :| [Form "--cache URL --all" "Push every valid path except derivations, and their closures"],
        commandDescription =
          "Copies the closure of each `PATH`, or with `--all` every valid path except derivations, to the binary cache at `URL`. A `PATH` is a store path, in full or as its base name; a derivation is refused, since a binary cache serves outputs. Paths the cache already holds are skipped, and every NAR is in place before any narinfo announces it. An `http://` or `https://` `URL` is a nova-cache server, which signs what it receives, and a narinfo is read back to check that the server signed it. A `file://` `URL` names a directory, which gets the layout upstream's `nix copy --to file://` writes, so that any web server can serve it: `nix-cache-info`, a narinfo for each path, signed with the secret key in `--secret-key-file`, and the NARs under `nar/`. The key file is one `nix-store --generate-binary-cache-key` writes, and a substituter of the cache trusts its public key. It prints how many paths it pushed and how many the cache already held.",
        commandExamples =
          Example
            "Push every path in the store except derivations, compressed with zstd, to a cache that takes the API key in `cache-api-key`."
            ("push" :| ["--cache", "https://cache.example.org", "--key-file", "cache-api-key", "--compression", "zstd", "--all"])
            []
            :| [ Example
                   "Write the closure of a build's output, here GNU Hello's, to a binary cache in `/srv/cache`, signed with the secret key in `cache-secret-key`; `HASH` stands for the hash in its path."
                   ("push" :| ["--cache", "file:///srv/cache", "--secret-key-file", "cache-secret-key", "/nix/store/HASH-hello"])
                   []
               ]
      }
  StoreDeleteCommand ->
    CommandEntry
      { commandForms = Form "PATH..." "Remove store paths, refused while a root or another valid path keeps them" :| [],
        commandDescription =
          "Deletes each `PATH`'s registration and its tree on disk, in the order given, and prints each one it deleted. A `PATH` is a store path, in full or as its base name, and an entry with only a registration or only a tree is deleted too. A path a root keeps, or that another valid path references, is refused, and the first refusal stops the command, so a path is named before the paths it references. A store directory that upstream Nix's database also manages, such as `/nix/store` where upstream Nix is installed, is refused.",
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
          "Deletes every store path no root keeps, directly or through the paths that reference it, and prints how many it deleted and the space it freed, as `nix-store --gc` does. The roots are the links `build` made to its outputs and each generation of a profile, while they stay, and the links and files under the store's `.nova-nix/gcroots` directory; a path a running program has open is not a root, and a rooted output does not keep the `.drv` that built it. A store directory that upstream Nix's database also manages, such as `/nix/store` where upstream Nix is installed, is refused. With `--print-roots` it lists the roots and deletes nothing.",
        commandExamples =
          Example "Delete everything no root keeps." ("store" :| ["gc"]) []
            :| [Example "List the roots, each as the link that names it and the path it keeps." ("store" :| ["gc", "--print-roots"]) []]
      }
  ProfileAddCommand ->
    CommandEntry
      { commandForms =
          Form "PATH" "Add a derivation from a .nix file, or the store path a path leads to, to a profile"
            :| [ Form "'<NAME>'" "Add a derivation from the file the search path finds for NAME to a profile",
                 Form "--expr 'EXPR'" "Add a derivation from an inline expression to a profile"
               ],
        commandDescription =
          "Adds a package to a profile and switches the profile to a new generation that holds it, as `nix profile add` does. A `PATH` that is in the store, or a link that leads into it, as the `./result` a build links does, adds the store path it is in, which must be valid. Any other `PATH`, a `<NAME>` or `--expr` is evaluated as `build` evaluates its target, and its derivation built for the outputs it installs: the one the value selects when its `outputSpecified` is true, else those its `meta.outputsToInstall` names, else every output. "
            <> targetCalls
            <> " The package is named after the names of its paths without their versions, with `-1`, `-2` and on appended to a name the profile already holds, and has the priority `--priority` gives, 5 unless it gives another. Where two packages hold a file at one path, the profile links the one with the lower number, and two with one number fail the command, which names both files and the commands that would resolve it. "
            <> profileRules
            <> " A store directory that upstream Nix's database also manages, such as `/nix/store` where upstream Nix is installed, is refused before anything is evaluated.",
        commandExamples =
          Example
            "Build GNU Hello for Windows from the directory a release unpacks to and add it to the default profile, which puts `hello` under `~/.nix-profile/bin`."
            ("profile" :| ["add", "pkgs/windows/hello.nix"])
            []
            :| [ Example
                   "Add what a build linked from `./result` to the default profile."
                   ("profile" :| ["add", "./result"])
                   [],
                 Example
                   "Add a package whose files the profile links in place of those of one at the default priority."
                   ("profile" :| ["add", "--priority", "4", "./result"])
                   []
               ]
      }
  ProfileRemoveCommand ->
    CommandEntry
      { commandForms =
          Form "NAME..." "Take packages off a profile, by name or by a store path they hold"
            :| [ Form "--regex PATTERN" "Take off a profile the packages whose names a regular expression matches",
                 Form "--all" "Take every package off a profile"
               ],
        commandDescription =
          "Takes packages off a profile and switches the profile to a new generation without them, as `nix profile remove` does. A `NAME` selects the package of that name, and a store path every package that holds it; `--regex` selects every package whose name, or for a package a flake added its flake reference and attribute, a POSIX extended regular expression matches whole, ignoring case; and `--all`, which takes nothing beside it, every package. Each package taken off is named, and then how many were taken off and kept; a selector that selects nothing is warned of, and when nothing is selected the profile is left as it is. A number is refused, as upstream refuses one in place of a name, and so is a line that selects nothing, or `--all` beside another selector. "
            <> profileRules
            <> " A store directory that upstream Nix's database also manages, such as `/nix/store` where upstream Nix is installed, is refused.",
        commandExamples =
          Example "Take GNU Hello off the default profile." ("profile" :| ["remove", "hello"]) []
            :| [Example "Take every package off a profile other than the default." ("profile" :| ["remove", "--all", "--profile", "./my-profile"]) []]
      }
  ProfileListCommand ->
    CommandEntry
      { commandForms =
          Form "" "Print the packages in a profile"
            :| [Form "--json" "Print a profile's manifest as JSON"],
        commandDescription =
          "Prints each package in a profile, as `nix profile list` does: its name, in bold, and its store paths, with a blank line between packages, and for a package a flake added its flake attribute and its original and locked flake references. With `--json` it prints the profile's `manifest.json` instead, indented when stdout is a terminal unless `--pretty` or `--no-pretty` says otherwise, the later of them winning. A profile with no manifest holds no packages. The default profile and its link are made when they are missing, and a profile whose current generation is outside the store is refused, as for every profile command.",
        commandExamples =
          Example "List the packages in the default profile." ("profile" :| ["list"]) []
            :| [Example "Print the default profile's manifest, indented." ("profile" :| ["list", "--json", "--pretty"]) []]
      }

-- | What a profile is and where it is, as the descriptions of the
-- commands that change one say.
profileRules :: Text
profileRules = "A profile is a link to one of its generations, `PROFILE-1-link`, `PROFILE-2-link` and on beside it, each a store path holding the packages' files linked into one tree and a `manifest.json` that lists the packages, and each a root the garbage collector keeps. The default profile is `profiles/profile` in the state directory, which is `$NIX_STATE_HOME`, else `$XDG_STATE_HOME/nix`, else `~/.local/state/nix`, or on Windows `nix\\state` in the Local AppData folder; `~/.nix-profile`, or with `use-xdg-base-directories` on `profile` in the state directory, links to it, and both are made when they are missing, whichever profile `--profile` names. A profile whose current generation is outside the store, as one upstream Nix keeps in `/nix/store` is while nova-nix uses another store, is refused. A change that finds the profile changed by another command since it began starts again. On Windows each link is a symlink, which needs Developer Mode or an elevated shell."

-- | What a command that takes a target evaluates, as its description
-- names it.
targetSource :: Text
targetSource = "the file `PATH` names, or its `default.nix` when `PATH` is a directory, the file the search path finds for `<NAME>`, or the expression `--expr` gives"

-- | How @develop@ and @print-dev-env@ capture a build environment, as
-- upstream's @nix develop@ does.
captureRule :: Text
captureRule = "The derivation's builder must be `bash`. A copy of it, named with `-env` after its name and with no checks on what its outputs refer to, is built in its place, with upstream's `get-env.sh` as its builder's script: it sources `$stdenv/setup` when `stdenv` is set, then writes the variables and functions bash holds to the copy's output, so the copy's path is the one `nix develop` builds. It reads the variables from the declare commands `declare -p` prints, which macOS's `/bin/bash`, bash 3.2, does not print, so a builder that is that bash captures none. What the derivation depends on is built or downloaded first, as `build` does, and a copy already built is not built again."

-- | What @develop@ and @print-dev-env@ refuse before evaluating anything.
developRefusals :: Text
developRefusals = "A store directory that upstream Nix's database also manages, such as `/nix/store` where upstream Nix is installed, is refused before anything is evaluated. On Windows the command is refused."

-- | How a value is called with the arguments @--arg@ and @--argstr@
-- give, upstream's @autoCallFunction@.
callRule :: Text
callRule = "a set with `__functor` stands for what its functor returns given the set, which is called in turn; a function with a set pattern is called with every argument when it takes `...` and otherwise with those its formals name, its defaults filling the rest, and a formal with neither an argument nor a default fails the command; any other value is left as it is."

-- | When @build@ and @trust@ call a function, as @nix-build@ does.
targetCalls :: Text
targetCalls = "Before each component of the `-A` path is selected from it, and once the path has selected it, the value is called as `nix-build` calls it, with the arguments from `--arg` and `--argstr` or with none: " <> callRule

-- ---------------------------------------------------------------------------
-- Flags
-- ---------------------------------------------------------------------------

-- | The flags, in the order the JSON and each command's list give them;
-- @--help@ groups them by the commands they apply to.
data FlagName
  = ExprFlag
  | AttrFlag
  | OutLinkFlag
  | NoOutLinkFlag
  | StrictFlag
  | AtermFlag
  | ReadWriteModeFlag
  | NixPathFlag
  | ArgFlag
  | ArgStrFlag
  | RestrictEvalFlag
  | PureEvalFlag
  | TraceVerboseFlag
  | NoAllowImportFromDerivationFlag
  | SubstituterFlag
  | TrustedKeyFlag
  | FallbackFlag
  | KeepFailedFlag
  | CheckFlag
  | ShellCommandFlag
  | PhaseFlag
  | UnpackFlag
  | ConfigureFlag
  | BuildPhaseFlag
  | CheckPhaseFlag
  | InstallFlag
  | InstallCheckFlag
  | IgnoreEnvFlag
  | KeepEnvVarFlag
  | UnsetEnvVarFlag
  | SetEnvVarFlag
  | SandboxFlag
  | NoSandboxFlag
  | RelaxedSandboxFlag
  | ExecWrapperFlag
  | CacheFlag
  | AllFlag
  | KeyFileFlag
  | SecretKeyFileFlag
  | CompressionFlag
  | PrintRootsFlag
  | ProfileFlag
  | PriorityFlag
  | RegexFlag
  | JsonFlag
  | PrettyFlag
  | NoPrettyFlag
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
    -- | A second long spelling, without its leading @--@, where upstream
    -- accepts one.
    flagAlias :: !(Maybe Text),
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

-- | A flag's spellings: the one-letter one first, where there is one,
-- then the long one and its alias.
flagSpellings :: FlagEntry -> NonEmpty Text
flagSpellings entry = case flagShort entry of
  Just letter -> T.pack ['-', letter] :| (longSpelling entry : alias)
  Nothing -> longSpelling entry :| alias
  where
    alias = map ("--" <>) (toList (flagAlias entry))

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
    long "expr" (Just "EXPR") "Evaluate `EXPR` in place of a file; a relative path in it resolves against the working directory" evaluating
  AttrFlag ->
    short 'A' "attr" (Just "ATTRPATH") "Select a dotted attribute path (`a.b.c`)" selecting
  OutLinkFlag ->
    short 'o' "out-link" (Just "PATH") "Link the output from `PATH` in place of `./result`" buildOnly
  NoOutLinkFlag ->
    aliased "no-out-link" "no-link" Nothing "Create no link to the output, and no GC root" buildOnly
  StrictFlag ->
    long "strict" Nothing "Deep-force all thunks before printing (warning: OOM on large results)" evalOnly
  AtermFlag ->
    long "aterm" Nothing "Print the `.drv` ATerm of the derivation the target evaluates to, in place of its value" evalOnly
  ReadWriteModeFlag ->
    long "read-write-mode" Nothing "Write what evaluation makes (`builtins.toFile`, `builtins.path` and the like) to the store and register it, and build or substitute what a path's context names, as `build` does; without it `eval` computes those paths and writes only what a fetcher fetches" evalOnly
  NixPathFlag ->
    long "nix-path" (Just "NAME=PATH") "Add search path (repeatable, merged with `NIX_PATH`)" evaluating
  ArgFlag ->
    long "arg" (Just "NAME EXPR") "Pass the value of `EXPR` as the argument `NAME` to a function the target evaluates to" evaluating
  ArgStrFlag ->
    long "argstr" (Just "NAME STRING") "Pass `STRING`, as a string, as the argument `NAME` to a function the target evaluates to" evaluating
  RestrictEvalFlag ->
    long "restrict-eval" Nothing "Confine reads to the search path roots and what the evaluation itself writes; fetch only `allowed-uris`" evaluating
  PureEvalFlag ->
    long "pure-eval" Nothing "Confine reads as `--restrict-eval` does, with no search path but the bundled `<nix/*>`, no `currentTime`, `currentSystem` or `storePath`, and pinned fetches only (`allowed-uris` applies only under `--restrict-eval`)" evaluating
  TraceVerboseFlag ->
    long "trace-verbose" Nothing "Make `builtins.traceVerbose` behave as `builtins.trace`; without it, the first argument is never forced" evaluating
  NoAllowImportFromDerivationFlag ->
    long "no-allow-import-from-derivation" Nothing "Refuse to build or substitute what a path's context names when `import` or another builtin reads the path, even an output already valid" evaluating
  SubstituterFlag ->
    long "substituter" (Just "URL") "Try this binary cache before building" evaluating
  TrustedKeyFlag ->
    long "trusted-key" (Just "KEY") "Trust signatures by this public key (`NAME:BASE64`) from any substituter" evaluating
  FallbackFlag ->
    long "fallback" Nothing "Build from source if substitution fails" evaluating
  KeepFailedFlag ->
    short 'K' "keep-failed" Nothing "Keep the directory of a build that fails, and the rebuild of an output `--check` finds different as `PATH.check`" building
  CheckFlag ->
    long "check" Nothing "Build the derivation again beside its valid outputs, and fail with status 104 if an output comes out different" buildOnly
  ShellCommandFlag ->
    short 'c' "command" (Just "CMD [ARG...]") "Run `CMD` with every argument after it, flags included, in place of an interactive shell" developOnly
  PhaseFlag ->
    long "phase" (Just "NAME") "Run the phase `NAME`, such as `build`, in place of an interactive shell; the last phase flag given wins" developOnly
  UnpackFlag ->
    long "unpack" Nothing "Run the `unpack` phase" developOnly
  ConfigureFlag ->
    long "configure" Nothing "Run the `configure` phase" developOnly
  BuildPhaseFlag ->
    long "build" Nothing "Run the `build` phase" developOnly
  CheckPhaseFlag ->
    long "check" Nothing "Run the `check` phase" developOnly
  InstallFlag ->
    long "install" Nothing "Run the `install` phase" developOnly
  InstallCheckFlag ->
    long "installcheck" Nothing "Run the `installCheck` phase" developOnly
  IgnoreEnvFlag ->
    shortAliased 'i' "ignore-env" "ignore-environment" Nothing "Start the shell with only the variables `--keep-env-var` names" developOnly
  KeepEnvVarFlag ->
    shortAliased 'k' "keep-env-var" "keep" (Just "NAME") "Keep the variable `NAME` under `--ignore-env` (repeatable)" developOnly
  UnsetEnvVarFlag ->
    shortAliased 'u' "unset-env-var" "unset" (Just "NAME") "Unset the variable `NAME` (repeatable)" developOnly
  SetEnvVarFlag ->
    short 's' "set-env-var" (Just "NAME VALUE") "Set the variable `NAME` to `VALUE` (repeatable)" developOnly
  SandboxFlag ->
    long "sandbox" Nothing "Enable sandboxing" evaluating
  NoSandboxFlag ->
    long "no-sandbox" Nothing "Disable sandboxing" evaluating
  RelaxedSandboxFlag ->
    long "relaxed-sandbox" Nothing "Enable sandboxing, but allow builds to disable it" evaluating
  ExecWrapperFlag ->
    long "exec-wrapper" (Just "S=PATH") "Run system `S`'s derivations through `PATH` (repeatable), e.g. `x86_64-windows=/usr/bin/wine`" (CommandFlag (EvalCommand :| [BuildCommand, ProfileAddCommand]))
  CacheFlag ->
    long "cache" (Just "URL") "The cache to push to: a nova-cache server's `http://` or `https://` URL, or a directory's `file://` URL (required)" pushOnly
  AllFlag ->
    long "all" Nothing "Select every valid path except derivations, in place of `PATH...`; for `profile remove`, every package" (CommandFlag (PushCommand :| [ProfileRemoveCommand]))
  KeyFileFlag ->
    long "key-file" (Just "PATH") "File holding a nova-cache server's API key" pushOnly
  SecretKeyFileFlag ->
    long "secret-key-file" (Just "PATH") "File holding the secret key that signs a `file://` cache's narinfos (required there)" pushOnly
  CompressionFlag ->
    long "compression" (Just "KIND") ("Artifact packaging (" <> pushCompressionValues <> "; default none)") pushOnly
  PrintRootsFlag ->
    long "print-roots" Nothing "List the roots as `LINK -> PATH` and delete nothing" (CommandFlag (StoreGcCommand :| []))
  ProfileFlag ->
    long "profile" (Just "PATH") "Work on the profile at `PATH` in place of the default profile" (CommandFlag (ProfileAddCommand :| [ProfileRemoveCommand, ProfileListCommand]))
  PriorityFlag ->
    long "priority" (Just "N") "Give the package priority `N` (default 5): of two files at one path, the profile links the one with the lower number" (CommandFlag (ProfileAddCommand :| []))
  RegexFlag ->
    long "regex" (Just "PATTERN") "Select the packages whose names a POSIX extended regular expression matches whole, ignoring case" (CommandFlag (ProfileRemoveCommand :| []))
  JsonFlag ->
    long "json" Nothing "Print JSON: the environment in place of the script, or the profile's `manifest.json` in place of its packages' lines" jsonCommands
  PrettyFlag ->
    long "pretty" Nothing "With `--json`, indent the JSON, as when stdout is a terminal" jsonCommands
  NoPrettyFlag ->
    long "no-pretty" Nothing "With `--json`, print the JSON on one line, as when stdout is not a terminal" jsonCommands
  StoreFlag ->
    long "store" (Just "DIR") "Use `DIR` as the store (default: the platform store)" (CommandFlag (EvalCommand :| [BuildCommand, DevelopCommand, PrintDevEnvCommand, TrustCommand, PushCommand, StoreDeleteCommand, StoreGcCommand, ProfileAddCommand, ProfileRemoveCommand, ProfileListCommand]))
  HelpFlag ->
    long "help" Nothing "Print the commands, flags and settings, and exit" GlobalFlag
  VersionFlag ->
    long "version" Nothing "Print the version and exit" GlobalFlag
  where
    long longName = FlagEntry longName Nothing Nothing
    short letter longName = FlagEntry longName (Just letter) Nothing
    aliased longName aliasName = FlagEntry longName Nothing (Just aliasName)
    shortAliased letter longName aliasName = FlagEntry longName (Just letter) (Just aliasName)
    evalOnly = CommandFlag (EvalCommand :| [])
    buildOnly = CommandFlag (BuildCommand :| [])
    pushOnly = CommandFlag (PushCommand :| [])
    developOnly = CommandFlag (DevelopCommand :| [])
    -- The commands that print JSON with --json.
    jsonCommands = CommandFlag (PrintDevEnvCommand :| [ProfileListCommand])
    -- The commands that select an attribute of what they evaluate, and
    -- those that evaluate.
    selecting = CommandFlag (BuildCommand :| [DevelopCommand, PrintDevEnvCommand, TrustCommand, ProfileAddCommand])
    evaluating = CommandFlag (EvalCommand :| [BuildCommand, DevelopCommand, PrintDevEnvCommand, TrustCommand, ProfileAddCommand])
    -- The commands whose builds keep-failed can keep: trust builds only
    -- with builtins, which keep nothing.
    building = CommandFlag (EvalCommand :| [BuildCommand, DevelopCommand, PrintDevEnvCommand, ProfileAddCommand])

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
    -- | The flag that adds to it on one invocation, where there is one: an
    -- entry to a list, or a Boolean moved from its default.
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
               Setting "keep-failed" "Keep the directory of a build that fails, and the rebuild of an output `--check` finds different as `PATH.check` (default `false`)" (Just KeepFailedFlag),
               Setting "always-allow-substitutes" "Substitute a derivation whose `allowSubstitutes` attribute is false" Nothing,
               Setting "cores" "The `NIX_BUILD_CORES` a builder is given, `0` asking it to use every core (default: the CPU limit of the process's cgroup on Linux, otherwise the hardware threads)" Nothing,
               Setting "max-call-depth" ("How many function calls may be active around a new one before evaluation refuses it (default " <> T.pack (show defaultMaxCallDepth) <> ")") Nothing,
               Setting "restrict-eval" "Turn on restricted evaluation" (Just RestrictEvalFlag),
               Setting "pure-eval" "Turn on pure evaluation" (Just PureEvalFlag),
               Setting "allowed-uris" "URI prefixes a fetch may reach under `restrict-eval`" Nothing,
               Setting "trace-verbose" "Make `builtins.traceVerbose` trace" (Just TraceVerboseFlag),
               Setting "allow-import-from-derivation" "Let a builtin that reads a path build or substitute what the path's context names (default: true)" (Just NoAllowImportFromDerivationFlag),
               Setting "trace-import-from-derivation" "Warn, naming the first output, when a builtin that reads a path realises what its context names" Nothing,
               Setting "sandbox" "Whether a build runs in a sandbox: `true`, `relaxed` (a build that is fixed-output or sets `__noChroot` does not) or `false` (default: `true` on Linux, `false` elsewhere)" (Just SandboxFlag),
               Setting "sandbox-fallback" "Build unsandboxed, after a warning, where a Linux system cannot make the sandbox; on macOS such a build fails (default `true`)" Nothing,
               Setting "sandbox-paths" "Host paths a sandboxed build sees, each `TARGET=SOURCE` or a path naming itself, with `?` after one that may be missing (default: on Linux `/bin/sh` as a release archive's bundled shell; on macOS `/System/Library/Frameworks`, `/System/Library/PrivateFrameworks`, `/bin/sh`, `/bin/bash` and `/usr/lib`)" Nothing,
               Setting "sandbox-build-dir" "Where a sandboxed build on Linux sees its build directory (default `/build`)" Nothing,
               Setting "sandbox-dev-shm-size" "The size of a sandboxed build's `/dev/shm`, as `tmpfs` takes it (default `50%`)" Nothing,
               Setting "allowed-impure-host-deps" "Host paths at or beneath which a derivation's `__impureHostDeps` may name one for a sandboxed build on macOS (default on macOS: `/System/Library`, `/usr/lib`, `/dev` and `/bin/sh`)" Nothing,
               Setting "darwin-log-sandbox-violations" "Send what a sandboxed build's profile refuses on macOS to the system log" Nothing,
               Setting "require-drop-supplementary-groups" "Fail a sandboxed build when the user's supplementary groups cannot be dropped (default: on for root)" Nothing,
               Setting "use-xdg-base-directories" "Link the default profile from `profile` in the state directory in place of `~/.nix-profile` (default `false`)" Nothing,
               Setting "ssl-cert-file" "The CA bundle a sandboxed fixed-output build sees (default: `$NIX_SSL_CERT_FILE`, `$SSL_CERT_FILE`, or the first that exists of `/etc/ssl/certs/ca-certificates.crt` and `etc/ssl/certs/ca-bundle.crt` under `/nix/var/nix/profiles/default`)" Nothing,
               Setting "bash-prompt" "The prompt (`PS1`) of an interactive `develop` shell, in place of the user's" Nothing,
               Setting "bash-prompt-prefix" "Text put before the prompt of an interactive `develop` shell" Nothing,
               Setting "bash-prompt-suffix" "Text put after the prompt of an interactive `develop` shell" Nothing
             ],
      configDescription =
        "`nix.conf` is read in upstream's order: `$NIX_CONF_DIR/nix.conf`, where `NIX_CONF_DIR` defaults to "
          <> codeSpan (systemConfDirName PosixHost)
          <> ", or "
          <> codeSpan (systemConfDirName WindowsHost)
          <> " on Windows; the XDG `nix/nix.conf` files or `$NIX_USER_CONF_FILES`; then `$NIX_CONFIG`. A later source replaces an earlier one's value, an `extra-` prefix on a list setting adds to it instead, and a flag adds to whatever they configure: an entry to a list, or a Boolean moved from its default, which a flag cannot move back."
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
