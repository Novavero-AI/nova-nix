-- | The command line's grammar: the argument vector parsed into the
-- command to run and the flags that shape it.
--
-- What each command and flag means is documented in "Nix.Cli.Reference",
-- which @--help@ prints.  The test suite checks every flag that reference
-- lists against this parser: each command listed for a flag accepts it,
-- and the flag takes the values its entry names, and none when it names
-- none.  The parser also accepts some flags where they have no effect,
-- such as @--strict@ before @build@, which the reference does not list.
module Nix.Cli.Args
  ( CliOpts (..),
    Command (..),
    Target (..),
    fileTarget,
    AutoArg (..),
    OutLink (..),
    outLinkFor,
    DevelopArgs (..),
    DevEnvFormat (..),
    PushArgs (..),
    ProfileSelection (..),
    RemoveSelection (..),
    RemoveMatcher (..),
    ListFormat (..),
    JsonLayout (..),
    parseArgs,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Nix.Builder.Sandbox (SandboxMode (..))
import Nix.Develop.Shell (EnvironmentBase (..))

-- | Parsed CLI options.
data CliOpts = CliOpts
  { optNixPaths :: ![T.Text],
    -- | @--arg@ and @--argstr@: what a function the target evaluates to
    -- is called with, by name as the command line spells it.  A later
    -- flag for a name replaces an earlier one, as upstream's
    -- @insert_or_assign@ does (common-eval-args.cc:62 and 70 at 2.24.9).
    optAutoArgs :: !(Map String AutoArg),
    optStrict :: !Bool,
    optAterm :: !Bool,
    -- | Store directory override (default: the platform store).
    optStore :: !(Maybe FilePath),
    -- | Binary cache URL to substitute from before building.
    optSubstituter :: !(Maybe String),
    -- | A public key (@name:base64@) trusted for every substituter's
    -- signatures, the configured ones included.
    optTrustedKey :: !(Maybe String),
    -- | @SYSTEM=PATH@ launchers for derivations this machine cannot execute
    -- directly, e.g. @x86_64-windows=/path/to/wine@.
    optExecWrappers :: ![String],
    -- | @--restrict-eval@: upstream's restrict-eval, turned on for this run.
    optRestrictEval :: !Bool,
    -- | @--pure-eval@: upstream's pure-eval, turned on for this run.
    optPureEval :: !Bool,
    -- | @--trace-verbose@: upstream's trace-verbose, turned on for this run.
    optTraceVerbose :: !Bool,
    -- | @--no-allow-import-from-derivation@: upstream's
    -- allow-import-from-derivation, turned off for this run.
    optNoAllowImportFromDerivation :: !Bool,
    -- | @--fallback@: upstream's fallback, turned on for this run.
    optFallback :: !Bool,
    -- | @--check@: the build checks its derivation, as @nix-build --check@
    -- does.
    optCheck :: !Bool,
    -- | @--read-write-mode@: @eval@ writes what evaluation makes to the
    -- store and realises what a path's context names, as
    -- @nix-instantiate --eval --read-write-mode@ does.
    optReadWriteMode :: !Bool,
    -- | @--sandbox@, @--no-sandbox@ or @--relaxed-sandbox@, the last given:
    -- upstream's @sandbox@ on this run.
    optSandbox :: !(Maybe SandboxMode),
    optCommand :: !Command
  }
  deriving (Eq, Show)

-- | The command an invocation runs.
data Command
  = -- | @eval@: evaluate the target and print its value.
    CmdEval !Target
  | CmdBuild !Target !(Maybe T.Text) !OutLink
  | -- | @develop@: start a shell with the build environment of the
    -- target's derivation, at the attribute path when one is given.
    CmdDevelop !Target !(Maybe T.Text) !DevelopArgs
  | -- | @print-dev-env@: print that environment, with JSON laid out as
    -- the layout says.
    CmdPrintDevEnv !Target !(Maybe T.Text) !DevEnvFormat !JsonLayout
  | -- | @trust@: report what the closure of the target's derivation, at
    -- the attribute path when one is given, relies on.
    CmdTrust !Target !(Maybe T.Text)
  | CmdPush !PushArgs
  | CmdStoreDelete ![String]
  | -- | @store gc@: collect, or with the flag only list the roots.
    CmdStoreGc !Bool
  | -- | @profile add@: add the target's derivation, built, or a store
    -- path a file argument leads to, to a profile, with the attribute
    -- path when one is given, and the priority @--priority@ gives, as the
    -- command line spells it.
    CmdProfileAdd !Target !(Maybe T.Text) !ProfileSelection !(Maybe String)
  | -- | @profile remove@: take what the selection selects off a profile.
    CmdProfileRemove !ProfileSelection !RemoveSelection
  | -- | @profile list@: print what a profile holds.
    CmdProfileList !ProfileSelection !ListFormat
  | -- | No command given.  Usage on stderr, non-zero: a bare invocation is
    -- a usage error, and a caller testing the exit status must see one.
    CmdUsage
  | -- | @--help@.  The same text on stdout, zero: it is a request that
    -- succeeded, and pipeable without redirecting stderr.
    CmdHelp
  | CmdVersion
  deriving (Eq, Show)

-- | Where a command's expression comes from.  A name or an expression is
-- the argument as 'System.Environment.getArgs' gave it, whose bytes the
-- command reads ("Nix.Environment"'s @argumentBytes@), as upstream reads
-- the bytes it was given.
data Target
  = -- | A @.nix@ file, or a directory standing for its @default.nix@, as
    -- upstream's @resolveExprPath@ reads one (eval.cc:2968-2985 at
    -- 2.24.9).  Relative paths inside it resolve beside the file.
    TargetFile !FilePath
  | -- | @\<NAME\>@: the file the search path finds for the name, as
    -- @\<NAME\>@ in an expression finds it, read as 'TargetFile' reads
    -- one.
    TargetSearchPath !String
  | -- | An inline expression.  Relative paths inside it resolve against the
    -- working directory, since there is no file to sit beside.
    TargetExpr !String
  deriving (Eq, Show)

-- | The target a file argument names: a search path lookup when it is
-- longer than two characters and written in angle brackets, as upstream's
-- @lookupFileArg@ reads one (common-eval-args.cc:188-191 at 2.24.9), and
-- otherwise a file or directory.
fileTarget :: String -> Target
fileTarget arg = case arg of
  '<' : rest
    | '>' : nameReversed@(_ : _) <- reverse rest -> TargetSearchPath (reverse nameReversed)
  _ -> TargetFile arg

-- | A value the function a target evaluates to is called with, the
-- argument as 'System.Environment.getArgs' gave it, whose bytes the
-- command reads.
data AutoArg
  = -- | @--arg@: an expression, evaluated in the root environment when
    -- the function uses it, its relative paths resolved against the
    -- working directory.
    AutoArgExpr !String
  | -- | @--argstr@: a string holding the argument's bytes.
    AutoArgString !String
  deriving (Eq, Show)

-- | Where a build links its output.
data OutLink
  = -- | Link it from a name made from this path ('outLinkFor'), and
    -- register the link as a GC root.
    OutLinkAt !FilePath
  | -- | @--no-out-link@: link nothing.  nix-build links into a temporary
    -- directory it deletes on exit (nix-build.cc:143 and 203-204 at
    -- 2.24.9), which leaves a dangling record that its next collection
    -- removes (gc.cc:246-249); linking nothing leaves the same, less the
    -- record.
    NoOutLink
  deriving (Eq, Show)

-- | What a build links its output from until a flag says otherwise:
-- nix-build's @.\/result@ (nix-build.cc:145 at 2.24.9).
defaultOutLink :: OutLink
defaultOutLink = OutLinkAt "result"

-- | The output whose link is the out-link's path itself.
unsuffixedOutput :: T.Text
unsuffixedOutput = "out"

-- | The link a build of the named output creates, if any: the out-link's
-- path for the output @out@, and the path with @-NAME@ appended for any
-- other, as nix-build names its links (nix-build.cc:701-720 at 2.24.9).
-- nix-build also appends @-2@, @-3@ and so on for the second and later
-- derivations it builds; a build here builds one.
outLinkFor :: OutLink -> T.Text -> Maybe FilePath
outLinkFor outLink outputName = case outLink of
  NoOutLink -> Nothing
  OutLinkAt path
    | outputName == unsuffixedOutput -> Just path
    | otherwise -> Just (path <> "-" <> T.unpack outputName)

-- | The profile a profile command works on.
data ProfileSelection
  = -- | The default profile.
    DefaultProfile
  | -- | The profile @--profile@ names, as the command line spells it.
    ProfileAt !FilePath
  deriving (Eq, Show)

-- | What @profile remove@ takes off.  Upstream refuses a line that
-- selects nothing, and @--all@ beside any other selector, as usage errors
-- (profile.cc:617-628), so neither is a selection.
data RemoveSelection
  = -- | @--all@: every element.
    RemoveAll
  | -- | The elements any of these select, the flags' first.
    RemoveSome !(NonEmpty RemoveMatcher)
  deriving (Eq, Show)

-- | One selector of @profile remove@, as the command line spells it.
data RemoveMatcher
  = -- | An element's name or a store path an element holds.
    RemoveNamed !String
  | -- | @--regex@: the elements whose identifier a pattern matches.
    RemoveRegex !String
  deriving (Eq, Show)

-- | How @profile list@ prints.
data ListFormat
  = -- | Upstream's lines for each element.
    ListText
  | -- | @--json@: the manifest's JSON, laid out so.
    ListJson !JsonLayout
  deriving (Eq, Show)

-- | How @profile list --json@ and @print-dev-env --json@ lay out their
-- JSON, as upstream's @MixPrintJSON@ chooses (common-args.hh:46-72 at
-- 2.35.4): @--pretty@ and @--no-pretty@ set it, the last one given
-- winning.
data JsonLayout
  = -- | Indented when stdout is a terminal, on one line otherwise:
    -- upstream's default.
    IndentedOnTerminal
  | -- | @--pretty@: indented.
    Indented
  | -- | @--no-pretty@: on one line.
    Compact
  deriving (Eq, Show)

-- | What @profile list@ is printing while its line is parsed.
data ListOutput = TextOutput | JsonOutput

-- | A selector of @profile remove@ while its line is parsed.
data Selected = SelectedAll | Selected !RemoveMatcher

-- | What @develop@ runs in place of an interactive shell, and how it
-- changes the shell's environment, as the command line gives them, each
-- name, value and word as 'System.Environment.getArgs' gave it.  The
-- refusals upstream makes only once the environment is built,
-- @--command@ beside a phase and an environment flag that makes no sense
-- with @--ignore-env@ or without it, are made then here too
-- ("Nix.Develop.Shell").
data DevelopArgs = DevelopArgs
  { -- | @--command@: every argument after it.
    daCommand :: !(Maybe (NonEmpty String)),
    -- | The phase @--phase@ or a phase flag named, the last one given.
    daPhase :: !(Maybe String),
    -- | @--ignore-env@.
    daEnvironmentBase :: !EnvironmentBase,
    -- | @--keep-env-var@'s names.
    daKeep :: !(Set String),
    -- | @--unset-env-var@'s names.
    daUnset :: !(Set String),
    -- | @--set-env-var@'s names, each with its value.
    daSet :: !(Map String String)
  }
  deriving (Eq, Show)

-- | @develop@'s arguments before any flag is parsed: an interactive shell
-- in the caller's environment.
emptyDevelopArgs :: DevelopArgs
emptyDevelopArgs = DevelopArgs Nothing Nothing CallerEnvironment Set.empty Set.empty Map.empty

-- | What @print-dev-env@ prints.
data DevEnvFormat
  = -- | The script that sets the environment.
    DevEnvScript
  | -- | @--json@: the environment as JSON.
    DevEnvJson
  deriving (Eq, Show)

-- | A command that evaluates one target to a derivation.
data TargetCommand
  = -- | @build@, which builds it.
    BuildsTarget
  | -- | @develop@, which starts a shell with its build environment.
    DevelopsTarget
  | -- | @print-dev-env@, which prints that environment.
    PrintsDevEnv
  | -- | @trust@, which reports what its closure relies on.
    TrustsTarget
  | -- | @profile add@, which builds it and adds it to a profile.
    AddsToProfile

-- | The words that select a target command.
targetWord :: TargetCommand -> String
targetWord kind = case kind of
  BuildsTarget -> "build"
  DevelopsTarget -> "develop"
  PrintsDevEnv -> "print-dev-env"
  TrustsTarget -> "trust"
  AddsToProfile -> "profile add"

-- | Whether a target command builds what it evaluates, and so takes the
-- launchers @--exec-wrapper@ names.  @develop@ and @print-dev-env@ build
-- a capture whose environment a shell on this host reads, which a
-- launcher for another system cannot serve, so they take none.
buildsTarget :: TargetCommand -> Bool
buildsTarget kind = case kind of
  BuildsTarget -> True
  AddsToProfile -> True
  DevelopsTarget -> False
  PrintsDevEnv -> False
  TrustsTarget -> False

-- | Arguments to a target command, while the target is still unknown.
data BuildArgs = BuildArgs
  { baTarget :: !(Maybe Target),
    baAttrPath :: !(Maybe T.Text),
    -- | Where @build@ links its output.  Only @build@ takes a flag that
    -- sets it.
    baOutLink :: !OutLink,
    -- | The profile @profile add@ adds to.  Only @profile add@ takes a
    -- flag that sets it.
    baProfile :: !ProfileSelection,
    -- | The priority @profile add@ gives what it adds.  Only
    -- @profile add@ takes a flag that sets it.
    baPriority :: !(Maybe String),
    -- | What @develop@ runs, and its shell's environment.  Only @develop@
    -- takes a flag that sets them.
    baDevelop :: !DevelopArgs,
    -- | What @print-dev-env@ prints.  Only @print-dev-env@ takes a flag
    -- that sets it.
    baFormat :: !DevEnvFormat,
    -- | How @print-dev-env@ lays out its JSON.  Only @print-dev-env@
    -- takes a flag that sets it.
    baJsonLayout :: !JsonLayout
  }

-- | Target command arguments before any flag is parsed.
emptyBuildArgs :: BuildArgs
emptyBuildArgs = BuildArgs Nothing Nothing defaultOutLink DefaultProfile Nothing emptyDevelopArgs DevEnvScript IndentedOnTerminal

-- | Arguments to the push command.
data PushArgs = PushArgs
  { paCacheUrl :: !(Maybe String),
    paKeyFile :: !(Maybe FilePath),
    -- | The file holding the secret key that signs a directory cache's
    -- narinfos.
    paSecretKeyFile :: !(Maybe FilePath),
    paCompressionArg :: !(Maybe String),
    paAll :: !Bool,
    paPaths :: ![String]
  }
  deriving (Eq, Show)

-- | Push arguments before any flag is parsed.
emptyPushArgs :: PushArgs
emptyPushArgs = PushArgs Nothing Nothing Nothing Nothing False []

-- | Parse the command line.  A malformed invocation is an error, never a
-- silent drop: an unknown or misspelled flag fails the parse, so nothing
-- after it (a requested @--substituter@, say) is quietly discarded.
parseArgs :: [String] -> Either String CliOpts
parseArgs = go (CliOpts [] Map.empty False False Nothing Nothing Nothing [] False False False False False False False Nothing CmdUsage)
  where
    go opts [] = Right opts
    go opts (flag : _) | Just answer <- answered flag = Right opts {optCommand = answer}
    go opts ("--nix-path" : val : rest) =
      go (opts {optNixPaths = optNixPaths opts ++ [T.pack val]}) rest
    go opts (flag : name : value : rest)
      | Just arg <- autoArg flag value = go (withAutoArg name arg opts) rest
    go opts ("--strict" : rest) =
      go (opts {optStrict = True}) rest
    go opts ("--aterm" : rest) =
      go (opts {optAterm = True}) rest
    go opts ("--store" : dir : rest) =
      go (opts {optStore = Just dir}) rest
    go opts ("--substituter" : url : rest) =
      go (opts {optSubstituter = Just url}) rest
    go opts ("--trusted-key" : key : rest) =
      go (opts {optTrustedKey = Just key}) rest
    go opts ("--exec-wrapper" : spec : rest) =
      go (opts {optExecWrappers = optExecWrappers opts ++ [spec]}) rest
    go opts ("--restrict-eval" : rest) =
      go (opts {optRestrictEval = True}) rest
    go opts ("--pure-eval" : rest) =
      go (opts {optPureEval = True}) rest
    go opts ("--trace-verbose" : rest) =
      go (opts {optTraceVerbose = True}) rest
    go opts ("--no-allow-import-from-derivation" : rest) =
      go (opts {optNoAllowImportFromDerivation = True}) rest
    go opts ("--fallback" : rest) =
      go (opts {optFallback = True}) rest
    go opts ("--read-write-mode" : rest) =
      go (opts {optReadWriteMode = True}) rest
    go opts (flag : rest)
      | Just mode <- sandboxFlag flag = go (opts {optSandbox = Just mode}) rest
    go opts ("eval" : rest) = goEval opts rest
    go opts ("build" : rest) = goTarget BuildsTarget opts emptyBuildArgs rest
    go opts ("develop" : rest) = goTarget DevelopsTarget opts emptyBuildArgs rest
    go opts ("print-dev-env" : rest) = goTarget PrintsDevEnv opts emptyBuildArgs rest
    go opts ("trust" : rest) = goTarget TrustsTarget opts emptyBuildArgs rest
    go opts ("push" : rest) = goPush opts emptyPushArgs rest
    go opts ("store" : rest) = goStore opts rest
    go opts ("profile" : rest) = goProfile opts rest
    go _ [flag]
      | flag `elem` valueFlags = Left (flag ++ " requires a value")
    go _ (flag : _)
      | flag `elem` autoArgFlags = Left (flag ++ autoArgShortfall)
    go _ (arg : _) = Left ("unknown argument: " ++ arg ++ " (run nova-nix --help for usage)")
    -- Sub-parser for eval: handles --strict and --expr interleaved with the file arg.
    goEval opts [] = Right opts
    goEval opts (flag : _) | Just answer <- answered flag = Right opts {optCommand = answer}
    goEval opts ("--strict" : rest) = goEval (opts {optStrict = True}) rest
    goEval opts ("--aterm" : rest) = goEval (opts {optAterm = True}) rest
    goEval opts ("--nix-path" : val : rest) =
      goEval (opts {optNixPaths = optNixPaths opts ++ [T.pack val]}) rest
    goEval opts (flag : name : value : rest)
      | Just arg <- autoArg flag value = goEval (withAutoArg name arg opts) rest
    -- Accepted here as well as at the top level, like build and push:
    -- eval-side reads follow the selected store, so the flag means as
    -- much after the subcommand as before it.
    goEval opts ("--store" : dir : rest) =
      goEval (opts {optStore = Just dir}) rest
    goEval opts ("--restrict-eval" : rest) =
      goEval (opts {optRestrictEval = True}) rest
    goEval opts ("--pure-eval" : rest) =
      goEval (opts {optPureEval = True}) rest
    goEval opts ("--trace-verbose" : rest) =
      goEval (opts {optTraceVerbose = True}) rest
    goEval opts ("--no-allow-import-from-derivation" : rest) =
      goEval (opts {optNoAllowImportFromDerivation = True}) rest
    goEval opts ("--fallback" : rest) =
      goEval (opts {optFallback = True}) rest
    goEval opts ("--read-write-mode" : rest) =
      goEval (opts {optReadWriteMode = True}) rest
    goEval opts ("--substituter" : url : rest) =
      goEval (opts {optSubstituter = Just url}) rest
    goEval opts ("--trusted-key" : key : rest) =
      goEval (opts {optTrustedKey = Just key}) rest
    goEval opts ("--exec-wrapper" : spec : rest) =
      goEval (opts {optExecWrappers = optExecWrappers opts ++ [spec]}) rest
    goEval opts (flag : rest)
      | Just mode <- sandboxFlag flag = goEval (opts {optSandbox = Just mode}) rest
    goEval opts ("--expr" : expr : rest) =
      go (opts {optCommand = CmdEval (TargetExpr expr)}) rest
    goEval _ [flag]
      | flag `elem` valueFlags = Left (flag ++ " requires a value")
    goEval _ (flag : _)
      | flag `elem` autoArgFlags = Left (flag ++ autoArgShortfall)
    goEval _ (arg@('-' : _) : _) = Left ("unknown eval flag: " ++ arg)
    goEval opts (path : rest) =
      go (opts {optCommand = CmdEval (fileTarget path)}) rest
    -- Sub-parser for build and trust: the target, -A, and the shared flags
    -- in any order, and for build its own flags.  The shared flags are
    -- handled here rather than deferred back to 'go' so that one can follow
    -- the file argument, which is where a caller reaches for it, and so
    -- that -A is still recognised after it.
    goTarget kind opts targetArgs [] = finishTarget kind opts targetArgs
    goTarget _ opts _ (flag : _) | Just answer <- answered flag = Right opts {optCommand = answer}
    goTarget kind opts targetArgs ("--store" : dir : rest) =
      goTarget kind (opts {optStore = Just dir}) targetArgs rest
    goTarget kind opts targetArgs ("--substituter" : url : rest) =
      goTarget kind (opts {optSubstituter = Just url}) targetArgs rest
    goTarget kind opts targetArgs ("--trusted-key" : key : rest) =
      goTarget kind (opts {optTrustedKey = Just key}) targetArgs rest
    goTarget kind opts targetArgs ("--nix-path" : val : rest) =
      goTarget kind (opts {optNixPaths = optNixPaths opts ++ [T.pack val]}) targetArgs rest
    goTarget kind opts targetArgs (flag : name : value : rest)
      | Just arg <- autoArg flag value = goTarget kind (withAutoArg name arg opts) targetArgs rest
    goTarget kind opts targetArgs ("--exec-wrapper" : spec : rest)
      | buildsTarget kind = goTarget kind (opts {optExecWrappers = optExecWrappers opts ++ [spec]}) targetArgs rest
    goTarget kind opts targetArgs ("--restrict-eval" : rest) =
      goTarget kind (opts {optRestrictEval = True}) targetArgs rest
    goTarget kind opts targetArgs ("--pure-eval" : rest) =
      goTarget kind (opts {optPureEval = True}) targetArgs rest
    goTarget kind opts targetArgs ("--trace-verbose" : rest) =
      goTarget kind (opts {optTraceVerbose = True}) targetArgs rest
    goTarget kind opts targetArgs ("--no-allow-import-from-derivation" : rest) =
      goTarget kind (opts {optNoAllowImportFromDerivation = True}) targetArgs rest
    goTarget kind opts targetArgs ("--fallback" : rest) =
      goTarget kind (opts {optFallback = True}) targetArgs rest
    goTarget BuildsTarget opts targetArgs ("--check" : rest) =
      goTarget BuildsTarget (opts {optCheck = True}) targetArgs rest
    goTarget kind opts targetArgs (flag : rest)
      | Just mode <- sandboxFlag flag = goTarget kind (opts {optSandbox = Just mode}) targetArgs rest
    goTarget kind opts targetArgs ("--expr" : expr : rest) =
      withTarget kind opts targetArgs (TargetExpr expr) rest
    goTarget kind opts targetArgs (flag : path : rest)
      | flag `elem` attrFlags = case baAttrPath targetArgs of
          Just _ -> Left (targetWord kind ++ " accepts one attribute path")
          Nothing -> goTarget kind opts (targetArgs {baAttrPath = Just (T.pack path)}) rest
    -- Each link flag replaces what the flags before it said, so the
    -- later one wins, as nix-build assigns its outLink as it reads each
    -- (nix-build.cc:203-204 and 212-213 at 2.24.9).
    goTarget BuildsTarget opts targetArgs (flag : path : rest)
      | flag `elem` outLinkFlags = goTarget BuildsTarget opts (targetArgs {baOutLink = OutLinkAt path}) rest
    goTarget BuildsTarget opts targetArgs (flag : rest)
      | flag `elem` noOutLinkFlags = goTarget BuildsTarget opts (targetArgs {baOutLink = NoOutLink}) rest
    goTarget AddsToProfile opts targetArgs ("--profile" : path : rest) =
      goTarget AddsToProfile opts (targetArgs {baProfile = ProfileAt path}) rest
    goTarget AddsToProfile opts targetArgs ("--priority" : priority : rest) =
      goTarget AddsToProfile opts (targetArgs {baPriority = Just priority}) rest
    -- Every argument after --command is the command's, a flag or --help
    -- included, as upstream's flag of any arity takes them
    -- (args.cc:398-418 at 2.35.4), and one is needed (develop.cc:525-526).
    goTarget DevelopsTarget opts targetArgs (flag : rest)
      | flag `elem` commandFlags = case rest of
          [] -> Left "error: --command requires at least one argument"
          first : more -> finishTarget DevelopsTarget opts (developing targetArgs (\args -> args {daCommand = Just (first :| more)}))
    goTarget DevelopsTarget opts targetArgs ("--phase" : phase : rest) =
      goTarget DevelopsTarget opts (developing targetArgs (\args -> args {daPhase = Just phase})) rest
    goTarget DevelopsTarget opts targetArgs (flag : rest)
      | Just phase <- lookup flag phaseFlags = goTarget DevelopsTarget opts (developing targetArgs (\args -> args {daPhase = Just phase})) rest
      | flag `elem` ignoreEnvFlags = goTarget DevelopsTarget opts (developing targetArgs (\args -> args {daEnvironmentBase = EmptyEnvironment})) rest
    -- The refusals upstream's flag handlers make as they read
    -- --unset-env-var and --set-env-var (command.cc:362-386 at 2.35.4),
    -- each after error:, as nova-nix reports upstream's errors.
    goTarget DevelopsTarget opts targetArgs (flag : name : rest)
      | flag `elem` keepFlags = goTarget DevelopsTarget opts (developing targetArgs (\args -> args {daKeep = Set.insert name (daKeep args)})) rest
      | flag `elem` unsetFlags,
        Map.member name (daSet (baDevelop targetArgs)) =
          Left ("error: Cannot unset environment variable '" ++ name ++ "' that is set with '--set-env-var'")
      | flag `elem` unsetFlags = goTarget DevelopsTarget opts (developing targetArgs (\args -> args {daUnset = Set.insert name (daUnset args)})) rest
    goTarget DevelopsTarget opts targetArgs (flag : name : value : rest)
      | flag `elem` setFlags,
        Set.member name (daUnset (baDevelop targetArgs)) =
          Left ("error: Cannot set environment variable '" ++ name ++ "' that is unset with '--unset-env-var'")
      | flag `elem` setFlags,
        Map.member name (daSet (baDevelop targetArgs)) =
          Left ("error: Duplicate definition of environment variable '" ++ name ++ "' with '--set-env-var' is ambiguous")
      | flag `elem` setFlags = goTarget DevelopsTarget opts (developing targetArgs (\args -> args {daSet = Map.insert name value (daSet args)})) rest
    goTarget DevelopsTarget _ _ [flag]
      | flag `elem` developValueFlags = Left (flag ++ " requires a value")
    goTarget DevelopsTarget _ _ (flag : _)
      | flag `elem` setFlags = Left (flag ++ " requires a name and a value")
    goTarget PrintsDevEnv opts targetArgs ("--json" : rest) =
      goTarget PrintsDevEnv opts (targetArgs {baFormat = DevEnvJson}) rest
    goTarget PrintsDevEnv opts targetArgs ("--pretty" : rest) =
      goTarget PrintsDevEnv opts (targetArgs {baJsonLayout = Indented}) rest
    goTarget PrintsDevEnv opts targetArgs ("--no-pretty" : rest) =
      goTarget PrintsDevEnv opts (targetArgs {baJsonLayout = Compact}) rest
    goTarget _ _ _ [flag]
      | flag `elem` valueFlags = Left (flag ++ " requires a value")
    goTarget _ _ _ (flag : _)
      | flag `elem` autoArgFlags = Left (flag ++ autoArgShortfall)
    goTarget kind _ _ (arg@('-' : _) : _) = Left ("unknown " ++ targetWord kind ++ " flag: " ++ arg)
    goTarget kind opts targetArgs (path : rest) =
      withTarget kind opts targetArgs (fileTarget path) rest
    -- A target command evaluates one expression, so a second target is a
    -- mistake worth naming rather than a silent last-one-wins.
    withTarget kind opts targetArgs target rest = case baTarget targetArgs of
      Just _ -> Left (targetWord kind ++ " takes one PATH, <NAME> or --expr, not two")
      Nothing -> goTarget kind opts (targetArgs {baTarget = Just target}) rest
    finishTarget kind opts targetArgs = case (baTarget targetArgs, kind) of
      (Nothing, _) -> Left (targetWord kind ++ " requires a PATH, <NAME> or --expr EXPR")
      (Just target, BuildsTarget) -> Right opts {optCommand = CmdBuild target (baAttrPath targetArgs) (baOutLink targetArgs)}
      (Just target, DevelopsTarget) -> Right opts {optCommand = CmdDevelop target (baAttrPath targetArgs) (baDevelop targetArgs)}
      (Just target, PrintsDevEnv) -> Right opts {optCommand = CmdPrintDevEnv target (baAttrPath targetArgs) (baFormat targetArgs) (baJsonLayout targetArgs)}
      (Just target, TrustsTarget) -> Right opts {optCommand = CmdTrust target (baAttrPath targetArgs)}
      (Just target, AddsToProfile) -> Right opts {optCommand = CmdProfileAdd target (baAttrPath targetArgs) (baProfile targetArgs) (baPriority targetArgs)}
    -- Sub-parser for push: flags and explicit store paths in any order.
    goPush opts pushArgs [] = Right opts {optCommand = CmdPush pushArgs}
    goPush opts _ (flag : _) | Just answer <- answered flag = Right opts {optCommand = answer}
    goPush opts pushArgs ("--store" : dir : rest) =
      goPush (opts {optStore = Just dir}) pushArgs rest
    goPush opts pushArgs ("--cache" : url : rest) =
      goPush opts (pushArgs {paCacheUrl = Just url}) rest
    goPush opts pushArgs ("--key-file" : path : rest) =
      goPush opts (pushArgs {paKeyFile = Just path}) rest
    goPush opts pushArgs ("--secret-key-file" : path : rest) =
      goPush opts (pushArgs {paSecretKeyFile = Just path}) rest
    goPush opts pushArgs ("--compression" : value : rest) =
      goPush opts (pushArgs {paCompressionArg = Just value}) rest
    goPush opts pushArgs ("--all" : rest) =
      goPush opts (pushArgs {paAll = True}) rest
    goPush _ _ [flag]
      | flag `elem` valueFlags = Left (flag ++ " requires a value")
    goPush _ _ (arg@('-' : _) : _) = Left ("unknown push flag: " ++ arg)
    goPush opts pushArgs (path : rest) =
      goPush opts (pushArgs {paPaths = paPaths pushArgs ++ [path]}) rest
    -- Sub-parser for store maintenance verbs.
    goStore _ [] = Left "store: expected a subcommand (delete, gc)"
    goStore opts (flag : _) | Just answer <- answered flag = Right opts {optCommand = answer}
    goStore opts ("delete" : rest) = goStoreDelete opts [] rest
    goStore opts ("gc" : rest) = goStoreGc opts False rest
    goStore _ (sub : _) = Left ("unknown store subcommand: " ++ sub ++ " (expected: delete, gc)")
    goStoreDelete opts paths []
      | null paths = Left "store delete: name at least one store path"
      | otherwise = Right opts {optCommand = CmdStoreDelete paths}
    goStoreDelete opts _ (flag : _) | Just answer <- answered flag = Right opts {optCommand = answer}
    goStoreDelete opts paths ("--store" : dir : rest) =
      goStoreDelete (opts {optStore = Just dir}) paths rest
    goStoreDelete _ _ [flag]
      | flag `elem` valueFlags = Left (flag ++ " requires a value")
    goStoreDelete _ _ (arg@('-' : _) : _) = Left ("unknown store delete flag: " ++ arg)
    goStoreDelete opts paths (path : rest) =
      goStoreDelete opts (paths ++ [path]) rest
    goStoreGc opts printRoots [] = Right opts {optCommand = CmdStoreGc printRoots}
    goStoreGc opts _ (flag : _) | Just answer <- answered flag = Right opts {optCommand = answer}
    goStoreGc opts printRoots ("--store" : dir : rest) =
      goStoreGc (opts {optStore = Just dir}) printRoots rest
    goStoreGc opts _ ("--print-roots" : rest) = goStoreGc opts True rest
    goStoreGc _ _ [flag]
      | flag `elem` valueFlags = Left (flag ++ " requires a value")
    goStoreGc _ _ (arg : _) = Left ("unknown store gc argument: " ++ arg)
    -- Sub-parser for profile verbs.
    goProfile _ [] = Left "profile: expected a subcommand (add, remove, list)"
    goProfile opts (flag : _) | Just answer <- answered flag = Right opts {optCommand = answer}
    goProfile opts ("add" : rest) = goTarget AddsToProfile opts emptyBuildArgs rest
    goProfile opts ("remove" : rest) = goProfileRemove opts DefaultProfile [] [] rest
    goProfile opts ("list" : rest) = goProfileList opts DefaultProfile TextOutput IndentedOnTerminal rest
    goProfile _ (sub : _) = Left ("unknown profile subcommand: " ++ sub ++ " (expected: add, remove, list)")
    -- The flags' matchers come before the arguments', each in the order
    -- given, as upstream runs a flag's handler as it reads the flag and
    -- the arguments' once the line is read.
    goProfileRemove opts profile flagged named [] =
      (\selection -> opts {optCommand = CmdProfileRemove profile selection}) <$> removeSelection (flagged ++ map (Selected . RemoveNamed) named)
    goProfileRemove opts _ _ _ (flag : _) | Just answer <- answered flag = Right opts {optCommand = answer}
    goProfileRemove opts profile flagged named ("--store" : dir : rest) =
      goProfileRemove (opts {optStore = Just dir}) profile flagged named rest
    goProfileRemove opts _ flagged named ("--profile" : path : rest) =
      goProfileRemove opts (ProfileAt path) flagged named rest
    goProfileRemove opts profile flagged named ("--regex" : regex : rest) =
      goProfileRemove opts profile (flagged ++ [Selected (RemoveRegex regex)]) named rest
    goProfileRemove opts profile flagged named ("--all" : rest) =
      goProfileRemove opts profile (flagged ++ [SelectedAll]) named rest
    goProfileRemove _ _ _ _ [flag]
      | flag `elem` valueFlags = Left (flag ++ " requires a value")
    goProfileRemove _ _ _ _ (arg@('-' : _) : _) = Left ("unknown profile remove flag: " ++ arg)
    goProfileRemove opts profile flagged named (element : rest) =
      goProfileRemove opts profile flagged (named ++ [element]) rest
    goProfileList opts profile output layout [] = Right opts {optCommand = CmdProfileList profile (listFormat output layout)}
    goProfileList opts _ _ _ (flag : _) | Just answer <- answered flag = Right opts {optCommand = answer}
    goProfileList opts profile output layout ("--store" : dir : rest) =
      goProfileList (opts {optStore = Just dir}) profile output layout rest
    goProfileList opts _ output layout ("--profile" : path : rest) =
      goProfileList opts (ProfileAt path) output layout rest
    goProfileList opts profile _ layout ("--json" : rest) = goProfileList opts profile JsonOutput layout rest
    -- Each sets the layout, the later winning, and has no effect without
    -- --json, as upstream's flags of these names (common-args.hh:46-72).
    goProfileList opts profile output _ ("--pretty" : rest) = goProfileList opts profile output Indented rest
    goProfileList opts profile output _ ("--no-pretty" : rest) = goProfileList opts profile output Compact rest
    goProfileList _ _ _ _ [flag]
      | flag `elem` valueFlags = Left (flag ++ " requires a value")
    goProfileList _ _ _ _ (arg : _) = Left ("unknown profile list argument: " ++ arg)
    listFormat output layout = case output of
      TextOutput -> ListText
      JsonOutput -> ListJson layout
    -- Upstream's two usage errors, in this parser's words.
    removeSelection selected = case selected of
      [] -> Left "profile remove: no packages specified"
      [SelectedAll] -> Right RemoveAll
      _ -> case traverse selectedMatcher selected of
        Just (first : rest) -> Right (RemoveSome (first :| rest))
        _ -> Left "profile remove: --all cannot be used with package names or regular expressions"
    selectedMatcher entry = case entry of
      Selected matcher -> Just matcher
      SelectedAll -> Nothing
    -- What --help and --version answer.  Every parser takes them where a
    -- flag may stand and leaves the rest of the line unparsed: --version
    -- must report the build even when the command after it is one this
    -- build does not have, and a parser that rejected either would make
    -- 'eval --help' a usage error rather than a request.
    answered "--help" = Just CmdHelp
    answered "--version" = Just CmdVersion
    answered _ = Nothing
    -- Upstream's three flags for the @sandbox@ setting (globals.cc:303-326
    -- at 2.24.9).  Each is registered under the setting's older names too,
    -- and a later registration of a name replaces an earlier one
    -- (args.cc:24-26), so upstream's @--build-use-chroot@ and
    -- @--build-use-sandbox@ are @--relaxed-sandbox@.  Upstream lists no
    -- alias among its flags (args.cc:507), and the reference lists none.
    sandboxFlag "--sandbox" = Just SandboxEnabled
    sandboxFlag "--no-sandbox" = Just SandboxDisabled
    sandboxFlag "--relaxed-sandbox" = Just SandboxRelaxed
    sandboxFlag "--build-use-chroot" = Just SandboxRelaxed
    sandboxFlag "--build-use-sandbox" = Just SandboxRelaxed
    sandboxFlag _ = Nothing
    -- Flags that consume the following argument as their value.
    valueFlags =
      ["--nix-path", "--store", "--substituter", "--trusted-key", "--exec-wrapper", "--expr", "--cache", "--key-file", "--secret-key-file", "--compression", "--profile", "--priority", "--regex"]
        ++ attrFlags
        ++ outLinkFlags
    -- Attribute selection, under both of upstream's spellings.
    attrFlags = ["-A", "--attr"]
    -- The result link, under both of nix-build's spellings.
    outLinkFlags = ["-o", "--out-link"]
    -- No result link, under both of nix-build's spellings.
    noOutLinkFlags = ["--no-out-link", "--no-link"]
    -- develop's flags, under each of upstream's spellings: the short one,
    -- the long one and the name 2.24.9 gave it (command.cc:333-388 and
    -- develop.cc:519-572 at 2.35.4).
    commandFlags = ["-c", "--command"]
    phaseFlags = [("--unpack", "unpack"), ("--configure", "configure"), ("--build", "build"), ("--check", "check"), ("--install", "install"), ("--installcheck", "installCheck")]
    ignoreEnvFlags = ["-i", "--ignore-env", "--ignore-environment"]
    keepFlags = ["-k", "--keep-env-var", "--keep"]
    unsetFlags = ["-u", "--unset-env-var", "--unset"]
    setFlags = ["-s", "--set-env-var"]
    developValueFlags = "--phase" : keepFlags ++ unsetFlags
    developing targetArgs change = targetArgs {baDevelop = change (baDevelop targetArgs)}
    -- The flags that pass a function an argument, each followed by the
    -- argument's name and its value, and what each makes of the value.
    autoArgKinds = [("--arg", AutoArgExpr), ("--argstr", AutoArgString)]
    autoArgFlags = map fst autoArgKinds
    autoArg flag value = ($ value) <$> lookup flag autoArgKinds
    autoArgShortfall = " requires a name and a value"
    withAutoArg name arg opts = opts {optAutoArgs = Map.insert name arg (optAutoArgs opts)}
