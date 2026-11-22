-- | The command line's grammar: the argument vector parsed into the
-- command to run and the flags that shape it.
--
-- What each command and flag means is documented in "Nix.Cli.Reference",
-- which @--help@ prints.  The test suite checks every flag that reference
-- lists against this parser: each command listed for a flag accepts it,
-- and the flag takes a value exactly when its entry names one.  The parser
-- also accepts some flags where they have no effect, such as @--fallback@
-- after @eval@, which the reference does not list.
module Nix.Cli.Args
  ( CliOpts (..),
    Command (..),
    BuildTarget (..),
    PushArgs (..),
    parseArgs,
  )
where

import qualified Data.Text as T

-- | Parsed CLI options.
data CliOpts = CliOpts
  { optNixPaths :: ![T.Text],
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
    -- | @--fallback@: upstream's fallback, turned on for this run.
    optFallback :: !Bool,
    -- | @--check@: the build checks its derivation, as @nix-build --check@
    -- does.
    optCheck :: !Bool,
    optCommand :: !Command
  }
  deriving (Eq, Show)

-- | The command an invocation runs.
data Command
  = CmdEvalFile !FilePath
  | CmdEvalExpr !T.Text
  | CmdBuild !BuildTarget !(Maybe T.Text) !(Maybe FilePath)
  | -- | @trust@: report what the closure of the target's derivation, at
    -- the attribute path when one is given, relies on.
    CmdTrust !BuildTarget !(Maybe T.Text)
  | CmdPush !PushArgs
  | CmdStoreDelete ![String]
  | -- | @store gc@: collect, or with the flag only list the roots.
    CmdStoreGc !Bool
  | -- | No command given.  Usage on stderr, non-zero: a bare invocation is
    -- a usage error, and a caller testing the exit status must see one.
    CmdUsage
  | -- | @--help@.  The same text on stdout, zero: it is a request that
    -- succeeded, and pipeable without redirecting stderr.
    CmdHelp
  | CmdVersion
  deriving (Eq, Show)

-- | Where a build's expression comes from.
data BuildTarget
  = -- | A @.nix@ file.  Relative paths inside it resolve beside the file.
    TargetFile !FilePath
  | -- | An inline expression.  Relative paths inside it resolve against the
    -- working directory, since there is no file to sit beside.
    TargetExpr !T.Text
  deriving (Eq, Show)

-- | A command that evaluates one target to a derivation.
data TargetCommand
  = -- | @build@, which builds it.
    BuildsTarget
  | -- | @trust@, which reports what its closure relies on.
    TrustsTarget

-- | The word that selects a target command.
targetWord :: TargetCommand -> String
targetWord kind = case kind of
  BuildsTarget -> "build"
  TrustsTarget -> "trust"

-- | Arguments to a target command, while the target is still unknown.
data BuildArgs = BuildArgs
  { baTarget :: !(Maybe BuildTarget),
    baAttrPath :: !(Maybe T.Text),
    -- | Where to create the result symlink, registered as a GC root.
    -- Only @build@ takes one.
    baOutLink :: !(Maybe FilePath)
  }

-- | Target command arguments before any flag is parsed.
emptyBuildArgs :: BuildArgs
emptyBuildArgs = BuildArgs Nothing Nothing Nothing

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
parseArgs = go (CliOpts [] False False Nothing Nothing Nothing [] False False False False False CmdUsage)
  where
    go opts [] = Right opts
    go opts (flag : _) | Just answer <- answered flag = Right opts {optCommand = answer}
    go opts ("--nix-path" : val : rest) =
      go (opts {optNixPaths = optNixPaths opts ++ [T.pack val]}) rest
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
    go opts ("--fallback" : rest) =
      go (opts {optFallback = True}) rest
    go opts ("eval" : rest) = goEval opts rest
    go opts ("build" : rest) = goTarget BuildsTarget opts emptyBuildArgs rest
    go opts ("trust" : rest) = goTarget TrustsTarget opts emptyBuildArgs rest
    go opts ("push" : rest) = goPush opts emptyPushArgs rest
    go opts ("store" : rest) = goStore opts rest
    go _ [flag]
      | flag `elem` valueFlags = Left (flag ++ " requires a value")
    go _ (arg : _) = Left ("unknown argument: " ++ arg ++ " (run nova-nix --help for usage)")
    -- Sub-parser for eval: handles --strict and --expr interleaved with the file arg.
    goEval opts [] = Right opts
    goEval opts (flag : _) | Just answer <- answered flag = Right opts {optCommand = answer}
    goEval opts ("--strict" : rest) = goEval (opts {optStrict = True}) rest
    goEval opts ("--aterm" : rest) = goEval (opts {optAterm = True}) rest
    goEval opts ("--nix-path" : val : rest) =
      goEval (opts {optNixPaths = optNixPaths opts ++ [T.pack val]}) rest
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
    goEval opts ("--fallback" : rest) =
      goEval (opts {optFallback = True}) rest
    goEval opts ("--expr" : expr : rest) =
      go (opts {optCommand = CmdEvalExpr (T.pack expr)}) rest
    goEval _ [flag]
      | flag `elem` valueFlags = Left (flag ++ " requires a value")
    goEval _ (arg@('-' : _) : _) = Left ("unknown eval flag: " ++ arg)
    goEval opts (path : rest) =
      go (opts {optCommand = CmdEvalFile path}) rest
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
    goTarget BuildsTarget opts targetArgs ("--exec-wrapper" : spec : rest) =
      goTarget BuildsTarget (opts {optExecWrappers = optExecWrappers opts ++ [spec]}) targetArgs rest
    goTarget kind opts targetArgs ("--restrict-eval" : rest) =
      goTarget kind (opts {optRestrictEval = True}) targetArgs rest
    goTarget kind opts targetArgs ("--pure-eval" : rest) =
      goTarget kind (opts {optPureEval = True}) targetArgs rest
    goTarget kind opts targetArgs ("--trace-verbose" : rest) =
      goTarget kind (opts {optTraceVerbose = True}) targetArgs rest
    goTarget kind opts targetArgs ("--fallback" : rest) =
      goTarget kind (opts {optFallback = True}) targetArgs rest
    goTarget BuildsTarget opts targetArgs ("--check" : rest) =
      goTarget BuildsTarget (opts {optCheck = True}) targetArgs rest
    goTarget kind opts targetArgs ("--expr" : expr : rest) =
      withTarget kind opts targetArgs (TargetExpr (T.pack expr)) rest
    goTarget kind opts targetArgs (flag : path : rest)
      | flag `elem` attrFlags = case baAttrPath targetArgs of
          Just _ -> Left (targetWord kind ++ " accepts one attribute path")
          Nothing -> goTarget kind opts (targetArgs {baAttrPath = Just (T.pack path)}) rest
    goTarget BuildsTarget opts targetArgs (flag : path : rest)
      | flag `elem` outLinkFlags = case baOutLink targetArgs of
          Just _ -> Left "build accepts one --out-link"
          Nothing -> goTarget BuildsTarget opts (targetArgs {baOutLink = Just path}) rest
    goTarget _ _ _ [flag]
      | flag `elem` valueFlags = Left (flag ++ " requires a value")
    goTarget kind _ _ (arg@('-' : _) : _) = Left ("unknown " ++ targetWord kind ++ " flag: " ++ arg)
    goTarget kind opts targetArgs (path : rest) =
      withTarget kind opts targetArgs (TargetFile path) rest
    -- A target command evaluates one expression, so a second target is a
    -- mistake worth naming rather than a silent last-one-wins.
    withTarget kind opts targetArgs target rest = case baTarget targetArgs of
      Just _ -> Left (targetWord kind ++ " takes one FILE.nix or one --expr, not both")
      Nothing -> goTarget kind opts (targetArgs {baTarget = Just target}) rest
    finishTarget kind opts targetArgs = case (baTarget targetArgs, kind) of
      (Nothing, _) -> Left (targetWord kind ++ " requires a FILE.nix argument or --expr EXPR")
      (Just target, BuildsTarget) -> Right opts {optCommand = CmdBuild target (baAttrPath targetArgs) (baOutLink targetArgs)}
      (Just target, TrustsTarget) -> Right opts {optCommand = CmdTrust target (baAttrPath targetArgs)}
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
    -- What --help and --version answer.  Every parser takes them where a
    -- flag may stand and leaves the rest of the line unparsed: --version
    -- must report the build even when the command after it is one this
    -- build does not have, and a parser that rejected either would make
    -- 'eval --help' a usage error rather than a request.
    answered "--help" = Just CmdHelp
    answered "--version" = Just CmdVersion
    answered _ = Nothing
    -- Flags that consume the following argument as their value.
    valueFlags =
      ["--nix-path", "--store", "--substituter", "--trusted-key", "--exec-wrapper", "--expr", "--cache", "--key-file", "--secret-key-file", "--compression"]
        ++ attrFlags
        ++ outLinkFlags
    -- Attribute selection, under both of upstream's spellings.
    attrFlags = ["-A", "--attr"]
    -- The result link, under both of nix-build's spellings.
    outLinkFlags = ["-o", "--out-link"]
