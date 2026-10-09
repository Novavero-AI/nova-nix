{-# LANGUAGE ScopedTypeVariables #-}

-- | nova-nix CLI entry point: parse the argument vector, dispatch one
-- command, map its outcome to an exit code.
--
-- The command and flag tables are deliberately not repeated here.
-- 'usageLines' is the one place they live, and both @--help@ and the
-- usage-error path print it, so the flag that adds itself to the parser
-- documents itself in the same edit.  A second copy in this header went
-- nine flags and two commands stale before anyone noticed, because
-- nothing renders it: Haddock builds library targets by default, and
-- this module is in the executable stanza.
module Main (main) where

import Control.Exception (IOException, displayException, try)
import Control.Monad (join, mfilter, void, (>=>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef (readIORef)
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Version (showVersion)
import Nix.Builder (BuildConfig (..), BuildResult (..), buildWithDeps, defaultBuildConfig, execWrapperConfig)
import Nix.Builtins (builtinEnv, parseNixPath)
import Nix.Config (NixConfig (..))
import qualified Nix.Config as Config
import Nix.Derivation (Derivation (..), DerivationOutput (..), fromATerm)
import Nix.Eval (MonadEval, NixValue (..), Thunk (..), attrSetFromMap, attrSetLookup, attrSetToAscList, attrSetToMap, eval, evaluated, force, readThunkValue)
import Nix.Eval.Arena (arenaInit)
import Nix.Eval.AttrPath (selectAttrPath)
import Nix.Eval.CallDepth (topLevelCallDepth)
import Nix.Eval.IO (EvalState (..), newEvalState, runEvalIO)
import Nix.Eval.Types (bytesToTextLossy, clistFromThunks, clistThunks, thunkToCPtr)
import Nix.Parser (parseNix, readFileAutoEncoding)
import Nix.Push (PushCompression (..), PushConfig (..), PushSummary (..), isDerivationPath, loadApiKeyFile, outputPathsOnly, parsePushCompression, pushCompressionValues, pushPaths, storePathBasename)
import Nix.Store (DeleteOutcome (..), GcRoot (..), LiveSet, Store (..), addOutLinkRoot, canonicalStoreDir, closeStore, collectGarbage, deleteStorePathChecked, findRoots, gcSummaryLine, materializeEvalSources, materializeEvalStoreWrites, openStore, queryAllValidPaths, resolveDeleteTarget, withLiveSet, writeDrv, writeDrvClosure)
import Nix.Store.Path (StoreDir (..), StorePath, defaultStoreDir, parseStorePath, parseStorePathBaseName, platformStoreDir, storePathToFilePath, storePathToText)
import Nix.Substituter (CacheConfig (..))
import Paths_nova_nix (getDataDir, version)
import System.Directory (Permissions (executable), XdgDirectory (XdgConfig), canonicalizePath, doesFileExist, findExecutable, getCurrentDirectory, getPermissions, getTemporaryDirectory, getXdgDirectory)
import System.Environment (getArgs, getExecutablePath, lookupEnv)
import System.Exit (exitFailure)
import System.FilePath (isAbsolute, splitSearchPath, takeDirectory, takeFileName, (</>))
import System.IO (BufferMode (..), hPutStrLn, hSetBuffering, hSetEncoding, stderr, stdout, utf8)
import System.IO.Error (isDoesNotExistError)
import qualified System.Info as SI

-- ---------------------------------------------------------------------------
-- Argument parsing
-- ---------------------------------------------------------------------------

-- | Parsed CLI options.
data CliOpts = CliOpts
  { optNixPaths :: ![T.Text],
    optStrict :: !Bool,
    optAterm :: !Bool,
    -- | Store directory override (default: the platform store).
    optStore :: !(Maybe FilePath),
    -- | Binary cache URL to substitute from before building.
    optSubstituter :: !(Maybe String),
    -- | Trusted public key (@name:base64@) for the substituter.
    optTrustedKey :: !(Maybe String),
    -- | @SYSTEM=PATH@ launchers for derivations this machine cannot execute
    -- directly, e.g. @x86_64-windows=/path/to/wine@.
    optExecWrappers :: ![String],
    optCommand :: !Command
  }

data Command
  = CmdEvalFile !FilePath
  | CmdEvalExpr !T.Text
  | CmdBuild !BuildTarget !(Maybe T.Text) !(Maybe FilePath)
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

-- | Where a build's expression comes from.
data BuildTarget
  = -- | A @.nix@ file.  Relative paths inside it resolve beside the file.
    TargetFile !FilePath
  | -- | An inline expression.  Relative paths inside it resolve against the
    -- working directory, since there is no file to sit beside.
    TargetExpr !T.Text

-- | Arguments to the build command, while the target is still unknown.
data BuildArgs = BuildArgs
  { baTarget :: !(Maybe BuildTarget),
    baAttrPath :: !(Maybe T.Text),
    -- | Where to create the result symlink, registered as a GC root.
    baOutLink :: !(Maybe FilePath)
  }

-- | Build arguments before any flag is parsed.
emptyBuildArgs :: BuildArgs
emptyBuildArgs = BuildArgs Nothing Nothing Nothing

-- | Arguments to the push command.
data PushArgs = PushArgs
  { paCacheUrl :: !(Maybe String),
    paKeyFile :: !(Maybe FilePath),
    paCompressionArg :: !(Maybe String),
    paAll :: !Bool,
    paPaths :: ![String]
  }

-- | Push arguments before any flag is parsed.
emptyPushArgs :: PushArgs
emptyPushArgs = PushArgs Nothing Nothing Nothing False []

-- | Parse the command line.  A malformed invocation is an error, never a
-- silent drop: an unknown or typo'd flag once ended parsing and quietly
-- discarded everything after it (e.g. a requested @--substituter@).
parseArgs :: [String] -> Either String CliOpts
parseArgs = go (CliOpts [] False False Nothing Nothing Nothing [] CmdUsage)
  where
    go opts [] = Right opts
    -- Answered before anything else is looked at, and the rest of the line
    -- is not parsed: --version must report the build even when the command
    -- after it is one this build does not have.
    go opts ("--version" : _) = Right opts {optCommand = CmdVersion}
    go opts ("--help" : _) = Right opts {optCommand = CmdHelp}
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
    go opts ("eval" : rest) = goEval opts rest
    go opts ("build" : rest) = goBuild opts emptyBuildArgs rest
    go opts ("push" : rest) = goPush opts emptyPushArgs rest
    go opts ("store" : rest) = goStore opts rest
    go _ [flag]
      | flag `elem` valueFlags = Left (flag ++ " requires a value")
    go _ (arg : _) = Left ("unknown argument: " ++ arg ++ " (run nova-nix --help for usage)")
    -- Sub-parser for eval: handles --strict and --expr interleaved with the file arg.
    goEval opts [] = Right opts
    goEval opts ("--strict" : rest) = goEval (opts {optStrict = True}) rest
    goEval opts ("--aterm" : rest) = goEval (opts {optAterm = True}) rest
    goEval opts ("--nix-path" : val : rest) =
      goEval (opts {optNixPaths = optNixPaths opts ++ [T.pack val]}) rest
    -- Accepted here as well as at the top level, like build and push:
    -- eval-side reads follow the selected store, so the flag means as
    -- much after the subcommand as before it.
    goEval opts ("--store" : dir : rest) =
      goEval (opts {optStore = Just dir}) rest
    goEval opts ("--expr" : expr : rest) =
      go (opts {optCommand = CmdEvalExpr (T.pack expr)}) rest
    goEval _ [flag]
      | flag `elem` valueFlags = Left (flag ++ " requires a value")
    goEval _ (arg@('-' : _) : _) = Left ("unknown eval flag: " ++ arg)
    goEval opts (path : rest) =
      go (opts {optCommand = CmdEvalFile path}) rest
    -- Sub-parser for build: the target, -A, and the shared flags in any
    -- order.  The shared flags are handled here rather than deferred back to
    -- 'go' so that one can follow the file argument, which is where a caller
    -- reaches for it, and so that -A is still recognised after it.
    goBuild opts buildArgs [] = finishBuild opts buildArgs
    -- Answered here as well as at the top level: a sub-parser that rejected
    -- them would make 'build --help' a usage error rather than a request.
    goBuild opts _ ("--help" : _) = Right opts {optCommand = CmdHelp}
    goBuild opts _ ("--version" : _) = Right opts {optCommand = CmdVersion}
    goBuild opts buildArgs ("--store" : dir : rest) =
      goBuild (opts {optStore = Just dir}) buildArgs rest
    goBuild opts buildArgs ("--substituter" : url : rest) =
      goBuild (opts {optSubstituter = Just url}) buildArgs rest
    goBuild opts buildArgs ("--trusted-key" : key : rest) =
      goBuild (opts {optTrustedKey = Just key}) buildArgs rest
    goBuild opts buildArgs ("--nix-path" : val : rest) =
      goBuild (opts {optNixPaths = optNixPaths opts ++ [T.pack val]}) buildArgs rest
    goBuild opts buildArgs ("--exec-wrapper" : spec : rest) =
      goBuild (opts {optExecWrappers = optExecWrappers opts ++ [spec]}) buildArgs rest
    goBuild opts buildArgs ("--expr" : expr : rest) =
      withTarget opts buildArgs (TargetExpr (T.pack expr)) rest
    goBuild opts buildArgs (flag : path : rest)
      | flag `elem` attrFlags = case baAttrPath buildArgs of
          Just _ -> Left "build accepts one attribute path"
          Nothing -> goBuild opts (buildArgs {baAttrPath = Just (T.pack path)}) rest
      | flag `elem` outLinkFlags = case baOutLink buildArgs of
          Just _ -> Left "build accepts one --out-link"
          Nothing -> goBuild opts (buildArgs {baOutLink = Just path}) rest
    goBuild _ _ [flag]
      | flag `elem` valueFlags = Left (flag ++ " requires a value")
    goBuild _ _ (arg@('-' : _) : _) = Left ("unknown build flag: " ++ arg)
    goBuild opts buildArgs (path : rest) =
      withTarget opts buildArgs (TargetFile path) rest
    -- A build evaluates one expression, so a second target is a mistake
    -- worth naming rather than a silent last-one-wins.
    withTarget opts buildArgs target rest = case baTarget buildArgs of
      Just _ -> Left "build takes one FILE.nix or one --expr, not both"
      Nothing -> goBuild opts (buildArgs {baTarget = Just target}) rest
    finishBuild opts buildArgs = case baTarget buildArgs of
      Nothing -> Left "build requires a FILE.nix argument or --expr EXPR"
      Just target -> Right opts {optCommand = CmdBuild target (baAttrPath buildArgs) (baOutLink buildArgs)}
    -- Sub-parser for push: flags and explicit store paths in any order.
    goPush opts pushArgs [] = Right opts {optCommand = CmdPush pushArgs}
    goPush opts pushArgs ("--store" : dir : rest) =
      goPush (opts {optStore = Just dir}) pushArgs rest
    goPush opts pushArgs ("--cache" : url : rest) =
      goPush opts (pushArgs {paCacheUrl = Just url}) rest
    goPush opts pushArgs ("--key-file" : path : rest) =
      goPush opts (pushArgs {paKeyFile = Just path}) rest
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
    goStore opts ("delete" : rest) = goStoreDelete opts [] rest
    goStore opts ("gc" : rest) = goStoreGc opts False rest
    goStore _ (sub : _) = Left ("unknown store subcommand: " ++ sub ++ " (expected: delete, gc)")
    goStoreDelete opts paths []
      | null paths = Left "store delete: name at least one store path"
      | otherwise = Right opts {optCommand = CmdStoreDelete paths}
    goStoreDelete opts paths ("--store" : dir : rest) =
      goStoreDelete (opts {optStore = Just dir}) paths rest
    goStoreDelete _ _ [flag]
      | flag `elem` valueFlags = Left (flag ++ " requires a value")
    goStoreDelete _ _ (arg@('-' : _) : _) = Left ("unknown store delete flag: " ++ arg)
    goStoreDelete opts paths (path : rest) =
      goStoreDelete opts (paths ++ [path]) rest
    goStoreGc opts printRoots [] = Right opts {optCommand = CmdStoreGc printRoots}
    goStoreGc opts printRoots ("--store" : dir : rest) =
      goStoreGc (opts {optStore = Just dir}) printRoots rest
    goStoreGc opts _ ("--print-roots" : rest) = goStoreGc opts True rest
    goStoreGc _ _ [flag]
      | flag `elem` valueFlags = Left (flag ++ " requires a value")
    goStoreGc _ _ (arg : _) = Left ("unknown store gc argument: " ++ arg)
    -- Flags that consume the following argument as their value.
    valueFlags =
      ["--nix-path", "--store", "--substituter", "--trusted-key", "--exec-wrapper", "--expr", "--cache", "--key-file", "--compression"]
        ++ attrFlags
        ++ outLinkFlags
    -- Attribute selection, under both of upstream's spellings.
    attrFlags = ["-A", "--attr"]
    -- The result link, under both of nix-build's spellings.
    outLinkFlags = ["-o", "--out-link"]

-- | Upstream C++ Nix's name for this directory, so an operator who knows
-- one knows the other.
nixDataDirVar :: String
nixDataDirVar = "NIX_DATA_DIR"

-- | The environment variable carrying inline nix.conf settings, above the
-- config files and below the command line in precedence.
nixConfigVar :: String
nixConfigVar = "NIX_CONFIG"

-- | The directory holding the system nix.conf, in place of @/etc/nix@.
nixConfDirVar :: String
nixConfDirVar = "NIX_CONF_DIR"

-- | The user config files, first strongest, in place of the XDG cascade.
nixUserConfFilesVar :: String
nixUserConfFilesVar = "NIX_USER_CONF_FILES"

-- | The XDG config dirs, searched for @nix\/nix.conf@ after the config home.
xdgConfigDirsVar :: String
xdgConfigDirsVar = "XDG_CONFIG_DIRS"

-- | Windows' all-users application data directory, the machine-wide
-- config directory there.
programDataVar :: String
programDataVar = "ProgramData"

-- | Upstream's @sysconfdir@ on Unix, which libstore's meson.build forces
-- absolute, so @\/etc\/nix@ is the compiled-in system directory.
posixSysconfDir :: FilePath
posixSysconfDir = "/etc"

-- | The XDG base directory spec's default for @XDG_CONFIG_DIRS@, the
-- literal upstream's @getConfigDirs@ falls back to.
posixXdgConfigDirs :: [FilePath]
posixXdgConfigDirs = ["/etc/xdg"]

-- | Where a release archive keeps the bundled expressions, relative to the
-- directory holding @bin@.
bundledDataSubdir :: FilePath
bundledDataSubdir = "share" </> "nova-nix"

-- | The one file a data dir must contain, used to tell a real one from a
-- directory that merely exists.
dataDirMarker :: FilePath
dataDirMarker = "nix" </> "fetchurl.nix"

-- | Locate the bundled @\<nix/*\>@ expressions.
--
-- Cabal bakes an absolute @datadir@ into the binary at configure time.  That
-- is right for a @cabal install@ on this machine and wrong for every copied
-- or downloaded one, because the path names the machine that did the build:
-- a released binary would resolve @\<nix/fetchurl.nix\>@ to a directory that
-- does not exist on the host running it.
--
-- @NIX_DATA_DIR@ wins when set, since an operator setting upstream's own
-- variable means it.  Otherwise a release layout (@bin/@ beside @share/@)
-- answers from the executable's own location, which needs no configuration
-- at all.  The Cabal path remains the fallback, so a local install is
-- unaffected.
resolveDataDir :: IO FilePath
resolveDataDir = do
  fromEnv <- lookupEnv nixDataDirVar
  case fromEnv of
    Just dir | not (null dir) -> pure dir
    _ -> do
      exeDir <- takeDirectory <$> getExecutablePath
      let bundled = takeDirectory exeDir </> bundledDataSubdir
      bundledUsable <- doesFileExist (bundled </> dataDirMarker)
      if bundledUsable then pure bundled else getDataDir

-- | Merge --nix-path entries, bundled data dir, and NIX_PATH search paths.
-- The data dir is appended last so user paths take priority.
mergeSearchPaths :: [T.Text] -> FilePath -> [Thunk] -> [Thunk]
mergeSearchPaths extraPaths dataDir envPaths =
  concatMap parseNixPath extraPaths ++ envPaths ++ parseNixPath (T.pack dataDir)

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  -- UTF-8 on both output handles regardless of the console code page:
  -- locale encodings THROW on any character they cannot represent, so a
  -- store path or eval result containing one would otherwise abort the
  -- whole invocation mid-print on legacy Windows consoles.
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  -- Initialize C data layer (symbol interning, thunk arena, env allocator)
  arenaInit
  args <- getArgs
  dataDir <- resolveDataDir
  opts <- either (failWith . T.pack) pure (parseArgs args)
  config <- loadNixConfig
  storeDir <- chosenStoreDir opts
  case optCommand opts of
    CmdEvalFile filePath -> evalFile config storeDir (optStrict opts) (optNixPaths opts) dataDir filePath
    CmdEvalExpr expr
      | optAterm opts -> evalExprAterm config storeDir (optNixPaths opts) dataDir expr
      | otherwise -> evalExpr config storeDir (optStrict opts) (optNixPaths opts) dataDir expr
    CmdBuild target attrPath outLink -> buildCommand config opts storeDir dataDir target attrPath outLink
    CmdPush pushArgs -> pushCommand storeDir pushArgs
    CmdStoreDelete paths -> storeDeleteCommand storeDir paths
    CmdStoreGc printRoots -> storeGcCommand storeDir printRoots
    CmdUsage -> mapM_ (hPutStrLn stderr) usageLines >> exitFailure
    CmdHelp -> mapM_ putStrLn usageLines
    CmdVersion -> putStrLn versionLine

-- | What @--version@ reports.  Cabal's version, which is the one the publish
-- workflow's tag guard checks a tag against, so a downloaded binary names
-- exactly the release it came from and a bug report can say which build.
versionLine :: String
versionLine = "nova-nix " <> showVersion version

-- | The usage text, returned rather than printed: a bare invocation is a
-- usage error (stderr, non-zero) while @--help@ is a request that succeeded
-- (stdout, zero), and the words are the same either way.
usageLines :: [String]
usageLines =
  [ "Usage: nova-nix [--nix-path NAME=PATH] <command>",
    "",
    "Commands:",
    "  eval FILE.nix          Evaluate a .nix file, print result",
    "  eval --expr 'EXPR'     Evaluate an inline expression",
    "  build FILE.nix         Build a derivation from a .nix file",
    "  build --expr 'EXPR'    Build a derivation from an inline expression",
    "  push --cache URL       Push store paths (and their closures) to a binary cache",
    "  store delete PATH...   Remove store paths, refused while a root or another valid path keeps them",
    "  store gc               Remove every store path not reachable from a root",
    "",
    "Flags:",
    "  --strict               Deep-force all thunks before printing (warning: OOM on large results)",
    "  --aterm                With eval --expr, print the derivation's .drv ATerm",
    "  -A, --attr ATTRPATH    With build: select a dotted attribute path (a.b.c)",
    "  -o, --out-link PATH    With build: create PATH as a symlink to the result and register it as a GC root",
    "  --print-roots          With store gc: list the roots as LINK -> PATH and delete nothing",
    "  --nix-path NAME=PATH   Add search path (repeatable, merged with NIX_PATH)",
    "  --all                  With push: select every valid path except derivations",
    "  --key-file PATH        With push: file holding the cache API key",
    "  --compression KIND     With push: artifact packaging (" <> T.unpack pushCompressionValues <> "; default none)",
    "  --exec-wrapper S=PATH  Run system S's derivations through PATH (repeatable),",
    "                         e.g. --exec-wrapper x86_64-windows=/usr/bin/wine",
    "  --store DIR            Use DIR as the store (default: the platform store)",
    "  --substituter URL      Try this binary cache before building",
    "  --trusted-key K        Public key (name:base64) for the substituter",
    "",
    "  substituters, trusted-public-keys and max-call-depth also read from",
    "  nix.conf, in upstream's order: $NIX_CONF_DIR (default " <> systemConfDirName <> "),",
    "  the XDG nix/nix.conf files or $NIX_USER_CONF_FILES, then $NIX_CONFIG;",
    "  the flags above add to whatever those configure.",
    "",
    "  --help                 Print this text and exit",
    "  --version              Print the version and exit"
  ]

-- | Canonicalize and read a source file.  The canonicalization matters:
-- relative path literals inside the file resolve against the file's
-- directory ('esBaseDir'), and a relative base dir would be re-prefixed on
-- every resolution (doubling it).  An unreadable argument (missing file,
-- a directory, no permission) is a clean CLI error, never an uncaught
-- exception.  Only 'IOException' is caught: an interrupt or any other
-- async exception must abort the run, not print as a read failure.
readSourceFile :: FilePath -> IO (FilePath, T.Text)
readSourceFile rawPath = do
  attempt <- try $ do
    path <- canonicalizePath rawPath
    source <- readFileAutoEncoding path
    pure (path, source)
  case attempt of
    Left (e :: IOException) ->
      failWith ("cannot read " <> T.pack rawPath <> ": " <> T.pack (displayException e))
    Right ok -> pure ok

-- | What a parse error names when the source came from @--expr@ and there is
-- no file to point at.
exprSourceName :: T.Text
exprSourceName = "<expr>"

-- | Evaluate a .nix file and print the result.
evalFile :: NixConfig -> StoreDir -> Bool -> [T.Text] -> FilePath -> FilePath -> IO ()
evalFile config storeDir strict extraPaths dataDir rawFilePath = do
  (filePath, source) <- readSourceFile rawFilePath
  case parseNix (takeDirectory filePath) (T.pack filePath) source of
    Left err -> do
      hPutStrLn stderr ("parse error: " ++ show err)
      exitFailure
    Right expr -> do
      st <- configuredEvalState config storeDir extraPaths dataDir (takeDirectory filePath)
      result <-
        runEvalIO st $
          eval (builtinEnv (esTimestamp st) (esSearchPaths st)) expr >>= finalize strict
      case result of
        Left err -> do
          TIO.hPutStrLn stderr ("error: " <> err)
          exitFailure
        Right forced -> TIO.putStrLn (prettyValue forced)

-- | Evaluate an inline expression and print the result.
evalExpr :: NixConfig -> StoreDir -> Bool -> [T.Text] -> FilePath -> T.Text -> IO ()
evalExpr config storeDir strict extraPaths dataDir source = do
  cwd <- getCurrentDirectory
  case parseNix cwd exprSourceName source of
    Left err -> do
      hPutStrLn stderr ("parse error: " ++ show err)
      exitFailure
    Right expr -> do
      st <- configuredEvalState config storeDir extraPaths dataDir cwd
      result <-
        runEvalIO st $
          eval (builtinEnv (esTimestamp st) (esSearchPaths st)) expr >>= finalize strict
      case result of
        Left err -> do
          TIO.hPutStrLn stderr ("error: " <> err)
          exitFailure
        Right forced -> TIO.putStrLn (prettyValue forced)

-- | Evaluate an inline expression to a derivation and print its ATerm (.drv
-- contents), for diffing nova-nix's serialization against upstream Nix.
evalExprAterm :: NixConfig -> StoreDir -> [T.Text] -> FilePath -> T.Text -> IO ()
evalExprAterm config storeDir extraPaths dataDir source = do
  cwd <- getCurrentDirectory
  case parseNix cwd exprSourceName source of
    Left err -> do
      hPutStrLn stderr ("parse error: " ++ show err)
      exitFailure
    Right expr -> do
      st <- configuredEvalState config storeDir extraPaths dataDir cwd
      result <- runEvalIO st $ do
        val <- eval (builtinEnv (esTimestamp st) (esSearchPaths st)) expr
        forceDerivationAttrs val
        pure val
      case result of
        Left err -> do
          TIO.hPutStrLn stderr ("error: " <> err)
          exitFailure
        Right val -> do
          drvSP <- derivationPath val
          drvClosure <- readIORef (esDrvClosure st)
          aterm <- recordedAterm drvClosure drvSP
          -- The recorded bytes, not a re-serialization: these are what the
          -- path hashes.  BC.putStrLn bypasses the handle encoding, so the
          -- printed .drv diffs byte-exactly against upstream's.
          BC.putStrLn aterm

-- | Parse, evaluate, extract derivation, build, and print result.
-- The file argument is canonicalized for the same reason as in 'evalFile'.
-- | Where a build reads its expression, and the directory relative paths
-- inside it resolve against.
loadBuildSource :: BuildTarget -> IO (FilePath, T.Text, T.Text)
loadBuildSource (TargetFile rawFilePath) = do
  (filePath, source) <- readSourceFile rawFilePath
  pure (takeDirectory filePath, T.pack filePath, source)
loadBuildSource (TargetExpr source) = do
  cwd <- getCurrentDirectory
  pure (cwd, exprSourceName, source)

-- | Force the attributes 'derivationPath' goes on to read.  @derivation@
-- is a lazy wrapper, and 'readThunkValue' answers 'Nothing' for a thunk that
-- was never forced, so skipping this reports a real derivation as not one.
-- Forcing @drvPath@ is also what computes the derivation and records its
-- @.drv@ in the session closure ('recordedAterm').
forceDerivationAttrs :: (MonadEval m) => NixValue -> m ()
forceDerivationAttrs val = case val of
  VAttrs attrs ->
    mapM_
      (\k -> maybe (pure ()) (void . force) (attrSetLookup k attrs))
      derivationAttrKeys
  _ -> pure ()

-- | What a build needs forced: the marker, the path whose closure the
-- build driver writes, and the output it realizes.
derivationAttrKeys :: [T.Text]
derivationAttrKeys = ["type", "drvPath", "outputName"]

buildCommand :: NixConfig -> CliOpts -> StoreDir -> FilePath -> BuildTarget -> Maybe T.Text -> Maybe FilePath -> IO ()
buildCommand config opts storeDir dataDir target attrPath outLink = do
  let caches = resolveCaches config (optSubstituter opts) (optTrustedKey opts)
  wrappers <- either failWith pure (execWrapperConfig (optExecWrappers opts)) >>= checkExecWrappers
  (baseDir, sourceName, source) <- loadBuildSource target
  case parseNix baseDir sourceName source of
    Left err -> do
      hPutStrLn stderr ("parse error: " ++ show err)
      exitFailure
    Right expr -> do
      st <- configuredEvalState config storeDir (optNixPaths opts) dataDir baseDir
      result <- runEvalIO st $ do
        root <- eval (builtinEnv (esTimestamp st) (esSearchPaths st)) expr
        selected <- case attrPath of
          Nothing -> pure (Right root)
          Just path -> selectAttrPath path root
        -- Forced after selection, not before: forcing the root leaves the
        -- selected value's own attributes unforced, and derivationPath
        -- then reports a real derivation as not being one.
        either (pure . Left) (\val -> Right val <$ forceDerivationAttrs val) selected
      case result of
        Left err -> do
          TIO.hPutStrLn stderr ("eval error: " <> err)
          exitFailure
        Right (Left selectionErr) -> do
          TIO.hPutStrLn stderr ("error: " <> selectionErr)
          exitFailure
        Right (Right val) -> do
          drvSP <- derivationPath val
          outputName <- defaultOutputName drvSP val
          -- The full .drv closure (root + every transitive input) recorded
          -- during evaluation; written to the store before building.  The
          -- root's own recipe is read from the same map, so what is built
          -- is what its path hashes.
          drvClosure <- readIORef (esDrvClosure st)
          drv <- recordedDerivation drvClosure drvSP
          -- Resolved before the build, as nix-build resolves the output it
          -- realizes before building anything.
          outputPath <- namedOutputPath drv outputName
          sourceCache <- readIORef (esSourcePathCache st)
          storeWrites <- readIORef (esStoreWriteCache st)
          store <- openStore storeDir
          -- Materialize eval-coerced source paths (src = ./file, path
          -- interpolation): evaluation computes their store paths as text
          -- only - the parity runner's store is not writable - so the build
          -- driver performs the copy and registration.
          materializeEvalSources store sourceCache
          -- builtins.toFile wrote these during evaluation but could not
          -- register them; a derivation naming one needs them valid first.
          materializeEvalStoreWrites store storeWrites
          buildResult <- buildAndRegister store caches wrappers drvClosure drv drvSP
          -- The root is registered before the handle closes: the handle's
          -- lease is what keeps a concurrent collection from running
          -- between the build's end and the link's creation.  The path
          -- prints after the link exists, as nix-build prints it, so a
          -- failed link prints nothing a script would take for success.
          rooted <- case buildResult of
            BuildSuccess _ -> traverse (\link -> addOutLinkRoot store link outputPath) outLink
            BuildFailure _ _ -> pure Nothing
          closeStore store
          case (buildResult, rooted) of
            (BuildFailure msg code, _) -> do
              TIO.hPutStrLn stderr ("build failed (exit " <> T.pack (show code) <> "): " <> msg)
              exitFailure
            (BuildSuccess _, Just (Left err)) -> failWith ("build: " <> err)
            (BuildSuccess _, _) ->
              TIO.putStrLn (T.pack (storePathToFilePath (stDir store) outputPath))

-- | The @.drv@ store path of an evaluated derivation value: a set with
-- @type = "derivation"@ whose @drvPath@ has been forced
-- ('forceDerivationAttrs').  Store paths are ASCII, so the byte payload
-- decodes strictly.
derivationPath :: NixValue -> IO StorePath
derivationPath (VAttrs attrs) = do
  case attrSetLookup "type" attrs of
    Just thunk | Just (VStr "derivation" _) <- readThunkValue thunk -> pure ()
    _ -> failWith "error: result is not a derivation (no type = \"derivation\")"
  case attrSetLookup "drvPath" attrs of
    Just thunk
      | Just (VStr pathBytes _) <- readThunkValue thunk,
        Right path <- TE.decodeUtf8' pathBytes ->
          maybe (failWith ("error: invalid drvPath: " <> path)) pure (parseStorePath defaultStoreDir path)
    _ -> failWith "error: derivation result missing drvPath"
derivationPath _ = failWith "error: result is not a derivation"

-- | The output a build realizes and prints: the value's @outputName@, the
-- first of its @outputs@, which is what upstream's nix-build builds and
-- prints for a derivation (nix-build.cc, @queryOutputName@), with its
-- message for a set that lacks the attribute.
defaultOutputName :: StorePath -> NixValue -> IO T.Text
defaultOutputName drvSP val = case val of
  VAttrs attrs
    | Just thunk <- attrSetLookup "outputName" attrs,
      Just (VStr nameBytes _) <- readThunkValue thunk,
      Right name <- TE.decodeUtf8' nameBytes ->
        pure name
  _ -> failWith ("error: derivation '" <> storePathToText defaultStoreDir drvSP <> "' lacks an 'outputName' attribute")

-- | The path of a named output.  A @.drv@ lists its outputs by name, so
-- the default output is found here by name, never by position.
namedOutputPath :: Derivation -> T.Text -> IO StorePath
namedOutputPath drv outputName =
  case find ((== outputName) . doName) (drvOutputs drv) of
    Just out -> pure (doPath out)
    Nothing -> failWith ("error: derivation has no output named '" <> outputName <> "'")

-- | The @.drv@ ATerm evaluation recorded under a derivation path: the exact
-- bytes whose hash is the path.  Every derivation computed in a session is
-- recorded when its @drvPath@ is forced ('esDrvClosure'), so an absent entry
-- means the path was never computed by this evaluation.
recordedAterm :: Map.Map T.Text BS.ByteString -> StorePath -> IO BS.ByteString
recordedAterm drvClosure drvSP =
  case Map.lookup (storePathToText defaultStoreDir drvSP) drvClosure of
    Just aterm -> pure aterm
    Nothing -> failWith ("error: no .drv was recorded for " <> storePathToText defaultStoreDir drvSP)

-- | The 'Derivation' a recorded ATerm describes ('recordedAterm').
recordedDerivation :: Map.Map T.Text BS.ByteString -> StorePath -> IO Derivation
recordedDerivation drvClosure drvSP = do
  aterm <- recordedAterm drvClosure drvSP
  either (failWith . (("error: the recorded .drv for " <> storePathToText defaultStoreDir drvSP <> " does not parse: ") <>)) pure (fromATerm aterm)

-- | The store directory selected by @--store@, or the platform default,
-- under the canonical spelling 'openStore' keys the store by, so what
-- evaluation and every command print is the spelling the database
-- holds.
chosenStoreDir :: CliOpts -> IO StoreDir
chosenStoreDir opts = canonicalStoreDir (maybe platformStoreDir StoreDir (optStore opts))

-- | Default priority for a config- or CLI-configured substituter
-- (cache.nixos.org is 40).
substituterPriority :: Int
substituterPriority = 50

-- | Turn resolved settings into the cache list.  One 'CacheConfig' per
-- substituter, each carrying the whole trusted-key set: upstream's keys
-- are not bound to a substituter, so a narinfo from any cache is accepted
-- by any trusted key.  A substituter with no trusted key anywhere is not
-- refused here (it simply accepts nothing at the signature gate), matching
-- upstream, where @substituters@ and @trusted-public-keys@ are independent.
configToCaches :: NixConfig -> [CacheConfig]
configToCaches config =
  [ CacheConfig
      { ccUrl = T.dropWhileEnd (== '/') url,
        ccPublicKeys = ncTrustedPublicKeys config,
        ccPriority = substituterPriority
      }
  | url <- ncSubstituters config
  ]

-- | The caches from the resolved config plus the CLI flags.  The CLI
-- @--substituter@ and @--trusted-key@ append on top, the highest
-- precedence, so a flag adds to the configured set rather than being
-- overridden by it.
resolveCaches :: NixConfig -> Maybe String -> Maybe String -> [CacheConfig]
resolveCaches base mUrl mKey =
  configToCaches
    base
      { ncSubstituters = ncSubstituters base ++ maybe [] (\url -> [T.pack url]) mUrl,
        ncTrustedPublicKeys = ncTrustedPublicKeys base ++ maybe [] (\key -> [T.pack key]) mKey
      }

-- | The state a command evaluates under: the store, the directory relative
-- paths resolve against, the search path merged from the flags, the data
-- directory and @NIX_PATH@, and the call-depth ceiling the config cascade
-- resolved.
configuredEvalState :: NixConfig -> StoreDir -> [T.Text] -> FilePath -> FilePath -> IO EvalState
configuredEvalState config storeDir extraPaths dataDir baseDir = do
  st0 <- newEvalState storeDir baseDir
  pure
    st0
      { esSearchPaths = mergeSearchPaths extraPaths dataDir (esSearchPaths st0),
        esCallDepth = topLevelCallDepth (ncMaxCallDepth config)
      }

-- | Resolve nix.conf from the system file, the user-file cascade and
-- @NIX_CONFIG@, the file locations coming from the environment and the
-- platform's XDG defaults.  A file that cannot be read is absent, but one
-- that is read and does not parse is a hard error, so a malformed
-- security-relevant setting cannot pass for no setting.
loadNixConfig :: IO NixConfig
loadNixConfig = do
  locations <- configLocations
  nixConfigEnv <- lookupEnv nixConfigVar
  paths <- either configError pure (Config.configFilePaths locations)
  Config.loadConfig readConfigFile paths (T.pack <$> nixConfigEnv)
    >>= either configError pure
  where
    configError = failWith . ("error: " <>)

-- | Where the config files are on this machine.  @NIX_CONF_DIR@ and
-- @XDG_CONFIG_DIRS@ are honoured on every platform; the defaults behind
-- them are the platform's.  Upstream compiles @\/etc\/nix@ in as the
-- system directory and falls back to @\/etc\/xdg@ for the dirs on every
-- platform, Windows included, where neither names anything (a path with
-- no drive resolves against the current drive).  The Windows analogues
-- are chosen here the way the user file's was (@%APPDATA%@ for the XDG
-- config home, the @directory@ package's own mapping): the system file is
-- @%ProgramData%\\nix\\nix.conf@, the all-users application data directory
-- being where Windows keeps machine-wide application configuration (it is
-- also where @directory@ maps the system-wide XDG dirs), and there is no
-- system file when @%ProgramData%@ is unset; the dirs default to none,
-- because the one Windows directory that answers to them is already the
-- system file, and a file read twice applies its @extra-@ settings twice.
-- The config home comes from @directory@ (@~\/.config@ on Unix,
-- @%APPDATA%@ on Windows, @XDG_CONFIG_HOME@ only when it is absolute); the
-- dirs list is filtered the same way, since 'splitSearchPath' turns an
-- empty POSIX entry into @.@, where upstream's tokenizer drops it, and the
-- XDG spec says a relative entry is ignored.
configLocations :: IO Config.ConfigLocations
configLocations = do
  confDir <- lookupEnv nixConfDirVar
  systemConfDir <- platformSystemConfDir
  userFiles <- lookupEnv nixUserConfFilesVar
  home <- getXdgDirectory XdgConfig ""
  configDirs <- lookupEnv xdgConfigDirsVar
  pure
    Config.ConfigLocations
      { Config.clConfDir = confDir,
        Config.clSystemConfDir = systemConfDir,
        Config.clUserConfFiles = userFiles,
        Config.clConfigHome = home,
        Config.clConfigDirs = maybe platformConfigDirs (filter isAbsolute . splitSearchPath) configDirs
      }

-- | Where the platform's machine-wide config directory comes from, with
-- @nix@ beneath it either way: a fixed path (upstream's @sysconfdir@ on
-- Unix) or an environment variable naming it (@%ProgramData%@ on
-- Windows).  One value serves both the lookup and the help text, so the
-- two cannot disagree.
data SystemConfDirSource = FixedConfDir !FilePath | EnvConfDir !String

platformSystemConfDirSource :: SystemConfDirSource
platformSystemConfDirSource = case SI.os of
  "mingw32" -> EnvConfDir programDataVar
  _ -> FixedConfDir posixSysconfDir

-- | The machine-wide config directory the system file is read from unless
-- @NIX_CONF_DIR@ moves it: upstream's @sysconfdir\/nix@ on Unix;
-- @%ProgramData%\\nix@ on Windows, or none when that variable is unset or
-- empty.
platformSystemConfDir :: IO (Maybe FilePath)
platformSystemConfDir = case platformSystemConfDirSource of
  FixedConfDir dir -> pure (Just (dir </> Config.nixConfDirName))
  EnvConfDir var -> fmap (</> Config.nixConfDirName) . mfilter (not . null) <$> lookupEnv var

-- | 'platformSystemConfDir' as the help text names it: the path on Unix,
-- the variable on Windows.
systemConfDirName :: String
systemConfDirName = case platformSystemConfDirSource of
  FixedConfDir dir -> dir </> Config.nixConfDirName
  EnvConfDir var -> "%" <> var <> "%" </> Config.nixConfDirName

-- | What @XDG_CONFIG_DIRS@ names when it is unset: the spec's default on
-- Unix, nothing on Windows (see 'configLocations').
platformConfigDirs :: [FilePath]
platformConfigDirs = case SI.os of
  "mingw32" -> []
  _ -> posixXdgConfigDirs

-- | A config file's text, or why it could not be read: nothing at the
-- path, or something that will not open (a directory, a permission
-- refusal).  Only 'IOException' is caught: an interrupt must abort the
-- run, not read as an absent file.
readConfigFile :: FilePath -> IO (Either Config.ConfigReadFailure T.Text)
readConfigFile path = do
  result <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
  pure (either (Left . classify) (Right . TE.decodeUtf8Lenient) result)
  where
    classify err
      | isDoesNotExistError err = Config.ConfigFileMissing
      | otherwise = Config.ConfigFileUnreadable

-- | Resolve every launcher before any building starts, so a typo'd path is
-- a configuration error now rather than a build failure after the whole
-- closure has been realized.  A bare name resolves through @PATH@, the way
-- a shell would; anything else has to be an executable file where it says.
checkExecWrappers :: Map.Map T.Text FilePath -> IO (Map.Map T.Text FilePath)
checkExecWrappers = Map.traverseWithKey check
  where
    check system path
      | path == takeFileName path =
          findExecutable path
            >>= maybe (failWith ("--exec-wrapper " <> system <> ": " <> T.pack path <> " is not on PATH")) pure
      | otherwise = do
          there <- doesFileExist path
          if not there
            then failWith ("--exec-wrapper " <> system <> ": " <> T.pack path <> " does not exist")
            else do
              perms <- getPermissions path
              if executable perms
                then pure path
                else failWith ("--exec-wrapper " <> system <> ": " <> T.pack path <> " is not executable")

-- | Write the .drv file to the store and build with dependency resolution.
-- The drvPath is the store path of the .drv file itself, extracted from
-- the evaluation result alongside the Derivation struct.
buildAndRegister :: Store -> [CacheConfig] -> Map.Map T.Text FilePath -> Map.Map T.Text BS.ByteString -> Derivation -> StorePath -> IO BuildResult
buildAndRegister store caches wrappers drvClosure drv drvSP = do
  -- Materialize the full input-.drv closure (every transitive dependency's
  -- recipe) to the store.  buildWithDeps reads these back to construct the
  -- dependency graph; without them it cannot realize any non-leaf derivation.
  writeDrvClosure store drvClosure
  -- Write the root .drv too (idempotent - it is also in the closure) so a build
  -- still works if the closure was not captured (e.g. a pre-built store drv).
  writeDrv store drv drvSP
  -- Build with dependency resolution
  tmpDir <- getTemporaryDirectory
  let config =
        (defaultBuildConfig (stDir store))
          { bcTmpDir = tmpDir,
            bcCaches = caches,
            bcExecWrappers = wrappers
          }
  buildWithDeps config store drv drvSP

-- ---------------------------------------------------------------------------
-- Push command
-- ---------------------------------------------------------------------------

-- | Push the closure of the selected store paths to a binary cache.
pushCommand :: StoreDir -> PushArgs -> IO ()
pushCommand storeDir pushArgs = do
  cacheUrl <- case paCacheUrl pushArgs of
    Just url -> pure (T.dropWhileEnd (== '/') (T.pack url))
    Nothing -> failWith "push: --cache URL is required"
  case (paAll pushArgs, paPaths pushArgs) of
    (True, _ : _) -> failWith "push: --all and explicit paths are mutually exclusive"
    (False, []) -> failWith "push: name store paths to push, or pass --all"
    _ -> pure ()
  apiKey <- case paKeyFile pushArgs of
    Nothing -> pure Nothing
    Just path -> do
      loaded <- loadApiKeyFile path
      either failWith (pure . Just) loaded
  compression <- case paCompressionArg pushArgs of
    Nothing -> pure PushNone
    Just value -> either (failWith . ("push: " <>)) pure (parsePushCompression (T.pack value))
  store <- openStore storeDir
  rootsResult <- resolvePushRoots store pushArgs
  case rootsResult of
    Left err -> do
      closeStore store
      failWith err
    Right roots -> do
      result <- pushPaths (PushConfig cacheUrl apiKey compression) store roots
      closeStore store
      case result of
        Left err -> failWith ("push failed: " <> err)
        Right summary ->
          TIO.putStrLn
            ( "pushed "
                <> T.pack (show (psPushed summary))
                <> " path(s), "
                <> T.pack (show (psSkipped summary))
                <> " already cached"
            )

-- | Delete store paths: registration rows and on-disk trees.  Every
-- argument is resolved before anything is touched; the paths are then
-- deleted in argument order under one collector lock and one live set,
-- with upstream's roots line printed once per invocation as its
-- @--delete@ prints it, and the first failure stops the run, so a
-- reference chain deletes leaf-first in one invocation.
storeDeleteCommand :: StoreDir -> [String] -> IO ()
storeDeleteCommand storeDir rawPaths = do
  targets <- either (failWith . prefixed) pure (traverse (resolveDeleteTarget storeDir . T.pack) rawPaths)
  store <- openStore storeDir
  result <- withLiveSet store (\live -> deleteEach store live targets)
  closeStore store
  either (failWith . prefixed) pure (join result)
  where
    prefixed err = "store delete: " <> err
    deleteEach :: Store -> LiveSet -> [T.Text] -> IO (Either T.Text ())
    deleteEach _ _ [] = pure (Right ())
    deleteEach store live (basename : rest) = do
      outcome <- deleteStorePathChecked store live basename
      case outcome of
        Left err -> pure (Left err)
        Right removed -> do
          TIO.putStrLn ("deleted " <> basename <> describeOutcome removed)
          deleteEach store live rest
    describeOutcome removed
      | doRowRemoved removed && doTreeRemoved removed = ""
      | doRowRemoved removed = " (no tree on disk)"
      | otherwise = " (unregistered tree)"

-- | Collect garbage, or list the roots.  The summary line is upstream's
-- (@PrintFreed@), on stdout; the progress lines come from the library on
-- stderr.  @--print-roots@ lists @LINK -> PATH@ sorted, each once, as
-- @nix-store --gc --print-roots@ does, and deletes nothing; it runs
-- under the handle's own lease, as upstream's listing takes no
-- collector lock, so it does not wait behind a running build.
storeGcCommand :: StoreDir -> Bool -> IO ()
storeGcCommand storeDir printRoots = do
  store <- openStore storeDir
  if printRoots
    then do
      roots <- findRoots store
      closeStore store
      mapM_ (\root -> TIO.putStrLn (T.pack (grLink root) <> " -> " <> grPath root)) roots
    else do
      results <- collectGarbage store
      closeStore store
      either (failWith . ("store gc: " <>)) (TIO.putStrLn . gcSummaryLine) results

-- | Resolve push roots: every valid non-derivation path with @--all@, otherwise
-- each named path.  Named paths may be full store paths in either store-dir
-- form, or a bare @hash-name@ basename.  A named derivation is refused
-- here, with the rule, rather than by the cache's 400 after its NAR has
-- already been uploaded.
resolvePushRoots :: Store -> PushArgs -> IO (Either T.Text [StorePath])
resolvePushRoots store pushArgs
  | paAll pushArgs = do
      pathTexts <- queryAllValidPaths (stDB store)
      pure (outputPathsOnly <$> traverse parseDbPath pathTexts)
  | otherwise = pure (traverse parseArgPath (paPaths pushArgs) >>= refuseDerivations)
  where
    refuseDerivations roots = case filter isDerivationPath roots of
      [] -> Right roots
      drv : _ -> Left ("push: " <> storePathBasename drv <> " is a derivation; a binary cache serves build outputs, not recipes")
    -- DB rows are rendered with the OPENED store's dir - parsing against
    -- platformStoreDir made 'push --all --store DIR' fail on every row of
    -- a non-default store.
    parseDbPath txt =
      maybe (Left ("unparseable store DB path: " <> txt)) Right (parseStorePath (stDir store) txt)
    parseArgPath raw =
      let txt = T.pack raw
          attempts =
            [ parseStorePath (stDir store) txt,
              parseStorePath platformStoreDir txt,
              parseStorePath defaultStoreDir txt,
              -- A bare basename passes the same charset gate as every
              -- other spelling: push targets are real store paths.
              parseStorePathBaseName txt
            ]
       in case catMaybes attempts of
            (sp : _) -> Right sp
            [] -> Left ("not a store path: " <> txt)

-- | Print an error to stderr and exit.
failWith :: T.Text -> IO a
failWith msg = do
  TIO.hPutStrLn stderr msg
  exitFailure

-- ---------------------------------------------------------------------------
-- Output formatting
-- ---------------------------------------------------------------------------

-- | Optionally deep-force a value before printing.
-- With @--strict@, all thunks are recursively materialized.
-- Without it, thunks display as @"thunk"@ - safe for large results.
finalize :: (MonadEval m) => Bool -> NixValue -> m NixValue
finalize True = deepForceValue
finalize False = pure

-- ---------------------------------------------------------------------------
-- Deep-force and pretty-print
-- ---------------------------------------------------------------------------

-- | Recursively force all thunks in a value, returning the fully
-- materialized tree.  Unlike 'deepForce' (which returns @()@), this
-- rebuilds the value with all thunks replaced by computed C thunks.
deepForceValue :: (MonadEval m) => NixValue -> m NixValue
deepForceValue (VList cl) = do
  let thunks = map Thunk (clistThunks cl)
  forced <- mapM (force >=> deepForceValue) thunks
  pure (VList (clistFromThunks (map (thunkToCPtr . evaluated) forced)))
deepForceValue (VAttrs attrs) = do
  let m = attrSetToMap attrs
  forced <- mapM (force >=> deepForceValue) m
  pure (VAttrs (attrSetFromMap (Map.map evaluated forced)))
deepForceValue val = pure val

-- | Nix-style pretty-printing of a fully forced value.
prettyValue :: NixValue -> T.Text
prettyValue (VInt n) = T.pack (show n)
prettyValue (VFloat f) = T.pack (show f)
prettyValue (VBool True) = "true"
prettyValue (VBool False) = "false"
prettyValue VNull = "null"
prettyValue (VStr s _) = "\"" <> escapeNixString (bytesToTextLossy s) <> "\""
prettyValue (VPath p) = p
prettyValue (VList cl) =
  wrapNixSeq "[" "]" (map (prettyThunk . Thunk) (clistThunks cl))
prettyValue (VAttrs attrs) =
  let entries = attrSetToAscList attrs
      rendered = map (\(k, t) -> k <> " = " <> prettyThunk t <> ";") entries
   in wrapNixSeq "{" "}" rendered
prettyValue (VLambda {}) = "<lambda>"
prettyValue (VBuiltin name _) = "<builtin " <> name <> ">"
prettyValue (VCompiledRegex _) = "<compiled-regex>"

-- | Render a bracketed sequence the way upstream prints one: the brackets are
-- separated from the contents by a space, and an empty sequence is @[ ]@ or
-- @{ }@ rather than the two spaces that a bare join of no elements leaves
-- between them.
wrapNixSeq :: T.Text -> T.Text -> [T.Text] -> T.Text
wrapNixSeq open close [] = open <> " " <> close
wrapNixSeq open close parts = open <> " " <> T.intercalate " " parts <> " " <> close

-- | Pretty-print a thunk.  After deep-forcing, all thunks should be
-- computed thunks render their value; pending thunks render as a placeholder.
prettyThunk :: Thunk -> T.Text
prettyThunk thunk = maybe "<thunk>" prettyValue (readThunkValue thunk)

-- | Escape a string for Nix-style output (quotes, backslashes, newlines, tabs, carriage returns).
escapeNixString :: T.Text -> T.Text
escapeNixString = T.concatMap escapeChar
  where
    escapeChar '\\' = "\\\\"
    escapeChar '"' = "\\\""
    escapeChar '\n' = "\\n"
    escapeChar '\t' = "\\t"
    escapeChar '\r' = "\\r"
    escapeChar c = T.singleton c
