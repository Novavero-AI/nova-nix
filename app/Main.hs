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
import Control.Monad (void, (>=>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef (readIORef)
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
import Nix.Derivation (Derivation (..), DerivationOutput (..), toATerm)
import Nix.Eval (MonadEval, NixValue (..), Thunk (..), attrSetFromMap, attrSetLookup, attrSetToAscList, attrSetToMap, eval, evaluated, force, readThunkValue)
import Nix.Eval.Arena (arenaInit)
import Nix.Eval.AttrPath (selectAttrPath)
import Nix.Eval.IO (EvalState (..), newEvalState, runEvalIO)
import Nix.Eval.Types (bytesToTextLossy, clistFromThunks, clistThunks, thunkToCPtr)
import Nix.Parser (parseNix, readFileAutoEncoding)
import Nix.Push (PushCompression (..), PushConfig (..), PushSummary (..), loadApiKeyFile, parsePushCompression, pushCompressionValues, pushPaths)
import Nix.Store (DeleteOutcome (..), Store (..), closeStore, deleteStorePathRaw, materializeEvalSources, materializeEvalStoreWrites, openStore, queryAllValidPaths, resolveDeleteTarget, writeDrv, writeDrvClosure)
import Nix.Store.Path (StoreDir (..), StorePath, defaultStoreDir, parseStorePath, parseStorePathBaseName, platformStoreDir, storePathToFilePath)
import Nix.Substituter (CacheConfig (..))
import Paths_nova_nix (getDataDir, version)
import System.Directory (Permissions (executable), XdgDirectory (XdgConfig), canonicalizePath, doesFileExist, findExecutable, getCurrentDirectory, getPermissions, getTemporaryDirectory, getXdgDirectory)
import System.Environment (getArgs, getExecutablePath, lookupEnv)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO (BufferMode (..), hPutStrLn, hSetBuffering, hSetEncoding, stderr, stdout, utf8)

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
  | CmdBuild !BuildTarget !(Maybe T.Text)
  | CmdPush !PushArgs
  | CmdStoreDelete ![String]
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
    baAttrPath :: !(Maybe T.Text)
  }

-- | Build arguments before any flag is parsed.
emptyBuildArgs :: BuildArgs
emptyBuildArgs = BuildArgs Nothing Nothing

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
      Just target -> Right opts {optCommand = CmdBuild target (baAttrPath buildArgs)}
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
    goStore _ [] = Left "store: expected a subcommand (delete)"
    goStore opts ("delete" : rest) = goStoreDelete opts [] rest
    goStore _ (sub : _) = Left ("unknown store subcommand: " ++ sub ++ " (expected: delete)")
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
    -- Flags that consume the following argument as their value.
    valueFlags =
      ["--nix-path", "--store", "--substituter", "--trusted-key", "--exec-wrapper", "--expr", "--cache", "--key-file", "--compression"]
        ++ attrFlags
    -- Attribute selection, under both of upstream's spellings.
    attrFlags = ["-A", "--attr"]

-- | Upstream C++ Nix's name for this directory, so an operator who knows
-- one knows the other.
nixDataDirVar :: String
nixDataDirVar = "NIX_DATA_DIR"

-- | The environment variable carrying inline nix.conf settings, above the
-- config files and below the command line in precedence.
nixConfigVar :: String
nixConfigVar = "NIX_CONFIG"

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
  case optCommand opts of
    CmdEvalFile filePath -> evalFile (chosenStoreDir opts) (optStrict opts) (optNixPaths opts) dataDir filePath
    CmdEvalExpr expr
      | optAterm opts -> evalExprAterm (chosenStoreDir opts) (optNixPaths opts) dataDir expr
      | otherwise -> evalExpr (chosenStoreDir opts) (optStrict opts) (optNixPaths opts) dataDir expr
    CmdBuild target attrPath -> buildCommand opts dataDir target attrPath
    CmdPush pushArgs -> pushCommand opts pushArgs
    CmdStoreDelete paths -> storeDeleteCommand opts paths
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
    "  store delete PATH...   Remove store paths, refused while other valid paths reference them",
    "",
    "Flags:",
    "  --strict               Deep-force all thunks before printing (warning: OOM on large results)",
    "  --aterm                With eval --expr, print the derivation's .drv ATerm",
    "  -A, --attr ATTRPATH    With build: select a dotted attribute path (a.b.c)",
    "  --nix-path NAME=PATH   Add search path (repeatable, merged with NIX_PATH)",
    "  --all                  With push: select every valid path in the store",
    "  --key-file PATH        With push: file holding the cache API key",
    "  --compression KIND     With push: artifact packaging (" <> T.unpack pushCompressionValues <> "; default none)",
    "  --exec-wrapper S=PATH  Run system S's derivations through PATH (repeatable),",
    "                         e.g. --exec-wrapper x86_64-windows=/usr/bin/wine",
    "  --store DIR            Use DIR as the store (default: the platform store)",
    "  --substituter URL      Try this binary cache before building",
    "  --trusted-key K        Public key (name:base64) for the substituter",
    "",
    "  substituters and trusted-public-keys also read from",
    "  $XDG_CONFIG_HOME/nix/nix.conf and $NIX_CONFIG; the flags above",
    "  add to whatever those configure.",
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
evalFile :: StoreDir -> Bool -> [T.Text] -> FilePath -> FilePath -> IO ()
evalFile storeDir strict extraPaths dataDir rawFilePath = do
  (filePath, source) <- readSourceFile rawFilePath
  case parseNix (takeDirectory filePath) (T.pack filePath) source of
    Left err -> do
      hPutStrLn stderr ("parse error: " ++ show err)
      exitFailure
    Right expr -> do
      st0 <- newEvalState storeDir (takeDirectory filePath)
      let searchPaths = mergeSearchPaths extraPaths dataDir (esSearchPaths st0)
          st = st0 {esSearchPaths = searchPaths}
      result <-
        runEvalIO st $
          eval (builtinEnv (esTimestamp st) searchPaths) expr >>= finalize strict
      case result of
        Left err -> do
          TIO.hPutStrLn stderr ("error: " <> err)
          exitFailure
        Right forced -> TIO.putStrLn (prettyValue forced)

-- | Evaluate an inline expression and print the result.
evalExpr :: StoreDir -> Bool -> [T.Text] -> FilePath -> T.Text -> IO ()
evalExpr storeDir strict extraPaths dataDir source = do
  cwd <- getCurrentDirectory
  case parseNix cwd exprSourceName source of
    Left err -> do
      hPutStrLn stderr ("parse error: " ++ show err)
      exitFailure
    Right expr -> do
      st0 <- newEvalState storeDir cwd
      let searchPaths = mergeSearchPaths extraPaths dataDir (esSearchPaths st0)
          st = st0 {esSearchPaths = searchPaths}
      result <-
        runEvalIO st $
          eval (builtinEnv (esTimestamp st) searchPaths) expr >>= finalize strict
      case result of
        Left err -> do
          TIO.hPutStrLn stderr ("error: " <> err)
          exitFailure
        Right forced -> TIO.putStrLn (prettyValue forced)

-- | Evaluate an inline expression to a derivation and print its ATerm (.drv
-- contents), for diffing nova-nix's serialization against upstream Nix.
evalExprAterm :: StoreDir -> [T.Text] -> FilePath -> T.Text -> IO ()
evalExprAterm storeDir extraPaths dataDir source = do
  cwd <- getCurrentDirectory
  case parseNix cwd exprSourceName source of
    Left err -> do
      hPutStrLn stderr ("parse error: " ++ show err)
      exitFailure
    Right expr -> do
      st0 <- newEvalState storeDir cwd
      let searchPaths = mergeSearchPaths extraPaths dataDir (esSearchPaths st0)
          st = st0 {esSearchPaths = searchPaths}
      result <- runEvalIO st $ do
        val <- eval (builtinEnv (esTimestamp st) searchPaths) expr
        forceDerivationAttrs val
        pure val
      case result of
        Left err -> do
          TIO.hPutStrLn stderr ("error: " <> err)
          exitFailure
        Right val -> do
          (drv, _) <- extractDerivation val
          -- Raw ATerm bytes (BC.putStrLn bypasses the handle encoding),
          -- so the printed .drv diffs byte-exactly against upstream's.
          BC.putStrLn (toATerm drv)

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

-- | Force the attributes 'extractDerivation' goes on to read.  @derivation@
-- is a lazy wrapper, and 'readThunkValue' answers 'Nothing' for a thunk that
-- was never forced, so skipping this reports a real derivation as not one.
forceDerivationAttrs :: (MonadEval m) => NixValue -> m ()
forceDerivationAttrs val = case val of
  VAttrs attrs ->
    mapM_
      (\k -> maybe (pure ()) (void . force) (attrSetLookup k attrs))
      derivationAttrKeys
  _ -> pure ()

-- | What a build needs forced: the marker, the wrapper, and the path whose
-- closure the build driver writes.
derivationAttrKeys :: [T.Text]
derivationAttrKeys = ["type", "_derivation", "drvPath"]

buildCommand :: CliOpts -> FilePath -> BuildTarget -> Maybe T.Text -> IO ()
buildCommand opts dataDir target attrPath = do
  let storeDir = chosenStoreDir opts
  configSources <- loadConfigSources
  caches <- either failWith pure (resolveCaches configSources (optSubstituter opts) (optTrustedKey opts))
  wrappers <- either failWith pure (execWrapperConfig (optExecWrappers opts)) >>= checkExecWrappers
  (baseDir, sourceName, source) <- loadBuildSource target
  case parseNix baseDir sourceName source of
    Left err -> do
      hPutStrLn stderr ("parse error: " ++ show err)
      exitFailure
    Right expr -> do
      st0 <- newEvalState storeDir baseDir
      let searchPaths = mergeSearchPaths (optNixPaths opts) dataDir (esSearchPaths st0)
          st = st0 {esSearchPaths = searchPaths}
      result <- runEvalIO st $ do
        root <- eval (builtinEnv (esTimestamp st) searchPaths) expr
        selected <- case attrPath of
          Nothing -> pure (Right root)
          Just path -> selectAttrPath path root
        -- Forced after selection, not before: forcing the root leaves the
        -- selected value's own attributes unforced, and extractDerivation
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
          (drv, drvSP) <- extractDerivation val
          -- The full .drv closure (root + every transitive input) recorded
          -- during evaluation; written to the store before building.
          drvClosure <- readIORef (esDrvClosure st)
          sourceCache <- readIORef (esSourcePathCache st)
          storeWrites <- readIORef (esStoreWriteCache st)
          store <- openStore (chosenStoreDir opts)
          -- Materialize eval-coerced source paths (src = ./file, path
          -- interpolation): evaluation computes their store paths as text
          -- only - the parity runner's store is not writable - so the build
          -- driver performs the copy and registration.
          materializeEvalSources store sourceCache
          -- builtins.toFile wrote these during evaluation but could not
          -- register them; a derivation naming one needs them valid first.
          materializeEvalStoreWrites store storeWrites
          buildResult <- buildAndRegister store caches wrappers drvClosure drv drvSP
          closeStore store
          case buildResult of
            BuildSuccess sp ->
              TIO.putStrLn (T.pack (storePathToFilePath (chosenStoreDir opts) sp))
            BuildFailure msg code -> do
              TIO.hPutStrLn stderr ("build failed (exit " <> T.pack (show code) <> "): " <> msg)
              exitFailure

-- | Extract a Derivation and its store path from an evaluated value.
-- The value must be a VAttrs with type = "derivation", a _derivation key
-- holding the Derivation struct, and a drvPath key holding the .drv store path.
-- Both are computed by builtinDerivation during evaluation.
extractDerivation :: NixValue -> IO (Derivation, StorePath)
extractDerivation (VAttrs attrs) = do
  -- Check type = "derivation"
  case attrSetLookup "type" attrs of
    Just thunk | Just (VStr "derivation" _) <- readThunkValue thunk -> pure ()
    _ -> do
      hPutStrLn stderr "error: result is not a derivation (no type = \"derivation\")"
      exitFailure
  -- Extract the Derivation struct from _derivation
  drv <- case attrSetLookup "_derivation" attrs of
    Just thunk | Just (VDerivation d) <- readThunkValue thunk -> pure d
    _ -> do
      hPutStrLn stderr "error: derivation result missing _derivation field"
      exitFailure
  -- Extract drvPath - this is the store path of the .drv file itself,
  -- computed by hashing the ATerm serialization during evaluation.
  -- Store paths are ASCII, so the byte payload decodes strictly.
  drvSP <- case attrSetLookup "drvPath" attrs of
    Just thunk
      | Just (VStr pathBytes _) <- readThunkValue thunk,
        Right path <- TE.decodeUtf8' pathBytes ->
          case parseStorePath defaultStoreDir path of
            Just sp -> pure sp
            Nothing -> do
              TIO.hPutStrLn stderr ("error: invalid drvPath: " <> path)
              exitFailure
    _ -> do
      hPutStrLn stderr "error: derivation result missing drvPath"
      exitFailure
  pure (drv, drvSP)
extractDerivation _ = do
  hPutStrLn stderr "error: result is not a derivation"
  exitFailure

-- | The store directory selected by @--store@, or the platform default.
chosenStoreDir :: CliOpts -> StoreDir
chosenStoreDir opts = maybe platformStoreDir StoreDir (optStore opts)

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

-- | Resolve the caches from the config sources plus the CLI flags.  The
-- sources are ordered weakest first (user file, then @NIX_CONFIG@); the
-- CLI @--substituter@ and @--trusted-key@ append on top, the highest
-- precedence, so a flag adds to the configured set rather than being
-- overridden by it.
resolveCaches :: [T.Text] -> Maybe String -> Maybe String -> Either T.Text [CacheConfig]
resolveCaches sources mUrl mKey = do
  base <- Config.resolveConfig sources
  let withCli =
        base
          { ncSubstituters = ncSubstituters base ++ maybe [] (\url -> [T.pack url]) mUrl,
            ncTrustedPublicKeys = ncTrustedPublicKeys base ++ maybe [] (\key -> [T.pack key]) mKey
          }
  pure (configToCaches withCli)

-- | The nix.conf sources, weakest first: the user file, then @NIX_CONFIG@.
-- Reading is best effort - a missing or unreadable file is simply absent -
-- but a file that IS read and does not parse is a hard error downstream,
-- so a malformed security-relevant setting cannot pass for no setting.
loadConfigSources :: IO [T.Text]
loadConfigSources = do
  userFile <- readUserConfigFile
  nixConfigEnv <- lookupEnv nixConfigVar
  pure (catMaybes [userFile, T.pack <$> nixConfigEnv])

-- | Read @$XDG_CONFIG_HOME\/nix\/nix.conf@ (the user config file), or
-- 'Nothing' when it is absent or unreadable.
readUserConfigFile :: IO (Maybe T.Text)
readUserConfigFile = do
  dir <- getXdgDirectory XdgConfig "nix"
  let path = dir </> "nix.conf"
  present <- doesFileExist path
  if not present
    then pure Nothing
    else do
      result <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
      pure (either (const Nothing) (Just . TE.decodeUtf8Lenient) result)

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
pushCommand :: CliOpts -> PushArgs -> IO ()
pushCommand opts pushArgs = do
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
  store <- openStore (chosenStoreDir opts)
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

-- | Delete store paths: registration rows and on-disk trees.  Paths are
-- processed in argument order and the first failure stops the run, so a
-- reference chain deletes leaf-first in one invocation.
storeDeleteCommand :: CliOpts -> [String] -> IO ()
storeDeleteCommand opts rawPaths = do
  store <- openStore (chosenStoreDir opts)
  result <- deleteEach store rawPaths
  closeStore store
  either failWith pure result
  where
    deleteEach _ [] = pure (Right ())
    deleteEach store (raw : rest) =
      case resolveDeleteTarget (stDir store) (T.pack raw) of
        Left err -> pure (Left ("store delete: " <> err))
        Right basename -> do
          outcome <- deleteStorePathRaw store basename
          case outcome of
            Left err -> pure (Left ("store delete: " <> err))
            Right removed -> do
              TIO.putStrLn ("deleted " <> basename <> describeOutcome removed)
              deleteEach store rest
    describeOutcome removed
      | doRowRemoved removed && doTreeRemoved removed = ""
      | doRowRemoved removed = " (no tree on disk)"
      | otherwise = " (unregistered tree)"

-- | Resolve push roots: every valid path with @--all@, otherwise each named
-- path.  Named paths may be full store paths in either store-dir form, or a
-- bare @hash-name@ basename.
resolvePushRoots :: Store -> PushArgs -> IO (Either T.Text [StorePath])
resolvePushRoots store pushArgs
  | paAll pushArgs = do
      pathTexts <- queryAllValidPaths (stDB store)
      pure (traverse parseDbPath pathTexts)
  | otherwise = pure (traverse parseArgPath (paPaths pushArgs))
  where
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
prettyValue (VDerivation drv) =
  case drvOutputs drv of
    (out : _) -> "<derivation " <> T.pack (storePathToFilePath platformStoreDir (doPath out)) <> ">"
    [] -> "<derivation>"

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
