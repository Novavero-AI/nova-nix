{-# LANGUAGE ScopedTypeVariables #-}

-- | nova-nix CLI entry point.
--
-- Commands:
--
-- @
-- nova-nix eval  FILE.nix                  Evaluate a .nix file, print result
-- nova-nix eval  --expr 'EXPR'             Evaluate an inline expression
-- nova-nix build FILE.nix                  Build a derivation from a .nix file
-- nova-nix push  --cache URL --all         Push store paths to a binary cache
-- @
--
-- Flags:
--
-- @
-- --nix-path NAME=PATH   Add a search path entry (repeatable, merged with NIX_PATH)
-- --expr EXPR            Evaluate an inline expression instead of a file
-- @
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
import Nix.Builder (BuildConfig (..), BuildResult (..), buildWithDeps, defaultBuildConfig)
import Nix.Builtins (builtinEnv, parseNixPath)
import Nix.Derivation (Derivation (..), DerivationOutput (..), toATerm)
import Nix.Eval (MonadEval, NixValue (..), Thunk (..), attrSetFromMap, attrSetLookup, attrSetToAscList, attrSetToMap, eval, evaluated, force, readThunkValue)
import Nix.Eval.Arena (arenaInit)
import Nix.Eval.IO (EvalState (..), newEvalState, runEvalIO)
import Nix.Eval.Types (bytesToTextLossy, clistFromThunks, clistThunks, thunkToCPtr)
import Nix.Parser (parseNix, readFileAutoEncoding)
import Nix.Push (PushCompression (..), PushConfig (..), PushSummary (..), loadApiKeyFile, parsePushCompression, pushCompressionValues, pushPaths)
import Nix.Store (DeleteOutcome (..), Store (..), closeStore, deleteStorePathRaw, materializeEvalSources, materializeEvalTextPaths, openStore, queryAllValidPaths, resolveDeleteTarget, writeDrv, writeDrvClosure)
import Nix.Store.Path (StoreDir (..), StorePath, defaultStoreDir, parseStorePath, parseStorePathBaseName, platformStoreDir, storePathToFilePath)
import Nix.Substituter (CacheConfig (..))
import Paths_nova_nix (getDataDir)
import System.Directory (canonicalizePath, getCurrentDirectory, getTemporaryDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory)
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
    optCommand :: !Command
  }

data Command
  = CmdEvalFile !FilePath
  | CmdEvalExpr !T.Text
  | CmdBuild !FilePath
  | CmdPush !PushArgs
  | CmdStoreDelete ![String]
  | CmdHelp

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
parseArgs = go (CliOpts [] False False Nothing Nothing Nothing CmdHelp)
  where
    go opts [] = Right opts
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
    go opts ("eval" : rest) = goEval opts rest
    go _ ["build"] = Left "build requires a FILE.nix argument"
    go opts ("build" : path : rest) =
      go (opts {optCommand = CmdBuild path}) rest
    go opts ("push" : rest) = goPush opts emptyPushArgs rest
    go opts ("store" : rest) = goStore opts rest
    go _ [flag]
      | flag `elem` valueFlags = Left (flag ++ " requires a value")
    go _ (arg : _) = Left ("unknown argument: " ++ arg ++ " (run nova-nix with no arguments for usage)")
    -- Sub-parser for eval: handles --strict and --expr interleaved with the file arg.
    goEval opts [] = Right opts
    goEval opts ("--strict" : rest) = goEval (opts {optStrict = True}) rest
    goEval opts ("--aterm" : rest) = goEval (opts {optAterm = True}) rest
    goEval opts ("--nix-path" : val : rest) =
      goEval (opts {optNixPaths = optNixPaths opts ++ [T.pack val]}) rest
    goEval opts ("--expr" : expr : rest) =
      go (opts {optCommand = CmdEvalExpr (T.pack expr)}) rest
    goEval _ [flag]
      | flag `elem` valueFlags = Left (flag ++ " requires a value")
    goEval _ (arg@('-' : _) : _) = Left ("unknown eval flag: " ++ arg)
    goEval opts (path : rest) =
      go (opts {optCommand = CmdEvalFile path}) rest
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
      ["--nix-path", "--store", "--substituter", "--trusted-key", "--expr", "--cache", "--key-file", "--compression"]

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
  dataDir <- getDataDir
  opts <- either (failWith . T.pack) pure (parseArgs args)
  case optCommand opts of
    CmdEvalFile filePath -> evalFile (optStrict opts) (optNixPaths opts) dataDir filePath
    CmdEvalExpr expr
      | optAterm opts -> evalExprAterm (optNixPaths opts) dataDir expr
      | otherwise -> evalExpr (optStrict opts) (optNixPaths opts) dataDir expr
    CmdBuild filePath -> buildFile opts dataDir filePath
    CmdPush pushArgs -> pushCommand opts pushArgs
    CmdStoreDelete paths -> storeDeleteCommand opts paths
    CmdHelp -> do
      hPutStrLn stderr "Usage: nova-nix [--nix-path NAME=PATH] <command>"
      hPutStrLn stderr ""
      hPutStrLn stderr "Commands:"
      hPutStrLn stderr "  eval FILE.nix          Evaluate a .nix file, print result"
      hPutStrLn stderr "  eval --expr 'EXPR'     Evaluate an inline expression"
      hPutStrLn stderr "  build FILE.nix         Build a derivation from a .nix file"
      hPutStrLn stderr "  push --cache URL       Push store paths (and their closures) to a binary cache"
      hPutStrLn stderr "  store delete PATH...   Remove store paths, refused while other valid paths reference them"
      hPutStrLn stderr ""
      hPutStrLn stderr "Flags:"
      hPutStrLn stderr "  --strict               Deep-force all thunks before printing (warning: OOM on large results)"
      hPutStrLn stderr "  --aterm                With eval --expr, print the derivation's .drv ATerm"
      hPutStrLn stderr "  --nix-path NAME=PATH   Add search path (repeatable, merged with NIX_PATH)"
      hPutStrLn stderr "  --all                  With push: select every valid path in the store"
      hPutStrLn stderr "  --key-file PATH        With push: file holding the cache API key"
      hPutStrLn stderr ("  --compression KIND     With push: artifact packaging (" <> T.unpack pushCompressionValues <> "; default none)")
      hPutStrLn stderr "  --store DIR            Use DIR as the store (default: the platform store)"
      hPutStrLn stderr "  --substituter URL      Try this binary cache before building"
      hPutStrLn stderr "  --trusted-key K        Public key (name:base64) for the substituter"
      exitFailure

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

-- | Evaluate a .nix file and print the result.
evalFile :: Bool -> [T.Text] -> FilePath -> FilePath -> IO ()
evalFile strict extraPaths dataDir rawFilePath = do
  (filePath, source) <- readSourceFile rawFilePath
  case parseNix (T.pack filePath) source of
    Left err -> do
      hPutStrLn stderr ("parse error: " ++ show err)
      exitFailure
    Right expr -> do
      st0 <- newEvalState (takeDirectory filePath)
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
evalExpr :: Bool -> [T.Text] -> FilePath -> T.Text -> IO ()
evalExpr strict extraPaths dataDir source = do
  case parseNix "<expr>" source of
    Left err -> do
      hPutStrLn stderr ("parse error: " ++ show err)
      exitFailure
    Right expr -> do
      cwd <- getCurrentDirectory
      st0 <- newEvalState cwd
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
evalExprAterm :: [T.Text] -> FilePath -> T.Text -> IO ()
evalExprAterm extraPaths dataDir source =
  case parseNix "<expr>" source of
    Left err -> do
      hPutStrLn stderr ("parse error: " ++ show err)
      exitFailure
    Right expr -> do
      cwd <- getCurrentDirectory
      st0 <- newEvalState cwd
      let searchPaths = mergeSearchPaths extraPaths dataDir (esSearchPaths st0)
          st = st0 {esSearchPaths = searchPaths}
      result <- runEvalIO st $ do
        val <- eval (builtinEnv (esTimestamp st) searchPaths) expr
        case val of
          VAttrs attrs ->
            mapM_
              (\k -> maybe (pure ()) (void . force) (attrSetLookup k attrs))
              ["type", "_derivation", "drvPath"]
          _ -> pure ()
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
buildFile :: CliOpts -> FilePath -> FilePath -> IO ()
buildFile opts dataDir rawFilePath = do
  caches <- either failWith pure (substituterConfig (optSubstituter opts) (optTrustedKey opts))
  (filePath, source) <- readSourceFile rawFilePath
  case parseNix (T.pack filePath) source of
    Left err -> do
      hPutStrLn stderr ("parse error: " ++ show err)
      exitFailure
    Right expr -> do
      st0 <- newEvalState (takeDirectory filePath)
      let searchPaths = mergeSearchPaths (optNixPaths opts) dataDir (esSearchPaths st0)
          st = st0 {esSearchPaths = searchPaths}
      result <- runEvalIO st $ do
        val <- eval (builtinEnv (esTimestamp st) searchPaths) expr
        -- 'derivation' is a lazy wrapper now; force the attrs extractDerivation
        -- reads (a build legitimately needs the drvPath + closure).
        case val of
          VAttrs attrs ->
            mapM_
              (\k -> maybe (pure ()) (void . force) (attrSetLookup k attrs))
              ["type", "_derivation", "drvPath"]
          _ -> pure ()
        pure val
      case result of
        Left err -> do
          TIO.hPutStrLn stderr ("eval error: " <> err)
          exitFailure
        Right val -> do
          (drv, drvSP) <- extractDerivation val
          -- The full .drv closure (root + every transitive input) recorded
          -- during evaluation; written to the store before building.
          drvClosure <- readIORef (esDrvClosure st)
          sourceCache <- readIORef (esSourcePathCache st)
          textPaths <- readIORef (esTextPathCache st)
          store <- openStore (chosenStoreDir opts)
          -- Materialize eval-coerced source paths (src = ./file, path
          -- interpolation): evaluation computes their store paths as text
          -- only - the parity runner's store is not writable - so the build
          -- driver performs the copy and registration.
          materializeEvalSources store sourceCache
          -- builtins.toFile wrote these during evaluation but could not
          -- register them; a derivation naming one needs them valid first.
          materializeEvalTextPaths store textPaths
          buildResult <- buildAndRegister store caches drvClosure drv drvSP
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

-- | Default priority for a CLI-configured substituter (cache.nixos.org is 40).
substituterPriority :: Int
substituterPriority = 50

-- | Build the cache list from @--substituter@\/@--trusted-key@.  Both or
-- neither: a substituter without a trusted key would skip signature
-- verification, and a key without a substituter is a mistake.
substituterConfig :: Maybe String -> Maybe String -> Either T.Text [CacheConfig]
substituterConfig Nothing Nothing = Right []
substituterConfig Nothing (Just _) = Left "--trusted-key requires --substituter"
substituterConfig (Just _) Nothing = Left "--substituter requires --trusted-key (name:base64)"
substituterConfig (Just url) (Just key) =
  Right
    [ CacheConfig
        { ccUrl = T.dropWhileEnd (== '/') (T.pack url),
          ccPublicKey = T.pack key,
          ccPriority = substituterPriority
        }
    ]

-- | Write the .drv file to the store and build with dependency resolution.
-- The drvPath is the store path of the .drv file itself, extracted from
-- the evaluation result alongside the Derivation struct.
buildAndRegister :: Store -> [CacheConfig] -> Map.Map T.Text BS.ByteString -> Derivation -> StorePath -> IO BuildResult
buildAndRegister store caches drvClosure drv drvSP = do
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
            bcCaches = caches
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
  "[ " <> T.intercalate " " (map (prettyThunk . Thunk) (clistThunks cl)) <> " ]"
prettyValue (VAttrs attrs) =
  let entries = attrSetToAscList attrs
      rendered = map (\(k, t) -> k <> " = " <> prettyThunk t <> ";") entries
   in "{ " <> T.intercalate " " rendered <> " }"
prettyValue (VLambda {}) = "<lambda>"
prettyValue (VBuiltin name _) = "<builtin " <> name <> ">"
prettyValue (VCompiledRegex _) = "<compiled-regex>"
prettyValue (VDerivation drv) =
  case drvOutputs drv of
    (out : _) -> "<derivation " <> T.pack (storePathToFilePath platformStoreDir (doPath out)) <> ">"
    [] -> "<derivation>"

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
