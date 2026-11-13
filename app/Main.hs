{-# LANGUAGE ScopedTypeVariables #-}

-- | nova-nix CLI entry point: parse the argument vector, dispatch one
-- command, map its outcome to an exit code.
--
-- The commands and flags are documented in "Nix.Cli.Reference" and
-- parsed by "Nix.Cli.Args", both in the package's @cli@ library, where
-- the test suite reaches them; @--help@ and the usage-error path print the
-- reference's 'helpLines'.
module Main (main) where

import Control.Exception (AsyncException (UserInterrupt), IOException, displayException, handle, handleJust, try)
import Control.Monad (join, mfilter, void, (>=>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import Data.IORef (readIORef)
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Version (showVersion)
import Nix.Builder (BuildConfig (..), BuildResult (..), buildWithDeps, defaultBuildConfig, execWrapperConfig)
import Nix.Builtins (builtinEnv, isNixPathPseudoUrl, parseNixPath, rootScopeNames, searchPathRoots)
import Nix.Cli.Args (BuildTarget (..), CliOpts (..), Command (..), PushArgs (..), parseArgs)
import Nix.Cli.Reference (Host (..), SystemConfDirSource (..), currentHost, helpLines, systemConfDirSource)
import Nix.Config (NixConfig (..))
import qualified Nix.Config as Config
import Nix.Derivation (Derivation (..), DerivationOutput (..), fromATerm)
import Nix.Environment (EnvLookup (..), lookupEnvBytes)
import Nix.Eval (Env, EvalPolicy (..), MonadEval, NixValue (..), Thunk (..), attrSetFromMap, attrSetLookup, attrSetToMap, eval, evaluated, force, readThunkValue)
import Nix.Eval.Arena (arenaInit)
import Nix.Eval.AttrPath (selectAttrPath)
import Nix.Eval.CallDepth (topLevelCallDepth)
import Nix.Eval.CanonPath (canonPathValue)
import Nix.Eval.IO (EvalState (..), allowEvalPath, newEvalState, renderEvalFailure, runEvalIOTraced)
import Nix.Eval.Print (printAmbiguous)
import Nix.Eval.Types (clistFromThunks, clistThunks, thunkToCPtr)
import Nix.Expr.Resolve (undefinedVariableMessage)
import Nix.Expr.Types (Expr)
import Nix.HostPath (hostPathFromBytes)
import Nix.Parser (SourceError (..), SourceOrigin (..), parseNixWithScope)
import Nix.Push (PushCompression (..), PushConfig (..), PushSummary (..), isDerivationPath, loadApiKeyFile, outputPathsOnly, parsePushCompression, pushPaths, storePathBasename)
import Nix.Realise (BuildMode (..), Fallback (..), SubstitutesSetting (..))
import Nix.Store (DeleteOutcome (..), GcRoot (..), LiveSet, Store (..), addOutLinkRoot, canonicalStoreDir, closeStore, collectGarbage, deleteStorePathChecked, findRoots, gcSummaryLine, materializeEvalSources, materializeEvalStoreWrites, openStore, queryAllValidPaths, resolveDeleteTarget, withLiveSet, writeDrv, writeDrvClosure)
import Nix.Store.Path (StoreDir (..), StorePath, defaultStoreDir, parseStorePath, parseStorePathBaseName, platformStoreDir, qualifyPathText, storePathToFilePath, storePathToText)
import Nix.Substituter (CacheConfig (..))
import Paths_nova_nix (getDataDir, version)
import System.Directory (Permissions (executable), canonicalizePath, doesFileExist, findExecutable, getCurrentDirectory, getPermissions, getTemporaryDirectory, makeAbsolute)
import qualified System.Directory.OsPath as OsDir
import System.Environment (getArgs, getExecutablePath, lookupEnv)
import System.Exit (ExitCode (..), exitFailure, exitWith)
import qualified System.File.OsPath as OsFile
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO (BufferMode (..), hPutStrLn, hSetBinaryMode, hSetBuffering, hSetEncoding, stderr, stdout, utf8)
import System.IO.Error (ioeGetErrorString, isDoesNotExistError, isUserError)
import System.OsPath (OsPath)
import qualified System.OsPath as OP

-- ---------------------------------------------------------------------------
-- Where the data and the configuration are
-- ---------------------------------------------------------------------------

-- | Upstream C++ Nix's name for this directory, so an operator who knows
-- one knows the other.
nixDataDirVar :: String
nixDataDirVar = "NIX_DATA_DIR"

-- | The environment variable carrying inline nix.conf settings, above the
-- config files and below the command line in precedence.
nixConfigVar :: BS.ByteString
nixConfigVar = "NIX_CONFIG"

-- | The directory holding the system nix.conf, in place of @/etc/nix@.
nixConfDirVar :: BS.ByteString
nixConfDirVar = "NIX_CONF_DIR"

-- | The user config files, first strongest, in place of the XDG cascade.
nixUserConfFilesVar :: BS.ByteString
nixUserConfFilesVar = "NIX_USER_CONF_FILES"

-- | The XDG config dirs, searched for @nix\/nix.conf@ after the config home.
xdgConfigDirsVar :: BS.ByteString
xdgConfigDirsVar = "XDG_CONFIG_DIRS"

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
  concatMap (parseNixPath . TE.encodeUtf8) extraPaths ++ envPaths ++ parseNixPath (TE.encodeUtf8 (T.pack dataDir))

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  -- UTF-8 on both output handles regardless of the console code page:
  -- locale encodings THROW on any character they cannot represent, so a
  -- store path or message containing one would otherwise abort the whole
  -- invocation mid-print on legacy Windows consoles.  An eval result does
  -- not pass through this: 'printResult' writes it as bytes.
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  -- Initialize C data layer (symbol interning, thunk arena, env allocator)
  arenaInit
  args <- getArgs
  dataDir <- resolveDataDir
  opts <- either (failWith . T.pack) pure (parseArgs args)
  config <- loadNixConfig
  storeDir <- chosenStoreDir opts
  -- SIGINT reaches the main thread as UserInterrupt, which uncaught ends
  -- the process by the signal with nothing said, and an I/O failure no
  -- command handles reaches GHC's default handler, which prints an
  -- "Uncaught exception" report with a backtrace.  Upstream's
  -- handleExceptions logs either as one error line and exits 1
  -- (libmain/shared.cc at 2.24.9): checkInterrupt throws Interrupted, a
  -- BaseError like any other failure.
  handleJust userInterrupt (const reportInterrupt) . handle reportIOFailure $ case optCommand opts of
    CmdEvalFile filePath -> evalFile config opts storeDir dataDir filePath
    CmdEvalExpr expr
      | optAterm opts -> evalExprAterm config opts storeDir dataDir expr
      | otherwise -> evalExpr config opts storeDir dataDir expr
    CmdBuild target attrPath outLink -> buildCommand config opts storeDir dataDir target attrPath outLink
    CmdPush pushArgs -> pushCommand storeDir pushArgs
    CmdStoreDelete paths -> storeDeleteCommand storeDir paths
    CmdStoreGc printRoots -> storeGcCommand storeDir printRoots
    CmdUsage -> mapM_ (TIO.hPutStrLn stderr) helpLines >> exitFailure
    CmdHelp -> mapM_ TIO.putStrLn helpLines
    CmdVersion -> putStrLn versionLine

-- | 'UserInterrupt' alone: every other asynchronous exception keeps
-- propagating.
userInterrupt :: AsyncException -> Maybe ()
userInterrupt UserInterrupt = Just ()
userInterrupt _ = Nothing

reportInterrupt :: IO a
reportInterrupt = do
  hPutStrLn stderr "error: interrupted by the user"
  exitWith (ExitFailure 1)

-- | An I/O failure no command handled, reported as upstream reports an
-- error: @error: @ and the message, then exit status 1.
reportIOFailure :: IOException -> IO a
reportIOFailure failure = do
  hPutStrLn stderr ("error: " <> ioFailureMessage failure)
  exitWith (ExitFailure 1)

-- | A 'userError' is its message alone, which 'show' would wrap in
-- @user error (...)@; any other 'IOException' also names the operation
-- and the file that failed.
ioFailureMessage :: IOException -> String
ioFailureMessage failure
  | isUserError failure = ioeGetErrorString failure
  | otherwise = displayException failure

-- | What @--version@ reports.  Cabal's version, which is the one the publish
-- workflow's tag guard checks a tag against, so a downloaded binary names
-- exactly the release it came from and a bug report can say which build.
versionLine :: String
versionLine = "nova-nix " <> showVersion version

-- | Canonicalize and read a source file.  The canonicalization matters:
-- relative path literals inside the file resolve against the file's
-- directory ('esBaseDir'), and a relative base dir would be re-prefixed on
-- every resolution (doubling it).  An unreadable argument (missing file,
-- a directory, no permission) is a clean CLI error, never an uncaught
-- exception.  Only 'IOException' is caught: an interrupt or any other
-- async exception must abort the run, not print as a read failure.
readSourceFile :: FilePath -> IO (FilePath, BS.ByteString)
readSourceFile rawPath = do
  attempt <- try $ do
    path <- canonicalizePath rawPath
    source <- BS.readFile path
    pure (path, source)
  case attempt of
    Left (e :: IOException) ->
      failWith ("cannot read " <> T.pack rawPath <> ": " <> T.pack (displayException e))
    Right ok -> pure ok

-- | Where @--expr@ source comes from: text with no file to point at, which
-- its parse errors name @<expr>@ and its positions name no file.
exprOrigin :: SourceOrigin
exprOrigin = SourceText "<expr>"

-- | The evaluation policy for this run: the config cascade's settings,
-- with each flag turning its setting on.  A flag only turns a setting on,
-- so a machine configured for pure evaluation stays that way whatever the
-- command line says, which is the direction a confinement should fail in.
evalPolicyFor :: CliOpts -> NixConfig -> EvalPolicy
evalPolicyFor opts config =
  EvalPolicy
    { epRestrictEval = optRestrictEval opts || ncRestrictEval config,
      epPureEval = optPureEval opts || ncPureEval config,
      epAllowedUris = ncAllowedUris config,
      epTraceVerbose = optTraceVerbose opts || ncTraceVerbose config
    }

-- | The evaluation state and root environment for one command.  Upstream's
-- constructor decides the same things in the same order (eval.cc at
-- 2.24.9): under pure-eval the lookup path is empty, so neither
-- @--nix-path@ nor @NIX_PATH@ contributes, and only the bundled @<nix/*>@
-- directory remains, which upstream serves from an in-memory corepkgs
-- filesystem and this binary serves from its data directory (so it stays
-- visible in @builtins.nixPath@, where upstream shows nothing); under
-- either mode every search path root is allowed, as @resolveLookupPathPath@
-- allows the lookup path, so @-I nixpkgs=...@ is what makes a nixpkgs
-- tree readable.
--
-- Each root is allowed under its given spelling and under its resolved
-- one.  The access walk follows symlinks, so a root reached through a
-- symlinked directory (macOS's @/tmp@ is @/private/tmp@) is checked under
-- the resolved spelling; upstream allows only the given one and refuses
-- the read, which is a mismatch between its own allow list and walk, not
-- a rule worth keeping.
setUpEval :: CliOpts -> NixConfig -> StoreDir -> FilePath -> FilePath -> IO (EvalState, Env)
setUpEval opts config storeDir dataDir baseDir = do
  let policy = evalPolicyFor opts config
  st0 <- newEvalState storeDir baseDir
  let searchPaths
        | epPureEval policy = parseNixPath (TE.encodeUtf8 (T.pack dataDir))
        | otherwise = mergeSearchPaths (optNixPaths opts) dataDir (esSearchPaths st0)
      st =
        st0
          { esSearchPaths = searchPaths,
            esPolicy = policy,
            esCallDepth = topLevelCallDepth (ncMaxCallDepth config)
          }
  -- A relative root resolves against the working directory, as upstream's
  -- absPath does, and a rooted one with no drive lands on the store's
  -- drive, where its lookups read ('qualifyPathText').  A pseudo-URL
  -- entry (channel:, flake:, a listed scheme) names a download this
  -- evaluator never performs and allows nothing:
  -- upstream allows such an entry's working-directory spelling only by
  -- falling through after a failed download (resolveLookupPathPath), and
  -- a run that cannot fetch should not widen what it may read.  A root
  -- with no UTF-8 reading allows nothing: allowed paths are text, so no
  -- read can name it, and a lossy spelling would allow a different path.
  roots <- mapM rootSpellings (mapMaybe textRoot (filter (not . isNixPathPseudoUrl) (searchPathRoots searchPaths)))
  mapM_ (allowEvalPath st) (concat roots)
  pure (st, builtinEnv policy (esTimestamp st) searchPaths)
  where
    textRoot = either (const Nothing) Just . TE.decodeUtf8'
    rootSpellings root = do
      given <- makeAbsolute (T.unpack (qualifyPathText storeDir root))
      resolved <- canonicalizePath given
      pure (map (canonPathValue . T.pack) [given, resolved])

-- | Parse and bind source against the names the root environment
-- 'setUpEval' builds will bind, which depend on the policy.
parseForEval :: CliOpts -> NixConfig -> FilePath -> SourceOrigin -> BS.ByteString -> Either SourceError Expr
parseForEval opts config = parseNixWithScope (rootScopeNames (evalPolicyFor opts config))

-- | Evaluate a .nix file and print the result.
evalFile :: NixConfig -> CliOpts -> StoreDir -> FilePath -> FilePath -> IO ()
evalFile config opts storeDir dataDir rawFilePath = do
  (filePath, source) <- readSourceFile rawFilePath
  case parseForEval opts config (takeDirectory filePath) (SourceFile (T.pack filePath)) source of
    Left err -> failSource err
    Right expr -> do
      (st, env) <- setUpEval opts config storeDir dataDir (takeDirectory filePath)
      result <- runEvalIOTraced st (eval env expr >>= finalize (optStrict opts))
      case result of
        Left failure -> do
          TIO.hPutStrLn stderr (renderEvalFailure failure)
          exitFailure
        Right forced -> printResult forced

-- | Evaluate an inline expression and print the result.
evalExpr :: NixConfig -> CliOpts -> StoreDir -> FilePath -> T.Text -> IO ()
evalExpr config opts storeDir dataDir source = do
  cwd <- getCurrentDirectory
  case parseForEval opts config cwd exprOrigin (TE.encodeUtf8 source) of
    Left err -> failSource err
    Right expr -> do
      (st, env) <- setUpEval opts config storeDir dataDir cwd
      result <- runEvalIOTraced st (eval env expr >>= finalize (optStrict opts))
      case result of
        Left failure -> do
          TIO.hPutStrLn stderr (renderEvalFailure failure)
          exitFailure
        Right forced -> printResult forced

-- | Evaluate an inline expression to a derivation and print its ATerm (.drv
-- contents), for diffing nova-nix's serialization against upstream Nix.
evalExprAterm :: NixConfig -> CliOpts -> StoreDir -> FilePath -> T.Text -> IO ()
evalExprAterm config opts storeDir dataDir source = do
  cwd <- getCurrentDirectory
  case parseForEval opts config cwd exprOrigin (TE.encodeUtf8 source) of
    Left err -> failSource err
    Right expr -> do
      (st, env) <- setUpEval opts config storeDir dataDir cwd
      result <- runEvalIOTraced st $ do
        val <- eval env expr
        forceDerivationAttrs val
        pure val
      case result of
        Left failure -> do
          TIO.hPutStrLn stderr (renderEvalFailure failure)
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
loadBuildSource :: BuildTarget -> IO (FilePath, SourceOrigin, BS.ByteString)
loadBuildSource (TargetFile rawFilePath) = do
  (filePath, source) <- readSourceFile rawFilePath
  pure (takeDirectory filePath, SourceFile (T.pack filePath), source)
loadBuildSource (TargetExpr source) = do
  cwd <- getCurrentDirectory
  pure (cwd, exprOrigin, TE.encodeUtf8 source)

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
  (baseDir, origin, source) <- loadBuildSource target
  case parseForEval opts config baseDir origin source of
    Left err -> failSource err
    Right expr -> do
      (st, env) <- setUpEval opts config storeDir dataDir baseDir
      result <- runEvalIOTraced st $ do
        root <- eval env expr
        selected <- case attrPath of
          Nothing -> pure (Right root)
          Just path -> selectAttrPath path root
        -- Forced after selection, not before: forcing the root leaves the
        -- selected value's own attributes unforced, and derivationPath
        -- then reports a real derivation as not being one.
        either (pure . Left) (\val -> Right val <$ forceDerivationAttrs val) selected
      case result of
        Left failure -> do
          TIO.hPutStrLn stderr (renderEvalFailure failure)
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
          buildResult <- buildAndRegister store caches (fallbackFor opts config) (substitutesFor config) (buildModeFor opts) wrappers drvClosure drv drvSP
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
            -- The status printed is the status exited with: 1, or upstream's
            -- 104 for a check that found an output different.
            (BuildFailure msg code, _) -> do
              TIO.hPutStrLn stderr ("build failed (exit " <> T.pack (show code) <> "): " <> msg)
              exitWith (ExitFailure code)
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

-- | What a failed substitute leads to on this run.  The flag only turns
-- fallback on, as upstream's does, so it adds to the config's setting.
fallbackFor :: CliOpts -> NixConfig -> Fallback
fallbackFor opts config
  | optFallback opts || ncFallback config = BuildFromSource
  | otherwise = NoFallback

-- | Whether a derivation's @allowSubstitutes@ attribute is heeded on this
-- run: the config's @always-allow-substitutes@.
substitutesFor :: NixConfig -> SubstitutesSetting
substitutesFor config
  | ncAlwaysAllowSubstitutes config = AlwaysAllowSubstitutes
  | otherwise = HeedAllowSubstitutes

-- | What this run does with the derivation it is asked for: with
-- @--check@ it checks it, as @nix-build --check@ selects upstream's
-- @bmCheck@.  Upstream has no setting for it, so the flag alone decides.
buildModeFor :: CliOpts -> BuildMode
buildModeFor opts
  | optCheck opts = BuildCheck
  | otherwise = BuildNormal

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

-- | Resolve nix.conf from the system file, the user-file cascade and
-- @NIX_CONFIG@, the file locations coming from the environment and the
-- platform's XDG defaults.  A file that cannot be read is absent, but one
-- that is read and does not parse is a hard error, so a malformed
-- security-relevant setting cannot pass for no setting.
loadNixConfig :: IO NixConfig
loadNixConfig = do
  locations <- configLocations
  nixConfigBytes <- configVariable nixConfigVar
  paths <- either configError pure (Config.configFilePaths locations)
  Config.loadConfig readConfigFile paths nixConfigBytes
    >>= either configError pure
  where
    configError = failWith . ("error: " <>)

-- | A variable the configuration reads, as its bytes, the way upstream's
-- @getEnv@ reads it.  A Windows value holding an unpaired UTF-16
-- surrogate has no bytes nova-nix can carry, so it is refused, as
-- @NIX_PATH@ holding one is.
configVariable :: BS.ByteString -> IO (Maybe BS.ByteString)
configVariable name = do
  found <- lookupEnvBytes name
  case found of
    EnvUnset -> pure Nothing
    EnvValue value -> pure (Just value)
    EnvUnpairedSurrogate ->
      failWith ("error: " <> TE.decodeLatin1 name <> " holds an unpaired UTF-16 surrogate, which has no UTF-8 form")

-- | A variable naming a directory or a list of them, as a host path.
configVariablePath :: BS.ByteString -> IO (Maybe OsPath)
configVariablePath name = configVariable name >>= traverse (variableHostPath name)

-- | A variable's bytes as a host path.  Every value 'configVariable'
-- answers has one, since a Windows value arrives as UTF-8, so the
-- refusal is for completeness.
variableHostPath :: BS.ByteString -> BS.ByteString -> IO OsPath
variableHostPath name value =
  maybe (failWith ("error: " <> TE.decodeLatin1 name <> " is not valid UTF-8, so it names no Windows path")) pure (hostPathFromBytes value)

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
  confDir <- configVariable nixConfDirVar
  systemConfDir <- platformSystemConfDir
  userFiles <- configVariable nixUserConfFilesVar
  home <- OsDir.getXdgDirectory OsDir.XdgConfig mempty
  configDirs <- configVariablePath xdgConfigDirsVar
  defaultConfigDirs <- traverse OP.encodeFS platformConfigDirs
  pure
    Config.ConfigLocations
      { Config.clConfDir = confDir,
        Config.clSystemConfDir = systemConfDir,
        Config.clUserConfFiles = userFiles,
        Config.clConfigHome = home,
        Config.clConfigDirs = maybe defaultConfigDirs (filter OP.isAbsolute . OP.splitSearchPath) configDirs
      }

-- | The machine-wide config directory the system file is read from unless
-- @NIX_CONF_DIR@ moves it: upstream's @sysconfdir\/nix@ on Unix;
-- @%ProgramData%\\nix@ on Windows, or none when that variable is unset or
-- empty.
platformSystemConfDir :: IO (Maybe OsPath)
platformSystemConfDir = case systemConfDirSource currentHost of
  FixedConfDir dir -> Just . (OP.</> Config.nixConfDirName) <$> OP.encodeFS dir
  EnvConfDir var -> do
    value <- mfilter (not . BS.null) <$> configVariable var
    traverse (fmap (OP.</> Config.nixConfDirName) . variableHostPath var) value

-- | What @XDG_CONFIG_DIRS@ names when it is unset: the spec's default on
-- Unix, nothing on Windows (see 'configLocations').
platformConfigDirs :: [FilePath]
platformConfigDirs = case currentHost of
  WindowsHost -> []
  PosixHost -> posixXdgConfigDirs

-- | A config file's bytes, or why it could not be read: nothing at the
-- path, or something that will not open (a directory, a permission
-- refusal).  The open takes the 'OsPath' itself, so a POSIX name reaches
-- it as the bytes it was spelled from rather than through the locale's
-- codec.  Only 'IOException' is caught: an interrupt must abort the run,
-- not read as an absent file.
readConfigFile :: OsPath -> IO (Either Config.ConfigReadFailure BS.ByteString)
readConfigFile path = do
  result <- try (OsFile.readFile' path) :: IO (Either IOException BS.ByteString)
  pure (either (Left . classify) Right result)
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
buildAndRegister :: Store -> [CacheConfig] -> Fallback -> SubstitutesSetting -> BuildMode -> Map.Map T.Text FilePath -> Map.Map T.Text BS.ByteString -> Derivation -> StorePath -> IO BuildResult
buildAndRegister store caches fallback substitutes mode wrappers drvClosure drv drvSP = do
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
            bcFallback = fallback,
            bcSubstitutes = substitutes,
            bcMode = mode,
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

-- | Fail on source that does not parse or bind.  An unbound variable is an
-- evaluation error upstream, raised before evaluation starts, and reads as
-- one here.
failSource :: SourceError -> IO a
failSource (SyntaxError err) = failWith ("parse error: " <> T.pack (show err))
failSource (UndefinedVariable name) = failWith ("error: " <> undefinedVariableMessage name)

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

-- | Write an eval result and its newline to stdout as bytes.  A string's
-- payload need not be UTF-8, so nothing on this path decodes or re-encodes
-- it: the handle is put in binary mode, as hPutBuilder asks, and no
-- encoding or newline translation applies.
printResult :: NixValue -> IO ()
printResult val = do
  hSetBinaryMode stdout True
  BB.hPutBuilder stdout (printAmbiguous val <> BB.char7 '\n')
