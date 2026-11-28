{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | nova-nix CLI entry point: parse the argument vector, dispatch one
-- command, map its outcome to an exit code.
--
-- The commands and flags are documented in "Nix.Cli.Reference" and
-- parsed by "Nix.Cli.Args", both in the package's @cli@ library, where
-- the test suite reaches them; @--help@ and the usage-error path print the
-- reference's 'helpLines'.
module Main (main) where

import Control.Applicative ((<|>))
import Control.Exception (AsyncException (UserInterrupt), bracket, bracketOnError, displayException, handle, handleJust, try)
import Control.Monad (filterM, foldM_, mfilter, void, when, (>=>))
import Control.Monad.Except (ExceptT (..), runExceptT)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import Data.Char (isDigit)
import Data.Functor.Identity (Identity (..))
import Data.IORef (readIORef)
import Data.Int (Int32)
import Data.List (find, intercalate, partition, sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Version (showVersion)
import Data.Void (absurd)
import Data.Word (Word32, Word64)
import Foreign.C.Error (Errno (..), ePIPE)
import GHC.IO.Exception (IOException (..))
import GHC.Profiling (requestHeapCensus)
import Nix.Builder (BuildConfig (..), BuildResult (..), BuildStop (..), SubstitutionAccess (..), buildWithDeps, defaultBuildConfig, execWrapperConfig, substituteFromCaches)
import Nix.Builder.BuildDir (FailedBuilds (..))
import Nix.Builder.Buildenv (Activity (..))
import qualified Nix.Builder.Cores as Cores
import Nix.Builder.Sandbox (DropGroups (..), SandboxConfig (..), SandboxFallback (..), SandboxMode (..), ViolationLog (..), darwinImpureHostPrefixes, darwinSandboxPaths, parseSandboxPaths)
import Nix.Builder.Sandbox.Spawn (filePathBytes, processIsRoot)
import Nix.Builtins (isNixPathPseudoUrl, parseNixPath, rootScopeNames, searchPathRoots)
import Nix.Cli.Args (AutoArg (..), CliOpts (..), Command (..), DevEnvFormat (..), DevelopArgs (..), JsonLayout (..), ListFormat (..), OutLink, ProfileSelection (..), PushArgs (..), RemoveMatcher (..), RemoveSelection (..), Target (..), outLinkFor, parseArgs)
import Nix.Cli.Reference (Host (..), SystemConfDirSource (..), currentHost, helpLines, renderIndentedJson, systemConfDirSource)
import Nix.Config (DefaultUse (..), NixConfig (..), PathSetValue (..))
import qualified Nix.Config as Config
import Nix.DependencyGraph (DepNode (..), buildDepGraph)
import Nix.Derivation (Derivation (..), DerivationOutput (..), DrvFile, drvFileATerm, drvFileDerivation, drvFilePath, fromATerm)
import Nix.Develop.Capture (CaptureFailure, OutputObject (..), captureDerivation, captureFailureMessage, environmentOutput, getEnvScriptContents, getEnvScriptName, getEnvScriptPath, noEnvironmentMessage)
import Nix.Develop.Environment (BuildEnvironment, EnvironmentScript (..), Prompt (..), ScriptDirectories (..), ScriptFile (..), Shell (..), buildEnvironmentJson, environmentScript, parseBuildEnvironment, renderRcfile)
import Nix.Develop.Shell (DevelopHost (..), EnvironmentChanges (..), bashProgram, developHost, shellArguments, shellEnvironment, shellModeFor)
import Nix.Environment (EnvLookup (..), argumentBytes, lookupEnvBytes)
import Nix.Eval (AttrSet, Env, EvalPolicy (..), MonadEval (..), NixValue (..), Thunk (..), attrSetElems, attrSetLookup, compileSource, deepForce, evalBytecode, evalParsed, evaluated, findInSearchPath, force, inputModuloHashes, readThunkValue, typeName)
import Nix.Eval.AttrPath (selectAttrPath)
import Nix.Eval.AutoCall (AutoArgs, autoCallFunction)
import Nix.Eval.CRegion (Regions (..), regionFigures, renderRegionFigures)
import Nix.Eval.CRoots (crootsAddThunk)
import Nix.Eval.CStats (CRoot (..), countSites, renderCensus, takeCensus, valueRoots)
import Nix.Eval.CallDepth (topLevelCallDepth)
import Nix.Eval.CanonPath (canonPathValue)
import Nix.Eval.IO (EvalFailure (..), EvalIO, EvalState (..), SessionSettings (..), SessionStore (..), allowEvalPath, evalBaseEnv, evalSearchPath, nixPathValue, readImportTarget, renderEvalFailure, resolveImportTarget, runEvalIOTraced, sourceCopies, withEvalSession)
import Nix.Eval.Policy (ImportFromDerivation (..))
import Nix.Eval.Print (printAmbiguous)
import Nix.Eval.Symbol (symbolIntern)
import Nix.Eval.Types (clistThunks, mkStrBytes, mkThunkBc)
import Nix.Expr.Resolve (undefinedVariableMessage)
import Nix.HostPath (hostPathFromBytes, hostPathText)
import Nix.Http (defaultFetchRetryPolicy)
import Nix.Json (Json, renderJson)
import Nix.Logger (Logger, escapeFilterFor, handleLogger)
import Nix.Parser (ParsedSource, SourceError (..), SourceOrigin (..), parseNixWithScope)
import Nix.Profile.Generation (Change (..), Changes (..), DatedGeneration (..), Generation (..), GenerationNumber (..), ProfileFailure (..), ProfileLinkPlace (..), RollbackTarget (..), WipeScope (..), absolutePath, checkProfileStore, currentEpochSeconds, cutoffBefore, findGenerations, generationDate, parseOlderThan, readGenerationNumber, readManifest, readProfileEnvironment, resolveDefaultProfile, switchGeneration, updateProfile, wipeHistory)
import Nix.Profile.Manifest (Conflict (..), ConflictSide (..), Element (..), FlakeSource (..), Manifest (..), Matcher (..), Selection (..), Selector (..), addElement, changeLine, defaultPriority, elementIdentifier, emptyManifest, flakeAttribute, manifestChanges, manifestErrorText, manifestJson, regexMatcher, selectElements)
import Nix.Profile.Tree (buildEnvFailureText)
import Nix.Push (PushCompression (..), PushConfig (..), PushSummary (..), defaultPartSize, isDerivationPath, loadKeyFile, outputPathsOnly, parsePushCompression, pushPaths, storePathBasename)
import Nix.Push.Directory (DirectoryPush (..), PushTarget (..), loadSecretKeyFile, pushTarget, pushToDirectory)
import Nix.Realise (BuildMode (..), ClosureOutcome (..), Fallback (..), SubstitutesSetting (..))
import Nix.Store (DeleteOutcome (..), GcRoot (..), Store (..), UpstreamHazard (..), canonicalStoreDir, closeStore, collectGarbage, deleteStorePaths, findRoots, gcSummaryLine, isValid, openStore, queryAllValidPaths, resolveDeleteTarget, storeEntryNameText, upstreamManagedRefusal, useStoreFileMask)
import Nix.Store.Ops (EvalObjects (..), StoreOps (..), StoreQueries (..), localQueries, withLocalStore)
import Nix.Store.Path (StoreDir (..), StorePath (spName), badStorePathText, defaultStoreDir, parseStorePath, parseStorePathBaseName, parseStorePathPrefix, platformStoreDir, qualifyPathText, storePathToFilePath, storePathToText)
import Nix.Substituter (CacheConfig (..))
import Nix.Trust (ContentFacts, fetchedByBuiltin, fixedOutputInputs, renderTrustReport, trustReport)
import Nix.Trust.Scan (scanFetched)
import qualified NovaCache.NAR as NAR
import Paths_nova_nix (getBinDir, getDataDir, version)
import System.Directory (Permissions (executable), canonicalizePath, doesDirectoryExist, doesFileExist, findExecutable, getCurrentDirectory, getFileSize, getPermissions, makeAbsolute, pathIsSymbolicLink)
import qualified System.Directory.OsPath as OsDir
import System.Environment (getArgs, getExecutablePath, lookupEnv)
import System.Exit (ExitCode (..), exitFailure, exitWith)
import qualified System.File.OsPath as OsFile
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO (BufferMode (..), hIsTerminalDevice, hPutStrLn, hSetBinaryMode, hSetBuffering, hSetEncoding, stderr, stdout, utf8)
import System.IO.Error (ioeGetErrorString, isDoesNotExistError, isUserError)
import qualified System.Info
import System.Mem (performMajorGC)
import System.OsPath (OsPath)
import qualified System.OsPath as OP
import Text.Read (readMaybe)

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

-- | The directory holding the system nix.conf, in place of
-- 'platformSystemConfDir'.
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
-- @NIX_DATA_DIR@ wins when set and not empty, since an operator setting
-- upstream's own variable means it.  Otherwise a release layout (@bin/@ beside
-- @share/@) answers from the executable's own location, which needs no
-- configuration at all.  The Cabal path remains the fallback, so a local
-- install is unaffected.
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

-- | Merge --nix-path entries, NIX_PATH's (its bytes) and the bundled data
-- dir, parsed in that order.  The data dir is appended last so user paths
-- take priority.
mergeSearchPaths :: [T.Text] -> FilePath -> BS.ByteString -> IO [Thunk]
mergeSearchPaths extraPaths dataDir nixPath = do
  extra <- concat <$> mapM (parseNixPath . TE.encodeUtf8) extraPaths
  envPaths <- parseNixPath nixPath
  bundled <- parseNixPath (TE.encodeUtf8 (T.pack dataDir))
  pure (extra ++ envPaths ++ bundled)

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  useStoreFileMask
  hSetBuffering stdout LineBuffering
  -- UTF-8 on both output handles regardless of the console code page:
  -- locale encodings THROW on any character they cannot represent, so a
  -- store path or message containing one would otherwise abort the whole
  -- invocation mid-print on legacy Windows consoles.  An eval result does
  -- not pass through this: 'printResult' writes it as bytes.
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  -- Counting by site begins before anything is allocated, so the census
  -- 'writeCensus' takes covers every environment and attribute set.
  wantsCensus <- isJust <$> censusTarget
  when wantsCensus countSites
  args <- getArgs
  dataDir <- resolveDataDir
  opts <- either (failWith . T.pack) pure (parseArgs args)
  config <- loadNixConfig
  storeDir <- chosenStoreDir opts
  -- SIGINT reaches the main thread as UserInterrupt, which uncaught ends
  -- the process by the signal with nothing said, and an I/O failure no
  -- command handles, or a walk of a tree on disk that failed to read an
  -- entry, reaches GHC's default handler, which prints an "Uncaught
  -- exception" report with a backtrace.  Upstream's handleExceptions logs
  -- each as one error line and exits 1 (libmain/shared.cc at 2.24.9):
  -- checkInterrupt throws Interrupted, a BaseError like any other failure.
  handleJust userInterrupt (const reportInterrupt) . handle reportIOFailure . handle reportWalkFailure $ case optCommand opts of
    CmdEval target
      | optAterm opts -> evalAterm config opts storeDir dataDir target
      | otherwise -> evalCommand config opts storeDir dataDir target
    CmdBuild target attrPath outLink -> buildCommand config opts storeDir dataDir target attrPath outLink
    CmdDevelop target attrPath developArgs -> developCommand config opts storeDir dataDir target attrPath developArgs
    CmdPrintDevEnv target attrPath format layout -> printDevEnvCommand config opts storeDir dataDir target attrPath format layout
    CmdTrust target attrPath -> trustCommand config opts storeDir dataDir target attrPath
    CmdPush pushArgs -> pushCommand storeDir pushArgs
    CmdStoreDelete paths -> storeDeleteCommand storeDir paths
    CmdStoreGc printRoots -> storeGcCommand storeDir printRoots
    CmdProfileAdd target attrPath selection priority -> profileAddCommand config opts storeDir dataDir target attrPath selection priority
    CmdProfileRemove selection removals -> profileRemoveCommand config storeDir selection removals
    CmdProfileList selection format -> profileListCommand config storeDir selection format
    CmdProfileHistory selection -> profileHistoryCommand config storeDir selection
    CmdProfileRollback selection to changes -> profileRollbackCommand config storeDir selection to changes
    CmdProfileWipeHistory selection olderThan changes -> profileWipeHistoryCommand config storeDir selection olderThan changes
    CmdUsage -> mapM_ (TIO.hPutStrLn stderr) helpLines >> exitFailure
    CmdHelp -> toStdout (mapM_ TIO.putStrLn helpLines)
    CmdVersion -> toStdout (putStrLn versionLine)

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

-- | A walk of a tree on disk that no command handled, reported as
-- 'reportIOFailure' reports an I/O failure: the entry, the access that
-- failed on it, and the 'IOException' it raised.
reportWalkFailure :: NAR.NarWalkError -> IO a
reportWalkFailure failure = do
  hPutStrLn stderr ("error: " <> displayException failure)
  exitWith (ExitFailure 1)

-- | Run a write to stdout, dropping what is left of it once stdout's
-- reader has gone ('readerGone'), as a write through upstream's
-- @std::cout@ is dropped: upstream's main thread blocks SIGPIPE
-- (@unix::startSignalHandlerThread@ in libutil\/unix\/signals.cc), so the
-- write fails with EPIPE, which only sets the stream's error state, and the
-- command goes on to its own exit status (@processExpr@ in
-- nix-instantiate.cc).  A command that writes as it works, as @store delete@
-- does, still does all of its work.  GHC's flush of stdout at exit meets the
-- same failure when the write left bytes in the buffer, and passes it to the
-- finalizer exception handler; this program sets none, and the default
-- ignores it (@flushStdHandles@ in ghc-internal at 9.14.1).
toStdout :: IO () -> IO ()
toStdout = handleJust readerGone pure

-- | A write to stdout that failed because no process holds the read end
-- of the pipe it writes to: EPIPE on stdout, the one failure GHC's own
-- top-level handler also ends quietly.  GHC on Windows reports a write to
-- a pipe being closed as EPIPE too (@blockingWriteRawBufferPtr@ in
-- ghc-internal at 9.14.1).
readerGone :: IOException -> Maybe ()
readerGone failure
  | fmap Errno (ioe_errno failure) == Just ePIPE && ioe_handle failure == Just stdout = Just ()
  | otherwise = Nothing

-- | The logger the library reports to: each message and each line a
-- builder writes on stderr, a builder's line filtered for what stderr is
-- ('escapeFilterFor').
cliLogger :: IO Logger
cliLogger = (`handleLogger` stderr) <$> escapeFilterFor stderr

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

-- | Canonicalize and read a source file, or the @default.nix@ of a
-- directory, which upstream's @resolveExprPath@ appends to a path that is
-- one and resolves no further (eval.cc), so relative paths in it resolve
-- against the directory.  The canonicalization matters: relative path
-- literals inside the file resolve against the file's directory
-- ('esBaseDir'), and a relative base dir would be re-prefixed on every
-- resolution (doubling it).  An unreadable argument (a missing file, a
-- directory with no @default.nix@, no permission) is a clean CLI error, never
-- an uncaught exception.  Only 'IOException' is caught: an interrupt or any
-- other async exception must abort the run, not print as a read failure.
readSourceFile :: FilePath -> IO (FilePath, BS.ByteString)
readSourceFile rawPath = do
  attempt <- try $ do
    canonical <- canonicalizePath rawPath
    isDirectory <- doesDirectoryExist canonical
    let path = if isDirectory then canonical </> directoryExprFile else canonical
    source <- BS.readFile path
    pure (path, source)
  case attempt of
    Left (e :: IOException) ->
      failWith ("cannot read " <> T.pack rawPath <> ": " <> T.pack (displayException e))
    Right ok -> pure ok

-- | The file a directory target stands for.
directoryExprFile :: FilePath
directoryExprFile = "default.nix"

-- | Where @--expr@ and @--arg@ source comes from: text with no file to
-- point at, which its parse errors name @<expr>@ and its positions name
-- no file.
exprOrigin :: SourceOrigin
exprOrigin = SourceText "<expr>"

-- | The evaluation policy for this run: the config cascade's settings,
-- with each flag moving its setting from the default.  A flag only moves
-- a setting that way, so a machine configured for pure evaluation, or
-- with import from derivation off, stays that way whatever the command
-- line says, which is the direction a confinement should fail in.
evalPolicyFor :: CliOpts -> NixConfig -> EvalPolicy
evalPolicyFor opts config =
  EvalPolicy
    { epRestrictEval = optRestrictEval opts || ncRestrictEval config,
      epPureEval = optPureEval opts || ncPureEval config,
      epAllowedUris = ncAllowedUris config,
      epTraceVerbose = optTraceVerbose opts || ncTraceVerbose config,
      epImportFromDerivation = importFromDerivationFor opts config
    }

-- | What @allow-import-from-derivation@ and @trace-import-from-derivation@
-- say for this run.  Off, by the flag or the configuration, the trace is
-- moot, as upstream's has no effect then (@traceImportFromDerivation@ in
-- eval-settings.hh, from 2.34.9).
importFromDerivationFor :: CliOpts -> NixConfig -> ImportFromDerivation
importFromDerivationFor opts config
  | optNoAllowImportFromDerivation opts || not (ncAllowImportFromDerivation config) = RefuseImportFromDerivation
  | ncTraceImportFromDerivation config = TraceImportFromDerivation
  | otherwise = AllowImportFromDerivation

-- | Run an action in the evaluation session of one command
-- ('withEvalSession'), its state's roots allowed.  Every value the
-- action evaluates is used inside it: the session's end frees them.
-- Upstream Nix decides the same things in the same order: under pure-eval
-- the lookup path is empty, so neither @--nix-path@ nor @NIX_PATH@
-- contributes, and only the bundled @<nix/*>@ directory remains, which
-- upstream serves from an in-memory corepkgs filesystem and this binary
-- serves from its data directory (so it stays visible in
-- @builtins.nixPath@, where upstream shows nothing); under either mode
-- every search path root is allowed but a pseudo-URL entry and one with no
-- UTF-8 reading, as upstream allows the lookup path, so
-- @--nix-path nixpkgs=...@ is what makes a nixpkgs tree readable.
--
-- Each root is allowed under its given spelling and under its resolved
-- one.  The access walk follows symlinks, so a root reached through a
-- symlinked directory (macOS's @/tmp@ is @/private/tmp@) is checked under
-- the resolved spelling; upstream allows only the given one and refuses
-- the read, which is a mismatch between its own allow list and walk, not
-- a rule worth keeping.
withCliSession :: CliOpts -> NixConfig -> StoreDir -> SessionStore -> FilePath -> FilePath -> Regions -> (EvalState -> IO a) -> IO a
withCliSession opts config storeDir store dataDir baseDir regions action = do
  let policy = evalPolicyFor opts config
  -- Read whatever the policy, so a NIX_PATH with no UTF-8 form is refused
  -- under pure-eval too, but parsed only where it is used.
  nixPath <- nixPathValue
  let searchPath
        | epPureEval policy = parseNixPath (TE.encodeUtf8 (T.pack dataDir))
        | otherwise = mergeSearchPaths (optNixPaths opts) dataDir nixPath
  logger <- cliLogger
  withEvalSession (SessionSettings policy searchPath storeDir baseDir regions store logger) $ \st0 -> do
    let st = st0 {esCallDepth = topLevelCallDepth (ncMaxCallDepth config)}
    allowSearchPathRoots st
    action st
  where
    -- A relative root resolves against the working directory, as
    -- upstream's absPath does, and a rooted one with no drive lands on
    -- the store's drive, where its lookups read ('qualifyPathText').  A
    -- pseudo-URL entry (channel:, flake:, a listed scheme) names a
    -- download this evaluator never performs and allows nothing:
    -- upstream allows such an entry's working-directory spelling only by
    -- falling through after a failed download (resolveLookupPathPath),
    -- and a run that cannot fetch should not widen what it may read.  A
    -- root with no UTF-8 reading allows nothing: allowed paths are text,
    -- so no read can name it, and a lossy spelling would allow a
    -- different path.
    allowSearchPathRoots st = do
      rootBytes <- searchPathRoots (evalSearchPath st)
      roots <- mapM rootSpellings (mapMaybe textRoot (filter (not . isNixPathPseudoUrl) rootBytes))
      mapM_ (allowEvalPath st) (concat roots)
    textRoot = either (const Nothing) Just . TE.decodeUtf8'
    rootSpellings root = do
      given <- makeAbsolute (T.unpack (qualifyPathText storeDir root))
      resolved <- canonicalizePath given
      pure (map (canonPathValue . T.pack) [given, resolved])

-- | Parse and bind source against the names the root environment
-- 'withCliSession' builds will bind, which depend on the policy.
parseForEval :: CliOpts -> NixConfig -> FilePath -> SourceOrigin -> BS.ByteString -> Either SourceError ParsedSource
parseForEval opts config = parseNixWithScope (rootScopeNames (evalPolicyFor opts config))

-- | Run an action on a target in the evaluation session of one command,
-- in the order upstream's commands take them (@main_nix_instantiate@ in
-- nix-instantiate.cc and @main_nix_build@ in nix-build.cc): the session, then
-- the cells @--arg@ and @--argstr@ give ('sessionAutoArgs'), then the target,
-- found, read and parsed.  The action runs with the target's directory as the
-- session's base, and gets the cells and the parsed target.  A failure to
-- find, read or parse is reported and exits.
withTarget :: NixConfig -> CliOpts -> StoreDir -> SessionStore -> FilePath -> Regions -> Target -> (EvalState -> AutoArgs -> ParsedSource -> IO a) -> IO a
withTarget config opts storeDir store dataDir regions target action = do
  cwd <- getCurrentDirectory
  withCliSession opts config storeDir store dataDir cwd regions $ \st -> do
    autoArgs <- sessionAutoArgs opts config cwd st
    (baseDir, origin, source) <- targetSource cwd st target
    case parseForEval opts config baseDir origin source of
      Left err -> failSource err
      Right parsed -> action (st {esBaseDir = baseDir}) autoArgs parsed

-- | The cells a target's function is called with, made in name order,
-- each name interned after its expression, as upstream's @getAutoArgs@
-- makes them (common-eval-args.cc).  An @--arg@ expression is parsed from the
-- argument's bytes, relative paths against the working directory, before
-- anything else is read, and evaluated in the root environment only when the
-- function uses it; an @--argstr@ value is a string of the argument's bytes.
-- A name is an attribute name, which is text here, so one whose bytes have no
-- UTF-8 reading is refused where upstream keeps the bytes.  Each cell
-- outlives the step that makes it, so it is registered as a root
-- ("Nix.Eval.CRoots").
sessionAutoArgs :: CliOpts -> NixConfig -> FilePath -> EvalState -> IO AutoArgs
sessionAutoArgs opts config cwd st = do
  prepared <- traverse prepare (Map.toAscList (optAutoArgs opts))
  Map.fromList <$> evalOrExit st (traverse cell prepared)
  where
    prepare (rawName, arg) = do
      let flag = autoArgFlag arg
      nameBytes <- commandLineBytes flag rawName
      name <- either (const (failWith ("error: " <> flag <> ": invalid UTF-8 in argument name"))) pure (TE.decodeUtf8' nameBytes)
      made <- case arg of
        AutoArgExpr source ->
          commandLineBytes flag source >>= either failSource (pure . Left) . parseForEval opts config cwd exprOrigin
        AutoArgString value -> Right <$> commandLineBytes flag value
      pure (name, made)
    cell (name, made) = do
      thunk <- either (compileSource >=> mkThunkBc (evalBaseEnv st)) (evaluated . mkStrBytes) made
      crootsAddThunk (unThunk thunk)
      (name, thunk) <$ symbolIntern name
    autoArgFlag (AutoArgExpr _) = "--arg"
    autoArgFlag (AutoArgString _) = "--argstr"

-- | The bytes of a command-line argument ('argumentBytes'), the command
-- failing for a Windows argument with no UTF-8 form.
commandLineBytes :: T.Text -> String -> IO BS.ByteString
commandLineBytes what arg =
  argumentBytes arg
    >>= maybe (failWith ("error: " <> what <> " holds an unpaired UTF-16 surrogate, which has no UTF-8 form")) pure

-- | Where a target's expression is read from: the directory relative
-- paths in it resolve against, its origin and its bytes.  A @\<NAME\>@
-- is found and read as @import \<NAME\>@ finds and reads its file
-- ('findInSearchPath', 'resolveImportTarget'), failing as that does, so
-- a file in the store keeps its store path's spelling and a path literal
-- in it names a place in that store path.
targetSource :: FilePath -> EvalState -> Target -> IO (FilePath, SourceOrigin, BS.ByteString)
targetSource _ _ (TargetFile path) = fileSource path
targetSource cwd st (TargetSearchPath name) = do
  nameBytes <- commandLineBytes "<NAME>" name
  (target, source) <- evalOrExit st $ do
    located@(target, _) <- findInSearchPath (evalSearchPath st) nameBytes >>= resolveImportTarget cwd
    (,) target <$> readImportTarget located
  pure (takeDirectory target, SourceFile (T.pack target), source)
targetSource cwd _ (TargetExpr source) = (,,) cwd exprOrigin <$> commandLineBytes "--expr" source

-- | A target file's directory, origin and bytes ('readSourceFile').
fileSource :: FilePath -> IO (FilePath, SourceOrigin, BS.ByteString)
fileSource rawPath = do
  (filePath, source) <- readSourceFile rawPath
  pure (takeDirectory filePath, SourceFile (T.pack filePath), source)

-- | Run an evaluation, reporting a failure and exiting.
evalOrExit :: EvalState -> EvalIO a -> IO a
evalOrExit st action =
  runEvalIOTraced st action >>= \case
    Left failure -> do
      TIO.hPutStrLn stderr (renderEvalFailure failure)
      exitFailure
    Right value -> pure value

-- | Evaluate a target and print its value.  A function is called first
-- when an argument is given, as @nix-instantiate --eval@ calls it only
-- then.  The session is read-only, as @nix-instantiate --eval@'s is
-- ('withReadOnlySession'), unless @--read-write-mode@ asks for a read-write
-- one, as it asks @nix-instantiate@'s: then what evaluation made is added
-- to the store before a context's realisation that needs it and once
-- evaluation succeeds, where upstream adds each object as it makes it.  An
-- evaluation that made nothing adds nothing, so it asks nothing of a store
-- upstream Nix manages.
evalCommand :: NixConfig -> CliOpts -> StoreDir -> FilePath -> Target -> IO ()
evalCommand config opts storeDir dataDir target = do
  regions <- regionsWanted
  withEvalSessionStore config opts storeDir $ \session -> withTarget config opts storeDir session dataDir regions target $ \st autoArgs parsed -> do
    let call
          | Map.null autoArgs = pure
          | otherwise = autoCallFunction autoArgs
    (env, forced) <- evalOrExit st (evalRooted (optStrict opts) (evalBaseEnv st) call parsed)
    case session of
      ReadOnlySession _ _ -> pure ()
      ReadWriteSession ops -> sessionObjects st >>= soAddEvalObjects ops >>= either (failWith . ("error: " <>)) pure
    printResult forced >> writeCensus env forced >> writeRegionCensus

-- | The store an @eval@ session realises contexts against: read-only by
-- default ('withReadOnlySession'), and under @--read-write-mode@ the
-- local store a build uses, under the build's configuration, so the
-- cache, key, fallback and launcher flags reach what it builds.
withEvalSessionStore :: NixConfig -> CliOpts -> StoreDir -> (SessionStore -> IO a) -> IO a
withEvalSessionStore config opts storeDir action
  | optReadWriteMode opts = do
      buildConfig <- buildConfigFor config opts storeDir
      logger <- cliLogger
      withLocalStore logger buildConfig (action . ReadWriteSession)
  | otherwise = withReadOnlySession config opts storeDir action

-- | What a session's evaluation made: the copies of the paths it coerced,
-- the trees it wrote and the derivations it recorded, as the store adds
-- them ('soAddEvalObjects').
sessionObjects :: EvalState -> IO EvalObjects
sessionObjects st =
  EvalObjects
    . sourceCopies
    <$> readIORef (esSourcePathCache st)
    <*> readIORef (esStoreWriteCache st)
    <*> readIORef (esDrvClosure st)

-- | Evaluate the source in the root environment, pass the value through
-- @call@, and give the result 'finalize' gives, with that environment,
-- which 'writeCensus' counts as held.
evalRooted :: Bool -> Env -> (NixValue -> EvalIO NixValue) -> ParsedSource -> EvalIO (Env, NixValue)
evalRooted strict env call parsed = do
  code <- compileSource parsed
  forced <- evalBytecode env code >>= call >>= finalize strict
  pure (env, forced)

-- | The variable naming the file 'writeCensus' writes.
censusVar :: BS.ByteString
censusVar = "NOVA_NIX_C_STATS"

-- | The file 'censusVar' names, when it names one: set and not empty.
censusTarget :: IO (Maybe BS.ByteString)
censusTarget = mfilter (not . BS.null) <$> configVariable censusVar

-- | With 'censusVar' set to a path, write a census of the C data layer
-- ("Nix.Eval.CStats") to it once an evaluation has succeeded.  Its roots
-- are what the process still holds once the evaluation has returned: the
-- result and the root environment, and every root the session registered
-- ("Nix.Eval.CRoots"), the import cache's values among them.  A census of GHC's heap
-- follows, which a run under a heap profile (@+RTS -hT@) records as its
-- last sample: taken after the C census, so that no periodic sample falls
-- between it and the end however long the mark runs, in the major
-- collection that leaves nothing in GHC's heap the evaluation has dropped.
writeCensus :: Env -> NixValue -> IO ()
writeCensus env result = do
  target <- censusTarget
  case target of
    Nothing -> pure ()
    Just bytes -> do
      path <- variableHostPath censusVar bytes
      census <- takeCensus (RootRegistered : RootEnv env : valueRoots result)
      requestHeapCensus
      performMajorGC
      case census of
        Left failure -> failWith ("error: " <> T.pack (displayException failure))
        Right taken -> OsFile.writeFile' path (renderCensus taken)

-- | The variable naming the file 'writeRegionCensus' writes.
regionCensusVar :: BS.ByteString
regionCensusVar = "NOVA_NIX_REGION_STATS"

-- | The file 'regionCensusVar' names, when it names one: set and not
-- empty.
regionCensusTarget :: IO (Maybe BS.ByteString)
regionCensusTarget = mfilter (not . BS.null) <$> configVariable regionCensusVar

-- | What an evaluation runs regions for: a census of them when
-- 'regionCensusVar' names a file; nothing when 'censusVar' does, since the
-- census of the C data layer counts everything the evaluation allocated,
-- which the collector would have freed some of; and the collector
-- otherwise.
regionsWanted :: IO Regions
regionsWanted = wanted <$> regionCensusTarget <*> censusTarget
  where
    wanted (Just _) _ = RegionsCensus
    wanted Nothing (Just _) = RegionsOff
    wanted Nothing Nothing = RegionsCollect

-- | With 'regionCensusVar' set to a path, write what the session's census
-- of regions found ("Nix.Eval.CRegion") once an evaluation has
-- succeeded: for each force, what it allocated and what still reached it
-- as it returned, and what collectors freeing at those points would have
-- held.
writeRegionCensus :: IO ()
writeRegionCensus = do
  target <- regionCensusTarget
  case target of
    Nothing -> pure ()
    Just bytes -> do
      path <- variableHostPath regionCensusVar bytes
      figures <- regionFigures
      case figures of
        Left failure -> failWith ("error: " <> T.pack (displayException failure))
        Right found -> OsFile.writeFile' path (renderRegionFigures found)

-- | Evaluate a target to a derivation and print its ATerm (.drv
-- contents), for diffing nova-nix's serialization against upstream Nix.
-- A function is called first, whether or not an argument is given, as
-- @nix-instantiate@ calls one before it writes a derivation
-- (@getDerivations@ in get-drvs.cc).  The session is read-write, as plain
-- @nix-instantiate@'s is, which runs read-only only under @--eval@
-- (@main_nix_instantiate@ in nix-instantiate.cc): what an import from
-- derivation names is added and built.
evalAterm :: NixConfig -> CliOpts -> StoreDir -> FilePath -> Target -> IO ()
evalAterm config opts storeDir dataDir target = do
  buildConfig <- buildConfigFor config opts storeDir
  logger <- cliLogger
  withLocalStore logger buildConfig $ \ops -> withTarget config opts storeDir (ReadWriteSession ops) dataDir RegionsCollect target $ \st autoArgs parsed -> do
    val <- evalOrExit st $ do
      val <- evalParsed (pure (evalBaseEnv st)) parsed >>= autoCallFunction autoArgs
      val <$ forceDerivationAttrs val
    drvSP <- derivationPath val
    drvClosure <- readIORef (esDrvClosure st)
    aterm <- recordedAterm drvClosure drvSP
    -- The recorded bytes, not a re-serialization: these are what the
    -- path hashes.  BC.putStrLn bypasses the handle encoding, so the
    -- printed .drv diffs byte-exactly against upstream's.
    toStdout (BC.putStrLn aterm)

-- | Force the attributes 'derivationPath' goes on to read.  @derivation@
-- is a lazy wrapper, and 'readThunkValue' answers 'Nothing' for a thunk that
-- was never forced, so skipping this reports a real derivation as not one.
-- Forcing @drvPath@ is also what computes the derivation and records its
-- @.drv@ in the session closure ('recordedAterm').
forceDerivationAttrs :: (MonadEval m) => NixValue -> m ()
forceDerivationAttrs val = case val of
  VAttrs attrs ->
    mapM_
      (\k -> attrSetLookup k attrs >>= maybe (pure ()) (void . force))
      derivationAttrKeys
  _ -> pure ()

-- | What a build needs forced: the marker, the path whose closure the
-- build driver writes, and the output it prints and links.
derivationAttrKeys :: [T.Text]
derivationAttrKeys = ["type", "drvPath", "outputName"]

-- | Evaluate a target command's target and select the attribute path, when
-- one is given, with the attributes 'derivationPath' reads forced, and
-- run an action in the session with the state the evaluation recorded
-- and the selected value.  A function is called with the command line's
-- arguments before each component of the path is selected from it, and
-- once the path has selected it, as @nix-build@ calls one.
-- The action reads what the command needs out of them: the session's end
-- frees the value.  A failure is reported and exits.
evalTarget :: NixConfig -> CliOpts -> StoreDir -> SessionStore -> FilePath -> Target -> Maybe T.Text -> (EvalState -> NixValue -> IO a) -> IO a
evalTarget config opts storeDir store dataDir target attrPath action =
  withTarget config opts storeDir store dataDir RegionsCollect target $ \st autoArgs parsed -> do
    let call = autoCallFunction autoArgs
    selected <- evalOrExit st $ do
      root <- evalParsed (pure (evalBaseEnv st)) parsed
      chosen <- case attrPath of
        Nothing -> pure (Right root)
        Just path -> selectAttrPath call path root
      -- Forced after selection, not before: forcing the root leaves the
      -- selected value's own attributes unforced, and derivationPath
      -- then reports a real derivation as not being one.
      traverse (call >=> \val -> val <$ forceDerivationAttrs val) chosen
    case selected of
      Left selectionErr -> do
        TIO.hPutStrLn stderr ("error: " <> selectionErr)
        exitFailure
      Right val -> action st val

-- | Parse, evaluate, extract the derivation, build it, and print its
-- output path.  A store directory upstream Nix's database also manages
-- is refused before evaluation, which can write to the store too
-- ("Nix.Store.Upstream").
buildCommand :: NixConfig -> CliOpts -> StoreDir -> FilePath -> Target -> Maybe T.Text -> OutLink -> IO ()
buildCommand config opts storeDir dataDir target attrPath outLink = do
  upstreamManagedRefusal ReplacesUpstreamPaths storeDir >>= mapM_ (failWith . ("build: " <>))
  buildConfig <- buildConfigFor config opts storeDir
  logger <- cliLogger
  -- One handle serves the evaluation, which adds and builds what an
  -- import from derivation names, and the build after it, and opens with
  -- whichever first asks for the store.  The root is registered before
  -- the store closes: its lease is what keeps a concurrent collection
  -- from running between the build's end and the link's creation.  The
  -- path prints after the store has closed and the link exists, as
  -- nix-build prints it, so a failed link prints nothing a script would
  -- take for success.
  (outcome, outputPath) <- withLocalStore logger buildConfig $ \ops -> do
    -- The session ends before the build starts, freeing the evaluation's
    -- memory: the build needs only what is read out of it here.  The
    -- full .drv closure (root + every transitive input) recorded during
    -- evaluation is written to the store before building, and the root's
    -- own recipe is read from the same map, so what is built is what its
    -- path hashes.
    (drvSP, outputName, evalObjects) <-
      evalTarget config opts storeDir (ReadWriteSession ops) dataDir target attrPath $ \st val -> do
        drvSP <- derivationPath val
        outputName <- defaultOutputName drvSP val
        (,,) drvSP outputName <$> sessionObjects st
    drv <- recordedDerivation (eoDerivations evalObjects) drvSP
    -- Resolved before the build, as nix-build resolves the output it
    -- realizes before building anything.
    outputPath <- namedOutputPath drv outputName
    let -- Evaluation computes the paths of the sources it coerced and of
        -- the .drv files it recorded, and writes trees such as
        -- builtins.toFile's.  An import from derivation adds all of it
        -- made by then, so a build adds the rest first; the add passes
        -- over what is valid.  The whole recorded .drv closure goes in,
        -- the root's own recipe among it, since the build reads each one
        -- back.
        -- With --check the derivation is checked, every output of it,
        -- and otherwise the output outputName names is realised, the
        -- only one nix-build realises.
        build = case buildModeFor opts of
          BuildNormal -> soBuild ops [(drvSP, Set.singleton outputName)]
          BuildCheck -> soCheck ops drvSP
    outcome <- runExceptT $ do
      ExceptT (first AddRefused <$> soAddEvalObjects ops evalObjects)
      ExceptT (first BuildStopped <$> build)
      traverse (\link -> ExceptT (first LinkRefused <$> soAddOutLinkRoot ops link outputPath)) (outLinkFor outLink outputName)
    pure (outcome, outputPath)
  case outcome of
    Left stop -> reportStop stop
    Right _ -> toStdout (TIO.putStrLn (T.pack (storePathToFilePath storeDir outputPath)))

-- | Report why a build command stopped once evaluation was done, and exit.
reportStop :: BuildCommandStop -> IO a
reportStop stop = case stop of
  -- Reported as a refusal an add throws is ('reportIOFailure').
  AddRefused reason -> do
    TIO.hPutStrLn stderr ("error: " <> reason)
    exitWith (ExitFailure 1)
  -- The status printed is the status exited with: 1, or upstream's 104
  -- for a check that found an output different.
  BuildStopped (BuildStop msg code) -> do
    TIO.hPutStrLn stderr ("build failed (exit " <> T.pack (show code) <> "): " <> msg)
    exitWith (ExitFailure code)
  LinkRefused err -> failWith ("build: " <> err)

-- | The configuration a command's builds run under: the caches, fallback,
-- substitution setting, cores, launchers, sandbox and what a failed build
-- keeps that its options and nix.conf give.
buildConfigFor :: NixConfig -> CliOpts -> StoreDir -> IO BuildConfig
buildConfigFor config opts storeDir = do
  wrappers <- either failWith pure (execWrapperConfig (optExecWrappers opts)) >>= checkExecWrappers
  cores <- buildCoresFor config
  sandbox <- sandboxConfigFor config opts
  pure
    (defaultBuildConfig storeDir)
      { bcCaches = resolveCaches config (optSubstituter opts) (optTrustedKey opts),
        bcFallback = fallbackFor opts config,
        bcSubstitutes = substitutesFor config,
        bcCores = cores,
        bcExecWrappers = wrappers,
        bcSandbox = sandbox,
        bcFailedBuilds = failedBuildsFor opts config
      }

-- | Run an action with a read-only session's store for the store in a
-- directory, as @nix-instantiate --eval@ runs read-only
-- (@main_nix_instantiate@ in nix-instantiate.cc).  Its queries make no store
-- and write no row ('localQueries', beside whose database SQLite can make its
-- wal-index files).  The store's operations open it only to add what a
-- fetcher fetched, as upstream's fetchers add theirs in any mode, or to
-- build: an output a context names is realised by a build that refuses every
-- substitution ('RefuseSubstitution'), which in a store upstream Nix manages
-- opens nothing.
withReadOnlySession :: NixConfig -> CliOpts -> StoreDir -> (SessionStore -> IO a) -> IO a
withReadOnlySession config opts storeDir action = do
  buildConfig <- buildConfigFor config opts storeDir
  logger <- cliLogger
  withLocalStore logger buildConfig {bcSubstitution = RefuseSubstitution} $ \ops ->
    action (ReadOnlySession (localQueries storeDir) ops)

-- | Why a build command stopped once evaluation was done.
data BuildCommandStop
  = -- | The store refused evaluation's objects.
    AddRefused !T.Text
  | -- | The build stopped.
    BuildStopped !BuildStop
  | -- | The out-link could not be made or rooted.
    LinkRefused !T.Text

-- | Evaluate the target, find the fixed-output derivations of its
-- derivation's closure, fetch each one nova-nix's builtins fetch alone,
-- substitute the others from the configured caches, scan the outputs the
-- store holds, and print what the closure relies on ("Nix.Trust").  The
-- graph is read from the recipes evaluation recorded.  The evaluation is
-- read-only ('withReadOnlySession'), so it writes only what a fetcher
-- fetches and what a context's realisation builds, and otherwise only the
-- fetches and substitutions of fixed-output inputs write to the store: a
-- closure with no fixed-output input does not open it after evaluation.
-- A store directory upstream Nix's database
-- also manages is refused before those are made, as a build refuses one
-- ("Nix.Store.Upstream").
trustCommand :: NixConfig -> CliOpts -> StoreDir -> FilePath -> Target -> Maybe T.Text -> IO ()
trustCommand config opts storeDir dataDir target attrPath = do
  (drvSP, drvClosure) <- withReadOnlySession config opts storeDir $ \session -> evalTarget config opts storeDir session dataDir target attrPath $ \st val ->
    (,) <$> derivationPath val <*> readIORef (esDrvClosure st)
  drv <- recordedDerivation drvClosure drvSP
  graph <- either (failWith . ("error: " <>)) pure (runIdentity (buildDepGraph (Identity . recordedRecipe drvClosure) drv drvSP))
  facts <- case fixedOutputInputs graph drvSP of
    [] -> pure Map.empty
    inputs -> do
      upstreamManagedRefusal ReplacesUpstreamPaths storeDir >>= mapM_ (failWith . ("trust: " <>))
      logger <- cliLogger
      bracket (openStore logger storeDir) closeStore (\store -> fetchAndScan config opts store inputs)
  toStdout (mapM_ TIO.putStrLn (renderTrustReport (trustReport facts graph drvSP)))

-- | Make each fixed-output input's output valid in a way that runs
-- nothing from the closure, then scan every one the store holds.  An
-- input that nova-nix's builtins fetch with no input of their own is
-- realised, which substitutes or fetches it; any other is only
-- substituted, with what it references, since making it would run its
-- builder.  That substitution leaves the derivation's @allowSubstitutes@
-- unread: the attribute chooses between building and substituting, and
-- there is no build here to choose.  The first fetch that fails stops
-- the command, as a build stops, and so does the first substitution that
-- fails, as upstream's goal for substituting a path fails when a
-- substituter fails, whatever @fallback@ says (@PathSubstitutionGoal::init@
-- in substitution-goal.cc at 2.24.9, @PathSubstitutionGoal::substitute@ at
-- 2.36.0): @--fallback@ passes over only a cache that cannot be asked, there
-- (its catch around @queryPathInfo@) and here.  An input no cache holds is
-- one the report names as not read.
fetchAndScan :: NixConfig -> CliOpts -> Store -> [(DepNode, StorePath)] -> IO (Map.Map StorePath ContentFacts)
fetchAndScan config opts store inputs = do
  mapM_ (fetch . fst) fetched
  mapM_ (substitute . snd) others
  Map.fromList . catMaybes <$> traverse scan inputs
  where
    (fetched, others) = partition (fetchedByBuiltin . dnDerivation . fst) inputs
    buildConfig =
      (defaultBuildConfig (stDir store))
        { bcCaches = resolveCaches config (optSubstituter opts) (optTrustedKey opts),
          bcFallback = fallbackFor opts config,
          bcSubstitutes = substitutesFor config
        }
    fetch node = do
      result <- buildWithDeps buildConfig store (dnDerivation node) (dnDrvPath node)
      case result of
        BuildSuccess _ -> pure ()
        BuildFailure msg _ -> failWith ("trust: " <> msg)
    substitute output = do
      outcome <- substituteFromCaches buildConfig store [output]
      case outcome of
        ClosureComplete -> pure ()
        ClosureUnavailable -> pure ()
        ClosureIncomplete -> pure ()
        ClosureFailed -> failWith ("trust: a substitute for '" <> storePathToText defaultStoreDir output <> "' or a path it references failed")
        ClosureQueryFailed failure -> failWith ("trust: " <> failure)
    scan (_, output) = do
      held <- isValid store output
      if held
        then Just . (,) output <$> scanFetched (storePathToFilePath (stDir store) output)
        else pure Nothing

-- | The @.drv@ store path of an evaluated derivation value: a set with
-- @type = "derivation"@ whose @drvPath@ has been forced
-- ('forceDerivationAttrs').  Store paths are ASCII, so the byte payload
-- decodes strictly.
derivationPath :: NixValue -> IO StorePath
derivationPath (VAttrs attrs) = do
  computedAttr "type" attrs >>= \case
    Just (VStr "derivation" _) -> pure ()
    _ -> failWith "error: result is not a derivation (no type = \"derivation\")"
  computedAttr "drvPath" attrs >>= \case
    Just (VStr pathBytes _)
      | Right path <- TE.decodeUtf8' pathBytes ->
          maybe (failWith ("error: invalid drvPath: " <> path)) pure (parseStorePath defaultStoreDir path)
    _ -> failWith "error: derivation result missing drvPath"
derivationPath _ = failWith "error: result is not a derivation"

-- | The output a build realises, prints and links (@--check@ checks every
-- output): the value's @outputName@, the first of its @outputs@, which
-- is what upstream's nix-build builds and prints for a derivation, with
-- its message for a set that lacks the attribute.
defaultOutputName :: StorePath -> NixValue -> IO T.Text
defaultOutputName drvSP val = do
  outputName <- case val of
    VAttrs attrs -> computedAttr "outputName" attrs
    _ -> pure Nothing
  case outputName of
    Just (VStr nameBytes _)
      | Right name <- TE.decodeUtf8' nameBytes -> pure name
    _ -> failWith ("error: derivation '" <> storePathToText defaultStoreDir drvSP <> "' lacks an 'outputName' attribute")

-- | An attribute's value if it has been computed, without forcing it.
computedAttr :: T.Text -> AttrSet -> IO (Maybe NixValue)
computedAttr key attrs = attrSetLookup key attrs >>= maybe (pure Nothing) readThunkValue

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
recordedAtermBytes :: Map.Map T.Text BS.ByteString -> StorePath -> Either T.Text BS.ByteString
recordedAtermBytes drvClosure drvSP =
  maybe (Left ("no .drv was recorded for " <> storePathToText defaultStoreDir drvSP)) Right (Map.lookup (storePathToText defaultStoreDir drvSP) drvClosure)

-- | The 'Derivation' a recorded ATerm describes ('recordedAtermBytes').
recordedRecipe :: Map.Map T.Text BS.ByteString -> StorePath -> Either T.Text Derivation
recordedRecipe drvClosure drvSP =
  recordedAtermBytes drvClosure drvSP
    >>= first (("the recorded .drv for " <> storePathToText defaultStoreDir drvSP <> " does not parse: ") <>) . fromATerm

-- | 'recordedAtermBytes', failing the command when there is none.
recordedAterm :: Map.Map T.Text BS.ByteString -> StorePath -> IO BS.ByteString
recordedAterm drvClosure = either (failWith . ("error: " <>)) pure . recordedAtermBytes drvClosure

-- | 'recordedRecipe', failing the command when there is none or it does
-- not parse.
recordedDerivation :: Map.Map T.Text BS.ByteString -> StorePath -> IO Derivation
recordedDerivation drvClosure = either (failWith . ("error: " <>)) pure . recordedRecipe drvClosure

-- | The store directory selected by @--store@, or the platform default,
-- under the canonical spelling 'openStore' keys the store by, so what
-- evaluation and every command print is the spelling the database
-- holds.
chosenStoreDir :: CliOpts -> IO StoreDir
chosenStoreDir opts = canonicalStoreDir (maybe platformStoreDir StoreDir (optStore opts))

-- | The priority a config- or CLI-configured HTTP substituter is asked at
-- (cache.nixos.org is 40).  A @file:\/\/@ one is asked at the priority its
-- @nix-cache-info@ states ("Nix.Substituter").
substituterPriority :: Int
substituterPriority = 50

-- | Turn resolved settings into the cache list.  One 'CacheConfig' per
-- substituter, by its URL as configured, each carrying the whole
-- trusted-key set: upstream's keys are not bound to a substituter, so a
-- narinfo from any cache is accepted by any trusted key.  A substituter
-- with no trusted key anywhere is not refused here (it simply accepts
-- nothing at the signature gate), matching upstream, where @substituters@
-- and @trusted-public-keys@ are independent.
configToCaches :: NixConfig -> [CacheConfig]
configToCaches config =
  [ CacheConfig
      { ccUrl = url,
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

-- | What a build that fails keeps on this run.  The flag only turns
-- keep-failed on, as nix-build's @-K@ does, so it adds to the config's
-- setting.
failedBuildsFor :: CliOpts -> NixConfig -> FailedBuilds
failedBuildsFor opts config
  | optKeepFailed opts || ncKeepFailed config = KeepFailed
  | otherwise = RemoveFailed

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

-- | How builds are sandboxed on this run: nix.conf's settings under the
-- sandbox flag, with upstream's defaults where a setting is unset that
-- take the platform or IO to find.  @sandbox@ is on by default on Linux
-- alone (@sandboxMode@ in globals.hh at 2.24.9, local-settings.hh from
-- 2.34.9).  @sandbox-paths@ keeps a release archive's bundled shell as
-- @\/bin\/sh@ on Linux, and macOS's system paths there
-- ('darwinSandboxPaths'), unless a plain assignment replaces it, as
-- upstream's default names the shell a package was built with and those
-- paths (the @Settings@ constructor in globals.cc);
-- @allowed-impure-host-deps@ keeps upstream's macOS prefixes the same
-- way.  @require-drop-supplementary-groups@ is on for root
-- (@requireDropSupplementaryGroups@ in globals.hh at 2.24.9,
-- local-settings.hh from 2.34.9).  @ssl-cert-file@ comes from
-- @NIX_SSL_CERT_FILE@, else @SSL_CERT_FILE@, else the first bundle that
-- exists (the @Settings@ constructor and @Settings::getDefaultSSLCertFile@ in
-- globals.cc at 2.24.9, the @FileTransferSettings@ ones in filetransfer.cc
-- from 2.34.9).  The helper is beside the executable, as a release archive
-- and a @cabal install@ put it, or in Cabal's install directory.
sandboxConfigFor :: NixConfig -> CliOpts -> IO SandboxConfig
sandboxConfigFor config opts = do
  exeDir <- takeDirectory <$> getExecutablePath
  binDir <- getBinDir
  helper <- listToMaybe <$> filterM doesFileExist [exeDir </> sandboxHelperName, binDir </> sandboxHelperName]
  shell <- bundledSandboxShell exeDir
  root <- processIsRoot
  caFile <- maybe defaultSslCertFile (pure . Just) (ncSslCertFile config)
  let withDefault defaults setting = case psvDefault setting of
        KeepsDefault -> Set.union defaults (psvPaths setting)
        ReplacesDefault -> psvPaths setting
  pure
    SandboxConfig
      { scMode = fromMaybe platformSandboxMode (optSandbox opts <|> ncSandbox config),
        scFallback = fallbackSetting (ncSandboxFallback config),
        scPaths = parseSandboxPaths (withDefault (Set.union shell platformSandboxPaths) (ncSandboxPaths config)),
        scBuildDir = ncSandboxBuildDir config,
        scDevShmSize = ncSandboxDevShmSize config,
        scDropGroups = dropGroupsSetting (fromMaybe root (ncRequireDropSupplementaryGroups config)),
        scAllowedImpureHostDeps = withDefault platformImpureHostPrefixes (ncAllowedImpureHostDeps config),
        scViolationLog = violationLogSetting (ncDarwinLogSandboxViolations config),
        scCaFile = caFile,
        scHelper = helper
      }
  where
    fallbackSetting True = FallBackUnsandboxed
    fallbackSetting False = RefuseUnsandboxed
    dropGroupsSetting True = RequireDropGroups
    dropGroupsSetting False = TolerateKeptGroups
    violationLogSetting True = ViolationsLogged
    violationLogSetting False = ViolationsUnlogged

-- | Upstream's default for @sandbox@ on this platform.
platformSandboxMode :: SandboxMode
platformSandboxMode
  | System.Info.os == "linux" = SandboxEnabled
  | otherwise = SandboxDisabled

-- | The @sandbox-paths@ default beside a sandbox shell: on macOS the
-- system paths a build reads ('darwinSandboxPaths'), and elsewhere
-- nothing.
platformSandboxPaths :: Set.Set BS.ByteString
platformSandboxPaths
  | System.Info.os == "darwin" = darwinSandboxPaths
  | otherwise = Set.empty

-- | Upstream's @allowed-impure-host-deps@ default: on macOS, the prefixes
-- a derivation's @__impureHostDeps@ may name a path beneath
-- ('darwinImpureHostPrefixes'), and elsewhere none.
platformImpureHostPrefixes :: Set.Set BS.ByteString
platformImpureHostPrefixes
  | System.Info.os == "darwin" = darwinImpureHostPrefixes
  | otherwise = Set.empty

-- | The sandbox helper's file name.
sandboxHelperName :: FilePath
sandboxHelperName = "nova-nix-sandbox"

-- | Where a release archive keeps the shell a sandboxed build sees as
-- @\/bin\/sh@, relative to the directory holding @bin@.
bundledShellSubpath :: FilePath
bundledShellSubpath = "libexec" </> "nova-nix" </> "sh"

-- | The @sandbox-paths@ default: a release archive's bundled shell as
-- @\/bin\/sh@ on Linux, where the archive carries one, and otherwise
-- nothing, upstream's default for a Nix built without a sandbox shell.
bundledSandboxShell :: FilePath -> IO (Set.Set BS.ByteString)
bundledSandboxShell exeDir
  | System.Info.os /= "linux" = pure Set.empty
  | otherwise = do
      let shell = takeDirectory exeDir </> bundledShellSubpath
      present <- doesFileExist shell
      Set.fromList <$> traverse (fmap ("/bin/sh=" <>) . filePathBytes) [shell | present]

-- | Upstream's @ssl-cert-file@ default: @NIX_SSL_CERT_FILE@ when it is
-- set, else @SSL_CERT_FILE@, when the value is not empty; else the first
-- of upstream's bundle paths that exists.
defaultSslCertFile :: IO (Maybe BS.ByteString)
defaultSslCertFile = do
  nixVariable <- lookupEnvBytes "NIX_SSL_CERT_FILE"
  plainVariable <- lookupEnvBytes "SSL_CERT_FILE"
  case (variableValue nixVariable, variableValue plainVariable) of
    (Just named, _) | not (BS.null named) -> pure (Just named)
    (Just _, _) -> firstBundle
    (Nothing, Just named) | not (BS.null named) -> pure (Just named)
    _ -> firstBundle
  where
    variableValue found = case found of
      EnvValue value -> Just value
      _ -> Nothing
    firstBundle = do
      present <- filterM doesFileExist sslCertFileDefaults
      traverse filePathBytes (listToMaybe present)

-- | Upstream's bundle paths, in the order it tries them
-- (@Settings::getDefaultSSLCertFile@ in globals.cc at 2.24.9,
-- @FileTransferSettings::getDefaultSSLCertFile@ in filetransfer.cc from
-- 2.34.9).
sslCertFileDefaults :: [FilePath]
sslCertFileDefaults = ["/etc/ssl/certs/ca-certificates.crt", "/nix/var/nix/profiles/default/etc/ssl/certs/ca-bundle.crt"]

-- | The cores a builder is told of on this run: nix.conf's @cores@, or
-- when nothing sets it, upstream's default on this machine.
buildCoresFor :: NixConfig -> IO Word32
buildCoresFor config = maybe machineCores pure (ncCores config)

-- | Upstream's default for @cores@ on this machine ('Cores.defaultCores'):
-- the limit of the process's cgroup, which upstream reads on Linux
-- alone, else the hardware thread count ('Cores.hardwareConcurrency').
machineCores :: IO Word32
machineCores = Cores.defaultCores <$> cgroupLimit <*> Cores.hardwareConcurrency
  where
    cgroupLimit
      | System.Info.os == "linux" = Cores.cgroupCoreLimit readKernelFile
      | otherwise = pure Nothing

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

-- | A variable the CLI reads, as its bytes, the way upstream's
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

-- | A kernel file's bytes, or 'Nothing' when it cannot be read, which
-- upstream's @getMaxCPU@ takes as no limit.  As in 'readConfigFile', the
-- open takes the path's own bytes, and only 'IOException' is caught.
readKernelFile :: OsPath -> IO (Maybe BS.ByteString)
readKernelFile path = do
  result <- try (OsFile.readFile' path) :: IO (Either IOException BS.ByteString)
  pure (either (const Nothing) Just result)

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

-- ---------------------------------------------------------------------------
-- Develop commands
-- ---------------------------------------------------------------------------

-- | Start @bash@ with the build environment of the target's derivation,
-- as @nix develop@ does (@CmdDevelop::run@ in develop.cc): capture the
-- environment ('captureEnvironment'), make the script that sets it, refuse
-- what upstream refuses only once it has, in its words and order, write the
-- structured attributes' files and the rcfile, and replace this process with
-- the shell ("Nix.Develop.Shell").  Upstream makes the rcfile, the files'
-- directory and the files before its three refusals (@CmdDevelop::run@, the
-- last two in @MixEnvironment::setEnviron@ in command.cc), and when one is
-- made leaves the rcfile behind, empty; here the directory is removed then,
-- and the rcfile and the files are written only once all three have passed.
-- The directory is removed too when there are no files to write, where
-- upstream leaves it empty.  The files it holds under structured attributes
-- outlive the shell, as upstream's do, since nothing is left after the exec
-- to remove them.
developCommand :: NixConfig -> CliOpts -> StoreDir -> FilePath -> Target -> Maybe T.Text -> DevelopArgs -> IO ()
developCommand config opts storeDir dataDir target attrPath args = do
  host <- hostOrRefuse
  command <- traverse (traverse (commandLineBytes "--command")) (daCommand args)
  phase <- traverse (commandLineBytes "--phase") (daPhase args)
  changes <- environmentChanges args
  (gcroot, environment) <- captureEnvironment "develop" config opts storeDir dataDir target attrPath
  cwd <- getCurrentDirectory >>= filePathBytes
  bracketOnError (makeTemporaryDirectory host developDirectoryPrefix) removeHostPath $ \attrsDir -> do
    script <- either failError pure (environmentScript (ScriptDirectories cwd attrsDir) environment)
    mode <- either failError pure (shellModeFor command phase (promptFor config))
    caller <- processEnvironment host
    shellEnv <- either failError pure (shellEnvironment changes (TE.encodeUtf8 (storePathToText storeDir gcroot)) caller)
    writeScriptFiles script
    when (null (scriptFiles script)) (removeHostPath attrsDir)
    bracketOnError (createRcfile host (\path -> renderRcfile (Shell path mode bashProgram) script)) removeHostPath $ \rcfile -> do
      started <- try (replaceProcess host (shellArguments mode rcfile) shellEnv)
      either (failWith . execFailure) absurd started
  where
    execFailure :: IOException -> T.Text
    execFailure failure = "error: unable to execute '" <> TE.decodeLatin1 bashProgram <> "': " <> T.pack (ioe_description failure)

-- | Print the script that sets the build environment of the target's
-- derivation, or with @--json@ the environment, as @nix print-dev-env@
-- does (@CmdPrintDevEnv@ in develop.cc).  The script names the structured
-- attributes' files in a temporary directory that is removed once it is
-- printed, as upstream removes it.  The script is followed by a newline, as
-- upstream's @writeToStdout@ writes it, and the JSON is printed as
-- 'printJson' prints it.
printDevEnvCommand :: NixConfig -> CliOpts -> StoreDir -> FilePath -> Target -> Maybe T.Text -> DevEnvFormat -> JsonLayout -> IO ()
printDevEnvCommand config opts storeDir dataDir target attrPath format layout = do
  host <- hostOrRefuse
  (_, environment) <- captureEnvironment "print-dev-env" config opts storeDir dataDir target attrPath
  case format of
    DevEnvJson -> printJson layout (buildEnvironmentJson environment)
    DevEnvScript -> do
      cwd <- getCurrentDirectory >>= filePathBytes
      bracket (makeTemporaryDirectory host printDirectoryPrefix) removeHostPath $ \attrsDir -> do
        script <- either failError pure (environmentScript (ScriptDirectories cwd attrsDir) environment)
        writeScriptFiles script
        toStdout (BS.putStr (scriptText script <> "\n"))

-- | What upstream names the directories of the structured attributes'
-- files, for @develop@ and for @print-dev-env@ (@CmdDevelop::run@ and
-- @CmdPrintDevEnv::run@ in develop.cc).
developDirectoryPrefix, printDirectoryPrefix :: BS.ByteString
developDirectoryPrefix = "nix-develop"
printDirectoryPrefix = "nix-dev-env"

-- | The t'DevelopHost' for this host, the command failing where it has none.
hostOrRefuse :: IO DevelopHost
hostOrRefuse = either failError pure developHost

promptFor :: NixConfig -> Prompt
promptFor config = Prompt (ncBashPrompt config) (ncBashPromptPrefix config) (ncBashPromptSuffix config)

-- | The environment flags' names and values as their bytes.
environmentChanges :: DevelopArgs -> IO EnvironmentChanges
environmentChanges args =
  EnvironmentChanges (daEnvironmentBase args)
    <$> names "--keep-env-var" (daKeep args)
    <*> names "--unset-env-var" (daUnset args)
    <*> (Map.fromList <$> traverse assignment (Map.toList (daSet args)))
  where
    names flagName = fmap Set.fromList . traverse (commandLineBytes flagName) . Set.toList
    assignment (name, value) = (,) <$> commandLineBytes "--set-env-var" name <*> commandLineBytes "--set-env-var" value

writeScriptFiles :: EnvironmentScript -> IO ()
writeScriptFiles = mapM_ (\file -> bytesHostPath (scriptFilePath file) >>= (`OsFile.writeFile'` scriptFileContents file)) . scriptFiles

-- | Remove a temporary file, or a directory with what it holds, passing
-- over one already gone.
removeHostPath :: BS.ByteString -> IO ()
removeHostPath path = bytesHostPath path >>= OsDir.removePathForcibly

-- | The host path a path's bytes name ('hostPathFromBytes').
bytesHostPath :: BS.ByteString -> IO OsPath
bytesHostPath path = maybe (failWith ("error: '" <> TE.decodeUtf8Lenient path <> "' is not valid UTF-8, so it names no Windows path")) pure (hostPathFromBytes path)

-- | The build environment of the target's derivation, captured as
-- upstream's @nix develop@ captures it (@getDerivationEnvironment@ in
-- develop.cc), and the store path of the output it was read from.  The target
-- is evaluated in a read-write session as @build@ evaluates it; the capture
-- of its derivation is made in that session ('recordCapture'), and what the
-- session made is added and every output of the capture realised, as @build@
-- adds and realises, so its inputs are built or substituted, and a capture
-- already valid is not built again.  A failure stops the command as it stops
-- @build@.  The store has closed by the time this returns.
captureEnvironment :: T.Text -> NixConfig -> CliOpts -> StoreDir -> FilePath -> Target -> Maybe T.Text -> IO (StorePath, BuildEnvironment)
captureEnvironment commandName config opts storeDir dataDir target attrPath = do
  upstreamManagedRefusal ReplacesUpstreamPaths storeDir >>= mapM_ (failWith . ((commandName <> ": ") <>))
  script <- readGetEnvScript dataDir
  scriptPath <- either (failWith . ("error: " <>) . badStorePathText) pure (getEnvScriptPath script)
  buildConfig <- buildConfigFor config opts storeDir
  logger <- cliLogger
  withLocalStore logger buildConfig $ \ops -> do
    (capture, evalObjects) <-
      evalTarget config opts storeDir (ReadWriteSession ops) dataDir target attrPath $ \st val -> do
        drvSP <- derivationPath val
        drv <- readIORef (esDrvClosure st) >>= (`recordedDerivation` drvSP)
        name <- maybe (failWith ("error: '" <> storePathToText defaultStoreDir drvSP <> "' is not a derivation's path")) pure (T.stripSuffix drvSuffix (spName drvSP))
        capture <- evalOrExit st (recordCapture script scriptPath name drv) >>= either (failWith . ("error: " <>) . captureFailureMessage) pure
        (,) capture <$> sessionObjects st
    let captured = drvFileDerivation capture
    outcome <- runExceptT $ do
      ExceptT (first AddRefused <$> soAddEvalObjects ops evalObjects)
      ExceptT (first BuildStopped <$> soBuild ops [(drvFilePath capture, Set.fromList (map doName (drvOutputs captured)))])
    either reportStop pure outcome
    readEnvironment storeDir captured
  where
    drvSuffix = ".drv"

-- | The capture of a derivation whose @.drv@ is named @name.drv@
-- ('captureDerivation'), with @get-env.sh@ written to the store and the
-- capture's @.drv@ recorded, so that adding what the session made adds
-- both, as upstream adds the script and writes the capture
-- (@getDerivationEnvironment@ in develop.cc).  A derivation the capture
-- refuses writes nothing, as upstream refuses its builder before adding the
-- script.  The input derivations' modulo hashes are the session's, which
-- evaluating the target computed.
recordCapture :: BS.ByteString -> StorePath -> T.Text -> Derivation -> EvalIO (Either CaptureFailure DrvFile)
recordCapture script scriptPath name drv = do
  inputHashes <- inputModuloHashes (drvInputDrvs drv)
  case captureDerivation scriptPath name inputHashes drv of
    Left failure -> pure (Left failure)
    Right capture -> do
      void (writeToStore getEnvScriptName (getEnvScriptContents script) [])
      recordDrvAterm (storePathToText defaultStoreDir (drvFilePath capture)) (drvFileATerm capture)
      pure (Right capture)

-- | Upstream's @get-env.sh@, from the data directory, where the package
-- installs it and a release archive ships it.
readGetEnvScript :: FilePath -> IO BS.ByteString
readGetEnvScript dataDir = do
  attempt <- try (BS.readFile (dataDir </> getEnvScriptFile))
  case attempt of
    Left (failure :: IOException) -> failWith ("error: cannot read get-env.sh: " <> T.pack (displayException failure))
    Right script -> pure script

-- | Where @get-env.sh@ is, relative to the data directory.
getEnvScriptFile :: FilePath
getEnvScriptFile = "develop" </> "get-env.sh"

-- | The environment a built capture holds, and the store path of the
-- output it is read from: the one 'environmentOutput' picks, read and
-- parsed as upstream's @getBuildEnvironment@ reads it (develop.cc).
readEnvironment :: StoreDir -> Derivation -> IO (StorePath, BuildEnvironment)
readEnvironment storeDir capture = do
  objects <- traverse (\output -> (,) (doName output) <$> outputObject (storePathToFilePath storeDir (doPath output))) (drvOutputs capture)
  output <-
    maybe (failWith ("error: " <> noEnvironmentMessage)) pure $
      environmentOutput (Map.fromList objects) >>= \name -> find ((== name) . doName) (drvOutputs capture)
  bytes <- BS.readFile (storePathToFilePath storeDir (doPath output))
  either (failWith . ("error: " <>)) (pure . (,) (doPath output)) (parseBuildEnvironment bytes)

-- | What an output's root is, as upstream's @maybeLstat@ answers: a link
-- is not followed.
outputObject :: FilePath -> IO OutputObject
outputObject path = do
  link <- pathIsSymbolicLink path
  regular <- if link then pure False else doesFileExist path
  if regular then RegularFile . fromIntegral <$> getFileSize path else pure NotRegularFile

-- ---------------------------------------------------------------------------
-- Push command
-- ---------------------------------------------------------------------------

-- | Push the closure of the selected store paths to a binary cache: a
-- nova-cache server, or a directory.
pushCommand :: StoreDir -> PushArgs -> IO ()
pushCommand storeDir pushArgs = do
  url <- maybe (failWith "push: --cache URL is required") pure (paCacheUrl pushArgs)
  target <- either (failWith . ("push: " <>)) pure (pushTarget url (paKeyFile pushArgs) (paSecretKeyFile pushArgs))
  case (paAll pushArgs, paPaths pushArgs) of
    (True, _ : _) -> failWith "push: --all and explicit paths are mutually exclusive"
    (False, []) -> failWith "push: name store paths to push, or pass --all"
    _ -> pure ()
  compression <- case paCompressionArg pushArgs of
    Nothing -> pure PushNone
    Just value -> either (failWith . ("push: " <>)) pure (parsePushCompression (T.pack value))
  push <- case target of
    ServerTarget cacheUrl apiKeyFile -> do
      apiKey <- traverse (loadKeyFile >=> either failWith pure) apiKeyFile
      pure $
        pushPaths
          PushConfig
            { pcCacheUrl = cacheUrl,
              pcApiKey = apiKey,
              pcCompression = compression,
              pcPartSize = defaultPartSize,
              pcRetry = defaultFetchRetryPolicy
            }
    DirectoryTarget dir secretKeyFile -> do
      secretKey <- loadSecretKeyFile secretKeyFile >>= either failWith pure
      pure (pushToDirectory (DirectoryPush dir secretKey compression))
  logger <- cliLogger
  store <- openStore logger storeDir
  rootsResult <- resolvePushRoots store pushArgs
  case rootsResult of
    Left err -> do
      closeStore store
      failWith err
    Right roots -> do
      result <- push store roots
      closeStore store
      case result of
        Left err -> failWith ("push failed: " <> err)
        Right summary ->
          toStdout . TIO.putStrLn $
            "pushed "
              <> T.pack (show (psPushed summary))
              <> " path(s), "
              <> T.pack (show (psSkipped summary))
              <> " already cached"

-- | Delete store paths: registration rows and on-disk trees.  Every
-- argument is resolved before anything is touched; one
-- 'deleteStorePaths' call then deletes the paths in argument order
-- under one collector lock and one live set, printing a line for each
-- as it goes, with upstream's roots line printed once per invocation as
-- its @--delete@ prints it, and the first failure stops the run, so a
-- reference chain deletes leaf-first in one invocation.
storeDeleteCommand :: StoreDir -> [String] -> IO ()
storeDeleteCommand storeDir rawPaths = do
  targets <- either (failWith . prefixed) pure (traverse (resolveDeleteTarget storeDir . T.pack) rawPaths)
  logger <- cliLogger
  store <- openStore logger storeDir
  result <- deleteStorePaths store targets reportDeleted
  closeStore store
  either (failWith . prefixed) pure result
  where
    prefixed err = "store delete: " <> err
    reportDeleted name removed = toStdout (TIO.putStrLn ("deleted " <> storeEntryNameText name <> describeOutcome removed))
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
  logger <- cliLogger
  store <- openStore logger storeDir
  if printRoots
    then do
      roots <- findRoots store
      closeStore store
      toStdout (mapM_ (\root -> TIO.putStrLn (T.pack (grLink root) <> " -> " <> grPath root)) roots)
    else do
      results <- collectGarbage store Set.empty
      closeStore store
      either (failWith . ("store gc: " <>)) (toStdout . TIO.putStrLn . gcSummaryLine) results

-- ---------------------------------------------------------------------------
-- Profile commands
-- ---------------------------------------------------------------------------

-- | Where the link to the default profile is, as nix.conf's
-- @use-xdg-base-directories@ says.
profileLinkPlace :: NixConfig -> ProfileLinkPlace
profileLinkPlace config
  | ncUseXdgBaseDirectories config = StateProfileLink
  | otherwise = HomeProfileLink

-- | The profile a profile command works on.  The default profile is
-- resolved first, its directory and its link made when they are missing,
-- whether or not @--profile@ names another, as upstream's
-- @MixDefaultProfile@ resolves it (command.cc); a profile @--profile@ names
-- is made absolute without reading a link (@MixProfile::updateProfile@ in
-- command.cc).
chosenProfile :: NixConfig -> ProfileSelection -> IO OsPath
chosenProfile config selection = do
  env <- cliLogger >>= readProfileEnvironment >>= either failError pure
  defaulted <- resolveDefaultProfile (profileLinkPlace config) env >>= either failError pure
  case selection of
    DefaultProfile -> pure defaulted
    ProfileAt raw -> do
      bytes <- commandLineBytes "--profile" raw
      path <- maybe (failError "--profile is not valid UTF-8, so it names no Windows path") pure (hostPathFromBytes bytes)
      cwd <- OsDir.getCurrentDirectory
      pure (absolutePath cwd path)

-- | Add a package to a profile: the store path a file argument leads to,
-- or the target's derivation, built for the outputs it installs, named
-- after its paths and at the priority @--priority@ gives.  A store
-- directory upstream Nix's database also manages is refused before
-- anything is evaluated, as a build refuses one.
profileAddCommand :: NixConfig -> CliOpts -> StoreDir -> FilePath -> Target -> Maybe T.Text -> ProfileSelection -> Maybe String -> IO ()
profileAddCommand config opts storeDir dataDir target attrPath selection priorityArg = do
  upstreamManagedRefusal ReplacesUpstreamPaths storeDir >>= mapM_ (failWith . ("profile add: " <>))
  priority <- maybe (pure defaultPriority) priorityValue priorityArg
  profile <- chosenProfile config selection
  -- The profile's store and its manifest are checked before anything is
  -- evaluated, as upstream reads the manifest before it builds
  -- (CmdProfileAdd::run in profile.cc), so another store's profile,
  -- nix-env's, or a manifest that cannot be read is refused before a build.
  -- Each attempt at the change checks them again.
  checkProfileStore storeDir profile >>= either (failError . profileFailureText storeDir) pure
  readManifest profile >>= either (failError . profileFailureText storeDir) (const (pure ()))
  buildConfig <- buildConfigFor config opts storeDir
  logger <- cliLogger
  outcome <- withLocalStore logger buildConfig $ \ops -> do
    paths <- addedPaths config opts storeDir dataDir ops target attrPath
    let element = Element paths Active priority Nothing
    updateProfile logger ops storeDir profile (pure . Changed . addElement (elementIdentifier element) element)
  either (failError . profileFailureText storeDir) (const (pure ())) outcome

-- | A @--priority@ value: an int, with an optional sign, as upstream reads
-- one (@string2IntWithUnitPrefix@ in util.hh), refused in its words
-- otherwise.  Upstream also takes a binary unit suffix and narrows a 64-bit
-- value into an int; neither is taken here.
priorityValue :: String -> IO Int
priorityValue raw = maybe (failError ("'" <> T.pack raw <> "' is not an integer")) pure parsed
  where
    parsed = do
      value <- case raw of
        '-' : digits -> negate <$> decimal digits
        '+' : digits -> decimal digits
        digits -> decimal digits
      if value >= toInteger (minBound :: Int32) && value <= toInteger (maxBound :: Int32) then Just (fromInteger value) else Nothing
    decimal digits
      | not (null digits), all isDigit digits = readMaybe digits
      | otherwise = Nothing

-- | The store paths @profile add@ adds: the store path a file argument
-- leads to, in itself or through its links, when no attribute is selected
-- (upstream's @followLinksToStorePath@ in store-api.cc), which must be valid;
-- otherwise the outputs the target's derivation installs
-- ('installedOutputs'), built.
addedPaths :: NixConfig -> CliOpts -> StoreDir -> FilePath -> StoreOps IO -> Target -> Maybe T.Text -> IO (Set.Set StorePath)
addedPaths config opts storeDir dataDir ops target attrPath = do
  inStore <- case (target, attrPath) of
    (TargetFile raw, Nothing) -> storePathLedTo storeDir raw
    _ -> pure Nothing
  case inStore of
    Just path -> do
      held <- sqValidPaths (soQueries ops) (Set.singleton path)
      if Set.member path held
        then pure (Set.singleton path)
        else failError ("path '" <> T.pack (storePathToFilePath storeDir path) <> "' is not valid")
    Nothing -> builtOutputs config opts storeDir dataDir ops target attrPath

-- | How many links 'storePathLedTo' follows before it gives up, as
-- upstream's @followLinksToStore@ does (store-api.cc, from 2.34.9).
maxLinkFollows :: Int
maxLinkFollows = 1024

-- | The store path a path argument is in, made absolute against the
-- working directory without reading a link and then followed through
-- links until it is in the store, by the store's own spelling; 'Nothing'
-- when it reaches something in no store.
storePathLedTo :: StoreDir -> FilePath -> IO (Maybe StorePath)
storePathLedTo storeDir raw = do
  bytes <- commandLineBytes "PATH" raw
  start <- maybe (failError "PATH is not valid UTF-8, so it names no Windows path") pure (hostPathFromBytes bytes)
  cwd <- OsDir.getCurrentDirectory
  follow maxLinkFollows (absolutePath cwd start)
  where
    follow remaining path = do
      spelled <- OP.decodeFS path
      case parseStorePathPrefix storeDir (T.pack spelled) of
        Just sp -> pure (Just sp)
        Nothing -> do
          probed <- try (OsDir.pathIsSymbolicLink path)
          case probed of
            Right True
              | remaining <= 0 -> failError ("too many symbolic links encountered while resolving '" <> T.pack raw <> "'")
              | otherwise -> OsDir.getSymbolicLinkTarget path >>= follow (remaining - 1) . absolutePath (OP.takeDirectory path)
            Right False -> pure Nothing
            Left (_ :: IOException) -> pure Nothing

-- | Evaluate the target to a derivation and build it for the outputs it
-- installs, as @build@ evaluates and builds one, and give their paths.
builtOutputs :: NixConfig -> CliOpts -> StoreDir -> FilePath -> StoreOps IO -> Target -> Maybe T.Text -> IO (Set.Set StorePath)
builtOutputs config opts storeDir dataDir ops target attrPath = do
  (drvSP, outputs, evalObjects) <-
    evalTarget config opts storeDir (ReadWriteSession ops) dataDir target attrPath $ \st val -> do
      drvSP <- derivationPath val
      outputs <- evalOrExit st (installedOutputs val)
      (,,) drvSP outputs <$> sessionObjects st
  drv <- recordedDerivation (eoDerivations evalObjects) drvSP
  paths <- traverse (namedOutputPath drv) (Set.toList outputs)
  outcome <- runExceptT $ do
    ExceptT (first AddRefused <$> soAddEvalObjects ops evalObjects)
    ExceptT (first BuildStopped <$> soBuild ops [(drvSP, outputs)])
  either reportStop (const (pure (Set.fromList paths))) outcome

-- | The outputs @profile add@ installs of a derivation, as upstream's
-- @queryOutputs(false, true)@ chooses them (get-drvs.cc), and @out@ when that
-- chooses none (@InstallableAttrPath::toDerivedPaths@ in
-- installable-attr-path.cc): the output @outputName@ names when
-- @outputSpecified@ is true; else those @meta.outputsToInstall@ names, when
-- it passes upstream's @checkMeta@ (through @queryMeta@ in get-drvs.cc); else
-- every output @outputs@ lists, or @out@ when there is no @outputs@.
installedOutputs :: NixValue -> EvalIO (Set.Set T.Text)
installedOutputs val = case val of
  VAttrs attrs -> do
    outputs <- attrSetLookup "outputs" attrs >>= maybe (pure (Set.singleton outName)) (force >=> outputNames)
    specified <- attrSetLookup "outputSpecified" attrs >>= maybe (pure False) (force >=> boolean "the 'outputSpecified' attribute of a derivation")
    chosen <-
      if specified
        then do
          selected <- attrSetLookup "outputName" attrs >>= maybe (pure "") (force >=> text "the output name of a derivation")
          if Set.member selected outputs
            then pure (Set.singleton selected)
            else throwEvalError ("derivation does not have output '" <> selected <> "'")
        else toInstall attrs outputs
    pure (if Set.null chosen then Set.singleton outName else chosen)
  _ -> pure (Set.singleton outName)
  where
    outName = "out"
    outputNames value = case value of
      VList items -> clistThunks items >>= fmap Set.fromList . traverse (force . Thunk >=> text "the name of an output of a derivation")
      other -> throwEvalError ("expected a list but found " <> typeName other <> " while evaluating the 'outputs' attribute of a derivation")
    toInstall attrs outputs =
      attrSetLookup "meta" attrs >>= \case
        Nothing -> pure outputs
        Just metaThunk ->
          force metaThunk >>= \case
            VAttrs meta ->
              attrSetLookup "outputsToInstall" meta >>= \case
                Nothing -> pure outputs
                Just listed -> do
                  value <- force listed
                  usable <- checkMeta value
                  if not usable then pure outputs else namedIn outputs value
            other -> throwEvalError ("expected a set but found " <> typeName other <> " while evaluating the 'meta' attribute of a derivation")
    namedIn outputs value = case value of
      VList items -> clistThunks items >>= fmap Set.fromList . traverse (force . Thunk >=> installed outputs)
      _ -> badOutputsToInstall
    installed outputs value = case value of
      VStr bytes _
        | Right name <- TE.decodeUtf8' bytes, Set.member name outputs -> pure name
      _ -> badOutputsToInstall
    badOutputsToInstall = throwEvalError "this derivation has bad 'meta.outputsToInstall'"
    boolean what value = case value of
      VBool b -> pure b
      other -> throwEvalError ("expected a Boolean but found " <> typeName other <> " while evaluating " <> what)
    text what value = case value of
      VStr bytes _ | Right decoded <- TE.decodeUtf8' bytes -> pure decoded
      other -> throwEvalError ("expected a string but found " <> typeName other <> " while evaluating " <> what)

-- | Whether a @meta@ value is one upstream reads (@checkMeta@ in
-- get-drvs.cc): an int, a Boolean, a string or a float, or a list of such
-- values, or a set of them that is not a derivation, which has an @outPath@.
checkMeta :: NixValue -> EvalIO Bool
checkMeta value = case value of
  VList items -> clistThunks items >>= allChecked . map Thunk
  VAttrs attrs ->
    attrSetLookup "outPath" attrs >>= \case
      Just _ -> pure False
      Nothing -> attrSetElems attrs >>= allChecked
  VInt _ -> pure True
  VBool _ -> pure True
  VStr _ _ -> pure True
  VFloat _ -> pure True
  _ -> pure False
  where
    allChecked thunks = case thunks of
      [] -> pure True
      thunk : rest -> force thunk >>= checkMeta >>= \ok -> if ok then allChecked rest else pure False

-- | Take packages off a profile, as the matchers select them.  A store
-- directory upstream Nix's database also manages is refused first.
profileRemoveCommand :: NixConfig -> StoreDir -> ProfileSelection -> RemoveSelection -> IO ()
profileRemoveCommand config storeDir selection removals = do
  upstreamManagedRefusal ReplacesUpstreamPaths storeDir >>= mapM_ (failWith . ("profile remove: " <>))
  selector <- case removals of
    RemoveAll -> pure SelectAll
    RemoveSome matchers -> SelectSome <$> traverse (removalMatcher storeDir) matchers
  profile <- chosenProfile config selection
  logger <- cliLogger
  outcome <- withLocalStore logger (defaultBuildConfig storeDir) $ \ops -> updateProfile logger ops storeDir profile (removing selector)
  either (failError . profileFailureText storeDir) (const (pure ())) outcome
  where
    warn = TIO.hPutStrLn stderr . ("warning: " <>)
    -- Upstream's lines (CmdProfileRemove::run in profile.cc): each element
    -- taken off, then the counts.
    removing selector manifest@(Manifest elements) = do
      let Selection names warnings = selectElements selector manifest
      mapM_ warn warnings
      if Set.null names
        then Unchanged <$ warn "No packages to remove. Use 'nova-nix profile list' to see the current profile."
        else do
          let kept = Map.withoutKeys elements names
          mapM_ (\element -> TIO.hPutStrLn stderr ("removing '" <> elementIdentifier element <> "'")) (Map.restrictKeys elements names)
          TIO.hPutStrLn stderr ("removed " <> T.pack (show (Map.size elements - Map.size kept)) <> " packages, kept " <> T.pack (show (Map.size kept)) <> " packages")
          pure (Changed (Manifest kept))

-- | What a @profile remove@ selector matches (the @MixProfileElementMatchers@
-- constructor in profile.cc): a number is refused, as upstream refuses an
-- index, in words of nova-nix's own, since upstream's say it no longer takes
-- one, which describes upstream's history and not nova-nix's; a store path,
-- in the canonical spelling or the store's own, selects the elements holding
-- it; any other word, the element of that name.  A pattern that does not
-- compile is refused before the profile is read.
removalMatcher :: StoreDir -> RemoveMatcher -> IO Matcher
removalMatcher storeDir removal = case removal of
  RemoveRegex raw -> do
    regex <- argumentText "--regex" raw
    maybe (failError ("invalid regular expression '" <> regex <> "'")) pure (regexMatcher regex)
  RemoveNamed raw -> do
    named <- argumentText "NAME" raw
    case indexNumber raw of
      Just index -> failError ("'nova-nix profile' does not support indices ('" <> T.pack (show index) <> "')")
      Nothing -> pure (maybe (MatchName named) MatchStorePath (parseStorePath storeDir named <|> parseStorePath defaultStoreDir named))
  where
    -- Upstream's string2Int<size_t>: digits after an optional '+', within
    -- 64 bits (util.cc).
    indexNumber arg = do
      value <- case arg of
        '+' : digits -> decimal digits
        digits -> decimal digits
      if value <= toInteger (maxBound :: Word64) then Just value else Nothing
    decimal :: String -> Maybe Integer
    decimal digits
      | not (null digits), all isDigit digits = readMaybe digits
      | otherwise = Nothing

-- | A command-line argument's text, refused when it is not UTF-8.
argumentText :: T.Text -> String -> IO T.Text
argumentText what arg = commandLineBytes what arg >>= either (const (failError (what <> " is not valid UTF-8"))) pure . TE.decodeUtf8'

-- | Print what a profile holds: upstream's lines, or with @--json@ its
-- manifest, as 'printJson' prints it.
profileListCommand :: NixConfig -> StoreDir -> ProfileSelection -> ListFormat -> IO ()
profileListCommand config storeDir selection format = do
  profile <- chosenProfile config selection
  -- Upstream lists a profile of any store; here its paths would be
  -- printed under this store's spelling, naming paths it does not hold.
  checkProfileStore storeDir profile >>= either (failError . profileFailureText storeDir) pure
  manifest <- readManifest profile >>= either (failError . profileFailureText storeDir) pure
  case format of
    ListJson layout -> printJson layout (manifestJson manifest)
    ListText -> do
      styling <- stdoutStyling
      toStdout (mapM_ TIO.putStrLn (listLines styling storeDir manifest))

-- | Print each generation of a profile and what changed in it from the
-- one listed before it, as upstream's @profile history@ does.  A
-- generation's lines are printed once its manifest is read, so one that
-- cannot be read stops the command after the lines of those before it.
profileHistoryCommand :: NixConfig -> StoreDir -> ProfileSelection -> IO ()
profileHistoryCommand config storeDir selection = do
  profile <- chosenProfile config selection
  -- Upstream reads the generations of a profile of any store; here they
  -- would be read against a store that does not hold them, as for list.
  checkProfileStore storeDir profile >>= either (failError . profileFailureText storeDir) pure
  (dated, current) <- findGenerations profile >>= either (failError . profileFailureText storeDir) pure
  styling <- stdoutStyling
  let printGeneration previous entry = do
        let generation = datedGeneration entry
        manifest <- readManifest (generationPath generation) >>= either (failError . profileFailureText storeDir) pure
        toStdout (mapM_ TIO.putStrLn (historyLines styling current previous entry manifest))
        pure (Just (generationNumber generation, manifest))
  foldM_ printGeneration Nothing dated

-- | One generation's lines in a profile's history, as upstream prints
-- them: a blank line when a generation was listed before it,
-- @Version N (DATE)@, the date in UTC, then @ <- M@ for that earlier
-- generation and a colon, the number in green for the current generation
-- and in bold for the others, then what changed from the earlier
-- generation's manifest, or from none, each line indented by two spaces.
historyLines :: Styling -> Maybe GenerationNumber -> Maybe (GenerationNumber, Manifest) -> DatedGeneration -> Manifest -> [T.Text]
historyLines styling current previous (DatedGeneration (Generation number _) made) manifest =
  [T.empty | isJust previous]
    ++ ["Version " <> style styling numberStyle <> generationText number <> style styling ansiNormal <> " (" <> generationDate made <> ")" <> maybe T.empty ((" <- " <>) . generationText . fst) previous <> ":"]
    ++ map (indent <>) (if null changes then ["No changes."] else map changeLine changes)
  where
    changes = manifestChanges (maybe emptyManifest snd previous) manifest
    numberStyle
      | Just number == current = ansiGreen
      | otherwise = ansiBold
    indent = "  "

-- | A generation's number, as a message shows it.
generationText :: GenerationNumber -> T.Text
generationText (GenerationNumber n) = T.pack (show n)

-- | Switch a profile to the generation before its current one, or to the
-- one the last @--to@ names, as upstream's @profile rollback@ does.  Every
-- @--to@ is read, in order, and the first that does not read is refused, as
-- upstream reads each as it parses its flags; the default profile is
-- resolved first, as upstream resolves it before it parses them.
profileRollbackCommand :: NixConfig -> StoreDir -> ProfileSelection -> [String] -> Changes -> IO ()
profileRollbackCommand config storeDir selection toArgs changes = do
  profile <- chosenProfile config selection
  numbers <- either failError pure (traverse readGenerationNumber toArgs)
  let target = maybe PreviousGeneration GenerationAt (listToMaybe (reverse numbers))
  checkProfileStore storeDir profile >>= either (failError . profileFailureText storeDir) pure
  logger <- cliLogger
  switchGeneration logger profile target changes >>= either (failError . profileFailureText storeDir) pure

-- | Delete a profile's generations but the current one, or with
-- @--older-than@ those made before its cutoff but the newest of them, as
-- upstream's @profile wipe-history@ does.  The cutoff is taken from the
-- clock as the age is read, before the profile's lock is waited for, as
-- upstream takes it.
profileWipeHistoryCommand :: NixConfig -> StoreDir -> ProfileSelection -> Maybe String -> Changes -> IO ()
profileWipeHistoryCommand config storeDir selection olderThan changes = do
  profile <- chosenProfile config selection
  scope <- case olderThan of
    Nothing -> pure WipeEveryOld
    Just age -> do
      days <- either failError pure (parseOlderThan age)
      now <- currentEpochSeconds
      pure (WipeOlderThan (cutoffBefore now days))
  checkProfileStore storeDir profile >>= either (failError . profileFailureText storeDir) pure
  logger <- cliLogger
  wipeHistory logger profile scope changes >>= either (failError . profileFailureText storeDir) pure

-- | Whether a line printed on stdout keeps the escapes that style it.
data Styling = Styled | Plain

-- | Whether this run's stdout takes the escapes that style a line, as
-- upstream decides for its stdout: never while @NO_COLOR@ or @NOCOLOR@ is
-- set, always while @CLICOLOR_FORCE@ or @FORCE_COLOR@ is, and otherwise
-- when stdout is a terminal and @TERM@ is set to something other than
-- @dumb@.  A variable counts as set whatever its value, an empty one
-- included.
stdoutStyling :: IO Styling
stdoutStyling = do
  uncoloured <- anySet ["NO_COLOR", "NOCOLOR"]
  forced <- anySet ["CLICOLOR_FORCE", "FORCE_COLOR"]
  terminal <- hIsTerminalDevice stdout
  term <- lookupEnvBytes "TERM"
  pure (if not uncoloured && (forced || terminal && rendersColour term) then Styled else Plain)
  where
    anySet = fmap (any (/= EnvUnset)) . traverse lookupEnvBytes
    rendersColour term = case term of
      EnvUnset -> False
      EnvValue value -> value /= "dumb"
      EnvUnpairedSurrogate -> True

-- | An escape as a styled line holds it: itself when stdout takes them,
-- and nothing otherwise, where upstream writes the line with every escape
-- taken out.
style :: Styling -> T.Text -> T.Text
style styling escape = case styling of
  Styled -> escape
  Plain -> T.empty

-- | Print JSON as upstream's @printJSON@ prints it
-- (common-args.cc, from 2.34.9): indented or on one line as the layout says,
-- by default indented when stdout is a terminal, and followed by a newline,
-- as @writeToStdout@ ends what it writes.
printJson :: JsonLayout -> Json -> IO ()
printJson layout json = do
  indented <- case layout of
    IndentedOnTerminal -> hIsTerminalDevice stdout
    Indented -> pure True
    Compact -> pure False
  toStdout (BS.putStr (if indented then renderIndentedJson json else renderJson json <> "\n"))

-- | The escapes upstream sets text in bold, red or green, and back with.
ansiBold, ansiRed, ansiGreen, ansiNormal :: T.Text
ansiBold = "\ESC[1m"
ansiRed = "\ESC[31;1m"
ansiGreen = "\ESC[32;1m"
ansiNormal = "\ESC[0m"

-- | What @profile list@ prints, as upstream prints it: for each element
-- its name, in bold, with @(inactive)@, in red, after an inactive one's,
-- its flake's lines for one a flake made, and its paths as the store spells
-- them, with a blank line between elements.  The escapes are written only
-- where stdout takes them ('stdoutStyling'); a name holding an escape of its
-- own keeps it, where upstream takes every escape out of the line.
listLines :: Styling -> StoreDir -> Manifest -> [T.Text]
listLines styling storeDir (Manifest elements) = intercalate [""] (map elementLines (Map.toAscList elements))
  where
    elementLines (name, element) =
      ("Name:               " <> style styling ansiBold <> name <> style styling ansiNormal <> activity (elementActivity element))
        : maybe [] sourceLines (elementSource element)
        ++ ["Store paths:        " <> T.unwords (sort [T.pack (storePathToFilePath storeDir path) | path <- Set.toList (elementPaths element)])]
    activity Active = ""
    activity Inactive = " " <> style styling ansiRed <> "(inactive)" <> style styling ansiNormal
    sourceLines source =
      [ "Flake attribute:    " <> flakeAttribute source,
        "Original flake URL: " <> sourceOriginalUrl source,
        "Locked flake URL:   " <> sourceUrl source
      ]

-- | A profile's failure in words.  A conflict is upstream's, with
-- nova-nix's commands in its hints (the @BuildEnvFileConflictError@ handler
-- of @CmdProfileAdd::run@ in profile.cc).
profileFailureText :: StoreDir -> ProfileFailure -> T.Text
profileFailureText storeDir failure = case failure of
  ProfileIOFailed reason -> reason
  ManifestFailed path manifestError -> manifestErrorText path manifestError
  NixEnvProfile profile -> "profile " <> quotedPath profile <> " was made by nix-env, whose manifest.nix nova-nix does not read"
  ForeignGeneration profile current ->
    "profile "
      <> quotedPath profile
      <> " is in another store: its current generation, "
      <> quotedPath current
      <> ", is not in the store \""
      <> T.pack (unStoreDir storeDir)
      <> "\"; name that store with --store, or another profile with --profile"
  TreeFailed treeFailure -> buildEnvFailureText treeFailure
  TreeConflict (Conflict (ConflictSide existingFile existingName _) (ConflictSide newFile _ newPaths) priority) ->
    T.concat
      [ "An existing package already provides the following file:\n\n  ",
        quotedPath existingFile,
        "\n\nThis is the conflicting file from the new package:\n\n  ",
        quotedPath newFile,
        "\n\nTo remove the existing package:\n\n  nova-nix profile remove ",
        existingName,
        "\n\nThe new package can also be added next to the existing one by assigning a different priority.\nThe conflicting packages have a priority of ",
        T.pack (show priority),
        ".\nTo prioritise the new package:\n\n  nova-nix profile add ",
        T.unwords (Set.toAscList newPaths),
        " --priority ",
        T.pack (show (priority - 1)),
        "\n\nTo prioritise the existing package:\n\n  nova-nix profile add ",
        T.unwords (Set.toAscList newPaths),
        " --priority ",
        T.pack (show (priority + 1)),
        "\n"
      ]
  StoreRefused reason -> reason
  NoSuchGeneration number -> "profile version " <> generationText number <> " does not exist"
  NoOlderGeneration current -> "no profile version older than the current (" <> generationText (fromMaybe (GenerationNumber 0) current) <> ") exists"
  where
    quotedPath path = "\"" <> hostPathText path <> "\""

-- | Fail with an error as upstream prints one: @error: @ and the message,
-- its later lines indented under the first.
failError :: T.Text -> IO a
failError = failWith . renderEvalFailure . EvalFailure []

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
    -- DB rows are spelled with the opened store's directory, which under
    -- --store is not platformStoreDir, so each row parses against it.
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

-- | With @--strict@, force everything inside the result before it prints,
-- as nix-instantiate does (@processExpr@ in nix-instantiate.cc).  The value
-- itself is what prints: the IO evaluator keeps each forced value in its
-- cell, where the printer reads it, so a set or list the result holds twice
-- is still one set or list, and prints as repeated the second time.  Without
-- @--strict@, an element or attribute never forced prints as upstream's thunk
-- marker.
finalize :: Bool -> NixValue -> EvalIO NixValue
finalize True val = val <$ deepForce val
finalize False val = pure val

-- | Write an eval result and its newline to stdout as bytes.  A string's
-- payload need not be UTF-8, so nothing on this path decodes or re-encodes
-- it: the handle is put in binary mode, as hPutBuilder asks, and no
-- encoding or newline translation applies.
printResult :: NixValue -> IO ()
printResult val = toStdout $ do
  hSetBinaryMode stdout True
  printed <- printAmbiguous val
  BB.hPutBuilder stdout (printed <> BB.char7 '\n')
