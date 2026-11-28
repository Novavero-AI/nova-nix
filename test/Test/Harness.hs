{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | What the test modules share: the result type, its runners and
-- assertions, evaluation under the pure and the IO evaluator, and the
-- fixtures the tests of more than one module use.
module Test.Harness
  ( TestResult (..),
    runTest,
    runTestM,
    assertEqual,
    andThen,
    bytesText,
    storePathStringValue,
    fetchedStorePath,
    assertRight,
    assertLeft,
    testBaseDir,
    testOrigin,
    testPos,
    assertParse,
    assertParseIn,
    withGlobals,
    evalNix,
    evalNixBytes,
    parseErrorTag,
    assertEval,
    assertEvalFail,
    assertEvalError,
    assertEvalErrorIO,
    assertParseFail,
    findTestShell,
    repeatedMarker,
    selfHoldingDerivation,
    selfHoldingPrinted,
    selfHoldingPrintedStrict,
    censusStoreHash,
    setLayerRhsMax,
    setMaxLayers,
    sitesAdded,
    evalNixIOStoreDir,
    evalNixIO,
    testEvalState,
    testSessionStore,
    evalDerivationIO,
    evalDerivationIOStore,
    runTestIO,
    nixQuotedPath,
    symlinksAvailable,
    walkWatchdogMicros,
    positionsFile,
    evalNixInFile,
    jsonInFile,
    positionJson,
    envSpelling,
    genJson,
    pureDepGraph,
    chunkReader,
    removeIfExists,
    quotedAsCxx,
    throwawayKey,
    withTempStore,
    deleteNamed,
    forceRemoveIfExists,
    tarPathOrDie,
    tarDir,
    tarFile,
    profileFixture,
    narSpecStr,
    narSpecVector,
    narSpecFileVector,
    offeredThenFetched,
    offeredSubstitute,
    servedPaths,
    narInfoRequest,
    narRequest,
    sessionClosure,
    fixtureGit,
    makeGitFixture,
    evalNixIOStore,
    evalNixIOIn,
    canonicalAs,
    simpleTestDrv,
    genDrvName,
    genFixturePath,
    genTextOf,
    testDrvPath,
    mkTestBuildDrv,
    hostToolsEnv,
    withHostTools,
    hostToolsAttrs,
    evalAndBuild,
    unexpectedOps,
    trustPe,
    evalChildFlag,
    evalChild,
    emits,
    inTestSession,
    posixOnlyEntries,
    decodeFormals,
    compiledBindings,
    sigTestKeyText,
    sigTestWrongKeyText,
    signedTestNarInfo,
    asciiOsPath,
    upstreamBaseEnvNames,
    purePolicy,
    restrictedPolicy,
    evalNixWith,
    withEnvVar,
    testLogger,
    genSignatures,
    genContentAddress,
  )
where

import qualified Codec.Archive.Tar.Entry as TarEntry
import Control.Exception (SomeAsyncException (..), SomeException, bracket, fromException, throwIO, try)
import Control.Monad (filterM, unless, when)
import Control.Monad.State.Strict (evalStateT)
import Data.Bits (shiftR, (.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.Functor.Identity (Identity (..))
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word32, Word64, Word8)
import qualified GHC.Foreign as GHCForeign
import GHC.IO.Encoding (getFileSystemEncoding)
import HttpFixture (Reply (..), httpResponse)
import Nix.Builder (BuildConfig (..), BuildResult (..), BuildStop (..), buildDerivation, defaultBuildConfig)
import Nix.Builtins (builtinEnv, rootScopeNames)
import qualified Nix.Daemon.Protocol as Proto
import qualified Nix.DependencyGraph as DepGraph
import Nix.Derivation (Derivation (..), DerivationOutput (..), Platform (..), currentPlatform, platformToText)
import Nix.Eval (EvalPolicy (..), MonadEval (..), NixValue (..), StringContext (..), StringContextElement (..), attrSetLookup, evalParsed, force, mkStr, runPureEval, typeName, unrestrictedPolicy)
import Nix.Eval.CBytecode (cbcArg1, cbcCountedPayload)
import Nix.Eval.CRegion (Regions (..))
import Nix.Eval.CStats (EnvSiteFigures (..), SetSiteFigures (..), SiteCounts (..), siteCounts)
import Nix.Eval.Compile (BcBindings (..), decodeBcBindings, decodeBcFormals)
import Nix.Eval.EvalFormals (EvalFormals (..))
import Nix.Eval.IO (EvalState (..), SessionSettings (..), SessionStore (..), evalBaseEnv, newEvalStateWith, runEvalIO, sourceCopies, withEvalSession)
import Nix.Expr.Resolve (staticGlobalNames, undefinedVariableMessage)
import Nix.Expr.Types
import Nix.Hash (FileIngestionMethod (..), hashAlgoBytes)
import Nix.Http (FetchRetryPolicy (..), RetryEffects (..))
import Nix.Json (Json (..))
import Nix.Logger (EscapeFilter (..), Logger, handleLogger)
import Nix.Parser (SourceError (..), parseNix, parseNixWithScope, parsedExpr)
import Nix.Push (storePathBasename)
import Nix.Push.Directory (ed25519SeedBytes)
import qualified Nix.Realise as Realise
import Nix.Store (DeleteOutcome (..), Store (..), closeStore, deleteStorePathRaw, materializeEvalSources, materializeEvalStoreWrites, openStore, storeEntryName)
import Nix.Store.Metadata (storeModificationTime)
import Nix.Store.Ops (StoreOps (..), StoreQueries (..), localQueries, withLocalStore)
import Nix.Store.Path (StoreDir (..), StorePath, defaultStoreDir, parseStorePath, storePathToText)
import Nix.Store.Path.Internal (StorePath (..))
import qualified Nix.Substituter as Subst
import NodeStatus (NodeStatus (..), nodeStatus)
import qualified NovaCache.Base64 as B64
import qualified NovaCache.Hash as CHash
import qualified NovaCache.NAR as NAR
import qualified NovaCache.NarInfo as NarInfo
import qualified NovaCache.Signing as Signing
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getPermissions, getTemporaryDirectory, removeDirectoryRecursive, writable)
import qualified System.Directory as Dir
import System.Environment (getExecutablePath, lookupEnv, unsetEnv)
import qualified System.Environment.Blank as Blank
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import qualified System.Info as SI
import System.OsPath (OsPath)
import qualified System.OsPath as OP
import qualified System.Process as Proc
import qualified Test.QuickCheck as QC

-- ---------------------------------------------------------------------------
-- Test harness
-- ---------------------------------------------------------------------------

data TestResult = Pass | Fail !Text

runTest :: Text -> TestResult -> IO Bool
runTest name result = case result of
  Pass -> do
    putStrLn $ "  PASS  " ++ T.unpack name
    pure True
  Fail msg -> do
    putStrLn $ "  FAIL  " ++ T.unpack name ++ ": " ++ T.unpack msg
    pure False

-- | Like 'runTest' but for tests that need IO to produce their result.
-- A synchronous exception escaping the action becomes one FAIL line
-- instead of aborting the whole suite (which would skip every later group
-- and its cleanup).  An asynchronous one, an interrupt or a killed
-- thread, stops the suite, as it stops any program.
runTestM :: Text -> IO TestResult -> IO Bool
runTestM name action = do
  outcome <- try action
  case outcome of
    Left (e :: SomeException)
      | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
      | otherwise -> runTest name (Fail ("uncaught exception: " <> T.pack (show e)))
    Right result -> runTest name result

assertEqual :: (Eq a, Show a) => Text -> a -> a -> TestResult
assertEqual label expected actual
  | expected == actual = Pass
  | otherwise =
      Fail $
        label
          <> ": expected "
          <> T.pack (show expected)
          <> " but got "
          <> T.pack (show actual)

-- | The first failure of two results, or 'Pass' when both pass.
andThen :: TestResult -> TestResult -> TestResult
andThen first second = case first of
  Pass -> second
  failed -> failed

-- | Render string-value bytes in a failure message (display-only decode).
bytesText :: BS.ByteString -> Text
bytesText = TE.decodeUtf8With lenientDecode

-- | A store path as a fetcher or a source importer returns it: its text,
-- carrying itself as context.
storePathStringValue :: StorePath -> NixValue
storePathStringValue sp = VStr (TE.encodeUtf8 (storePathToText defaultStoreDir sp)) (StringContext (Set.singleton (SCPlain sp)))

-- | The store path a fetcher's result names, when it is the string with
-- itself as context that upstream's fetchers return.
fetchedStorePath :: NixValue -> Maybe StorePath
fetchedStorePath val = case val of
  VStr bytes _ -> do
    sp <- parseStorePath defaultStoreDir (bytesText bytes)
    if val == storePathStringValue sp then Just sp else Nothing
  _ -> Nothing

assertRight :: (Show e) => Text -> Either e a -> (a -> TestResult) -> TestResult
assertRight label result check = case result of
  Left err -> Fail (label <> ": got error: " <> T.pack (show err))
  Right val -> check val

assertLeft :: (Show a) => Text -> Either e a -> TestResult
assertLeft _ (Left _) = Pass
assertLeft label (Right val) = Fail (label <> ": expected error but got: " <> T.pack (show val))

-- | What a test expression's relative path literals resolve against.
-- Absolute, because that is the invariant the parser establishes: no
-- relative path literal survives parsing, so nothing downstream has to know
-- what one would have meant.
testBaseDir :: FilePath
testBaseDir = "/nova-nix-test"

-- | Where a test expression's source comes from: text with no file, as an
-- @--expr@ argument is.
testOrigin :: SourceOrigin
testOrigin = SourceText "<test>"

-- | A position in a test expression's source.
testPos :: Int -> Int -> SrcPos
testPos = SrcPos testOrigin

-- | Helper: parse and check result.
assertParse :: Text -> Text -> Expr -> TestResult
assertParse = assertParseIn []

-- | 'assertParse' for source whose free variables are bound around it, the
-- way @scopedImport@ binds its scope: each stays a name-based 'EVar'.
assertParseIn :: [Text] -> Text -> Text -> Expr -> TestResult
assertParseIn names label source expected =
  assertRight label (parsedExpr <$> parseNixWithScope (withGlobals names) testBaseDir testOrigin (TE.encodeUtf8 source)) $ \actual ->
    assertEqual label expected actual

-- | The root environment's names with more bound around them.
withGlobals :: [Text] -> Set.Set Text
withGlobals names = Set.union (Set.fromList names) staticGlobalNames

-- | Helper: parse Nix source and evaluate with builtinEnv.  Parse errors
-- are tagged so failure assertions can tell them apart from eval errors.
evalNix :: Text -> Either Text NixValue
evalNix = evalNixBytes . TE.encodeUtf8

-- | 'evalNix' over source bytes, which need not be UTF-8.
evalNixBytes :: BS.ByteString -> Either Text NixValue
evalNixBytes source = case parseNix testBaseDir testOrigin source of
  Left err -> Left (sourceFailure err)
  Right parsed -> runPureEval (evalParsed (builtinEnv unrestrictedPolicy 0 []) parsed)

-- | Prefix marking a parse (not eval) failure in 'evalNix' results.
parseErrorTag :: Text
parseErrorTag = "parse error: "

-- | A source failure as an 'evalNix' result: a syntax error carries
-- 'parseErrorTag'; an unbound variable reads as the evaluation error
-- upstream raises for it, before evaluating anything.
sourceFailure :: SourceError -> Text
sourceFailure (SyntaxError err) = parseErrorTag <> T.pack (show err)
sourceFailure (UndefinedVariable name) = undefinedVariableMessage name

-- | Assert that a Nix expression evaluates to the expected value.
assertEval :: Text -> Text -> NixValue -> TestResult
assertEval label source expected =
  assertRight label (evalNix source) $ \actual ->
    assertEqual label expected actual

-- | Assert that a Nix expression parses but fails to EVALUATE.  A parse
-- failure fails the assertion: a test documenting a runtime error must
-- not keep passing after a regression stops the construct from parsing.
assertEvalFail :: Text -> Text -> TestResult
assertEvalFail label source = case evalNix source of
  Left err
    | parseErrorTag `T.isPrefixOf` err ->
        Fail (label <> ": expected an eval error but the source did not parse: " <> err)
    | otherwise -> Pass
  Right val -> Fail (label <> ": expected eval failure but got: " <> T.pack (show val))

-- | Assert that a Nix expression parses and fails to evaluate with exactly
-- the given message, for errors whose wording is upstream's.
assertEvalError :: Text -> Text -> Text -> TestResult
assertEvalError label source expected = case evalNix source of
  Left err
    | parseErrorTag `T.isPrefixOf` err ->
        Fail (label <> ": expected an eval error but the source did not parse: " <> err)
    | otherwise -> assertEqual label expected err
  Right val -> Fail (label <> ": expected eval failure but got: " <> T.pack (show val))

-- | 'assertEvalError' under the IO evaluator, which keeps a forced
-- thunk's value and marks one while it is being forced, as upstream's
-- evaluator does; the pure one does neither.
assertEvalErrorIO :: FilePath -> Text -> Text -> Text -> IO TestResult
assertEvalErrorIO baseDir label source expected = do
  result <- evalNixIO baseDir source
  pure $ case result of
    Left err -> assertEqual label expected err
    Right val -> Fail (label <> ": expected eval failure but got: " <> T.pack (show val))

-- | Assert that a Nix expression fails at PARSE time.
assertParseFail :: Text -> Text -> TestResult
assertParseFail label source = case evalNix source of
  Left err
    | parseErrorTag `T.isPrefixOf` err -> Pass
    | otherwise ->
        Fail (label <> ": expected a parse error but evaluation failed instead: " <> err)
  Right val -> Fail (label <> ": expected parse failure but got: " <> T.pack (show val))

-- ---------------------------------------------------------------------------
-- Shell discovery for builder tests
-- ---------------------------------------------------------------------------

-- | Find a POSIX-compatible shell for builder tests.
-- On Unix, always @\/bin\/sh@.  On Windows, searches for @bash.exe@
-- at known Git for Windows locations first, then PATH.  Checks known
-- paths first to avoid picking up the WSL launcher at
-- @C:\\Windows\\System32\\bash.exe@ which exits 1 when WSL is not
-- configured.  Real Nix builders always use bash from the store -
-- this bridges the gap until nova-nix bootstraps its own bash
-- derivation.
findTestShell :: IO Text
findTestShell = case SI.os of
  "mingw32" -> do
    let known =
          [ "C:\\Program Files\\Git\\bin\\bash.exe",
            "C:\\Program Files (x86)\\Git\\bin\\bash.exe"
          ]
    found <- filterM Dir.doesFileExist known
    case found of
      (p : _) -> pure (T.pack p)
      [] -> do
        inPath <- Dir.findExecutable "bash"
        case inPath of
          Just p -> pure (T.pack p)
          Nothing ->
            error
              "bash not found: install Git for Windows or add bash to PATH"
  _ -> pure "/bin/sh"

-- ---------------------------------------------------------------------------
-- Tests: Eval - Values reached twice
-- ---------------------------------------------------------------------------

-- | Upstream's repeated marker, @printRepeated@ (print.cc at 2.24.9).
repeatedMarker :: Text
repeatedMarker = "\x00ABrepeated\x00BB"

-- | A derivation with one output, which its @all@ and @out@ attributes
-- hold, so the derivation holds itself.
selfHoldingDerivation :: Text
selfHoldingDerivation = "derivation { name = \"a\"; system = \"x\"; builder = \"/bin/sh\"; }"

-- | What @nix-instantiate --eval@ 2.24.9 prints for
-- 'selfHoldingDerivation', as its wrapper (primops/derivation.nix) and
-- @printAmbiguous@ make it: forcing the result forces the arguments and
-- the output's set and neither @all@ nor the paths, and the output
-- attribute is the result's own set, printed first, so it prints as
-- repeated.
selfHoldingPrinted :: Text
selfHoldingPrinted =
  "{ all = <CODE>; builder = \"/bin/sh\"; drvAttrs = { builder = \"/bin/sh\"; name = \"a\"; system = \"x\"; }; drvPath = <CODE>; name = \"a\"; out = "
    <> repeatedMarker
    <> "; outPath = <CODE>; outputName = \"out\"; system = \"x\"; type = \"derivation\"; }"

-- | What @nix-instantiate --eval --strict@ 2.24.9 prints for
-- 'selfHoldingDerivation'.  The drvPath is 2.24.9's, and the outPath is
-- the one that .drv names: the drvPath hashes the .drv's text, which
-- holds it.
selfHoldingPrintedStrict :: Text
selfHoldingPrintedStrict =
  "{ all = [ "
    <> repeatedMarker
    <> " ]; builder = \"/bin/sh\"; drvAttrs = { builder = \"/bin/sh\"; name = \"a\"; system = \"x\"; }; drvPath = \"/nix/store/l9ldjc53ry43x24hvq73jx7dx0489mhj-a.drv\"; name = \"a\"; out = "
    <> repeatedMarker
    <> "; outPath = \"/nix/store/0pd4llb3k36fnppfxwp6nlwsxwlv0y2c-a\"; outputName = \"out\"; system = \"x\"; type = \"derivation\"; }"

-- ---------------------------------------------------------------------------
-- Tests: Eval - frames tied in C
-- ---------------------------------------------------------------------------

-- | A store path hash: 32 characters of the base-32 alphabet.
censusStoreHash :: Text
censusStoreHash = "0123456789abcdfghijklmnpqrsvwxyz"

-- | The most entries a right operand of @//@ has for the result to be a
-- layer, and the most sets a chain of layers holds:
-- @NN_ATTRSET_LAYER_RHS_MAX@ on a 64-bit target and
-- @NN_ATTRSET_MAX_LAYERS@ (cbits/nn_attrset.h).
setLayerRhsMax, setMaxLayers :: Int
setLayerRhsMax = 16
setMaxLayers = 8

-- | The figures of each site and size that counting by site adds while
-- an action runs; a site or size that gains nothing is left out.
sitesAdded :: IO () -> IO (Either Text SiteCounts)
sitesAdded action = do
  before <- siteCounts
  action
  after <- siteCounts
  pure $ case (before, after) of
    (Just earlier, Just later) ->
      Right
        SiteCounts
          { sitesEnvs = gained envMinus (EnvSiteFigures 0 0 0) (sitesEnvs later) (sitesEnvs earlier),
            sitesEnvSizes = gained (-) 0 (sitesEnvSizes later) (sitesEnvSizes earlier),
            sitesSets = gained setMinus (SetSiteFigures 0 0 0) (sitesSets later) (sitesSets earlier),
            sitesUpdateCopied = sitesUpdateCopied later - sitesUpdateCopied earlier
          }
    _ -> Left "counting by site is off"
  where
    gained minus none later earlier =
      Map.filter (/= none) (Map.mapWithKey (\k figures -> maybe figures (minus figures) (Map.lookup k earlier)) later)
    envMinus (EnvSiteFigures a b c) (EnvSiteFigures x y z) = EnvSiteFigures (a - x) (b - y) (c - z)
    setMinus (SetSiteFigures a b c) (SetSiteFigures x y z) = SetSiteFigures (a - x) (b - y) (c - z)

-- ---------------------------------------------------------------------------
-- Tests: import and IO builtins (IO)
-- ---------------------------------------------------------------------------

-- | The per-suite temp store 'evalNixIO' evaluates against, exposed so
-- a test inspecting materialized trees resolves the same directory.
evalNixIOStoreDir :: IO StoreDir
evalNixIOStoreDir = do
  tmpBase <- getTemporaryDirectory
  pure (StoreDir (tmpBase </> "nova-nix-test-eval-store"))

-- | IO eval against a per-suite temp store, so no test needs a real,
-- writable platform store root: eval-time store writes land under the
-- temp directory, and the suite runs everywhere without provisioning.
evalNixIO :: FilePath -> Text -> IO (Either Text NixValue)
evalNixIO baseDir source = do
  storeDir <- evalNixIOStoreDir
  evalNixIOStore storeDir baseDir source

-- | An evaluation state with no search path, whatever @NIX_PATH@ the suite
-- runs under.  'nixPathSearchPath' makes @NIX_PATH@'s entries, interning
-- their strings, so a test whose result depends on the order names were
-- interned in would otherwise depend on the machine; a test of
-- @NIX_PATH@ itself uses 'evalNixIOWithNixPath'.
testEvalState :: StoreDir -> FilePath -> IO EvalState
testEvalState storeDir = newEvalStateWith testLogger unrestrictedPolicy [] (testSessionStore storeDir) storeDir

-- | The store a test's evaluation realises contexts against: the local
-- store in the directory, read-write as a build's session is, so what
-- evaluation writes reads back.  Each operation opens the store for
-- itself and closes it ('eachOpening'), since a test's state is made
-- outside any bracket.
testSessionStore :: StoreDir -> SessionStore
testSessionStore storeDir = ReadWriteSession (eachOpening (defaultBuildConfig storeDir))

-- | The local store's operations, each opening the store for itself and
-- closing it once it is done.
eachOpening :: BuildConfig -> StoreOps IO
eachOpening config =
  StoreOps
    { soQueries =
        StoreQueries
          { sqValidPaths = \paths -> withLocalStore testLogger config (\ops -> sqValidPaths (soQueries ops) paths),
            sqReferences = \path -> withLocalStore testLogger config (\ops -> sqReferences (soQueries ops) path)
          },
      soAddEvalObjects = \objects -> withLocalStore testLogger config (`soAddEvalObjects` objects),
      soAddSourceTree = \name refs entry -> withLocalStore testLogger config (\ops -> soAddSourceTree ops name refs entry),
      soBuild = \requests -> withLocalStore testLogger config (`soBuild` requests),
      soCheck = \drvPath -> withLocalStore testLogger config (`soCheck` drvPath),
      soAddOutLinkRoot = \link path -> withLocalStore testLogger config (\ops -> soAddOutLinkRoot ops link path)
    }

-- | IO eval of a derivation expression to the 'Derivation' the session
-- recorded for it: forcing the expression's @drvPath@ runs derivationStrict,
-- which records the @.drv@ under that path, and the recipe is read back the
-- way the build driver reads it.  Only the IO evaluator keeps the record.
evalDerivationIO :: FilePath -> Text -> IO (Either Text Derivation)
evalDerivationIO baseDir source = do
  storeDir <- evalNixIOStoreDir
  evalDerivationIOStore storeDir baseDir source

-- | 'evalDerivationIO' against an explicit store directory.
evalDerivationIOStore :: StoreDir -> FilePath -> Text -> IO (Either Text Derivation)
evalDerivationIOStore storeDir baseDir source = case parseNix baseDir testOrigin (TE.encodeUtf8 ("(" <> source <> ").drvPath")) of
  Left err -> pure (Left (T.pack (show err)))
  Right parsed -> do
    st <- testEvalState storeDir baseDir
    runEvalIO st $ do
      pathVal <- evalParsed (pure (evalBaseEnv st)) parsed
      case pathVal of
        VStr pathBytes _ -> do
          recorded <- lookupSessionDrv (bytesText pathBytes)
          maybe (throwEvalError ("no .drv was recorded for " <> bytesText pathBytes)) pure recorded
        other -> throwEvalError ("expected a drvPath string, got " <> typeName other)

-- | Run a named IO eval test - single label, no double-wrapping.
runTestIO :: Text -> FilePath -> Text -> NixValue -> IO Bool
runTestIO label baseDir source expected = do
  result <- evalNixIO baseDir source
  runTest label $ assertRight label result $ \actual ->
    assertEqual label expected actual

-- | Quoted path literal for embedding absolute paths in Nix source.
nixQuotedPath :: FilePath -> Text
nixQuotedPath p = T.pack (show p)

-- | Whether this host can create symlinks (Windows needs Developer Mode
-- or elevation).  The symlink-walk fixtures cannot be built without it,
-- so their groups skip loudly rather than fail.
symlinksAvailable :: IO Bool
symlinksAvailable = do
  tmpBase <- getTemporaryDirectory
  let probeDir = tmpBase </> "nova-nix-test-link-probe"
  Dir.removePathForcibly probeDir
  createDirectoryIfMissing True probeDir
  BS.writeFile (probeDir </> "target.txt") "t"
  outcome <- try (Dir.createFileLink "target.txt" (probeDir </> "link"))
  Dir.removePathForcibly probeDir
  pure $ case (outcome :: Either SomeException ()) of
    Left _ -> False
    Right () -> True

-- | Watchdog for cycle-termination tests: with symlinks as leaves every
-- walk returns promptly, while a regression to link-following recurses
-- forever - and a hung suite is worse than a failed one.
walkWatchdogMicros :: Int
walkWatchdogMicros = 30 * 1000 * 1000

-- ---------------------------------------------------------------------------
-- Tests: builtins.unsafeGetAttrPos
-- ---------------------------------------------------------------------------

-- | The file 'evalNixInFile' parses its source as.
positionsFile :: Text
positionsFile = "/nova-nix-test/positions.nix"

-- | 'evalNix' for source parsed as the file 'positionsFile', so the
-- positions recorded in it name that file, as upstream's do for a file.
evalNixInFile :: Text -> Either Text NixValue
evalNixInFile source = case parseNix testBaseDir (SourceFile positionsFile) (TE.encodeUtf8 source) of
  Left err -> Left (sourceFailure err)
  Right parsed -> runPureEval (evalParsed (builtinEnv unrestrictedPolicy 0 []) parsed)

-- | Assert that @builtins.toJSON@ of source parsed as 'positionsFile' is
-- the expected JSON.  toJSON pins the set @EvalState::mkPos@ builds: its
-- three attributes only, the file a string and the line and column
-- integers.
jsonInFile :: Text -> Text -> Text -> IO Bool
jsonInFile label source expected =
  runTest label $
    assertRight label (evalNixInFile ("builtins.toJSON (" <> source <> ")")) $ \actual ->
      assertEqual label (mkStr expected) actual

-- | The JSON of the position set for a line and column in a file.
positionJson :: Text -> Int -> Int -> Text
positionJson file line column =
  "{\"column\":" <> T.pack (show column) <> ",\"file\":\"" <> file <> "\",\"line\":" <> T.pack (show line) <> "}"

-- ---------------------------------------------------------------------------
-- Tests: environment values as bytes
-- ---------------------------------------------------------------------------

-- | The 'String' that base's environment setters and file functions, and
-- a spawned process's environment, write as the given bytes: on POSIX
-- they encode with the file-system encoding, whose round-trip escape
-- restores every byte its decoding escaped, and on Windows they write
-- UTF-16.
envSpelling :: BS.ByteString -> IO (Maybe String)
envSpelling bytes
  | SI.os == "mingw32" = pure (either (const Nothing) (Just . T.unpack) (TE.decodeUtf8' bytes))
  | otherwise = do
      encoding <- getFileSystemEncoding
      Just <$> BS.useAsCStringLen bytes (GHCForeign.peekCStringLen encoding)

-- ---------------------------------------------------------------------------
-- Tests: structured attributes and the JSON they travel as
-- ---------------------------------------------------------------------------

-- | JSON values up to a nesting depth, strings drawn from what the
-- renderer escapes and what it passes through as UTF-8, numbers across
-- both integer types and finite floats.
genJson :: Int -> QC.Gen Json
genJson depth = QC.oneof (scalars <> [nested | depth > 0])
  where
    scalars =
      [ pure JsonNull,
        JsonBool <$> QC.arbitrary,
        JsonInt . toInteger <$> (QC.arbitrary :: QC.Gen Int64),
        JsonInt . toInteger <$> (QC.arbitrary :: QC.Gen Word64),
        JsonFloat <$> (QC.arbitrary `QC.suchThat` (\d -> not (isNaN d || isInfinite d))),
        JsonString <$> genJsonText
      ]
    nested =
      QC.oneof
        [ JsonArray <$> QC.resize 4 (QC.listOf (genJson (depth - 1))),
          JsonObject . Map.fromList <$> QC.resize 4 (QC.listOf ((,) <$> genJsonText <*> genJson (depth - 1)))
        ]

-- | Text mixing the characters JSON escapes with plain and multi-byte ones.
genJsonText :: QC.Gen Text
genJsonText =
  T.pack <$> QC.listOf (QC.elements ['"', '\\', '/', '\b', '\f', '\n', '\r', '\t', '\0', '\31', '\127', 'a', ' ', '\233', '\8364', '\128512'])

-- ---------------------------------------------------------------------------
-- Tests: DependencyGraph (Phase 3, Batch 5)
-- ---------------------------------------------------------------------------

-- | 'DepGraph.buildDepGraph' over a pure .drv lookup.
pureDepGraph :: (StorePath -> Either Text Derivation) -> Derivation -> StorePath -> Either Text DepGraph.DepGraph
pureDepGraph readDrv rootDrv rootPath = runIdentity (DepGraph.buildDepGraph (Identity . readDrv) rootDrv rootPath)

-- ---------------------------------------------------------------------------
-- Tests: Substituter (Phase 3, Batch 6)
-- ---------------------------------------------------------------------------

-- | A fake HTTP body reader: yields the given chunks, then empty
-- forever (an HTTP BodyReader is just an IO ByteString).
chunkReader :: [BS.ByteString] -> IO (IO BS.ByteString)
chunkReader chunks = do
  ref <- newIORef chunks
  pure $
    atomicModifyIORef' ref $ \case
      [] -> ([], BS.empty)
      (c : cs) -> (cs, c)

-- ---------------------------------------------------------------------------
-- Tests: Store.DB (Phase 2, Batch 1)
-- ---------------------------------------------------------------------------

removeIfExists :: FilePath -> IO ()
removeIfExists path = do
  exists <- doesDirectoryExist path
  when exists (removeDirectoryRecursive path)

-- | A path as upstream's formatter streams a @std::filesystem::path@:
-- through @std::quoted@, in double quotes with a double quote or a
-- backslash inside escaped by a backslash.
quotedAsCxx :: FilePath -> Text
quotedAsCxx path = "\"" <> T.replace "\"" "\\\"" (T.replace "\\" "\\\\" (T.pack path)) <> "\""

-- ---------------------------------------------------------------------------
-- Tests: Push (pure narinfo construction + closure computation)
-- ---------------------------------------------------------------------------

-- | A secret key from the seed, as the text @nix-store
-- --generate-binary-cache-key@ writes (@name:@ and the base64 of the seed
-- and its public half), with the public key.
throwawayKey :: BS.ByteString -> Either String (Text, Signing.PublicKey)
throwawayKey seed = do
  publicKey <- Signing.parseSecretKey (keyText (seed <> BS.replicate ed25519SeedBytes 0)) >>= Signing.toPublicKey
  pure (keyText (seed <> Signing.pkBytes publicKey), publicKey)
  where
    keyText bytes = "test-cache-1:" <> B64.encode bytes

-- | Helper: create a fresh temp store for IO tests.
withTempStore :: (Store -> IO [Bool]) -> IO [Bool]
withTempStore action = do
  tmpBase <- getTemporaryDirectory
  let tmpStore = tmpBase </> "nova-nix-test-store-ops"
  forceRemoveIfExists tmpStore
  createDirectoryIfMissing True tmpStore
  store <- openStore testLogger (StoreDir tmpStore)
  results <- action store
  closeStore store
  forceRemoveIfExists tmpStore
  pure results

-- | 'deleteStorePathRaw' of a name spelled as text, refused as
-- 'storeEntryName' refuses it.
deleteNamed :: Store -> Text -> IO (Either Text DeleteOutcome)
deleteNamed store = either (pure . Left) (deleteStorePathRaw store) . storeEntryName

-- | Recursively restore writable permissions then remove.
-- Needed because addToStore/canonicalizeMetadata makes paths read-only.
forceRemoveIfExists :: FilePath -> IO ()
forceRemoveIfExists path = do
  exists <- doesDirectoryExist path
  when exists $ do
    restoreWritable path
    removeDirectoryRecursive path

-- | Give a tree's owner write permission on everything in it, so it can be
-- removed.  A symlink is passed over, never followed: it has no mode of
-- its own, and what it names, such as a chroot's /dev/fd naming
-- /proc/self/fd, is not the tree's to change.
restoreWritable :: FilePath -> IO ()
restoreWritable path = do
  isLink <- Dir.pathIsSymbolicLink path
  unless isLink $ do
    isDir <- doesDirectoryExist path
    when isDir $ do
      perms <- getPermissions path
      -- Read and search too, for a directory a test left closed.
      Dir.setPermissions path (Dir.setOwnerWritable True (Dir.setOwnerReadable True (Dir.setOwnerSearchable True perms)))
      entries <- Dir.listDirectory path
      mapM_ (restoreWritable . (path </>)) entries
    isFile <- Dir.doesFileExist path
    when isFile $ do
      perms <- getPermissions path
      Dir.setPermissions path (Dir.setOwnerWritable True perms)

-- | A test archive's entry path.  The paths the tests pass are short
-- literals, so encoding one cannot fail; 'error' marks a setup bug, not a
-- test failure.
tarPathOrDie :: Bool -> FilePath -> TarEntry.TarPath
tarPathOrDie isDir p =
  either (error . ("test tar path: " ++)) id (TarEntry.toTarPath isDir p)

tarDir :: FilePath -> TarEntry.Entry
tarDir p = TarEntry.directoryEntry (tarPathOrDie True p)

tarFile :: FilePath -> BL.ByteString -> TarEntry.Entry
tarFile p = TarEntry.fileEntry (tarPathOrDie False p)

-- ---------------------------------------------------------------------------
-- Tests: profiles
-- ---------------------------------------------------------------------------

-- | A store path of a test's own, by the letter its hash ends in and its
-- name.
profileFixture :: Char -> Text -> StorePath
profileFixture letter = StorePath (T.replicate 31 "0" <> T.singleton letter)

-- ---------------------------------------------------------------------------
-- Tests: NAR and hash known-answer vectors
-- ---------------------------------------------------------------------------

-- | Encode one NAR string per the spec: little-endian u64 length, the
-- bytes, zero-padding to the next 8-byte boundary.  Written here from the
-- format definition, deliberately independent of nova-cache's encoder, so
-- these vectors pin the wire layout rather than the library against
-- itself.
narSpecStr :: BS.ByteString -> BS.ByteString
narSpecStr s = lenLE <> s <> padding
  where
    n = BS.length s
    lenLE = BS.pack [fromIntegral ((n `shiftR` (8 * i)) .&. 0xff) | i <- [0 .. 7]]
    padding = BS.replicate ((8 - n `mod` 8) `mod` 8) 0

-- | Hand-encoded NAR of a directory holding an executable, a plain file,
-- and a symlink - covering entry ordering, the executable marker, content
-- padding, and all node kinds.
narSpecVector :: BS.ByteString
narSpecVector =
  BS.concat $
    map
      narSpecStr
      [ "nix-archive-1",
        "(",
        "type",
        "directory",
        "entry",
        "(",
        "name",
        "bin",
        "node",
        "(",
        "type",
        "directory",
        "entry",
        "(",
        "name",
        "tool",
        "node",
        "(",
        "type",
        "regular",
        "executable",
        "",
        "contents",
        "#!x",
        ")",
        ")",
        ")",
        ")",
        "entry",
        "(",
        "name",
        "data.txt",
        "node",
        "(",
        "type",
        "regular",
        "contents",
        "hello\n",
        ")",
        ")",
        "entry",
        "(",
        "name",
        "link",
        "node",
        "(",
        "type",
        "symlink",
        "target",
        "data.txt",
        ")",
        ")",
        ")"
      ]

-- | Hand-encoded NAR of just @data.txt@, for the on-disk addToStore vector.
-- No executable entry, so the vector stays a fixture for addToStore rather
-- than for the exec bit; 'testExecBit' covers that on both platforms.
narSpecFileVector :: BS.ByteString
narSpecFileVector =
  BS.concat $
    map
      narSpecStr
      [ "nix-archive-1",
        "(",
        "type",
        "directory",
        "entry",
        "(",
        "name",
        "data.txt",
        "node",
        "(",
        "type",
        "regular",
        "contents",
        "hello\n",
        ")",
        ")",
        ")"
      ]

-- | Fetch a path as a build does, from the first cache that offers it: the
-- reference query, then the fetch from its substitute.  A path no cache
-- offers is 'Subst.SubstNotFound', and a query that fails is
-- 'Subst.SubstError' with its message.
offeredThenFetched :: FetchRetryPolicy -> RetryEffects IO -> Store -> [Subst.CacheConfig] -> StorePath -> IO Subst.SubstResult
offeredThenFetched policy effects store caches sp = do
  offer <- evalStateT (Subst.queryReferencesWith testLogger policy effects Realise.NoFallback caches sp) Subst.emptyCacheMemo
  case offer of
    Realise.Offered substitute _ -> Subst.fetchSubstituteWith policy effects store substitute
    Realise.NotOffered -> pure Subst.SubstNotFound
    Realise.QueryFailed failure -> pure (Subst.SubstError failure)

-- | The substitute a cache offered, if one did.
offeredSubstitute :: Realise.Offer s -> Maybe s
offeredSubstitute (Realise.Offered substitute _) = Just substitute
offeredSubstitute Realise.NotOffered = Nothing
offeredSubstitute (Realise.QueryFailed _) = Nothing

-- | What a loopback cache answers for paths with their references: each
-- one's narinfo, signed by 'sigTestKeyText' and naming them, at
-- 'narInfoRequest', and its NAR ('servedNar'), uncompressed at
-- 'narRequest'.  Any other request is a 404.  With the public key that
-- verifies the narinfos.
servedPaths :: [(StorePath, [StorePath])] -> Either String (Text, BS.ByteString -> Reply)
servedPaths entries = do
  publicKey <- Signing.renderPublicKey <$> (Signing.parseSecretKey sigTestKeyText >>= Signing.toPublicKey)
  answers <- Map.fromList . concat <$> traverse served entries
  pure (publicKey, \target -> Send (maybe (httpResponse "404 Not Found" "") (httpResponse "200 OK") (Map.lookup target answers)))
  where
    served (path, references) = do
      (_, narInfo) <- signedTestNarInfo path references (narUrl path) (servedNar path)
      pure [(narInfoRequest path, TE.encodeUtf8 narInfo), (narRequest path, servedNar path)]
    narUrl path = "nar/" <> spName path <> ".nar"

-- | The request target of a path's narinfo.
narInfoRequest :: StorePath -> BS.ByteString
narInfoRequest path = "/" <> TE.encodeUtf8 (spHash path) <> ".narinfo"

-- | The request target of a path's NAR in 'servedPaths'.
narRequest :: StorePath -> BS.ByteString
narRequest path = "/nar/" <> TE.encodeUtf8 (spName path) <> ".nar"

-- | The NAR 'servedPaths' serves for a path: one file holding its name.
servedNar :: StorePath -> BS.ByteString
servedNar path = NAR.serialise (NAR.NarRegular False (TE.encodeUtf8 (spName path) <> "\n"))

-- | Every derivation reachable from @roots@ through input derivations, keyed
-- by @.drv@ path, as the evaluation session recorded them.
sessionClosure :: (MonadEval m) => [Text] -> m (Map.Map Text Derivation)
sessionClosure = go Map.empty
  where
    go seen [] = pure seen
    go seen (path : rest)
      | Map.member path seen = go seen rest
      | otherwise = do
          recorded <- lookupSessionDrv path
          case recorded of
            Nothing -> throwEvalError ("no .drv was recorded for " <> path)
            Just drv ->
              go (Map.insert path drv seen) (map (storePathToText defaultStoreDir) (Map.keys (drvInputDrvs drv)) ++ rest)

-- ---------------------------------------------------------------------------
-- builtins.fetchGit against a real repository
-- ---------------------------------------------------------------------------

-- | One git command against a fixture repository, identity and signing
-- pinned so no host configuration participates.  Nonzero exit throws
-- with git's stderr, failing the calling test loudly.
fixtureGit :: FilePath -> [String] -> IO Text
fixtureGit dir args = do
  (code, out, errOut) <-
    Proc.readCreateProcessWithExitCode
      (Proc.proc "git" (["-C", dir, "-c", "user.name=nova-nix-test", "-c", "user.email=test@nova-nix.invalid", "-c", "commit.gpgsign=false", "-c", "core.autocrlf=false"] ++ args))
      ""
  case code of
    ExitSuccess -> pure (T.strip (T.pack out))
    ExitFailure n -> fail ("fixture git " ++ unwords args ++ " exited " ++ show n ++ ": " ++ errOut)

-- | A repository whose history is one commit per given file, oldest
-- first; returns the commit SHAs in that order.  SHA wants are enabled
-- up front: a pinned rev that is no branch tip is only fetchable when
-- the serving side allows it, the same opt-in real servers use.
makeGitFixture :: FilePath -> [(FilePath, Text)] -> IO [Text]
makeGitFixture dir files = do
  Dir.createDirectoryIfMissing True dir
  _ <- fixtureGit dir ["init", "--quiet"]
  _ <- fixtureGit dir ["config", "uploadpack.allowAnySHA1InWant", "true"]
  mapM commitOne files
  where
    commitOne (name, contents) = do
      BS.writeFile (dir </> name) (TE.encodeUtf8 contents)
      _ <- fixtureGit dir ["add", "-A"]
      _ <- fixtureGit dir ["commit", "--quiet", "-m", name]
      fixtureGit dir ["rev-parse", "HEAD"]

-- | 'evalNixIO' against an explicit store directory, for expressions
-- whose evaluation writes store objects.
evalNixIOStore :: StoreDir -> FilePath -> Text -> IO (Either Text NixValue)
evalNixIOStore storeDir baseDir source = do
  st <- testEvalState storeDir baseDir
  evalNixIOIn st baseDir source

-- | Evaluate the source under the IO evaluator in the given state.
evalNixIOIn :: EvalState -> FilePath -> Text -> IO (Either Text NixValue)
evalNixIOIn st baseDir source = case parseNix baseDir testOrigin (TE.encodeUtf8 source) of
  Left err -> pure (Left (T.pack (show err)))
  Right parsed -> runEvalIO st (evalParsed (pure (evalBaseEnv st)) parsed)

-- | Whether a node has the form 'canonicalizeMetadata' gives it.  On
-- POSIX that is the given mode and the store's time, as @lstat@ reads
-- them.  Windows has no mode, so there it is a file or directory that is
-- read-only and has the store's time.
canonicalAs :: Word32 -> FilePath -> IO TestResult
canonicalAs expected path = do
  status <- nodeStatus path
  case status of
    Just found -> pure (assertEqual (T.pack path) (NodeStatus expected storeModificationTime) found)
    Nothing -> do
      perms <- getPermissions path
      time <- Dir.getModificationTime path
      pure (assertEqual (T.pack path) (False, storeTime) (writable perms, time))
  where
    storeTime = posixSecondsToUTCTime (fromIntegral storeModificationTime)

-- ---------------------------------------------------------------------------
-- Tests: fromATerm + Derivation Output Population (Phase 2, Batch 3)
-- ---------------------------------------------------------------------------

-- | A simple test derivation for round-trip testing.
simpleTestDrv :: Derivation
simpleTestDrv =
  Derivation
    { drvOutputs =
        [ DerivationOutput
            { doName = "out",
              doPath = StorePath "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "hello-1.0",
              doHashAlgo = "",
              doHash = ""
            }
        ],
      drvInputDrvs = Map.empty,
      drvInputSrcs = [],
      drvPlatform = X86_64_Linux,
      drvBuilder = "/nix/store/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb-bash-5.2/bin/bash",
      drvArgs = ["-e", "/nix/store/cccccccccccccccccccccccccccccccc-stdenv/setup"],
      drvEnv = Map.fromList [("name", "hello-1.0"), ("system", "x86_64-linux")]
    }

-- ---------------------------------------------------------------------------
-- Tests: input-addressed derivations
-- ---------------------------------------------------------------------------

-- | A derivation name the store accepts, short enough that every output's
-- composed name is accepted too.
genDrvName :: QC.Gen Text
genDrvName = T.pack <$> ((:) <$> QC.elements nameStart <*> QC.resize 15 (QC.listOf (QC.elements (nameStart <> "+-._?="))))
  where
    nameStart = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9']

-- | A store path of a generated name, its hash in the nix-base32 alphabet.
genFixturePath :: Text -> QC.Gen StorePath
genFixturePath suffix = StorePath <$> genTextOf 32 "0123456789abcdfghijklmnpqrsvwxyz" <*> ((<> suffix) <$> genDrvName)

-- | Text of a given length drawn from an alphabet.
genTextOf :: Int -> String -> QC.Gen Text
genTextOf len alphabet = T.pack <$> QC.vectorOf len (QC.elements alphabet)

-- ---------------------------------------------------------------------------
-- Tests: Builder (Phase 2, Batch 4)
-- ---------------------------------------------------------------------------

-- | The @.drv@ path a builder test hands 'buildDerivation'.  Only a failure
-- message that names the derivation reads it.
testDrvPath :: StorePath
testDrvPath = StorePath (T.replicate 32 "d") "test-build.drv"

-- | Create a minimal Derivation for builder tests.
-- The shell path is discovered once via 'findTestShell' and threaded through.
-- All scripts are POSIX shell - bash is used on every platform - and may
-- run the host's own tools ('withHostTools').
mkTestBuildDrv :: Text -> StorePath -> Text -> Derivation
mkTestBuildDrv shell outSP script =
  withHostTools
    Derivation
      { drvOutputs =
          [ DerivationOutput
              { doName = "out",
                doPath = outSP,
                doHashAlgo = "",
                doHash = ""
              }
          ],
        drvInputDrvs = Map.empty,
        drvInputSrcs = [],
        drvPlatform = currentPlatform,
        drvBuilder = TE.encodeUtf8 shell,
        drvArgs = ["-c", TE.encodeUtf8 script],
        drvEnv = Map.fromList [("name", "test-build"), ("system", TE.encodeUtf8 (platformToText currentPlatform))]
      }

-- | What a test derivation that runs the host's own tools sets so that it
-- finds them, as any derivation must name the programs it runs: on Unix
-- a @PATH@ of the directories they are in, since a build's own names none;
-- on Windows nothing, since a build's @PATH@ there holds the test shell's
-- own tools.
hostToolsEnv :: Map.Map Text BS.ByteString
hostToolsEnv
  | SI.os == "mingw32" = Map.empty
  | otherwise = Map.singleton "PATH" "/usr/bin:/bin"

-- | A test derivation with 'hostToolsEnv' among its variables, its own
-- winning.
withHostTools :: Derivation -> Derivation
withHostTools drv = drv {drvEnv = Map.union (drvEnv drv) hostToolsEnv}

-- | 'hostToolsEnv' as attributes of a derivation written in source.
hostToolsAttrs :: Text
hostToolsAttrs = Map.foldrWithKey (\name value rest -> name <> " = \"" <> TE.decodeUtf8 value <> "\"; " <> rest) "" hostToolsEnv

-- ---------------------------------------------------------------------------
-- Tests: CLI Integration (Phase 2, Batch 5)
-- ---------------------------------------------------------------------------

-- | End-to-end: eval .nix source, extract derivation, build, verify output.
evalAndBuild :: StoreDir -> Text -> IO (Either Text (BuildResult, Store))
evalAndBuild storeDir source = do
  case parseNix testBaseDir testOrigin (TE.encodeUtf8 source) of
    Left err -> pure (Left ("parse error: " <> T.pack (show err)))
    Right parsed -> do
      -- The SAME store the build below is given.  Evaluating against the
      -- platform default while building elsewhere is what let a store dir
      -- honored on writes but not on reads pass the whole suite.
      st <- testEvalState storeDir "."
      evalResult <- runEvalIO st $ do
        val <- evalParsed (pure (evalBaseEnv st)) parsed
        -- What the CLI driver does: force drvPath, which runs
        -- derivationStrict (so a missing required attr surfaces as an eval
        -- error) and records the .drv the build reads back by that path.
        drvPathThunk <- case val of
          VAttrs attrs -> attrSetLookup "drvPath" attrs
          _ -> pure Nothing
        case drvPathThunk of
          Just thunk -> do
            pathVal <- force thunk
            case pathVal of
              VStr pathBytes _ -> lookupSessionDrv (bytesText pathBytes)
              _ -> pure Nothing
          Nothing -> pure Nothing
      case evalResult of
        Left err -> pure (Left ("eval error: " <> err))
        Right Nothing -> pure (Left "result is not a derivation")
        Right (Just drv) -> do
          store <- openStore testLogger storeDir
          let config = defaultBuildConfig storeDir
          -- What the CLI driver does before building, in the same
          -- order: eval has no store DB handle, so anything it wrote
          -- is registered here or not at all.  Skipping it made this
          -- harness unable to see a whole class of driver bug.
          sourceCache <- readIORef (esSourcePathCache st)
          storeWrites <- readIORef (esStoreWriteCache st)
          materializeEvalSources store (sourceCopies sourceCache)
          materializeEvalStoreWrites store storeWrites
          result <- buildDerivation config store drv testDrvPath
          pure (Right (result, store))

-- ---------------------------------------------------------------------------
-- Tests: realising a string's context
-- ---------------------------------------------------------------------------

-- | The operations of a read-only session that should add and realise
-- nothing: each is a failure the test sees.
unexpectedOps :: StoreOps IO
unexpectedOps =
  StoreOps
    { soQueries = StoreQueries {sqValidPaths = \_ -> pure Set.empty, sqReferences = \_ -> pure (Right Set.empty)},
      soAddEvalObjects = \_ -> pure (Left "an add no test asked for"),
      soAddSourceTree = \_ _ _ -> pure (Left "a tree no test asked for"),
      soBuild = \requests -> pure (Left (BuildStop ("a build no test asked for: " <> T.pack (show requests)) 1)),
      soCheck = \_ -> pure (Left (BuildStop "a check no test asked for" 1)),
      soAddOutLinkRoot = \_ _ -> pure (Left "a root no test asked for")
    }

-- ---------------------------------------------------------------------------
-- Tests: trust
-- ---------------------------------------------------------------------------

-- | The smallest Portable Executable the scanner recognises: an @MZ@
-- header whose @e_lfanew@, at 0x3C, points at a @PE\\0\\0@ signature at
-- 0x40.
trustPe :: BS.ByteString
trustPe = "MZ" <> BS.replicate 0x3A 0 <> BS.pack [0x40, 0, 0, 0] <> "PE\0\0"

-- ---------------------------------------------------------------------------
-- Arena lifecycle guard (C FFI)
-- ---------------------------------------------------------------------------

-- | Run as a child, evaluate one expression under the IO evaluator, which
-- writes traces and warnings to stderr as it goes, then write its failure
-- to the same stream, so the parent reads them in the order they came.
evalChildFlag :: String
evalChildFlag = "--eval-child"

evalChild :: String -> IO ()
evalChild source = do
  result <- evalNixIO "." (T.pack source)
  either (\err -> hPutStrLn stderr ("error: " <> T.unpack err)) (const (pure ())) result

-- | Evaluate the source in a child process running 'evalChild' and
-- compare the lines it wrote to stderr with the expected ones.
emits :: Text -> Text -> [Text] -> IO Bool
emits label source expected = runTestM label $ do
  self <- getExecutablePath
  (_, _, err) <- Proc.readCreateProcessWithExitCode (Proc.proc self [evalChildFlag, T.unpack source]) ""
  pure (assertEqual label expected (T.lines (T.pack err)))

-- | Run an action in an evaluation session with no search path, as the
-- suite and its children run theirs; a state made inside it is its own.
inTestSession :: IO a -> IO a
inTestSession action = do
  storeDir <- evalNixIOStoreDir
  withEvalSession (SessionSettings unrestrictedPolicy (pure []) storeDir testBaseDir RegionsCollect (ReadOnlySession (localQueries storeDir) unexpectedOps) testLogger) (const action)

-- ---------------------------------------------------------------------------
-- Tests: store-path name validation at write sinks
-- ---------------------------------------------------------------------------

-- | Entry names upstream's restore writes on POSIX as it finds them and
-- Windows cannot hold, each as compared against @nix-store --restore@
-- 2.33.2: device stems (one with a superscript digit), trailing dots and
-- spaces, colons (perl's man pages carry them), a backslash, the
-- punctuation Win32 reserves, a control character, and links pointing
-- at such names.
posixOnlyEntries :: [(BS.ByteString, NAR.NarEntry)]
posixOnlyEntries =
  [ ("COM1", NAR.NarRegular False "com1"),
    ("ExtUtils::MakeMaker.3", NAR.NarRegular False "man"),
    ("LPT\xC2\xB9.txt", NAR.NarRegular False "lpt"),
    ("NUL", NAR.NarRegular False "nul"),
    ("a:b", NAR.NarRegular False "colon"),
    ("a\\b", NAR.NarRegular False "backslash"),
    ("aux.c", NAR.NarRegular False "aux"),
    ("con.h", NAR.NarRegular False "con"),
    ("ctl\x01", NAR.NarRegular False "control"),
    ("dir.", NAR.NarDirectory [("prn", NAR.NarRegular True "prn")]),
    ("link-aux", NAR.NarSymlink "aux.c"),
    ("link-colon", NAR.NarSymlink "a:b"),
    ("link-dot", NAR.NarSymlink "x."),
    ("q\"*?<>|", NAR.NarRegular False "reserved"),
    ("x.", NAR.NarRegular False "dot"),
    ("y ", NAR.NarRegular False "space")
  ]

-- ---------------------------------------------------------------------------
-- Bytecode tags: what the compiler writes decodes back, and nothing else does
-- ---------------------------------------------------------------------------

-- | A lambda's formals decoded from the bytecode ('decodeBcFormals'),
-- failing on a default kind the compiler never writes.
decodeFormals :: Word8 -> Word32 -> IO (Maybe EvalFormals)
decodeFormals = decodeBcFormals (\kind -> ioError (userError ("a formal's default has kind " <> show kind)))

-- | The bindings of a compiled set or let, their names counted.
compiledBindings :: Word32 -> IO BcBindings
compiledBindings idx = do
  (count, dataOff) <- cbcCountedPayload idx =<< cbcArg1 idx
  decodeBcBindings count dataOff

-- ---------------------------------------------------------------------------
-- Tests: substituter signature verification (the --trusted-key trust gate)
-- ---------------------------------------------------------------------------

-- | Deterministic Ed25519 test keys: a nix secret key file is
-- seed(32) + public(32), and both signing and public-key derivation use
-- only the seed, so the public half can be zeros here.  Both keys carry
-- the SAME name so the wrong-key test exercises the cryptographic
-- verification, not just the name comparison.
sigTestKeyText, sigTestWrongKeyText :: Text
sigTestKeyText = "test-key-1:" <> B64.encode (BS.pack ([1 .. 32] ++ replicate 32 0))
sigTestWrongKeyText = "test-key-1:" <> B64.encode (BS.pack ([101 .. 132] ++ replicate 32 0))

-- | The narinfo of a path whose NAR is these bytes, served uncompressed
-- at this URL and naming these references, signed by 'sigTestKeyText',
-- with the public key that verifies it.  Left on any setup failure.
signedTestNarInfo :: StorePath -> [StorePath] -> Text -> BS.ByteString -> Either String (Text, Text)
signedTestNarInfo path references url nar = do
  secretKey <- Signing.parseSecretKey sigTestKeyText
  publicKey <- Signing.toPublicKey secretKey
  sig <- Signing.sign secretKey unsigned
  pure (Signing.renderPublicKey publicKey, NarInfo.renderNarInfo unsigned {NarInfo.niSigs = [sig]})
  where
    unsigned =
      NarInfo.NarInfo
        { NarInfo.niStorePath = storePathToText defaultStoreDir path,
          NarInfo.niUrl = url,
          NarInfo.niCompression = "none",
          NarInfo.niFileHash = Nothing,
          NarInfo.niFileSize = Nothing,
          NarInfo.niNarHash = CHash.formatNixHash (CHash.hashBytes nar),
          NarInfo.niNarSize = fromIntegral (BS.length nar),
          NarInfo.niReferences = map storePathBasename references,
          NarInfo.niDeriver = Nothing,
          NarInfo.niSigs = [],
          NarInfo.niCA = Nothing
        }

-- | A test path as the config expander names it.  Test paths spelled
-- this way are ASCII, which 'OP.unsafeFromChar' narrows to one path unit
-- exactly on every platform.
asciiOsPath :: FilePath -> OsPath
asciiOsPath = OP.pack . map OP.unsafeFromChar

-- ---------------------------------------------------------------------------
-- Tests: resolver static globals stay in sync with the root env
-- ---------------------------------------------------------------------------

-- | The names upstream 2.24.9's base environment binds with default
-- settings, recorded from its source: every @addConstant@ and @addPrimOp@
-- in @createBaseEnv@ (src/libexpr/primops.cc) and every @RegisterPrimOp@
-- in libexpr and libflake not gated on an experimental feature, under the
-- name it registers.  @__importNative@ and @__exec@ need
-- @allow-unsafe-native-code-during-evaluation@ and are not here.
upstreamBaseEnvNames :: Set.Set Text
upstreamBaseEnvNames =
  Set.fromList
    [ "__add",
      "__addDrvOutputDependencies",
      "__addErrorContext",
      "__all",
      "__any",
      "__appendContext",
      "__attrNames",
      "__attrValues",
      "__bitAnd",
      "__bitOr",
      "__bitXor",
      "__catAttrs",
      "__ceil",
      "__compareVersions",
      "__concatLists",
      "__concatMap",
      "__concatStringsSep",
      "__convertHash",
      "__currentSystem",
      "__currentTime",
      "__deepSeq",
      "__div",
      "__elem",
      "__elemAt",
      "__fetchurl",
      "__filter",
      "__filterSource",
      "__findFile",
      "__floor",
      "__foldl'",
      "__fromJSON",
      "__functionArgs",
      "__genList",
      "__genericClosure",
      "__getAttr",
      "__getContext",
      "__getEnv",
      "__groupBy",
      "__hasAttr",
      "__hasContext",
      "__hashFile",
      "__hashString",
      "__head",
      "__intersectAttrs",
      "__isAttrs",
      "__isBool",
      "__isFloat",
      "__isFunction",
      "__isInt",
      "__isList",
      "__isPath",
      "__isString",
      "__langVersion",
      "__length",
      "__lessThan",
      "__listToAttrs",
      "__mapAttrs",
      "__match",
      "__mul",
      "__nixPath",
      "__nixVersion",
      "__parseDrvName",
      "__partition",
      "__path",
      "__pathExists",
      "__readDir",
      "__readFile",
      "__readFileType",
      "__replaceStrings",
      "__seq",
      "__sort",
      "__split",
      "__splitVersion",
      "__storeDir",
      "__storePath",
      "__stringLength",
      "__sub",
      "__substring",
      "__tail",
      "__toFile",
      "__toJSON",
      "__toPath",
      "__toXML",
      "__trace",
      "__traceVerbose",
      "__tryEval",
      "__typeOf",
      "__unsafeDiscardOutputDependency",
      "__unsafeDiscardStringContext",
      "__unsafeGetAttrPos",
      "__warn",
      "__zipAttrsWith",
      "abort",
      "baseNameOf",
      "break",
      "builtins",
      "derivation",
      "derivationStrict",
      "dirOf",
      "false",
      "fetchGit",
      "fetchMercurial",
      "fetchTarball",
      "fromTOML",
      "import",
      "isNull",
      "map",
      "null",
      "placeholder",
      "removeAttrs",
      "scopedImport",
      "throw",
      "toString",
      "true"
    ]

-- ---------------------------------------------------------------------------
-- Tests: evaluation policy - restricted and pure modes
-- ---------------------------------------------------------------------------

-- | The policies under test.
restrictedPolicy, purePolicy :: EvalPolicy
restrictedPolicy = unrestrictedPolicy {epRestrictEval = True}
purePolicy = unrestrictedPolicy {epPureEval = True}

-- | Parse and evaluate in 'PureEval' with the given policy's builtins
-- table, for the constants the table itself decides.
evalNixWith :: EvalPolicy -> Text -> Either Text NixValue
evalNixWith policy source = case parseNixWithScope (rootScopeNames policy) testBaseDir testOrigin (TE.encodeUtf8 source) of
  Left err -> Left (sourceFailure err)
  Right parsed -> runPureEval (evalParsed (builtinEnv policy 0 []) parsed)

-- | Run an action with a variable set (to an empty value too, which
-- 'setEnv' would unset), restoring what the variable held afterwards.
withEnvVar :: String -> String -> IO a -> IO a
withEnvVar name value action =
  bracket (lookupEnv name) restore (const (Blank.setEnv name value True >> action))
  where
    restore = maybe (unsetEnv name) (\saved -> Blank.setEnv name saved True)

-- ---------------------------------------------------------------------------
-- Logging
-- ---------------------------------------------------------------------------

-- | The logger the suite's stores, builds and evaluations report to:
-- each message and each build line on stderr, as the command line
-- writes them, with every escape sequence dropped from a build line.
testLogger :: Logger
testLogger = handleLogger DropEscapes stderr

-- ---------------------------------------------------------------------------
-- Generators shared by the daemon and store tests
-- ---------------------------------------------------------------------------

-- | A signature: a key name of letters, dashes, dots and digits, and
-- bytes that are not empty.
genSignature :: QC.Gen Proto.Signature
genSignature = QC.suchThatMap ((,) <$> keyName <*> (BS.pack <$> QC.resize 70 (QC.listOf1 QC.arbitrary))) (uncurry Proto.signatureFromParts)
  where
    keyName = BS8.pack <$> QC.listOf1 (QC.elements (['a' .. 'z'] ++ "-.0"))

-- | A few signatures, possibly none.
genSignatures :: QC.Gen (Set.Set Proto.Signature)
genSignatures = Set.fromList <$> QC.resize 3 (QC.listOf genSignature)

-- | A content address under each method and algorithm upstream's has.
genContentAddress :: QC.Gen Proto.ContentAddress
genContentAddress = do
  method <- QC.elements [Proto.TextMethod, Proto.FixedMethod FlatIngestion, Proto.FixedMethod NarIngestion]
  algo <- QC.elements ["md5", "sha1", "sha256", "sha512"]
  QC.suchThatMap (BS.pack <$> QC.vectorOf (fromMaybe 0 (hashAlgoBytes algo)) QC.arbitrary) (Proto.contentAddressFromParts method algo)
