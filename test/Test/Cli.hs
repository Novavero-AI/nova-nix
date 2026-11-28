{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests of the command line and the configuration it reads.
module Test.Cli
  ( testProfileCLI,
    testE2E,
    testExecWrapperCLI,
    testUncaughtIOFailureCLI,
    testClosedStdoutCLI,
    testImportFromDerivationCLI,
    testCheckCLI,
    testDevelopCLI,
    testBuildUmaskCLI,
    testSandboxSessionsCLI,
    testBuildCoresCLI,
    testTargetArgsCLI,
    testOutLinkCLI,
    testTrustCLI,
    cliJsonPath,
    writeCliJsonFlag,
    testCliReference,
    testNixConfig,
    testEvalPolicyCLI,
    testResultPrinterCLI,
    testConfigBytesCLI,
  )
where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import Control.Exception (IOException, bracket, bracket_, displayException, try)
import Control.Monad (filterM, forM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAsciiLower, isAsciiUpper)
import Data.Foldable (toList)
import Data.Functor.Identity (Identity (..))
import Data.List (findIndex, inits, intercalate, isInfixOf, isPrefixOf, isSuffixOf, nub, sort, stripPrefix)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Word (Word32)
import HttpFixture (Reply (..), httpResponse, withHttpServer, withKeepAliveDirectoryServer)
import Nix.Builder (BuildResult (..))
import qualified Nix.Builder.Cores as Cores
import Nix.CacheDirectory (directoryUrl)
import qualified Nix.Cli.Args as Cli
import qualified Nix.Cli.Reference as Ref
import Nix.Config (NixConfig (..))
import qualified Nix.Config as Config
import Nix.Derivation (Platform (..), currentPlatform, platformToText)
import Nix.Develop.Environment (BuildEnvironment (..), Variable (..), parseBuildEnvironment)
import Nix.Eval.CanonPath (canonPathValue)
import Nix.Hash (FileIngestionMethod (..), makeFixedOutputPath, makeTextPath, sha256Digest)
import qualified Nix.Hash as Hash
import Nix.HostPath (hostPathFromBytes)
import Nix.Json (Json (..), parseJson)
import qualified Nix.Profile.Generation as Gen
import Nix.Push.Directory (ed25519SeedBytes)
import Nix.Store (Store (..), addToStore, canonicalizeMetadata, closeStore, isValid, openStore)
import qualified Nix.Store.ExecBit as ExecBit
import Nix.Store.Metadata (executablePermissions, readOnlyPermissions)
import Nix.Store.Path (StoreDir (..), StorePath, defaultStoreDir, storePathToFilePath, storePathToText)
import Nix.Store.Path.Internal (StorePath (..))
import qualified NovaCache.NAR as NAR
import qualified NovaCache.Signing as Signing
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getTemporaryDirectory)
import qualified System.Directory as Dir
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath (searchPathSeparator, takeDirectory, takeFileName, (</>))
import System.IO (hClose)
import System.IO.Error (ioeGetErrorString)
import qualified System.Info as SI
import qualified System.OsPath as OP
import qualified System.Process as Proc
import Test.Harness (TestResult (..), andThen, asciiOsPath, assertEqual, assertLeft, assertRight, canonicalAs, envSpelling, evalAndBuild, evalNixIOStore, fetchedStorePath, findTestShell, forceRemoveIfExists, genJson, hostToolsAttrs, makeGitFixture, nixQuotedPath, profileFixture, runTest, runTestM, selfHoldingDerivation, selfHoldingPrinted, selfHoldingPrintedStrict, signedTestNarInfo, symlinksAvailable, tarFile, testLogger, throwawayKey, trustPe)
import qualified Test.QuickCheck as QC
import Text.Read (readMaybe)

-- ---------------------------------------------------------------------------
-- Tests: profiles
-- ---------------------------------------------------------------------------

-- | The profile commands through the real CLI, in an environment whose
-- every home, state and configuration directory is under the test's own
-- root, which the test checks before it runs anything: adding puts a
-- program under ~/.nix-profile/bin, listing prints upstream's lines and
-- JSON, each generation is a root the collector keeps, removing takes the
-- program off, and a profile of another store or a store upstream Nix
-- manages is refused; a rollback brings back an earlier generation, the
-- history prints upstream's lines, and wiping it deletes old generations,
-- whose roots the next collection drops along with what only they kept.
testProfileCLI :: IO [Bool]
testProfileCLI = do
  putStrLn "cli/profile"
  canLink <- symlinksAvailable
  shell <- findTestShell
  tmpBase <- getTemporaryDirectory
  ambient <- getEnvironment
  let root = tmpBase </> "nova-nix-test-cli-profile"
  if not canLink
    then [] <$ putStrLn "  SKIP  a profile's links need symlink privilege (Windows needs Developer Mode or elevation)"
    else bracket_ (forceRemoveIfExists root >> mapM_ (createDirectoryIfMissing True . (root </>)) ["work", "home", "home-xdg", "home-state-link", "real", "hist"]) (forceRemoveIfExists root) $ do
      -- Built, if it is not, with the ambient environment cabal needs, then
      -- run by its path, so the isolated environment reaches only it.
      _ <- Proc.readCreateProcessWithExitCode (Proc.proc "cabal" ["run", "-v0", "exe:nova-nix", "--", "--version"]) ""
      (_, listed, _) <- Proc.readCreateProcessWithExitCode (Proc.proc "cabal" ["list-bin", "-v0", "exe:nova-nix"]) ""
      let exe = fromMaybe "nova-nix" (listToMaybe (reverse (filter (not . null) (lines (filter (/= '\r') listed)))))
          locations homeDir stateVars extra =
            [ ("HOME", homeDir),
              ("USERPROFILE", homeDir),
              ("ZDOTDIR", homeDir),
              ("LOCALAPPDATA", root </> "local-app-data"),
              ("XDG_DATA_HOME", root </> "xdg-data"),
              ("XDG_CONFIG_HOME", root </> "xdg-config"),
              ("XDG_CONFIG_DIRS", root </> "xdg-dirs"),
              ("NIX_CONF_DIR", root </> "system")
            ]
              ++ stateVars
              ++ extra
          -- The variables that decide whether stdout is styled, which no
          -- run inherits.
          colourVariables = ["NO_COLOR", "NOCOLOR", "CLICOLOR_FORCE", "FORCE_COLOR"]
          isolated given config =
            ("NIX_CONFIG", "substituters =\ntrusted-public-keys =\n" <> config)
              : given
              ++ filter (\(key, _) -> key `notElem` ("NIX_CONFIG" : "NIX_USER_CONF_FILES" : "NIX_PATH" : "NIX_STATE_HOME" : "XDG_STATE_HOME" : colourVariables ++ map fst given)) ambient
          -- A run's process, given variables that name no location beside
          -- the locations.
          processIn settings given config args = (Proc.proc exe args) {Proc.env = Just (isolated (settings ++ given) config), Proc.cwd = Just (root </> "work")}
          refusedRun = "the test refused to run: a location is outside " <> root
          home = root </> "home"
          state = [("NIX_STATE_HOME", root </> "state")]
          vars = locations home state []
          onWindows = SI.os == "mingw32"
          -- Each run is refused, without starting the binary, unless every
          -- location it is given, and the state directory, profiles and
          -- links the binary computes from them on this host, are under the
          -- root.  On Windows a state directory NIX_STATE_HOME does not name
          -- is the Local AppData folder, which no variable moves.
          runIn = runWith []
          -- A run with variables that name no location beside the
          -- locations it is given.
          runWith settings given config args = do
            inside <- locationsUnder given
            if inside
              then Proc.readCreateProcessWithExitCode (processIn settings given config args) ""
              else pure (ExitFailure 1, "", refusedRun)
          -- A run in the default locations whose stdout is read as bytes,
          -- since a profile's history is not ASCII, with variables beside
          -- them.  stderr is read once stdout ends, which a run that writes
          -- little there allows.
          bytesIn settings args = do
            inside <- locationsUnder vars
            if inside
              then Proc.withCreateProcess ((processIn settings vars "" args) {Proc.std_out = Proc.CreatePipe, Proc.std_err = Proc.CreatePipe}) $ \_ out err process -> do
                printed <- maybe (pure BS.empty) BS.hGetContents out
                complaint <- maybe (pure BS.empty) BS.hGetContents err
                code <- Proc.waitForProcess process
                pure (code, printed, complaint)
              else pure (ExitFailure 1, BS.empty, BS8.pack refusedRun)
          locationsUnder given = do
            rootPath <- OP.encodeFS root
            stateHome <- case (lookup "NIX_STATE_HOME" given, onWindows) of
              (Just dir, _) -> Just . Gen.NixStateHome <$> OP.encodeFS dir
              (Nothing, True) -> pure Nothing
              (Nothing, False) -> Just . Gen.XdgStateHome <$> traverse OP.encodeFS (lookup "XDG_STATE_HOME" given)
            homeDir <- traverse OP.encodeFS (lookup (if onWindows then "USERPROFILE" else "HOME") given)
            let under path = OP.splitDirectories rootPath `isPrefixOf` OP.splitDirectories path
                computed = case (stateHome, homeDir) of
                  (Just stateFrom, Just homeFrom) ->
                    let env = Gen.ProfileEnvironment stateFrom homeFrom
                     in all under [Gen.stateDirectory env, Gen.profilesDirectory env, Gen.profileLinkPath Gen.HomeProfileLink env, Gen.profileLinkPath Gen.StateProfileLink env]
                  _ -> False
            pure (all ((root `isPrefixOf`) . snd) given && computed)
          nn = runIn vars ""
          outcome (code, out, err) = "exit " <> T.pack (show code) <> "; stdout=" <> T.pack out <> "; stderr=" <> T.pack err
          succeeded run@(code, _, _) check = if code == ExitSuccess then check else Fail (outcome run)
          stderrLines (_, _, err) = lines (filter (/= '\r') err)
          profiles = root </> "state" </> "profiles"
          helloPath = home </> ".nix-profile" </> "bin" </> "hello"
          worldPath = home </> ".nix-profile" </> "bin" </> "world"
          hello = profileFixture 'a' "hello-1.0"
          world = profileFixture 'b' "world-2.0"
          other = profileFixture 'c' "other-1.0"
          script message = "#!/bin/sh\necho " <> message <> "\n"
          stage name = root </> "stage" </> name
          writeStaged path bytes = createDirectoryIfMissing True (takeDirectory path) *> BS.writeFile path bytes
          -- A read that fails is the test's to report, not the suite's.
          readIfThere path = either (\(_ :: IOException) -> Nothing) Just <$> try (BS.readFile path)
          linkIfThere path = either (\(_ :: IOException) -> Nothing) Just <$> try (Dir.getSymbolicLinkTarget path)
          twoOutputs =
            "derivation { name = \"two-1.0\"; system = builtins.currentSystem; builder = "
              <> nixQuotedPath (T.unpack shell)
              <> "; outputs = [ \"out\" \"dev\" ]; args = [ \"-c\" \"echo out > $out; echo dev > $dev\" ]; }"
      do
        -- The store is under a linked directory, so the tree names a
        -- directory it expands at a spelling other than the store's.
        Dir.createDirectoryLink (root </> "real") (root </> "via")
        store <- openStore testLogger (StoreDir (root </> "via" </> "store"))
        let storeDir = stDir store
            storeArg = unStoreDir storeDir
            inStore = storePathToFilePath storeDir
            canonical = storePathToText defaultStoreDir
        writeStaged (stage "hello" </> "bin" </> "hello") (script "hello from the profile")
        writeStaged (stage "world" </> "bin" </> "world") (script "world")
        writeStaged (stage "other" </> "bin" </> "hello") (script "the other hello")
        forM_ [stage "hello" </> "bin" </> "hello", stage "world" </> "bin" </> "world", stage "other" </> "bin" </> "hello"] ExecBit.markExecutable
        mapM_ (\(name, path) -> addToStore store (stage name) path Nothing []) [("hello", hello), ("world", world), ("other", other)]
        closeStore store
        listedEmpty <- nn ["profile", "list", "--store", storeArg]
        emptyLink <- linkIfThere (home </> ".nix-profile")
        addHello <- nn ["profile", "add", "--store", storeArg, inStore hello]
        helloContent <- readIfThere helloPath
        ran <-
          if SI.os == "mingw32"
            then pure Nothing
            else Just <$> Proc.readProcessWithExitCode (T.unpack shell) [helloPath] ""
        listedText <- nn ["profile", "list", "--store", storeArg]
        listedStyled <- runWith [("FORCE_COLOR", "1")] vars "" ["profile", "list", "--store", storeArg]
        listedUnstyled <- runWith [("CLICOLOR_FORCE", ""), ("NOCOLOR", "")] vars "" ["profile", "list", "--store", storeArg]
        listedJson <- nn ["profile", "list", "--json", "--store", storeArg]
        prettyJson <- nn ["profile", "list", "--json", "--pretty", "--store", storeArg]
        compactJson <- nn ["profile", "list", "--pretty", "--json", "--no-pretty", "--store", storeArg]
        conflicting <- nn ["profile", "add", "--store", storeArg, inStore other]
        prioritised <- nn ["profile", "add", "--priority", "4", "--store", storeArg, inStore other]
        otherContent <- readIfThere helloPath
        addWorld <- nn ["profile", "add", "--store", storeArg, inStore world]
        roots <- nn ["store", "gc", "--print-roots", "--store", storeArg]
        removed <- nn ["profile", "remove", "--store", storeArg, "hello", "other"]
        helloAfter <- Dir.doesPathExist helloPath
        worldAfter <- Dir.doesPathExist worldPath
        collected <- nn ["store", "gc", "--store", storeArg]
        generationsKept <- traverse (\n -> Dir.doesPathExist (profiles </> ("profile-" <> show (n :: Int) <> "-link") </> "manifest.json")) [1, 2, 3, 4]
        helloKept <- Dir.doesPathExist (inStore hello)
        index <- nn ["profile", "remove", "--store", storeArg, "3"]
        nothingSelected <- nn ["profile", "remove", "--store", storeArg]
        built <- nn ["profile", "add", "--store", storeArg, "--expr", T.unpack twoOutputs]
        toInstall <- nn ["profile", "add", "--store", storeArg, "--expr", "(" <> T.unpack twoOutputs <> ") // { meta.outputsToInstall = [ \"dev\" ]; }"]
        specified <- nn ["profile", "add", "--store", storeArg, "--expr", "(" <> T.unpack twoOutputs <> ").dev // { outputSpecified = true; }"]
        builtJson <- nn ["profile", "list", "--json", "--store", storeArg]
        refusedOther <- nn ["profile", "add", "--store", root </> "other-store", "--expr", T.unpack twoOutputs]
        otherStoreMade <- Dir.doesPathExist (root </> "other-store" </> ".nova-nix")
        listedOther <- nn ["profile", "list", "--store", root </> "other-store"]
        createDirectoryIfMissing True (root </> "up" </> "nix" </> "store")
        writeStaged (root </> "up" </> "nix" </> "var" </> "nix" </> "db" </> "db.sqlite") ""
        upstream <- nn ["profile", "add", "--store", root </> "up" </> "nix" </> "store", inStore hello]
        xdgCases <-
          if onWindows
            then [] <$ putStrLn "  SKIP  XDG_STATE_HOME: Windows reads the Local AppData folder in its place"
            else do
              xdgListed <- runIn (locations (root </> "home-xdg") [("XDG_STATE_HOME", root </> "xdg-state")] []) "" ["profile", "list", "--store", storeArg]
              xdgLink <- linkIfThere (root </> "home-xdg" </> ".nix-profile")
              sequence
                [ runTest "with NIX_STATE_HOME unset, profiles are in XDG_STATE_HOME's nix" $
                    succeeded xdgListed (assertEqual "link" (Just (root </> "xdg-state" </> "nix" </> "profiles" </> "profile")) xdgLink)
                ]
        stateLinked <- runIn (locations (root </> "home-state-link") [("NIX_STATE_HOME", root </> "state-link")] []) "use-xdg-base-directories = true\n" ["profile", "list", "--store", storeArg]
        stateLink <- linkIfThere (root </> "state-link" </> "profile")
        homeLinkMade <- Dir.doesPathExist (root </> "home-state-link" </> ".nix-profile")
        -- A profile --profile names, for rollbacks and the history, with
        -- packages of its own, added after the collection above, which took
        -- what no root kept then.
        let historyProfile = root </> "hist" </> "profile"
            alpha = profileFixture 'd' "alpha-1.0"
            beta = profileFixture 'f' "beta-2.0"
            onHistory args = nn (["profile"] ++ args ++ ["--store", storeArg, "--profile", historyProfile])
            historyLink n = historyProfile <> "-" <> show (n :: Int) <> "-link"
            historyLinks = traverse (fmap isJust . linkIfThere . historyLink) [1, 2, 3, 4]
            historyArgs = ["profile", "history", "--store", storeArg, "--profile", historyProfile]
            utcDay = T.pack . formatTime defaultTimeLocale "%Y-%m-%d" <$> getCurrentTime
            defaultProfileState = (,) . sort <$> Dir.listDirectory profiles <*> linkIfThere (profiles </> "profile")
        writeStaged (stage "alpha" </> "bin" </> "alpha") (script "alpha")
        writeStaged (stage "beta" </> "bin" </> "beta") (script "beta")
        historyStore <- openStore testLogger storeDir
        mapM_ (\(name, path) -> addToStore historyStore (stage name) path Nothing []) [("alpha", alpha), ("beta", beta)]
        closeStore historyStore
        dayBefore <- utcDay
        historyMade <- traverse onHistory [["add", inStore alpha], ["add", inStore beta], ["remove", "alpha"]]
        rolledBack <- onHistory ["rollback"]
        rolledLink <- linkIfThere historyProfile
        alphaBack <- Dir.doesPathExist (historyProfile </> "bin" </> "alpha")
        rolledTo <- onHistory ["rollback", "--to", "3"]
        toLink <- linkIfThere historyProfile
        alphaOff <- Dir.doesPathExist (historyProfile </> "bin" </> "alpha")
        dryRolled <- onHistory ["rollback", "--dry-run"]
        dryLink <- linkIfThere historyProfile
        missingVersion <- onHistory ["rollback", "--to", "9"]
        badUnit <- onHistory ["rollback", "--to", "5x", "--to", "2"]
        toOldest <- onHistory ["rollback", "--to", "1"]
        noOlder <- onHistory ["rollback"]
        backToSecond <- onHistory ["rollback", "--to", "2"]
        afterRollback <- onHistory ["remove", "beta"]
        fourthLink <- linkIfThere historyProfile
        wipeDry <- onHistory ["wipe-history", "--dry-run"]
        wipeRecent <- onHistory ["wipe-history", "--older-than", "1d"]
        linksKept <- historyLinks
        wipeUnsuffixed <- onHistory ["wipe-history", "--older-than", "14"]
        wipeZero <- onHistory ["wipe-history", "--older-than", "0d"]
        defaultBefore <- defaultProfileState
        wipeForeign <- nn ["profile", "wipe-history", "--store", root </> "other-store"]
        rollbackForeign <- nn ["profile", "rollback", "--store", root </> "other-store"]
        defaultAfter <- defaultProfileState
        -- The first two generations' links take a store path's time, a
        -- second past the epoch, long before any cutoff.  The other two keep
        -- the time they were made, where the paths they lead to have a store
        -- path's time too, so a date of theirs is their link's own.
        mapM_ (canonicalizeMetadata . historyLink) [1, 2]
        printedHistory <- bytesIn [] historyArgs
        colouredHistory <- bytesIn [("FORCE_COLOR", "1")] historyArgs
        -- NO_COLOR counts as set whatever its value, as upstream reads it:
        -- given one, and given none, which reads as set on Windows too.
        uncolouredHistory <- bytesIn [("FORCE_COLOR", "1"), ("NO_COLOR", "1")] historyArgs
        blankNoColourHistory <- bytesIn [("FORCE_COLOR", "1"), ("NO_COLOR", "")] historyArgs
        dayAfter <- utcDay
        wipeOld <- onHistory ["wipe-history", "--older-than", "5d"]
        oldLinks <- historyLinks
        wipeAll <- onHistory ["wipe-history"]
        allLinks <- historyLinks
        swept <- nn ["store", "gc", "--store", storeArg]
        betaSwept <- not <$> Dir.doesPathExist (inStore beta)
        alphaKept <- Dir.doesPathExist (inStore alpha)
        let jsonOf paths = "{\"elements\":{" <> T.intercalate "," paths <> "},\"version\":3}"
            elementJson name priority paths = "\"" <> name <> "\":{\"active\":true,\"priority\":" <> T.pack (show (priority :: Int)) <> ",\"storePaths\":[" <> T.intercalate "," ["\"" <> path <> "\"" | path <- paths] <> "]}"
            builtPaths = case builtJson of
              (ExitSuccess, out, _) -> either (const []) pathsOf (parseJson (T.strip (T.pack out)))
              _ -> []
            pathsOf json = case json of
              JsonObject top | Just (JsonObject elements) <- Map.lookup "elements" top -> [(name, [p | JsonString p <- paths]) | (name, JsonObject element) <- Map.toList elements, Just (JsonArray paths) <- [Map.lookup "storePaths" element]]
              _ -> []
            outputOf name = filter (T.isSuffixOf ("-two-1.0" <> name))
            ranWell (code, _, _) = code == ExitSuccess
            exitOf (code, _, _) = code
            -- A version line names the day its generation was made, which
            -- for a generation this run made is the day it began or ended.
            undated line = foldr (\day -> T.replace ("(" <> day <> ")") "(DAY)") line [dayBefore, dayAfter]
            historyIs (code, printed, complaint) expected = case (code, TE.decodeUtf8' (BS8.filter (/= '\r') printed)) of
              (ExitSuccess, Right text) -> assertEqual "lines" expected (map undated (T.lines text))
              _ -> Fail ("exit " <> T.pack (show code) <> "; stdout=" <> T.pack (show printed) <> "; stderr=" <> T.pack (show complaint))
            historyLines = versionLines ("", "")
            colouredLines = versionLines ("\ESC[1m", "\ESC[32;1m")
            versionLines (bold, green) =
              [ "Version " <> bold <> "1" <> reset bold <> " (1970-01-01):",
                "  alpha: \x2205 -> 1.0",
                "",
                "Version " <> bold <> "2" <> reset bold <> " (1970-01-01) <- 1:",
                "  beta: \x2205 -> 2.0",
                "",
                "Version " <> bold <> "3" <> reset bold <> " (DAY) <- 2:",
                "  alpha: 1.0 -> \x2205",
                "",
                "Version " <> green <> "4" <> reset green <> " (DAY) <- 3:",
                "  alpha: \x2205 -> 1.0",
                "  beta: 2.0 -> \x2205"
              ]
            reset escape = if T.null escape then "" else "\ESC[0m"
            staleRecordOf n line = "removing stale link from \"" `isPrefixOf` line && "hist" `isInfixOf` line && ("profile-" <> show n <> "-link\"") `isSuffixOf` line
        (++ xdgCases)
          <$> sequence
            [ runTest "profile list of a profile that is not there prints nothing and makes ~/.nix-profile" $
                succeeded listedEmpty (assertEqual "stdout" "" (let (_, out, _) = listedEmpty in out) `andThen` assertEqual "link" (Just (root </> "state" </> "profiles" </> "profile")) emptyLink),
              runTest "profile add puts the program under ~/.nix-profile/bin" $
                succeeded addHello (assertEqual "bin/hello" (Just (script "hello from the profile")) helloContent),
              runTest "the program runs through ~/.nix-profile/bin" $
                case ran of
                  Nothing -> Pass
                  Just (code, out, err) -> assertEqual "run" (ExitSuccess, "hello from the profile\n") (code, out) `andThen` assertEqual "stderr" "" err,
              runTest "profile list prints upstream's lines, the name in bold where colour is forced and in neither a pipe nor under NOCOLOR"
                $ succeeded listedText . succeeded listedStyled . succeeded listedUnstyled
                $ let printed (_, out, _) = lines (filter (/= '\r') out)
                      paths = "Store paths:        " <> inStore hello
                   in assertEqual "a pipe" ["Name:               hello", paths] (printed listedText)
                        `andThen` assertEqual "forced" ["Name:               \ESC[1mhello\ESC[0m", paths] (printed listedStyled)
                        `andThen` assertEqual "NOCOLOR" ["Name:               hello", paths] (printed listedUnstyled),
              runTest "profile list --json prints the manifest, paths in the canonical spelling"
                $ succeeded listedJson
                $ assertEqual "json" (jsonOf [elementJson "hello" 5 [canonical hello]]) (let (_, out, _) = listedJson in T.strip (T.pack out)),
              runTest "profile list --json --pretty indents the manifest, and a later --no-pretty puts it on one line"
                $ succeeded prettyJson . succeeded compactJson
                $ let (_, compactOut, _) = compactJson
                      (_, prettyOut, _) = prettyJson
                   in case parseJson (T.strip (T.pack compactOut)) of
                        Right json ->
                          assertEqual "indented" (Ref.renderIndentedJson json) (TE.encodeUtf8 (T.pack (filter (/= '\r') prettyOut)))
                            `andThen` assertEqual "one line" 1 (length (lines compactOut))
                        Left err -> Fail err,
              runTest "a file at one path in two packages of one priority fails the add in upstream's words, with nova-nix's commands" $
                case conflicting of
                  (ExitFailure 1, _, _) ->
                    assertEqual "first line" (Just "error: An existing package already provides the following file:") (listToMaybe (stderrLines conflicting))
                      `andThen` assertEqual "hint" True (("         nova-nix profile remove hello" :: String) `elem` stderrLines conflicting)
                  _ -> Fail (outcome conflicting),
              runTest "each generation is a root, listed by its link"
                $ succeeded addWorld . succeeded roots
                $ let (_, out, _) = roots
                      rootLinks = [takeFileName (takeWhile (/= ' ') line) | line <- lines (filter (/= '\r') out)]
                   in assertEqual "links" ["profile-1-link", "profile-2-link", "profile-3-link"] (filter ("profile-" `isPrefixOf`) rootLinks),
              runTest "profile remove takes the program off, in upstream's words, and keeps the rest"
                $ succeeded removed
                $ assertEqual "lines" ["removing 'hello'", "removing 'other'", "removed 2 packages, kept 1 packages"] (stderrLines removed)
                  `andThen` assertEqual "bin/hello, bin/world" (False, True) (helloAfter, worldAfter),
              runTest "store gc keeps every generation and what only an old one holds" $
                succeeded collected (assertEqual "generations" [True, True, True, True] generationsKept `andThen` assertEqual "hello" True helloKept),
              runTest "a lower priority number puts a package's file in place of one already there" $
                succeeded prioritised (assertEqual "bin/hello" (Just (script "the other hello")) otherContent),
              runTest "a number is refused as upstream refuses an index" $
                assertEqual "index" (ExitFailure 1, ["error: 'nova-nix profile' does not support indices ('3')"]) (let (code, _, _) = index in code, stderrLines index),
              runTest "profile remove with nothing selected is a usage error, before the profile is read" $
                assertEqual "usage" (ExitFailure 1, ["profile remove: no packages specified"]) (let (code, _, _) = nothingSelected in code, stderrLines nothingSelected),
              runTest "a derivation is built and added for every output, the one its outputSpecified selects, or those meta.outputsToInstall names" $
                succeeded built . succeeded toInstall . succeeded specified $
                  case (lookup "two" builtPaths, lookup "two-1" builtPaths, lookup "two-2" builtPaths) of
                    (Just every, Just installed, Just selected) ->
                      assertEqual "every output" 2 (length every)
                        `andThen` assertEqual "outputsToInstall" (outputOf "-dev" every) installed
                        `andThen` assertEqual "outputSpecified" (outputOf "-dev" every) selected
                    _ -> Fail (T.pack (show builtPaths)),
              runTest "a profile whose generation is in another store is refused before anything is built" $
                case refusedOther of
                  (ExitFailure 1, _, _) ->
                    assertEqual "names the flags" True (any (\line -> "--store" `isInfixOf` line && "--profile" `isInfixOf` line) (stderrLines refusedOther))
                      `andThen` assertEqual "nothing built" False otherStoreMade
                  _ -> Fail (outcome refusedOther),
              runTest "profile list refuses a profile whose generation is in another store, as the commands that change one do" $
                case listedOther of
                  (ExitFailure 1, "", _) -> assertEqual "names the flags" True (any (\line -> "--store" `isInfixOf` line && "--profile" `isInfixOf` line) (stderrLines listedOther))
                  _ -> Fail (outcome listedOther),
              runTest "a store upstream Nix manages is refused, as build refuses it" $
                case upstream of
                  (ExitFailure 1, _, err) | "profile add: " `isPrefixOf` err -> Pass
                  _ -> Fail (outcome upstream),
              runTest "use-xdg-base-directories links the default profile from the state directory, not the home" $
                succeeded stateLinked (assertEqual "link" (Just (root </> "state-link" </> "profiles" </> "profile")) stateLink `andThen` assertEqual "home link" False homeLinkMade),
              runTest "a rollback switches the profile to the generation before the current one, which brings back what a later change took off" $
                case filter (not . ranWell) historyMade of
                  [] -> succeeded rolledBack (assertEqual "line" ["switching profile from version 3 to 2"] (stderrLines rolledBack) `andThen` assertEqual "link" (Just "profile-2-link") rolledLink `andThen` assertEqual "bin/alpha" True alphaBack)
                  failed -> Fail (T.intercalate "; " (map outcome failed)),
              runTest "rollback --to switches the profile to the generation it names" $
                succeeded rolledTo (assertEqual "line" ["switching profile from version 2 to 3"] (stderrLines rolledTo) `andThen` assertEqual "link" (Just "profile-3-link") toLink `andThen` assertEqual "bin/alpha" False alphaOff),
              runTest "rollback --dry-run says where it would switch the profile, and switches nothing" $
                succeeded dryRolled (assertEqual "line" ["switching profile from version 3 to 2"] (stderrLines dryRolled) `andThen` assertEqual "link" (Just "profile-3-link") dryLink),
              runTest "a rollback to a generation the profile lacks, or from its oldest, and one with any --to that does not read, are refused in upstream's words" $
                assertEqual "--to 9" (ExitFailure 1, ["error: profile version 9 does not exist"]) (exitOf missingVersion, stderrLines missingVersion)
                  `andThen` assertEqual "--to 5x before a --to that reads" (ExitFailure 1, ["error: invalid unit specifier 'X'"]) (exitOf badUnit, stderrLines badUnit)
                  `andThen` succeeded toOldest (assertEqual "from the oldest" (ExitFailure 1, ["error: no profile version older than the current (1) exists"]) (exitOf noOlder, stderrLines noOlder)),
              runTest "a change after a rollback makes the generation after the newest, not after the current one"
                $ succeeded backToSecond . succeeded afterRollback
                $ assertEqual "link" (Just "profile-4-link") fourthLink,
              runTest "wipe-history --dry-run names every generation but the current one, and --older-than keeps those made since its cutoff"
                $ succeeded wipeDry . succeeded wipeRecent
                $ assertEqual "dry run" ["would remove profile version 1", "would remove profile version 2", "would remove profile version 3"] (stderrLines wipeDry)
                  `andThen` assertEqual "since the cutoff" [] (stderrLines wipeRecent)
                  `andThen` assertEqual "links" [True, True, True, True] linksKept,
              runTest "an --older-than without its d, or of fewer than one day, is refused in upstream's words" $
                assertEqual "14" (ExitFailure 1, ["error: invalid number of days specifier '14', expected something like '14d'"]) (exitOf wipeUnsuffixed, stderrLines wipeUnsuffixed)
                  `andThen` assertEqual "0d" (ExitFailure 1, ["error: invalid number of days specifier '0d'"]) (exitOf wipeZero, stderrLines wipeZero),
              runTest "rollback and wipe-history refuse a profile whose current generation is in another store, and change nothing" $
                case (exitOf wipeForeign, exitOf rollbackForeign) of
                  (ExitFailure 1, ExitFailure 1) ->
                    assertEqual "names the flags" [True, True] [any (\line -> "--store" `isInfixOf` line && "--profile" `isInfixOf` line) (stderrLines run) | run <- [wipeForeign, rollbackForeign]]
                      `andThen` assertEqual "the default profile" defaultBefore defaultAfter
                  _ -> Fail (outcome wipeForeign <> "; " <> outcome rollbackForeign),
              runTest "profile history prints each generation's day in UTC by its link's own time, and what changed from the one before it, in upstream's lines" $
                historyIs printedHistory historyLines,
              runTest "profile history sets the current generation's number in green and the others' in bold where colour is forced, and in neither under NO_COLOR" $
                historyIs colouredHistory colouredLines `andThen` historyIs uncolouredHistory historyLines `andThen` historyIs blankNoColourHistory historyLines,
              runTest "wipe-history --older-than deletes what was made before its cutoff but the newest of those" $
                succeeded wipeOld (assertEqual "line" ["removing profile version 1"] (stderrLines wipeOld) `andThen` assertEqual "links" [False, True, True, True] oldLinks),
              runTest "wipe-history deletes every generation but the current one" $
                succeeded wipeAll (assertEqual "lines" ["removing profile version 2", "removing profile version 3"] (stderrLines wipeAll) `andThen` assertEqual "links" [False, False, False, True] allLinks),
              runTest "the next collection drops the deleted generations' records in upstream's line and deletes what only they kept"
                $ succeeded swept
                $ assertEqual "stale records" [1, 2, 3] [n | n <- [1, 2, 3 :: Int], any (staleRecordOf n) (stderrLines swept)]
                  `andThen` assertEqual "beta deleted, alpha kept" (True, True) (betaSwept, alphaKept)
            ]

-- ---------------------------------------------------------------------------
-- Tests: CLI Integration (Phase 2, Batch 5)
-- ---------------------------------------------------------------------------

-- | A tree holding a reserved device stem, a colon and a trailing dot,
-- names POSIX holds and Windows does not, in the order a sorted listing
-- gives them.  The colon follows more than one letter: bsdtar, which
-- fetchTarball's extraction runs on macOS, drops a one-letter prefix
-- such as @a:@ as a drive letter once @--strip-components@ has exposed
-- it.
reservedNameFiles :: [(FilePath, BS.ByteString)]
reservedNameFiles = [("aux.c", "device\n"), ("dot.", "dot\n"), ("key:value", "colon\n")]

-- | Where upstream stores 'reservedNameFiles' as a coerced source
-- directory named @src@ (nix-instantiate 2.33.2, @"${./src}"@) and as
-- the tree of a fetchTarball (with @--store 'local?root=...'@, which
-- holds it under exactly these names).
reservedNameSourcePath, reservedNameTarballPath :: StorePath
reservedNameSourcePath = StorePath "42hs0hrpla9b23iz9p3nj0px0np0jv6f" "src"
reservedNameTarballPath = StorePath "rlwykawmqrmdiq4xmcr4rf2zxf6y9k09" "source"

testE2E :: IO [Bool]
testE2E = do
  putStrLn "cli/e2e"
  shell <- findTestShell
  sequence
    [ -- End-to-end: eval -> build a simple derivation
      runTestM "e2e eval -> build" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-e2e1"
        forceRemoveIfExists tmpStore
        -- Build the Nix source with the discovered shell path.
        -- Nix strings use \\ for literal backslash, \" for literal quote.
        let nixEscape = T.concatMap (\c -> if c == '\\' then "\\\\" else if c == '"' then "\\\"" else T.singleton c)
            e2eSource =
              T.concat
                [ "derivation { name = \"e2e-test\"; system = builtins.currentSystem; " <> hostToolsAttrs,
                  "builder = \"" <> nixEscape shell <> "\"; ",
                  "args = [\"-c\" \"mkdir -p $out && echo e2e > $out/e2e.txt\"]; }"
                ]
        result <- evalAndBuild (StoreDir tmpStore) e2eSource
        ret <- case result of
          Left err -> pure (Fail err)
          Right (BuildSuccess _, store) -> do
            closeStore store
            pure Pass
          Right (BuildFailure msg code, store) -> do
            closeStore store
            pure (Fail ("build failed (" <> T.pack (show code) <> "): " <> msg))
        forceRemoveIfExists tmpStore
        forceRemoveIfExists (tmpBase </> "nova-nix-e2e-tmp")
        pure ret,
      -- Every eval-time store writer must register, not only toFile: an
      -- unrecorded write reaches drvInputSrcs and the build dies with
      -- "path '...' is not valid".
      runTestM "e2e eval-time store writes register as derivation inputs" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-e2e-inputs"
            srcDir = tmpBase </> "nova-nix-test-e2e-inputs-src"
            nixEscape = T.concatMap (\c -> if c == '\\' then "\\\\" else if c == '"' then "\\\"" else T.singleton c)
            drvWith srcExpr =
              T.concat
                [ "derivation { name = \"inputs-test\"; system = builtins.currentSystem; ",
                  "builder = \"" <> nixEscape shell <> "\"; ",
                  "args = [\"-c\" \"echo ok > $out\"]; ",
                  "src = " <> srcExpr <> "; }"
                ]
        forceRemoveIfExists tmpStore
        forceRemoveIfExists srcDir
        Dir.createDirectoryIfMissing True srcDir
        writeFile (srcDir </> "f.txt") "content\n"
        let cases =
              [ ("toFile", "builtins.toFile \"note\" \"text\""),
                -- A path LITERAL, not a string, so it must be spelled the
                -- way the evaluator spells path values: the lexer rejects a
                -- backslash, and a native Windows temp dir is full of them.
                -- Through 'canonPathValue' rather than a separator replace,
                -- because a backslash is an ordinary file-name character on
                -- POSIX and only a separator on Windows.
                ("path", "builtins.path { path = " <> canonPathValue (T.pack srcDir) <> "; name = \"sd\"; }")
              ]
        outcomes <-
          mapM
            ( \(label, expr) -> do
                result <- evalAndBuild (StoreDir tmpStore) (drvWith expr)
                pure (label, result)
            )
            cases
        let failures =
              [ label <> ": " <> reason
              | (label, result) <- outcomes,
                Just reason <-
                  [ case result of
                      Left err -> Just err
                      Right (BuildFailure msg _, _) -> Just msg
                      Right (BuildSuccess _, _) -> Nothing
                  ]
              ]
        mapM_
          ( \(_, result) -> case result of
              Right (_, store) -> closeStore store
              Left _ -> pure ()
          )
          outcomes
        forceRemoveIfExists tmpStore
        forceRemoveIfExists srcDir
        pure $ case failures of
          [] -> Pass
          errs -> Fail (T.intercalate "; " errs),
      -- POSIX holds names Windows reserves, and upstream stores them as
      -- their bytes there.  A coerced source and a fetchTarball
      -- tree holding a device stem, a colon and a trailing dot restore
      -- with exactly those names, under the paths upstream computes
      -- for them ('reservedNameSourcePath'), as the raw copy they
      -- replace stored them.
      runTestM "e2e sources holding names Windows reserves restore with their names" $
        if SI.os == "mingw32"
          then Pass <$ putStrLn "  SKIP  a Windows volume cannot hold these names"
          else do
            tmpBase <- getTemporaryDirectory
            environment <- getEnvironment
            let root = tmpBase </> "nova-nix-test-e2e-names"
                srcDir = root </> "src"
                stage = root </> "stage"
                tarball = root </> "names.tar.gz"
                tmpStore = tmpBase </> "nova-nix-test-e2e-names-store"
                nixEscape = T.concatMap (\c -> if c == '\\' then "\\\\" else if c == '"' then "\\\"" else T.singleton c)
                source =
                  T.concat
                    [ "derivation { name = \"names\"; system = builtins.currentSystem; ",
                      "builder = \"" <> nixEscape shell <> "\"; ",
                      "args = [\"-c\" \"echo ok > $out\"]; ",
                      "srcs = [ " <> canonPathValue (T.pack srcDir) <> " ",
                      "(builtins.fetchTarball \"file://" <> nixEscape (T.pack tarball) <> "\") ]; }"
                    ]
                -- bsdtar would otherwise add AppleDouble entries for
                -- any extended attributes the files carry.
                tarProcess =
                  (Proc.proc "tar" ["-czf", tarball, "-C", stage, "top"])
                    { Proc.env = Just (("COPYFILE_DISABLE", "1") : environment)
                    }
                writeTree dir = do
                  createDirectoryIfMissing True dir
                  mapM_ (\(name, bytes) -> BS.writeFile (dir </> name) bytes) reservedNameFiles
                storedAs store sp = do
                  let dest = storePathToFilePath (stDir store) sp
                  present <- doesDirectoryExist dest
                  if not present
                    then pure (Just (spName sp <> ": no tree at " <> T.pack dest))
                    else do
                      names <- sort <$> Dir.listDirectory dest
                      contents <- traverse (BS.readFile . (dest </>)) names
                      valid <- isValid store sp
                      pure $
                        if valid && zip names contents == reservedNameFiles
                          then Nothing
                          else Just (spName sp <> " valid " <> T.pack (show valid) <> ", stored as " <> T.pack (show (zip names contents)))
            forceRemoveIfExists root
            forceRemoveIfExists tmpStore
            writeTree srcDir
            writeTree (stage </> "top")
            (tarCode, _, tarErr) <- Proc.readCreateProcessWithExitCode tarProcess ""
            verdict <- case tarCode of
              ExitFailure n -> pure (Fail ("tar exited " <> T.pack (show n) <> ": " <> T.pack tarErr))
              ExitSuccess -> do
                built <- evalAndBuild (StoreDir tmpStore) source
                case built of
                  Left err -> pure (Fail err)
                  Right (BuildFailure msg _, store) -> Fail msg <$ closeStore store
                  Right (BuildSuccess _, store) -> do
                    problems <- catMaybes <$> traverse (storedAs store) [reservedNameSourcePath, reservedNameTarballPath]
                    closeStore store
                    pure (if null problems then Pass else Fail (T.intercalate "; " problems))
            forceRemoveIfExists root
            forceRemoveIfExists tmpStore
            pure verdict,
      -- The tree the issue compared with nix-instantiate's: an evaluation
      -- alone, which registers nothing, leaves it as upstream's
      -- addToStoreFromDump does, a file 0444 and an executable 0555, each
      -- with the store's time, whatever modes the archive held.
      runTestM "an eval-time fetchTarball's tree takes upstream's modes and the store's time" $
        if SI.os == "mingw32"
          then Pass <$ putStrLn "  SKIP  the modes the issue compares are POSIX's"
          else do
            tmpBase <- getTemporaryDirectory
            environment <- getEnvironment
            let root = tmpBase </> "nova-nix-test-eval-tarball"
                stage = root </> "stage"
                tarball = root </> "tree.tar.gz"
                storeDir = StoreDir (root </> "store")
                nixEscape = T.concatMap (\c -> if c == '\\' then "\\\\" else if c == '"' then "\\\"" else T.singleton c)
                tarProcess =
                  (Proc.proc "tar" ["-czf", tarball, "-C", stage, "top"])
                    { Proc.env = Just (("COPYFILE_DISABLE", "1") : environment)
                    }
            forceRemoveIfExists root
            createDirectoryIfMissing True (stage </> "top")
            BS.writeFile (stage </> "top" </> "data") "data\n"
            BS.writeFile (stage </> "top" </> "run") "#!/bin/sh\n"
            ExecBit.markExecutable (stage </> "top" </> "run")
            (tarCode, _, tarErr) <- Proc.readCreateProcessWithExitCode tarProcess ""
            verdict <- case tarCode of
              ExitFailure n -> pure (Fail ("tar exited " <> T.pack (show n) <> ": " <> T.pack tarErr))
              ExitSuccess -> do
                fetched <- evalNixIOStore storeDir root ("builtins.fetchTarball \"file://" <> nixEscape (T.pack tarball) <> "\"")
                case fetched of
                  Right val | Just sp <- fetchedStorePath val -> do
                    let dest = storePathToFilePath storeDir sp
                    foldr andThen Pass
                      <$> traverse
                        (uncurry canonicalAs)
                        [(executablePermissions, dest), (readOnlyPermissions, dest </> "data"), (executablePermissions, dest </> "run")]
                  other -> pure (Fail ("fetchTarball gave " <> T.pack (show other)))
            forceRemoveIfExists root
            pure verdict,
      -- The writer half of the eval-write pair: an interrupted earlier
      -- run's truncated file at a toFile path must be rewritten, not
      -- adopted on bare existence (adopting it would register such a file
      -- valid under the full content's hash, seal it read-only, and let a
      -- build consume it).
      runTestM "a truncated pre-existing toFile write is rewritten, not adopted" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-tofile-verify"
            nixEscape = T.concatMap (\c -> if c == '\\' then "\\\\" else if c == '"' then "\\\"" else T.singleton c)
            goodText = "the full intended content" :: Text
            goodBytes = TE.encodeUtf8 goodText
        forceRemoveIfExists tmpStore
        case makeTextPath "note" (sha256Digest goodBytes) [] of
          Left err -> pure (Fail ("test store path rejected: " <> T.pack (show err)))
          Right sp -> do
            let dest = storePathToFilePath (StoreDir tmpStore) sp
            Dir.createDirectoryIfMissing True (takeDirectory dest)
            BS.writeFile dest (TE.encodeUtf8 "the full")
            let expr =
                  "derivation { name = \"tofile-verify\"; system = builtins.currentSystem; "
                    <> "builder = \""
                    <> nixEscape shell
                    <> "\"; "
                    <> "args = [\"-c\" \"echo ok > $out\"]; "
                    <> "src = builtins.toFile \"note\" \""
                    <> goodText
                    <> "\"; }"
            result <- evalAndBuild (StoreDir tmpStore) expr
            rewritten <- BS.readFile dest
            outcome <- case result of
              Left err -> pure (Fail err)
              Right (BuildFailure msg _, store) -> closeStore store >> pure (Fail msg)
              Right (BuildSuccess _, store) -> do
                registered <- isValid store sp
                closeStore store
                pure $
                  if rewritten == goodBytes && registered
                    then Pass
                    else Fail "adopted the truncated file instead of rewriting it"
            forceRemoveIfExists tmpStore
            pure outcome,
      -- Same guarantee for the tree writer behind builtins.path: a
      -- corrupted partial tree at the computed path is cleared and
      -- re-unpacked, never adopted.
      runTestM "a corrupted pre-existing builtins.path tree is re-copied, not adopted" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-path-verify"
            srcDir = tmpBase </> "nova-nix-test-path-verify-src"
            nixEscape = T.concatMap (\c -> if c == '\\' then "\\\\" else if c == '"' then "\\\"" else T.singleton c)
        forceRemoveIfExists tmpStore
        forceRemoveIfExists srcDir
        Dir.createDirectoryIfMissing True srcDir
        BS.writeFile (srcDir </> "f.txt") (TE.encodeUtf8 "real content\n")
        entry <- NAR.serialiseFromPath srcDir
        case makeFixedOutputPath "sd" "sha256" NarIngestion (sha256Digest (NAR.serialise entry)) of
          Left err -> pure (Fail ("test store path rejected: " <> T.pack (show err)))
          Right sp -> do
            let dest = storePathToFilePath (StoreDir tmpStore) sp
            Dir.createDirectoryIfMissing True dest
            BS.writeFile (dest </> "f.txt") (TE.encodeUtf8 "partial garbage")
            let expr =
                  "derivation { name = \"path-verify\"; system = builtins.currentSystem; "
                    <> "builder = \""
                    <> nixEscape shell
                    <> "\"; "
                    <> "args = [\"-c\" \"echo ok > $out\"]; "
                    <> "src = builtins.path { path = "
                    <> canonPathValue (T.pack srcDir)
                    <> "; name = \"sd\"; }; }"
            result <- evalAndBuild (StoreDir tmpStore) expr
            outcome <- case result of
              Left err -> pure (Fail err)
              Right (BuildFailure msg _, store) -> closeStore store >> pure (Fail msg)
              Right (BuildSuccess _, store) -> do
                -- Strict bytes: a lazy read here holds the file open past
                -- the cleanup below, and Windows refuses to delete it.
                adopted <- BS.readFile (dest </> "f.txt")
                registered <- isValid store sp
                closeStore store
                pure $
                  if adopted == TE.encodeUtf8 "real content\n" && registered
                    then Pass
                    else Fail ("adopted the corrupted tree: " <> T.pack (show adopted))
            forceRemoveIfExists tmpStore
            forceRemoveIfExists srcDir
            pure outcome,
      -- The build path joins the per-path lock protocol that delete,
      -- materialize and register use.  Lock files persist
      -- by design, so the one guarding the output is the observable
      -- evidence the build took it; without it two concurrent builds share
      -- a build directory and interleave into one tree.
      runTestM "e2e a build takes its output's path lock" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-e2e-lock"
            nixEscape = T.concatMap (\c -> if c == '\\' then "\\\\" else if c == '"' then "\\\"" else T.singleton c)
            lockSource =
              T.concat
                [ "derivation { name = \"lock-test\"; system = builtins.currentSystem; ",
                  "builder = \"" <> nixEscape shell <> "\"; ",
                  "args = [\"-c\" \"echo locked > $out\"]; }"
                ]
        forceRemoveIfExists tmpStore
        result <- evalAndBuild (StoreDir tmpStore) lockSource
        ret <- case result of
          Left err -> pure (Fail err)
          Right (BuildFailure msg code, store) -> do
            closeStore store
            pure (Fail ("build failed (" <> T.pack (show code) <> "): " <> msg))
          Right (BuildSuccess sp, store) -> do
            closeStore store
            let outPath = storePathToFilePath (StoreDir tmpStore) sp
            locked <- Dir.doesFileExist (outPath <> ".lock")
            pure $
              if locked
                then Pass
                else Fail "no lock file beside the built output"
        forceRemoveIfExists tmpStore
        forceRemoveIfExists (tmpBase </> "nova-nix-e2e-tmp")
        pure ret,
      -- Parse error produces Left
      runTestM "e2e parse error" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-e2e2"
        forceRemoveIfExists tmpStore
        result <- evalAndBuild (StoreDir tmpStore) "{{ invalid nix"
        forceRemoveIfExists tmpStore
        pure $ case result of
          Left msg ->
            if "parse error" `T.isInfixOf` msg
              then Pass
              else Fail ("expected parse error, got: " <> msg)
          Right _ -> Fail "expected error but got success",
      -- Eval error produces Left
      runTestM "e2e eval error" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-e2e3"
        forceRemoveIfExists tmpStore
        result <- evalAndBuild (StoreDir tmpStore) "derivation { }"
        forceRemoveIfExists tmpStore
        pure $ case result of
          Left msg ->
            if "error" `T.isInfixOf` T.toLower msg
              then Pass
              else Fail ("expected eval error, got: " <> msg)
          Right _ -> Fail "expected error but got success",
      -- E2E with subprocess: nova-nix eval on a .nix file
      runTestM "e2e nova-nix eval subprocess" $ do
        tmpBase <- getTemporaryDirectory
        let tmpDir = tmpBase </> "nova-nix-test-e2e-sub"
            nixFile = tmpDir </> "test.nix"
        forceRemoveIfExists tmpDir
        createDirectoryIfMissing True tmpDir
        writeFile nixFile "1 + 2"
        -- Run nova-nix eval via Process
        (exitCode, stdoutStr, stderrStr) <-
          Proc.readCreateProcessWithExitCode
            (Proc.proc "cabal" ["run", "exe:nova-nix", "--", "eval", nixFile])
            ""
        forceRemoveIfExists tmpDir
        pure $ case exitCode of
          ExitSuccess ->
            let nonEmpty = filter (not . T.null) (T.lines (T.pack stdoutStr))
                lastLine = case reverse nonEmpty of
                  (l : _) -> Just l
                  [] -> Nothing
             in if lastLine == Just "3"
                  then Pass
                  else Fail ("expected 3 as last line, got: " <> T.pack stdoutStr)
          ExitFailure code ->
            Fail ("nova-nix eval failed (" <> T.pack (show code) <> "): stderr=" <> T.pack stderrStr)
    ]

-- | Exercise the real CLI: testing 'execWrapperConfig' alone cannot catch
-- a sub-parser that never passes the option to it. The foreign builder is
-- deliberately "-c", so only launching it through the shell can succeed.
testExecWrapperCLI :: IO [Bool]
testExecWrapperCLI = do
  putStrLn "cli/exec-wrapper"
  shell <- findTestShell
  tmpBase <- getTemporaryDirectory
  ambient <- getEnvironment
  let root = tmpBase </> "nova-nix-test-cli-wrapper"
      system = platformToText (if currentPlatform == X86_64_Windows then X86_64_Linux else X86_64_Windows)
      wrapper = ["--exec-wrapper", T.unpack (system <> "=" <> shell)]
      otherWrapper = ["--exec-wrapper", T.unpack (platformToText currentPlatform <> "=" <> shell)]
      source =
        "derivation { name = \"cli-wrapper\"; system = \""
          <> T.unpack system
          <> "\"; builder = \"-c\"; args = [\"printf wrapped > \\\"$out\\\"\"]; }"
      target = ["--expr", source]
      isolatedEnv =
        ("NIX_CONFIG", "substituters =\ntrusted-public-keys =\n")
          : ("XDG_CONFIG_HOME", root </> "config")
          : filter (\(key, _) -> key `notElem` ["NIX_CONFIG", "XDG_CONFIG_HOME"]) ambient
      runCLI args =
        Proc.readCreateProcessWithExitCode
          ((Proc.proc "cabal" (["run", "-v0", "exe:nova-nix", "--"] ++ args)) {Proc.env = Just isolatedEnv})
          ""
      buildCases =
        [ ("before build", wrapper ++ ["build"] ++ target),
          ("before target", ["build"] ++ wrapper ++ target),
          ("after target", ["build"] ++ target ++ wrapper),
          ("repeated across build", otherWrapper ++ ["build"] ++ target ++ wrapper)
        ]
      errorCases =
        [ ("missing value", ["build"] ++ target ++ ["--exec-wrapper"], "--exec-wrapper requires a value"),
          ("malformed spec", ["build"] ++ target ++ ["--exec-wrapper", "invalid"], "--exec-wrapper expects SYSTEM=PATH, got: invalid"),
          ("duplicate system", wrapper ++ ["build"] ++ target ++ wrapper, "--exec-wrapper names " <> system <> " twice"),
          ("missing launcher", ["build"] ++ target ++ ["--exec-wrapper", T.unpack system ++ "=" ++ (root </> "missing-launcher")], "does not exist")
        ]
  bracket_
    (forceRemoveIfExists root >> createDirectoryIfMissing True root)
    (forceRemoveIfExists root)
    ( do
        builds <-
          mapM
            ( \(label, args) -> runTestM ("CLI wrapper " <> label) $ do
                (code, out, err) <- runCLI (args ++ ["--store", root </> T.unpack label, "--no-out-link"])
                case (code, reverse (filter (not . null) (lines out))) of
                  (ExitSuccess, path : _) -> assertEqual "wrapped output" "wrapped" <$> BS.readFile path
                  _ -> pure (Fail ("CLI failed: " <> T.pack (show code) <> "; stdout=" <> T.pack out <> "; stderr=" <> T.pack err))
            )
            buildCases
        errors <-
          mapM
            ( \(label, args, expected) -> runTestM ("CLI wrapper " <> label) $ do
                (code, _, err) <- runCLI args
                pure $
                  if code /= ExitSuccess && expected `T.isInfixOf` T.pack err
                    then Pass
                    else Fail ("expected failure containing " <> expected <> ", got " <> T.pack (show (code, err)))
            )
            errorCases
        pure (builds ++ errors)
    )

-- | A failure no command handles prints as upstream's handleExceptions
-- prints one, a single @error:@ line and exit status 1, not GHC's
-- "Uncaught exception" report.  A derivation whose context names a path
-- the store never registered meets the store's refusal while its @.drv@
-- is written, before anything builds.
testUncaughtIOFailureCLI :: IO [Bool]
testUncaughtIOFailureCLI = do
  putStrLn "cli/uncaught-io-failure"
  tmpBase <- getTemporaryDirectory
  ambient <- getEnvironment
  let root = tmpBase </> "nova-nix-test-cli-io-failure"
      missing :: String
      missing = "/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-gone"
      source =
        "derivation { name = \"unregistered\"; system = builtins.currentSystem; builder = \"/bin/sh\"; dep = builtins.appendContext \"\" { \""
          <> missing
          <> "\" = { path = true; }; }; }"
      isolatedEnv =
        ("NIX_CONFIG", "substituters =\ntrusted-public-keys =\n")
          : ("XDG_CONFIG_HOME", root </> "config")
          : filter (\(key, _) -> key `notElem` ["NIX_CONFIG", "XDG_CONFIG_HOME"]) ambient
  bracket_
    (forceRemoveIfExists root >> createDirectoryIfMissing True root)
    (forceRemoveIfExists root)
    ( do
        (code, _, err) <-
          Proc.readCreateProcessWithExitCode
            ((Proc.proc "cabal" ["run", "-v0", "exe:nova-nix", "--", "build", "--expr", source, "--store", root </> "store"]) {Proc.env = Just isolatedEnv})
            ""
        sequence
          [ runTest "uncaught I/O failure exits 1" (assertEqual "exit code" (ExitFailure 1) code),
            runTest "uncaught I/O failure prints upstream's error line" $
              assertEqual "stderr" ["error: path '" <> missing <> "' is not valid"] (map (filter (/= '\r')) (lines err))
          ]
    )

-- | Runs whose stdout is a pipe nothing reads, as after @| head -1@ has
-- read its line.  Upstream's writes through @std::cout@ then fail with
-- the stream's error state alone, SIGPIPE being blocked
-- (@unix::startSignalHandlerThread@ in libutil\/unix\/signals.cc), so
-- nothing is reported and the command ends with its own status
-- (@processExpr@ in nix-instantiate.cc): an eval or @--help@ exits 0, and
-- @store delete@, which prints each path as it deletes it, deletes every
-- path it names.  The read end is closed before the process starts, so
-- its first write meets it.
testClosedStdoutCLI :: IO [Bool]
testClosedStdoutCLI = do
  putStrLn "cli/closed-stdout"
  tmpBase <- getTemporaryDirectory
  ambient <- getEnvironment
  let root = tmpBase </> "nova-nix-test-cli-closed-stdout"
      store = root </> "store"
      doomed = [T.unpack (T.replicate 32 letter) <> "-doomed" | letter <- ["a", "b"]]
      isolatedEnv =
        ("NIX_CONFIG", "substituters =\ntrusted-public-keys =\n")
          : ("XDG_CONFIG_HOME", root </> "config")
          : filter (\(key, _) -> key `notElem` ["NIX_CONFIG", "XDG_CONFIG_HOME"]) ambient
      runClosed args =
        bracket Proc.createPipe (\(readEnd, writeEnd) -> hClose readEnd >> hClose writeEnd) $ \(readEnd, writeEnd) -> do
          hClose readEnd
          Proc.withCreateProcess
            ( (Proc.proc "cabal" (["run", "-v0", "exe:nova-nix", "--"] ++ args))
                { Proc.env = Just isolatedEnv,
                  Proc.std_out = Proc.UseHandle writeEnd,
                  Proc.std_err = Proc.CreatePipe
                }
            )
            $ \_ _ errPipe process -> do
              err <- maybe (pure BS.empty) BS.hGetContents errPipe
              code <- Proc.waitForProcess process
              pure (code, filter (not . BS.null) (BS8.lines (BS8.filter (/= '\r') err)))
      quiet label args =
        runTestM ("CLI " <> label <> " into a closed pipe exits 0 and reports nothing") $
          assertEqual "status and stderr" (ExitSuccess, []) <$> runClosed args
  bracket_
    (forceRemoveIfExists root >> createDirectoryIfMissing True store >> mapM_ (\name -> BS.writeFile (store </> name) "doomed\n") doomed)
    (forceRemoveIfExists root)
    $ sequence
      [ quiet "--help" ["--help"],
        quiet "eval" ["eval", "--expr", "1 + 2"],
        runTestM "CLI store delete into a closed pipe deletes every path it names and exits 0" $ do
          (code, err) <- runClosed (["store", "delete", "--store", store] ++ doomed)
          left <- filterM (Dir.doesPathExist . (store </>)) doomed
          pure (assertEqual "status, stderr and what is left" (ExitSuccess, ["finding garbage collector roots..."], []) (code, err, left))
      ]

-- ---------------------------------------------------------------------------
-- Tests: realising a string's context
-- ---------------------------------------------------------------------------

-- | Import from derivation through the real CLI: a build imports and reads
-- an output it builds and one it substitutes, a value forced before the
-- build reads the same after it, the outputs and what they reference are
-- readable under restrict-eval and pure-eval, @eval --aterm@ builds as
-- @build@ does, @eval@ refuses a derivation it computed in upstream's
-- words, builds an output that may not be substituted and refuses one
-- that may, and reads what a fetcher fetched.
testImportFromDerivationCLI :: IO [Bool]
testImportFromDerivationCLI = do
  putStrLn "cli/import-from-derivation"
  shell <- findTestShell
  tmpBase <- getTemporaryDirectory
  ambient <- getEnvironment
  let root = tmpBase </> "nova-nix-test-cli-ifd"
      isolatedEnvWith extraConfig =
        ("NIX_CONFIG", "substituters =\ntrusted-public-keys =\n" <> extraConfig)
          : ("XDG_CONFIG_HOME", root </> "config")
          : ("XDG_CACHE_HOME", root </> "cache")
          : filter (\(key, _) -> key `notElem` ["NIX_CONFIG", "XDG_CONFIG_HOME", "XDG_CACHE_HOME"]) ambient
      runCLIWith extraConfig args =
        Proc.readCreateProcessWithExitCode
          ((Proc.proc "cabal" (["run", "-v0", "exe:nova-nix", "--"] ++ args)) {Proc.env = Just (isolatedEnvWith extraConfig)})
          ""
      runCLI = runCLIWith ""
      nixString text = "\"" <> T.replace "${" "\\${" (T.replace "\"" "\\\"" (T.replace "\\" "\\\\" text)) <> "\""
      system = nixString (platformToText currentPlatform)
      derivationOf name script extra =
        "derivation { name = " <> nixString name <> "; system = " <> system <> "; builder = " <> nixString shell <> "; args = [ \"-c\" " <> nixString script <> " ]; " <> hostToolsAttrs <> extra <> " }"
      -- A build of its own makes the file the expression imports and the
      -- one it reads; what the top derivation writes is what it read.
      ifd = derivationOf "ifd-made" "mkdir $out && printf '{ v = \"built\"; }' > $out/default.nix && printf data > $out/data" ""
      topWriting value = derivationOf "ifd-top" "printf %s \"$value\" > $out" ("value = " <> value <> ";")
      -- A value forced before the build and read after it.
      built = T.unpack ("let made = " <> ifd <> "; before = { kept = \"kept\"; }; read = builtins.seq before.kept ((import made).v + builtins.readFile \"${made}/data\"); in " <> topWriting "before.kept + read")
      substitutedBytes = "{ v = \"substituted\"; }" :: BS.ByteString
      fixedAttrs = "outputHashMode = \"flat\"; outputHashAlgo = \"sha256\"; outputHash = \"" <> Hash.bytesToHexText (sha256Digest substitutedBytes) <> "\";"
      substituted = T.unpack ("let fetched = " <> derivationOf "ifd-fetched.nix" "exit 1" fixedAttrs <> "; in " <> topWriting "(import fetched).v")
      readOnly = "import (derivation { name = \"ifd-read-only\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; })"
      -- The file read is in a dependency of the output imported, which the
      -- output's references name: readable only through its closure.  The
      -- output names it by its logical path, which a build in a store kept
      -- elsewhere would rewrite to where it is kept, so the path is put
      -- together from the dependency's base name, which keeps its context.
      dependency = derivationOf "ifd-dependency" "mkdir $out && printf depvalue > $out/value" ""
      reading = derivationOf "ifd-reading" "mkdir $out && printf 'builtins.readFile \"/nix/%s/%s/value\"' store \"$dependencyName\" > $out/default.nix" ("dependencyName = builtins.baseNameOf \"${" <> dependency <> "}\";")
      throughDependency = T.unpack (topWriting ("import (" <> reading <> ")"))
      -- A build of something else adds both derivations, as every
      -- derivation evaluation recorded goes in, and builds neither.
      unsubstitutable = derivationOf "ifd-unsubstitutable" "mkdir $out && printf built-read-only > $out/data" "allowSubstitutes = false;"
      substitutable = derivationOf "ifd-substitutable" "mkdir $out && printf never > $out/data" ""
      holder = T.unpack ("builtins.seq (" <> unsubstitutable <> ").drvPath (builtins.seq (" <> substitutable <> ").drvPath (" <> derivationOf "ifd-holder" "printf held > $out" "" <> "))")
      readData derivation = T.unpack ("builtins.readFile \"${" <> derivation <> "}/data\"")
      stage = root </> "stage"
      tarball = root </> "fetched.tar.gz"
      repo = root </> "repo"
      fileUrl path = "file://" <> canonPathValue (T.pack path)
      tarProcess =
        (Proc.proc "tar" ["-czf", tarball, "-C", stage, "top"])
          { Proc.env = Just (("COPYFILE_DISABLE", "1") : ambient)
          }
      evalAt store source = runCLI ["eval", "--expr", source, "--store", root </> store]
      lastLineOf err = listToMaybe (reverse (filter (not . T.null) (map T.strip (T.lines (T.pack err)))))
      refusesImport line =
        "error: cannot build '/nix/store/" `T.isPrefixOf` line
          && "-ifd-made.drv^out' during evaluation because the option 'allow-import-from-derivation' is disabled" `T.isSuffixOf` line
      -- A store upstream Nix's database manages, holding the derivation
      -- of substitutable and not its output.
      upstreamRoot = root </> "upstream"
      upstreamStore = upstreamRoot </> "nix" </> "store"
      upstreamDb = upstreamRoot </> "nix" </> "var" </> "nix" </> "db" </> "db.sqlite"
      substitutableDrvPath = T.unpack ("(" <> substitutable <> ").drvPath")
      buildWith extraConfig store source flags = runCLIWith extraConfig (["build", "--expr", source, "--store", root </> store, "--no-out-link"] ++ flags)
      build = buildWith ""
      printed out = listToMaybe (reverse (filter (not . null) (lines (filter (/= '\r') out))))
      contentsOf out = maybe (pure Nothing) (fmap Just . BS.readFile) (printed out)
      outcome (code, out, err) = "exit " <> T.pack (show code) <> "; stdout=" <> T.pack out <> "; stderr=" <> T.pack err
      narInfoTarget out = "/" <> TE.encodeUtf8 (spHash out) <> ".narinfo"
      substitutedNar = NAR.serialise (NAR.NarRegular False substitutedBytes)
  fetchedOut <- either (fail . show) pure (makeFixedOutputPath "ifd-fetched.nix" "sha256" FlatIngestion (sha256Digest substitutedBytes))
  (trustedKey, fetchedNarInfo) <- either fail pure (signedTestNarInfo fetchedOut [] "nar/fetched.nar" substitutedNar)
  let route target _
        | target == narInfoTarget fetchedOut = Send (httpResponse "200 OK" (TE.encodeUtf8 fetchedNarInfo))
        | target == "/nar/fetched.nar" = Send (httpResponse "200 OK" substitutedNar)
        | otherwise = Send (httpResponse "404 Not Found" "missing")
  bracket_
    (forceRemoveIfExists root >> createDirectoryIfMissing True root)
    (forceRemoveIfExists root)
    ( do
        madeRun@(_, madeOut, _) <- build "built" built []
        made <- contentsOf madeOut
        -- The same store, where the outputs are valid now.
        flagOffRun@(flagOffCode, _, flagOffErr) <- build "built" built ["--no-allow-import-from-derivation"]
        configOffRun@(configOffCode, _, configOffErr) <- buildWith "allow-import-from-derivation = false\n" "import-off" built []
        tracedRun@(_, tracedOut, tracedErr) <- buildWith "trace-import-from-derivation = true\n" "traced" built []
        traced <- contentsOf tracedOut
        restrictedRun@(_, restrictedOut, _) <- build "restricted" throughDependency ["--restrict-eval"]
        restricted <- contentsOf restrictedOut
        pureRun@(_, pureOut, _) <- build "pure" throughDependency ["--pure-eval"]
        pureRead <- contentsOf pureOut
        atermRun@(atermCode, atermOut, _) <- runCLI ["eval", "--aterm", "--expr", built, "--store", root </> "aterm"]
        holderRun@(holderCode, _, _) <- build "read-only-build" holder []
        unsubstitutableRun@(_, unsubstitutableOut, _) <- evalAt "read-only-build" (readData unsubstitutable)
        substitutableRun@(substitutableCode, _, substitutableErr) <- evalAt "read-only-build" (readData substitutable)
        (_, drvPathOut, _) <- evalAt "upstream-source" substitutableDrvPath
        (_, atermOfSubstitutable, _) <- runCLI ["eval", "--aterm", "--expr", T.unpack substitutable, "--store", root </> "upstream-source"]
        createDirectoryIfMissing True upstreamStore
        createDirectoryIfMissing True (takeDirectory upstreamDb)
        BS.writeFile upstreamDb ""
        forM_ (printed drvPathOut) $ \quoted ->
          BS.writeFile (upstreamStore </> takeFileName (filter (/= '"') quoted)) (BS8.pack (filter (`notElem` ("\r\n" :: String)) atermOfSubstitutable))
        upstreamRun@(upstreamCode, _, upstreamErr) <- runCLI ["eval", "--expr", readData substitutable, "--store", upstreamStore]
        -- What eval writes, read-only by default and under --read-write-mode.
        BS.writeFile (root </> "eval-source.txt") "a source\n"
        let pathExpr = T.unpack ("builtins.path { path = " <> nixString (T.pack (root </> "eval-source.txt")) <> "; }")
            toFileExpr = "builtins.toFile \"eval-written\" \"written\""
        computedRun@(computedCode, computedOut, _) <- evalAt "eval-read-only" toFileExpr
        computedStoreMade <- Dir.doesPathExist (root </> "eval-read-only")
        upstreamPathRun@(upstreamPathCode, upstreamPathOut, _) <- runCLI ["eval", "--expr", pathExpr, "--store", upstreamStore]
        upstreamPathWritten <- maybe (pure False) (\line -> Dir.doesPathExist (upstreamStore </> takeFileName (filter (/= '"') line))) (printed upstreamPathOut)
        -- An evaluation that made nothing adds nothing there, and one that
        -- must write is refused.
        nothingMadeRun@(nothingMadeCode, nothingMadeOut, _) <- runCLI ["eval", "--read-write-mode", "--expr", "1", "--store", upstreamStore]
        neededWriteRun@(neededWriteCode, _, _) <- runCLI ["eval", "--read-write-mode", "--expr", toFileExpr, "--store", upstreamStore]
        -- A pinned fetch, then the same fetch with its source gone.
        createDirectoryIfMissing True (root </> "fetch-source")
        BS.writeFile (root </> "fetch-source" </> "eval-pinned") "pinned\n"
        let pinnedExpr = T.unpack ("builtins.fetchurl { url = " <> nixString (fileUrl (root </> "fetch-source" </> "eval-pinned")) <> "; sha256 = \"" <> Hash.bytesToHexText (sha256Digest "pinned\n") <> "\"; }")
        firstFetchRun@(_, firstFetchOut, _) <- evalAt "eval-fetching" pinnedExpr
        Dir.removeFile (root </> "fetch-source" </> "eval-pinned")
        secondFetchRun@(secondFetchCode, secondFetchOut, _) <- evalAt "eval-fetching" pinnedExpr
        writtenRun@(writtenCode, writtenOut, _) <- runCLI ["eval", "--read-write-mode", "--expr", toFileExpr, "--store", root </> "eval-read-write"]
        readBackRun@(_, readBackOut, _) <- evalAt "eval-read-write" ("builtins.readFile (" <> toFileExpr <> ")")
        evalBuiltRun@(_, evalBuiltOut, _) <-
          runCLI ["eval", "--read-write-mode", "--expr", T.unpack ("let made = " <> ifd <> "; in (import made).v + builtins.readFile \"${made}/data\""), "--store", root </> "eval-built"]
        evalSubstitutedRun@(_, evalSubstitutedOut, _) <-
          withHttpServer route $ \base _ _ ->
            runCLI ["eval", "--read-write-mode", "--substituter", T.unpack base, "--trusted-key", T.unpack trustedKey, "--expr", T.unpack ("(import (" <> derivationOf "ifd-fetched.nix" "exit 1" fixedAttrs <> ")).v"), "--store", root </> "eval-substituted"]
        createDirectoryIfMissing True (stage </> "top")
        BS.writeFile (stage </> "top" </> "default.nix") "{ v = \"from a tarball\"; }\n"
        tarballRun <-
          if SI.os == "mingw32"
            then pure Nothing
            else do
              (tarCode, _, tarErr) <- Proc.readCreateProcessWithExitCode tarProcess ""
              case tarCode of
                ExitFailure _ -> pure (Just (tarCode, "", tarErr))
                ExitSuccess -> Just <$> evalAt "fetching" (T.unpack ("(import \"${builtins.fetchTarball " <> nixString (fileUrl tarball) <> "}/default.nix\").v"))
        _ <- makeGitFixture repo [("VERSION", "1.2.3\n")]
        gitRun@(_, gitOut, _) <- evalAt "fetching" (T.unpack ("builtins.readFile \"${builtins.fetchGit { url = " <> nixString (T.pack repo) <> "; }}/VERSION\""))
        substitutedRun@(_, substitutedOut, _) <-
          withHttpServer route $ \base _ _ ->
            build "substituted" substituted ["--substituter", T.unpack base, "--trusted-key", T.unpack trustedKey]
        fetched <- contentsOf substitutedOut
        refusedRun@(refusedCode, _, refusedErr) <- runCLI ["eval", "--expr", readOnly, "--store", root </> "read-only"]
        readOnlyStoreMade <- Dir.doesPathExist (root </> "read-only")
        let lastLine = lastLineOf refusedErr
        sequence
          [ runTest "build imports and reads an output it builds, and a value forced before the build reads the same after it" $
              if made == Just "keptbuiltdata" then Pass else Fail (outcome madeRun),
            -- realiseContext refuses before it builds, once the paths are
            -- valid (primops.cc), naming the first Built element as
            -- upstream renders it (DerivedPath::Built::to_string in
            -- derived-path.cc).
            runTest "--no-allow-import-from-derivation refuses an import from a derivation whose output is valid, in upstream's words" $
              if flagOffCode == ExitFailure 1 && maybe False refusesImport (lastLineOf flagOffErr) then Pass else Fail (outcome flagOffRun),
            runTest "allow-import-from-derivation = false in NIX_CONFIG refuses an import from a derivation before anything is built" $
              if configOffCode == ExitFailure 1 && maybe False refusesImport (lastLineOf configOffErr) && not ("[build]" `isInfixOf` configOffErr)
                then Pass
                else Fail (outcome configOffRun),
            -- The warning comes before the build (realiseContext in
            -- primops.cc at 2.34.9), so it precedes the build's own lines.
            runTest "trace-import-from-derivation in NIX_CONFIG warns of the import, naming its output, before the build" $
              case break ("[build]  ifd-made" `isInfixOf`) (lines tracedErr) of
                (before, _ : _)
                  | traced == Just "keptbuiltdata",
                    any (\line -> "warning: built '/nix/store/" `isPrefixOf` line && "-ifd-made.drv^out' during evaluation due to an import from derivation" `isSuffixOf` line) before ->
                      Pass
                _ -> Fail (outcome tracedRun),
            runTest "build imports an output a cache holds, and runs no builder for it" $
              if fetched == Just "substituted" then Pass else Fail (outcome substitutedRun),
            runTest "an output built during evaluation, and a file in what it references, are readable under restrict-eval" $
              if restricted == Just "depvalue" then Pass else Fail (outcome restrictedRun),
            runTest "an output built during evaluation, and a file in what it references, are readable under pure-eval" $
              if pureRead == Just "depvalue" then Pass else Fail (outcome pureRun),
            runTest "eval --aterm adds and builds what an import from derivation names, as plain nix-instantiate does" $
              if atermCode == ExitSuccess && "keptbuiltdata" `T.isInfixOf` T.pack atermOut then Pass else Fail (outcome atermRun),
            -- Upstream's read-only evaluation refuses a substitution and
            -- builds an output that may not be substituted
            -- (PathSubstitutionGoal::init in substitution-goal.cc at
            -- 2.24.9, PathSubstitutionGoal::substitute at 2.36.0, and
            -- DerivationGoal::haveDerivation in derivation-goal.cc).
            runTest "eval builds an output of a derivation the store holds that may not be substituted, and reads it" $
              if holderCode == ExitSuccess && printed unsubstitutableOut == Just "\"built-read-only\"" then Pass else Fail (outcome holderRun <> "; " <> outcome unsubstitutableRun),
            runTest "eval refuses to substitute an output of a derivation the store holds, in upstream's words" $
              case lastLineOf substitutableErr of
                Just line
                  | substitutableCode == ExitFailure 1,
                    "error: cannot substitute path '/nix/store/" `T.isPrefixOf` line,
                    "-ifd-substitutable' - no write access to the Nix store" `T.isSuffixOf` line ->
                      Pass
                _ -> Fail (outcome substitutableRun),
            -- Upstream's substitution goal refuses before anything is built
            -- or written (PathSubstitutionGoal::init in substitution-goal.cc
            -- at 2.24.9, PathSubstitutionGoal::substitute at 2.36.0), so the
            -- store's own refusal is never reached.
            runTest "eval refuses to substitute an output in a store upstream Nix manages in upstream's words, before refusing the store" $
              case lastLineOf upstreamErr of
                Just line
                  | upstreamCode == ExitFailure 1,
                    "error: cannot substitute path '/nix/store/" `T.isPrefixOf` line,
                    "-ifd-substitutable' - no write access to the Nix store" `T.isSuffixOf` line ->
                      Pass
                _ -> Fail (outcome upstreamRun),
            -- nix-instantiate --eval computes these paths without writing
            -- them (prim_toFile and addPath in primops.cc).
            runTest "eval prints a toFile result's path and makes no store" $
              if computedCode == ExitSuccess && printed computedOut == printed writtenOut && not computedStoreMade then Pass else Fail (outcome computedRun <> "; " <> outcome writtenRun),
            runTest "eval prints a builtins.path copy's path in a store upstream Nix manages, writing nothing there" $
              case printed upstreamPathOut of
                Just line
                  | upstreamPathCode == ExitSuccess,
                    "\"/nix/store/" `isPrefixOf` line,
                    "-eval-source.txt\"" `isSuffixOf` line,
                    not upstreamPathWritten ->
                      Pass
                _ -> Fail (outcome upstreamPathRun),
            runTest "eval --read-write-mode that makes nothing adds nothing, so a store upstream Nix manages refuses nothing" $
              if nothingMadeCode == ExitSuccess && printed nothingMadeOut == Just "1" then Pass else Fail (outcome nothingMadeRun),
            runTest "eval --read-write-mode that must write in a store upstream Nix manages is refused" $
              if neededWriteCode == ExitFailure 1 then Pass else Fail (outcome neededWriteRun),
            -- Upstream's local store adds a fetch as it lands, in any mode
            -- (downloadFile in tarball.cc), so its next evaluation takes the
            -- pinned fetch's early exit (fetch in fetchTree.cc).
            runTest "eval registers what a pinned fetch writes, so a second eval of it needs no source" $
              if secondFetchCode == ExitSuccess && isJust (printed firstFetchOut) && printed secondFetchOut == printed firstFetchOut
                then Pass
                else Fail (outcome firstFetchRun <> "; " <> outcome secondFetchRun),
            -- Upstream's read-write evaluation adds what it makes as it goes
            -- (prim_toFile in primops.cc), so a later evaluation finds it
            -- valid.
            runTest "eval --read-write-mode writes and registers a toFile result, which a read-only eval then reads" $
              if writtenCode == ExitSuccess && printed readBackOut == Just "\"written\"" then Pass else Fail (outcome writtenRun <> "; " <> outcome readBackRun),
            runTest "eval --read-write-mode imports and reads an output it builds" $
              if printed evalBuiltOut == Just "\"builtdata\"" then Pass else Fail (outcome evalBuiltRun),
            runTest "eval --read-write-mode imports an output a cache holds, through --substituter and --trusted-key" $
              if printed evalSubstitutedOut == Just "\"substituted\"" then Pass else Fail (outcome evalSubstitutedRun),
            runTest "eval imports a fetchTarball result through its context" $
              case tarballRun of
                Nothing -> Pass
                Just run@(_, tarballOut, _) -> if printed tarballOut == Just "\"from a tarball\"" then Pass else Fail (outcome run),
            runTest "eval reads a fetchGit result through its context" $
              if printed gitOut == Just "\"1.2.3\\n\"" then Pass else Fail (outcome gitRun),
            runTest "eval refuses an import from a derivation in upstream's words, under its line of context, and makes no store" $
              case lastLine of
                Just line
                  | refusedCode == ExitFailure 1,
                    "error: path '/nix/store/" `T.isPrefixOf` line,
                    "-ifd-read-only.drv' is not valid" `T.isSuffixOf` line,
                    "while realising the context of path '/nix/store/" `T.isInfixOf` T.pack refusedErr,
                    not readOnlyStoreMade ->
                      Pass
                _ -> Fail (outcome refusedRun)
          ]
    )

-- | @build --check@ and @keep-failed@ through the real CLI, which the
-- tests of the build itself cannot show: that the flags and the setting
-- reach the build, that a check which passes prints the path a build
-- prints, that one which finds a difference exits with upstream's status
-- for it, and what a build that fails keeps and names on stderr.
testCheckCLI :: IO [Bool]
testCheckCLI = do
  putStrLn "cli/check"
  shell <- findTestShell
  tmpBase <- getTemporaryDirectory
  ambient <- getEnvironment
  let root = tmpBase </> "nova-nix-test-cli-check"
      isolatedEnv configText =
        ("NIX_CONFIG", "substituters =\ntrusted-public-keys =\n" <> configText)
          : ("XDG_CONFIG_HOME", root </> "config")
          : filter (\(key, _) -> key `notElem` ["NIX_CONFIG", "XDG_CONFIG_HOME"]) ambient
      runCLIWith configText args =
        Proc.readCreateProcessWithExitCode
          ((Proc.proc "cabal" (["run", "-v0", "exe:nova-nix", "--"] ++ args)) {Proc.env = Just (isolatedEnv configText)})
          ""
      nixString text = "\"" <> T.replace "${" "\\${" (T.replace "\"" "\\\"" (T.replace "\\" "\\\\" text)) <> "\""
      expression name script =
        T.unpack
          ( "derivation { name = "
              <> nixString name
              <> "; system = builtins.currentSystem; builder = "
              <> nixString shell
              <> "; args = [ \"-c\" "
              <> nixString script
              <> " ]; }"
          )
      -- Its output holds its own path, which a rebuild beside the valid
      -- output writes as the scratch path and the check rewrites.
      same = expression "check-same" "echo $out > $out"
      -- Its output holds the hash of its own path cut in two, where no
      -- rewrite of the whole hash reaches it, so a rebuild at a scratch
      -- path comes out different.
      differing =
        expression
          "check-differs"
          "stem=${out%-check-differs}; front=${stem%????????????????}; printf '%s\\n%s\\n' \"$front\" \"${stem#\"$front\"}\" > \"$out\""
      -- It leaves a file where it works, then fails.
      failing = expression "keep-failed" "echo left > left; exit 1"
      buildWith configText source store flags = runCLIWith configText (["build", "--expr", source, "--store", root </> store, "--no-out-link"] ++ flags)
      build = buildWith ""
      printed out = listToMaybe (reverse (filter (not . null) (lines (filter (/= '\r') out))))
      outcome (code, out, err) = "exit " <> T.pack (show code) <> "; stdout=" <> T.pack out <> "; stderr=" <> T.pack err
      stderrLines err = lines (filter (/= '\r') err)
      -- The directories upstream's note names, in the order it names them.
      keptDirs err = [dir | line <- stderrLines err, Just quoted <- [stripPrefix "note: keeping build directory \"" line], Just dir <- [stripSuffix "\"" quoted]]
      stripSuffix suffix text = reverse <$> stripPrefix (reverse suffix) (reverse text)
      -- What the builder left in the directory a note names, if it is
      -- there.
      leftIn dir = do
        present <- Dir.doesFileExist (dir </> "left")
        if present then Just <$> BS.readFile (dir </> "left") else pure Nothing
      buildsUnder store = do
        let builds = root </> store </> ".nova-nix" </> "builds"
        present <- doesDirectoryExist builds
        if present then Dir.listDirectory builds else pure []
  bracket_
    (forceRemoveIfExists root >> createDirectoryIfMissing True root)
    (forceRemoveIfExists root)
    ( do
        sameBuilt@(_, sameOut, _) <- build same "same" []
        sameChecked@(sameCode, sameCheckedOut, sameErr) <- build same "same" ["--check"]
        unbuilt@(unbuiltCode, _, unbuiltErr) <- build same "unbuilt" ["--check"]
        differingBuilt@(_, differingOut, _) <- build differing "differing" []
        differingChecked@(differingCode, _, differingErr) <- build differing "differing" ["--check"]
        keptChecked@(keptCode, _, keptErr) <- build differing "differing" ["--check", "--keep-failed"]
        keptRebuild <- traverse (\path -> Dir.doesPathExist (path <> ".check")) (printed differingOut)
        flagKept@(flagKeptCode, _, flagKeptErr) <- build failing "failing-flag" ["-K"]
        flagLeft <- traverse leftIn (keptDirs flagKeptErr)
        configKept@(configKeptCode, _, configKeptErr) <- buildWith "keep-failed = true\n" failing "failing-config" []
        configLeft <- traverse leftIn (keptDirs configKeptErr)
        removed@(removedCode, _, removedErr) <- build failing "failing-removed" []
        removedLeft <- buildsUnder "failing-removed"
        sequence
          [ runTest "a check that passes exits 0 and prints the path the build printed" $
              case (printed sameOut, sameCode) of
                (Just path, ExitSuccess)
                  | printed sameCheckedOut == Just path && "[check]" `T.isInfixOf` T.pack sameErr -> Pass
                _ -> Fail ("build: " <> outcome sameBuilt <> "; check: " <> outcome sameChecked),
            runTest "a check with no valid output exits 1 with upstream's message" $
              if unbuiltCode == ExitFailure 1 && "are not valid, so checking is not possible" `T.isInfixOf` T.pack unbuiltErr
                then Pass
                else Fail (outcome unbuilt),
            runTest "a check that finds a difference exits 104 with upstream's message" $
              case printed differingOut of
                Just path
                  | differingCode == ExitFailure 104
                      && ("may not be deterministic: output \"" <> T.pack path <> "\" differs\n") `T.isInfixOf` T.pack differingErr
                      && null (keptDirs differingErr) ->
                      Pass
                _ -> Fail ("build: " <> outcome differingBuilt <> "; check: " <> outcome differingChecked),
            -- The check's failure keeps its build directory too, as any
            -- failure does under keep-failed.
            runTest "a check under --keep-failed that finds a difference keeps the rebuild as PATH.check and names it" $
              case (printed differingOut, keptDirs keptErr) of
                (Just path, [_])
                  | keptCode == ExitFailure 104
                      && ("may not be deterministic: output \"" <> T.pack path <> "\" differs from \"" <> T.pack path <> ".check\"") `T.isInfixOf` T.pack keptErr
                      && keptRebuild == Just True ->
                      Pass
                _ -> Fail ("check: " <> outcome keptChecked <> "; kept: " <> T.pack (show keptRebuild)),
            -- Upstream prints its note when it cleans up after the
            -- builder, before the failure is reported.
            runTest "a build that fails under -K keeps the directory its builder worked in and names it before the failure" $
              case (flagLeft, findIndex ("note: keeping build directory " `isPrefixOf`) (stderrLines flagKeptErr), findIndex ("build failed" `isPrefixOf`) (stderrLines flagKeptErr)) of
                ([Just "left\n"], Just note, Just failure)
                  | flagKeptCode == ExitFailure 1 && note < failure -> Pass
                _ -> Fail (outcome flagKept <> "; left: " <> T.pack (show flagLeft)),
            runTest "a build that fails with keep-failed in nix.conf keeps its directory" $
              if configKeptCode == ExitFailure 1 && configLeft == [Just "left\n"]
                then Pass
                else Fail (outcome configKept <> "; left: " <> T.pack (show configLeft)),
            runTest "a build that fails without keep-failed keeps no directory and names none" $
              if removedCode == ExitFailure 1 && null (keptDirs removedErr) && null removedLeft
                then Pass
                else Fail (outcome removed <> "; left under builds: " <> T.pack (show removedLeft))
          ]
    )

-- | @develop@ and @print-dev-env@ through the built CLI, over
-- test/data/develop/shell.nix: what a command, a phase and the
-- environment flags see, the refusals before and after the capture and
-- their order, the descriptors the command starts with after a
-- substitution over HTTP, structured attributes' files, the JSON
-- layouts, the temporary files left behind, and that a second run builds
-- nothing.  The binary runs itself, with every descriptor of the suite's
-- closed, and reads get-env.sh from the repository's data directory.
-- The fixture's builder is a bash whose @declare -p@, given no name,
-- prints declare commands, which get-env.sh reads: macOS's /bin/bash 3.2
-- prints @NAME=value@ lines there, from which it captures no variable.
-- The same bash leads @PATH@, so that it is the shell too, which needs
-- @declare -A@ under structured attributes.  Each command removes the
-- fresh @NIX_BUILD_TOP@ the rcfile makes, which macOS's @mktemp -t@ puts
-- in the user's temporary directory whatever @TMPDIR@ says.  On Windows
-- both commands are refused.
testDevelopCLI :: IO [Bool]
testDevelopCLI = do
  putStrLn "cli/develop"
  ambient <- getEnvironment
  tmpBase <- getTemporaryDirectory
  let root = tmpBase </> "nova-nix-test-cli-develop"
      store = root </> "store"
      marker = root </> "marker"
      shellTemp = root </> "tmp"
      isolatedNames = ["NIX_CONFIG", "NIX_CONF_DIR", "NIX_USER_CONF_FILES", "XDG_CONFIG_HOME", "XDG_CONFIG_DIRS", "XDG_STATE_HOME", "XDG_DATA_HOME", "NIX_STATE_HOME", "HOME", "ZDOTDIR", "BASH_ENV", "ENV", "TMPDIR", "NIX_DATA_DIR", "nova_nix_datadir", "DEVELOP_KEEP", "DEVELOP_DROP", "DEVELOP_NEW"]
      isolatedEnv dataDir =
        [ ("NIX_CONFIG", "substituters =\ntrusted-public-keys =\n"),
          ("NIX_CONF_DIR", root </> "system"),
          ("NIX_USER_CONF_FILES", ""),
          ("XDG_CONFIG_HOME", root </> "home" </> "config"),
          ("XDG_STATE_HOME", root </> "home" </> "state"),
          ("XDG_DATA_HOME", root </> "home" </> "data"),
          ("NIX_STATE_HOME", root </> "home" </> "nix"),
          ("HOME", root </> "home"),
          ("ZDOTDIR", root </> "home"),
          ("TMPDIR", shellTemp),
          ("NIX_DATA_DIR", dataDir),
          ("DEVELOP_KEEP", "kept"),
          ("DEVELOP_DROP", "dropped")
        ]
          <> filter ((`notElem` isolatedNames) . fst) ambient
  if SI.os == "mingw32"
    then do
      let refused args = runTestM ("nova-nix " <> T.pack (unwords args) <> " is refused on Windows") $ do
            (code, _, err) <- Proc.readCreateProcessWithExitCode ((Proc.proc "cabal" (["run", "-v0", "exe:nova-nix", "--"] ++ args)) {Proc.env = Just (isolatedEnv "data")}) ""
            pure $
              if code == ExitFailure 1 && listToMaybe (reverse (lines (filter (/= '\r') err))) == Just "error: develop and print-dev-env do not run on Windows yet"
                then Pass
                else Fail ("exit " <> T.pack (show code) <> ": " <> T.pack err)
      bracket_
        (forceRemoveIfExists root >> createDirectoryIfMissing True root)
        (forceRemoveIfExists root)
        (sequence [refused ["develop", "--expr", "1"], refused ["print-dev-env", "--expr", "1"]])
    else do
      builder <- declaringBash
      case builder of
        Nothing -> [True] <$ putStrLn "  SKIP  no bash whose declare -p prints declare commands"
        Just bash -> do
          (built, _, buildErr) <- Proc.readCreateProcessWithExitCode (Proc.proc "cabal" ["build", "-v0", "exe:nova-nix"]) ""
          (_, listed, _) <- Proc.readCreateProcessWithExitCode (Proc.proc "cabal" ["list-bin", "-v0", "exe:nova-nix"]) ""
          dataDir <- Dir.makeAbsolute "data"
          fixture <- Dir.makeAbsolute ("test" </> "data" </> "develop" </> "shell.nix")
          case (built, lines listed) of
            (ExitSuccess, [binary]) ->
              bracket_
                (forceRemoveIfExists root >> mapM_ (createDirectoryIfMissing True) [root </> "home", shellTemp])
                (forceRemoveIfExists root)
                (runDevelopCLI bash binary fixture (isolatedEnv dataDir) root store marker shellTemp)
            _ -> sequence [runTest "the CLI builds" (Fail (T.pack buildErr))]
  where
    -- A bash get-env.sh can run: the first whose declare -p, given no
    -- name as get-env.sh gives it none, prints a declare command for a
    -- variable.
    declaringBash = do
      onPath <- Dir.findExecutable "bash"
      firstM declares (["/bin/bash", "/usr/bin/bash", "/opt/homebrew/bin/bash", "/usr/local/bin/bash"] <> maybeToList onPath)
    declares candidate = do
      present <- Dir.doesFileExist candidate
      if not present
        then pure False
        else do
          (code, out, _) <- Proc.readCreateProcessWithExitCode (Proc.proc candidate ["-c", "x=1; declare -p"]) ""
          pure (code == ExitSuccess && any ("declare -- x=" `isPrefixOf`) (lines out))
    firstM _ [] = pure Nothing
    firstM check (candidate : rest) = do
      found <- check candidate
      if found then pure (Just candidate) else firstM check rest

-- | The runs of 'testDevelopCLI' on a POSIX host, given the bash the
-- fixture's builder is, the binary, the fixture, the environment, and
-- the root, store, marker file and temporary directory under it.
runDevelopCLI :: FilePath -> FilePath -> FilePath -> [(String, String)] -> FilePath -> FilePath -> FilePath -> FilePath -> IO [Bool]
runDevelopCLI bash binary fixture ambientEnv root store marker shellTemp = do
  let env = ("PATH", takeDirectory bash <> maybe "" (searchPathSeparator :) (lookup "PATH" ambientEnv)) : filter ((/= "PATH") . fst) ambientEnv
      runIn environment args = Proc.readCreateProcessWithExitCode (Proc.proc binary args) {Proc.env = Just environment, Proc.close_fds = True} ""
      run = runIn env
      target command builder markerFile = [command, fixture, "--argstr", "marker", markerFile, "--argstr", "builder", builder, "--no-sandbox"]
      developIn environment storeDir flags = runIn environment (target "develop" bash marker <> ["--store", storeDir] <> flags)
      develop = developIn env
      -- A command that first removes the fresh NIX_BUILD_TOP, which only
      -- an empty directory lets it do.
      inShell script = ["--command", "/bin/sh", "-c", "rmdir \"$NIX_BUILD_TOP\" && " <> script]
      probe = "printf '%s/%s/%s' \"${DEVELOP_KEEP-unset}\" \"${DEVELOP_DROP-unset}\" \"${DEVELOP_NEW-unset}\""
      outcome (code, out, err) = "exit " <> T.pack (show code) <> "; stdout=" <> T.pack out <> "; stderr=" <> T.pack err
      -- Refused with status 1 and the message as stderr's last line, after
      -- what the capture's build logged.
      refusedWith message result@(code, _, err)
        | code == ExitFailure 1 && listToMaybe (reverse (lines err)) == Just message = Pass
        | otherwise = Fail (outcome result)
      structuredTemp = root </> "structured-tmp"
      structuredEnv = ("TMPDIR", structuredTemp) : filter ((/= "TMPDIR") . fst) env
      -- A variant's capture is another derivation, with a marker of its
      -- own, so that the main one counts the main capture's runs alone.
      structured command flags = runIn structuredEnv (target command bash (root </> "structured-marker") <> ["--store", store, "--arg", "structured", "true"] <> flags)
      substituting storeDir flags = run (target "develop" bash (root </> "substitution-marker") <> ["--store", storeDir] <> flags)
  createDirectoryIfMissing True structuredTemp
  seen@(_, seenOut, _) <- develop store (inShell "develop-tool && printf '%s|%s' \"$greeting\" \"$NIX_GCROOT\"")
  exited <- develop store (inShell "exit 7")
  phase <- develop store ["--phase", "build"]
  phaseFlag <- develop store ["--build"]
  ignored <- develop store (["-i", "-k", "DEVELOP_KEEP", "-k", "PATH"] <> inShell probe)
  changed <- develop store (["-u", "DEVELOP_DROP", "-s", "DEVELOP_NEW", "new"] <> inShell probe)
  emptyName <- developIn (("", "x") : env) store (inShell "true")
  unsetIgnored <- develop store ["-i", "-u", "X", "--command", "true"]
  keptWithout <- develop store ["-k", "X", "--command", "true"]
  both <- develop store ["--phase", "build", "--command", "true"]
  scriptFirst <- run (target "develop" bash (root </> "array-marker") <> ["--store", store, "--arg", "arrayOutput", "true", "-i", "-u", "X", "--phase", "build", "--command", "true"])
  unexecutable <- develop store ["-s", "PATH", "/nonexistent", "--command", "true"]
  parseRefused <- develop store ["-u", "X", "-s", "X", "1", "--command", "true"]
  commandless <- develop store ["--command"]
  notBash <- run (target "develop" "/bin/sh" marker <> ["--store", root </> "refused-store", "--command", "true"])
  printed@(_, printedOut, _) <- run (target "print-dev-env" bash marker <> ["--store", store])
  let printedScript = root </> "printed.sh"
  BS.writeFile printedScript (TE.encodeUtf8 (T.pack printedOut))
  sourced <- Proc.readCreateProcessWithExitCode (Proc.proc bash ["-c", ". \"$1\" && rmdir \"$NIX_BUILD_TOP\" && printf '%s' \"$greeting\"", "bash", printedScript]) {Proc.env = Just env} ""
  json@(_, jsonOut, _) <- run (target "print-dev-env" bash marker <> ["--store", store, "--json"])
  pretty@(_, prettyOut, _) <- run (target "print-dev-env" bash marker <> ["--store", store, "--no-pretty", "--json", "--pretty"])
  notPretty@(_, notPrettyOut, _) <- run (target "print-dev-env" bash marker <> ["--store", store, "--pretty", "--json", "--no-pretty"])
  structuredSeen@(_, structuredOut, _) <- structured "develop" (inShell "printf '%s|%s' \"$NIX_ATTRS_SH_FILE\" \"$NIX_ATTRS_JSON_FILE\"")
  structuredRoot <- Dir.canonicalizePath structuredTemp
  structuredFiles <- case T.splitOn "|" (T.pack structuredOut) of
    [shFile, jsonFile] -> do
      sh <- BS.readFile (T.unpack shFile)
      jsonBytes <- BS.readFile (T.unpack jsonFile)
      pure (Right (T.unpack shFile, T.unpack jsonFile, sh, jsonBytes))
    _ -> pure (Left ())
  structuredPrinted@(_, structuredPrintedOut, _) <- structured "print-dev-env" []
  structuredLeft <- sort <$> Dir.listDirectory structuredTemp
  descriptors <- substitutedDescriptors run substituting inShell fixture root "http" withKeepAliveDirectoryServer
  directDescriptors <- substitutedDescriptors run substituting inShell fixture root "file" (\cache use -> use (directoryUrl cache))
  direct <- Proc.readCreateProcessWithExitCode (Proc.proc "/bin/sh" ["-c", "ls /dev/fd/"]) {Proc.env = Just env, Proc.close_fds = True} ""
  gcrootEnvironment <- case T.splitOn "|" (T.pack seenOut) of
    [_, gcroot] -> Right <$> BS.readFile (T.unpack gcroot)
    _ -> pure (Left ())
  refusedStore <- Dir.doesDirectoryExist (root </> "refused-store")
  refusedScripts <- if refusedStore then filter ("-get-env.sh" `isSuffixOf`) <$> Dir.listDirectory (root </> "refused-store") else pure []
  runs <- BS8.lines <$> BS.readFile marker
  leftBehind <- Dir.listDirectory shellTemp
  sequence
    [ runTest "--command sees the captured variables and the build input on PATH" $
        case T.splitOn "|" (T.pack seenOut) of
          [greetingSeen, gcroot]
            | greetingSeen == "tool ran\n" <> fixtureGreeting
                && T.pack store `T.isPrefixOf` gcroot
                && "-develop-fixture-env" `T.isSuffixOf` gcroot ->
                Pass
          _ -> Fail (outcome seen),
      runTest "NIX_GCROOT names the output the environment was read from" $
        assertRight "read" gcrootEnvironment $ \bytes ->
          assertRight "parsed" (parseBuildEnvironment bytes) $
            assertEqual "greeting" (Just (ExportedString fixtureGreeting)) . Map.lookup "greeting" . beVariables,
      runTest "the shell's exit status is the command's" $
        assertEqual "status" (ExitFailure 7) (fst3 exited),
      runTest "--phase and a phase flag run the phase through runPhase" $
        assertEqual "phase" (ExitSuccess, "build phase ran: tool ran\n") (fst3 phase, snd3 phase)
          `andThen` assertEqual "flag" (ExitSuccess, "build phase ran: tool ran\n") (fst3 phaseFlag, snd3 phaseFlag),
      runTest "--ignore-env keeps what --keep-env-var names" $
        assertEqual "seen" (ExitSuccess, "kept/unset/unset") (fst3 ignored, snd3 ignored),
      runTest "--unset-env-var and --set-env-var change the caller's environment" $
        assertEqual "seen" (ExitSuccess, "kept/unset/new") (fst3 changed, snd3 changed),
      runTest "an environment entry with an empty name, which unsetenv refuses, does not stop the shell" $
        assertEqual "status" ExitSuccess (fst3 emptyName),
      runTest "upstream's refusals once the environment is built are its whole lines" $
        refusedWith "error: --unset-env-var does not make sense with --ignore-env" unsetIgnored
          `andThen` refusedWith "error: --keep-env-var does not make sense without --ignore-env" keptWithout
          `andThen` refusedWith "error: you cannot use both '--command' and '--phase'" both,
      runTest "a script that cannot be made is refused before the flags, as upstream makes the script first" $
        refusedWith "error: bash variable is not a string" scriptFirst,
      runTest "a shell that cannot start is reported in upstream's words" $
        refusedWith "error: unable to execute 'bash': No such file or directory" unexecutable,
      runTest "the refusals made as the command line is read are whole lines after error:" $
        refusedWith "error: Cannot set environment variable 'X' that is unset with '--unset-env-var'" parseRefused
          `andThen` refusedWith "error: --command requires at least one argument" commandless,
      runTest "a builder that is not bash is refused in upstream's words, and get-env.sh is not written" $
        refusedWith "error: 'nix develop' only works on derivations that use 'bash' as their builder" notBash
          `andThen` assertEqual "written" [] refusedScripts,
      runTest "after a substitution over HTTP, the command starts with the descriptors a process started directly has" $
        assertRight "substituted" descriptors $ \listing ->
          assertEqual "descriptors" (snd3 direct) listing,
      runTest "after a substitution from a file:// cache, the command starts with the descriptors a process started directly has" $
        assertRight "substituted" directDescriptors $ \listing ->
          assertEqual "descriptors" (snd3 direct) listing,
      runTest "print-dev-env prints a script bash sources, followed by a newline" $
        assertEqual "sourced" (ExitSuccess, T.unpack fixtureGreeting) (fst3 sourced, snd3 sourced)
          `andThen` (if "eval \"${shellHook:-}\"\n\n" `isSuffixOf` printedOut then Pass else Fail (outcome printed)),
      runTest "print-dev-env --json prints the environment on one line where stdout is no terminal, and --pretty and --no-pretty choose, the last winning" $
        case (lines jsonOut, lines prettyOut, lines notPrettyOut) of
          ([line], _ : _ : _, [notPrettyLine]) ->
            assertRight "parsed" (parseBuildEnvironment (TE.encodeUtf8 (T.pack line))) $ \environment ->
              assertEqual "greeting" (Just (ExportedString fixtureGreeting)) (Map.lookup "greeting" (beVariables environment))
                `andThen` assertEqual "pretty" (Right environment) (parseBuildEnvironment (TE.encodeUtf8 (T.pack prettyOut)))
                `andThen` assertEqual "no-pretty" line notPrettyLine
          _ -> Fail (outcome json <> "; " <> outcome pretty <> "; " <> outcome notPretty),
      runTest "under structured attributes the shell reads the files from a directory develop leaves for it" $
        assertRight "files" structuredFiles $ \(shFile, jsonFile, sh, jsonBytes) ->
          assertEqual "status" ExitSuccess (fst3 structuredSeen)
            `andThen` assertEqual "directory" (takeDirectory shFile) (takeDirectory jsonFile)
            `andThen` assertEqual "under TMPDIR, its links resolved" structuredRoot (takeDirectory (takeDirectory shFile))
            `andThen` assertEqual "named" True ("nix-develop-" `isPrefixOf` takeFileName (takeDirectory shFile))
            `andThen` assertEqual "names" (".attrs.sh", ".attrs.json") (takeFileName shFile, takeFileName jsonFile)
            `andThen` assertEqual "written" (True, True) ("greeting" `BS.isInfixOf` sh, "greeting" `BS.isInfixOf` jsonBytes)
            `andThen` assertEqual "left" [takeFileName (takeDirectory shFile)] structuredLeft,
      runTest "print-dev-env names the structured attributes' files in a directory it removes" $
        if fst3 structuredPrinted == ExitSuccess && (T.pack structuredRoot <> "/nix-dev-env-") `T.isInfixOf` T.pack structuredPrintedOut
          then assertEqual "left" 1 (length structuredLeft)
          else Fail (outcome structuredPrinted),
      runTest "the capture is built once" $
        assertEqual "runs" ["ran"] runs,
      runTest "nothing is left in the temporary directory: the rcfile removes itself, and no directory is kept for files there are none of" $
        assertEqual "left" [] leftBehind
    ]
  where
    fixtureGreeting = "it's \"quoted\"\nand on two lines"
    fst3 (code, _, _) = code
    snd3 (_, out, _) = out

-- | What @ls /dev/fd/@ prints in a @develop@ shell whose capture first
-- substitutes the fixture's build input from a binary cache: the input is
-- built into a store of its own, pushed to a directory as a signed cache,
-- and the directory handed to the substitution by its URL, as the given
-- function serves it.  Served over HTTP by a server that keeps its
-- connections open, the substitution leaves one in the HTTP client's pool;
-- read through its @file:\/\/@ URL, it opens the cache's files.  Given the
-- CLI, @develop@ into a store with flags, the command the shell runs, the
-- fixture, the root, a label naming this run's cache and store under it,
-- and the serving.  'Left' with what went wrong.
substitutedDescriptors ::
  ([String] -> IO (ExitCode, String, String)) ->
  (FilePath -> [String] -> IO (ExitCode, String, String)) ->
  (String -> [String]) ->
  FilePath ->
  FilePath ->
  String ->
  (FilePath -> (Text -> IO (ExitCode, String, String)) -> IO (ExitCode, String, String)) ->
  IO (Either Text String)
substitutedDescriptors run develop inShell fixture root label serving = do
  seed <- BS.pack <$> QC.generate (QC.vectorOf ed25519SeedBytes QC.arbitrary)
  case throwawayKey seed of
    Left err -> pure (Left (T.pack err))
    Right (keyText, publicKey) -> do
      let keyFile = root </> ("cache-secret-key-" <> label)
          cache = root </> ("cache-" <> label)
          source = root </> "source-store"
      BS.writeFile keyFile (TE.encodeUtf8 keyText)
      built@(_, builtOut, _) <- run ["build", "--expr", "{ fixture }: builtins.head (import fixture { marker = \"\"; }).buildInputs", "--argstr", "fixture", fixture, "--store", source, "--no-out-link", "--no-sandbox"]
      case listToMaybe (reverse (lines builtOut)) of
        Nothing -> pure (Left ("build: " <> T.pack (show built)))
        Just tool -> do
          pushed <- run ["push", "--cache", "file://" <> cache, "--secret-key-file", keyFile, "--store", source, tool]
          shown <- serving cache $ \base ->
            develop
              (root </> ("substituted-store-" <> label))
              (["--substituter", T.unpack base, "--trusted-key", T.unpack (Signing.renderPublicKey publicKey)] <> inShell "ls /dev/fd/")
          pure $ case shown of
            (ExitSuccess, listing, err)
              | any (\line -> "[subst]" `isInfixOf` line && "develop-tool" `isSuffixOf` line) (lines err) -> Right listing
            _ -> Left ("push: " <> T.pack (show pushed) <> "; develop: " <> T.pack (show shown))

-- | The CLI runs with upstream's file-creation mask, 0022, whatever the one
-- it was started with, so a builder it runs does too.  POSIX only: Windows
-- has no such mask.
testBuildUmaskCLI :: IO [Bool]
testBuildUmaskCLI = do
  putStrLn "cli/build-umask"
  if SI.os == "mingw32"
    then do
      putStrLn "  SKIP  POSIX only"
      pure []
    else do
      tmpBase <- getTemporaryDirectory
      ambient <- getEnvironment
      let root = tmpBase </> "nova-nix-test-cli-build-umask"
          configVars = ["NIX_CONFIG", "NIX_CONF_DIR", "NIX_USER_CONF_FILES"]
          expression = "derivation { name = \"umask-cli\"; system = builtins.currentSystem; builder = \"/bin/sh\"; args = [ \"-c\" \"umask > $out\" ]; }"
          started =
            (Proc.proc "/bin/sh" ["-c", "umask 077; exec cabal run -v0 exe:nova-nix -- build --expr \"$1\" --store \"$2\" --no-out-link", "sh", expression, root </> "store"])
              { Proc.env = Just (("NIX_CONFIG", "substituters =\ntrusted-public-keys =\nsandbox = false\n") : ("NIX_CONF_DIR", root </> "system") : ("NIX_USER_CONF_FILES", "") : filter ((`notElem` configVars) . fst) ambient)
              }
      bracket_
        (forceRemoveIfExists root >> createDirectoryIfMissing True root)
        (forceRemoveIfExists root)
        ( do
            (code, out, err) <- Proc.readCreateProcessWithExitCode started ""
            seen <- case (code, listToMaybe (reverse (filter (not . null) (lines out)))) of
              (ExitSuccess, Just path) -> Right . T.strip <$> TIO.readFile path
              _ -> pure (Left ("exit " <> T.pack (show code) <> "; stdout=" <> T.pack out <> "; stderr=" <> T.pack err))
            sequence [runTest "a builder the CLI runs gets upstream's file-creation mask, whatever the CLI was started with" (assertEqual "umask" (Right "0022") seen)]
        )

-- | Every session the CLI makes builds under the sandbox settings, an
-- import from a derivation included: under @sandbox = true@ a derivation
-- that sets @__noChroot@ is refused with upstream's error, which is decided
-- before the system is probed and so holds on every platform, and under
-- @--relaxed-sandbox@, given after the command, it builds unsandboxed.
-- Each run has a store of its own, so none finds an output another built.
-- A read-only session writes no @.drv@, and builds only a derivation the
-- store holds and only an output that may not be substituted, so for
-- @eval@ and @trust@ a read-write evaluation of the derivation's path
-- writes it first, building nothing, and the derivation sets
-- @allowSubstitutes = false@.
testSandboxSessionsCLI :: IO [Bool]
testSandboxSessionsCLI = do
  putStrLn "cli/sandbox-sessions"
  shell <- findTestShell
  tmpBase <- getTemporaryDirectory
  ambient <- getEnvironment
  let root = tmpBase </> "nova-nix-test-cli-sandbox-sessions"
      configVars = ["NIX_CONFIG", "NIX_CONF_DIR", "NIX_USER_CONF_FILES", "XDG_CONFIG_HOME", "XDG_CACHE_HOME"]
      isolatedEnv =
        ("NIX_CONFIG", "substituters =\ntrusted-public-keys =\nsandbox = true\n")
          : ("NIX_CONF_DIR", root </> "system")
          : ("NIX_USER_CONF_FILES", "")
          : ("XDG_CONFIG_HOME", root </> "config")
          : ("XDG_CACHE_HOME", root </> "cache")
          : filter ((`notElem` configVars) . fst) ambient
      runCLI store args =
        Proc.readCreateProcessWithExitCode
          ((Proc.proc "cabal" (["run", "-v0", "exe:nova-nix", "--"] ++ args ++ ["--store", root </> store])) {Proc.env = Just isolatedEnv})
          ""
      nixString text = "\"" <> T.replace "${" "\\${" (T.replace "\"" "\\\"" (T.replace "\\" "\\\\" text)) <> "\""
      unsandboxable name script extra =
        "derivation { name = " <> nixString name <> "; system = builtins.currentSystem; builder = " <> nixString shell <> "; args = [ \"-c\" " <> nixString script <> " ]; __noChroot = true; allowSubstitutes = false; " <> extra <> " }"
      made = unsandboxable "sandbox-session-read" "printf made > $out" ""
      reading = "builtins.readFile (" <> made <> ")"
      top = unsandboxable "sandbox-session-top" "printf %s \"$made\" > $out" ("made = " <> reading <> ";")
      writingDrv = ["eval", "--read-write-mode", "--expr", T.unpack ("(" <> made <> ").drvPath")]
      sessions :: [(Text, [[String]], [String], Text)]
      sessions =
        [ ("eval", [writingDrv], ["eval"], reading),
          ("eval --read-write-mode", [], ["eval", "--read-write-mode"], reading),
          ("build", [], ["build", "--no-out-link"], top),
          ("trust", [writingDrv], ["trust"], top)
        ]
      refusal = "has '__noChroot' set, but that's not allowed when 'sandbox' is 'true'"
      outcome (code, out, err) = "exit " <> T.pack (show code) <> "; stdout=" <> T.pack out <> "; stderr=" <> T.pack err
      runSession (index, (label, preparing, command, expression)) = do
        let refusedStore = "refused-" <> show index
            relaxedStore = "relaxed-" <> show index
        refusedPrepared <- traverse (runCLI refusedStore) preparing
        refused <- runCLI refusedStore (command ++ ["--expr", T.unpack expression])
        relaxedPrepared <- traverse (runCLI relaxedStore) preparing
        relaxed <- runCLI relaxedStore (take 1 command ++ ["--relaxed-sandbox"] ++ drop 1 command ++ ["--expr", T.unpack expression])
        pure (label, refusedPrepared ++ relaxedPrepared, refused, relaxed)
  bracket_
    (forceRemoveIfExists root)
    (forceRemoveIfExists root)
    ( do
        runs <- traverse runSession (zip [0 :: Int ..] sessions)
        sequence
          [ runTest (label <> " refuses a __noChroot derivation an evaluation builds under sandbox = true, and builds it under --relaxed-sandbox") $
              case (filter (\(code, _, _) -> code /= ExitSuccess) prepared, refused, relaxed) of
                ([], (ExitFailure _, _, refusedErr), (ExitSuccess, _, _)) | refusal `T.isInfixOf` T.pack refusedErr -> Pass
                (failedPreparing, _, _) -> Fail ("preparing: " <> T.intercalate ", " (map outcome failedPreparing) <> "; refused: " <> outcome refused <> "; relaxed: " <> outcome relaxed)
          | (label, prepared, refused, relaxed) <- runs
          ]
    )

-- | nix.conf's cores reaches a builder the CLI runs, and with nothing
-- setting it the builder is told the machine's count, a positive number.
testBuildCoresCLI :: IO [Bool]
testBuildCoresCLI = do
  putStrLn "cli/build-cores"
  shell <- findTestShell
  tmpBase <- getTemporaryDirectory
  ambient <- getEnvironment
  let root = tmpBase </> "nova-nix-test-cli-build-cores"
      configVars = ["NIX_CONFIG", "NIX_CONF_DIR", "NIX_USER_CONF_FILES"]
      -- No nix.conf of the machine's is read: the system file is looked
      -- for under the run's root and an empty NIX_USER_CONF_FILES names
      -- no user file, so NIX_CONFIG alone sets cores.
      buildWith nixConfig store =
        Proc.readCreateProcessWithExitCode
          ( (Proc.proc "cabal" ["run", "-v0", "exe:nova-nix", "--", "build", "--expr", expression, "--store", root </> store, "--no-out-link"])
              { Proc.env =
                  Just
                    ( ("NIX_CONFIG", "substituters =\ntrusted-public-keys =\n" <> nixConfig)
                        : ("NIX_CONF_DIR", root </> "system")
                        : ("NIX_USER_CONF_FILES", "")
                        : filter ((`notElem` configVars) . fst) ambient
                    )
              }
          )
          ""
      nixString text = "\"" <> T.replace "${" "\\${" (T.replace "\"" "\\\"" (T.replace "\\" "\\\\" text)) <> "\""
      expression =
        T.unpack
          ("derivation { name = \"cores-cli\"; system = builtins.currentSystem; builder = " <> nixString shell <> "; args = [ \"-c\" \"echo $NIX_BUILD_CORES > $out\" ]; }")
      echoed (code, out, err) = case (code, listToMaybe (reverse (filter (not . null) (lines (filter (/= '\r') out))))) of
        (ExitSuccess, Just path) -> Right . T.strip <$> TIO.readFile path
        _ -> pure (Left ("exit " <> T.pack (show code) <> "; stdout=" <> T.pack out <> "; stderr=" <> T.pack err))
  bracket_
    (forceRemoveIfExists root >> createDirectoryIfMissing True root)
    (forceRemoveIfExists root)
    ( do
        configured <- buildWith "cores = 3\n" "configured" >>= echoed
        unset <- buildWith "" "unset" >>= echoed
        hardware <- Cores.hardwareConcurrency
        sequence
          [ runTest "nix.conf's cores is a CLI build's NIX_BUILD_CORES" $
              assertEqual "configured" (Right "3") configured,
            -- Off Linux there is no cgroup limit to read, so the count is
            -- the hardware's; on Linux the runner's cgroup may limit it.
            runTest "with nothing setting cores a CLI build is told the machine's count" $
              case unset of
                Right value
                  | Just (count :: Word32) <- readMaybe (T.unpack value),
                    count > 0,
                    SI.os == "linux" || count == Cores.defaultCores Nothing hardware ->
                      Pass
                other -> Fail ("unset: " <> T.pack (show other) <> "; hardware count " <> T.pack (show hardware))
          ]
    )

-- | A target and its arguments through the real CLI: a directory's
-- @default.nix@, a @\<NAME\>@ the search path finds, read as
-- @import \<NAME\>@ reads it, and a function called with @--arg@ and
-- @--argstr@, its defaults filling the rest, as @nix-instantiate --eval@
-- and @nix-build@ call one.  @eval@ calls a function only when an
-- argument is given, and under @--aterm@ always; @build@ calls the value
-- before @-A@ selects from it and once it has.  Where the command line
-- can hold a byte with no UTF-8 reading (POSIX), each value it gives
-- keeps that byte.
testTargetArgsCLI :: IO [Bool]
testTargetArgsCLI = do
  putStrLn "cli/target-args"
  shell <- findTestShell
  tmpBase <- getTemporaryDirectory
  ambient <- getEnvironment
  let root = tmpBase </> "nova-nix-test-cli-target-args"
      isolatedEnv =
        ("NIX_CONFIG", "substituters =\ntrusted-public-keys =\n")
          : ("XDG_CONFIG_HOME", root </> "config")
          : filter (\(key, _) -> key `notElem` ["NIX_CONFIG", "XDG_CONFIG_HOME", "NIX_PATH"]) ambient
      runCLI args =
        Proc.readCreateProcessWithExitCode
          ((Proc.proc "cabal" (["run", "-v0", "exe:nova-nix", "--"] ++ args)) {Proc.env = Just isolatedEnv})
          ""
      build args = runCLI (["build", "--store", root </> "store", "--no-out-link"] ++ args)
      builder = nixQuotedPath (T.unpack shell)
      derivationWriting name content = "derivation { name = " <> name <> "; system = builtins.currentSystem; builder = " <> builder <> "; args = [ \"-c\" \"printf " <> content <> " > $out\" ]; }"
      files =
        [ ("f.nix", "{ x, y ? 2 }: x + y"),
          ("needs.nix", "{ x, y }: x"),
          ("only-x.nix", "{ x }: x"),
          ("evaldir" </> "default.nix", "\"read from default.nix\""),
          ("pkg" </> "default.nix", "{ name ? \"pkg\" }: " <> derivationWriting "name" "pkg"),
          ("nixpkgs" </> "default.nix", "{ ... }: { hello = " <> derivationWriting "\"hello\"" "hello" <> "; }"),
          ("ellipsis.nix", "{ x, ... }@given: given.z"),
          ("functor.nix", "{ k = 10; __functor = self: { x }: x + self.k; }")
        ]
      -- A store path holding a default.nix whose value is a path literal,
      -- which names a place in the store path when the file is read as
      -- import reads it, and the file's on-disk directory otherwise.
      storeDerivation = "derivation { name = \"store-target\"; system = builtins.currentSystem; " <> hostToolsAttrs <> "builder = " <> builder <> "; args = [ \"-c\" \"mkdir $out; printf ./data > $out/default.nix; printf hi > $out/data\" ]; }"
      md5OfFf = "\"00594fd4f42ba43fc1ca0427a0576295\""
      lastLine out = listToMaybe (reverse (filter (not . null) (lines (filter (/= '\r') out))))
      printed (code, out, _) = case code of
        ExitSuccess -> lastLine out
        ExitFailure _ -> Nothing
      errorLines (code, _, err) = (code, lines (filter (/= '\r') err))
      outcome (code, out, err) = "exit " <> T.pack (show code) <> "; stdout=" <> T.pack out <> "; stderr=" <> T.pack err
      prints expected run = if printed run == Just expected then Pass else Fail (outcome run)
      missingLines name =
        [ "error: cannot evaluate a function that has an argument without a value ('" <> name <> "')",
          "       Nix attempted to evaluate a function as a top level expression; in",
          "       this case it must have its arguments supplied either by default",
          "       values, or passed explicitly with '--arg' or '--argstr'. See",
          "       https://nixos.org/manual/nix/stable/language/constructs.html#functions."
        ]
      at = (root </>)
  bracket_
    ( do
        forceRemoveIfExists root
        mapM_ (\(name, source) -> createDirectoryIfMissing True (takeDirectory (at name)) >> BS.writeFile (at name) (TE.encodeUtf8 source)) files
    )
    (forceRemoveIfExists root)
    ( do
        uncalled <- runCLI ["eval", at "f.nix"]
        called <- runCLI ["eval", "--arg", "x", "1", at "f.nix"]
        stringArg <- runCLI ["eval", "--argstr", "s", "hi", "--expr", "{ s }: s + \"!\""]
        missing <- runCLI ["eval", "--arg", "y", "1", at "needs.nix"]
        unused <- runCLI ["eval", "--arg", "x", "1", "--arg", "z", "throw \"never\"", at "only-x.nix"]
        directory <- runCLI ["eval", at "evaldir"]
        defaulted <- build [at "pkg"]
        renamed <- build ["--argstr", "name", "renamed", at "pkg"]
        found <- build ["--nix-path", "nixpkgs=" <> at "nixpkgs", "<nixpkgs>", "-A", "hello"]
        imported <- build ["--nix-path", "nixpkgs=" <> at "nixpkgs", "--expr", "(import <nixpkgs> { }).hello"]
        notFound <- build ["<nope>"]
        builtContent <- traverse BS.readFile (printed found)
        ellipsis <- runCLI ["eval", "--arg", "x", "1", "--arg", "z", "2", at "ellipsis.nix"]
        functor <- runCLI ["eval", "--arg", "x", "1", at "functor.nix"]
        strictCalled <- runCLI ["eval", "--strict", "--arg", "x", "1", "--expr", "{ x }: { a = x + 1; }"]
        lazyCalled <- runCLI ["eval", "--arg", "x", "1", "--expr", "{ x }: { a = x + 1; }"]
        aterm <- runCLI ["eval", "--aterm", at "pkg"]
        storeOutput <- build ["--expr", T.unpack storeDerivation]
        let storeBase = maybe "" takeFileName (printed storeOutput)
            storeEntry = "st=/nix/store/" <> storeBase
        storeTarget <- runCLI ["eval", "--store", root </> "store", "--nix-path", storeEntry, "<st>"]
        storeImport <- runCLI ["eval", "--store", root </> "store", "--nix-path", storeEntry, "--expr", "import <st>"]
        bytes <-
          if SI.os == "mingw32"
            then [True] <$ putStrLn "  SKIP  a command-line value keeps a byte with no UTF-8 reading: a Windows command line cannot hold one"
            else do
              (_, listed, _) <- Proc.readCreateProcessWithExitCode (Proc.proc "cabal" ["list-bin", "-v0", "exe:nova-nix"]) ""
              -- The shell makes the byte, 0xFF, so no codec on this side
              -- stands between it and the binary's arguments, and the
              -- binary runs itself: cabal run drops such a byte from the
              -- arguments it passes on (cabal-install 3.18.2).
              let withFf script =
                    Proc.readCreateProcessWithExitCode
                      ((Proc.proc (T.unpack shell) ["-c", "ff=$(printf '\\377'); " <> script, fromMaybe "nova-nix" (lastLine listed), at "evaldir"]) {Proc.env = Just isolatedEnv})
                      ""
              argstr <- withFf "exec \"$0\" eval --argstr s \"$ff\" --expr '{ s }: builtins.hashString \"md5\" s'"
              argExpr <- withFf "exec \"$0\" eval --arg s \"\\\"$ff\\\"\" --expr '{ s }: builtins.hashString \"md5\" s'"
              expr <- withFf "exec \"$0\" eval --expr \"builtins.hashString \\\"md5\\\" \\\"$ff\\\"\""
              searchName <- withFf "NIX_PATH=\"$ff=$1\" exec \"$0\" eval \"<$ff>\""
              badName <- withFf "exec \"$0\" eval --arg \"$ff\" 1 --expr '{ ... }: 1'"
              sequence
                [ runTest "--argstr keeps a byte with no UTF-8 reading" $
                    prints md5OfFf argstr,
                  runTest "--arg parses its expression's bytes" $
                    prints md5OfFf argExpr,
                  runTest "--expr parses its expression's bytes" $
                    prints md5OfFf expr,
                  runTest "a <NAME> is looked up by its bytes" $
                    prints "\"read from default.nix\"" searchName,
                  runTest "an argument name with no UTF-8 reading is refused, as an attribute name here is text" $
                    assertEqual "name" (ExitFailure 1, ["error: --arg: invalid UTF-8 in argument name"]) (errorLines badName)
                ]
        (++ bytes)
          <$> sequence
            [ runTest "eval prints a function given no argument, as nix-instantiate --eval does" $
                prints "<LAMBDA>" uncalled,
              runTest "eval calls a function with --arg, its default filling the rest" $
                prints "3" called,
              runTest "eval calls a function with --argstr's string" $
                prints "\"hi!\"" stringArg,
              runTest "a function missing an argument fails with upstream's message" $
                assertEqual "missing" (ExitFailure 1, missingLines "x") (errorLines missing),
              runTest "an --arg the function does not take is neither passed nor evaluated" $
                prints "1" unused,
              runTest "eval reads a directory's default.nix" $
                prints "\"read from default.nix\"" directory,
              runTest "build reads a directory's default.nix and calls its function with the defaults" $
                case printed defaulted of
                  Just path | "-pkg" `isSuffixOf` path -> Pass
                  _ -> Fail (outcome defaulted),
              runTest "build calls the function with --argstr's string" $
                case printed renamed of
                  Just path | "-renamed" `isSuffixOf` path -> Pass
                  _ -> Fail (outcome renamed),
              runTest "build <nixpkgs> -A hello calls the function the search path finds and selects from what it gives" $
                if isJust (printed found) && printed found == printed imported && builtContent == Just "hello"
                  then Pass
                  else Fail (outcome found <> "; the import: " <> outcome imported),
              runTest "a <NAME> the search path does not hold fails as the expression <NAME> does" $
                assertEqual "not found" (ExitFailure 1, ["error: file 'nope' was not found in the Nix search path (add it using $NIX_PATH or -I)"]) (errorLines notFound),
              runTest "a function that takes ... is passed every argument" $
                prints "2" ellipsis,
              runTest "a set with __functor is called as the function its functor gives" $
                prints "11" functor,
              runTest "--strict evaluates the whole of what the call gives" $
                prints "{ a = 2; }" strictCalled `andThen` prints "{ a = <CODE>; }" lazyCalled,
              runTest "--aterm takes a file target and calls its function with the defaults" $
                case printed aterm of
                  Just line | "Derive(" `isPrefixOf` line, "-pkg\"" `isInfixOf` line -> Pass
                  _ -> Fail (outcome aterm),
              runTest "a <NAME> in the store is read as import <NAME> reads it, its path literals naming places in the store path" $
                case (printed storeTarget, printed storeImport) of
                  (Just fromTarget, Just fromImport)
                    | not (null storeBase), fromTarget == fromImport, fromTarget == "/nix/store/" <> storeBase <> "/data" -> Pass
                  _ -> Fail (outcome storeOutput <> "; the target: " <> outcome storeTarget <> "; the import: " <> outcome storeImport)
            ]
    )

-- | The link a build makes to its output, through the real CLI run in a
-- directory of the test's own: @./result@ when no flag names another,
-- with @-NAME@ appended for an output other than @out@, each a root that
-- @store gc@ keeps, as nix-build links and roots them; with
-- @--no-out-link@ no link and no root.  The parse and the names are
-- checked on their own first.
testOutLinkCLI :: IO [Bool]
testOutLinkCLI = do
  putStrLn "cli/out-link"
  shell <- findTestShell
  canLink <- symlinksAvailable
  projectDir <- Dir.getCurrentDirectory
  tmpBase <- getTemporaryDirectory
  ambient <- getEnvironment
  let root = tmpBase </> "nova-nix-test-cli-out-link"
      linkedDir = root </> "linked"
      unlinkedDir = root </> "unlinked"
      isolatedEnv =
        ("NIX_CONFIG", "substituters =\ntrusted-public-keys =\n")
          : ("XDG_CONFIG_HOME", root </> "config")
          : filter (\(key, _) -> key `notElem` ["NIX_CONFIG", "XDG_CONFIG_HOME"]) ambient
      -- cabal finds the project from the directory it runs in, which is
      -- the directory the link is made in, so the project is named.
      runIn dir args =
        Proc.readCreateProcessWithExitCode
          ((Proc.proc "cabal" (["run", "-v0", "--project-dir", projectDir, "exe:nova-nix", "--"] ++ args)) {Proc.env = Just isolatedEnv, Proc.cwd = Just dir})
          ""
      expression =
        T.unpack
          ( "derivation { name = \"out-link\"; system = builtins.currentSystem; builder = "
              <> nixQuotedPath (T.unpack shell)
              <> "; outputs = [ \"out\" \"dev\" ]; args = [ \"-c\" \"echo out > $out; echo dev > $dev\" ]; }"
          )
      buildIn dir store flags = runIn dir (["build", "--expr", expression, "--store", root </> store] ++ flags)
      collectIn dir store = runIn dir ["store", "gc", "--store", root </> store]
      printed (code, out, _) = case code of
        ExitSuccess -> listToMaybe (reverse (filter (not . null) (lines (filter (/= '\r') out))))
        ExitFailure _ -> Nothing
      outcome (code, out, err) = "exit " <> T.pack (show code) <> "; stdout=" <> T.pack out <> "; stderr=" <> T.pack err
      linkTarget path = either (\(_ :: IOException) -> Nothing) Just <$> try (Dir.getSymbolicLinkTarget path)
      parsed = fmap Cli.optCommand . Cli.parseArgs
      buildOf = Cli.CmdBuild (Cli.TargetExpr "1") Nothing
  parses <-
    sequence
      [ runTest "build links ./result unless --out-link names another path, or --no-out-link or --no-link none" $
          assertEqual "no flag" (Right (buildOf (Cli.OutLinkAt "result"))) (parsed ["build", "--expr", "1"])
            `andThen` assertEqual "--out-link" (Right (buildOf (Cli.OutLinkAt "out"))) (parsed ["build", "-o", "out", "--expr", "1"])
            `andThen` assertEqual "--no-out-link" (Right (buildOf Cli.NoOutLink)) (parsed ["build", "--expr", "1", "--no-out-link"])
            `andThen` assertEqual "--no-link" (Right (buildOf Cli.NoOutLink)) (parsed ["build", "--no-link", "--expr", "1"])
            `andThen` assertEqual "--no-out-link twice" (Right (buildOf Cli.NoOutLink)) (parsed ["build", "--no-out-link", "--expr", "1", "--no-out-link"]),
        runTest "a --no-out-link after an --out-link links nothing, as nix-build's later flag wins" $
          assertEqual "--no-out-link" (Right (buildOf Cli.NoOutLink)) (parsed ["build", "-o", "out", "--no-out-link", "--expr", "1"])
            `andThen` assertEqual "--no-link" (Right (buildOf Cli.NoOutLink)) (parsed ["build", "-o", "out", "--expr", "1", "--no-link"]),
        runTest "an --out-link after a --no-out-link links its path, as nix-build's later flag wins" $
          assertEqual "--no-out-link" (Right (buildOf (Cli.OutLinkAt "out"))) (parsed ["build", "--no-out-link", "--expr", "1", "--out-link", "out"])
            `andThen` assertEqual "--no-link" (Right (buildOf (Cli.OutLinkAt "out"))) (parsed ["build", "--no-link", "-o", "out", "--expr", "1"]),
        runTest "a second --out-link replaces the first, as nix-build's later flag wins" $
          assertEqual "two links" (Right (buildOf (Cli.OutLinkAt "b"))) (parsed ["build", "-o", "a", "--expr", "1", "--out-link", "b"]),
        runTest "an output other than out is linked with -NAME appended to the out-link, as nix-build names it" $
          assertEqual "out" (Just "result") (Cli.outLinkFor (Cli.OutLinkAt "result") "out")
            `andThen` assertEqual "dev" (Just "result-dev") (Cli.outLinkFor (Cli.OutLinkAt "result") "dev")
            `andThen` assertEqual "a path" (Just ("links" </> "hello-bin")) (Cli.outLinkFor (Cli.OutLinkAt ("links" </> "hello")) "bin")
            `andThen` assertEqual "no link" Nothing (Cli.outLinkFor Cli.NoOutLink "dev")
      ]
  runs <-
    bracket_
      (forceRemoveIfExists root >> mapM_ (createDirectoryIfMissing True) [linkedDir, unlinkedDir])
      (forceRemoveIfExists root)
      ( do
          linked <-
            if not canLink
              then [] <$ putStrLn "  SKIP  a build's link needs symlink privilege"
              else do
                outBuilt <- buildIn linkedDir "linked-store" []
                devBuilt <- buildIn linkedDir "linked-store" ["-A", "dev"]
                outLink <- linkTarget (linkedDir </> "result")
                devLink <- linkTarget (linkedDir </> "result-dev")
                collected@(collectedCode, _, _) <- collectIn linkedDir "linked-store"
                kept <- traverse Dir.doesPathExist (catMaybes [printed outBuilt, printed devBuilt])
                sequence
                  [ runTest "build links its output from ./result" $
                      case printed outBuilt of
                        Just path | outLink == Just path -> Pass
                        _ -> Fail (outcome outBuilt <> "; link: " <> T.pack (show outLink)),
                    runTest "build links an output other than out from ./result-NAME" $
                      case printed devBuilt of
                        Just path | devLink == Just path -> Pass
                        _ -> Fail (outcome devBuilt <> "; link: " <> T.pack (show devLink)),
                    runTest "store gc keeps the outputs a build's links root" $
                      if collectedCode == ExitSuccess && kept == [True, True]
                        then Pass
                        else Fail (outcome collected <> "; kept: " <> T.pack (show kept))
                  ]
          unlinkedBuilt <- buildIn unlinkedDir "unlinked-store" ["--no-out-link"]
          left <- Dir.listDirectory unlinkedDir
          unlinkedCollected@(unlinkedCode, _, _) <- collectIn unlinkedDir "unlinked-store"
          survived <- traverse Dir.doesPathExist (printed unlinkedBuilt)
          unlinked <-
            runTest "build --no-out-link links nothing, and store gc then deletes the output" $
              if null left && unlinkedCode == ExitSuccess && survived == Just False
                then Pass
                else Fail (outcome unlinkedBuilt <> "; left: " <> T.pack (show left) <> "; gc: " <> outcome unlinkedCollected)
          -- nix-build realises only the output outputName names: with
          -- out valid and dev deleted, building out builds nothing.
          let marked = T.unpack ("derivation { name = \"one-output\"; system = builtins.currentSystem; builder = " <> nixQuotedPath (T.unpack shell) <> "; outputs = [ \"out\" \"dev\" ]; args = [ \"-c\" \"echo the-builder-ran >&2; echo out > $out; echo dev > $dev\" ]; }")
              buildMarked flags = runIn unlinkedDir (["build", "--expr", marked, "--store", root </> "one-output-store", "--no-out-link"] ++ flags)
          devBuilt <- buildMarked ["-A", "dev"]
          deleted <- traverse (\path -> runIn unlinkedDir ["store", "delete", "--store", root </> "one-output-store", path]) (printed devBuilt)
          outBuilt@(_, _, outErr) <- buildMarked []
          devGone <- traverse (fmap not . Dir.doesPathExist) (printed devBuilt)
          oneOutput <-
            runTest "build realises only the output its outputName names, as nix-build does" $
              case (deleted, printed outBuilt, devGone) of
                (Just (ExitSuccess, _, _), Just _, Just True)
                  | not ("the-builder-ran" `isInfixOf` outErr) -> Pass
                _ -> Fail (outcome outBuilt <> "; dev: " <> outcome devBuilt <> "; dev gone: " <> T.pack (show devGone))
          pure (linked ++ [unlinked, oneOutput])
      )
  pure (parses ++ runs)

-- ---------------------------------------------------------------------------
-- Tests: trust
-- ---------------------------------------------------------------------------

-- | @trust@ end to end: a derivation built by a seed a loopback server
-- serves, taking an archive it serves too, both fetched by
-- builtin:fetchurl into a store of the run's own, scanned, and reported,
-- and two fixed-output derivations whose builder is a host program, which
-- trust must not build: one the server holds as a binary cache, which is
-- substituted and scanned, and one it does not, which is not read.  Each
-- host program fails if it runs, and a failed build stops the command.
testTrustCLI :: IO [Bool]
testTrustCLI = do
  putStrLn "cli/trust"
  tmpBase <- getTemporaryDirectory
  ambient <- getEnvironment
  let root = tmpBase </> "nova-nix-test-cli-trust"
      archive = BL.toStrict (GZip.compress (Tar.write [tarFile "tool/bin/tool.exe" (BL.fromStrict trustPe), tarFile "tool/bin/run.sh" "#!/bin/sh\necho hi\n"]))
      madeBytes = "what a host program would make\n" :: BS.ByteString
      unheldBytes = "what no cache holds\n" :: BS.ByteString
      madeNar = NAR.serialise (NAR.NarRegular False madeBytes)
      isolatedEnv =
        ("NIX_CONFIG", "substituters =\ntrusted-public-keys =\n")
          : ("NIX_CONF_DIR", root </> "system")
          : ("NIX_USER_CONF_FILES", "")
          : filter ((`notElem` ["NIX_CONFIG", "NIX_CONF_DIR", "NIX_USER_CONF_FILES"]) . fst) ambient
      fixedAttrs bytes = "outputHashMode = \"flat\"; outputHashAlgo = \"sha256\"; outputHash = \"" <> Hash.bytesToHexText (sha256Digest bytes) <> "\";"
      fetch base name target bytes =
        "derivation { name = \"" <> name <> "\"; url = \"" <> base <> target <> "\"; system = \"builtin\"; builder = \"builtin:fetchurl\"; " <> fixedAttrs bytes <> " }"
      hostBuilt name bytes =
        "derivation { name = \"" <> name <> "\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; args = [ \"-c\" \"exit 1\" ]; " <> fixedAttrs bytes <> " }"
      expression base =
        "let seed = "
          <> fetch base "seed.exe" "/seed" trustPe
          <> "; archive = "
          <> fetch base "tool.tar.gz" "/archive" archive
          <> "; made = "
          <> hostBuilt "made.txt" madeBytes
          <> "; unheld = "
          <> hostBuilt "unheld.txt" unheldBytes
          <> "; in derivation { name = \"tool\"; system = \"x86_64-linux\"; builder = \"${seed}\"; args = [ \"${archive}\" \"${made}\" \"${unheld}\" ]; }"
      narInfoTarget out = "/" <> TE.encodeUtf8 (spHash out) <> ".narinfo"
  seedOut <- either (fail . show) pure (makeFixedOutputPath "seed.exe" "sha256" FlatIngestion (sha256Digest trustPe))
  archiveOut <- either (fail . show) pure (makeFixedOutputPath "tool.tar.gz" "sha256" FlatIngestion (sha256Digest archive))
  madeOut <- either (fail . show) pure (makeFixedOutputPath "made.txt" "sha256" FlatIngestion (sha256Digest madeBytes))
  unheldOut <- either (fail . show) pure (makeFixedOutputPath "unheld.txt" "sha256" FlatIngestion (sha256Digest unheldBytes))
  (trustedKey, madeNarInfo) <- either fail pure (signedTestNarInfo madeOut [] "nar/made.nar" madeNar)
  let route target _
        | target == "/seed" = Send (httpResponse "200 OK" trustPe)
        | target == "/archive" = Send (httpResponse "200 OK" archive)
        | target == narInfoTarget madeOut = Send (httpResponse "200 OK" (TE.encodeUtf8 madeNarInfo))
        | target == "/nar/made.nar" = Send (httpResponse "200 OK" madeNar)
        | otherwise = Send (httpResponse "404 Not Found" "missing")
  (code, out, err, seen) <-
    bracket_ (forceRemoveIfExists root >> createDirectoryIfMissing True root) (forceRemoveIfExists root) $
      withHttpServer route $ \base requests _ -> do
        (code, out, err) <-
          Proc.readCreateProcessWithExitCode
            ((Proc.proc "cabal" ["run", "-v0", "exe:nova-nix", "--", "trust", "--store", root </> "store", "--substituter", T.unpack base, "--trusted-key", T.unpack trustedKey, "--expr", T.unpack (expression base)]) {Proc.env = Just isolatedEnv})
            ""
        (,,,) code out err <$> requests
  let path = storePathToText defaultStoreDir
      asked = [narInfoTarget madeOut, "/nar/made.nar", narInfoTarget unheldOut]
  sequence
    [ runTest "trust fetches what a builtin fetches, substitutes what a cache holds, scans both, and builds nothing" $
        case code of
          ExitSuccess ->
            assertEqual
              "stdout"
              [ "Fixed-output inputs run as builders: 1",
                "  " <> path seedOut,
                "    fetched by builtin:fetchurl",
                "    builder of 1 derivation: " <> path seedOut,
                "    PE image: the fetched file",
                "Fixed-output inputs holding prebuilt code: 1",
                "  " <> path archiveOut,
                "    fetched by builtin:fetchurl",
                "    PE image: tool/bin/tool.exe",
                "Fixed-output inputs not fully read: 1",
                "  " <> path unheldOut,
                "    built in the closure by /bin/sh, its bytes pinned by its hash",
                "    not read: the store does not hold it",
                "Fixed-output inputs holding no prebuilt code: 1",
                "Builders outside the closure: 1",
                "  /bin/sh, for 2 derivations",
                "Builders in input sources: 0",
                "Builtin builders: 1",
                "  builtin:fetchurl, for 2 derivations",
                "Input sources: 0",
                "This reports what the closure relies on, not whether any of it is safe."
              ]
              (map (T.filter (/= '\r')) (T.lines (T.pack out)))
          _ -> Fail ("exited " <> T.pack (show code) <> ": " <> T.pack err),
      runTest "trust asks the caches for each input no builtin fetches before it reports one as not read" $
        assertEqual "requests" (sort asked) (sort (nub (filter (`elem` asked) seen)))
    ]

-- ---------------------------------------------------------------------------
-- Tests: the CLI reference
-- ---------------------------------------------------------------------------

-- | Where the reference's JSON is committed, from the repository root.
cliJsonPath :: FilePath
cliJsonPath = "docs" </> "cli.json"

-- | The argument that writes 'Ref.referenceJson' to 'cliJsonPath' in place
-- of running the suite.
writeCliJsonFlag :: String
writeCliJsonFlag = "--write-cli-json"

-- | The reference against the parser it documents, the committed JSON
-- against the reference, and each example that shows output against what
-- the CLI prints.
testCliReference :: IO [Bool]
testCliReference = do
  putStrLn "cli/reference"
  committed <- try (BS.readFile cliJsonPath)
  tmpBase <- getTemporaryDirectory
  ambient <- getEnvironment
  let root = tmpBase </> "nova-nix-test-cli-reference"
      -- No nix.conf of the machine's is read: each directory the config
      -- files are looked for in is under the run's own root.
      isolatedEnv =
        ("NIX_CONFIG", "substituters =\ntrusted-public-keys =\n")
          : ("NIX_CONF_DIR", root </> "system")
          : ("XDG_CONFIG_HOME", root </> "config")
          : ("XDG_CONFIG_DIRS", root </> "config-dirs")
          : filter (\(key, _) -> key `notElem` ["NIX_CONFIG", "NIX_CONF_DIR", "NIX_USER_CONF_FILES", "XDG_CONFIG_HOME", "XDG_CONFIG_DIRS"]) ambient
      runCLI args =
        Proc.readCreateProcessWithExitCode
          ((Proc.proc "cabal" (["run", "-v0", "exe:nova-nix", "--"] ++ args)) {Proc.env = Just isolatedEnv})
          ""
      printsHelp label args = runTestM label $ do
        (code, out, err) <- runCLI args
        pure $ case code of
          ExitSuccess -> assertEqual "stdout" Ref.helpLines (map (T.filter (/= '\r')) (T.lines (T.pack out)))
          _ -> Fail ("exited " <> T.pack (show code) <> ": " <> T.pack err)
      shown =
        [ example
        | entry <- Ref.globalForm : map Ref.command Ref.commandNames,
          example <- toList (Ref.commandExamples entry),
          not (null (Ref.exampleOutput example))
        ]
  checks <-
    sequence
      [ runTest "docs/cli.json is the reference's JSON" $ case committed of
          Left (err :: IOException) -> Fail (T.pack (cliJsonPath <> ": " <> displayException err <> "; " <> regenerateCliJson))
          Right bytes
            | bytes == Ref.referenceJson -> Pass
            | otherwise -> Fail (T.pack (cliJsonPath <> " differs from what Nix.Cli.Reference renders; " <> regenerateCliJson)),
        runTest "every command a flag lists accepts it" $
          allPass
            [ case Cli.parseArgs args of
                Right opts | cliSelects name (Cli.optCommand opts) -> Pass
                other -> Fail (T.pack (unwords args <> ": " <> show (Cli.optCommand <$> other)))
            | (flagName, entry) <- referenceFlags,
              Ref.CommandFlag names <- [Ref.flagScope entry],
              name <- toList names,
              spelling <- toList (Ref.flagSpellings entry),
              let args = cliLine name flagName spelling (cliValue entry)
            ],
        -- The other way round, so --help lists exactly what each command
        -- takes after its words.  A global flag is answered anywhere, and
        -- a spelling two commands give different flags, as build's and
        -- develop's --check, is the other flag's there.
        runTest "no command takes, after its words, a flag the reference does not list for it" $
          allPass
            [ case Cli.parseArgs args of
                Right opts | cliSelects name (Cli.optCommand opts) -> Fail (T.pack (unwords args))
                _ -> Pass
            | (flagName, entry) <- referenceFlags,
              Ref.CommandFlag names <- [Ref.flagScope entry],
              name <- Ref.commandNames,
              name `notElem` names,
              spelling <- toList (Ref.flagSpellings entry),
              spelling `notElem` commandSpellings name,
              let args = cliLine name flagName spelling (cliValue entry)
            ],
        runTest "a global flag is answered on its own" $
          allPass
            [ case Cli.parseArgs [T.unpack spelling] of
                Right opts | Cli.optCommand opts `elem` [Cli.CmdHelp, Cli.CmdVersion] -> Pass
                other -> Fail (spelling <> ": " <> T.pack (show (Cli.optCommand <$> other)))
            | flagName <- Ref.globalFlags,
              spelling <- toList (Ref.flagSpellings (Ref.flag flagName))
            ],
        runTest "a global flag is answered after each command's words and after its operands" $
          allPass
            [ case Cli.optCommand <$> Cli.parseArgs args of
                Right answer | Just answer == globalAnswer flagName -> Pass
                other -> Fail (T.pack (unwords args <> ": " <> show other))
            | flagName <- Ref.globalFlags,
              spelling <- toList (Ref.flagSpellings (Ref.flag flagName)),
              prefix <- nub (concatMap cliPrefixes Ref.commandNames ++ [cliWords name ++ cliOperands name flagName | name <- Ref.commandNames]),
              let args = prefix ++ [T.unpack spelling]
            ],
        -- A flag with values is asked for them when the line ends before
        -- it has them all; one without parses when it ends a complete
        -- line, where a clause that wanted a value would have nothing to
        -- take.
        runTest "a flag asks for values exactly when its entry names them" $
          allPass
            [ let values = cliValue entry
                  missing = Left (cliShortfall flagName spelling values)
                  given count = Cli.optCommand <$> Cli.parseArgs (prefix ++ [T.unpack spelling] ++ take count values)
                  complete = prefix ++ operands ++ [T.unpack spelling]
               in case (values, Cli.parseArgs complete) of
                    (_ : _, _) | all ((== missing) . given) [0 .. length values - 1] -> Pass
                    ([], Right _) | given 0 /= missing -> Pass
                    (_, completed) -> Fail (T.pack (unwords (prefix ++ [T.unpack spelling]) <> ": " <> show (map given [0 .. length values]) <> "; " <> unwords complete <> ": " <> show (Cli.optCommand <$> completed)))
            | (flagName, entry) <- referenceFlags,
              (prefix, operands) <- case Ref.flagScope entry of
                Ref.GlobalFlag -> [([], [])]
                Ref.CommandFlag names -> [(cliWords name, cliOperands name flagName) | name <- toList names],
              spelling <- toList (Ref.flagSpellings entry)
            ],
        runTest "every example parses as the command it documents" $
          allPass
            ( [ exampleParses (`elem` [Cli.CmdHelp, Cli.CmdVersion]) example
              | example <- toList (Ref.commandExamples Ref.globalForm)
              ]
                ++ [ exampleParses (cliSelects name) example
                   | name <- Ref.commandNames,
                     example <- toList (Ref.commandExamples (Ref.command name))
                   ]
            ),
        runTest "spellings are well formed and name one flag each where they apply" $
          let spellings = [(scope, spelling) | (_, entry) <- referenceFlags, scope <- scopeCommands (Ref.flagScope entry), spelling <- toList (Ref.flagSpellings entry)]
              scopeCommands scope = case scope of
                Ref.GlobalFlag -> [Nothing]
                Ref.CommandFlag names -> map Just (toList names)
              longFormed = all (\part -> not (T.null part) && T.all isAsciiLower part) . T.splitOn "-"
              wellFormed entry =
                all longFormed (Ref.flagLong entry : toList (Ref.flagAlias entry))
                  && maybe True isAsciiLetter (Ref.flagShort entry)
           in assertEqual "malformed" [] [Ref.flagLong entry | (_, entry) <- referenceFlags, not (wellFormed entry)]
                `andThen` assertEqual "repeated" [] [spelling | (spelling, count) <- Map.toList (Map.fromListWith (+) [(spelling, 1 :: Int) | spelling <- spellings]), count > 1],
        -- A value of 1 or 0 moves each from its default, a Boolean that is
        -- on by default by 0, and sandbox, whose values are words, by
        -- relaxed.
        runTest "every setting is one nix.conf resolves" $
          let moves setting value = either (const False) (/= Config.defaultNixConfig) (Config.resolveConfig [Config.ConfigAssignment (TE.encodeUtf8 (Ref.settingName setting)) value])
           in allPass
                [ if any (moves setting) ["1", "0", "relaxed"] then Pass else Fail (Ref.settingName setting <> ": none of 1, 0 and relaxed moves the configuration")
                | setting <- toList (Ref.configSettings Ref.configuration)
                ],
        runTest "help lists --no-allow-import-from-derivation" $
          if any ("--no-allow-import-from-derivation" `T.isInfixOf`) Ref.helpLines then Pass else Fail (T.unlines Ref.helpLines),
        runTest "marked text closes every code span, and help prints no marks" $
          let marked =
                [ text
                | entry <- Ref.globalForm : map Ref.command Ref.commandNames,
                  text <-
                    Ref.commandDescription entry
                      : map Ref.formSummary (toList (Ref.commandForms entry))
                      ++ map Ref.exampleDescription (toList (Ref.commandExamples entry))
                ]
                  ++ [Ref.flagDescription (Ref.flag name) | name <- Ref.flagNames]
                  ++ [Ref.settingDescription setting | setting <- toList (Ref.configSettings Ref.configuration)]
                  ++ [Ref.configDescription Ref.configuration]
           in assertEqual "unclosed" [] (filter (odd . T.count (T.singleton Ref.codeMark)) marked)
                `andThen` assertEqual "printed" [] (filter (T.any (== Ref.codeMark)) Ref.helpLines),
        runTest "help fits its width" $
          assertEqual "longer" [] (filter ((> Ref.helpWidth) . T.length) Ref.helpLines),
        runTest "an example reads as it is typed" $
          assertEqual "shell" "nova-nix eval --expr '1 + 2'" (Ref.exampleCommandLine (Ref.Example "" ("eval" :| ["--expr", "1 + 2"]) []))
            `andThen` assertEqual "quote" "'it'\\''s'" (Ref.shellQuote "it's")
            `andThen` assertEqual "empty" "''" (Ref.shellQuote "")
            `andThen` assertEqual "zsh's equals expansion" "'=ls'" (Ref.shellQuote "=ls"),
        runTestM "a quoted argument reads back as itself" $ do
          result <-
            QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} $
              QC.forAll genShellText $
                \text -> readShellWord (Ref.shellQuote text) QC.=== Just text
          pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result))),
        runTestM "wrapped text keeps its words and fits its width" $ do
          result <-
            QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} $
              QC.forAll ((,) <$> QC.chooseInt (1, 40) <*> genWrapText) $ \(width, text) ->
                let wrapped = Ref.wrapWords width text
                 in T.words (T.unwords wrapped) QC.=== T.words text
                      QC..&&. all (\line -> T.length line <= width || length (T.words line) == 1) wrapped
          pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result))),
        runTestM "indented JSON parses back to the value it came from" $ do
          result <-
            QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} $
              QC.forAll (genJson 3) $ \json ->
                (parseJson =<< either (Left . T.pack . show) Right (TE.decodeUtf8' (Ref.renderIndentedJson json))) QC.=== Right json
          pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))
      ]
  ran <-
    bracket_
      (forceRemoveIfExists root >> createDirectoryIfMissing True root)
      (forceRemoveIfExists root)
      ( do
          examples <-
            mapM
              ( \example -> runTestM ("example prints what it shows: " <> Ref.exampleCommandLine example) $ do
                  (code, out, err) <- runCLI (map T.unpack (toList (Ref.exampleArguments example)))
                  pure $ case code of
                    ExitSuccess -> assertEqual "stdout" (Ref.exampleOutput example) (map (T.filter (/= '\r')) (T.lines (T.pack out)))
                    _ -> Fail ("exited " <> T.pack (show code) <> ": " <> T.pack err)
              )
              shown
          helps <-
            mapM
              (\prefix -> printsHelp ("nova-nix " <> T.pack (unwords prefix) <> " --help prints the help and exits 0") (prefix ++ ["--help"]))
              (nub (concatMap cliPrefixes Ref.commandNames))
          pure (examples ++ helps)
      )
  pure (checks ++ ran)
  where
    referenceFlags = [(name, Ref.flag name) | name <- Ref.flagNames]
    commandSpellings name =
      [ spelling
      | (_, entry) <- referenceFlags,
        Ref.CommandFlag names <- [Ref.flagScope entry],
        name `elem` names,
        spelling <- toList (Ref.flagSpellings entry)
      ]
    regenerateCliJson = "from the repository root, regenerate it with: cabal run --enable-tests -v0 nova-nix-test -- " <> writeCliJsonFlag
    allPass = foldr andThen Pass
    isAsciiLetter c = isAsciiLower c || isAsciiUpper c
    exampleParses accepts example =
      let args = map T.unpack (toList (Ref.exampleArguments example))
       in case Cli.parseArgs args of
            Right opts | accepts (Cli.optCommand opts) -> Pass
            other -> Fail (T.pack (unwords args <> ": " <> show (Cli.optCommand <$> other)))

-- | The arguments that select a command.
cliWords :: Ref.CommandName -> [String]
cliWords = map T.unpack . Ref.commandWords

-- | The words that start a command: its first word, as @store@, through
-- all of them.
cliPrefixes :: Ref.CommandName -> [[String]]
cliPrefixes = drop 1 . inits . cliWords

-- | The command a global flag selects.
globalAnswer :: Ref.FlagName -> Maybe Cli.Command
globalAnswer flagName = case flagName of
  Ref.HelpFlag -> Just Cli.CmdHelp
  Ref.VersionFlag -> Just Cli.CmdVersion
  _ -> Nothing

-- | A value for each value a flag needs, which the parser accepts
-- unchecked: one for each name its entry gives outside brackets.
cliValue :: Ref.FlagEntry -> [String]
cliValue entry = maybe [] (map (const "value") . filter (not . ("[" `T.isPrefixOf`)) . T.words) (Ref.flagArgument entry)

-- | A command line with a flag and its values in it, completed by the
-- operands the command needs: after the flag, or before it for
-- @--command@, which takes every argument after it.
cliLine :: Ref.CommandName -> Ref.FlagName -> Text -> [String] -> [String]
cliLine name flagName spelling values = case flagName of
  Ref.ShellCommandFlag -> cliWords name ++ cliOperands name flagName ++ [T.unpack spelling] ++ values
  _ -> cliWords name ++ [T.unpack spelling] ++ values ++ cliOperands name flagName

-- | What the parser says to a flag given fewer values than it needs:
-- upstream's words after @error: @ for @--command@, under either
-- spelling.
cliShortfall :: Ref.FlagName -> Text -> [String] -> String
cliShortfall flagName spelling values = case (flagName, values) of
  (Ref.ShellCommandFlag, _) -> "error: --command requires at least one argument"
  (_, [_, _]) -> T.unpack spelling <> " requires a name and a value"
  _ -> T.unpack spelling <> " requires a value"

-- | What completes a command line once a flag is in it: the operands the
-- command needs, but an expression when the flag is @--expr@, which gives
-- one.
cliOperands :: Ref.CommandName -> Ref.FlagName -> [String]
cliOperands name flagName = case name of
  Ref.EvalCommand -> expression
  Ref.BuildCommand -> expression
  Ref.DevelopCommand -> expression
  Ref.PrintDevEnvCommand -> expression
  Ref.TrustCommand -> expression
  Ref.PushCommand -> ["--cache", "https://cache.example.org", "hash-name"]
  Ref.StoreDeleteCommand -> ["hash-name"]
  Ref.StoreGcCommand -> []
  Ref.ProfileAddCommand -> expression
  Ref.ProfileRemoveCommand -> ["hello" | flagName /= Ref.AllFlag]
  Ref.ProfileListCommand -> []
  Ref.ProfileHistoryCommand -> []
  Ref.ProfileRollbackCommand -> []
  Ref.ProfileWipeHistoryCommand -> []
  where
    expression = concat [["--expr", "1"] | flagName /= Ref.ExprFlag]

-- | Whether a parsed command is the reference's command.
cliSelects :: Ref.CommandName -> Cli.Command -> Bool
cliSelects name parsed = case (name, parsed) of
  (Ref.EvalCommand, Cli.CmdEval _) -> True
  (Ref.BuildCommand, Cli.CmdBuild {}) -> True
  (Ref.DevelopCommand, Cli.CmdDevelop {}) -> True
  (Ref.PrintDevEnvCommand, Cli.CmdPrintDevEnv {}) -> True
  (Ref.TrustCommand, Cli.CmdTrust {}) -> True
  (Ref.PushCommand, Cli.CmdPush _) -> True
  (Ref.StoreDeleteCommand, Cli.CmdStoreDelete _) -> True
  (Ref.StoreGcCommand, Cli.CmdStoreGc _) -> True
  (Ref.ProfileAddCommand, Cli.CmdProfileAdd {}) -> True
  (Ref.ProfileRemoveCommand, Cli.CmdProfileRemove {}) -> True
  (Ref.ProfileListCommand, Cli.CmdProfileList {}) -> True
  (Ref.ProfileHistoryCommand, Cli.CmdProfileHistory {}) -> True
  (Ref.ProfileRollbackCommand, Cli.CmdProfileRollback {}) -> True
  (Ref.ProfileWipeHistoryCommand, Cli.CmdProfileWipeHistory {}) -> True
  _ -> False

-- | One word read back as a POSIX shell reads it, for what
-- 'Ref.shellQuote' writes: plain characters, single-quoted runs, and a
-- character escaped by a backslash.  A character the shell would act on
-- is no reading at all.
readShellWord :: Text -> Maybe Text
readShellWord = go ""
  where
    go acc rest = case T.uncons rest of
      Nothing -> Just acc
      Just ('\'', more) -> case T.break (== '\'') more of
        (quoted, closing) -> case T.uncons closing of
          Just (_, after) -> go (acc <> quoted) after
          Nothing -> Nothing
      Just ('\\', more) -> case T.uncons more of
        Just (escaped, after) -> go (T.snoc acc escaped) after
        Nothing -> Nothing
      Just (c, more)
        | c `elem` shellSpecial -> Nothing
        | otherwise -> go (T.snoc acc c) more
    shellSpecial = " \t\n\"#$&()*;<>?[]`{|}~!" :: String

-- | Text with the characters a shell acts on mixed among plain ones.
genShellText :: QC.Gen Text
genShellText = T.pack <$> QC.listOf (QC.elements "a1_-=:/.,@%+ '\"\\$`;&|<>()*?[]#~{}!\t\n")

-- | Words of a few lengths, runs of spaces between them.
genWrapText :: QC.Gen Text
genWrapText = T.pack <$> QC.listOf (QC.elements "abcdefghij        ")

-- ---------------------------------------------------------------------------
-- Tests: substituter signature verification (the --trusted-key trust gate)
-- ---------------------------------------------------------------------------

-- | The nix.conf / NIX_CONFIG resolver: parsing, aliases, the extra-
-- append rule, the precedence fold, include expansion over an in-memory
-- file map, and the file cascade.  The trusted-key cases are the
-- security-relevant ones, a plain assignment in a higher source replaces
-- the trusted set, an extra- widens it.
testNixConfig :: IO [Bool]
testNixConfig = do
  putStrLn "config/nix-conf"
  let -- A root the platform agrees is absolute: "/" is not one on
      -- Windows, for System.FilePath and for upstream's canonPath alike.
      root = if SI.os == "mingw32" then "C:\\" else "/"
      etcNix = root </> "etc" </> "nix"
      etcXdg = root </> "etc" </> "xdg"
      etcXdgLocal = root </> "etc" </> "xdg-local"
      configHome = root </> "home" </> "u" </> ".config"
      systemConf = etcNix </> "nix.conf"
      xdgConf dir = dir </> "nix" </> "nix.conf"
      -- The expander's one effect, served from an in-memory map: a path
      -- is in it or there is nothing there.
      readFrom files path = Identity (maybe (Left Config.ConfigFileMissing) Right (Map.lookup path files))
      resolveWith files paths env = runIdentity (Config.loadConfig (readFrom files) paths env)
      substitutersOf files paths env = ncSubstituters <$> resolveWith files paths env
      fileMap = Map.mapKeys asciiOsPath . Map.fromList
      -- The same, over the cascade a 'ConfigLocations' computes.
      substitutersAt files locations env = Config.configFilePaths locations >>= \paths -> substitutersOf files paths env
      firstPathOf locations = listToMaybe <$> Config.configFilePaths locations
      -- Successive texts as successive files, weakest first.
      resolveTexts texts =
        let paths = [asciiOsPath (root </> "src" </> (show index ++ ".conf")) | index <- [1 .. length texts]]
         in resolveWith (Map.fromList (zip paths texts)) paths Nothing
      subs = fmap ncSubstituters . resolveTexts
      keys = fmap ncTrustedPublicKeys . resolveTexts
      fallback = fmap ncFallback . resolveTexts
      keepFailed = fmap ncKeepFailed . resolveTexts
      alwaysAllowSubstitutes = fmap ncAlwaysAllowSubstitutes . resolveTexts
      uris = fmap ncAllowedUris . resolveTexts
      cascade =
        Config.ConfigLocations
          { Config.clConfDir = Nothing,
            Config.clSystemConfDir = Just (asciiOsPath etcNix),
            Config.clUserConfFiles = Nothing,
            Config.clConfigHome = asciiOsPath configHome,
            Config.clConfigDirs = map asciiOsPath [etcXdg, etcXdgLocal]
          }
      cascadeFiles =
        fileMap
          [ (systemConf, "substituters = https://system"),
            (xdgConf etcXdgLocal, "substituters = https://xdg-local"),
            (xdgConf etcXdg, "substituters = https://xdg"),
            (xdgConf configHome, "substituters = https://home")
          ]
      listVar = BS8.pack . intercalate [searchPathSeparator]
      depth = fmap ncMaxCallDepth . resolveTexts
      depthOf value = depth ["max-call-depth = " <> value]
      refused = either (const True) (const False) . depthOf
      cores = fmap ncCores . resolveTexts
      -- Bytes with no UTF-8 reading (0xE9, Latin-1's e-acute).  They are
      -- a host path on POSIX and none on Windows, whose paths are UTF-16,
      -- so the cases holding them branch on whether they spell one.
      latin1 = "caf\xE9" :: BS.ByteString
      -- What a lossy decode would open instead: U+FFFD's UTF-8.
      replaced = "caf\xEF\xBF\xBD"
      nixConfName = asciiOsPath "nix.conf"
  sequence
    [ runTest "parses a whitespace-split substituters list" $
        assertEqual "subs" (Right ["https://a", "https://b"]) (subs ["substituters = https://a https://b"]),
      runTest "a comment truncates the line" $
        assertEqual "comment" (Right ["https://a"]) (subs ["substituters = https://a # not https://b"]),
      runTest "a plain assignment replaces across sources" $
        assertEqual "replace" (Right ["https://b"]) (subs ["substituters = https://a", "substituters = https://b"]),
      runTest "extra- appends within a source set" $
        assertEqual "append" (Right ["https://a", "https://b"]) (subs ["substituters = https://a", "extra-substituters = https://b"]),
      runTest "binary-caches aliases substituters" $
        assertEqual "alias" (Right ["https://a"]) (subs ["binary-caches = https://a"]),
      runTest "extra- composes with an alias" $
        assertEqual "extra-alias" (Right ["https://a", "https://b"]) (subs ["substituters = https://a", "extra-binary-caches = https://b"]),
      runTest "binary-cache-public-keys aliases trusted-public-keys" $
        assertEqual "key-alias" (Right ["k1"]) (keys ["binary-cache-public-keys = k1"]),
      runTest "a higher source plainly replaces the trusted set" $
        assertEqual "trust-replace" (Right ["k2"]) (keys ["trusted-public-keys = k1", "trusted-public-keys = k2"]),
      runTest "a higher source with extra- widens the trusted set" $
        assertEqual "trust-widen" (Right ["k1", "k2"]) (keys ["trusted-public-keys = k1", "extra-trusted-public-keys = k2"]),
      runTest "fallback is a Boolean, off by default, and build-fallback aliases it" $
        assertEqual "default" (Right False) (fallback [])
          `andThen` assertEqual "on" (Right True) (fallback ["fallback = true"])
          `andThen` assertEqual "alias" (Right True) (fallback ["build-fallback = yes"])
          `andThen` assertEqual "replaced" (Right False) (fallback ["fallback = true", "fallback = false"])
          `andThen` assertEqual "typo" (Left "Boolean setting 'fallback' has invalid value 'ture'") (fallback ["fallback = ture"]),
      runTest "an invalid build-fallback is refused naming fallback, as upstream's error names the setting" $
        assertEqual "alias-typo" (Left "Boolean setting 'fallback' has invalid value 'maybe'") (fallback ["build-fallback = maybe"]),
      runTest "keep-failed is a Boolean, off by default" $
        assertEqual "default" (Right False) (keepFailed [])
          `andThen` assertEqual "on" (Right True) (keepFailed ["keep-failed = yes"])
          `andThen` assertEqual "replaced" (Right False) (keepFailed ["keep-failed = true", "keep-failed = false"])
          `andThen` assertEqual "typo" (Left "Boolean setting 'keep-failed' has invalid value 'maybe'") (keepFailed ["keep-failed = maybe"]),
      runTest "always-allow-substitutes is a Boolean, off by default" $
        assertEqual "default" (Right False) (alwaysAllowSubstitutes [])
          `andThen` assertEqual "on" (Right True) (alwaysAllowSubstitutes ["always-allow-substitutes = true"])
          `andThen` assertEqual "replaced" (Right False) (alwaysAllowSubstitutes ["always-allow-substitutes = true", "always-allow-substitutes = no"])
          `andThen` assertEqual
            "typo"
            (Left "Boolean setting 'always-allow-substitutes' has invalid value 'ture'")
            (alwaysAllowSubstitutes ["always-allow-substitutes = ture"]),
      runTest "an unknown setting is ignored, not an error" $
        assertEqual "unknown" (Right ["https://a"]) (subs ["cores = 4\nsubstituters = https://a"]),
      runTest "blank and comment-only lines are skipped" $
        assertEqual "blanks" (Right ["https://a"]) (subs ["# a comment\n\nsubstituters = https://a\n"]),
      runTest "a line missing = is a syntax error quoting the line as truncated, untrimmed" $
        let expected = "nix.conf: syntax error in configuration line '   substituters https://a ' in '" <> T.pack (root </> "src" </> "1.conf") <> "'"
         in assertEqual "syntax" (Left expected) (resolveTexts ["   substituters https://a # comment"]),
      runTest "the default config is empty" $
        assertEqual "default" (Right []) (subs []),
      -- Tokens, as upstream's tokenizeString splits them: on space, tab,
      -- CR and LF only, which nix config show 2.33.2 agrees with.
      runTest "a value splits on space, tab and CR, and not on other spaces" $
        assertEqual
          "separators"
          (Right ["https://a\xA0https://b\vhttps://c\fhttps://d", "https://e", "https://f", "https://g"])
          (uris ["allowed-uris = https://a\xC2\xA0https://b\vhttps://c\fhttps://d https://e\rhttps://f\thttps://g"]),
      runTest "a CRLF line ends where its LF does" $
        assertEqual "crlf" (Right ["https://a"]) (subs ["substituters = https://a\r\n"]),
      runTest "a Boolean value ending in a no-break space is upstream's invalid-value error" $
        assertEqual "nbsp-bool" (Left "Boolean setting 'pure-eval' has invalid value 'true\xA0'") (ncPureEval <$> resolveTexts ["pure-eval = true\xC2\xA0"]),
      -- Bytes with no UTF-8 reading in a setting.
      runTest "a comment and an unknown setting holding a byte with no UTF-8 reading are never decoded" $
        assertEqual "bytes-ignored" (Right ["https://a"]) (substitutersOf Map.empty [] (Just ("trusted-users = " <> latin1 <> "\nsubstituters = https://a # " <> latin1))),
      runTest "a list setting holding a byte with no UTF-8 reading is refused under its own name" $
        let expected = "setting 'substituters' has invalid value 'https://caf\xFFFD.example': not valid UTF-8"
         in assertEqual "list-bytes" (Left expected) (substitutersOf Map.empty [] (Just ("extra-binary-caches = https://" <> latin1 <> ".example"))),
      runTest "a Boolean or integer setting holding such a byte is upstream's invalid-value error" $
        assertEqual
          "scalar-bytes"
          (Left "Boolean setting 'pure-eval' has invalid value 'true\xFFFD'", Left "setting 'max-call-depth' has invalid value '5\xFFFD'")
          (ncPureEval <$> resolveTexts ["pure-eval = true\xE9"], depthOf "5\xE9"),
      -- max-call-depth: a scalar under upstream's unsigned-integer grammar.
      runTest "max-call-depth defaults to upstream's 10000" $
        assertEqual "depth-default" (Right 10000) (depth []),
      runTest "max-call-depth parses a decimal" $
        assertEqual "depth-decimal" (Right 5) (depthOf "5"),
      runTest "a higher source replaces max-call-depth" $
        assertEqual "depth-replace" (Right 7) (depth ["max-call-depth = 5", "max-call-depth = 7"]),
      runTest "max-call-depth takes a binary unit suffix in either case" $
        assertEqual "depth-unit" (Right [10240, 5120, 1048576]) (traverse depthOf ["10K", "5k", "1M"]),
      runTest "max-call-depth takes a leading plus and leading zeros" $
        assertEqual "depth-plus" (Right [5, 7]) (traverse depthOf ["+5", "007"]),
      runTest "max-call-depth accepts the top of unsigned int" $
        assertEqual "depth-top" (Right 4294967295) (depthOf "4294967295"),
      runTest "an invalid max-call-depth is refused with upstream's words" $
        assertEqual "depth-invalid" (Left "setting 'max-call-depth' has invalid value 'abc'") (depthOf "abc"),
      runTest "max-call-depth refuses a sign, a fraction, hex, an unknown unit, an empty value and a second token" $
        assertEqual "depth-refused" (True, 6) (all refused rejects, length rejects),
      runTest "max-call-depth refuses a value past unsigned int, a unit product included" $
        assertEqual "depth-range" True (refused "4294967296" && refused "5G"),
      runTest "extra-max-call-depth is an unknown name, not an append" $
        assertEqual "depth-extra" (Right 10000) (depth ["extra-max-call-depth = 5"]),
      -- cores: a scalar under the same grammar, unset until a source sets
      -- it, since its default is the machine's.
      runTest "cores is unset when no source sets it" $
        assertEqual "cores-unset" (Right Nothing) (cores []),
      runTest "cores and its alias build-cores set it, 0 included" $
        assertEqual "cores-set" (Right [Just 4, Just 6, Just 0, Just 2048]) (traverse (cores . pure) ["cores = 4", "build-cores = 6", "cores = 0", "cores = 2K"]),
      runTest "a higher source replaces cores, whichever name each uses" $
        assertEqual "cores-replace" (Right (Just 2)) (cores ["cores = 8", "build-cores = 2"]),
      runTest "an invalid cores is refused in upstream's words, which name the setting and not the alias" $
        assertEqual
          "cores-invalid"
          (Left "setting 'cores' has invalid value 'all'", Left "setting 'cores' has invalid value '-1'")
          (cores ["cores = all"], cores ["build-cores = -1"]),
      runTest "extra-cores and extra-build-cores are unknown names, not appends" $
        assertEqual "cores-extra" (Right Nothing) (cores ["extra-cores = 4", "extra-build-cores = 4"]),
      -- Includes.
      runTest "an include is spliced in at its line, resolved against the including file's directory" $
        let files =
              fileMap
                [ (systemConf, "substituters = https://a\ninclude site.conf\nextra-substituters = https://c"),
                  (etcNix </> "site.conf", "extra-substituters = https://b")
                ]
         in assertEqual "inline" (Right ["https://a", "https://b", "https://c"]) (substitutersOf files [asciiOsPath systemConf] Nothing),
      runTest "a nested include resolves against the nested file, with .. collapsed" $
        let files =
              fileMap
                [ (systemConf, "include sub/one.conf"),
                  (etcNix </> "sub" </> "one.conf", "include ../two.conf"),
                  (etcNix </> "two.conf", "substituters = https://two")
                ]
         in assertEqual "nested" (Right ["https://two"]) (substitutersOf files [asciiOsPath systemConf] Nothing),
      runTest "!include of a file that cannot be read is skipped" $
        let files = fileMap [(systemConf, "substituters = https://a\n!include missing.conf\nextra-substituters = https://b")]
         in assertEqual "optional" (Right ["https://a", "https://b"]) (substitutersOf files [asciiOsPath systemConf] Nothing),
      runTest "include of a missing file is an error in upstream's words, naming both files" $
        let files = fileMap [(systemConf, "include missing.conf")]
            expected = "nix.conf: file '" <> T.pack (etcNix </> "missing.conf") <> "' included from '" <> T.pack systemConf <> "' not found"
         in assertEqual "required" (Left expected) (substitutersOf files [asciiOsPath systemConf] Nothing),
      -- A file that exists but will not open: upstream drops it, here a
      -- required include of it is an error in its own words.
      runTest "include of a file that exists but cannot be read is an error, not a silent drop" $
        let locked = etcNix </> "locked.conf"
            readLocked path = Identity (if path == asciiOsPath locked then Left Config.ConfigFileUnreadable else Right "include locked.conf")
            expected = "nix.conf: file '" <> T.pack locked <> "' included from '" <> T.pack systemConf <> "' cannot be read"
         in assertEqual "unreadable" (Left expected) (ncSubstituters <$> runIdentity (Config.loadConfig readLocked [asciiOsPath systemConf] Nothing)),
      runTest "!include of a file that exists but cannot be read is skipped" $
        let locked = etcNix </> "locked.conf"
            readLocked path = Identity (if path == asciiOsPath locked then Left Config.ConfigFileUnreadable else Right "!include locked.conf\nsubstituters = https://a")
         in assertEqual "unreadable-optional" (Right ["https://a"]) (ncSubstituters <$> runIdentity (Config.loadConfig readLocked [asciiOsPath systemConf] Nothing)),
      runTest "an include cycle is refused rather than followed" $
        let files = fileMap [(systemConf, "include site.conf"), (etcNix </> "site.conf", "include nix.conf")]
         in assertLeft "cycle" (substitutersOf files [asciiOsPath systemConf] Nothing),
      runTest "an include with no path or two paths is a syntax error" $
        assertEqual "arity" [True, True, True] (map (either (const True) (const False) . resolveTexts . pure) ["include", "include a b", "!include"]),
      runTest "a relative include from NIX_CONFIG is refused as not absolute" $
        let expected = "nix.conf: not an absolute path: '" <> T.pack ("." </> "rel.conf") <> "'"
         in assertEqual "env-relative" (Left expected) (substitutersOf Map.empty [] (Just "include rel.conf")),
      runTest "an absolute include from NIX_CONFIG is expanded" $
        let files = fileMap [(etcNix </> "x.conf", "substituters = https://x")]
         in assertEqual "env-absolute" (Right ["https://x"]) (substitutersOf files [] (Just ("include " <> BS8.pack (etcNix </> "x.conf")))),
      -- An include target is bytes from the line to the open: the
      -- file its bytes name is read, never the one a lossy decode names.
      runTest "an include target with no UTF-8 reading opens the file its bytes name" $
        let target = latin1 <> ".conf"
            including = fileMap [(systemConf, "include " <> target)]
            decoy = maybe including (\name -> Map.insert (asciiOsPath etcNix OP.</> name) "substituters = https://replacement" including) (hostPathFromBytes (replaced <> ".conf"))
         in case hostPathFromBytes target of
              Just name ->
                let files = Map.insert (asciiOsPath etcNix OP.</> name) "substituters = https://latin1" decoy
                 in assertEqual "include-bytes" (Right ["https://latin1"]) (substitutersOf files [asciiOsPath systemConf] Nothing)
              Nothing ->
                let expected = "nix.conf: file 'caf\xFFFD.conf' included from '" <> T.pack systemConf <> "' is not valid UTF-8, so it names no Windows file"
                 in assertEqual "include-bytes-windows" (Left expected) (substitutersOf decoy [asciiOsPath systemConf] Nothing),
      runTest "!include of a target with no UTF-8 reading that names nothing goes on without it" $
        let files = fileMap [(systemConf, "!include " <> latin1 <> ".conf\nsubstituters = https://a")]
         in assertEqual "optional-bytes" (Right ["https://a"]) (substitutersOf files [asciiOsPath systemConf] Nothing),
      -- The directories a cascade names keep their bytes too, so the
      -- system file under such a NIX_CONF_DIR, and an include beside a
      -- file under such a config home, are the files those bytes name.
      runTest "NIX_CONF_DIR holding a byte with no UTF-8 reading names the system file by its bytes" $
        let confDir = BS8.pack (etcNix ++ "-") <> latin1
            located = cascade {Config.clConfDir = Just confDir, Config.clUserConfFiles = Just ""}
         in case hostPathFromBytes confDir of
              Just dir ->
                let files = Map.fromList [(dir OP.</> nixConfName, "include site.conf"), (dir OP.</> asciiOsPath "site.conf", "substituters = https://conf-dir")]
                 in assertEqual "conf-dir-bytes" (Right (Just (dir OP.</> nixConfName)), Right ["https://conf-dir"]) (firstPathOf located, substitutersAt files located Nothing)
              Nothing -> assertLeft "conf-dir-bytes-windows" (firstPathOf located),
      runTest "an include beside a user file under a config home holding such a byte resolves beneath it" $
        case hostPathFromBytes (BS8.pack (configHome ++ "-") <> latin1) of
          Just home ->
            let located = cascade {Config.clSystemConfDir = Nothing, Config.clConfigHome = home, Config.clConfigDirs = []}
                userDir = home OP.</> asciiOsPath "nix"
                files = Map.fromList [(userDir OP.</> nixConfName, "include more.conf"), (userDir OP.</> asciiOsPath "more.conf", "substituters = https://home-bytes")]
             in assertEqual "home-bytes" (Right ["https://home-bytes"]) (substitutersAt files located Nothing)
          Nothing -> assertEqual "home-bytes-windows" "mingw32" SI.os,
      -- The cascade: which files, in which order, and who wins.
      runTest "the cascade is the system file, XDG_CONFIG_DIRS back to front, then the config home" $
        assertEqual "order" (Right (map asciiOsPath [systemConf, xdgConf etcXdgLocal, xdgConf etcXdg, xdgConf configHome])) (Config.configFilePaths cascade),
      runTest "the config home beats XDG_CONFIG_DIRS beats the system file" $
        assertEqual "home-wins" (Right ["https://home"]) (substitutersAt cascadeFiles cascade Nothing),
      runTest "the first XDG_CONFIG_DIRS entry beats the later ones" $
        assertEqual "xdg-first" (Right ["https://xdg"]) (substitutersAt (Map.delete (asciiOsPath (xdgConf configHome)) cascadeFiles) cascade Nothing),
      runTest "a listed file that cannot be read is absent, not an error" $
        assertEqual "absent" (Right ["https://system"]) (substitutersAt (Map.filterWithKey (\path _ -> path == asciiOsPath systemConf) cascadeFiles) cascade Nothing),
      runTest "extra- in a stronger file widens what a weaker file set" $
        let files = fileMap [(systemConf, "trusted-public-keys = k1"), (xdgConf configHome, "extra-trusted-public-keys = k2")]
         in assertEqual "widen-across-files" (Right ["k1", "k2"]) (fmap ncTrustedPublicKeys (Config.configFilePaths cascade >>= \paths -> resolveWith files paths Nothing)),
      runTest "NIX_CONFIG beats every file" $
        assertEqual "env-wins" (Right ["https://env"]) (substitutersAt cascadeFiles cascade (Just "substituters = https://env")),
      runTest "NIX_USER_CONF_FILES replaces the XDG files, first listed strongest" $
        let listed = cascade {Config.clUserConfFiles = Just (listVar [etcNix </> "a.conf", etcNix </> "b.conf"])}
         in assertEqual "listed" (Right (map asciiOsPath [systemConf, etcNix </> "b.conf", etcNix </> "a.conf"])) (Config.configFilePaths listed),
      runTest "an empty NIX_USER_CONF_FILES names no user files" $
        assertEqual "none" (Right [asciiOsPath systemConf]) (Config.configFilePaths cascade {Config.clUserConfFiles = Just ""}),
      runTest "empty NIX_USER_CONF_FILES entries are dropped" $
        assertEqual "empties" (Right (map asciiOsPath [systemConf, etcNix </> "a.conf"])) (Config.configFilePaths cascade {Config.clUserConfFiles = Just (listVar ["", etcNix </> "a.conf", ""])}),
      runTest "NIX_CONF_DIR moves the system file" $
        assertEqual "conf-dir" (Right (Just (asciiOsPath (etcXdg </> "nix.conf")))) (firstPathOf cascade {Config.clConfDir = Just (BS8.pack etcXdg)}),
      runTest "an empty NIX_CONF_DIR is unset" $
        assertEqual "conf-dir-empty" (Right (Just (asciiOsPath systemConf))) (firstPathOf cascade {Config.clConfDir = Just ""}),
      runTest "a relative NIX_CONF_DIR is refused with upstream's complaint" $
        assertEqual "conf-dir-relative" (Left "nix.conf: not an absolute path: 'rel'") (firstPathOf cascade {Config.clConfDir = Just "rel"}),
      runTest "dot and dot-dot in NIX_CONF_DIR collapse, so the system file is named canonically" $
        assertEqual "conf-dir-canonical" (Right (Just (asciiOsPath systemConf))) (firstPathOf cascade {Config.clConfDir = Just (BS8.pack (root </> "etc" </> "x" </> ".." </> "." </> "nix"))}),
      -- A UNC directory keeps its server on Windows, where System.FilePath
      -- and std::filesystem both read it as the drive; a doubled POSIX
      -- root is one root, as upstream's canonPath folds it.
      runTest "a UNC NIX_CONF_DIR keeps its root, and its include resolves beneath it" $
        let (dir, expected) =
              if SI.os == "mingw32"
                then ("\\\\server\\share", "\\\\server\\share\\nix.conf")
                else ("//server/share", "/server/share/nix.conf")
            uncConf = takeDirectory expected </> "site.conf"
            files = fileMap [(expected, "include site.conf"), (uncConf, "substituters = https://unc")]
            unc = cascade {Config.clConfDir = Just (BS8.pack dir), Config.clUserConfFiles = Just ""}
         in assertEqual "unc" (Right (Just (asciiOsPath expected)), Right ["https://unc"]) (firstPathOf unc, substitutersAt files unc Nothing),
      runTest "a platform with no machine-wide directory has no system file" $
        assertEqual "no-system-dir" (Right (map asciiOsPath [xdgConf etcXdgLocal, xdgConf etcXdg, xdgConf configHome])) (Config.configFilePaths cascade {Config.clSystemConfDir = Nothing}),
      runTest "NIX_CONF_DIR names a system file even where the platform has no directory for one" $
        assertEqual "conf-dir-no-default" (Right (Just (asciiOsPath (etcXdg </> "nix.conf")))) (firstPathOf cascade {Config.clSystemConfDir = Nothing, Config.clConfDir = Just (BS8.pack etcXdg)})
    ]
  where
    rejects = ["-1", "1.5", "0x10", "5X", "", "5 6"]

-- ---------------------------------------------------------------------------
-- Tests: evaluation policy - restricted and pure modes
-- ---------------------------------------------------------------------------

-- | The flags and settings through the binary: the modes and
-- @trace-verbose@ select themselves from the command line and from
-- NIX_CONFIG, and a refusal surfaces as the eval error it is.  Under
-- @trace-verbose@, @builtins.traceVerbose@ is upstream's @prim_trace@
-- (primops.cc at 2.24.9).
testEvalPolicyCLI :: IO [Bool]
testEvalPolicyCLI = do
  putStrLn "cli/eval-policy"
  tmpBase <- getTemporaryDirectory >>= Dir.canonicalizePath
  ambient <- getEnvironment
  cwd <- Dir.getCurrentDirectory
  let root = tmpBase </> "nova-nix-test-cli-policy"
      -- Where a channel: entry would land if it were taken for a path.
      cwdChannelEntry = T.unpack (canonPathValue (T.pack (cwd </> "channel:nova-probe")))
      allowed = root </> "allowed"
      -- A string, since a drive-letter path (C:/...) is a URI token and
      -- a native spelling (C:\...) is no token at all; an absolute
      -- string is coerced to a path on every platform.
      outsideFile = T.unpack (canonPathValue (T.pack (root </> "outside.txt")))
      helpFirstLine = maybe "" T.unpack (listToMaybe Ref.helpLines)
      isolate extraConfig =
        ("NIX_CONFIG", "substituters =\ntrusted-public-keys =\n" <> extraConfig)
          : ("XDG_CONFIG_HOME", root </> "config")
          : filter (\(key, _) -> key `notElem` ["NIX_CONFIG", "XDG_CONFIG_HOME", "NIX_PATH"]) ambient
      runCLI extraConfig args =
        Proc.readCreateProcessWithExitCode
          ((Proc.proc "cabal" (["run", "-v0", "exe:nova-nix", "--"] ++ args)) {Proc.env = Just (isolate extraConfig)})
          ""
      expectOut label extraConfig args expected =
        runTestM ("CLI " <> label) $ do
          (code, out, err) <- runCLI extraConfig args
          pure $ case (code, lines out) of
            (ExitSuccess, line : _) -> assertEqual label expected line
            _ -> Fail ("CLI: " <> T.pack (show code) <> "; stdout=" <> T.pack out <> "; stderr=" <> T.pack err)
      expectErr label extraConfig args needle =
        runTestM ("CLI " <> label) $ do
          (code, _, err) <- runCLI extraConfig args
          pure $
            if code /= ExitSuccess && needle `T.isInfixOf` T.pack err
              then Pass
              else Fail ("expected a failure mentioning " <> needle <> ", got " <> T.pack (show (code, err)))
      -- The whole run: the exit code, then stdout and stderr line by line.
      expectRun label extraConfig args expected =
        runTestM ("CLI " <> label) $ do
          (code, out, err) <- runCLI extraConfig args
          pure (assertEqual label expected (code, lines out, lines err))
      traceVerboseHi = ["eval", "--expr", "builtins.traceVerbose \"hi\" 1"]
  bracket_
    ( do
        forceRemoveIfExists root
        createDirectoryIfMissing True allowed
        BS.writeFile (allowed </> "data.txt") "hello\n"
        BS.writeFile (root </> "outside.txt") "no\n"
    )
    (forceRemoveIfExists root)
    $ sequence
      [ expectOut "--pure-eval hides currentTime" "" ["eval", "--pure-eval", "--expr", "builtins ? currentTime"] "false",
        expectOut "--restrict-eval empties getEnv" "" ["eval", "--restrict-eval", "--expr", "builtins.getEnv \"HOME\""] "\"\"",
        expectOut "pure-eval from NIX_CONFIG" "pure-eval = true\n" ["eval", "--expr", "builtins ? currentSystem"] "false",
        expectOut "a --nix-path root is readable under --restrict-eval" "" ["eval", "--restrict-eval", "--nix-path", "allowed=" ++ allowed, "--expr", "builtins.readFile <allowed/data.txt>"] "\"hello\\n\"",
        expectErr "a read outside the roots fails the command" "" ["eval", "--restrict-eval", "--expr", "builtins.readFile \"" ++ outsideFile ++ "\""] "is forbidden in restricted mode",
        expectErr "a channel: search path entry allows nothing" "" ["eval", "--restrict-eval", "--nix-path", "nixpkgs=channel:nova-probe", "--expr", "builtins.readFile \"" ++ cwdChannelEntry ++ "\""] "is forbidden in restricted mode",
        expectOut "build accepts the flag before its target" "" ["build", "--pure-eval", "--help"] helpFirstLine,
        expectRun "traceVerbose prints nothing by default" "" traceVerboseHi (ExitSuccess, ["1"], []),
        expectRun "--trace-verbose makes traceVerbose trace" "" ("--trace-verbose" : traceVerboseHi) (ExitSuccess, ["1"], ["trace: hi"]),
        expectRun "trace-verbose from NIX_CONFIG makes traceVerbose trace" "trace-verbose = true\n" traceVerboseHi (ExitSuccess, ["1"], ["trace: hi"]),
        expectRun "under --trace-verbose traceVerbose forces its first argument" "" ["eval", "--trace-verbose", "--expr", "builtins.traceVerbose (throw \"a\") 1"] (ExitFailure 1, [], ["error: a"]),
        expectRun "under --trace-verbose traceVerbose prints before its second argument fails" "" ["eval", "--trace-verbose", "--expr", "builtins.traceVerbose \"hi\" (throw \"b\")"] (ExitFailure 1, [], ["trace: hi", "error: b"]),
        -- prim_trace runs only once it has both arguments (callFunction,
        -- eval.cc at 2.24.9), so a partial application forces nothing.
        expectRun "under --trace-verbose a partial traceVerbose leaves its argument unforced" "" ["eval", "--trace-verbose", "--expr", "builtins.seq (builtins.traceVerbose (throw \"a\")) 1"] (ExitSuccess, ["1"], []),
        expectOut "build accepts --trace-verbose before its target" "" ["build", "--trace-verbose", "--help"] helpFirstLine,
        expectOut "build accepts --no-allow-import-from-derivation before its target" "" ["build", "--no-allow-import-from-derivation", "--help"] helpFirstLine,
        expectOut "trust accepts --no-allow-import-from-derivation before its target" "" ["trust", "--no-allow-import-from-derivation", "--help"] helpFirstLine,
        expectOut "eval accepts --no-allow-import-from-derivation" "" ["eval", "--no-allow-import-from-derivation", "--expr", "1"] "1",
        -- A flag only moves a setting from its default, so none turns an
        -- import from derivation the configuration refuses back on.
        expectErr "there is no --allow-import-from-derivation to undo the configuration's refusal" "" ["eval", "--allow-import-from-derivation", "--expr", "1"] "unknown eval flag"
      ]

-- | The eval result through the binary: stdout carries the printer's bytes
-- and a bare newline, with no text encoding or newline translation between.
testResultPrinterCLI :: IO [Bool]
testResultPrinterCLI = do
  putStrLn "cli/eval-result"
  tmpBase <- getTemporaryDirectory
  ambient <- getEnvironment
  let root = tmpBase </> "nova-nix-test-cli-result"
      file = root </> "latin1"
      notUtf8 = "a" <> BS.singleton 0xfc <> "b"
      -- A string, as in testEvalPolicyCLI: a drive-letter path is no
      -- path literal.
      fileString = T.unpack (canonPathValue (T.pack file))
      isolated =
        ("XDG_CONFIG_HOME", root </> "config")
          : filter (\(key, _) -> key `notElem` ["NIX_CONFIG", "XDG_CONFIG_HOME"]) ambient
      cliProcess args = (Proc.proc "cabal" (["run", "-v0", "exe:nova-nix", "--"] ++ args)) {Proc.env = Just isolated, Proc.std_out = Proc.CreatePipe}
      printsThrough label args expected = runTestM label (Proc.withCreateProcess (cliProcess args) (checkPrinted label expected))
      checkPrinted label expected _ stdoutPipe _ process = case stdoutPipe of
        Nothing -> pure (Fail "no stdout pipe")
        Just out -> do
          printed <- BS.hGetContents out
          code <- Proc.waitForProcess process
          pure $ case code of
            ExitSuccess -> assertEqual label expected printed
            ExitFailure _ -> Fail ("CLI: " <> T.pack (show code))
      -- The binary's heap is capped for a derivation, which holds itself:
      -- an eval that forgets what it has entered then exhausts the cap
      -- and fails, where it would run until the machine had no memory
      -- left.  The eval needs a few megabytes.
      heapCapped args = ["+RTS", "-M64m", "-RTS"] ++ args
  bracket_
    (forceRemoveIfExists root >> createDirectoryIfMissing True root >> BS.writeFile file notUtf8)
    (forceRemoveIfExists root)
    $ sequence
      [ printsThrough "CLI eval writes a string's bytes and a bare newline" ["eval", "--expr", "builtins.readFile \"" ++ fileString ++ "\""] ("\"" <> notUtf8 <> "\"\n"),
        printsThrough
          "CLI eval prints a derivation once and its output as repeated"
          (heapCapped ["eval", "--expr", T.unpack selfHoldingDerivation])
          (TE.encodeUtf8 selfHoldingPrinted <> "\n"),
        printsThrough
          "CLI eval --strict prints a derivation, its outputs in all and out as repeated"
          (heapCapped ["eval", "--strict", "--expr", T.unpack selfHoldingDerivation])
          (TE.encodeUtf8 selfHoldingPrintedStrict <> "\n")
      ]

-- | nix.conf bytes through the binary: an include target, the
-- system directory and NIX_CONFIG reach the file system and the settings
-- as the bytes they hold.  Each case reads whether pure-eval came on,
-- which hides currentSystem.  A name that is not UTF-8 exists only on a
-- volume that holds one, so a case needing such a name makes it first
-- and skips, with the reason, where the temp volume refuses it: APFS
-- refuses the name, and HFS+ holds it but opens no path beneath a
-- directory so named.  A Windows name or variable is UTF-16 and holds no
-- such byte at all.  The decoy cases need no such name: a file named by
-- U+FFFD's UTF-8, which a lossy decode of the byte would open instead,
-- must stay unread, as it does under nix-instantiate 2.33.2.
testConfigBytesCLI :: IO [Bool]
testConfigBytesCLI = do
  putStrLn "cli/config-bytes"
  tmpBase <- getTemporaryDirectory
  ambient <- getEnvironment
  let root = tmpBase </> "nova-nix-test-cli-config-bytes"
      confDir = root </> "etc"
      noConfDir = root </> "none"
      configVars = ["NIX_CONFIG", "NIX_CONF_DIR", "NIX_USER_CONF_FILES", "XDG_CONFIG_DIRS"]
      -- An empty NIX_USER_CONF_FILES names no user files, so only the
      -- system file under NIX_CONF_DIR and NIX_CONFIG are read.
      runCLI dir nixConfig =
        Proc.readCreateProcessWithExitCode
          ( (Proc.proc "cabal" ["run", "-v0", "exe:nova-nix", "--", "eval", "--expr", "builtins ? currentSystem"])
              { Proc.env = Just (("NIX_CONF_DIR", dir) : ("NIX_USER_CONF_FILES", "") : ("NIX_CONFIG", nixConfig) : filter ((`notElem` configVars) . fst) ambient)
              }
          )
          ""
      expectAnswer label expected dir nixConfig = do
        (code, out, err) <- runCLI dir nixConfig
        pure $ case (code, lines out) of
          (ExitSuccess, line : _) -> assertEqual label expected line
          _ -> Fail ("CLI: " <> T.pack (show code) <> "; stdout=" <> T.pack out <> "; stderr=" <> T.pack err)
      expectPure label = expectAnswer label "false"
      expectImpure label = expectAnswer label "true"
      expectRefusal nixConfig needle = do
        (code, _, err) <- runCLI noConfDir nixConfig
        pure $
          if code /= ExitSuccess && needle `T.isInfixOf` T.pack err
            then Pass
            else Fail ("expected a failure mentioning " <> needle <> ", got " <> T.pack (show (code, err)))
      -- A case whose name or value holds bytes, spelled as the
      -- platform's strings carry them.
      spelledCase label bytes check = do
        spelled <- envSpelling bytes
        case spelled of
          Nothing -> True <$ putStrLn ("  SKIP  " <> T.unpack label <> ": a Windows name or variable cannot hold a byte with no UTF-8 reading")
          Just spelling -> check spelling
      envCase label bytes check = spelledCase label bytes (runTestM label . check)
      -- A case over a file whose name holds bytes, made before the case
      -- runs and skipped where the temp volume refuses it.
      -- A decoy's name is UTF-8, which every volume holds.
      decoyCase label bytes make check = spelledCase label bytes $ \name -> make name >> runTestM label (check name)
      fileCase label bytes make check = spelledCase label bytes $ \name -> do
        made <- try (make name)
        case made of
          -- The error string alone: the path in the full message holds
          -- the very byte stdout's encoding cannot write.
          Left (err :: IOException) -> True <$ putStrLn ("  SKIP  " <> T.unpack label <> ": the temp volume cannot hold a name that is not UTF-8 (" <> ioeGetErrorString err <> ")")
          Right () -> runTestM label (check name)
      latin1 = "caf\xE9" :: BS.ByteString
      -- The name a lossy decode of latin1 spells: U+FFFD's UTF-8.
      decoy = "caf\xEF\xBF\xBD" :: BS.ByteString
      decoyDir = root </> "decoy"
  bracket_
    (forceRemoveIfExists root >> createDirectoryIfMissing True confDir >> createDirectoryIfMissing True decoyDir)
    (forceRemoveIfExists root)
    $ sequence
      [ fileCase
          "CLI an include target with no UTF-8 reading opens the file it names"
          (latin1 <> ".conf")
          ( \name -> do
              BS.writeFile (confDir </> name) "pure-eval = true\n"
              BS.writeFile (confDir </> "nix.conf") ("include " <> latin1 <> ".conf\n")
          )
          (const (expectPure "include-bytes" confDir "")),
        fileCase
          "CLI a NIX_CONF_DIR holding a byte with no UTF-8 reading names the system file"
          latin1
          ( \name -> do
              createDirectoryIfMissing True (root </> name)
              BS.writeFile (root </> name </> "nix.conf") "pure-eval = true\n"
          )
          (\name -> expectPure "conf-dir-bytes" (root </> name) ""),
        decoyCase
          "CLI an !include target with no UTF-8 reading leaves the U+FFFD-named file unread"
          (decoy <> ".conf")
          ( \name -> do
              BS.writeFile (decoyDir </> name) "pure-eval = true\n"
              BS.writeFile (decoyDir </> "nix.conf") ("!include " <> latin1 <> ".conf\n")
          )
          (const (expectImpure "include-decoy" decoyDir "")),
        spelledCase "CLI a NIX_CONF_DIR holding such a byte leaves the U+FFFD-named directory unread" latin1 $ \confName ->
          decoyCase
            "CLI a NIX_CONF_DIR holding such a byte leaves the U+FFFD-named directory unread"
            decoy
            ( \name -> do
                createDirectoryIfMissing True (decoyDir </> name)
                BS.writeFile (decoyDir </> name </> "nix.conf") "pure-eval = true\n"
            )
            (const (expectImpure "conf-dir-decoy" (decoyDir </> confName) "")),
        envCase
          "CLI NIX_CONFIG holding such a byte in a comment and an unknown setting applies the rest"
          ("trusted-users = " <> latin1 <> "\npure-eval = true # " <> latin1 <> "\n")
          (expectPure "env-bytes-ignored" noConfDir),
        envCase
          "CLI NIX_CONFIG holding such a byte in a list setting is refused"
          ("pure-eval = true\nextra-substituters = https://" <> latin1 <> ".example\n")
          (`expectRefusal` "setting 'substituters' has invalid value"),
        envCase
          "CLI a Boolean ending in a no-break space is invalid, as upstream splits the line"
          "pure-eval = true\xC2\xA0\n"
          (`expectRefusal` "Boolean setting 'pure-eval' has invalid value")
      ]
