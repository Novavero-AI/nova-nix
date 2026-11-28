{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests of the package set, and that the CI workflows spell each value
-- they share one way.
module Test.Pkgs
  ( testPkgsFetchurl,
    testStage0Pe32Chain,
    testMesWindows,
    testTinyccWindows,
    testGnumakeWindows,
    testBusyboxWindows,
    testChecksForbidSubstitutes,
    testSeedStdenvWindows,
    testBinutilsWindows,
    testGccCoreWindows,
    testGcc46Windows,
    testGcc46CxxWindows,
    testGcc10Windows,
    testMingwW64LatestWindows,
    testGccLatestWindows,
    testBinutilsX8664Windows,
    testGccCoreX8664Windows,
    testMingwW64CrtX8664Windows,
    testGccX8664Windows,
    testBinutilsNativeX8664Windows,
    testGccNativeX8664Windows,
    testStdenvWindows,
    testWindowsSources,
    testWindowsRungs,
    testWorkflowSpellings,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (IOException, displayException, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Functor ((<&>))
import Data.IORef (readIORef)
import Data.List (mapAccumL, nub, sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, mapMaybe, maybeToList)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Nix.Builder.Unpack (builtinUnpackBuilder)
import Nix.Builtins (parseNixPath)
import Nix.Derivation (Derivation (..), DerivationOutput (..), Platform (..))
import Nix.Eval (MonadEval (..), NixValue (..), evalParsed, mkStr, typeName, unrestrictedPolicy)
import Nix.Eval.IO (EvalState (..), evalBaseEnv, newEvalStateWith, runEvalIO)
import Nix.Hash (hashPlaceholder, sha256Digest)
import qualified Nix.Hash as Hash
import Nix.Parser (parseNix)
import Nix.Store.Path (StoreWriteMode (..), defaultStoreDir, defaultStoreDirText, parseStorePath, storePathToFilePath, storePathToText)
import Nix.Store.Path.Internal (StorePath (..))
import qualified NovaCache.NAR as NAR
import qualified System.Directory as Dir
import System.FilePath ((</>))
import Test.Harness (TestResult (..), assertEqual, bytesText, evalNixIO, evalNixIOStoreDir, nixQuotedPath, runTest, runTestIO, runTestM, sessionClosure, testLogger, testOrigin, testSessionStore)
import Text.Read (readMaybe)

-- ---------------------------------------------------------------------------
-- Tests: NAR and hash known-answer vectors
-- ---------------------------------------------------------------------------

-- | @pkgs/windows/fetchurl.nix@ evaluated from the checkout.  The
-- @mirror://@ expansion is what a source derivation records, so a
-- mirrors.nix refresh or a regex edit moves every drvPath built on it;
-- these pin the list against nixpkgs' order and the eval-time errors.
testPkgsFetchurl :: IO [Bool]
testPkgsFetchurl = do
  putStrLn "pkgs/fetchurl"
  cwd <- Dir.getCurrentDirectory
  let fetchurl = "(import " <> nixQuotedPath (cwd </> "pkgs" </> "windows" </> "fetchurl.nix") <> ")"
      urlsOf source = "builtins.concatStringsSep \"\\n\" (" <> fetchurl <> " { " <> source <> " sha256 = \"" <> hash <> "\"; }).urls"
      failsWith expected result = case result of
        Left err
          | expected `T.isInfixOf` err -> Pass
          | otherwise -> Fail ("expected an error naming " <> expected <> ", got: " <> err)
        Right val -> Fail ("expected an eval error, got: " <> T.pack (show val))
  sequence
    [ runTestIO
        "mirror://gnu expands to the https hosts of mirrors.nix, in nixpkgs' order"
        cwd
        (urlsOf "url = \"mirror://gnu/hello/hello-2.12.3.tar.gz\";")
        (mkStr (T.intercalate "\n" (map (<> "hello/hello-2.12.3.tar.gz") gnuHosts))),
      runTestIO
        "a plain URL in urls passes through beside an expanded one"
        cwd
        (urlsOf "urls = [ \"https://example.invalid/sed-4.10.tar.xz\" \"mirror://gnu/sed/sed-4.10.tar.xz\" ];")
        (mkStr (T.intercalate "\n" ("https://example.invalid/sed-4.10.tar.xz" : map (<> "sed/sed-4.10.tar.xz") gnuHosts))),
      runTestM "an unknown mirror site is an eval error naming the recipe's URL" $
        failsWith "fetchurl: unknown mirror site 'nope' in 'mirror://nope/x'" <$> evalNixIO cwd (urlsOf "url = \"mirror://nope/x\";"),
      runTestM "a mirror URL with no path is an eval error" $
        failsWith "fetchurl: malformed mirror URL 'mirror://gnu'" <$> evalNixIO cwd (urlsOf "url = \"mirror://gnu\";")
    ]
  where
    hash :: Text
    hash = "0d5f60154382fee10b114a1c34e785d8b1f492073ae2d3a6f7b147687b366aa0"
    -- The gnu set of pkgs/windows/mirrors.nix without its ftp:// host.
    gnuHosts :: [Text]
    gnuHosts =
      [ "https://ftpmirror.gnu.org/",
        "https://ftp.nluug.nl/pub/gnu/",
        "https://mirrors.kernel.org/gnu/",
        "https://mirror.ibcp.fr/pub/gnu/",
        "https://mirror.dogado.de/gnu/",
        "https://mirror.tochlab.net/pub/gnu/",
        "https://ftp.gnu.org/pub/gnu/"
      ]

-- | @pkgs/windows@'s stage0-pe32 chain, read back as the build driver reads
-- it: every derivation reachable from the seed, the ten links and
-- mescc-tools-extra's thirteen programs.  The chain is full-source only if
-- nothing reaches it but the seed and the sources, so the graph's leaves are
-- exactly those six fetches, no derivation takes a plain input source, and
-- each of the thirty commands of stage0-pe32's @x86\/mescc-tools-mini.cmd@,
-- and each of the five steps of every mescc-tools-extra program, is a step run
-- by a program the graph builds.  Every program built from C, hex2-new and kaem
-- included, is those five steps with the arguments the .cmd gives kaem.  hex0
-- carries the seed's hash as its own, so the build itself checks that the
-- seed reproduces itself.
--
-- The bin directories and @test\/data\/run-command.nix@'s writeText and
-- runCommand builds add derivations kaem builds and no leaf, and read nothing
-- but the scripts evaluation writes.  Each bin directory's script copies the
-- programs its variables name to @bin\/\<name\>.exe@, runCommand runs its
-- script from those directories on a PATH joined with @;@, and a store path
-- in that script is spelled from @NIX_STORE@.
testStage0Pe32Chain :: IO [Bool]
testStage0Pe32Chain = do
  putStrLn "pkgs/stage0-pe32"
  cwd <- Dir.getCurrentDirectory
  storeDir <- evalNixIOStoreDir
  -- The package set's fetchurl hands a single URL to <nix/fetchurl.nix>,
  -- which resolves against the checkout's data directory as the installed
  -- binary resolves it against its own.
  searchPaths <- parseNixPath (TE.encodeUtf8 (T.pack (cwd </> "data")))
  st <- newEvalStateWith testLogger unrestrictedPolicy searchPaths (testSessionStore storeDir) storeDir cwd
  let chain = "(import " <> nixQuotedPath (cwd </> "pkgs" </> "windows") <> ").stage0-pe32"
      runCommandTests = "import " <> nixQuotedPath (cwd </> "test" </> "data" </> "run-command.nix")
      quoted name = "\"" <> name <> "\""
      source =
        T.concat
          [ "let chain = ",
            chain,
            "; runs = ",
            runCommandTests,
            "; in builtins.concatStringsSep \" \" (map (name: chain.${name}.drvPath) [ ",
            T.unwords (map quoted ("seed" : links ++ bins)),
            " ] ++ map (name: chain.mescc-tools-extra.${name}.drvPath) [ ",
            T.unwords (map quoted extras),
            " ] ++ map (name: runs.${name}.drvPath) [ ",
            T.unwords (map quoted runs),
            " ])"
          ]
      chainAttrs = ("seed" : links) ++ map extraAttr extras
  outcome <- case parseNix cwd testOrigin (TE.encodeUtf8 source) of
    Left err -> pure (Left (T.pack (show err)))
    Right parsed -> runEvalIO st $ do
      value <- evalParsed (pure (evalBaseEnv st)) parsed
      case value of
        VStr bytes _ -> do
          let rootOf = Map.fromList (zip (("seed" : links ++ bins) ++ map extraAttr extras ++ map runAttr runs) (T.words (bytesText bytes)))
          graph <- sessionClosure (mapMaybe (`Map.lookup` rootOf) chainAttrs)
          full <- sessionClosure (Map.elems rootOf)
          pure (rootOf, graph, full)
        other -> throwEvalError ("expected the drvPaths as one string, got " <> typeName other)
  writes <- readIORef (esStoreWriteCache st)
  case outcome of
    Left err -> sequence [runTest instantiates (Fail err)]
    Right (rootOf, graph, full) -> do
      let derivations = Map.elems graph
          fetches = filter ((== fetchurlBuilder) . drvBuilder) derivations
          unpacks = filter ((== builtinUnpackBuilder) . drvBuilder) derivations
          steps = filter (\drv -> drvBuilder drv `notElem` [fetchurlBuilder, builtinUnpackBuilder]) derivations
          leaves = filter (Map.null . drvInputDrvs) derivations
          nameOf drv = maybe "" bytesText (Map.lookup "name" (drvEnv drv))
          isFixedOutput drv = not (all (T.null . doHash) (drvOutputs drv))
          outOf drv = [TE.encodeUtf8 (storePathToText defaultStoreDir (doPath o)) | o <- drvOutputs drv, doName o == "out"]
          inputsOf drv = mapMaybe ((`Map.lookup` full) . storePathToText defaultStoreDir) (Map.keys (drvInputDrvs drv))
          builtBy attr = Map.lookup attr rootOf >>= (`Map.lookup` full)
          builderNamesOf drv = [nameOf input | input <- inputsOf drv, drvBuilder drv `elem` outOf input]
          fixedHashOf drv = [(doHashAlgo o, doHash o) | o <- drvOutputs drv, doName o == "out"]
          isStep drv = drvBuilder drv `notElem` [fetchurlBuilder, builtinUnpackBuilder]
          -- A program's steps, last first: each step after the compile has
          -- one input that is a step and not the program it runs.
          stepsBack drv = drv : maybe [] stepsBack (listToMaybe [input | input <- inputsOf drv, isStep input, drvBuilder drv `notElem` outOf input])
          -- An argument as the .cmd spells it: the step's own output, which
          -- the .drv holds as its placeholder until the build, as $out,
          -- another step's output by that step's name, and a file in an
          -- unpacked repository as <repo>/<path>.
          spell drv arg
            | bytesText arg == hashPlaceholder "out" = "$out"
            | otherwise =
                fromMaybe (bytesText arg) $
                  listToMaybe ([nameOf input | input <- inputsOf drv, arg `elem` outOf input] ++ mapMaybe (inRepository arg) unpacks)
          inRepository arg unpack = do
            root <- listToMaybe (outOf unpack)
            rest <- BS.stripPrefix (root <> "/") arg
            let repo = T.dropEnd 1 (fst (T.breakOnEnd "-" (nameOf unpack)))
            pure (repo <> "/" <> bytesText (BS.drop 1 (BS8.dropWhile (/= '/') rest)))
          command drv = (nameOf drv, builderNamesOf drv, map (spell drv) (drvArgs drv))
          -- What the bin directories and run-command.nix add to the chain.
          added = Map.elems (Map.difference full graph)
          fullLeaves = filter (Map.null . drvInputDrvs) (Map.elems full)
          variableOf name drv = maybe "" bytesText (Map.lookup name (drvEnv drv))
          outText attr = maybe "" bytesText (builtBy attr >>= listToMaybe . outOf)
          -- Each variable of a bin directory that names a program, with the
          -- name of the program's derivation.
          installs drv = sort [(var, nameOf input) | (var, value) <- Map.toList (drvEnv drv), var /= "builder", input <- inputsOf drv, value `elem` outOf input]
          wroteAsText path = fmap snd (Map.lookup (storePathToText defaultStoreDir path) writes) == Just WriteText
          runsWith drv = (bytesText (drvBuilder drv), map (spell drv) (drvArgs drv), variableOf "PATH" drv)
          scriptOf drv = listToMaybe [input | input <- inputsOf drv, nameOf input == nameOf drv <> "-builder"]
          textFile drv = (bytesText (drvBuilder drv), variableOf "PATH" drv, variableOf "passAsFile" drv, variableOf "text" drv, sort (map nameOf (inputsOf drv)))
          kaemExe = outText "kaem" <> "/bin/kaem.exe"
          extraPath = outText "mescc-tools-extra" <> "/bin"
          hello = "${NIX_STORE}/" <> T.drop (T.length defaultStoreDirText + 1) (outText (runAttr "hello"))
      sequence
        [ runTest instantiates $
            assertEqual "steps, unpacks, fetches" (95, 5, 6) (length steps, length unpacks, length fetches),
          runTest "no derivation of the chain takes a plain input source" $
            case [nameOf drv | drv <- derivations, not (null (drvInputSrcs drv))] of
              [] -> Pass
              named -> Fail ("input sources in: " <> T.intercalate ", " named),
          runTest "the chain's leaves are the seed and the five source archives, all fixed-output" $
            assertEqual
              "leaves"
              (fetchNames, True)
              ( sort (map nameOf leaves),
                all (\drv -> drvBuilder drv == fetchurlBuilder && isFixedOutput drv) leaves
              ),
          runTest "each source archive is unpacked from its fetch alone" $
            assertEqual "unpack inputs" (replicate 5 [fetchurlBuilder]) [map drvBuilder (inputsOf drv) | drv <- unpacks],
          runTest "every step is an i686-windows derivation" $
            case [nameOf drv | drv <- steps, drvPlatform drv /= I686_Windows] of
              [] -> Pass
              named -> Fail ("not i686-windows: " <> T.intercalate ", " named),
          runTest "each step is run by an input, the program its command runs" $
            assertEqual
              "file written, program run"
              (sort ([(written, [program]) | (written, program) <- commands] ++ [(written, program) | (_, built) <- extraPrograms, (written, program, _) <- built]))
              (sort [(nameOf drv, builderNamesOf drv) | drv <- steps]),
          runTest "each program built from C is the five steps that build kaem, with their arguments" $
            assertEqual
              "file written, program run, arguments"
              (cmdPrograms ++ extraPrograms)
              [(attr, maybe [] (map command . reverse . stepsBack) (builtBy attr)) | (attr, _) <- cmdPrograms ++ extraPrograms],
          runTest "hex0 is fixed-output to the seed's own hash" $
            case (fixedHashOf <$> builtBy "seed", fixedHashOf <$> builtBy "hex0") of
              (Just seedHash@[(_, digest)], Just hex0Hash)
                | not (T.null digest) -> assertEqual "hex0's output hash" seedHash hex0Hash
              other -> Fail ("expected the seed's fixed-output hash on hex0: " <> T.pack (show other)),
          runTest "the bin directories and run-command.nix add ten derivations" $
            assertEqual
              "derivations added"
              (sort ["kaem", "mescc-tools", "mescc-tools-extra", "hello.txt", "run-command-copies", "run-command-copies-builder", "run-command-fails", "run-command-fails-builder", "run-command-marks", "run-command-marks-builder"])
              (sort (map nameOf added)),
          runTest "with them the leaves are still the seed and the five source archives" $
            assertEqual "leaves" fetchNames (sort (map nameOf fullLeaves)),
          runTest "they read no input source but the scripts evaluation writes as text" $
            assertEqual
              "derivation and its sources, all written as text"
              ( [ ("hello.txt", ["write-text.kaem"]),
                  ("kaem", ["kaem-bin.kaem"]),
                  ("mescc-tools", ["mescc-tools-bin.kaem"]),
                  ("mescc-tools-extra", ["mescc-tools-extra-bin.kaem"]),
                  ("run-command-copies-builder", ["write-text.kaem"]),
                  ("run-command-fails-builder", ["write-text.kaem"]),
                  ("run-command-marks-builder", ["write-text.kaem"])
                ],
                True
              )
              ( sort [(nameOf drv, sort (map spName (drvInputSrcs drv))) | drv <- added, not (null (drvInputSrcs drv))],
                all (all wroteAsText . drvInputSrcs) added
              ),
          runTest "every derivation they add is i686-windows" $
            case [nameOf drv | drv <- added, drvPlatform drv /= I686_Windows] of
              [] -> Pass
              named -> Fail ("not i686-windows: " <> T.intercalate ", " named),
          runTest "kaem lays out each bin directory from the programs its variables name" $
            assertEqual
              "bin directory, program run, variable and program"
              [ ("kaem", ["kaem.exe"], sort [("chmod", "chmod.exe"), ("cp", "cp.exe"), ("kaem", "kaem.exe"), ("mkdir", "mkdir.exe")]),
                ("mescc-tools", ["kaem.exe"], sort [("M1", "M1.exe"), ("M2-Planet", "M2.exe"), ("chmod", "chmod.exe"), ("cp", "cp.exe"), ("hex2", "hex2-new.exe"), ("mkdir", "mkdir.exe")]),
                ("mescc-tools-extra", ["kaem.exe"], sort [(name, name <> ".exe") | name <- extras])
              ]
              [(attr, maybe [] builderNamesOf (builtBy attr), maybe [] installs (builtBy attr)) | attr <- bins],
          runTestM "each bin directory's script copies its programs to bin/<name>.exe and chmods each 555, and writeText's copies the text" $ do
            let scriptsOf drv = map (storePathToFilePath storeDir) (drvInputSrcs drv)
            scripts <- traverse (traverse BS.readFile . maybe [] scriptsOf . builtBy) (bins ++ [runAttr "hello"])
            pure $
              assertEqual
                "scripts"
                [[installScript ["kaem"]], [installScript ["M1", "M2-Planet", "hex2"]], [installScript extras], ["cp ${textPath} ${out}\n"]]
                (map (map bytesText) scripts),
          runTest "runCommand runs its script with kaem from the bin directories, on PATH joined with ';'" $
            assertEqual
              "builder, arguments, PATH"
              [ (kaemExe, ["--verbose", "--strict", "--file", name <> "-builder"], T.intercalate ";" [outText attr <> "/bin" | attr <- bins])
              | name <- ["run-command-copies", "run-command-fails", "run-command-marks"]
              ]
              (mapMaybe (fmap runsWith . builtBy . runAttr) ["copies", "fails", "marks"]),
          runTest "marks is fixed-output to the NAR hash of its tree with bin/hello executable" $
            assertEqual
              "output hash"
              (Just [("r:sha256", "d7b2412a32f4ff4be9340e6bbd33e316c64fdf38623ba4e76ed8af570dfaee9a")])
              (fixedHashOf <$> builtBy (runAttr "marks")),
          runTest "writeText passes its text as a file, and runCommand's script spells a store path from NIX_STORE" $
            assertEqual
              "builder, PATH, passAsFile, text and inputs"
              [ (kaemExe, extraPath, "text", "stage0-pe32 runs a kaem script\n", ["kaem", "mescc-tools-extra"]),
                ( kaemExe,
                  extraPath,
                  "text",
                  T.unlines ["mkdir -p ${out}/a/b", "cp " <> hello <> " ${out}/a/b/hello.txt", "catm ${out}/twice.txt " <> hello <> " " <> hello],
                  ["hello.txt", "kaem", "mescc-tools-extra"]
                )
              ]
              (map textFile (catMaybes [builtBy (runAttr "hello"), builtBy (runAttr "copies") >>= scriptOf]))
        ]
  where
    instantiates :: Text
    instantiates = "the chain instantiates: ninety-five steps, five unpacks and six fetches"
    fetchurlBuilder :: BS.ByteString
    fetchurlBuilder = "builtin:fetchurl"
    links :: [Text]
    links = ["hex0", "hex1", "hex2", "catm", "M0", "cc_x86", "M2", "M1", "hex2-new", "kaem-unwrapped"]
    extras :: [Text]
    extras = ["sha256sum", "match", "mkdir", "untar", "ungz", "unbz2", "unxz", "catm", "cp", "chmod", "rm", "replace", "wrap"]
    extraAttr :: Text -> Text
    extraAttr = ("mescc-tools-extra." <>)
    bins :: [Text]
    bins = ["kaem", "mescc-tools", "mescc-tools-extra"]
    runs :: [Text]
    runs = ["hello", "copies", "fails", "marks"]
    runAttr :: Text -> Text
    runAttr = ("run-command." <>)
    -- The script kaem runs to lay out a bin directory of the programs named.
    installScript :: [Text] -> Text
    installScript names =
      T.unlines
        ( "${mkdir} -p ${out}/bin"
            : concat
              [ ["${cp} ${" <> name <> "} ${out}/bin/" <> name <> ".exe", "${chmod} 555 ${out}/bin/" <> name <> ".exe"]
              | name <- sort names
              ]
        )
    fetchNames :: [Text]
    fetchNames = sort ["M2-Planet-ee0bf91.tar.gz", "M2libc-ef9dfc2.tar.gz", "hex0-seed.exe", "mescc-tools-578ac99.tar.gz", "mescc-tools-extra-d1d8146.tar.gz", "stage0-pe32-a81c837.tar.gz"]
    -- The steps that build a C program on M2libc's POSIX layer, named for
    -- stem: the file each writes, the program it runs and its arguments.
    cProgram :: Text -> Text -> [Text] -> [(Text, [Text], [Text])]
    cProgram executable stem files =
      [ (stem <> ".M1", ["M2.exe"], ["--architecture", "x86", "-D", "__windows__=1"] ++ concatMap (\file -> ["-f", file]) files ++ ["-o", "$out"]),
        (stem <> "-0.M1", ["catm.exe"], ["$out", "M2libc/x86/x86_defs.M1", "M2libc/x86/windows/libc-core.M1", "M2libc/x86/windows/libc-full.M1", stem <> ".M1", "M2libc/x86/windows/pe-end-shim.M1"]),
        (stem <> ".hex2", ["M0.exe"], [stem <> "-0.M1", "$out"]),
        (stem <> "-0.hex2", ["catm.exe"], ["$out", "M2libc/x86/windows/PE32-i386.hex2", "M2libc/x86/windows/ntdll-i386.hex2", stem <> ".hex2"]),
        (executable, ["hex2.exe"], [stem <> "-0.hex2", "$out"])
      ]
    -- Phase-7's M2libc files, which are what <stdio.h> reaches on the
    -- x86/windows target and bootstrappable.c, and Phase-8's, which add the
    -- string.c that <string.h> reaches.  catm is the one program here that
    -- does not include <string.h>.
    libcPosix :: [Text]
    libcPosix = map ("M2libc/" <>) (libcBelowString ++ libcAboveString)
    libcString :: [Text]
    libcString = map ("M2libc/" <>) (libcBelowString ++ ["string.c"] ++ libcAboveString)
    libcBelowString :: [Text]
    libcBelowString = ["sys/types.h", "stddef.h", "sys/utsname.h", "x86/windows/ntdll.c", "x86/windows/unistd.c", "x86/windows/process.c", "x86/windows/fcntl.c", "fcntl.c", "x86/windows/sys/stat.c", "ctype.c", "stdlib.c"]
    libcAboveString :: [Text]
    libcAboveString = ["stdarg.h", "stdio.h", "stdio.c", "bootstrappable.c"]
    -- The .cmd's two programs built from C and mescc-tools-extra's, by
    -- attribute, with the steps that build each.
    cmdPrograms :: [(Text, [(Text, [Text], [Text])])]
    cmdPrograms =
      [ ("hex2-new", cProgram "hex2-new.exe" "hex2_linker" (libcPosix ++ map ("mescc-tools/" <>) ["hex2.h", "hex2_linker.c", "hex2_word.c", "hex2.c"])),
        ("kaem-unwrapped", cProgram "kaem.exe" "kaem" (libcString ++ map ("mescc-tools/Kaem/" <>) ["kaem.h", "variable.c", "kaem_globals.c", "kaem.c"]))
      ]
    extraPrograms :: [(Text, [(Text, [Text], [Text])])]
    extraPrograms =
      [ (extraAttr name, cProgram (name <> ".exe") name ((if name == "catm" then libcPosix else libcString) ++ ["mescc-tools-extra/" <> name <> ".c"]))
      | name <- extras
      ]
    -- Each command of the .cmd: the file it writes and the program it runs.
    commands :: [(Text, Text)]
    commands =
      [ ("hex0.exe", "hex0-seed.exe"),
        ("hex1.exe", "hex0.exe"),
        ("hex2.exe", "hex1.exe"),
        ("catm.exe", "hex2.exe"),
        ("M0.hex2", "catm.exe"),
        ("M0.exe", "hex2.exe"),
        ("cc_x86-0.hex2", "M0.exe"),
        ("cc_x86.hex2", "catm.exe"),
        ("cc_x86.exe", "hex2.exe"),
        ("M2-0.c", "catm.exe"),
        ("M2-0.M1", "cc_x86.exe"),
        ("M2-0-0.M1", "catm.exe"),
        ("M2-0.hex2", "M0.exe"),
        ("M2-0-0.hex2", "catm.exe"),
        ("M2.exe", "hex2.exe"),
        ("M1-macro.M1", "M2.exe"),
        ("M1-macro-0.M1", "catm.exe"),
        ("M1-macro.hex2", "M0.exe"),
        ("M1-macro-0.hex2", "catm.exe"),
        ("M1.exe", "hex2.exe"),
        ("hex2_linker.M1", "M2.exe"),
        ("hex2_linker-0.M1", "catm.exe"),
        ("hex2_linker.hex2", "M0.exe"),
        ("hex2_linker-0.hex2", "catm.exe"),
        ("hex2-new.exe", "hex2.exe"),
        ("kaem.M1", "M2.exe"),
        ("kaem-0.M1", "catm.exe"),
        ("kaem.hex2", "M0.exe"),
        ("kaem-0.hex2", "catm.exe"),
        ("kaem.exe", "hex2.exe")
      ]

-- | @pkgs/windows@'s Mes, read back as the build driver reads it: what its
-- source tree, libraries, compiler and C library sources, and
-- @test\/data\/mes.nix@'s checks, add to the closure of the stage0 tools
-- they run on.  The rung is full-source only if the leaves it adds are the
-- Mes and NYACC archives and it reads no input source but the scripts
-- evaluation writes.  The builder spells a store path with a backslash,
-- which starts an escape in a C or Scheme string, so no script writes a path
-- into Mes's source with replace, and every script that runs Mes sets the
-- paths and sizes it reads in its environment instead.  exec-bit is
-- fixed-output to the NAR hash of the files its program makes, with the
-- execute bits their modes set.
testMesWindows :: IO [Bool]
testMesWindows = do
  putStrLn "pkgs/mes"
  cwd <- Dir.getCurrentDirectory
  storeDir <- evalNixIOStoreDir
  searchPaths <- parseNixPath (TE.encodeUtf8 (T.pack (cwd </> "data")))
  st <- newEvalStateWith testLogger unrestrictedPolicy searchPaths (testSessionStore storeDir) storeDir cwd
  let windows = "(import " <> nixQuotedPath (cwd </> "pkgs" </> "windows") <> ")"
      checks = "import " <> nixQuotedPath (cwd </> "test" </> "data" </> "mes.nix")
      quoted name = "\"" <> name <> "\""
      source =
        T.concat
          [ "let pkgs = ",
            windows,
            "; checks = ",
            checks,
            "; in builtins.concatStringsSep \" \" ([ pkgs.mes.srcPrefix ] ++ map (name: pkgs.stage0-pe32.${name}.drvPath) [ ",
            T.unwords (map quoted tools),
            " ] ++ map (name: pkgs.mes.${name}.drvPath) [ ",
            T.unwords (map quoted mesAttrs),
            " ] ++ map (name: checks.${name}.drvPath) [ ",
            T.unwords (map quoted checkAttrs),
            " ])"
          ]
      roots = tools ++ map ("mes." <>) mesAttrs ++ map ("checks." <>) checkAttrs
  outcome <- case parseNix cwd testOrigin (TE.encodeUtf8 source) of
    Left err -> pure (Left (T.pack (show err)))
    Right parsed -> runEvalIO st $ do
      value <- evalParsed (pure (evalBaseEnv st)) parsed
      case value of
        VStr bytes _ -> case T.words (bytesText bytes) of
          prefix : paths -> do
            let rootOf = Map.fromList (zip roots paths)
            toolGraph <- sessionClosure (mapMaybe (`Map.lookup` rootOf) tools)
            full <- sessionClosure (Map.elems rootOf)
            pure (prefix, rootOf, toolGraph, full)
          [] -> throwEvalError "expected srcPrefix and the drvPaths"
        other -> throwEvalError ("expected srcPrefix and the drvPaths as one string, got " <> typeName other)
  writes <- readIORef (esStoreWriteCache st)
  case outcome of
    Left err -> sequence [runTest instantiates (Fail err)]
    Right (prefix, rootOf, toolGraph, full) -> do
      let added = Map.elems (Map.difference full toolGraph)
          nameOf drv = maybe "" bytesText (Map.lookup "name" (drvEnv drv))
          variableOf name drv = maybe "" bytesText (Map.lookup name (drvEnv drv))
          outOf drv = [storePathToText defaultStoreDir (doPath o) | o <- drvOutputs drv, doName o == "out"]
          builtBy attr = Map.lookup attr rootOf >>= (`Map.lookup` full)
          outText attr = T.concat (maybe [] outOf (builtBy attr))
          fixedHashOf drv = [(doHashAlgo o, doHash o) | o <- drvOutputs drv, doName o == "out"]
          inputsOf drv = mapMaybe ((`Map.lookup` full) . storePathToText defaultStoreDir) (Map.keys (drvInputDrvs drv))
          scriptOf drv = listToMaybe [variableOf "text" input | input <- inputsOf drv, nameOf input == nameOf drv <> "-builder"]
          isFetch drv = drvBuilder drv == "builtin:fetchurl"
          isFixedOutput drv = not (all (T.null . doHash) (drvOutputs drv))
          addedLeaves = filter (Map.null . drvInputDrvs) added
          wroteAsText path = fmap snd (Map.lookup (storePathToText defaultStoreDir path) writes) == Just WriteText
          runsMes script = any (`T.isInfixOf` script) ["/bin/mes-m2.exe ", "/bin/mes.exe "]
          runners = [drv | drv <- added, Just script <- [scriptOf drv], runsMes script]
          quote dir = "\"" <> dir <> "\""
          mesEnv =
            [ ("MES_ARENA", "20000000"),
              ("MES_STACK", "60000"),
              ("MES_PREFIX", prefix),
              ("GUILE_LOAD_PATH", T.intercalate ":" (map quote [prefix <> "/mes/module", prefix <> "/module", outText "mes.nyacc" <> "/share/nyacc-1.09.1/module"])),
              ("M1", outText "mescc-tools" <> "/bin/M1.exe"),
              ("HEX2", outText "mescc-tools" <> "/bin/hex2.exe")
            ]
          -- What each replace line of a script writes into the file it edits.
          replacements script = [snd (T.breakOn "--replace-with" line) | line <- T.lines script, "replace " `T.isPrefixOf` T.stripStart line]
          written = concatMap replacements (mapMaybe scriptOf added)
      sequence
        [ runTest instantiates $
            assertEqual "derivations added" (sort addedNames) (sort (map nameOf added)),
          runTest "every derivation it adds but the two fetches is i686-windows" $
            case [nameOf drv | drv <- added, not (isFetch drv), drvPlatform drv /= I686_Windows] of
              [] -> Pass
              named -> Fail ("not i686-windows: " <> T.intercalate ", " named),
          runTest "the leaves it adds are the Mes and NYACC archives, both fetched fixed-output" $
            assertEqual
              "leaves added"
              (sort ["mes-77ff40b.tar.gz", "nyacc-1.09.1.tar.gz"], True)
              (sort (map nameOf addedLeaves), all (\drv -> isFetch drv && isFixedOutput drv) addedLeaves),
          runTest "it reads no input source but what evaluation writes as text" $
            assertEqual "input sources not written as text" [] [nameOf drv | drv <- added, not (all wroteAsText (drvInputSrcs drv))],
          runTest "every script that runs Mes sets its arena, MES_PREFIX, GUILE_LOAD_PATH, M1 and HEX2" $
            assertEqual
              "derivation and its environment"
              [(name, mesEnv) | name <- sort mesRunners]
              (sort [(nameOf drv, [(var, variableOf var drv) | (var, _) <- mesEnv]) | drv <- runners]),
          runTest "no script writes a path into Mes's source with replace" $
            assertEqual
              "replacements, and those that name a path"
              (3, [])
              (length written, filter (\with -> "/" `T.isInfixOf` with || "${" `T.isInfixOf` with) written),
          runTest "exec-bit is fixed-output to the NAR hash of the files its program makes, open-0755 and chmod-0755 executable" $
            assertEqual
              "output hash"
              [("r:sha256", Hash.bytesToHexText (sha256Digest (NAR.serialise execBitTree)))]
              (maybe [] fixedHashOf (builtBy "checks.exec-bit"))
        ]
  where
    instantiates :: Text
    instantiates = "the Mes derivations instantiate, adding thirty-two to the stage0 tools'"
    tools :: [Text]
    tools = ["kaem", "mescc-tools", "mescc-tools-extra"]
    mesAttrs :: [Text]
    mesAttrs = ["nyacc", "srcPost", "libs", "compiler", "libc"]
    checkAttrs :: [Text]
    checkAttrs = ["mes-m2", "mes", "hello", "exec-bit"]
    -- Each kaem.runCommand adds its script, written by writeText as
    -- <name>-builder; mes-libc-0.28 is both the libc MesCC compiles and
    -- the sources for TinyCC, as in nixpkgs.
    addedNames :: [Text]
    addedNames =
      ["mes-77ff40b.tar.gz", "nyacc-1.09.1.tar.gz", "hello.c", "exec-bit.c"]
        ++ concat [[name, name <> "-builder"] | name <- mesRunners ++ ["nyacc-1.09.1", "mes-src-0.28", "mes-m2-libs-0.28", "mes-libc-0.28"]]
    mesRunners :: [Text]
    mesRunners =
      [ "mes-crt1-0.28",
        "mes-libc-mini-0.28",
        "mes-libmescc-0.28",
        "mes-libc-0.28",
        "mes-libc+tcc-0.28",
        "mes-0.28",
        "mes-m2-get-version",
        "mes-get-version",
        "mes-hello",
        "mes-exec-bit"
      ]
    -- What test/data/mes.nix's exec-bit program makes: three files holding
    -- one line, the one open creates with mode 0755 and the one chmod
    -- gives 0755 executable, the one open creates with 0644 not.
    execBitTree :: NAR.NarEntry
    execBitTree =
      NAR.NarDirectory
        [ ("open-0755", NAR.NarRegular True execBitContents),
          ("open-0644", NAR.NarRegular False execBitContents),
          ("chmod-0755", NAR.NarRegular True execBitContents)
        ]
    execBitContents :: BS.ByteString
    execBitContents = "written by Mes libc\n"

-- | @pkgs/windows@'s TinyCC, read back as the build driver reads it: what
-- the rounds from tinycc-boot-mes to tinycc-native and
-- @test\/data\/tinycc.nix@'s checks add to the closure of the Mes rung they
-- run on.  The rung is full-source only if the leaves it adds are the two
-- TinyCC archives and it reads no input source but the scripts evaluation
-- writes.  The builder spells a store path with a backslash, which starts
-- an escape in a C string, so no script puts a store path between the
-- escaped quotes a @-D@ or a @replace@ turns into a C string literal, and
-- every script that runs tcc names the headers it compiles against in
-- @C_INCLUDE_PATH@ instead, then Mes's when the tcc it runs is on Mes libc.
testTinyccWindows :: IO [Bool]
testTinyccWindows = do
  putStrLn "pkgs/tinycc"
  cwd <- Dir.getCurrentDirectory
  storeDir <- evalNixIOStoreDir
  searchPaths <- parseNixPath (TE.encodeUtf8 (T.pack (cwd </> "data")))
  st <- newEvalStateWith testLogger unrestrictedPolicy searchPaths (testSessionStore storeDir) storeDir cwd
  let windows = "(import " <> nixQuotedPath (cwd </> "pkgs" </> "windows") <> ")"
      checks = "import " <> nixQuotedPath (cwd </> "test" </> "data" </> "tinycc.nix")
      quoted name = "\"" <> name <> "\""
      source =
        T.concat
          [ "let pkgs = ",
            windows,
            "; checks = ",
            checks,
            "; in builtins.concatStringsSep \" \" ([ pkgs.mes.srcPrefix ] ++ map (name: pkgs.stage0-pe32.${name}.drvPath) [ ",
            T.unwords (map quoted tools),
            " ] ++ map (name: pkgs.mes.${name}.drvPath) [ ",
            T.unwords (map quoted mesAttrs),
            " ] ++ [ ",
            T.unwords ["pkgs." <> path <> ".drvPath" | path <- tinyccPaths],
            " ] ++ map (name: checks.${name}.drvPath) [ ",
            T.unwords (map quoted checkAttrs),
            " ])"
          ]
      mesRoots = tools ++ map ("mes." <>) mesAttrs
      roots = mesRoots ++ tinyccPaths ++ map ("checks." <>) checkAttrs
  outcome <- case parseNix cwd testOrigin (TE.encodeUtf8 source) of
    Left err -> pure (Left (T.pack (show err)))
    Right parsed -> runEvalIO st $ do
      value <- evalParsed (pure (evalBaseEnv st)) parsed
      case value of
        VStr bytes _ -> case T.words (bytesText bytes) of
          prefix : paths -> do
            let rootOf = Map.fromList (zip roots paths)
            mesGraph <- sessionClosure (mapMaybe (`Map.lookup` rootOf) mesRoots)
            full <- sessionClosure (Map.elems rootOf)
            pure (prefix, rootOf, mesGraph, full)
          [] -> throwEvalError "expected srcPrefix and the drvPaths"
        other -> throwEvalError ("expected srcPrefix and the drvPaths as one string, got " <> typeName other)
  writes <- readIORef (esStoreWriteCache st)
  case outcome of
    Left err -> sequence [runTest instantiates (Fail err)]
    Right (prefix, rootOf, mesGraph, full) -> do
      let added = Map.elems (Map.difference full mesGraph)
          nameOf drv = maybe "" bytesText (Map.lookup "name" (drvEnv drv))
          variableOf name drv = maybe "" bytesText (Map.lookup name (drvEnv drv))
          outOf drv = [storePathToText defaultStoreDir (doPath o) | o <- drvOutputs drv, doName o == "out"]
          builtBy attr = Map.lookup attr rootOf >>= (`Map.lookup` full)
          outText attr = T.concat (maybe [] outOf (builtBy attr))
          byOut = Map.fromList [(path, drv) | drv <- Map.elems full, path <- outOf drv]
          -- A kaem.runCommand's last argument is its script, which writeText
          -- writes.
          scriptOf drv = do
            path <- listToMaybe (reverse (drvArgs drv))
            writer <- Map.lookup (bytesText path) byOut
            pure (variableOf "text" writer)
          isFetch drv = drvBuilder drv == "builtin:fetchurl"
          isFixedOutput drv = not (all (T.null . doHash) (drvOutputs drv))
          addedLeaves = filter (Map.null . drvInputDrvs) added
          wroteAsText path = fmap snd (Map.lookup (storePathToText defaultStoreDir path) writes) == Just WriteText
          -- What a script runs: the first word of each of its lines, which
          -- for a line continuing a command is an argument.
          commandsOf script = mapMaybe (listToMaybe . T.words) (T.lines script)
          runners program = [drv | drv <- added, Just script <- [scriptOf drv], any (\command -> any (`T.isSuffixOf` command) program) (commandsOf script)]
          mesProgram = ["/bin/mes-m2.exe", "/bin/mes.exe"]
          quote dir = "\"" <> dir <> "\""
          mesEnv =
            [ ("MES_ARENA", "20000000"),
              ("MES_STACK", "60000"),
              ("MES_PREFIX", prefix),
              ("GUILE_LOAD_PATH", T.intercalate ":" (map quote [prefix <> "/mes/module", prefix <> "/module", outText "mes.nyacc" <> "/share/nyacc-1.09.1/module"])),
              ("M1", outText "mescc-tools" <> "/bin/M1.exe"),
              ("HEX2", outText "mescc-tools" <> "/bin/hex2.exe")
            ]
          -- A TinyCC source tree, and its include directory, then Mes's.
          treeOf name tree = T.concat [path | drv <- added, nameOf drv == name, path <- outOf drv] <> "/" <> tree
          mesInclude = prefix <> "/include"
          jannekeInclude = treeOf ("tinycc-bootstrappable-" <> version <> "-source") ("tinycc-" <> rev) <> "/include;" <> mesInclude
          upstreamTree = treeOf ("tinycc-" <> upstreamVersion <> "-source") ("tinycc-" <> upstreamRev)
          upstreamInclude = upstreamTree <> "/include;" <> mesInclude
          -- TinyCC's own headers, the C library's and the Windows API's.
          win32Include = T.intercalate ";" [upstreamTree <> "/include", upstreamTree <> "/win32/include", upstreamTree <> "/win32/include/winapi"]
          -- The text between each pair of kaem's escaped quotes, which a -D
          -- hands MesCC or tcc, or a replace writes into a source, as a C
          -- string literal.
          cStrings script = [text | (i, text) <- zip [0 :: Int ..] (T.splitOn "\\\"" script), odd i]
          strings = concatMap cStrings (Set.toList (Set.fromList (mapMaybe scriptOf added)))
      sequence
        [ runTest instantiates $
            assertEqual "derivations added" (sort addedNames) (sort (map nameOf added)),
          runTest "every derivation it adds but the fetch is i686-windows" $
            case [nameOf drv | drv <- added, not (isFetch drv), drvPlatform drv /= I686_Windows] of
              [] -> Pass
              named -> Fail ("not i686-windows: " <> T.intercalate ", " named),
          runTest "the two leaves it adds are the TinyCC archives, fetched fixed-output" $
            assertEqual
              "leaves added"
              (sort ["tinycc-" <> rev <> ".tar.gz", "tinycc-" <> upstreamRev <> ".tar.gz"], True)
              (sort (map nameOf addedLeaves), all (\drv -> isFetch drv && isFixedOutput drv) addedLeaves),
          runTest "it reads no input source but what evaluation writes as text" $
            assertEqual "input sources not written as text" [] [nameOf drv | drv <- added, not (all wroteAsText (drvInputSrcs drv))],
          runTest "every script that runs Mes sets its arena, MES_PREFIX, GUILE_LOAD_PATH, M1 and HEX2" $
            assertEqual
              "derivation and its environment"
              [(name, mesEnv) | name <- sort mesRunners]
              (sort [(nameOf drv, [(var, variableOf var drv) | (var, _) <- mesEnv]) | drv <- runners mesProgram]),
          runTest "every script that runs tcc names the headers it compiles against in C_INCLUDE_PATH, then Mes's for a tcc on Mes libc" $
            assertEqual
              "derivation and its C_INCLUDE_PATH"
              ( sort
                  ( [(name, jannekeInclude) | name <- jannekeRunners]
                      ++ [(name, upstreamInclude) | name <- upstreamRunners]
                      ++ [(name, win32Include <> ";" <> mesInclude) | name <- mesWin32Runners]
                      ++ [(name, win32Include) | name <- nativeRunners]
                  )
              )
              (sort [(nameOf drv, variableOf "C_INCLUDE_PATH" drv) | drv <- runners ["/bin/tcc.exe"]]),
          -- Nine per Mes round's compile, five per native round's, and three
          -- in the edits TinyCC's own source gets: the format of nixpkgs'
          -- i386-asm.c replacement, and the two PATHSEP values.
          runTest "no C string literal a script passes in a -D or writes with replace names a store path" $
            assertEqual
              "string literals, and those that name a store path"
              (9 * (length rounds + length mesRounds) + 5 * length nativeRounds + 3, [])
              (length strings, filter ("${NIX_STORE}" `T.isInfixOf`) strings)
        ]
  where
    instantiates :: Text
    instantiates = "the TinyCC derivations instantiate, adding a hundred and fifty-four to Mes's"
    version :: Text
    version = "unstable-2024-07-07"
    rev :: Text
    rev = "ea3900f6d5e71776c5cfabcabee317652e3a19ee"
    upstreamVersion :: Text
    upstreamVersion = "unstable-2025-12-03"
    upstreamRev :: Text
    upstreamRev = "cb41cbfe717e4c00d7bb70035cda5ee5f0ff9341"
    tools :: [Text]
    tools = ["kaem", "mescc-tools", "mescc-tools-extra"]
    mesAttrs :: [Text]
    mesAttrs = ["nyacc", "srcPost", "libs", "compiler", "libc"]
    -- The first round of janneke's source and the last, which is built from
    -- all the others, both rounds of TinyCC's own on Mes libc, and both
    -- native rounds.
    tinyccPaths :: [Text]
    tinyccPaths =
      [ "tinycc.boot-mes.compiler",
        "tinycc.boot-mes.libs",
        "tinycc.bootstrappable.compiler",
        "tinycc.bootstrappable.libs",
        "tinycc-mes.prev.compiler",
        "tinycc-mes.prev.libs",
        "tinycc-mes.compiler",
        "tinycc-mes.libs",
        "tinycc-native.prev.compiler",
        "tinycc-native.prev.libs",
        "tinycc-native.compiler",
        "tinycc-native.libs"
      ]
    checkAttrs :: [Text]
    checkAttrs =
      [ "boot-mes",
        "hello",
        "bootstrappable",
        "bootstrappable-hello",
        "bootstrappable-float",
        "mes",
        "mes-hello",
        "mes-float",
        "mes-printf-float",
        "native",
        "native-search-dirs",
        "native-mingw32",
        "native-hello",
        "native-float",
        "native-printf-float",
        "native-winsock",
        "native-math"
      ]
    -- Each round of janneke's source, in the order each compiles the next,
    -- and then each round of TinyCC's own.
    rounds :: [Text]
    rounds = ["tinycc-boot-mes", "tinycc-boot0", "tinycc-boot1", "tinycc-boot2", "tinycc-boot3", "tinycc-bootstrappable"]
    mesRounds :: [Text]
    mesRounds = ["tinycc-mes-boot", "tinycc-mes"]
    -- The native rounds, the first compiled by tinycc-mes.
    nativeRounds :: [Text]
    nativeRounds = ["tinycc-native-boot", "tinycc-native"]
    -- What a round's library is compiled in, each by that round's tcc.
    libraryParts :: [Text]
    libraryParts = ["crt", "libtcc1.a", "libc.a", "libgetopt.a"]
    -- Each kaem.runCommand adds its script, written by writeText as
    -- <name>-builder.
    addedNames :: [Text]
    addedNames =
      ["tinycc-" <> rev <> ".tar.gz", "tinycc-" <> upstreamRev <> ".tar.gz", "hello.c", "float.c", "printf-float.c", "mingw32.c", "winsock.c", "math.c"]
        ++ concat
          [ [name, name <> "-builder"]
          | name <-
              ("tinycc-bootstrappable-" <> version <> "-source")
                : concat [[pname <> "-" <> version, pname <> "-libs-" <> version] ++ libraryParts | pname <- rounds]
                ++ tccRunnerChecks
                ++ ("tinycc-" <> upstreamVersion <> "-source")
                : ("tccdefs-" <> upstreamVersion)
                : concat [[pname <> "-" <> upstreamVersion, pname <> "-libs-" <> upstreamVersion] ++ libraryParts | pname <- mesRounds]
                ++ mesRunnerChecks
                ++ ("tinycc-mes-win32-libs-" <> upstreamVersion)
                : concat [[pname <> "-" <> upstreamVersion, pname <> "-libs-" <> upstreamVersion] | pname <- nativeRounds]
                ++ nativeRunnerChecks
          ]
    mesRunners :: [Text]
    mesRunners = ["tinycc-boot-mes-" <> version]
    tccRunnerChecks :: [Text]
    tccRunnerChecks =
      [ "tinycc-boot-mes-get-version",
        "tinycc-boot-mes-hello",
        "tinycc-bootstrappable-get-version",
        "tinycc-bootstrappable-hello",
        "tinycc-bootstrappable-float"
      ]
    mesRunnerChecks :: [Text]
    mesRunnerChecks =
      [ "tinycc-mes-get-version",
        "tinycc-mes-hello",
        "tinycc-mes-float",
        "tinycc-mes-printf-float"
      ]
    -- The scripts a tcc of janneke's source runs in: every library part of
    -- its rounds, every round's tcc but the first, which MesCC compiles, the
    -- checks, and what tinycc-bootstrappable compiles of TinyCC's own
    -- source, c2str and tinycc-mes-boot's tcc.
    jannekeRunners :: [Text]
    jannekeRunners =
      concatMap (const libraryParts) rounds
        ++ [pname <> "-" <> version | pname <- drop 1 rounds]
        ++ tccRunnerChecks
        ++ ["tccdefs-" <> upstreamVersion, "tinycc-mes-boot-" <> upstreamVersion]
    nativeRunnerChecks :: [Text]
    nativeRunnerChecks =
      [ "tinycc-native-get-version",
        "tinycc-native-search-dirs",
        "tinycc-native-mingw32",
        "tinycc-native-hello",
        "tinycc-native-float",
        "tinycc-native-printf-float",
        "tinycc-native-winsock",
        "tinycc-native-math"
      ]
    -- The scripts a tcc of TinyCC's own source on Mes libc runs in to
    -- compile against Mes libc: its rounds' library parts, tinycc-mes's tcc
    -- and the checks.
    upstreamRunners :: [Text]
    upstreamRunners = concatMap (const libraryParts) mesRounds ++ ["tinycc-mes-" <> upstreamVersion] ++ mesRunnerChecks
    -- The scripts tinycc-mes runs in to compile against the Windows
    -- headers: the runtime the first native tcc links with, and that tcc.
    mesWin32Runners :: [Text]
    mesWin32Runners = ["tinycc-mes-win32-libs-" <> upstreamVersion, "tinycc-native-boot-" <> upstreamVersion]
    -- The scripts a native tcc runs in: each round's runtime, the second
    -- round's tcc and the checks.
    nativeRunners :: [Text]
    nativeRunners = [pname <> "-libs-" <> upstreamVersion | pname <- nativeRounds] ++ ["tinycc-native-" <> upstreamVersion] ++ nativeRunnerChecks

-- | @pkgs/windows@'s GNU make, read back as the build driver reads it:
-- what make and @test\/data\/gnumake.nix@'s checks add to the closure of
-- tinycc-native, which compiles it.  The rung is full-source only if the
-- one leaf it adds is make's archive and it reads no input source but the
-- scripts and the import library evaluation writes.  make's script runs
-- tinycc-native, so it names the headers that compiler reads in
-- @C_INCLUDE_PATH@, as tinycc-native's own library script does, and no C
-- string literal its edits write names a store path.
testGnumakeWindows :: IO [Bool]
testGnumakeWindows = do
  putStrLn "pkgs/gnumake"
  cwd <- Dir.getCurrentDirectory
  storeDir <- evalNixIOStoreDir
  searchPaths <- parseNixPath (TE.encodeUtf8 (T.pack (cwd </> "data")))
  st <- newEvalStateWith testLogger unrestrictedPolicy searchPaths (testSessionStore storeDir) storeDir cwd
  let windows = "(import " <> nixQuotedPath (cwd </> "pkgs" </> "windows") <> ")"
      checks = "import " <> nixQuotedPath (cwd </> "test" </> "data" </> "gnumake.nix")
      source =
        T.concat
          [ "let pkgs = ",
            windows,
            "; checks = ",
            checks,
            "; in builtins.concatStringsSep \" \" [ ",
            T.unwords [path <> ".drvPath" | path <- roots],
            " ]"
          ]
  outcome <- case parseNix cwd testOrigin (TE.encodeUtf8 source) of
    Left err -> pure (Left (T.pack (show err)))
    Right parsed -> runEvalIO st $ do
      value <- evalParsed (pure (evalBaseEnv st)) parsed
      case value of
        VStr bytes _ -> do
          let rootOf = Map.fromList (zip roots (T.words (bytesText bytes)))
          tinyccGraph <- sessionClosure (mapMaybe (`Map.lookup` rootOf) tinyccRoots)
          full <- sessionClosure (Map.elems rootOf)
          pure (rootOf, tinyccGraph, full)
        other -> throwEvalError ("expected the drvPaths as one string, got " <> typeName other)
  writes <- readIORef (esStoreWriteCache st)
  case outcome of
    Left err -> sequence [runTest instantiates (Fail err)]
    Right (rootOf, tinyccGraph, full) -> do
      let added = Map.elems (Map.difference full tinyccGraph)
          nameOf drv = maybe "" bytesText (Map.lookup "name" (drvEnv drv))
          variableOf name drv = maybe "" bytesText (Map.lookup name (drvEnv drv))
          builtBy attr = Map.lookup attr rootOf >>= (`Map.lookup` full)
          inputsOf drv = mapMaybe ((`Map.lookup` full) . storePathToText defaultStoreDir) (Map.keys (drvInputDrvs drv))
          scriptOf drv = listToMaybe [variableOf "text" input | input <- inputsOf drv, nameOf input == nameOf drv <> "-builder"]
          isFetch drv = drvBuilder drv == "builtin:fetchurl"
          isFixedOutput drv = not (all (T.null . doHash) (drvOutputs drv))
          addedLeaves = filter (Map.null . drvInputDrvs) added
          wroteAsText path = fmap snd (Map.lookup (storePathToText defaultStoreDir path) writes) == Just WriteText
          runsTcc script = "/bin/tcc.exe " `T.isInfixOf` script
          -- The text between each pair of kaem's escaped quotes, which a
          -- replace writes into a source as a C string literal.
          cStrings script = [text | (i, text) <- zip [0 :: Int ..] (T.splitOn "\\\"" script), odd i]
          strings = concatMap cStrings (mapMaybe scriptOf added)
      sequence
        [ runTest instantiates $
            assertEqual "derivations added" (sort addedNames) (sort (map nameOf added)),
          runTest "every derivation it adds but the fetch is i686-windows" $
            case [nameOf drv | drv <- added, not (isFetch drv), drvPlatform drv /= I686_Windows] of
              [] -> Pass
              named -> Fail ("not i686-windows: " <> T.intercalate ", " named),
          runTest "the one leaf it adds is make's archive, fetched fixed-output" $
            assertEqual
              "leaves added"
              (["make-" <> version <> ".tar.gz"], True)
              (map nameOf addedLeaves, all (\drv -> isFetch drv && isFixedOutput drv) addedLeaves),
          runTest "it reads no input source but what evaluation writes as text" $
            assertEqual "input sources not written as text" [] [nameOf drv | drv <- added, not (all wroteAsText (drvInputSrcs drv))],
          runTest "make's script runs tinycc-native with the headers its library script names" $
            assertEqual
              "derivations that run tcc, and their C_INCLUDE_PATH"
              [("gnumake-" <> version, variableOf "C_INCLUDE_PATH" <$> builtBy "pkgs.tinycc-native.libs")]
              [(nameOf drv, Just (variableOf "C_INCLUDE_PATH" drv)) | drv <- added, Just script <- [scriptOf drv], runsTcc script],
          -- One per include or library directory the edits remove.
          runTest "no C string literal make's edits write names a store path" $
            assertEqual
              "string literals, and those that name a store path"
              (5, [])
              (length strings, filter ("${NIX_STORE}" `T.isInfixOf`) strings)
        ]
  where
    instantiates :: Text
    instantiates = "the make derivations instantiate, adding ten to tinycc-native's"
    version :: Text
    version = "4.4.1"
    tinyccRoots :: [Text]
    tinyccRoots = ["pkgs.tinycc-native.compiler", "pkgs.tinycc-native.libs"]
    roots :: [Text]
    roots = tinyccRoots ++ ["pkgs.gnumake", "checks.version", "checks.pattern-rule"]
    -- Each kaem.runCommand adds its script, written by writeText as
    -- <name>-builder.
    addedNames :: [Text]
    addedNames =
      ["make-" <> version <> ".tar.gz", "pattern-rule.mk", "greeting.in"]
        ++ concat [[name, name <> "-builder"] | name <- ["gnumake-" <> version, "gnumake-get-version", "gnumake-pattern-rule"]]

-- | @pkgs/windows@'s busybox-w32, read back as the build driver reads it:
-- what busybox-w32 and @test\/data\/busybox.nix@'s checks add to the
-- closure of tinycc-native, which compiles it.  The rung is full-source
-- only if the leaves it adds are busybox-w32's release archive and the
-- mingw-w64-crt sources it links, and it reads no input source but what
-- evaluation writes as text and the package's own compatibility headers
-- and generators.  Its script runs tinycc-native, so it names the headers
-- that compiler reads in @C_INCLUDE_PATH@, and no C string literal it
-- passes names a store path.  replace writes a file unchanged when its
-- match is missing, so every file an edit writes has a sum the script
-- checks after the last edit.
testBusyboxWindows :: IO [Bool]
testBusyboxWindows = do
  putStrLn "pkgs/busybox-w32"
  cwd <- Dir.getCurrentDirectory
  storeDir <- evalNixIOStoreDir
  searchPaths <- parseNixPath (TE.encodeUtf8 (T.pack (cwd </> "data")))
  st <- newEvalStateWith testLogger unrestrictedPolicy searchPaths (testSessionStore storeDir) storeDir cwd
  let windows = "(import " <> nixQuotedPath (cwd </> "pkgs" </> "windows") <> ")"
      checks = "import " <> nixQuotedPath (cwd </> "test" </> "data" </> "busybox.nix")
      source =
        T.concat
          [ "let pkgs = ",
            windows,
            "; checks = ",
            checks,
            "; in builtins.concatStringsSep \" \" [ ",
            T.unwords [path <> ".drvPath" | path <- roots],
            " ]"
          ]
  outcome <- case parseNix cwd testOrigin (TE.encodeUtf8 source) of
    Left err -> pure (Left (T.pack (show err)))
    Right parsed -> runEvalIO st $ do
      value <- evalParsed (pure (evalBaseEnv st)) parsed
      case value of
        VStr bytes _ -> do
          let rootOf = Map.fromList (zip roots (T.words (bytesText bytes)))
          tinyccGraph <- sessionClosure (mapMaybe (`Map.lookup` rootOf) tinyccRoots)
          full <- sessionClosure (Map.elems rootOf)
          pure (rootOf, tinyccGraph, full)
        other -> throwEvalError ("expected the drvPaths as one string, got " <> typeName other)
  writes <- readIORef (esStoreWriteCache st)
  case outcome of
    Left err -> sequence [runTest instantiates (Fail err)]
    Right (rootOf, tinyccGraph, full) -> do
      let added = Map.elems (Map.difference full tinyccGraph)
          nameOf drv = maybe "" bytesText (Map.lookup "name" (drvEnv drv))
          variableOf name drv = maybe "" bytesText (Map.lookup name (drvEnv drv))
          builtBy attr = Map.lookup attr rootOf >>= (`Map.lookup` full)
          inputsOf drv = mapMaybe ((`Map.lookup` full) . storePathToText defaultStoreDir) (Map.keys (drvInputDrvs drv))
          scriptOf drv = listToMaybe [variableOf "text" input | input <- inputsOf drv, nameOf input == nameOf drv <> "-builder"]
          isFetch drv = drvBuilder drv == "builtin:fetchurl"
          isFixedOutput drv = not (all (T.null . doHash) (drvOutputs drv))
          addedLeaves = filter (Map.null . drvInputDrvs) added
          wroteAsText path = fmap snd (Map.lookup (storePathToText defaultStoreDir path) writes) == Just WriteText
          runsTcc script = "/bin/tcc.exe " `T.isInfixOf` script
          -- The text between each pair of kaem's escaped quotes, which a
          -- -D hands tcc as a C string literal.
          cStrings script = [text | (i, text) <- zip [0 :: Int ..] (T.splitOn "\\\"" script), odd i]
          strings = concatMap cStrings (mapMaybe scriptOf added)
          busyboxScript = fromMaybe "" (builtBy "pkgs.busybox-w32" >>= scriptOf)
          edited = Set.fromList [TE.encodeUtf8 file | line <- T.lines busyboxScript, "replace" : "--file" : file : _ <- [T.words line]]
          -- The sums reach the build through its script, whose writeText
          -- derivation reads them as an input source.
          sumsPath = listToMaybe [path | drv <- added, path <- drvInputSrcs drv, spName path == "busybox-w32-edits.sha256"]
          numbered = zip [0 :: Int ..] (T.lines busyboxScript)
          checkedAfterEdits = case [i | (i, line) <- numbered, "sha256sum --check " `T.isPrefixOf` line] of
            [check] -> all (< check) [i | (i, line) <- numbered, "replace " `T.isPrefixOf` line]
            _ -> False
      sums <- traverse (BS.readFile . storePathToFilePath storeDir) sumsPath
      let summed = Set.fromList [file | bytes <- maybeToList sums, line <- BS8.lines bytes, [_, file] <- [BS8.words line]]
      sequence
        [ runTest instantiates $
            assertEqual "derivations added" (sort addedNames) (sort (map nameOf added)),
          runTest "every derivation it adds but the fetches is i686-windows" $
            case [nameOf drv | drv <- added, not (isFetch drv), drvPlatform drv /= I686_Windows] of
              [] -> Pass
              named -> Fail ("not i686-windows: " <> T.intercalate ", " named),
          runTest "the leaves it adds are busybox-w32's archive and the mingw-w64-crt sources, fetched fixed-output" $
            assertEqual
              "leaves added"
              (sort (("busybox-w32-" <> version <> ".tgz") : crtSources), True)
              (sort (map nameOf addedLeaves), all (\drv -> isFetch drv && isFixedOutput drv) addedLeaves),
          runTest "it reads no input source but what evaluation writes as text and the package's own sources" $
            assertEqual
              "input sources not written as text"
              ["compat", "gen_build_files.c", "usage_compressed.c"]
              (sort [spName path | drv <- added, path <- drvInputSrcs drv, not (wroteAsText path)]),
          runTest "busybox-w32's script runs tinycc-native with the headers its library script names" $
            assertEqual
              "derivations that run tcc, and their C_INCLUDE_PATH"
              [("busybox-w32-" <> version, variableOf "C_INCLUDE_PATH" <$> builtBy "pkgs.tinycc-native.libs")]
              [(nameOf drv, Just (variableOf "C_INCLUDE_PATH" drv)) | drv <- added, Just text <- [scriptOf drv], runsTcc text],
          -- One per busybox-w32 source, the empty MINGW_VER Makefile.flags
          -- defines.
          runTest "no C string literal busybox-w32's script passes names a store path" $
            assertEqual
              "string literals, and those that name a store path"
              (sourceCount, [])
              (length strings, filter ("${NIX_STORE}" `T.isInfixOf`) strings),
          runTest "every file busybox-w32's edits write is checked against its sum, once they are all made" $
            assertEqual
              "files edited, and files summed, and the one check after the last edit"
              (edited, True, True)
              (summed, not (Set.null edited), checkedAfterEdits)
        ]
  where
    instantiates :: Text
    instantiates = "the busybox-w32 derivations instantiate, adding thirteen to tinycc-native's"
    version :: Text
    version = "FRP-6354-g5f09fb446"
    sourceCount :: Int
    sourceCount = 336
    crtSources :: [Text]
    crtSources = ["getopt.c", "mempcpy.c", "mingw_usleep.c", "strtok_r.c", "msvcr80plus_ftruncate64.c"]
    tinyccRoots :: [Text]
    tinyccRoots = ["pkgs.tinycc-native.compiler", "pkgs.tinycc-native.libs"]
    roots :: [Text]
    roots = tinyccRoots ++ ["pkgs.busybox-w32", "checks.version", "checks.configure-tools"]
    -- Each kaem.runCommand adds its script, written by writeText as
    -- <name>-builder.
    addedNames :: [Text]
    addedNames =
      ("busybox-w32-" <> version <> ".tgz")
        : "configure-tools.sh"
        : crtSources
        ++ concat [[name, name <> "-builder"] | name <- ["busybox-w32-" <> version, "busybox-w32-get-version", "busybox-w32-configure-tools"]]

-- | The checks under @test\/data@ that the ladder's CI jobs build, each of
-- which has to forbid substitutes: CI publishes what it builds on main, and
-- a check whose output a cache held would be downloaded where it is meant
-- to run, and would test nothing while its job stayed green.  The one
-- attribute that may be substituted is @run-command.nix@'s hello, a file
-- @writeText@ writes, which takes no attributes.
testChecksForbidSubstitutes :: IO [Bool]
testChecksForbidSubstitutes = do
  putStrLn "pkgs/checks"
  cwd <- Dir.getCurrentDirectory
  storeDir <- evalNixIOStoreDir
  searchPaths <- parseNixPath (TE.encodeUtf8 (T.pack (cwd </> "data")))
  st <- newEvalStateWith testLogger unrestrictedPolicy searchPaths (testSessionStore storeDir) storeDir cwd
  let fileOf name = nixQuotedPath (cwd </> "test" </> "data" </> T.unpack name)
      source =
        T.concat
          [ "let substitutable = file: set: map (name: \"${file}:${name}\") (builtins.filter (name: (set.${name}.allowSubstitutes or true) != false) (builtins.attrNames set)); in builtins.concatStringsSep \" \" (",
            T.intercalate " ++ " ["substitutable \"" <> name <> "\" (import " <> fileOf name <> ")" | name <- checkFiles],
            ")"
          ]
  outcome <- case parseNix cwd testOrigin (TE.encodeUtf8 source) of
    Left err -> pure (Left (T.pack (show err)))
    Right parsed -> runEvalIO st $ do
      value <- evalParsed (pure (evalBaseEnv st)) parsed
      case value of
        VStr bytes _ -> pure (T.words (bytesText bytes))
        other -> throwEvalError ("expected the attributes as one string, got " <> typeName other)
  sequence
    [ runTest "every check forbids substitutes but the file writeText writes" $
        either Fail (assertEqual "checks that may be substituted" ["run-command.nix:hello"]) outcome
    ]
  where
    checkFiles :: [Text]
    checkFiles = ["binutils.nix", "binutils-native-x86_64.nix", "binutils-x86_64.nix", "busybox.nix", "gcc-core.nix", "gcc-core-x86_64.nix", "gcc-latest.nix", "gcc-native-x86_64.nix", "gcc-x86_64.nix", "gcc10.nix", "gcc46.nix", "gcc46-cxx.nix", "gnumake.nix", "mes.nix", "mingw-w64-latest.nix", "mingw-w64-x86_64.nix", "run-command.nix", "seed-stdenv.nix", "stdenv.nix", "tinycc.nix"]

-- | @pkgs/windows@'s seed stdenv, read back as the build driver reads it:
-- what the stdenv and GNU Hello built with it, @test\/data\/seed-stdenv.nix@'s
-- hello, add to the closure of the tools it is made of, busybox-w32, GNU
-- make and tinycc-native.  The one leaf it adds is hello's archive, and it
-- reads no input source but the stdenv's own files and what evaluation
-- writes as text.  The leaves of hello's whole closure are the seed and
-- the sources the ladder fetches; MinGW-w64's files there are the five
-- mingw-w64-crt sources busybox-w32 compiles and the declarations its
-- compat headers and the stdenv's sdkddkver.h transcribe.
testSeedStdenvWindows :: IO [Bool]
testSeedStdenvWindows = do
  putStrLn "pkgs/seed-stdenv"
  cwd <- Dir.getCurrentDirectory
  storeDir <- evalNixIOStoreDir
  searchPaths <- parseNixPath (TE.encodeUtf8 (T.pack (cwd </> "data")))
  st <- newEvalStateWith testLogger unrestrictedPolicy searchPaths (testSessionStore storeDir) storeDir cwd
  let windows = "(import " <> nixQuotedPath (cwd </> "pkgs" </> "windows") <> ")"
      checks = "import " <> nixQuotedPath (cwd </> "test" </> "data" </> "seed-stdenv.nix")
      source =
        T.concat
          [ "let pkgs = ",
            windows,
            "; checks = ",
            checks,
            "; in builtins.concatStringsSep \" \" [ ",
            T.unwords [path <> ".drvPath" | path <- roots],
            " ]"
          ]
  outcome <- case parseNix cwd testOrigin (TE.encodeUtf8 source) of
    Left err -> pure (Left (T.pack (show err)))
    Right parsed -> runEvalIO st $ do
      value <- evalParsed (pure (evalBaseEnv st)) parsed
      case value of
        VStr bytes _ -> do
          let rootOf = Map.fromList (zip roots (T.words (bytesText bytes)))
              closureOf attrs = sessionClosure (mapMaybe (`Map.lookup` rootOf) attrs)
          tools <- closureOf toolRoots
          seed <- closureOf [seedHello]
          pure (tools, seed)
        other -> throwEvalError ("expected the drvPaths as one string, got " <> typeName other)
  writes <- readIORef (esStoreWriteCache st)
  case outcome of
    Left err -> sequence [runTest instantiates (Fail err)]
    Right (tools, seed) -> do
      let added = Map.elems (Map.difference seed tools)
          nameOf drv = maybe "" bytesText (Map.lookup "name" (drvEnv drv))
          isFetch drv = drvBuilder drv == "builtin:fetchurl"
          isFixedOutput drv = not (all (T.null . doHash) (drvOutputs drv))
          leavesOf = filter (Map.null . drvInputDrvs)
          addedLeaves = leavesOf added
          seedLeaves = leavesOf (Map.elems seed)
          wroteAsText path = fmap snd (Map.lookup (storePathToText defaultStoreDir path) writes) == Just WriteText
      sequence
        [ runTest instantiates $
            assertEqual "derivations added" (sort addedNames) (sort (map nameOf added)),
          runTest "every derivation it adds but the fetch is i686-windows" $
            case [nameOf drv | drv <- added, not (isFetch drv), drvPlatform drv /= I686_Windows] of
              [] -> Pass
              named -> Fail ("not i686-windows: " <> T.intercalate ", " named),
          runTest "the one leaf it adds is hello's archive, fetched fixed-output" $
            assertEqual
              "leaves added"
              ([helloArchive], True)
              (map nameOf addedLeaves, all (\drv -> isFetch drv && isFixedOutput drv) addedLeaves),
          runTest "it reads no input source but what evaluation writes as text and the stdenv's own files" $
            assertEqual
              "input sources not written as text"
              ["applet.c", "config.site", "include", "setup.sh"]
              (sort [spName path | drv <- added, path <- drvInputSrcs drv, not (wroteAsText path)]),
          runTest "the leaves of hello's closure are the seed and the ladder's sources, all fetched fixed-output" $
            assertEqual
              "leaves"
              (sort closureLeaves, True)
              (sort (map nameOf seedLeaves), all (\drv -> isFetch drv && isFixedOutput drv) seedLeaves)
        ]
  where
    instantiates :: Text
    instantiates = "the seed stdenv and its hello instantiate, adding three to the closure of their tools"
    toolRoots :: [Text]
    toolRoots = ["pkgs.busybox-w32", "pkgs.gnumake", "pkgs.tinycc-native.compiler", "pkgs.tinycc-native.libs"]
    seedHello :: Text
    seedHello = "checks.hello"
    roots :: [Text]
    roots = toolRoots ++ [seedHello]
    helloArchive :: Text
    helloArchive = "hello-2.12.3.tar.gz"
    addedNames :: [Text]
    addedNames = [helloArchive, "busybox-w32-FRP-6354-g5f09fb446-applets", "hello"]
    -- stage0-pe32's seed and sources, Mes and NYACC, janneke's TinyCC and
    -- TinyCC's own, GNU make, busybox-w32 and the mingw-w64-crt sources it
    -- compiles, and hello.
    closureLeaves :: [Text]
    closureLeaves =
      [ "hex0-seed.exe",
        "stage0-pe32-a81c837.tar.gz",
        "M2-Planet-ee0bf91.tar.gz",
        "M2libc-ef9dfc2.tar.gz",
        "mescc-tools-578ac99.tar.gz",
        "mescc-tools-extra-d1d8146.tar.gz",
        "mes-77ff40b.tar.gz",
        "nyacc-1.09.1.tar.gz",
        "tinycc-ea3900f6d5e71776c5cfabcabee317652e3a19ee.tar.gz",
        "tinycc-cb41cbfe717e4c00d7bb70035cda5ee5f0ff9341.tar.gz",
        "make-4.4.1.tar.gz",
        "busybox-w32-FRP-6354-g5f09fb446.tgz",
        "getopt.c",
        "mempcpy.c",
        "mingw_usleep.c",
        "strtok_r.c",
        "msvcr80plus_ftruncate64.c",
        helloArchive
      ]

-- | @pkgs/windows@'s binutils, read back as the build driver reads it: what
-- binutils and @test\/data\/binutils.nix@'s checks add to the closure of
-- the seed stdenv's tools and applets, which build it.  The rung is
-- full-source only if the one leaf it adds is binutils' archive, and it
-- reads no input source but the stdenv's own files, the package's patches
-- and compatibility headers, and what evaluation writes as text.  The
-- stdenv's @setup.sh@ applies the patches and searches the headers only if
-- the derivation names them in the variables it reads, @patches@ and
-- @buildInputs@.  The target is the triple GCC is configured for on the
-- next rung, which finds these tools by its name.
testBinutilsWindows :: IO [Bool]
testBinutilsWindows = do
  putStrLn "pkgs/binutils"
  cwd <- Dir.getCurrentDirectory
  storeDir <- evalNixIOStoreDir
  searchPaths <- parseNixPath (TE.encodeUtf8 (T.pack (cwd </> "data")))
  st <- newEvalStateWith testLogger unrestrictedPolicy searchPaths (testSessionStore storeDir) storeDir cwd
  let windows = "(import " <> nixQuotedPath (cwd </> "pkgs" </> "windows") <> ")"
      checks = "import " <> nixQuotedPath (cwd </> "test" </> "data" </> "binutils.nix")
      source =
        T.concat
          [ "let pkgs = ",
            windows,
            "; checks = ",
            checks,
            "; in builtins.concatStringsSep \" \" [ ",
            T.unwords [path <> ".drvPath" | path <- roots],
            " ]"
          ]
  outcome <- case parseNix cwd testOrigin (TE.encodeUtf8 source) of
    Left err -> pure (Left (T.pack (show err)))
    Right parsed -> runEvalIO st $ do
      value <- evalParsed (pure (evalBaseEnv st)) parsed
      case value of
        VStr bytes _ -> do
          let rootOf = Map.fromList (zip roots (T.words (bytesText bytes)))
          stdenvGraph <- sessionClosure (mapMaybe (`Map.lookup` rootOf) stdenvRoots)
          full <- sessionClosure (Map.elems rootOf)
          pure (rootOf, stdenvGraph, full)
        other -> throwEvalError ("expected the drvPaths as one string, got " <> typeName other)
  writes <- readIORef (esStoreWriteCache st)
  case outcome of
    Left err -> sequence [runTest instantiates (Fail err)]
    Right (rootOf, stdenvGraph, full) -> do
      let added = Map.elems (Map.difference full stdenvGraph)
          nameOf drv = maybe "" bytesText (Map.lookup "name" (drvEnv drv))
          variableOf name drv = maybe "" bytesText (Map.lookup name (drvEnv drv))
          isFetch drv = drvBuilder drv == "builtin:fetchurl"
          isFixedOutput drv = not (all (T.null . doHash) (drvOutputs drv))
          addedLeaves = filter (Map.null . drvInputDrvs) added
          wroteAsText path = fmap snd (Map.lookup (storePathToText defaultStoreDir path) writes) == Just WriteText
          binutils = Map.lookup binutilsRoot rootOf >>= (`Map.lookup` full)
          -- The input sources of binutils' derivation under a name, as its
          -- environment spells a store path.
          sourcesNamed name = [storePathToText defaultStoreDir path | drv <- maybeToList binutils, path <- drvInputSrcs drv, spName path == name]
          wordsOf name = maybe [] (T.words . variableOf name) binutils
      sequence
        [ runTest instantiates $
            assertEqual "derivations added" (sort addedNames) (sort (map nameOf added)),
          runTest "every derivation it adds but the fetch is i686-windows" $
            case [nameOf drv | drv <- added, not (isFetch drv), drvPlatform drv /= I686_Windows] of
              [] -> Pass
              named -> Fail ("not i686-windows: " <> T.intercalate ", " named),
          runTest "the one leaf it adds is binutils' archive, fetched fixed-output" $
            assertEqual
              "leaves added"
              ([archive], True)
              (map nameOf addedLeaves, all (\drv -> isFetch drv && isFixedOutput drv) addedLeaves),
          runTest "it reads no input source but what evaluation writes as text, the stdenv's files and the package's patches and headers" $
            assertEqual
              "input sources not written as text"
              (sort (["compat", "config.site", "include", "setup.sh"] ++ patches))
              (sort [spName path | drv <- added, path <- drvInputSrcs drv, not (wroteAsText path)]),
          runTest "binutils names its patches and its headers where setup.sh reads them" $
            assertEqual
              "patches and buildInputs"
              (concatMap sourcesNamed patches, sourcesNamed "compat", True)
              (wordsOf "patches", wordsOf "buildInputs", isJust binutils),
          runTest "binutils is configured for the target GCC is" $
            assertEqual "target flags" ["--target=" <> target] (filter ("--target=" `T.isPrefixOf`) (wordsOf "configureFlags"))
        ]
  where
    instantiates :: Text
    instantiates = "the binutils derivations instantiate, adding seven to the closure of the seed stdenv's tools"
    version :: Text
    version = "2.41"
    archive :: Text
    archive = "binutils-" <> version <> ".tar.xz"
    patches :: [Text]
    patches = ["fix-tinycc-attribute.patch", "temp-file-binary.patch"]
    target :: Text
    target = "i686-w64-mingw32"
    binutilsRoot :: Text
    binutilsRoot = "pkgs.binutils"
    stdenvRoots :: [Text]
    stdenvRoots = ["pkgs.busybox-w32", "pkgs.gnumake", "pkgs.tinycc-native.compiler", "pkgs.tinycc-native.libs", "pkgs.seedStdenv.applets"]
    roots :: [Text]
    roots = stdenvRoots ++ [binutilsRoot, "checks.version", "checks.answer"]
    -- Each kaem.runCommand adds its script, written by writeText as
    -- <name>-builder.
    addedNames :: [Text]
    addedNames =
      [archive, "binutils-" <> version, "answer.sh"]
        ++ concat [[name, name <> "-builder"] | name <- ["binutils-get-version", "binutils-answer"]]

-- | @pkgs/windows@'s GCC rung, read back as the build driver reads it: what
-- mingw-w64-headers, mingw-w64-dirent, gcc-core and
-- @test\/data\/gcc-core.nix@'s checks add to the closure of the seed
-- stdenv's tools and binutils, which build them.  The rung is full-source
-- only if the leaves it adds are the six sources it names, and it reads no
-- input source but the stdenv's own files, gcc-core's patch, the header
-- mingw-w64-dirent compiles its source against, and what evaluation writes
-- as text.  The stdenv's @setup.sh@ applies the patch only if the
-- derivation names it in @patches@, and gcc-core's @preConfigure@ unpacks
-- gmp, mpfr and mpc from the variables that name their archives.  GCC's
-- configure is told where the headers, the assembler, the linker and the
-- dirent library are by flags, so each has to name the output of the
-- derivation that builds it.  The headers are named with @--with-headers@
-- and never with @--with-sysroot@: GCC's top level hands its options to
-- mpc's configure, whose libtool has a @--with-sysroot@ of its own and
-- stops at a path that begins with a drive letter, as every store path on
-- Windows does.  @--with-headers@ names the @include@ directory of
-- mingw-w64-headers' output, where its configure installs them unless a
-- flag moves them.  libgcc is built and installed by the same make,
-- without debugging information, which reaches it through the target
-- libraries' CFLAGS and through a make variable of GCC's own.
testGccCoreWindows :: IO [Bool]
testGccCoreWindows = do
  putStrLn "pkgs/gcc-core"
  cwd <- Dir.getCurrentDirectory
  storeDir <- evalNixIOStoreDir
  searchPaths <- parseNixPath (TE.encodeUtf8 (T.pack (cwd </> "data")))
  st <- newEvalStateWith testLogger unrestrictedPolicy searchPaths (testSessionStore storeDir) storeDir cwd
  let windows = "(import " <> nixQuotedPath (cwd </> "pkgs" </> "windows") <> ")"
      checks = "import " <> nixQuotedPath (cwd </> "test" </> "data" </> "gcc-core.nix")
      source =
        T.concat
          [ "let pkgs = ",
            windows,
            "; checks = ",
            checks,
            "; in builtins.concatStringsSep \" \" [ ",
            T.unwords [path <> ".drvPath" | path <- roots],
            " ]"
          ]
  outcome <- case parseNix cwd testOrigin (TE.encodeUtf8 source) of
    Left err -> pure (Left (T.pack (show err)))
    Right parsed -> runEvalIO st $ do
      value <- evalParsed (pure (evalBaseEnv st)) parsed
      case value of
        VStr bytes _ -> do
          let rootOf = Map.fromList (zip roots (T.words (bytesText bytes)))
          before <- sessionClosure (mapMaybe (`Map.lookup` rootOf) builtBefore)
          full <- sessionClosure (Map.elems rootOf)
          pure (rootOf, before, full)
        other -> throwEvalError ("expected the drvPaths as one string, got " <> typeName other)
  writes <- readIORef (esStoreWriteCache st)
  gmpPatchLines <- BS8.lines <$> BS.readFile (cwd </> "pkgs" </> "windows" </> "by-name" </> "gc" </> "gcc-core" </> T.unpack gmpPatch)
  case outcome of
    Left err -> sequence [runTest instantiates (Fail err)]
    Right (rootOf, before, full) -> do
      let added = Map.elems (Map.difference full before)
          nameOf drv = maybe "" bytesText (Map.lookup "name" (drvEnv drv))
          variableOf name drv = maybe "" bytesText (Map.lookup name (drvEnv drv))
          isFetch drv = drvBuilder drv == "builtin:fetchurl"
          isFixedOutput drv = not (all (T.null . doHash) (drvOutputs drv))
          addedLeaves = filter (Map.null . drvInputDrvs) added
          wroteAsText path = fmap snd (Map.lookup (storePathToText defaultStoreDir path) writes) == Just WriteText
          derivationAt root = Map.lookup root rootOf >>= (`Map.lookup` full)
          gccCore = derivationAt gccCoreRoot
          headers = derivationAt headersRoot
          -- The output of a root's derivation, as a flag spells a store
          -- path, or a word no flag holds where the root is missing.
          outputOf root = maybe "<no output>" (storePathToText defaultStoreDir . doPath) (derivationAt root >>= listToMaybe . drvOutputs)
          outputNamed name = [storePathToText defaultStoreDir (doPath out) | drv <- added, nameOf drv == name, out <- drvOutputs drv]
          sourcesNamed name = [storePathToText defaultStoreDir path | drv <- maybeToList gccCore, path <- drvInputSrcs drv, spName path == name]
          wordsOf name = maybe [] (T.words . variableOf name)
          flagsWith prefixes drv = [flag | flag <- wordsOf "configureFlags" drv, any (`T.isPrefixOf` flag) prefixes]
          -- gmp's patch: the file it changes, the lines it removes, and the
          -- digit strings among the lines it adds, each a quoted value to
          -- sixteen places.
          gmpPatchFiles = [rest | line <- gmpPatchLines, Just rest <- [BS.stripPrefix "+++ " line]]
          gmpPatchRemoved = [rest | line <- gmpPatchLines, Just rest <- [BS.stripPrefix "-" line], not ("-- " `BS.isPrefixOf` rest)]
          gmpPatchAdded = [rest | line <- gmpPatchLines, Just rest <- [BS.stripPrefix "+" line], not ("++ " `BS.isPrefixOf` rest)]
          quoted line = [part | (index, part) <- zip [0 :: Int ..] (BS8.split '"' line), odd index]
          isDigits text = case BS8.uncons text of
            Just (leading, rest) -> leading `elem` ("01" :: String) && BS.length rest == 17 && "." `BS.isPrefixOf` rest && BS8.all isDigit (BS.drop 1 rest)
            Nothing -> False
          gmpDigits = concatMap (filter isDigits . quoted) gmpPatchAdded
          -- Whether a base's digits, read as a number, are log 2 / log base
          -- as this host's double precision computes it, to within
          -- digitsTolerance.
          nearValue :: Int -> BS.ByteString -> Bool
          nearValue base text = maybe False (\value -> abs (value - logBase (fromIntegral base) 2) < digitsTolerance) (readMaybe (BS8.unpack text))
      sequence
        [ runTest instantiates $
            assertEqual "derivations added" (sort addedNames) (sort (map nameOf added)),
          runTest "every derivation it adds but the fetches is i686-windows" $
            case [nameOf drv | drv <- added, not (isFetch drv), drvPlatform drv /= I686_Windows] of
              [] -> Pass
              named -> Fail ("not i686-windows: " <> T.intercalate ", " named),
          runTest "the leaves it adds are its six sources, fetched fixed-output" $
            assertEqual
              "leaves added"
              (sort archives, True)
              (sort (map nameOf addedLeaves), all (\drv -> isFetch drv && isFixedOutput drv) addedLeaves),
          runTest "it reads no input source but what evaluation writes as text, the stdenv's files, its two patches and the dirent header" $
            assertEqual
              "input sources not written as text"
              ["config.site", "config.site", patch, gmpPatch, "include", "include", "include", "setup.sh", "setup.sh"]
              (sort [spName path | drv <- added, path <- drvInputSrcs drv, not (wroteAsText path)]),
          runTest "gcc-core names its patch, its build inputs and its three libraries' archives where setup.sh and preConfigure read them" $
            assertEqual
              "patches, buildInputs and the archives"
              (sourcesNamed patch, [outputOf direntRoot, outputOf binutilsRoot], map (outputNamed . snd) mathArchives, True)
              (wordsOf "patches" gccCore, wordsOf "buildInputs" gccCore, [wordsOf variable gccCore | (variable, _) <- mathArchives], isJust gccCore),
          runTest "gcc-core names gmp's patch and applies it once preConfigure has unpacked gmp" $
            assertEqual
              "gmpPatch and the preConfigure lines from gmp's unpacking"
              ( sourcesNamed gmpPatch,
                ["tar xf \"$gmpSrc\"", "mv gmp-4.3.2 gmp", "(cd gmp && patch -p1 -i \"$gmpPatch\")"]
              )
              ( wordsOf "gmpPatch" gccCore,
                take 3 (dropWhile (/= "tar xf \"$gmpSrc\"") (maybe [] (map T.strip . T.lines . variableOf "preConfigure") gccCore))
              ),
          runTest "gmp's patch has gmp's base table written from fixed digits, log 2 / log base for each base from 2 to 256, in place of gmp's log, and changes nothing else" $
            assertEqual
              "the file it changes, the lines it removes, how many digit strings it adds, and whether each base's are its value"
              (["b/gen-bases.c"], removedByGmpPatch, length tableBases, [(base, True) | base <- tableBases])
              ( gmpPatchFiles,
                gmpPatchRemoved,
                length gmpDigits,
                [(base, nearValue base text) | (base, text) <- zip tableBases gmpDigits]
              ),
          runTest "gcc-core's flags name the target binutils is configured for and the outputs of the headers, binutils and the dirent library, and no sysroot" $
            assertEqual
              "flags"
              [ "--target=" <> target,
                "--with-headers=" <> outputOf headersRoot <> "/include",
                "--with-as=" <> outputOf binutilsRoot <> "/bin/" <> target <> "-as.exe",
                "--with-ld=" <> outputOf binutilsRoot <> "/bin/" <> target <> "-ld.exe",
                "--with-stage1-libs=" <> outputOf direntRoot <> "/lib/libdirent.a"
              ]
              (flagsWith ["--target=", "--with-headers", "--with-sysroot", "--with-build-sysroot", "--with-as=", "--with-ld=", "--with-stage1-libs="] gccCore),
          runTest "gcc-core builds and installs the compiler and libgcc, make running NIX_BUILD_CORES jobs with its makeFlags" $
            assertEqual
              "phases and parallelism"
              ( "make ${enableParallelBuilding:+-j$NIX_BUILD_CORES} $makeFlags all-gcc all-target-libgcc",
                "make ${enableParallelInstalling:+-j$NIX_BUILD_CORES} $makeFlags install-gcc install-target-libgcc",
                "1",
                "1"
              )
              ( maybe "" (variableOf "buildPhase") gccCore,
                maybe "" (variableOf "installPhase") gccCore,
                maybe "" (variableOf "enableParallelBuilding") gccCore,
                maybe "" (variableOf "enableParallelInstalling") gccCore
              ),
          runTestIO
            "the seed stdenv installs in parallel where a package builds in parallel and does not turn installing off"
            cwd
            ( "let mk = "
                <> windows
                <> ".seedStdenv.mkDerivation; installing = d: if d.drvAttrs ? enableParallelInstalling then (if d.drvAttrs.enableParallelInstalling then \"on\" else \"off\") else \"unset\"; in "
                <> "builtins.concatStringsSep \" \" (map installing [ (mk { name = \"a\"; enableParallelBuilding = true; }) (mk { name = \"b\"; enableParallelBuilding = true; enableParallelInstalling = false; }) (mk { name = \"c\"; }) ])"
            )
            (mkStr "on off unset"),
          runTest "gcc-core passes configure CFLAGS_FOR_TARGET=-O2 and make LIBGCC2_DEBUG_CFLAGS=-g0, the two settings that keep -g from libgcc" $
            assertEqual
              "the target's CFLAGS and makeFlags"
              (["CFLAGS_FOR_TARGET=-O2"], ["LIBGCC2_DEBUG_CFLAGS=-g0"])
              (flagsWith ["CFLAGS_FOR_TARGET="] gccCore, wordsOf "makeFlags" gccCore),
          runTest "mingw-w64-headers is configured in its own directory, for msvcrt, with no flag that moves the include directory gcc-core names" $
            assertEqual
              "preConfigure and flags"
              ("cd mingw-w64-headers", ["--with-default-msvcrt=msvcrt"])
              (maybe "" (variableOf "preConfigure") headers, wordsOf "configureFlags" headers)
        ]
  where
    instantiates :: Text
    instantiates = "the GCC rung's derivations instantiate, adding fifteen to the closure of the seed stdenv's tools and binutils"
    version :: Text
    version = "4.6.4"
    mingwVersion :: Text
    mingwVersion = "8.0.3"
    patch :: Text
    patch = "fix-tinycc-attribute.patch"
    gmpPatch :: Text
    gmpPatch = "gmp-bases-digits.patch"
    -- The lines gmp's patch takes out of its table generator: the math
    -- header, the double that holds each base's value, the value's
    -- computation with the build machine's log, and the two formats that
    -- print the double.
    removedByGmpPatch :: [BS.ByteString]
    removedByGmpPatch =
      [ "#include <math.h>",
        "",
        "double chars_per_bit_exactly;",
        "  chars_per_bit_exactly = 0.69314718055994530942 / log ((double) base);",
        "          printf (\"%u, %.16f, 0x%x },\\n\",",
        "          printf (\"%u, %.16f, CNST_LIMB(0x\","
      ]
    -- The bases gmp's table holds a value for, each with its digits.
    tableBases :: [Int]
    tableBases = [2 .. 256]
    -- How far a base's digits may be from the value this host computes:
    -- far outside their rounding to sixteen places and any host log's
    -- error, and far inside the gap between two bases' values.
    digitsTolerance :: Double
    digitsTolerance = 1.0e-15
    target :: Text
    target = "i686-w64-mingw32"
    -- The variable gcc-core's preConfigure unpacks each library from, and
    -- the archive it names.
    mathArchives :: [(Text, Text)]
    mathArchives = [("gmpSrc", "gmp-4.3.2.tar.gz"), ("mpfrSrc", "mpfr-2.4.2.tar.gz"), ("mpcSrc", "mpc-1.0.3.tar.gz")]
    archives :: [Text]
    archives = ["gcc-core-" <> version <> ".tar.gz", "mingw-w64-v" <> mingwVersion <> ".tar.bz2", "dirent.c"] ++ map snd mathArchives
    gccCoreRoot :: Text
    gccCoreRoot = "pkgs.gcc-core"
    headersRoot :: Text
    headersRoot = "pkgs.mingw-w64-headers"
    direntRoot :: Text
    direntRoot = "pkgs.mingw-w64-dirent"
    binutilsRoot :: Text
    binutilsRoot = "pkgs.binutils"
    builtBefore :: [Text]
    builtBefore = ["pkgs.busybox-w32", "pkgs.gnumake", "pkgs.tinycc-native.compiler", "pkgs.tinycc-native.libs", "pkgs.seedStdenv.applets", binutilsRoot]
    roots :: [Text]
    roots = builtBefore ++ [headersRoot, direntRoot, gccCoreRoot, "checks.version", "checks.answer"]
    -- Each kaem.runCommand adds its script, written by writeText as
    -- <name>-builder.
    addedNames :: [Text]
    addedNames =
      archives
        ++ ["mingw-w64-headers-" <> mingwVersion, "gcc-core-" <> version, "answer.sh"]
        ++ concat [[name, name <> "-builder"] | name <- ["mingw-w64-dirent-" <> mingwVersion, "gcc-core-get-version", "gcc-core-answer"]]

-- | @pkgs/windows@'s rung after GCC's core, read back as the build driver
-- reads it: what mingw-w64-crt, gcc46 and @test\/data\/gcc46.nix@'s checks
-- add to the closure of the GCC rung, which builds them.  The rung fetches
-- nothing: the runtime is compiled from mingw-w64-headers' archive, so it
-- adds no leaf, and it reads no input source but the stdenv's own files.
-- The runtime's configure takes the compiler and archiver the environment
-- names, and the stdenv's are tcc's, so its @preConfigure@ unsets them,
-- and @C_INCLUDE_PATH@, which GCC reads as well; its flags name the target
-- as the host and the headers' output as the sysroot, and its CFLAGS leave
-- out the @-g@ configure would add.  gcc46 compiles nothing: busybox's sh
-- copies gcc-core's output and the runtime's into one prefix, where the
-- driver, run from there, finds the start files and libraries every link
-- names.
testGcc46Windows :: IO [Bool]
testGcc46Windows = do
  putStrLn "pkgs/gcc46"
  cwd <- Dir.getCurrentDirectory
  storeDir <- evalNixIOStoreDir
  searchPaths <- parseNixPath (TE.encodeUtf8 (T.pack (cwd </> "data")))
  st <- newEvalStateWith testLogger unrestrictedPolicy searchPaths (testSessionStore storeDir) storeDir cwd
  let windows = "(import " <> nixQuotedPath (cwd </> "pkgs" </> "windows") <> ")"
      checks = "import " <> nixQuotedPath (cwd </> "test" </> "data" </> "gcc46.nix")
      source =
        T.concat
          [ "let pkgs = ",
            windows,
            "; checks = ",
            checks,
            "; in builtins.concatStringsSep \" \" [ ",
            T.unwords [path <> ".drvPath" | path <- roots],
            " ]"
          ]
  outcome <- case parseNix cwd testOrigin (TE.encodeUtf8 source) of
    Left err -> pure (Left (T.pack (show err)))
    Right parsed -> runEvalIO st $ do
      value <- evalParsed (pure (evalBaseEnv st)) parsed
      case value of
        VStr bytes _ -> do
          let rootOf = Map.fromList (zip roots (T.words (bytesText bytes)))
          before <- sessionClosure (mapMaybe (`Map.lookup` rootOf) builtBefore)
          full <- sessionClosure (Map.elems rootOf)
          pure (rootOf, before, full)
        other -> throwEvalError ("expected the drvPaths as one string, got " <> typeName other)
  writes <- readIORef (esStoreWriteCache st)
  case outcome of
    Left err -> sequence [runTest instantiates (Fail err)]
    Right (rootOf, before, full) -> do
      let added = Map.elems (Map.difference full before)
          nameOf drv = maybe "" bytesText (Map.lookup "name" (drvEnv drv))
          variableOf name drv = maybe "" bytesText (Map.lookup name (drvEnv drv))
          addedLeaves = filter (Map.null . drvInputDrvs) added
          wroteAsText path = fmap snd (Map.lookup (storePathToText defaultStoreDir path) writes) == Just WriteText
          derivationAt root = Map.lookup root rootOf >>= (`Map.lookup` full)
          crt = derivationAt crtRoot
          gcc46 = derivationAt gcc46Root
          headers = derivationAt headersRoot
          -- The output of a root's derivation, as a flag spells a store
          -- path, or a word no flag holds where the root is missing.
          outputOf root = maybe "<no output>" (storePathToText defaultStoreDir . doPath) (derivationAt root >>= listToMaybe . drvOutputs)
          wordsOf name = maybe [] (T.words . variableOf name)
          -- What preConfigure runs before it moves to the build directory.
          firstLine = maybe "" (T.takeWhile (/= '\n') . variableOf "preConfigure")
      sequence
        [ runTest instantiates $
            assertEqual "derivations added" (sort addedNames) (sort (map nameOf added)),
          runTest "every derivation it adds is i686-windows" $
            case [nameOf drv | drv <- added, drvPlatform drv /= I686_Windows] of
              [] -> Pass
              named -> Fail ("not i686-windows: " <> T.intercalate ", " named),
          runTest "it adds no leaf: every source it builds from is the GCC rung's" $
            assertEqual "leaves added" [] (map nameOf addedLeaves),
          runTest "it reads no input source but what evaluation writes as text and the stdenv's files" $
            assertEqual
              "input sources not written as text"
              ["config.site", "include", "setup.sh"]
              (sort [spName path | drv <- added, path <- drvInputSrcs drv, not (wroteAsText path)]),
          runTest "mingw-w64-crt is built from mingw-w64-headers' archive by gcc-core and binutils, with tcc's variables unset" $
            assertEqual
              "src, buildInputs and preConfigure's first line"
              (wordsOf "src" headers, [outputOf gccCoreRoot, outputOf binutilsRoot], "unset CC AR C_INCLUDE_PATH", isJust headers)
              (wordsOf "src" crt, wordsOf "buildInputs" crt, firstLine crt, isJust crt),
          runTest "mingw-w64-crt's flags name the target as the host, mingw-w64-headers' output as the sysroot, msvcrt and the 32-bit runtime alone, and CFLAGS with no -g" $
            assertEqual
              "flags"
              [ "--host=" <> target,
                "--with-sysroot=" <> outputOf headersRoot,
                "--with-default-msvcrt=msvcrt",
                "--enable-lib32",
                "--disable-lib64",
                "--disable-dependency-tracking",
                "CFLAGS=-O2"
              ]
              (wordsOf "configureFlags" crt),
          runTest "mingw-w64-crt builds with NIX_BUILD_CORES jobs and installs with one" $
            assertEqual
              "parallelism"
              ("1", "")
              (maybe "" (variableOf "enableParallelBuilding") crt, maybe "" (variableOf "enableParallelInstalling") crt),
          runTest "gcc46 compiles nothing: busybox's sh runs its script over gcc-core's output, mingw-w64-crt's and the target's name" $
            assertEqual
              "builder, input derivations and the variables its script reads"
              ( outputOf busyboxRoot <> "/bin/busybox.exe",
                sort (mapMaybe (`Map.lookup` rootOf) [busyboxRoot, gccCoreRoot, crtRoot]),
                [outputOf gccCoreRoot, outputOf crtRoot, target]
              )
              ( maybe "" (bytesText . drvBuilder) gcc46,
                maybe [] (sort . map (storePathToText defaultStoreDir) . Map.keys . drvInputDrvs) gcc46,
                [maybe "" (variableOf name) gcc46 | name <- ["gccCore", "crt", "target"]]
              )
        ]
  where
    instantiates :: Text
    instantiates = "the rung's derivations instantiate, adding eight to the closure of the GCC rung"
    target :: Text
    target = "i686-w64-mingw32"
    crtRoot :: Text
    crtRoot = "pkgs.mingw-w64-crt"
    gcc46Root :: Text
    gcc46Root = "pkgs.gcc46"
    gccCoreRoot :: Text
    gccCoreRoot = "pkgs.gcc-core"
    headersRoot :: Text
    headersRoot = "pkgs.mingw-w64-headers"
    binutilsRoot :: Text
    binutilsRoot = "pkgs.binutils"
    busyboxRoot :: Text
    busyboxRoot = "pkgs.busybox-w32"
    builtBefore :: [Text]
    builtBefore = [busyboxRoot, "pkgs.gnumake", "pkgs.tinycc-native.compiler", "pkgs.tinycc-native.libs", "pkgs.seedStdenv.applets", binutilsRoot, headersRoot, "pkgs.mingw-w64-dirent", gccCoreRoot]
    roots :: [Text]
    roots = builtBefore ++ [crtRoot, gcc46Root, "checks.crt", "checks.hello"]
    -- Each kaem.runCommand adds its script, written by writeText as
    -- <name>-builder.
    addedNames :: [Text]
    addedNames =
      ["mingw-w64-crt-8.0.3", "gcc-4.6.4", "crt.sh", "hello.sh"]
        ++ concat [[name, name <> "-builder"] | name <- ["mingw-w64-crt-contents", "gcc46-hello"]]

-- | @pkgs\/windows@'s C++ rung, read back as the build driver reads it: what
-- gcc46-cxx and @test\/data\/gcc46-cxx.nix@'s checks add to the closure of
-- the rung before it, gcc46, which builds it.  The rung is full-source only
-- if the one leaf it adds is GCC's g++ archive and it reads no input source
-- but the stdenv's own files and what evaluation writes as text; the rest
-- of GCC's tree is gcc-core's four archives, which the stdenv's
-- @setup.sh@ (@src@) and its @preConfigure@ (the other three) unpack from
-- the variables that name them.  Its configure is told by
-- flags where MinGW-w64's headers and runtime, the assembler and the
-- linker are, and which compiler, archiver and linker the build runs, and
-- its install which strip, so each has to name the output of the
-- derivation that builds it, or the target's name binutils and gcc46
-- install their programs under.  Every flag that sets a compiler's options
-- leaves out the @-g@ whose debugging information would name the build
-- directory, and every link and strip is told to write no time stamp.
testGcc46CxxWindows :: IO [Bool]
testGcc46CxxWindows = do
  putStrLn "pkgs/gcc46-cxx"
  cwd <- Dir.getCurrentDirectory
  storeDir <- evalNixIOStoreDir
  searchPaths <- parseNixPath (TE.encodeUtf8 (T.pack (cwd </> "data")))
  st <- newEvalStateWith testLogger unrestrictedPolicy searchPaths (testSessionStore storeDir) storeDir cwd
  let windows = "(import " <> nixQuotedPath (cwd </> "pkgs" </> "windows") <> ")"
      checks = "import " <> nixQuotedPath (cwd </> "test" </> "data" </> "gcc46-cxx.nix")
      source =
        T.concat
          [ "let pkgs = ",
            windows,
            "; checks = ",
            checks,
            "; in builtins.concatStringsSep \" \" [ ",
            T.unwords [path <> ".drvPath" | path <- roots],
            " ]"
          ]
  outcome <- case parseNix cwd testOrigin (TE.encodeUtf8 source) of
    Left err -> pure (Left (T.pack (show err)))
    Right parsed -> runEvalIO st $ do
      value <- evalParsed (pure (evalBaseEnv st)) parsed
      case value of
        VStr bytes _ -> do
          let rootOf = Map.fromList (zip roots (T.words (bytesText bytes)))
          before <- sessionClosure (mapMaybe (`Map.lookup` rootOf) builtBefore)
          full <- sessionClosure (Map.elems rootOf)
          pure (rootOf, before, full)
        other -> throwEvalError ("expected the drvPaths as one string, got " <> typeName other)
  writes <- readIORef (esStoreWriteCache st)
  case outcome of
    Left err -> sequence [runTest instantiates (Fail err)]
    Right (rootOf, before, full) -> do
      let added = Map.elems (Map.difference full before)
          nameOf drv = maybe "" bytesText (Map.lookup "name" (drvEnv drv))
          variableOf name drv = maybe "" bytesText (Map.lookup name (drvEnv drv))
          isFetch drv = drvBuilder drv == "builtin:fetchurl"
          isFixedOutput drv = not (all (T.null . doHash) (drvOutputs drv))
          addedLeaves = filter (Map.null . drvInputDrvs) added
          wroteAsText path = fmap snd (Map.lookup (storePathToText defaultStoreDir path) writes) == Just WriteText
          derivationAt root = Map.lookup root rootOf >>= (`Map.lookup` full)
          gccCxx = derivationAt gccCxxRoot
          gccCore = derivationAt gccCoreRoot
          -- The output of a root's derivation, as a flag spells a store
          -- path, or a word no flag holds where the root is missing.
          outputOf root = maybe "<no output>" (storePathToText defaultStoreDir . doPath) (derivationAt root >>= listToMaybe . drvOutputs)
          outputNamed name = [storePathToText defaultStoreDir (doPath out) | drv <- added, nameOf drv == name, out <- drvOutputs drv]
          wordsOf name = maybe [] (T.words . variableOf name)
          flagsWith prefixes drv = [flag | flag <- wordsOf "configureFlags" drv, any (`T.isPrefixOf` flag) prefixes]
      sequence
        [ runTest instantiates $
            assertEqual "derivations added" (sort addedNames) (sort (map nameOf added)),
          runTest "every derivation it adds but the fetch is i686-windows" $
            case [nameOf drv | drv <- added, not (isFetch drv), drvPlatform drv /= I686_Windows] of
              [] -> Pass
              named -> Fail ("not i686-windows: " <> T.intercalate ", " named),
          runTest "the one leaf it adds is GCC's g++ archive, fetched fixed-output" $
            assertEqual
              "leaves added"
              ([cxxArchive], True)
              (map nameOf addedLeaves, all (\drv -> isFetch drv && isFixedOutput drv) addedLeaves),
          runTest "it reads no input source but what evaluation writes as text, the stdenv's files and gmp's patch" $
            assertEqual
              "input sources not written as text"
              ["config.site", gmpPatch, "include", "setup.sh"]
              (sort [spName path | drv <- added, path <- drvInputSrcs drv, not (wroteAsText path)]),
          runTest "gcc46-cxx names gcc-core's four archives, gcc-core's patch of gmp and the g++ archive where setup.sh and preConfigure read them, with gcc46 and binutils as its build inputs and no patch of GCC's own" $
            assertEqual
              "the archives, gmp's patch, buildInputs and patches"
              ( [wordsOf variable gccCore | variable <- coreArchives] ++ [outputNamed cxxArchive],
                [outputOf gcc46Root, outputOf binutilsRoot],
                [],
                isJust gccCore
              )
              ( [wordsOf variable gccCxx | variable <- coreArchives ++ ["cxxSrc"]],
                wordsOf "buildInputs" gccCxx,
                wordsOf "patches" gccCxx,
                isJust gccCxx
              ),
          runTest "gcc46-cxx's flags name the target, the outputs of the headers, the runtime and binutils, and no sysroot" $
            assertEqual
              "flags"
              [ "--target=" <> target,
                "--with-headers=" <> outputOf headersRoot <> "/include",
                "--with-libs=" <> outputOf crtRoot <> "/lib",
                "--with-as=" <> outputOf binutilsRoot <> "/bin/" <> target <> "-as.exe",
                "--with-ld=" <> outputOf binutilsRoot <> "/bin/" <> target <> "-ld.exe"
              ]
              (flagsWith ["--target=", "--with-headers", "--with-libs", "--with-sysroot", "--with-build-sysroot", "--with-as=", "--with-ld="] gccCxx),
          runTest "gcc46-cxx builds with gcc46's driver and binutils' archiver and linker, by the target's names" $
            assertEqual
              "tools"
              ["CC=" <> target <> "-gcc", "AR=" <> target <> "-ar", "LD=" <> target <> "-ld"]
              (flagsWith ["CC=", "AR=", "LD=", "STRIP=", "CXX=", "RANLIB="] gccCxx),
          runTest "gcc46-cxx gives every link it makes --no-insert-timestamp, and LDFLAGS nothing else" $
            assertEqual
              "LDFLAGS"
              ["LDFLAGS=-Wl,--no-insert-timestamp"]
              (flagsWith ["LDFLAGS=", "LDFLAGS_FOR_BUILD="] gccCxx),
          runTest "gcc46-cxx's compiler options carry no -g, and libgcc's debugging flags are -g0" $
            assertEqual
              "the options and makeFlags"
              (["CFLAGS=-O1", "CXXFLAGS=-O1", "CFLAGS_FOR_TARGET=-O0", "CXXFLAGS_FOR_TARGET=-O0"], ["LIBGCC2_DEBUG_CFLAGS=-g0"])
              (flagsWith ["CFLAGS=", "CXXFLAGS=", "CFLAGS_FOR_TARGET=", "CXXFLAGS_FOR_TARGET="] gccCxx, wordsOf "makeFlags" gccCxx),
          runTest "gcc46-cxx builds C and C++, static, with no registry lookup" $
            assertEqual
              "languages, libraries and registry"
              ["--enable-languages=c,c++", "--disable-shared", "--disable-win32-registry"]
              (flagsWith ["--enable-languages=", "--enable-shared", "--disable-shared", "--enable-win32-registry", "--disable-win32-registry"] gccCxx),
          runTest "gcc46-cxx patches gmp's source with gmp's patch once preConfigure has unpacked it" $
            assertEqual
              "the preConfigure lines from gmp's unpacking"
              ["tar xf \"$gmpSrc\"", "mv gmp-4.3.2 gmp", "(cd gmp && patch -p1 -i \"$gmpPatch\")"]
              (take 3 (dropWhile (/= "tar xf \"$gmpSrc\"") (maybe [] (map T.strip . T.lines . variableOf "preConfigure") gccCxx))),
          runTest "gcc46-cxx unsets the stdenv's C_INCLUDE_PATH, and nothing else, before configure" $
            assertEqual
              "preConfigure's unset lines"
              ["unset C_INCLUDE_PATH"]
              (filter ("unset " `T.isPrefixOf`) (maybe [] (map T.strip . T.lines . variableOf "preConfigure") gccCxx)),
          runTest "gcc46-cxx builds all with NIX_BUILD_CORES jobs and installs with install-strip, whose strip keeps the zero time stamp ld writes" $
            assertEqual
              "phases and parallelism"
              ( "",
                "make ${enableParallelInstalling:+-j$NIX_BUILD_CORES} $makeFlags 'STRIP=" <> target <> "-strip -p' install-strip",
                "1",
                "1"
              )
              ( maybe "" (variableOf "buildPhase") gccCxx,
                maybe "" (variableOf "installPhase") gccCxx,
                maybe "" (variableOf "enableParallelBuilding") gccCxx,
                maybe "" (variableOf "enableParallelInstalling") gccCxx
              )
        ]
  where
    instantiates :: Text
    instantiates = "the C++ rung's derivations instantiate, adding eight to the closure of gcc46's rung"
    target :: Text
    target = "i686-w64-mingw32"
    cxxArchive :: Text
    cxxArchive = "gcc-g++-4.6.4.tar.gz"
    -- The variables gcc-core names its archives and gmp's patch in, which
    -- gcc46-cxx takes.
    coreArchives :: [Text]
    coreArchives = ["src", "gmpSrc", "gmpPatch", "mpfrSrc", "mpcSrc"]
    gmpPatch :: Text
    gmpPatch = "gmp-bases-digits.patch"
    gccCxxRoot :: Text
    gccCxxRoot = "pkgs.gcc46-cxx"
    gccCoreRoot :: Text
    gccCoreRoot = "pkgs.gcc-core"
    gcc46Root :: Text
    gcc46Root = "pkgs.gcc46"
    crtRoot :: Text
    crtRoot = "pkgs.mingw-w64-crt"
    headersRoot :: Text
    headersRoot = "pkgs.mingw-w64-headers"
    binutilsRoot :: Text
    binutilsRoot = "pkgs.binutils"
    builtBefore :: [Text]
    builtBefore = ["pkgs.busybox-w32", "pkgs.gnumake", "pkgs.tinycc-native.compiler", "pkgs.tinycc-native.libs", "pkgs.seedStdenv.applets", binutilsRoot, headersRoot, "pkgs.mingw-w64-dirent", gccCoreRoot, crtRoot, gcc46Root]
    roots :: [Text]
    roots = builtBefore ++ [gccCxxRoot, "checks.hello", "checks.stamps"]
    -- Each kaem.runCommand adds its script, written by writeText as
    -- <name>-builder.
    addedNames :: [Text]
    addedNames =
      [cxxArchive, "gcc-cxx-4.6.4", "hello.sh", "stamps.sh"]
        ++ concat [[name, name <> "-builder"] | name <- ["gcc46-cxx-hello", "gcc46-cxx-stamps"]]

-- | @pkgs\/windows@'s GCC 10 rung, read back as the build driver reads it:
-- what gcc10 and @test\/data\/gcc10.nix@'s checks add to the closure of the
-- rung before it, gcc46-cxx, which builds it.  The rung is full-source only
-- if the four leaves it adds are the archives of GCC, GMP, MPFR and MPC and
-- it reads no input source but the stdenv's own files, its patch and what
-- evaluation writes as text; the stdenv's @setup.sh@ unpacks GCC's (@src@) and
-- @preConfigure@ the other three, into the directories of GCC's tree its
-- configure builds them from.  Its configure is told by flags where
-- MinGW-w64's headers and runtime, the assembler and the linker are, and
-- which compilers, archiver and linker the build runs, and its install
-- which strip, so each has to name the output of the derivation that
-- builds it, or the target's name binutils and gcc46-cxx install their
-- programs under.  Every flag that sets a compiler's options leaves out
-- the @-g@ whose debugging information would name the build directory,
-- libgcc's own @-g@ is stripped after the install, and every link and
-- strip is told to write no time stamp.
testGcc10Windows :: IO [Bool]
testGcc10Windows = do
  putStrLn "pkgs/gcc10"
  cwd <- Dir.getCurrentDirectory
  storeDir <- evalNixIOStoreDir
  searchPaths <- parseNixPath (TE.encodeUtf8 (T.pack (cwd </> "data")))
  st <- newEvalStateWith testLogger unrestrictedPolicy searchPaths (testSessionStore storeDir) storeDir cwd
  let windows = "(import " <> nixQuotedPath (cwd </> "pkgs" </> "windows") <> ")"
      checks = "import " <> nixQuotedPath (cwd </> "test" </> "data" </> "gcc10.nix")
      source =
        T.concat
          [ "let pkgs = ",
            windows,
            "; checks = ",
            checks,
            "; in builtins.concatStringsSep \" \" [ ",
            T.unwords [path <> ".drvPath" | path <- roots],
            " ]"
          ]
  outcome <- case parseNix cwd testOrigin (TE.encodeUtf8 source) of
    Left err -> pure (Left (T.pack (show err)))
    Right parsed -> runEvalIO st $ do
      value <- evalParsed (pure (evalBaseEnv st)) parsed
      case value of
        VStr bytes _ -> do
          let rootOf = Map.fromList (zip roots (T.words (bytesText bytes)))
          before <- sessionClosure (mapMaybe (`Map.lookup` rootOf) builtBefore)
          full <- sessionClosure (Map.elems rootOf)
          pure (rootOf, before, full)
        other -> throwEvalError ("expected the drvPaths as one string, got " <> typeName other)
  writes <- readIORef (esStoreWriteCache st)
  case outcome of
    Left err -> sequence [runTest instantiates (Fail err)]
    Right (rootOf, before, full) -> do
      let added = Map.elems (Map.difference full before)
          nameOf drv = maybe "" bytesText (Map.lookup "name" (drvEnv drv))
          variableOf name drv = maybe "" bytesText (Map.lookup name (drvEnv drv))
          isFetch drv = drvBuilder drv == "builtin:fetchurl"
          isFixedOutput drv = not (all (T.null . doHash) (drvOutputs drv))
          addedLeaves = filter (Map.null . drvInputDrvs) added
          wroteAsText path = fmap snd (Map.lookup (storePathToText defaultStoreDir path) writes) == Just WriteText
          derivationAt root = Map.lookup root rootOf >>= (`Map.lookup` full)
          gcc10 = derivationAt gcc10Root
          -- The output of a root's derivation, as a flag spells a store
          -- path, or a word no flag holds where the root is missing.
          outputOf root = maybe "<no output>" (storePathToText defaultStoreDir . doPath) (derivationAt root >>= listToMaybe . drvOutputs)
          outputNamed name = [storePathToText defaultStoreDir (doPath out) | drv <- added, nameOf drv == name, out <- drvOutputs drv]
          sourcesNamed name = [storePathToText defaultStoreDir path | drv <- maybeToList gcc10, path <- drvInputSrcs drv, spName path == name]
          wordsOf name = maybe [] (T.words . variableOf name)
          linesOf name = maybe [] (map T.strip . T.lines . variableOf name)
          flagsWith prefixes drv = [flag | flag <- wordsOf "configureFlags" drv, any (`T.isPrefixOf` flag) prefixes]
      sequence
        [ runTest instantiates $
            assertEqual "derivations added" (sort addedNames) (sort (map nameOf added)),
          runTest "every derivation it adds but the fetches is i686-windows" $
            case [nameOf drv | drv <- added, not (isFetch drv), drvPlatform drv /= I686_Windows] of
              [] -> Pass
              named -> Fail ("not i686-windows: " <> T.intercalate ", " named),
          runTest "the four leaves it adds are the archives of GCC, GMP, MPFR and MPC, fetched fixed-output" $
            assertEqual
              "leaves added"
              (sort (map snd archives), True)
              (sort (map nameOf addedLeaves), all (\drv -> isFetch drv && isFixedOutput drv) addedLeaves),
          runTest "it reads no input source but what evaluation writes as text, the stdenv's files and its patch" $
            assertEqual
              "input sources not written as text"
              (sort ["config.site", "include", "setup.sh", patch])
              (sort [spName path | drv <- added, path <- drvInputSrcs drv, not (wroteAsText path)]),
          runTest "gcc10 names its patch and its four archives where setup.sh and preConfigure read them, with gcc46-cxx and binutils as its build inputs" $
            assertEqual
              "the archives, buildInputs and patches"
              ( [outputNamed archive | (_, archive) <- archives],
                [outputOf gccCxxRoot, outputOf binutilsRoot],
                sourcesNamed patch
              )
              ( [wordsOf variable gcc10 | (variable, _) <- archives],
                wordsOf "buildInputs" gcc10,
                wordsOf "patches" gcc10
              ),
          runTest "gcc10's preConfigure moves GMP, MPFR and MPC to the directories GCC's configure builds them from" $
            assertEqual
              "preConfigure's mv lines"
              ["mv " <> library <> "-" <> libraryVersion <> " " <> library | (library, libraryVersion) <- libraries]
              (filter ("mv " `T.isPrefixOf`) (linesOf "preConfigure" gcc10)),
          runTest "gcc10's flags name the target, the outputs of the headers, the runtime and binutils, and no sysroot" $
            assertEqual
              "flags"
              [ "--target=" <> target,
                "--with-headers=" <> outputOf headersRoot <> "/include",
                "--with-libs=" <> outputOf crtRoot <> "/lib",
                "--with-as=" <> outputOf binutilsRoot <> "/bin/" <> target <> "-as.exe",
                "--with-ld=" <> outputOf binutilsRoot <> "/bin/" <> target <> "-ld.exe"
              ]
              (flagsWith ["--target=", "--with-headers", "--with-libs", "--with-sysroot", "--with-build-sysroot", "--with-native-system-header-dir", "--with-as=", "--with-ld="] gcc10),
          runTest "gcc10 builds with gcc46-cxx's drivers and binutils' archiver and linker, by the target's names" $
            assertEqual
              "tools"
              ["CC=" <> target <> "-gcc", "CXX=" <> target <> "-g++", "AR=" <> target <> "-ar", "LD=" <> target <> "-ld"]
              (flagsWith ["CC=", "CXX=", "AR=", "LD=", "STRIP=", "RANLIB="] gcc10),
          runTest "gcc10 gives every link it makes --no-insert-timestamp, and LDFLAGS nothing else" $
            assertEqual
              "LDFLAGS"
              ["LDFLAGS=-Wl,--no-insert-timestamp"]
              (flagsWith ["LDFLAGS=", "LDFLAGS_FOR_BUILD="] gcc10),
          runTest "gcc10's compiler options carry no -g, and it gives make no flags" $
            assertEqual
              "the options and makeFlags"
              (["CFLAGS=-O1", "CXXFLAGS=-O1", "CFLAGS_FOR_TARGET=-O2", "CXXFLAGS_FOR_TARGET=-O0"], [])
              (flagsWith ["CFLAGS=", "CXXFLAGS=", "CFLAGS_FOR_TARGET=", "CXXFLAGS_FOR_TARGET="] gcc10, wordsOf "makeFlags" gcc10),
          runTest "gcc10 builds C and C++, static, with no registry lookup" $
            assertEqual
              "languages, libraries and registry"
              ["--enable-languages=c,c++", "--disable-shared", "--disable-win32-registry"]
              (flagsWith ["--enable-languages=", "--enable-shared", "--disable-shared", "--enable-win32-registry", "--disable-win32-registry"] gcc10),
          runTest "gcc10 unsets the stdenv's C_INCLUDE_PATH, and nothing else, before configure" $
            assertEqual
              "preConfigure's unset lines"
              ["unset C_INCLUDE_PATH"]
              (filter ("unset " `T.isPrefixOf`) (linesOf "preConfigure" gcc10)),
          runTest "gcc10 builds all with NIX_BUILD_CORES jobs, installs with install-strip, whose strip keeps the zero time stamp ld writes, and strips libgcc's debugging information" $
            assertEqual
              "phases and parallelism"
              ( "",
                [ "make ${enableParallelInstalling:+-j$NIX_BUILD_CORES} 'STRIP=" <> target <> "-strip -p' install-strip",
                  "libgccDir=$out/lib/gcc/" <> target <> "/" <> version,
                  target <> "-strip -p --strip-debug \"$libgccDir/libgcc.a\" \"$libgccDir/libgcov.a\" \"$libgccDir/crtfastmath.o\""
                ],
                "1",
                "1"
              )
              ( maybe "" (variableOf "buildPhase") gcc10,
                linesOf "installPhase" gcc10,
                maybe "" (variableOf "enableParallelBuilding") gcc10,
                maybe "" (variableOf "enableParallelInstalling") gcc10
              )
        ]
  where
    instantiates :: Text
    instantiates = "the GCC 10 rung's derivations instantiate, adding fourteen to the closure of gcc46-cxx's rung"
    target :: Text
    target = "i686-w64-mingw32"
    version :: Text
    version = "10.4.0"
    -- GMP, MPFR and MPC, each with the release of it the rung builds in
    -- GCC's tree.
    libraries :: [(Text, Text)]
    libraries = [("gmp", "6.2.1"), ("mpfr", "4.2.2"), ("mpc", "1.4.1")]
    -- The variable the rung names each archive in, with the archive's
    -- name.
    archives :: [(Text, Text)]
    archives =
      ("src", "gcc-" <> version <> ".tar.xz")
        : [(library <> "Src", library <> "-" <> libraryVersion <> ".tar.xz") | (library, libraryVersion) <- libraries]
    -- GCC's commit that has libcpp's configure look the archiver up.
    patch :: Text
    patch = "libcpp-configure-ar.patch"
    gcc10Root :: Text
    gcc10Root = "pkgs.gcc10"
    gccCxxRoot :: Text
    gccCxxRoot = "pkgs.gcc46-cxx"
    crtRoot :: Text
    crtRoot = "pkgs.mingw-w64-crt"
    headersRoot :: Text
    headersRoot = "pkgs.mingw-w64-headers"
    binutilsRoot :: Text
    binutilsRoot = "pkgs.binutils"
    builtBefore :: [Text]
    builtBefore = ["pkgs.busybox-w32", "pkgs.gnumake", "pkgs.tinycc-native.compiler", "pkgs.tinycc-native.libs", "pkgs.seedStdenv.applets", binutilsRoot, headersRoot, "pkgs.mingw-w64-dirent", "pkgs.gcc-core", crtRoot, "pkgs.gcc46", gccCxxRoot]
    roots :: [Text]
    roots = builtBefore ++ [gcc10Root, "checks.hello-c", "checks.hello-cxx", "checks.stamps"]
    -- Each kaem.runCommand adds its script, written by writeText as
    -- <name>-builder.
    addedNames :: [Text]
    addedNames =
      map snd archives
        ++ ["gcc-" <> version, "hello-c.sh", "hello-cxx.sh", "stamps.sh"]
        ++ concat [[name, name <> "-builder"] | name <- ["gcc10-hello-c", "gcc10-hello-cxx", "gcc10-stamps"]]

-- | @pkgs\/windows@'s current MinGW-w64, read back as the build driver
-- reads it: what mingw-w64-headers-latest, mingw-w64-crt-latest and
-- @test\/data\/mingw-w64-latest.nix@'s checks add to the closure of gcc10,
-- which compiles the runtime.  The rung is full-source only if the one
-- leaf it adds is the release's archive and it reads no input source but
-- the stdenv's own files and what evaluation writes as text.  The headers
-- install from the archive's own directory, for msvcrt.dll.  The runtime
-- is compiled from the headers' archive, against the headers' output,
-- which its flags name as the sysroot; its configure takes the compiler
-- and archiver the environment names, and the stdenv's are tcc's, so its
-- @preConfigure@ unsets them, and @C_INCLUDE_PATH@, which GCC reads as
-- well; and its CFLAGS leave out the @-g@ configure would add.
testMingwW64LatestWindows :: IO [Bool]
testMingwW64LatestWindows = do
  putStrLn "pkgs/mingw-w64-latest"
  cwd <- Dir.getCurrentDirectory
  storeDir <- evalNixIOStoreDir
  searchPaths <- parseNixPath (TE.encodeUtf8 (T.pack (cwd </> "data")))
  st <- newEvalStateWith testLogger unrestrictedPolicy searchPaths (testSessionStore storeDir) storeDir cwd
  let windows = "(import " <> nixQuotedPath (cwd </> "pkgs" </> "windows") <> ")"
      checks = "import " <> nixQuotedPath (cwd </> "test" </> "data" </> "mingw-w64-latest.nix")
      source =
        T.concat
          [ "let pkgs = ",
            windows,
            "; checks = ",
            checks,
            "; in builtins.concatStringsSep \" \" [ ",
            T.unwords [path <> ".drvPath" | path <- roots],
            " ]"
          ]
  outcome <- case parseNix cwd testOrigin (TE.encodeUtf8 source) of
    Left err -> pure (Left (T.pack (show err)))
    Right parsed -> runEvalIO st $ do
      value <- evalParsed (pure (evalBaseEnv st)) parsed
      case value of
        VStr bytes _ -> do
          let rootOf = Map.fromList (zip roots (T.words (bytesText bytes)))
          before <- sessionClosure (mapMaybe (`Map.lookup` rootOf) builtBefore)
          full <- sessionClosure (Map.elems rootOf)
          pure (rootOf, before, full)
        other -> throwEvalError ("expected the drvPaths as one string, got " <> typeName other)
  writes <- readIORef (esStoreWriteCache st)
  case outcome of
    Left err -> sequence [runTest instantiates (Fail err)]
    Right (rootOf, before, full) -> do
      let added = Map.elems (Map.difference full before)
          nameOf drv = maybe "" bytesText (Map.lookup "name" (drvEnv drv))
          variableOf name drv = maybe "" bytesText (Map.lookup name (drvEnv drv))
          isFetch drv = drvBuilder drv == "builtin:fetchurl"
          isFixedOutput drv = not (all (T.null . doHash) (drvOutputs drv))
          addedLeaves = filter (Map.null . drvInputDrvs) added
          wroteAsText path = fmap snd (Map.lookup (storePathToText defaultStoreDir path) writes) == Just WriteText
          derivationAt root = Map.lookup root rootOf >>= (`Map.lookup` full)
          headers = derivationAt headersRoot
          crt = derivationAt crtRoot
          -- The output of a root's derivation, as a flag spells a store
          -- path, or a word no flag holds where the root is missing.
          outputOf root = maybe "<no output>" (storePathToText defaultStoreDir . doPath) (derivationAt root >>= listToMaybe . drvOutputs)
          wordsOf name = maybe [] (T.words . variableOf name)
          -- What preConfigure runs before it moves to the build directory.
          firstLine = maybe "" (T.takeWhile (/= '\n') . variableOf "preConfigure")
      sequence
        [ runTest instantiates $
            assertEqual "derivations added" (sort addedNames) (sort (map nameOf added)),
          runTest "every derivation it adds but the fetch is i686-windows" $
            case [nameOf drv | drv <- added, not (isFetch drv), drvPlatform drv /= I686_Windows] of
              [] -> Pass
              named -> Fail ("not i686-windows: " <> T.intercalate ", " named),
          runTest "the one leaf it adds is MinGW-w64's release archive, fetched fixed-output" $
            assertEqual
              "leaves added"
              ([archive], True)
              (map nameOf addedLeaves, all (\drv -> isFetch drv && isFixedOutput drv) addedLeaves),
          runTest "it reads no input source but what evaluation writes as text and the stdenv's files" $
            assertEqual
              "input sources not written as text"
              ["config.site", "include", "setup.sh"]
              (sort (map spName (nub [path | drv <- added, path <- drvInputSrcs drv, not (wroteAsText path)]))),
          runTest "mingw-w64-headers-latest is configured in its own directory, for msvcrt, from the archive" $
            assertEqual
              "preConfigure, flags and src"
              ("cd mingw-w64-headers", ["--with-default-msvcrt=msvcrt"], [archive])
              ( maybe "" (variableOf "preConfigure") headers,
                wordsOf "configureFlags" headers,
                [nameOf drv | drv <- addedLeaves, any ((`elem` wordsOf "src" headers) . storePathToText defaultStoreDir . doPath) (drvOutputs drv)]
              ),
          runTest "mingw-w64-crt-latest is built from mingw-w64-headers-latest's archive by gcc10 and binutils, with tcc's variables unset" $
            assertEqual
              "src, buildInputs and preConfigure's first line"
              (wordsOf "src" headers, [outputOf gcc10Root, outputOf binutilsRoot], "unset CC AR C_INCLUDE_PATH", isJust headers)
              (wordsOf "src" crt, wordsOf "buildInputs" crt, firstLine crt, isJust crt),
          runTest "mingw-w64-crt-latest's flags name the target as the host, mingw-w64-headers-latest's output as the sysroot, msvcrt and the 32-bit runtime alone, and CFLAGS with no -g" $
            assertEqual
              "flags"
              [ "--host=" <> target,
                "--with-sysroot=" <> outputOf headersRoot,
                "--with-default-msvcrt=msvcrt",
                "--enable-lib32",
                "--disable-lib64",
                "--disable-dependency-tracking",
                "CFLAGS=-O2"
              ]
              (wordsOf "configureFlags" crt),
          runTest "mingw-w64-crt-latest builds and installs with NIX_BUILD_CORES jobs" $
            assertEqual
              "parallelism"
              ("1", "1")
              (maybe "" (variableOf "enableParallelBuilding") crt, maybe "" (variableOf "enableParallelInstalling") crt)
        ]
  where
    instantiates :: Text
    instantiates = "the current MinGW-w64's derivations instantiate, adding nine to the closure of gcc10"
    target :: Text
    target = "i686-w64-mingw32"
    version :: Text
    version = "14.0.0"
    archive :: Text
    archive = "mingw-w64-v" <> version <> ".tar.bz2"
    headersRoot :: Text
    headersRoot = "pkgs.mingw-w64-headers-latest"
    crtRoot :: Text
    crtRoot = "pkgs.mingw-w64-crt-latest"
    gcc10Root :: Text
    gcc10Root = "pkgs.gcc10"
    binutilsRoot :: Text
    binutilsRoot = "pkgs.binutils"
    builtBefore :: [Text]
    builtBefore = ["pkgs.busybox-w32", "pkgs.gnumake", "pkgs.tinycc-native.compiler", "pkgs.tinycc-native.libs", "pkgs.seedStdenv.applets", binutilsRoot, gcc10Root]
    roots :: [Text]
    roots = builtBefore ++ [headersRoot, crtRoot, "checks.crt", "checks.hello"]
    -- Each kaem.runCommand adds its script, written by writeText as
    -- <name>-builder.
    addedNames :: [Text]
    addedNames =
      [archive, "mingw-w64-headers-" <> version, "mingw-w64-crt-" <> version, "crt.sh", "hello.sh"]
        ++ concat [[name, name <> "-builder"] | name <- ["mingw-w64-latest-crt", "mingw-w64-latest-hello"]]

-- | @pkgs\/windows@'s GCC 16 rung, read back as the build driver reads it:
-- what gcc-latest and @test\/data\/gcc-latest.nix@'s checks add to the
-- closure of the rungs before it, gcc10, which builds it, and the current
-- MinGW-w64, its target's headers and runtime.  The rung is full-source
-- only if the three leaves it adds are the archives of GCC, GMP and MPC,
-- MPFR's being gcc10's, and it reads no input source but the stdenv's own
-- files and what evaluation writes as text; the stdenv's @setup.sh@
-- unpacks GCC's (@src@) and @preConfigure@ the other three, into the
-- directories of GCC's tree its configure builds them from.  Its
-- configure is told by flags where MinGW-w64's headers and runtime, the
-- assembler and the linker are, and which compilers, archiver and linker
-- the build runs, and its install which strip, so each has to name the
-- output of the derivation that builds it, or the target's name binutils
-- and gcc10 install their programs under.  Every flag that sets a
-- compiler's options leaves out the @-g@ whose debugging information
-- would name the build directory, libgcc's own @-g@ is stripped after
-- the install, every link and strip is told to write no time stamp, and
-- the build leaves the configure arguments, which name store paths, out
-- of the compiler.
testGccLatestWindows :: IO [Bool]
testGccLatestWindows = do
  putStrLn "pkgs/gcc-latest"
  cwd <- Dir.getCurrentDirectory
  storeDir <- evalNixIOStoreDir
  searchPaths <- parseNixPath (TE.encodeUtf8 (T.pack (cwd </> "data")))
  st <- newEvalStateWith testLogger unrestrictedPolicy searchPaths (testSessionStore storeDir) storeDir cwd
  let windows = "(import " <> nixQuotedPath (cwd </> "pkgs" </> "windows") <> ")"
      checks = "import " <> nixQuotedPath (cwd </> "test" </> "data" </> "gcc-latest.nix")
      source =
        T.concat
          [ "let pkgs = ",
            windows,
            "; checks = ",
            checks,
            "; in builtins.concatStringsSep \" \" [ ",
            T.unwords [path <> ".drvPath" | path <- roots],
            " ]"
          ]
  outcome <- case parseNix cwd testOrigin (TE.encodeUtf8 source) of
    Left err -> pure (Left (T.pack (show err)))
    Right parsed -> runEvalIO st $ do
      value <- evalParsed (pure (evalBaseEnv st)) parsed
      case value of
        VStr bytes _ -> do
          let rootOf = Map.fromList (zip roots (T.words (bytesText bytes)))
          before <- sessionClosure (mapMaybe (`Map.lookup` rootOf) builtBefore)
          full <- sessionClosure (Map.elems rootOf)
          pure (rootOf, before, full)
        other -> throwEvalError ("expected the drvPaths as one string, got " <> typeName other)
  writes <- readIORef (esStoreWriteCache st)
  case outcome of
    Left err -> sequence [runTest instantiates (Fail err)]
    Right (rootOf, before, full) -> do
      let added = Map.elems (Map.difference full before)
          nameOf drv = maybe "" bytesText (Map.lookup "name" (drvEnv drv))
          variableOf name drv = maybe "" bytesText (Map.lookup name (drvEnv drv))
          isFetch drv = drvBuilder drv == "builtin:fetchurl"
          isFixedOutput drv = not (all (T.null . doHash) (drvOutputs drv))
          addedLeaves = filter (Map.null . drvInputDrvs) added
          wroteAsText path = fmap snd (Map.lookup (storePathToText defaultStoreDir path) writes) == Just WriteText
          derivationAt root = Map.lookup root rootOf >>= (`Map.lookup` full)
          gccLatest = derivationAt gccLatestRoot
          -- The output of a root's derivation, as a flag spells a store
          -- path, or a word no flag holds where the root is missing.
          outputOf root = maybe "<no output>" (storePathToText defaultStoreDir . doPath) (derivationAt root >>= listToMaybe . drvOutputs)
          outputNamed name = [storePathToText defaultStoreDir (doPath out) | drv <- Map.elems full, nameOf drv == name, out <- drvOutputs drv]
          wordsOf name = maybe [] (T.words . variableOf name)
          linesOf name = maybe [] (map T.strip . T.lines . variableOf name)
          flagsWith prefixes drv = [flag | flag <- wordsOf "configureFlags" drv, any (`T.isPrefixOf` flag) prefixes]
      sequence
        [ runTest instantiates $
            assertEqual "derivations added" (sort addedNames) (sort (map nameOf added)),
          runTest "every derivation it adds but the fetches is i686-windows" $
            case [nameOf drv | drv <- added, not (isFetch drv), drvPlatform drv /= I686_Windows] of
              [] -> Pass
              named -> Fail ("not i686-windows: " <> T.intercalate ", " named),
          runTest "the three leaves it adds are the archives of GCC, GMP and MPC, fetched fixed-output" $
            assertEqual
              "leaves added"
              (sort [archive | (variable, archive) <- archives, variable /= "mpfrSrc"], True)
              (sort (map nameOf addedLeaves), all (\drv -> isFetch drv && isFixedOutput drv) addedLeaves),
          runTest "it reads no input source but what evaluation writes as text and the stdenv's files" $
            assertEqual
              "input sources not written as text"
              ["config.site", "include", "setup.sh"]
              (sort (map spName (nub [path | drv <- added, path <- drvInputSrcs drv, not (wroteAsText path)]))),
          runTest "gcc-latest names its four archives where setup.sh and preConfigure read them, with gcc10 and binutils as its build inputs and no patch" $
            assertEqual
              "the archives, buildInputs and patches"
              ( [outputNamed archive | (_, archive) <- archives],
                [outputOf gcc10Root, outputOf binutilsRoot],
                []
              )
              ( [wordsOf variable gccLatest | (variable, _) <- archives],
                wordsOf "buildInputs" gccLatest,
                wordsOf "patches" gccLatest
              ),
          runTest "gcc-latest's preConfigure moves GMP, MPFR and MPC to the directories GCC's configure builds them from" $
            assertEqual
              "preConfigure's mv lines"
              ["mv " <> library <> "-" <> libraryVersion <> " " <> library | (library, libraryVersion, _) <- libraries]
              (filter ("mv " `T.isPrefixOf`) (linesOf "preConfigure" gccLatest)),
          runTest "gcc-latest's flags name the target, the outputs of the current MinGW-w64's headers and runtime and of binutils, and no sysroot" $
            assertEqual
              "flags"
              [ "--target=" <> target,
                "--with-headers=" <> outputOf headersRoot <> "/include",
                "--with-libs=" <> outputOf crtRoot <> "/lib",
                "--with-as=" <> outputOf binutilsRoot <> "/bin/" <> target <> "-as.exe",
                "--with-ld=" <> outputOf binutilsRoot <> "/bin/" <> target <> "-ld.exe"
              ]
              (flagsWith ["--target=", "--with-headers", "--with-libs", "--with-sysroot", "--with-build-sysroot", "--with-native-system-header-dir", "--with-as=", "--with-ld="] gccLatest),
          runTest "gcc-latest builds with gcc10's drivers and binutils' archiver, linker and resource compiler, by the target's names, the resource compiler named in the environment and run with --use-temp-file" $
            assertEqual
              "tools, and WINDRES"
              (["CC=" <> target <> "-gcc", "CXX=" <> target <> "-g++", "AR=" <> target <> "-ar", "LD=" <> target <> "-ld"], target <> "-windres --use-temp-file")
              (flagsWith ["CC=", "CXX=", "AR=", "LD=", "STRIP=", "RANLIB=", "WINDRES="] gccLatest, maybe "" (variableOf "WINDRES") gccLatest),
          runTest "gcc-latest gives every link it makes --no-insert-timestamp, and LDFLAGS nothing else" $
            assertEqual
              "LDFLAGS"
              ["LDFLAGS=-Wl,--no-insert-timestamp"]
              (flagsWith ["LDFLAGS=", "LDFLAGS_FOR_BUILD="] gccLatest),
          runTest "gcc-latest's compiler options carry no -g, and it gives make no flags" $
            assertEqual
              "the options and makeFlags"
              (["CFLAGS=-O1", "CXXFLAGS=-O1", "CFLAGS_FOR_TARGET=-O2", "CXXFLAGS_FOR_TARGET=-O0"], [])
              (flagsWith ["CFLAGS=", "CXXFLAGS=", "CFLAGS_FOR_TARGET=", "CXXFLAGS_FOR_TARGET="] gccLatest, wordsOf "makeFlags" gccLatest),
          runTest "gcc-latest builds C and C++, static, with a build ID on every link, no default PIE and no registry lookup" $
            assertEqual
              "languages, libraries, links and registry"
              ["--enable-languages=c,c++", "--enable-linker-build-id", "--disable-shared", "--disable-win32-registry"]
              (flagsWith ["--enable-languages=", "--enable-linker-build-id", "--enable-default-pie", "--enable-shared", "--disable-shared", "--enable-win32-registry", "--disable-win32-registry"] gccLatest),
          runTest "gcc-latest unsets the stdenv's C_INCLUDE_PATH, and nothing else, before configure" $
            assertEqual
              "preConfigure's unset lines"
              ["unset C_INCLUDE_PATH"]
              (filter ("unset " `T.isPrefixOf`) (linesOf "preConfigure" gccLatest)),
          runTest "gcc-latest drops the configure arguments from the top-level Makefile, builds with NIX_BUILD_CORES jobs, installs with install-strip, whose strip keeps the zero time stamp ld writes, strips libgcc's debugging information, removes what ng.nix removes and libstdc++'s gdb script, and removes the directories that leaves empty" $
            assertEqual
              "phases and parallelism"
              ( [ "sed -e '/TOPLEVEL_CONFIGURE_ARGUMENTS=/d' -i Makefile",
                  "make ${enableParallelBuilding:+-j$NIX_BUILD_CORES}"
                ],
                [ "make ${enableParallelInstalling:+-j$NIX_BUILD_CORES} 'STRIP=" <> target <> "-strip -p' install-strip",
                  "libgccDir=$out/lib/gcc/" <> target <> "/" <> version,
                  target <> "-strip -p --strip-debug \"$libgccDir\"/*.a \"$libgccDir\"/*.o",
                  "rm -r \"$out/libexec/gcc/" <> target <> "/" <> version <> "/install-tools\" \"$libgccDir/install-tools\"",
                  "rm -rf \"$out/share/gcc-" <> version <> "/python\" \"$out/share/man\" \"$out/share/info\"",
                  "rm \"$out/" <> target <> "/lib/libstdc++.a-gdb.py\"",
                  "rmdir \"$out/share/gcc-" <> version <> "\" \"$out/share\""
                ],
                "1",
                "1"
              )
              ( linesOf "buildPhase" gccLatest,
                linesOf "installPhase" gccLatest,
                maybe "" (variableOf "enableParallelBuilding") gccLatest,
                maybe "" (variableOf "enableParallelInstalling") gccLatest
              )
        ]
  where
    instantiates :: Text
    instantiates = "the GCC 16 rung's derivations instantiate, adding thirteen to the closure of gcc10 and the current MinGW-w64"
    target :: Text
    target = "i686-w64-mingw32"
    version :: Text
    version = "16.2.0"
    -- GMP, MPFR and MPC, each with the release of it the rung builds in
    -- GCC's tree and the compression of its archive.
    libraries :: [(Text, Text, Text)]
    libraries = [("gmp", "6.3.0", "xz"), ("mpfr", "4.2.2", "xz"), ("mpc", "1.3.1", "gz")]
    -- The variable the rung names each archive in, with the archive's
    -- name.
    archives :: [(Text, Text)]
    archives =
      ("src", "gcc-" <> version <> ".tar.xz")
        : [(library <> "Src", library <> "-" <> libraryVersion <> ".tar." <> compression) | (library, libraryVersion, compression) <- libraries]
    gccLatestRoot :: Text
    gccLatestRoot = "pkgs.gcc-latest"
    gcc10Root :: Text
    gcc10Root = "pkgs.gcc10"
    crtRoot :: Text
    crtRoot = "pkgs.mingw-w64-crt-latest"
    headersRoot :: Text
    headersRoot = "pkgs.mingw-w64-headers-latest"
    binutilsRoot :: Text
    binutilsRoot = "pkgs.binutils"
    builtBefore :: [Text]
    builtBefore = ["pkgs.busybox-w32", "pkgs.gnumake", "pkgs.tinycc-native.compiler", "pkgs.tinycc-native.libs", "pkgs.seedStdenv.applets", binutilsRoot, gcc10Root, headersRoot, crtRoot]
    roots :: [Text]
    roots = builtBefore ++ [gccLatestRoot, "checks.hello-c", "checks.hello-cxx", "checks.stamps"]
    -- Each kaem.runCommand adds its script, written by writeText as
    -- <name>-builder.
    addedNames :: [Text]
    addedNames =
      [archive | (variable, archive) <- archives, variable /= "mpfrSrc"]
        ++ ["gcc-" <> version, "hello-c.sh", "hello-cxx.sh", "stamps.sh"]
        ++ concat [[name, name <> "-builder"] | name <- ["gcc-latest-hello-c", "gcc-latest-hello-cxx", "gcc-latest-stamps"]]

-- | @pkgs\/windows@'s x86_64 binutils, read back as the build driver reads
-- it: what binutils-x86_64 and @test\/data\/binutils-x86_64.nix@'s checks
-- add to the closure of gcc-latest and binutils, which build it.  The
-- rung is full-source only if the one leaf it adds is the archive nixpkgs
-- fetches and it reads no input source but the stdenv's own files and
-- what evaluation writes as text.  Its configure is told the target the
-- x86_64 compilers are built for, so the tools install under the name
-- they look them up by; the build runs gcc-latest's compiler and
-- binutils' archiver and linker by the name of their target, since the
-- stdenv's CC, AR and LD are tcc's; its CFLAGS leave out the @-g@
-- configure would add, and every link is told to write no time stamp.
testBinutilsX8664Windows :: IO [Bool]
testBinutilsX8664Windows = do
  putStrLn "pkgs/binutils-x86_64"
  cwd <- Dir.getCurrentDirectory
  storeDir <- evalNixIOStoreDir
  searchPaths <- parseNixPath (TE.encodeUtf8 (T.pack (cwd </> "data")))
  st <- newEvalStateWith testLogger unrestrictedPolicy searchPaths (testSessionStore storeDir) storeDir cwd
  let windows = "(import " <> nixQuotedPath (cwd </> "pkgs" </> "windows") <> ")"
      checks = "import " <> nixQuotedPath (cwd </> "test" </> "data" </> "binutils-x86_64.nix")
      source =
        T.concat
          [ "let pkgs = ",
            windows,
            "; checks = ",
            checks,
            "; in builtins.concatStringsSep \" \" [ ",
            T.unwords [path <> ".drvPath" | path <- roots],
            " ]"
          ]
  outcome <- case parseNix cwd testOrigin (TE.encodeUtf8 source) of
    Left err -> pure (Left (T.pack (show err)))
    Right parsed -> runEvalIO st $ do
      value <- evalParsed (pure (evalBaseEnv st)) parsed
      case value of
        VStr bytes _ -> do
          let rootOf = Map.fromList (zip roots (T.words (bytesText bytes)))
          before <- sessionClosure (mapMaybe (`Map.lookup` rootOf) builtBefore)
          full <- sessionClosure (Map.elems rootOf)
          pure (rootOf, before, full)
        other -> throwEvalError ("expected the drvPaths as one string, got " <> typeName other)
  writes <- readIORef (esStoreWriteCache st)
  case outcome of
    Left err -> sequence [runTest instantiates (Fail err)]
    Right (rootOf, before, full) -> do
      let added = Map.elems (Map.difference full before)
          nameOf drv = maybe "" bytesText (Map.lookup "name" (drvEnv drv))
          variableOf name drv = maybe "" bytesText (Map.lookup name (drvEnv drv))
          isFetch drv = drvBuilder drv == "builtin:fetchurl"
          isFixedOutput drv = not (all (T.null . doHash) (drvOutputs drv))
          addedLeaves = filter (Map.null . drvInputDrvs) added
          wroteAsText path = fmap snd (Map.lookup (storePathToText defaultStoreDir path) writes) == Just WriteText
          derivationAt root = Map.lookup root rootOf >>= (`Map.lookup` full)
          binutils = derivationAt binutilsX8664Root
          -- The output of a root's derivation, as a flag spells a store
          -- path, or a word no flag holds where the root is missing.
          outputOf root = maybe "<no output>" (storePathToText defaultStoreDir . doPath) (derivationAt root >>= listToMaybe . drvOutputs)
          outputNamed name = [storePathToText defaultStoreDir (doPath out) | drv <- added, nameOf drv == name, out <- drvOutputs drv]
          wordsOf name = maybe [] (T.words . variableOf name)
          linesOf name = maybe [] (map T.strip . T.lines . variableOf name)
          flagsWith prefixes drv = [flag | flag <- wordsOf "configureFlags" drv, any (`T.isPrefixOf` flag) prefixes]
      sequence
        [ runTest instantiates $
            assertEqual "derivations added" (sort addedNames) (sort (map nameOf added)),
          runTest "every derivation it adds but the fetch is i686-windows" $
            case [nameOf drv | drv <- added, not (isFetch drv), drvPlatform drv /= I686_Windows] of
              [] -> Pass
              named -> Fail ("not i686-windows: " <> T.intercalate ", " named),
          runTest "the one leaf it adds is nixpkgs' binutils archive, fetched fixed-output" $
            assertEqual
              "leaves added"
              ([archive], True)
              (map nameOf addedLeaves, all (\drv -> isFetch drv && isFixedOutput drv) addedLeaves),
          runTest "it reads no input source but what evaluation writes as text and the stdenv's files" $
            assertEqual
              "input sources not written as text"
              ["config.site", "include", "setup.sh"]
              (sort (map spName (nub [path | drv <- added, path <- drvInputSrcs drv, not (wroteAsText path)]))),
          runTest "binutils-x86_64 names its archive where setup.sh reads it, with gcc-latest and binutils as its build inputs and no patch" $
            assertEqual
              "src, buildInputs and patches"
              (outputNamed archive, [outputOf gccLatestRoot, outputOf binutilsRoot], [])
              (wordsOf "src" binutils, wordsOf "buildInputs" binutils, wordsOf "patches" binutils),
          runTest "binutils-x86_64 is configured for x86_64-w64-mingw32 with no sysroot, and builds with gcc-latest's driver and binutils' archiver and linker, by their target's name" $
            assertEqual
              "target and tools"
              ["--target=" <> target, "CC=" <> host <> "-gcc", "AR=" <> host <> "-ar", "LD=" <> host <> "-ld"]
              (flagsWith ["--target=", "--host=", "--with-sysroot", "CC=", "CXX=", "AR=", "LD=", "RANLIB=", "STRIP="] binutils),
          runTest "binutils-x86_64's CFLAGS carry no -g, and LDFLAGS gives every link --no-insert-timestamp and nothing else" $
            assertEqual
              "CFLAGS and LDFLAGS"
              ["CFLAGS=-O2", "LDFLAGS=-Wl,--no-insert-timestamp"]
              (flagsWith ["CFLAGS=", "CXXFLAGS=", "LDFLAGS="] binutils),
          runTest "binutils-x86_64 unsets the stdenv's C_INCLUDE_PATH, configures from a build directory beside the source, and builds with NIX_BUILD_CORES jobs and nixpkgs' MAKEINFO" $
            assertEqual
              "preConfigure, makeFlags and parallelism"
              ( [ "unset C_INCLUDE_PATH",
                  "mkdir ../build",
                  "cd ../build",
                  "configureScript=../binutils-with-gold-" <> version <> "/configure"
                ],
                ["MAKEINFO=true"],
                "1"
              )
              (linesOf "preConfigure" binutils, wordsOf "makeFlags" binutils, maybe "" (variableOf "enableParallelBuilding") binutils)
        ]
  where
    instantiates :: Text
    instantiates = "the x86_64 binutils derivations instantiate, adding eight to the closure of gcc-latest and binutils"
    version :: Text
    version = "2.46"
    archive :: Text
    archive = "binutils-with-gold-" <> version <> ".tar.bz2"
    target :: Text
    target = "x86_64-w64-mingw32"
    -- The target of the compiler and tools the build runs.
    host :: Text
    host = "i686-w64-mingw32"
    binutilsX8664Root :: Text
    binutilsX8664Root = "pkgs.binutils-x86_64"
    gccLatestRoot :: Text
    gccLatestRoot = "pkgs.gcc-latest"
    binutilsRoot :: Text
    binutilsRoot = "pkgs.binutils"
    builtBefore :: [Text]
    builtBefore = ["pkgs.busybox-w32", "pkgs.gnumake", "pkgs.tinycc-native.compiler", "pkgs.tinycc-native.libs", "pkgs.seedStdenv.applets", binutilsRoot, gccLatestRoot]
    roots :: [Text]
    roots = builtBefore ++ [binutilsX8664Root, "checks.link", "checks.stamps"]
    -- Each kaem.runCommand adds its script, written by writeText as
    -- <name>-builder.
    addedNames :: [Text]
    addedNames =
      [archive, target <> "-binutils-" <> version, "link.sh", "stamps.sh"]
        ++ concat [[name, name <> "-builder"] | name <- ["binutils-x86_64-link", "binutils-x86_64-stamps"]]

-- | @pkgs\/windows@'s x86_64 GCC core, read back as the build driver reads
-- it: what gcc-core-x86_64 and @test\/data\/gcc-core-x86_64.nix@'s checks
-- add to the closure of gcc-latest, binutils-x86_64 and the current
-- MinGW-w64's headers, which build it.  It fetches nothing: its four
-- archives are gcc-latest's, which @setup.sh@ and @preConfigure@ unpack
-- from the variables that name them, so it adds no leaf, and it reads no
-- input source but the stdenv's own files and what evaluation writes as
-- text.  Its configure is told by flags where the headers, the assembler
-- and the linker are, and which compilers, archiver, linker and resource
-- compiler the build runs, so each has to name the output of the
-- derivation that builds it, or the target's name binutils and gcc-latest
-- install their programs under.  The compiler is C alone, with a static,
-- single-threaded libgcc, which is what compiling the runtime needs; no
-- compiler option carries @-g@, every link and strip is told to write no
-- time stamp, and the build leaves the configure arguments, which name
-- store paths, out of the compiler.
testGccCoreX8664Windows :: IO [Bool]
testGccCoreX8664Windows = do
  putStrLn "pkgs/gcc-core-x86_64"
  cwd <- Dir.getCurrentDirectory
  storeDir <- evalNixIOStoreDir
  searchPaths <- parseNixPath (TE.encodeUtf8 (T.pack (cwd </> "data")))
  st <- newEvalStateWith testLogger unrestrictedPolicy searchPaths (testSessionStore storeDir) storeDir cwd
  let windows = "(import " <> nixQuotedPath (cwd </> "pkgs" </> "windows") <> ")"
      checks = "import " <> nixQuotedPath (cwd </> "test" </> "data" </> "gcc-core-x86_64.nix")
      source =
        T.concat
          [ "let pkgs = ",
            windows,
            "; checks = ",
            checks,
            "; in builtins.concatStringsSep \" \" [ ",
            T.unwords [path <> ".drvPath" | path <- roots],
            " ]"
          ]
  outcome <- case parseNix cwd testOrigin (TE.encodeUtf8 source) of
    Left err -> pure (Left (T.pack (show err)))
    Right parsed -> runEvalIO st $ do
      value <- evalParsed (pure (evalBaseEnv st)) parsed
      case value of
        VStr bytes _ -> do
          let rootOf = Map.fromList (zip roots (T.words (bytesText bytes)))
          before <- sessionClosure (mapMaybe (`Map.lookup` rootOf) builtBefore)
          full <- sessionClosure (Map.elems rootOf)
          pure (rootOf, before, full)
        other -> throwEvalError ("expected the drvPaths as one string, got " <> typeName other)
  writes <- readIORef (esStoreWriteCache st)
  case outcome of
    Left err -> sequence [runTest instantiates (Fail err)]
    Right (rootOf, before, full) -> do
      let added = Map.elems (Map.difference full before)
          nameOf drv = maybe "" bytesText (Map.lookup "name" (drvEnv drv))
          variableOf name drv = maybe "" bytesText (Map.lookup name (drvEnv drv))
          isFetch drv = drvBuilder drv == "builtin:fetchurl"
          addedLeaves = filter (Map.null . drvInputDrvs) added
          wroteAsText path = fmap snd (Map.lookup (storePathToText defaultStoreDir path) writes) == Just WriteText
          derivationAt root = Map.lookup root rootOf >>= (`Map.lookup` full)
          gccCore = derivationAt gccCoreRoot
          gccLatest = derivationAt gccLatestRoot
          -- The output of a root's derivation, as a flag spells a store
          -- path, or a word no flag holds where the root is missing.
          outputOf root = maybe "<no output>" (storePathToText defaultStoreDir . doPath) (derivationAt root >>= listToMaybe . drvOutputs)
          wordsOf name = maybe [] (T.words . variableOf name)
          linesOf name = maybe [] (map T.strip . T.lines . variableOf name)
          flagsWith prefixes drv = [flag | flag <- wordsOf "configureFlags" drv, any (`T.isPrefixOf` flag) prefixes]
      sequence
        [ runTest instantiates $
            assertEqual "derivations added" (sort addedNames) (sort (map nameOf added)),
          runTest "every derivation it adds but the fetches is i686-windows" $
            case [nameOf drv | drv <- added, not (isFetch drv), drvPlatform drv /= I686_Windows] of
              [] -> Pass
              named -> Fail ("not i686-windows: " <> T.intercalate ", " named),
          runTest "it adds no leaf, fetching nothing gcc-latest does not" $
            assertEqual "leaves added" [] (map nameOf addedLeaves),
          runTest "it reads no input source but what evaluation writes as text and the stdenv's files" $
            assertEqual
              "input sources not written as text"
              ["config.site", "include", "setup.sh"]
              (sort (map spName (nub [path | drv <- added, path <- drvInputSrcs drv, not (wroteAsText path)]))),
          runTest "gcc-core-x86_64 names gcc-latest's four archives where setup.sh and preConfigure read them, with gcc-latest, binutils and binutils-x86_64 as its build inputs and no patch" $
            assertEqual
              "the archives, buildInputs and patches"
              ( [wordsOf variable gccLatest | variable <- archiveVariables],
                [outputOf gccLatestRoot, outputOf binutilsRoot, outputOf binutilsX8664Root],
                [],
                True
              )
              ( [wordsOf variable gccCore | variable <- archiveVariables],
                wordsOf "buildInputs" gccCore,
                wordsOf "patches" gccCore,
                isJust gccLatest
              ),
          runTest "gcc-core-x86_64's preConfigure moves GMP, MPFR and MPC to the directories GCC's configure builds them from, and unsets the stdenv's C_INCLUDE_PATH" $
            assertEqual
              "preConfigure's mv and unset lines"
              (["mv gmp-6.3.0 gmp", "mv mpfr-4.2.2 mpfr", "mv mpc-1.3.1 mpc"], ["unset C_INCLUDE_PATH"])
              (filter ("mv " `T.isPrefixOf`) (linesOf "preConfigure" gccCore), filter ("unset " `T.isPrefixOf`) (linesOf "preConfigure" gccCore)),
          runTest "gcc-core-x86_64's flags name the target, the outputs of the current MinGW-w64's headers and of binutils-x86_64, and no sysroot" $
            assertEqual
              "flags"
              [ "--target=" <> target,
                "--with-headers=" <> outputOf headersRoot <> "/include",
                "--with-as=" <> outputOf binutilsX8664Root <> "/bin/" <> target <> "-as.exe",
                "--with-ld=" <> outputOf binutilsX8664Root <> "/bin/" <> target <> "-ld.exe"
              ]
              (flagsWith ["--target=", "--with-headers", "--with-libs", "--with-sysroot", "--with-build-sysroot", "--with-native-system-header-dir", "--with-as=", "--with-ld="] gccCore),
          runTest "gcc-core-x86_64 builds with gcc-latest's drivers and binutils' archiver, linker and resource compiler, by their target's names, the resource compiler named in the environment and run with --use-temp-file" $
            assertEqual
              "tools, and WINDRES"
              (["CC=" <> host <> "-gcc", "CXX=" <> host <> "-g++", "AR=" <> host <> "-ar", "LD=" <> host <> "-ld"], host <> "-windres --use-temp-file")
              (flagsWith ["CC=", "CXX=", "AR=", "LD=", "STRIP=", "RANLIB=", "WINDRES="] gccCore, maybe "" (variableOf "WINDRES") gccCore),
          runTest "gcc-core-x86_64 gives every link it makes --no-insert-timestamp, and LDFLAGS nothing else" $
            assertEqual
              "LDFLAGS"
              ["LDFLAGS=-Wl,--no-insert-timestamp"]
              (flagsWith ["LDFLAGS=", "LDFLAGS_FOR_BUILD=", "LDFLAGS_FOR_TARGET="] gccCore),
          runTest "gcc-core-x86_64's compiler options carry no -g, and it gives make no flags" $
            assertEqual
              "the options and makeFlags"
              (["CFLAGS=-O2", "CXXFLAGS=-O2", "CFLAGS_FOR_TARGET=-O2"], [])
              (flagsWith ["CFLAGS=", "CXXFLAGS=", "CFLAGS_FOR_TARGET=", "CXXFLAGS_FOR_TARGET="] gccCore, wordsOf "makeFlags" gccCore),
          runTest "gcc-core-x86_64 builds C alone, static and single-threaded, with no registry lookup" $
            assertEqual
              "languages, libraries, threads and registry"
              ["--enable-languages=c", "--disable-shared", "--disable-threads", "--disable-win32-registry"]
              (flagsWith ["--enable-languages=", "--enable-shared", "--disable-shared", "--enable-threads", "--disable-threads", "--enable-win32-registry", "--disable-win32-registry"] gccCore),
          runTest "gcc-core-x86_64 drops the configure arguments from the top-level Makefile, builds the compiler and libgcc with NIX_BUILD_CORES jobs, installs them stripped of debugging information and time stamps, and removes what gcc-latest's install removes and the directory that leaves empty" $
            assertEqual
              "phases and parallelism"
              ( [ "sed -e '/TOPLEVEL_CONFIGURE_ARGUMENTS=/d' -i Makefile",
                  "make ${enableParallelBuilding:+-j$NIX_BUILD_CORES} all-gcc all-target-libgcc"
                ],
                [ "make ${enableParallelInstalling:+-j$NIX_BUILD_CORES} 'STRIP=" <> host <> "-strip -p' install-strip-gcc install-strip-target-libgcc",
                  "libgccDir=$out/lib/gcc/" <> target <> "/" <> version,
                  target <> "-strip -p --strip-debug \"$libgccDir\"/*.a \"$libgccDir\"/*.o",
                  "rm -r \"$out/libexec/gcc/" <> target <> "/" <> version <> "/install-tools\" \"$libgccDir/install-tools\"",
                  "rm -r \"$out/share/man\" \"$out/share/info\"",
                  "rmdir \"$out/share\""
                ],
                "1",
                "1"
              )
              ( linesOf "buildPhase" gccCore,
                linesOf "installPhase" gccCore,
                maybe "" (variableOf "enableParallelBuilding") gccCore,
                maybe "" (variableOf "enableParallelInstalling") gccCore
              )
        ]
  where
    instantiates :: Text
    instantiates = "the x86_64 GCC core's derivations instantiate, adding seven to the closure of gcc-latest, binutils-x86_64 and the current MinGW-w64's headers"
    target :: Text
    target = "x86_64-w64-mingw32"
    -- The target of the compiler and tools the build runs.
    host :: Text
    host = "i686-w64-mingw32"
    version :: Text
    version = "16.2.0"
    -- The variables that name GCC's archive and the three it builds in its
    -- tree.
    archiveVariables :: [Text]
    archiveVariables = ["src", "gmpSrc", "mpfrSrc", "mpcSrc"]
    gccCoreRoot :: Text
    gccCoreRoot = "pkgs.gcc-core-x86_64"
    gccLatestRoot :: Text
    gccLatestRoot = "pkgs.gcc-latest"
    headersRoot :: Text
    headersRoot = "pkgs.mingw-w64-headers-latest"
    binutilsRoot :: Text
    binutilsRoot = "pkgs.binutils"
    binutilsX8664Root :: Text
    binutilsX8664Root = "pkgs.binutils-x86_64"
    builtBefore :: [Text]
    builtBefore = ["pkgs.busybox-w32", "pkgs.gnumake", "pkgs.tinycc-native.compiler", "pkgs.tinycc-native.libs", "pkgs.seedStdenv.applets", binutilsRoot, gccLatestRoot, headersRoot, binutilsX8664Root]
    roots :: [Text]
    roots = builtBefore ++ [gccCoreRoot, "checks.link", "checks.stamps"]
    -- Each kaem.runCommand adds its script, written by writeText as
    -- <name>-builder.
    addedNames :: [Text]
    addedNames =
      [target <> "-gcc-core-" <> version, "link.sh", "stamps.sh"]
        ++ concat [[name, name <> "-builder"] | name <- ["gcc-core-x86_64-link", "gcc-core-x86_64-stamps"]]

-- | @pkgs\/windows@'s x86_64 MinGW-w64 runtime, read back as the build
-- driver reads it: what mingw-w64-crt-x86_64 and
-- @test\/data\/mingw-w64-x86_64.nix@'s checks add to the closure of
-- gcc-core-x86_64, which compiles it.  It fetches nothing: the runtime is
-- compiled from mingw-w64-headers-latest's archive, so it adds no leaf,
-- and it reads no input source but the stdenv's own files and what
-- evaluation writes as text.  Its configure takes the compiler and
-- archiver the environment names, and the stdenv's are tcc's, so its
-- @preConfigure@ unsets them, and @C_INCLUDE_PATH@, which GCC reads as
-- well; its flags name x86_64 as the host, the headers' output as the
-- sysroot, msvcrt and the 64-bit runtime alone, and CFLAGS with no @-g@.
testMingwW64CrtX8664Windows :: IO [Bool]
testMingwW64CrtX8664Windows = do
  putStrLn "pkgs/mingw-w64-crt-x86_64"
  cwd <- Dir.getCurrentDirectory
  storeDir <- evalNixIOStoreDir
  searchPaths <- parseNixPath (TE.encodeUtf8 (T.pack (cwd </> "data")))
  st <- newEvalStateWith testLogger unrestrictedPolicy searchPaths (testSessionStore storeDir) storeDir cwd
  let windows = "(import " <> nixQuotedPath (cwd </> "pkgs" </> "windows") <> ")"
      checks = "import " <> nixQuotedPath (cwd </> "test" </> "data" </> "mingw-w64-x86_64.nix")
      source =
        T.concat
          [ "let pkgs = ",
            windows,
            "; checks = ",
            checks,
            "; in builtins.concatStringsSep \" \" [ ",
            T.unwords [path <> ".drvPath" | path <- roots],
            " ]"
          ]
  outcome <- case parseNix cwd testOrigin (TE.encodeUtf8 source) of
    Left err -> pure (Left (T.pack (show err)))
    Right parsed -> runEvalIO st $ do
      value <- evalParsed (pure (evalBaseEnv st)) parsed
      case value of
        VStr bytes _ -> do
          let rootOf = Map.fromList (zip roots (T.words (bytesText bytes)))
          before <- sessionClosure (mapMaybe (`Map.lookup` rootOf) builtBefore)
          full <- sessionClosure (Map.elems rootOf)
          pure (rootOf, before, full)
        other -> throwEvalError ("expected the drvPaths as one string, got " <> typeName other)
  writes <- readIORef (esStoreWriteCache st)
  case outcome of
    Left err -> sequence [runTest instantiates (Fail err)]
    Right (rootOf, before, full) -> do
      let added = Map.elems (Map.difference full before)
          nameOf drv = maybe "" bytesText (Map.lookup "name" (drvEnv drv))
          variableOf name drv = maybe "" bytesText (Map.lookup name (drvEnv drv))
          isFetch drv = drvBuilder drv == "builtin:fetchurl"
          addedLeaves = filter (Map.null . drvInputDrvs) added
          wroteAsText path = fmap snd (Map.lookup (storePathToText defaultStoreDir path) writes) == Just WriteText
          derivationAt root = Map.lookup root rootOf >>= (`Map.lookup` full)
          headers = derivationAt headersRoot
          crt = derivationAt crtRoot
          -- The output of a root's derivation, as a flag spells a store
          -- path, or a word no flag holds where the root is missing.
          outputOf root = maybe "<no output>" (storePathToText defaultStoreDir . doPath) (derivationAt root >>= listToMaybe . drvOutputs)
          wordsOf name = maybe [] (T.words . variableOf name)
          -- What preConfigure runs before it moves to the build directory.
          firstLine = maybe "" (T.takeWhile (/= '\n') . variableOf "preConfigure")
      sequence
        [ runTest instantiates $
            assertEqual "derivations added" (sort addedNames) (sort (map nameOf added)),
          runTest "every derivation it adds but the fetches is i686-windows" $
            case [nameOf drv | drv <- added, not (isFetch drv), drvPlatform drv /= I686_Windows] of
              [] -> Pass
              named -> Fail ("not i686-windows: " <> T.intercalate ", " named),
          runTest "it adds no leaf, fetching nothing mingw-w64-headers-latest does not" $
            assertEqual "leaves added" [] (map nameOf addedLeaves),
          runTest "it reads no input source but what evaluation writes as text and the stdenv's files" $
            assertEqual
              "input sources not written as text"
              ["config.site", "include", "setup.sh"]
              (sort (map spName (nub [path | drv <- added, path <- drvInputSrcs drv, not (wroteAsText path)]))),
          runTest "mingw-w64-crt-x86_64 is built from mingw-w64-headers-latest's archive by gcc-core-x86_64 and binutils-x86_64, with tcc's variables unset" $
            assertEqual
              "src, buildInputs and preConfigure's first line"
              (wordsOf "src" headers, [outputOf gccCoreRoot, outputOf binutilsX8664Root], "unset CC AR C_INCLUDE_PATH", isJust headers)
              (wordsOf "src" crt, wordsOf "buildInputs" crt, firstLine crt, isJust crt),
          runTest "mingw-w64-crt-x86_64's flags name x86_64 as the host, mingw-w64-headers-latest's output as the sysroot, msvcrt and the 64-bit runtime alone, and CFLAGS with no -g" $
            assertEqual
              "flags"
              [ "--host=" <> target,
                "--with-sysroot=" <> outputOf headersRoot,
                "--with-default-msvcrt=msvcrt",
                "--disable-lib32",
                "--enable-lib64",
                "--disable-dependency-tracking",
                "CFLAGS=-O2"
              ]
              (wordsOf "configureFlags" crt),
          runTest "mingw-w64-crt-x86_64 builds and installs with NIX_BUILD_CORES jobs" $
            assertEqual
              "parallelism"
              ("1", "1")
              (maybe "" (variableOf "enableParallelBuilding") crt, maybe "" (variableOf "enableParallelInstalling") crt)
        ]
  where
    instantiates :: Text
    instantiates = "the x86_64 runtime's derivations instantiate, adding seven to the closure of gcc-core-x86_64"
    target :: Text
    target = "x86_64-w64-mingw32"
    version :: Text
    version = "14.0.0"
    headersRoot :: Text
    headersRoot = "pkgs.mingw-w64-headers-latest"
    crtRoot :: Text
    crtRoot = "pkgs.mingw-w64-crt-x86_64"
    gccCoreRoot :: Text
    gccCoreRoot = "pkgs.gcc-core-x86_64"
    binutilsX8664Root :: Text
    binutilsX8664Root = "pkgs.binutils-x86_64"
    builtBefore :: [Text]
    builtBefore = ["pkgs.busybox-w32", "pkgs.gnumake", "pkgs.tinycc-native.compiler", "pkgs.tinycc-native.libs", "pkgs.seedStdenv.applets", headersRoot, binutilsX8664Root, gccCoreRoot]
    roots :: [Text]
    roots = builtBefore ++ [crtRoot, "checks.crt", "checks.hello"]
    -- Each kaem.runCommand adds its script, written by writeText as
    -- <name>-builder.
    addedNames :: [Text]
    addedNames =
      ["mingw-w64-crt-" <> target <> "-" <> version, "crt.sh", "hello.sh"]
        ++ concat [[name, name <> "-builder"] | name <- ["mingw-w64-x86_64-crt", "mingw-w64-x86_64-hello"]]

-- | @pkgs\/windows@'s x86_64 GCC, read back as the build driver reads it:
-- what gcc-x86_64 and @test\/data\/gcc-x86_64.nix@'s checks add to the
-- closure of gcc-latest, binutils-x86_64 and the x86_64 MinGW-w64
-- runtime, which build it.  It fetches nothing: its four archives are
-- gcc-latest's, so it adds no leaf, and it reads no input source but the
-- stdenv's own files and what evaluation writes as text.  Its configure
-- is told by flags where its target's headers and runtime, the assembler
-- and the linker are, and which compilers, archiver, linker and resource
-- compiler the build runs, so each has to name the output of the
-- derivation that builds it, or the target's name binutils and gcc-latest
-- install their programs under.  It builds C and C++ with static
-- libraries and the win32 thread model; no compiler option carries @-g@,
-- libgcc's own @-g@ is stripped after the install, every link and strip
-- is told to write no time stamp, and the build leaves the configure
-- arguments, which name store paths, out of the compiler.
testGccX8664Windows :: IO [Bool]
testGccX8664Windows = do
  putStrLn "pkgs/gcc-x86_64"
  cwd <- Dir.getCurrentDirectory
  storeDir <- evalNixIOStoreDir
  searchPaths <- parseNixPath (TE.encodeUtf8 (T.pack (cwd </> "data")))
  st <- newEvalStateWith testLogger unrestrictedPolicy searchPaths (testSessionStore storeDir) storeDir cwd
  let windows = "(import " <> nixQuotedPath (cwd </> "pkgs" </> "windows") <> ")"
      checks = "import " <> nixQuotedPath (cwd </> "test" </> "data" </> "gcc-x86_64.nix")
      source =
        T.concat
          [ "let pkgs = ",
            windows,
            "; checks = ",
            checks,
            "; in builtins.concatStringsSep \" \" [ ",
            T.unwords [path <> ".drvPath" | path <- roots],
            " ]"
          ]
  outcome <- case parseNix cwd testOrigin (TE.encodeUtf8 source) of
    Left err -> pure (Left (T.pack (show err)))
    Right parsed -> runEvalIO st $ do
      value <- evalParsed (pure (evalBaseEnv st)) parsed
      case value of
        VStr bytes _ -> do
          let rootOf = Map.fromList (zip roots (T.words (bytesText bytes)))
          before <- sessionClosure (mapMaybe (`Map.lookup` rootOf) builtBefore)
          full <- sessionClosure (Map.elems rootOf)
          pure (rootOf, before, full)
        other -> throwEvalError ("expected the drvPaths as one string, got " <> typeName other)
  writes <- readIORef (esStoreWriteCache st)
  case outcome of
    Left err -> sequence [runTest instantiates (Fail err)]
    Right (rootOf, before, full) -> do
      let added = Map.elems (Map.difference full before)
          nameOf drv = maybe "" bytesText (Map.lookup "name" (drvEnv drv))
          variableOf name drv = maybe "" bytesText (Map.lookup name (drvEnv drv))
          isFetch drv = drvBuilder drv == "builtin:fetchurl"
          addedLeaves = filter (Map.null . drvInputDrvs) added
          wroteAsText path = fmap snd (Map.lookup (storePathToText defaultStoreDir path) writes) == Just WriteText
          derivationAt root = Map.lookup root rootOf >>= (`Map.lookup` full)
          gcc = derivationAt gccRoot
          gccLatest = derivationAt gccLatestRoot
          -- The output of a root's derivation, as a flag spells a store
          -- path, or a word no flag holds where the root is missing.
          outputOf root = maybe "<no output>" (storePathToText defaultStoreDir . doPath) (derivationAt root >>= listToMaybe . drvOutputs)
          wordsOf name = maybe [] (T.words . variableOf name)
          linesOf name = maybe [] (map T.strip . T.lines . variableOf name)
          flagsWith prefixes drv = [flag | flag <- wordsOf "configureFlags" drv, any (`T.isPrefixOf` flag) prefixes]
      sequence
        [ runTest instantiates $
            assertEqual "derivations added" (sort addedNames) (sort (map nameOf added)),
          runTest "every derivation it adds but the fetches is i686-windows" $
            case [nameOf drv | drv <- added, not (isFetch drv), drvPlatform drv /= I686_Windows] of
              [] -> Pass
              named -> Fail ("not i686-windows: " <> T.intercalate ", " named),
          runTest "it adds no leaf, fetching nothing gcc-latest does not" $
            assertEqual "leaves added" [] (map nameOf addedLeaves),
          runTest "it reads no input source but what evaluation writes as text and the stdenv's files" $
            assertEqual
              "input sources not written as text"
              ["config.site", "include", "setup.sh"]
              (sort (map spName (nub [path | drv <- added, path <- drvInputSrcs drv, not (wroteAsText path)]))),
          runTest "gcc-x86_64 names gcc-latest's four archives where setup.sh and preConfigure read them, with gcc-latest, binutils and binutils-x86_64 as its build inputs and no patch" $
            assertEqual
              "the archives, buildInputs and patches"
              ( [wordsOf variable gccLatest | variable <- archiveVariables],
                [outputOf gccLatestRoot, outputOf binutilsRoot, outputOf binutilsX8664Root],
                [],
                True
              )
              ( [wordsOf variable gcc | variable <- archiveVariables],
                wordsOf "buildInputs" gcc,
                wordsOf "patches" gcc,
                isJust gccLatest
              ),
          runTest "gcc-x86_64's preConfigure moves GMP, MPFR and MPC to the directories GCC's configure builds them from, and unsets the stdenv's C_INCLUDE_PATH" $
            assertEqual
              "preConfigure's mv and unset lines"
              (["mv gmp-6.3.0 gmp", "mv mpfr-4.2.2 mpfr", "mv mpc-1.3.1 mpc"], ["unset C_INCLUDE_PATH"])
              (filter ("mv " `T.isPrefixOf`) (linesOf "preConfigure" gcc), filter ("unset " `T.isPrefixOf`) (linesOf "preConfigure" gcc)),
          runTest "gcc-x86_64's flags name the target, the outputs of the current MinGW-w64's headers, of the x86_64 runtime and of binutils-x86_64, and no sysroot" $
            assertEqual
              "flags"
              [ "--target=" <> target,
                "--with-headers=" <> outputOf headersRoot <> "/include",
                "--with-libs=" <> outputOf crtRoot <> "/lib",
                "--with-as=" <> outputOf binutilsX8664Root <> "/bin/" <> target <> "-as.exe",
                "--with-ld=" <> outputOf binutilsX8664Root <> "/bin/" <> target <> "-ld.exe"
              ]
              (flagsWith ["--target=", "--with-headers", "--with-libs", "--with-sysroot", "--with-build-sysroot", "--with-native-system-header-dir", "--with-as=", "--with-ld="] gcc),
          runTest "gcc-x86_64 builds with gcc-latest's drivers and binutils' archiver, linker and resource compiler, by their target's names, the resource compiler named in the environment and run with --use-temp-file" $
            assertEqual
              "tools, and WINDRES"
              (["CC=" <> host <> "-gcc", "CXX=" <> host <> "-g++", "AR=" <> host <> "-ar", "LD=" <> host <> "-ld"], host <> "-windres --use-temp-file")
              (flagsWith ["CC=", "CXX=", "AR=", "LD=", "STRIP=", "RANLIB=", "WINDRES="] gcc, maybe "" (variableOf "WINDRES") gcc),
          runTest "gcc-x86_64 gives every link it makes --no-insert-timestamp, and LDFLAGS nothing else" $
            assertEqual
              "LDFLAGS"
              ["LDFLAGS=-Wl,--no-insert-timestamp"]
              (flagsWith ["LDFLAGS=", "LDFLAGS_FOR_BUILD=", "LDFLAGS_FOR_TARGET="] gcc),
          runTest "gcc-x86_64's compiler options carry no -g, and it gives make no flags" $
            assertEqual
              "the options and makeFlags"
              (["CFLAGS=-O2", "CXXFLAGS=-O2", "CFLAGS_FOR_TARGET=-O2", "CXXFLAGS_FOR_TARGET=-O2"], [])
              (flagsWith ["CFLAGS=", "CXXFLAGS=", "CFLAGS_FOR_TARGET=", "CXXFLAGS_FOR_TARGET="] gcc, wordsOf "makeFlags" gcc),
          runTest "gcc-x86_64 builds C and C++, static, with the win32 thread model, no LTO and no registry lookup" $
            assertEqual
              "languages, libraries, threads, LTO and registry"
              ["--enable-languages=c,c++", "--enable-threads=win32", "--disable-lto", "--disable-shared", "--disable-win32-registry"]
              (flagsWith ["--enable-languages=", "--enable-shared", "--disable-shared", "--enable-threads", "--disable-threads", "--enable-lto", "--disable-lto", "--enable-win32-registry", "--disable-win32-registry"] gcc),
          runTest "gcc-x86_64 drops the configure arguments from the top-level Makefile, builds with NIX_BUILD_CORES jobs, installs with install-strip, whose strip keeps the zero time stamp ld writes, strips libgcc's debugging information with the target's strip, and removes what gcc-latest's install removes" $
            assertEqual
              "phases and parallelism"
              ( [ "sed -e '/TOPLEVEL_CONFIGURE_ARGUMENTS=/d' -i Makefile",
                  "make ${enableParallelBuilding:+-j$NIX_BUILD_CORES}"
                ],
                [ "make ${enableParallelInstalling:+-j$NIX_BUILD_CORES} 'STRIP=" <> host <> "-strip -p' install-strip",
                  "libgccDir=$out/lib/gcc/" <> target <> "/" <> version,
                  target <> "-strip -p --strip-debug \"$libgccDir\"/*.a \"$libgccDir\"/*.o",
                  "rm -r \"$out/libexec/gcc/" <> target <> "/" <> version <> "/install-tools\" \"$libgccDir/install-tools\"",
                  "rm -rf \"$out/share/gcc-" <> version <> "/python\" \"$out/share/man\" \"$out/share/info\"",
                  "rm \"$out/" <> target <> "/lib/libstdc++.a-gdb.py\"",
                  "rmdir \"$out/share/gcc-" <> version <> "\" \"$out/share\""
                ],
                "1",
                "1"
              )
              ( linesOf "buildPhase" gcc,
                linesOf "installPhase" gcc,
                maybe "" (variableOf "enableParallelBuilding") gcc,
                maybe "" (variableOf "enableParallelInstalling") gcc
              )
        ]
  where
    instantiates :: Text
    instantiates = "the x86_64 GCC's derivations instantiate, adding ten to the closure of gcc-latest, binutils-x86_64 and the x86_64 runtime"
    target :: Text
    target = "x86_64-w64-mingw32"
    -- The target of the compiler and tools the build runs.
    host :: Text
    host = "i686-w64-mingw32"
    version :: Text
    version = "16.2.0"
    -- The variables that name GCC's archive and the three it builds in its
    -- tree.
    archiveVariables :: [Text]
    archiveVariables = ["src", "gmpSrc", "mpfrSrc", "mpcSrc"]
    gccRoot :: Text
    gccRoot = "pkgs.gcc-x86_64"
    gccLatestRoot :: Text
    gccLatestRoot = "pkgs.gcc-latest"
    headersRoot :: Text
    headersRoot = "pkgs.mingw-w64-headers-latest"
    crtRoot :: Text
    crtRoot = "pkgs.mingw-w64-crt-x86_64"
    binutilsRoot :: Text
    binutilsRoot = "pkgs.binutils"
    binutilsX8664Root :: Text
    binutilsX8664Root = "pkgs.binutils-x86_64"
    builtBefore :: [Text]
    builtBefore = ["pkgs.busybox-w32", "pkgs.gnumake", "pkgs.tinycc-native.compiler", "pkgs.tinycc-native.libs", "pkgs.seedStdenv.applets", binutilsRoot, gccLatestRoot, headersRoot, binutilsX8664Root, crtRoot]
    roots :: [Text]
    roots = builtBefore ++ [gccRoot, "checks.hello-c", "checks.hello-cxx", "checks.stamps"]
    -- Each kaem.runCommand adds its script, written by writeText as
    -- <name>-builder.
    addedNames :: [Text]
    addedNames =
      [target <> "-gcc-" <> version, "hello-c.sh", "hello-cxx.sh", "stamps.sh"]
        ++ concat [[name, name <> "-builder"] | name <- ["gcc-x86_64-hello-c", "gcc-x86_64-hello-cxx", "gcc-x86_64-stamps"]]

-- | @pkgs\/windows@'s native x86_64 binutils, read back as the build driver
-- reads it: what binutils-native-x86_64 and
-- @test\/data\/binutils-native-x86_64.nix@'s checks add to the closure of
-- gcc-latest, gcc-x86_64 and binutils-x86_64, which build it.  It fetches
-- nothing: its archive is binutils-x86_64's, so it adds no leaf, and it
-- reads no input source but the stdenv's own files and what evaluation
-- writes as text.  Its configure is told x86_64-w64-mingw32 as the host and
-- the target, so the tools install under their own names; the build runs
-- gcc-x86_64's compiler and binutils-x86_64's archiver and linker by the
-- name of their target, since the stdenv's CC, AR and LD are tcc's, and
-- gcc-latest's compiler for the build machine; and its other flags are
-- binutils-x86_64's, so its CFLAGS leave out @-g@ and every link is told to
-- write no time stamp.
testBinutilsNativeX8664Windows :: IO [Bool]
testBinutilsNativeX8664Windows = do
  putStrLn "pkgs/binutils-native-x86_64"
  cwd <- Dir.getCurrentDirectory
  storeDir <- evalNixIOStoreDir
  searchPaths <- parseNixPath (TE.encodeUtf8 (T.pack (cwd </> "data")))
  st <- newEvalStateWith testLogger unrestrictedPolicy searchPaths (testSessionStore storeDir) storeDir cwd
  let windows = "(import " <> nixQuotedPath (cwd </> "pkgs" </> "windows") <> ")"
      checks = "import " <> nixQuotedPath (cwd </> "test" </> "data" </> "binutils-native-x86_64.nix")
      source =
        T.concat
          [ "let pkgs = ",
            windows,
            "; checks = ",
            checks,
            "; in builtins.concatStringsSep \" \" [ ",
            T.unwords [path <> ".drvPath" | path <- roots],
            " ]"
          ]
  outcome <- case parseNix cwd testOrigin (TE.encodeUtf8 source) of
    Left err -> pure (Left (T.pack (show err)))
    Right parsed -> runEvalIO st $ do
      value <- evalParsed (pure (evalBaseEnv st)) parsed
      case value of
        VStr bytes _ -> do
          let rootOf = Map.fromList (zip roots (T.words (bytesText bytes)))
          before <- sessionClosure (mapMaybe (`Map.lookup` rootOf) builtBefore)
          full <- sessionClosure (Map.elems rootOf)
          pure (rootOf, before, full)
        other -> throwEvalError ("expected the drvPaths as one string, got " <> typeName other)
  writes <- readIORef (esStoreWriteCache st)
  case outcome of
    Left err -> sequence [runTest instantiates (Fail err)]
    Right (rootOf, before, full) -> do
      let added = Map.elems (Map.difference full before)
          nameOf drv = maybe "" bytesText (Map.lookup "name" (drvEnv drv))
          variableOf name drv = maybe "" bytesText (Map.lookup name (drvEnv drv))
          isFetch drv = drvBuilder drv == "builtin:fetchurl"
          addedLeaves = filter (Map.null . drvInputDrvs) added
          wroteAsText path = fmap snd (Map.lookup (storePathToText defaultStoreDir path) writes) == Just WriteText
          derivationAt root = Map.lookup root rootOf >>= (`Map.lookup` full)
          binutilsNative = derivationAt binutilsNativeRoot
          binutilsCross = derivationAt binutilsX8664Root
          -- The output of a root's derivation, as a flag spells a store
          -- path, or a word no flag holds where the root is missing.
          outputOf root = maybe "<no output>" (storePathToText defaultStoreDir . doPath) (derivationAt root >>= listToMaybe . drvOutputs)
          wordsOf name = maybe [] (T.words . variableOf name)
          linesOf name = maybe [] (map T.strip . T.lines . variableOf name)
          flagsWith prefixes drv = [flag | flag <- wordsOf "configureFlags" drv, any (`T.isPrefixOf` flag) prefixes]
          sharedFlags drv = [flag | flag <- wordsOf "configureFlags" drv, not (any (`T.isPrefixOf` flag) hostAndToolPrefixes)]
      sequence
        [ runTest instantiates $
            assertEqual "derivations added" (sort addedNames) (sort (map nameOf added)),
          runTest "every derivation it adds but the fetches is i686-windows" $
            case [nameOf drv | drv <- added, not (isFetch drv), drvPlatform drv /= I686_Windows] of
              [] -> Pass
              named -> Fail ("not i686-windows: " <> T.intercalate ", " named),
          runTest "it adds no leaf, fetching nothing binutils-x86_64 does not" $
            assertEqual "leaves added" [] (map nameOf addedLeaves),
          runTest "it reads no input source but what evaluation writes as text and the stdenv's files" $
            assertEqual
              "input sources not written as text"
              ["config.site", "include", "setup.sh"]
              (sort (map spName (nub [path | drv <- added, path <- drvInputSrcs drv, not (wroteAsText path)]))),
          runTest "binutils-native-x86_64 builds binutils-x86_64's archive, with gcc-latest, gcc-x86_64 and binutils-x86_64 as its build inputs and no patch" $
            assertEqual
              "src, buildInputs and patches"
              (wordsOf "src" binutilsCross, [outputOf gccLatestRoot, outputOf gccX8664Root, outputOf binutilsX8664Root], [], True)
              (wordsOf "src" binutilsNative, wordsOf "buildInputs" binutilsNative, wordsOf "patches" binutilsNative, isJust binutilsCross),
          runTest "binutils-native-x86_64 is configured for x86_64-w64-mingw32 as its host and target with no sysroot, and builds with gcc-x86_64's driver and binutils-x86_64's archiver and linker by their target's name, and gcc-latest's driver for the build machine" $
            assertEqual
              "host, target and tools"
              [ "--host=" <> target,
                "--target=" <> target,
                "CC=" <> target <> "-gcc",
                "AR=" <> target <> "-ar",
                "LD=" <> target <> "-ld",
                "CC_FOR_BUILD=" <> buildTarget <> "-gcc"
              ]
              (flagsWith ["--target=", "--host=", "--with-sysroot", "--with-build-sysroot", "CC=", "CXX=", "AR=", "LD=", "RANLIB=", "STRIP=", "CC_FOR_BUILD=", "CXX_FOR_BUILD=", "AR_FOR_BUILD=", "RANLIB_FOR_BUILD="] binutilsNative),
          runTest "binutils-native-x86_64's other flags are binutils-x86_64's, so its CFLAGS carry no -g and LDFLAGS gives every link --no-insert-timestamp and nothing else" $
            assertEqual
              "the other flags, CFLAGS and LDFLAGS"
              (sharedFlags binutilsCross, ["CFLAGS=-O2", "LDFLAGS=-Wl,--no-insert-timestamp"])
              (sharedFlags binutilsNative, flagsWith ["CFLAGS=", "CXXFLAGS=", "LDFLAGS="] binutilsNative),
          runTest "binutils-native-x86_64 unsets the stdenv's C_INCLUDE_PATH, configures from a build directory beside the source, and builds with NIX_BUILD_CORES jobs and nixpkgs' MAKEINFO" $
            assertEqual
              "preConfigure, makeFlags and parallelism"
              ( [ "unset C_INCLUDE_PATH",
                  "mkdir ../build",
                  "cd ../build",
                  "configureScript=../binutils-with-gold-" <> version <> "/configure"
                ],
                ["MAKEINFO=true"],
                "1"
              )
              (linesOf "preConfigure" binutilsNative, wordsOf "makeFlags" binutilsNative, maybe "" (variableOf "enableParallelBuilding") binutilsNative)
        ]
  where
    instantiates :: Text
    instantiates = "the native x86_64 binutils derivations instantiate, adding seven to the closure of gcc-latest, gcc-x86_64 and binutils-x86_64"
    version :: Text
    version = "2.46"
    target :: Text
    target = "x86_64-w64-mingw32"
    -- The target of gcc-latest, the build machine's compiler.
    buildTarget :: Text
    buildTarget = "i686-w64-mingw32"
    -- What starts the flags that name the host or a tool the build runs,
    -- the flags in which the native and the cross binutils differ.
    hostAndToolPrefixes :: [Text]
    hostAndToolPrefixes = ["--host=", "CC=", "AR=", "LD=", "CC_FOR_BUILD="]
    binutilsNativeRoot :: Text
    binutilsNativeRoot = "pkgs.binutils-native-x86_64"
    binutilsX8664Root :: Text
    binutilsX8664Root = "pkgs.binutils-x86_64"
    gccLatestRoot :: Text
    gccLatestRoot = "pkgs.gcc-latest"
    gccX8664Root :: Text
    gccX8664Root = "pkgs.gcc-x86_64"
    builtBefore :: [Text]
    builtBefore = ["pkgs.busybox-w32", "pkgs.gnumake", "pkgs.tinycc-native.compiler", "pkgs.tinycc-native.libs", "pkgs.seedStdenv.applets", gccLatestRoot, binutilsX8664Root, gccX8664Root]
    roots :: [Text]
    roots = builtBefore ++ [binutilsNativeRoot, "checks.link", "checks.stamps"]
    -- Each kaem.runCommand adds its script, written by writeText as
    -- <name>-builder.
    addedNames :: [Text]
    addedNames =
      ["binutils-" <> version, "link.sh", "stamps.sh"]
        ++ concat [[name, name <> "-builder"] | name <- ["binutils-native-x86_64-link", "binutils-native-x86_64-stamps"]]

-- | @pkgs\/windows@'s native x86_64 GCC, read back as the build driver
-- reads it: what gcc-native-x86_64 and @test\/data\/gcc-native-x86_64.nix@'s
-- checks add to the closure of gcc-x86_64 and binutils-native-x86_64, which
-- build it and assemble and link for it.  It fetches nothing: its four
-- archives are gcc-latest's, so it adds no leaf, and it reads no input
-- source but the stdenv's own files, what evaluation writes as text, and
-- its patch, which takes @\/usr\/local\/include@ out of its search.
-- Its configure is told x86_64-w64-mingw32 as the host and the target,
-- where its target's headers are at build time, since a compiler whose
-- host is its target takes no @--with-headers@ or @--with-libs@, and which
-- assembler and linker it runs, so each has to name the output of the
-- derivation that builds it; the build runs gcc-x86_64's compilers and
-- binutils-x86_64's tools for the host by the name of their target, and
-- gcc-latest's and binutils' for the build machine by theirs.  It builds C
-- and C++ with static libraries and the win32 thread model, its other
-- flags are gcc-x86_64's, and its install copies the target's headers and
-- runtime where the installed driver finds them.
testGccNativeX8664Windows :: IO [Bool]
testGccNativeX8664Windows = do
  putStrLn "pkgs/gcc-native-x86_64"
  cwd <- Dir.getCurrentDirectory
  storeDir <- evalNixIOStoreDir
  searchPaths <- parseNixPath (TE.encodeUtf8 (T.pack (cwd </> "data")))
  st <- newEvalStateWith testLogger unrestrictedPolicy searchPaths (testSessionStore storeDir) storeDir cwd
  let windows = "(import " <> nixQuotedPath (cwd </> "pkgs" </> "windows") <> ")"
      checks = "import " <> nixQuotedPath (cwd </> "test" </> "data" </> "gcc-native-x86_64.nix")
      source =
        T.concat
          [ "let pkgs = ",
            windows,
            "; checks = ",
            checks,
            "; in builtins.concatStringsSep \" \" [ ",
            T.unwords [path <> ".drvPath" | path <- roots],
            " ]"
          ]
  outcome <- case parseNix cwd testOrigin (TE.encodeUtf8 source) of
    Left err -> pure (Left (T.pack (show err)))
    Right parsed -> runEvalIO st $ do
      value <- evalParsed (pure (evalBaseEnv st)) parsed
      case value of
        VStr bytes _ -> do
          let rootOf = Map.fromList (zip roots (T.words (bytesText bytes)))
          before <- sessionClosure (mapMaybe (`Map.lookup` rootOf) builtBefore)
          full <- sessionClosure (Map.elems rootOf)
          pure (rootOf, before, full)
        other -> throwEvalError ("expected the drvPaths as one string, got " <> typeName other)
  writes <- readIORef (esStoreWriteCache st)
  patchLines <- BS8.lines <$> BS.readFile (cwd </> "pkgs" </> "windows" </> "by-name" </> "gc" </> "gcc-native-x86_64" </> T.unpack patch)
  case outcome of
    Left err -> sequence [runTest instantiates (Fail err)]
    Right (rootOf, before, full) -> do
      let added = Map.elems (Map.difference full before)
          nameOf drv = maybe "" bytesText (Map.lookup "name" (drvEnv drv))
          variableOf name drv = maybe "" bytesText (Map.lookup name (drvEnv drv))
          isFetch drv = drvBuilder drv == "builtin:fetchurl"
          addedLeaves = filter (Map.null . drvInputDrvs) added
          wroteAsText path = fmap snd (Map.lookup (storePathToText defaultStoreDir path) writes) == Just WriteText
          -- A unified diff's new file names, and the lines it adds and
          -- removes.
          patchFiles = [rest | line <- patchLines, Just rest <- [BS.stripPrefix "+++ " line]]
          patchAdded = [rest | line <- patchLines, Just rest <- [BS.stripPrefix "+" line], not ("++ " `BS.isPrefixOf` rest)]
          patchRemoved = [rest | line <- patchLines, Just rest <- [BS.stripPrefix "-" line], not ("-- " `BS.isPrefixOf` rest)]
          derivationAt root = Map.lookup root rootOf >>= (`Map.lookup` full)
          gccNative = derivationAt gccNativeRoot
          gccCross = derivationAt gccX8664Root
          gccLatest = derivationAt gccLatestRoot
          -- The output of a root's derivation, as a flag spells a store
          -- path, or a word no flag holds where the root is missing.
          outputOf root = maybe "<no output>" (storePathToText defaultStoreDir . doPath) (derivationAt root >>= listToMaybe . drvOutputs)
          wordsOf name = maybe [] (T.words . variableOf name)
          linesOf name = maybe [] (map T.strip . T.lines . variableOf name)
          flagsWith prefixes drv = [flag | flag <- wordsOf "configureFlags" drv, any (`T.isPrefixOf` flag) prefixes]
          sharedFlags drv = [flag | flag <- wordsOf "configureFlags" drv, not (any (`T.isPrefixOf` flag) hostAndToolPrefixes)]
      sequence
        [ runTest instantiates $
            assertEqual "derivations added" (sort addedNames) (sort (map nameOf added)),
          runTest "every derivation it adds but the fetches is i686-windows" $
            case [nameOf drv | drv <- added, not (isFetch drv), drvPlatform drv /= I686_Windows] of
              [] -> Pass
              named -> Fail ("not i686-windows: " <> T.intercalate ", " named),
          runTest "it adds no leaf, fetching nothing gcc-latest does not" $
            assertEqual "leaves added" [] (map nameOf addedLeaves),
          runTest "it reads no input source but what evaluation writes as text, the stdenv's files and its patch" $
            assertEqual
              "input sources not written as text"
              (sort ["config.site", "include", "setup.sh", patch])
              (sort (map spName (nub [path | drv <- added, path <- drvInputSrcs drv, not (wroteAsText path)]))),
          runTest "gcc-native-x86_64 names gcc-latest's four archives where setup.sh and preConfigure read them, with gcc-latest, binutils, gcc-x86_64 and binutils-x86_64 as its build inputs and its patch" $
            assertEqual
              "the archives, buildInputs and patches"
              ( [wordsOf variable gccLatest | variable <- archiveVariables],
                [outputOf gccLatestRoot, outputOf binutilsRoot, outputOf gccX8664Root, outputOf binutilsX8664Root],
                [storePathToText defaultStoreDir path | drv <- maybeToList gccNative, path <- drvInputSrcs drv, spName path == patch],
                True
              )
              ( [wordsOf variable gccNative | variable <- archiveVariables],
                wordsOf "buildInputs" gccNative,
                wordsOf "patches" gccNative,
                isJust gccLatest
              ),
          runTest "gcc-native-x86_64's preConfigure moves GMP, MPFR and MPC to the directories GCC's configure builds them from, and unsets the stdenv's C_INCLUDE_PATH" $
            assertEqual
              "preConfigure's mv and unset lines"
              (["mv gmp-6.3.0 gmp", "mv mpfr-4.2.2 mpfr", "mv mpc-1.3.1 mpc"], ["unset C_INCLUDE_PATH"])
              (filter ("mv " `T.isPrefixOf`) (linesOf "preConfigure" gccNative), filter ("unset " `T.isPrefixOf`) (linesOf "preConfigure" gccNative)),
          runTest "gcc-native-x86_64's flags name x86_64-w64-mingw32 as its host and target, the current MinGW-w64's headers' output as the build sysroot with /include as their directory, binutils-native-x86_64's assembler and linker, and no sysroot and no headers or runtime to copy" $
            assertEqual
              "flags"
              [ "--host=" <> target,
                "--target=" <> target,
                "--with-build-sysroot=" <> outputOf headersRoot,
                "--with-native-system-header-dir=/include",
                "--with-as=" <> outputOf binutilsNativeRoot <> "/bin/as.exe",
                "--with-ld=" <> outputOf binutilsNativeRoot <> "/bin/ld.exe"
              ]
              (flagsWith ["--host=", "--target=", "--with-headers", "--with-libs", "--with-sysroot", "--with-build-sysroot", "--with-native-system-header-dir", "--with-gxx-include-dir", "--with-as=", "--with-ld="] gccNative),
          runTest "gcc-native-x86_64 builds the host's programs with gcc-x86_64's drivers and binutils-x86_64's archiver, linker and resource compiler, and the build machine's with gcc-latest's drivers and binutils' archiver and ranlib, each by its target's name, the resource compiler named in the environment and run with --use-temp-file" $
            assertEqual
              "tools, and WINDRES"
              ( [ "CC=" <> target <> "-gcc",
                  "CXX=" <> target <> "-g++",
                  "AR=" <> target <> "-ar",
                  "LD=" <> target <> "-ld",
                  "CC_FOR_BUILD=" <> buildTarget <> "-gcc",
                  "CXX_FOR_BUILD=" <> buildTarget <> "-g++",
                  "AR_FOR_BUILD=" <> buildTarget <> "-ar",
                  "RANLIB_FOR_BUILD=" <> buildTarget <> "-ranlib"
                ],
                target <> "-windres --use-temp-file"
              )
              ( flagsWith ["CC=", "CXX=", "AR=", "LD=", "STRIP=", "RANLIB=", "WINDRES=", "CC_FOR_BUILD=", "CXX_FOR_BUILD=", "AR_FOR_BUILD=", "RANLIB_FOR_BUILD=", "LD_FOR_BUILD=", "NM_FOR_BUILD="] gccNative,
                maybe "" (variableOf "WINDRES") gccNative
              ),
          runTest "gcc-native-x86_64 gives every link it makes --no-insert-timestamp, and LDFLAGS nothing else" $
            assertEqual
              "LDFLAGS"
              ["LDFLAGS=-Wl,--no-insert-timestamp"]
              (flagsWith ["LDFLAGS=", "LDFLAGS_FOR_BUILD=", "LDFLAGS_FOR_TARGET="] gccNative),
          runTest "gcc-native-x86_64's compiler options carry no -g, and it gives make no flags" $
            assertEqual
              "the options and makeFlags"
              (["CFLAGS=-O2", "CXXFLAGS=-O2", "CFLAGS_FOR_TARGET=-O2", "CXXFLAGS_FOR_TARGET=-O2"], [])
              (flagsWith ["CFLAGS=", "CXXFLAGS=", "CFLAGS_FOR_TARGET=", "CXXFLAGS_FOR_TARGET=", "CFLAGS_FOR_BUILD=", "CXXFLAGS_FOR_BUILD="] gccNative, wordsOf "makeFlags" gccNative),
          runTest "gcc-native-x86_64 builds C and C++, static, with the win32 thread model, no LTO and no registry lookup" $
            assertEqual
              "languages, libraries, threads, LTO and registry"
              ["--enable-languages=c,c++", "--enable-threads=win32", "--disable-lto", "--disable-shared", "--disable-win32-registry"]
              (flagsWith ["--enable-languages=", "--enable-shared", "--disable-shared", "--enable-threads", "--disable-threads", "--enable-lto", "--disable-lto", "--enable-win32-registry", "--disable-win32-registry"] gccNative),
          runTest "gcc-native-x86_64's other flags are gcc-x86_64's" $
            assertEqual "the other flags" (sharedFlags gccCross, True) (sharedFlags gccNative, isJust gccCross),
          runTest "gcc-native-x86_64 hands configure, through the environment, the host's strip with -p, which install-strip runs on each program and which keeps the zero time stamp ld writes" $
            assertEqual "STRIP" (target <> "-strip -p") (maybe "" (variableOf "STRIP") gccNative),
          runTest "gcc-native-x86_64 drops the configure arguments from the top-level Makefile, builds with NIX_BUILD_CORES jobs, installs with install-strip, strips libgcc's debugging information, removes what gcc-x86_64's install removes, and copies the current MinGW-w64's headers and the x86_64 runtime into its tool directory" $
            assertEqual
              "phases and parallelism"
              ( [ "sed -e '/TOPLEVEL_CONFIGURE_ARGUMENTS=/d' -i Makefile",
                  "make ${enableParallelBuilding:+-j$NIX_BUILD_CORES}"
                ],
                [ "make ${enableParallelInstalling:+-j$NIX_BUILD_CORES} install-strip",
                  "libgccDir=$out/lib/gcc/" <> target <> "/" <> version,
                  target <> "-strip -p --strip-debug \"$libgccDir\"/*.a \"$libgccDir\"/*.o",
                  "rm -r \"$out/libexec/gcc/" <> target <> "/" <> version <> "/install-tools\" \"$libgccDir/install-tools\"",
                  "rm -rf \"$out/share/gcc-" <> version <> "/python\" \"$out/share/man\" \"$out/share/info\"",
                  "rm \"$out/lib/libstdc++.a-gdb.py\"",
                  "rmdir \"$out/share/gcc-" <> version <> "\" \"$out/share\"",
                  "mkdir -p \"$out/" <> target <> "/include\" \"$out/" <> target <> "/lib\"",
                  "cp -r \"" <> outputOf headersRoot <> "/include/.\" \"$out/" <> target <> "/include\"",
                  "cp -r \"" <> outputOf crtRoot <> "/lib/.\" \"$out/" <> target <> "/lib\""
                ],
                "1",
                "1"
              )
              ( linesOf "buildPhase" gccNative,
                linesOf "installPhase" gccNative,
                maybe "" (variableOf "enableParallelBuilding") gccNative,
                maybe "" (variableOf "enableParallelInstalling") gccNative
              ),
          runTest "no-local-include-dir.patch undefines LOCAL_INCLUDE_DIR in gcc/cppdefault.cc, and changes nothing else" $
            assertEqual
              "the files it changes, and the lines it adds and removes"
              (["b/gcc/cppdefault.cc"], ["#undef LOCAL_INCLUDE_DIR", ""], [])
              (patchFiles, patchAdded, patchRemoved)
        ]
  where
    patch :: Text
    patch = "no-local-include-dir.patch"
    instantiates :: Text
    instantiates = "the native x86_64 GCC's derivations instantiate, adding ten to the closure of gcc-x86_64 and binutils-native-x86_64"
    target :: Text
    target = "x86_64-w64-mingw32"
    -- The target of gcc-latest and binutils, the build machine's compilers
    -- and tools.
    buildTarget :: Text
    buildTarget = "i686-w64-mingw32"
    version :: Text
    version = "16.2.0"
    -- The variables that name GCC's archive and the three it builds in its
    -- tree.
    archiveVariables :: [Text]
    archiveVariables = ["src", "gmpSrc", "mpfrSrc", "mpcSrc"]
    -- What starts the flags that name the host, where the target's headers
    -- and runtime come from, the assembler and linker, or a tool the build
    -- runs, the flags in which the native and the cross GCC differ.
    hostAndToolPrefixes :: [Text]
    hostAndToolPrefixes =
      [ "--host=",
        "--with-headers=",
        "--with-libs=",
        "--with-build-sysroot=",
        "--with-native-system-header-dir=",
        "--with-as=",
        "--with-ld=",
        "CC=",
        "CXX=",
        "AR=",
        "LD=",
        "CC_FOR_BUILD=",
        "CXX_FOR_BUILD=",
        "AR_FOR_BUILD=",
        "RANLIB_FOR_BUILD="
      ]
    gccNativeRoot :: Text
    gccNativeRoot = "pkgs.gcc-native-x86_64"
    gccX8664Root :: Text
    gccX8664Root = "pkgs.gcc-x86_64"
    gccLatestRoot :: Text
    gccLatestRoot = "pkgs.gcc-latest"
    headersRoot :: Text
    headersRoot = "pkgs.mingw-w64-headers-latest"
    crtRoot :: Text
    crtRoot = "pkgs.mingw-w64-crt-x86_64"
    binutilsRoot :: Text
    binutilsRoot = "pkgs.binutils"
    binutilsX8664Root :: Text
    binutilsX8664Root = "pkgs.binutils-x86_64"
    binutilsNativeRoot :: Text
    binutilsNativeRoot = "pkgs.binutils-native-x86_64"
    builtBefore :: [Text]
    builtBefore = ["pkgs.busybox-w32", "pkgs.gnumake", "pkgs.tinycc-native.compiler", "pkgs.tinycc-native.libs", "pkgs.seedStdenv.applets", binutilsRoot, gccLatestRoot, headersRoot, binutilsX8664Root, crtRoot, gccX8664Root, binutilsNativeRoot]
    roots :: [Text]
    roots = builtBefore ++ [gccNativeRoot, "checks.hello-c", "checks.hello-cxx", "checks.stamps"]
    -- Each kaem.runCommand adds its script, written by writeText as
    -- <name>-builder.
    addedNames :: [Text]
    addedNames =
      ["gcc-" <> version, "hello-c.sh", "hello-cxx.sh", "stamps.sh"]
        ++ concat [[name, name <> "-builder"] | name <- ["gcc-native-x86_64-hello-c", "gcc-native-x86_64-hello-cxx", "gcc-native-x86_64-stamps"]]

-- | @pkgs\/windows@'s stdenv, read back as the build driver reads it:
-- what its wrappers and @test\/data\/stdenv.nix@'s checks, GNU Hello and
-- the set's other five packages built with it and flags, add to the
-- closure of the tools it is made of, busybox-w32, GNU make, the seed
-- stdenv's applets and the native x86_64 binutils and GCC.
-- The leaves it adds are the archives of Hello, sed and zlib, every
-- derivation but those fetches is x86_64-windows, and it reads no input
-- source but the stdenv's own files, the packages' source directories,
-- the flags check's sources and what evaluation writes as text.  Its
-- builds run its own @setup.sh@, which runs the seed stdenv's and then
-- the fixup phase, with the applets, GNU make and the wrappers on @PATH@ and
-- neither toolchain's bin directory, whose programs GNU make would run
-- before a wrapper of the same name; @CC@, @CXX@ and @LD@ name the
-- wrappers, @AR@ binutils' archiver, and nothing sets the
-- @C_INCLUDE_PATH@ the seed stdenv sets for TinyCC; @OBJDUMP@ and
-- @STRIP@ name binutils' programs, @WINDRES@ the windres wrapper, and the
-- fixup looks for a DLL in GCC's bin directory first.  The wrappers are
-- written from gcc-native-x86_64's drivers and binutils-native-x86_64's
-- linker.
testStdenvWindows :: IO [Bool]
testStdenvWindows = do
  putStrLn "pkgs/stdenv"
  cwd <- Dir.getCurrentDirectory
  storeDir <- evalNixIOStoreDir
  searchPaths <- parseNixPath (TE.encodeUtf8 (T.pack (cwd </> "data")))
  st <- newEvalStateWith testLogger unrestrictedPolicy searchPaths (testSessionStore storeDir) storeDir cwd
  let windows = "(import " <> nixQuotedPath (cwd </> "pkgs" </> "windows") <> ")"
      checksOf name = "import " <> nixQuotedPath (cwd </> "test" </> "data" </> name)
      source =
        T.concat
          [ "let pkgs = ",
            windows,
            "; checks = ",
            checksOf "stdenv.nix",
            "; seedChecks = ",
            checksOf "seed-stdenv.nix",
            "; in builtins.concatStringsSep \" \" [ ",
            T.unwords [path <> ".drvPath" | path <- roots],
            " ]"
          ]
  outcome <- case parseNix cwd testOrigin (TE.encodeUtf8 source) of
    Left err -> pure (Left (T.pack (show err)))
    Right parsed -> runEvalIO st $ do
      value <- evalParsed (pure (evalBaseEnv st)) parsed
      case value of
        VStr bytes _ -> do
          let rootOf = Map.fromList (zip roots (T.words (bytesText bytes)))
              closureOf attrs = sessionClosure (mapMaybe (`Map.lookup` rootOf) attrs)
          tools <- closureOf toolRoots
          built <- closureOf ([wrappersRoot, helloRoot, flagsRoot] ++ packageRoots)
          full <- sessionClosure (Map.elems rootOf)
          pure (rootOf, tools, built, full)
        other -> throwEvalError ("expected the drvPaths as one string, got " <> typeName other)
  writes <- readIORef (esStoreWriteCache st)
  case outcome of
    Left err -> sequence [runTest instantiates (Fail err)]
    Right (rootOf, tools, built, full) -> do
      let added = Map.elems (Map.difference built tools)
          nameOf drv = maybe "" bytesText (Map.lookup "name" (drvEnv drv))
          variableOf name drv = maybe "" bytesText (Map.lookup name (drvEnv drv))
          isFetch drv = drvBuilder drv == "builtin:fetchurl"
          isFixedOutput drv = not (all (T.null . doHash) (drvOutputs drv))
          addedLeaves = filter (Map.null . drvInputDrvs) added
          wroteAsText path = fmap snd (Map.lookup (storePathToText defaultStoreDir path) writes) == Just WriteText
          derivationAt root = Map.lookup root rootOf >>= (`Map.lookup` full)
          -- The output of a root's derivation, as a variable spells a
          -- store path, or a word no variable holds where the root is
          -- missing.
          outputOf root = maybe "<no output>" (storePathToText defaultStoreDir . doPath) (derivationAt root >>= listToMaybe . drvOutputs)
          hello = derivationAt helloRoot
          seedHello = derivationAt seedHelloRoot
          wrappers = derivationAt wrappersRoot
          wordsOf name = maybe [] (T.words . variableOf name)
          storePathIn = parseStorePath defaultStoreDir
          -- The script a derivation's builder runs: the second argument,
          -- after busybox's applet name, sh.
          scriptOf drv = case drvArgs drv of
            [_, script] -> storePathIn (bytesText script)
            _ -> Nothing
          configSiteOf drv = storePathIn (variableOf "CONFIG_SITE" drv)
          binutilsProgram program = outputOf binutilsNativeRoot <> "/bin/" <> program <> ".exe"
      sequence
        [ runTest instantiates $
            assertEqual "derivations added" (sort addedNames) (sort (map nameOf added)),
          runTest "every derivation it adds but the fetch is x86_64-windows" $
            case [nameOf drv | drv <- added, not (isFetch drv), drvPlatform drv /= X86_64_Windows] of
              [] -> Pass
              named -> Fail ("not x86_64-windows: " <> T.intercalate ", " named),
          runTest "the leaves it adds are the archives of hello, sed and zlib, each fetched fixed-output" $
            assertEqual
              "leaves added"
              (sort archives, True)
              (sort (map nameOf addedLeaves), all (\drv -> isFetch drv && isFixedOutput drv) addedLeaves),
          -- Two scripts named setup.sh, the stdenv's and the seed
          -- stdenv's it runs, and the source directories of libgreet,
          -- zdemo and greeter.
          runTest "it reads no input source but what evaluation writes as text, the stdenv's own files, the packages' source directories and the flags check's sources" $
            assertEqual
              "input sources not written as text"
              (sort ["cc-wrapper.sh", "config.site", "ld-wrapper.sh", "link.specs", "setup.sh", "setup.sh", "src", "src", "src", "stdenv-flags", "windres-wrapper.sh", "wrap.sh"])
              (sort (map spName (nub [path | drv <- added, path <- drvInputSrcs drv, not (wroteAsText path)]))),
          runTest "hello built with it runs its own setup.sh under busybox's sh, which runs the seed stdenv's, with a config.site that is not the seed stdenv's" $
            assertEqual
              "the builder, the script and whether it is the seed stdenv's, whether seedSetup is the seed stdenv's script, and CONFIG_SITE and whether it is the seed stdenv's"
              (outputOf busyboxRoot <> "/bin/busybox.exe", Just "setup.sh", False, True, Just "config.site", False)
              ( maybe "" (bytesText . drvBuilder) hello,
                spName <$> (scriptOf =<< hello),
                (scriptOf =<< hello) == (scriptOf =<< seedHello),
                (storePathIn . variableOf "seedSetup" =<< hello) == (scriptOf =<< seedHello),
                spName <$> (configSiteOf =<< hello),
                (configSiteOf =<< hello) == (configSiteOf =<< seedHello)
              ),
          runTest "OBJDUMP and STRIP name binutils-native-x86_64's programs, WINDRES the windres wrapper, and the fixup looks in gcc-native-x86_64's bin directory first" $
            assertEqual
              "OBJDUMP, STRIP, WINDRES and dllSearchPath"
              (Just (binutilsProgram "objdump", binutilsProgram "strip", "windres", outputOf gccNativeRoot <> "/bin"))
              ( hello <&> \drv ->
                  (variableOf "OBJDUMP" drv, variableOf "STRIP" drv, variableOf "WINDRES" drv, variableOf "dllSearchPath" drv)
              ),
          runTest "its builds' PATH is the seed stdenv's applets, GNU make and the wrappers, with neither toolchain's bin directory on it" $
            assertEqual
              "initialPath and buildInputs"
              ([outputOf appletsRoot, outputOf gnumakeRoot, outputOf wrappersRoot], [])
              (wordsOf "initialPath" hello, wordsOf "buildInputs" hello),
          runTest "CC, CXX and LD name the wrappers, AR binutils-native-x86_64's archiver, configure's build is x86_64-w64-mingw32, and nothing sets C_INCLUDE_PATH" $
            assertEqual
              "CC, CXX, LD, AR, buildTriple, and whether C_INCLUDE_PATH is set"
              (Just ("gcc", "g++", "ld", outputOf binutilsNativeRoot <> "/bin/ar.exe", target, False))
              ( hello <&> \drv ->
                  ( variableOf "CC" drv,
                    variableOf "CXX" drv,
                    variableOf "LD" drv,
                    variableOf "AR" drv,
                    variableOf "buildTriple" drv,
                    Map.member "C_INCLUDE_PATH" (drvEnv drv)
                  )
              ),
          runTest "the wrappers are written from gcc-native-x86_64's drivers and binutils-native-x86_64's linker and resource compiler" $
            assertEqual
              "compilers, linker and windres"
              (Just (outputOf gccNativeRoot <> "/bin", outputOf binutilsNativeRoot <> "/bin/ld.exe", binutilsProgram "windres"))
              (wrappers <&> \drv -> (variableOf "compilers" drv, variableOf "linker" drv, variableOf "windres" drv))
        ]
  where
    instantiates :: Text
    instantiates = "the stdenv and its checks instantiate, adding eleven to the closure of their tools"
    target :: Text
    target = "x86_64-w64-mingw32"
    busyboxRoot :: Text
    busyboxRoot = "pkgs.busybox-w32"
    gnumakeRoot :: Text
    gnumakeRoot = "pkgs.gnumake"
    appletsRoot :: Text
    appletsRoot = "pkgs.seedStdenv.applets"
    binutilsNativeRoot :: Text
    binutilsNativeRoot = "pkgs.binutils-native-x86_64"
    gccNativeRoot :: Text
    gccNativeRoot = "pkgs.gcc-native-x86_64"
    toolRoots :: [Text]
    toolRoots = [busyboxRoot, gnumakeRoot, appletsRoot, binutilsNativeRoot, gccNativeRoot]
    wrappersRoot :: Text
    wrappersRoot = "pkgs.stdenv.cc"
    helloRoot :: Text
    helloRoot = "checks.hello"
    flagsRoot :: Text
    flagsRoot = "checks.flags"
    seedHelloRoot :: Text
    seedHelloRoot = "seedChecks.hello"
    roots :: [Text]
    roots = toolRoots ++ [wrappersRoot, helloRoot, flagsRoot, seedHelloRoot] ++ packageRoots
    -- The set's other five packages, built with the stdenv.
    packageRoots :: [Text]
    packageRoots = ["checks.sed", "checks.zlib", "checks.libgreet", "checks.zdemo", "checks.greeter"]
    archives :: [Text]
    archives = ["hello-2.12.3.tar.gz", "sed-4.10.tar.xz", "zlib-1.3.2.tar.gz"]
    addedNames :: [Text]
    addedNames = archives ++ ["gcc-wrapper-16.2.0", "hello", "stdenv-flags", "sed", "zlib", "libgreet", "zdemo", "greeter"]

-- | @pkgs\/windows\/sources.nix@ against the package set whose sources it
-- lists: the fetches in the closure of every derivation in the set are the
-- list's, each fixed-output and listed once.  A package that fetches a
-- source and leaves it out of its @srcs@ fails here.  CI publishes the list
-- to the public cache, so a source missing from it is one every run fetches
-- from its upstream host.  A package's derivations are its value and the
-- attributes at most two sets below it, as @tinycc.boot0.libs@ is.
testWindowsSources :: IO [Bool]
testWindowsSources = do
  putStrLn "pkgs/sources"
  cwd <- Dir.getCurrentDirectory
  storeDir <- evalNixIOStoreDir
  searchPaths <- parseNixPath (TE.encodeUtf8 (T.pack (cwd </> "data")))
  st <- newEvalStateWith testLogger unrestrictedPolicy searchPaths (testSessionStore storeDir) storeDir cwd
  let windows = cwd </> "pkgs" </> "windows"
      source =
        T.concat
          [ "let pkgs = import ",
            nixQuotedPath windows,
            "; sources = import ",
            nixQuotedPath (windows </> "sources.nix"),
            "; derivationsIn = depth: value:",
            " if !(builtins.isAttrs value) then [ ]",
            " else if (value.type or null) == \"derivation\" then [ value.drvPath ]",
            " else if depth == 0 then [ ]",
            " else builtins.concatMap (name: derivationsIn (depth - 1) value.${name}) (builtins.attrNames value);",
            " in builtins.concatStringsSep \" \" (map (source: source.drvPath) sources ++ [ \"",
            listEnd,
            "\" ] ++ derivationsIn ",
            T.pack (show packageDepth),
            " pkgs)"
          ]
  outcome <- case parseNix cwd testOrigin (TE.encodeUtf8 source) of
    Left err -> pure (Left (T.pack (show err)))
    Right parsed -> runEvalIO st $ do
      value <- evalParsed (pure (evalBaseEnv st)) parsed
      case value of
        VStr bytes _ -> do
          let (listed, roots) = break (== listEnd) (T.words (bytesText bytes))
          closure <- sessionClosure (listed ++ drop 1 roots)
          pure (listed, closure)
        other -> throwEvalError ("expected the drvPaths as one string, got " <> typeName other)
  case outcome of
    Left err -> sequence [runTest lists (Fail err)]
    Right (listed, closure) -> do
      let isFetch drv = drvBuilder drv == "builtin:fetchurl"
          isFixedOutput drv = not (all (T.null . doHash) (drvOutputs drv))
          fetches = Map.filter isFetch closure
          nameOf path = maybe path (maybe path bytesText . Map.lookup "name" . drvEnv) (Map.lookup path closure)
      sequence
        [ runTest lists $
            assertEqual "sources" (sort (map nameOf (Map.keys fetches))) (sort (map nameOf listed)),
          runTest "it lists the same derivations, not only the same names" $
            assertEqual "derivations" (Map.keys fetches) (sort listed),
          runTest "every source is fixed-output" $
            assertEqual "not fixed-output" [] [nameOf path | (path, drv) <- Map.toList fetches, not (isFixedOutput drv)],
          runTest "the set fetches the seed, the ladder's sources and its packages' sources" $
            assertEqual "sources missing" [] [name | name <- knownSources, name `notElem` map nameOf listed]
        ]
  where
    lists :: Text
    lists = "sources.nix lists every source the package set fetches, and no other"
    -- What ends the list's drvPaths in the string the expression returns: no
    -- store path is spelled so.
    listEnd :: Text
    listEnd = "|"
    packageDepth :: Int
    packageDepth = 3
    -- One source of each kind, so that an empty closure does not pass as an
    -- empty list: the seed, a rung's archive and a package's archive.
    knownSources :: [Text]
    knownSources = ["hex0-seed.exe", "busybox-w32-FRP-6354-g5f09fb446.tgz", "hello-2.12.3.tar.gz"]

-- | The workflow whose jobs build one leg of the seed ladder, a job for each
-- stage, from the repository root.
ladderWorkflowPath :: FilePath
ladderWorkflowPath = ".github" </> "workflows" </> "ladder.yml"

-- | The package set's file as a workflow step names it.
packageSetFile :: Text
packageSetFile = "pkgs/windows/default.nix"

-- | What precedes the attribute path where a step builds from the set.
packageSetBuild :: Text
packageSetBuild = "build " <> packageSetFile <> " -A "

-- | The attribute paths a job's steps build from the package set, in the
-- order the steps first name them, read from the workflow's lines.  A step
-- names one outright, or through its loop's variable, alone or inside a
-- path, the loop being one @for NAME in WORD ...; do@ line.  A build
-- spelled any other way is an error and not a build passed over, so the
-- test fails on a step it cannot read where it would otherwise miss a rung.
jobPackageSetBuilds :: Text -> [Text] -> Either Text [Text]
jobPackageSetBuilds jobId workflow = case dropWhile (/= jobKey) workflow of
  [] -> Left ("the workflow has no job " <> jobId)
  _ : rest -> nub . concat <$> sequence (snd (mapAccumL buildsOn Nothing (takeWhile (not . isJobKey) rest)))
  where
    jobKey = "  " <> jobId <> ":"
    -- Under @jobs:@ a line indented by exactly two spaces is a job's key or
    -- a comment.
    isJobKey line = case T.unpack (T.take 3 line) of
      [' ', ' ', next] -> next /= ' ' && next /= '#'
      _ -> False
    -- Each line's builds, with the loop the line is in: the last for line
    -- at or above it.
    buildsOn loop line =
      let current = forLoop line <|> loop
       in (current, lineBuilds current line)
    forLoop line = case T.words line of
      "for" : name : "in" : items
        | (leading, [final, "do"]) <- break (";" `T.isSuffixOf`) items -> Just (name, leading ++ [T.dropEnd 1 final])
      _ -> Nothing
    lineBuilds loop line
      | "#" `T.isPrefixOf` T.stripStart line = Right []
      | (_, found) <- T.breakOn packageSetBuild line,
        not (T.null found) =
          maybe (unread line) Right (expand loop (T.filter (/= '"') (T.takeWhile isArgumentChar (T.drop (T.length packageSetBuild) found))))
      | packageSetFile `T.isInfixOf` line = unread line
      | otherwise = Right []
    isArgumentChar c = isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` ("._-$\"" :: String)
    -- The paths an argument names, or nothing where it is not a path and
    -- not the loop's variable in one.
    expand loop argument
      | T.null argument = Nothing
      | not ("$" `T.isInfixOf` argument) = Just [argument]
      | Just (name, items) <- loop,
        let variable = "$" <> name,
        variable `T.isInfixOf` argument,
        not ("$" `T.isInfixOf` T.replace variable "" argument) =
          Just [T.replace variable item argument | item <- items]
      | otherwise = Nothing
    unread line = Left ("a step of " <> jobId <> " builds from " <> packageSetFile <> " in a way this test does not read: " <> T.strip line)

-- | @pkgs\/windows\/rungs.nix@ against the CI jobs whose rungs it lists: each
-- stage's list is the attributes its job's steps build from the package
-- set, in the order the steps first name them, and each names a
-- derivation.  CI realises a stage's list before those steps from the
-- public cache in substitute mode, and in the next stage's job from the
-- stage's handoff, and fails on a listed rung that was built though the
-- cache holds it, so a rung a step builds and the list lacks is one those
-- checks never see.
testWindowsRungs :: IO [Bool]
testWindowsRungs = do
  putStrLn "pkgs/rungs"
  cwd <- Dir.getCurrentDirectory
  storeDir <- evalNixIOStoreDir
  workflow <- try (BS.readFile ladderWorkflowPath)
  searchPaths <- parseNixPath (TE.encodeUtf8 (T.pack (cwd </> "data")))
  st <- newEvalStateWith testLogger unrestrictedPolicy searchPaths (testSessionStore storeDir) storeDir cwd
  let windows = cwd </> "pkgs" </> "windows"
      source =
        T.concat
          [ "let pkgs = import ",
            nixQuotedPath windows,
            "; rungs = import ",
            nixQuotedPath (windows </> "rungs.nix"),
            "; stages = builtins.attrNames rungs;",
            " select = path: builtins.foldl' (set: name: set.${name}) pkgs (builtins.filter builtins.isString (builtins.split \"[.]\" path));",
            " isDerivation = path: let value = select path; in builtins.isAttrs value && (value.type or null) == \"derivation\";",
            " in builtins.concatStringsSep \" \" (builtins.concatMap (stage: [ stage ] ++ rungs.${stage} ++ [ \"",
            stageEnd,
            "\" ]) stages ++ builtins.filter (path: !(isDerivation path)) (builtins.concatMap (stage: rungs.${stage}) stages))"
          ]
      built jobId = case workflow of
        Left (err :: IOException) -> Left (T.pack (ladderWorkflowPath <> ": " <> displayException err))
        Right bytes -> jobPackageSetBuilds jobId (T.lines (bytesText bytes))
  outcome <- case parseNix cwd testOrigin (TE.encodeUtf8 source) of
    Left err -> pure (Left (T.pack (show err)))
    Right parsed -> runEvalIO st $ do
      value <- evalParsed (pure (evalBaseEnv st)) parsed
      case value of
        VStr bytes _ -> pure (stageLists (T.words (bytesText bytes)))
        other -> throwEvalError ("expected the stages as one string, got " <> typeName other)
  case outcome of
    Left err -> sequence [runTest lists (Fail err)]
    Right (stages, notDerivations) -> do
      let rungsOf stage = fromMaybe [] (lookup stage stages)
      sequence $
        [ runTest lists $
            assertEqual "stages" (sort (map fst stageJobs)) (map fst stages),
          runTest "every rung is a derivation of the package set" $
            assertEqual "not derivations" [] (nub notDerivations),
          runTest "each stage's list reaches its last rung" $
            assertEqual "rungs missing" [] [rung | (stage, rung) <- knownRungs, rung `notElem` rungsOf stage]
        ]
          ++ [ runTest ("the " <> stage <> " stage's rungs are what " <> jobId <> " builds from the package set, in order") $
                 either Fail (\named -> assertEqual "rungs" named (rungsOf stage)) (built jobId)
             | (stage, jobId) <- stageJobs
             ]
  where
    lists :: Text
    lists = "rungs.nix lists the ladder's stages"
    -- Each stage of rungs.nix with the job that builds its rungs.
    stageJobs :: [(Text, Text)]
    stageJobs =
      [ ("mes", "mes-windows"),
        ("tinycc", "tinycc-windows"),
        ("gcc46", "gcc46-windows"),
        ("gcc46-cxx", "gcc46-cxx-windows"),
        ("gcc10", "gcc10-windows"),
        ("gcc-latest", "gcc-latest-windows"),
        ("x86_64", "x86_64-windows"),
        ("gcc-x86_64", "gcc-x86_64-windows"),
        ("native-x86_64", "native-x86_64-windows"),
        ("stdenv", "stdenv-windows")
      ]
    -- The rung each stage ends on, so that a workflow read as building
    -- nothing does not pass against an empty list.
    knownRungs :: [(Text, Text)]
    knownRungs = [("mes", "mes.libc"), ("tinycc", "seedStdenv.applets"), ("gcc46", "gcc46"), ("gcc46-cxx", "gcc46-cxx"), ("gcc10", "mingw-w64-crt-latest"), ("gcc-latest", "gcc-latest"), ("x86_64", "mingw-w64-crt-x86_64"), ("gcc-x86_64", "gcc-x86_64"), ("native-x86_64", "gcc-native-x86_64"), ("stdenv", "stdenv.cc")]
    -- What ends a stage's rungs in the string the expression returns: no
    -- attribute path is spelled so.
    stageEnd :: Text
    stageEnd = "|"
    -- The stages with their rungs, then the words after the last stage.
    stageLists :: [Text] -> ([(Text, [Text])], [Text])
    stageLists listed = case break (== stageEnd) listed of
      (stage : rungs, _ : rest) -> let (stages, others) = stageLists rest in ((stage, rungs) : stages, others)
      (others, _) -> ([], others)

-- | The workflows' directory, from the repository root.
workflowsDir :: FilePath
workflowsDir = ".github" </> "workflows"

-- | The values the lines of a workflow give a key as literals: @KEY: VALUE@,
-- the value bare or quoted.  An expression or an empty value is none.
literalValues :: Text -> [Text] -> [Text]
literalValues key = mapMaybe value
  where
    prefix = key <> ":"
    value line = T.stripPrefix prefix (T.stripStart line) >>= literal . T.dropAround (`elem` ("\"'" :: String)) . T.strip
    literal text
      | T.null text || "${{" `T.isPrefixOf` text = Nothing
      | otherwise = Just text

-- | The versions a line names as an @ormolu-VERSION@ package id: the digits
-- and dots after each @ormolu-@ that a digit follows.
ormoluVersions :: Text -> [Text]
ormoluVersions line =
  [ version
  | (_, after) <- T.breakOnAll ormoluPrefix line,
    let version = T.takeWhile (\c -> isDigit c || c == '.') (T.drop (T.length ormoluPrefix) after),
    maybe False (isDigit . fst) (T.uncons version)
  ]
  where
    ormoluPrefix = "ormolu-"

-- | The values that workflows spell in more than one place, and that have
-- to agree.  GHC's version is in every haskell-actions/setup step, and
-- ormolu's in CI's Ormolu job and in caches.yml: caches.yml saves the
-- caches the other workflows restore under keys that name these versions,
-- so a bump that missed it would leave every one of them unsaved.  The
-- public cache's address and signing key are the CACHE_URL and
-- CACHE_PUBLIC_KEY of CI, Seed ladder and Public cache, whose jobs take
-- from the cache or publish to it, and a cache move or a key rotation
-- that missed one would leave that workflow's jobs asking the old address
-- or trusting the old key.  Each value has one spelling, and the
-- workflows known to name it are among those found naming it, so a
-- reading that finds nothing fails.
testWorkflowSpellings :: IO [Bool]
testWorkflowSpellings = do
  putStrLn "ci/spellings"
  outcome <- try $ do
    names <- Dir.listDirectory workflowsDir
    let workflows = sort [name | name <- names, ".yml" `T.isSuffixOf` T.pack name]
    traverse (\name -> (,) name . T.lines . bytesText <$> BS.readFile (workflowsDir </> name)) workflows
  case outcome of
    Left (err :: IOException) -> sequence [runTest "the workflows read" (Fail (T.pack (workflowsDir <> ": " <> displayException err)))]
    Right workflows ->
      let code = [(name, line) | (name, ls) <- workflows, line <- ls, not ("#" `T.isPrefixOf` T.stripStart line)]
          ghc = [(name, version) | (name, line) <- code, version <- literalValues "ghc-version" [line]]
          ormolu =
            [(name, version) | (name, line) <- code, version <- ormoluVersions line]
              ++ [(name, version) | (name, line) <- code, version <- literalValues "ORMOLU_VERSION" [line]]
          cacheUrl = [(name, url) | (name, line) <- code, url <- literalValues "CACHE_URL" [line]]
          cacheKey = [(name, key) | (name, line) <- code, key <- literalValues "CACHE_PUBLIC_KEY" [line]]
       in sequence
            [ runTest "every workflow names one GHC version, ci.yml and caches.yml among them" $
                oneSpelling "GHC's version" ["caches.yml", "ci.yml"] ghc,
              runTest "every workflow names one ormolu version, ci.yml and caches.yml among them" $
                oneSpelling "ormolu's version" ["caches.yml", "ci.yml"] ormolu,
              runTest "every workflow names one public cache address, ci.yml, public-cache.yml and seed-ladder.yml among them" $
                oneSpelling "the public cache's address" cacheWorkflows cacheUrl,
              runTest "every workflow names one public cache key, ci.yml, public-cache.yml and seed-ladder.yml among them" $
                oneSpelling "the public cache's key" cacheWorkflows cacheKey
            ]
  where
    cacheWorkflows :: [FilePath]
    cacheWorkflows = ["ci.yml", "public-cache.yml", "seed-ladder.yml"]
    oneSpelling :: Text -> [FilePath] -> [(FilePath, Text)] -> TestResult
    oneSpelling value known found = case ([name | name <- known, name `notElem` map fst found], nub (map snd found)) of
      ([], [_]) -> Pass
      ([], _) -> Fail (value <> " is spelled more than one way: " <> T.pack (show found))
      (missing, _) -> Fail (value <> " is named in none of " <> T.pack (show missing))
