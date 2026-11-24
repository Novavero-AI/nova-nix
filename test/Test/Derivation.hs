{-# LANGUAGE LambdaCase #-}

-- | Tests of derivations, their ATerm and structured attributes, and the
-- build environments develop captures from them.
module Test.Derivation
  ( testDerivation,
    testBatchG,
    testDerivationWrapper,
    testStructuredAttrs,
    testFromATerm,
    testInputAddressed,
    testDevelopCapture,
    testDevelopEnvironment,
    testDevelopShell,
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Either (isLeft, isRight)
import Data.List (find, sort)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Nix.Cli.Args as Cli
import Nix.Derivation (Derivation (..), DerivationOutput (..), DrvFile, Platform (..), currentPlatform, drvFileATerm, drvFileDerivation, drvFilePath, extraPlatforms, fromATerm, inputAddressed, platformToText, textToPlatform, toATerm, toATermForHash)
import Nix.Derivation.StructuredAttrs (StructuredAttrs (..), decodeStructuredAttrs, encodeStructuredAttrs, renderAttrsShell, stringsAttr, withOutputPlaceholders)
import Nix.Develop.Capture (CaptureFailure (..), OutputObject (..), captureDerivation, captureFailureMessage, environmentOutput, getEnvScriptPath, noEnvironmentMessage)
import Nix.Develop.Environment (AttrsFiles (..), BuildEnvironment (..), EnvironmentScript (..), Prompt (..), ScriptDirectories (..), ScriptFile (..), Shell (..), ShellMode (..), Variable (..), buildEnvironmentJson, environmentScript, escapeShellArgAlways, ignoreVars, parseBuildEnvironment, renderRcfile)
import Nix.Develop.Shell (EnvironmentBase (..), EnvironmentChanges (..), bashProgram, shellArguments, shellEnvironment, shellModeFor)
import Nix.Eval (MonadEval (..), NixValue (..), evalParsed, mkStr, typeName)
import Nix.Eval.CanonPath (canonPathValue)
import Nix.Eval.IO (evalBaseEnv, runEvalIO)
import Nix.Hash (hashPlaceholder, sha256Digest)
import qualified Nix.Hash as Hash
import Nix.Json (IntegerRange (..), Json (..), JsonBuilder (..), parseJson, parseJsonInto, renderJson)
import Nix.Parser (parseNix)
import Nix.Store (Store (..), closeStore, isValid, openStore, writeDrv, writeDrvClosure)
import Nix.Store.DB (PathRegistration (..), queryReferences, registerPath)
import Nix.Store.Path (StoreDir (..), StorePath, defaultStoreDir, storePathToFilePath, storePathToText)
import Nix.Store.Path.Internal (StorePath (..))
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import qualified System.Info as SI
import qualified System.Process as Proc
import Test.Harness (TestResult (..), andThen, assertEqual, assertEval, assertEvalError, assertEvalFail, assertLeft, assertRight, bytesText, evalDerivationIO, evalNixIO, evalNixIOStoreDir, forceRemoveIfExists, genDrvName, genFixturePath, genJson, genTextOf, runTest, runTestIO, runTestM, simpleTestDrv, testBaseDir, testEvalState, testOrigin)
import qualified Test.QuickCheck as QC

-- ---------------------------------------------------------------------------
-- Test harness
-- ---------------------------------------------------------------------------

-- | The bytes of a string value, and nothing for any other value.
stringBytes :: NixValue -> BS.ByteString
stringBytes (VStr bytes _) = bytes
stringBytes _ = BS.empty

-- ---------------------------------------------------------------------------
-- Tests: Derivation (existing)
-- ---------------------------------------------------------------------------

testDerivation :: IO [Bool]
testDerivation = do
  putStrLn "derivation"
  sequence
    [ runTest "current platform is known" $
        case currentPlatform of
          OtherPlatform _ -> Fail "currentPlatform returned OtherPlatform"
          _ -> Pass
    ]

-- ---------------------------------------------------------------------------
-- Tests: Batch G - ATerm serialization
-- ---------------------------------------------------------------------------

testBatchG :: IO [Bool]
testBatchG = do
  putStrLn "derivation/aterm"
  let minimalDrv =
        Derivation
          { drvOutputs = [],
            drvInputDrvs = Map.empty,
            drvInputSrcs = [],
            drvPlatform = X86_64_Linux,
            drvBuilder = "/bin/sh",
            drvArgs = [],
            drvEnv = Map.empty
          }
  let drvWithOutput =
        minimalDrv
          { drvOutputs =
              [ DerivationOutput
                  { doName = "out",
                    doPath = StorePath "abc" "hello",
                    doHashAlgo = "",
                    doHash = ""
                  }
              ]
          }
  let drvWithEnv =
        minimalDrv
          { drvEnv = Map.fromList [("name", "hello"), ("system", "x86_64-linux")]
          }
  sequence
    [ runTest "ATerm minimal" $
        let aterm = toATerm minimalDrv
         in if BS.isPrefixOf "Derive(" aterm && BS.isSuffixOf ")" aterm
              then Pass
              else Fail ("bad ATerm: " <> bytesText aterm),
      runTest "ATerm has output" $
        let aterm = toATerm drvWithOutput
         in if "\"out\"" `BS.isInfixOf` aterm
              then Pass
              else Fail ("missing output in ATerm: " <> bytesText aterm),
      runTest "ATerm env sorted" $
        let aterm = toATerm drvWithEnv
         in -- "name" should come before "system" in sorted order
            case (BS.breakSubstring "\"name\"" aterm, BS.breakSubstring "\"system\"" aterm) of
              ((before1, _), (before2, _)) ->
                if BS.length before1 < BS.length before2
                  then Pass
                  else Fail ("env not sorted in ATerm: " <> bytesText aterm),
      runTest "ATerm string escaping" $
        let drv = minimalDrv {drvEnv = Map.fromList [("msg", "hello\nworld")]}
            aterm = toATerm drv
         in if "\\n" `BS.isInfixOf` aterm
              then Pass
              else Fail ("missing escaped newline: " <> bytesText aterm),
      runTest "ATerm deterministic" $
        assertEqual "deterministic" (toATerm minimalDrv) (toATerm minimalDrv),
      -- platformToText
      runTest "platformToText linux" $
        assertEqual "linux" "x86_64-linux" (platformToText X86_64_Linux),
      runTest "platformToText darwin" $
        assertEqual "darwin" "x86_64-darwin" (platformToText X86_64_Darwin),
      runTest "platformToText aarch64-darwin" $
        assertEqual "aarch64" "aarch64-darwin" (platformToText Aarch64_Darwin),
      runTest "platformToText windows" $
        assertEqual "windows" "x86_64-windows" (platformToText X86_64_Windows),
      runTest "platformToText aarch64-linux" $
        assertEqual "aarch64-linux" "aarch64-linux" (platformToText Aarch64_Linux),
      runTest "platformToText i686" $
        assertEqual "i686" ["i686-linux", "i686-windows"] (map platformToText [I686_Linux, I686_Windows]),
      runTest "platformToText other" $
        assertEqual "other" "riscv64-freebsd" (platformToText (OtherPlatform "riscv64-freebsd")),
      -- The i686 spellings parse to their own constructors, not to
      -- OtherPlatform, or extraPlatforms could never name them.
      runTest "textToPlatform round-trips every named platform" $
        assertEqual
          "round-trip"
          namedPlatforms
          (map (textToPlatform . platformToText) namedPlatforms),
      -- Upstream's default extra-platforms, and nothing more: the x86_64
      -- hosts run their i686 counterpart, every other host runs only itself.
      runTest "extraPlatforms is the i686 counterpart on x86_64 hosts only" $
        assertEqual
          "extra"
          [[I686_Linux], [I686_Windows], [], [], [], [], []]
          (map extraPlatforms [X86_64_Linux, X86_64_Windows, X86_64_Darwin, Aarch64_Darwin, Aarch64_Linux, I686_Linux, I686_Windows])
    ]
  where
    namedPlatforms = [X86_64_Linux, X86_64_Darwin, Aarch64_Darwin, X86_64_Windows, Aarch64_Linux, I686_Linux, I686_Windows]

-- ---------------------------------------------------------------------------
-- Tests: what a program can observe of the derivation wrapper
-- ---------------------------------------------------------------------------

-- | What a program can observe of @derivation@ beyond the attributes
-- 'testBatchH' reads, each expected value written out: the name the
-- pattern binds, which builtin forces @outputs@ and in what order, how the
-- layers of the result replace each other, and the arguments' own values
-- in the result.  The parity job probes the same things against upstream's
-- nix-instantiate (test/data/derivation-probes.txt).
testDerivationWrapper :: IO [Bool]
testDerivationWrapper = do
  putStrLn "derivation/wrapper"
  tmpBase <- getTemporaryDirectory
  let required = "name = \"w\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; "
      drv fields = "derivation { " <> required <> fields <> " }"
      atWhnf fields = "builtins.seq (" <> drv fields <> ") null"
      throwing = drv "outputs = [ \"out\" \"dev\" ]; x = throw \"forced\";"
  sequence
    [ runTest "toXML shows the name the pattern binds" $
        assertEval
          "xml"
          "builtins.toXML derivation"
          (mkStr "<?xml version='1.0' encoding='utf-8'?>\n<expr>\n  <function>\n    <attrspat ellipsis=\"1\" name=\"drvAttrs\">\n      <attr name=\"outputs\" />\n    </attrspat>\n  </function>\n</expr>\n"),
      -- map's error rather than head's: map forces outputs first.
      runTest "map forces outputs before head does" $
        assertEvalError "map" (atWhnf "outputs = \"out\";") "builtins.map: expected a list, got a string",
      runTest "listToAttrs forces the output names first to last" $
        assertEvalError "first" (atWhnf "outputs = [ (throw \"the first\") \"out\" (throw \"the last\") ];") "the first",
      runTest "every output name is forced with the derivation" $
        assertEvalError "last" (atWhnf "outputs = [ \"out\" (throw \"the last\") ];") "the last",
      runTest "an argument that throws stays unforced through the result's attributes" $
        assertEval
          "spine"
          ("let d = " <> throwing <> "; in builtins.toJSON [ (builtins.attrNames d) (map (o: o.outputName) d.all) d.dev.type (builtins.attrNames d.drvAttrs) ]")
          (mkStr "[[\"all\",\"builder\",\"dev\",\"drvAttrs\",\"drvPath\",\"name\",\"out\",\"outPath\",\"outputName\",\"outputs\",\"system\",\"type\",\"x\"],[\"out\",\"dev\"],\"derivation\",[\"builder\",\"name\",\"outputs\",\"system\",\"x\"]]"),
      -- == forces both sides, then takes one value as equal to itself, so a
      -- function is equal only to itself.  Under the IO evaluator, which
      -- keeps a forced binding's value as upstream's does, where the pure
      -- one evaluates args again at each use.
      runTestIO
        "the arguments' values reach the result unchanged"
        tmpBase
        ("let args = { " <> required <> "f = y: y; }; d = derivation args; in builtins.intersectAttrs { f = null; } d == builtins.intersectAttrs { f = null; } args")
        (VBool True),
      runTest "the result's own attributes replace the outputs' and the arguments'" $
        assertEval
          "layers"
          ("let d = " <> drv "outputs = [ \"out\" \"all\" ]; type = \"t\"; all = 1; out = 6;" <> "; in builtins.toJSON [ d.type (builtins.isList d.all) d.out.outputName d.drvAttrs.type d.drvAttrs.all ]")
          (mkStr "[\"derivation\",true,\"out\",\"t\",1]")
    ]

-- ---------------------------------------------------------------------------
-- Tests: structured attributes and the JSON they travel as
-- ---------------------------------------------------------------------------

-- | @__structuredAttrs@ end to end on the evaluation side, and the JSON
-- rules the evaluator and builder share.  Every drvPath, rendering and
-- @.attrs.sh@ expected here was read off Nix 2.33.2 (nix-instantiate and
-- a build on aarch64-darwin), whose code for each is 2.24.9's.
testStructuredAttrs :: IO [Bool]
testStructuredAttrs = do
  putStrLn "derivation/structured-attrs"
  tmpBase <- getTemporaryDirectory
  let base = "name = \"sa\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; __structuredAttrs = true; "
      structured attrs = "(derivation { " <> base <> attrs <> " })"
      withoutContext expr = "builtins.unsafeDiscardStringContext " <> expr
      drvPathIs label attrs expected =
        runTest label (assertEval label (withoutContext (structured attrs <> ".drvPath")) (mkStr expected))
      dependency = "let d = derivation { name = \"d\"; system = \"x\"; builder = \"b\"; }; in "
  envResult <- evalDerivationIO tmpBase (structured "outputs = [ \"out\" \"dev\" \"lib\" ];")
  -- A file for the outPath that is a path, copied to the store as upstream
  -- copies it: "hello\n" named x is m5zd3qj7...-x there.
  let jsonDir = tmpBase </> "nova-nix-test-json-outpath"
  createDirectoryIfMissing True jsonDir
  BS.writeFile (jsonDir </> "x") "hello\n"
  copiedOutPath <- evalNixIO jsonDir "builtins.unsafeDiscardStringContext (builtins.toJSON { outPath = ./x; })"
  lengthOfPath <- evalNixIO jsonDir "builtins.stringLength ./x"
  substringOfPath <- evalNixIO jsonDir "builtins.substring 11 32 ./x"
  substringContext <- evalNixIO jsonDir "builtins.hasContext (builtins.substring 11 32 ./x)"
  discardedPath <- evalNixIO jsonDir "builtins.unsafeDiscardStringContext ./x"
  discardedContext <- evalNixIO jsonDir "builtins.hasContext (builtins.unsafeDiscardStringContext ./x)"
  copiedContext <- evalNixIO jsonDir "builtins.attrNames (builtins.getContext (builtins.toJSON { outPath = ./x; })) == [ \"/nix/store/m5zd3qj7c4bs4g5d8hqvs5pc5xicclfg-x\" ]"
  -- The IO evaluator, because it is the one with a store to copy into.
  uncopiedPath <- evalNixIO jsonDir "builtins.toJSON { __toString = s: ./x; }"
  uncopiedNested <- evalNixIO jsonDir "builtins.toJSON { __toString = s: { outPath = ./x; }; }"
  removeDirectoryRecursive jsonDir
  let jsonDirPath = mkStr ("\"" <> canonPathValue (T.pack (jsonDir </> "x")) <> "\"")
  sequence
    [ drvPathIs
        "a nested set and a mixed list are one JSON object"
        "nested = { a = 1; b = { c = [ \"x\" \"y\" ]; }; \"with space\" = true; }; list = [ \"a\" \"b c\" 1 2.5 null true false ];"
        "/nix/store/rkxyljjwm2miccr3ffbkvgm7fzmjqddg-sa.drv",
      drvPathIs
        "scalars render as nlohmann renders them"
        "i = 42; neg = -7; f = 1.0; f2 = 0.1; big = 1.0e20; small = 1.0e-7; b = true; n = null; s = \"str\\n\\t\\\"q\\\"\\\\\";"
        "/nix/store/hgzhpjmp6cpjj0r768xp982an303rh2q-sa.drv",
      drvPathIs
        "member names order by their bytes"
        "\"\233\" = 1; \"Z\" = 2; \"a\" = 3; \"_\" = 4; \"1x\" = 5;"
        "/nix/store/s2aaxxxqvmdh95wqq3sa75fqal0nv2vf-sa.drv",
      drvPathIs "a kept null is a JSON null" "n = null;" "/nix/store/r3880xygjwsqk8a4xiq8zrbhpanlf9a3-sa.drv",
      drvPathIs
        "__ignoreNulls leaves a null out of the object"
        "n = null; __ignoreNulls = true;"
        "/nix/store/cms7b67rf9bn7b376f64i05v1cw2chrh-sa.drv",
      drvPathIs
        "a placeholder stays a placeholder in the object"
        "here = builtins.placeholder \"out\";"
        "/nix/store/2iqn1i3rl4x9ll8a6j46k1rkk9c3fkd5-sa.drv",
      drvPathIs
        "a fixed-output derivation keeps its hash fields in the object"
        "outputHash = \"sha256-47DEQpj8HBSKTXA6xJbgU1CNMmz7W08F06XOTYP1hmM=\"; outputHashMode = \"flat\"; outputHashAlgo = \"sha256\";"
        "/nix/store/dp6sib5psffrm1krca5l9wn9nshspn7f-sa.drv",
      drvPathIs
        "a set under outputChecks is passed as it is"
        "outputChecks.out = { allowedReferences = [ ]; maxSize = 100; };"
        "/nix/store/xzkk2dxqm69akjjshlvk45s8yd6y1dbn-sa.drv",
      runTest "__structuredAttrs = false keeps one variable per attribute" $
        assertEval
          "sa-false"
          (withoutContext "(derivation { name = \"sa\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; __structuredAttrs = false; x = [ \"a\" \"b\" ]; }).drvPath")
          (mkStr "/nix/store/i9rm0fa7pbzrbrsaakninfa4232wmj7c-sa.drv"),
      runTest "every output of a structured derivation gets upstream's path" $
        assertEval
          "sa-outputs"
          (withoutContext (structured "outputs = [ \"out\" \"dev\" \"lib\" ];" <> ".dev.outPath"))
          (mkStr "/nix/store/jfnikb9k7953srg5mlp3mccmqaijvxyy-sa-dev"),
      runTest "the environment holds __json and the outputs, nothing else" $
        assertRight "sa-env" envResult $ \drv ->
          assertEqual
            "sa-env"
            [ ("__json", "{\"builder\":\"/bin/sh\",\"name\":\"sa\",\"outputs\":[\"out\",\"dev\",\"lib\"],\"system\":\"x86_64-linux\"}"),
              ("dev", "/nix/store/jfnikb9k7953srg5mlp3mccmqaijvxyy-sa-dev"),
              ("lib", "/nix/store/zpjd7d2l5jqzynv3377a36nk6rzb5il4-sa-lib"),
              ("out", "/nix/store/bs6ppbphi38qlbjfz67pfk86sx7l4vks-sa")
            ]
            (Map.toList (drvEnv drv)),
      -- toJSON's rules for a set, which the object above shares.
      runTest "toJSON of a set with outPath converts the outPath value, not its string" $
        assertEval "json-outpath-int" "builtins.toJSON { outPath = 1; }" (mkStr "1")
          `andThen` assertEval "json-outpath-set" "builtins.toJSON { outPath = { a = 1; }; }" (mkStr "{\"a\":1}")
          `andThen` assertEval "json-outpath-list" "builtins.toJSON { outPath = [ 1 { outPath = 2; } ]; }" (mkStr "[1,2]")
          `andThen` assertEval "json-outpath-null" "builtins.toJSON { outPath = null; }" (mkStr "null"),
      runTest "toJSON of a set whose outPath is a path copies it to the store, with context" $
        assertRight "json-outpath-path" copiedOutPath (assertEqual "json-outpath-path" (mkStr "\"/nix/store/m5zd3qj7c4bs4g5d8hqvs5pc5xicclfg-x\""))
          `andThen` assertRight "json-outpath-path-context" copiedContext (assertEqual "json-outpath-path-context" (VBool True)),
      runTest "a builtin taking a string copies a path to the store first" $
        assertRight "strlen-path" lengthOfPath (assertEqual "strlen-path" (VInt 45))
          `andThen` assertRight "substring-path" substringOfPath (assertEqual "substring-path-text" "m5zd3qj7c4bs4g5d8hqvs5pc5xicclfg" . stringBytes)
          `andThen` assertRight "substring-path-context" substringContext (assertEqual "substring-path-context" (VBool True))
          `andThen` assertRight "discard-path" discardedPath (assertEqual "discard-path" (mkStr "/nix/store/m5zd3qj7c4bs4g5d8hqvs5pc5xicclfg-x"))
          `andThen` assertRight "discard-path-context" discardedContext (assertEqual "discard-path-context" (VBool False)),
      runTest "toJSON of a __toString set returning a path keeps the path's own text, uncopied" $
        assertRight "json-tostring-path" uncopiedPath (assertEqual "json-tostring-path" jsonDirPath)
          `andThen` assertRight "json-tostring-nested" uncopiedNested (assertEqual "json-tostring-nested" jsonDirPath),
      runTest "toJSON of a __toString set returning a string keeps its context" $
        let json = "(builtins.toJSON { __toString = s: \"${d}\"; })"
         in assertEval "json-tostring-string" (dependency <> withoutContext json) (mkStr "\"/nix/store/74ii7m3ms7x609g9mcy8j52jfvpdizcp-d\"")
              `andThen` assertEval
                "json-tostring-context"
                (dependency <> "builtins.getContext " <> json <> " == { \"/nix/store/nzvhjdd45mdh8mkqm4vz4hdqsjrvkm8j-d.drv\" = { outputs = [ \"out\" ]; }; }")
                (VBool True),
      runTest "toJSON of a __toString set returning an integer is an error, not a coercion" $
        assertEvalFail "json-tostring-int" "builtins.toJSON { __toString = s: 1; }",
      drvPathIs
        "a structured attribute's outPath and __toString follow toJSON's rules"
        "o = { outPath = { inner = 1; }; }; i = { outPath = 1; }; t = { __toString = s: \"str\"; };"
        "/nix/store/rz7pfawzsr1a5ig11s1xssbsc2mxys9b-sa.drv",
      runTest "toJSON escapes backspace and form feed by name" $
        assertEval
          "json-bf"
          "builtins.toJSON (builtins.fromJSON \"\\\"\\\\u0008\\\\u000c\\\\u0001\\\"\")"
          (mkStr "\"\\b\\f\\u0001\""),
      runTest "__structuredAttrs must be a boolean" $
        assertEvalFail "sa-flag" "(derivation { name = \"sa\"; system = \"x\"; builder = \"b\"; __structuredAttrs = \"yes\"; }).drvPath",
      runTest "a structured system may not refer to a store path" $
        assertEvalError
          "sa-system-ctx"
          (dependency <> "(derivation { name = \"sa\"; builder = \"/bin/sh\"; __structuredAttrs = true; system = \"${d}\"; }).drvPath")
          "the string '/nix/store/74ii7m3ms7x609g9mcy8j52jfvpdizcp-d' is not allowed to refer to a store path (such as '!out!nzvhjdd45mdh8mkqm4vz4hdqsjrvkm8j-d.drv')",
      runTest "a structured output name may not refer to a store path" $
        assertEvalError
          "sa-output-ctx"
          (dependency <> structured "outputs = [ \"out\" \"${d}\" ];" <> ".drvPath")
          "the string '/nix/store/74ii7m3ms7x609g9mcy8j52jfvpdizcp-d' is not allowed to refer to a store path (such as '!out!nzvhjdd45mdh8mkqm4vz4hdqsjrvkm8j-d.drv')",
      runTest "a structured builder must be a string, not a value coerced to one" $
        assertEvalError
          "sa-builder"
          "(derivation { name = \"sa\"; system = \"x\"; __structuredAttrs = true; builder = 1; }).drvPath"
          "expected a string but found an integer: 1",
      runTest "a structured outputHash must be a string" $
        assertEvalError "sa-outputhash" (structured "outputHash = 1;" <> ".drvPath") "expected a string but found an integer: 1",
      runTest "a function cannot become JSON" $
        assertEvalFail "sa-function" (structured "f = x: x;" <> ".drvPath"),
      -- The JSON both halves share.
      runTest "JSON escapes control characters as nlohmann does" $
        assertEqual
          "json-escape"
          (TE.encodeUtf8 "\"a\\b\\f\\n\\r\\t\\u0001\\u001f\127/\\\"\\\\\233\"")
          (renderJson (JsonString "a\b\f\n\r\t\1\31\127/\"\\\233")),
      runTest "JSON numbers render as nlohmann renders them" $
        assertEqual
          "json-numbers"
          "[1.0,1e+20,1e-07,0.1,null,null,-7,18446744073709551615]"
          (renderJson (JsonArray [JsonFloat 1, JsonFloat 1e20, JsonFloat 1e-7, JsonFloat 0.1, JsonFloat (1 / 0), JsonFloat (0 / 0), JsonInt (-7), JsonInt 18446744073709551615])),
      runTest "upstream's .attrs.json reads back and renders to the same bytes" $
        assertRight "json-upstream" (decodeStructuredAttrs upstreamAttrsJson) $ \attrs ->
          assertEqual "json-upstream" (TE.encodeUtf8 upstreamAttrsJson) (encodeStructuredAttrs attrs),
      runTest "the .attrs.sh renderer writes what upstream writes" $
        assertRight "attrs-sh" (decodeStructuredAttrs upstreamAttrsJson) $ \attrs ->
          assertEqual "attrs-sh" upstreamAttrsShell (renderAttrsShell attrs),
      runTest "outputs become an object of placeholders" $
        assertEqual
          "outputs-object"
          (StructuredAttrs (Map.fromList [("outputs", JsonObject (Map.fromList [("dev", JsonString (hashPlaceholder "dev")), ("out", JsonString (hashPlaceholder "out"))]))]))
          (withOutputPlaceholders ["out", "dev"] (StructuredAttrs (Map.fromList [("outputs", JsonArray [JsonString "out", JsonString "dev"])]))),
      runTest "a null __json is an empty object, another non-object an error" $
        assertEqual "decode-shape" (Right (StructuredAttrs Map.empty)) (decodeStructuredAttrs "null")
          `andThen` assertLeft "decode-array" (decodeStructuredAttrs "[1]"),
      runTest "a list-of-strings attribute is read strictly" $
        let attrs = StructuredAttrs (Map.fromList [("good", JsonArray [JsonString "a", JsonString "b"]), ("mixed", JsonArray [JsonString "a", JsonInt 1]), ("scalar", JsonString "a b")])
         in assertEqual "strings-good" (Right (Just ["a", "b"])) (stringsAttr "good" attrs)
              `andThen` assertEqual "strings-absent" (Right Nothing) (stringsAttr "absent" attrs)
              `andThen` assertLeft "strings-mixed" (stringsAttr "mixed" attrs)
              `andThen` assertLeft "strings-scalar" (stringsAttr "scalar" attrs),
      runTest "JSON parsing decodes every escape, surrogate pairs included" $
        assertEqual
          "parse-escapes"
          (Right (JsonString "\"\\/\b\f\n\r\t\233\128512"))
          (parseJson "\"\\\"\\\\\\/\\b\\f\\n\\r\\t\\u00E9\\ud83d\\ude00\""),
      runTest "JSON parsing keeps integers apart from floats, as nlohmann's lexer does" $
        assertEqual
          "parse-numbers"
          (Right (JsonArray [JsonInt 18446744073709551615, JsonFloat 18446744073709551616, JsonInt (-9223372036854775808), JsonFloat (-9223372036854775809), JsonInt 0, JsonFloat 1, JsonFloat 100]))
          (parseJson "[18446744073709551615,18446744073709551616,-9223372036854775808,-9223372036854775809,-0,1.0,1E2]"),
      runTest "JSON parsing takes the last of a repeated name, whitespace and a byte order mark around" $
        assertEqual
          "parse-object"
          (Right (JsonObject (Map.fromList [("a", JsonArray [JsonInt 2, JsonNull])])))
          (parseJson "\65279 \n{ \"a\" : 1 , \"a\":[ 2 ,null ] }\r\n"),
      runTest "JSON parsing hands a builder each object's members in document order, a repeated name each time" $
        let memberNames =
              JsonBuilder
                { buildNull = [],
                  buildBool = const [],
                  buildInt = const [],
                  buildFloat = const [],
                  buildString = const [],
                  buildArray = concat,
                  buildObject = concatMap (uncurry (:))
                }
         in assertEqual
              "parse-member-order"
              (Right ["zb", "zc", "za", "zd", "zb"])
              (parseJsonInto NixRange memberNames "{\"zb\":{\"zc\":1},\"za\":[{\"zd\":null}],\"zb\":2}"),
      runTest "JSON parsing refuses what nlohmann refuses" $
        foldr
          (\bad acc -> assertLeft ("parse-refuses " <> bad) (parseJson bad) `andThen` acc)
          Pass
          ["\"a\tb\"", "01", "[1,]", "1e400", "{\"a\" 1}", "tru", "1 2", "\"\\ud800\"", "\"\\udc00\"", "\"\\x\"", "1.", "-", "\"unterminated", ""],
      runTestM "rendered JSON parses back to the value it came from" $ do
        result <-
          QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} $
            QC.forAll (genJson 3) $ \json ->
              (parseJson =<< either (Left . T.pack . show) Right (TE.decodeUtf8' (renderJson json))) QC.=== Right json
        pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))
    ]

-- | A subset of the @.attrs.json@ a Nix 2.33.2 build handed its builder
-- (the members holding no store path), byte for byte.
upstreamAttrsJson :: Text
upstreamAttrsJson =
  "{\"1x\":\"skipped\",\"badList\":[\"a\",[\"nested\"]],\"badObj\":{\"k\":{\"nested\":1}},\"big\":1099511627776,\"ctl\":\"a\\b\\f\\u0001\\u001f\127/\\\"\\\\z\",\"emptyList\":[],\"emptyObj\":{},\"f\":1.5,\"fbig\":1e+20,\"fi\":3.0,\"fl\":false,\"fltList\":[1.5],\"fltObj\":{\"a\":1,\"b\":2.5},\"fneg\":-1e+20,\"fneg2\":-2.5,\"fsmall\":0.1,\"ftrunc\":16777217.5,\"huge\":9223372036854775807,\"int\":42,\"list\":[\"a\",\"b c\",1,2.0,true,false,null],\"n\":null,\"neg\":-7,\"obj\":{\"b\":false,\"k\":\"v\",\"key with space\":1,\"q\":\"'\"},\"str\":\"it's a \\\"test\\\"\\n\",\"t\":true,\"uni\":\"\233\8364\128512\",\"with space\":\"skipped\"}"

-- | The lines of that build's @.attrs.sh@ for the members of
-- 'upstreamAttrsJson'.  Missing ones (a non-integral number, a nested
-- array or object, a name bash cannot hold) are upstream's omissions;
-- @fbig@ and @fneg@ are aarch64's saturating float conversion.
upstreamAttrsShell :: Text
upstreamAttrsShell =
  "declare big=0\ndeclare ctl='a\8\12\1\31\127/\"\\z'\ndeclare -a emptyList=()\ndeclare -A emptyObj=()\ndeclare fbig=2147483647\ndeclare fi=3\ndeclare fl=\ndeclare fneg=-2147483648\ndeclare ftrunc=16777217\ndeclare huge=-1\ndeclare int=42\ndeclare -a list=('a' 'b c' 1 2 1  '' )\ndeclare n=''\ndeclare neg=-7\ndeclare -A obj=(['b']= ['k']='v' ['key with space']=1 ['q']=''\\''' )\ndeclare str='it'\\''s a \"test\"\n'\ndeclare t=1\ndeclare uni='\233\8364\128512'\n"

-- ---------------------------------------------------------------------------
-- Tests: fromATerm + Derivation Output Population (Phase 2, Batch 3)
-- ---------------------------------------------------------------------------

-- | A complex test derivation with multiple outputs, input drvs, and input srcs.
complexTestDrv :: Derivation
complexTestDrv =
  Derivation
    { drvOutputs =
        [ DerivationOutput "dev" (StorePath "dddddddddddddddddddddddddddddddd" "pkg-2.0-dev") "" "",
          DerivationOutput "out" (StorePath "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "pkg-2.0") "" ""
        ],
      drvInputDrvs =
        Map.fromList
          -- Hash fixtures stay inside the nix-base32 alphabet (no e o u t):
          -- fromATerm parses these through parseStorePath, which
          -- charset-checks the hash component.
          [ (StorePath "cccccccccccccccccccccccccccccccc" "dep1.drv", ["out"]),
            (StorePath "ffffffffffffffffffffffffffffffff" "dep2.drv", ["lib", "out"])
          ],
      drvInputSrcs = [StorePath "gggggggggggggggggggggggggggggggg" "source.tar.gz"],
      drvPlatform = Aarch64_Darwin,
      drvBuilder = "/nix/store/hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh-bash/bin/bash",
      drvArgs = ["-e", "build.sh"],
      drvEnv =
        Map.fromList
          [ ("buildInputs", "/nix/store/cccccccccccccccccccccccccccccccc-dep1"),
            ("name", "pkg-2.0"),
            ("out", "/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-pkg-2.0"),
            ("system", "aarch64-darwin")
          ]
    }

testFromATerm :: IO [Bool]
testFromATerm = do
  putStrLn "derivation/fromATerm"
  sequence
    [ -- Round-trip: simple derivation
      runTest "fromATerm round-trip simple" $
        assertEqual "simple round-trip" (Right simpleTestDrv) (fromATerm (toATerm simpleTestDrv)),
      -- Round-trip: complex derivation
      runTest "fromATerm round-trip complex" $
        assertEqual "complex round-trip" (Right complexTestDrv) (fromATerm (toATerm complexTestDrv)),
      -- A .drv read from disk is input: an output name that violates the
      -- store-name rules (here a traversal shape that would later join
      -- the build dir) must refuse at parse.
      runTest "fromATerm rejects a traversal-shaped output name" $
        let evil =
              simpleTestDrv
                { drvOutputs =
                    [DerivationOutput "../evil" (StorePath (T.replicate 32 "a") "pkg") "" ""]
                }
         in case fromATerm (toATerm evil) of
              Left _ -> Pass
              Right _ -> Fail "parsed a drv with a traversal output name",
      -- drv4: the modulo-substitution section merges input-drv entries that
      -- share a modulo-hash key, unioning their output-name sets, so it is
      -- byte-identical to the already-merged form.  (Just subs replaces the
      -- whole input-drv section, so simpleTestDrv's own inputs are irrelevant.)
      runTest "inputDrvsSubst merges equal modulo keys" $
        assertEqual
          "subst-merge"
          (toATermForHash False (Just [("00000000", ["dev", "out"])]) simpleTestDrv)
          (toATermForHash False (Just [("00000000", ["out"]), ("00000000", ["dev"])]) simpleTestDrv),
      runTest "inputDrvsSubst unions and dedups merged out-names" $
        assertEqual
          "subst-union"
          (toATermForHash False (Just [("aabbccdd", ["dev", "out"])]) simpleTestDrv)
          (toATermForHash False (Just [("aabbccdd", ["out"]), ("aabbccdd", ["out", "dev"])]) simpleTestDrv),
      -- Distinct keys are untouched: still one entry each, ascending by key.
      runTest "inputDrvsSubst preserves distinct keys in order" $
        assertEqual
          "subst-distinct"
          (toATermForHash False (Just [("00000000", ["out"]), ("11111111", ["out"])]) simpleTestDrv)
          (toATermForHash False (Just [("11111111", ["out"]), ("00000000", ["out"])]) simpleTestDrv),
      -- Round-trip: empty derivation (no outputs, no inputs, no args, no env)
      runTest "fromATerm round-trip empty" $
        let emptyDrv =
              Derivation
                { drvOutputs = [],
                  drvInputDrvs = Map.empty,
                  drvInputSrcs = [],
                  drvPlatform = X86_64_Linux,
                  drvBuilder = "/bin/true",
                  drvArgs = [],
                  drvEnv = Map.empty
                }
         in assertEqual "empty round-trip" (Right emptyDrv) (fromATerm (toATerm emptyDrv)),
      -- Reject empty string
      runTest "fromATerm rejects empty" $
        assertLeft "empty" (fromATerm ""),
      -- Reject malformed
      runTest "fromATerm rejects malformed" $
        assertLeft "malformed" (fromATerm "NotADerivation"),
      -- Reject truncated
      runTest "fromATerm rejects truncated" $
        assertLeft "truncated" (fromATerm "Derive(["),
      -- Escaping round-trip: strings with special chars
      runTest "fromATerm escaping round-trip" $
        let escapeDrv =
              Derivation
                { drvOutputs =
                    [ DerivationOutput
                        { doName = "out",
                          doPath = StorePath "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "esc-test",
                          doHashAlgo = "",
                          doHash = ""
                        }
                    ],
                  drvInputDrvs = Map.empty,
                  drvInputSrcs = [],
                  drvPlatform = X86_64_Linux,
                  drvBuilder = "/bin/bash",
                  drvArgs = ["-c", "echo \"hello\nworld\""],
                  drvEnv = Map.fromList [("msg", "line1\nline2\ttab\\slash")]
                }
         in assertEqual "escape round-trip" (Right escapeDrv) (fromATerm (toATerm escapeDrv)),
      -- A non-standard escape keeps the byte and drops the backslash,
      -- matching upstream's .drv string parser.
      runTest "fromATerm drops the backslash on a non-standard escape" $
        let aterm = "Derive([(\"out\",\"/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-x\",\"\",\"\")],[],[],\"x86_64-linux\",\"/bin/sh\",[],[(\"k\",\"a\\xb\")])"
         in assertRight "nonstandard escape" (fromATerm aterm) $ \drv ->
              assertEqual "escape dropped" (Just "axb") (Map.lookup "k" (drvEnv drv)),
      -- writeDrv writes correct ATerm
      runTestM "writeDrv writes correct ATerm" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-writeDrv"
        forceRemoveIfExists tmpStore
        store <- openStore (StoreDir tmpStore)
        let sp = StorePath "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "test.drv"
            destFile = storePathToFilePath (stDir store) sp
        writeDrv store simpleTestDrv sp
        contents <- BS.readFile destFile
        closeStore store
        forceRemoveIfExists tmpStore
        pure (assertEqual "writeDrv content" (toATerm simpleTestDrv) contents),
      -- The .drv closure registers as store objects: a row for every
      -- recipe plus edges to its input sources and input .drvs, so an
      -- output reference scan that names an input .drv resolves instead
      -- of failing the registration batch.
      runTestM "writeDrvClosure registers recipes with their references" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-drv-closure"
            sd = StoreDir tmpStore
        forceRemoveIfExists tmpStore
        store <- openStore sd
        let srcSp = StorePath "gggggggggggggggggggggggggggggggg" "source.tar.gz"
            drvASp = StorePath "cccccccccccccccccccccccccccccccc" "dep1.drv"
            drvBSp = StorePath "dddddddddddddddddddddddddddddddd" "top.drv"
            drvB =
              simpleTestDrv
                { drvInputDrvs = Map.fromList [(drvASp, ["out"])],
                  drvInputSrcs = [srcSp]
                }
        -- The source row registers first, as in the build driver, where
        -- materializeEvalSources precedes the closure write.
        registerPath (stDB store) (PathRegistration srcSp "sha256:0" 0 Nothing [])
        writeDrvClosure
          store
          ( Map.fromList
              [ (storePathToText defaultStoreDir drvASp, toATerm simpleTestDrv),
                (storePathToText defaultStoreDir drvBSp, toATerm drvB)
              ]
          )
        validA <- isValid store drvASp
        validB <- isValid store drvBSp
        refsB <- queryReferences (stDB store) drvBSp
        closeStore store
        forceRemoveIfExists tmpStore
        let expectedRefs =
              sort
                [ T.pack (storePathToFilePath sd drvASp),
                  T.pack (storePathToFilePath sd srcSp)
                ]
        pure $
          if not validA
            then Fail "input .drv not registered"
            else
              if not validB
                then Fail "root .drv not registered"
                else assertEqual "drv references" expectedRefs (sort refsB),
      -- The recorded derivation populates drvOutputs.  These three read the
      -- Derivation the session records, which only the IO evaluator keeps.
      runTestM "derivationStrict populates drvOutputs" $ do
        tmpBase <- getTemporaryDirectory
        result <- evalDerivationIO tmpBase "derivation { name = \"test\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; }"
        pure $ assertRight "drvOutputs" result $ \drv ->
          case drvOutputs drv of
            [] -> Fail "drvOutputs is empty"
            (firstOut : _) ->
              if doName firstOut == "out"
                then Pass
                else Fail ("first output name: " <> doName firstOut),
      -- The .drv lists outputs sorted by name, as upstream writes them; the
      -- declared order lives in the value's outputs and all.
      runTestM "derivationStrict multi-output" $ do
        tmpBase <- getTemporaryDirectory
        result <- evalDerivationIO tmpBase "derivation { name = \"multi\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; outputs = [\"out\" \"dev\"]; }"
        pure $ assertRight "multi-output" result $ \drv ->
          let names = map doName (drvOutputs drv)
           in if names == ["dev", "out"]
                then Pass
                else Fail ("output names: " <> T.pack (show names)),
      -- The .drv env carries the output paths ($out, ...) and the build
      -- attributes, and no "drvPath" key - matching C++ Nix, which never
      -- writes one.
      runTestM "derivationStrict populates drvEnv" $ do
        tmpBase <- getTemporaryDirectory
        result <- evalDerivationIO tmpBase "derivation { name = \"test\"; system = \"x86_64-linux\"; builder = \"/bin/sh\"; }"
        pure $ assertRight "drvEnv" result $ \drv ->
          case (Map.lookup "out" (drvEnv drv), Map.lookup "name" (drvEnv drv)) of
            (Just op, Just "test")
              | "/nix/store/" `BS.isPrefixOf` op -> Pass
            _ -> Fail ("drvEnv keys: " <> T.pack (show (Map.toList (drvEnv drv))))
    ]

-- ---------------------------------------------------------------------------
-- Tests: input-addressed derivations
-- ---------------------------------------------------------------------------

-- | 'inputAddressed' over derivations whose paths upstream computed, as the
-- evaluator tests pin them: @a@ and the @b@ chains are nix-instantiate
-- 2.24.9's ('selfHoldingPrintedStrict', 'testDeepContextClosureIO'), @sa@
-- is 2.33.2's ('testStructuredAttrs').  Then its laws over generated
-- derivations, and what derivationStrict records held to them.
testInputAddressed :: IO [Bool]
testInputAddressed = do
  putStrLn "derivation/input-addressed"
  storeDir <- evalNixIOStoreDir
  sequence
    [ runTest "a derivation gets upstream's drvPath and output path"
        $ assertRight "a" aFile
        $ assertEqual "a" ("/nix/store/l9ldjc53ry43x24hvq73jx7dx0489mhj-a.drv", [("out", "/nix/store/0pd4llb3k36fnppfxwp6nlwsxwlv0y2c-a")]) . drvFilePathTexts,
      runTest "every output gets upstream's path, and the variable of its name holds it" $
        assertRight "sa" (inputAddressed "sa" [] structured) $ \file ->
          assertEqual "sa-outputs" saOutputs (snd (drvFilePathTexts file))
            `andThen` assertEqual "sa-env" (Map.fromList (("__json", saJson) : [(output, TE.encodeUtf8 path) | (output, path) <- saOutputs])) (drvEnv (drvFileDerivation file)),
      runTest "an input derivation's modulo hash stands in for its path" $
        assertRight "a" aFile $ \a ->
          assertRight "b" (inputAddressed "b" [(moduloHex [] a, ["out"])] (overDrvPath "b" a [a])) $
            assertEqual "b" "/nix/store/r3pz4ykyxg5s9f08fafcksw6bxfds40r-b.drv" . fst . drvFilePathTexts,
      runTest "each input of a chain stands in by its own modulo hash" $
        assertRight "a" aFile $ \a ->
          let aHash = (moduloHex [] a, ["out"])
              m = (plainDerivation "m" [("dep", outPathBytes a)]) {drvInputDrvs = Map.singleton (drvFilePath a) ["out"]}
           in assertRight "m" (inputAddressed "m" [aHash] m) $ \mFile ->
                assertRight "b" (inputAddressed "b" [aHash, (moduloHex [aHash] mFile, ["out"])] (overDrvPath "b" mFile [a, mFile])) $
                  assertEqual "b" "/nix/store/00pfjcd57v5dd5j4w4zmsp3s8lcpv6wg-b.drv" . fst . drvFilePathTexts,
      runTestM "only the outputs and their variables change, each variable holding its output's path" $
        quickCheckPasses $
          QC.forAll genFillCase $ \(FillCase name inputHashes drv) ->
            case inputAddressed name inputHashes drv of
              Left err -> QC.counterexample (show err) False
              Right file ->
                let filled = drvFileDerivation file
                    withoutOutputs d = d {drvOutputs = [], drvEnv = foldr (Map.delete . doName) (drvEnv d) (drvOutputs drv)}
                 in ( withoutOutputs filled,
                      map doName (drvOutputs filled),
                      [(Map.lookup (doName output) (drvEnv filled), doHashAlgo output, doHash output) | output <- drvOutputs filled],
                      drvFileATerm file,
                      spName (drvFilePath file)
                    )
                      QC.=== ( withoutOutputs drv,
                               sort (map doName (drvOutputs drv)),
                               [(Just (TE.encodeUtf8 (storePathToText defaultStoreDir (doPath output))), "", "") | output <- drvOutputs filled],
                               toATerm filled,
                               name <> ".drv"
                             ),
      runTestM "the file does not depend on what the outputs and their variables held" $
        quickCheckPasses $
          QC.forAll genFillCase $ \(FillCase name inputHashes drv) ->
            QC.forAll (genHolding drv) $ \holding ->
              let file = inputAddressed name inputHashes drv
               in (inputAddressed name inputHashes holding, inputAddressed name inputHashes . drvFileDerivation =<< file) QC.=== (file, file),
      runTestM "what derivationStrict records is filled in already, at the paths it returns" $
        quickCheckPasses (QC.forAll genStrictCase (QC.ioProperty . strictCaseAgrees storeDir))
    ]
  where
    quickCheckPasses property = do
      result <- QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} property
      pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))
    aFile = inputAddressed "a" [] (plainDerivation "a" [])
    -- The modulo hash a dependent substitutes for a filled-in derivation:
    -- upstream's hashDerivationModulo, unmasked, over its own inputs' hashes.
    moduloHex inputHashes file = Hash.bytesToHexText (sha256Digest (toATermForHash False (Just inputHashes) (drvFileDerivation file)))
    -- @x = "${dep.drvPath}";@: the .drv closure in inputSrcs, and each .drv
    -- in it among the inputs with every output, here @out@ alone.
    overDrvPath name dep closure =
      (plainDerivation name [("x", TE.encodeUtf8 (fst (drvFilePathTexts dep)))])
        { drvInputDrvs = Map.fromList [(drvFilePath file, ["out"]) | file <- closure],
          drvInputSrcs = map drvFilePath closure
        }
    structured =
      Derivation
        { drvOutputs = map pendingOutput ["out", "dev", "lib"],
          drvInputDrvs = Map.empty,
          drvInputSrcs = [],
          drvPlatform = X86_64_Linux,
          drvBuilder = "/bin/sh",
          drvArgs = [],
          drvEnv = Map.singleton "__json" saJson
        }
    saJson = "{\"builder\":\"/bin/sh\",\"name\":\"sa\",\"outputs\":[\"out\",\"dev\",\"lib\"],\"system\":\"x86_64-linux\"}"
    saOutputs =
      [ ("dev", "/nix/store/jfnikb9k7953srg5mlp3mccmqaijvxyy-sa-dev"),
        ("lib", "/nix/store/zpjd7d2l5jqzynv3377a36nk6rzb5il4-sa-lib"),
        ("out", "/nix/store/bs6ppbphi38qlbjfz67pfk86sx7l4vks-sa")
      ]

-- | A file's drvPath, and each output's name and path, as text.
drvFilePathTexts :: DrvFile -> (Text, [(Text, Text)])
drvFilePathTexts file =
  ( storePathToText defaultStoreDir (drvFilePath file),
    [(doName output, storePathToText defaultStoreDir (doPath output)) | output <- drvOutputs (drvFileDerivation file)]
  )

-- | The path of a file's @out@, as an environment value holds it.
outPathBytes :: DrvFile -> BS.ByteString
outPathBytes file = TE.encodeUtf8 (fromMaybe "" (lookup "out" (snd (drvFilePathTexts file))))

-- | An output carrying only its name, as derivationStrict hands one to
-- 'inputAddressed'.
pendingOutput :: Text -> DerivationOutput
pendingOutput name = DerivationOutput name (StorePath "" "") "" ""

-- | @derivation { name = <name>; system = "x"; builder = "/bin/sh"; }@
-- with more variables, as derivationStrict hands it to 'inputAddressed':
-- no inputs, and its @out@ pending.
plainDerivation :: Text -> [(Text, BS.ByteString)] -> Derivation
plainDerivation name variables =
  Derivation
    { drvOutputs = [pendingOutput "out"],
      drvInputDrvs = Map.empty,
      drvInputSrcs = [],
      drvPlatform = OtherPlatform "x",
      drvBuilder = "/bin/sh",
      drvArgs = [],
      drvEnv = Map.fromList ([("builder", "/bin/sh"), ("name", TE.encodeUtf8 name), ("system", "x")] <> variables)
    }

-- | A derivation for 'inputAddressed''s laws with the name and input hashes
-- it is filled in with: outputs and their variables holding anything,
-- inputs the hashes need not match, and any bytes in its fields.
data FillCase = FillCase !Text ![(Text, [Text])] !Derivation
  deriving (Show)

genFillCase :: QC.Gen FillCase
genFillCase = do
  name <- genDrvName
  outputs <- genOutputNames >>= traverse genHeldOutput
  inputDrvs <- Map.fromList <$> QC.resize 3 (QC.listOf ((,) <$> genFixturePath ".drv" <*> genOutputNames))
  inputSrcs <- QC.resize 3 (QC.listOf (genFixturePath ""))
  platform <- QC.elements [X86_64_Linux, Aarch64_Darwin, OtherPlatform "x"]
  builder <- genFieldBytes
  args <- QC.resize 3 (QC.listOf genFieldBytes)
  env <- Map.fromList <$> QC.resize 6 (QC.listOf ((,) <$> QC.elements (["name", "system", "builder", "src", "__json"] <> outputPool) <*> genFieldBytes))
  inputHashes <- QC.resize 3 (QC.listOf ((,) <$> genTextOf 64 "0123456789abcdef" <*> genOutputNames))
  pure
    ( FillCase
        name
        inputHashes
        Derivation
          { drvOutputs = outputs,
            drvInputDrvs = inputDrvs,
            drvInputSrcs = inputSrcs,
            drvPlatform = platform,
            drvBuilder = builder,
            drvArgs = args,
            drvEnv = env
          }
    )

-- | A derivation with each output, and the variable of its name, holding
-- something else.
genHolding :: Derivation -> QC.Gen Derivation
genHolding drv = do
  outputs <- traverse (genHeldOutput . doName) (drvOutputs drv)
  held <- traverse (const genFieldBytes) outputs
  pure drv {drvOutputs = outputs, drvEnv = foldr (uncurry Map.insert) (drvEnv drv) (zip (map doName outputs) held)}

-- | An output holding a path and hash fields of any kind.
genHeldOutput :: Text -> QC.Gen DerivationOutput
genHeldOutput name =
  DerivationOutput name
    <$> genFixturePath ""
    <*> QC.elements ["", "sha256", "r:sha256"]
    <*> QC.elements ["", "0123456789abcdef", "impure"]

-- | Output names, at least one and none twice, in any order.
genOutputNames :: QC.Gen [Text]
genOutputNames = QC.sublistOf outputPool `QC.suchThat` (not . null) >>= QC.shuffle

-- | The output names the generators draw on.
outputPool :: [Text]
outputPool = ["out", "dev", "lib", "bin", "doc", "man"]

-- | Bytes for a builder, an argument or a variable: any bytes, the ATerm
-- specials among them.
genFieldBytes :: QC.Gen BS.ByteString
genFieldBytes = BS.pack <$> QC.resize 12 (QC.listOf QC.arbitrary)

-- | A derivationStrict call as Nix source, with the name it gives: its
-- outputs in any order, structured or not, some attributes, and perhaps
-- one that depends on another derivation, through an output, through its
-- drvPath, or through a fixed output.
data StrictCase = StrictCase !Text !Text
  deriving (Show)

genStrictCase :: QC.Gen StrictCase
genStrictCase = do
  name <- genDrvName
  system <- QC.elements ["x", "x86_64-linux"]
  builder <- QC.elements ["/bin/sh", "b"]
  outputs <- genOutputNames
  structured <- QC.elements ["true", "false"]
  attributes <- QC.sublistOf ["s = \"line\\n\\\"quoted\\\" \\\\ \\${x}\";", "i = 42;", "l = [ \"a\" \"b c\" ];", "n = null;", "t = true;"]
  dependency <- QC.elements ["", "dep = d;", "dep = d.dev;", "dep = \"${d.drvPath}\";", "dep = f;"]
  pure . StrictCase name . T.unwords $
    [ "let d = derivation { name = \"d\"; system = \"x\"; builder = \"b\"; outputs = [ \"out\" \"dev\" ]; };",
      "f = derivation { name = \"f\"; system = \"x\"; builder = \"b\"; outputHash = \"sha256-47DEQpj8HBSKTXA6xJbgU1CNMmz7W08F06XOTYP1hmM=\"; };",
      "r = derivationStrict { name = \"" <> name <> "\"; system = \"" <> system <> "\"; builder = \"" <> builder <> "\";",
      "outputs = [ " <> T.unwords ["\"" <> output <> "\"" | output <- outputs] <> " ]; __structuredAttrs = " <> structured <> ";"
    ]
      <> attributes
      <> [ dependency,
           "}; in builtins.concatStringsSep \"\\n\" (map (n: n + \"=\" + builtins.unsafeDiscardStringContext r.${n}) (builtins.attrNames r))"
         ]

-- | Whether what derivationStrict records for a case is filled in already:
-- 'inputAddressed', given the case's name and the modulo hashes the session
-- cached for the recorded inputs, gives back the recorded derivation, at
-- the drvPath and output paths derivationStrict returned.
strictCaseAgrees :: StoreDir -> StrictCase -> IO QC.Property
strictCaseAgrees storeDir (StrictCase name source) = case parseNix testBaseDir testOrigin (TE.encodeUtf8 source) of
  Left err -> pure (QC.counterexample (show err) False)
  Right parsed -> do
    st <- testEvalState storeDir testBaseDir
    outcome <- runEvalIO st $ do
      returned <-
        evalParsed (pure (evalBaseEnv st)) parsed >>= \case
          VStr bytes _ -> pure [T.drop 1 <$> T.breakOn "=" line | line <- T.lines (bytesText bytes)]
          other -> throwEvalError ("expected a string, got " <> typeName other)
      recorded <- maybe (pure Nothing) lookupSessionDrv (lookup "drvPath" returned)
      drv <- maybe (throwEvalError "no .drv was recorded") pure recorded
      inputHashes <- traverse cachedHash (Map.toList (drvInputDrvs drv))
      pure (returned, drv, inputHashes)
    pure $ case outcome of
      Left err -> QC.counterexample (T.unpack err) False
      Right (returned, drv, inputHashes) -> case inputAddressed name inputHashes drv of
        Left err -> QC.counterexample (show err) False
        Right file ->
          let (drvPath, outputs) = drvFilePathTexts file
           in (drvFileDerivation file, sort returned) QC.=== (drv, sort (("drvPath", drvPath) : outputs))
  where
    cachedHash (input, outputs) = do
      let inputText = storePathToText defaultStoreDir input
      lookupDrvHash inputText >>= maybe (throwEvalError ("no modulo hash was cached for " <> inputText)) (\hash -> pure (hash, outputs))

-- ---------------------------------------------------------------------------
-- Tests: develop's capture
-- ---------------------------------------------------------------------------

-- | The derivation develop builds to capture a target's environment
-- ("Nix.Develop.Capture"), over fixtures and over generated derivations.
-- The script is data/develop/get-env.sh, and its store path is the one
-- upstream 2.34.9 and 2.35.4 add the same bytes at.
testDevelopCapture :: IO [Bool]
testDevelopCapture = do
  putStrLn "develop/capture"
  scriptPath <- getEnvScriptPath <$> BS.readFile ("data" </> "develop" </> "get-env.sh")
  let capture name target check = assertRight "script" scriptPath $ \script -> assertRight "capture" (captureDerivation script name [] target) (check script)
      taken target = capture "hello" target (\_ _ -> Pass)
      refused target failure = assertRight "script" scriptPath $ \script -> assertEqual "refusal" (Left failure) (drvFilePath <$> captureDerivation script "hello" [] target)
  sequence
    [ runTest "get-env.sh is at the store path upstream adds it at"
        $ assertRight "script" scriptPath
        $ assertEqual "path" "/nix/store/hyhh9rhzxhzmfspllmpf75xzkzns109r-get-env.sh" . storePathToText defaultStoreDir,
      runTest "the script is the builder's one argument and an input source, and the capture is named NAME-env" $
        capture "hello" plain $ \script file ->
          let result = drvFileDerivation file
           in assertEqual "args" [pathBytes script] (drvArgs result)
                `andThen` assertEqual "inputSrcs" [source, script] (drvInputSrcs result)
                `andThen` assertEqual "name" (Just "hello-env") (Map.lookup "name" (drvEnv result))
                `andThen` assertEqual "file" "hello-env.drv" (spName (drvFilePath file)),
      runTest "a script already among the input sources is there once" $
        assertRight "script" scriptPath $ \script ->
          let target = plain {drvInputSrcs = [script, source]}
           in assertRight "capture" (captureDerivation script "hello" [] target) $
                assertEqual "inputSrcs" [script, source] . drvInputSrcs . drvFileDerivation,
      runTest "the four checks on references are removed and every other variable kept" $
        capture "hello" plain $ \_ file ->
          let result = drvFileDerivation file
           in assertEqual "env" (Map.fromList [("builder", "/bin/sh"), ("kept", "k"), ("name", "hello-env"), ("out", outputBytes result "out"), ("system", "x")]) (drvEnv result),
      runTest "under structured attributes outputChecks is removed, __json written as nlohmann's dump() writes it, and name is a variable" $
        capture "s" structured $ \_ file ->
          let result = drvFileDerivation file
           in assertEqual
                "env"
                (Map.fromList [("__json", "{\"a\":[1,2.5,\"x\"],\"name\":\"s\",\"z\":1}"), ("allowedReferences", "kept"), ("name", "s-env"), ("out", outputBytes result "out")])
                (drvEnv result),
      runTest "a fixed-output target's capture is input-addressed, its hash variables kept" $
        capture "f" fixed $ \_ file ->
          let result = drvFileDerivation file
           in assertEqual "outputs" [("out", "", "")] [(doName output, doHashAlgo output, doHash output) | output <- drvOutputs result]
                `andThen` assertEqual "outputHash" (Just (TE.encodeUtf8 fixedHash)) (Map.lookup "outputHash" (drvEnv result))
                `andThen` assertEqual "out" (outputBytes result "out") (fromMaybe "" (Map.lookup "out" (drvEnv result))),
      runTest "a builder whose base name is bash is taken, trailing slashes dropped" $
        foldr (andThen . (\builder -> taken plain {drvBuilder = builder})) Pass ["bash", "/bin/bash", "/nix/store/x-bash/bin/bash/", "/bin/bash//"],
      runTest "a builder of another base name is refused in upstream's words" $
        foldr (andThen . (\builder -> refused plain {drvBuilder = builder} NotBashBuilder)) Pass ["/bin/sh", "/bin/bash5", "", "/", "///", "bash.exe", "/bin/bash/x"]
          `andThen` assertEqual "message" "'nix develop' only works on derivations that use 'bash' as their builder" (captureFailureMessage NotBashBuilder),
      runTest "a floating content-addressed, impure or mixed output is refused" $
        refused plain {drvOutputs = [DerivationOutput "out" (StorePath "" "") "r:sha256" ""]} UncapturableOutputs
          `andThen` refused plain {drvOutputs = [DerivationOutput "out" (StorePath "" "") "r:sha256" "impure"]} UncapturableOutputs
          `andThen` refused fixed {drvOutputs = drvOutputs fixed <> [pendingOutput "dev"]} UncapturableOutputs,
      runTest "structured attributes that are not an object are refused" $
        foldr
          (andThen . (\json -> assertRight "script" scriptPath $ \script -> assertLeft json (drvFilePath <$> captureDerivation script "s" [] structured {drvEnv = Map.insert "__json" (TE.encodeUtf8 json) (drvEnv structured)})))
          Pass
          ["null", "[1]", "{", "\"x\""],
      runTestM "the capture keeps the builder, input derivations, platform and every variable but the checks, name and the outputs" $
        either (pure . Fail . T.pack . show) (quickCheckPasses . QC.forAll genCaptureCase . capturedKeeps) scriptPath,
      runTest "the environment is read from the first output in name order that is a regular file of at least one byte" $
        assertEqual "out" (Just "out") (environmentOutput (Map.fromList [("dev", RegularFile 0), ("lib", NotRegularFile), ("out", RegularFile 10)]))
          `andThen` assertEqual "a" (Just "a") (environmentOutput (Map.fromList [("a", RegularFile 5), ("b", RegularFile 9)])),
      runTest "no output holding such a file is upstream's missing environment" $
        assertEqual "none" Nothing (environmentOutput (Map.fromList [("dev", RegularFile 0), ("out", NotRegularFile)]))
          `andThen` assertEqual "message" "get-env.sh failed to produce an environment" noEnvironmentMessage
    ]
  where
    quickCheckPasses property = do
      result <- QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} property
      pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))
    bash = "/nix/store/00000000000000000000000000000000-bash/bin/bash"
    source = StorePath "11111111111111111111111111111111" "builder.sh"
    pathBytes = TE.encodeUtf8 . storePathToText defaultStoreDir
    outputBytes drv name = maybe "" (pathBytes . doPath) (find ((== name) . doName) (drvOutputs drv))
    plain =
      (plainDerivation "hello" [("allowedReferences", "a"), ("allowedRequisites", "b"), ("disallowedReferences", "c"), ("disallowedRequisites", "d"), ("kept", "k")])
        { drvBuilder = bash,
          drvInputSrcs = [source]
        }
    -- Upstream reads a structured derivation's name from __json; the
    -- variable it adds is the capture's alone.
    structured =
      (plainDerivation "s" [])
        { drvBuilder = bash,
          drvEnv =
            Map.fromList
              [ ("__json", "{ \"z\": 1, \"outputChecks\": { \"out\": { \"allowedReferences\": [] } }, \"a\": [1, 2.5, \"x\"], \"name\": \"s\" }"),
                ("allowedReferences", "kept"),
                ("out", "")
              ]
        }
    fixedHash = "0000000000000000000000000000000000000000000000000000000000000000" :: Text
    fixed =
      (plainDerivation "f" [("outputHash", TE.encodeUtf8 fixedHash), ("outputHashAlgo", "sha256"), ("outputHashMode", "recursive")])
        { drvBuilder = bash,
          drvOutputs = [DerivationOutput "out" (StorePath "22222222222222222222222222222222" "f") "r:sha256" fixedHash]
        }

-- | A derivation the capture takes, from 'genFillCase''s: a builder that
-- is bash, its outputs input-addressed, no structured attributes, and
-- some of the checks on references set.
genCaptureCase :: QC.Gen FillCase
genCaptureCase = do
  FillCase name inputHashes drv <- genFillCase
  directory <- genFieldBytes
  checks <- QC.sublistOf ["allowedReferences", "allowedRequisites", "disallowedReferences", "disallowedRequisites"]
  values <- traverse (const genFieldBytes) checks
  pure
    ( FillCase
        name
        inputHashes
        drv
          { drvBuilder = BS8.filter (/= '/') directory <> "/bash",
            drvOutputs = [output {doHashAlgo = "", doHash = ""} | output <- drvOutputs drv],
            drvEnv = Map.union (Map.fromList (zip checks values)) (Map.delete "__json" (drvEnv drv))
          }
    )

-- | What a capture of a generated derivation keeps: everything but the
-- checks on references, @name@ and the outputs and their variables, with
-- the script its one argument and among its input sources once.
capturedKeeps :: StorePath -> FillCase -> QC.Property
capturedKeeps script (FillCase name inputHashes drv) =
  case captureDerivation script name inputHashes drv of
    Left failure -> QC.counterexample (show failure) False
    Right file ->
      let result = drvFileDerivation file
          outputNames = map doName (drvOutputs drv)
          rest d = foldr Map.delete (drvEnv d) ("name" : outputNames <> ["allowedReferences", "allowedRequisites", "disallowedReferences", "disallowedRequisites"])
       in ( drvBuilder result,
            drvInputDrvs result,
            drvPlatform result,
            sort (map doName (drvOutputs result)),
            rest result,
            drvArgs result,
            Set.fromList (drvInputSrcs result),
            length (filter (== script) (drvInputSrcs result)),
            Map.lookup "name" (drvEnv result)
          )
            QC.=== ( drvBuilder drv,
                     drvInputDrvs drv,
                     drvPlatform drv,
                     sort outputNames,
                     rest drv,
                     [TE.encodeUtf8 (storePathToText defaultStoreDir script)],
                     Set.insert script (Set.fromList (drvInputSrcs drv)),
                     1,
                     Just (TE.encodeUtf8 (name <> "-env"))
                   )

-- ---------------------------------------------------------------------------
-- Tests: develop's captured environment and rcfile
-- ---------------------------------------------------------------------------

-- | The captured environment and the scripts made from it.  Three of the
-- fixtures in test/data/develop are what upstream's get-env.sh (2.35.4)
-- writes when bash 5.3 runs it over a stand-in stdenv setup: one output,
-- three, and structured attributes with two.  Two more are small
-- documents for the rewriting rules.  Each golden is what upstream's own
-- code makes of a fixture: develop.cc's BuildEnvironment, makeRcScript and
-- the rcfile lines of CmdDevelop::run, with util.cc's escapeShellArgAlways
-- and rewriteStrings, built against nlohmann_json 3.11.3 with the store
-- taken out.
testDevelopEnvironment :: IO [Bool]
testDevelopEnvironment = do
  putStrLn "develop/environment"
  tmpBase <- getTemporaryDirectory
  sequence $
    map dumpGolden developFixtures
      <> [rcfileGolden fixture mode | fixture <- developFixtures, mode <- developModes]
      <> map (rcfileGolden "one-output") promptModes
      <> [ scriptGolden "rewrites" (ScriptDirectories "/w" "/t") [],
           scriptGolden "paths" (ScriptDirectories "/" "/t/") [ScriptFile "/t/.attrs.sh" "declare x=1", ScriptFile "/t/.attrs.json" "{\"x\":1}"],
           runTestM "the structured attributes' files are written to the temporary directory" $
             withDevelopFixture "structured" $ \env ->
               pure . assertRight "script" (environmentScript goldenDirectories env) $ \script ->
                 assertEqual
                   "files"
                   [ScriptFile (goldenTemporary <> "/.attrs.sh") (attrsFileBytes attrsSh env), ScriptFile (goldenTemporary <> "/.attrs.json") (attrsFileBytes attrsJson env)]
                   (scriptFiles script),
           runTestM "a script without structured attributes names no files" $
             withDevelopFixture "several-outputs" $ \env ->
               pure (assertRight "script" (environmentScript goldenDirectories env) (assertEqual "files" [] . scriptFiles)),
           runTestM "a variable of a kind get-env.sh writes as unknown is dropped" $
             withDevelopFixture "one-output" $ \env ->
               pure (assertEqual "dropped" [Nothing, Nothing, Just "impure"] [kept <$> Map.lookup name (beVariables env) | name <- ["BASHOPTS", "NIX_FIXTURE_COUNT", "IN_NIX_SHELL"]]),
           runTest "a variable of any kind but the four is dropped" $
             assertEqual
               "dropped"
               (Right (BuildEnvironment Map.empty Map.empty Nothing))
               (parseBuildEnvironment "{\"bashFunctions\":{},\"variables\":{\"i\":{\"type\":\"integer\",\"value\":3},\"r\":{\"type\":\"unknown\"}}}"),
           runTest "a document of another shape is refused" $
             assertEqual "accepted" [] [document | document <- malformedEnvironments, isRight (parseBuildEnvironment document)],
           runTest "ignoreVars is upstream's" $
             assertEqual "ignoreVars" (Set.fromList upstreamIgnoreVars) ignoreVars,
           runTest "a variable in ignoreVars is left as the shell has it"
             $ assertRight "script" (developScript (("KEPT", ExportedString "k") : ("outputs", ShellString "") : [(name, ExportedString "x") | name <- upstreamIgnoreVars]))
             $ assertEqual "assigned" ["KEPT", "outputs"] . assignedNames,
           runTest "an outputs string splits on carriage returns too"
             $ assertRight "script" (developScript [("a", ShellString "/s/a"), ("b", ShellString "/s/b"), ("outputs", ShellString "a\rb"), ("v", ShellString "/s/a /s/b")])
             $ assertEqual "v" True . BS.isInfixOf "\nv='/w/outputs/a /w/outputs/b'\n" . scriptText,
           runTest "an outputs array names the outputs, the first of two with one path taking it"
             $ assertRight "script" (developScript [("a", ShellString "/s/same"), ("b", ShellString "/s/same"), ("outputs", IndexedArray ["b", "a"]), ("v", ShellString "/s/same")])
             $ assertEqual "v" True . BS.isInfixOf "\nv='/w/outputs/b'\n" . scriptText,
           runTest "an outputs associative array names the outputs by its keys"
             $ assertRight "script" (developScript [("a", ShellString "/s/a"), ("b", ShellString "/s/b"), ("outputs", AssociativeArray (Map.fromList [("a", "/s/x"), ("b", "/s/y")])), ("v", ShellString "/s/a /s/b /s/x")])
             $ assertEqual "v" True . BS.isInfixOf "\nv='/w/outputs/a /w/outputs/b /s/x'\n" . scriptText,
           runTest "an environment with no outputs is refused in upstream's words" $
             assertEqual "refused" (Left "derivation does not have an 'outputs' attribute") (scriptText <$> developScript [("out", ShellString "/s/o")]),
           runTest "an output's variable that is not a string is refused in upstream's words" $
             assertEqual "refused" (Left "bash variable is not a string") (scriptText <$> developScript [("outputs", ShellString "out"), ("out", IndexedArray ["/s/o"])]),
           runTest "structured attributes' outputs that are not an associative array are refused in upstream's words" $
             assertEqual "refused" (Left "bash variable is not an associative array") (scriptText <$> structuredScript [("outputs", ShellString "out"), ("out", ShellString "/s/o")]),
           runTest "an output with no variable is refused" $
             assertEqual "refused" (Left "the build environment has no variable 'out'") (scriptText <$> developScript [("outputs", ShellString "out")]),
           runTest "structured attributes with no file variable are refused" $
             assertEqual "refused" (Left "the build environment has no variable 'NIX_ATTRS_SH_FILE'") (scriptText <$> structuredScript [("outputs", AssociativeArray (Map.singleton "out" "/s/o"))]),
           runTest "a file variable that is not a string is refused in upstream's words" $
             assertEqual "refused" (Left "bash variable is not a string") (scriptText <$> structuredScript [("NIX_ATTRS_SH_FILE", IndexedArray []), ("outputs", AssociativeArray (Map.singleton "out" "/s/o"))]),
           runTest "a replacement holding what it replaces is refused where upstream's replacing never ends" $
             assertEqual "accepted" [] [path | path <- ["/w", "", "/w/outputs"], isRight (developScript [("outputs", ShellString "out"), ("out", ShellString path)])],
           runTest "a replacement holding what it replaces is harmless where the script does not hold it" $
             assertEqual "refused" [] [path | path <- ["/w", "/w/", "/w/outputs"], isLeft (developScript [("outputs", ShellString "HOME"), ("HOME", ShellString path)])],
           runTest "a path that is its own replacement is left as it is"
             $ assertRight "script" (developScript [("outputs", ShellString "out"), ("out", ShellString "/w/outputs/out")])
             $ assertEqual "out" True . BS.isInfixOf "\nout='/w/outputs/out'\n" . scriptText,
           runTestM "reading what is written gives the environment back" $
             quickCheckPasses (QC.forAll genBuildEnvironment (\env -> parseBuildEnvironment (renderJson (buildEnvironmentJson env)) QC.=== Right env)),
           shellWordLaw (tmpBase </> "nova-nix-test-shell-word")
         ]
  where
    dumpGolden fixture =
      runTestM (fixture <> ": written back as upstream's toJSON writes it") $
        withDevelopFixture fixture $ \env -> do
          expected <- BS.readFile (developData (fixture <> ".dump.json"))
          pure (assertEqual "dump" expected (renderJson (buildEnvironmentJson env)))
    rcfileGolden fixture (modeName, mode) =
      runTestM (fixture <> ": the " <> modeName <> " rcfile is upstream's") $
        withDevelopFixture fixture $ \env -> do
          expected <- BS.readFile (developData (fixture <> "." <> modeName <> ".rcfile"))
          pure . assertRight "script" (environmentScript goldenDirectories env) $
            assertEqual "rcfile" expected . renderRcfile (Shell "/tmp/nix-shell.Xc9Tq2" mode "bash")
    scriptGolden fixture directories files =
      runTestM (fixture <> ": the script is upstream's") $
        withDevelopFixture fixture $ \env -> do
          expected <- BS.readFile (developData (fixture <> ".script"))
          pure . assertRight "script" (environmentScript directories env) $ \script ->
            assertEqual "script" expected (scriptText script) `andThen` assertEqual "files" files (scriptFiles script)
    goldenDirectories = ScriptDirectories "/home/user/project" goldenTemporary
    goldenTemporary = "/tmp/nix-develop-4242-0"
    developModes =
      [ ("interactive", Interactive (Prompt "" "" "")),
        ("prompts", Interactive goldenPrompt),
        ("command", RunCommand ("make" :| ["CFLAGS=-O2 -g", "it's"])),
        ("phase", RunPhase "build")
      ]
    promptModes =
      [ ("prompt", Interactive (Prompt (bashPrompt goldenPrompt) "" "")),
        ("prompt-prefix", Interactive (Prompt "" (bashPromptPrefix goldenPrompt) "")),
        ("prompt-suffix", Interactive (Prompt "" "" (bashPromptSuffix goldenPrompt)))
      ]
    goldenPrompt = Prompt "\\[\\e[1;32m\\]dev\\$ " "(nix) " " it's"
    attrsFileBytes field env = maybe "" (TE.encodeUtf8 . field) (beAttrsFiles env)
    kept variable = case variable of
      ExportedString s -> s
      ShellString s -> s
      _ -> ""
    developScript variables = environmentScript (ScriptDirectories "/w" "/t") (BuildEnvironment (Map.fromList variables) Map.empty Nothing)
    structuredScript variables = environmentScript (ScriptDirectories "/w" "/t") (BuildEnvironment (Map.fromList variables) Map.empty (Just (AttrsFiles "" "")))
    -- The names a script assigns a quoted string to.
    assignedNames script = [name | line <- BS8.lines (scriptText script), (name, rest) <- [BS8.break (== '=') line], "='" `BS.isPrefixOf` rest]
    quickCheckPasses property = do
      result <- QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} property
      pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))

-- | The fixtures get-env.sh wrote, each with goldens in every mode.
developFixtures :: [Text]
developFixtures = ["one-output", "several-outputs", "structured"]

-- | A file of test/data/develop.
developData :: Text -> FilePath
developData name = "test" </> "data" </> "develop" </> T.unpack name

-- | Run a check over a fixture's environment as 'parseBuildEnvironment'
-- reads it.
withDevelopFixture :: Text -> (BuildEnvironment -> IO TestResult) -> IO TestResult
withDevelopFixture fixture check = do
  document <- BS.readFile (developData (fixture <> ".json"))
  either (pure . Fail . ("fixture: " <>)) check (parseBuildEnvironment document)

-- | Upstream's ignoreVars (develop.cc:315-332 at 2.35.4).
upstreamIgnoreVars :: [Text]
upstreamIgnoreVars = ["BASHOPTS", "HOME", "NIX_BUILD_TOP", "NIX_ENFORCE_PURITY", "NIX_LOG_FD", "NIX_REMOTE", "PPID", "SHELLOPTS", "SSL_CERT_FILE", "TEMP", "TEMPDIR", "TERM", "TMP", "TMPDIR", "TZ", "UID"]

-- | Documents upstream's reader has no defined answer for, each refused.
malformedEnvironments :: [BS.ByteString]
malformedEnvironments =
  [ "{\"bashFunctions\":{},\"variables\":{\"v\":{\"type\":\"var\",\"value\":\"\xff\"}}}",
    "[]",
    "{\"bashFunctions\":{}}",
    "{\"variables\":{}}",
    "{\"bashFunctions\":[],\"variables\":{}}",
    "{\"bashFunctions\":{\"f\":1},\"variables\":{}}",
    "{\"bashFunctions\":{},\"variables\":{\"v\":\"x\"}}",
    "{\"bashFunctions\":{},\"variables\":{\"v\":{\"value\":\"x\"}}}",
    "{\"bashFunctions\":{},\"variables\":{\"v\":{\"type\":1,\"value\":\"x\"}}}",
    "{\"bashFunctions\":{},\"variables\":{\"v\":{\"type\":\"exported\"}}}",
    "{\"bashFunctions\":{},\"variables\":{\"v\":{\"type\":\"var\",\"value\":1}}}",
    "{\"bashFunctions\":{},\"variables\":{\"v\":{\"type\":\"array\",\"value\":[\"a\",1]}}}",
    "{\"bashFunctions\":{},\"variables\":{\"v\":{\"type\":\"array\",\"value\":{}}}}",
    "{\"bashFunctions\":{},\"variables\":{\"v\":{\"type\":\"associative\",\"value\":[]}}}",
    "{\"bashFunctions\":{},\"variables\":{\"v\":{\"type\":\"associative\",\"value\":{\"k\":null}}}}",
    "{\"bashFunctions\":{},\"variables\":{},\"structuredAttrs\":null}",
    "{\"bashFunctions\":{},\"variables\":{},\"structuredAttrs\":{\".attrs.sh\":\"\"}}",
    "{\"bashFunctions\":{},\"variables\":{},\"structuredAttrs\":{\".attrs.sh\":\"\",\".attrs.json\":{}}}"
  ]

-- | Environments of every shape: any text, control characters and quotes
-- among it, in names, values, function bodies and the files.
genBuildEnvironment :: QC.Gen BuildEnvironment
genBuildEnvironment =
  BuildEnvironment
    <$> genEntries genVariable
    <*> genEntries genAnyText
    <*> QC.oneof [pure Nothing, Just <$> (AttrsFiles <$> genAnyText <*> genAnyText)]
  where
    genEntries genValue = Map.fromList <$> QC.resize 6 (QC.listOf ((,) <$> genAnyText <*> genValue))
    genVariable =
      QC.oneof
        [ ShellString <$> genAnyText,
          ExportedString <$> genAnyText,
          IndexedArray <$> QC.resize 4 (QC.listOf genAnyText),
          AssociativeArray <$> genEntries genAnyText
        ]
    genAnyText = T.pack <$> QC.resize 12 (QC.listOf (QC.frequency [(4, QC.arbitrary), (1, QC.elements "'\"\\\n\t\0{}[]")]))

-- | A POSIX sh reads a word 'escapeShellArgAlways' quotes back as the same
-- bytes, whatever they are but NUL, which no shell word holds.
shellWordLaw :: FilePath -> IO Bool
shellWordLaw scriptPath
  | SI.os == "mingw32" = True <$ putStrLn "  SKIP  a POSIX sh reads an escapeShellArgAlways word back: POSIX only"
  | otherwise =
      runTestM "a POSIX sh reads an escapeShellArgAlways word back as the same bytes" $ do
        result <- QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} (QC.forAll genWord (QC.ioProperty . readBack))
        pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))
  where
    genWord = BS.pack <$> QC.resize 16 (QC.listOf (QC.frequency [(1, pure 0x27), (1, QC.elements [0x5c, 0x22, 0x24, 0x60, 0x0a]), (4, QC.choose (1, 255))]))
    readBack word = do
      BS.writeFile scriptPath ("printf '%s' " <> escapeShellArgAlways word)
      printed <-
        Proc.withCreateProcess (Proc.proc "/bin/sh" [scriptPath]) {Proc.std_out = Proc.CreatePipe, Proc.env = Just [("LC_ALL", "C")]} $ \_ out _ process -> do
          bytes <- maybe (pure BS.empty) BS.hGetContents out
          code <- Proc.waitForProcess process
          pure (code, bytes)
      pure (printed QC.=== (ExitSuccess, word))

-- ---------------------------------------------------------------------------
-- Tests: develop's shell and its command line
-- ---------------------------------------------------------------------------

-- | What @develop@'s shell runs and the environment it starts with, and
-- how the command line gives them, each refusal in upstream's words.
testDevelopShell :: IO [Bool]
testDevelopShell = do
  putStrLn "develop/shell"
  sequence
    [ runTest "a phase, a command, or else an interactive shell with the prompt" $
        assertEqual "phase" (Right (RunPhase "build")) (shellModeFor Nothing (Just "build") prompt)
          `andThen` assertEqual "command" (Right (RunCommand ("sh" :| ["-c", "true"]))) (shellModeFor (Just ("sh" :| ["-c", "true"])) Nothing prompt)
          `andThen` assertEqual "interactive" (Right (Interactive prompt)) (shellModeFor Nothing Nothing prompt),
      runTest "a command beside a phase is refused in upstream's words" $
        assertEqual "both" (Left "you cannot use both '--command' and '--phase'") (shellModeFor (Just ("true" :| [])) (Just "build") prompt),
      runTest "an interactive shell reads the rcfile through --rcfile, a command or a phase runs it as a script" $
        assertEqual "interactive" ["--rcfile", "/t/rc"] (shellArguments (Interactive prompt) "/t/rc")
          `andThen` assertEqual "command" ["/t/rc"] (shellArguments (RunCommand ("true" :| [])) "/t/rc")
          `andThen` assertEqual "phase" ["/t/rc"] (shellArguments (RunPhase "build") "/t/rc")
          `andThen` assertEqual "program" "bash" bashProgram,
      runTest "the caller's environment, set variables replacing its own and unset ones removed, then NIX_GCROOT and SHELL" $
        assertEqual
          "environment"
          (Right (Map.fromList [("A", "set"), ("C", "c"), ("NIX_GCROOT", gcroot), ("SHELL", "bash")]))
          (shellEnvironment (changes CallerEnvironment [] ["B", "NIX_GCROOT"] [("A", "set"), ("SHELL", "zsh")]) gcroot caller),
      runTest "--ignore-env keeps only the variables --keep-env-var names, with those --set-env-var sets" $
        assertEqual
          "environment"
          (Right (Map.fromList [("B", "b"), ("D", "d"), ("NIX_GCROOT", gcroot), ("SHELL", "bash")]))
          (shellEnvironment (changes EmptyEnvironment ["B", "missing"] [] [("D", "d")]) gcroot caller),
      runTest "--unset-env-var under --ignore-env, and --keep-env-var without it, are refused in upstream's words" $
        assertEqual "unset" (Left "--unset-env-var does not make sense with --ignore-env") (shellEnvironment (changes EmptyEnvironment [] ["A"] []) gcroot caller)
          `andThen` assertEqual "keep" (Left "--keep-env-var does not make sense without --ignore-env") (shellEnvironment (changes CallerEnvironment ["A"] [] []) gcroot caller),
      runTest "a name setenv refuses is left out, as upstream's replaceEnv goes on without it" $
        assertEqual
          "names"
          (Right ["A", "B", "C", "NIX_GCROOT", "SHELL", "x"])
          (Map.keys <$> shellEnvironment (changes CallerEnvironment [] [] [("", "empty"), ("A=B", "eq"), ("x", "y")]) gcroot caller),
      runTestM "under --ignore-env the environment depends on the caller's only through the variables kept" $
        quickCheckPasses . QC.forAll ((,,) <$> genChanges <*> genCaller <*> genCaller) $ \(given, first, second) ->
          let kept = ecKeep given
              agreeing = Map.union (Map.restrictKeys first kept) (Map.withoutKeys second kept)
           in shellEnvironment given gcroot first QC.=== shellEnvironment given gcroot agreeing,
      runTest "develop takes a target and build's target and store flags" $
        assertEqual
          "parsed"
          (Right (Cli.CmdDevelop (Cli.TargetExpr "1") (Just "a.b") developDefaults))
          (Cli.optCommand <$> Cli.parseArgs ["develop", "--expr", "1", "-A", "a.b", "--store", "/s", "--no-sandbox", "--substituter", "u", "--arg", "x", "1"])
          `andThen` assertEqual "requires a target" (Left "develop requires a PATH, <NAME> or --expr EXPR") (Cli.optCommand <$> Cli.parseArgs ["develop"])
          `andThen` assertEqual "exec-wrapper" (Left "unknown develop flag: --exec-wrapper") (Cli.optCommand <$> Cli.parseArgs ["develop", "--exec-wrapper", "s=p", "--expr", "1"])
          `andThen` assertEqual "out-link" (Left "unknown develop flag: --no-out-link") (Cli.optCommand <$> Cli.parseArgs ["develop", "--no-out-link", "--expr", "1"]),
      runTest "--command takes every argument after it, flags and --help included, and needs one" $
        assertEqual
          "command"
          (Right (Just ("sh" :| ["-c", "exit 7", "--phase", "x", "--help"])))
          (developed ["develop", "--expr", "1", "-c", "sh", "-c", "exit 7", "--phase", "x", "--help"] Cli.daCommand)
          `andThen` assertEqual "long" (Right (Just ("true" :| []))) (developed ["develop", "--expr", "1", "--command", "true"] Cli.daCommand)
          `andThen` assertEqual "none" (Left "error: --command requires at least one argument") (Cli.optCommand <$> Cli.parseArgs ["develop", "--expr", "1", "-c"])
          `andThen` assertEqual "none, short" (Left "error: --command requires at least one argument") (Cli.optCommand <$> Cli.parseArgs ["develop", "--expr", "1", "--command"]),
      runTest "the last phase flag wins, installcheck naming installCheck, and --check is develop's phase there" $
        assertEqual "last" (Right (Just "install")) (developed ["develop", "--phase", "build", "--install", "--expr", "1"] Cli.daPhase)
          `andThen` assertEqual "installcheck" (Right (Just "installCheck")) (developed ["develop", "--unpack", "--installcheck", "--expr", "1"] Cli.daPhase)
          `andThen` assertEqual "check" (Right (Just "check")) (developed ["develop", "--check", "--expr", "1"] Cli.daPhase)
          `andThen` assertEqual "phase" (Right (Just "fixup")) (developed ["develop", "--configure", "--phase", "fixup", "--expr", "1"] Cli.daPhase)
          `andThen` assertEqual "build's check" (Right True) (Cli.optCheck <$> Cli.parseArgs ["build", "--check", "--expr", "1"])
          `andThen` assertEqual "a command beside a phase parses" (Right (Just ("true" :| []), Just "build")) (developed ["develop", "--build", "--expr", "1", "-c", "true"] (\args -> (Cli.daCommand args, Cli.daPhase args))),
      runTest "the environment flags, under each spelling, gather what they name" $
        assertEqual
          "gathered"
          (Right (EmptyEnvironment, Set.fromList ["K", "L"], Set.fromList ["U", "V"], Map.fromList [("S", "s"), ("T", "t")]))
          ( developed
              ["develop", "-i", "-k", "K", "--keep-env-var", "L", "-u", "U", "--unset", "V", "-s", "S", "s", "--set-env-var", "T", "t", "--expr", "1"]
              (\args -> (Cli.daEnvironmentBase args, Cli.daKeep args, Cli.daUnset args, Cli.daSet args))
          )
          `andThen` assertEqual "ignore-environment" (Right EmptyEnvironment) (developed ["develop", "--ignore-environment", "--expr", "1"] Cli.daEnvironmentBase)
          `andThen` assertEqual "keep" (Right (Set.singleton "K")) (developed ["develop", "--keep", "K", "--expr", "1"] Cli.daKeep),
      runTest "a variable both set and unset, or set twice, is refused as it is read, in upstream's words" $
        assertEqual "unset after set" (Left "error: Cannot unset environment variable 'X' that is set with '--set-env-var'") (Cli.optCommand <$> Cli.parseArgs ["develop", "-s", "X", "1", "-u", "X", "--expr", "1"])
          `andThen` assertEqual "set after unset" (Left "error: Cannot set environment variable 'X' that is unset with '--unset-env-var'") (Cli.optCommand <$> Cli.parseArgs ["develop", "--unset", "X", "--set-env-var", "X", "1", "--expr", "1"])
          `andThen` assertEqual "set twice" (Left "error: Duplicate definition of environment variable 'X' with '--set-env-var' is ambiguous") (Cli.optCommand <$> Cli.parseArgs ["develop", "-s", "X", "1", "-s", "X", "2", "--expr", "1"])
          `andThen` assertEqual "one value" (Left "-s requires a name and a value") (Cli.optCommand <$> Cli.parseArgs ["develop", "--expr", "1", "-s", "X"]),
      runTest "print-dev-env prints the script, or with --json the environment, and takes no develop flag" $
        assertEqual "script" (Right (Cli.CmdPrintDevEnv (Cli.TargetFile "f.nix") Nothing Cli.DevEnvScript Cli.IndentedOnTerminal)) (Cli.optCommand <$> Cli.parseArgs ["print-dev-env", "f.nix"])
          `andThen` assertEqual "json" (Right (Cli.CmdPrintDevEnv (Cli.TargetExpr "1") (Just "a") Cli.DevEnvJson Cli.IndentedOnTerminal)) (Cli.optCommand <$> Cli.parseArgs ["print-dev-env", "--json", "--expr", "1", "-A", "a"])
          `andThen` assertEqual "pretty" (Right (Cli.CmdPrintDevEnv (Cli.TargetExpr "1") Nothing Cli.DevEnvJson Cli.Indented)) (Cli.optCommand <$> Cli.parseArgs ["print-dev-env", "--no-pretty", "--json", "--pretty", "--expr", "1"])
          `andThen` assertEqual "no-pretty" (Right (Cli.CmdPrintDevEnv (Cli.TargetExpr "1") Nothing Cli.DevEnvScript Cli.Compact)) (Cli.optCommand <$> Cli.parseArgs ["print-dev-env", "--expr", "1", "--pretty", "--no-pretty"])
          `andThen` assertEqual "develop's pretty" (Left "unknown develop flag: --pretty") (Cli.optCommand <$> Cli.parseArgs ["develop", "--pretty", "--expr", "1"])
          `andThen` assertEqual "command" (Left "unknown print-dev-env flag: --command") (Cli.optCommand <$> Cli.parseArgs ["print-dev-env", "--command", "true", "--expr", "1"])
          `andThen` assertEqual "develop's json" (Left "unknown develop flag: --json") (Cli.optCommand <$> Cli.parseArgs ["develop", "--json", "--expr", "1"])
    ]
  where
    prompt = Prompt "p" "" ""
    gcroot = "/s/hash-x-env"
    caller = Map.fromList [("A", "a"), ("B", "b"), ("C", "c")]
    changes base keep unset set = EnvironmentChanges base (Set.fromList keep) (Set.fromList unset) (Map.fromList set)
    developDefaults = Cli.DevelopArgs Nothing Nothing CallerEnvironment Set.empty Set.empty Map.empty
    developed args field = case Cli.optCommand <$> Cli.parseArgs args of
      Right (Cli.CmdDevelop _ _ given) -> Right (field given)
      Right other -> Left (T.pack (show other))
      Left err -> Left (T.pack err)
    genName = QC.elements ["A", "B", "C", "D", "NIX_GCROOT", "SHELL", "", "E=F"]
    genNames = Set.fromList <$> QC.listOf genName
    genCaller = Map.fromList <$> QC.listOf ((,) <$> genName <*> QC.elements ["1", "2", ""])
    genChanges = EnvironmentChanges EmptyEnvironment <$> genNames <*> pure Set.empty <*> (Map.fromList <$> QC.listOf ((,) <$> genName <*> QC.elements ["s", ""]))
    quickCheckPasses property = do
      result <- QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} property
      pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))
