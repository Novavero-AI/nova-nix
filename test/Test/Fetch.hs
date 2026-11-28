-- | Tests of fetcher inputs, their URLs and flake references, read and
-- written as upstream 2.36.0 reads and writes them, and of the
-- experimental features they need.
module Test.Fetch
  ( testFetchUrls,
    testFlakeRefs,
    testExperimentalFeatures,
    testFlakeRefBuiltins,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import Nix.Config (ConfigAssignment (..), NixConfig (..), resolveConfig)
import Nix.Eval.IO (newEvalStateWith)
import Nix.Eval.Policy (EvalPolicy (..), PathRules (..), unrestrictedPolicy)
import Nix.Eval.Types (NixValue (..), mkStr)
import Nix.ExperimentalFeature (addFeatures, featureEnabled, featureSetWarnings, flakesFeature, noFeatures, parseFeatures, replaceFeatures)
import Nix.Fetch.Attrs (AttrValue (..), Attrs)
import Nix.Fetch.Input (InputContext (..), isLegalRefName)
import Nix.Fetch.Url (Authority (..), HostType (..), Leniency (..), ParsedUrl (..), fixGitUrl, parseUrl, pathToUrlPath, percentEncode, renderUrl, runChecked, urlPathToPath)
import Nix.Flake.Ref (FlakeRef, flakeRefAttrs, flakeRefFromAttrs, flakeRefToString, parseFlakeRef, parseFlakeRefWithFragment)
import Test.Harness (TestResult (..), andThen, assertEqual, assertEval, assertEvalError, evalNixIOIn, evalNixIOStoreDir, runTest, runTestM, testBaseDir, testLogger, testSessionStore)
import qualified Test.QuickCheck as QC

-- | A QuickCheck property as a test result.
property :: (QC.Testable prop) => prop -> IO TestResult
property prop = do
  result <- QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} prop
  pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))

-- | A context with flakes on, under POSIX paths.
flakesOn :: InputContext
flakesOn = InputContext (parseFeatures "flakes") PosixPaths

-- | A context with no feature on.
featuresOff :: InputContext
featuresOff = InputContext noFeatures PosixPaths

-- ---------------------------------------------------------------------------
-- URLs
-- ---------------------------------------------------------------------------

-- | Upstream's URL grammar, encodings, Git URLs and paths.
testFetchUrls :: IO [Bool]
testFetchUrls = do
  putStrLn "fetch/urls"
  roundTrip <- property (QC.forAll genUrl (\url -> fst (runChecked (parseUrl Strict (renderUrl url))) == Right url))
  sequence
    [ runTest "a rendered URL parses back to itself" roundTrip,
      runTest "a URL's parts decode" $
        assertEqual
          "parsed"
          (Right (ParsedUrl "https" (Just (Authority HostName "example.org" (Just "me") Nothing (Just 8080))) ["", "a b", "c"] (Map.fromList [("k", "v w")]) "f g"))
          (fst (runChecked (parseUrl Strict "https://me@example.org:8080/a%20b/c?k=v%20w#f%20g"))),
      runTest "a lenient parse encodes a query's spaces and quotes and a fragment's carets" $
        assertEqual
          "rendered"
          (Right "github:a/b?ref=x%20y#%22n%5E%22")
          (renderUrl <$> fst (runChecked (parseUrl Lenient "github:a/b?ref=x y#\"n^\""))),
      runTest "a strict parse refuses a space" $
        assertEqual "refused" (Left "'github:a/b?ref=x y' is not a valid URL: leftover") (fst (runChecked (parseUrl Strict "github:a/b?ref=x y"))),
      runTest "a URL needs a scheme" $
        assertEqual "refused" (Left "'/tmp/x' doesn't have a scheme") (fst (runChecked (parseUrl Lenient "/tmp/x"))),
      runTest "a file URL names no host" $
        assertEqual "refused" (Left "file:// URL 'file://host/x' has unexpected authority 'host'") (fst (runChecked (parseUrl Strict "file://host/x"))),
      runTest "a file URL with no path has the root" $
        assertEqual "path" (Right ["", ""]) (urlPath <$> fst (runChecked (parseUrl Strict "file://"))),
      runTest "a port must be from 1 to 65535" $
        assertEqual "refused" (Left "port '0' is invalid") (fst (runChecked (parseUrl Strict "http://h:0/"))),
      runTest "a parameter with no value is dropped with a warning" $
        assertEqual
          "parsed"
          (Right (Map.fromList [("b", "1")]), ["dubious URI query 'a' is missing equal sign '=', ignoring"])
          (let (result, warnings) = runChecked (parseUrl Strict "x:y?a&b=1&b=2") in (urlQuery <$> result, warnings)),
      runTest "encoding keeps the unreserved characters and those asked" $
        assertEqual "encoded" "a-._~%2B%2F:@%25" (percentEncode ":@" "a-._~+/:@%"),
      runTest "an SCP-style Git URL becomes an SSH URL with an absolute path, and warns" $
        let (result, warnings) = runChecked (fixGitUrl PosixPaths "git@github.com:NixOS/nix")
         in assertEqual "url" (Right "ssh://git@github.com/NixOS/nix") (renderUrl <$> result)
              `andThen` assertEqual "warned" 1 (length warnings),
      runTest "an SCP-style URL's bracketed IPv6 host is written canonically" $
        assertEqual "url" (Right "ssh://u@[::1]/~/r") (renderUrl <$> fst (runChecked (fixGitUrl PosixPaths "u@[0:0::1]:~/r"))),
      runTest "an absolute path is a file URL, and a git+ scheme loses its git" $
        assertEqual
          "urls"
          [Right "file:///tmp/repo", Right "https://example.org/r"]
          (map (fmap renderUrl . fst . runChecked . fixGitUrl PosixPaths) ["/tmp/repo", "git+https://example.org/r"]),
      runTest "a path's URL segments are std::filesystem's elements" $
        assertEqual
          "segments"
          [["", "a", "b"], ["", ""], ["", "a", "", ""], [".", "x"], ["", "C:", "x"]]
          (map (uncurry pathToUrlPath) [(PosixPaths, "/a//b"), (PosixPaths, "/"), (PosixPaths, "/a/"), (PosixPaths, "./x"), (WindowsPaths, "C:/x")]),
      runTest "a path from URL segments appends as std::filesystem's /= does" $
        assertEqual
          "paths"
          [Right "/a/b", Right "/a/", Right "C:/x", Left "URL path component 'a/b' contains '/', which is not allowed in file names"]
          (map (fst . runChecked . uncurry urlPathToPath) [(PosixPaths, ["", "a", "b"]), (PosixPaths, ["", "a", "", ""]), (WindowsPaths, ["", "C:", "x"]), (PosixPaths, ["", "a/b"])])
    ]

-- | A URL the grammar admits, its parts as a parse of its rendering
-- gives them back: a scheme that is not a file URL's, a path that starts
-- with an empty segment under an authority and never with two without
-- one, and any bytes in the segments, the query and the fragment.
genUrl :: QC.Gen ParsedUrl
genUrl = do
  scheme <- QC.elements ["https", "git+ssh", "github", "x-y.z"]
  authority <- QC.oneof [pure Nothing, Just <$> genAuthority]
  segments <- QC.listOf genBytes
  firstSegment <- genNonEmptyBytes
  path <- case authority of
    Just _ -> QC.elements [[""], "" : segments]
    Nothing -> QC.elements [[""], "" : firstSegment : segments, firstSegment : segments]
  query <- Map.fromList <$> QC.listOf ((,) <$> genBytes <*> genBytes)
  ParsedUrl scheme authority path query <$> genBytes
  where
    genAuthority = do
      host <- QC.elements ["example.org", "", "h"]
      user <- QC.oneof [pure Nothing, Just <$> genBytes]
      password <- maybe (pure Nothing) (const (QC.oneof [pure Nothing, Just <$> genBytes])) user
      port <- QC.oneof [pure Nothing, Just <$> QC.choose (1, maxBound)]
      pure (Authority HostName host user password port)
    genBytes = BS.pack <$> QC.listOf genByte
    genNonEmptyBytes = BS.pack <$> QC.listOf1 genByte
    genByte = QC.frequency [(3, QC.elements (BS.unpack "abc/?#%:@ +=&")), (1, QC.arbitrary :: QC.Gen Word8)]

-- ---------------------------------------------------------------------------
-- Flake references
-- ---------------------------------------------------------------------------

-- | A reference read with flakes on: its attributes and its string.
readRef :: ByteString -> Either Text (Attrs, ByteString)
readRef ref = fst (runChecked (parseFlakeRef flakesOn ref >>= \flakeRef -> (,) (flakeRefAttrs flakeRef) <$> flakeRefToString flakesOn flakeRef))

-- | Attributes from strings.
strings :: [(Text, ByteString)] -> Attrs
strings = Map.fromList . map (fmap AttrString)

-- | Flake references in each of upstream's forms, and what they read as.
testFlakeRefs :: IO [Bool]
testFlakeRefs = do
  putStrLn "fetch/flake-refs"
  githubRoundTrip <- property (QC.forAll genForgeAttrs (\attrs -> fmap fst (readRef . snd =<< rendered attrs) == Right attrs))
  sequence
    [ runTest "a forge reference that renders reads back to its attributes" githubRoundTrip,
      runTest "a flake ID with a ref and a revision, its empty segments skipped" $
        assertEqual
          "read"
          (Right (strings [("type", "indirect"), ("id", "nixpkgs"), ("ref", "branch"), ("rev", rev)], "flake:nixpkgs/branch/" <> rev))
          (readRef ("nixpkgs/branch///" <> rev <> "///")),
      runTest "an indirect URL's empty segments are skipped" $
        assertEqual "read" (Right (strings [("type", "indirect"), ("id", "nixpkgs"), ("ref", "branch")], "flake:nixpkgs/branch")) (readRef "flake:/nixpkgs///branch////"),
      runTest "a GitHub URL's path is decoded and its authority ignored" $
        assertEqual
          "read"
          (Right (strings [("type", "github"), ("owner", "ownerB"), ("repo", "repoA"), ("ref", "branchC")], "github:ownerB/repoA/branchC"))
          (readRef "github://////owner%42/////repo%41///branch%43////"),
      runTest "a GitLab owner keeps a percent it decodes to, encoded again" $
        assertEqual
          "read"
          (Right (strings [("type", "gitlab"), ("owner", "owner%2Fsubgroup"), ("repo", "repoA"), ("ref", "branchC")], "gitlab:owner%252Fsubgroup/repoA/branchC"))
          (readRef "gitlab:/owner%252Fsubgroup/////repo%41///branch%43////"),
      runTest "a GitHub ref of several segments is one segment, encoded" $
        assertEqual "string" (Right "github:nixos/nix/master%2Fsomething") (snd <$> readRef "github:nixos/nix//master///something/"),
      runTest "a ref with a hash sign" $
        assertEqual "strings" [Right "github:foo/bar/branch%23", Right "github:foo/bar/branch%23"] (map (fmap snd . readRef) ["github:foo/bar/branch%23", "github:foo/bar?ref=branch%23"]),
      runTest "a fragment with quotes is read leniently" $
        assertEqual
          "fragment"
          (Right "\"name.with.dot\"")
          (snd <$> fst (runChecked (parseFlakeRefWithFragment flakesOn "github:foo/bar?ref=branch#\"name.with.dot\""))),
      runTest "a Git URL loses its git+ and keeps its ref" $
        assertEqual
          "read"
          (Right (strings [("type", "git"), ("ref", "branch"), ("url", "https://somewhere.example/repo")], "git+https://somewhere.example/repo?ref=branch"))
          (readRef "git+https://somewhere.example/repo?ref=branch"),
      runTest "a path's query becomes attributes, its counts numbers" $
        assertEqual
          "read"
          (Right (Map.fromList [("type", AttrString "path"), ("path", AttrString "/foo/bar"), ("rev", AttrString aRev), ("revCount", AttrInt 123)], "path:/foo/bar?rev=" <> aRev <> "&revCount=123"))
          (readRef ("/foo/bar?revCount=123&rev=" <> aRev)),
      runTest "a path's subdirectory is its dir parameter" $
        assertEqual
          "read"
          (Right (strings [("type", "path"), ("path", "/foo bar/baz"), ("dir", "bla space")], "path:/foo%20bar/baz?dir=bla%20space"))
          (readRef "/foo bar/baz?dir=bla space"),
      runTest "a tarball's plus sign is encoded" $
        assertEqual "string" (Right "http://localhost:8181/test/%2B3d.tar.gz") (snd <$> readRef "http://localhost:8181/test/+3d.tar.gz"),
      runTest "the refusals are upstream's" $
        assertEqual
          "refusals"
          [ Left "URL 'github:NixOS' is invalid",
            Left "URL 'github:a/b?xyzzy=1' contains unknown parameter 'xyzzy'",
            Left "flake reference './relative' is not an absolute path",
            Left "unexpected fragment 'hello' in flake reference 'nixpkgs#hello'",
            Left "input 'foo:bar' is unsupported",
            Left "input 'file+git:///x' is unsupported; did you mean 'git+file' instead of 'file+git'?",
            Left "'1abc' is not a valid flake ID",
            Left "GitHub URL 'flake:nixpkgs/a/b/c' is invalid"
          ]
          (map (fmap snd . readRef) ["github:NixOS", "github:a/b?xyzzy=1", "./relative", "nixpkgs#hello", "foo:bar", "file+git:///x", "flake:1abc", "flake:nixpkgs/a/b/c"]),
      runTest "attributes become a reference" $
        assertEqual
          "strings"
          [Right "github:NixOS/nixpkgs/23.05?dir=lib", Right "git+file:///tmp/repo", Right ("tarball+https://x/y?narHash=sha256-" <> zeroSri), Right "path:/a?narHash=&revCount=3"]
          ( map
              fromAttrs
              [ strings [("type", "github"), ("owner", "NixOS"), ("repo", "nixpkgs"), ("ref", "23.05"), ("dir", "lib")],
                strings [("type", "git"), ("url", "/tmp/repo")],
                strings [("type", "tarball"), ("url", "tarball+https://x/y"), ("narHash", "")],
                Map.fromList [("type", AttrString "path"), ("path", AttrString "/a"), ("narHash", AttrString ""), ("revCount", AttrInt 3)]
              ]
          ),
      runTest "the attribute refusals are upstream's" $
        assertEqual
          "refusals"
          [ Left "input attribute 'foo' not supported by scheme 'github'",
            Left "'type' attribute to specify input scheme is required but not provided",
            Left "cannot show unsupported input '{\"type\":\"unknown\",\"x\":1}'",
            Left ("input {\"owner\":\"a\",\"ref\":\"x\",\"repo\":\"b\",\"rev\":\"" <> T.pack (BC.unpack aRev) <> "\",\"type\":\"github\"} contains both a commit hash ('" <> T.pack (BC.unpack aRev) <> "') and a branch/tag name ('x')")
          ]
          ( map
              fromAttrs
              [ strings [("type", "github"), ("owner", "a"), ("repo", "b"), ("foo", "c")],
                strings [("owner", "a")],
                Map.fromList [("type", AttrString "unknown"), ("x", AttrInt 1)],
                strings [("type", "github"), ("owner", "a"), ("repo", "b"), ("ref", "x"), ("rev", aRev)]
              ]
          ),
      runTest "a scheme flakes own needs the feature, and Git's needs none" $
        assertEqual
          "read"
          [Left missingFlakes, Left missingFlakes, Left missingFlakes, Right "git+https://example.org/r?ref=main"]
          (map (\ref -> fst (runChecked (parseFlakeRef featuresOff ref >>= flakeRefToString featuresOff))) ["github:NixOS/nixpkgs", "nixpkgs", "/tmp/x", "git+https://example.org/r?ref=main"]),
      runTest "Git accepts the ref names libgit2 does" $
        assertEqual
          "legal"
          [True, True, True, True, False, False, False, False, False, False, False, False]
          (map isLegalRefName ["main", "refs/heads/x", "HEAD", "v1.0", "@", "a..b", "a.lock", ".a", "a/", "a b", "a@{1}", "a."])
    ]
  where
    rev = "2aae6c35c94fcfb415dbe95f408b9ce91ee846ed"
    aRev = BC.replicate 40 'a'
    zeroSri = BC.replicate 43 'A' <> "%3D"
    missingFlakes = "experimental Nix feature 'flakes' is disabled; add '--extra-experimental-features flakes' to enable it"
    fromAttrs attrs = fst (runChecked (flakeRefFromAttrs flakesOn attrs >>= flakeRefToString flakesOn))
    rendered :: Attrs -> Either Text (FlakeRef, ByteString)
    rendered attrs = fst (runChecked (flakeRefFromAttrs flakesOn attrs >>= \flakeRef -> (,) flakeRef <$> flakeRefToString flakesOn flakeRef))

-- | Forge and flake ID attributes upstream writes back as they read:
-- names of letters and digits, a legal ref or a lower-case revision.
genForgeAttrs :: QC.Gen Attrs
genForgeAttrs = do
  scheme <- QC.elements ["github", "gitlab", "sourcehut", "indirect"]
  name <- genName
  repo <- genName
  pin <- QC.oneof [pure [], (\r -> [("ref", r)]) <$> genRef, (\r -> [("rev", r)]) <$> genRev]
  subdir <- QC.oneof [pure [], (\d -> [("dir", d)]) <$> genName]
  let base
        | scheme == "indirect" = [("type", scheme), ("id", name)]
        | otherwise = [("type", scheme), ("owner", name), ("repo", repo)]
  pure (strings (base ++ pin ++ subdir))
  where
    genName = BC.pack <$> ((:) <$> QC.elements ['a' .. 'z'] <*> QC.listOf (QC.elements (['a' .. 'z'] ++ ['0' .. '9'])))
    genRef = BC.intercalate "/" <$> QC.listOf1 genName
    genRev = BC.pack <$> QC.vectorOf 40 (QC.elements "0123456789abcdef")

-- ---------------------------------------------------------------------------
-- Experimental features
-- ---------------------------------------------------------------------------

-- | The setting's values, its warnings, and its place in nix.conf.
testExperimentalFeatures :: IO [Bool]
testExperimentalFeatures = do
  putStrLn "fetch/experimental-features"
  sequence
    [ runTest "a value's names are read in byte order, an unknown one dropped with a warning" $
        assertEqual
          "warnings"
          ["unknown experimental feature 'aaa'", "experimental feature 'no-url-literals' has been stabilized and renamed; use 'lint-url-literals = fatal' setting instead", "unknown experimental feature 'zzz'"]
          (featureSetWarnings (parseFeatures "zzz flakes no-url-literals aaa flakes")),
      runTest "a value turns on the features it names" $
        assertEqual "flakes" (True, False) (featureEnabled flakesFeature (parseFeatures " nix-command\tflakes "), featureEnabled flakesFeature (parseFeatures "nix-command")),
      runTest "an assignment replaces the features and an extra one adds to them, the warnings of both kept" $
        let replaced = replaceFeatures (parseFeatures "flakes bogus") (parseFeatures "nix-command")
            added = addFeatures (parseFeatures "nix-command") (parseFeatures "flakes")
         in assertEqual "replaced" (False, ["unknown experimental feature 'bogus'"]) (featureEnabled flakesFeature replaced, featureSetWarnings replaced)
              `andThen` assertEqual "added" True (featureEnabled flakesFeature added),
      runTest "nix.conf sets the features, extra- adds to them" $
        assertEqual
          "flakes"
          [Right True, Right False, Right True]
          ( map
              (fmap (featureEnabled flakesFeature . ncExperimentalFeatures) . resolveConfig . map (uncurry ConfigAssignment))
              [ [("experimental-features", "nix-command flakes")],
                [("experimental-features", "flakes"), ("experimental-features", "nix-command")],
                [("experimental-features", "nix-command"), ("extra-experimental-features", "flakes")]
              ]
          )
    ]

-- ---------------------------------------------------------------------------
-- The builtins
-- ---------------------------------------------------------------------------

-- | The policy with flakes on.
flakesPolicy :: EvalPolicy
flakesPolicy = unrestrictedPolicy {epExperimentalFeatures = parseFeatures "flakes"}

-- | Evaluate under the IO evaluator with flakes on, as the pure one
-- evaluates under the default policy whatever its builtins were made
-- under.
evalWithFlakes :: Text -> IO (Either Text NixValue)
evalWithFlakes source = do
  storeDir <- evalNixIOStoreDir
  st <- newEvalStateWith testLogger flakesPolicy [] (testSessionStore storeDir) storeDir testBaseDir
  evalNixIOIn st testBaseDir source

-- | @builtins.parseFlakeRef@ and @builtins.flakeRefToString@.
testFlakeRefBuiltins :: IO [Bool]
testFlakeRefBuiltins = do
  putStrLn "fetch/flake-ref-builtins"
  sequence
    [ runTestM "parseFlakeRef gives a reference's attributes" $
        assertEqual "attrs" (Right (mkStr "{\"dir\":\"lib\",\"owner\":\"NixOS\",\"ref\":\"23.05\",\"repo\":\"nixpkgs\",\"type\":\"github\"}"))
          <$> evalWithFlakes "builtins.toJSON (builtins.parseFlakeRef \"github:NixOS/nixpkgs/23.05?dir=lib\")",
      runTestM "parseFlakeRef gives a count as an integer" $
        assertEqual "count" (Right (VInt 123)) <$> evalWithFlakes "(builtins.parseFlakeRef \"path:/foo?revCount=123\").revCount",
      runTestM "flakeRefToString writes a reference back" $
        assertEqual "string" (Right (mkStr "github:NixOS/nixpkgs/23.05?dir=lib"))
          <$> evalWithFlakes "builtins.flakeRefToString { dir = \"lib\"; owner = \"NixOS\"; ref = \"23.05\"; repo = \"nixpkgs\"; type = \"github\"; }",
      runTestM "flakeRefToString checks the set's attributes in its order" $
        assertEqual "refusal" (Left "flake reference attribute sets may only contain integers, Booleans, and strings, but attribute 'type' is a list")
          <$> evalWithFlakes "builtins.flakeRefToString { type = [ ]; zz = throw \"later\"; }",
      runTestM "flakeRefToString refuses a negative integer" $
        assertEqual "refusal" (Left "negative value given for flake ref attr revCount: -1")
          <$> evalWithFlakes "builtins.flakeRefToString { type = \"path\"; path = \"/a\"; revCount = -1; }",
      runTest "with no feature on, a GitHub reference is refused" $
        assertEvalError "parseFlakeRef" "builtins.parseFlakeRef \"github:NixOS/nixpkgs\"" "experimental Nix feature 'flakes' is disabled; add '--extra-experimental-features flakes' to enable it",
      runTest "both are builtins whatever the features, getFlake is not listed, and fetchTree is not there" $
        assertEval "listed" "builtins.toJSON [ (builtins ? parseFlakeRef) (builtins ? flakeRefToString) (builtins ? getFlake) (builtins ? fetchTree) ]" (mkStr "[true,true,false,false]")
    ]
