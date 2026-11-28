-- | Tests of flake registries: their files read as upstream 2.36.0 reads
-- them, the lookup that resolves a reference against them, and the lines
-- @registry list@ prints.
module Test.Registry
  ( testRegistryFiles,
    testRegistryLookups,
    testRegistryReadIO,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Nix.Eval.Policy (PathRules (..))
import Nix.ExperimentalFeature (flakesFeature, missingFeatureMessage, noFeatures, parseFeatures)
import Nix.Fetch.Attrs (AttrValue (..))
import Nix.Fetch.Input (Input (..), InputContext (..), Scheme (..), schemeName)
import Nix.Fetch.Registry (GlobalRegistry (..), Registry (..), RegistryEntry (..), RegistryLocations (..), RegistryType (..), RegistryUse (..), globalRegistryFrom, lookupInRegistries, parseRegistry, readRegistries, registryListLines, resolveFlakeRef)
import Nix.Fetch.Url (Checked, runChecked)
import Nix.Flake.Ref (flakeRefToString, parseFlakeRef)
import Nix.Http (defaultFetchRetryPolicy)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory)
import System.FilePath ((</>))
import Test.Harness (TestResult (..), andThen, assertEqual, forceRemoveIfExists, runTest, runTestM, testLogger)

-- | A context with flakes on, under POSIX paths.
flakesOn :: InputContext
flakesOn = InputContext (parseFeatures "flakes") PosixPaths

-- | A checked step's value, or its refusal.
checked :: Checked a -> Either Text a
checked = fst . runChecked

-- | An input of a scheme with attributes, as reading them gives one.
input :: Scheme -> [(Text, ByteString)] -> Input
input scheme attrs = Input (Just scheme) (Map.fromList (("type", AttrString (TE.encodeUtf8 (schemeName scheme))) : map (fmap AttrString) attrs))

-- | An indirect input, with a ref when one is given.
indirect :: ByteString -> Maybe ByteString -> Input
indirect flakeId ref = input SchemeIndirect (("id", flakeId) : [("ref", r) | Just r <- [ref]])

-- | A GitHub input, with a ref when one is given.
github :: ByteString -> ByteString -> Maybe ByteString -> Input
github owner repo ref = input SchemeGitHub ([("owner", owner), ("repo", repo)] ++ [("ref", r) | Just r <- [ref]])

-- | A registry entry from one input to another.
entry :: Input -> Input -> RegistryEntry
entry from to = RegistryEntry from to Map.empty False

-- | A reference resolved against registries, as its URL.
resolved :: RegistryUse -> [Registry] -> ByteString -> Either Text ByteString
resolved use registries text =
  checked (parseFlakeRef flakesOn text >>= resolveFlakeRef PosixPaths use registries >>= flakeRefToString flakesOn)

-- | Read a registry file's bytes as the user's registry named @r@.
readAs :: InputContext -> ByteString -> (Registry, [Text])
readAs ctx = parseRegistry ctx UserRegistry "r"

-- ---------------------------------------------------------------------------
-- Files
-- ---------------------------------------------------------------------------

-- | Registry files and their failures.
testRegistryFiles :: IO [Bool]
testRegistryFiles = do
  putStrLn "fetch/registry-files"
  sequence
    [ runTest "a version 2 file's entries are read in order, a dir kept apart and exact read" $
        assertEqual
          "entries"
          ( [ RegistryEntry (indirect "a" Nothing) (github "o" "a" Nothing) (Map.singleton "dir" (AttrString "sub")) False,
              RegistryEntry (indirect "b" Nothing) (github "o" "b" Nothing) Map.empty True
            ],
            []
          )
          ( let (registry, warnings) =
                  readAs
                    flakesOn
                    "{\"version\": 2, \"flakes\": [\
                    \{\"from\": {\"type\": \"indirect\", \"id\": \"a\"}, \"to\": {\"type\": \"github\", \"owner\": \"o\", \"repo\": \"a\", \"dir\": \"sub\"}},\
                    \{\"from\": {\"type\": \"indirect\", \"id\": \"b\"}, \"to\": {\"type\": \"github\", \"owner\": \"o\", \"repo\": \"b\"}, \"exact\": true}]}"
             in (registryEntries registry, warnings)
          ),
      runTest "a file of another version is refused, naming it" $
        assertEqual "warnings" ["cannot read flake registry 'r': flake registry 'r' has unsupported version 1"] (snd (readAs flakesOn "{\"version\": 1, \"flakes\": []}"))
          `andThen` assertEqual "no version" ["cannot read flake registry 'r': flake registry 'r' has unsupported version 0"] (snd (readAs flakesOn "{}")),
      runTest "JSON of the wrong shape is refused in nlohmann's words" $
        assertEqual "array" ["cannot parse flake registry 'r': [json.exception.type_error.306] cannot use value() with array"] (snd (readAs flakesOn "[]"))
          `andThen` assertEqual "version" ["cannot parse flake registry 'r': [json.exception.type_error.302] type must be number, but is string"] (snd (readAs flakesOn "{\"version\": \"2\"}"))
          `andThen` assertEqual "exact" ["cannot parse flake registry 'r': [json.exception.type_error.302] type must be boolean, but is number"] (snd (readAs flakesOn "{\"version\": 2, \"flakes\": [{\"from\": {\"type\": \"indirect\", \"id\": \"a\"}, \"to\": {\"type\": \"indirect\", \"id\": \"b\"}, \"exact\": 1}]}")),
      runTest "a file that is not JSON is refused as one that does not parse" $
        assertEqual "prefix" [True] (map ("cannot parse flake registry 'r': " `T.isPrefixOf`) (snd (readAs flakesOn "not json"))),
      runTest "an entry that fails keeps the entries before it" $
        let (registry, warnings) =
              readAs
                flakesOn
                "{\"version\": 2, \"flakes\": [\
                \{\"from\": {\"type\": \"indirect\", \"id\": \"a\"}, \"to\": {\"type\": \"github\", \"owner\": \"o\", \"repo\": \"a\"}},\
                \{\"from\": {\"type\": \"indirect\", \"id\": \"b\"}, \"to\": {\"type\": \"github\", \"owner\": [\"o\"], \"repo\": \"b\"}},\
                \{\"from\": {\"type\": \"indirect\", \"id\": \"c\"}, \"to\": {\"type\": \"github\", \"owner\": \"o\", \"repo\": \"c\"}}]}"
         in assertEqual "entries" [entry (indirect "a" Nothing) (github "o" "a" Nothing)] (registryEntries registry)
              `andThen` assertEqual "warnings" ["cannot read flake registry 'r': unsupported input attribute type in lock file"] warnings,
      runTest "an entry needing flakes is refused with the feature off" $
        assertEqual
          "warnings"
          ["cannot read flake registry 'r': " <> missingFeatureMessage flakesFeature]
          (snd (readAs (InputContext noFeatures PosixPaths) "{\"version\": 2, \"flakes\": [{\"from\": {\"type\": \"indirect\", \"id\": \"a\"}, \"to\": {\"type\": \"git\", \"url\": \"https://example.org/a\"}}]}")),
      runTest "the setting names no global registry, a file or a URL" $
        assertEqual "none" NoGlobalRegistry (globalRegistryFrom "")
          `andThen` assertEqual "file" (GlobalRegistryFile "/etc/registry.json") (globalRegistryFrom "/etc/registry.json")
          `andThen` assertEqual "url" (GlobalRegistryUrl "https://channels.nixos.org/flake-registry.json") (globalRegistryFrom "https://channels.nixos.org/flake-registry.json"),
      runTest "registry list prints each registry's entries under its padded name" $
        assertEqual
          "lines"
          (Right ["user   flake:a github:o/a?dir=sub", "system flake:b github:o/b", "global flake:c github:o/c"])
          ( checked
              ( registryListLines
                  PosixPaths
                  [ Registry UserRegistry [(entry (indirect "a" Nothing) (github "o" "a" Nothing)) {entryExtraAttrs = Map.singleton "dir" (AttrString "sub")}],
                    Registry SystemRegistry [entry (indirect "b" Nothing) (github "o" "b" Nothing)],
                    Registry GlobalRegistry [entry (indirect "c" Nothing) (github "o" "c" Nothing)]
                  ]
              )
          )
    ]

-- ---------------------------------------------------------------------------
-- Lookups
-- ---------------------------------------------------------------------------

-- | How a reference resolves against registries.
testRegistryLookups :: IO [Bool]
testRegistryLookups = do
  putStrLn "fetch/registry-lookups"
  let global = Registry GlobalRegistry [entry (indirect "nixpkgs" Nothing) (github "NixOS" "nixpkgs" (Just "nixpkgs-unstable"))]
      user = Registry UserRegistry [entry (indirect "nixpkgs" Nothing) (github "me" "nixpkgs" Nothing)]
  sequence
    [ runTest "an indirect reference resolves to what the entry maps it to" $
        assertEqual "resolved" (Right "github:NixOS/nixpkgs/nixpkgs-unstable") (resolved AllRegistries [global] "nixpkgs"),
      runTest "the user's registry is asked before the global one" $
        assertEqual "resolved" (Right "github:me/nixpkgs") (resolved AllRegistries [user, global] "nixpkgs"),
      runTest "a limited lookup asks only the flag and global registries" $
        assertEqual "resolved" (Right "github:NixOS/nixpkgs/nixpkgs-unstable") (resolved LimitedRegistries [user, global] "nixpkgs"),
      runTest "a ref the entry does not set is carried over" $
        assertEqual "resolved" (Right "github:me/nixpkgs/nixos-24.05") (resolved AllRegistries [user] "nixpkgs/nixos-24.05"),
      runTest "a revision the entry does not set is carried over" $
        assertEqual "resolved" (Right "github:me/nixpkgs/2aae6c35c94fcfb415dbe95f408b9ce91ee846ed") (resolved AllRegistries [user] "nixpkgs/2aae6c35c94fcfb415dbe95f408b9ce91ee846ed"),
      runTest "a forge given both a ref and a revision is refused" $
        assertEqual
          "refused"
          (Left "cannot apply both a commit hash (2aae6c35c94fcfb415dbe95f408b9ce91ee846ed) and a branch/tag name ('main') to input 'github:me/nixpkgs'")
          (resolved AllRegistries [user] "nixpkgs/main/2aae6c35c94fcfb415dbe95f408b9ce91ee846ed"),
      runTest "an entry that sets a ref keeps its own" $
        assertEqual "resolved" (Right "github:NixOS/nixpkgs/nixpkgs-unstable") (resolved AllRegistries [Registry GlobalRegistry [entry (indirect "nixpkgs" (Just "nixpkgs-unstable")) (github "NixOS" "nixpkgs" (Just "nixpkgs-unstable"))]] "nixpkgs/nixpkgs-unstable"),
      runTest "an exact entry maps only a reference equal to its own" $
        let exact = Registry UserRegistry [(entry (indirect "nixpkgs" Nothing) (github "me" "exact" Nothing)) {entryExact = True}]
         in assertEqual "equal" (Right "github:me/exact") (resolved AllRegistries [exact, global] "nixpkgs")
              `andThen` assertEqual "with a ref" (Right "github:NixOS/nixpkgs/nixos-24.05") (resolved AllRegistries [exact, global] "nixpkgs/nixos-24.05"),
      runTest "a lookup follows an entry to another indirect reference" $
        assertEqual "resolved" (Right "github:NixOS/nixpkgs/nixpkgs-unstable") (resolved AllRegistries [Registry UserRegistry [entry (indirect "pkgs" Nothing) (indirect "nixpkgs" Nothing)], global] "pkgs"),
      runTest "the entry's dir is the reference's subdirectory" $
        assertEqual "resolved" (Right "github:o/a?dir=sub") (resolved AllRegistries [Registry UserRegistry [(entry (indirect "a" Nothing) (github "o" "a" Nothing)) {entryExtraAttrs = Map.singleton "dir" (AttrString "sub")}]] "a"),
      runTest "a reference no entry maps is refused" $
        assertEqual "refused" (Left "cannot find flake 'flake:zzz' in the flake registries") (resolved AllRegistries [global] "zzz"),
      runTest "a direct reference is itself" $
        assertEqual "resolved" (Right "github:a/b") (resolved AllRegistries [global] "github:a/b"),
      runTest "an entry mapping a reference to itself is a cycle" $
        assertEqual "refused" (Left "cycle detected in flake registry for 'flake:a'") (resolved AllRegistries [Registry UserRegistry [entry (indirect "a" Nothing) (indirect "a" Nothing)]] "a"),
      runTest "a ref is refused for a tarball" $
        assertEqual
          "refused"
          (Left "don't know how to set branch/tag name of input 'https://example.org/a.tar.gz' to 'main'")
          (resolved AllRegistries [Registry UserRegistry [entry (indirect "a" Nothing) (input SchemeTarball [("url", "https://example.org/a.tar.gz")])]] "a/main"),
      runTest "a lookup returns the last entry's attributes with its input" $
        assertEqual
          "attrs"
          (Right (Map.singleton "dir" (AttrString "sub")))
          (snd <$> checked (lookupInRegistries PosixPaths AllRegistries [Registry UserRegistry [(entry (indirect "a" Nothing) (github "o" "a" Nothing)) {entryExtraAttrs = Map.singleton "dir" (AttrString "sub")}]] (indirect "a" Nothing)))
    ]

-- ---------------------------------------------------------------------------
-- Reading the files
-- ---------------------------------------------------------------------------

-- | The registries read from their files: a missing file is an empty
-- registry, and a global one a file names is read as the others are.
testRegistryReadIO :: IO [Bool]
testRegistryReadIO = do
  putStrLn "fetch/registry-read-io"
  tmpBase <- getTemporaryDirectory
  let root = tmpBase </> "nova-nix-test-registry"
      file name = root </> name
      registryJson :: ByteString -> ByteString
      registryJson flakeId = "{\"version\": 2, \"flakes\": [{\"from\": {\"type\": \"indirect\", \"id\": \"" <> flakeId <> "\"}, \"to\": {\"type\": \"github\", \"owner\": \"o\", \"repo\": \"" <> flakeId <> "\"}}]}"
  forceRemoveIfExists root
  createDirectoryIfMissing True root
  BS.writeFile (file "user.json") (registryJson "u")
  BS.writeFile (file "global.json") (registryJson "g")
  results <-
    sequence
      [ runTestM "the user's, the machine's and the global registry are read in order" $ do
          outcome <- readRegistries testLogger defaultFetchRetryPolicy flakesOn (RegistryLocations (file "user.json") (Just (file "absent.json")) (GlobalRegistryFile (file "global.json")))
          pure $ case outcome of
            Left err -> Fail err
            Right (registries, warnings) ->
              assertEqual "lines" (Right ["user   flake:u github:o/u", "global flake:g github:o/g"]) (checked (registryListLines PosixPaths registries))
                `andThen` assertEqual "warnings" [] warnings
      ]
  forceRemoveIfExists root
  pure results
