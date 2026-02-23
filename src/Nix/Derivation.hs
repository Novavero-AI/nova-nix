-- | Nix derivation representation and serialization.
--
-- == What is a derivation?
--
-- A derivation (.drv file) is a BUILD RECIPE.  It is the output of
-- evaluating a Nix expression.  When you write:
--
-- @
-- stdenv.mkDerivation {
--   pname = "hello";
--   version = "2.12.1";
--   src = fetchurl { ... };
--   buildInputs = [ zlib ];
-- }
-- @
--
-- The evaluator reduces this to a @.drv@ file that says:
--
-- @
-- Derive(
--   [("out", "\/nix\/store\/abc...-hello-2.12.1", "", "sha256")],
--   [("\/nix\/store\/def...-zlib-1.3.1.drv", ["out"]),
--    ("\/nix\/store\/ghi...-bash-5.2.drv", ["out"])],
--   ["\/nix\/store\/jkl...-hello-2.12.1.tar.gz"],
--   "x86_64-linux",
--   "\/nix\/store\/mno...-bash-5.2\/bin\/bash",
--   ["\/nix\/store\/pqr...-stdenv\/setup"],
--   [("buildInputs", "\/nix\/store\/def...-zlib-1.3.1"),
--    ("builder", "\/nix\/store\/mno...-bash-5.2\/bin\/bash"),
--    ("name", "hello-2.12.1"),
--    ("out", "\/nix\/store\/abc...-hello-2.12.1"),
--    ("src", "\/nix\/store\/jkl...-hello-2.12.1.tar.gz"),
--    ("system", "x86_64-linux")]
-- )
-- @
--
-- That's ATerm format.  Every derivation is:
--
-- 1. __Outputs__: what store paths will be produced (usually just "out")
-- 2. __Input derivations__: other .drv files this depends on (and which
--    of their outputs we need)
-- 3. __Input sources__: non-derivation store paths (source tarballs, patches)
-- 4. __Platform__: what system to build on ("x86_64-linux", "x86_64-windows")
-- 5. __Builder__: the executable to run (usually bash)
-- 6. __Args__: command-line arguments to the builder
-- 7. __Environment__: environment variables for the build
--
-- The HASH of this .drv file (after ATerm serialization) determines the
-- output store path.  Change any input → different hash → different output
-- path.  This is how Nix achieves reproducibility.
module Nix.Derivation
  ( -- * Derivation type
    Derivation (..),
    DerivationOutput (..),

    -- * ATerm serialization
    toATerm,
    fromATerm,

    -- * Platform
    Platform (..),
    currentPlatform,
  )
where

import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Nix.Store.Path (StorePath)
import qualified System.Info as SI

-- | Target platform for a derivation.
data Platform
  = X86_64_Linux
  | X86_64_Darwin
  | Aarch64_Darwin
  | X86_64_Windows
  | Aarch64_Linux
  | OtherPlatform !Text
  deriving (Eq, Ord, Show)

-- | Detect the current platform using 'System.Info'.
-- GHC bakes @os@ and @arch@ into the compiled binary at compile time,
-- so this is equivalent to CPP ifdefs but ormolu-compatible.
currentPlatform :: Platform
currentPlatform = case (SI.arch, SI.os) of
  ("x86_64", "mingw32") -> X86_64_Windows
  ("x86_64", "darwin") -> X86_64_Darwin
  ("aarch64", "darwin") -> Aarch64_Darwin
  ("aarch64", "linux") -> Aarch64_Linux
  ("x86_64", "linux") -> X86_64_Linux
  (arch, os) -> OtherPlatform (packPlatform arch os)

-- | Format an arch-os pair as a Nix platform string.
packPlatform :: String -> String -> Text
packPlatform arch os =
  let archText = case arch of
        "x86_64" -> "x86_64"
        "aarch64" -> "aarch64"
        other -> other
      osText = case os of
        "mingw32" -> "windows"
        "darwin" -> "darwin"
        "linux" -> "linux"
        other -> other
   in mconcat [toText archText, "-", toText osText]

-- | Convert String to Text.
toText :: String -> Text
toText = T.pack

-- | A single output of a derivation.
data DerivationOutput = DerivationOutput
  { -- | Output name (usually "out", sometimes "dev", "lib", "doc").
    doName :: !Text,
    -- | The store path this output will be placed at.
    doPath :: !StorePath,
    -- | Hash algorithm (empty for input-addressed, "sha256" for fixed).
    doHashAlgo :: !Text,
    -- | Expected hash (empty for input-addressed, actual hash for fixed).
    doHash :: !Text
  }
  deriving (Eq, Show)

-- | A complete derivation — everything needed to build a package.
data Derivation = Derivation
  { -- | What this derivation produces.
    drvOutputs :: ![DerivationOutput],
    -- | Other derivations this depends on, and which outputs we need.
    drvInputDrvs :: !(Map StorePath [Text]),
    -- | Non-derivation store paths used as inputs (sources, patches).
    drvInputSrcs :: ![StorePath],
    -- | Target platform: "x86_64-linux", "x86_64-windows", etc.
    drvPlatform :: !Platform,
    -- | The builder executable (store path to bash, or other).
    drvBuilder :: !Text,
    -- | Arguments to the builder.
    drvArgs :: ![Text],
    -- | Environment variables for the build.
    drvEnv :: !(Map Text Text)
  }
  deriving (Eq, Show)

-- | Serialize a derivation to ATerm format (the .drv file format).
-- This serialization is what gets hashed to compute the store path.
toATerm :: Derivation -> Text
toATerm _drv = "" -- TODO: implement ATerm serialization

-- | Parse a .drv file from ATerm format.
fromATerm :: Text -> Either Text Derivation
fromATerm _text = Left "ATerm parser not yet implemented"
