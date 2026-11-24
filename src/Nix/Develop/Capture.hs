-- | The derivation @nix develop@ builds to capture a target's build
-- environment, as upstream Nix 2.35.4 makes it
-- (@getDerivationEnvironment@, @src\/nix\/develop.cc@:232-311).
--
-- The capture is the target's derivation with upstream's @get-env.sh@ as
-- its builder's one argument.  Its builder sources the stdenv's setup as
-- the target's build would, then writes what bash holds as JSON to the
-- first output, which "Nix.Develop.Environment" reads.  The checks on
-- what the outputs refer to are dropped, the derivation is renamed
-- @NAME-env@, and its outputs are input-addressed again, so a fixed-output
-- target's capture is input-addressed too.
--
-- Everything here is pure: the caller adds the script to the store,
-- writes and builds the capture, and reads which of its outputs holds
-- the environment.
module Nix.Develop.Capture
  ( -- * The script
    getEnvScriptName,
    getEnvScriptContents,
    getEnvScriptPath,

    -- * The capture
    CaptureFailure (..),
    captureFailureMessage,
    captureDerivation,

    -- * The environment's output
    OutputObject (..),
    environmentOutput,
    noEnvironmentMessage,
  )
where

import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word64)
import Nix.Derivation (Derivation (..), DerivationOutput (..), DrvFile, fixedOutput, inputAddressed)
import Nix.Derivation.StructuredAttrs (StructuredAttrs (..), encodeStructuredAttrs, structuredAttrsKey)
import Nix.Hash (makeTextPath, sha256Digest)
import Nix.Json (Json (..), parseJson)
import Nix.Store.Path (BadStorePath, StorePath)
import qualified Nix.Store.Path as SP

-- ---------------------------------------------------------------------------
-- The script
-- ---------------------------------------------------------------------------

-- | The name upstream adds @get-env.sh@ to the store under.
getEnvScriptName :: Text
getEnvScriptName = "get-env.sh"

-- | What upstream stores for @get-env.sh@, given the file's bytes: a
-- newline, then the file.  Upstream's build embeds the file in a raw
-- string literal its generator opens with @R"__NIX_STR(@ and a newline
-- (@nix-meson-build-support\/generate-header\/meson.build@:7 at 2.35.4),
-- so the string, and the store object made from it, starts with that
-- newline.
getEnvScriptContents :: ByteString -> ByteString
getEnvScriptContents = BC.cons '\n'

-- | The store path of @get-env.sh@, given the file's bytes: a text object
-- with no references, as upstream adds it (develop.cc:241-250).
getEnvScriptPath :: ByteString -> Either BadStorePath StorePath
getEnvScriptPath file = makeTextPath getEnvScriptName (sha256Digest (getEnvScriptContents file)) []

-- ---------------------------------------------------------------------------
-- The capture
-- ---------------------------------------------------------------------------

-- | Why a derivation has no capture.
data CaptureFailure
  = -- | Its builder's base name is not @bash@.
    NotBashBuilder
  | -- | An output is floating content-addressed or impure, which upstream
    -- keeps in the capture and nova-nix builds for no derivation.
    UncapturableOutputs
  | -- | Its structured attributes are not a JSON object, with why.
    UnreadableStructuredAttrs !Text
  | -- | The capture's name breaks the store-path name rules.
    BadCaptureName !BadStorePath
  deriving (Eq, Show)

-- | A capture failure in upstream's words where upstream has them.
captureFailureMessage :: CaptureFailure -> Text
captureFailureMessage failure = case failure of
  NotBashBuilder -> "'nix develop' only works on derivations that use 'bash' as their builder"
  UncapturableOutputs -> "the derivation has a floating content-addressed or impure output, which nova-nix does not build"
  UnreadableStructuredAttrs reason -> "cannot process " <> structuredAttrsKey <> " attribute: " <> reason
  BadCaptureName bad -> SP.badStorePathText bad

-- | The capture of the derivation whose @.drv@ is named @NAME.drv@, given
-- @get-env.sh@'s store path and the modulo hashes of its input
-- derivations as 'inputAddressed' takes them.  Upstream's steps
-- (develop.cc:236-288 at 2.35.4):
--
-- * a builder whose base name is not @bash@ is refused;
-- * the script is the one argument, and an input source;
-- * the checks on what the outputs refer to are removed: the four
--   variables, or under structured attributes their @outputChecks@, with
--   @__json@ written again as nlohmann's @dump()@ writes it
--   (parsed-derivations.cc:34-38);
-- * @name@ is @NAME-env@, the capture's name, and a variable even under
--   structured attributes;
-- * each input-addressed or fixed output is deferred and then filled in
--   input-addressed (@fillInOutputPaths@).
--
-- Everything else is kept.  An output that is neither, which upstream
-- would keep as it is, is refused.
captureDerivation :: StorePath -> Text -> [(Text, [Text])] -> Derivation -> Either CaptureFailure DrvFile
captureDerivation script name inputHashes drv
  | builderBaseName (drvBuilder drv) /= bashBuilder = Left NotBashBuilder
  | not (capturable drv) = Left UncapturableOutputs
  | otherwise = do
      env <- withoutChecks (drvEnv drv)
      first BadCaptureName $
        inputAddressed
          captureName
          inputHashes
          drv
            { drvArgs = [TE.encodeUtf8 (SP.storePathToText SP.defaultStoreDir script)],
              drvInputSrcs = if script `elem` drvInputSrcs drv then drvInputSrcs drv else drvInputSrcs drv <> [script],
              drvEnv = Map.insert "name" (TE.encodeUtf8 captureName) env
            }
  where
    captureName = name <> captureSuffix

-- | What upstream appends to the target's name.
captureSuffix :: Text
captureSuffix = "-env"

-- | The base name a builder must have.
bashBuilder :: ByteString
bashBuilder = "bash"

-- | Upstream's @baseNameOf@ (file-system.cc:137-153 at 2.35.4): what
-- follows the last @/@, once trailing ones but the first byte are dropped.
builderBaseName :: ByteString -> ByteString
builderBaseName path = case BS.uncons path of
  Nothing -> BS.empty
  Just (start, rest) -> BC.takeWhileEnd (/= '/') (BS.cons start (BC.dropWhileEnd (== '/') rest))

-- | Whether every output is one upstream defers: all input-addressed, or
-- the derivation fixed-output.  Upstream refuses a fixed output beside
-- others as it reads the derivation (derivations.cc:740-744 at 2.24.9).
capturable :: Derivation -> Bool
capturable drv = all (T.null . doHashAlgo) (drvOutputs drv) || isJust (fixedOutput drv)

-- | The environment less the checks on what the outputs refer to
-- (develop.cc:254-262 at 2.35.4).  A derivation is structured when it
-- has @__json@, as upstream's @StructuredAttrs::tryExtract@ decides
-- (parsed-derivations.cc:22-32), and its @__json@ is read as
-- @StructuredAttrs::parse@ reads it (:10-20), into an object
-- (parsed-derivations.hh:22): @null@ is refused there, where
-- 'Nix.Derivation.StructuredAttrs.decodeStructuredAttrs', which reads the
-- entry for a build as 2.24.9 does, takes it as an empty object.
withoutChecks :: Map Text ByteString -> Either CaptureFailure (Map Text ByteString)
withoutChecks env = case Map.lookup structuredAttrsKey env of
  Nothing -> Right (foldr Map.delete env referenceChecks)
  Just encoded -> do
    text <- first (UnreadableStructuredAttrs . T.pack . show) (TE.decodeUtf8' encoded)
    parsed <- first UnreadableStructuredAttrs (parseJson text)
    case parsed of
      JsonObject attrs -> Right (Map.insert structuredAttrsKey (encodeStructuredAttrs (StructuredAttrs (Map.delete outputChecks attrs))) env)
      _ -> Left (UnreadableStructuredAttrs "type must be object")

-- | The structured attribute that holds the checks.
outputChecks :: Text
outputChecks = "outputChecks"

-- | The variables that hold the checks without structured attributes.
referenceChecks :: [Text]
referenceChecks = ["allowedReferences", "allowedRequisites", "disallowedReferences", "disallowedRequisites"]

-- ---------------------------------------------------------------------------
-- The environment's output
-- ---------------------------------------------------------------------------

-- | What one of a capture's built outputs is, as upstream's @maybeLstat@
-- of its root answers.
data OutputObject
  = -- | A regular file of this many bytes.
    RegularFile !Word64
  | -- | A directory or a symlink.
    NotRegularFile
  deriving (Eq, Show)

-- | The output the environment is read from: the first, in name order,
-- that is a regular file of at least one byte, as upstream reads the
-- outputs (develop.cc:303-310 at 2.35.4).  'Nothing' when none is, where
-- upstream fails with 'noEnvironmentMessage'.
environmentOutput :: Map Text OutputObject -> Maybe Text
environmentOutput outputs = listToMaybe [name | (name, RegularFile size) <- Map.toAscList outputs, size > 0]

-- | Upstream's failure when no output holds an environment.
noEnvironmentMessage :: Text
noEnvironmentMessage = "get-env.sh failed to produce an environment"
