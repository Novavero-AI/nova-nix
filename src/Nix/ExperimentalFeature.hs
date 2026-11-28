-- | Upstream's experimental features: the names its
-- @experimental-features@ setting takes (experimental-features.cc at
-- 2.36.0), the set a configuration enables, and the refusal of what needs
-- a feature that is off.
--
-- A feature is one upstream knows by name; a name it does not know is
-- dropped from the set with a warning, as upstream's setting parses it
-- (configuration.cc).  nova-nix implements few of them: a feature turned
-- on is held whatever nova-nix does with it, so a check reads the set as
-- upstream's would.
module Nix.ExperimentalFeature
  ( -- * Features
    ExperimentalFeature,
    featureName,
    flakesFeature,
    verifiedFetchesFeature,

    -- * Sets
    FeatureSet,
    noFeatures,
    featureEnabled,
    parseFeatures,
    replaceFeatures,
    addFeatures,
    featureSetWarnings,

    -- * Refusals
    requireFeature,
    missingFeatureMessage,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)

-- | A feature upstream knows, by its name.  The constructor is not
-- exported: a feature is one of 'knownFeatureNames'.
newtype ExperimentalFeature = ExperimentalFeature Text
  deriving (Eq, Ord, Show)

-- | A feature's name, as the setting spells it.
featureName :: ExperimentalFeature -> Text
featureName (ExperimentalFeature name) = name

-- | The names of upstream's features (@xpFeatureDetails@ in
-- experimental-features.cc at 2.36.0).
knownFeatureNames :: Set Text
knownFeatureNames =
  Set.fromList
    [ "ca-derivations",
      "impure-derivations",
      "flakes",
      "fetch-tree",
      "nix-command",
      "git-hashing",
      "recursive-nix",
      "fetch-closure",
      "auto-allocate-uids",
      "cgroups",
      "dynamic-derivations",
      "parse-toml-timestamps",
      "read-only-local-store",
      "local-overlay-store",
      "configurable-impure-env",
      "mounted-ssh-store",
      "verified-fetches",
      "pipe-operators",
      "external-builders",
      "blake3-hashes"
    ]

-- | @flakes@: flake references and the fetchers only flakes use.
flakesFeature :: ExperimentalFeature
flakesFeature = ExperimentalFeature "flakes"

-- | @verified-fetches@: a Git input's commit signature attributes.
verifiedFetchesFeature :: ExperimentalFeature
verifiedFetchesFeature = ExperimentalFeature "verified-fetches"

-- | The name a feature had before upstream stabilized it as a setting,
-- which the setting refuses with a warning naming the setting.
stabilizedUrlLiterals :: Text
stabilizedUrlLiterals = "no-url-literals"

-- | The features a configuration enables, and the warnings parsing its
-- values gave, in the order given, which the caller reports once the
-- configuration is read.
data FeatureSet = FeatureSet
  { fsFeatures :: !(Set ExperimentalFeature),
    fsWarnings :: ![Text]
  }
  deriving (Eq, Show)

-- | No feature, upstream's default.
noFeatures :: FeatureSet
noFeatures = FeatureSet Set.empty []

-- | Whether a feature is on.
featureEnabled :: ExperimentalFeature -> FeatureSet -> Bool
featureEnabled feature = Set.member feature . fsFeatures

-- | The warnings parsing the set's values gave.
featureSetWarnings :: FeatureSet -> [Text]
featureSetWarnings = fsWarnings

-- | Parse a value as upstream's setting parses one: its whitespace
-- separated names as a set, so a repeated name counts once and the names
-- are read in byte order, each it does not know dropped with a warning.
parseFeatures :: ByteString -> FeatureSet
parseFeatures value = foldr add noFeatures (Set.toAscList names)
  where
    names = Set.fromList (filter (not . BC.null) (BC.splitWith (`elem` separators) value))
    separators = " \t\n\r" :: String
    add raw (FeatureSet features warnings)
      | name `Set.member` knownFeatureNames = FeatureSet (Set.insert (ExperimentalFeature name) features) warnings
      | name == stabilizedUrlLiterals =
          FeatureSet features (("experimental feature '" <> name <> "' has been stabilized and renamed; use 'lint-url-literals = fatal' setting instead") : warnings)
      | otherwise = FeatureSet features (("unknown experimental feature '" <> name <> "'") : warnings)
      where
        name = TE.decodeUtf8With lenientDecode raw

-- | An assignment of the setting: its features replace the set's, and
-- its warnings follow the set's.
replaceFeatures :: FeatureSet -> FeatureSet -> FeatureSet
replaceFeatures (FeatureSet _ earlier) (FeatureSet features warnings) = FeatureSet features (earlier ++ warnings)

-- | An @extra-@ assignment: its features join the set's.
addFeatures :: FeatureSet -> FeatureSet -> FeatureSet
addFeatures (FeatureSet held earlier) (FeatureSet features warnings) = FeatureSet (Set.union held features) (earlier ++ warnings)

-- | Refuse what needs a feature that is off, in upstream's words.
requireFeature :: ExperimentalFeature -> FeatureSet -> Either Text ()
requireFeature feature set
  | featureEnabled feature set = Right ()
  | otherwise = Left (missingFeatureMessage feature)

-- | Upstream's refusal of what needs a feature that is off
-- (@MissingExperimentalFeature@ in experimental-features.cc).
missingFeatureMessage :: ExperimentalFeature -> Text
missingFeatureMessage feature =
  "experimental Nix feature '" <> featureName feature <> "' is disabled; add '--extra-experimental-features " <> featureName feature <> "' to enable it"
