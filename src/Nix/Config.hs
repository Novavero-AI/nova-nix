-- | Nix configuration: the @nix.conf@ / @NIX_CONFIG@ settings this
-- implementation reads: which binary caches a machine substitutes from,
-- which public keys it trusts, and how deep function calls may nest.
--
-- == The format
--
-- One @name = value@ assignment per line.  A @#@ truncates the rest of the
-- line (a comment).  A list value is whitespace separated.  These are the
-- rules upstream's @parseConfigFiles@ applies (comment truncation, no line
-- continuation, whitespace tokenizing), matched here.  An integer value is
-- upstream's too ('parseUnsignedSetting'): decimal digits, an optional
-- leading @+@, and an optional binary unit suffix (@K@, @M@, @G@, @T@).
--
-- == Precedence
--
-- Sources are folded weakest first, so a later source overrides an earlier
-- one.  A plain assignment REPLACES the accumulated value; an @extra-@
-- prefixed assignment APPENDS to it, for a list setting (upstream accepts
-- @extra-@ on an appendable setting only, so @extra-max-call-depth@ is an
-- unknown name).  The caller supplies the sources in order (built-in
-- default, then files, then @NIX_CONFIG@, then the command line), so the
-- command line wins, exactly as upstream orders them.
--
-- == Security
--
-- @trusted-public-keys@ decides which signatures a substituted path is
-- accepted under.  The precedence is therefore load bearing: a source that
-- REPLACES the trusted set where it should APPEND, or an @extra-@ that is
-- mishandled, silently widens what the machine trusts.  The fold here is
-- pure and total so the whole rule set can be tested directly.
--
-- Not yet modelled (tracked separately): @include@ \/ @!include@
-- directives, the @\/etc\/nix@ system file and the @XDG_CONFIG_DIRS@
-- cascade.  A line opening with @include@ is refused loudly rather than
-- silently skipped, so a config that depends on one fails visibly.
module Nix.Config
  ( -- * Resolved settings
    NixConfig (..),
    defaultNixConfig,

    -- * Parsing and folding
    ConfigAssignment (..),
    parseConfigText,
    parseUnsignedSetting,
    applyAssignment,
    applyConfigText,
    resolveConfig,
  )
where

import Control.Monad (foldM)
import Data.Char (toUpper)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Word (Word32)
import Nix.Eval.CallDepth (defaultMaxCallDepth)

-- ---------------------------------------------------------------------------
-- Settings
-- ---------------------------------------------------------------------------

-- | The subset of Nix settings that this layer resolves: the binary caches
-- to try, the public keys their signatures are trusted under, and the
-- function-call nesting ceiling.  The first two are ordered lists
-- (upstream's @substituters@ is a @Strings@, and @trusted-public-keys@ is
-- too); order is preserved on write, though it does not affect key
-- acceptance (any trusted key is enough).
data NixConfig = NixConfig
  { ncSubstituters :: ![Text],
    ncTrustedPublicKeys :: ![Text],
    -- | Upstream's @max-call-depth@: how many function calls may be
    -- active around a new one before evaluation refuses it.
    ncMaxCallDepth :: !Word32
  }
  deriving (Eq, Show)

-- | The baseline the fold starts from: no substituters, no trusted keys,
-- and upstream's call-depth ceiling.  The empty lists are nova-nix's
-- existing default (nothing is substituted unless configured), a
-- deliberate divergence from upstream's cache.nixos.org default: turning a
-- cache on for every machine is the operator's decision to make in a
-- config file, not a built-in.
defaultNixConfig :: NixConfig
defaultNixConfig =
  NixConfig
    { ncSubstituters = [],
      ncTrustedPublicKeys = [],
      ncMaxCallDepth = defaultMaxCallDepth
    }

-- ---------------------------------------------------------------------------
-- Parsing
-- ---------------------------------------------------------------------------

-- | One parsed @name = value@ assignment, before its name is resolved
-- against the known settings and aliases.
data ConfigAssignment = ConfigAssignment
  { caName :: !Text,
    caValue :: !Text
  }
  deriving (Eq, Show)

-- | Parse config text into ordered assignments.  Comments and blank lines
-- drop out; a malformed line (fewer than @name = value@, or a missing
-- @=@) is a loud error rather than a silent skip, matching upstream's
-- @UsageError@ - a typo in a security-relevant file must not pass for an
-- empty setting.  An @include@ \/ @!include@ line is refused as not yet
-- supported.
parseConfigText :: Text -> Either Text [ConfigAssignment]
parseConfigText = traverse parseLine . filter (not . isBlank) . map stripComment . T.lines
  where
    isBlank line = null (T.words line)
    stripComment = T.takeWhile (/= '#')
    parseLine line = case T.words line of
      (directive : _)
        | directive == includeDirective || directive == bangIncludeDirective ->
            Left (configErrorPrefix <> "'" <> directive <> "' is not supported yet")
      (name : eq : valueTokens)
        | eq == assignEq -> Right (ConfigAssignment name (T.unwords valueTokens))
      _ -> Left (configErrorPrefix <> "syntax error in line '" <> T.strip line <> "'")

-- | Parse an unsigned integer setting the way upstream's
-- @BaseSetting\<unsigned int\>@ does (@string2IntWithUnitPrefix@ over
-- @boost::lexical_cast@, config-impl.hh and util.hh at 2.24.9): decimal
-- digits with an optional leading @+@, then an optional binary unit
-- letter, @K@, @M@, @G@ or @T@ in either case, multiplying by 1024 to
-- that power.  Anything else (a minus sign, a fraction, hex, a number
-- past @unsigned int@) is upstream's @UsageError@, worded as it words it,
-- with no source label: upstream names the setting alone, and the value
-- may have come from @NIX_CONFIG@ rather than a file.
-- One divergence: upstream computes the unit product in 64 bits and
-- narrows it silently, so @5G@ is accepted there as 1073741824; here it
-- is refused like any other value outside the range, since a ceiling
-- that cannot mean what it says must not quietly become another number.
parseUnsignedSetting :: Text -> Text -> Either Text Word32
parseUnsignedSetting name value = maybe (Left invalid) Right (unitValue >>= inRange)
  where
    invalid = "setting '" <> name <> "' has invalid value '" <> value <> "'"
    unsigned = fromMaybe value (T.stripPrefix plusSign value)
    unitValue = case T.unsnoc unsigned of
      Just (digits, unit) | Just multiplier <- unitMultiplier unit -> (* multiplier) <$> decimalInteger digits
      _ -> decimalInteger unsigned
    inRange n
      | n <= toInteger (maxBound :: Word32) = Just (fromInteger n)
      | otherwise = Nothing

-- | A whole run of decimal digits as an 'Integer'; anything else, the empty
-- text included, is 'Nothing'.
decimalInteger :: Text -> Maybe Integer
decimalInteger digits = case TR.decimal digits of
  Right (n, rest) | T.null rest -> Just n
  _ -> Nothing

-- | Upstream's binary unit letters.
unitMultiplier :: Char -> Maybe Integer
unitMultiplier unit = case toUpper unit of
  'K' -> Just (unitBase ^ (1 :: Int))
  'M' -> Just (unitBase ^ (2 :: Int))
  'G' -> Just (unitBase ^ (3 :: Int))
  'T' -> Just (unitBase ^ (4 :: Int))
  _ -> Nothing

unitBase :: Integer
unitBase = 1024

-- ---------------------------------------------------------------------------
-- Applying
-- ---------------------------------------------------------------------------

-- | Apply one assignment to the accumulated config.  The name is resolved
-- through the aliases (@binary-caches@ -> @substituters@,
-- @binary-cache-public-keys@ -> @trusted-public-keys@) and the @extra-@
-- prefix (append rather than replace, list settings only).  A name
-- outside the known set is ignored, not an error: upstream warns and
-- continues, and refusing every unknown key would reject a config that
-- also carries settings this layer does not model yet.  A value that does
-- not parse for its setting is an error, as upstream's @UsageError@ is.
applyAssignment :: NixConfig -> ConfigAssignment -> Either Text NixConfig
applyAssignment config (ConfigAssignment name value) =
  case resolveName name of
    Nothing -> Right config
    Just (ListSetting field mode) ->
      Right (setList field (combine mode (getList field config) (T.words value)) config)
    Just (ScalarSetting MaxCallDepthField) ->
      (\limit -> config {ncMaxCallDepth = limit}) <$> parseUnsignedSetting name value
  where
    combine ReplaceMode _ new = new
    combine AppendMode old new = old ++ new

-- | Which setting a name targets, once the @extra-@ prefix and the aliases
-- are resolved: a list, with whether it replaces or appends, or a scalar,
-- which only ever replaces.
data ConfigSetting
  = ListSetting !ListField !ApplyMode
  | ScalarSetting !ScalarField
  deriving (Eq, Show)

resolveName :: Text -> Maybe ConfigSetting
resolveName name = case T.stripPrefix extraPrefix name of
  Just base -> (`ListSetting` AppendMode) <$> listField base
  Nothing
    | Just field <- listField name -> Just (ListSetting field ReplaceMode)
    | Just field <- scalarField name -> Just (ScalarSetting field)
    | otherwise -> Nothing

listField :: Text -> Maybe ListField
listField name
  | name == substitutersKey || name == substitutersAlias = Just SubstitutersField
  | name == trustedKeysKey || name == trustedKeysAlias = Just TrustedKeysField
  | otherwise = Nothing

scalarField :: Text -> Maybe ScalarField
scalarField name
  | name == maxCallDepthKey = Just MaxCallDepthField
  | otherwise = Nothing

-- | The list settings this layer resolves.
data ListField = SubstitutersField | TrustedKeysField
  deriving (Eq, Show)

-- | The scalar settings this layer resolves.
data ScalarField = MaxCallDepthField
  deriving (Eq, Show)

-- | Whether an assignment replaces the accumulated value or appends to it.
data ApplyMode = ReplaceMode | AppendMode
  deriving (Eq, Show)

getList :: ListField -> NixConfig -> [Text]
getList SubstitutersField = ncSubstituters
getList TrustedKeysField = ncTrustedPublicKeys

setList :: ListField -> [Text] -> NixConfig -> NixConfig
setList SubstitutersField v config = config {ncSubstituters = v}
setList TrustedKeysField v config = config {ncTrustedPublicKeys = v}

-- | Parse and apply one source's text onto the accumulated config.
applyConfigText :: NixConfig -> Text -> Either Text NixConfig
applyConfigText config text = parseConfigText text >>= foldM applyAssignment config

-- | Fold a list of sources onto the default config, weakest first.  The
-- caller orders the list so the command line is last (and so wins); a
-- parse error in any source aborts, since a security-relevant file that
-- does not parse must not be treated as absent.
resolveConfig :: [Text] -> Either Text NixConfig
resolveConfig = foldM applyConfigText defaultNixConfig

-- ---------------------------------------------------------------------------
-- Setting names
-- ---------------------------------------------------------------------------

substitutersKey, substitutersAlias :: Text
substitutersKey = "substituters"
substitutersAlias = "binary-caches"

trustedKeysKey, trustedKeysAlias :: Text
trustedKeysKey = "trusted-public-keys"
trustedKeysAlias = "binary-cache-public-keys"

maxCallDepthKey :: Text
maxCallDepthKey = "max-call-depth"

extraPrefix :: Text
extraPrefix = "extra-"

assignEq :: Text
assignEq = "="

plusSign :: Text
plusSign = "+"

includeDirective, bangIncludeDirective :: Text
includeDirective = "include"
bangIncludeDirective = "!include"

-- | What a syntax error this module reports opens with: the format's
-- name, since the parser is not told which source a line came from.  A
-- setting's value error carries none, as upstream's does not.
configErrorPrefix :: Text
configErrorPrefix = "nix.conf: "
