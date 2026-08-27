-- | Nix configuration: the @nix.conf@ / @NIX_CONFIG@ settings that decide
-- which binary caches a machine substitutes from and which public keys it
-- trusts.
--
-- == The format
--
-- One @name = value@ assignment per line.  A @#@ truncates the rest of the
-- line (a comment).  A list value is whitespace separated.  These are the
-- rules upstream's @parseConfigFiles@ applies (comment truncation, no line
-- continuation, whitespace tokenizing), matched here.
--
-- == Precedence
--
-- Sources are folded weakest first, so a later source overrides an earlier
-- one.  A plain assignment REPLACES the accumulated value; an @extra-@
-- prefixed assignment APPENDS to it.  The caller supplies the sources in
-- order (built-in default, then files, then @NIX_CONFIG@, then the command
-- line), so the command line wins, exactly as upstream orders them.
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
    applyAssignment,
    applyConfigText,
    resolveConfig,
  )
where

import Data.Text (Text)
import qualified Data.Text as T

-- ---------------------------------------------------------------------------
-- Settings
-- ---------------------------------------------------------------------------

-- | The subset of Nix settings that this layer resolves: the binary caches
-- to try and the public keys their signatures are trusted under.  Both are
-- ordered lists (upstream's @substituters@ is a @Strings@, and
-- @trusted-public-keys@ is too); order is preserved on write, though it
-- does not affect key acceptance (any trusted key is enough).
data NixConfig = NixConfig
  { ncSubstituters :: ![Text],
    ncTrustedPublicKeys :: ![Text]
  }
  deriving (Eq, Show)

-- | The baseline the fold starts from: no substituters and no trusted
-- keys.  This is nova-nix's existing default (nothing is substituted
-- unless configured), a deliberate divergence from upstream's
-- cache.nixos.org default: turning a cache on for every machine is the
-- operator's decision to make in a config file, not a built-in.
defaultNixConfig :: NixConfig
defaultNixConfig = NixConfig {ncSubstituters = [], ncTrustedPublicKeys = []}

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
            Left ("nix.conf: '" <> directive <> "' is not supported yet")
      (name : eq : valueTokens)
        | eq == assignEq -> Right (ConfigAssignment name (T.unwords valueTokens))
      _ -> Left ("nix.conf: syntax error in line '" <> T.strip line <> "'")

-- ---------------------------------------------------------------------------
-- Applying
-- ---------------------------------------------------------------------------

-- | Apply one assignment to the accumulated config.  The name is resolved
-- through the aliases (@binary-caches@ -> @substituters@,
-- @binary-cache-public-keys@ -> @trusted-public-keys@) and the @extra-@
-- prefix (append rather than replace).  A name outside the known set is
-- ignored, not an error: upstream warns and continues, and refusing every
-- unknown key would reject a config that also carries settings this layer
-- does not model yet.
applyAssignment :: NixConfig -> ConfigAssignment -> NixConfig
applyAssignment config (ConfigAssignment name value) =
  case resolveName name of
    Nothing -> config
    Just (field, mode) ->
      let parsed = T.words value
       in setField field (combine mode (getField field config) parsed) config
  where
    combine ReplaceMode _ new = new
    combine AppendMode old new = old ++ new

-- | Which list a setting name targets, once the @extra-@ prefix and the
-- aliases are resolved, and whether it replaces or appends.
resolveName :: Text -> Maybe (ConfigField, ApplyMode)
resolveName name =
  case T.stripPrefix extraPrefix name of
    Just base -> tag AppendMode (baseField base)
    Nothing -> tag ReplaceMode (baseField name)
  where
    tag mode mField = case mField of
      Just field -> Just (field, mode)
      Nothing -> Nothing
    baseField n
      | n == substitutersKey || n == substitutersAlias = Just SubstitutersField
      | n == trustedKeysKey || n == trustedKeysAlias = Just TrustedKeysField
      | otherwise = Nothing

-- | The two list settings this layer resolves.
data ConfigField = SubstitutersField | TrustedKeysField
  deriving (Eq, Show)

-- | Whether an assignment replaces the accumulated value or appends to it.
data ApplyMode = ReplaceMode | AppendMode
  deriving (Eq, Show)

getField :: ConfigField -> NixConfig -> [Text]
getField SubstitutersField = ncSubstituters
getField TrustedKeysField = ncTrustedPublicKeys

setField :: ConfigField -> [Text] -> NixConfig -> NixConfig
setField SubstitutersField v config = config {ncSubstituters = v}
setField TrustedKeysField v config = config {ncTrustedPublicKeys = v}

-- | Parse and apply one source's text onto the accumulated config.
applyConfigText :: NixConfig -> Text -> Either Text NixConfig
applyConfigText config text = foldl applyAssignment config <$> parseConfigText text

-- | Fold a list of sources onto the default config, weakest first.  The
-- caller orders the list so the command line is last (and so wins); a
-- parse error in any source aborts, since a security-relevant file that
-- does not parse must not be treated as absent.
resolveConfig :: [Text] -> Either Text NixConfig
resolveConfig = foldl step (Right defaultNixConfig)
  where
    step acc source = acc >>= \config -> applyConfigText config source

-- ---------------------------------------------------------------------------
-- Setting names
-- ---------------------------------------------------------------------------

substitutersKey, substitutersAlias :: Text
substitutersKey = "substituters"
substitutersAlias = "binary-caches"

trustedKeysKey, trustedKeysAlias :: Text
trustedKeysKey = "trusted-public-keys"
trustedKeysAlias = "binary-cache-public-keys"

extraPrefix :: Text
extraPrefix = "extra-"

assignEq :: Text
assignEq = "="

includeDirective, bangIncludeDirective :: Text
includeDirective = "include"
bangIncludeDirective = "!include"
