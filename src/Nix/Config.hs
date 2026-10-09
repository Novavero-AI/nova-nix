-- | Nix configuration: the @nix.conf@ / @NIX_CONFIG@ settings this
-- implementation reads: which binary caches a machine substitutes from,
-- which public keys it trusts, and how deep function calls may nest.
--
-- == The format
--
-- One @name = value@ assignment per line.  A @#@ truncates the rest of the
-- line (a comment).  A list value is whitespace separated.  An @include@
-- or @!include@ line splices another file in at that position: the path
-- is resolved against the directory of the file being parsed, and the two
-- forms differ only in that @!include@ goes on without a file that cannot
-- be read.  These are the rules upstream's @parseConfigFiles@ applies
-- (@config.cc@ at 2.24.9: comment truncation, no line continuation,
-- whitespace tokenizing, inline include expansion), matched here.  An
-- integer value is upstream's too ('parseUnsignedSetting'): decimal
-- digits, an optional leading @+@, and an optional binary unit suffix
-- (@K@, @M@, @G@, @T@).
--
-- == Where the files are
--
-- Upstream's @loadConfFile@ (@globals.cc@) reads, weakest first: the system
-- file @$NIX_CONF_DIR\/nix.conf@, by default under the platform's
-- machine-wide config directory (@\/etc\/nix@ where upstream runs), the
-- directory having to be absolute and being collapsed lexically as the
-- @Settings@ constructor runs it through @canonPath@; the user files,
-- which are @$NIX_USER_CONF_FILES@ when that is set and otherwise
-- @nix\/nix.conf@ under the XDG config home and then under each
-- @XDG_CONFIG_DIRS@ entry, applied back to front so the first listed wins;
-- then @NIX_CONFIG@.  'configFilePaths' computes that order from the
-- environment and the platform's directories, which the caller looks up
-- (this module holds no platform literal), and 'loadConfig' reads and
-- folds it.
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
-- pure and total so the whole rule set can be tested directly, and the
-- include expander is written over a file-reading effect
-- ('ReadConfigFile') for the same reason.
--
-- One deliberate divergence: upstream silently drops an @include@ whose
-- file exists but cannot be read (its own TODO questions that).  Here a
-- required include that cannot be read is an error, because the only
-- thing @include@ promises over @!include@ is to fail when the file is
-- unusable, and an include is where a site narrows the substituter or
-- trusted-key set; dropping it leaves the wider set in force.  The reader
-- therefore reports which of the two happened ('ConfigReadFailure'), so a
-- missing include is reported in upstream's words and an unreadable one
-- in its own.
module Nix.Config
  ( -- * Resolved settings
    NixConfig (..),
    defaultNixConfig,

    -- * Where the files are
    ConfigLocations (..),
    configFilePaths,
    nixConfFileName,
    nixConfDirName,

    -- * Sources and includes
    ConfigSource (..),
    nixConfigSourceName,
    ConfigReadFailure (..),
    ReadConfigFile,
    loadConfig,
    expandConfigSource,

    -- * Parsing and folding
    ConfigLine (..),
    IncludeMode (..),
    ConfigAssignment (..),
    parseConfigLines,
    parseUnsignedSetting,
    applyAssignment,
    resolveConfig,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (foldM, mfilter, when)
import Control.Monad.Except (ExceptT (..), liftEither, runExceptT, throwError)
import Control.Monad.Trans (lift)
import Data.Char (toUpper)
import Data.Either (rights)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Word (Word32)
import Nix.Eval.CallDepth (defaultMaxCallDepth)
import Nix.Eval.CanonPath (canonPath)
import System.FilePath (isAbsolute, isPathSeparator, searchPathSeparator, splitDrive, takeDirectory, (</>))

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
-- Where the files are
-- ---------------------------------------------------------------------------

-- | The environment that decides which files are read.  The two variables
-- are carried raw, as 'System.Environment.lookupEnv' returns them,
-- because upstream reads them differently: an empty @NIX_CONF_DIR@ is
-- unset (@getEnvNonEmpty@), while an empty @NIX_USER_CONF_FILES@ is set
-- and names no files at all.  The directories arrive resolved, since the
-- platform defaults behind them (where the machine-wide config directory
-- is, what the XDG config home and dirs fall back to) are the caller's to
-- look up.
data ConfigLocations = ConfigLocations
  { -- | @NIX_CONF_DIR@.
    clConfDir :: !(Maybe String),
    -- | The platform's machine-wide config directory, where the system
    -- file is unless @NIX_CONF_DIR@ moves it; 'Nothing' on a platform
    -- that names none, and then there is no system file to read.
    clSystemConfDir :: !(Maybe FilePath),
    -- | @NIX_USER_CONF_FILES@.
    clUserConfFiles :: !(Maybe String),
    -- | The XDG config home.
    clConfigHome :: !FilePath,
    -- | The XDG config dirs, first entry strongest.
    clConfigDirs :: ![FilePath]
  }
  deriving (Eq, Show)

-- | The config files to read, weakest first: the system file, under
-- @NIX_CONF_DIR@ when that is set and non-empty and otherwise under the
-- platform's machine-wide directory (no file at all when the platform has
-- none); then the user files in reverse of the order upstream lists them,
-- since @loadConfFile@ walks @nixUserConfFiles@ back to front
-- (globals.cc:139) so that the first listed, the config home, wins.
-- The system directory goes through 'canonicalAbsolutePath', as upstream's
-- @Settings@ constructor runs it through @canonPath@ (globals.cc:65): a
-- relative one is refused, and @.@ and @..@ in it collapse, so the file
-- is named canonically wherever it is reported and its includes resolve
-- against the collapsed directory.  The user files are read as named;
-- upstream canonicalizes none of them.
-- @NIX_USER_CONF_FILES@ replaces the XDG list outright, in its own order.
-- It is split on the platform's list separator with empty entries dropped
-- as upstream's tokenizer drops them; upstream splits on @:@ on every
-- platform, which cannot carry a Windows drive letter, so @;@ is honoured
-- there as it is for @PATH@ and @XDG_CONFIG_DIRS@.
configFilePaths :: ConfigLocations -> Either Text [FilePath]
configFilePaths locations = do
  systemDir <- traverse canonicalAbsolutePath (mfilter (not . null) (clConfDir locations) <|> clSystemConfDir locations)
  pure (maybeToList ((</> nixConfFileName) <$> systemDir) ++ reverse userFiles)
  where
    userFiles = case clUserConfFiles locations of
      Just listed -> splitListVar listed
      Nothing -> map underXdgDir (clConfigHome locations : clConfigDirs locations)
    underXdgDir dir = dir </> nixConfDirName </> nixConfFileName

-- | Split a path-list variable on the platform's list separator, dropping
-- empty entries.
splitListVar :: String -> [FilePath]
splitListVar = filter (not . null) . splitOn
  where
    splitOn text = case break (== searchPathSeparator) text of
      (item, []) -> [item]
      (item, _ : rest) -> item : splitOn rest

-- | The file name every source directory is read under.
nixConfFileName :: FilePath
nixConfFileName = "nix.conf"

-- | The directory @nix.conf@ sits in beneath a config directory: @nix@
-- under each XDG directory (@getUserConfigFiles@) and under @sysconfdir@
-- for the system file (libstore's meson.build at 2.24.9).
nixConfDirName :: FilePath
nixConfDirName = "nix"

-- ---------------------------------------------------------------------------
-- Sources and includes
-- ---------------------------------------------------------------------------

-- | One source of config text and the name it is reported under: a file's
-- path, or 'nixConfigSourceName' for the environment variable.  The name
-- also anchors relative includes, so a source that is not a file cannot
-- include relatively; upstream passes the literal @NIX_CONFIG@ as the
-- path and its @canonPath@ refuses the @.\/x@ that results.
data ConfigSource = ConfigSource
  { csName :: !FilePath,
    csText :: !Text
  }
  deriving (Eq, Show)

-- | The name upstream applies the @NIX_CONFIG@ text under.
nixConfigSourceName :: FilePath
nixConfigSourceName = "NIX_CONFIG"

-- | Why a file could not be read: there is nothing at the path, or there
-- is and it cannot be read (a directory, a permission refusal).  Upstream
-- tells the two apart with @pathExists@ before @readFile@; here the
-- distinction decides only how a failed @include@ is reported.
data ConfigReadFailure = ConfigFileMissing | ConfigFileUnreadable
  deriving (Eq, Show)

-- | How the expander reads a file: its text, or why it could not be read.
-- A function so the expander runs over an in-memory map in tests and over
-- the filesystem in the CLI.
type ReadConfigFile m = FilePath -> m (Either ConfigReadFailure Text)

-- | Read, expand and fold the whole configuration: the files at the given
-- paths, weakest first (one that cannot be read is simply absent, as
-- upstream's @applyConfigFile@ treats a @SystemError@), then the
-- @NIX_CONFIG@ text when the variable is set.  A file that is read and
-- does not expand is an error, never treated as absent.
loadConfig :: (Monad m) => ReadConfigFile m -> [FilePath] -> Maybe Text -> m (Either Text NixConfig)
loadConfig readConfigFile paths nixConfigText = runExceptT $ do
  fileSources <- lift (rights <$> traverse readSource paths)
  let sources = fileSources ++ maybeToList (ConfigSource nixConfigSourceName <$> nixConfigText)
  assignments <- traverse (ExceptT . expandConfigSource readConfigFile) sources
  liftEither (resolveConfig (concat assignments))
  where
    readSource path = fmap (ConfigSource path) <$> readConfigFile path

-- | Expand one source into its assignments, every include spliced in at
-- its line, as upstream's @parseConfigFiles@ pushes an included file's
-- pairs into the same list.  An include whose resolved path is already
-- being expanded is refused: upstream recurses until the stack overflows,
-- and no cycle can mean anything.
expandConfigSource :: (Monad m) => ReadConfigFile m -> ConfigSource -> m (Either Text [ConfigAssignment])
expandConfigSource readConfigFile = runExceptT . expandWithin readConfigFile []

-- | Expand one source beneath the names of the files including it.
expandWithin :: (Monad m) => ReadConfigFile m -> [FilePath] -> ConfigSource -> ExceptT Text m [ConfigAssignment]
expandWithin readConfigFile ancestors (ConfigSource name text) = do
  parsed <- liftEither (parseConfigLines name text)
  concat <$> traverse (expandLine readConfigFile (name :| ancestors)) parsed

-- | Expand one line of the file at the chain's head: an assignment is
-- itself; an include is the included file, expanded beneath the chain.
expandLine :: (Monad m) => ReadConfigFile m -> NonEmpty FilePath -> ConfigLine -> ExceptT Text m [ConfigAssignment]
expandLine _ _ (LineAssignment assignment) = pure [assignment]
expandLine readConfigFile chain@(from :| _) (LineInclude mode target) = do
  path <- liftEither (resolveIncludePath from target)
  when (path `elem` chain) (throwError (includeCycle path from))
  contents <- lift (readConfigFile path)
  case (contents, mode) of
    (Right text, _) -> expandWithin readConfigFile (NE.toList chain) (ConfigSource path text)
    (Left _, IncludeOptional) -> pure []
    (Left failure, IncludeRequired) -> throwError (includeFailed failure path from)

-- | Where an include points: the target joined under the including
-- file's directory (an absolute target stands alone), then collapsed
-- through 'canonicalAbsolutePath', as upstream's @absPath@ then
-- @canonPath@ do.  A result that is still relative is refused: the
-- including source has no directory, which is the case for @NIX_CONFIG@
-- and for a user file named relatively.
resolveIncludePath :: FilePath -> FilePath -> Either Text FilePath
resolveIncludePath from target = canonicalAbsolutePath (takeDirectory from </> target)

-- | An absolute native path collapsed lexically, or upstream's own
-- complaint (@canonPath@, file-system.cc at 2.24.9) for a relative one.
-- The drive 'splitDrive' finds (the POSIX root, a Windows drive letter or
-- UNC server) is kept as spelled, and the path beneath it collapses as
-- upstream's @canonPath@ collapses: @.@ drops, @..@ pops and drops at the
-- root, repeated separators fold to one.  'canonPath' does that collapse
-- for eval path values, which are rooted in the @\/nix\/store@ sense on
-- every platform and so see no drive; a native path is split here first
-- so a UNC root survives rather than folding to the current drive.
canonicalAbsolutePath :: FilePath -> Either Text FilePath
canonicalAbsolutePath path
  | isAbsolute path = Right (T.unpack (root <> canonPath (T.takeEnd 1 rootSeparators <> T.pack below)))
  | otherwise = Left (configErrorPrefix <> "not an absolute path: '" <> T.pack path <> "'")
  where
    (drive, below) = splitDrive path
    (root, rootSeparators) = (T.dropWhileEnd isPathSeparator driveText, T.takeWhileEnd isPathSeparator driveText)
    driveText = T.pack drive

includeFailed :: ConfigReadFailure -> FilePath -> FilePath -> Text
includeFailed failure path from =
  configErrorPrefix <> "file '" <> T.pack path <> "' included from '" <> T.pack from <> "' " <> reason
  where
    reason = case failure of
      ConfigFileMissing -> "not found"
      ConfigFileUnreadable -> "cannot be read"

includeCycle :: FilePath -> FilePath -> Text
includeCycle path from =
  configErrorPrefix <> "file '" <> T.pack path <> "' included from '" <> T.pack from <> "' is already being included (include cycle)"

-- ---------------------------------------------------------------------------
-- Parsing
-- ---------------------------------------------------------------------------

-- | One line of a config source after comment truncation: an assignment,
-- or an include directive and the path it names.
data ConfigLine
  = LineAssignment !ConfigAssignment
  | LineInclude !IncludeMode !FilePath
  deriving (Eq, Show)

-- | @include@ fails when its file cannot be read; @!include@ goes on
-- without it.  That is the only difference between the two directives.
data IncludeMode = IncludeRequired | IncludeOptional
  deriving (Eq, Show)

-- | One parsed @name = value@ assignment, before its name is resolved
-- against the known settings and aliases.
data ConfigAssignment = ConfigAssignment
  { caName :: !Text,
    caValue :: !Text
  }
  deriving (Eq, Show)

-- | Parse one source's text into its lines.  Comments and blank lines
-- drop out; a malformed line (fewer than @name = value@, a missing @=@,
-- or an include with other than exactly one path) is a loud error rather
-- than a silent skip, matching upstream's @UsageError@ - a typo in a
-- security-relevant file must not pass for an empty setting.  The error
-- quotes the line as upstream does, comment-truncated and otherwise
-- verbatim, leading whitespace included.
parseConfigLines :: FilePath -> Text -> Either Text [ConfigLine]
parseConfigLines name = traverse parseLine . filter (not . isBlank) . map stripComment . T.lines
  where
    isBlank line = null (T.words line)
    stripComment = T.takeWhile (/= '#')
    parseLine line = case T.words line of
      [directive, target]
        | directive == includeDirective -> Right (LineInclude IncludeRequired (T.unpack target))
        | directive == bangIncludeDirective -> Right (LineInclude IncludeOptional (T.unpack target))
      (directive : _)
        | directive == includeDirective || directive == bangIncludeDirective -> syntaxError line
      (key : eq : valueTokens)
        | eq == assignEq -> Right (LineAssignment (ConfigAssignment key (T.unwords valueTokens)))
      _ -> syntaxError line
    syntaxError line =
      Left (configErrorPrefix <> "syntax error in configuration line '" <> line <> "' in '" <> T.pack name <> "'")

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

-- | Fold expanded assignments onto the default config, weakest first.
-- The caller orders them so the strongest source's assignments are last
-- (and so win).  A value a setting cannot take aborts the fold, as
-- upstream's @UsageError@ does.
resolveConfig :: [ConfigAssignment] -> Either Text NixConfig
resolveConfig = foldM applyAssignment defaultNixConfig

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

-- | What every error about the files themselves opens with (a syntax
-- error, a failed include, a path that is not absolute): the format's
-- name.  A setting's value error carries none, as upstream's does not.
configErrorPrefix :: Text
configErrorPrefix = "nix.conf: "
