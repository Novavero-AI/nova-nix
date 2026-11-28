-- | Nix configuration: the @nix.conf@ / @NIX_CONFIG@ settings that decide
-- which binary caches a machine substitutes from, which public keys it
-- trusts, whether a failed substitute builds from source (@fallback@),
-- whether a derivation may forbid substitutes
-- (@always-allow-substitutes@), how many cores a builder is told it may
-- use (@cores@), how deep function calls may nest, how far an evaluation
-- may reach (@restrict-eval@, @pure-eval@, @allowed-uris@), whether
-- @builtins.traceVerbose@ traces (@trace-verbose@), whether a path
-- reader may build what its argument's context names
-- (@allow-import-from-derivation@, @trace-import-from-derivation@), how
-- builds are sandboxed (@sandbox@ and the settings beside it, macOS's
-- @allowed-impure-host-deps@ and @darwin-log-sandbox-violations@ among
-- them), what a build that fails leaves (@keep-failed@), where the link to
-- the default profile is
-- (@use-xdg-base-directories@), the prompt of a @develop@ shell
-- (@bash-prompt@ and its prefix and suffix), and which experimental
-- features are on (@experimental-features@).
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
-- whitespace tokenizing, inline include expansion), matched here.  A
-- Boolean value is one of @true@, @yes@, @1@, @false@, @no@, @0@
-- (upstream's @BaseSetting\<bool\>::parse@), and any other spelling is
-- an error.  An integer value is upstream's too ('parseUnsignedSetting'):
-- decimal digits, an optional leading @+@, and an optional binary unit
-- suffix (@K@, @M@, @G@, @T@).
--
-- == Bytes
--
-- A source is bytes until a setting takes a value from it, as upstream's
-- @std::string@ is.  A line splits into tokens on upstream's separators
-- alone (space, tab, CR, LF), not on every Unicode space; a comment, and
-- the value of a setting this layer does not model, are never decoded;
-- and an include target reaches the open as the bytes the line holds
-- ("Nix.HostPath"), so a target with no UTF-8 reading names the file it
-- names on disk.  A Boolean or integer setting compares the value's bytes
-- with what it accepts, so a byte with no UTF-8 reading there is
-- upstream's own invalid-value error.  A list setting's tokens are
-- decoded strictly where upstream keeps them as bytes, because everything
-- downstream holds them as text (a substituter's URL, a key matched
-- against a narinfo's signatures, a URI prefix matched against the
-- evaluator's URIs).  A token that is not UTF-8 is refused rather than
-- spelled with U+FFFD, which would name a different cache, key or URI.
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
-- prefixed assignment APPENDS to a list or a set.  An @extra-@ on a Boolean or a
-- scalar names no setting (upstream appends only to an appendable
-- setting, so @extra-max-call-depth@ is an unknown name) and is ignored
-- like any unknown name.  'loadConfig' folds over the built-in default
-- the files, in the order the caller lists them, then @NIX_CONFIG@, so
-- @NIX_CONFIG@ overrides every file, as upstream Nix orders them.  The
-- command line is not a source here: the CLI applies its flags over the
-- resolved settings.
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
    PathSetValue (..),
    DefaultUse (..),

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
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (isDigit, ord, toUpper)
import Data.Either (rights)
import Data.List (dropWhileEnd, intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, fromMaybe, maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word32)
import Nix.Builder.Sandbox (SandboxMode, parseSandboxMode)
import Nix.Eval.CallDepth (defaultMaxCallDepth)
import Nix.ExperimentalFeature (FeatureSet, addFeatures, noFeatures, parseFeatures, replaceFeatures)
import Nix.HostPath (hostPathFromBytes, hostPathText)
import System.OsPath (OsChar, OsPath, OsString, (</>))
import qualified System.OsPath as OP

-- ---------------------------------------------------------------------------
-- Settings
-- ---------------------------------------------------------------------------

-- | The subset of Nix settings that this layer resolves.  The lists are
-- ordered (upstream's @substituters@, @trusted-public-keys@ and
-- @allowed-uris@ are each a @Strings@); order is preserved on write, though
-- it does not affect key acceptance (any trusted key is enough) or URI
-- admission (any prefix is enough).
data NixConfig = NixConfig
  { ncSubstituters :: ![Text],
    ncTrustedPublicKeys :: ![Text],
    -- | Upstream @fallback@: build from source when a substitute fails.
    ncFallback :: !Bool,
    -- | Upstream @always-allow-substitutes@: substitute a derivation
    -- whatever its @allowSubstitutes@ attribute says.
    ncAlwaysAllowSubstitutes :: !Bool,
    -- | Upstream's @cores@, what a builder's @NIX_BUILD_CORES@ says, when
    -- a source sets it.  Its default is the machine's
    -- ("Nix.Builder.Cores"), which only IO can find, so unset is
    -- 'Nothing' rather than a number.
    ncCores :: !(Maybe Word32),
    -- | Upstream's @max-call-depth@: how many function calls may be
    -- active around a new one before evaluation refuses it.
    ncMaxCallDepth :: !Word32,
    -- | URI prefixes a fetch may reach under @restrict-eval@.
    ncAllowedUris :: ![Text],
    -- | Upstream @restrict-eval@.
    ncRestrictEval :: !Bool,
    -- | Upstream @pure-eval@.
    ncPureEval :: !Bool,
    -- | Upstream @trace-verbose@.
    ncTraceVerbose :: !Bool,
    -- | Upstream @allow-import-from-derivation@.
    ncAllowImportFromDerivation :: !Bool,
    -- | Upstream @trace-import-from-derivation@.
    ncTraceImportFromDerivation :: !Bool,
    -- | Upstream's @sandbox@, when a source sets it.  Its default is
    -- @true@ on Linux and @false@ elsewhere, the caller's to apply.
    ncSandbox :: !(Maybe SandboxMode),
    -- | Upstream @sandbox-fallback@.
    ncSandboxFallback :: !Bool,
    -- | Upstream @sandbox-paths@, as bytes.
    ncSandboxPaths :: !PathSetValue,
    -- | Upstream @sandbox-build-dir@.
    ncSandboxBuildDir :: !ByteString,
    -- | Upstream @sandbox-dev-shm-size@.
    ncSandboxDevShmSize :: !ByteString,
    -- | Upstream's @require-drop-supplementary-groups@, when a source sets
    -- it.  Its default is whether the process runs as root, which only IO
    -- can find.
    ncRequireDropSupplementaryGroups :: !(Maybe Bool),
    -- | Upstream's @ssl-cert-file@, when a source sets it.  Its default
    -- comes from the environment and the files there are, which only IO
    -- can find.
    ncSslCertFile :: !(Maybe ByteString),
    -- | Upstream @use-xdg-base-directories@: the link to the default
    -- profile is in the state directory, not the home.
    ncUseXdgBaseDirectories :: !Bool,
    -- | Upstream's @bash-prompt@: a @develop@ shell's prompt in place of
    -- the user's, none when empty.
    ncBashPrompt :: !ByteString,
    -- | Upstream's @bash-prompt-prefix@: text before that prompt.
    ncBashPromptPrefix :: !ByteString,
    -- | Upstream's @bash-prompt-suffix@: text after it.
    ncBashPromptSuffix :: !ByteString,
    -- | Upstream's @allowed-impure-host-deps@, as bytes.  Its default is
    -- macOS's alone, the caller's to apply.
    ncAllowedImpureHostDeps :: !PathSetValue,
    -- | Upstream's @darwin-log-sandbox-violations@.
    ncDarwinLogSandboxViolations :: !Bool,
    -- | Upstream's @keep-failed@: a build that fails keeps its directory,
    -- and a check its rebuild of an output that differs.
    ncKeepFailed :: !Bool,
    -- | Upstream's @experimental-features@, with the warnings its values
    -- gave, which the caller reports.
    ncExperimentalFeatures :: !FeatureSet,
    -- | Upstream's @flake-registry@: the path or URL of the global flake
    -- registry, none when empty.
    ncFlakeRegistry :: !ByteString
  }
  deriving (Eq, Show)

-- | A set of paths whose default is the caller's: upstream's
-- @sandbox-paths@ defaults to the sandbox shell a binary was built with,
-- which is where it was installed, and on macOS to system paths, as its
-- @allowed-impure-host-deps@ does.  An @extra-@ assignment adds to the
-- default, and a plain one replaces it.
data PathSetValue = PathSetValue
  { psvDefault :: !DefaultUse,
    psvPaths :: !(Set ByteString)
  }
  deriving (Eq, Show)

-- | Whether a t'PathSetValue' keeps its default beside its own paths.
data DefaultUse = KeepsDefault | ReplacesDefault
  deriving (Eq, Show)

-- | The baseline the fold starts from: no substituters, no trusted keys,
-- no fallback, @allowSubstitutes@ heeded, @cores@ unset so that the
-- machine's default applies, upstream's call-depth ceiling, no allowed
-- URIs, both evaluation modes off, @trace-verbose@ off, an import from
-- derivation allowed and not traced, upstream's sandbox settings,
-- those whose defaults take the platform or IO unset or kept for the
-- caller, the default profile's link in the home, the three prompt
-- settings empty, as upstream's are, and nothing kept of a build that
-- fails.  The empty
-- substituter list is nova-nix's existing default (nothing is substituted
-- unless configured), a deliberate divergence from upstream's
-- cache.nixos.org default: turning a cache on for every machine is the
-- operator's decision to make in a config file, not a built-in.  The
-- other defaults are upstream's own.
defaultNixConfig :: NixConfig
defaultNixConfig =
  NixConfig
    { ncSubstituters = [],
      ncTrustedPublicKeys = [],
      ncFallback = False,
      ncAlwaysAllowSubstitutes = False,
      ncCores = Nothing,
      ncMaxCallDepth = defaultMaxCallDepth,
      ncAllowedUris = [],
      ncRestrictEval = False,
      ncPureEval = False,
      ncTraceVerbose = False,
      ncAllowImportFromDerivation = True,
      ncTraceImportFromDerivation = False,
      ncSandbox = Nothing,
      ncSandboxFallback = True,
      ncSandboxPaths = PathSetValue KeepsDefault Set.empty,
      ncSandboxBuildDir = "/build",
      ncSandboxDevShmSize = "50%",
      ncRequireDropSupplementaryGroups = Nothing,
      ncSslCertFile = Nothing,
      ncUseXdgBaseDirectories = False,
      ncBashPrompt = "",
      ncBashPromptPrefix = "",
      ncBashPromptSuffix = "",
      ncAllowedImpureHostDeps = PathSetValue KeepsDefault Set.empty,
      ncDarwinLogSandboxViolations = False,
      ncKeepFailed = False,
      ncExperimentalFeatures = noFeatures,
      ncFlakeRegistry = defaultFlakeRegistry
    }

-- ---------------------------------------------------------------------------
-- Where the files are
-- ---------------------------------------------------------------------------

-- | The environment that decides which files are read.  The two variables
-- are carried raw, as the bytes 'Nix.Environment.lookupEnvBytes' returns,
-- because upstream reads them differently: an empty @NIX_CONF_DIR@ is
-- unset (@getEnvNonEmpty@), while an empty @NIX_USER_CONF_FILES@ is set
-- and names no files at all.  The directories arrive resolved, since the
-- platform defaults behind them (where the machine-wide config directory
-- is, what the XDG config home and dirs fall back to) are the caller's to
-- look up.
data ConfigLocations = ConfigLocations
  { -- | @NIX_CONF_DIR@.
    clConfDir :: !(Maybe ByteString),
    -- | The platform's machine-wide config directory, where the system
    -- file is unless @NIX_CONF_DIR@ moves it; 'Nothing' on a platform
    -- that names none, and then there is no system file to read.
    clSystemConfDir :: !(Maybe OsPath),
    -- | @NIX_USER_CONF_FILES@.
    clUserConfFiles :: !(Maybe ByteString),
    -- | The XDG config home.
    clConfigHome :: !OsPath,
    -- | The XDG config dirs, first entry strongest.
    clConfigDirs :: ![OsPath]
  }
  deriving (Eq, Show)

-- | The config files to read, weakest first: the system file, under
-- @NIX_CONF_DIR@ when that is set and non-empty and otherwise under the
-- platform's machine-wide directory (no file at all when the platform has
-- none); then the user files in reverse of the order upstream lists them,
-- since @loadConfFile@ walks @nixUserConfFiles@ back to front
-- (globals.cc) so that the first listed, the config home, wins.  The system
-- directory goes through 'canonicalAbsolutePath', as upstream's @Settings@
-- constructor runs it through @canonPath@ (globals.cc): a relative one is
-- refused, and @.@ and @..@ in it collapse, so the file is named canonically
-- wherever it is reported and its includes resolve against the collapsed
-- directory.  The user files are read as named; upstream canonicalizes none
-- of them.  @NIX_USER_CONF_FILES@ replaces the XDG list outright, in its own
-- order.  It is split on the platform's list separator with empty entries
-- dropped as upstream's tokenizer drops them; upstream splits on @:@ on every
-- platform, which cannot carry a Windows drive letter, so @;@ is honoured
-- there as it is for @PATH@ and @XDG_CONFIG_DIRS@.
configFilePaths :: ConfigLocations -> Either Text [OsPath]
configFilePaths locations = do
  confDir <- traverse variablePath (mfilter (not . BS.null) (clConfDir locations))
  systemDir <- traverse canonicalAbsolutePath (confDir <|> clSystemConfDir locations)
  userFiles <- case clUserConfFiles locations of
    Just listed -> splitListVar <$> variablePath listed
    Nothing -> pure (map underXdgDir (clConfigHome locations : clConfigDirs locations))
  pure (maybeToList ((</> nixConfFileName) <$> systemDir) ++ reverse userFiles)
  where
    underXdgDir dir = dir </> nixConfDirName </> nixConfFileName
    variablePath bytes = maybe (Left (noHostSpelling bytes)) Right (hostPathFromBytes bytes)

-- | Split a path-list variable on the platform's list separator, dropping
-- empty entries.
splitListVar :: OsString -> [OsPath]
splitListVar = map OP.pack . filter (not . null) . splitOn . OP.unpack
  where
    splitOn units = case break (== OP.searchPathSeparator) units of
      (item, []) -> [item]
      (item, _ : rest) -> item : splitOn rest

-- | The file name every source directory is read under.
nixConfFileName :: OsPath
nixConfFileName = asciiPath "nix.conf"

-- | The directory @nix.conf@ sits in beneath a config directory: @nix@
-- under each XDG directory (@getUserConfigFiles@) and under @sysconfdir@
-- for the system file (libstore's meson.build at 2.24.9).
nixConfDirName :: OsPath
nixConfDirName = asciiPath "nix"

-- | A path this module spells itself.  'OP.unsafeFromChar' narrows a
-- character to the platform's path unit (a byte, or a UTF-16 unit), which
-- is exact for ASCII on both.
asciiPath :: String -> OsPath
asciiPath = OP.pack . map OP.unsafeFromChar

-- ---------------------------------------------------------------------------
-- Sources and includes
-- ---------------------------------------------------------------------------

-- | One source of config text and the name it is reported under: a file's
-- path, or 'nixConfigSourceName' for the environment variable.  The name
-- also anchors relative includes, so a source that is not a file cannot
-- include relatively; upstream passes the literal @NIX_CONFIG@ as the
-- path and its @canonPath@ refuses the @.\/x@ that results.
data ConfigSource = ConfigSource
  { csName :: !OsPath,
    csText :: !ByteString
  }
  deriving (Eq, Show)

-- | The name upstream applies the @NIX_CONFIG@ text under.
nixConfigSourceName :: OsPath
nixConfigSourceName = asciiPath "NIX_CONFIG"

-- | Why a file could not be read: there is nothing at the path, or there
-- is and it cannot be read (a directory, a permission refusal).  Upstream
-- tells the two apart with @pathExists@ before @readFile@; here the
-- distinction decides only how a failed @include@ is reported.
data ConfigReadFailure = ConfigFileMissing | ConfigFileUnreadable
  deriving (Eq, Show)

-- | How the expander reads a file: its bytes, or why it could not be
-- read.  A function so the expander runs over an in-memory map in tests
-- and over the filesystem in the CLI.
type ReadConfigFile m = OsPath -> m (Either ConfigReadFailure ByteString)

-- | Read, expand and fold the whole configuration: the files at the given
-- paths, weakest first (one that cannot be read is simply absent, as
-- upstream's @applyConfigFile@ treats a @SystemError@), then the
-- @NIX_CONFIG@ bytes when the variable is set.  A file that is read and
-- does not expand is an error, never treated as absent.
loadConfig :: (Monad m) => ReadConfigFile m -> [OsPath] -> Maybe ByteString -> m (Either Text NixConfig)
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
expandWithin :: (Monad m) => ReadConfigFile m -> [OsPath] -> ConfigSource -> ExceptT Text m [ConfigAssignment]
expandWithin readConfigFile ancestors (ConfigSource name text) = do
  parsed <- liftEither (parseConfigLines name text)
  concat <$> traverse (expandLine readConfigFile (name :| ancestors)) parsed

-- | Expand one line of the file at the chain's head: an assignment is
-- itself; an include is the included file, expanded beneath the chain.
-- A target with no host spelling (bytes that are not UTF-8, on Windows)
-- names no file that can exist, so @!include@ goes on without it as it
-- does without a missing file.
expandLine :: (Monad m) => ReadConfigFile m -> NonEmpty OsPath -> ConfigLine -> ExceptT Text m [ConfigAssignment]
expandLine _ _ (LineAssignment assignment) = pure [assignment]
expandLine readConfigFile chain@(from :| _) (LineInclude mode target) =
  case (hostPathFromBytes target, mode) of
    (Nothing, IncludeOptional) -> pure []
    (Nothing, IncludeRequired) -> throwError (includeUnspellable target from)
    (Just targetPath, _) -> do
      path <- liftEither (resolveIncludePath from targetPath)
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
resolveIncludePath :: OsPath -> OsPath -> Either Text OsPath
resolveIncludePath from target = canonicalAbsolutePath (OP.takeDirectory from </> target)

-- | An absolute native path collapsed lexically, or upstream's own
-- complaint (@canonPath@, file-system.cc at 2.24.9) for a relative one.
-- The drive 'OP.splitDrive' finds (the POSIX root, a Windows drive letter
-- or UNC server) is kept as spelled, and the path beneath it collapses as
-- upstream's @canonPath@ collapses ('collapseUnits'), from the drive's
-- last separator on, so a UNC root survives rather than folding to the
-- current drive.
canonicalAbsolutePath :: OsPath -> Either Text OsPath
canonicalAbsolutePath path
  | OP.isAbsolute path = Right (OP.pack (root ++ collapseUnits (lastSeparator ++ OP.unpack below)))
  | otherwise = Left (configErrorPrefix <> "not an absolute path: '" <> hostPathText path <> "'")
  where
    (drive, below) = OP.splitDrive path
    driveUnits = OP.unpack drive
    root = dropWhileEnd OP.isPathSeparator driveUnits
    lastSeparator = take 1 (takeWhile OP.isPathSeparator (reverse driveUnits))

-- | Upstream's lexical collapse, over a native path's units: @.@ drops,
-- @..@ pops a real predecessor (at a root it drops, in a relative path it
-- stays), repeated separators fold to one, and an empty result is @.@.
-- The same rule 'Nix.Eval.CanonPath.canonPath' applies to path values,
-- which are text; a native path here is bytes, which text cannot hold.
-- The separator is the platform's when the path spells one with it and
-- @/@ otherwise, as there.
collapseUnits :: [OsChar] -> [OsChar]
collapseUnits units
  | rooted = separator : joined
  | null resolved = [dotUnit]
  | otherwise = joined
  where
    rooted = any OP.isPathSeparator (take 1 units)
    segments = filter (not . null) (splitOnSeparators units)
    resolved = reverse (foldl' (collapseStep rooted) [] segments)
    separator = if OP.pathSeparator `elem` units then OP.pathSeparator else slashUnit
    joined = intercalate [separator] resolved

-- | One segment of the collapse fold; the accumulator holds resolved
-- segments in reverse.
collapseStep :: Bool -> [[OsChar]] -> [OsChar] -> [[OsChar]]
collapseStep rooted acc segment
  | segment == [dotUnit] = acc
  | segment == parent = case acc of
      [] -> [parent | not rooted]
      (top : rest)
        | top == parent -> parent : acc
        | otherwise -> rest
  | otherwise = segment : acc
  where
    parent = [dotUnit, dotUnit]

splitOnSeparators :: [OsChar] -> [[OsChar]]
splitOnSeparators units = case break OP.isPathSeparator units of
  (segment, []) -> [segment]
  (segment, _ : rest) -> segment : splitOnSeparators rest

dotUnit, slashUnit :: OsChar
dotUnit = OP.unsafeFromChar '.'
slashUnit = OP.unsafeFromChar '/'

includeFailed :: ConfigReadFailure -> OsPath -> OsPath -> Text
includeFailed failure path from =
  configErrorPrefix <> "file '" <> hostPathText path <> "' included from '" <> hostPathText from <> "' " <> reason
  where
    reason = case failure of
      ConfigFileMissing -> "not found"
      ConfigFileUnreadable -> "cannot be read"

includeCycle :: OsPath -> OsPath -> Text
includeCycle path from =
  configErrorPrefix <> "file '" <> hostPathText path <> "' included from '" <> hostPathText from <> "' is already being included (include cycle)"

includeUnspellable :: ByteString -> OsPath -> Text
includeUnspellable target from =
  configErrorPrefix <> "file '" <> shownBytes target <> "' included from '" <> hostPathText from <> "' " <> noWindowsSpelling

-- | The refusal of a path variable with no host spelling.  Unreachable
-- while the environment reaches this module as "Nix.Environment" reads
-- it, which spells a Windows value as UTF-8.
noHostSpelling :: ByteString -> Text
noHostSpelling bytes = configErrorPrefix <> "path '" <> shownBytes bytes <> "' " <> noWindowsSpelling

noWindowsSpelling :: Text
noWindowsSpelling = "is not valid UTF-8, so it names no Windows file"

-- ---------------------------------------------------------------------------
-- Parsing
-- ---------------------------------------------------------------------------

-- | One line of a config source after comment truncation: an assignment,
-- or an include directive and the bytes of the path it names.
data ConfigLine
  = LineAssignment !ConfigAssignment
  | LineInclude !IncludeMode !ByteString
  deriving (Eq, Show)

-- | @include@ fails when its file cannot be read; @!include@ goes on
-- without it.  That is the only difference between the two directives.
data IncludeMode = IncludeRequired | IncludeOptional
  deriving (Eq, Show)

-- | One parsed @name = value@ assignment, before its name is resolved
-- against the known settings and aliases.  The value is its tokens
-- joined by single spaces, as upstream joins them.
data ConfigAssignment = ConfigAssignment
  { caName :: !ByteString,
    caValue :: !ByteString
  }
  deriving (Eq, Show)

-- | Parse one source's bytes into its lines.  Comments and blank lines
-- drop out; a malformed line (fewer than @name =@, a missing @=@,
-- or an include with other than exactly one path) is a loud error rather
-- than a silent skip, as upstream Nix makes it - a typo in a
-- security-relevant file must not pass for an empty setting.  The error
-- quotes the line as upstream does, comment-truncated and otherwise
-- verbatim, leading whitespace included.
parseConfigLines :: OsPath -> ByteString -> Either Text [ConfigLine]
parseConfigLines name = fmap catMaybes . traverse (parseLine . BS8.takeWhile (/= commentChar)) . BS8.lines
  where
    parseLine line = case configTokens line of
      [] -> Right Nothing
      [directive, target]
        | directive == includeDirective -> Right (Just (LineInclude IncludeRequired target))
        | directive == bangIncludeDirective -> Right (Just (LineInclude IncludeOptional target))
      (directive : _)
        | directive == includeDirective || directive == bangIncludeDirective -> syntaxError line
      (key : eq : valueTokens)
        | eq == assignEq -> Right (Just (LineAssignment (ConfigAssignment key (BS.intercalate valueSeparator valueTokens))))
      _ -> syntaxError line
    syntaxError line =
      Left (configErrorPrefix <> "syntax error in configuration line '" <> shownBytes line <> "' in '" <> hostPathText name <> "'")

-- | Upstream's @tokenizeString@ under its default separators (util.hh at
-- 2.24.9): the maximal runs of bytes other than space, tab, CR and LF.
configTokens :: ByteString -> [ByteString]
configTokens = filter (not . BS.null) . BS8.splitWith (`BS8.elem` tokenSeparators)

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
parseUnsignedSetting :: ByteString -> ByteString -> Either Text Word32
parseUnsignedSetting name value = maybe (Left invalid) Right (unitValue >>= inRange)
  where
    invalid = "setting '" <> shownBytes name <> "' has invalid value '" <> shownBytes value <> "'"
    unsigned = fromMaybe value (BS.stripPrefix plusSign value)
    unitValue = case BS8.unsnoc unsigned of
      Just (digits, unit) | Just multiplier <- unitMultiplier unit -> (* multiplier) <$> decimalInteger digits
      _ -> decimalInteger unsigned
    inRange n
      | n <= toInteger (maxBound :: Word32) = Just (fromInteger n)
      | otherwise = Nothing

-- | A whole run of decimal digits as an 'Integer'; anything else, the
-- empty run included, is 'Nothing'.
decimalInteger :: ByteString -> Maybe Integer
decimalInteger digits
  | not (BS.null digits) && BS8.all isDigit digits = Just (BS8.foldl' step 0 digits)
  | otherwise = Nothing
  where
    step acc digit = acc * 10 + toInteger (ord digit - ord '0')

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
-- @binary-cache-public-keys@ -> @trusted-public-keys@,
-- @build-fallback@ -> @fallback@, @build-cores@ -> @cores@,
-- @build-use-chroot@ and @build-use-sandbox@ -> @sandbox@,
-- @build-chroot-dirs@ and @build-sandbox-paths@ -> @sandbox-paths@) and the
-- @extra-@ prefix (append rather than replace, list and path-set settings
-- only).  A name
-- outside the known set is ignored, not an error: upstream warns and
-- continues, and refusing every unknown key would reject a config that
-- also carries settings this layer does not model.  A value that does
-- not parse for its setting is an error, as it is in upstream Nix:
-- @pure-eval = ture@ must not pass for off.
applyAssignment :: NixConfig -> ConfigAssignment -> Either Text NixConfig
applyAssignment config (ConfigAssignment name value) =
  case resolveName name of
    Nothing -> Right config
    Just (ListSetting field, mode) ->
      (\items -> setList field (combine mode (getList field config) items) config) <$> listItems field value
    -- Upstream's Boolean and integer errors name the setting by its own
    -- name, not the alias it was set through (@BaseSetting\<T\>::parse@
    -- throws with @name@, the setting's, which @Config::set@ reaches
    -- through an alias's entry, config.cc at 2.24.9).
    Just (BoolSetting field, _) ->
      (\flag -> setBool field flag config) <$> parseBool (boolFieldKey field) value
    Just (ScalarSetting field, _) ->
      (\number -> setScalar field number config) <$> parseUnsignedSetting (scalarFieldKey field) value
    Just (ModeSetting, _) ->
      (\mode -> config {ncSandbox = Just mode}) <$> parseSandboxMode value
    Just (BytesSetting field, _) -> Right (setBytes field value config)
    Just (PathSetSetting field, mode) ->
      Right (setPathSet field (combinePaths mode (getPathSet field config) (Set.fromList (configTokens value))) config)
    Just (FeaturesSetting, mode) ->
      Right (config {ncExperimentalFeatures = combineFeatures mode (ncExperimentalFeatures config) (parseFeatures value)})
  where
    combine ReplaceMode _ new = new
    combine AppendMode old new = old ++ new
    -- Upstream's @StringSet@ setting: a plain assignment replaces the
    -- set, the default included, and @extra-@ adds to it
    -- (its @parse@ and @appendOrSet@ in config.cc at 2.24.9,
    -- configuration.cc at 2.36.0).
    combinePaths ReplaceMode _ new = PathSetValue ReplacesDefault new
    combinePaths AppendMode old new = old {psvPaths = Set.union (psvPaths old) new}
    -- Upstream's set of features: a plain assignment replaces it and
    -- @extra-@ adds to it (@appendOrSet@ in configuration.cc at 2.36.0).
    combineFeatures ReplaceMode = replaceFeatures
    combineFeatures AppendMode = addFeatures

-- | A list value's tokens as text, split as upstream's @Strings@ setting
-- splits it, or the refusal of a token that is not UTF-8 (see the module
-- header's Bytes section).  The refusal names the setting as upstream's
-- value errors do, by its own name rather than an alias or an @extra-@
-- spelling.
listItems :: ListField -> ByteString -> Either Text [Text]
listItems field value = maybe (Left refusal) Right (traverse decoded (configTokens value))
  where
    decoded = either (const Nothing) Just . TE.decodeUtf8'
    refusal = "setting '" <> shownBytes (listFieldKey field) <> "' has invalid value '" <> shownBytes value <> "': not valid UTF-8"

-- | Which setting a name targets, once the @extra-@ prefix and the aliases
-- are resolved, and whether it replaces or appends.  @extra-@ composes
-- with a list or a path set only: upstream looks the base name up and appends
-- when the setting is appendable, and a Boolean or a scalar is not.
resolveName :: ByteString -> Maybe (Setting, ApplyMode)
resolveName name =
  case BS.stripPrefix extraPrefix name of
    Just base -> case baseSetting base of
      Just setting@(ListSetting _) -> Just (setting, AppendMode)
      Just setting@(PathSetSetting _) -> Just (setting, AppendMode)
      Just FeaturesSetting -> Just (FeaturesSetting, AppendMode)
      _ -> Nothing
    Nothing -> case baseSetting name of
      Just setting -> Just (setting, ReplaceMode)
      Nothing -> Nothing
  where
    baseSetting n
      | n == substitutersKey || n == substitutersAlias = Just (ListSetting SubstitutersField)
      | n == trustedKeysKey || n == trustedKeysAlias = Just (ListSetting TrustedKeysField)
      | n == allowedUrisKey = Just (ListSetting AllowedUrisField)
      | n == fallbackKey || n == fallbackAlias = Just (BoolSetting FallbackField)
      | n == alwaysAllowSubstitutesKey = Just (BoolSetting AlwaysAllowSubstitutesField)
      | n == restrictEvalKey = Just (BoolSetting RestrictEvalField)
      | n == pureEvalKey = Just (BoolSetting PureEvalField)
      | n == traceVerboseKey = Just (BoolSetting TraceVerboseField)
      | n == allowImportFromDerivationKey = Just (BoolSetting AllowImportFromDerivationField)
      | n == traceImportFromDerivationKey = Just (BoolSetting TraceImportFromDerivationField)
      | n == maxCallDepthKey = Just (ScalarSetting MaxCallDepthField)
      | n == coresKey || n == coresAlias = Just (ScalarSetting CoresField)
      | n == sandboxKey || n `elem` sandboxAliases = Just ModeSetting
      | n == sandboxFallbackKey = Just (BoolSetting SandboxFallbackField)
      | n == sandboxPathsKey || n `elem` sandboxPathsAliases = Just (PathSetSetting SandboxPathsField)
      | n == allowedImpureHostDepsKey = Just (PathSetSetting AllowedImpureHostDepsField)
      | n == darwinLogSandboxViolationsKey = Just (BoolSetting DarwinLogSandboxViolationsField)
      | n == keepFailedKey = Just (BoolSetting KeepFailedField)
      | n == sandboxBuildDirKey = Just (BytesSetting SandboxBuildDirField)
      | n == sandboxDevShmSizeKey = Just (BytesSetting SandboxDevShmSizeField)
      | n == requireDropGroupsKey = Just (BoolSetting RequireDropGroupsField)
      | n == sslCertFileKey = Just (BytesSetting SslCertFileField)
      | n == useXdgBaseDirectoriesKey = Just (BoolSetting UseXdgBaseDirectoriesField)
      | n == bashPromptKey = Just (BytesSetting BashPromptField)
      | n == bashPromptPrefixKey = Just (BytesSetting BashPromptPrefixField)
      | n == bashPromptSuffixKey = Just (BytesSetting BashPromptSuffixField)
      | n == experimentalFeaturesKey = Just FeaturesSetting
      | n == flakeRegistryKey = Just (BytesSetting FlakeRegistryField)
      | otherwise = Nothing

-- | A setting this layer resolves, by the shape of its value: a list of
-- text, a Boolean, an unsigned integer, @sandbox@'s mode, a string kept as
-- bytes, a set of paths, or @experimental-features@' set of features.
data Setting
  = ListSetting !ListField
  | BoolSetting !BoolField
  | ScalarSetting !ScalarField
  | ModeSetting
  | BytesSetting !BytesField
  | PathSetSetting !PathSetField
  | FeaturesSetting
  deriving (Eq, Show)

-- | The settings whose value is a set of paths, kept as bytes.
data PathSetField = SandboxPathsField | AllowedImpureHostDepsField
  deriving (Eq, Show)

-- | The list settings.
data ListField = SubstitutersField | TrustedKeysField | AllowedUrisField
  deriving (Eq, Show)

-- | The Boolean settings.
data BoolField
  = FallbackField
  | AlwaysAllowSubstitutesField
  | RestrictEvalField
  | PureEvalField
  | TraceVerboseField
  | AllowImportFromDerivationField
  | TraceImportFromDerivationField
  | SandboxFallbackField
  | RequireDropGroupsField
  | UseXdgBaseDirectoriesField
  | DarwinLogSandboxViolationsField
  | KeepFailedField
  deriving (Eq, Show)

-- | The settings whose value is a string, kept as bytes.
data BytesField
  = SandboxBuildDirField
  | SandboxDevShmSizeField
  | SslCertFileField
  | BashPromptField
  | BashPromptPrefixField
  | BashPromptSuffixField
  | FlakeRegistryField
  deriving (Eq, Show)

-- | The scalar settings, which only ever replace.
data ScalarField = MaxCallDepthField | CoresField
  deriving (Eq, Show)

-- | Whether an assignment replaces the accumulated value or appends to it.
data ApplyMode = ReplaceMode | AppendMode
  deriving (Eq, Show)

getList :: ListField -> NixConfig -> [Text]
getList SubstitutersField = ncSubstituters
getList TrustedKeysField = ncTrustedPublicKeys
getList AllowedUrisField = ncAllowedUris

setList :: ListField -> [Text] -> NixConfig -> NixConfig
setList SubstitutersField v config = config {ncSubstituters = v}
setList TrustedKeysField v config = config {ncTrustedPublicKeys = v}
setList AllowedUrisField v config = config {ncAllowedUris = v}

listFieldKey :: ListField -> ByteString
listFieldKey SubstitutersField = substitutersKey
listFieldKey TrustedKeysField = trustedKeysKey
listFieldKey AllowedUrisField = allowedUrisKey

setScalar :: ScalarField -> Word32 -> NixConfig -> NixConfig
setScalar MaxCallDepthField v config = config {ncMaxCallDepth = v}
setScalar CoresField v config = config {ncCores = Just v}

scalarFieldKey :: ScalarField -> ByteString
scalarFieldKey MaxCallDepthField = maxCallDepthKey
scalarFieldKey CoresField = coresKey

boolFieldKey :: BoolField -> ByteString
boolFieldKey FallbackField = fallbackKey
boolFieldKey AlwaysAllowSubstitutesField = alwaysAllowSubstitutesKey
boolFieldKey RestrictEvalField = restrictEvalKey
boolFieldKey PureEvalField = pureEvalKey
boolFieldKey TraceVerboseField = traceVerboseKey
boolFieldKey AllowImportFromDerivationField = allowImportFromDerivationKey
boolFieldKey TraceImportFromDerivationField = traceImportFromDerivationKey
boolFieldKey SandboxFallbackField = sandboxFallbackKey
boolFieldKey RequireDropGroupsField = requireDropGroupsKey
boolFieldKey UseXdgBaseDirectoriesField = useXdgBaseDirectoriesKey
boolFieldKey DarwinLogSandboxViolationsField = darwinLogSandboxViolationsKey
boolFieldKey KeepFailedField = keepFailedKey

setBool :: BoolField -> Bool -> NixConfig -> NixConfig
setBool FallbackField v config = config {ncFallback = v}
setBool AlwaysAllowSubstitutesField v config = config {ncAlwaysAllowSubstitutes = v}
setBool RestrictEvalField v config = config {ncRestrictEval = v}
setBool PureEvalField v config = config {ncPureEval = v}
setBool TraceVerboseField v config = config {ncTraceVerbose = v}
setBool AllowImportFromDerivationField v config = config {ncAllowImportFromDerivation = v}
setBool TraceImportFromDerivationField v config = config {ncTraceImportFromDerivation = v}
setBool SandboxFallbackField v config = config {ncSandboxFallback = v}
setBool RequireDropGroupsField v config = config {ncRequireDropSupplementaryGroups = Just v}
setBool UseXdgBaseDirectoriesField v config = config {ncUseXdgBaseDirectories = v}
setBool DarwinLogSandboxViolationsField v config = config {ncDarwinLogSandboxViolations = v}
setBool KeepFailedField v config = config {ncKeepFailed = v}

getPathSet :: PathSetField -> NixConfig -> PathSetValue
getPathSet SandboxPathsField = ncSandboxPaths
getPathSet AllowedImpureHostDepsField = ncAllowedImpureHostDeps

setPathSet :: PathSetField -> PathSetValue -> NixConfig -> NixConfig
setPathSet SandboxPathsField v config = config {ncSandboxPaths = v}
setPathSet AllowedImpureHostDepsField v config = config {ncAllowedImpureHostDeps = v}

-- | Upstream's @Setting\<std::string\>@: the value as it reads, its tokens
-- joined by single spaces.
setBytes :: BytesField -> ByteString -> NixConfig -> NixConfig
setBytes SandboxBuildDirField v config = config {ncSandboxBuildDir = v}
setBytes SandboxDevShmSizeField v config = config {ncSandboxDevShmSize = v}
setBytes SslCertFileField v config = config {ncSslCertFile = Just v}
setBytes BashPromptField v config = config {ncBashPrompt = v}
setBytes BashPromptPrefixField v config = config {ncBashPromptPrefix = v}
setBytes BashPromptSuffixField v config = config {ncBashPromptSuffix = v}
setBytes FlakeRegistryField v config = config {ncFlakeRegistry = v}

-- | Upstream's Boolean spellings (@BaseSetting\<bool\>::parse@): anything
-- else is an error with upstream's wording, which names the setting alone,
-- as 'parseUnsignedSetting' does.
parseBool :: ByteString -> ByteString -> Either Text Bool
parseBool name value
  | value `elem` trueSpellings = Right True
  | value `elem` falseSpellings = Right False
  | otherwise = Left ("Boolean setting '" <> shownBytes name <> "' has invalid value '" <> shownBytes value <> "'")

trueSpellings, falseSpellings :: [ByteString]
trueSpellings = ["true", "yes", "1"]
falseSpellings = ["false", "no", "0"]

-- | Fold expanded assignments onto the default config, weakest first.
-- The caller orders them so the strongest source's assignments are last
-- (and so win).  A value a setting cannot take aborts the fold, as
-- upstream's @UsageError@ does.
resolveConfig :: [ConfigAssignment] -> Either Text NixConfig
resolveConfig = foldM applyAssignment defaultNixConfig

-- ---------------------------------------------------------------------------
-- Messages
-- ---------------------------------------------------------------------------

-- | Bytes from a source as a message quotes them, for display only: their
-- UTF-8 reading, with U+FFFD where it fails.  Upstream writes the bytes
-- themselves; a message here is text.
shownBytes :: ByteString -> Text
shownBytes = TE.decodeUtf8Lenient

-- | What every error about the files themselves opens with (a syntax
-- error, a failed include, a path that is not absolute): the format's
-- name.  A setting's value error carries none, as upstream's does not.
configErrorPrefix :: Text
configErrorPrefix = "nix.conf: "

-- ---------------------------------------------------------------------------
-- Setting names and syntax
-- ---------------------------------------------------------------------------

substitutersKey, substitutersAlias :: ByteString
substitutersKey = "substituters"
substitutersAlias = "binary-caches"

trustedKeysKey, trustedKeysAlias :: ByteString
trustedKeysKey = "trusted-public-keys"
trustedKeysAlias = "binary-cache-public-keys"

fallbackKey, fallbackAlias :: ByteString
fallbackKey = "fallback"
fallbackAlias = "build-fallback"

coresKey, coresAlias :: ByteString
coresKey = "cores"
coresAlias = "build-cores"

allowImportFromDerivationKey, allowedUrisKey, alwaysAllowSubstitutesKey, maxCallDepthKey, pureEvalKey, restrictEvalKey, traceImportFromDerivationKey, traceVerboseKey :: ByteString
allowImportFromDerivationKey = "allow-import-from-derivation"
allowedUrisKey = "allowed-uris"
alwaysAllowSubstitutesKey = "always-allow-substitutes"
maxCallDepthKey = "max-call-depth"
pureEvalKey = "pure-eval"
restrictEvalKey = "restrict-eval"
traceImportFromDerivationKey = "trace-import-from-derivation"
traceVerboseKey = "trace-verbose"

sandboxKey, sandboxFallbackKey, sandboxPathsKey, sandboxBuildDirKey, sandboxDevShmSizeKey, requireDropGroupsKey, sslCertFileKey :: ByteString
sandboxKey = "sandbox"
sandboxFallbackKey = "sandbox-fallback"
sandboxPathsKey = "sandbox-paths"
sandboxBuildDirKey = "sandbox-build-dir"
sandboxDevShmSizeKey = "sandbox-dev-shm-size"
requireDropGroupsKey = "require-drop-supplementary-groups"
sslCertFileKey = "ssl-cert-file"

useXdgBaseDirectoriesKey :: ByteString
useXdgBaseDirectoriesKey = "use-xdg-base-directories"

-- | The settings of a @develop@ shell's prompt (@DevelopSettings@ in
-- develop.cc).
bashPromptKey, bashPromptPrefixKey, bashPromptSuffixKey :: ByteString
bashPromptKey = "bash-prompt"
bashPromptPrefixKey = "bash-prompt-prefix"
bashPromptSuffixKey = "bash-prompt-suffix"

allowedImpureHostDepsKey, darwinLogSandboxViolationsKey :: ByteString
allowedImpureHostDepsKey = "allowed-impure-host-deps"
darwinLogSandboxViolationsKey = "darwin-log-sandbox-violations"

experimentalFeaturesKey :: ByteString
experimentalFeaturesKey = "experimental-features"

flakeRegistryKey :: ByteString
flakeRegistryKey = "flake-registry"

-- | Upstream's global flake registry, the default of @flake-registry@.
defaultFlakeRegistry :: ByteString
defaultFlakeRegistry = "https://channels.nixos.org/flake-registry.json"

-- | The setting for what a build that fails leaves, which upstream gives no
-- other name.
keepFailedKey :: ByteString
keepFailedKey = "keep-failed"

-- | Upstream's older names for @sandbox@ and @sandbox-paths@
-- (the aliases of @sandboxMode@ and @sandboxPaths@ in globals.hh at
-- 2.24.9, local-settings.hh from 2.34.9).
sandboxAliases, sandboxPathsAliases :: [ByteString]
sandboxAliases = ["build-use-chroot", "build-use-sandbox"]
sandboxPathsAliases = ["build-chroot-dirs", "build-sandbox-paths"]

extraPrefix :: ByteString
extraPrefix = "extra-"

assignEq :: ByteString
assignEq = "="

plusSign :: ByteString
plusSign = "+"

includeDirective, bangIncludeDirective :: ByteString
includeDirective = "include"
bangIncludeDirective = "!include"

commentChar :: Char
commentChar = '#'

tokenSeparators :: ByteString
tokenSeparators = " \t\n\r"

-- | What a value's tokens are joined with, as upstream's
-- @concatStringsSep(" ", ...)@ joins them.
valueSeparator :: ByteString
valueSeparator = " "
