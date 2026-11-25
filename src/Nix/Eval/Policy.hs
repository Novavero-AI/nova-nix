-- | The evaluation policy: upstream's @restrict-eval@ and @pure-eval@
-- settings, the @allowed-uris@ list, and the pure decisions they drive,
-- plus the @trace-verbose@ setting, which decides what
-- @builtins.traceVerbose@ does, and @allow-import-from-derivation@ and
-- @trace-import-from-derivation@, which decide whether a path reader may
-- build what its argument's context names.
--
-- Upstream gates evaluation in two layers.  Under either setting the root
-- filesystem is wrapped in an accessor that admits a path when it lies
-- within an allowed prefix or is an ancestor of one, and the allowed set
-- starts as the search path roots and grows as the evaluation copies,
-- fetches or writes store objects.  Under @restrict-eval@ alone a fetch is
-- also checked against @allowed-uris@.  @pure-eval@ additionally empties the
-- search path, hides the impure constants, and makes the fetchers demand
-- a hash or revision.  The search path is the caller's to choose as it
-- starts the session, the constants are left out by
-- 'Nix.Builtins.builtinEnv' from the policy it is given, and the fetchers'
-- demand lives with the builtins and consults t'EvalPolicy' through
-- 'Nix.Eval.Types.MonadEval'.
--
-- Everything here is pure so each rule can be tested on its own, the
-- walk of a path through its symlinks included ('walkPath'), which takes
-- the read of a link as an argument; "Nix.Eval.IO" hands it the
-- filesystem.
module Nix.Eval.Policy
  ( -- * Policy
    EvalPolicy (..),
    ImportFromDerivation (..),
    unrestrictedPolicy,
    pathsRestricted,

    -- * Allowed paths
    AllowedPaths,
    noAllowedPaths,
    allowPathIn,
    isAllowedPath,
    isAllowedPrefix,
    pathComponents,
    joinComponents,
    isAbsolutePath,
    storeRootPrefix,
    windowsLinkTargetJoins,

    -- * Walking a path's symlinks
    PathRules (..),
    hostPathRules,
    SymlinkResolution (..),
    LinkTarget (..),
    linkTarget,
    WalkStop (..),
    walkPath,
    forbiddenPathMessage,

    -- * Allowed URIs
    isAllowedUri,
    uriAccess,
    fileUrlPrefix,
  )
where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List (inits, isPrefixOf)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Nix.Eval.CanonPath (canonPathValue)
import Nix.Store.Path (StoreDir, defaultStoreDirText, qualifyPathText)
import qualified System.Info

-- ---------------------------------------------------------------------------
-- Policy
-- ---------------------------------------------------------------------------

-- | What an evaluation may reach, and whether @builtins.traceVerbose@
-- traces.  The two modes are independent, as upstream's settings are:
-- @pure-eval@ does not imply the URI check (@checkURI@ consults
-- @restrictEval@ alone), and @restrict-eval@ does not hide the impure
-- constants or empty the search path.
data EvalPolicy = EvalPolicy
  { -- | Upstream @restrict-eval@: filesystem reads are confined to the
    -- allowed prefixes, @getEnv@ answers empty, and a fetch must match
    -- 'epAllowedUris'.
    epRestrictEval :: !Bool,
    -- | Upstream @pure-eval@: the same filesystem confinement, no search
    -- path, no @currentTime@ or @currentSystem@, no @storePath@, and the
    -- fetchers require a hash or revision.
    epPureEval :: !Bool,
    -- | Upstream @allowed-uris@: prefixes a fetch may reach under
    -- @restrict-eval@.
    epAllowedUris :: ![Text],
    -- | Upstream @trace-verbose@: @builtins.traceVerbose@ traces as
    -- @builtins.trace@ does, where otherwise it returns its second
    -- argument without forcing its first.
    epTraceVerbose :: !Bool,
    -- | Whether a path reader may build or substitute what its argument's
    -- context names.
    epImportFromDerivation :: !ImportFromDerivation
  }
  deriving (Eq, Show)

-- | What upstream's @allow-import-from-derivation@ and
-- @trace-import-from-derivation@ settings say together
-- (eval-settings.hh at 2.34.9): the second has no effect when the
-- first is off, so the three cases are all there are.
data ImportFromDerivation
  = -- | @allow-import-from-derivation = false@: a context that names an
    -- output is refused before anything is built, even an output that is
    -- valid already.
    RefuseImportFromDerivation
  | -- | The default: an output a context names is realised.
    AllowImportFromDerivation
  | -- | @trace-import-from-derivation = true@ as well: it is realised after
    -- a warning naming the first output.
    TraceImportFromDerivation
  deriving (Eq, Show)

-- | Upstream's default: every confinement off, nothing listed, and an
-- import from derivation allowed and not traced.
unrestrictedPolicy :: EvalPolicy
unrestrictedPolicy = EvalPolicy {epRestrictEval = False, epPureEval = False, epAllowedUris = [], epTraceVerbose = False, epImportFromDerivation = AllowImportFromDerivation}

-- | Whether filesystem access goes through the allow list: upstream wraps
-- the root filesystem when either setting is on.
pathsRestricted :: EvalPolicy -> Bool
pathsRestricted policy = epRestrictEval policy || epPureEval policy

-- | The clause a path refusal ends with.  Upstream names pure mode when
-- it is on, restricted mode otherwise (@eval.cc@, the accessor's error
-- callback).  The @--impure@ hint is upstream's text, kept for parity.
modeInformation :: EvalPolicy -> Text
modeInformation policy
  | epPureEval policy = "in pure evaluation mode (use '--impure' to override)"
  | otherwise = "in restricted mode"

-- ---------------------------------------------------------------------------
-- Allowed paths
-- ---------------------------------------------------------------------------

-- | The allowed prefixes, as component lists under lexicographic order.
-- That order is upstream's @CanonPath@ order (a separator sorts before
-- every other character), which is what makes "the smallest element at
-- or after a path is its descendant or nothing is" a correct test.
newtype AllowedPaths = AllowedPaths (Set [Text])
  deriving (Eq, Show)

-- | Nothing allowed yet.
noAllowedPaths :: AllowedPaths
noAllowedPaths = AllowedPaths Set.empty

-- | Grant access to a prefix (upstream @allowPrefix@).  The text is
-- canonicalized first, so the spelling that arrives does not matter.
allowPathIn :: Text -> AllowedPaths -> AllowedPaths
allowPathIn path (AllowedPaths allowed) = AllowedPaths (Set.insert (pathComponents path) allowed)

-- | Upstream @CanonPath::isAllowed@: access is allowed when the path is
-- within an allowed prefix (the prefix itself included) or when an
-- allowed prefix is within the path, which keeps the ancestors of an
-- allowed path reachable.
isAllowedPath :: AllowedPaths -> Text -> Bool
isAllowedPath allowed = isAllowedPrefix allowed . pathComponents

-- | 'isAllowedPath' over components, for a caller walking a path one
-- component at a time.
isAllowedPrefix :: AllowedPaths -> [Text] -> Bool
isAllowedPrefix (AllowedPaths allowed) path =
  descendantAllowed || any (`Set.member` allowed) (inits path)
  where
    descendantAllowed = maybe False (path `isPrefixOf`) (Set.lookupGE path allowed)

-- | Whether a prefix of a path, both as components, is the canonical
-- store directory or one of its ancestors while the path lies in the
-- store.  Such a prefix stands for the store an evaluation was given,
-- wherever that is, as upstream's store accessor has the store
-- directory for its root and looks for links only beneath it, so no
-- link is read there; under any other path it is a host directory.
storeRootPrefix :: [Text] -> [Text] -> Bool
storeRootPrefix path prefix = prefix `isPrefixOf` storeDirComponents && storeDirComponents `isPrefixOf` path
  where
    storeDirComponents = pathComponents defaultStoreDirText

-- | Whether a link target as Windows reports it names a place a path
-- value can spell.  Windows answers for every reparse point: a volume
-- mount point's target is the volume's GUID name (@Volume{...}@ once its
-- @\\??\\@ prefix is stripped) and a remote link's a UNC or device path,
-- neither of which a path value names, so a walk leaves such a link in
-- place for the read to follow.
windowsLinkTargetJoins :: Text -> Bool
windowsLinkTargetJoins target = not (any (`T.isPrefixOf` target) ["Volume{", "UNC\\", "\\\\"])

-- | A canonical path's components: the root has none, and a Windows
-- drive designator is the first one.
pathComponents :: Text -> [Text]
pathComponents = filter (not . T.null) . T.splitOn "/" . canonPathValue

-- | The canonical text of a component list, inverse of 'pathComponents'.
joinComponents :: [Text] -> Text
joinComponents components = case components of
  (drive : rest)
    | isDriveDesignator drive -> drive <> "/" <> T.intercalate "/" rest
  _ -> "/" <> T.intercalate "/" components

-- | Whether a component is a Windows drive designator, @X:@.
isDriveDesignator :: Text -> Bool
isDriveDesignator d = T.length d == driveDesignatorLength && T.isSuffixOf ":" d

-- | @X:@
driveDesignatorLength :: Int
driveDesignatorLength = 2

-- | Whether a path's text is rooted: a leading @/@, or a drive designator
-- (a canonical Windows path value spells @C:/...@).  The gates refuse
-- anything else, since a relative path would be checked against one
-- location and opened at another.  A leading @/@ with no drive is fixed
-- in place too: on Windows it names the store directory's drive
-- ('Nix.Store.Path.qualifyPathText'), which the gates check and the
-- reads open.
isAbsolutePath :: Text -> Bool
isAbsolutePath path = T.isPrefixOf "/" path || hasDrive
  where
    hasDrive = case T.unpack (T.take (driveDesignatorLength + 1) path) of
      [letter, ':', '/'] -> isAsciiLetter letter
      _ -> False

-- | An ASCII letter, as a drive designator's is.
isAsciiLetter :: Char -> Bool
isAsciiLetter c = isAsciiLower c || isAsciiUpper c

-- ---------------------------------------------------------------------------
-- Walking a path's symlinks
-- ---------------------------------------------------------------------------

-- | The path rules a link's target is read under: the host's, since the
-- host's file system wrote it.  A walk takes them as an argument, so the
-- Windows rules are tested on every host.
data PathRules
  = -- | @/@ separates, and a target leading with it is absolute.
    PosixPaths
  | -- | @/@ and @\\@ separate, a target leading with either or with a
    -- drive designator is absolute, and a drive designator met inside a
    -- path starts it again at that drive.
    WindowsPaths
  deriving (Eq, Show)

-- | The rules of the host evaluation runs on.
hostPathRules :: PathRules
hostPathRules
  | System.Info.os == "mingw32" = WindowsPaths
  | otherwise = PosixPaths

-- | Which symlinks a walk follows, upstream's @SymlinkResolution@: every
-- one, or every one but the last component's, which is then checked in
-- place.
data SymlinkResolution = ResolveFull | ResolveAncestors
  deriving (Eq, Show)

-- | A link's target as a walk splices it, as components: an absolute one
-- restarts the walk at the root, a relative one resumes it in the link's
-- own directory.
data LinkTarget = AbsoluteTarget [Text] | RelativeTarget [Text]
  deriving (Eq, Show)

-- | A link's target text under the host's rules, or 'Nothing' for one no
-- path value names ('windowsLinkTargetJoins'), which a walk leaves in
-- place.  A target leading with a separator is absolute on every host,
-- store text among them, where Windows' own @isAbsolute@ wants a drive.
linkTarget :: PathRules -> Text -> Maybe LinkTarget
linkTarget rules target = case rules of
  WindowsPaths
    | not (windowsLinkTargetJoins target) -> Nothing
    | rooted || startsWithDrive -> Just (AbsoluteTarget components)
  PosixPaths
    | rooted -> Just (AbsoluteTarget components)
  _ -> Just (RelativeTarget components)
  where
    separates c = c == '/' || (rules == WindowsPaths && c == '\\')
    components = filter (not . T.null) (T.split separates target)
    rooted = maybe False (separates . fst) (T.uncons target)
    -- @C:@ alone or before a separator; @C:name@ is drive-relative.
    startsWithDrive = case T.unpack (T.take (driveDesignatorLength + 1) target) of
      [letter, ':'] -> isAsciiLetter letter
      [letter, ':', next] -> isAsciiLetter letter && separates next
      _ -> False

-- | Why a walk stopped short of a path: the check's refusal of a prefix,
-- or a chain of links past upstream's bound.
data WalkStop e = WalkRefused e | WalkLoops
  deriving (Eq, Show)

-- | The symlink-resolving prefix walk of upstream's
-- @SourceAccessor::resolveSymlinks@ (source-accessor.cc at 2.35.4) over
-- an absolute canonical path, giving the path it resolves to.  Each
-- component is pushed, the check is asked about the prefix so far where
-- upstream's @maybeLstat@ would be, and a link at that prefix, read with
-- the reader given, has its target ('linkTarget') spliced in front of
-- what remains.  The joining is over path-value text alone: the host's
-- rules decide only how a target reads.  Under 'ResolveAncestors' the
-- last component's own link is left alone, as upstream consults
-- @maybeLstat@ only while something remains to resolve.  For a path in
-- the store the canonical store directory and its ancestors stand for
-- the store an evaluation was given, wherever that is, so no link is
-- read there ('storeRootPrefix').
walkPath ::
  (Monad m) =>
  PathRules ->
  SymlinkResolution ->
  ([Text] -> Text -> Either e ()) ->
  (Text -> m (Maybe Text)) ->
  Text ->
  m (Either (WalkStop e) Text)
walkPath rules mode refusalAt readLink canonical = go symlinkFollowLimit [] (pathComponents canonical)
  where
    go _ done [] = pure (Right (joined (reverse done)))
    go linksLeft done (component : rest)
      | component == "." = go linksLeft done rest
      | component == ".." = go linksLeft (drop 1 done) rest
      -- A rooted value whose first component is a drive (@/C:/...@, as
      -- @/. + "C:\\..."@ spells one) names that drive, so it starts the
      -- path again there rather than nesting under the store's drive.
      | WindowsPaths <- rules, isDriveDesignator component, not (null done) = go linksLeft [] (component : rest)
      | Left refusal <- refusalAt prefix prefixText = pure (Left (WalkRefused refusal))
      | otherwise = do
          target <- if followsLinkAt prefix rest then readLink prefixText else pure Nothing
          case target >>= linkTarget rules of
            Nothing -> go linksLeft prefixReversed rest
            Just _ | linksLeft <= 0 -> pure (Left WalkLoops)
            Just (AbsoluteTarget spliced) -> go (linksLeft - 1) [] (spliced ++ rest)
            Just (RelativeTarget spliced) -> go (linksLeft - 1) done (spliced ++ rest)
      where
        prefixReversed = component : done
        prefix = reverse prefixReversed
        prefixText = joined prefix
    followsLinkAt prefix rest =
      not (storeRootPrefix (prefix ++ rest) prefix) && (mode == ResolveFull || not (null rest))
    -- A drive designator leads only a Windows path; on POSIX @C:@ is a
    -- name like any other.
    joined components = case rules of
      WindowsPaths -> joinComponents components
      PosixPaths -> "/" <> T.intercalate "/" components

-- | Upstream's bound on symlinks followed while resolving one path.
symlinkFollowLimit :: Int
symlinkFollowLimit = 1024

-- | Upstream's refusal, from the accessor's error callback in @eval.cc@.
forbiddenPathMessage :: EvalPolicy -> Text -> Text
forbiddenPathMessage policy path =
  "access to absolute path '" <> path <> "' is forbidden " <> modeInformation policy

-- ---------------------------------------------------------------------------
-- Allowed URIs
-- ---------------------------------------------------------------------------

-- | Upstream @isAllowedURI@ (@eval.cc@ at 2.24.9): a URI is allowed when
-- it equals a prefix, or extends a prefix at a @/@ boundary (the prefix
-- ends in @/@, or the URI's next character is @/@), or the prefix is a
-- bare scheme ending in @:@.  @https://github.co@ therefore does not
-- admit @https://github.com@.
isAllowedUri :: [Text] -> Text -> Bool
isAllowedUri prefixes uri = any admits prefixes
  where
    admits prefix =
      uri == prefix
        || ( T.length uri > T.length prefix
               && not (T.null prefix)
               && T.isPrefixOf prefix uri
               && ( T.isSuffixOf "/" prefix
                      || T.isPrefixOf "/" (T.drop (T.length prefix) uri)
                      || isJustSchemePrefix prefix
                  )
           )

-- | Upstream @isJustSchemePrefix@: a scheme name followed by a colon and
-- nothing else, the scheme spelled as @url-parts.hh@ allows
-- (@[a-z][a-z0-9+.-]*@).
isJustSchemePrefix :: Text -> Bool
isJustSchemePrefix prefix = case T.unsnoc prefix of
  Just (scheme, ':') -> isValidSchemeName scheme
  _ -> False

isValidSchemeName :: Text -> Bool
isValidSchemeName scheme = case T.uncons scheme of
  Just (leading, rest) -> isAsciiLower leading && T.all schemeChar rest
  Nothing -> False
  where
    schemeChar c = isAsciiLower c || isDigit c || c `elem` schemePunctuation

-- | The punctuation a scheme name may carry after its first character.
schemePunctuation :: [Char]
schemePunctuation = "+.-"

-- | Upstream @EvalState::checkURI@: nothing is checked unless
-- @restrict-eval@ is on; an allowed URI passes; a URI that is a path, or
-- a @file://@ URL, is checked against the allowed paths instead
-- (upstream passes the remainder through @CanonPath@, so a rootless
-- remainder is rooted, and one that spells a drive, @file://C:/...@, is
-- rooted already); anything else is refused.  The path is checked on the
-- drive the given store directory puts it on ('qualifyPathText'), the
-- one the fetch reads it from.
uriAccess :: StoreDir -> EvalPolicy -> AllowedPaths -> Text -> Either Text ()
uriAccess storeDir policy allowed uri
  | not (epRestrictEval policy) = Right ()
  | isAllowedUri (epAllowedUris policy) uri = Right ()
  | T.isPrefixOf "/" uri = pathAccess uri
  | Just rest <- T.stripPrefix fileUrlPrefix uri = pathAccess (rooted rest)
  | otherwise = Left (forbiddenUriMessage uri)
  where
    rooted rest = if isAbsolutePath rest then rest else "/" <> rest
    pathAccess path =
      let canonical = qualifyPathText storeDir (canonPathValue path)
       in if isAllowedPath allowed canonical then Right () else Left (forbiddenPathMessage policy canonical)

-- | What a local file URL begins with, its path or its drive after it.
fileUrlPrefix :: Text
fileUrlPrefix = "file://"

-- | Upstream's refusal from @checkURI@.
forbiddenUriMessage :: Text -> Text
forbiddenUriMessage uri = "access to URI '" <> uri <> "' is forbidden in restricted mode"
