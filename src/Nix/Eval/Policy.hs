-- | The evaluation policy: upstream's @restrict-eval@ and @pure-eval@
-- settings, the @allowed-uris@ list, and the pure decisions they drive.
--
-- Upstream gates evaluation in two layers (@src/libexpr/eval.cc@ at
-- 2.24.9).  Under either setting the root filesystem is wrapped in an
-- @AllowListSourceAccessor@ that admits a path when it lies within an
-- allowed prefix or is an ancestor of one (@CanonPath::isAllowed@), and
-- the allowed set starts as the search path roots and grows as the
-- evaluation copies, fetches or writes store objects (@allowPath@).
-- Under @restrict-eval@ alone a fetch is also checked against
-- @allowed-uris@ (@checkURI@).  @pure-eval@ additionally empties the
-- search path, hides the impure constants, and makes the fetchers demand
-- a hash or revision; those decisions live with the builtins and consult
-- 'EvalPolicy' through 'Nix.Eval.Types.MonadEval'.
--
-- Everything here is pure so each rule can be tested on its own.  The
-- filesystem side (walking a path through its symlinks, prefix by
-- prefix) lives in "Nix.Eval.IO".
module Nix.Eval.Policy
  ( -- * Policy
    EvalPolicy (..),
    unrestrictedPolicy,
    pathsRestricted,
    modeInformation,

    -- * Allowed paths
    AllowedPaths,
    noAllowedPaths,
    allowPathIn,
    isAllowedPath,
    isAllowedPrefix,
    pathComponents,
    joinComponents,
    isAbsolutePath,
    forbiddenPathMessage,

    -- * Allowed URIs
    isAllowedUri,
    uriAccess,
    forbiddenUriMessage,
  )
where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List (inits, isPrefixOf)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Nix.Eval.CanonPath (canonPathValue)

-- ---------------------------------------------------------------------------
-- Policy
-- ---------------------------------------------------------------------------

-- | What an evaluation may reach.  The two flags are independent, as
-- upstream's settings are: @pure-eval@ does not imply the URI check
-- (@checkURI@ consults @restrictEval@ alone), and @restrict-eval@ does
-- not hide the impure constants or empty the search path.
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
    epAllowedUris :: ![Text]
  }
  deriving (Eq, Show)

-- | Upstream's default: both settings off, nothing listed.
unrestrictedPolicy :: EvalPolicy
unrestrictedPolicy = EvalPolicy {epRestrictEval = False, epPureEval = False, epAllowedUris = []}

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
  where
    isDriveDesignator d = T.length d == driveDesignatorLength && T.isSuffixOf ":" d

-- | @X:@
driveDesignatorLength :: Int
driveDesignatorLength = 2

-- | Whether a path's text is rooted: a leading @/@, or a drive designator
-- (a canonical Windows path value spells @C:/...@).  The gates refuse
-- anything else, since a relative path would be checked against one
-- location and opened at another.
isAbsolutePath :: Text -> Bool
isAbsolutePath path = T.isPrefixOf "/" path || hasDrive
  where
    hasDrive = case T.unpack (T.take (driveDesignatorLength + 1) path) of
      [letter, ':', '/'] -> isAsciiLetter letter
      _ -> False
    isAsciiLetter c = isAsciiLower c || isAsciiUpper c

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
-- rooted already); anything else is refused.
uriAccess :: EvalPolicy -> AllowedPaths -> Text -> Either Text ()
uriAccess policy allowed uri
  | not (epRestrictEval policy) = Right ()
  | isAllowedUri (epAllowedUris policy) uri = Right ()
  | T.isPrefixOf "/" uri = pathAccess uri
  | Just rest <- T.stripPrefix fileUrlPrefix uri = pathAccess (rooted rest)
  | otherwise = Left (forbiddenUriMessage uri)
  where
    rooted rest = if isAbsolutePath rest then rest else "/" <> rest
    pathAccess path =
      let canonical = canonPathValue path
       in if isAllowedPath allowed canonical then Right () else Left (forbiddenPathMessage policy canonical)

fileUrlPrefix :: Text
fileUrlPrefix = "file://"

-- | Upstream's refusal from @checkURI@.
forbiddenUriMessage :: Text -> Text
forbiddenUriMessage uri = "access to URI '" <> uri <> "' is forbidden in restricted mode"
