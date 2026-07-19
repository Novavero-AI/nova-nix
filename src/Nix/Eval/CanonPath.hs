-- | Lexical path canonicalization for eval-produced path values.
--
-- Upstream Nix canonicalizes every path value (@CanonPath@): @.@ segments
-- drop, @..@ pops the previous segment (at a root it drops), repeated
-- separators collapse to one.  nova-nix applies the same algorithm at the
-- points a path value is produced - literal resolution, @toPath@, path
-- concatenation, search-path candidates - so the canonical text, not the
-- user's spelling, is what reaches store-copy names, string coercions, and
-- comparisons.
--
-- Purely lexical: no filesystem access, and symlinks are not resolved
-- (upstream resolves symlinks separately, where required).  The separator
-- style of the input is preserved: a canonical forward-slash path stays
-- forward-slash (this is the identity form for store paths, on every
-- platform), and a native Windows path keeps its backslashes.  A leading
-- separator marks a rooted path independent of platform, because eval-time
-- path values are rooted in the canonical @\/nix\/store@ sense even on
-- Windows, where 'System.FilePath.splitDrive' would not see a bare @\/@ as
-- rooted.  A relative input stays relative: leading @..@ segments are kept,
-- and a fully-collapsed relative path is @.@.
module Nix.Eval.CanonPath
  ( canonPath,
    canonBaseName,
  )
where

import Data.Char (isAlpha)
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (isPathSeparator, pathSeparator)

-- | Canonicalize a path's text form.  See the module comment for the
-- algorithm and the separator-preservation guarantee.
canonPath :: Text -> Text
canonPath t
  | T.null t = "."
  | otherwise = assemble
  where
    (drive, afterDrive) = splitDriveLetter t
    (leadingSeps, body) = T.span isPathSeparator afterDrive
    rooted = not (T.null leadingSeps)
    -- Preserve style: emit backslashes only when the input already uses the
    -- platform separator (Windows '\\').  On POSIX 'pathSeparator' is '/',
    -- so a literal backslash - an ordinary file-name character there - never
    -- flips the choice.
    sep = if T.any (== pathSeparator) t then pathSeparator else '/'
    segments = filter (not . T.null) (splitOnSeparators body)
    resolved = reverse (foldl' (collapseStep rooted) [] segments)
    joined = T.intercalate (T.singleton sep) resolved
    rootTok = if rooted then T.singleton sep else ""
    assemble
      | rooted = drive <> rootTok <> joined
      | not (T.null drive) = drive <> joined -- drive-relative (@C:foo@)
      | null resolved = "."
      | otherwise = joined

-- | Split a leading @X:@ drive letter (Windows) from the rest.  A bare
-- rooted path (@\/foo@) or a POSIX path has no drive.  UNC roots are left
-- to the leading-separator handling, which collapses them to a single root.
splitDriveLetter :: Text -> (Text, Text)
splitDriveLetter t
  | Just (c0, rest0) <- T.uncons t,
    isAlpha c0,
    Just (':', _) <- T.uncons rest0 =
      T.splitAt 2 t
  | otherwise = ("", t)

-- | One segment of the collapse fold; the accumulator holds resolved
-- segments in reverse.  A @..@ pops a real predecessor, drops at a root,
-- and is otherwise kept (a relative path may lead with @..@).
collapseStep :: Bool -> [Text] -> Text -> [Text]
collapseStep rooted acc segment = case segment of
  "." -> acc
  ".." -> case acc of
    [] -> [".." | not rooted]
    (top : below)
      | top == ".." -> ".." : acc
      | otherwise -> below
  _ -> segment : acc

-- | Split on platform path separators: both @\/@ and @\\@ on Windows, only
-- @\/@ on POSIX, where a backslash is an ordinary file-name character.
splitOnSeparators :: Text -> [Text]
splitOnSeparators = T.split isPathSeparator

-- | Last segment of a path - upstream @baseNameOf@.  Empty for a root or a
-- trailing separator, which the caller treats as "no base name".
canonBaseName :: Text -> Text
canonBaseName = T.takeWhileEnd (not . isPathSeparator)
