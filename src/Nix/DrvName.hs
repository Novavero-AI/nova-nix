-- | Where a derivation name splits into its name and its version, as
-- upstream splits one: at the first dash that something other than a
-- letter follows, so @apache-httpd-2.0.48@ is @apache-httpd@ and
-- @2.0.48@, and @foo-.1@ is @foo@ and @.1@.  A letter is an ASCII
-- letter: upstream asks the C library with no locale set, where no
-- other byte is one.  A dash that ends the name is followed by nothing
-- and does not split it.
--
-- @builtins.parseDrvName@ splits a Nix string, which is bytes, and a
-- profile names an element by the name part of a store path's name,
-- which is text, so the split runs over whichever units the caller
-- takes off its string.
module Nix.DrvName
  ( drvNameDash,
  )
where

import Data.Char (isAsciiLower, isAsciiUpper)

-- | The position of the dash a derivation name splits at, counted in
-- the units @uncons@ takes off: the name is everything before it and
-- the version everything after it.  'Nothing' when no dash splits the
-- string, which is then all name.
drvNameDash :: (s -> Maybe (Char, s)) -> s -> Maybe Int
drvNameDash uncons = go 0
  where
    go !at s = case uncons s of
      Nothing -> Nothing
      Just ('-', rest)
        | Just (next, _) <- uncons rest,
          not (isAsciiLetter next) ->
            Just at
      Just (_, rest) -> go (at + 1) rest
    isAsciiLetter c = isAsciiLower c || isAsciiUpper c
