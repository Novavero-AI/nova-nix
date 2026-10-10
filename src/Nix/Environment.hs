{-# LANGUAGE CPP #-}

-- | The process environment, read the way a Nix string holds it: as bytes.
--
-- Upstream reads a variable with @getenv@ and keeps the bytes it returns
-- (@getEnv@ in src\/libutil\/environment-variables.cc at Nix 2.24.9), so
-- a value reaches @builtins.getEnv@ and @builtins.nixPath@ unchanged
-- whatever its encoding.  On POSIX this module makes the same call.
-- 'System.Environment.lookupEnv' does not: it decodes the value with the
-- file-system encoding, which carries an undecodable byte as a lone
-- surrogate that 'Data.Text.pack' then replaces with U+FFFD, and it
-- encodes the name with the foreign encoding, which drops a character
-- the locale cannot spell instead of failing.
--
-- A Windows environment is UTF-16.  A value becomes the UTF-8 bytes a
-- Nix string carries, and a name is read as UTF-8.  An unpaired
-- surrogate has no UTF-8 form, so a value holding one is refused rather
-- than approximated, the rule the NAR serialiser applies to such a file
-- name.
module Nix.Environment
  ( EnvLookup (..),
    lookupEnvBytes,
  )
where

import Data.ByteString (ByteString)
#if defined(mingw32_HOST_OS)
import Data.Char (GeneralCategory (Surrogate), generalCategory)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Environment (lookupEnv)
#else
import qualified System.Posix.Env.ByteString as PosixEnv
#endif

-- | What the environment holds under one name.
data EnvLookup
  = -- | No variable has the name.
    EnvUnset
  | -- | The value, as the bytes a Nix string carries.
    EnvValue !ByteString
  | -- | A Windows value holding an unpaired UTF-16 surrogate, which has no
    -- UTF-8 form.  Never the answer on POSIX, where a value is bytes.
    EnvUnpairedSurrogate
  deriving (Eq, Show)

-- | Look up a variable by the bytes of its name.  Nothing is caught: the
-- read cannot fail other than by finding nothing, since @getenv@ answers
-- NULL for a missing name and base's 'System.Environment.lookupEnv'
-- answers 'Nothing' whenever @GetEnvironmentVariableW@ finds no value.
lookupEnvBytes :: ByteString -> IO EnvLookup
#if defined(mingw32_HOST_OS)
lookupEnvBytes name = case TE.decodeUtf8' name of
  -- A Windows name is UTF-16, so bytes with no UTF-8 reading name no
  -- variable.
  Left _ -> pure EnvUnset
  Right decoded -> maybe EnvUnset utf8Value <$> lookupEnv (T.unpack decoded)
  where
    -- base decodes a surrogate pair into one character and keeps an
    -- unpaired surrogate as its own code point, which 'T.pack' would
    -- replace with U+FFFD.
    utf8Value value
      | any ((== Surrogate) . generalCategory) value = EnvUnpairedSurrogate
      | otherwise = EnvValue (TE.encodeUtf8 (T.pack value))
#else
lookupEnvBytes name = maybe EnvUnset EnvValue <$> PosixEnv.getEnv name
#endif
