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
-- name.  A variable set to nothing is set, as upstream reads it on every
-- platform: @GetEnvironmentVariableW@ is asked first for the space the
-- value needs, which is none only when the variable is not set, where
-- 'System.Environment.lookupEnv' reads a value of no characters as unset.
--
-- A command-line argument is read the same way: its bytes as the file
-- system takes them when it names a file, which on POSIX are the bytes
-- the program was given, and on Windows the UTF-8 form of its UTF-16.
module Nix.Environment
  ( EnvLookup (..),
    lookupEnvBytes,
    argumentBytes,
  )
where

import Data.ByteString (ByteString)
#if defined(mingw32_HOST_OS)
import Data.Char (GeneralCategory (Surrogate), generalCategory)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word32)
import Foreign.C.String (CWString, peekCWStringLen, withCWString)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (nullPtr)
#else
import qualified Data.ByteString.Char8 as BS8
import qualified System.OsPath as OP
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
-- NULL for a missing name and @GetEnvironmentVariableW@ needs no space
-- for one.
lookupEnvBytes :: ByteString -> IO EnvLookup
#if defined(mingw32_HOST_OS)
lookupEnvBytes name = case TE.decodeUtf8' name of
  -- A Windows name is UTF-16, so bytes with no UTF-8 reading name no
  -- variable.
  Left _ -> pure EnvUnset
  Right decoded -> maybe EnvUnset utf8Value <$> windowsVariable (T.unpack decoded)
  where
    -- base decodes a surrogate pair into one character and keeps an
    -- unpaired surrogate as its own code point ('hasNoUtf8Form').
    utf8Value value
      | any hasNoUtf8Form value = EnvUnpairedSurrogate
      | otherwise = EnvValue (TE.encodeUtf8 (T.pack value))
#else
lookupEnvBytes name = maybe EnvUnset EnvValue <$> PosixEnv.getEnv name
#endif

-- | The bytes of a command-line argument as 'System.Environment.getArgs'
-- gave it, or 'Nothing' for a Windows argument holding an unpaired
-- surrogate.  On POSIX they are the argument encoded back with the file
-- system encoding it was decoded with, which keeps a byte with no UTF-8
-- reading as it was.
argumentBytes :: String -> IO (Maybe ByteString)
#if defined(mingw32_HOST_OS)
argumentBytes arg
  | any hasNoUtf8Form arg = pure Nothing
  | otherwise = pure (Just (TE.encodeUtf8 (T.pack arg)))
#else
argumentBytes arg = Just . BS8.pack . map OP.toChar . OP.unpack <$> OP.encodeFS arg
#endif

#if defined(mingw32_HOST_OS)
-- | Whether a character is an unpaired surrogate's own code point, which
-- has no UTF-8 form and which 'T.pack' would replace with U+FFFD.
hasNoUtf8Form :: Char -> Bool
hasNoUtf8Form = (== Surrogate) . generalCategory

-- | A variable's value, as upstream reads one: @GetEnvironmentVariableW@
-- asked for the space the value needs with its terminating NUL, none for
-- a variable that is not set, then for the value, which on success is
-- shorter than the space given and otherwise is the space it now needs,
-- had it grown between the calls.
windowsVariable :: String -> IO (Maybe String)
windowsVariable name = withCWString name $ \key -> do
  needed <- c_GetEnvironmentVariable key nullPtr 0
  if needed == 0 then pure Nothing else Just <$> readValue key needed
  where
    readValue key space = allocaArray (fromIntegral space) $ \buffer -> do
      written <- c_GetEnvironmentVariable key buffer space
      if written >= space
        then readValue key written
        else peekCWStringLen (buffer, fromIntegral written)

foreign import ccall unsafe "windows.h GetEnvironmentVariableW"
  c_GetEnvironmentVariable :: CWString -> CWString -> Word32 -> IO Word32
#endif
