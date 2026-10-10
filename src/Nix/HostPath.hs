{-# LANGUAGE CPP #-}

-- | A path as the host's file API takes it ('OsPath'), spelled from the
-- bytes nova-nix holds for it, and shown back in a message.  One spelling
-- serves every such path: a NAR's entry names and link targets
-- ("Nix.Store.EntryName"), and nix.conf's include targets and directories
-- ("Nix.Config").
--
-- Upstream keeps such a path as the bytes it read and hands them to the
-- file system unchanged (@RestoreSink@ in fs-sink.cc, @parseConfigFiles@
-- in config.cc, at Nix 2.24.9).  A POSIX path is bytes, and an 'OsPath'
-- holds them as such, so the call receives the same bytes whatever the
-- locale.  Spelling the path through 'Data.Text.Text' would replace a byte
-- with no UTF-8 reading by U+FFFD, and through 'FilePath' would pass it
-- through the locale's codec and back, which does not return every byte
-- string under the multi-byte encodings (EUC-JP, CP932, GB18030 and
-- Big5-HKSCS through macOS's iconv); either way the call could name a
-- different file.
--
-- A Windows path is UTF-16, and the bytes nova-nix holds for one are UTF-8
-- (a NAR's names are, and "Nix.Environment" reads a Windows variable that
-- way), so a path is spelled from their UTF-8 reading, and bytes with none
-- name no Windows file.
module Nix.HostPath
  ( hostPathFromBytes,
    hostPathText,
  )
where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import System.OsPath (OsPath)
import qualified System.OsPath as OP
#if defined(mingw32_HOST_OS)
import qualified Data.Text as T
#else
import qualified Data.ByteString.Char8 as BS8
#endif

-- | The host path the bytes name: on POSIX the bytes themselves, on
-- Windows their UTF-8 reading, or 'Nothing' when they have none.
hostPathFromBytes :: ByteString -> Maybe OsPath
#if defined(mingw32_HOST_OS)
-- 'OP.encodeUtf' writes strict UTF-16LE, the form of every Windows path;
-- text read from UTF-8 always has one.
hostPathFromBytes bytes = either (const Nothing) (OP.encodeUtf . T.unpack) (TE.decodeUtf8' bytes)
#else
-- Each byte becomes one path unit.  'OP.unsafeFromChar' narrows a
-- character to the unit's width, which loses nothing on characters drawn
-- from bytes.
hostPathFromBytes = Just . OP.pack . map OP.unsafeFromChar . BS8.unpack
#endif

-- | A host path as a message shows it, for display only: its UTF-8
-- (POSIX) or UTF-16 (Windows) reading, with U+FFFD where that reading
-- fails.  Never the way back to a path.
hostPathText :: OsPath -> Text
#if defined(mingw32_HOST_OS)
-- 'T.pack' replaces each lone surrogate, which only the fallback holds.
hostPathText path = maybe (T.pack (map OP.toChar (OP.unpack path))) T.pack (OP.decodeUtf path)
#else
hostPathText = TE.decodeUtf8Lenient . BS8.pack . map OP.toChar . OP.unpack
#endif
