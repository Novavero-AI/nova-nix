-- | Which NAR entry names and symlink targets a host's filesystem can
-- hold, and the host path that spells one.
--
-- A NAR carries names and link targets as bytes.  Upstream's restore on
-- POSIX appends each name to the destination path as it finds it and
-- hands each target to @symlink@ unchanged (@RestoreSink@ in fs-sink.cc
-- at 2.24.9), refusing only what the archive grammar refuses: an empty
-- name, @.@, @..@, a @/@ or a NUL (@parse@ in archive.cc).  Those
-- refusals hold on every host, through the parser's own check.  Every
-- other limit belongs to a filesystem, so it is decided per host
-- ('NameRules'): a name the host cannot hold is refused before anything
-- is written, and a store path holding one does not materialize there,
-- whichever way it arrives.
--
-- The decisions are pure and take the rules as an argument, so every
-- host's answer is testable on any host.  Only 'entryNamePath' and
-- 'linkTargetPath' consult the running host.
module Nix.Store.EntryName
  ( -- * Hosts
    NameRules (..),
    hostNameRules,

    -- * Decisions
    checkEntryName,
    checkLinkTarget,

    -- * Host paths
    entryNamePath,
    linkTargetPath,

    -- * Display
    shownBytes,
  )
where

import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (ord)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)
import Nix.HostPath (hostPathFromBytes)
import qualified NovaCache.NAR.Stream as Stream
import NovaCache.SafeName (hasTrailingDotOrSpace, isReservedDeviceName)
import qualified System.Info
import System.OsPath (OsPath)

-- ---------------------------------------------------------------------------
-- Hosts
-- ---------------------------------------------------------------------------

-- | The names a family of filesystems can hold.
data NameRules
  = -- | Every name the grammar admits and every target, as bytes: Linux
    -- and the other POSIX hosts, where upstream restores names as it
    -- finds them.
    PosixNames
  | -- | A name must be UTF-8 holding no noncharacter.  APFS refuses any
    -- other name at the create (@EILSEQ@, and the same for a code point
    -- outside its Unicode tables, which is left to it since those tables
    -- move with the system release); HFS+ stores one with each byte
    -- outside UTF-8, and U+FFFE and U+FFFF, rewritten into a @%XX@
    -- escape, so the tree no longer names what its NAR does.  Both keep
    -- a link target's bytes as given.
    DarwinNames
  | -- | A name must have a UTF-16 form, avoid every character Win32
    -- reserves (@\<@, @>@, @:@, @\"@, @\\@, @|@, @?@, @*@ and the
    -- controls), not name a device, and not end in a dot or a space,
    -- which Win32 strips.  A colon or a backslash would not even fail:
    -- one opens an alternate data stream or a drive-relative path, the
    -- other a separator, so the bytes land somewhere other than the
    -- named entry.  A link target must have a UTF-16 form.
    WindowsNames
  deriving (Eq, Show)

-- | The rules of the host this process runs on.  Decided per operating
-- system, not per volume: a Darwin store on APFS and one on HFS+ both
-- need a name to be UTF-8.  A volume the rules do not describe (FAT
-- mounted on Linux) answers at its own create, and a tree it respells
-- fails the on-disk check that precedes registration.
hostNameRules :: NameRules
hostNameRules = case System.Info.os of
  "mingw32" -> WindowsNames
  "darwin" -> DarwinNames
  _ -> PosixNames

-- ---------------------------------------------------------------------------
-- Decisions
-- ---------------------------------------------------------------------------

-- | Refuse a directory entry name the grammar refuses, or one a host
-- under the given rules cannot hold.
checkEntryName :: NameRules -> ByteString -> Either Text ()
checkEntryName rules name = do
  first T.pack (Stream.checkEntryName Nothing name)
  case rules of
    PosixNames -> Right ()
    DarwinNames -> darwinName
    WindowsNames -> windowsName
  where
    darwinName = case TE.decodeUtf8' name of
      Left _ -> refuse "is not valid UTF-8, which a macOS filesystem cannot hold"
      Right text
        | T.any isNoncharacter text ->
            refuse "holds a Unicode noncharacter, which a macOS filesystem cannot hold"
        | otherwise -> Right ()
    windowsName = case TE.decodeUtf8' name of
      Left _ -> Left (noUtf16Form entryNameNoun name)
      Right _
        | BS.any isWin32Reserved name -> refuse "holds a character Windows does not allow in a name"
        | isReservedDeviceName name -> refuse "is a Windows device name"
        | hasTrailingDotOrSpace name -> refuse "ends in a dot or a space, which Windows strips"
        | otherwise -> Right ()
    refuse reason = Left (entryNameNoun <> " " <> shownBytes name <> " " <> reason)

-- | Refuse a symlink target a host under the given rules cannot hold.
-- POSIX and Darwin hold any bytes; a Windows link target is UTF-16.
checkLinkTarget :: NameRules -> ByteString -> Either Text ()
checkLinkTarget rules target = case (rules, TE.decodeUtf8' target) of
  (WindowsNames, Left _) -> Left (noUtf16Form linkTargetNoun target)
  _ -> Right ()

-- | How a refusal names what it refuses: a directory entry name.
entryNameNoun :: Text
entryNameNoun = "NAR directory entry name"

-- | How a refusal names what it refuses: a symlink target.
linkTargetNoun :: Text
linkTargetNoun = "NAR symlink target"

-- | The refusal of bytes with no UTF-8 reading where names are UTF-16.
noUtf16Form :: Text -> ByteString -> Text
noUtf16Form noun bytes =
  noun <> " " <> shownBytes bytes <> " is not valid UTF-8, so it has no UTF-16 form for Windows"

-- | Bytes as a refusal shows them: quoted text where they read as UTF-8,
-- an escaped literal where they do not, so the message neither drops
-- nor invents a byte.
shownBytes :: ByteString -> Text
shownBytes bytes = case TE.decodeUtf8' bytes of
  Right text -> "'" <> text <> "'"
  Left _ -> T.pack (show bytes)

-- | A Unicode noncharacter: U+FDD0 to U+FDEF, and the last two code
-- points of every plane.  The set is closed by the standard, unlike the
-- unassigned code points APFS also refuses.
isNoncharacter :: Char -> Bool
isNoncharacter c =
  (code >= 0xFDD0 && code <= 0xFDEF) || code `mod` planeSize >= planeSize - 2
  where
    code = ord c
    planeSize = 0x10000

-- | A byte Win32 refuses in a name: the reserved punctuation and the
-- controls.  Each is ASCII, and no byte of a multi-byte UTF-8 sequence
-- is, so the byte test is exact on the UTF-8 name.
isWin32Reserved :: Word8 -> Bool
isWin32Reserved byte = byte < 0x20 || BS.elem byte "<>:\"\\|?*"

-- ---------------------------------------------------------------------------
-- Host paths
-- ---------------------------------------------------------------------------

-- | The host path component spelling an entry name, or the running
-- host's refusal of the name ('checkEntryName' under 'hostNameRules').
entryNamePath :: ByteString -> IO (Either Text OsPath)
entryNamePath name = case checkEntryName hostNameRules name of
  Left refusal -> pure (Left refusal)
  Right () -> spell entryNameNoun name

-- | The host path a symlink is created pointing at, or the running
-- host's refusal of the target ('checkLinkTarget' under
-- 'hostNameRules').
linkTargetPath :: ByteString -> IO (Either Text OsPath)
linkTargetPath target = case checkLinkTarget hostNameRules target of
  Left refusal -> pure (Left refusal)
  Right () -> spell linkTargetNoun target

-- | Spell bytes the host accepted as a host path ('hostPathFromBytes').
-- Only Windows refuses any, and its checks admit only UTF-8, so this
-- refusal is theirs, repeated.
spell :: Text -> ByteString -> IO (Either Text OsPath)
spell noun bytes = pure (maybe (Left (noUtf16Form noun bytes)) Right (hostPathFromBytes bytes))
