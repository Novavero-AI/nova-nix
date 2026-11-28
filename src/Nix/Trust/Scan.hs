{-# LANGUAGE ScopedTypeVariables #-}

-- | Reading what a fetched output's bytes hold, for "Nix.Trust".
--
-- A fetched output is one file or a tree of them.  Each file is read for
-- prebuilt code by its header ('prebuiltKind'): an executable image, an
-- object file, or an ar archive, whose members are not read.  A file that
-- is a tar archive, plain or compressed with gzip, bzip2, xz or zstd, is
-- opened instead, and each regular file in it is read the same way; an
-- archive inside it is not opened, nor reported as not read.  A file of
-- the output in another archive format the scan knows is reported as not
-- read, never as holding no prebuilt code.
-- The bytes say which a file is, never its name: a source archive and a
-- prebuilt package share their suffixes.  A symbolic link is not
-- followed: a target inside the tree is read where it is, and one outside
-- it is not part of the output.
--
-- A file that cannot be read is reported as not read, with why, and the
-- scan goes on with the rest.  A fetched file is read whole; what it
-- decompresses to is read as it streams, and of each member only as far
-- as its header needs, except that xz and bzip2 decode a stream whole,
-- within 'decompressedBudget'.
module Nix.Trust.Scan
  ( scanFetched,
  )
where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as TarEntry
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.Zstd.Lazy as Zstd
import Control.Exception (ErrorCall, Handler (..), IOException, catches, displayException, evaluate, try)
import Data.Bifunctor (bimap)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (find, sort)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import Nix.Builder.Unpack (UnpackLimits (..), defaultUnpackLimits)
import Nix.Compression.Stream (Compression (..), bzip2Reason, compressionOf, xzReason)
import Nix.Trust (CodePlace (..), ContentFacts (..), FoundCode (..), UnreadFile (..), prebuiltKind)
import qualified NovaCache.Bzip2 as Bzip2
import qualified NovaCache.Xz as Xz
import System.Directory (doesDirectoryExist, listDirectory, pathIsSymbolicLink)
import System.FilePath ((</>))

-- ---------------------------------------------------------------------------
-- Formats
-- ---------------------------------------------------------------------------

-- | How a failure names a stream.
codecName :: Compression -> Text
codecName codec = case codec of
  Gzip -> "gzip"
  Zstd -> "zstd"
  Xz -> "xz"
  Bzip2 -> "bzip2"
  Uncompressed -> "file"

-- | Archive and compression formats the scan recognises and does not
-- open, each as the report names a file of it and by the bytes such a
-- file starts with.
unopenedFormats :: [(Text, BS.ByteString)]
unopenedFormats =
  [ ("a zip archive", "PK\x03\x04"),
    ("a zip archive", "PK\x05\x06"),
    ("a zip archive", "PK\x07\x08"),
    ("a 7z archive", BS.pack [0x37, 0x7A, 0xBC, 0xAF, 0x27, 0x1C]),
    ("a rar archive", BS.pack [0x52, 0x61, 0x72, 0x21, 0x1A, 0x07]),
    ("a cabinet archive", "MSCF"),
    ("an rpm package", BS.pack [0xED, 0xAB, 0xEE, 0xDB]),
    ("an lzip stream", "LZIP"),
    ("an lz4 stream", BS.pack [0x04, 0x22, 0x4D, 0x18]),
    ("a compress stream", BS.pack [0x1F, 0x9D]),
    ("an OLE compound file, such as an MSI installer", BS.pack [0xD0, 0xCF, 0x11, 0xE0, 0xA1, 0xB1, 0x1A, 0xE1])
  ]

-- | Where a POSIX or GNU tar header holds its magic.
tarMagicOffset :: Int
tarMagicOffset = 257

-- | The magic of POSIX's ustar format, which GNU tar's own begins with
-- too.
tarMagic :: BL.ByteString
tarMagic = "ustar"

-- | How much an xz or bzip2 stream may decompress to: the budget
-- @builtin:unpack@ extracts within.  Those decoders return the whole
-- output at once, where gzip and zstd stream theirs.
decompressedBudget :: Word64
decompressedBudget = fromIntegral (ulMaxBytes defaultUnpackLimits)

-- | The separator of a path inside a store path, as a NAR and a tar
-- archive write it on every platform.
treeSeparator :: Text
treeSeparator = "/"

-- ---------------------------------------------------------------------------
-- Scanning
-- ---------------------------------------------------------------------------

-- | What a scan finds in one file.
data Finding = Found !FoundCode | Unread !UnreadFile

-- | What the fetched output at this path holds.
scanFetched :: FilePath -> IO ContentFacts
scanFetched root = do
  findings <- scanEntry root []
  pure (ContentFacts [code | Found code <- findings] [unread | Unread unread <- findings])

-- | The findings at a path of the output, given by its components in the
-- tree, none for the output itself.  A directory's entries are read in
-- order of name, so a scan reports its findings in the same order on
-- every run.
scanEntry :: FilePath -> [FilePath] -> IO [Finding]
scanEntry root components = do
  probed <- try ((,) <$> pathIsSymbolicLink here <*> doesDirectoryExist here)
  case probed of
    Left (failure :: IOException) -> pure [unread failure]
    Right (True, _) -> pure []
    Right (False, True) -> do
      listed <- try (listDirectory here)
      case listed of
        Left (failure :: IOException) -> pure [unread failure]
        Right names -> concat <$> traverse (\name -> scanEntry root (components ++ [name])) (sort names)
    Right (False, False) -> scanFile here inTree
  where
    here = foldl' (</>) root components
    inTree = case components of
      [] -> Nothing
      _ -> Just (T.intercalate treeSeparator (map T.pack components))
    unread failure = Unread (UnreadFile inTree (T.pack (displayException failure)))

-- | The findings in one file, named by its path in a fetched tree, or
-- 'Nothing' for a fetched file.  The gzip and zstd decoders decode as the
-- walk reads, and report a corrupt stream by throwing:
-- 'GZip.DecompressError' from gzip, and 'ErrorCall' from zstd, whose
-- lazy decoder raises every decoding error with @error@, as nothing else
-- on this path does.  The walk's result is forced inside the handlers,
-- so no decoding happens after them.
scanFile :: FilePath -> Maybe Text -> IO [Finding]
scanFile path inTree = do
  attempt <- try (BS.readFile path)
  case attempt of
    Left (failure :: IOException) -> pure [unreadBecause (T.pack (displayException failure))]
    Right bytes -> do
      let codec = compressionOf bytes
          streamError detail = Left (codecName codec <> " stream error: " <> T.pack detail)
      decoded <- decode codec bytes
      walked <- case decoded of
        Left reason -> pure (Left reason)
        Right contents ->
          (Right <$> evaluate (codeIn inTree contents))
            `catches` [ Handler (\(e :: GZip.DecompressError) -> pure (streamError (show e))),
                        Handler (\(e :: ErrorCall) -> pure (streamError (show e)))
                      ]
      pure $ case walked of
        Left reason -> [unreadBecause reason]
        Right (Walked code stopped) -> map Found code ++ [unreadBecause reason | Just reason <- [stopped]]
  where
    unreadBecause reason = Unread (UnreadFile inTree reason)

-- | A file's bytes as the scan reads them: decompressed as their codec
-- says, otherwise as they are.  Nothing is decoded here for gzip and
-- zstd, which decode as their output is read.
decode :: Compression -> BS.ByteString -> IO (Either Text BL.ByteString)
decode codec bytes = case codec of
  Gzip -> pure (Right (GZip.decompress whole))
  Zstd -> pure (Right (Zstd.decompress whole))
  Xz -> pure (bimap xzReason BL.fromStrict (Xz.decompress xzLimits bytes))
  Bzip2 -> bimap bzip2Reason BL.fromStrict <$> Bzip2.decompress bzip2Limits bytes
  Uncompressed -> pure (Right whole)
  where
    whole = BL.fromStrict bytes
    xzLimits = Xz.XzLimits {Xz.xzMaxOutputBytes = decompressedBudget, Xz.xzMaxDecoderMemoryBytes = Xz.defaultXzDecoderMemoryBytes}
    bzip2Limits = Bzip2.Bzip2Limits {Bzip2.bzip2MaxOutputBytes = decompressedBudget}

-- | What reading a file's decoded bytes found: the prebuilt code, and why
-- the reading stopped short, if it did.  Its constructor is built only
-- once the walk ends, so forcing it runs the whole walk.
data Walked = Walked ![FoundCode] !(Maybe Text)

-- | The prebuilt code in a file's decoded bytes: in each regular member
-- of a tar archive, or in the bytes themselves.  Bytes in an archive
-- format the scan does not open are not read, with that as the reason.
codeIn :: Maybe Text -> BL.ByteString -> Walked
codeIn inTree contents
  | BL.take (BL.length tarMagic) (BL.drop (fromIntegral tarMagicOffset) contents) == tarMagic =
      membersIn inTree (Tar.decodeLongNames (Tar.read contents))
  | Just (format, _) <- find ((`BL.isPrefixOf` contents) . BL.fromStrict . snd) unopenedFormats =
      Walked [] (Just (format <> ", which the scan does not open"))
  | otherwise = case prebuiltKind contents of
      Just kind -> Walked [FoundCode kind (maybe FetchedFile TreeFile inTree)] Nothing
      Nothing -> Walked [] Nothing

-- | The prebuilt code among a tar archive's regular members, in archive
-- order, and why the archive stops reading, if it does before its end.
-- Each member is read only as far as its header needs, and what it is
-- is forced before the next member is read, so the walk holds no
-- member's bytes past its own.
membersIn ::
  Maybe Text ->
  Tar.GenEntries BL.ByteString FilePath FilePath (Either Tar.FormatError Tar.DecodeLongNamesError) ->
  Walked
membersIn inTree = go []
  where
    go !found entries = case entries of
      Tar.Done -> Walked (reverse found) Nothing
      Tar.Fail failure -> Walked (reverse found) (Just ("tar archive stops reading: " <> T.pack (either show show failure)))
      Tar.Next entry rest -> case memberCode entry of
        Nothing -> go found rest
        Just !code -> go (code : found) rest
    memberCode entry = case TarEntry.entryContent entry of
      Tar.NormalFile content _ -> (\kind -> FoundCode kind (placed (T.pack (TarEntry.entryTarPath entry)))) <$> prebuiltKind content
      _ -> Nothing
    placed member = maybe (ArchiveMember member) (`TreeArchiveMember` member) inTree
