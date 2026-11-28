-- | The entries of a tar archive, read as libarchive's tar reader reads
-- them, the reader upstream's tarball fetcher unpacks with (tarfile.cc at
-- 2.36.0, over libarchive's @archive_read_support_format_tar.c@).
--
-- The reader is a pure machine fed chunks, so an archive is read as it
-- arrives: only a header, or the body of a header that names the next
-- entry, is ever held whole, and a file's contents pass through as slices
-- of the chunks fed.  It reads the formats libarchive reads for source
-- archives: v7, ustar with its prefix, GNU with its long names, and pax
-- extended headers.  It passes over what libarchive passes over: pax
-- global headers, whose attributes it applies to nothing, GNU volume
-- labels and Solaris ACLs.  A sparse member is refused, since nothing
-- here rebuilds its holes.
--
-- Names, link targets and pax values are bytes, as the archive carries
-- them.
module Nix.Fetch.Tar
  ( TarEntry (..),
    TarNode (..),
    TarFailure (..),
    TarStep (..),
    FileStep (..),
    tarReader,
  )
where

import Data.Bits (complement, shiftL, testBit, xor, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int64, Int8)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64, Word8)

-- ---------------------------------------------------------------------------
-- Entries and events
-- ---------------------------------------------------------------------------

-- | One entry, once every header naming it is read.
data TarEntry = TarEntry
  { -- | The path, as the archive names it.
    tarPath :: !ByteString,
    -- | Seconds since the epoch.
    tarMtime :: !Int64,
    -- | The owner's execute bit of the entry's mode.
    tarExecutable :: !Bool,
    -- | What libarchive warns of while reading the entry's headers: the
    -- last warning, as its error string holds only the last.
    tarWarning :: !(Maybe Text)
  }
  deriving (Eq, Show)

-- | An entry that is not a regular file.
data TarNode
  = TarDirectory
  | TarSymlink !ByteString
  | -- | A hard link to the entry the path names.
    TarHardLink !ByteString
  | -- | A device or a FIFO, by the file type libarchive gives it.
    TarSpecial !Int
  deriving (Eq, Show)

-- | Why an archive stopped reading, in libarchive's words.
data TarFailure
  = -- | A header, with the name the entry has so far, if it has one.
    TarHeaderFailure !(Maybe ByteString) !Text
  | -- | A regular file's contents.
    TarDataFailure !Text
  deriving (Eq, Show)

-- | The reader between entries.  Hand 'TarAwait' the next chunk, the
-- empty chunk meaning the end of input.  After 'TarDone' the rest of the
-- input is not read, as libarchive reads nothing past the end-of-archive
-- mark.  The continuations of 'TarFile' and 'TarOther' are lazy, so an
-- entry is handed over before the next is worked out.
data TarStep
  = TarAwait !(ByteString -> TarStep)
  | -- | A regular file of the given size, whose contents follow.
    TarFile !TarEntry !Word64 FileStep
  | TarOther !TarEntry !TarNode TarStep
  | TarDone
  | TarFail !TarFailure

-- | The reader inside a regular file's contents, which pass through as
-- slices of the chunks fed.
data FileStep
  = FileAwait !(ByteString -> FileStep)
  | FileChunk !ByteString FileStep
  | -- | The contents are complete, and the archive goes on.
    FileEnd TarStep
  | FileFail !TarFailure

-- | The reader at the start of an archive.
tarReader :: TarStep
tarReader = headers FirstHeader FormatOld noPending BS.empty

-- ---------------------------------------------------------------------------
-- Layout
-- ---------------------------------------------------------------------------

-- | Every header is one block, and every body is padded to whole blocks.
blockBytes :: Int
blockBytes = 512

-- | The bytes that pad a body of the given size to whole blocks.
paddingOf :: Int64 -> Int64
paddingOf size = negate size .&. fromIntegral (blockBytes - 1)

-- | A header field, by its offset and width in the ustar layout.
data Field = Field !Int !Int

nameField, modeField, uidField, gidField, sizeField, mtimeField, checksumField, linkNameField, magicField, versionField, devMajorField, devMinorField, prefixField :: Field
nameField = Field 0 100
modeField = Field 100 8
uidField = Field 108 8
gidField = Field 116 8
sizeField = Field 124 12
mtimeField = Field 136 12
checksumField = Field 148 8
linkNameField = Field 157 100
magicField = Field 257 6
versionField = Field 263 2
devMajorField = Field 329 8
devMinorField = Field 337 8
prefixField = Field 345 155

-- | Where the type flag is.
typeFlagOffset :: Int
typeFlagOffset = 156

-- | The bytes of a field.
fieldBytes :: Field -> ByteString -> ByteString
fieldBytes (Field offset width) = BS.take width . BS.drop offset

-- | A string field, to its first NUL.
fieldString :: Field -> ByteString -> ByteString
fieldString field = BS.takeWhile (/= 0) . fieldBytes field

-- | A numeric field, as @tar_atol@ reads one.
fieldNumber :: Field -> ByteString -> Int64
fieldNumber field = tarNumber . fieldBytes field

-- | The type flag of a header.
typeFlag :: ByteString -> Word8
typeFlag block = fromMaybe 0 (BS.indexMaybe block typeFlagOffset)

-- | The magic and version a POSIX ustar header carries.
ustarMagic, ustarVersion :: ByteString
ustarMagic = "ustar\0"
ustarVersion = "00"

-- | The magic and version GNU tar writes, eight bytes together.
gnuMagic, gnuVersion :: ByteString
gnuMagic = "ustar "
gnuVersion = " \0"

-- | Which writer a header's magic names, as libarchive tells them apart:
-- GNU's whole eight bytes, then the five that POSIX's begins with.
data Variant = VariantGnu | VariantUstar | VariantOld

variantOf :: ByteString -> Variant
variantOf block
  | hasGnuMagic block = VariantGnu
  | BS.take ustarStemBytes (fieldBytes magicField block) == BS.take ustarStemBytes ustarMagic = VariantUstar
  | otherwise = VariantOld
  where
    ustarStemBytes = 5

hasGnuMagic, hasUstarMagic :: ByteString -> Bool
hasGnuMagic block = fieldBytes magicField block == gnuMagic && fieldBytes versionField block == gnuVersion
hasUstarMagic block = fieldBytes magicField block == ustarMagic && fieldBytes versionField block == ustarVersion

-- | The path a header names: a ustar header's prefix and name joined by a
-- slash unless the prefix ends in one, and otherwise the name.  A GNU
-- header holds times where ustar's prefix is, so it has none.
headerPath :: ByteString -> ByteString
headerPath block = case (variantOf block, BS.unsnoc prefix) of
  (VariantUstar, Just (_, lastByte))
    | lastByte == slash -> prefix <> name
    | otherwise -> BS.snoc prefix slash <> name
  _ -> name
  where
    prefix = fieldString prefixField block
    name = fieldString nameField block

-- ---------------------------------------------------------------------------
-- Numbers
-- ---------------------------------------------------------------------------

-- | A numeric field as @tar_atol@ reads it: base 256 when its first byte
-- has the high bit set, as GNU tar writes a value octal cannot hold, and
-- otherwise octal.
tarNumber :: ByteString -> Int64
tarNumber bytes = case BS.uncons bytes of
  Just (lead, _) | testBit lead 7 -> base256 bytes
  _ -> baseN 8 bytes

-- | @tar_atol_base_n@: leading spaces and tabs, an optional minus sign,
-- then digits to the first byte that is not one, saturating on overflow.
baseN :: Integer -> ByteString -> Int64
baseN base bytes = go 0 digits
  where
    trimmed = BS.dropWhile (\b -> b == space || b == tab) bytes
    (sign, digits) = case BS.uncons trimmed of
      Just (b, rest) | b == minus -> (-1, rest)
      _ -> (1, trimmed)
    go :: Integer -> ByteString -> Int64
    go !acc rest = case BS.uncons rest of
      Just (b, more) | b >= zero && toInteger (b - zero) < base -> step (acc * base + sign * toInteger (b - zero)) more
      _ -> fromInteger acc
    step next more
      | next > toInteger (maxBound :: Int64) = maxBound
      | next < toInteger (minBound :: Int64) = minBound
      | otherwise = go next more

-- | @tar_atol256@: a big-endian two's complement value whose first byte's
-- high bit is the base-256 marker, saturating where it overflows 64 bits.
base256 :: ByteString -> Int64
base256 bytes = case BS.uncons bytes of
  Nothing -> 0
  Just (lead, rest)
    | testBit lead 6 -> fit 0xff (complement 0) (lead .|. 0x80) rest (BS.length bytes)
    | otherwise -> fit 0 0 (lead .&. 0x7f) rest (BS.length bytes)
  where
    fit :: Word8 -> Word64 -> Word8 -> ByteString -> Int -> Int64
    fit signByte start c rest count
      | count > int64Bytes = case BS.uncons rest of
          Just (next, more) | c == signByte -> fit signByte start next more (count - 1)
          _ -> saturated signByte
      | (c `xor` signByte) .&. 0x80 /= 0 = saturated signByte
      | otherwise = fromIntegral (BS.foldl' shiftIn (shiftIn start c) rest)
    saturated signByte = if signByte == 0 then maxBound else minBound
    shiftIn acc b = shiftL acc 8 .|. fromIntegral b
    int64Bytes = 8

-- ---------------------------------------------------------------------------
-- Header checks
-- ---------------------------------------------------------------------------

-- | Whether a block is the end-of-archive mark, all zeros.
nullBlock :: ByteString -> Bool
nullBlock = BS.all (== 0)

-- | libarchive's checksum test: the field holds octal, and equals the sum
-- of the header's bytes with the field read as spaces, the bytes unsigned
-- as POSIX says or signed as some old writers summed them.
checksumValid :: ByteString -> Bool
checksumValid block = BS.all octalOrPadding stored && (expected == unsignedSum || expected == signedSum)
  where
    stored = fieldBytes checksumField block
    octalOrPadding c = c == space || c == 0 || (c >= zero && c <= seven)
    expected = toInteger (baseN 8 stored)
    Field offset width = checksumField
    outside = BS.take offset block <> BS.drop (offset + width) block
    fieldAsSpaces = toInteger width * toInteger space
    unsignedSum = fieldAsSpaces + BS.foldl' (\acc b -> acc + toInteger b) 0 outside
    signedSum = fieldAsSpaces + BS.foldl' (\acc b -> acc + toInteger (fromIntegral b :: Int8)) 0 outside

-- | Whether libarchive's tar bidder takes a header: a valid checksum, a
-- type flag that is NUL or alphanumeric, and number fields that are base
-- 256 or octal with only padding after.
bidsAsTar :: ByteString -> Bool
bidsAsTar block =
  checksumValid block
    && (flag == 0 || isAlphaNumeric flag)
    && all (validNumber . (`fieldBytes` block)) [modeField, uidField, gidField, mtimeField, sizeField, devMajorField, devMinorField]
  where
    flag = typeFlag block
    isAlphaNumeric c = (c >= zero && c <= nine) || (c >= upperA && c <= upperZ) || (c >= lowerA && c <= lowerZ)
    validNumber bytes = case BS.uncons bytes of
      Just (marker, _) | marker == 0x80 || marker == 0xff || marker == 0 -> True
      _ -> BS.all (\c -> c == space || c == 0) (BS.dropWhile (\c -> c >= zero && c <= seven) (BS.dropWhile (== space) bytes))

-- | Whether the bidder scores a block past 50, its threshold for taking
-- the block after a hard link header as the next header: a header it
-- takes, with POSIX's or GNU's magic.
bidsAsHeader :: ByteString -> Bool
bidsAsHeader block = bidsAsTar block && (hasUstarMagic block || hasGnuMagic block)

-- ---------------------------------------------------------------------------
-- The header sequence
-- ---------------------------------------------------------------------------

-- | Whether a header is the archive's first, where libarchive decides the
-- input is a tar archive at all.
data Position = FirstHeader | LaterHeader

-- | The format libarchive records the archive as, as the last header
-- sets it: it decides whether a hard link's size counts.
data Format = FormatOld | FormatUstar | FormatGnu | FormatPax

-- | What the headers before an entry's own have said about it.
data Pending = Pending
  { pendingPath :: !(Maybe ByteString),
    pendingLink :: !(Maybe ByteString),
    pendingSize :: !(Maybe Int64),
    pendingMtime :: !(Maybe Int64),
    pendingSparse :: !Bool,
    pendingWarning :: !(Maybe Text),
    -- | The type flags of the headers seen.
    pendingSeen :: ![Word8],
    -- | Whether a header was seen after which the end of input is a
    -- damaged archive rather than its end: any but a volume label or a
    -- global header.
    pendingStarted :: !Bool
  }

noPending :: Pending
noPending = Pending Nothing Nothing Nothing Nothing False Nothing [] False

-- | Read the headers of the next entry, from what is left of the last
-- chunk.
headers :: Position -> Format -> Pending -> ByteString -> TarStep
headers position format pending leftover = collect blockBytes leftover header short
  where
    short partial
      | BS.null partial && pendingStarted pending = failHeader "Damaged tar archive (end-of-archive within a sequence of headers)"
      | BS.null partial = TarDone
      | FirstHeader <- position = failHeader unrecognized
      | otherwise = failHeader "Truncated tar archive detected while reading next header"
    header block rest
      | nullBlock block = TarDone
      | FirstHeader <- position, not (bidsAsTar block) = failHeader unrecognized
      | not (checksumValid block) = failHeader "Damaged tar archive (bad header checksum)"
      | otherwise = special block rest
    unrecognized = "Unrecognized archive format"
    failHeader = TarFail . TarHeaderFailure (pendingPath pending)
    later = headers LaterHeader
    seen flag = flag `elem` pendingSeen pending
    marked flag =
      pending
        { pendingSeen = flag : pendingSeen pending,
          pendingStarted = pendingStarted pending || (flag /= volumeFlag && flag /= globalFlag)
        }
    warned flag message
      | seen flag = (marked flag) {pendingWarning = Just message}
      | otherwise = marked flag
    metadataShort = failHeader "Truncated archive detected while reading metadata"
    special block rest = case typeFlag block of
      flag
        | flag == aclFlag, seen flag -> failHeader "Redundant 'A' header"
        | flag == aclFlag -> specialBody block rest (const (marked flag)) (later FormatPax)
        | flag == globalFlag, seen flag -> failHeader "Redundant 'g' header"
        | flag == globalFlag -> skipping (padded (bodySize block)) rest (later FormatPax (marked flag)) metadataShort
        | flag == longLinkFlag ->
            specialBody block rest (\body -> (warned flag "Damaged archive: Redundant 'K' headers may cause linknames to be incorrect") {pendingLink = Just (cString body)}) (later FormatGnu)
        | flag == longNameFlag ->
            specialBody block rest (\body -> (warned flag "Damaged archive: Redundant 'L' headers may cause filenames to be incorrect") {pendingPath = Just (cString body)}) (later FormatGnu)
        | flag == volumeFlag, bodySize block < 0 || bodySize block > pathnameLimit -> failHeader "Damaged tar archive (invalid GNU volume header)"
        | flag == volumeFlag -> skipping (padded (bodySize block)) rest (later FormatGnu (warned flag "Redundant 'V' header")) metadataShort
        | flag == sunPaxFlag || flag == paxFlag,
          seen sunPaxFlag || seen paxFlag ->
            failHeader (if flag == paxFlag then "Redundant 'x' header" else "Redundant 'X'/'x' header")
        | flag == sunPaxFlag || flag == paxFlag -> paxHeader block rest (marked flag)
        | otherwise -> entryHeader format pending block rest
    -- The body of a long name, a long link or an ACL, held whole.
    specialBody block rest update next
      | size < 0 || size > entryLimit = failHeader ("Special header has invalid size: " <> T.pack (show size))
      | size > pathnameLimit =
          skipping (padded size) rest (next (update BS.empty) {pendingWarning = Just ("Special header too large: " <> T.pack (show size) <> " > 1MiB")}) metadataShort
      | otherwise = collect (fromIntegral size) rest (\body afterBody -> skipping (paddingOf size) afterBody (next (update body)) metadataShort) (const metadataShort)
      where
        size = bodySize block
    paxHeader block rest marking
      | size < 0 = failHeader ("pax extension header has invalid size: " <> T.pack (show size))
      | size == 0 = failHeader "Invalid empty pax extended header"
      | size > paxBodyLimit =
          skipping (padded size) rest (later FormatPax marking {pendingWarning = Just ("Ignoring oversized pax extensions: " <> T.pack (show size) <> " > " <> T.pack (show paxBodyLimit))}) metadataShort
      | otherwise =
          collect
            (fromIntegral size)
            rest
            (\body afterBody -> skipping (paddingOf size) afterBody (later FormatPax (paxRecords marking body)) metadataShort)
            (const (failHeader "Truncated tar archive detected while reading pax attribute name"))
      where
        size = bodySize block

-- | The size a special header's body declares.
bodySize :: ByteString -> Int64
bodySize = fieldNumber sizeField

-- | A body's size with its padding.
padded :: Int64 -> Int64
padded size = size + paddingOf size

-- | A string to its first NUL, as libarchive copies a header's body.
cString :: ByteString -> ByteString
cString = BS.takeWhile (/= 0)

-- | The longest body libarchive reads for a long name, a long link or a
-- volume label: 1 MiB.
pathnameLimit :: Int64
pathnameLimit = 1048576

-- | The largest pax body libarchive reads, 1 GiB, past which it passes
-- over the body with a warning.
paxBodyLimit :: Int64
paxBodyLimit = 1073741824

-- | The largest entry size libarchive accepts, 2^60 bytes.
entryLimit :: Int64
entryLimit = 0xfffffffffffffff

-- ---------------------------------------------------------------------------
-- pax records
-- ---------------------------------------------------------------------------

-- | Apply a pax body's records, each @LENGTH KEY=VALUE\\n@, as libarchive
-- does: a malformed record warns and ends the reading, and the records
-- before it stay applied.
paxRecords :: Pending -> ByteString -> Pending
paxRecords pending body
  | BS.null body = pending
  | otherwise = case paxRecord body of
      Left message -> pending {pendingWarning = Just message}
      Right (key, value, rest) -> paxRecords (paxAttribute pending key value) rest

-- | The first record of a pax body, and the body after it.  libarchive
-- looks for the length and the key in the record's first 512 bytes.
paxRecord :: ByteString -> Either Text (ByteString, ByteString, ByteString)
paxRecord body = do
  let window = BS.take paxPrologueBytes body
      (digits, afterDigits) = BS.span (\c -> c >= zero && c <= nine) window
  case BS.uncons afterDigits of
    Nothing -> Left "Ignoring malformed pax attributes: overlarge attribute size field"
    Just (c, _) | c /= space -> Left "Ignoring malformed pax attributes: malformed attribute size field"
    _ -> Right ()
  let lineLength = baseN 10 digits
      keyStart = BS.length digits + 1
  if lineLength > fromIntegral (BS.length body)
    then Left ("Ignoring malformed pax attribute:  " <> T.pack (show lineLength) <> " > " <> T.pack (show (BS.length body)))
    else Right ()
  let line = BS.take (fromIntegral lineLength) body
      keyArea = BS.drop keyStart (BS.take (fromIntegral lineLength) window)
  case BS.uncons keyArea of
    Just (c, _) | c /= equals -> Right ()
    _ -> Left "Ignoring malformed pax attributes: empty name found"
  case BS.elemIndex equals keyArea of
    Nothing -> Left "Ignoring malformed pax attributes: overlarge attribute name"
    Just keyLength -> case BS.unsnoc (BS.drop (keyStart + keyLength + 1) line) of
      Just (value, lastByte)
        | lastByte == lineFeed -> Right (BS.take keyLength keyArea, value, BS.drop (fromIntegral lineLength) body)
      _ -> Left "Malformed pax attributes"

-- | The most of a record libarchive reads to find its length and key.
paxPrologueBytes :: Int
paxPrologueBytes = 512

-- | One pax attribute, for the keys that decide what an entry is.
paxAttribute :: Pending -> ByteString -> ByteString -> Pending
paxAttribute pending key value
  | key == "path" = pending {pendingPath = Just (cString value)}
  | key == "linkpath" = pending {pendingLink = Just (cString value)}
  | key == "size" = pending {pendingSize = Just (baseN 10 value)}
  | key == "mtime" = pending {pendingMtime = Just (baseN 10 value)}
  | "GNU.sparse." `BS.isPrefixOf` key = pending {pendingSparse = True}
  | otherwise = pending

-- ---------------------------------------------------------------------------
-- An entry's own header
-- ---------------------------------------------------------------------------

-- | Read an entry's own header, with what the headers before it said, and
-- go on past its body.
entryHeader :: Format -> Pending -> ByteString -> ByteString -> TarStep
entryHeader format pending block rest
  | pendingSparse pending || flag == sparseFlag = failWith "sparse archive members are not supported"
  | size < 0 = failWith "Tar entry has negative file size"
  | size > entryLimit = failWith "Tar entry size overflow"
  | otherwise = case node of
      Nothing -> TarFile entry (fromIntegral size) (fileContents size rest (\after -> skipping (paddingOf size) after next truncatedData))
      Just other -> TarOther entry other $ case other of
        TarHardLink _ -> hardLinkBody
        TarDirectory | flag == dumpDirFlag -> skipping (padded size) rest next truncatedData
        _ -> next rest
  where
    flag = typeFlag block
    headerFormat = case (variantOf block, format) of
      (VariantGnu, _) -> FormatGnu
      (VariantUstar, FormatPax) -> FormatPax
      (VariantUstar, _) -> FormatUstar
      (VariantOld, _) -> FormatOld
    path = case pendingPath pending of
      Just named | not (BS.null named) -> named
      _ -> headerPath block
    link = case pendingLink pending of
      Just named | not (BS.null named) -> named
      _ -> fieldString linkNameField block
    size = fromMaybe (fieldNumber sizeField block) (pendingSize pending)
    -- libarchive reads a regular entry whose name ends in a slash as a
    -- directory, with no body, when its headers gave no warning.
    endsInSlash = maybe False ((== slash) . snd) (BS.unsnoc path)
    -- What the entry is, when it is not a regular file.
    node
      | flag == hardLinkFlag = Just (TarHardLink link)
      | flag == symlinkFlag = Just (TarSymlink link)
      | flag == charDeviceFlag = Just (TarSpecial characterDeviceType)
      | flag == blockDeviceFlag = Just (TarSpecial blockDeviceType)
      | flag == directoryFlag || flag == dumpDirFlag = Just TarDirectory
      | flag == fifoFlag = Just (TarSpecial fifoType)
      | endsInSlash && isNothing (pendingWarning pending) = Just TarDirectory
      | otherwise = Nothing
    entry =
      TarEntry
        { tarPath = path,
          tarMtime = fromMaybe (fieldNumber mtimeField block) (pendingMtime pending),
          tarExecutable = testBit (fieldNumber modeField block) ownerExecuteBit,
          tarWarning = pendingWarning pending
        }
    next = headers LaterHeader headerFormat noPending
    -- A hard link's size counts in a pax archive and not in a GNU or old
    -- one.  In a ustar archive libarchive takes the block after the
    -- header as the next header when the bidder takes it, and otherwise
    -- counts the size.
    hardLinkBody
      | size == 0 = next rest
      | otherwise = case headerFormat of
          FormatPax -> skipBody rest
          FormatGnu -> next rest
          FormatOld -> next rest
          FormatUstar -> collect blockBytes rest (\ahead afterAhead -> if bidsAsHeader ahead then next (ahead <> afterAhead) else skipBody (ahead <> afterAhead)) skipBody
    skipBody bytes = skipping (padded size) bytes next truncatedData
    failWith = TarFail . TarHeaderFailure (Just path)
    truncatedData = TarFail (TarDataFailure truncatedDataMessage)

-- | The owner's execute bit of a mode.
ownerExecuteBit :: Int
ownerExecuteBit = 6

-- | libarchive's file types for the entries upstream refuses, as its
-- refusal prints them.
characterDeviceType, blockDeviceType, fifoType :: Int
characterDeviceType = 0o020000
blockDeviceType = 0o060000
fifoType = 0o010000

-- ---------------------------------------------------------------------------
-- Reading bytes
-- ---------------------------------------------------------------------------

-- | Collect exactly @n@ bytes from what is left of the last chunk and then
-- from chunks fed, handing them on with what follows them, or handing on
-- what was collected when the input ends first.
collect :: Int -> ByteString -> (ByteString -> ByteString -> TarStep) -> (ByteString -> TarStep) -> TarStep
collect n leftover got short
  | BS.length leftover >= n = uncurry got (BS.splitAt n leftover)
  | otherwise = go [leftover] (BS.length leftover)
  where
    go held have = TarAwait (fed held have)
    fed held have chunk
      | BS.null chunk = short (BS.concat (reverse held))
      | have + BS.length chunk >= n = uncurry got (BS.splitAt n (BS.concat (reverse (chunk : held))))
      | otherwise = go (chunk : held) (have + BS.length chunk)

-- | Pass over @n@ bytes, then go on with what follows them, or fail when
-- the input ends first.
skipping :: Int64 -> ByteString -> (ByteString -> TarStep) -> TarStep -> TarStep
skipping n leftover next truncated = go n leftover
  where
    go owed bytes
      | owed <= fromIntegral (BS.length bytes) = next (BS.drop (fromIntegral owed) bytes)
      | otherwise = TarAwait $ \chunk ->
          if BS.null chunk then truncated else go (owed - fromIntegral (BS.length bytes)) chunk

-- | A file's @n@ bytes of contents as they arrive, then the archive
-- after them, or a failure when the input ends first.
fileContents :: Int64 -> ByteString -> (ByteString -> TarStep) -> FileStep
fileContents n leftover next = go n leftover
  where
    go owed bytes
      | owed <= fromIntegral (BS.length bytes) =
          let (slice, rest) = BS.splitAt (fromIntegral owed) bytes
           in chunk slice (FileEnd (next rest))
      | otherwise = chunk bytes (FileAwait (fed (owed - fromIntegral (BS.length bytes))))
    fed owed bytes
      | BS.null bytes = FileFail (TarDataFailure truncatedDataMessage)
      | otherwise = go owed bytes
    chunk slice after
      | BS.null slice = after
      | otherwise = FileChunk slice after

-- | libarchive's words for an archive that ends inside an entry's body.
truncatedDataMessage :: Text
truncatedDataMessage = "Truncated tar archive detected while reading data"

-- ---------------------------------------------------------------------------
-- Bytes
-- ---------------------------------------------------------------------------

space, tab, minus, zero, seven, nine, equals, lineFeed, slash, upperA, upperZ, lowerA, lowerZ :: Word8
space = 0x20
tab = 0x09
minus = 0x2d
zero = 0x30
seven = 0x37
nine = 0x39
equals = 0x3d
lineFeed = 0x0a
slash = 0x2f
upperA = 0x41
upperZ = 0x5a
lowerA = 0x61
lowerZ = 0x7a

-- | The type flags the reader acts on.
hardLinkFlag, symlinkFlag, charDeviceFlag, blockDeviceFlag, directoryFlag, fifoFlag, dumpDirFlag, sparseFlag, aclFlag, globalFlag, longLinkFlag, longNameFlag, volumeFlag, sunPaxFlag, paxFlag :: Word8
hardLinkFlag = 0x31
symlinkFlag = 0x32
charDeviceFlag = 0x33
blockDeviceFlag = 0x34
directoryFlag = 0x35
fifoFlag = 0x36
dumpDirFlag = 0x44
sparseFlag = 0x53
aclFlag = 0x41
globalFlag = 0x67
longLinkFlag = 0x4b
longNameFlag = 0x4c
volumeFlag = 0x56
sunPaxFlag = 0x58
paxFlag = 0x78
