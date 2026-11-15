{-# LANGUAGE TupleSections #-}

-- | What a derivation's closure relies on: its fixed-output derivations,
-- which of them its builds run as builders and which hold prebuilt code,
-- and the builders and sources it takes from outside itself.
--
-- A fixed-output derivation's output is held to the hash its recipe
-- states, whatever makes it: its bytes are what the hash says, from a
-- download, a cache or a build.  Every one in the closure is an input
-- the closure relies on ('fixedOutputInputs'), whether a builtin fetches
-- it or the closure builds it, and each says which ('Provenance').
--
-- Each fixed-output input is classified by two kinds of fact.  One is in
-- the graph: a derivation whose builder is inside the input's output runs
-- it as a builder, and so does one whose builder is inside the output of
-- a builtin derivation (@builtin:unpack@, say) made from it, where the
-- input is part of the tree the builder runs from; which of the
-- builtin's inputs holds the builder itself is not in the graph.  A
-- builtin runs nothing from the closure, so neither case needs the bytes.
-- The other fact is in the bytes: an input whose files hold prebuilt code
-- ('prebuiltKind'), an executable image, an object file or a static
-- library, holds what no step of the closure built from source.  The
-- content facts are an argument, keyed by output path, so classification
-- runs over a pure graph and pure facts; "Nix.Trust.Scan" gathers them
-- from the store.
--
-- The rest is read from the graph alone: builders the closure does not
-- contain, such as a program on the host; builders in its input sources;
-- the builtins nova-nix runs itself; and the input sources.  Nothing here
-- says whether any of it is safe.
module Nix.Trust
  ( -- * Prebuilt code
    CodeKind (..),
    prebuiltKind,
    codeKindName,

    -- * Content facts
    CodePlace (..),
    FoundCode (..),
    UnreadFile (..),
    ContentFacts (..),

    -- * The closure
    fixedOutputPath,
    fixedOutputInputs,
    fetchedByBuiltin,

    -- * The report
    Scan (..),
    Provenance (..),
    FixedInput (..),
    InputClass (..),
    inputClass,
    TrustReport (..),
    trustReport,
    renderTrustReport,
  )
where

import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import Data.Word (Word32, Word8)
import Nix.DependencyGraph (DepGraph (..), DepNode (..), transitiveDeps)
import Nix.Derivation (Derivation (..), DerivationOutput (..))
import Nix.Store.Path (StorePath (spName), defaultStoreDir, defaultStoreDirText, parseStorePathBaseName, storePathToText)

-- ---------------------------------------------------------------------------
-- Prebuilt code
-- ---------------------------------------------------------------------------

-- | The kinds of prebuilt code the scan recognises, each by its header.
data CodeKind
  = -- | A Portable Executable, Windows' format: an @MZ@ header whose
    -- @e_lfanew@ field points at a @PE\\0\\0@ signature.
    PeImage
  | -- | An ELF executable or shared object.
    ElfImage
  | -- | An ELF relocatable object.
    ElfObject
  | -- | Another ELF file, such as a core dump, or one whose header ends
    -- before its type.
    ElfOther
  | -- | A Mach-O file for one architecture, of either width and byte
    -- order.
    MachO
  | -- | A Mach-O universal file, holding code for several architectures.
    MachOUniversal
  | -- | A COFF object, in the plain form or the bigobj one.
    CoffObject
  | -- | An ar archive: a static library or an import library, whose
    -- members are not read.
    ArArchive
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | How the report names a kind.
codeKindName :: CodeKind -> Text
codeKindName kind = case kind of
  PeImage -> "PE image"
  ElfImage -> "ELF image"
  ElfObject -> "ELF object"
  ElfOther -> "ELF"
  MachO -> "Mach-O"
  MachOUniversal -> "Mach-O universal"
  CoffObject -> "COFF object"
  ArArchive -> "ar archive"

-- | The prebuilt code these bytes are, read from their header, or
-- 'Nothing'.  A script, whatever its @#!@ line names, is not prebuilt
-- code, and neither is an @MZ@ header without a PE signature where its
-- @e_lfanew@ points, nor a COFF header whose tables do not fit in the
-- bytes.  Lazy bytes are read only as far as the header needs: the first
-- few bytes, for an @MZ@ header the bytes up to the signature its
-- @e_lfanew@ names, and for a COFF header the bytes up to the end of its
-- tables.
prebuiltKind :: BL.ByteString -> Maybe CodeKind
prebuiltKind bytes
  | elfMagic `BL.isPrefixOf` bytes = Just (elfKind bytes)
  | any (`BL.isPrefixOf` bytes) machOMagics = Just MachO
  | any (`BL.isPrefixOf` bytes) universalMagics,
    Just count <- fieldAt 4 BigEndian universalCountOffset bytes,
    count > 0,
    count < javaClassFirstMajor =
      Just MachOUniversal
  | arMagic `BL.isPrefixOf` bytes = Just ArArchive
  | dosMagic `BL.isPrefixOf` bytes,
    Just offset <- fieldAt 4 LittleEndian peOffsetField bytes,
    peSignature `BL.isPrefixOf` BL.drop (fromIntegral offset) bytes =
      Just PeImage
  | isCoffObject bytes || isBigObj bytes = Just CoffObject
  | otherwise = Nothing

-- | The order of a multi-byte field's bytes.
data ByteOrder = BigEndian | LittleEndian

-- | The field of this many bytes at an offset, or 'Nothing' past the end.
fieldAt :: Int64 -> ByteOrder -> Int64 -> BL.ByteString -> Maybe Word32
fieldAt width order offset bytes
  | BL.length field == width = Just (BL.foldl' (\acc byte -> acc `shiftL` 8 .|. fromIntegral byte) 0 (mostSignificantFirst field))
  | otherwise = Nothing
  where
    field = BL.take width (BL.drop offset bytes)
    mostSignificantFirst = case order of
      BigEndian -> id
      LittleEndian -> BL.reverse

-- | Whether the bytes run to at least this length, read only that far.
reaches :: BL.ByteString -> Int64 -> Bool
reaches bytes size = BL.length (BL.take size bytes) == size

-- | ELF's identification bytes.
elfMagic :: BL.ByteString
elfMagic = BL.pack [0x7F, 0x45, 0x4C, 0x46]

-- | An ELF file's kind, from its type field, read in the byte order its
-- identification names.
elfKind :: BL.ByteString -> CodeKind
elfKind bytes = case BL.indexMaybe bytes elfDataOffset >>= orderOf >>= \order -> fieldAt 2 order elfTypeOffset bytes of
  Just elfType
    | elfType == elfRelocatable -> ElfObject
    | elfType == elfExecutable || elfType == elfShared -> ElfImage
  _ -> ElfOther
  where
    orderOf :: Word8 -> Maybe ByteOrder
    orderOf encoding
      | encoding == elfLittleEndian = Just LittleEndian
      | encoding == elfBigEndian = Just BigEndian
      | otherwise = Nothing

-- | Where ELF's identification names the byte order, and the two values
-- it takes there.
elfDataOffset :: Int64
elfDataOffset = 5

elfLittleEndian, elfBigEndian :: Word8
elfLittleEndian = 1
elfBigEndian = 2

-- | Where an ELF header holds its type, and the types of a relocatable,
-- an executable and a shared object.
elfTypeOffset :: Int64
elfTypeOffset = 16

elfRelocatable, elfExecutable, elfShared :: Word32
elfRelocatable = 1
elfExecutable = 2
elfShared = 3

-- | Mach-O's magic numbers as a file starts with them: 32-bit and 64-bit,
-- each in big-endian and little-endian order.
machOMagics :: [BL.ByteString]
machOMagics =
  map
    BL.pack
    [ [0xFE, 0xED, 0xFA, 0xCE],
      [0xCE, 0xFA, 0xED, 0xFE],
      [0xFE, 0xED, 0xFA, 0xCF],
      [0xCF, 0xFA, 0xED, 0xFE]
    ]

-- | The universal header's magic numbers, 32-bit and 64-bit, which the
-- format writes big-endian.
universalMagics :: [BL.ByteString]
universalMagics = map BL.pack [[0xCA, 0xFE, 0xBA, 0xBE], [0xCA, 0xFE, 0xBA, 0xBF]]

-- | Where the universal header holds its count of architectures.
universalCountOffset :: Int64
universalCountOffset = 4

-- | A Java class file starts with the universal header's 32-bit magic,
-- and where that header counts its architectures holds its minor and
-- major version, two bytes each, so the word is at least the first major
-- version, 45, where no universal file has that many architectures.
javaClassFirstMajor :: Word32
javaClassFirstMajor = 45

-- | An ar archive's global header.
arMagic :: BL.ByteString
arMagic = "!<arch>\n"

-- | The DOS header's magic, which every Portable Executable starts with.
dosMagic :: BL.ByteString
dosMagic = "MZ"

-- | Where the DOS header holds @e_lfanew@, the offset of the PE
-- signature.
peOffsetField :: Int64
peOffsetField = 0x3C

-- | The signature a Portable Executable's NT headers start with.
peSignature :: BL.ByteString
peSignature = BL.pack [0x50, 0x45, 0x00, 0x00]

-- | The machines a COFF object is written for here: i386, x86-64, ARM64
-- and ARM Thumb-2, as the machine field spells them.
coffMachines :: [Word32]
coffMachines = [0x014C, 0x8664, 0xAA64, 0x01C4]

-- | The size of a COFF file header, of a section header, and of a symbol
-- table entry in the plain form and in the bigobj one.
coffHeaderSize, coffSectionSize, coffSymbolSize, bigObjHeaderSize, bigObjSymbolSize :: Int64
coffHeaderSize = 20
coffSectionSize = 40
coffSymbolSize = 18
bigObjHeaderSize = 56
bigObjSymbolSize = 20

-- | Whether bytes start a COFF object in the plain form: a machine field
-- this scan knows, at least one section, no optional header, which only
-- an image has, and a section table and symbol table that end within
-- the bytes.  Without a symbol table its pointer is zero.
isCoffObject :: BL.ByteString -> Bool
isCoffObject bytes = fromMaybe False $ do
  machine <- fieldAt 2 LittleEndian 0 bytes
  sections <- fieldAt 2 LittleEndian 2 bytes
  symbolTable <- fieldAt 4 LittleEndian 8 bytes
  symbols <- fieldAt 4 LittleEndian 12 bytes
  optionalHeader <- fieldAt 2 LittleEndian 16 bytes
  pure
    ( machine `elem` coffMachines
        && optionalHeader == 0
        && tablesFit bytes coffHeaderSize sections coffSymbolSize symbolTable symbols
    )

-- | The first two words of a bigobj header, which no plain header has:
-- an unknown machine, then all bits set.
bigObjSignature :: BL.ByteString
bigObjSignature = BL.pack [0x00, 0x00, 0xFF, 0xFF]

-- | The bigobj form's first version, and its class identifier, which
-- follows the machine and the time stamp.
bigObjVersion :: Word32
bigObjVersion = 2

bigObjClassId :: BL.ByteString
bigObjClassId = BL.pack [0xC7, 0xA1, 0xBA, 0xD1, 0xEE, 0xBA, 0xA9, 0x4B, 0xAF, 0x20, 0xFA, 0xF6, 0x6A, 0xA4, 0xDC, 0xB8]

-- | Where the bigobj header holds its class identifier.
bigObjClassIdOffset :: Int64
bigObjClassIdOffset = 12

-- | Whether bytes start a COFF object in the bigobj form: its signature,
-- a version from the first bigobj one on, a machine this scan knows, its
-- class identifier, at least one section, and a section table and
-- symbol table that end within the bytes.
isBigObj :: BL.ByteString -> Bool
isBigObj bytes = fromMaybe False $ do
  version <- fieldAt 2 LittleEndian 4 bytes
  machine <- fieldAt 2 LittleEndian 6 bytes
  sections <- fieldAt 4 LittleEndian 44 bytes
  symbolTable <- fieldAt 4 LittleEndian 48 bytes
  symbols <- fieldAt 4 LittleEndian 52 bytes
  pure
    ( bigObjSignature `BL.isPrefixOf` bytes
        && version >= bigObjVersion
        && machine `elem` coffMachines
        && bigObjClassId `BL.isPrefixOf` BL.drop bigObjClassIdOffset bytes
        && tablesFit bytes bigObjHeaderSize sections bigObjSymbolSize symbolTable symbols
    )

-- | Whether a COFF object's section table, at least one section long
-- after its header, and its symbol table end within the bytes.
tablesFit :: BL.ByteString -> Int64 -> Word32 -> Int64 -> Word32 -> Word32 -> Bool
tablesFit bytes headerSize sections symbolSize symbolTable symbols =
  sections > 0
    && reaches bytes (headerSize + coffSectionSize * fromIntegral sections)
    && symbolsFit
  where
    symbolsFit
      | symbolTable == 0 = symbols == 0
      | otherwise = reaches bytes (fromIntegral symbolTable + symbolSize * fromIntegral symbols)

-- ---------------------------------------------------------------------------
-- Content facts
-- ---------------------------------------------------------------------------

-- | Where in a fetched output a file of prebuilt code is.  Paths are text
-- for the report to show.
data CodePlace
  = -- | The output is one file, and it is the code.
    FetchedFile
  | -- | A file of a fetched tree, by its path in the tree.
    TreeFile !Text
  | -- | A member of the fetched file, an archive, by its path in it.
    ArchiveMember !Text
  | -- | A member of an archive in a fetched tree: the archive's path in
    -- the tree, then the member's path in the archive.
    TreeArchiveMember !Text !Text
  deriving (Eq, Ord, Show)

-- | A file of prebuilt code a scan found.
data FoundCode = FoundCode
  { fcKind :: !CodeKind,
    fcPlace :: !CodePlace
  }
  deriving (Eq, Ord, Show)

-- | A file a scan did not read in full, and why: one that could not be
-- opened, a compressed stream that does not decode, a tar archive that
-- stops reading, or an archive in a format the scan does not open.
data UnreadFile = UnreadFile
  { -- | Its path in a fetched tree, or 'Nothing' for a fetched file.
    ufFile :: !(Maybe Text),
    ufReason :: !Text
  }
  deriving (Eq, Show)

-- | What a scan of a fetched output found in its bytes: the files of
-- prebuilt code, and the files it did not read in full.  Both empty
-- means every file was read and none is prebuilt code.
data ContentFacts = ContentFacts
  { cfCode :: ![FoundCode],
    cfUnread :: ![UnreadFile]
  }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- The closure
-- ---------------------------------------------------------------------------

-- | The name of the one output a fixed-output derivation has.
fixedOutputName :: Text
fixedOutputName = "out"

-- | What an impure derivation's output records as its hash, upstream's
-- @DerivationOutput::Impure@ in a @.drv@.
impureOutputHash :: Text
impureOutputHash = "impure"

-- | The output of a fixed-output derivation: its one output, @out@, with
-- the hash that fixes it.  A floating content-addressed output names an
-- algorithm and no hash, and an impure one upstream's @impure@ marker;
-- neither is fixed.
fixedOutput :: Derivation -> Maybe DerivationOutput
fixedOutput drv = case drvOutputs drv of
  [out]
    | doName out == fixedOutputName,
      not (T.null (doHashAlgo out)),
      not (T.null (doHash out)),
      doHash out /= impureOutputHash ->
        Just out
  _ -> Nothing

-- | The store path of a fixed-output derivation's output ('fixedOutput').
fixedOutputPath :: Derivation -> Maybe StorePath
fixedOutputPath = fmap doPath . fixedOutput

-- | The derivations of the root's closure, keyed by @.drv@ path, and
-- among them the fixed-output ones, each with its fixed output.
data Walk = Walk
  { walkNodes :: !(Map StorePath DepNode),
    walkFixed :: !(Map StorePath (DepNode, DerivationOutput))
  }

-- | The derivations of the root's closure: the root and every derivation
-- it depends on, directly or not, that the graph holds.
walkClosure :: DepGraph -> StorePath -> Walk
walkClosure graph root = Walk nodes (Map.mapMaybe (\node -> (node,) <$> fixedOutput (dnDerivation node)) nodes)
  where
    nodes = Map.restrictKeys (unDepGraph graph) (Set.insert root (transitiveDeps graph root))

-- | The fixed-output derivations of the root's closure, each with its
-- output, in @.drv@ path order.
fixedOutputInputs :: DepGraph -> StorePath -> [(DepNode, StorePath)]
fixedOutputInputs graph root = [(node, doPath out) | (node, out) <- Map.elems (walkFixed (walkClosure graph root))]

-- | What every builder of a builtin derivation starts with: upstream's
-- marker for a builder the Nix process runs itself.
builtinBuilderPrefix :: ByteString
builtinBuilderPrefix = "builtin:"

-- | Whether a derivation's builder is one nova-nix runs itself.
isBuiltin :: Derivation -> Bool
isBuiltin drv = builtinBuilderPrefix `BS.isPrefixOf` drvBuilder drv

-- | Whether realising a derivation runs nothing but nova-nix: its builder
-- is a builtin, and it takes no input derivation or source, so nothing
-- from the closure is built or read first.
fetchedByBuiltin :: Derivation -> Bool
fetchedByBuiltin drv = isBuiltin drv && Map.null (drvInputDrvs drv) && null (drvInputSrcs drv)

-- | The separator after the store directory in a canonical store path.
pathSeparator :: Word8
pathSeparator = 0x2F

-- | The store path a builder is inside: its first component under the
-- canonical store directory.  A store path's own component is ASCII, so
-- only it is decoded, and a builder whose path within the store path is
-- not UTF-8 still names the store path it is in.
builderStorePath :: ByteString -> Maybe StorePath
builderStorePath builder = do
  rest <- BS.stripPrefix (TE.encodeUtf8 defaultStoreDirText <> BS.singleton pathSeparator) builder
  either (const Nothing) parseStorePathBaseName (TE.decodeUtf8' (BS.takeWhile (/= pathSeparator) rest))

-- | Where a derivation's builder comes from.
data BuilderUse
  = -- | It is one of nova-nix's builtins.
    BuiltinUse
  | -- | It is inside the output of a derivation of the closure, whose
    -- @.drv@ path is given.
    OutputUse !StorePath
  | -- | It is inside one of the closure's input sources.
    SourceUse
  | -- | It is not in the closure: a path outside the store, or a store
    -- path nothing in the closure provides.
    OutsideUse

-- | Where a derivation's builder comes from, given which @.drv@ makes
-- each output of the closure, and the closure's input sources.
builderUse :: Map StorePath StorePath -> Set StorePath -> Derivation -> BuilderUse
builderUse owners sources drv
  | isBuiltin drv = BuiltinUse
  | Just path <- builderStorePath (drvBuilder drv) =
      case Map.lookup path owners of
        Just owner -> OutputUse owner
        Nothing
          | Set.member path sources -> SourceUse
          | otherwise -> OutsideUse
  | otherwise = OutsideUse

-- | How a builder reaches a fixed-output input it runs.
data Reach
  = -- | The builder is inside the input's own output.
    Inside
  | -- | The builder is inside a builtin derivation's output made from the
    -- input, with any others.
    InTree
  deriving (Eq, Ord, Show)

-- | The fixed-output derivations whose bytes the output of this @.drv@
-- holds as their hash pins them, each with how: itself when it is one,
-- and for a builtin derivation each fixed-output input of it and of the
-- builtin derivations beneath it.  Any other derivation runs a program to
-- make an output no hash fixes, and the chain stops there.
heldAsPinned :: Walk -> StorePath -> [(StorePath, Reach)]
heldAsPinned walk start
  | Map.member start (walkFixed walk) = [(start, Inside)]
  | otherwise = map (,InTree) (Set.toList (go Set.empty Set.empty [start]))
  where
    go !found !_ [] = found
    go found seen (path : rest)
      | Set.member path seen = go found seen rest
      | Map.member path (walkFixed walk) = go (Set.insert path found) (Set.insert path seen) rest
      | Just node <- Map.lookup path (walkNodes walk),
        isBuiltin (dnDerivation node) =
          go found (Set.insert path seen) (Map.keys (dnDeps node) ++ rest)
      | otherwise = go found (Set.insert path seen) rest

-- ---------------------------------------------------------------------------
-- The report
-- ---------------------------------------------------------------------------

-- | What is known of a fixed-output input's bytes.
data Scan
  = -- | The store does not hold its output, so its bytes were not read.
    NotInStore
  | -- | Its output was scanned, and this is what the scan found.
    Scanned !ContentFacts
  deriving (Eq, Show)

-- | How a fixed-output input's bytes come to be, each case with its
-- builder.  Either way the hash pins them.
data Provenance
  = -- | A builtin fetches them, such as @builtin:fetchurl@.
    FetchedBy !ByteString
  | -- | The closure builds them, running this builder.
    BuiltBy !ByteString
  deriving (Eq, Show)

-- | A fixed-output derivation of the closure.
data FixedInput = FixedInput
  { -- | Its @.drv@ path.
    inputDrvPath :: !StorePath,
    -- | Its one output, which the hash fixes.
    inputOutput :: !StorePath,
    inputProvenance :: !Provenance,
    -- | The outputs of the closure's other fixed-output derivations whose
    -- hash, algorithm and mode included, is this one's, so whose bytes
    -- are the same, by output name.
    inputSameHash :: ![StorePath],
    -- | The builders inside its output, each with how many of the
    -- closure's derivations it builds.
    inputBuilders :: !(Map ByteString Int),
    -- | The builders inside a builtin derivation's output made from it,
    -- the tree they run from, each with how many derivations it builds.
    inputTreeBuilders :: !(Map ByteString Int),
    inputScan :: !Scan
  }
  deriving (Eq, Show)

-- | The class a fixed-output input falls in, most opaque first.
data InputClass
  = -- | A build runs it as its builder, or as part of its builder's tree
    -- ('inputBuilders', 'inputTreeBuilders'), whatever its bytes.
    RunAsBuilder
  | -- | It holds prebuilt code.
    HoldsPrebuiltCode
  | -- | Its bytes were not read, or not all of them, and no prebuilt
    -- code was found in what was.
    NotFullyRead
  | -- | Every file was read, and none is prebuilt code.
    HoldsNoPrebuiltCode
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The class a fixed-output input falls in: the structural fact first,
-- since it needs no bytes, then what its bytes hold.
inputClass :: FixedInput -> InputClass
inputClass input
  | not (Map.null (inputBuilders input) && Map.null (inputTreeBuilders input)) = RunAsBuilder
  | otherwise = case inputScan input of
      NotInStore -> NotFullyRead
      Scanned facts -> case (cfCode facts, cfUnread facts) of
        (_ : _, _) -> HoldsPrebuiltCode
        ([], _ : _) -> NotFullyRead
        ([], []) -> HoldsNoPrebuiltCode

-- | What a closure relies on.
data TrustReport = TrustReport
  { -- | The fixed-output inputs, by output name.
    trInputs :: ![FixedInput],
    -- | The builders the closure does not contain, each with how many of
    -- its derivations it builds.
    trOutsideBuilders :: !(Map ByteString Int),
    -- | The builders inside input sources, each with how many derivations
    -- it builds.
    trSourceBuilders :: !(Map ByteString Int),
    -- | The builtin builders, each with how many derivations it builds.
    trBuiltinBuilders :: !(Map ByteString Int),
    -- | The input sources: store paths the closure's derivations take
    -- that none of them builds, such as files evaluation copies from the
    -- expression's tree.
    trSources :: !(Set StorePath)
  }
  deriving (Eq, Show)

-- | What the closure of the root relies on, given what scans found in the
-- outputs of its fixed-output inputs, keyed by output path.  An input
-- whose output the facts do not hold is not in the store.  Every
-- derivation of the closure counts, a fixed-output one included: a fixed
-- output built by running a fetched seed shows the seed run as its
-- builder.
trustReport :: Map StorePath ContentFacts -> DepGraph -> StorePath -> TrustReport
trustReport facts graph root =
  TrustReport
    { trInputs = sortOn (\input -> (spName (inputOutput input), inputOutput input)) (map inputFor (Map.toList (walkFixed walk))),
      trOutsideBuilders = tally [drvBuilder drv | (drv, OutsideUse) <- uses],
      trSourceBuilders = tally [drvBuilder drv | (drv, SourceUse) <- uses],
      trBuiltinBuilders = tally [drvBuilder drv | (drv, BuiltinUse) <- uses],
      trSources = sources
    }
  where
    walk = walkClosure graph root
    closureDrvs = map dnDerivation (Map.elems (walkNodes walk))
    sources = Set.fromList (concatMap drvInputSrcs closureDrvs)
    owners = Map.fromList [(doPath out, path) | (path, node) <- Map.toList (walkNodes walk), out <- drvOutputs (dnDerivation node)]
    uses = [(drv, builderUse owners sources drv) | drv <- closureDrvs]
    runs =
      Map.fromListWith
        (Map.unionWith (+))
        [((input, reach), Map.singleton (drvBuilder drv) 1) | (drv, OutputUse owner) <- uses, (input, reach) <- heldAsPinned walk owner]
    runsBy reach path = Map.findWithDefault Map.empty (path, reach) runs
    pinnedBy out = (doHashAlgo out, doHash out)
    byHash = Map.fromListWith (++) [(pinnedBy out, [doPath out]) | (_, out) <- Map.elems (walkFixed walk)]
    inputFor (path, (node, out)) =
      FixedInput
        { inputDrvPath = path,
          inputOutput = doPath out,
          inputProvenance = provenanceOf (dnDerivation node),
          inputSameHash = sortOn (\other -> (spName other, other)) (filter (/= doPath out) (Map.findWithDefault [] (pinnedBy out) byHash)),
          inputBuilders = runsBy Inside path,
          inputTreeBuilders = runsBy InTree path,
          inputScan = maybe NotInStore Scanned (Map.lookup (doPath out) facts)
        }
    provenanceOf drv
      | isBuiltin drv = FetchedBy (drvBuilder drv)
      | otherwise = BuiltBy (drvBuilder drv)
    tally = Map.fromListWith (+) . map (,1)

-- | The report as lines: the fixed-output inputs run as builders, those
-- holding prebuilt code and those not fully read, each with how its bytes
-- come to be, the other inputs pinned to the same hash, the builders
-- inside it or its tree, the prebuilt code found and what was not read;
-- the count of those holding no prebuilt code; the builders from outside
-- the closure, from its input sources and the builtins, each with how
-- many derivations it builds; the count of input sources; and what the
-- report does not say.
renderTrustReport :: TrustReport -> [Text]
renderTrustReport report =
  concat
    [ inputSection "Fixed-output inputs run as builders" RunAsBuilder,
      inputSection "Fixed-output inputs holding prebuilt code" HoldsPrebuiltCode,
      inputSection "Fixed-output inputs not fully read" NotFullyRead,
      [counted "Fixed-output inputs holding no prebuilt code" (length (inClass HoldsNoPrebuiltCode))],
      builderSection "Builders outside the closure" (trOutsideBuilders report),
      builderSection "Builders in input sources" (trSourceBuilders report),
      builderSection "Builtin builders" (trBuiltinBuilders report),
      [counted "Input sources" (Set.size (trSources report))],
      [reliedOnNotSafe]
    ]
  where
    inClass wanted = filter ((== wanted) . inputClass) (trInputs report)
    inputSection label wanted =
      let members = inClass wanted
       in counted label (length members) : concatMap describedInput members
    builderSection label builders =
      counted label (Map.size builders)
        : [indented (displayBytes builder <> ", for " <> derivations count) | (builder, count) <- Map.toList builders]
    counted label count = label <> ": " <> T.pack (show count)
    describedInput input =
      indented (storePathToText defaultStoreDir (inputOutput input))
        : map
          (indented . indented)
          ( provenanceLine (inputProvenance input)
              : map (("same hash as " <>) . storePathToText defaultStoreDir) (inputSameHash input)
              ++ runLines "builder of " (inputBuilders input)
              ++ runLines "in the tree of the builder of " (inputTreeBuilders input)
              ++ scanLines (inputScan input)
          )
    provenanceLine (FetchedBy builder) = "fetched by " <> displayBytes builder
    provenanceLine (BuiltBy builder) = "built in the closure by " <> displayBytes builder <> ", its bytes pinned by its hash"
    runLines lead builders = [lead <> derivations count <> ": " <> displayBytes builder | (builder, count) <- Map.toList builders]
    scanLines NotInStore = ["not read: the store does not hold it"]
    scanLines (Scanned facts) = map codeLine (cfCode facts) ++ map unreadLine (cfUnread facts)
    codeLine code = codeKindName (fcKind code) <> ": " <> placeText (fcPlace code)
    unreadLine unread = "not read: " <> maybe "" (<> ": ") (ufFile unread) <> ufReason unread
    indented = ("  " <>)

-- | Where a file of prebuilt code is, as the report says it.
placeText :: CodePlace -> Text
placeText place = case place of
  FetchedFile -> "the fetched file"
  TreeFile path -> path
  ArchiveMember member -> member
  TreeArchiveMember archive member -> member <> " in " <> archive

-- | A count of derivations in words.
derivations :: Int -> Text
derivations 1 = "1 derivation"
derivations count = T.pack (show count) <> " derivations"

-- | A builder's bytes for the report to show.  Display only: a builder
-- that is not UTF-8 is shown with replacement characters, and the bytes
-- are never used as a path.
displayBytes :: ByteString -> Text
displayBytes = TE.decodeUtf8With lenientDecode

-- | The report's last line.
reliedOnNotSafe :: Text
reliedOnNotSafe = "This reports what the closure relies on, not whether any of it is safe."
