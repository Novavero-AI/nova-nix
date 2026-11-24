-- | Tests of trust: the classification of a closure and the scan of what
-- it fetches.
module Test.Trust
  ( testTrust,
    testTrustScanIO,
    testTrustWindowsGraphs,
  )
where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as TarEntry
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.Zstd.Lazy as ZstdL
import Control.Exception (bracket_)
import Control.Monad (when)
import Data.Bits (shiftR)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.Char (isDigit)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word16, Word32, Word8)
import Nix.Builtins (parseNixPath)
import qualified Nix.Cli.Args as Cli
import qualified Nix.DependencyGraph as DepGraph
import Nix.Derivation (Derivation (..), DerivationOutput (..), Platform (..))
import Nix.Eval (MonadEval (..), NixValue (..), evalParsed, typeName, unrestrictedPolicy)
import Nix.Eval.IO (evalBaseEnv, newEvalStateWith, runEvalIO)
import Nix.Hash (sha256Digest)
import qualified Nix.Hash as Hash
import Nix.Parser (parseNix)
import Nix.Store.Path (StorePath, defaultStoreDir, parseStorePath, storePathHashLen, storePathToText)
import Nix.Store.Path.Internal (StorePath (..))
import qualified Nix.Trust as Trust
import Nix.Trust.Scan (scanFetched)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory)
import qualified System.Directory as Dir
import System.FilePath (takeDirectory, (</>))
import Test.Harness (TestResult (..), andThen, assertEqual, bytesText, evalNixIOStoreDir, forceRemoveIfExists, nixQuotedPath, runTest, runTestM, sessionClosure, symlinksAvailable, tarDir, tarFile, testOrigin, testSessionStore, trustPe)
import qualified Test.QuickCheck as QC
import Text.Read (readMaybe)

-- ---------------------------------------------------------------------------
-- Tests: Push (pure narinfo construction + closure computation)
-- ---------------------------------------------------------------------------

-- | Zstd compression level for test archives (zstd's own default).
testZstdCompressionLevel :: Int
testZstdCompressionLevel = 3

-- | A zstd-compressed tar archive of these entries.
zstdArchive :: [TarEntry.Entry] -> BL.ByteString
zstdArchive = ZstdL.compress testZstdCompressionLevel . Tar.write

-- ---------------------------------------------------------------------------
-- Tests: trust
-- ---------------------------------------------------------------------------

-- | A store path whose hash repeats one nix-base32 letter.
trustPath :: Char -> Text -> StorePath
trustPath letter = StorePath (T.replicate storePathHashLen (T.singleton letter))

-- | A store path as a builder or an argument names it.
trustText :: StorePath -> BS.ByteString
trustText = TE.encodeUtf8 . storePathToText defaultStoreDir

-- | A hash a fixed-output test derivation states, one of its own for each
-- output name.
trustHashOf :: StorePath -> Text
trustHashOf output = Hash.bytesToHexText (sha256Digest (TE.encodeUtf8 (spName output)))

-- | A derivation with one output, fixed to a hash when one is given, built
-- by this builder from these derivations and sources.
trustDrv :: StorePath -> Maybe Text -> BS.ByteString -> [StorePath] -> [StorePath] -> Derivation
trustDrv output fixedHash builder inputs sources =
  Derivation
    { drvOutputs = [DerivationOutput "out" output (maybe "" (const "sha256") fixedHash) (fromMaybe "" fixedHash)],
      drvInputDrvs = Map.fromList [(input, ["out"]) | input <- inputs],
      drvInputSrcs = sources,
      drvPlatform = X86_64_Windows,
      drvBuilder = builder,
      drvArgs = [],
      drvEnv = Map.empty
    }

-- | A graph of these derivations, each under its @.drv@ path.
trustGraph :: [(StorePath, Derivation)] -> DepGraph.DepGraph
trustGraph nodes = DepGraph.DepGraph (Map.fromList [(path, DepGraph.DepNode path drv (Map.map Set.fromList (drvInputDrvs drv))) | (path, drv) <- nodes])

-- | A little-endian field of two bytes, and one of four.
trustLe16 :: Word16 -> BS.ByteString
trustLe16 value = BS.pack [fromIntegral value, fromIntegral (value `shiftR` 8)]

trustLe32 :: Word32 -> BS.ByteString
trustLe32 value = BS.pack [fromIntegral (value `shiftR` bits) | bits <- [0, 8, 16, 24]]

-- | An ELF header through its type, 64-bit, in the byte order its
-- encoding byte names (1 little-endian, 2 big-endian), with this type.
trustElf :: Word8 -> Word16 -> BS.ByteString
trustElf encoding elfType = BS.pack [0x7F, 0x45, 0x4C, 0x46, 2, encoding, 1] <> BS.replicate 9 0 <> ordered (trustLe16 elfType)
  where
    ordered = if encoding == 2 then BS.reverse else id

-- | A COFF object for a machine, laid out as a compiler lays one out: the
-- file header, naming one section and one symbol and no optional header,
-- the section header, and the symbol table right after it.
trustCoff :: Word16 -> BS.ByteString
trustCoff machine =
  trustLe16 machine
    <> trustLe16 1
    <> trustLe32 0
    <> trustLe32 60
    <> trustLe32 1
    <> trustLe16 0
    <> trustLe16 0
    <> ".text"
    <> BS.replicate 35 0
    <> BS.replicate 18 0

-- | A COFF object in the bigobj form for x86-64, as LLVM writes one for an
-- object of more sections than the plain form counts: the header with
-- its class identifier, one section header, and one 20-byte symbol.
trustBigObj :: BS.ByteString
trustBigObj =
  BS.pack [0, 0, 0xFF, 0xFF]
    <> trustLe16 2
    <> trustLe16 0x8664
    <> trustLe32 0
    <> BS.pack [0xC7, 0xA1, 0xBA, 0xD1, 0xEE, 0xBA, 0xA9, 0x4B, 0xAF, 0x20, 0xFA, 0xF6, 0x6A, 0xA4, 0xDC, 0xB8]
    <> BS.concat (replicate 4 (trustLe32 0))
    <> trustLe32 1
    <> trustLe32 96
    <> trustLe32 1
    <> ".text"
    <> BS.replicate 35 0
    <> BS.replicate 20 0

-- | An ar archive of one member, a COFF object, with the member header
-- the format gives every member.
trustAr :: BS.ByteString
trustAr = "!<arch>\n" <> field 16 "crt2.o/" <> field 12 "0" <> field 6 "0" <> field 6 "0" <> field 8 "644" <> field 10 (BS8.pack (show (BS.length object))) <> "`\n" <> object
  where
    object = trustCoff 0x8664
    field width text = text <> BS.replicate (width - BS.length text) 0x20

-- | 'trustPe' as @pkg/tool.exe@ in a ustar archive, compressed with xz, by
-- Python's lzma module: the suite has no xz compressor.
trustXzArchive :: BS.ByteString
trustXzArchive =
  fromMaybe BS.empty . Hash.hexToBytes $
    "fd377a585a000004e6d6b446020021011c00000010cf58cce007ff004b5d00381ac9159cbbcdf1065aadd096d83e844fa103a52c8bdef734e90ff4360d9ce437ad74279b938ad2a38e3ee7c2d25136f5b6f3ca8685a1de9669221704ebfe730166fada2b7057674ea6730000ac8e93cf09c3d66d0001678010000000320c5b37b1c467fb020000000004595a"

-- | The same archive compressed with bzip2, by Python's bz2 module.
trustBzip2Archive :: BS.ByteString
trustBzip2Archive =
  fromMaybe BS.empty . Hash.hexToBytes $
    "425a683931415926535944dfe55f000036df88cc804001f78042024410628cde400000880820005441400d3401a6807a9ed504a28c868032000043177caa0f6bd410b38ba723a5ae510505db5b18231443389085e9364876c2828444a5ba2b71e2a94e3e01833ccff17724538509044dfe55f0"

-- | What the header scanner makes of strict bytes.
headerOf :: BS.ByteString -> Maybe Trust.CodeKind
headerOf = Trust.prebuiltKind . BL.fromStrict

-- | Each fixed-output input of a report by output name, with its class.
trustClasses :: Trust.TrustReport -> [(Text, Trust.InputClass)]
trustClasses report = [(spName (Trust.inputOutput input), Trust.inputClass input) | input <- Trust.trInputs report]

-- | The fixed-output inputs a report says a build runs as builders.
trustRunAsBuilders :: Trust.TrustReport -> [StorePath]
trustRunAsBuilders report = [Trust.inputDrvPath input | input <- Trust.trInputs report, Trust.inputClass input == Trust.RunAsBuilder]

-- | What a scan found: this prebuilt code and nothing unread.
codeOnly :: [Trust.FoundCode] -> Trust.ContentFacts
codeOnly code = Trust.ContentFacts code []

-- | One file of prebuilt code a scan found, and nothing unread.
oneCode :: Trust.CodeKind -> Trust.CodePlace -> Trust.ContentFacts
oneCode kind place = codeOnly [Trust.FoundCode kind place]

-- | A scan that read everything and found no prebuilt code.
noCode :: Trust.ContentFacts
noCode = Trust.ContentFacts [] []

-- | The header scanner over byte strings, the parser's trust command, and
-- the classification over hand built graphs.  As in @pkgs/windows@,
-- hex0's builder is the flat seed it fetches, hex0 is pinned to the
-- seed's hash, and stage0-pe32's source archive holds a copy of that
-- seed.  As in a stdenv made of binary packages, hello's builder is bash
-- in prebuilt packages builtin:unpack makes one tree of, and prebuilt
-- packages hold its toolchain.
testTrust :: IO [Bool]
testTrust = do
  putStrLn "trust"
  let seed = trustPath 'a' "hex0-seed.exe.drv"
      seedOut = trustPath 'b' "hex0-seed.exe"
      hex0 = trustPath 'c' "hex0.exe.drv"
      hex0Out = trustPath 'd' "hex0.exe"
      hex1 = trustPath 'f' "hex1.exe.drv"
      hex1Out = trustPath 'g' "hex1.exe"
      stage0Archive = trustPath 'h' "stage0-pe32-a81c837.tar.gz.drv"
      stage0ArchiveOut = trustPath 'i' "stage0-pe32-a81c837.tar.gz"
      stage0Tree = trustPath 'j' "stage0-pe32-a81c837.drv"
      stage0TreeOut = trustPath 'k' "stage0-pe32-a81c837"
      bash = trustPath 'l' "bash.tar.zst.drv"
      bashOut = trustPath 'm' "bash.tar.zst"
      runtime = trustPath 'n' "runtime.tar.zst.drv"
      runtimeOut = trustPath 'p' "runtime.tar.zst"
      tools = trustPath 'q' "tools.drv"
      toolsOut = trustPath 'r' "tools"
      gcc = trustPath 's' "gcc.tar.zst.drv"
      gccOut = trustPath 'v' "gcc.tar.zst"
      crt = trustPath 'w' "crt.tar.zst.drv"
      crtOut = trustPath 'x' "crt.tar.zst"
      toolchain = trustPath 'y' "toolchain.drv"
      toolchainOut = trustPath 'z' "toolchain"
      helloSrc = trustPath '0' "hello-2.12.3.tar.gz.drv"
      helloSrcOut = trustPath '1' "hello-2.12.3.tar.gz"
      hello = trustPath '2' "hello.drv"
      helloOut = trustPath '3' "hello"
      setup = trustPath '4' "setup.sh"
      greeting = trustPath '5' "greeting.drv"
      greetingOut = trustPath '6' "greeting"
      root = trustPath '7' "closure.drv"
      rootOut = trustPath '8' "closure"
      fetched output = trustDrv output (Just (trustHashOf output)) "builtin:fetchurl" [] []
      unpacked output inputs = trustDrv output Nothing "builtin:unpack" inputs []
      hex0Drv = trustDrv hex0Out (Just (trustHashOf seedOut)) (trustText seedOut) [seed, stage0Archive] []
      bashExe = trustText toolsOut <> "/bin/bash.exe"
      graph =
        trustGraph
          [ (seed, fetched seedOut),
            (hex0, hex0Drv),
            (hex1, trustDrv hex1Out Nothing (trustText hex0Out) [hex0, stage0Tree] []),
            (stage0Archive, fetched stage0ArchiveOut),
            (stage0Tree, unpacked stage0TreeOut [stage0Archive]),
            (bash, fetched bashOut),
            (runtime, fetched runtimeOut),
            (tools, unpacked toolsOut [bash, runtime]),
            (gcc, fetched gccOut),
            (crt, fetched crtOut),
            (toolchain, unpacked toolchainOut [gcc, crt]),
            (helloSrc, fetched helloSrcOut),
            (hello, trustDrv helloOut Nothing bashExe [tools, toolchain, helloSrc] [setup]),
            (greeting, trustDrv greetingOut Nothing "/bin/sh" [] []),
            (root, trustDrv rootOut Nothing "/bin/sh" [hello, hex1, greeting] [])
          ]
      facts =
        Map.fromList
          [ (seedOut, oneCode Trust.PeImage Trust.FetchedFile),
            (stage0ArchiveOut, oneCode Trust.PeImage (Trust.ArchiveMember "stage0-pe32-a81c8374592e8e897c0863d31c6480ea69669009/bootstrap-seeds/PE32/i386/hex0-seed.exe")),
            (bashOut, oneCode Trust.PeImage (Trust.ArchiveMember "bin/bash.exe")),
            (runtimeOut, oneCode Trust.PeImage (Trust.ArchiveMember "bin/runtime.dll")),
            (gccOut, Trust.ContentFacts [Trust.FoundCode Trust.PeImage (Trust.ArchiveMember "bin/gcc.exe")] [Trust.UnreadFile Nothing "tar archive stops reading: truncated"]),
            (helloSrcOut, noCode)
          ]
      report = Trust.trustReport facts graph root
      builtOut = trustPath '9' "tool"
      built = trustPath 'a' "tool.drv"
      user = trustPath 'b' "user.drv"
      foreignBuilder = trustText (trustPath 'c' "foreign") <> "/bin/sh"
      nonUtf8Builder = trustText builtOut <> "/bin/\xFF"
      provenanceOf path = [(Trust.inputProvenance input, Trust.inputSameHash input) | input <- Trust.trInputs report, Trust.inputDrvPath input == path]
  sequence
    [ runTest "a Portable Executable is an MZ header whose e_lfanew points at a PE signature" $
        assertEqual "PE" (Just Trust.PeImage) (headerOf trustPe),
      runTest "an MZ header without a PE signature where e_lfanew points is not prebuilt code" $
        assertEqual "MZ alone" Nothing (headerOf ("MZ" <> BS.replicate 0x3A 0 <> BS.pack [0x40, 0, 0, 0] <> "NE\0\0"))
          `andThen` assertEqual "e_lfanew past the end" Nothing (headerOf ("MZ" <> BS.replicate 0x3A 0 <> BS.pack [0xFF, 0, 0, 0] <> "PE\0\0"))
          `andThen` assertEqual "no e_lfanew" Nothing (headerOf "MZ"),
      runTest "an ELF file is an object, an image or neither by its type, in either byte order" $
        assertEqual
          "kinds"
          (map Just [Trust.ElfObject, Trust.ElfImage, Trust.ElfImage, Trust.ElfObject, Trust.ElfImage, Trust.ElfOther, Trust.ElfOther])
          (map headerOf [trustElf 1 1, trustElf 1 2, trustElf 1 3, trustElf 2 1, trustElf 2 3, trustElf 1 4, BS.pack [0x7F, 0x45, 0x4C, 0x46, 2, 1, 1]]),
      runTest "Mach-O files of each width and order, and the universal form, are prebuilt code" $
        assertEqual
          "formats"
          (map Just [Trust.MachO, Trust.MachO, Trust.MachO, Trust.MachO, Trust.MachOUniversal, Trust.MachOUniversal])
          ( map
              (headerOf . BS.pack)
              [ [0xFE, 0xED, 0xFA, 0xCE],
                [0xCE, 0xFA, 0xED, 0xFE],
                [0xFE, 0xED, 0xFA, 0xCF],
                [0xCF, 0xFA, 0xED, 0xFE],
                [0xCA, 0xFE, 0xBA, 0xBE, 0, 0, 0, 2],
                [0xCA, 0xFE, 0xBA, 0xBF, 0, 0, 0, 1]
              ]
          ),
      runTest "a Java class file, which shares the universal magic, is not prebuilt code" $
        assertEqual "class" Nothing (headerOf (BS.pack [0xCA, 0xFE, 0xBA, 0xBE, 0, 0, 0, 0x34])),
      runTest "a COFF object is one for each machine, in the plain form and the bigobj one" $
        assertEqual "plain" (replicate 4 (Just Trust.CoffObject)) (map (headerOf . trustCoff) [0x014C, 0x8664, 0xAA64, 0x01C4])
          `andThen` assertEqual "bigobj" (Just Trust.CoffObject) (headerOf trustBigObj),
      runTest "a COFF header whose tables do not fit, or that has an optional header, is not an object" $
        assertEqual "symbol table cut" Nothing (headerOf (BS.take 70 (trustCoff 0x8664)))
          `andThen` assertEqual "too many sections" Nothing (headerOf (trustLe16 0x8664 <> trustLe16 0xFFFF <> BS.drop 4 (trustCoff 0x8664)))
          `andThen` assertEqual "optional header" Nothing (headerOf (BS.take 16 (trustCoff 0x8664) <> trustLe16 0xE0 <> BS.drop 18 (trustCoff 0x8664)))
          `andThen` assertEqual "unknown machine" Nothing (headerOf (trustLe16 0x0200 <> BS.drop 2 (trustCoff 0x8664)))
          `andThen` assertEqual "bigobj cut" Nothing (headerOf (BS.take 100 trustBigObj))
          `andThen` assertEqual "bigobj class" Nothing (headerOf (BS.take 12 trustBigObj <> BS.replicate 16 0 <> BS.drop 28 trustBigObj)),
      runTest "text that starts with i386's machine bytes is not a COFF object" $
        assertEqual "text" Nothing (headerOf "L\x01 is how an i386 object's header starts, but this is text\n"),
      runTest "an ar archive is prebuilt code by its magic, members unread" $
        assertEqual "ar" (Just Trust.ArArchive) (headerOf trustAr)
          `andThen` assertEqual "magic alone" (Just Trust.ArArchive) (headerOf "!<arch>\n")
          `andThen` assertEqual "no newline" Nothing (headerOf "!<arch> is how an ar archive starts"),
      runTest "a script is not prebuilt code, whatever its #! line runs" $
        assertEqual "script" Nothing (headerOf "#!/nix/store/hash-bash/bin/bash\necho hello\n")
          `andThen` assertEqual "empty" Nothing (headerOf ""),
      runTest "trust takes a target and -A, and refuses the flags only build takes" $
        assertEqual "target" (Right (Cli.CmdTrust (Cli.TargetExpr "1") (Just "a.b"))) (Cli.optCommand <$> Cli.parseArgs ["trust", "--expr", "1", "-A", "a.b"])
          `andThen` assertEqual "check" (Left "unknown trust flag: --check") (Cli.optCommand <$> Cli.parseArgs ["trust", "--check", "--expr", "1"])
          `andThen` assertEqual "out-link" (Left "unknown trust flag: -o") (Cli.optCommand <$> Cli.parseArgs ["trust", "-o", "result", "--expr", "1"])
          `andThen` assertEqual "no-out-link" (Left "unknown trust flag: --no-out-link") (Cli.optCommand <$> Cli.parseArgs ["trust", "--no-out-link", "--expr", "1"])
          `andThen` assertEqual "exec-wrapper" (Left "unknown trust flag: --exec-wrapper") (Cli.optCommand <$> Cli.parseArgs ["trust", "--exec-wrapper", "x86_64-windows=wine", "--expr", "1"]),
      runTest "every fixed-output derivation of the closure is an input, one with inputs of its own included" $
        assertEqual
          "inputs"
          (sort [seed, hex0, stage0Archive, bash, runtime, gcc, crt, helloSrc])
          (sort (map (DepGraph.dnDrvPath . fst) (Trust.fixedOutputInputs graph root))),
      runTest "each fixed-output input lands in the class its facts give it" $
        assertEqual
          "classes"
          [ ("bash.tar.zst", Trust.RunAsBuilder),
            ("crt.tar.zst", Trust.NotFullyRead),
            ("gcc.tar.zst", Trust.HoldsPrebuiltCode),
            ("hello-2.12.3.tar.gz", Trust.HoldsNoPrebuiltCode),
            ("hex0-seed.exe", Trust.RunAsBuilder),
            ("hex0.exe", Trust.RunAsBuilder),
            ("runtime.tar.zst", Trust.RunAsBuilder),
            ("stage0-pe32-a81c837.tar.gz", Trust.HoldsPrebuiltCode)
          ]
          (trustClasses report),
      runTest "a flat seed that is a builder runs as one, and so does a fixed output built from it" $
        assertEqual
          "builders"
          [(seed, Map.singleton (trustText seedOut) 1, Map.empty), (hex0, Map.singleton (trustText hex0Out) 1, Map.empty)]
          [(Trust.inputDrvPath input, Trust.inputBuilders input, Trust.inputTreeBuilders input) | input <- Trust.trInputs report, Trust.inputDrvPath input `elem` [seed, hex0]],
      runTest "a fixed output the closure builds names its builder, and two pinned to one hash name each other" $
        assertEqual "seed" [(Trust.FetchedBy "builtin:fetchurl", [hex0Out])] (provenanceOf seed)
          `andThen` assertEqual "hex0" [(Trust.BuiltBy (trustText seedOut), [seedOut])] (provenanceOf hex0)
          `andThen` assertEqual "archive" [(Trust.FetchedBy "builtin:fetchurl", [])] (provenanceOf stage0Archive),
      runTest "packages builtin:unpack makes the builder's tree of run as builders, as part of the tree" $
        assertEqual
          "builders"
          [(Map.empty, Map.singleton bashExe 1), (Map.empty, Map.singleton bashExe 1)]
          [(Trust.inputBuilders input, Trust.inputTreeBuilders input) | input <- Trust.trInputs report, Trust.inputDrvPath input `elem` [bash, runtime]],
      runTest "a host program, the builtins and the input sources are counted" $
        assertEqual "outside" (Map.singleton "/bin/sh" 2) (Trust.trOutsideBuilders report)
          `andThen` assertEqual "builtins" (Map.fromList [("builtin:fetchurl", 7), ("builtin:unpack", 3)]) (Trust.trBuiltinBuilders report)
          `andThen` assertEqual "source builders" Map.empty (Trust.trSourceBuilders report)
          `andThen` assertEqual "sources" (Set.singleton setup) (Trust.trSources report),
      runTest "a builder a program in the closure made runs nothing as a fixed-output input" $
        let made =
              trustGraph
                [ (seed, fetched seedOut),
                  (built, trustDrv builtOut Nothing "/bin/sh" [seed] []),
                  (user, trustDrv rootOut Nothing (trustText builtOut <> "/bin/tool") [built] [])
                ]
         in assertEqual "run as builders" [] (trustRunAsBuilders (Trust.trustReport Map.empty made user))
              `andThen` assertEqual "outside" (Map.singleton "/bin/sh" 1) (Trust.trOutsideBuilders (Trust.trustReport Map.empty made user)),
      runTest "a builder whose path in its store path is not UTF-8 is still in that store path" $
        let made =
              trustGraph
                [ (built, trustDrv builtOut (Just (trustHashOf builtOut)) "builtin:fetchurl" [] []),
                  (user, trustDrv rootOut Nothing nonUtf8Builder [built] [])
                ]
         in assertEqual "run as builders" [built] (trustRunAsBuilders (Trust.trustReport Map.empty made user)),
      runTest "a builder in a store path the closure does not provide is outside it, and one in its sources is a source builder" $
        let foreignGraph = trustGraph [(user, trustDrv rootOut Nothing foreignBuilder [] [])]
            sourceReport = Trust.trustReport Map.empty (trustGraph [(user, trustDrv rootOut Nothing (trustText setup) [] [setup])]) user
         in assertEqual "foreign" (Map.singleton foreignBuilder 1) (Trust.trOutsideBuilders (Trust.trustReport Map.empty foreignGraph user))
              `andThen` assertEqual "source outside" Map.empty (Trust.trOutsideBuilders sourceReport)
              `andThen` assertEqual "source builder" (Map.singleton (trustText setup) 1) (Trust.trSourceBuilders sourceReport),
      runTest "only a fixed-output derivation a builtin fetches with no input is fetched by trust" $
        assertEqual
          "fetched"
          [True, False, False, False]
          (map Trust.fetchedByBuiltin [fetched seedOut, hex0Drv, unpacked stage0TreeOut [stage0Archive], trustDrv seedOut (Just (trustHashOf seedOut)) "builtin:fetchurl" [] [setup]]),
      runTest "the report lists the opaque inputs first, each with how it came to be, then the counts, then what it does not say" $
        let shell = trustPath 'f' "shell.drv"
            script = trustPath 'g' "script.drv"
            small =
              trustGraph
                [ (seed, fetched seedOut),
                  (hex0, hex0Drv),
                  (hex1, trustDrv hex1Out Nothing (trustText hex0Out) [hex0] []),
                  (stage0Archive, fetched stage0ArchiveOut),
                  (helloSrc, fetched helloSrcOut),
                  (bash, fetched bashOut),
                  (tools, unpacked toolsOut [bash]),
                  (shell, trustDrv greetingOut Nothing bashExe [tools] []),
                  (script, trustDrv helloOut Nothing (trustText setup) [] [setup]),
                  (root, trustDrv rootOut Nothing "/bin/sh" [hex1, helloSrc, shell, script] [setup])
                ]
            smallFacts =
              Map.fromList
                [ (seedOut, oneCode Trust.PeImage Trust.FetchedFile),
                  (stage0ArchiveOut, codeOnly [Trust.FoundCode Trust.PeImage (Trust.ArchiveMember "stage0-pe32/bootstrap-seeds/PE32/i386/hex0-seed.exe"), Trust.FoundCode Trust.ArArchive (Trust.ArchiveMember "stage0-pe32/lib/libc.a")]),
                  (helloSrcOut, Trust.ContentFacts [] [Trust.UnreadFile Nothing "gzip stream error: truncated"]),
                  (bashOut, oneCode Trust.PeImage (Trust.ArchiveMember "bin/bash.exe"))
                ]
            path = storePathToText defaultStoreDir
         in assertEqual
              "lines"
              [ "Fixed-output inputs run as builders: 3",
                "  " <> path bashOut,
                "    fetched by builtin:fetchurl",
                "    in the tree of the builder of 1 derivation: " <> bytesText bashExe,
                "    PE image: bin/bash.exe",
                "  " <> path seedOut,
                "    fetched by builtin:fetchurl",
                "    same hash as " <> path hex0Out,
                "    builder of 1 derivation: " <> path seedOut,
                "    PE image: the fetched file",
                "  " <> path hex0Out,
                "    built in the closure by " <> path seedOut <> ", its bytes pinned by its hash",
                "    same hash as " <> path seedOut,
                "    builder of 1 derivation: " <> path hex0Out,
                "    not read: the store does not hold it",
                "Fixed-output inputs holding prebuilt code: 1",
                "  " <> path stage0ArchiveOut,
                "    fetched by builtin:fetchurl",
                "    PE image: stage0-pe32/bootstrap-seeds/PE32/i386/hex0-seed.exe",
                "    ar archive: stage0-pe32/lib/libc.a",
                "Fixed-output inputs not fully read: 1",
                "  " <> path helloSrcOut,
                "    fetched by builtin:fetchurl",
                "    not read: gzip stream error: truncated",
                "Fixed-output inputs holding no prebuilt code: 0",
                "Builders outside the closure: 1",
                "  /bin/sh, for 1 derivation",
                "Builders in input sources: 1",
                "  " <> path setup <> ", for 1 derivation",
                "Builtin builders: 2",
                "  builtin:fetchurl, for 4 derivations",
                "  builtin:unpack, for 1 derivation",
                "Input sources: 1",
                "This reports what the closure relies on, not whether any of it is safe."
              ]
              (Trust.renderTrustReport (Trust.trustReport smallFacts small root)),
      runTestM "every fixed-output derivation of a closure is in the report once, in one class, cycles and all" $ do
        result <-
          QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} $
            QC.forAll genTrustCase $ \(nodes, fixedHere, rootPath, caseFacts) ->
              QC.within 2000000 $
                let caseGraph = trustGraph nodes
                    caseReport = Trust.trustReport caseFacts caseGraph rootPath
                    expected = sort (Set.toList (Set.intersection fixedHere (trustReachable caseGraph rootPath)))
                    counts = [readMaybe (T.unpack (T.takeWhileEnd isDigit line)) | line <- Trust.renderTrustReport caseReport, "Fixed-output inputs" `T.isPrefixOf` line]
                 in sort (map Trust.inputDrvPath (Trust.trInputs caseReport)) QC.=== expected
                      QC..&&. sum (catMaybes counts) QC.=== length expected
        pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result))),
      runTestM "what runs as a builder is read from the graph alone, whatever the scans found" $ do
        result <-
          QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} $
            QC.forAll genTrustCase $ \(nodes, _, rootPath, caseFacts) ->
              QC.within 2000000 $
                let caseGraph = trustGraph nodes
                 in trustRunAsBuilders (Trust.trustReport caseFacts caseGraph rootPath) QC.=== trustRunAsBuilders (Trust.trustReport Map.empty caseGraph rootPath)
        pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))
    ]

-- | The derivations a root reaches in a graph, itself included.
trustReachable :: DepGraph.DepGraph -> StorePath -> Set.Set StorePath
trustReachable (DepGraph.DepGraph graph) root = go Set.empty [root]
  where
    go seen [] = seen
    go seen (path : rest)
      | Set.member path seen = go seen rest
      | otherwise = case Map.lookup path graph of
          Nothing -> go seen rest
          Just node -> go (Set.insert path seen) (Map.keys (DepGraph.dnDeps node) ++ rest)

-- | What kind of output a generated derivation has.
data TrustOutputKind = FixedKind | FloatingKind | ImpureKind | InputAddressedKind
  deriving (Show, Enum, Bounded)

-- | Up to a dozen derivations, each with a fixed, floating, impure or
-- input-addressed output, built by a host program, a builtin, something
-- inside another's output or an input source, each depending on any of
-- them, itself and cycles included; which of them are fixed-output, as
-- generated; a root among them; and facts for some of their outputs.
genTrustCase :: QC.Gen ([(StorePath, Derivation)], Set.Set StorePath, StorePath, Map.Map StorePath Trust.ContentFacts)
genTrustCase = do
  count <- QC.chooseInt (1, length letters)
  let ids = take count letters
      drvPathOf letter = trustPath letter "node.drv"
      outOf letter = trustPath letter "node"
      source = trustPath 'z' "source"
      outputFor letter kind = case kind of
        FixedKind -> DerivationOutput "out" (outOf letter) "sha256" (trustHashOf (outOf letter))
        FloatingKind -> DerivationOutput "out" (outOf letter) "r:sha256" ""
        ImpureKind -> DerivationOutput "out" (outOf letter) "r:sha256" "impure"
        InputAddressedKind -> DerivationOutput "out" (outOf letter) "" ""
      builderFor =
        QC.oneof
          [ pure "/bin/sh",
            pure "builtin:fetchurl",
            pure "builtin:unpack",
            pure (trustText source),
            (\letter sub -> trustText (outOf letter) <> sub) <$> QC.elements ids <*> QC.elements ["", "/bin/tool"]
          ]
      nodeFor letter = do
        kind <- QC.elements [minBound .. maxBound]
        builder <- builderFor
        inputs <- QC.sublistOf ids
        sources <- QC.elements [[], [source]]
        pure ((drvPathOf letter, (trustDrv (outOf letter) Nothing builder (map drvPathOf inputs) sources) {drvOutputs = [outputFor letter kind]}), kind)
      factFor = do
        code <- QC.sublistOf [Trust.FoundCode Trust.PeImage Trust.FetchedFile, Trust.FoundCode Trust.ElfImage (Trust.TreeFile "bin/tool")]
        unread <- QC.sublistOf [Trust.UnreadFile Nothing "unreadable"]
        pure (Trust.ContentFacts code unread)
  generated <- traverse nodeFor ids
  root <- QC.elements ids
  scanned <- QC.sublistOf ids
  facts <- traverse (\letter -> (,) (outOf letter) <$> factFor) scanned
  pure
    ( map fst generated,
      Set.fromList [path | ((path, _), FixedKind) <- generated],
      drvPathOf root,
      Map.fromList facts
    )
  where
    letters = "abcdfghijklm"

-- | 'Nix.Trust.Scan.scanFetched' over files and trees written here: a
-- fetched file, a tar archive in each compression it opens, a package of
-- object code, a tree, and the limits of the scan.  Names are chosen
-- against the format each file has, since the bytes decide.
testTrustScanIO :: IO [Bool]
testTrustScanIO = do
  putStrLn "trust/scan"
  tmpBase <- getTemporaryDirectory
  canLink <- symlinksAvailable
  let root = tmpBase </> "nova-nix-test-trust-scan"
      at = (root </>)
      member = Trust.ArchiveMember
      archive = Tar.write [tarDir "pkg", tarFile "pkg/bin/tool.exe" (BL.fromStrict trustPe), tarFile "pkg/bin/run.sh" "#!/bin/sh\necho hi\n", tarFile "pkg/README" "readme\n"]
      elf = trustElf 1 3
      machO = BS.pack [0xCF, 0xFA, 0xED, 0xFE, 7, 0, 0, 1]
      nested = GZip.compress (Tar.write [tarFile "inner/x.exe" (BL.fromStrict trustPe)])
      zstdTar = BL.toStrict (zstdArchive [tarFile "usr/bin/tool" (BL.fromStrict elf)])
      objectCode =
        zstdArchive
          [ tarFile "include/stdio.h" "int printf(const char *, ...);\n",
            tarFile "lib/crt2.o" (BL.fromStrict (trustCoff 0x8664)),
            tarFile "lib/libmingw32.a" (BL.fromStrict trustAr)
          ]
      skippable = BS.pack [0x50, 0x2A, 0x4D, 0x18, 4, 0, 0, 0] <> "pzst"
      write name bytes = createDirectoryIfMissing True (takeDirectory (at name)) >> BS.writeFile (at name) bytes
      crtOut = trustPath 'x' "crt.tar.zst"
      crtDrv = trustPath 'w' "crt.tar.zst.drv"
      crtGraph = trustGraph [(crtDrv, trustDrv crtOut (Just (trustHashOf crtOut)) "builtin:fetchurl" [] [])]
  bracket_
    ( do
        forceRemoveIfExists root
        write "seed.txt" trustPe
        write "script.exe" "#!/bin/sh\necho hi\n"
        write "source.exe" (BL.toStrict (GZip.compress archive))
        write "package.tar.gz" zstdTar
        write "skippable.tar.zst" (skippable <> zstdTar)
        write "package.tar.xz" trustXzArchive
        write "package.tar.bz2" trustBzip2Archive
        write "plain.tar" (BL.toStrict archive)
        write "crt.tar.zst" (BL.toStrict objectCode)
        write "library.txt" trustAr
        write "nested.tar.gz" (BL.toStrict (GZip.compress (Tar.write [tarFile "outer/inner.tar.gz" nested])))
        write "corrupt.tar.gz" (BS.pack [0x1F, 0x8B, 0x08] <> "not a deflate stream")
        write "notes.bz2" "BZh is how a bzip2 stream starts\n"
        write "tools.tar" "PK\x03\x04 a zip archive's local file header\n"
        write ("tree" </> "bin" </> "tool") machO
        write ("tree" </> "src" </> "main.c") "int main(void) { return 0; }\n"
        write ("tree" </> "tests" </> "broken.xz") (BS.take 12 trustXzArchive)
        write ("tree" </> "vendor" </> "blob") (BL.toStrict nested)
        write ("outside" </> "tool.exe") trustPe
        createDirectoryIfMissing True (at "linked")
        when canLink (Dir.createFileLink (at ("outside" </> "tool.exe")) (at ("linked" </> "tool.exe")))
    )
    (forceRemoveIfExists root)
    $ do
      results <- traverse (scanFetched . at) ["seed.txt", "script.exe", "source.exe", "package.tar.gz", "skippable.tar.zst", "package.tar.xz", "package.tar.bz2", "plain.tar", "crt.tar.zst", "library.txt", "nested.tar.gz", "corrupt.tar.gz", "notes.bz2", "tools.tar", "tree", "linked"]
      case results of
        [seedFacts, scriptFacts, sourceFacts, zstdFacts, skippableFacts, xzFacts, bzip2Facts, plainFacts, crtFacts, libraryFacts, nestedFacts, corruptFacts, notesFacts, zipFacts, treeFacts, linkedFacts] ->
          sequence
            [ runTest "a fetched file that is a Portable Executable is one, whatever its name" $
                assertEqual "seed" (oneCode Trust.PeImage Trust.FetchedFile) seedFacts,
              runTest "a script holds no prebuilt code, whatever its name" $
                assertEqual "script" noCode scriptFacts,
              runTest "a gzip tar archive is opened, and only its prebuilt code is reported" $
                assertEqual "source" (oneCode Trust.PeImage (member "pkg/bin/tool.exe")) sourceFacts,
              runTest "a zstd tar archive is opened, whatever its name, and so is one a skippable frame starts" $
                assertEqual "zstd" (oneCode Trust.ElfImage (member "usr/bin/tool")) zstdFacts
                  `andThen` assertEqual "skippable" (oneCode Trust.ElfImage (member "usr/bin/tool")) skippableFacts,
              runTest "xz and bzip2 tar archives are opened" $
                assertEqual "xz" (oneCode Trust.PeImage (member "pkg/tool.exe")) xzFacts
                  `andThen` assertEqual "bzip2" (oneCode Trust.PeImage (member "pkg/tool.exe")) bzip2Facts,
              runTest "an uncompressed tar archive is opened" $
                assertEqual "plain" (oneCode Trust.PeImage (member "pkg/bin/tool.exe")) plainFacts,
              runTest "a package of an object and a static library holds prebuilt code, both named" $
                assertEqual "crt" (codeOnly [Trust.FoundCode Trust.CoffObject (member "lib/crt2.o"), Trust.FoundCode Trust.ArArchive (member "lib/libmingw32.a")]) crtFacts
                  `andThen` assertEqual
                    "class"
                    [Trust.HoldsPrebuiltCode]
                    (map Trust.inputClass (Trust.trInputs (Trust.trustReport (Map.singleton crtOut crtFacts) crtGraph crtDrv))),
              runTest "a fetched ar archive is one file of prebuilt code, whatever its name" $
                assertEqual "library" (oneCode Trust.ArArchive Trust.FetchedFile) libraryFacts,
              runTest "an archive inside an archive is not opened" $
                assertEqual "nested" noCode nestedFacts,
              runTest "a stream that does not decode is not read, with why" $
                case corruptFacts of
                  Trust.ContentFacts [] [Trust.UnreadFile Nothing reason] | "gzip stream error: " `T.isPrefixOf` reason -> Pass
                  other -> Fail (T.pack (show other)),
              runTest "text that starts as a bzip2 magic does, without its block size, is read as it is" $
                assertEqual "notes" noCode notesFacts,
              runTest "an archive in a format the scan does not open is not read, whatever its name" $
                assertEqual "zip" (Trust.ContentFacts [] [Trust.UnreadFile Nothing "a zip archive, which the scan does not open"]) zipFacts,
              runTest "each file of a tree is read in name order, an archive in it is opened, and one that does not decode is not read" $
                case treeFacts of
                  Trust.ContentFacts code [Trust.UnreadFile (Just "tests/broken.xz") reason]
                    | "xz stream error: " `T.isPrefixOf` reason ->
                        assertEqual "code" [Trust.FoundCode Trust.MachO (Trust.TreeFile "bin/tool"), Trust.FoundCode Trust.PeImage (Trust.TreeArchiveMember "vendor/blob" "inner/x.exe")] code
                  other -> Fail (T.pack (show other)),
              runTest "a symbolic link in a tree is not followed" $
                if canLink then assertEqual "linked" noCode linkedFacts else Pass
            ]
        _ -> sequence [runTest "trust scan results" (Fail "one result per fixture expected")]

-- | The closures of @pkgs/windows@'s stage0-pe32 kaem and GNU Hello as
-- evaluation records them, classified with no content facts: what runs as
-- a builder, and how each fixed output comes to be, is in the graph
-- alone.
testTrustWindowsGraphs :: IO [Bool]
testTrustWindowsGraphs = do
  putStrLn "trust/pkgs-windows"
  cwd <- Dir.getCurrentDirectory
  storeDir <- evalNixIOStoreDir
  searchPaths <- parseNixPath (TE.encodeUtf8 (T.pack (cwd </> "data")))
  st <- newEvalStateWith unrestrictedPolicy searchPaths (testSessionStore storeDir) storeDir cwd
  let windows = "(import " <> nixQuotedPath (cwd </> "pkgs" </> "windows") <> ")"
      source = "builtins.concatStringsSep \" \" [ " <> windows <> ".stage0-pe32.kaem-unwrapped.drvPath " <> windows <> ".hello.drvPath ]"
  outcome <- case parseNix cwd testOrigin (TE.encodeUtf8 source) of
    Left err -> pure (Left (T.pack (show err)))
    Right parsed -> runEvalIO st $ do
      value <- evalParsed (pure (evalBaseEnv st)) parsed
      case value of
        VStr bytes _ -> do
          let roots = T.words (bytesText bytes)
          closure <- sessionClosure roots
          pure (roots, closure)
        other -> throwEvalError ("expected the drvPaths as one string, got " <> typeName other)
  case outcome of
    Right ([kaemText, helloText], closure)
      | Just kaem <- parseStorePath defaultStoreDir kaemText,
        Just helloDrv <- parseStorePath defaultStoreDir helloText -> do
          let graph = DepGraph.DepGraph (Map.fromList [(sp, DepGraph.DepNode sp drv (Map.map Set.fromList (drvInputDrvs drv))) | (text, drv) <- Map.toList closure, Just sp <- [parseStorePath defaultStoreDir text]])
              kaemReport = Trust.trustReport Map.empty graph kaem
              helloReport = Trust.trustReport Map.empty graph helloDrv
              ranNames report = sort [spName (Trust.inputOutput input) | input <- Trust.trInputs report, Trust.inputClass input == Trust.RunAsBuilder]
              ranDirectly report = sort [spName (Trust.inputOutput input) | input <- Trust.trInputs report, not (Map.null (Trust.inputBuilders input))]
              treeBuilders report = Set.fromList (concatMap (Map.keys . Trust.inputTreeBuilders) (Trust.trInputs report))
              named name report = [input | input <- Trust.trInputs report, spName (Trust.inputOutput input) == name]
          sequence
            [ runTest "kaem's closure runs the seed as a builder, and hex0, which its hash fixes to the seed" $
                assertEqual "run as builders" ["hex0-seed.exe", "hex0.exe"] (ranNames kaemReport)
                  `andThen` assertEqual "directly" ["hex0-seed.exe", "hex0.exe"] (ranDirectly kaemReport)
                  `andThen` assertEqual "outside" Map.empty (Trust.trOutsideBuilders kaemReport)
                  `andThen` assertEqual "builtins" ["builtin:fetchurl", "builtin:unpack"] (Map.keys (Trust.trBuiltinBuilders kaemReport)),
              runTest "kaem's hex0 is built in the closure by the fetched seed, and has the seed's hash" $
                case (named "hex0-seed.exe" kaemReport, named "hex0.exe" kaemReport) of
                  ([seedInput], [hex0Input]) ->
                    assertEqual "seed" (Trust.FetchedBy "builtin:fetchurl", [Trust.inputOutput hex0Input]) (Trust.inputProvenance seedInput, Trust.inputSameHash seedInput)
                      `andThen` assertEqual "hex0" (Trust.BuiltBy (trustText (Trust.inputOutput seedInput)), [Trust.inputOutput seedInput]) (Trust.inputProvenance hex0Input, Trust.inputSameHash hex0Input)
                  other -> Fail ("inputs: " <> T.pack (show other)),
              runTest "hello's closure, through the stdenv, runs only the seed and hex0 as builders, as kaem's does" $
                assertEqual "run as builders" ["hex0-seed.exe", "hex0.exe"] (ranNames helloReport)
                  `andThen` assertEqual "directly" ["hex0-seed.exe", "hex0.exe"] (ranDirectly helloReport)
                  `andThen` assertEqual "tree builders" Set.empty (treeBuilders helloReport)
                  `andThen` assertEqual "outside" Map.empty (Trust.trOutsideBuilders helloReport)
            ]
    Right other -> sequence [runTest "trust over pkgs/windows evaluates" (Fail ("unexpected roots: " <> T.pack (show (fst other))))]
    Left err -> sequence [runTest "trust over pkgs/windows evaluates" (Fail err)]
