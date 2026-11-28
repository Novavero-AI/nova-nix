-- | Tests of fetched archives: the tar reader, the tar sink's rules, a
-- tree's NAR, and @builtins.fetchTarball@ unpacking in process, each as
-- upstream 2.36.0 reads and places an archive's entries.
module Test.Archive
  ( testTarReader,
    testTarSink,
    testArchiveProperties,
    testFetchTarballIO,
  )
where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as TarEntry
import qualified Codec.Compression.GZip as GZip
import Control.Monad (void)
import Control.Monad.State.Strict (State, evalState, gets, modify, state)
import Data.Bifunctor (second)
import Data.Bits (shiftL)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Nix.Eval.CanonPath (canonPathValue)
import Nix.Fetch.Archive (ArchiveEntry (..), ArchiveNode (..), FetchFailure (..), UnpackedArchive (..), drainFile, emptySink, finishSink, readArchive, sinkEntry)
import Nix.Fetch.Tree (NarPiece (..), Tree (..), dereferenceSingleton, narPieces)
import Nix.Hash (FileIngestionMethod (..), makeFixedOutputPath, sha256Digest)
import Nix.Store.CaseSensitive (probeCaseSensitivity, volumeCaseHack)
import qualified Nix.Store.ExecBit as ExecBit
import Nix.Store.Path (StoreDir (..), storePathToFilePath)
import qualified NovaCache.Hash as Hash
import qualified NovaCache.NAR as NAR
import qualified NovaCache.Zstd as Zstd
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory)
import System.FilePath ((</>))
import Test.Harness (TestResult (..), andThen, assertEqual, evalNixIOStore, fetchedStorePath, forceRemoveIfExists, runTest, runTestM)
import qualified Test.QuickCheck as QC

-- | A QuickCheck property as a test result.
property :: (QC.Testable prop) => prop -> IO TestResult
property prop = do
  result <- QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} prop
  pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))

-- ---------------------------------------------------------------------------
-- Driving the reader over bytes in memory
-- ---------------------------------------------------------------------------

-- | Unpack an archive's bytes, fed in the chunks given, with each file's
-- contents as its blob.
unpackChunks :: [ByteString] -> ([Text], Either FetchFailure (UnpackedArchive ByteString))
unpackChunks chunks = evalState (readArchive pull store) (chunks, [])
  where
    pull :: State ([ByteString], [ByteString]) (Either Text ByteString)
    pull = state $ \(remaining, held) -> case remaining of
      chunk : rest -> (Right chunk, (rest, held))
      [] -> (Right BS.empty, ([], held))
    store _ _ contents = do
      modify (\(remaining, _) -> (remaining, []))
      after <- drainFile pull (modify . second . (:)) contents
      held <- gets snd
      pure ((,) (BS.concat (reverse held)) <$> after)

-- | Unpack an archive's bytes in one chunk.
unpackBytes :: BL.ByteString -> ([Text], Either FetchFailure (UnpackedArchive ByteString))
unpackBytes = unpackChunks . BL.toChunks

-- | The tree an archive unpacks to, or why it does not.
treeOf :: BL.ByteString -> Either FetchFailure (Tree ByteString)
treeOf = fmap unpackedTree . snd . unpackBytes

-- | An archive the tar package writes, long names and link targets as GNU
-- entries.
archiveOf :: [Tar.GenEntry BL.ByteString FilePath FilePath] -> BL.ByteString
archiveOf = Tar.write . concatMap Tar.encodeLongNames

file :: FilePath -> BL.ByteString -> Tar.GenEntry BL.ByteString FilePath FilePath
file = TarEntry.fileEntry

executable :: FilePath -> BL.ByteString -> Tar.GenEntry BL.ByteString FilePath FilePath
executable path contents = (TarEntry.fileEntry path contents) {TarEntry.entryPermissions = TarEntry.executableFilePermissions}

directory :: FilePath -> Tar.GenEntry BL.ByteString FilePath FilePath
directory = TarEntry.directoryEntry

symlink :: FilePath -> FilePath -> Tar.GenEntry BL.ByteString FilePath FilePath
symlink path target = TarEntry.simpleEntry path (Tar.SymbolicLink target)

hardLink :: FilePath -> FilePath -> Tar.GenEntry BL.ByteString FilePath FilePath
hardLink path target = TarEntry.simpleEntry path (Tar.HardLink target)

-- | An entry of another type, with its body, as a pax or global header is.
special :: Char -> FilePath -> BL.ByteString -> Tar.GenEntry BL.ByteString FilePath FilePath
special flag path body = TarEntry.simpleEntry path (Tar.OtherEntryType flag body (BL.length body))

-- | A pax body of records, each with its length counted as pax counts it:
-- the digits of the length are part of the record.
paxBody :: [(ByteString, ByteString)] -> BL.ByteString
paxBody = BL.fromStrict . BS.concat . map record
  where
    record (key, value) =
      let rest = " " <> key <> "=" <> value <> "\n"
       in BC.pack (show (recordLength (BS.length rest))) <> rest
    recordLength restLength =
      let guess = restLength + digits restLength
       in if digits guess == digits restLength then guess else guess + 1
    digits = length . show

-- | A directory of entries.
dir :: [(ByteString, Tree blob)] -> Tree blob
dir = TreeDirectory . Map.fromList

-- ---------------------------------------------------------------------------
-- The reader
-- ---------------------------------------------------------------------------

-- | The tar formats libarchive reads, and its refusals.
testTarReader :: IO [Bool]
testTarReader = do
  putStrLn "fetch/tar-reader"
  sequence
    [ runTest "a ustar archive's files, directories and links unpack" $
        assertEqual
          "tree"
          (Right (dir [("a", TreeFile False "one"), ("bin", dir [("run", TreeFile True "#!")]), ("empty", dir []), ("link", TreeSymlink "a")]))
          (treeOf (archiveOf [file "top/a" "one", directory "top/bin", executable "top/bin/run" "#!", symlink "top/link" "a", directory "top/empty"])),
      runTest "a long name is a GNU long name, and a long path a ustar prefix" $
        let long = replicate 120 'n'
            deep = replicate 60 'd' </> replicate 60 'e' </> "f"
         in assertEqual
              "tree"
              (Right (dir [(BC.pack long, TreeFile False "long"), (BC.pack (replicate 60 'd'), dir [(BC.pack (replicate 60 'e'), dir [("f", TreeFile False "deep")])]), ("t", TreeSymlink (BC.pack (replicate 150 't')))]))
              (treeOf (archiveOf [file long "long", file deep "deep", symlink "t" (replicate 150 't')])),
      runTest "a pax header names the path, size and time of the entry after it" $
        assertEqual
          "unpacked"
          (Right (UnpackedArchive (dir [("named by pax", TreeFile False "pax"), ("other", TreeFile False "o")]) 1234567))
          ( snd
              ( unpackBytes
                  (archiveOf [special 'x' "PaxHeader" (paxBody [("path", "named by pax"), ("mtime", "1234567.5")]), file "ignored" "pax", file "other" "o"])
              )
          ),
      runTest "a pax global header is passed over, as GitHub's archives carry one" $
        assertEqual
          "tree"
          (Right (dir [("f", TreeFile False "x")]))
          (treeOf (archiveOf [special 'g' "pax_global_header" (paxBody [("comment", "2aae6c35c94fcfb415dbe95f408b9ce91ee846ed")]), directory "repo", file "repo/f" "x"])),
      runTest "a malformed pax record warns, naming the member, and is ignored" $
        assertEqual
          "unpacked"
          (["getting archive member 'f': Ignoring malformed pax attributes: malformed attribute size field"], Right (dir [("f", TreeFile False "x"), ("g", TreeFile False "y")]))
          (fmap unpackedTree <$> unpackBytes (archiveOf [special 'x' "PaxHeader" "x9 path=no\n", file "f" "x", file "g" "y"])),
      runTest "a hard link entry names its target" $
        assertEqual
          "tree"
          (Right (dir [("a", TreeFile False "same"), ("b", TreeFile False "same")]))
          (treeOf (archiveOf [file "a" "same", hardLink "b" "a"])),
      runTest "a regular entry whose name ends in a slash is a directory" $
        assertEqual
          "tree"
          (Right (dir [("d", dir [("f", TreeFile False "x")]), ("g", TreeFile False "y")]))
          (treeOf (archiveOf [(file "d/" "") {TarEntry.entryTarPath = "d/"}, file "d/f" "x", file "g" "y"])),
      runTest "an empty input is an empty archive" $
        assertEqual "unpacked" ([], Right (UnpackedArchive (dir []) 0)) (unpackBytes BL.empty),
      runTest "bytes that are not an archive are refused as libarchive refuses them" $
        assertEqual "refused" (Left "cannot get archive member name: Unrecognized archive format") (failureOf (treeOf "this is not a tar archive")),
      runTest "a damaged header after the first is refused" $
        let archive = archiveOf [file "a" "x", file "b" "y"]
            damaged = BL.take 1024 archive <> BL.map (const 0x41) (BL.take 512 (BL.drop 1024 archive)) <> BL.drop 1536 archive
         in assertEqual "refused" (Left "cannot get archive member name: Damaged tar archive (bad header checksum)") (failureOf (treeOf damaged)),
      runTest "an archive that ends inside a file is refused" $
        assertEqual
          "refused"
          (Left "cannot read file from tarball: Truncated tar archive detected while reading data")
          (failureOf (treeOf (BL.take 600 (archiveOf [file "a" (BL.replicate 1000 0x61)])))),
      runTest "every chunking of an archive reads the same" $
        let archive = BL.toStrict (archiveOf [file "a" (BL.replicate 700 0x61), symlink "l" "a", directory "d", file "d/b" "b"])
            whole = snd (unpackChunks [archive])
            byteWise = snd (unpackChunks (map BS.singleton (BS.unpack archive)))
         in assertEqual "unpacked" whole byteWise
    ]
  where
    failureOf = either (Left . fetchMessage) (const (Right ()))

-- ---------------------------------------------------------------------------
-- The sink
-- ---------------------------------------------------------------------------

-- | Place entries in order, as the sink places them.
placed :: [ArchiveEntry Text] -> ([Text], Either FetchFailure (UnpackedArchive Text))
placed = go [] emptySink
  where
    go warnings sink entries = case entries of
      [] -> (warnings, finishSink sink)
      entry : rest -> case sinkEntry sink entry of
        (Left failure, given) -> (warnings ++ given, Left failure)
        (Right updated, given) -> go (warnings ++ given) updated rest

at :: ByteString -> ArchiveNode Text -> ArchiveEntry Text
at path = ArchiveEntry path 0

-- | The rules of upstream's tar sink.
testTarSink :: IO [Bool]
testTarSink = do
  putStrLn "fetch/tar-sink"
  sequence
    [ runTest "the last entry at a path wins" $
        assertEqual "tree" (Right (dir [("a", TreeFile False "second"), ("b", TreeFile False "b")])) (treeIn [at "a" (ArchiveFile False "first"), at "a" (ArchiveFile False "second"), at "b" (ArchiveFile False "b")]),
      runTest "a directory onto a directory changes nothing" $
        assertEqual "tree" (Right (dir [("d", dir [("f", TreeFile False "x")]), ("e", dir [])])) (treeIn [at "d" ArchiveDirectory, at "d/f" (ArchiveFile False "x"), at "d" ArchiveDirectory, at "e" ArchiveDirectory]),
      runTest "a non-empty directory is not replaced" $
        assertEqual "refused" (Left (FetchFailure [] "cannot create '/d', conflicting non-empty directory")) (treeIn [at "d/f" (ArchiveFile False "x"), at "d" (ArchiveFile False "y")]),
      runTest "an empty directory is replaced" $
        assertEqual "tree" (Right (dir [("d", TreeFile False "y"), ("e", dir [])])) (treeIn [at "d" ArchiveDirectory, at "d" (ArchiveFile False "y"), at "e" ArchiveDirectory]),
      runTest "an entry under a file is refused" $
        assertEqual "refused" (Left (FetchFailure [] "parent of '/a/b' is not a directory")) (treeIn [at "a" (ArchiveFile False "x"), at "a/b" (ArchiveFile False "y")]),
      runTest "a path is read as CanonPath reads it" $
        assertEqual "tree" (Right (dir [("b", TreeFile False "x"), ("c", TreeFile False "y")])) (treeIn [at "./a/../b" (ArchiveFile False "x"), at "/c//" (ArchiveFile False "y")]),
      runTest "a hard link made before its target is replaced keeps what the target was, with a warning" $
        assertEqual
          "placed"
          ( [ "'/t' is replaced, though a hard link makes it the same file as '/l'. Older versions of Nix resolved hard links differently, so this archive unpacks to a different store path than it used to."
            ],
            Right (dir [("l", TreeFile False "old"), ("t", TreeFile False "new")])
          )
          (fmap unpackedTree <$> placed [at "t" (ArchiveFile False "old"), at "l" (ArchiveHardLink "t"), at "t" (ArchiveFile False "new")]),
      runTest "a hard link made after its target is replaced names what replaced it" $
        assertEqual
          "placed"
          ([], Right (dir [("l", TreeFile False "new"), ("t", TreeFile False "new")]))
          (fmap unpackedTree <$> placed [at "t" (ArchiveFile False "old"), at "t" (ArchiveFile False "new"), at "l" (ArchiveHardLink "t")]),
      runTest "replacing one of three linked paths names the other two" $
        assertEqual
          "warnings"
          ["'/b' is replaced, though a hard link makes it the same file as '/a', '/c'. Older versions of Nix resolved hard links differently, so this archive unpacks to a different store path than it used to."]
          (fst (placed [at "a" (ArchiveFile False "x"), at "b" (ArchiveHardLink "a"), at "c" (ArchiveHardLink "a"), at "b" (ArchiveFile False "y")])),
      runTest "a hard link to nothing is refused, naming the link" $
        assertEqual "refused" (Left (FetchFailure ["while creating a hard link from '/l' to '/x'"] "target does not exist")) (treeIn [at "l" (ArchiveHardLink "x")]),
      runTest "a hard link to a directory is refused" $
        assertEqual "refused" (Left (FetchFailure ["while creating a hard link from '/l' to '/d'"] "target is a directory")) (treeIn [at "d" ArchiveDirectory, at "l" (ArchiveHardLink "d")]),
      runTest "a device is refused by its file type" $
        assertEqual "refused" (Left (FetchFailure [] "file 'top/dev' in tarball has unsupported file type 8192")) (treeIn [at "top/dev" (ArchiveSpecial 8192)]),
      runTest "one top-level directory is the tree" $
        assertEqual "tree" (Right (dir [("f", TreeFile False "x")])) (treeIn [at "pkg/f" (ArchiveFile False "x")]),
      runTest "two top-level directories are kept" $
        assertEqual "tree" (Right (dir [("a", dir [("f", TreeFile False "x")]), ("b", dir [("g", TreeFile False "y")])])) (treeIn [at "a/f" (ArchiveFile False "x"), at "b/g" (ArchiveFile False "y")]),
      runTest "one top-level file is kept under the root" $
        assertEqual "tree" (Right (dir [("f", TreeFile False "x")])) (treeIn [at "f" (ArchiveFile False "x")]),
      runTest "a root replaced by a file is refused" $
        assertEqual "refused" (Left (FetchFailure [] "the archive's root is not a directory")) (treeIn [at "." (ArchiveFile False "x")]),
      runTest "the newest modification time of any entry is the archive's" $
        assertEqual
          "time"
          (Right 300)
          (unpackedLastModified <$> snd (placed [ArchiveEntry "d" 300 ArchiveDirectory, ArchiveEntry "d/f" 200 (ArchiveFile False "x"), ArchiveEntry "l" (-5) (ArchiveSymlink "d")]))
    ]
  where
    treeIn = fmap unpackedTree . snd . placed

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

-- | A tree's NAR, from its pieces with each blob its own bytes.
narOfPieces :: Tree ByteString -> ByteString
narOfPieces tree = BS.concat (map piece (narPieces (fromIntegral . BS.length) tree))
  where
    piece p = case p of
      NarBytes bytes -> bytes
      NarContents _ contents -> contents

-- | A tree as nova-cache's NAR holds one.
narEntryOf :: Tree ByteString -> NAR.NarEntry
narEntryOf tree = case tree of
  TreeFile isExecutable contents -> NAR.NarRegular isExecutable contents
  TreeSymlink target -> NAR.NarSymlink target
  TreeDirectory entries -> NAR.NarDirectory (Map.toList (Map.map narEntryOf entries))

-- | A tree whose directories each hold something, its names from the
-- characters a ustar name holds, some long enough to need a prefix or a
-- GNU long name.
genTree :: Int -> QC.Gen (Tree ByteString)
genTree depth = dir <$> QC.listOf1 ((,) <$> genName <*> genNode)
  where
    genNode =
      QC.frequency
        [ (4, TreeFile <$> QC.arbitrary <*> (BS.pack <$> QC.resize 1200 (QC.listOf QC.arbitrary))),
          (1, TreeSymlink . BC.pack <$> QC.listOf1 (QC.elements nameChars)),
          (if depth > 0 then 2 else 0, genTree (depth - 1))
        ]
    genName = QC.oneof [short, long]
    short = BC.pack <$> QC.resize 12 (QC.listOf1 (QC.elements nameChars)) `QC.suchThat` (`notElem` [".", ".."])
    long = BC.pack <$> (QC.choose (90, 130) >>= (`QC.vectorOf` QC.elements nameChars))
    nameChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "._-"

-- | A tree's entries in the order a writer lists them, a directory before
-- what it holds, each with a modification time drawn for it.
entriesOf :: [Int64] -> Tree ByteString -> [Tar.GenEntry BL.ByteString FilePath FilePath]
entriesOf times tree = zipWith timed (cycle (0 : times)) (go "" tree)
  where
    go prefix node = case node of
      TreeDirectory entries -> concatMap (\(name, sub) -> visit (prefix </> BC.unpack name) sub) (Map.toList entries)
      _ -> []
    visit path node = case node of
      TreeFile isExecutable contents -> [(if isExecutable then executable else file) path (BL.fromStrict contents)]
      TreeSymlink target -> [symlink path (BC.unpack target)]
      TreeDirectory _ -> directory path : go path node
    timed time entry = entry {TarEntry.entryTime = time}

-- | The reader and the sink unpack an archive written from a tree to that
-- tree, however it is chunked; a tree's NAR pieces are its NAR.
testArchiveProperties :: IO [Bool]
testArchiveProperties = do
  putStrLn "fetch/archive-properties"
  unpacks <-
    property $
      QC.forAll (genTree 2) $ \tree ->
        QC.forAll (QC.listOf (QC.choose (0, 1 `shiftL` 20))) $ \times ->
          QC.forAll (QC.listOf1 (QC.choose (1, 2000))) $ \sizes ->
            let entries = entriesOf times tree
                archive = BL.toStrict (archiveOf entries)
                newest = foldl' max 0 (map TarEntry.entryTime entries)
             in snd (unpackChunks (chunked sizes archive)) == Right (UnpackedArchive (dereferenceSingleton tree) newest)
  pieces <- property (QC.forAll (genTree 2) (\tree -> narOfPieces tree == NAR.serialise (narEntryOf tree)))
  sequence
    [ runTest "an archive written from a tree unpacks to that tree" unpacks,
      runTest "a tree's NAR pieces are its NAR" pieces
    ]
  where
    chunked sizes = go (cycle sizes)
      where
        go sizesLeft remaining
          | BS.null remaining = []
          | otherwise = case sizesLeft of
              size : more -> BS.take size remaining : go more (BS.drop size remaining)
              [] -> [remaining]

-- ---------------------------------------------------------------------------
-- fetchTarball
-- ---------------------------------------------------------------------------

-- | @builtins.fetchTarball@ unpacks in process, as upstream does: the
-- tree is the archive's own, its path the one its NAR gives, and a pair
-- of names a volume folds keeps both names.
testFetchTarballIO :: IO [Bool]
testFetchTarballIO = do
  putStrLn "fetch/fetch-tarball-io"
  tmpBase <- getTemporaryDirectory
  let root = tmpBase </> "nova-nix-test-fetch-tarball"
      storeDir = StoreDir (root </> "store")
      fileUrl path = "file://" <> canonPathValue (T.pack path)
      fetchOf path = "builtins.fetchTarball \"" <> T.concatMap nixEscaped (fileUrl path) <> "\""
      nixEscaped c = if c == '"' || c == '\\' then T.pack ['\\', c] else T.singleton c
      write name = BL.writeFile (root </> name)
      expectedPath tree = either (const Nothing) Just (makeFixedOutputPath "source" "sha256" NarIngestion (sha256Digest (NAR.serialise (narEntryOf tree))))
      fetchedAs tree name = do
        fetched <- evalNixIOStore storeDir root (fetchOf (root </> name))
        pure $ case fetched of
          Right val -> assertEqual "path" (expectedPath tree) (fetchedStorePath val)
          Left err -> Fail err
      refusedAs message expr = do
        fetched <- evalNixIOStore storeDir root expr
        pure (assertEqual "refused" (Left message) (void fetched))
      twoTops = dir [("a", dir [("f", TreeFile False "f\n")]), ("b", dir [("g", TreeFile False "g\n")])]
      folded = dir [("Makefile", TreeFile False "upper\n"), ("makefile", TreeFile False "lower\n"), ("run", TreeFile True "#!/bin/sh\n")]
  forceRemoveIfExists root
  createDirectoryIfMissing True (root </> "plain-directory")
  write "two.tar.gz" (GZip.compress (archiveOf [directory "a", file "a/f" "f\n", directory "b", file "b/g" "g\n"]))
  write "folded.tar" (archiveOf [directory "pkg", file "pkg/Makefile" "upper\n", file "pkg/makefile" "lower\n", executable "pkg/run" "#!/bin/sh\n"])
  write "folded.tar.zst" (BL.fromStrict (Zstd.compress Zstd.defaultCompressionLevel (BL.toStrict (archiveOf [directory "pkg", file "pkg/Makefile" "upper\n", file "pkg/makefile" "lower\n", executable "pkg/run" "#!/bin/sh\n"]))))
  results <-
    sequence
      [ runTestM "a tarball with two top-level directories keeps both" (fetchedAs twoTops "two.tar.gz"),
        runTestM "a zstd tarball unpacks to the same tree as a plain one" $
          andThen <$> fetchedAs folded "folded.tar" <*> fetchedAs folded "folded.tar.zst",
        runTestM "a pair of names the store's volume folds keeps both names" $ do
          fetched <- evalNixIOStore storeDir root (fetchOf (root </> "folded.tar"))
          case fetched >>= maybe (Left "not a store path") Right . fetchedStorePath of
            Left err -> pure (Fail err)
            Right sp -> do
              let dest = storePathToFilePath storeDir sp
              sensitivity <- probeCaseSensitivity (unStoreDir storeDir)
              Hash.NixHash onDisk <- ExecBit.narHashOfPath (volumeCaseHack sensitivity) dest
              pure (assertEqual "NAR on disk" (sha256Digest (NAR.serialise (narEntryOf folded))) onDisk),
        runTestM "a tarball that is not there is refused in upstream's words" $
          refusedAs ("tarball \"" <> T.pack (root </> "absent.tar.gz") <> "\" does not exist.") (fetchOf (root </> "absent.tar.gz")),
        runTestM "a directory is refused as no tarball" $
          refusedAs ("tarball \"" <> T.pack (root </> "plain-directory") <> "\" is a directory, not a file.") (fetchOf (root </> "plain-directory"))
      ]
  forceRemoveIfExists root
  pure results
