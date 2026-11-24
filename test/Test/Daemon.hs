-- | Tests of the daemon's wire format and protocol.
module Test.Daemon
  ( testDaemonWire,
    testDaemonProtocol,
    testDaemonFixtures,
  )
where

import Control.Monad (replicateM, void)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.List (isSuffixOf, sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word64, Word8)
import qualified Nix.Daemon.Protocol as Proto
import qualified Nix.Daemon.Wire as Wire
import Nix.Hash (FileIngestionMethod (..), sha256Digest)
import qualified Nix.Hash as Hash
import Nix.Push (storePathBasename)
import Nix.Store.Path (StoreDir (..), StorePath, defaultStoreDir, storePathHashLen, storePathToText)
import Nix.Store.Path.Internal (StorePath (..))
import qualified NovaCache.Base32 as Nix32
import qualified System.Directory as Dir
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Test.Harness (TestResult (..), andThen, assertEqual, assertRight, runTest, runTestM)
import qualified Test.QuickCheck as QC

-- ---------------------------------------------------------------------------
-- Tests: the daemon's wire codec
-- ---------------------------------------------------------------------------

-- | The bytes a builder writes.
builtBytes :: BB.Builder -> BS.ByteString
builtBytes = BL.toStrict . BB.toLazyByteString

-- | A value decoded from all of its input, or why not: a decode that
-- leaves bytes over did not read what was written.
decodeWhole :: Wire.Decoder a -> BS.ByteString -> Either Text a
decodeWhole reader input = case Wire.runDecoder reader input of
  Left err -> Left (T.pack (show err))
  Right (value, rest)
    | BS.null rest -> Right value
    | otherwise -> Left (T.pack (show (BS.length rest)) <> " bytes left over")

-- | Upstream's refusal of bytes, as 'decodeWhole' reports it.
wireRefusal :: Text -> Either Text a
wireRefusal message = Left (T.pack (show (Wire.Malformed message)))

-- | A QuickCheck property as a test result.
codecProperty :: (QC.Testable prop) => prop -> IO TestResult
codecProperty prop = do
  result <- QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} prop
  pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))

-- | Upstream's framing: numbers and their ranges, strings and their
-- padding, lists, and framed streams however their input arrives.
testDaemonWire :: IO [Bool]
testDaemonWire = do
  putStrLn "daemon/wire"
  sequence
    [ runTestM "a word reads back as itself" $
        codecProperty $ \n ->
          decodeWhole Wire.getWord64 (builtBytes (Wire.putNum (n :: Word64))) QC.=== Right n,
      runTestM "a number reads back up to its type's largest value and is refused above it, naming the type as upstream does" $
        codecProperty $
          QC.forAll genWord $ \n ->
            let encoded = builtBytes (Wire.putNum n)
                bounded :: (Integral a) => Text -> Word64 -> Either Text a
                bounded typeCode bound
                  | n <= bound = Right (fromIntegral n)
                  | otherwise = wireRefusal ("serialised integer " <> T.pack (show n) <> " is too large for type '" <> typeCode <> "'")
             in QC.conjoin
                  [ decodeWhole Wire.getWord8 encoded QC.=== bounded "h" 255,
                    decodeWhole Wire.getWord32 encoded QC.=== bounded "j" 4294967295,
                    decodeWhole Wire.getInt64 encoded QC.=== bounded "l" 9223372036854775807,
                    decodeWhole Wire.getBool encoded QC.=== fmap (/= (0 :: Word64)) (bounded "b" 1)
                  ],
      runTest "a negative number is written as its two's complement, which upstream's int64_t read refuses" $
        assertEqual "read" (wireRefusal "serialised integer 18446744073709551615 is too large for type 'l'") (decodeWhole Wire.getInt64 (builtBytes (Wire.putNum (-1 :: Int64)))),
      runTestM "a string reads back as itself, padded with zeros to a multiple of eight" $
        codecProperty $
          QC.forAll genWireBytes $ \bytes ->
            let encoded = builtBytes (Wire.putBytes bytes)
                padding = BS.drop (8 + BS.length bytes) encoded
             in QC.conjoin
                  [ decodeWhole Wire.getBytes encoded QC.=== Right bytes,
                    BS.length encoded `mod` 8 QC.=== 0,
                    QC.property (BS.length padding < 8 && BS.all (== 0) padding)
                  ],
      runTestM "a string with a byte of its padding set is refused" $
        codecProperty $
          QC.forAll (genWireBytes `QC.suchThat` ((/= 0) . (`mod` 8) . BS.length)) $ \bytes ->
            let encoded = builtBytes (Wire.putBytes bytes)
                paddingStart = 8 + BS.length bytes
             in QC.forAll ((,) <$> QC.choose (paddingStart, BS.length encoded - 1) <*> QC.choose (1, 255)) $ \(at, byte) ->
                  let corrupted = BS.take at encoded <> BS.singleton byte <> BS.drop (at + 1) encoded
                   in decodeWhole Wire.getBytes corrupted QC.=== wireRefusal "non-zero padding",
      runTestM "a string cut short asks for more input, and fails when the input ends" $
        codecProperty $
          QC.forAll genWireBytes $ \bytes ->
            let encoded = builtBytes (Wire.putBytes bytes)
             in QC.forAll (QC.choose (0, BS.length encoded - 1)) $ \cut ->
                  let prefix = BS.take cut encoded
                      waiting = case Wire.feedInput (Just prefix) (Wire.startDecoding Wire.getBytes) of
                        Wire.NeedInput _ -> True
                        _ -> False
                   in QC.conjoin [QC.property waiting, fmap fst (Wire.runDecoder Wire.getBytes prefix) QC.=== Left Wire.InputEnded],
      runTestM "a list reads back as itself" $
        codecProperty $
          QC.forAll (QC.listOf genWireBytes) $ \items ->
            decodeWhole (Wire.getList Wire.getBytes) (builtBytes (Wire.putList Wire.putBytes items)) QC.=== Right items,
      runTestM "a framed stream reads back as its bytes however its input is split, and stops at the chunk that ends it" $
        codecProperty $
          QC.forAll ((,) <$> QC.listOf genWireBytes <*> genWireBytes) $ \(chunks, after) ->
            let encoded = builtBytes (foldMap Wire.putFrame chunks <> Wire.putFrameEnd) <> after
             in QC.forAll (genSplit encoded) $ \pieces ->
                  readFrames pieces QC.=== Right (BS.concat chunks, after),
      runTest "a decode that needs no input keeps all it is given as what follows" $
        assertEqual "rest" (Right ((), "rest")) (Wire.runDecoder (pure ()) "rest"),
      runTest "a frame is its length and its bytes, unpadded, and an empty one ends the stream" $
        assertEqual
          "frames"
          ("\3\0\0\0\0\0\0\0abc\0\0\0\0\0\0\0\0" :: BS.ByteString)
          (builtBytes (Wire.putFrame "abc" <> Wire.putFrame "" <> Wire.putFrameEnd)),
      runTest "a frame's length is read as upstream's unsigned int" $
        assertEqual "read" (wireRefusal "serialised integer 4294967296 is too large for type 'j'") (decodeWhole Wire.getFrame (builtBytes (Wire.putNum (4294967296 :: Word64))))
    ]
  where
    genWord = QC.oneof [QC.arbitrary, QC.choose (0, 300), QC.choose (4294967200, 4294967400), QC.choose (9223372036854775800, maxBound), QC.choose (0, 2)]

-- | Bytes of any length, short ones most.
genWireBytes :: QC.Gen BS.ByteString
genWireBytes = BS.pack <$> QC.resize 40 (QC.listOf QC.arbitrary)

-- | Bytes cut at random places into pieces, as reads from a socket might
-- deliver them.
genSplit :: BS.ByteString -> QC.Gen [BS.ByteString]
genSplit bytes = do
  cuts <- sort <$> QC.listOf (QC.choose (0, BS.length bytes))
  pure (zipWith slice (0 : cuts) (cuts ++ [BS.length bytes]))
  where
    slice from to = BS.take (to - from) (BS.drop from bytes)

-- | A framed stream read a chunk at a time from input that arrives in
-- pieces: the bytes it carries, and the input after its end.
readFrames :: [BS.ByteString] -> Either Wire.DecodeError (BS.ByteString, BS.ByteString)
readFrames = go [] (Wire.startDecoding Wire.getFrame)
  where
    go chunks decoding pieces = case decoding of
      Wire.NeedInput _ -> case pieces of
        piece : rest -> go chunks (Wire.feedInput (Just piece) decoding) rest
        [] -> go chunks (Wire.feedInput Nothing decoding) []
      Wire.DecodeFailed err -> Left err
      Wire.Decoded Nothing rest -> Right (BS.concat (reverse chunks), BS.concat (rest : pieces))
      Wire.Decoded (Just chunk) rest -> go (chunk : chunks) (Wire.feedInput (Just rest) (Wire.startDecoding Wire.getFrame)) pieces

-- | Every version the codec's gates distinguish, from upstream's oldest
-- tested to its newest, each with and without the feature that changes
-- how realisations travel.
gateVersions :: [Proto.Version]
gateVersions =
  [ Proto.Version (Proto.VersionNumber 1 minor) features
  | minor <- [10 .. Proto.vnMinor Proto.latestVersionNumber],
    features <- [Set.empty, Set.singleton Proto.featureRealisationWithPath]
  ]

-- | The version at a minor number, with no features.
protoVersion :: Word8 -> Proto.Version
protoVersion minor = Proto.Version (Proto.VersionNumber 1 minor) Set.empty

-- | The newest version, with the feature that carries realisations.
realisationVersion :: Proto.Version
realisationVersion = Proto.Version Proto.latestVersionNumber (Set.singleton Proto.featureRealisationWithPath)

-- | Whether a version's number is at least 1.@minor@.
versionAtLeast :: Proto.Version -> Word8 -> Bool
versionAtLeast version minor = Proto.versionNumber version >= Proto.VersionNumber 1 minor

-- | Whether a version carries realisations.
carriesRealisations :: Proto.Version -> Bool
carriesRealisations = Set.member Proto.featureRealisationWithPath . Proto.versionFeatures

-- | The law every serialiser keeps: at every version, what is written
-- reads back as what that version carries of it, and nothing is left over.
-- @carried@ is this suite's own account of each gate, so a gate dropped
-- from both the reader and the writer still fails it.
roundTripsAtEveryVersion ::
  (Eq a, Show a) =>
  (Proto.Version -> QC.Gen a) ->
  (Proto.Version -> a -> a) ->
  (Proto.Version -> Wire.Decoder a) ->
  (Proto.Version -> a -> BB.Builder) ->
  QC.Property
roundTripsAtEveryVersion gen carried reader writer =
  QC.conjoin
    [ QC.counterexample (show version) $
        QC.forAll (gen version) $ \value ->
          decodeWhole (reader version) (builtBytes (writer version value)) QC.=== Right (carried version value)
    | version <- gateVersions
    ]

-- | The store directory the codec's properties name paths under.
wireStoreDir :: StoreDir
wireStoreDir = defaultStoreDir

-- | A store path: a nix-base32 hash and a name, some of them derivations.
genWireStorePath :: QC.Gen StorePath
genWireStorePath = do
  hashPart <- QC.vectorOf storePathHashLen (QC.elements "0123456789abcdfghijklmnpqrsvwxyz")
  first <- QC.elements ['a' .. 'z']
  rest <- QC.resize 12 (QC.listOf (QC.elements (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "+-._?=")))
  suffix <- QC.elements ["", ".drv"]
  pure (StorePath (T.pack hashPart) (T.pack (first : rest) <> suffix))

genOutputName :: QC.Gen Proto.OutputName
genOutputName = Proto.OutputName . T.pack <$> ((:) <$> QC.elements ['a' .. 'z'] <*> QC.resize 6 (QC.listOf (QC.elements (['a' .. 'z'] ++ ['0' .. '9'] ++ "-_"))))

genOutputsSpec :: QC.Gen Proto.OutputsSpec
genOutputsSpec = QC.oneof [pure Proto.AllOutputs, QC.suchThatMap (Set.fromList <$> QC.resize 3 (QC.listOf1 genOutputName)) Proto.namedOutputs]

genDerivedPath :: QC.Gen Proto.DerivedPath
genDerivedPath = QC.oneof [Proto.DerivedOpaque <$> genWireStorePath, Proto.DerivedBuilt <$> genWireStorePath <*> genOutputsSpec]

genSignature :: QC.Gen Proto.Signature
genSignature = QC.suchThatMap ((,) <$> keyName <*> (BS.pack <$> QC.resize 70 (QC.listOf1 QC.arbitrary))) (uncurry Proto.signatureFromParts)
  where
    keyName = BS8.pack <$> QC.listOf1 (QC.elements (['a' .. 'z'] ++ "-.0"))

genSignatures :: QC.Gen (Set.Set Proto.Signature)
genSignatures = Set.fromList <$> QC.resize 3 (QC.listOf genSignature)

genNarHash :: QC.Gen Proto.NarHash
genNarHash = QC.suchThatMap (BS.pack <$> QC.vectorOf 32 QC.arbitrary) Proto.narHashFromDigest

genContentAddress :: QC.Gen Proto.ContentAddress
genContentAddress = do
  method <- QC.elements [Proto.TextMethod, Proto.FixedMethod FlatIngestion, Proto.FixedMethod NarIngestion]
  algo <- QC.elements ["md5", "sha1", "sha256", "sha512"]
  QC.suchThatMap (BS.pack <$> QC.vectorOf (fromMaybe 0 (Hash.hashAlgoBytes algo)) QC.arbitrary) (Proto.contentAddressFromParts method algo)

genPathInfo :: QC.Gen Proto.UnkeyedValidPathInfo
genPathInfo =
  Proto.UnkeyedValidPathInfo
    <$> QC.oneof [pure Nothing, Just <$> genWireStorePath]
    <*> genNarHash
    <*> (Set.fromList <$> QC.resize 3 (QC.listOf genWireStorePath))
    <*> QC.choose (0, maxBound)
    <*> QC.arbitrary
    <*> QC.arbitrary
    <*> genSignatures
    <*> QC.oneof [pure Nothing, Just <$> genContentAddress]

-- | What a version carries of a path's info: its trust fields from 1.16.
carriedPathInfo :: Proto.Version -> Proto.UnkeyedValidPathInfo -> Proto.UnkeyedValidPathInfo
carriedPathInfo version info
  | versionAtLeast version 16 = info
  | otherwise = info {Proto.upiUltimate = False, Proto.upiSignatures = Set.empty, Proto.upiContentAddress = Nothing}

genUnkeyedRealisation :: QC.Gen Proto.UnkeyedRealisation
genUnkeyedRealisation = Proto.UnkeyedRealisation <$> genWireStorePath <*> genSignatures

genBuildResult :: QC.Gen Proto.BuildResult
genBuildResult = do
  outcome <-
    QC.oneof
      [ Proto.Succeeded <$> (Proto.Success <$> QC.elements [minBound .. maxBound] <*> (Map.fromList <$> QC.resize 3 (QC.listOf ((,) <$> genOutputName <*> genUnkeyedRealisation)))),
        Proto.Failed <$> (Proto.Failure <$> QC.elements [minBound .. maxBound] <*> genWireBytes <*> QC.arbitrary)
      ]
  Proto.BuildResult outcome
    <$> QC.arbitrary
    <*> QC.choose (0, maxBound)
    <*> QC.choose (0, maxBound)
    <*> QC.oneof [pure Nothing, Just <$> QC.choose (0, maxBound)]
    <*> QC.oneof [pure Nothing, Just <$> QC.choose (0, maxBound)]

-- | What a version carries of a build result: how often and when it built
-- from 1.29, its CPU times from 1.37, and its outputs from 1.28, their
-- signatures only with the realisation feature.
carriedBuildResult :: Proto.Version -> Proto.BuildResult -> Proto.BuildResult
carriedBuildResult version result = withCpu (withTimes (result {Proto.brOutcome = outcome}))
  where
    withTimes r
      | versionAtLeast version 29 = r
      | otherwise = r {Proto.brTimesBuilt = 0, Proto.brStartTime = 0, Proto.brStopTime = 0}
    withCpu r
      | versionAtLeast version 37 = r
      | otherwise = r {Proto.brCpuUser = Nothing, Proto.brCpuSystem = Nothing}
    outcome = case Proto.brOutcome result of
      Proto.Succeeded (Proto.Success status outputs) -> Proto.Succeeded (Proto.Success status (carriedOutputs outputs))
      Proto.Failed (Proto.Failure status message nonDeterministic) ->
        Proto.Failed (Proto.Failure status message (nonDeterministic && versionAtLeast version 29))
    carriedOutputs outputs
      | carriesRealisations version = outputs
      | versionAtLeast version 28 = Map.map (\realisation -> realisation {Proto.urSignatures = Set.empty}) outputs
      | otherwise = Map.empty

-- | Whether a store path is a derivation's.
isDrvPath :: StorePath -> Bool
isDrvPath path = ".drv" `T.isSuffixOf` spName path

-- | What a version carries of a derived path: before 1.30 all of a path's
-- outputs are spelled as the bare path, which reads back as the path
-- itself unless it is a derivation's.
carriedDerivedPath :: Proto.Version -> Proto.DerivedPath -> Proto.DerivedPath
carriedDerivedPath version derived = case derived of
  Proto.DerivedBuilt path Proto.AllOutputs
    | not (versionAtLeast version 30) && not (isDrvPath path) -> Proto.DerivedOpaque path
  _ -> derived

-- | Whether a version can spell a derived path: before 1.30 a derivation
-- file itself cannot be asked for.
spellable :: Proto.Version -> Proto.DerivedPath -> Bool
spellable version derived = case derived of
  Proto.DerivedOpaque path -> versionAtLeast version 30 || not (isDrvPath path)
  Proto.DerivedBuilt _ _ -> True

genDaemonError :: QC.Gen Proto.DaemonError
genDaemonError =
  (Proto.DaemonError . Proto.Verbosity <$> QC.arbitrary)
    <*> genWireBytes
    <*> QC.resize 3 (QC.listOf genWireBytes)
    <*> QC.arbitrary

genFields :: QC.Gen [Proto.Field]
genFields = QC.resize 4 (QC.listOf (QC.oneof [Proto.FieldInt <$> QC.arbitrary, Proto.FieldString <$> genWireBytes]))

genStderrMessage :: QC.Gen Proto.StderrMessage
genStderrMessage =
  QC.oneof
    [ Proto.StderrNext <$> genWireBytes,
      Proto.StderrRead <$> QC.arbitrary,
      Proto.StderrWrite <$> genWireBytes,
      pure Proto.StderrLast,
      Proto.StderrError <$> genDaemonError,
      Proto.StderrStartActivity
        <$> ( (Proto.Activity . Proto.ActivityId <$> QC.arbitrary)
                <*> (Proto.Verbosity <$> QC.arbitrary)
                <*> (Proto.ActivityType <$> QC.arbitrary)
                <*> genWireBytes
                <*> genFields
                <*> (Proto.ActivityId <$> QC.arbitrary)
            ),
      Proto.StderrStopActivity . Proto.ActivityId <$> QC.arbitrary,
      (Proto.StderrResult . Proto.ActivityId <$> QC.arbitrary) <*> (Proto.ResultType <$> QC.arbitrary) <*> genFields
    ]

-- | What a version carries of a message: from 1.26 an error goes as a
-- record without its exit status, which reads back as 1; before, as its
-- message and exit status, which read back at the error level with no
-- traces.
carriedStderrMessage :: Proto.Version -> Proto.StderrMessage -> Proto.StderrMessage
carriedStderrMessage version message = case message of
  Proto.StderrError err
    | versionAtLeast version 26 -> Proto.StderrError err {Proto.errExitStatus = 1}
    | otherwise -> Proto.StderrError err {Proto.errLevel = Proto.errorVerbosity, Proto.errTraces = []}
  _ -> message

-- | Every value the protocol carries, written and read back at every
-- version gate, and the refusals upstream makes of bytes it does not
-- accept.
testDaemonProtocol :: IO [Bool]
testDaemonProtocol = do
  putStrLn "daemon/protocol"
  sequence
    [ runTestM "a version number reads back as itself" $
        codecProperty $ \major minor ->
          let number = Proto.VersionNumber major minor
           in decodeWhole Proto.getVersionNumber (builtBytes (Proto.putVersionNumber number)) QC.=== Right number,
      runTest "a version number is the major number in the second byte and the minor in the first" $
        assertEqual "1.38" ("\x26\x01\0\0\0\0\0\0" :: BS.ByteString) (builtBytes (Proto.putVersionNumber Proto.latestVersionNumber)),
      runTestM "a feature list reads back as its set" $
        codecProperty $
          QC.forAll (QC.listOf genWireBytes) $ \names ->
            let features = Set.fromList (map Proto.Feature names)
             in decodeWhole Proto.getFeatures (builtBytes (Wire.putList Wire.putBytes names)) QC.=== Right features
                  QC..&&. decodeWhole Proto.getFeatures (builtBytes (Proto.putFeatures features)) QC.=== Right features,
      runTestM "what a daemon tells its client after the handshake reads back as far as each version carries it"
        $ codecProperty
        $ roundTripsAtEveryVersion
          (const ((Proto.HandshakeInfo . Just <$> genWireBytes) <*> QC.elements [Nothing, Just Proto.Trusted, Just Proto.NotTrusted]))
          ( \version (Proto.HandshakeInfo daemonVersion trust) ->
              Proto.HandshakeInfo (if versionAtLeast version 33 then daemonVersion else Nothing) (if versionAtLeast version 35 then trust else Nothing)
          )
          Proto.getHandshakeInfo
          (\version (Proto.HandshakeInfo daemonVersion trust) -> Proto.putHandshakeInfo version (fromMaybe BS.empty daemonVersion) trust),
      runTestM "a client's obsolete fields are read whole at every version"
        $ codecProperty
        $ QC.conjoin [decodeWhole (Proto.getClientObsoleteFields version) (builtBytes (Proto.putClientObsoleteFields version)) QC.=== Right () | version <- gateVersions],
      runTest "a client's CPU affinity, when set, is followed by a number" $
        assertEqual "read" (Right ()) (decodeWhole (Proto.getClientObsoleteFields (protoVersion 14)) (builtBytes (Wire.putNum (1 :: Word64) <> Wire.putNum (7 :: Word64) <> Wire.putBool False))),
      runTest "a handshake that opens with another word is refused" $
        assertEqual "client" (wireRefusal "protocol mismatch") (decodeWhole Proto.getClientMagic (builtBytes Proto.putServerMagic))
          `andThen` assertEqual "server" (wireRefusal "nix-daemon protocol mismatch from") (decodeWhole Proto.getServerMagic (builtBytes Proto.putClientMagic)),
      runTestM "a store path, a store path or none, and a set of them read back as themselves" $
        codecProperty $
          QC.forAll ((,,) <$> genWireStorePath <*> QC.oneof [pure Nothing, Just <$> genWireStorePath] <*> (Set.fromList <$> QC.listOf genWireStorePath)) $ \(path, optional, paths) ->
            QC.conjoin
              [ decodeWhole (Proto.getStorePath wireStoreDir) (builtBytes (Proto.putStorePath wireStoreDir path)) QC.=== Right path,
                decodeWhole (Proto.getStorePath (StoreDir "/tmp/nova-store")) (builtBytes (Proto.putStorePath (StoreDir "/tmp/nova-store") path)) QC.=== Right path,
                decodeWhole (Proto.getOptionalStorePath wireStoreDir) (builtBytes (Proto.putOptionalStorePath wireStoreDir optional)) QC.=== Right optional,
                decodeWhole (Proto.getStorePathSet wireStoreDir) (builtBytes (Proto.putStorePathSet wireStoreDir paths)) QC.=== Right paths
              ],
      runTestM "a store path spelled with empty, . and .. components reads as the path it names, as upstream's canonPath makes it" $
        codecProperty $
          QC.forAll ((,) <$> genWireStorePath <*> QC.elements ["/nix/store//", "/nix/./store/", "/nix/x/../store/", "//nix/store/", "/../nix/store/"]) $ \(path, prefix) ->
            readStorePath (prefix <> storePathBasename path <> "/") QC.=== Right path,
      runTest "a store path is refused in upstream's words" $
        assertEqual "empty" (wireRefusal "empty path is not a valid store path") (readStorePath "")
          `andThen` assertEqual "relative" (wireRefusal "not an absolute path: \"nix/store/x\"") (readStorePath "nix/store/x")
          `andThen` assertEqual "the store itself" (wireRefusal "path \"/nix/store\" is not in the Nix store") (readStorePath "/nix/store/.")
          `andThen` assertEqual "elsewhere" (wireRefusal "path \"/tmp/g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-foo\" is not in the Nix store") (readStorePath "/tmp/g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-foo")
          `andThen` assertEqual "beneath one" (wireRefusal "path \"/nix/store/g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-foo/bin\" is not in the Nix store") (readStorePath "/nix/store/g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-foo/bin")
          `andThen` assertEqual "short" (wireRefusal "'g1w7-foo' is too short to be a valid store path") (readStorePath "/nix/store/g1w7-foo")
          `andThen` assertEqual "no dash" (wireRefusal "'g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q_foo' can't name a store path because the hash part is not followed by a '-'") (readStorePath "/nix/store/g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q_foo")
          `andThen` assertEqual "hash" (wireRefusal "store path 'e1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-foo' contains illegal base-32 character 'e'") (readStorePath "/nix/store/e1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-foo")
          `andThen` assertEqual "name" (wireRefusal "path 'g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-f$o' is not a valid store path: name 'f$o' contains illegal character '$'") (readStorePath "/nix/store/g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-f$o"),
      runTestM "a signature set, a NAR hash and a content address read back as themselves" $
        codecProperty $
          QC.forAll ((,,) <$> genSignatures <*> genNarHash <*> QC.oneof [pure Nothing, Just <$> genContentAddress]) $ \(sigs, narHash, ca) ->
            QC.conjoin
              [ decodeWhole Proto.getSignatureSet (builtBytes (Proto.putSignatureSet sigs)) QC.=== Right sigs,
                decodeWhole Proto.getNarHash (builtBytes (Proto.putNarHash narHash)) QC.=== Right narHash,
                decodeWhole Proto.getOptionalContentAddress (builtBytes (Proto.putOptionalContentAddress ca)) QC.=== Right ca
              ],
      runTest "a NAR hash reads in any spelling of a SHA-256 hash" $
        let digest = sha256Digest "nar"
         in assertEqual "nix32" (Proto.narHashFromDigest digest) (either (const Nothing) Just (decodeWhole Proto.getNarHash (builtBytes (Wire.putBytes (TE.encodeUtf8 ("sha256:" <> Nix32.encode digest))))))
              `andThen` assertEqual "other algorithm" (wireRefusal "hash 'sha1:0000000000000000000000000000000000000000' should have type 'sha256'") (decodeWhole Proto.getNarHash (builtBytes (Wire.putBytes "sha1:0000000000000000000000000000000000000000"))),
      runTest "a signature is refused in upstream's words" $
        let readSignatures spelled = decodeWhole Proto.getSignatureSet (builtBytes (Wire.putList Wire.putBytes [spelled]))
         in assertEqual "no colon" (wireRefusal "signature is corrupt") (readSignatures "key")
              `andThen` assertEqual "no name" (wireRefusal "signature is corrupt") (readSignatures ":AAAA")
              `andThen` assertEqual "no bytes" (wireRefusal "signature is corrupt") (readSignatures "key:")
              `andThen` assertEqual "not base64" (wireRefusal "invalid character in Base64 string: '@'") (readSignatures "key:AA@A"),
      runTest "a content address is refused in upstream's words" $
        let readCa spelled = decodeWhole Proto.getContentAddress (builtBytes (Wire.putBytes spelled))
         in assertEqual "no prefix" (wireRefusal "not a content address because it is not in the form '<prefix>:<rest>': sha256") (readCa "sha256")
              `andThen` assertEqual "prefix" (wireRefusal "content address prefix 'flat' is unrecognized. Recogonized prefixes are 'text' or 'fixed'") (readCa "flat:sha256:x")
              `andThen` assertEqual "no hash" (wireRefusal "content address hash must be in form '<algo>:<hash>', but found: text:sha256") (readCa "text:sha256")
              `andThen` assertEqual "git" (wireRefusal "experimental Nix feature 'git-hashing' is disabled; add '--extra-experimental-features git-hashing' to enable it") (readCa "fixed:git:sha1:x")
              `andThen` assertEqual "algorithm" (wireRefusal "unknown hash algorithm 'sha3', expect 'md5', 'sha1', 'sha256', or 'sha512'") (readCa "fixed:r:sha3:x"),
      runTest "a content address's base64 digest ends at its padding, whatever bytes follow it, as upstream reads it" $
        assertEqual
          "read"
          (maybe (Left "no expected value") Right (Proto.contentAddressFromParts (Proto.FixedMethod FlatIngestion) "md5" (BS.replicate 16 0)))
          (decodeWhole Proto.getContentAddress (builtBytes (Wire.putBytes ("fixed:md5:" <> BS8.replicate 22 'A' <> "=\xff")))),
      runTestM "a path's info reads back as far as each version carries it"
        $ codecProperty
        $ roundTripsAtEveryVersion
          (const genPathInfo)
          carriedPathInfo
          (Proto.getUnkeyedValidPathInfo wireStoreDir)
          (Proto.putUnkeyedValidPathInfo wireStoreDir),
      runTestM "a path and its info read back as far as each version carries it"
        $ codecProperty
        $ roundTripsAtEveryVersion
          (const (Proto.ValidPathInfo <$> genWireStorePath <*> genPathInfo))
          (\version (Proto.ValidPathInfo path info) -> Proto.ValidPathInfo path (carriedPathInfo version info))
          (Proto.getValidPathInfo wireStoreDir)
          (Proto.putValidPathInfo wireStoreDir),
      runTestM "a derived path reads back as far as each version spells it, and one it cannot spell is refused"
        $ codecProperty
        $ QC.conjoin
          [ QC.counterexample (show version) $
              QC.forAll genDerivedPath $ \derived ->
                case Proto.putDerivedPath wireStoreDir version derived of
                  Right builder ->
                    QC.counterexample "written" (QC.property (spellable version derived))
                      QC..&&. decodeWhole (Proto.getDerivedPath wireStoreDir version) (builtBytes builder) QC.=== Right (carriedDerivedPath version derived)
                  Left _ -> QC.counterexample "refused" (QC.property (not (spellable version derived)))
          | version <- gateVersions
          ],
      runTest "a derivation file is refused before 1.30 in upstream's words" $
        assertEqual
          "refusal"
          (Left "trying to request '/nix/store/g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-bar.drv', but daemon protocol 1.29 is too old (< 1.29) to request a derivation file")
          (builtBytes <$> Proto.putDerivedPath wireStoreDir (protoVersion 29) (Proto.DerivedOpaque (g1w7 "bar.drv"))),
      runTest "a derived path is refused in upstream's words" $
        let readDerived minor spelled = decodeWhole (Proto.getDerivedPath wireStoreDir (protoVersion minor)) (builtBytes (Wire.putBytes spelled))
            bar = "/nix/store/g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-bar.drv"
         in assertEqual
              "dynamic"
              (wireRefusal "experimental Nix feature 'dynamic-derivations' is disabled (building output 'out' of 'g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-bar.drv'); add '--extra-experimental-features dynamic-derivations' to enable it")
              (readDerived 30 (bar <> "!out!lib"))
              `andThen` assertEqual "no names" (wireRefusal "name must not be empty") (readDerived 30 (bar <> "!"))
              `andThen` assertEqual "empty name" (wireRefusal "name must not be empty") (readDerived 30 (bar <> "!out,,lib"))
              `andThen` assertEqual "bad name" (wireRefusal "name 'a$' contains illegal character '$'") (readDerived 30 (bar <> "!z$,a$"))
              `andThen` assertEqual "unchecked before 1.30" (Right (Proto.DerivedBuilt (g1w7 "bar.drv") (fromMaybe Proto.AllOutputs (Proto.namedOutputs (Set.fromList [Proto.OutputName "a$", Proto.OutputName "out!x"]))))) (readDerived 29 (bar <> "!a$,,out!x"))
              `andThen` assertEqual "a bare derivation before 1.30" (Right (Proto.DerivedBuilt (g1w7 "bar.drv") Proto.AllOutputs)) (readDerived 29 (bar <> "!,")),
      runTestM "realisations read back with the feature that carries them, and are refused without it"
        $ codecProperty
        $ QC.conjoin
          [ QC.counterexample (show version) $
              QC.forAll ((,,) <$> genUnkeyedRealisation <*> (Proto.DrvOutput <$> genWireStorePath <*> genOutputName) <*> QC.oneof [pure Nothing, Just <$> genUnkeyedRealisation]) $ \(unkeyed, output, optional) ->
                let readBack reader = fmap (decodeWhole (reader wireStoreDir version) . builtBytes)
                    buildTrace = "the daemon is missing the 'realisation-with-path-not-hash' protocol feature, needed to understand build trace"
                    contentAddressing = "the daemon is missing the 'realisation-with-path-not-hash' protocol feature, needed to support content-addressing derivations"
                    pathBytes path = Wire.putBytes (TE.encodeUtf8 (storePathToText wireStoreDir path))
                 in if carriesRealisations version
                      then
                        QC.conjoin
                          [ readBack Proto.getUnkeyedRealisation (Proto.putUnkeyedRealisation wireStoreDir version unkeyed) QC.=== Right (Right unkeyed),
                            readBack Proto.getDrvOutput (Proto.putDrvOutput wireStoreDir version output) QC.=== Right (Right output),
                            readBack Proto.getRealisation (Proto.putRealisation wireStoreDir version (Proto.Realisation output unkeyed)) QC.=== Right (Right (Proto.Realisation output unkeyed)),
                            readBack Proto.getOptionalUnkeyedRealisation (Proto.putOptionalUnkeyedRealisation wireStoreDir version optional) QC.=== Right (Right optional)
                          ]
                      else
                        QC.conjoin
                          [ fmap builtBytes (Proto.putUnkeyedRealisation wireStoreDir version unkeyed) QC.=== Left buildTrace,
                            fmap builtBytes (Proto.putDrvOutput wireStoreDir version output) QC.=== Left contentAddressing,
                            fmap builtBytes (Proto.putRealisation wireStoreDir version (Proto.Realisation output unkeyed)) QC.=== Left contentAddressing,
                            fmap builtBytes (Proto.putOptionalUnkeyedRealisation wireStoreDir version (Just unkeyed)) QC.=== Left buildTrace,
                            decodeWhole (Proto.getUnkeyedRealisation wireStoreDir version) (builtBytes (pathBytes (Proto.urOutPath unkeyed) <> Wire.putList Wire.putBytes [])) QC.=== wireRefusal buildTrace,
                            decodeWhole (Proto.getDrvOutput wireStoreDir version) (builtBytes (pathBytes (Proto.doDrvPath output) <> Proto.putOutputName (Proto.doOutputName output))) QC.=== wireRefusal contentAddressing,
                            readBack Proto.getOptionalUnkeyedRealisation (Proto.putOptionalUnkeyedRealisation wireStoreDir version Nothing) QC.=== Right (Right Nothing)
                          ]
          | version <- gateVersions
          ],
      runTestM "a build result reads back as far as each version carries it"
        $ codecProperty
        $ roundTripsAtEveryVersion (const genBuildResult) carriedBuildResult (Proto.getBuildResult wireStoreDir) (Proto.putBuildResult wireStoreDir),
      runTestM "a keyed build result reads back as far as each version carries it"
        $ codecProperty
        $ QC.conjoin
          [ QC.counterexample (show version) $
              QC.forAll ((,) <$> (genDerivedPath `QC.suchThat` spellable version) <*> genBuildResult) $ \(derived, result) ->
                fmap (decodeWhole (Proto.getKeyedBuildResult wireStoreDir version) . builtBytes) (Proto.putKeyedBuildResult wireStoreDir version (Proto.KeyedBuildResult derived result))
                  QC.=== Right (Right (Proto.KeyedBuildResult (carriedDerivedPath version derived) (carriedBuildResult version result)))
          | version <- gateVersions
          ],
      runTest "a build's outputs in the older JSON form are refused in upstream's words" $
        let readOutputs pairs = decodeWhole (Proto.getBuildResult wireStoreDir (protoVersion 28)) (builtBytes (Wire.putNum (0 :: Word64) <> Wire.putBytes "" <> Wire.putList (\(k, v) -> Wire.putBytes k <> Wire.putBytes v) pairs))
         in assertEqual "id" (wireRefusal "Invalid derivation output id out") (readOutputs [("out", "{}")])
              `andThen` assertEqual "object" (wireRefusal "Expected JSON value to be of type 'object' but it is of type 'array': []") (readOutputs [("x!out", "[]")])
              `andThen` assertEqual "key" (wireRefusal "Expected JSON object to contain key 'outPath' but it doesn't: {\"id\":\"x\"}") (readOutputs [("x!out", "{\"id\":\"x\"}")])
              `andThen` assertEqual "string" (wireRefusal "Expected JSON value to be of type 'string' but it is of type 'number': 5") (readOutputs [("x!out", "{\"outPath\":5}")]),
      runTest "a build mode, a status, a trust flag, a collection's action and paths, and a tag are each refused out of range" $
        let readByte reader byte = decodeWhole reader (builtBytes (Wire.putNum (byte :: Word64)))
         in assertEqual "mode" (wireRefusal "Invalid build mode") (readByte Proto.getBuildMode 3)
              `andThen` assertEqual "status" (wireRefusal "Invalid BuildResult status 15 from remote") (void (decodeWhole (Proto.getBuildResult wireStoreDir (protoVersion 27)) (builtBytes (Wire.putNum (15 :: Word64) <> Wire.putBytes ""))))
              `andThen` assertEqual "trust" (wireRefusal "Invalid trusted status from remote") (readByte Proto.getTrust 3)
              `andThen` assertEqual "action" (wireRefusal "Invalid GC action") (readByte Proto.getGCAction 4)
              `andThen` assertEqual "paths" (wireRefusal "Invalid whole store indicator from remote") (readByte (Proto.getGCPaths wireStoreDir) 2)
              `andThen` assertEqual "realisation" (wireRefusal "Invalid optional build trace from remote") (readByte (Proto.getOptionalUnkeyedRealisation wireStoreDir realisationVersion) 2)
              `andThen` assertEqual
                "duration"
                (wireRefusal "Invalid optional tag from remote")
                (void (decodeWhole (Proto.getBuildResult wireStoreDir (protoVersion 37)) (builtBytes (Wire.putNum (0 :: Word64) <> Wire.putBytes "" <> foldMap Wire.putNum [0, 0, 0, 0 :: Word64] <> Wire.putNum (2 :: Word64))))),
      runTestM "a build mode, a trust flag, a collection's action and its paths read back as themselves"
        $ codecProperty
        $ QC.forAll
          ( (,,,)
              <$> QC.elements [Proto.BuildNormal, Proto.BuildRepair, Proto.BuildCheck]
              <*> QC.elements [Nothing, Just Proto.Trusted, Just Proto.NotTrusted]
              <*> QC.elements [Proto.ReturnLive, Proto.ReturnDead, Proto.DeleteDead, Proto.DeleteSpecific]
              <*> QC.oneof [pure Proto.WholeStore, (Proto.SpecificPaths . Set.fromList <$> QC.listOf genWireStorePath) <*> QC.arbitrary]
          )
        $ \(mode, trust, action, paths) ->
          QC.conjoin
            [ decodeWhole Proto.getBuildMode (builtBytes (Proto.putBuildMode mode)) QC.=== Right mode,
              decodeWhole Proto.getTrust (builtBytes (Proto.putTrust trust)) QC.=== Right trust,
              decodeWhole Proto.getGCAction (builtBytes (Proto.putGCAction action)) QC.=== Right action,
              decodeWhole (Proto.getGCPaths wireStoreDir) (builtBytes (Proto.putGCPaths wireStoreDir paths)) QC.=== Right paths
            ],
      runTestM "a daemon's message to its client reads back as far as each version carries it"
        $ codecProperty
        $ roundTripsAtEveryVersion (const genStderrMessage) carriedStderrMessage Proto.getStderrMessage Proto.putStderrMessage,
      runTest "a daemon's messages, its errors and a collection's paths are laid out as upstream writes them" $
        let word = Wire.putNum :: Word64 -> BB.Builder
            failure = Proto.DaemonError (Proto.Verbosity 3) "msg" ["outer", "inner"] 7
            activity = Proto.Activity (Proto.ActivityId 5) (Proto.Verbosity 4) (Proto.ActivityType 105) "text" [Proto.FieldInt 9, Proto.FieldString "s"] (Proto.ActivityId 2)
            written version message = builtBytes (Proto.putStderrMessage (protoVersion version) message)
         in assertEqual
              "error record"
              (builtBytes (word 0x63787470 <> Wire.putBytes "Error" <> word 3 <> Wire.putBytes "Error" <> Wire.putBytes "msg" <> word 0 <> word 2 <> word 0 <> Wire.putBytes "outer" <> word 0 <> Wire.putBytes "inner"))
              (written 26 (Proto.StderrError failure))
              `andThen` assertEqual "error before 1.26" (builtBytes (word 0x63787470 <> Wire.putBytes "msg" <> word 7)) (written 25 (Proto.StderrError failure))
              `andThen` assertEqual
                "activity"
                (builtBytes (word 0x53545254 <> word 5 <> word 4 <> word 105 <> Wire.putBytes "text" <> word 2 <> word 0 <> word 9 <> word 1 <> Wire.putBytes "s" <> word 2))
                (written 38 (Proto.StderrStartActivity activity))
              `andThen` assertEqual "result" (builtBytes (word 0x52534c54 <> word 5 <> word 101 <> word 1 <> word 1 <> Wire.putBytes "s")) (written 38 (Proto.StderrResult (Proto.ActivityId 5) (Proto.ResultType 101) [Proto.FieldString "s"]))
              `andThen` assertEqual "stop" (builtBytes (word 0x53544f50 <> word 5)) (written 38 (Proto.StderrStopActivity (Proto.ActivityId 5)))
              `andThen` assertEqual "read" (builtBytes (word 0x64617461 <> word 4096)) (written 38 (Proto.StderrRead 4096))
              `andThen` assertEqual "action" (builtBytes (word 3)) (builtBytes (Proto.putGCAction Proto.DeleteSpecific))
              `andThen` assertEqual
                "specific paths"
                (builtBytes (word 0 <> word 1 <> Wire.putBytes "/nix/store/g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-foo" <> word 1))
                (builtBytes (Proto.putGCPaths wireStoreDir (Proto.SpecificPaths (Set.singleton (g1w7 "foo")) True)))
              `andThen` assertEqual "whole store" (builtBytes (word 1)) (builtBytes (Proto.putGCPaths wireStoreDir Proto.WholeStore))
              `andThen` assertEqual "features" (builtBytes (word 2 <> Wire.putBytes "a" <> Wire.putBytes "b")) (builtBytes (Proto.putFeatures (Set.fromList [Proto.Feature "b", Proto.Feature "a"]))),
      runTest "a daemon's message is refused in upstream's words" $
        let version = protoVersion 38
            readMessage builder = decodeWhole (Proto.getStderrMessage version) (builtBytes builder)
         in assertEqual "message" (wireRefusal "got unknown message type 1234 from Nix daemon") (readMessage (Wire.putNum (0x1234 :: Word64)))
              `andThen` assertEqual "error type" (wireRefusal "unexpected error type 'Oops'") (readMessage (Wire.putNum (0x63787470 :: Word64) <> Wire.putBytes "Oops"))
              `andThen` assertEqual
                "position"
                (wireRefusal "deserializing error positions is not supported")
                (readMessage (Wire.putNum (0x63787470 :: Word64) <> Wire.putBytes "Error" <> Wire.putNum (0 :: Word64) <> Wire.putBytes "Error" <> Wire.putBytes "msg" <> Wire.putNum (1 :: Word64)))
              `andThen` assertEqual
                "field"
                (wireRefusal "got unsupported field type 2 from Nix daemon")
                (readMessage (Wire.putNum (0x52534c54 :: Word64) <> Wire.putNum (1 :: Word64) <> Wire.putNum (0 :: Word64) <> Wire.putNum (1 :: Word64) <> Wire.putNum (2 :: Word64)))
    ]
  where
    readStorePath spelled = decodeWhole (Proto.getStorePath wireStoreDir) (builtBytes (Wire.putBytes (TE.encodeUtf8 spelled)))

-- | A store path under upstream's fixtures' hash.
g1w7 :: Text -> StorePath
g1w7 = StorePath "g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q"

-- | The environment variable naming a directory holding upstream's
-- worker-protocol fixtures (@src/libstore-tests/data/worker-protocol@ at
-- the tag CI fetches).  Unset, the group skips; set, every fixture must be
-- there.
workerProtocolDirVar :: String
workerProtocolDirVar = "NOVA_NIX_TEST_WORKER_PROTOCOL_DIR"

-- | One of upstream's fixtures and what reading it must give: the value
-- its own test gives at the version it reads at, and, where upstream
-- writes the value back too, the same bytes from writing what was read.
wireFixture :: (Eq a, Show a) => FilePath -> Wire.Decoder a -> Maybe a -> Maybe (a -> Either Text BB.Builder) -> (FilePath, BS.ByteString -> TestResult)
wireFixture stem reader expected writer = (stem <> ".bin", check)
  where
    check bytes = case (decodeWhole reader bytes, expected) of
      (Left err, _) -> Fail ("read: " <> err)
      (Right _, Nothing) -> Fail "the expected value does not build"
      (Right value, Just wanted) ->
        assertEqual "read" wanted value `andThen` case writer of
          Nothing -> Pass
          Just write -> assertRight "write" (write value) (assertEqual "written" bytes . builtBytes)

-- | A writer that cannot refuse.
totalWriter :: (a -> BB.Builder) -> Maybe (a -> Either Text BB.Builder)
totalWriter writer = Just (Right . writer)

-- | Upstream 2.35.4's worker-protocol fixtures, each as its
-- @worker-protocol.cc@ reads it.  @build-result-1.28@, @-1.29@ and @-1.37@
-- hold outputs in the older JSON form with a real derivation hash, which
-- upstream reads but writes back with 'dummy' hashes, so it reads them only
-- (@build-result-1.29-compat@ is the form it writes).
upstreamWireFixtures :: [(FilePath, BS.ByteString -> TestResult)]
upstreamWireFixtures =
  [ -- Upstream's last string is the C literal "oh no \0\0\0 what was that!",
    -- which becomes a std::string at its first NUL.
    wireFixture "string" (replicateM 5 Wire.getBytes) (Just ["", "hi", "white rabbit", BS.pack [0xe5, 0xa4, 0xa7, 0xe7, 0x99, 0xbd, 0xe5, 0x85, 0x94], "oh no "]) (totalWriter (foldMap Wire.putBytes)),
    wireFixture "store-path" (replicateM 2 (Proto.getStorePath defaultStoreDir)) (Just [g1w7 "foo", g1w7 "foo-bar"]) (totalWriter (foldMap (Proto.putStorePath defaultStoreDir))),
    wireFixture "content-address" (replicateM 3 Proto.getContentAddress) (sequence [textDerive, flatBlob, narParens]) (totalWriter (foldMap Proto.putContentAddress)),
    wireFixture "derived-path-1.29" (derivedPaths 29 3) (Just [Proto.DerivedOpaque (g1w7 "foo"), Proto.DerivedBuilt (g1w7 "bar.drv") Proto.AllOutputs, builtXY]) (derivedWriter 29),
    wireFixture "derived-path-1.30" (derivedPaths 30 4) (Just [Proto.DerivedOpaque (g1w7 "foo"), Proto.DerivedOpaque (g1w7 "foo.drv"), Proto.DerivedBuilt (g1w7 "bar.drv") Proto.AllOutputs, builtXY]) (derivedWriter 30),
    wireFixture
      "drv-output-realisation-with-path-not-hash"
      (replicateM 2 (Proto.getDrvOutput defaultStoreDir realisationVersion))
      (Just [Proto.DrvOutput (g1w7 "foo.drv") (Proto.OutputName "baz"), Proto.DrvOutput (g1w7 "foo.drv") (Proto.OutputName "quux")])
      (Just (fmap mconcat . traverse (Proto.putDrvOutput defaultStoreDir realisationVersion))),
    wireFixture "unkeyed-realisation-realisation-with-path-not-hash" (Proto.getUnkeyedRealisation defaultStoreDir realisationVersion) (Proto.UnkeyedRealisation (g1w7 "foo") <$> zeroSignatures ["asdf", "qwer"]) (Just (Proto.putUnkeyedRealisation defaultStoreDir realisationVersion)),
    wireFixture
      "realisation-realisation-with-path-not-hash"
      (Proto.getRealisation defaultStoreDir realisationVersion)
      (Proto.Realisation (Proto.DrvOutput (g1w7 "foo.drv") (Proto.OutputName "baz")) . Proto.UnkeyedRealisation (g1w7 "foo") <$> zeroSignatures ["asdf", "qwer"])
      (Just (Proto.putRealisation defaultStoreDir realisationVersion)),
    wireFixture "build-result-1.27" (buildResults (protoVersion 27)) (Just [rejected, notDeterministic False 0, built Map.empty 0 Nothing]) (buildWriter (protoVersion 27)),
    wireFixture "build-result-1.28" (buildResults (protoVersion 28)) (Just [rejected, notDeterministic False 0, built fooBar 0 Nothing]) Nothing,
    wireFixture "build-result-1.29" (buildResults (protoVersion 29)) (Just [rejected, notDeterministic True 3, built fooBar 1 Nothing]) Nothing,
    wireFixture "build-result-1.29-compat" (buildResults (protoVersion 29)) (Just [rejected, notDeterministic True 3, built fooBar 1 Nothing]) (buildWriter (protoVersion 29)),
    wireFixture "build-result-1.37" (buildResults (protoVersion 37)) (Just [rejected, notDeterministic True 3, built fooBar 1 cpuTimes]) Nothing,
    wireFixture "build-result-realisation-with-path-not-hash" (buildResults realisationVersion) (Just [rejected, notDeterministic True 3, built fooBar 1 cpuTimes]) (buildWriter realisationVersion),
    wireFixture
      "keyed-build-result-1.29"
      (replicateM 2 (Proto.getKeyedBuildResult defaultStoreDir (protoVersion 29)))
      ( Just
          [ Proto.KeyedBuildResult (Proto.DerivedOpaque (g1w7 "xxx")) rejected,
            Proto.KeyedBuildResult (Proto.DerivedBuilt (g1w7 "bar.drv") (fromMaybe Proto.AllOutputs (Proto.namedOutputs (Set.singleton (Proto.OutputName "out"))))) (notDeterministic True 3)
          ]
      )
      (Just (fmap mconcat . traverse (Proto.putKeyedBuildResult defaultStoreDir (protoVersion 29)))),
    wireFixture
      "unkeyed-valid-path-info-1.15"
      (replicateM 2 (Proto.getUnkeyedValidPathInfo defaultStoreDir (protoVersion 15)))
      (sequence [pathInfo Nothing [], pathInfo (Just (g1w7 "bar.drv")) [StorePath "g1w7hyyyy1w7hy3qg1w7hy3qgqqqqy3q" "foo.drv"]])
      (totalWriter (foldMap (Proto.putUnkeyedValidPathInfo defaultStoreDir (protoVersion 15)))),
    wireFixture
      "valid-path-info-1.15"
      (replicateM 2 (Proto.getValidPathInfo defaultStoreDir (protoVersion 15)))
      (sequence [Proto.ValidPathInfo (g1w7 "bar") <$> pathInfo Nothing [], Proto.ValidPathInfo (g1w7 "bar") <$> pathInfo (Just (g1w7 "bar.drv")) [yyyyFoo, g1w7 "bar"]])
      (totalWriter (foldMap (Proto.putValidPathInfo defaultStoreDir (protoVersion 15)))),
    wireFixture
      "valid-path-info-1.16"
      (replicateM 3 (Proto.getValidPathInfo defaultStoreDir (protoVersion 16)))
      ( sequence
          [ Proto.ValidPathInfo (g1w7 "bar") . (\info -> info {Proto.upiUltimate = True}) <$> pathInfo Nothing [],
            Proto.ValidPathInfo (g1w7 "bar") <$> ((\info sigs -> info {Proto.upiSignatures = sigs}) <$> pathInfo (Just (g1w7 "bar.drv")) [yyyyFoo, g1w7 "bar"] <*> zeroSignatures ["fake-sig-1", "fake-sig-2"]),
            fixedFoo >>= \path -> Proto.ValidPathInfo path <$> ((\info ca -> info {Proto.upiContentAddress = Just ca}) <$> pathInfo Nothing [g1w7 "bar", path] <*> narParens)
          ]
      )
      (totalWriter (foldMap (Proto.putValidPathInfo defaultStoreDir (protoVersion 16)))),
    wireFixture "build-mode" (replicateM 3 Proto.getBuildMode) (Just [Proto.BuildNormal, Proto.BuildRepair, Proto.BuildCheck]) (totalWriter (foldMap Proto.putBuildMode)),
    wireFixture "optional-trusted-flag" (replicateM 3 Proto.getTrust) (Just [Nothing, Just Proto.Trusted, Just Proto.NotTrusted]) (totalWriter (foldMap Proto.putTrust)),
    wireFixture
      "vector"
      ((,) <$> replicateM 3 (Wire.getList Wire.getBytes) <*> Wire.getList (Wire.getList Wire.getBytes))
      (Just ([[], [""], ["", "foo", "bar"]], [[], [""], ["", "1", "2"]]))
      (totalWriter (\(vectors, nested) -> foldMap (Wire.putList Wire.putBytes) vectors <> Wire.putList (Wire.putList Wire.putBytes) nested)),
    wireFixture
      "set"
      ((,) <$> replicateM 3 stringSet <*> (Set.fromList <$> Wire.getList stringSet))
      (Just ([Set.empty, Set.singleton "", Set.fromList ["", "foo", "bar"]], Set.fromList [Set.empty, Set.singleton "", Set.fromList ["", "1", "2"]]))
      (totalWriter (\(sets, nested) -> foldMap putStringSet sets <> Wire.putList putStringSet (Set.toAscList nested))),
    wireFixture "optional-store-path" (replicateM 2 (Proto.getOptionalStorePath defaultStoreDir)) (Just [Nothing, Just (g1w7 "foo-bar")]) (totalWriter (foldMap (Proto.putOptionalStorePath defaultStoreDir))),
    wireFixture "optional-content-address" (replicateM 2 Proto.getOptionalContentAddress) (sequence [Just Nothing, Just <$> flatBlob]) (totalWriter (foldMap Proto.putOptionalContentAddress)),
    wireFixture "client-handshake-info_1_30" (replicateM 1 (Proto.getHandshakeInfo (protoVersion 30))) (Just [Proto.HandshakeInfo Nothing Nothing]) (handshakeWriter 30),
    wireFixture "client-handshake-info_1_33" (replicateM 2 (Proto.getHandshakeInfo (protoVersion 33))) (Just [Proto.HandshakeInfo (Just "foo") Nothing, Proto.HandshakeInfo (Just "bar") Nothing]) (handshakeWriter 33),
    wireFixture
      "client-handshake-info_1_35"
      (replicateM 2 (Proto.getHandshakeInfo (protoVersion 35)))
      (Just [Proto.HandshakeInfo (Just "foo") (Just Proto.NotTrusted), Proto.HandshakeInfo (Just "bar") (Just Proto.Trusted)])
      (handshakeWriter 35),
    wireFixture "handshake-to-client" (Proto.getServerMagic *> Proto.getVersionNumber) (Just (Proto.VersionNumber 1 10)) (totalWriter (\number -> Proto.putServerMagic <> Proto.putVersionNumber number))
  ]
  where
    derivedPaths minor count = replicateM count (Proto.getDerivedPath defaultStoreDir (protoVersion minor))
    derivedWriter minor = Just (fmap mconcat . traverse (Proto.putDerivedPath defaultStoreDir (protoVersion minor)))
    builtXY = Proto.DerivedBuilt (g1w7 "bar.drv") (fromMaybe Proto.AllOutputs (Proto.namedOutputs (Set.fromList [Proto.OutputName "x", Proto.OutputName "y"])))
    buildResults version = replicateM 3 (Proto.getBuildResult defaultStoreDir version)
    buildWriter version = totalWriter (foldMap (Proto.putBuildResult defaultStoreDir version))
    result outcome timesBuilt cpu = Proto.BuildResult outcome timesBuilt (if timesBuilt == 0 then 0 else 30) (if timesBuilt == 0 then 0 else 50) (fst <$> cpu) (snd <$> cpu)
    rejected = result (Proto.Failed (Proto.Failure Proto.OutputRejected "no idea why" False)) 0 Nothing
    notDeterministic nonDeterministic timesBuilt = result (Proto.Failed (Proto.Failure Proto.NotDeterministic "no idea why" nonDeterministic)) timesBuilt Nothing
    built outputs = result (Proto.Succeeded (Proto.Success Proto.Built outputs))
    fooBar = Map.fromList [(Proto.OutputName name, Proto.UnkeyedRealisation (g1w7 name) Set.empty) | name <- ["foo", "bar"]]
    cpuTimes = Just (500000000, 604000000)
    zeroSignatures names = Set.fromList <$> traverse (\name -> Proto.signatureFromParts name (BS.replicate 64 0)) names
    contentAddress method algo input = Hash.rawHashWithAlgo algo input >>= Proto.contentAddressFromParts method algo
    textDerive = contentAddress Proto.TextMethod "sha256" "Derive(...)"
    flatBlob = contentAddress (Proto.FixedMethod FlatIngestion) "sha1" "blob blob..."
    narParens = contentAddress (Proto.FixedMethod NarIngestion) "sha256" "(...)"
    narHash = Hash.base64ToBytes "FePFYIlMuycIXPZbWi7LGEiMmZSX9FMbaQenWBzm1Sc=" >>= Proto.narHashFromDigest
    pathInfo deriver references = (\hash -> Proto.UnkeyedValidPathInfo deriver hash (Set.fromList references) 23423 34878 False Set.empty Nothing) <$> narHash
    yyyyFoo = StorePath "g1w7hyyyy1w7hy3qg1w7hy3qgqqqqy3q" "foo"
    -- The path upstream's makeFromCA gives "foo" with that content
    -- address, referring to bar and to itself.
    fixedFoo = either (const Nothing) Just (Hash.makeStorePath defaultStoreDir "source:/nix/store/g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-bar:self" (sha256Digest "(...)") "foo")
    stringSet = Set.fromList <$> Wire.getList Wire.getBytes
    putStringSet strings = Wire.putList Wire.putBytes (Set.toAscList strings)
    handshakeWriter minor = totalWriter (foldMap (\(Proto.HandshakeInfo daemonVersion trust) -> Proto.putHandshakeInfo (protoVersion minor) (fromMaybe BS.empty daemonVersion) trust))

-- | Fixtures upstream 2.35.4 keeps that no test of its reads: realisations
-- keyed by a derivation's hash, the form 2.35.4 stopped speaking.  Read as
-- 2.35.4 reads a realisation, each is refused.
orphanedWireFixtures :: [(FilePath, BS.ByteString -> TestResult)]
orphanedWireFixtures =
  [ ("drv-output.bin", refused (Proto.getDrvOutput defaultStoreDir realisationVersion)),
    ("realisation.bin", refused (Proto.getRealisation defaultStoreDir realisationVersion)),
    ("realisation-with-deps.bin", refused (Proto.getRealisation defaultStoreDir realisationVersion))
  ]
  where
    refused reader bytes = case Wire.runDecoder reader bytes of
      Left (Wire.Malformed _) -> Pass
      other -> Fail ("expected a refusal, got " <> T.pack (show (fmap snd other)))

-- | Upstream's worker-protocol fixtures read, and written back where
-- upstream writes them, byte for byte.  Every @.bin@ in the directory
-- must have a case, so a fixture a newer tag adds fails here until it is
-- read.
testDaemonFixtures :: IO [Bool]
testDaemonFixtures = do
  putStrLn "daemon/fixtures"
  named <- lookupEnv workerProtocolDirVar
  case named of
    Nothing -> do
      putStrLn ("  SKIP  needs " ++ workerProtocolDirVar)
      pure []
    Just dir -> do
      present <- Dir.doesDirectoryExist dir
      if not present
        then (: []) <$> runTest (T.pack workerProtocolDirVar <> " names a directory") (Fail (T.pack dir))
        else do
          let cases = upstreamWireFixtures ++ orphanedWireFixtures
          listed <- filter (".bin" `isSuffixOf`) <$> Dir.listDirectory dir
          let unread = filter (`notElem` map fst cases) listed
          checked <- mapM (readFixture dir) cases
          complete <- runTest "every fixture has a case" (assertEqual "fixtures with no case" [] (sort unread))
          pure (checked ++ [complete])
  where
    readFixture dir (file, check) =
      runTestM (T.pack file) $ do
        exists <- Dir.doesFileExist (dir </> file)
        if exists then check <$> BS.readFile (dir </> file) else pure (Fail "missing")
