{-# LANGUAGE LambdaCase #-}

-- | Tests of the daemon's wire format, its protocol, and its side of a
-- connection, scripted byte by byte against a store in memory.
module Test.Daemon
  ( testDaemonWire,
    testDaemonProtocol,
    testDaemonFixtures,
    testDaemonServer,
  )
where

import Control.Monad (replicateM, void, when)
import Control.Monad.State.Strict (State, gets, modify', runState, state)
import Data.Bits (shiftR)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (traverse_)
import Data.Int (Int64)
import Data.List (isSuffixOf, sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word64, Word8)
import Nix.Config (NixConfig (..), defaultNixConfig)
import qualified Nix.Daemon.Connection as Conn
import qualified Nix.Daemon.Ops as Ops
import qualified Nix.Daemon.Protocol as Proto
import qualified Nix.Daemon.Wire as Wire
import Nix.Hash (FileIngestionMethod (..), sha256Digest)
import qualified Nix.Hash as Hash
import Nix.Push (storePathBasename)
import Nix.Store.Path (StoreDir (..), StorePath, defaultStoreDir, storePathHashLen, storePathToText)
import Nix.Store.Path.Internal (StorePath (..))
import qualified NovaCache.Base32 as Nix32
import qualified NovaCache.Base64 as B64
import qualified System.Directory as Dir
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Test.Harness (TestResult (..), andThen, assertEqual, assertRight, genContentAddress, genSignatures, runTest, runTestM)
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
      runTest "a refused read leaves the input after the bytes it read, where upstream's source is left" $
        case Wire.feedInput (Just (builtBytes (Wire.putNum (4294967296 :: Word64)) <> "rest")) (Wire.startDecoding Wire.getWord32) of
          Wire.DecodeFailed (Wire.Malformed _) rest -> assertEqual "rest" "rest" rest
          _ -> Fail "expected a refusal",
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
      Wire.DecodeFailed err _ -> Left err
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

genNarHash :: QC.Gen Proto.NarHash
genNarHash = QC.suchThatMap (BS.pack <$> QC.vectorOf 32 QC.arbitrary) Proto.narHashFromDigest

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
      runTest "an upload's method and algorithm are read as upstream reads them, a refusal quoting the colon they are read with" $
        assertEqual "text" (Right (Proto.TextMethod, "sha256")) (Proto.parseContentAddressMethod "text:sha256")
          `andThen` assertEqual "recursive" (Right (Proto.FixedMethod NarIngestion, "sha256")) (Proto.parseContentAddressMethod "fixed:r:sha256")
          `andThen` assertEqual "flat, what follows ignored" (Right (Proto.FixedMethod FlatIngestion, "md5")) (Proto.parseContentAddressMethod "fixed:md5:ignored")
          `andThen` assertEqual "git" (Left "experimental Nix feature 'git-hashing' is disabled; add '--extra-experimental-features git-hashing' to enable it") (Proto.parseContentAddressMethod "fixed:git:sha1")
          `andThen` assertEqual "no algorithm" (Left "content address hash must be in form '<algo>:<hash>', but found: text:") (Proto.parseContentAddressMethod "text")
          `andThen` assertEqual "algorithm" (Left "unknown hash algorithm 'sha3', expect 'md5', 'sha1', 'sha256', or 'sha512'") (Proto.parseContentAddressMethod "text:sha3")
          `andThen` assertEqual "prefix" (Left "content address prefix 'bogus' is unrecognized. Recogonized prefixes are 'text' or 'fixed'") (Proto.parseContentAddressMethod "bogus:sha256"),
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
    alsoWrittenByTheDaemon (wireFixture "handshake-to-client" (Proto.getServerMagic *> Proto.getVersionNumber) (Just (Proto.VersionNumber 1 10)) (totalWriter (\number -> Proto.putServerMagic <> Proto.putVersionNumber number)))
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

-- ---------------------------------------------------------------------------
-- Tests: the daemon's side of a connection
-- ---------------------------------------------------------------------------

-- Wire bytes spelled out by hand, apart from the codec, so a script holds
-- what the connection writes to upstream's layout rather than to the
-- codec's own idea of it.

-- | A number as upstream writes every one: eight bytes, little-endian.
le64 :: Word64 -> BS.ByteString
le64 n = BS.pack [fromIntegral (n `shiftR` (8 * i)) | i <- [0 .. 7]]

-- | A string: its length, its bytes, and zeros to a multiple of eight.
wstr :: BS.ByteString -> BS.ByteString
wstr bytes = le64 (fromIntegral (BS.length bytes)) <> bytes <> BS.replicate ((8 - BS.length bytes `mod` 8) `mod` 8) 0

-- | A list of strings.
wstrs :: [BS.ByteString] -> BS.ByteString
wstrs items = le64 (fromIntegral (length items)) <> BS.concat (map wstr items)

-- | A chunk of a framed stream, and the empty one that ends it.
wframe :: BS.ByteString -> BS.ByteString
wframe chunk = le64 (fromIntegral (BS.length chunk)) <> chunk

frameEnd :: BS.ByteString
frameEnd = le64 0

-- | The words that open the daemon's messages, and its handshake's.
nextWord, lastWord, errorWord, startWord, stopWord, resultWord, clientMagicWord, serverMagicWord :: Word64
nextWord = 0x6f6c6d67
lastWord = 0x616c7473
errorWord = 0x63787470
startWord = 0x53545254
stopWord = 0x53544f50
resultWord = 0x52534c54
clientMagicWord = 0x6e697863
serverMagicWord = 0x6478696f

-- | A line, the end of an operation's work, and an error as a record
-- (from 1.26) and as its message and exit status (before).
msgNext :: BS.ByteString -> BS.ByteString
msgNext line = le64 nextWord <> wstr line

msgLast :: BS.ByteString
msgLast = le64 lastWord

msgError :: BS.ByteString -> BS.ByteString
msgError message = le64 errorWord <> wstr "Error" <> le64 0 <> wstr "Error" <> wstr message <> le64 0 <> le64 0

msgErrorBefore126 :: BS.ByteString -> BS.ByteString
msgErrorBefore126 message = le64 errorWord <> wstr message <> le64 1

-- | A path as the wire names it.
pathOnWire :: StorePath -> BS.ByteString
pathOnWire = TE.encodeUtf8 . storePathToText defaultStoreDir

-- | A set of paths, in order.
wpaths :: [StorePath] -> BS.ByteString
wpaths = wstrs . map pathOnWire . Set.toAscList . Set.fromList

-- | A signature as the wire spells it.
sigBytes :: Proto.Signature -> BS.ByteString
sigBytes sig = Proto.sigKeyName sig <> ":" <> TE.encodeUtf8 (B64.encode (Proto.sigBytes sig))

-- | A path's info as a version from 1.16 writes it.
winfo :: Proto.UnkeyedValidPathInfo -> BS.ByteString
winfo info =
  wstr (maybe "" pathOnWire (Proto.upiDeriver info))
    <> wstr (TE.encodeUtf8 (Hash.bytesToHexText (Proto.narHashDigest (Proto.upiNarHash info))))
    <> wpaths (Set.toList (Proto.upiReferences info))
    <> le64 (fromIntegral (Proto.upiRegistrationTime info))
    <> le64 (Proto.upiNarSize info)
    <> le64 (if Proto.upiUltimate info then 1 else 0)
    <> wstrs (map sigBytes (Set.toAscList (Proto.upiSignatures info)))
    <> wstr ""

-- | The features the scripts' daemon offers, as it lists them.
serverFeatures :: [BS.ByteString]
serverFeatures = ["delete-dead-specific-referrers", "realisation-with-path-not-hash"]

-- | The Nix version the scripts' daemon names itself by.
serverNixVersion :: BS.ByteString
serverNixVersion = "nova-nix (test)"

-- | The substituter the scripts' daemon uses, and the one it trusts for
-- its clients.
daemonSubstituter, trustedSubstituter :: Text
daemonSubstituter = "https://cache.example.org/"
trustedSubstituter = "https://trusted.example.org/"

serverConfigAt :: Proto.Version -> Conn.ServerConfig
serverConfigAt version =
  Conn.ServerConfig
    { Conn.scStoreDir = defaultStoreDir,
      Conn.scVersion = version,
      Conn.scNixVersion = serverNixVersion,
      Conn.scVerbosity = Proto.Verbosity 3,
      Conn.scConfig = defaultNixConfig {ncSubstituters = [daemonSubstituter]},
      Conn.scTrustedSubstituters = [trustedSubstituter]
    }

serverConfig :: Conn.ServerConfig
serverConfig = serverConfigAt (Proto.Version Proto.latestVersionNumber (Set.fromList (map Proto.Feature serverFeatures)))

-- | What a client at a minor version opens a connection with: the magic
-- word, its version, its features from 1.38 (one the daemon does not
-- offer among them), and its obsolete fields.
clientHello :: Word8 -> BS.ByteString
clientHello minor =
  le64 clientMagicWord
    <> le64 (0x100 + fromIntegral minor)
    <> (if minor >= 38 then wstrs ["realisation-with-path-not-hash", "some-other-feature", "delete-dead-specific-referrers"] else "")
    <> (if minor >= 14 then le64 0 else "")
    <> (if minor >= 11 then le64 0 else "")

-- | What the daemon answers a client's opening with: its magic word and
-- version, its features from 1.38, its Nix version from 1.33, its trust
-- from 1.35, and the end of its startup work.
serverHello :: Word8 -> Proto.Trust -> BS.ByteString
serverHello minor trust =
  le64 serverMagicWord
    <> le64 0x126
    <> (if minor >= 38 then wstrs serverFeatures else "")
    <> (if minor >= 33 then wstr serverNixVersion else "")
    <> (if minor >= 35 then le64 (if trust == Proto.Trusted then 1 else 2) else "")
    <> msgLast

-- | Paths the scripts name: valid ones, one that refers to another, a
-- derivation, one a substituter offers, one nothing knows, one whose
-- query reports as it works, two uploads, and a dump's.
fooPath, barPath, drvPath, offeredPath, missingPath, noisyPath, newPath, refusedPath, dumpPath :: StorePath
fooPath = StorePath "00000000000000000000000000000001" "foo"
barPath = StorePath "00000000000000000000000000000002" "bar"
drvPath = StorePath "00000000000000000000000000000003" "foo.drv"
offeredPath = StorePath "00000000000000000000000000000004" "offered"
missingPath = StorePath "00000000000000000000000000000005" "missing"
noisyPath = StorePath "00000000000000000000000000000006" "noisy"
newPath = StorePath "00000000000000000000000000000007" "new"
refusedPath = StorePath "00000000000000000000000000000008" "refused"
dumpPath = StorePath "00000000000000000000000000000009" "dump"

-- | A path's NAR in the scripts.
narOf :: StorePath -> BS.ByteString
narOf path = "nar of " <> TE.encodeUtf8 (spName path)

-- | A path's info whose NAR is 'narOf' the path.
infoFor :: StorePath -> [StorePath] -> Maybe StorePath -> Maybe Proto.UnkeyedValidPathInfo
infoFor path references deriver = withNar <$> Proto.narHashFromDigest (sha256Digest (narOf path))
  where
    withNar narHash =
      Proto.UnkeyedValidPathInfo
        { Proto.upiDeriver = deriver,
          Proto.upiNarHash = narHash,
          Proto.upiReferences = Set.fromList references,
          Proto.upiRegistrationTime = 1700000000,
          Proto.upiNarSize = fromIntegral (BS.length (narOf path)),
          Proto.upiUltimate = False,
          Proto.upiSignatures = Set.empty,
          Proto.upiContentAddress = Nothing
        }

-- | The infos the scripts' store and uploads hold.
data Fixtures = Fixtures
  { fooInfo :: !Proto.UnkeyedValidPathInfo,
    barInfo :: !Proto.UnkeyedValidPathInfo,
    drvInfo :: !Proto.UnkeyedValidPathInfo,
    noisyInfo :: !Proto.UnkeyedValidPathInfo,
    newInfo :: !Proto.UnkeyedValidPathInfo,
    refusedInfo :: !Proto.UnkeyedValidPathInfo
  }

fixtures :: Maybe Fixtures
fixtures =
  Fixtures
    <$> infoFor fooPath [] (Just drvPath)
    <*> infoFor barPath [fooPath] Nothing
    <*> infoFor drvPath [] Nothing
    <*> infoFor noisyPath [] Nothing
    <*> infoFor newPath [fooPath] Nothing
    <*> infoFor refusedPath [] Nothing

-- | A script's test, given the fixtures.
scriptTest :: Text -> (Fixtures -> TestResult) -> IO Bool
scriptTest name check = runTest name (maybe (Fail "the fixtures do not build") check fixtures)

-- | The scripts' store: the valid paths and their infos, what it was
-- asked in order, latest first, the NARs it imported, and the settings
-- its latest call ran under.
data Fake = Fake
  { fakeValid :: !(Map.Map StorePath Proto.UnkeyedValidPathInfo),
    fakeCalls :: ![Text],
    fakeImports :: ![(StorePath, BS.ByteString)],
    fakeSettings :: !(Maybe Ops.ConnectionSettings)
  }

-- | The store every script starts with.
startingStore :: Fixtures -> Fake
startingStore fx =
  Fake (Map.fromList [(fooPath, fooInfo fx), (barPath, barInfo fx), (drvPath, drvInfo fx), (noisyPath, noisyInfo fx)]) [] [] Nothing

-- | What a script runs over: the client's input still to come, what the
-- daemon has written, latest first, and the store.
data World = World
  { worldInput :: ![BS.ByteString],
    worldOutput :: ![BS.ByteString],
    worldFake :: !Fake
  }

type Scripted = State World

scriptTransport :: Conn.Transport Scripted
scriptTransport =
  Conn.Transport
    { Conn.transportReceive =
        state $ \world -> case worldInput world of
          [] -> (Nothing, world)
          chunk : rest -> (Just chunk, world {worldInput = rest}),
      Conn.transportSend = \bytes -> modify' (\world -> world {worldOutput = builtBytes bytes : worldOutput world})
    }

onFake :: (Fake -> Fake) -> Scripted ()
onFake change = modify' (\world -> world {worldFake = change (worldFake world)})

validPaths :: Scripted (Map.Map StorePath Proto.UnkeyedValidPathInfo)
validPaths = gets (fakeValid . worldFake)

-- | Record a call, and the settings it ran under.
asked :: Ops.Work Scripted -> Text -> Scripted ()
asked w call = onFake (\fake -> fake {fakeCalls = call : fakeCalls fake, fakeSettings = Just (Ops.workSettings w)})

-- | An intake that keeps what it is fed and acts on all of it at its end,
-- recording an upload that broke off.
collecting :: BS.ByteString -> (BS.ByteString -> Scripted (Either Proto.DaemonError a)) -> Ops.Intake Scripted a
collecting held atEnd = Ops.Taking (\chunk -> pure (collecting (held <> chunk) atEnd)) (atEnd held) (onFake (\fake -> fake {fakeCalls = "broken off" : fakeCalls fake}))

-- | What a build reports: its activity's start (a build's, type 105), a
-- line its builder wrote (a result of type 101), and its stop.
buildEvents :: [Ops.LogEvent]
buildEvents =
  [ Ops.ActivityStarted (Proto.Activity (Proto.ActivityId 7) (Proto.Verbosity 3) (Proto.ActivityType 105) "building noisy" [Proto.FieldString "noisy.drv", Proto.FieldInt 1] (Proto.ActivityId 0)),
    Ops.ActivityResult (Proto.ActivityId 7) (Proto.ResultType 101) [Proto.FieldString "a build line"],
    Ops.ActivityStopped (Proto.ActivityId 7)
  ]

-- | Those reports as upstream's daemon sends them from 1.20.
buildBytes :: BS.ByteString
buildBytes =
  (le64 startWord <> le64 7 <> le64 3 <> le64 105 <> wstr "building noisy" <> le64 2 <> le64 1 <> wstr "noisy.drv" <> le64 0 <> le64 1 <> le64 0)
    <> (le64 resultWord <> le64 7 <> le64 101 <> le64 1 <> le64 1 <> wstr "a build line")
    <> (le64 stopWord <> le64 7)

-- | What querying 'noisyPath' reports as it works: a line at the info
-- level, one at a level above it, and a build.
noisyEvents :: [Ops.LogEvent]
noisyEvents = Ops.LogLine (Proto.Verbosity 3) "checking noisy" : Ops.LogLine (Proto.Verbosity 5) "too quiet to hear" : buildEvents

-- | A build's result in the scripts: built once, its output at 'fooPath'.
builtResult :: Proto.BuildResult
builtResult =
  Proto.BuildResult
    (Proto.Succeeded (Proto.Success Proto.Built (Map.singleton (Proto.OutputName "out") (Proto.UnkeyedRealisation fooPath Set.empty))))
    1
    10
    20
    Nothing
    Nothing

-- | The roots the scripts' store holds, as a client is shown them.
rootsShown :: Ops.RootCensoring -> Map.Map StorePath (Set.Set BS.ByteString)
rootsShown censoring = Map.singleton fooPath (Set.singleton link)
  where
    link = case censoring of
      Ops.ShowRoots -> "/home/user/result"
      Ops.CensorRoots -> "{censored}"

-- | Upstream's refusal of a path that is not valid.
notValid :: StorePath -> Proto.DaemonError
notValid path = Ops.daemonError ("path '" <> storePathToText defaultStoreDir path <> "' is not valid")

fakeStore :: Ops.DaemonStore Scripted
fakeStore =
  Ops.DaemonStore
    { Ops.dsIsValidPath = \w path -> do
        asked w ("IsValidPath " <> spName path)
        when (path == noisyPath) (traverse_ (Ops.workReport w) noisyEvents)
        Right . Map.member path <$> validPaths,
      Ops.dsQueryValidPaths = \w substitute paths -> do
        asked w ("QueryValidPaths " <> T.pack (show substitute))
        Right . (\known -> Set.filter (`Map.member` known) paths) <$> validPaths,
      Ops.dsQuerySubstitutablePaths = \w paths -> Right (Set.filter (== offeredPath) paths) <$ asked w "QuerySubstitutablePaths",
      Ops.dsQueryReferrers = \w path -> do
        asked w "QueryReferrers"
        Right . Map.keysSet . Map.filter (Set.member path . Proto.upiReferences) <$> validPaths,
      Ops.dsQueryValidDerivers = \w path -> do
        asked w "QueryValidDerivers"
        Right . maybe Set.empty (maybe Set.empty Set.singleton . Proto.upiDeriver) . Map.lookup path <$> validPaths,
      Ops.dsQueryDerivationOutputMap = \w _ ->
        Right (Map.fromList [(Proto.OutputName "out", Just fooPath), (Proto.OutputName "dev", Nothing)]) <$ asked w "QueryDerivationOutputMap",
      Ops.dsQueryAllValidPaths = \w -> asked w "QueryAllValidPaths" >> (Right . Map.keysSet <$> validPaths),
      Ops.dsQueryPathInfo = \w path -> asked w "QueryPathInfo" >> (Right . Map.lookup path <$> validPaths),
      Ops.dsQueryMissing = \w _ -> Right (Ops.Missing (Set.singleton drvPath) (Set.singleton offeredPath) Set.empty 10 20) <$ asked w "QueryMissing",
      Ops.dsAddToStore = \w upload -> do
        asked w ("AddToStore " <> TE.decodeLatin1 (Ops.duName upload) <> " " <> T.pack (show (Ops.duMethod upload)) <> " " <> Ops.duAlgo upload)
        pure . collecting BS.empty $ \dump -> do
          onFake (\fake -> fake {fakeImports = (dumpPath, dump) : fakeImports fake})
          pure $ case Proto.narHashFromDigest (sha256Digest dump) of
            Nothing -> Left (Ops.daemonError "no hash")
            Just narHash -> Right (Proto.ValidPathInfo dumpPath (Proto.UnkeyedValidPathInfo Nothing narHash (Ops.duReferences upload) 1700000000 (fromIntegral (BS.length dump)) True Set.empty Nothing)),
      Ops.dsAddToStoreNar = \w (Proto.ValidPathInfo path info) repair checkSigs -> do
        asked w ("AddToStoreNar " <> spName path <> " " <> T.pack (show repair) <> " " <> T.pack (show checkSigs) <> " ultimate " <> T.pack (show (Proto.upiUltimate info)))
        importing path info repair <$> validPaths,
      Ops.dsBuildPaths = \w _ mode -> do
        asked w ("BuildPaths " <> T.pack (show mode))
        Right () <$ traverse_ (Ops.workReport w) buildEvents,
      Ops.dsBuildPathsWithResults = \w targets mode -> Right [Proto.KeyedBuildResult target builtResult | target <- targets] <$ asked w ("BuildPathsWithResults " <> T.pack (show mode)),
      Ops.dsEnsurePath = \w path -> do
        asked w ("EnsurePath " <> spName path)
        (\known -> if Map.member path known then Right () else Left (notValid path)) <$> validPaths,
      Ops.dsAddTempRoot = \w path -> Right () <$ asked w ("AddTempRoot " <> spName path),
      Ops.dsAddIndirectRoot = \w link -> Right () <$ asked w ("AddIndirectRoot " <> TE.decodeLatin1 link),
      Ops.dsFindRoots = \w censoring -> Right (rootsShown censoring) <$ asked w ("FindRoots " <> T.pack (show censoring)),
      Ops.dsCollectGarbage = \w request ->
        Right (Ops.GCResult (Set.singleton (pathOnWire (StorePath "0000000000000000000000000000000a" "dead"))) 4096) <$ asked w ("CollectGarbage " <> T.pack (show request)),
      Ops.dsOptimiseStore = \w -> Right () <$ asked w "OptimiseStore",
      Ops.dsVerifyStore = \w contents repair -> Right False <$ asked w ("VerifyStore " <> T.pack (show contents) <> " " <> T.pack (show repair)),
      Ops.dsAddSignatures = \w path sigs -> Right () <$ asked w ("AddSignatures " <> spName path <> " " <> T.pack (show (Set.size sigs))),
      Ops.dsNarFromPath = \path sink -> do
        onFake (\fake -> fake {fakeCalls = ("NarFromPath " <> spName path) : fakeCalls fake})
        known <- validPaths
        if Map.member path known
          then Right () <$ traverse_ sink [BS.take 4 (narOf path), BS.drop 4 (narOf path)]
          else Left (notValid path) <$ sink "partial"
    }

-- | How the scripts' store imports a NAR: 'refusedPath' is refused before
-- a byte is read, as upstream refuses an unsigned path; a valid path is
-- left as it is without its NAR being read, unless it is repaired; any
-- other is held to its info's hash and size.
importing :: StorePath -> Proto.UnkeyedValidPathInfo -> Ops.Repair -> Map.Map StorePath Proto.UnkeyedValidPathInfo -> Ops.Intake Scripted ()
importing path info repair known
  | path == refusedPath = Ops.Taken (Left (Ops.daemonError ("cannot add path '" <> storePathToText defaultStoreDir path <> "' because it lacks a signature by a trusted key")))
  | Map.member path known && repair == Ops.NoRepair = Ops.Taken (Right ())
  | otherwise = collecting BS.empty imported
  where
    imported nar
      | Proto.narHashFromDigest (sha256Digest nar) == Just (Proto.upiNarHash info) && fromIntegral (BS.length nar) == Proto.upiNarSize info =
          Right () <$ onFake (\fake -> fake {fakeValid = Map.insert path info (fakeValid fake), fakeImports = (path, nar) : fakeImports fake})
      | otherwise = pure (Left (Ops.daemonError ("hash mismatch importing path '" <> storePathToText defaultStoreDir path <> "'")))

-- | Run a client's bytes against the scripts' store: how the connection
-- ended, every byte the daemon wrote, and the store after.
runScript :: Conn.ServerConfig -> Proto.Trust -> Fake -> [BS.ByteString] -> (Conn.ConnectionEnd, BS.ByteString, Fake)
runScript config trust fake input = (ended, BS.concat (reverse (worldOutput world)), worldFake world)
  where
    (ended, world) = runState (Conn.serveConnection config trust fakeStore scriptTransport) (World (filter (not . BS.null) input) [] fake)

-- | A session at a minor version: what the daemon writes after its
-- handshake, how the connection ends, and a check of the store after.
assertSession :: Proto.Trust -> Word8 -> Fake -> [BS.ByteString] -> BS.ByteString -> Conn.ConnectionEnd -> (Fake -> TestResult) -> TestResult
assertSession trust minor fake ops expected end check =
  let (ended, written, after) = runScript serverConfig trust fake (clientHello minor : ops)
   in assertEqual "written" (serverHello minor trust <> expected) written
        `andThen` assertEqual "end" end ended
        `andThen` check after

-- | The store was asked exactly these, in order.
askedOnly :: [Text] -> Fake -> TestResult
askedOnly calls fake = assertEqual "calls" calls (reverse (fakeCalls fake))

-- | A request naming one path.
reqPath :: Word64 -> StorePath -> BS.ByteString
reqPath code path = le64 code <> wstr (pathOnWire path)

-- | Settings as upstream's client sends them, at a verbosity, with the
-- other settings given.
setOptionsAt :: Word64 -> [(BS.ByteString, BS.ByteString)] -> BS.ByteString
setOptionsAt verbosity overrides =
  le64 19
    <> foldMap le64 [1, 0, 1, verbosity, 1, 0, 1, 7, 0, 0, 4, 1]
    <> le64 (fromIntegral (length overrides))
    <> foldMap (\(name, value) -> wstr name <> wstr value) overrides

-- | A path's info as an upload sends it.
uploadInfo :: StorePath -> Proto.UnkeyedValidPathInfo -> BS.ByteString
uploadInfo path info = wstr (pathOnWire path) <> winfo info

-- | A path derived from 'drvPath', its output @out@.
drvOut :: BS.ByteString
drvOut = pathOnWire drvPath <> "!out"

-- | The scripted connections: a client's bytes against a fake store, and
-- every byte the daemon writes back, spelled out by hand.
testDaemonServer :: IO [Bool]
testDaemonServer = do
  putStrLn "daemon/server"
  sequence
    [ scriptTest "a trusted client's queries are each answered after the end of their work, as upstream's daemon answers them" $ \fx ->
        assertSession
          Proto.Trusted
          38
          (startingStore fx)
          [ setOptionsAt 3 [],
            reqPath 1 fooPath,
            reqPath 1 missingPath,
            reqPath 26 fooPath,
            reqPath 26 missingPath,
            le64 31 <> wpaths [fooPath, missingPath] <> le64 1,
            le64 32 <> wpaths [offeredPath, missingPath],
            reqPath 6 fooPath,
            reqPath 33 fooPath,
            reqPath 41 drvPath,
            le64 23,
            le64 40 <> wstrs [drvOut],
            reqPath 10 fooPath,
            reqPath 11 fooPath,
            le64 12 <> wstr "home/user/./result",
            le64 12 <> wstr "/home/user/../user//result2/",
            le64 14,
            le64 20 <> le64 2 <> le64 1 <> le64 0 <> le64 100 <> le64 0 <> le64 0 <> le64 0,
            le64 34,
            le64 35 <> le64 1 <> le64 0,
            le64 37 <> wstr (pathOnWire fooPath) <> wstrs ["key:" <> TE.encodeUtf8 (B64.encode (BS.replicate 64 0))],
            reqPath 38 fooPath
          ]
          ( msgLast
              <> (msgLast <> le64 1)
              <> (msgLast <> le64 0)
              <> (msgLast <> le64 1 <> winfo (fooInfo fx))
              <> (msgLast <> le64 0)
              <> (msgLast <> wpaths [fooPath])
              <> (msgLast <> wpaths [offeredPath])
              <> (msgLast <> wpaths [barPath])
              <> (msgLast <> wpaths [drvPath])
              <> (msgLast <> le64 2 <> wstr "dev" <> wstr "" <> wstr "out" <> wstr (pathOnWire fooPath))
              <> (msgLast <> wpaths [fooPath, barPath, drvPath, noisyPath])
              <> (msgLast <> wpaths [drvPath] <> wpaths [offeredPath] <> wpaths [] <> le64 10 <> le64 20)
              <> (msgLast <> le64 1)
              <> (msgLast <> le64 1)
              <> (msgLast <> le64 1)
              <> (msgLast <> le64 1)
              <> (msgLast <> le64 1 <> wstr "/home/user/result" <> wstr (pathOnWire fooPath))
              <> (msgLast <> wstrs ["/nix/store/0000000000000000000000000000000a-dead"] <> le64 4096 <> le64 0)
              <> (msgLast <> le64 1)
              <> (msgLast <> le64 0)
              <> (msgLast <> le64 1)
              <> (msgLast <> narOf fooPath)
          )
          Conn.ClientClosed
          ( askedOnly
              [ "IsValidPath foo",
                "IsValidPath missing",
                "QueryPathInfo",
                "QueryPathInfo",
                "QueryValidPaths Substitute",
                "QuerySubstitutablePaths",
                "QueryReferrers",
                "QueryValidDerivers",
                "QueryDerivationOutputMap",
                "QueryAllValidPaths",
                "QueryMissing",
                "EnsurePath foo",
                "AddTempRoot foo",
                "AddIndirectRoot /home/user/result",
                "AddIndirectRoot /home/user/result2",
                "FindRoots ShowRoots",
                "CollectGarbage " <> T.pack (show (Ops.GCRequest Proto.DeleteDead Proto.WholeStore 100)),
                "OptimiseStore",
                "VerifyStore CheckContents NoRepair",
                "AddSignatures foo 1",
                "NarFromPath foo"
              ]
          ),
      scriptTest "a build's activity and its log line reach the client as it runs, and its results are keyed by the paths asked for" $ \fx ->
        assertSession
          Proto.Trusted
          38
          (startingStore fx)
          [le64 9 <> wstrs [drvOut] <> le64 0, le64 46 <> wstrs [drvOut] <> le64 2]
          ( buildBytes
              <> (msgLast <> le64 1)
              <> msgLast
              <> le64 1
              <> wstr drvOut
              <> (le64 0 <> wstr "" <> le64 1 <> le64 0 <> le64 10 <> le64 20 <> le64 0 <> le64 0)
              <> (le64 1 <> wstr "out" <> wstr (pathOnWire fooPath) <> le64 0)
          )
          Conn.ClientClosed
          (askedOnly ["BuildPaths BuildNormal", "BuildPathsWithResults BuildCheck"]),
      scriptTest "a message and an activity the store reports mid-operation reach the client as upstream's daemon forwards them" $ \fx ->
        assertSession Proto.Trusted 38 (startingStore fx) [reqPath 1 noisyPath] (msgNext "checking noisy\n" <> buildBytes <> msgLast <> le64 1) Conn.ClientClosed (const Pass),
      scriptTest "before 1.20 an activity's start reaches the client as a line, and the rest of what it reports does not" $ \fx ->
        assertSession
          Proto.Trusted
          19
          (startingStore fx)
          [reqPath 1 noisyPath]
          (msgNext "checking noisy\n" <> msgNext "building noisy...\n" <> msgLast <> le64 1)
          Conn.ClientClosed
          (const Pass),
      scriptTest "a client's verbosity decides which of the store's lines reach it" $ \fx ->
        assertSession
          Proto.Trusted
          38
          (startingStore fx)
          [setOptionsAt 5 [], reqPath 1 noisyPath, setOptionsAt 0 [], reqPath 1 noisyPath]
          ( msgLast
              <> msgNext "checking noisy\n"
              <> msgNext "too quiet to hear\n"
              <> buildBytes
              <> (msgLast <> le64 1)
              <> msgLast
              <> buildBytes
              <> (msgLast <> le64 1)
          )
          Conn.ClientClosed
          (const Pass),
      scriptTest "an operation that fails in its work is sent upstream's error record, and the connection goes on" $ \fx ->
        assertSession
          Proto.Trusted
          38
          (startingStore fx)
          [reqPath 10 missingPath, reqPath 1 fooPath]
          (msgError "path '/nix/store/00000000000000000000000000000005-missing' is not valid" <> msgLast <> le64 1)
          Conn.ClientClosed
          (const Pass),
      scriptTest "before 1.26 a failure is sent as its message and exit status" $ \fx ->
        assertSession
          Proto.Trusted
          25
          (startingStore fx)
          [reqPath 10 missingPath, reqPath 1 fooPath]
          (msgErrorBefore126 "path '/nix/store/00000000000000000000000000000005-missing' is not valid" <> msgLast <> le64 1)
          Conn.ClientClosed
          (const Pass),
      scriptTest "a failure before an operation's work is sent twice and closes the connection, as upstream's daemon closes it" $ \fx ->
        let closed message ops =
              assertSession Proto.Trusted 38 (startingStore fx) (ops ++ [reqPath 1 fooPath]) (msgError message <> msgError message) (Conn.ClosedOnError (Ops.daemonError (TE.decodeUtf8 message))) (askedOnly [])
         in closed "invalid operation 99" [le64 99]
              `andThen` closed "path \"/tmp/x\" is not in the Nix store" [le64 1 <> wstr "/tmp/x"]
              `andThen` closed "Invalid build mode" [le64 9 <> wstrs [drvOut] <> le64 3],
      scriptTest "an operation's code too large to read is sent once and closes the connection, and input ending inside a request is sent twice" $ \fx ->
        assertSession
          Proto.Trusted
          38
          (startingStore fx)
          [le64 4294967296]
          (msgError "serialised integer 4294967296 is too large for type 'j'")
          (Conn.ClosedOnError (Ops.daemonError "serialised integer 4294967296 is too large for type 'j'"))
          (askedOnly [])
          `andThen` assertSession
            Proto.Trusted
            38
            (startingStore fx)
            [le64 1 <> le64 64 <> "/nix/store"]
            (msgError "unexpected end-of-file" <> msgError "unexpected end-of-file")
            (Conn.ClosedOnError (Ops.daemonError "unexpected end-of-file"))
            (askedOnly []),
      runTest "a client whose input ends between operations closes the connection, with nothing more sent" $
        let (ended, written, _) = runScript serverConfig Proto.Trusted emptyFake [clientHello 38, BS.take 3 (le64 1)]
         in assertEqual "end" Conn.ClientClosed ended `andThen` assertEqual "written" (serverHello 38 Proto.Trusted) written,
      scriptTest "an untrusted client is refused what upstream's daemon refuses it, and a trusted one is not" $ \fx ->
        let store = startingStore fx
            narUpload repair dontCheck = le64 39 <> uploadInfo newPath ((newInfo fx) {Proto.upiUltimate = True}) <> le64 repair <> le64 dontCheck <> wframe (narOf newPath) <> frameEnd
            refusedBefore message request =
              assertSession Proto.NotTrusted 38 store [request, reqPath 1 fooPath] (msgError message <> msgError message) (Conn.ClosedOnError (Ops.daemonError (TE.decodeUtf8 message))) (askedOnly [])
            refusedInWork message request =
              assertSession Proto.NotTrusted 38 store [request, reqPath 1 fooPath] (msgError message <> msgLast <> le64 1) Conn.ClientClosed (askedOnly ["IsValidPath foo"])
            served trust request reply calls = assertSession trust 38 store [request] reply Conn.ClientClosed (askedOnly calls)
         in refusedBefore "you are not privileged to repair paths" (le64 7 <> wstr "dump" <> wstr "text:sha256" <> wpaths [] <> le64 1)
              `andThen` refusedBefore "you are not privileged to repair store paths" (narUpload 1 0)
              `andThen` refusedBefore "you are not privileged to repair store paths" (le64 44 <> le64 1 <> le64 0)
              `andThen` refusedBefore "repairing is not allowed because you are not in 'trusted-users'" (le64 9 <> wstrs [drvOut] <> le64 1)
              `andThen` refusedBefore "repairing is not allowed because you are not in 'trusted-users'" (le64 46 <> wstrs [drvOut] <> le64 1)
              `andThen` refusedInWork "you are not privileged to repair paths" (le64 35 <> le64 0 <> le64 1)
              `andThen` served Proto.Trusted (le64 9 <> wstrs [drvOut] <> le64 1) (buildBytes <> msgLast <> le64 1) ["BuildPaths BuildRepair"]
              `andThen` served Proto.Trusted (le64 35 <> le64 0 <> le64 1) (msgLast <> le64 0) ["VerifyStore SkipContents Repair"]
              `andThen` served Proto.Trusted (narUpload 1 1) msgLast ["AddToStoreNar new Repair NoCheckSigs ultimate True"]
              `andThen` served Proto.NotTrusted (narUpload 0 1) msgLast ["AddToStoreNar new NoRepair CheckSigs ultimate False"]
              `andThen` served
                Proto.NotTrusted
                (le64 44 <> le64 0 <> le64 1 <> wframe (le64 1 <> uploadInfo newPath ((newInfo fx) {Proto.upiUltimate = True}) <> narOf newPath) <> frameEnd)
                msgLast
                ["AddToStoreNar new NoRepair CheckSigs ultimate False"]
              `andThen` served Proto.NotTrusted (le64 14) (msgLast <> le64 1 <> wstr "{censored}" <> wstr (pathOnWire fooPath)) ["FindRoots CensorRoots"],
      scriptTest "no client may have a collection ignore liveness, and the refusal leaves the connection open" $ \fx ->
        let ignoring = le64 20 <> le64 2 <> le64 1 <> le64 1 <> le64 0 <> le64 0 <> le64 0 <> le64 0
            refused trust =
              assertSession trust 38 (startingStore fx) [ignoring, reqPath 1 fooPath] (msgError "you are not allowed to ignore liveness" <> msgLast <> le64 1) Conn.ClientClosed (askedOnly ["IsValidPath foo"])
         in refused Proto.Trusted `andThen` refused Proto.NotTrusted,
      scriptTest "a collection's paths are a set with a referrers flag under the delete-specific feature and a bare set without it, where a collection of the dead limited to some is refused" $ \fx ->
        let tail3 = le64 0 <> le64 0 <> le64 0
            bare action paths = le64 20 <> le64 action <> wpaths paths <> le64 0 <> le64 0 <> tail3
            reply = msgLast <> wstrs ["/nix/store/0000000000000000000000000000000a-dead"] <> le64 4096 <> le64 0
            asks request = "CollectGarbage " <> T.pack (show request)
            message = "Garbage collecting specific paths requested but it is not supported by the negotiated protocol"
         in assertSession
              Proto.Trusted
              38
              (startingStore fx)
              [le64 20 <> le64 2 <> le64 0 <> wpaths [fooPath] <> le64 1 <> le64 0 <> le64 7 <> tail3]
              reply
              Conn.ClientClosed
              (askedOnly [asks (Ops.GCRequest Proto.DeleteDead (Proto.SpecificPaths (Set.singleton fooPath) True) 7)])
              `andThen` assertSession
                Proto.Trusted
                37
                (startingStore fx)
                [bare 3 [fooPath], bare 0 []]
                (reply <> reply)
                Conn.ClientClosed
                (askedOnly [asks (Ops.GCRequest Proto.DeleteSpecific (Proto.SpecificPaths (Set.singleton fooPath) False) 0), asks (Ops.GCRequest Proto.ReturnLive Proto.WholeStore 0)])
              `andThen` assertSession Proto.Trusted 37 (startingStore fx) [bare 2 [fooPath]] (msgError message <> msgError message) (Conn.ClosedOnError (Ops.daemonError (TE.decodeUtf8 message))) (askedOnly []),
      scriptTest "a NAR upload is fed to the store up to its declared size, and the rest of its frames are read past" $ \fx ->
        let nar = narOf newPath
            upload = le64 39 <> uploadInfo newPath (newInfo fx) <> le64 0 <> le64 0 <> foldMap wframe [BS.take 3 nar, BS.drop 3 nar <> "trailing", "more"] <> frameEnd
         in assertSession
              Proto.Trusted
              38
              (startingStore fx)
              [upload, reqPath 1 newPath]
              (msgLast <> msgLast <> le64 1)
              Conn.ClientClosed
              (assertEqual "imported" [(newPath, nar)] . fakeImports),
      scriptTest "a store that fails an upload, or needs none of it, has the rest read past before the reply, and the connection goes on" $ \fx ->
        let upload path info nar = le64 39 <> uploadInfo path info <> le64 0 <> le64 0 <> wframe nar <> wframe "more" <> frameEnd
            unsigned = "cannot add path '/nix/store/00000000000000000000000000000008-refused' because it lacks a signature by a trusted key"
            mismatch = "hash mismatch importing path '/nix/store/00000000000000000000000000000007-new'"
            session request reply = assertSession Proto.Trusted 38 (startingStore fx) [request, reqPath 1 fooPath] (reply <> msgLast <> le64 1) Conn.ClientClosed
         in session (upload refusedPath (refusedInfo fx) (narOf refusedPath)) (msgError unsigned) (assertEqual "imported" [] . fakeImports)
              `andThen` session (upload newPath (newInfo fx) "not the nar of new") (msgError mismatch) (assertEqual "imported" [] . fakeImports)
              `andThen` session (upload fooPath (fooInfo fx) (narOf fooPath)) msgLast (assertEqual "imported" [] . fakeImports),
      scriptTest "a frame too long to read fails its upload, and reading past the upload goes on from the bytes after the length, as upstream's does" $ \fx ->
        assertSession
          Proto.Trusted
          38
          (startingStore fx)
          [le64 39 <> uploadInfo newPath (newInfo fx) <> le64 0 <> le64 0 <> le64 4294967296 <> wframe "x" <> frameEnd, reqPath 1 fooPath]
          (msgError "serialised integer 4294967296 is too large for type 'j'" <> msgLast <> le64 1)
          Conn.ClientClosed
          (askedOnly ["AddToStoreNar new NoRepair CheckSigs ultimate False", "broken off", "IsValidPath foo"]),
      scriptTest "input that ends inside an upload fails it once, and the connection then closes as the client's input has ended" $ \fx ->
        assertSession
          Proto.Trusted
          38
          (startingStore fx)
          [le64 39 <> uploadInfo newPath (newInfo fx) <> le64 0 <> le64 0 <> wframe "abc"]
          (msgError "unexpected end-of-file")
          Conn.ClientClosed
          (askedOnly ["AddToStoreNar new NoRepair CheckSigs ultimate False", "broken off"]),
      scriptTest "AddMultipleToStore imports each path in turn from one framed stream, reading past the NAR of one already valid, and none is ultimate" $ \fx ->
        let payload =
              le64 2
                <> uploadInfo fooPath ((fooInfo fx) {Proto.upiUltimate = True})
                <> narOf fooPath
                <> uploadInfo newPath ((newInfo fx) {Proto.upiUltimate = True})
                <> narOf newPath
            request = le64 44 <> le64 0 <> le64 0 <> foldMap wframe [BS.take 5 payload, BS.take 100 (BS.drop 5 payload), BS.drop 105 payload] <> frameEnd
         in assertSession
              Proto.Trusted
              38
              (startingStore fx)
              [request, reqPath 1 newPath]
              (msgLast <> msgLast <> le64 1)
              Conn.ClientClosed
              ( \after ->
                  askedOnly ["AddToStoreNar foo NoRepair CheckSigs ultimate False", "AddToStoreNar new NoRepair CheckSigs ultimate False", "IsValidPath new"] after
                    `andThen` assertEqual "imported" [(newPath, narOf newPath)] (fakeImports after)
              ),
      scriptTest "AddMultipleToStore stops at the first path the store refuses, and an upload that ends before its count does fails it" $ \fx ->
        let failing = wframe (le64 2 <> uploadInfo refusedPath (refusedInfo fx) <> narOf refusedPath <> uploadInfo newPath (newInfo fx) <> narOf newPath)
            short = wframe (le64 2 <> uploadInfo newPath (newInfo fx) <> narOf newPath)
            session frames reply calls = assertSession Proto.Trusted 38 (startingStore fx) [le64 44 <> le64 0 <> le64 0 <> frames <> frameEnd, reqPath 1 fooPath] (reply <> msgLast <> le64 1) Conn.ClientClosed (askedOnly (calls ++ ["IsValidPath foo"]))
         in session failing (msgError "cannot add path '/nix/store/00000000000000000000000000000008-refused' because it lacks a signature by a trusted key") ["AddToStoreNar refused NoRepair CheckSigs ultimate False"]
              `andThen` session short (msgError "reached end of FramedSource") ["AddToStoreNar new NoRepair CheckSigs ultimate False"],
      scriptTest "AddToStore makes a path from its framed dump and replies with the path's info" $ \fx ->
        case Proto.narHashFromDigest (sha256Digest "hello") of
          Nothing -> Fail "no hash"
          Just narHash ->
            assertSession
              Proto.Trusted
              38
              (startingStore fx)
              [le64 7 <> wstr "dump" <> wstr "fixed:r:sha256" <> wpaths [fooPath] <> le64 0 <> wframe "hel" <> wframe "lo" <> frameEnd]
              (msgLast <> wstr (pathOnWire dumpPath) <> winfo (Proto.UnkeyedValidPathInfo Nothing narHash (Set.singleton fooPath) 1700000000 5 True Set.empty Nothing))
              Conn.ClientClosed
              (askedOnly ["AddToStore dump FixedMethod NarIngestion sha256"]),
      scriptTest "an upload's method is read before its dump, so a method refused leaves the dump to be read as the next operation, as upstream leaves it" $ \fx ->
        assertSession
          Proto.Trusted
          38
          (startingStore fx)
          [le64 7 <> wstr "dump" <> wstr "bogus:sha256" <> wpaths [] <> le64 0 <> wframe "hello" <> frameEnd]
          ( msgError "content address prefix 'bogus' is unrecognized. Recogonized prefixes are 'text' or 'fixed'"
              <> msgError "invalid operation 5"
              <> msgError "invalid operation 5"
          )
          (Conn.ClosedOnError (Ops.daemonError "invalid operation 5"))
          (askedOnly []),
      scriptTest "an upload in a form older than a framed one is refused as its request begins" $ \fx ->
        let refused minor message ops =
              assertSession Proto.Trusted minor (startingStore fx) ops (msgErrorBefore126 message <> msgErrorBefore126 message) (Conn.ClosedOnError (Ops.daemonError (TE.decodeUtf8 message))) (askedOnly [])
         in refused 24 "AddToStore before protocol 1.25 is not supported by this daemon" [le64 7, wstr "dump"]
              `andThen` refused 22 "AddToStoreNar before protocol 1.23 is not supported by this daemon" [le64 39]
              `andThen` assertSession Proto.Trusted 26 (startingStore fx) [le64 31 <> wpaths [fooPath]] (msgLast <> wpaths [fooPath]) Conn.ClientClosed (askedOnly ["QueryValidPaths NoSubstitute"]),
      scriptTest "an untrusted client's settings apply as far as upstream's daemon lets them, with its warnings in the settings' order" $ \fx ->
        let overrides =
              [ ("substituters", "https://trusted.example.org https://evil.example.org " <> TE.encodeUtf8 daemonSubstituter),
                ("plugin-files", "/x.so"),
                ("keep-going", "true"),
                ("build-poll-interval", "3"),
                ("ssh-auth-sock", "/tmp/agent"),
                ("experimental-features", "nix-command")
              ]
            warnings =
              msgNext "warning: ignoring the client-specified setting 'keep-going', because it is a restricted setting and you are not a trusted user\n"
                <> msgNext "warning: Ignoring the client-specified plugin-files.\nThe client specifying plugins to the daemon never made sense, and was removed in Nix >=2.14.\n"
                <> msgNext "warning: ignoring untrusted substituter 'https://evil.example.org', you are not a trusted user.\nRun `man nix.conf` for more information on the `substituters` configuration option.\n"
         in assertSession
              Proto.NotTrusted
              38
              (startingStore fx)
              [setOptionsAt 3 overrides, reqPath 1 fooPath]
              (warnings <> msgLast <> msgLast <> le64 1)
              Conn.ClientClosed
              ( \after -> case fakeSettings after of
                  Nothing -> Fail "the store saw no settings"
                  Just settings ->
                    assertEqual "substituters" [trustedSubstituter, daemonSubstituter] (ncSubstituters (Ops.connConfig settings))
                      `andThen` assertEqual "keep-failed" True (ncKeepFailed (Ops.connConfig settings))
                      `andThen` assertEqual "fallback" True (ncFallback (Ops.connConfig settings))
                      `andThen` assertEqual "cores" (Just 4) (ncCores (Ops.connConfig settings))
                      `andThen` assertEqual "verbosity" (Proto.Verbosity 3) (Ops.connVerbosity settings)
              )
              `andThen` assertSession Proto.NotTrusted 38 (startingStore fx) [setOptionsAt 0 [("keep-going", "true")]] msgLast Conn.ClientClosed (const Pass),
      scriptTest "a trusted client's settings apply as a nix.conf's lines would, a value refused warned of as upstream warns of it" $ \fx ->
        assertSession
          Proto.Trusted
          38
          (startingStore fx)
          [setOptionsAt 3 [("fallback", "maybe"), ("substituters", "https://any.example.org"), ("max-jobs", "8")], reqPath 1 fooPath]
          (msgNext "warning: Boolean setting 'fallback' has invalid value 'maybe'" <> msgLast <> msgLast <> le64 1)
          Conn.ClientClosed
          ( \after -> case fakeSettings after of
              Nothing -> Fail "the store saw no settings"
              Just settings ->
                assertEqual "substituters" ["https://any.example.org"] (ncSubstituters (Ops.connConfig settings))
                  `andThen` assertEqual "fallback" True (ncFallback (Ops.connConfig settings))
          ),
      scriptTest "a NAR that cannot be written closes the connection, its error following the end of the work" $ \fx ->
        assertSession
          Proto.Trusted
          38
          (startingStore fx)
          [reqPath 38 missingPath, reqPath 1 fooPath]
          (msgLast <> "partial" <> msgError "path '/nix/store/00000000000000000000000000000005-missing' is not valid" <> msgError "path '/nix/store/00000000000000000000000000000005-missing' is not valid")
          (Conn.ClosedOnError (notValid missingPath))
          (askedOnly ["NarFromPath missing"]),
      runTest "a handshake that opens with another word is refused with nothing sent" $
        let (ended, written, _) = runScript serverConfig Proto.Trusted emptyFake [le64 serverMagicWord]
         in assertEqual "end" (Conn.HandshakeFailed "protocol mismatch") ended `andThen` assertEqual "written" "" written,
      runTest "a client older than 1.18 is refused once the versions are exchanged, and sent nothing more" $
        let (ended, written, _) = runScript serverConfig Proto.Trusted emptyFake [clientHello 17, reqPath 1 fooPath]
         in assertEqual "end" (Conn.HandshakeFailed "the Nix client version is too old") ended `andThen` assertEqual "written" (le64 serverMagicWord <> le64 0x126) written,
      scriptTest "a client newer than the daemon is served at the daemon's version, and one below 1.38 exchanges no features" $ \fx ->
        assertSession Proto.Trusted 40 (startingStore fx) [reqPath 1 fooPath] (msgLast <> le64 1) Conn.ClientClosed (const Pass)
          `andThen` assertSession Proto.NotTrusted 37 (startingStore fx) [reqPath 1 fooPath] (msgLast <> le64 1) Conn.ClientClosed (const Pass),
      runTestM "however a client's bytes are split into reads, the daemon writes the same bytes, and each reply reads back as what the store answered" $
        codecProperty $
          QC.forAll genQuerySession $ \(known, queries) ->
            let input = clientHello 38 <> foldMap queryBytes queries
                fake = Fake known [] [] Nothing
                (_, whole, _) = runScript serverConfig Proto.Trusted fake [input]
                hello = serverHello 38 Proto.Trusted
             in QC.forAll (genSplit input) $ \pieces ->
                  let (_, split, _) = runScript serverConfig Proto.Trusted fake pieces
                   in QC.conjoin
                        [ split QC.=== whole,
                          BS.take (BS.length hello) whole QC.=== hello,
                          decodeWhole (traverse replyReader queries) (BS.drop (BS.length hello) whole) QC.=== Right (map (answerFrom known) queries)
                        ]
    ]

-- | No paths at all.
emptyFake :: Fake
emptyFake = Fake Map.empty [] [] Nothing

-- | A query the property sends, and the answers it reads back.
data Query
  = AskValid !StorePath
  | AskInfo !StorePath
  | AskValidAmong !(Set.Set StorePath)
  | AskReferrers !StorePath
  | AskAll
  deriving (Show)

data Answer
  = Valid !Bool
  | Info !(Maybe Proto.UnkeyedValidPathInfo)
  | Paths !(Set.Set StorePath)
  deriving (Eq, Show)

-- | A store of up to four paths, some referring to others, and queries
-- about them and about paths it does not hold.
genQuerySession :: QC.Gen (Map.Map StorePath Proto.UnkeyedValidPathInfo, [Query])
genQuerySession = do
  paths <- QC.resize 4 (QC.listOf genWireStorePath)
  infos <- traverse (\_ -> withReferences paths =<< genPathInfo) paths
  let known = Map.fromList (zip paths infos)
      anyPath = QC.oneof (genWireStorePath : [QC.elements paths | not (null paths)])
  queries <-
    QC.resize 8 . QC.listOf $
      QC.oneof
        [ AskValid <$> anyPath,
          AskInfo <$> anyPath,
          AskValidAmong . Set.fromList <$> QC.resize 3 (QC.listOf anyPath),
          AskReferrers <$> anyPath,
          pure AskAll
        ]
  pure (known, queries)
  where
    withReferences paths info = (\refs -> info {Proto.upiReferences = Set.fromList refs}) <$> QC.sublistOf paths

-- | A query as a client sends it.
queryBytes :: Query -> BS.ByteString
queryBytes = \case
  AskValid path -> reqPath 1 path
  AskInfo path -> reqPath 26 path
  AskValidAmong paths -> le64 31 <> wpaths (Set.toList paths) <> le64 0
  AskReferrers path -> reqPath 6 path
  AskAll -> le64 23

-- | What the scripts' store answers a query, from its paths alone.
answerFrom :: Map.Map StorePath Proto.UnkeyedValidPathInfo -> Query -> Answer
answerFrom known = \case
  AskValid path -> Valid (Map.member path known)
  AskInfo path -> Info (Map.lookup path known)
  AskValidAmong paths -> Paths (Set.filter (`Map.member` known) paths)
  AskReferrers path -> Paths (Map.keysSet (Map.filter (Set.member path . Proto.upiReferences) known))
  AskAll -> Paths (Map.keysSet known)

-- | A query's reply as the protocol's own readers read it: the end of
-- the work, then the answer.
replyReader :: Query -> Wire.Decoder Answer
replyReader query =
  Proto.getStderrMessage replyVersion >>= \case
    Proto.StderrLast -> case query of
      AskValid _ -> Valid <$> Wire.getBool
      AskInfo _ -> Wire.getBool >>= \valid -> Info <$> if valid then Just <$> Proto.getUnkeyedValidPathInfo defaultStoreDir replyVersion else pure Nothing
      AskValidAmong _ -> Paths <$> Proto.getStorePathSet defaultStoreDir
      AskReferrers _ -> Paths <$> Proto.getStorePathSet defaultStoreDir
      AskAll -> Paths <$> Proto.getStorePathSet defaultStoreDir
    other -> Wire.malformed ("expected the end of the work, got " <> T.pack (show other))
  where
    replyVersion = Proto.Version Proto.latestVersionNumber (Set.fromList (map Proto.Feature serverFeatures))

-- | Upstream's handshake fixture is what its daemon writes when it and its
-- client both offer 1.10, and what this daemon writes offering 1.10, as it
-- then refuses a client below the oldest it serves.
alsoWrittenByTheDaemon :: (FilePath, BS.ByteString -> TestResult) -> (FilePath, BS.ByteString -> TestResult)
alsoWrittenByTheDaemon (file, check) = (file, \bytes -> check bytes `andThen` daemonWrites bytes)
  where
    daemonWrites bytes =
      let (ended, written, _) = runScript (serverConfigAt (Proto.Version (Proto.VersionNumber 1 10) Set.empty)) Proto.Trusted emptyFake [le64 clientMagicWord <> le64 0x10a]
       in assertEqual "the daemon's handshake" bytes written `andThen` assertEqual "end" (Conn.HandshakeFailed "the Nix client version is too old") ended
