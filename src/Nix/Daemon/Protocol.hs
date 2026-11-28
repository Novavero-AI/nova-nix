{-# LANGUAGE LambdaCase #-}

-- | The values upstream's worker protocol carries, and how each is read and
-- written at every protocol version, as upstream 2.35.4 reads and writes
-- them: @worker-protocol.cc@, @common-protocol.cc@, @serialise.cc@ and
-- @worker-protocol-connection.cc@.  Every gate upstream's serialisers
-- have is kept, the one at 1.16 below the 1.18 floor included: upstream's
-- daemon reads the path info in an upload at 1.16 whatever version was
-- agreed.  A daemon refuses clients below 'minimumVersionNumber' at its
-- handshake, not here.
--
-- A reader takes the store directory paths are named under and the
-- t'Version' both sides agreed, where it needs them, and refuses bytes
-- upstream refuses with upstream's message (without its colours), but for
-- a malformed hash, which is worded as "Nix.Hash" words it, after 2.24.9.
-- A writer is total where upstream's is; one that refuses a value at some
-- versions returns upstream's message for it instead.
--
-- Wire strings are bytes.  The ones whose grammar is ASCII (store paths,
-- hashes, content addresses, checked output names) are read as text, each
-- byte that is not part of valid UTF-8 becoming a NUL: one byte for one,
-- so a hash's spelling keeps the length in bytes upstream picks its
-- decoder by, and a character every such grammar refuses where upstream
-- refuses the byte.  Base64 ends at its padding, and what follows it is
-- ignored there as upstream ignores it.
module Nix.Daemon.Protocol
  ( -- * Versions
    VersionNumber (..),
    Version (..),
    Feature (..),
    latestVersionNumber,
    minimumVersionNumber,
    featureRealisationWithPath,
    getVersionNumber,
    putVersionNumber,
    getFeatures,
    putFeatures,

    -- * Handshake
    getClientMagic,
    putClientMagic,
    getServerMagic,
    putServerMagic,
    getClientObsoleteFields,
    putClientObsoleteFields,
    Trust (..),
    getTrust,
    putTrust,
    HandshakeInfo (..),
    getHandshakeInfo,
    putHandshakeInfo,

    -- * Store paths
    getStorePath,
    putStorePath,
    getOptionalStorePath,
    putOptionalStorePath,
    getStorePathSet,
    putStorePathSet,

    -- * Path info
    NarHash,
    narHashFromDigest,
    narHashDigest,
    getNarHash,
    putNarHash,
    Signature,
    signatureFromParts,
    sigKeyName,
    sigBytes,
    parseSignature,
    renderSignature,
    getSignatureSet,
    putSignatureSet,
    ContentAddressMethod (..),
    ContentAddress,
    contentAddressFromParts,
    caMethod,
    caAlgo,
    caDigest,
    parseContentAddress,
    renderContentAddress,
    getContentAddress,
    putContentAddress,
    getOptionalContentAddress,
    putOptionalContentAddress,
    UnkeyedValidPathInfo (..),
    getUnkeyedValidPathInfo,
    putUnkeyedValidPathInfo,
    ValidPathInfo (..),
    getValidPathInfo,
    putValidPathInfo,

    -- * Derived paths
    OutputName (..),
    getOutputName,
    putOutputName,
    OutputsSpec (AllOutputs),
    namedOutputs,
    outputsSpecNames,
    DerivedPath (..),
    getDerivedPath,
    putDerivedPath,

    -- * Realisations
    UnkeyedRealisation (..),
    getUnkeyedRealisation,
    putUnkeyedRealisation,
    getOptionalUnkeyedRealisation,
    putOptionalUnkeyedRealisation,
    DrvOutput (..),
    getDrvOutput,
    putDrvOutput,
    Realisation (..),
    getRealisation,
    putRealisation,

    -- * Builds
    BuildMode (..),
    getBuildMode,
    putBuildMode,
    SuccessStatus (..),
    FailureStatus (..),
    Success (..),
    Failure (..),
    BuildOutcome (..),
    BuildResult (..),
    getBuildResult,
    putBuildResult,
    KeyedBuildResult (..),
    getKeyedBuildResult,
    putKeyedBuildResult,

    -- * Garbage collection
    GCAction (..),
    getGCAction,
    putGCAction,
    GCPaths (..),
    getGCPaths,
    putGCPaths,

    -- * Messages to the client
    Verbosity (..),
    errorVerbosity,
    ActivityId (..),
    ActivityType (..),
    ResultType (..),
    Field (..),
    Activity (..),
    DaemonError (..),
    StderrMessage (..),
    getStderrMessage,
    putStderrMessage,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (foldM, replicateM, unless, void, when)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word32, Word64, Word8)
import Nix.Daemon.Wire (Decoder, getBool, getBytes, getCount, getInt64, getList, getWord32, getWord64, getWord8, malformed, putBool, putBytes, putList, putNum)
import Nix.Hash (FileIngestionMethod (..), base64ToBytes, bytesToHexText, digestByLength, hashAlgoBytes, parseAnyHash)
import Nix.Json (Json (..), expectJson, jsonText, parseJson, renderJson)
import Nix.Store.Path (StoreDir (..), StorePath, checkStorePathName, parseStorePathBaseName, spName, storePathBaseNameError, storePathBasename, storePathHashLen, storePathNameUpstreamText, storePathToText)
import qualified NovaCache.Base32 as Nix32
import qualified NovaCache.Base64 as B64
import Numeric (showHex)

-- ---------------------------------------------------------------------------
-- Versions
-- ---------------------------------------------------------------------------

-- | A protocol version number.  Upstream compares numbers major first.
data VersionNumber = VersionNumber
  { -- | The major number, 1 for every version upstream has spoken.
    vnMajor :: !Word8,
    -- | The minor number, which every gate compares.
    vnMinor :: !Word8
  }
  deriving (Eq, Ord, Show)

-- | A protocol feature: a name both sides list at the handshake from 1.38,
-- enabling what neither version number promises.
newtype Feature = Feature ByteString
  deriving (Eq, Ord, Show)

-- | The version a connection agreed: the lower of the two numbers, and
-- the features both sides listed.
data Version = Version
  { -- | The lower of the two sides' numbers.
    versionNumber :: !VersionNumber,
    -- | The features both sides listed.
    versionFeatures :: !(Set Feature)
  }
  deriving (Eq, Show)

-- | The newest version number upstream speaks (@WorkerProto::latest@).
latestVersionNumber :: VersionNumber
latestVersionNumber = VersionNumber 1 38

-- | The oldest version number upstream's daemon serves
-- (@WorkerProto::minimum@).
minimumVersionNumber :: VersionNumber
minimumVersionNumber = VersionNumber 1 18

-- | Realisations named by their derivation's path rather than by its hash,
-- each carrying its output path and signatures.  Without it, realisations
-- cannot cross the wire, and a build result carries its outputs in the
-- older JSON form.
featureRealisationWithPath :: Feature
featureRealisationWithPath = Feature "realisation-with-path-not-hash"

-- | Whether a version's number is at least the given one.
atLeast :: Version -> VersionNumber -> Bool
atLeast version gate = versionNumber version >= gate

-- | Whether a version carries a feature.
hasFeature :: Feature -> Version -> Bool
hasFeature feature version = Set.member feature (versionFeatures version)

-- | Read what a version carries, or take what it does not.
since :: Version -> VersionNumber -> a -> Decoder a -> Decoder a
since version gate absent present
  | atLeast version gate = present
  | otherwise = pure absent

-- | Write what a version carries, and nothing where it does not.
whenAtLeast :: Version -> VersionNumber -> Builder -> Builder
whenAtLeast version gate present
  | atLeast version gate = present
  | otherwise = mempty

-- The version gates, each named for what it adds.

-- | A path's info carries its ultimate flag, signatures and content
-- address.
pathInfoTrustSince :: VersionNumber
pathInfoTrustSince = VersionNumber 1 16

-- | The client sends its obsolete reserve-space flag after the handshake.
reserveSpaceSince :: VersionNumber
reserveSpaceSince = VersionNumber 1 11

-- | The client sends its obsolete CPU affinity after the handshake.
cpuAffinitySince :: VersionNumber
cpuAffinitySince = VersionNumber 1 14

-- | An error goes to the client as a record rather than a message and an
-- exit status.
errorRecordSince :: VersionNumber
errorRecordSince = VersionNumber 1 26

-- | A build result carries the outputs it built.
builtOutputsSince :: VersionNumber
builtOutputsSince = VersionNumber 1 28

-- | A build result carries how often it built, its determinism and its
-- start and stop times.
buildTimesSince :: VersionNumber
buildTimesSince = VersionNumber 1 29

-- | A derived path asks for all outputs with @*@, names a derivation file
-- itself by its bare path, and has its output names checked.
derivedPathOutputsSince :: VersionNumber
derivedPathOutputsSince = VersionNumber 1 30

-- | The daemon tells the client its Nix version after the handshake.
daemonVersionSince :: VersionNumber
daemonVersionSince = VersionNumber 1 33

-- | The daemon tells the client whether it trusts it.
trustSince :: VersionNumber
trustSince = VersionNumber 1 35

-- | A build result carries the CPU time its build took.
cpuTimesSince :: VersionNumber
cpuTimesSince = VersionNumber 1 37

-- | A version number as upstream sends it, the major number in the second
-- byte of an @unsigned int@ and the minor in the first; bits above those
-- are dropped, as upstream's @Number::fromWire@ drops them.
getVersionNumber :: Decoder VersionNumber
getVersionNumber = fromWire <$> getWord32
  where
    fromWire word = VersionNumber (fromIntegral (shiftR word 8 .&. 0xff)) (fromIntegral (word .&. 0xff))

-- | Write a version number.
putVersionNumber :: VersionNumber -> Builder
putVersionNumber (VersionNumber major minor) =
  putNum (shiftL (fromIntegral major :: Word32) 8 .|. fromIntegral minor)

-- | A side's feature list, read as upstream's @readStrings@ reads it into
-- a set.
getFeatures :: Decoder (Set Feature)
getFeatures = Set.fromList . map Feature <$> getList getBytes

-- | Write a feature list, in order.
putFeatures :: Set Feature -> Builder
putFeatures features = putList (\(Feature name) -> putBytes name) (Set.toAscList features)

-- ---------------------------------------------------------------------------
-- Handshake
-- ---------------------------------------------------------------------------

-- | The word a client opens a connection with (@WORKER_MAGIC_1@).
clientMagic :: Word32
clientMagic = 0x6e697863

-- | The word a daemon answers with (@WORKER_MAGIC_2@).
serverMagic :: Word32
serverMagic = 0x6478696f

-- | A client's opening word, which a daemon reads before it answers.
getClientMagic :: Decoder ()
getClientMagic = getWord32 >>= \word -> unless (word == clientMagic) (malformed "protocol mismatch")

-- | Write a client's opening word.
putClientMagic :: Builder
putClientMagic = putNum clientMagic

-- | A daemon's answering word, which a client reads before the daemon's
-- version.
getServerMagic :: Decoder ()
getServerMagic = getWord32 >>= \word -> unless (word == serverMagic) (malformed "nix-daemon protocol mismatch from")

-- | Write a daemon's answering word.
putServerMagic :: Builder
putServerMagic = putNum serverMagic

-- | The obsolete fields a client sends once the version is agreed: a CPU
-- affinity, a flag followed by a number when the flag is set, and a
-- reserve-space flag.  Their values mean nothing.
getClientObsoleteFields :: Version -> Decoder ()
getClientObsoleteFields version = do
  when (atLeast version cpuAffinitySince) $ do
    affinity <- getWord32
    when (affinity /= 0) (void getWord32)
  when (atLeast version reserveSpaceSince) (void getWord32)

-- | Write the obsolete fields as upstream's client does: no affinity, and
-- no reserved space.
putClientObsoleteFields :: Version -> Builder
putClientObsoleteFields version =
  whenAtLeast version cpuAffinitySince (putNum (0 :: Word32))
    <> whenAtLeast version reserveSpaceSince (putBool False)

-- | Whether the daemon trusts a client.
data Trust
  = -- | It does: the client may do what the daemon's own user may.
    Trusted
  | -- | It does not.
    NotTrusted
  deriving (Eq, Show)

-- | Whether a daemon trusts its client, or 'Nothing' where it does not
-- say.
getTrust :: Decoder (Maybe Trust)
getTrust =
  getWord8 >>= \case
    0 -> pure Nothing
    1 -> pure (Just Trusted)
    2 -> pure (Just NotTrusted)
    _ -> malformed "Invalid trusted status from remote"

-- | Write whether a daemon trusts its client.
putTrust :: Maybe Trust -> Builder
putTrust trust = putNum $ case trust of
  Nothing -> 0 :: Word8
  Just Trusted -> 1
  Just NotTrusted -> 2

-- | What a daemon tells its client once the handshake is done, as far as
-- the version carries it.
data HandshakeInfo = HandshakeInfo
  { -- | The daemon's Nix version, from 1.33.
    hiDaemonVersion :: !(Maybe ByteString),
    -- | Whether the daemon trusts the client, from 1.35.
    hiTrust :: !(Maybe Trust)
  }
  deriving (Eq, Show)

-- | Read what a daemon tells its client.
getHandshakeInfo :: Version -> Decoder HandshakeInfo
getHandshakeInfo version =
  HandshakeInfo
    <$> since version daemonVersionSince Nothing (Just <$> getBytes)
    <*> since version trustSince Nothing getTrust

-- | Write what a daemon tells its client: its Nix version and whether it
-- trusts it, each where the version carries it.
putHandshakeInfo :: Version -> ByteString -> Maybe Trust -> Builder
putHandshakeInfo version daemonVersion trust =
  whenAtLeast version daemonVersionSince (putBytes daemonVersion)
    <> whenAtLeast version trustSince (putTrust trust)

-- ---------------------------------------------------------------------------
-- Store paths
-- ---------------------------------------------------------------------------

-- | Wire bytes as text, for the ASCII grammars the module header names,
-- each byte that is not part of valid UTF-8 a NUL.  @text@ calls the
-- handler once for each such byte.
grammarText :: ByteString -> Text
grammarText = TE.decodeUtf8With (\_ _ -> Just '\0')

-- | A store path, as upstream's @parseStorePath@ reads one on POSIX: the
-- path is made canonical, its parent must be the store directory, and its
-- last component must be a store path's base name.
getStorePath :: StoreDir -> Decoder StorePath
getStorePath storeDir = getBytes >>= either malformed pure . parseWireStorePath storeDir . grammarText

-- | Write a store path under the store directory.
putStorePath :: StoreDir -> StorePath -> Builder
putStorePath storeDir path = putBytes (TE.encodeUtf8 (storePathToText storeDir path))

-- | Upstream's @StoreDirConfig::parseStorePath@ (store-dir-config.cc at
-- 2.35.4), whose @canonPath@ is lexical: no link is followed.
parseWireStorePath :: StoreDir -> Text -> Either Text StorePath
parseWireStorePath (StoreDir dir) path
  | T.null path = Left "empty path is not a valid store path"
  | not ("/" `T.isPrefixOf` path) = Left ("not an absolute path: \"" <> path <> "\"")
  | Just base <- T.stripPrefix (T.pack dir <> "/") canonical,
    not (T.any (== '/') base) =
      storePathFromBaseName base
  | otherwise = Left ("path \"" <> canonical <> "\" is not in the Nix store")
  where
    canonical = canonicalAbsolute path

-- | An absolute path with its empty and @.@ components dropped, each @..@
-- taking away the component before it, and no trailing separator, as
-- upstream's @canonPath@ makes one on POSIX.
canonicalAbsolute :: Text -> Text
canonicalAbsolute = ("/" <>) . T.intercalate "/" . reverse . foldl' step [] . T.split (== '/')
  where
    step kept component = case component of
      "" -> kept
      "." -> kept
      ".." -> drop 1 kept
      _ -> component : kept

-- | A store path from its base name, refused in the words of upstream's
-- @StorePath@ constructor (path.cc at 2.35.4): it is too short, the hash
-- is not followed by a dash, the hash has a character outside nix-base32,
-- or the name breaks a rule.
storePathFromBaseName :: Text -> Either Text StorePath
storePathFromBaseName base = maybe (Left refusal) Right (parseStorePathBaseName base)
  where
    refusal = fromMaybe ("'" <> base <> "' is not a valid store path") (noDash <|> storePathBaseNameError base)
    noDash = case T.uncons (T.drop storePathHashLen base) of
      Just (c, _) | c /= '-' -> Just ("'" <> base <> "' can't name a store path because the hash part is not followed by a '-'")
      _ -> Nothing

-- | A store path, or 'Nothing' for the empty string.
getOptionalStorePath :: StoreDir -> Decoder (Maybe StorePath)
getOptionalStorePath storeDir = getBytes >>= parsed
  where
    parsed bytes
      | BS.null bytes = pure Nothing
      | otherwise = either malformed (pure . Just) (parseWireStorePath storeDir (grammarText bytes))

-- | Write a store path, or the empty string for none.
putOptionalStorePath :: StoreDir -> Maybe StorePath -> Builder
putOptionalStorePath storeDir = maybe (putBytes BS.empty) (putStorePath storeDir)

-- | A set of store paths.
getStorePathSet :: StoreDir -> Decoder (Set StorePath)
getStorePathSet storeDir = Set.fromList <$> getList (getStorePath storeDir)

-- | Write a set of store paths, in order.
putStorePathSet :: StoreDir -> Set StorePath -> Builder
putStorePathSet storeDir paths = putList (putStorePath storeDir) (Set.toAscList paths)

-- ---------------------------------------------------------------------------
-- Path info
-- ---------------------------------------------------------------------------

-- | The name of the algorithm a NAR hash is taken with.
narHashAlgo :: Text
narHashAlgo = "sha256"

-- | The SHA-256 digest of a path's NAR serialisation.
newtype NarHash = NarHash ByteString
  deriving (Eq, Show)

-- | A NAR hash from its digest, which must be a SHA-256 digest's length.
narHashFromDigest :: ByteString -> Maybe NarHash
narHashFromDigest digest
  | Just (BS.length digest) == hashAlgoBytes narHashAlgo = Just (NarHash digest)
  | otherwise = Nothing

-- | A NAR hash's digest.
narHashDigest :: NarHash -> ByteString
narHashDigest (NarHash digest) = digest

-- | A NAR hash, read as upstream's @Hash::parseAny@ reads it with SHA-256
-- for the algorithm: any spelling of a SHA-256 hash.
getNarHash :: Decoder NarHash
getNarHash = getBytes >>= either malformed (pure . NarHash . snd) . parseAnyHash (Just narHashAlgo) . grammarText

-- | Write a NAR hash in base16, with no algorithm, as upstream does.
putNarHash :: NarHash -> Builder
putNarHash (NarHash digest) = putBytes (TE.encodeUtf8 (bytesToHexText digest))

-- | A signature: the name of the key that made it and its bytes, spelled
-- @name:base64@ on the wire.  It is built only by 'signatureFromParts' and
-- by reading, so every one reads back from its spelling.
data Signature = Signature !ByteString !ByteString
  deriving (Eq, Ord, Show)

-- | The name of the key that made a signature.
sigKeyName :: Signature -> ByteString
sigKeyName (Signature name _) = name

-- | A signature's bytes.
sigBytes :: Signature -> ByteString
sigBytes (Signature _ bytes) = bytes

-- | A signature from its key's name and its bytes: a name that is not
-- empty and has no colon, and bytes that are not empty, which is every
-- signature that reads back from its spelling.
signatureFromParts :: ByteString -> ByteString -> Maybe Signature
signatureFromParts name bytes
  | BS.null name || BS.elem colon name || BS.null bytes = Nothing
  | otherwise = Just (Signature name bytes)

-- | A signature from its spelling, as upstream's @Signature::parse@ reads
-- it (local-keys.cc at 2.35.4): the name before the first colon, and the
-- base64 after it, neither empty.
parseSignature :: ByteString -> Either Text Signature
parseSignature spelled = case BS.break (== colon) spelled of
  (name, rest)
    | not (BS.null name),
      Just encoded <- grammarText <$> BS.stripPrefix ":" rest ->
        case base64ToBytes encoded of
          Just bytes | not (BS.null bytes) -> Right (Signature name bytes)
          Just _ -> Left corrupt
          Nothing -> Left (maybe corrupt badCharacter (T.find (not . isBase64Char) (T.takeWhile (/= '=') encoded)))
  _ -> Left corrupt
  where
    corrupt = "signature is corrupt"
    badCharacter c = "invalid character in Base64 string: '" <> T.singleton c <> "'"
    isBase64Char c = isAsciiUpper c || isAsciiLower c || isDigit c || c == '+' || c == '/' || c == '\n'

-- | The byte that ends a signature's key name.
colon :: Word8
colon = 0x3a

-- | A signature as upstream spells it, its bytes in padded base64.
renderSignature :: Signature -> ByteString
renderSignature (Signature name bytes) = name <> ":" <> TE.encodeUtf8 (B64.encode bytes)

-- | A set of signatures.
getSignatureSet :: Decoder (Set Signature)
getSignatureSet = Set.fromList <$> getList (getBytes >>= either malformed pure . parseSignature)

-- | Write a set of signatures, in order.
putSignatureSet :: Set Signature -> Builder
putSignatureSet sigs = putList (putBytes . renderSignature) (Set.toAscList sigs)

-- | How a content address hashes a path.
data ContentAddressMethod
  = -- | The bytes of a file whose references are listed, as a @.drv@ or
    -- @builtins.toFile@ is (@text@).
    TextMethod
  | -- | A fixed output's contents, by the ingestion method (@fixed@).
    FixedMethod !FileIngestionMethod
  deriving (Eq, Show)

-- | A content address: the method, the algorithm and the digest.  It is
-- built only by 'contentAddressFromParts' and by reading, so its
-- algorithm is one 'hashAlgoBytes' knows and its digest that algorithm's
-- length.
data ContentAddress = ContentAddress !ContentAddressMethod !Text !ByteString
  deriving (Eq, Show)

-- | How a content-addressed path's contents were hashed.
caMethod :: ContentAddress -> ContentAddressMethod
caMethod (ContentAddress method _ _) = method

-- | The name of a content address's algorithm.
caAlgo :: ContentAddress -> Text
caAlgo (ContentAddress _ algo _) = algo

-- | A content address's digest.
caDigest :: ContentAddress -> ByteString
caDigest (ContentAddress _ _ digest) = digest

-- | A content address from its parts: an algorithm 'hashAlgoBytes' knows,
-- and a digest of its length.
contentAddressFromParts :: ContentAddressMethod -> Text -> ByteString -> Maybe ContentAddress
contentAddressFromParts method algo digest
  | hashAlgoBytes algo == Just (BS.length digest) = Just (ContentAddress method algo digest)
  | otherwise = Nothing

-- | A content address from its spelling, as upstream's
-- @ContentAddress::parse@ reads it (content-address.cc at 2.35.4): @text:@
-- or @fixed:@ with @r:@ for a NAR, the algorithm, and the digest in any
-- unprefixed spelling but SRI.  The @git:@ method needs upstream's
-- @git-hashing@ experimental feature, which nova-nix does not have.
parseContentAddress :: Text -> Either Text ContentAddress
parseContentAddress spelled = case T.breakOn ":" spelled of
  (prefix, rest)
    | Just afterPrefix <- T.stripPrefix ":" rest -> case prefix of
        "text" -> hashed TextMethod afterPrefix
        "fixed"
          | Just afterMethod <- T.stripPrefix "r:" afterPrefix -> hashed (FixedMethod NarIngestion) afterMethod
          | "git:" `T.isPrefixOf` afterPrefix -> Left (missingExperimentalFeature "git-hashing" Nothing)
          | otherwise -> hashed (FixedMethod FlatIngestion) afterPrefix
        _ -> Left ("content address prefix '" <> prefix <> "' is unrecognized. Recogonized prefixes are 'text' or 'fixed'")
  _ -> Left ("not a content address because it is not in the form '<prefix>:<rest>': " <> spelled)
  where
    hashed method afterMethod = case T.breakOn ":" afterMethod of
      (algo, rest)
        | Just digest <- T.stripPrefix ":" rest -> ContentAddress method algo <$> digestByLength algo digest
      _ -> Left ("content address hash must be in form '<algo>:<hash>', but found: " <> spelled)

-- | A content address as upstream renders it: the method, the algorithm
-- and the digest in nix-base32.
renderContentAddress :: ContentAddress -> Text
renderContentAddress (ContentAddress method algo digest) = prefix <> algo <> ":" <> Nix32.encode digest
  where
    prefix = case method of
      TextMethod -> "text:"
      FixedMethod FlatIngestion -> "fixed:"
      FixedMethod NarIngestion -> "fixed:r:"

-- | Upstream's refusal for an experimental feature it has not been given,
-- with the reason it needed it, if it gives one.
missingExperimentalFeature :: Text -> Maybe Text -> Text
missingExperimentalFeature feature reason =
  "experimental Nix feature '"
    <> feature
    <> "' is disabled"
    <> maybe "" (\why -> " (" <> why <> ")") reason
    <> "; add '--extra-experimental-features "
    <> feature
    <> "' to enable it"

-- | A content address.
getContentAddress :: Decoder ContentAddress
getContentAddress = getBytes >>= either malformed pure . parseContentAddress . grammarText

-- | Write a content address.
putContentAddress :: ContentAddress -> Builder
putContentAddress = putBytes . TE.encodeUtf8 . renderContentAddress

-- | A content address, or 'Nothing' for the empty string.
getOptionalContentAddress :: Decoder (Maybe ContentAddress)
getOptionalContentAddress = getBytes >>= parsed
  where
    parsed bytes
      | BS.null bytes = pure Nothing
      | otherwise = either malformed (pure . Just) (parseContentAddress (grammarText bytes))

-- | Write a content address, or the empty string for none.
putOptionalContentAddress :: Maybe ContentAddress -> Builder
putOptionalContentAddress = maybe (putBytes BS.empty) putContentAddress

-- | What the store records of a valid path, but the path.
data UnkeyedValidPathInfo = UnkeyedValidPathInfo
  { -- | The derivation that built the path, if it is known.
    upiDeriver :: !(Maybe StorePath),
    -- | The hash of the path's NAR serialisation.
    upiNarHash :: !NarHash,
    -- | The store paths the path refers to.
    upiReferences :: !(Set StorePath),
    -- | When the path was registered, in seconds since the epoch.
    upiRegistrationTime :: !Int64,
    -- | The size of the path's NAR serialisation.
    upiNarSize :: !Word64,
    -- | Whether the path was built here rather than copied in, from 1.16.
    upiUltimate :: !Bool,
    -- | The path's signatures, from 1.16.
    upiSignatures :: !(Set Signature),
    -- | The path's content address, if it has one, from 1.16.
    upiContentAddress :: !(Maybe ContentAddress)
  }
  deriving (Eq, Show)

-- | Read a path's info.
getUnkeyedValidPathInfo :: StoreDir -> Version -> Decoder UnkeyedValidPathInfo
getUnkeyedValidPathInfo storeDir version = do
  deriver <- getOptionalStorePath storeDir
  narHash <- getNarHash
  references <- getStorePathSet storeDir
  registrationTime <- getInt64
  narSize <- getWord64
  (ultimate, sigs, ca) <-
    since version pathInfoTrustSince (False, Set.empty, Nothing) $
      (,,) <$> getBool <*> getSignatureSet <*> getOptionalContentAddress
  pure (UnkeyedValidPathInfo deriver narHash references registrationTime narSize ultimate sigs ca)

-- | Write a path's info, as far as the version carries it.
putUnkeyedValidPathInfo :: StoreDir -> Version -> UnkeyedValidPathInfo -> Builder
putUnkeyedValidPathInfo storeDir version info =
  putOptionalStorePath storeDir (upiDeriver info)
    <> putNarHash (upiNarHash info)
    <> putStorePathSet storeDir (upiReferences info)
    <> putNum (upiRegistrationTime info)
    <> putNum (upiNarSize info)
    <> whenAtLeast
      version
      pathInfoTrustSince
      ( putBool (upiUltimate info)
          <> putSignatureSet (upiSignatures info)
          <> putOptionalContentAddress (upiContentAddress info)
      )

-- | A valid path and what the store records of it.
data ValidPathInfo = ValidPathInfo
  { -- | The path.
    vpiPath :: !StorePath,
    -- | What the store records of it.
    vpiInfo :: !UnkeyedValidPathInfo
  }
  deriving (Eq, Show)

-- | Read a path and its info.
getValidPathInfo :: StoreDir -> Version -> Decoder ValidPathInfo
getValidPathInfo storeDir version =
  ValidPathInfo <$> getStorePath storeDir <*> getUnkeyedValidPathInfo storeDir version

-- | Write a path and its info.
putValidPathInfo :: StoreDir -> Version -> ValidPathInfo -> Builder
putValidPathInfo storeDir version (ValidPathInfo path info) =
  putStorePath storeDir path <> putUnkeyedValidPathInfo storeDir version info

-- ---------------------------------------------------------------------------
-- Derived paths
-- ---------------------------------------------------------------------------

-- | The name of a derivation's output.  Upstream checks an output name
-- only where it reads a derived path from 1.30 and takes any bytes
-- elsewhere; a name here is text, so one that is not UTF-8 is refused
-- wherever it is read.
newtype OutputName = OutputName Text
  deriving (Eq, Ord, Show)

-- | An output name, unchecked.
getOutputName :: Decoder OutputName
getOutputName = getBytes >>= outputNameFromBytes

-- | Write an output name.
putOutputName :: OutputName -> Builder
putOutputName (OutputName name) = putBytes (TE.encodeUtf8 name)

-- | An output name from wire bytes, which must be UTF-8.
outputNameText :: ByteString -> Either Text OutputName
outputNameText bytes =
  either (const (Left ("output name '" <> grammarText bytes <> "' is not valid UTF-8"))) (Right . OutputName) (TE.decodeUtf8' bytes)

-- | 'outputNameText' in a t'Decoder'.
outputNameFromBytes :: ByteString -> Decoder OutputName
outputNameFromBytes = either malformed pure . outputNameText

-- | The byte that separates a derived path from its outputs.
bang :: Word8
bang = 0x21

-- | The byte that separates output names.
comma :: Word8
comma = 0x2c

-- | Which outputs of a derivation are asked for: all of them, or some,
-- by name.
data OutputsSpec
  = -- | Every output.
    AllOutputs
  | -- | The outputs named, which are never none.
    SomeOutputs !(Set OutputName)
  deriving (Eq, Show)

-- | Some outputs by name, or 'Nothing' for no names.
namedOutputs :: Set OutputName -> Maybe OutputsSpec
namedOutputs names
  | Set.null names = Nothing
  | otherwise = Just (SomeOutputs names)

-- | The outputs named, or 'Nothing' for every output.
outputsSpecNames :: OutputsSpec -> Maybe (Set OutputName)
outputsSpecNames spec = case spec of
  AllOutputs -> Nothing
  SomeOutputs names -> Just names

-- | A path a client asks the daemon to make valid.
data DerivedPath
  = -- | A store path as it is.
    DerivedOpaque !StorePath
  | -- | Outputs of the derivation at a store path.
    DerivedBuilt !StorePath !OutputsSpec
  deriving (Eq, Show)

-- | Whether a store path names a derivation, as upstream's
-- @isDerivation@ asks: its name ends in @.drv@.
isDerivationPath :: StorePath -> Bool
isDerivationPath path = ".drv" `T.isSuffixOf` spName path

-- | Read a derived path.  From 1.30 it is upstream's @parseLegacy@
-- (derived-path.cc at 2.35.4): the path, then after the last @!@ either
-- @*@ or names checked as store path names, and a path built from another
-- built output needs upstream's @dynamic-derivations@ experimental feature,
-- which nova-nix does not have.  Before 1.30 it is @parsePathWithOutputs@
-- (path-with-outputs.cc): names after the first @!@, unchecked, and a bare
-- derivation path asks for all of its outputs.
getDerivedPath :: StoreDir -> Version -> Decoder DerivedPath
getDerivedPath storeDir version = getBytes >>= either malformed pure . parse
  where
    parse spelled
      | atLeast version derivedPathOutputsSince = parseLegacy storeDir spelled
      | otherwise = parseWithOutputs storeDir spelled

-- | A derived path as upstream reads one from 1.30.  A second @!@ makes
-- the path before the last one a built output itself, which upstream
-- refuses once it has read the store path before the first, naming the
-- output between the first and the second.
parseLegacy :: StoreDir -> ByteString -> Either Text DerivedPath
parseLegacy storeDir spelled = case BS.break (== bang) spelled of
  (path, rest)
    | BS.null rest -> DerivedOpaque <$> parseWireStorePath storeDir (grammarText path)
    | otherwise -> do
        drv <- parseWireStorePath storeDir (grammarText path)
        case BS.break (== bang) (BS.drop 1 rest) of
          (outputs, more)
            | BS.null more -> DerivedBuilt drv <$> parseOutputsSpec outputs
            | otherwise ->
                Left (missingExperimentalFeature "dynamic-derivations" (Just ("building output '" <> grammarText outputs <> "' of '" <> storePathBasename drv <> "'")))

-- | Which outputs a derived path asks for from 1.30, as upstream's
-- @OutputsSpec::parse@ reads them: @*@, or names separated by commas,
-- each checked as a store path name.  Upstream checks the names in order
-- once they are a set, so the first refused is the least.
parseOutputsSpec :: ByteString -> Either Text OutputsSpec
parseOutputsSpec spelled
  | spelled == "*" = Right AllOutputs
  | otherwise = do
      names <- traverse checkedName (Set.toAscList (Set.fromList (BS.split comma spelled)))
      maybe (Left "name must not be empty") Right (namedOutputs (Set.fromList names))
  where
    checkedName bytes =
      let name = grammarText bytes
       in either (Left . storePathNameUpstreamText) (const (Right (OutputName name))) (checkStorePathName name)

-- | A derived path as upstream reads one before 1.30.
parseWithOutputs :: StoreDir -> ByteString -> Either Text DerivedPath
parseWithOutputs storeDir spelled = do
  let (pathPart, rest) = BS.break (== bang) spelled
      names = filter (not . BS.null) (BS.split comma (BS.drop 1 rest))
  path <- parseWireStorePath storeDir (grammarText pathPart)
  outputs <- traverse outputNameText names
  pure $ case namedOutputs (Set.fromList outputs) of
    Just spec -> DerivedBuilt path spec
    Nothing
      | isDerivationPath path -> DerivedBuilt path AllOutputs
      | otherwise -> DerivedOpaque path

-- | Write a derived path, or upstream's refusal of one the version cannot
-- spell: before 1.30 a derivation file itself, which reads back as asking
-- for its outputs.
putDerivedPath :: StoreDir -> Version -> DerivedPath -> Either Text Builder
putDerivedPath storeDir version derived = putBytes . TE.encodeUtf8 <$> spelling
  where
    pathText = storePathToText storeDir
    joinNames names = T.intercalate "," [name | OutputName name <- Set.toAscList names]
    spelling
      | atLeast version derivedPathOutputsSince = Right $ case derived of
          DerivedOpaque path -> pathText path
          DerivedBuilt drv AllOutputs -> pathText drv <> "!*"
          DerivedBuilt drv (SomeOutputs names) -> pathText drv <> "!" <> joinNames names
      | otherwise = case derived of
          DerivedOpaque path
            | isDerivationPath path ->
                Left
                  ( "trying to request '"
                      <> pathText path
                      <> "', but daemon protocol "
                      <> T.pack (show (vnMajor (versionNumber version)))
                      <> "."
                      <> T.pack (show (vnMinor (versionNumber version)))
                      <> " is too old (< 1.29) to request a derivation file"
                  )
            | otherwise -> Right (pathText path)
          DerivedBuilt drv AllOutputs -> Right (pathText drv)
          DerivedBuilt drv (SomeOutputs names) -> Right (pathText drv <> "!" <> joinNames names)

-- ---------------------------------------------------------------------------
-- Realisations
-- ---------------------------------------------------------------------------

-- | The path a derivation's output was realised at, and its signatures.
data UnkeyedRealisation = UnkeyedRealisation
  { -- | The output's path.
    urOutPath :: !StorePath,
    -- | Signatures over the realisation.
    urSignatures :: !(Set Signature)
  }
  deriving (Eq, Show)

-- | Upstream's refusal of a realisation where the version lacks the
-- feature that lets one cross the wire, saying what it was needed for.
missingRealisationFeature :: Text -> Text
missingRealisationFeature neededFor =
  "the daemon is missing the '" <> featureName <> "' protocol feature, needed to " <> neededFor
  where
    Feature nameBytes = featureRealisationWithPath
    featureName = grammarText nameBytes

-- | What upstream says a realisation without its path was needed for.
buildTracePurpose :: Text
buildTracePurpose = "understand build trace"

-- | What upstream says a derivation's output was needed for.
contentAddressingPurpose :: Text
contentAddressingPurpose = "support content-addressing derivations"

-- | Refuse to read a realisation where the version lacks the feature.
requireRealisationFeature :: Version -> Text -> Decoder ()
requireRealisationFeature version neededFor =
  unless (hasFeature featureRealisationWithPath version) (malformed (missingRealisationFeature neededFor))

-- | Bytes a version with the feature reads, or upstream's refusal to write
-- them for one without it.
withRealisationFeature :: Version -> Text -> Builder -> Either Text Builder
withRealisationFeature version neededFor bytes
  | hasFeature featureRealisationWithPath version = Right bytes
  | otherwise = Left (missingRealisationFeature neededFor)

-- | Read where an output was realised.
getUnkeyedRealisation :: StoreDir -> Version -> Decoder UnkeyedRealisation
getUnkeyedRealisation storeDir version = do
  requireRealisationFeature version buildTracePurpose
  UnkeyedRealisation <$> getStorePath storeDir <*> getSignatureSet

-- | Write where an output was realised, or upstream's refusal where the
-- version lacks 'featureRealisationWithPath'.
putUnkeyedRealisation :: StoreDir -> Version -> UnkeyedRealisation -> Either Text Builder
putUnkeyedRealisation storeDir version realisation =
  withRealisationFeature version buildTracePurpose (unkeyedRealisationBytes storeDir realisation)

-- | Where an output was realised, as a version with the feature reads it.
unkeyedRealisationBytes :: StoreDir -> UnkeyedRealisation -> Builder
unkeyedRealisationBytes storeDir (UnkeyedRealisation path sigs) =
  putStorePath storeDir path <> putSignatureSet sigs

-- | Where an output was realised, if it was.  Without the feature,
-- upstream reads a string in its place and takes it as none.
getOptionalUnkeyedRealisation :: StoreDir -> Version -> Decoder (Maybe UnkeyedRealisation)
getOptionalUnkeyedRealisation storeDir version
  | hasFeature featureRealisationWithPath version =
      getWord8 >>= \case
        0 -> pure Nothing
        1 -> Just <$> getUnkeyedRealisation storeDir version
        _ -> malformed "Invalid optional build trace from remote"
  | otherwise = Nothing <$ getBytes

-- | Write where an output was realised, if it was: none at any version,
-- which a version without the feature reads as an empty string, and a
-- realisation as 'putUnkeyedRealisation' writes it.
putOptionalUnkeyedRealisation :: StoreDir -> Version -> Maybe UnkeyedRealisation -> Either Text Builder
putOptionalUnkeyedRealisation storeDir version realisation = case realisation of
  Nothing -> Right (putNum (0 :: Word8))
  Just unkeyed -> (putNum (1 :: Word8) <>) <$> putUnkeyedRealisation storeDir version unkeyed

-- | An output of a derivation, named by the derivation's path.
data DrvOutput = DrvOutput
  { -- | The derivation's path.
    doDrvPath :: !StorePath,
    -- | The output's name.
    doOutputName :: !OutputName
  }
  deriving (Eq, Show)

-- | Read an output of a derivation.
getDrvOutput :: StoreDir -> Version -> Decoder DrvOutput
getDrvOutput storeDir version = do
  requireRealisationFeature version contentAddressingPurpose
  DrvOutput <$> getStorePath storeDir <*> getOutputName

-- | Write an output of a derivation, or upstream's refusal where the
-- version lacks 'featureRealisationWithPath'.
putDrvOutput :: StoreDir -> Version -> DrvOutput -> Either Text Builder
putDrvOutput storeDir version (DrvOutput drv name) =
  withRealisationFeature version contentAddressingPurpose (putStorePath storeDir drv <> putOutputName name)

-- | An output of a derivation and where it was realised.
data Realisation = Realisation
  { -- | The output.
    rId :: !DrvOutput,
    -- | Where it was realised.
    rRealisation :: !UnkeyedRealisation
  }
  deriving (Eq, Show)

-- | Read a realisation.
getRealisation :: StoreDir -> Version -> Decoder Realisation
getRealisation storeDir version =
  Realisation <$> getDrvOutput storeDir version <*> getUnkeyedRealisation storeDir version

-- | Write a realisation, or upstream's refusal where the version lacks
-- 'featureRealisationWithPath', which names the output it writes first.
putRealisation :: StoreDir -> Version -> Realisation -> Either Text Builder
putRealisation storeDir version (Realisation output realisation) =
  (<>) <$> putDrvOutput storeDir version output <*> putUnkeyedRealisation storeDir version realisation

-- ---------------------------------------------------------------------------
-- Builds
-- ---------------------------------------------------------------------------

-- | How a build treats outputs that are already valid.
data BuildMode
  = -- | Keep them.
    BuildNormal
  | -- | Build them again and replace them.
    BuildRepair
  | -- | Build them again and compare.
    BuildCheck
  deriving (Eq, Show)

-- | Read a build mode.
getBuildMode :: Decoder BuildMode
getBuildMode =
  getWord8 >>= \case
    0 -> pure BuildNormal
    1 -> pure BuildRepair
    2 -> pure BuildCheck
    _ -> malformed "Invalid build mode"

-- | Write a build mode.
putBuildMode :: BuildMode -> Builder
putBuildMode mode = putNum $ case mode of
  BuildNormal -> 0 :: Word8
  BuildRepair -> 1
  BuildCheck -> 2

-- | How a build that succeeded did.
data SuccessStatus
  = -- | The outputs were built.
    Built
  | -- | The outputs were substituted.
    Substituted
  | -- | The outputs were valid already.
    AlreadyValid
  | -- | The derivation resolved to one whose outputs were valid already.
    ResolvesToAlreadyValid
  deriving (Eq, Show, Enum, Bounded)

-- | Why a build failed.  Upstream's @HashMismatch@ goes to the client as
-- 'OutputRejected', since the protocol has no word for it, so it has no
-- constructor here.
data FailureStatus
  = -- | The builder failed.
    PermanentFailure
  | -- | An input was refused.
    InputRejected
  | -- | An output was refused, or its hash was not the one fixed.
    OutputRejected
  | -- | The build failed in a way that may not recur.
    TransientFailure
  | -- | The build failed before and the failure was cached.
    CachedFailure
  | -- | The build ran past its time limit or its silence limit.
    TimedOut
  | -- | Any other failure.
    MiscFailure
  | -- | A dependency failed to build.
    DependencyFailed
  | -- | The build logged more than its limit.
    LogLimitExceeded
  | -- | A repeated build gave different outputs.
    NotDeterministic
  | -- | The outputs could not be substituted, and building was not
    -- allowed.
    NoSubstituters
  deriving (Eq, Show, Enum, Bounded)

-- | A status's byte on the wire (@buildResultStatusTable@,
-- common-protocol.cc at 2.35.4).
statusByte :: Either SuccessStatus FailureStatus -> Word8
statusByte status = case status of
  Left Built -> 0
  Left Substituted -> 1
  Left AlreadyValid -> 2
  Right PermanentFailure -> 3
  Right InputRejected -> 4
  Right OutputRejected -> 5
  Right TransientFailure -> 6
  Right CachedFailure -> 7
  Right TimedOut -> 8
  Right MiscFailure -> 9
  Right DependencyFailed -> 10
  Right LogLimitExceeded -> 11
  Right NotDeterministic -> 12
  Left ResolvesToAlreadyValid -> 13
  Right NoSubstituters -> 14

-- | Every status, keyed by its byte.
statusesByByte :: Map Word8 (Either SuccessStatus FailureStatus)
statusesByByte =
  Map.fromList [(statusByte status, status) | status <- map Left [minBound .. maxBound] ++ map Right [minBound .. maxBound]]

-- | Read a build's status.
getBuildStatus :: Decoder (Either SuccessStatus FailureStatus)
getBuildStatus = getWord8 >>= \byte -> maybe (malformed ("Invalid BuildResult status " <> T.pack (show byte) <> " from remote")) pure (Map.lookup byte statusesByByte)

-- | A build that succeeded.
data Success = Success
  { -- | How.
    successStatus :: !SuccessStatus,
    -- | Where each output was realised.
    successOutputs :: !(Map OutputName UnkeyedRealisation)
  }
  deriving (Eq, Show)

-- | A build that failed.
data Failure = Failure
  { -- | Why.
    failureStatus :: !FailureStatus,
    -- | What the build said.
    failureMessage :: !ByteString,
    -- | Whether builds of it that were repeated gave different outputs.
    failureNonDeterministic :: !Bool
  }
  deriving (Eq, Show)

-- | How a build ended.
data BuildOutcome
  = -- | It succeeded.
    Succeeded !Success
  | -- | It failed.
    Failed !Failure
  deriving (Eq, Show)

-- | A build's result, as far as the version carries it.
data BuildResult = BuildResult
  { -- | How it ended.
    brOutcome :: !BuildOutcome,
    -- | How many times it was built, from 1.29.
    brTimesBuilt :: !Word32,
    -- | When it started, in seconds since the epoch, from 1.29.
    brStartTime :: !Int64,
    -- | When it stopped, from 1.29.
    brStopTime :: !Int64,
    -- | The user CPU time it took, in microseconds, from 1.37.
    brCpuUser :: !(Maybe Int64),
    -- | The system CPU time it took, in microseconds, from 1.37.
    brCpuSystem :: !(Maybe Int64)
  }
  deriving (Eq, Show)

-- | A duration that may be absent: a tag, then the microseconds.
getOptionalMicroseconds :: Decoder (Maybe Int64)
getOptionalMicroseconds =
  getWord8 >>= \case
    0 -> pure Nothing
    1 -> Just <$> getInt64
    _ -> malformed "Invalid optional tag from remote"

-- | Write a duration that may be absent.
putOptionalMicroseconds :: Maybe Int64 -> Builder
putOptionalMicroseconds = maybe (putNum (0 :: Word8)) (\micros -> putNum (1 :: Word8) <> putNum micros)

-- | The hash upstream puts in the identifiers of a build's outputs where it
-- sends them in JSON: @Hash::dummy@, a SHA-256 digest of zeros, which an
-- older client splits off unread.
dummyOutputHash :: Text
dummyOutputHash = "sha256:" <> T.replicate 64 "0"

-- | Read a build result.
getBuildResult :: StoreDir -> Version -> Decoder BuildResult
getBuildResult storeDir version = do
  status <- getBuildStatus
  message <- getBytes
  (timesBuilt, nonDeterministic, startTime, stopTime) <-
    since version buildTimesSince (0, False, 0, 0) $
      (,,,) <$> getWord32 <*> getBool <*> getInt64 <*> getInt64
  (cpuUser, cpuSystem) <-
    since version cpuTimesSince (Nothing, Nothing) $
      (,) <$> getOptionalMicroseconds <*> getOptionalMicroseconds
  outputs <- getBuiltOutputs storeDir version
  let outcome = case status of
        Left success -> Succeeded (Success success outputs)
        Right failure -> Failed (Failure failure message nonDeterministic)
  pure (BuildResult outcome timesBuilt startTime stopTime cpuUser cpuSystem)

-- | A build's outputs: with the feature, each name and its realisation;
-- from 1.28 without it, upstream's older JSON form, of which only each
-- output's name and path are read; before 1.28, none.
getBuiltOutputs :: StoreDir -> Version -> Decoder (Map OutputName UnkeyedRealisation)
getBuiltOutputs storeDir version
  | hasFeature featureRealisationWithPath version =
      Map.fromList <$> getList ((,) <$> getOutputName <*> getUnkeyedRealisation storeDir version)
  | atLeast version builtOutputsSince = do
      -- Upstream reads the pairs into a map, the last of a repeated key
      -- winning, and then walks it in key order.
      pairs <- Map.fromList <$> getList ((,) <$> getBytes <*> getBytes)
      foldM addOutput Map.empty (Map.toAscList pairs)
  | otherwise = pure Map.empty
  where
    addOutput outputs (outputId, json) = case BS.breakSubstring "!" outputId of
      (_, rest)
        | BS.null rest -> malformed ("Invalid derivation output id " <> grammarText outputId)
        | otherwise -> do
            name <- outputNameFromBytes (BS.drop 1 rest)
            path <- either malformed pure (outPathOf json)
            pure (Map.insert name (UnkeyedRealisation path Set.empty) outputs)
    outPathOf json = do
      document <- either (const (Left ("output realisation is not valid JSON: " <> grammarText json))) Right (TE.decodeUtf8' json) >>= parseJson
      members <- expectJson "object" (\case JsonObject members -> Just members; _ -> Nothing) document
      value <- maybe (Left ("Expected JSON object to contain key 'outPath' but it doesn't: " <> jsonText document)) Right (Map.lookup "outPath" members)
      base <- expectJson "string" (\case JsonString text -> Just text; _ -> Nothing) value
      storePathFromBaseName base

-- | Write a build result, as far as the version carries it.
putBuildResult :: StoreDir -> Version -> BuildResult -> Builder
putBuildResult storeDir version result =
  putNum (statusByte status)
    <> putBytes message
    <> whenAtLeast
      version
      buildTimesSince
      (putNum (brTimesBuilt result) <> putBool nonDeterministic <> putNum (brStartTime result) <> putNum (brStopTime result))
    <> whenAtLeast version cpuTimesSince (putOptionalMicroseconds (brCpuUser result) <> putOptionalMicroseconds (brCpuSystem result))
    <> putBuiltOutputs storeDir version outputs
  where
    -- The protocol carries a failure's message and determinism with a
    -- success too, and a success's outputs with a failure, each empty
    -- where its case has none.
    (status, message, nonDeterministic, outputs) = case brOutcome result of
      Succeeded (Success how built) -> (Left how, BS.empty, False, built)
      Failed (Failure why said differed) -> (Right why, said, differed, Map.empty)

-- | Write a build's outputs, as far as the version carries them.  Without
-- the feature, each goes as upstream's older JSON form, under an
-- identifier made with 'dummyOutputHash', and its signatures are not
-- sent.
putBuiltOutputs :: StoreDir -> Version -> Map OutputName UnkeyedRealisation -> Builder
putBuiltOutputs storeDir version outputs
  | hasFeature featureRealisationWithPath version =
      putList (\(name, realisation) -> putOutputName name <> unkeyedRealisationBytes storeDir realisation) (Map.toAscList outputs)
  | atLeast version builtOutputsSince = putList jsonEntry (Map.toAscList outputs)
  | otherwise = mempty
  where
    jsonEntry (OutputName name, UnkeyedRealisation path _) =
      let outputId = dummyOutputHash <> "!" <> name
       in putBytes (TE.encodeUtf8 outputId)
            <> putBytes (renderJson (JsonObject (Map.fromList [("id", JsonString outputId), ("outPath", JsonString (storePathBasename path))])))

-- | A build result and the derived path it answers.
data KeyedBuildResult = KeyedBuildResult
  { -- | The path that was asked for.
    kbrPath :: !DerivedPath,
    -- | Its result.
    kbrResult :: !BuildResult
  }
  deriving (Eq, Show)

-- | Read a keyed build result.
getKeyedBuildResult :: StoreDir -> Version -> Decoder KeyedBuildResult
getKeyedBuildResult storeDir version =
  KeyedBuildResult <$> getDerivedPath storeDir version <*> getBuildResult storeDir version

-- | Write a keyed build result, or the refusal 'putDerivedPath' gives its
-- path.
putKeyedBuildResult :: StoreDir -> Version -> KeyedBuildResult -> Either Text Builder
putKeyedBuildResult storeDir version (KeyedBuildResult path result) =
  (<> putBuildResult storeDir version result) <$> putDerivedPath storeDir version path

-- ---------------------------------------------------------------------------
-- Garbage collection
-- ---------------------------------------------------------------------------

-- | What a collection does.
data GCAction
  = -- | Report the paths that are live.
    ReturnLive
  | -- | Report the paths that are dead.
    ReturnDead
  | -- | Delete the paths that are dead.
    DeleteDead
  | -- | Delete the paths given, which must be dead.
    DeleteSpecific
  deriving (Eq, Show)

-- | Read a collection's action.
getGCAction :: Decoder GCAction
getGCAction =
  getWord32 >>= \case
    0 -> pure ReturnLive
    1 -> pure ReturnDead
    2 -> pure DeleteDead
    3 -> pure DeleteSpecific
    _ -> malformed "Invalid GC action"

-- | Write a collection's action.
putGCAction :: GCAction -> Builder
putGCAction action = putNum $ case action of
  ReturnLive -> 0 :: Word32
  ReturnDead -> 1
  DeleteDead -> 2
  DeleteSpecific -> 3

-- | Which paths a collection considers.
data GCPaths
  = -- | Every path in the store.
    WholeStore
  | -- | The paths given, and whether their referrers go with them.
    SpecificPaths !(Set StorePath) !Bool
  deriving (Eq, Show)

-- | Read which paths a collection considers.
getGCPaths :: StoreDir -> Decoder GCPaths
getGCPaths storeDir =
  getWord8 >>= \case
    0 -> SpecificPaths <$> getStorePathSet storeDir <*> getBool
    1 -> pure WholeStore
    _ -> malformed "Invalid whole store indicator from remote"

-- | Write which paths a collection considers.
putGCPaths :: StoreDir -> GCPaths -> Builder
putGCPaths storeDir paths = case paths of
  SpecificPaths specific deleteReferrers ->
    putNum (0 :: Word8) <> putStorePathSet storeDir specific <> putBool deleteReferrers
  WholeStore -> putNum (1 :: Word8)

-- ---------------------------------------------------------------------------
-- Messages to the client
-- ---------------------------------------------------------------------------

-- | How loud a message is: upstream's @Verbosity@, 0 for an error.
newtype Verbosity = Verbosity Word32
  deriving (Eq, Show)

-- | The verbosity of an error (@lvlError@).
errorVerbosity :: Verbosity
errorVerbosity = Verbosity 0

-- | An activity's number, unique within the daemon.
newtype ActivityId = ActivityId Word64
  deriving (Eq, Show)

-- | What kind of activity one is: upstream's @ActivityType@.
newtype ActivityType = ActivityType Word32
  deriving (Eq, Show)

-- | What kind of result an activity reports: upstream's @ResultType@.
newtype ResultType = ResultType Word32
  deriving (Eq, Show)

-- | A field of an activity or a result.
data Field
  = -- | A number.
    FieldInt !Word64
  | -- | A string.
    FieldString !ByteString
  deriving (Eq, Show)

-- | An activity's start.
data Activity = Activity
  { -- | The activity's number.
    actId :: !ActivityId,
    -- | How loud it is.
    actLevel :: !Verbosity,
    -- | What kind it is.
    actType :: !ActivityType,
    -- | What it says it is doing.
    actText :: !ByteString,
    -- | Its fields, as its type defines them.
    actFields :: ![Field],
    -- | The activity it is part of, 0 for none.
    actParent :: !ActivityId
  }
  deriving (Eq, Show)

-- | An error a daemon sends its client.  From 1.26 it goes as a record and
-- its exit status stays behind, reading back as upstream's default of 1;
-- before 1.26 only its message and exit status go, and it reads back at
-- the error level with no traces.
data DaemonError = DaemonError
  { -- | How loud it is.
    errLevel :: !Verbosity,
    -- | What went wrong.
    errMessage :: !ByteString,
    -- | What was being done when it happened, as upstream lists it: each
    -- added in front, so the outermost first.
    errTraces :: ![ByteString],
    -- | The status a command that fails with it exits with.
    errExitStatus :: !Word32
  }
  deriving (Eq, Show)

-- | What a daemon tells its client while it works on an operation.
data StderrMessage
  = -- | A line to log.
    StderrNext !ByteString
  | -- | A request for up to this many bytes of the client's upload.
    StderrRead !Word64
  | -- | Bytes for the client's sink.
    StderrWrite !ByteString
  | -- | The operation's work is done, and its reply follows.
    StderrLast
  | -- | The operation failed.
    StderrError !DaemonError
  | -- | An activity started.
    StderrStartActivity !Activity
  | -- | An activity stopped.
    StderrStopActivity !ActivityId
  | -- | An activity reported a result, of a type and with fields.
    StderrResult !ActivityId !ResultType ![Field]
  deriving (Eq, Show)

-- The words that open each message (worker-protocol.hh at 2.35.4).

stderrNext, stderrRead, stderrWrite, stderrLast, stderrError, stderrStartActivity, stderrStopActivity, stderrResult :: Word64
stderrNext = 0x6f6c6d67
stderrRead = 0x64617461
stderrWrite = 0x64617416
stderrLast = 0x616c7473
stderrError = 0x63787470
stderrStartActivity = 0x53545254
stderrStopActivity = 0x53544f50
stderrResult = 0x52534c54

-- | The type an error record names, the only one upstream writes or reads.
errorRecordType :: ByteString
errorRecordType = "Error"

-- | The exit status of an error that does not carry one (@ErrorInfo@'s
-- default).
defaultExitStatus :: Word32
defaultExitStatus = 1

-- | Read a message from a daemon, as upstream's client reads it
-- (@processStderrReturn@, worker-protocol-connection.cc at 2.35.4).
getStderrMessage :: Version -> Decoder StderrMessage
getStderrMessage version = getWord64 >>= message
  where
    message code
      | code == stderrNext = StderrNext <$> getBytes
      | code == stderrRead = StderrRead <$> getWord64
      | code == stderrWrite = StderrWrite <$> getBytes
      | code == stderrLast = pure StderrLast
      | code == stderrError = StderrError <$> getDaemonError version
      | code == stderrStartActivity =
          fmap StderrStartActivity $
            (Activity . ActivityId <$> getWord64)
              <*> (Verbosity <$> getWord32)
              <*> (ActivityType <$> getWord32)
              <*> getBytes
              <*> getFields
              <*> (ActivityId <$> getWord64)
      | code == stderrStopActivity = StderrStopActivity . ActivityId <$> getWord64
      | code == stderrResult = (StderrResult . ActivityId <$> getWord64) <*> (ResultType <$> getWord32) <*> getFields
      | otherwise = malformed ("got unknown message type " <> T.pack (showHex code "") <> " from Nix daemon")

-- | Write a message to a client.
putStderrMessage :: Version -> StderrMessage -> Builder
putStderrMessage version message = case message of
  StderrNext line -> putNum stderrNext <> putBytes line
  StderrRead size -> putNum stderrRead <> putNum size
  StderrWrite bytes -> putNum stderrWrite <> putBytes bytes
  StderrLast -> putNum stderrLast
  StderrError err -> putNum stderrError <> putDaemonError version err
  StderrStartActivity (Activity (ActivityId act) (Verbosity level) (ActivityType kind) text fields (ActivityId parent)) ->
    putNum stderrStartActivity <> putNum act <> putNum level <> putNum kind <> putBytes text <> putFields fields <> putNum parent
  StderrStopActivity (ActivityId act) -> putNum stderrStopActivity <> putNum act
  StderrResult (ActivityId act) (ResultType kind) fields -> putNum stderrResult <> putNum act <> putNum kind <> putFields fields

-- | An activity's fields: a count, then each field's type and value.
getFields :: Decoder [Field]
getFields = getWord32 >>= \count -> replicateM (fromIntegral count) field
  where
    field =
      getWord32 >>= \case
        0 -> FieldInt <$> getWord64
        1 -> FieldString <$> getBytes
        kind -> malformed ("got unsupported field type " <> T.pack (showHex kind "") <> " from Nix daemon")

-- | Write an activity's fields.
putFields :: [Field] -> Builder
putFields = putList field
  where
    field (FieldInt n) = putNum (0 :: Word32) <> putNum n
    field (FieldString s) = putNum (1 :: Word32) <> putBytes s

-- | Read an error, as upstream's @readError@ reads its record from 1.26
-- (serialise.cc at 2.35.4).
getDaemonError :: Version -> Decoder DaemonError
getDaemonError version
  | atLeast version errorRecordSince = do
      kind <- getBytes
      unless (kind == errorRecordType) (malformed ("unexpected error type '" <> grammarText kind <> "'"))
      level <- getWord32
      _name <- getBytes
      msg <- getBytes
      noPosition
      traceCount <- getCount
      traces <- replicateM traceCount (noPosition *> getBytes)
      pure (DaemonError (Verbosity level) msg traces defaultExitStatus)
  | otherwise = do
      msg <- getBytes
      DaemonError errorVerbosity msg [] <$> getWord32
  where
    noPosition = getWord64 >>= \havePos -> unless (havePos == 0) (malformed "deserializing error positions is not supported")

-- | Write an error, as far as the version carries it.
putDaemonError :: Version -> DaemonError -> Builder
putDaemonError version (DaemonError (Verbosity level) msg traces status)
  | atLeast version errorRecordSince =
      putBytes errorRecordType
        <> putNum level
        <> putBytes errorRecordType
        <> putBytes msg
        <> noPosition
        <> putList (\hint -> noPosition <> putBytes hint) traces
  | otherwise = putBytes msg <> putNum status
  where
    noPosition = putNum (0 :: Word64)
