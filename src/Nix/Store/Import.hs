{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Paths a store takes from elsewhere with what they claim of themselves
-- already stated: a NAR with its path's info ('importNar'), or a text with
-- the references it names ('addTextToStore'), each held to upstream's
-- checks before it becomes valid, and refused in upstream's words.
--
-- A NAR is taken when one of the caller's keys signed its path's info, or
-- when its content address and references make exactly its path, which
-- its bytes then have to reproduce.  Its hash and size are held to the
-- bytes that arrive.  A derivation, by either route, is taken only when
-- each of its outputs is the path its inputs make ('checkDerivationOutputs').
module Nix.Store.Import
  ( -- * Importing a NAR
    SignatureRequirement (..),
    importNar,

    -- * Adding a text
    addTextToStore,

    -- * The checks
    addressMakesPath,
    checkDerivationOutputs,
  )
where

import Control.Exception (IOException, displayException, onException, try)
import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT (..), liftEither, runExceptT, throwError, withExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (StateT, evalStateT, gets, modify')
import qualified Data.ByteString as BS
import Data.Foldable (for_, traverse_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Nix.Daemon.Protocol (ContentAddress, ContentAddressMethod (..), caAlgo, caDigest, caMethod, contentAddressFromParts, renderSignature)
import Nix.Derivation (Derivation (..), DerivationOutput (..), drvExtension, drvFileDerivation, fixedOutput, fixedOutputModulo, fromATerm, inputAddressed, inputAddressedModulo)
import Nix.Hash (FileIngestionMethod (..), bytesToHexText, hexToBytes, makeFixedOutputPath, makeStorePath, makeTextPath, sha256Digest)
import Nix.Logger (logWarning)
import Nix.Store (ArchiveEnd (..), NarStreamFailure (..), PathRegistration (..), Store (..), UpstreamHazard (..), canonicalizeMetadata, checkNarClaim, finishNarUnpack, isValid, newNarUnpackSink, registerPath, registrationFor, sinkNarStream, upstreamManagedRefusal, volumeCaseHack, withPathLock)
import qualified Nix.Store.ExecBit as ExecBit
import Nix.Store.Path (StoreDir, StorePath (spName), badStorePathText, defaultStoreDir, storePathBasename, storePathToFilePath, storePathToText)
import Nix.Substituter (checkContentAddress)
import qualified NovaCache.Hash as Hash
import qualified NovaCache.NarInfo as NarInfo
import qualified NovaCache.Signing as Signing
import qualified System.Directory as Dir

-- ---------------------------------------------------------------------------
-- Importing a NAR
-- ---------------------------------------------------------------------------

-- | Whether a NAR's path must be signed to be imported, as upstream's
-- store asks under @require-sigs@.
data SignatureRequirement
  = -- | One of these keys must have signed the path's info, unless its
    -- content address makes its path ('addressMakesPath').
    RequireSignature ![Signing.PublicKey]
  | -- | It is taken unsigned, as upstream takes a path from a client it
    -- trusts that asks it not to check.
    SignaturesNotRequired

-- | Import a path from its NAR, given what the path claims of itself as
-- a registration: its NAR hash and size, its references and deriver, its
-- content address, its signatures and whether it is ultimately trusted.
-- Upstream's local store adds a path it is handed a NAR for the same way.
--
-- A path that must be signed and is neither signed by one of the keys nor
-- content-addressed is refused before anything is read.  A stated
-- address that does not make the path is warned of there, when
-- signatures are required, and again when it is refused, as upstream
-- warns of it.  A path already valid is left as it is and the source is
-- not read.  Otherwise, under the path's lock and with the path still not
-- valid, the NAR is unpacked at the path from the source, a chunk at a
-- time until it gives an empty one, which it gives where the archive
-- ends; then the archive's hash and size are held to the claim, the tree
-- as it lies on disk to the hash, the tree to the address it states, the
-- address to the path, and a derivation to its outputs.  A refusal
-- removes the tree.  What passes is registered as the claim states it.  A
-- store upstream Nix's database may also manage is refused before
-- anything is read ("Nix.Store.Upstream").
importNar :: Store -> SignatureRequirement -> PathRegistration -> IO BS.ByteString -> IO (Either Text ())
importNar store requirement claim source = runExceptT $ do
  narHash <- liftEither (claimedNarHash claim)
  when (requiresSignature requirement && maybe False (not . addressMakesPath sp (prReferences claim)) (prContentAddress claim)) $
    liftIO (logWarning (stLogger store) (notContentAddressed sp))
  unless (trusted requirement claim narHash) $
    throwError ("cannot add path '" <> logical <> "' because it lacks a signature by a trusted key")
  refusal <- liftIO (upstreamManagedRefusal ReplacesUpstreamPaths (stDir store))
  traverse_ throwError refusal
  ExceptT $ withPathLock (stLogger store) dir sp $ \_ -> do
    valid <- isValid store sp
    if valid then pure (Right ()) else replacing dest (restoreAndCheck narHash)
  where
    sp = prPath claim
    dir = stDir store
    dest = storePathToFilePath dir sp
    logical = storePathToText defaultStoreDir sp
    caseHack = volumeCaseHack (stCaseSensitivity store)
    restoreAndCheck narHash = do
      sink <- liftIO (newNarUnpackSink (stCaseSensitivity store) dest)
      (digest, size) <- withExceptT streamFailure (ExceptT (sinkNarStream AtEndOfInput sink source))
      liftEither (heldToClaim narHash digest size)
      ExceptT (finishNarUnpack sink)
      liftIO (canonicalizeMetadata dest)
      onDisk <- liftIO (ExecBit.narHashOfPath caseHack dest)
      liftEither (heldToClaim narHash onDisk size)
      for_ (prContentAddress claim) (ExceptT . checkContentAddress caseHack defaultStoreDir sp dest)
      ExceptT (registrable store claim)
      liftIO (registerPath (stDB store) claim)
    streamFailure failure = case failure of
      NarStreamRefused reason -> reason
      NarStreamMalformed reason -> reason
    heldToClaim narHash got size = checkNarClaim logical narHash (toInteger (prNarSize claim)) got (toInteger size)

-- | Run a write of a path that leaves it only when it succeeds: what was
-- at the path is cleared first, and the path is cleared again on a
-- refusal or an exception.
replacing :: FilePath -> ExceptT Text IO () -> IO (Either Text ())
replacing dest write = do
  Dir.removePathForcibly dest
  outcome <- runExceptT write `onException` Dir.removePathForcibly dest
  either (\reason -> Left reason <$ Dir.removePathForcibly dest) (pure . Right) outcome

-- | The NAR hash a claim states, which must be a SHA-256 hash in the
-- store's spelling.
claimedNarHash :: PathRegistration -> Either Text Hash.NixHash
claimedNarHash claim =
  either (const (Left ("cannot add path '" <> storePathToText defaultStoreDir (prPath claim) <> "': its NAR hash '" <> prNarHash claim <> "' is not a SHA-256 hash in nix-base32"))) Right (Hash.parseNixHash (prNarHash claim))

-- | Whether a requirement asks for a signature at all.
requiresSignature :: SignatureRequirement -> Bool
requiresSignature (RequireSignature _) = True
requiresSignature SignaturesNotRequired = False

-- | Whether a claim may be imported as far as its signatures go, as
-- upstream counts a path's signatures: a path whose content address
-- makes it needs none, and any other needs one a key given verifies over
-- its fingerprint.
trusted :: SignatureRequirement -> PathRegistration -> Hash.NixHash -> Bool
trusted requirement claim narHash = case requirement of
  SignaturesNotRequired -> True
  RequireSignature keys ->
    maybe False (addressMakesPath (prPath claim) (prReferences claim)) (prContentAddress claim)
      || any (\sig -> any (\key -> either (const False) (Signing.verify key fingerprinted) (TE.decodeUtf8' (renderSignature sig))) keys) (Set.toList (prSignatures claim))
  where
    -- The fields a path's fingerprint is made of, which is what its
    -- signatures sign; a narinfo's transport fields, which no signature
    -- covers, are empty.
    fingerprinted =
      NarInfo.NarInfo
        { NarInfo.niStorePath = storePathToText defaultStoreDir (prPath claim),
          NarInfo.niUrl = "",
          NarInfo.niCompression = "",
          NarInfo.niFileHash = Nothing,
          NarInfo.niFileSize = Nothing,
          NarInfo.niNarHash = Hash.formatNixHash narHash,
          NarInfo.niNarSize = toInteger (prNarSize claim),
          NarInfo.niReferences = map storePathBasename (prReferences claim),
          NarInfo.niDeriver = Nothing,
          NarInfo.niSigs = [],
          NarInfo.niCA = Nothing
        }

-- | Whether a claim may be registered as it lies on disk: its content
-- address, if it states one, makes its path, or upstream's warning and
-- refusal, and a derivation's outputs are the paths its inputs make.
registrable :: Store -> PathRegistration -> IO (Either Text ())
registrable store claim = case prContentAddress claim of
  Just address
    | not (addressMakesPath sp (prReferences claim) address) -> do
        logWarning (stLogger store) (notContentAddressed sp)
        pure (Left ("cannot add path '" <> logical <> "' to the Nix store because it claims to be content-addressed but isn't"))
  _
    | isDerivation sp -> either (pure . Left) (checkDerivationOutputs dir sp) =<< readDerivationAt dir sp
    | otherwise -> pure (Right ())
  where
    sp = prPath claim
    dir = stDir store
    logical = storePathToText defaultStoreDir sp

-- | Upstream's warning of a stated content address that does not make
-- its path, given each time it tests the address: once when it counts a
-- path's signatures and again when it registers the path.
notContentAddressed :: StorePath -> Text
notContentAddressed sp = "path '" <> storePathToText defaultStoreDir sp <> "' claims to be content-addressed but isn't"

-- | Whether a content address and the references it is stated with make
-- exactly a path, as upstream makes a path from a content address: a
-- text by its SHA-256 with its references; a NAR by its SHA-256 as a
-- @source@ path with its references, and @self@ when it references
-- itself; any other fixed address with no references at all.
addressMakesPath :: StorePath -> [StorePath] -> ContentAddress -> Bool
addressMakesPath sp refs address = case caMethod address of
  TextMethod -> caAlgo address == sha256Algo && makeTextPath name digest refs == Right sp
  FixedMethod NarIngestion
    | caAlgo address == sha256Algo -> makeStorePath defaultStoreDir sourceType digest name == Right sp
  FixedMethod method -> null others && not self && makeFixedOutputPath name (caAlgo address) method digest == Right sp
  where
    name = spName sp
    digest = caDigest address
    self = sp `elem` refs
    others = Set.toAscList (Set.delete sp (Set.fromList refs))
    sourceType = "source" <> T.concat [":" <> storePathToText defaultStoreDir r | r <- others] <> (if self then ":self" else "")

-- ---------------------------------------------------------------------------
-- Adding a text
-- ---------------------------------------------------------------------------

-- | Add a text, with the store paths it references, as the path its
-- SHA-256 and its name make, and answer the path, as upstream adds a
-- text, which is how a client writes a @.drv@.  A valid path is
-- passed over.  Otherwise, under the path's lock, the text is written at
-- the path, its metadata made canonical, a derivation held to its outputs
-- ('checkDerivationOutputs'), and it is registered with its text address; a
-- refusal removes it.  The references must be valid.  A store upstream
-- Nix's database may also manage is refused before anything is written
-- ("Nix.Store.Upstream").
addTextToStore :: Store -> Text -> BS.ByteString -> [StorePath] -> IO (Either Text StorePath)
addTextToStore store name contents refs = runExceptT $ do
  sp <- liftEither (either (Left . badStorePathText) Right (makeTextPath name digest refs))
  refusal <- liftIO (upstreamManagedRefusal ReplacesUpstreamPaths (stDir store))
  traverse_ throwError refusal
  let dest = storePathToFilePath (stDir store) sp
  ExceptT $ withPathLock (stLogger store) (stDir store) sp $ \_ -> do
    valid <- isValid store sp
    if valid
      then pure (Right ())
      else replacing dest $ do
        liftIO (BS.writeFile dest contents)
        liftIO (canonicalizeMetadata dest)
        when (isDerivation sp) $ do
          drv <- liftEither (parsedDerivation sp contents)
          ExceptT (checkDerivationOutputs (stDir store) sp drv)
        reg <- liftIO (registrationFor store sp Nothing refs)
        liftIO (registerPath (stDB store) reg {prContentAddress = contentAddressFromParts TextMethod sha256Algo digest})
  pure sp
  where
    digest = sha256Digest contents

-- ---------------------------------------------------------------------------
-- Derivations
-- ---------------------------------------------------------------------------

-- | Whether a derivation's outputs are the paths its inputs make, as
-- upstream checks a derivation it registers: a fixed output's path is the
-- one its hash makes, each output of any other the one the derivation's
-- masked modulo hash makes, its inputs' modulo hashes read from their
-- @.drv@ files in the store, and each output's environment variable
-- names its path.  The refusals are upstream's.  A derivation with
-- outputs that are neither, a floating content address or more than one
-- fixed output, is refused: nova-nix builds neither.
checkDerivationOutputs :: StoreDir -> StorePath -> Derivation -> IO (Either Text ())
checkDerivationOutputs dir sp drv = runExceptT $ do
  expected <- case fixedOutput drv of
    Just out -> liftEither (fixedOutputPaths out)
    Nothing
      | not (all (T.null . doHashAlgo) (drvOutputs drv)) ->
          throwError ("derivation '" <> storePathToText defaultStoreDir sp <> "' has outputs with content addresses nova-nix does not check")
      | otherwise -> do
          inputs <- evalStateT (inputModuloHashes dir drv) Map.empty
          made <- liftEither (either (Left . badStorePathText) Right (inputAddressed drvName inputs drv))
          pure (Map.fromList [(doName out, doPath out) | out <- drvOutputs (drvFileDerivation made)])
  for_ (drvOutputs drv) $ \out -> do
    let stated = doPath out
    for_ (Map.lookup (doName out) expected) $ \path ->
      unless (path == stated) $
        throwError ("derivation has incorrect output '" <> storePathToText defaultStoreDir stated <> "', should be '" <> storePathToText defaultStoreDir path <> "'")
    let pathText = storePathToText defaultStoreDir stated
    case Map.lookup (doName out) (drvEnv drv) of
      Nothing -> throwError ("derivation has missing environment variable '" <> doName out <> "', should be '" <> pathText <> "' but is not present")
      Just value
        | value /= TE.encodeUtf8 pathText ->
            throwError ("derivation has incorrect environment variable '" <> doName out <> "', should be '" <> pathText <> "' but is actually '" <> TE.decodeUtf8Lenient value <> "'")
        | otherwise -> pure ()
  where
    drvName = fromMaybe (spName sp) (T.stripSuffix drvExtension (spName sp))
    fixedOutputPaths out = case hexToBytes (doHash out) of
      Nothing -> Left ("derivation '" <> storePathToText defaultStoreDir sp <> "' has a malformed hash for output '" <> doName out <> "'")
      Just digest ->
        let (method, algo) = case T.stripPrefix "r:" (doHashAlgo out) of
              Just rest -> (NarIngestion, rest)
              Nothing -> (FlatIngestion, doHashAlgo out)
         in either (Left . badStorePathText) (Right . Map.singleton (doName out)) (makeFixedOutputPath drvName algo method digest)

-- | The modulo hash (hex) of each input derivation with the outputs taken
-- from it, in path order, as 'Nix.Derivation.inputAddressed' takes them,
-- each read from its @.drv@ file in the store, and each derivation hashed
-- once however many paths reach it.
inputModuloHashes :: StoreDir -> Derivation -> StateT (Map StorePath Text) (ExceptT Text IO) [(Text, [Text])]
inputModuloHashes dir drv = traverse (\(input, outs) -> (,outs) <$> moduloHashOf input) (Map.toList (drvInputDrvs drv))
  where
    moduloHashOf input = do
      known <- gets (Map.lookup input)
      case known of
        Just hex -> pure hex
        Nothing -> do
          inputDrv <- liftEither =<< liftIO (readDerivationAt dir input)
          hex <- case fixedOutputModulo inputDrv of
            Just modulo -> pure (bytesToHexText modulo)
            Nothing -> bytesToHexText . (`inputAddressedModulo` inputDrv) <$> inputModuloHashes dir inputDrv
          modify' (Map.insert input hex)
          pure hex

-- | The derivation a @.drv@ file in the store holds, or why it cannot be
-- read.
readDerivationAt :: StoreDir -> StorePath -> IO (Either Text Derivation)
readDerivationAt dir sp = do
  contents <- try (BS.readFile (storePathToFilePath dir sp))
  pure $ case contents of
    Left (failure :: IOException) -> Left ("cannot read derivation '" <> storePathToText defaultStoreDir sp <> "': " <> T.pack (displayException failure))
    Right bytes -> parsedDerivation sp bytes

-- | A derivation parsed from its file, or upstream's refusal.
parsedDerivation :: StorePath -> BS.ByteString -> Either Text Derivation
parsedDerivation sp bytes = either (\err -> Left ("error parsing derivation '" <> storePathToText defaultStoreDir sp <> "': " <> err)) Right (fromATerm bytes)

-- | Whether a store path names a derivation, by its name, as upstream
-- tells one.
isDerivation :: StorePath -> Bool
isDerivation sp = drvExtension `T.isSuffixOf` spName sp

-- | The algorithm a text address and a @source@ path hash with.
sha256Algo :: Text
sha256Algo = "sha256"
