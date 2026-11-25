{-# LANGUAGE LambdaCase #-}

-- | Tests of substitution from binary caches.
module Test.Substituter
  ( testSubstituter,
    testSubstituterRetry,
    testStatedContentAddress,
    testVerifySigs,
    testNarInfoValidation,
  )
where

import Control.Exception (ErrorCall (..), SomeAsyncException (..), SomeException, asyncExceptionToException, bracket, bracket_, fromException, throwIO, try)
import Control.Monad (void)
import Control.Monad.State.Strict (evalStateT)
import qualified Data.ByteString as BS
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.IO.Encoding (getFileSystemEncoding, mkTextEncoding, setFileSystemEncoding)
import HttpFixture (Reply (..), httpResponse, withHttpServer)
import qualified Network.HTTP.Client as HTTP
import Nix.Builder (BuildConfig (..), BuildResult (..), buildWithDeps, defaultBuildConfig, substitutePaths)
import Nix.Derivation (Derivation (..), DerivationOutput (..), currentPlatform)
import Nix.Hash (moduloHashFinalize, moduloHashInit, moduloHashUpdate, rawHashWithAlgo)
import Nix.Http (FetchRetryPolicy (..), RetryEffects (..), defaultFetchRetryPolicy, ioRetryEffects)
import qualified Nix.Realise as Realise
import Nix.Store (Store (..), caseHackDiskNames, closeStore, onDiskNameKey, openStore, releasePathLock, tryAcquirePathLock, unpackNarEntry, writeDrv)
import Nix.Store.CaseSensitive (CaseSensitivity (..), probeCaseSensitivity, trySetCaseSensitiveDir, volumeCaseHack)
import Nix.Store.DB (PathRegistration (..))
import qualified Nix.Store.ExecBit as ExecBit
import Nix.Store.Metadata (executablePermissions, readOnlyPermissions)
import Nix.Store.Path (StoreDir (..), defaultStoreDir, storePathToFilePath, storePathToText)
import Nix.Store.Path.Internal (StorePath (..))
import qualified Nix.Substituter as Subst
import qualified NovaCache.Base32 as Nix32
import qualified NovaCache.Base64 as B64
import qualified NovaCache.Hash as CHash
import qualified NovaCache.NAR as NAR
import qualified NovaCache.NarInfo as NarInfo
import qualified NovaCache.Signing as Signing
import qualified NovaCache.Zstd as CZstd
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getPermissions, getTemporaryDirectory, removeDirectoryRecursive)
import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified System.Info as SI
import Test.Harness (TestResult (..), andThen, assertEqual, assertLeft, canonicalAs, chunkReader, forceRemoveIfExists, narRequest, offeredSubstitute, offeredThenFetched, posixOnlyEntries, quotedAsCxx, runTest, runTestM, servedPaths, sigTestKeyText, sigTestWrongKeyText)
import qualified Test.QuickCheck as QC

-- ---------------------------------------------------------------------------
-- Tests: Substituter (Phase 3, Batch 6)
-- ---------------------------------------------------------------------------

-- | Drain a chunk source to its byte count - the shape of a streaming
-- consumer, for tests that only care whether the pipeline fails.
drainChunkSource :: IO BS.ByteString -> IO (Either Subst.AttemptFailure Int)
drainChunkSource pull = go 0
  where
    go n = do
      chunk <- pull
      if BS.null chunk
        then pure (Right n)
        else let total = n + BS.length chunk in total `seq` go total

testSubstituter :: IO [Bool]
testSubstituter = do
  putStrLn "substituter"
  -- Every tree this group materializes under the temp directory keys
  -- sibling names the way a store on that volume would.
  tmpSensitivity <- tempCaseSensitivity
  sequence
    [ -- sortCaches: priority ordering
      runTest "sortCaches priority ordering" $
        let c1 = Subst.CacheConfig "https://a.example.com" ["key-a"] 40
            c2 = Subst.CacheConfig "https://b.example.com" ["key-b"] 10
            c3 = Subst.CacheConfig "https://c.example.com" ["key-c"] 30
            sorted = Subst.sortCaches [c1, c2, c3]
         in case sorted of
              [s1, s2, s3] ->
                if Subst.ccPriority s1 == 10 && Subst.ccPriority s2 == 30 && Subst.ccPriority s3 == 40
                  then Pass
                  else Fail ("bad order: " <> T.pack (show (map Subst.ccPriority sorted)))
              _ -> Fail "expected 3 caches",
      -- sortCaches: empty list
      runTest "sortCaches empty" $
        assertEqual "empty-sort" [] (Subst.sortCaches []),
      -- readBodyCapped: the client-side body cap (mirror of the server's
      -- readBodyLimited).  A body reader is just IO ByteString yielding
      -- chunks then empty.
      runTestM "readBodyCapped concatenates under the cap" $ do
        reader <- chunkReader ["abc", "def", "g"]
        body <- Subst.readBodyCapped 10 reader
        pure (assertEqual "under cap" (Just "abcdefg") body),
      runTestM "readBodyCapped allows exactly the cap" $ do
        reader <- chunkReader ["abcde", "fghij"]
        body <- Subst.readBodyCapped 10 reader
        pure (assertEqual "at cap" (Just "abcdefghij") body),
      runTestM "readBodyCapped aborts past the cap" $ do
        reader <- chunkReader ["abcdef", "ghijkl"]
        body <- Subst.readBodyCapped 10 reader
        pure (assertEqual "over cap" Nothing body),
      -- decompressorFor decides support from the narinfo's declared
      -- values alone, so unsupported compression rejects before any
      -- NAR download; xz resolves a bounded decompressor.
      runTest "decompressorFor xz resolves" $
        case Subst.decompressorFor 1024 "xz" of
          Right _ -> Pass
          Left err -> Fail ("xz rejected: " <> err),
      runTestM "decompressorFor none is identity" $
        case Subst.decompressorFor 5 "none" of
          Right decompress -> assertEqual "identity" (Right "bytes") <$> decompress "bytes"
          Left err -> pure (Fail ("expected none to be supported, got: " <> err)),
      -- verifyNarSize: the declared NarSize is a signed claim that flows
      -- into the store DB, so it must equal the downloaded byte count.
      runTest "verifyNarSize accepts matching size" $
        assertEqual
          "narsize-match"
          (Right ())
          (Subst.verifyNarSize ((sampleNarInfo sampleNarHash) {NarInfo.niNarSize = toInteger (BS.length sampleNarBytes)}) sampleNarBytes),
      runTest "verifyNarSize rejects mismatched size" $
        case Subst.verifyNarSize ((sampleNarInfo sampleNarHash) {NarInfo.niNarSize = toInteger (BS.length sampleNarBytes) + 1}) sampleNarBytes of
          Left err | "size mismatch" `T.isInfixOf` err -> Pass
          other -> Fail ("expected size mismatch, got: " <> T.pack (show other)),
      -- decompressNar: "none" passes through
      runTestM "decompressNar none" $
        let input = "fake nar data"
         in assertEqual "decompress-none" (Right input) <$> Subst.decompressNar (toInteger (BS.length input)) "none" input,
      -- decompressNar: an empty Compression field means bzip2 upstream
      -- (the field's historical default), never identity, so the empty
      -- spelling must decode a real bzip2 body exactly as the named
      -- one does.
      runTestM "decompressNar empty means bzip2" $
        case B64.decode bzip2FixtureB64 of
          Left err -> pure (Fail ("fixture base64 does not decode: " <> T.pack err))
          Right compressed -> do
            out <- Subst.decompressNar xzFixtureSize "" compressed
            pure $ case out of
              Right decoded -> assertEqual "empty-is-bzip2" xzFixturePayload decoded
              Left err -> Fail ("empty Compression failed to decode as bzip2: " <> err),
      -- decompressNar: bzip2 decodes under the same declared-NarSize
      -- bound as the other codecs; the fixture comes from the bzip2
      -- CLI, so the decoder is checked against the format's own
      -- reference encoder rather than its own library.
      runTestM "decompressNar bzip2 bounded roundtrip" $
        case B64.decode bzip2FixtureB64 of
          Left err -> pure (Fail ("fixture base64 does not decode: " <> T.pack err))
          Right compressed -> do
            out <- Subst.decompressNar xzFixtureSize "bzip2" compressed
            pure $ case out of
              Right decoded -> assertEqual "bzip2-roundtrip" xzFixturePayload decoded
              Left err -> Fail ("bzip2 roundtrip failed: " <> err),
      runTestM "decompressNar bzip2 over-bound rejects" $
        case B64.decode bzip2FixtureB64 of
          Left err -> pure (Fail ("fixture base64 does not decode: " <> T.pack err))
          Right compressed -> do
            out <- Subst.decompressNar (xzFixtureSize - 1) "bzip2" compressed
            pure $ case out of
              Left err | "NarSize" `T.isInfixOf` err -> Pass
              other -> Fail ("expected over-bound rejection, got: " <> T.pack (show other)),
      -- decompressNar: xz decodes under the declared-NarSize bound
      -- (the fixture is a real 112-byte xz stream of 1792 payload
      -- bytes, embedded base64 to keep the source ASCII).
      runTestM "decompressNar xz bounded roundtrip" $
        case B64.decode xzFixtureB64 of
          Left err -> pure (Fail ("fixture base64 does not decode: " <> T.pack err))
          Right compressed -> do
            out <- Subst.decompressNar xzFixtureSize "xz" compressed
            pure $ case out of
              Right decoded -> assertEqual "xz-roundtrip" xzFixturePayload decoded
              Left err -> Fail ("xz roundtrip failed: " <> err),
      -- The output bound is exact: one byte under the real size must
      -- refuse, naming the declared NarSize.
      runTestM "decompressNar xz over-bound rejects" $
        case B64.decode xzFixtureB64 of
          Left err -> pure (Fail ("fixture base64 does not decode: " <> T.pack err))
          Right compressed -> do
            out <- Subst.decompressNar (xzFixtureSize - 1) "xz" compressed
            pure $ case out of
              Left err | "NarSize" `T.isInfixOf` err -> Pass
              other -> Fail ("expected over-bound rejection, got: " <> T.pack (show other)),
      -- decompressNar: bytes that are not an xz stream are an error,
      -- never silently passed through
      runTestM "decompressNar xz garbage rejects" $ do
        out <- Subst.decompressNar 64 "xz" "not an xz stream"
        pure $ case out of
          Left _ -> Pass
          Right _ -> Fail "garbage decoded as xz",
      -- decompressNar: zstd decodes under the same declared-NarSize
      -- bound; the fixture compresses with the sublibrary's own
      -- encoder, the same pairing the push path ships.
      runTestM "decompressNar zstd bounded roundtrip" $ do
        out <- Subst.decompressNar xzFixtureSize "zstd" zstdFixtureCompressed
        pure $ case out of
          Right decoded -> assertEqual "zstd-roundtrip" xzFixturePayload decoded
          Left err -> Fail ("zstd roundtrip failed: " <> err),
      runTestM "decompressNar zstd over-bound rejects" $ do
        out <- Subst.decompressNar (xzFixtureSize - 1) "zstd" zstdFixtureCompressed
        pure $ case out of
          Left err | "NarSize" `T.isInfixOf` err -> Pass
          other -> Fail ("expected over-bound rejection, got: " <> T.pack (show other)),
      -- decompressNar: unknown compression
      runTestM "decompressNar unknown" $ do
        out <- Subst.decompressNar 4 "brotli" "data"
        pure $ case out of
          Left _ -> Pass
          Right _ -> Fail "expected error for unknown",
      -- parseReferences: narinfo references are wire-format basenames
      runTest "parseReferences basenames" $
        let refs = ["aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-hello", "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb-dep-2.0"]
            expected =
              [ StorePath "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "hello",
                StorePath "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" "dep-2.0"
              ]
         in assertEqual "parse-refs" (Right expected) (Subst.parseReferences refs),
      -- parseReferences: a malformed token is a loud error, never dropped
      runTest "parseReferences malformed rejected" $
        case Subst.parseReferences ["not-a-store-path"] of
          Left _ -> Pass
          Right refs -> Fail ("expected rejection, got: " <> T.pack (show refs)),
      -- parseDeriver: basename becomes the DB's full path text
      runTest "parseDeriver basename" $
        let sp = StorePath "cccccccccccccccccccccccccccccccc" "hello.drv"
            expected = Just (T.pack (storePathToFilePath defaultStoreDir sp))
         in assertEqual "parse-deriver" (Right expected) (Subst.parseDeriver defaultStoreDir (Just "cccccccccccccccccccccccccccccccc-hello.drv")),
      -- parseDeriver: upstream's unknown-deriver sentinel means no deriver
      runTest "parseDeriver unknown sentinel" $
        assertEqual "parse-deriver-unknown" (Right Nothing) (Subst.parseDeriver defaultStoreDir (Just "unknown-deriver")),
      -- parseDeriver: absent stays absent, malformed is an error
      runTest "parseDeriver absent and malformed" $
        case (Subst.parseDeriver defaultStoreDir Nothing, Subst.parseDeriver defaultStoreDir (Just "bogus")) of
          (Right Nothing, Left _) -> Pass
          other -> Fail ("unexpected: " <> T.pack (show other)),
      -- Recorded production narinfos from cache.nixos.org (paths this
      -- codebase did not produce): the parse -> validate -> signature
      -- pipeline against real cache data, the production ed25519
      -- signature verified offline with the shipped public key.
      runTest "recorded cache.nixos.org narinfo verifies end to end" $
        case NarInfo.parseNarInfo recordedHelloNarInfo of
          Left err -> Fail ("parse failed: " <> T.pack err)
          Right ni ->
            case Subst.validateNarInfoFields ni >> Subst.verifySigs Subst.defaultCacheConfig ni of
              Left err -> Fail ("validate/verify failed: " <> err)
              Right () -> assertEqual "compression" "xz" (NarInfo.niCompression ni),
      -- References of a path built by Hydra, including the self
      -- reference, and its Deriver - both produced by upstream Nix.
      runTest "recorded narinfo references and deriver parse" $
        case NarInfo.parseNarInfo recordedHelloNarInfo of
          Left err -> Fail ("parse failed: " <> T.pack err)
          Right ni ->
            case (Subst.parseReferences (NarInfo.niReferences ni), Subst.parseDeriver defaultStoreDir (NarInfo.niDeriver ni)) of
              (Right refs, Right (Just _)) ->
                assertEqual
                  "reference hashes"
                  ["dska0s3gxxd8azbsn85pwm6xcqhivldw", "jppkr0h8aap5z7m4xy4vg5yrwlly9h2v"]
                  (map spHash refs)
              other -> Fail ("references/deriver did not parse: " <> T.pack (show other)),
      -- The empty References line ("References: " with a trailing
      -- space) production caches emit for leaf paths.
      runTest "recorded no-reference narinfo verifies" $
        case NarInfo.parseNarInfo recordedHelloDocNarInfo of
          Left err -> Fail ("parse failed: " <> T.pack err)
          Right ni ->
            case Subst.validateNarInfoFields ni >> Subst.verifySigs Subst.defaultCacheConfig ni >> Subst.parseReferences (NarInfo.niReferences ni) of
              Right [] -> Pass
              other -> Fail ("expected verified empty references, got: " <> T.pack (show other)),
      -- The fingerprint covers NarSize: altering the signed claim must
      -- break the production signature.
      runTest "tampered recorded narinfo fails signature" $
        case NarInfo.parseNarInfo recordedHelloNarInfo of
          Left err -> Fail ("parse failed: " <> T.pack err)
          Right ni ->
            case Subst.verifySigs Subst.defaultCacheConfig (ni {NarInfo.niNarSize = NarInfo.niNarSize ni + 1}) of
              Left _ -> Pass
              Right () -> Fail "signature verified over an altered NarSize",
      -- Streaming pipeline: the chunk-fed download materializes,
      -- hashes, and verifies without ever holding the NAR.  The tree
      -- carries the interesting shapes: nesting, an executable, a
      -- symlink, and a sibling pair colliding on folding filesystems -
      -- the digest equality proves the materialized tree re-serialises
      -- to its NAR on every platform strategy.
      -- Every length word, tag, and padding run must survive splitting
      -- across Await boundaries, so the whole materialization runs at
      -- several chunk sizes including single-byte feeds.
      runTestM "streaming unpack materializes and verifies across chunk sizes" $ do
        tmpBase <- getTemporaryDirectory
        let tmpDir = tmpBase </> "nova-nix-test-stream-unpack"
            chunkSizes = [1, 7, 8, 11] :: [Int]
            runAt n = do
              let dest = tmpDir </> ("out-" <> show n)
              source <- chunkReader (streamChunks n streamTestNar)
              result <- Subst.consumeNarStream tmpSensitivity dest (streamTestNarInfo streamTestNar) streamTestDigest source
              onDisk <- ExecBit.serialiseFromPath (volumeCaseHack tmpSensitivity) dest
              pure $ case result of
                Left err ->
                  Just ("chunk size " <> T.pack (show n) <> ": " <> Subst.attemptFailureMessage err)
                Right narByteCount
                  | narByteCount == BS.length streamTestNar
                      && CHash.hashBytes (NAR.serialise onDisk) == streamTestDigest ->
                      Nothing
                  | otherwise ->
                      Just ("chunk size " <> T.pack (show n) <> ": streamed tree diverges from its NAR")
        forceRemoveIfExists tmpDir
        createDirectoryIfMissing True tmpDir
        outcomes <- mapM runAt chunkSizes
        forceRemoveIfExists tmpDir
        pure $ case catMaybes outcomes of
          [] -> Pass
          (msg : _) -> Fail msg,
      -- A truncated stream is a loud parse failure, never a short
      -- tree - and it retries: truncation and a torn transfer
      -- parse-fail the same way.
      runTestM "streaming unpack refuses a truncated stream as transient" $ do
        tmpBase <- getTemporaryDirectory
        let tmpDir = tmpBase </> "nova-nix-test-stream-trunc"
        forceRemoveIfExists tmpDir
        createDirectoryIfMissing True tmpDir
        source <- chunkReader (streamChunks 7 (BS.take (BS.length streamTestNar - 10) streamTestNar))
        result <- Subst.consumeNarStream tmpSensitivity (tmpDir </> "out") (streamTestNarInfo streamTestNar) streamTestDigest source
        forceRemoveIfExists tmpDir
        pure $ case result of
          Left (Subst.TransientFailure _) -> Pass
          Left (Subst.FatalFailure err) -> Fail ("truncation classified fatal: " <> err)
          Right _ -> Fail "truncated NAR stream was accepted",
      -- A digest mismatch reports before the tree is trusted, and it
      -- is fatal: the size matched, so the transfer completed.
      runTestM "streaming unpack refuses a digest mismatch as fatal" $ do
        tmpBase <- getTemporaryDirectory
        let tmpDir = tmpBase </> "nova-nix-test-stream-digest"
        forceRemoveIfExists tmpDir
        createDirectoryIfMissing True tmpDir
        source <- chunkReader (streamChunks 7 streamTestNar)
        result <- Subst.consumeNarStream tmpSensitivity (tmpDir </> "out") (streamTestNarInfo streamTestNar) (CHash.hashBytes "not the nar") source
        forceRemoveIfExists tmpDir
        pure $ case result of
          Left (Subst.FatalFailure err) | "hash mismatch" `T.isInfixOf` err -> Pass
          Left other -> Fail ("expected fatal hash mismatch, got: " <> Subst.attemptFailureMessage other)
          Right _ -> Fail "digest mismatch was accepted",
      -- A narinfo lying about NarSize in either direction is refused
      -- at NarDone - the grammar completed, so the mismatch is the
      -- narinfo misdeclaring, and fatal.
      runTestM "streaming unpack refuses an overdeclared NarSize" $ do
        tmpBase <- getTemporaryDirectory
        let tmpDir = tmpBase </> "nova-nix-test-stream-oversize"
            lying = (streamTestNarInfo streamTestNar) {NarInfo.niNarSize = toInteger (BS.length streamTestNar) + 1}
        forceRemoveIfExists tmpDir
        createDirectoryIfMissing True tmpDir
        source <- chunkReader (streamChunks 7 streamTestNar)
        result <- Subst.consumeNarStream tmpSensitivity (tmpDir </> "out") lying streamTestDigest source
        forceRemoveIfExists tmpDir
        pure $ case result of
          Left (Subst.FatalFailure err) | "size mismatch" `T.isInfixOf` err -> Pass
          Left other -> Fail ("expected fatal size mismatch, got: " <> Subst.attemptFailureMessage other)
          Right _ -> Fail "overdeclared NarSize was accepted",
      runTestM "streaming unpack refuses an underdeclared NarSize" $ do
        tmpBase <- getTemporaryDirectory
        let tmpDir = tmpBase </> "nova-nix-test-stream-undersize"
            lying = (streamTestNarInfo streamTestNar) {NarInfo.niNarSize = toInteger (BS.length streamTestNar) - 1}
        forceRemoveIfExists tmpDir
        createDirectoryIfMissing True tmpDir
        source <- chunkReader (streamChunks 7 streamTestNar)
        result <- Subst.consumeNarStream tmpSensitivity (tmpDir </> "out") lying streamTestDigest source
        forceRemoveIfExists tmpDir
        pure $ case result of
          Left (Subst.FatalFailure err) | "size mismatch" `T.isInfixOf` err -> Pass
          Left other -> Fail ("expected fatal size mismatch, got: " <> Subst.attemptFailureMessage other)
          Right _ -> Fail "underdeclared NarSize was accepted",
      -- cappedBodySource: the streaming mirror of readBodyCapped -
      -- a body past the key-trusted declared size aborts mid-stream.
      runTestM "cappedBodySource aborts past the cap" $ do
        reader <- chunkReader ["abcdef", "ghijkl"]
        source <- Subst.cappedBodySource 10 reader
        firstChunk <- source
        overCap <- try source :: IO (Either SomeException BS.ByteString)
        pure $ case (firstChunk, overCap) of
          ("abcdef", Left _) -> Pass
          other -> Fail ("expected abort past the cap, got: " <> T.pack (show other)),
      -- withDecompressedSource: xz chunks decompress under the
      -- declared bound; the support decision mirrors decompressorFor.
      runTestM "withDecompressedSource xz chunked roundtrip" $
        case B64.decode xzFixtureB64 of
          Left err -> pure (Fail ("fixture base64 does not decode: " <> T.pack err))
          Right compressed -> do
            source <- chunkReader (streamChunks 7 compressed)
            result <- Subst.withDecompressedSource xzFixtureSize "xz" source $ \pull ->
              let go acc = do
                    chunk <- pull
                    if BS.null chunk
                      then pure (Right (BS.concat (reverse acc)))
                      else go (chunk : acc)
               in go []
            pure $ case result of
              Right out | out == xzFixturePayload -> Pass
              other -> Fail ("xz source roundtrip diverged: " <> T.pack (show (fmap BS.length other))),
      runTestM "withDecompressedSource zstd chunked roundtrip" $ do
        source <- chunkReader (streamChunks 7 zstdFixtureCompressed)
        result <- Subst.withDecompressedSource xzFixtureSize "zstd" source $ \pull ->
          let go acc = do
                chunk <- pull
                if BS.null chunk
                  then pure (Right (BS.concat (reverse acc)))
                  else go (chunk : acc)
           in go []
        pure $ case result of
          Right out | out == xzFixturePayload -> Pass
          other -> Fail ("zstd source roundtrip diverged: " <> T.pack (show (fmap BS.length other))),
      runTestM "withDecompressedSource xz single-byte chunks" $
        case B64.decode xzFixtureB64 of
          Left err -> pure (Fail ("fixture base64 does not decode: " <> T.pack err))
          Right compressed -> do
            source <- chunkReader (streamChunks 1 compressed)
            result <- Subst.withDecompressedSource xzFixtureSize "xz" source $ \pull ->
              let go acc = do
                    chunk <- pull
                    if BS.null chunk
                      then pure (Right (BS.concat (reverse acc)))
                      else go (chunk : acc)
               in go []
            pure $ case result of
              Right out | out == xzFixturePayload -> Pass
              other -> Fail ("xz 1-byte roundtrip diverged: " <> T.pack (show (fmap BS.length other))),
      -- The LIVE pipeline's defenses, not the strict oracle's: hostile
      -- compressed input through withDecompressedSource must land in
      -- the Left channel with the right retry class, never escape as
      -- an exception.
      runTestM "withDecompressedSource xz over-bound is fatal" $
        case B64.decode xzFixtureB64 of
          Left err -> pure (Fail ("fixture base64 does not decode: " <> T.pack err))
          Right compressed -> do
            source <- chunkReader (streamChunks 7 compressed)
            result <- Subst.withDecompressedSource (xzFixtureSize - 1) "xz" source drainChunkSource
            pure $ case result of
              Left (Subst.FatalFailure err) | "NarSize" `T.isInfixOf` err -> Pass
              other -> Fail ("expected fatal over-bound, got: " <> T.pack (show (void other))),
      runTestM "withDecompressedSource xz garbage is transient" $ do
        source <- chunkReader ["not an xz stream"]
        result <- Subst.withDecompressedSource 64 "xz" source drainChunkSource
        pure $ case result of
          Left (Subst.TransientFailure _) -> Pass
          other -> Fail ("expected transient stream error, got: " <> T.pack (show (void other))),
      runTestM "withDecompressedSource xz truncated is transient" $
        case B64.decode xzFixtureB64 of
          Left err -> pure (Fail ("fixture base64 does not decode: " <> T.pack err))
          Right compressed -> do
            source <- chunkReader (streamChunks 7 (BS.take 40 compressed))
            result <- Subst.withDecompressedSource xzFixtureSize "xz" source drainChunkSource
            pure $ case result of
              Left (Subst.TransientFailure _) -> Pass
              other -> Fail ("expected transient truncation, got: " <> T.pack (show (void other))),
      -- The failure contract of the post-download pipeline: every
      -- failing materialization removes the tree it wrote, and a
      -- clean one registers the verified metadata.
      runTestM "materialize registers a verified tree" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-materialize-ok"
        forceRemoveIfExists tmpStore
        createDirectoryIfMissing True tmpStore
        store <- openStore (StoreDir tmpStore)
        let sp = StorePath sampleHash "stream"
            destPath = storePathToFilePath (StoreDir tmpStore) sp
        source <- chunkReader (streamChunks 7 streamTestNar)
        result <- Subst.materializeNarFromSource store sp (streamTestNarInfo streamTestNar) streamTestDigest [] Nothing source
        survived <- doesDirectoryExist destPath
        closeStore store
        forceRemoveIfExists tmpStore
        pure $ case result of
          Left err -> Fail ("materialize failed: " <> Subst.attemptFailureMessage err)
          Right reg
            | not survived -> Fail "verified tree missing after materialize"
            | prNarSize reg /= BS.length streamTestNar -> Fail "registration NarSize diverges"
            | prNarHash reg /= CHash.formatNixHash streamTestDigest -> Fail "registration NarHash diverges"
            | otherwise -> Pass,
      runTestM "materialize removes the tree on a digest mismatch" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-materialize-digest"
        forceRemoveIfExists tmpStore
        createDirectoryIfMissing True tmpStore
        store <- openStore (StoreDir tmpStore)
        let sp = StorePath sampleHash "stream"
            destPath = storePathToFilePath (StoreDir tmpStore) sp
        source <- chunkReader (streamChunks 7 streamTestNar)
        result <- Subst.materializeNarFromSource store sp (streamTestNarInfo streamTestNar) (CHash.hashBytes "not the nar") [] Nothing source
        survived <- doesDirectoryExist destPath
        closeStore store
        forceRemoveIfExists tmpStore
        pure $ case result of
          Right _ -> Fail "digest mismatch was accepted"
          Left (Subst.TransientFailure err) -> Fail ("mismatch classified transient: " <> err)
          Left (Subst.FatalFailure err)
            | not ("hash mismatch" `T.isInfixOf` err) -> Fail ("unexpected failure: " <> err)
            | survived -> Fail "tree survived a digest mismatch"
            | otherwise -> Pass,
      runTestM "materialize removes the tree on a truncated stream" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-materialize-trunc"
        forceRemoveIfExists tmpStore
        createDirectoryIfMissing True tmpStore
        store <- openStore (StoreDir tmpStore)
        let sp = StorePath sampleHash "stream"
            destPath = storePathToFilePath (StoreDir tmpStore) sp
        source <- chunkReader (streamChunks 7 (BS.take (BS.length streamTestNar - 10) streamTestNar))
        result <- Subst.materializeNarFromSource store sp (streamTestNarInfo streamTestNar) streamTestDigest [] Nothing source
        survived <- doesDirectoryExist destPath
        closeStore store
        forceRemoveIfExists tmpStore
        pure $ case result of
          Right _ -> Fail "truncated stream was accepted"
          Left (Subst.FatalFailure err) -> Fail ("truncation classified fatal: " <> err)
          Left (Subst.TransientFailure _)
            | survived -> Fail "tree survived a truncated stream"
            | otherwise -> Pass,
      -- The download cap is the ceiling of the SIGNED NarSize, so the
      -- unsigned FileSize moves it in neither direction: one smaller
      -- than the body, as a narinfo records after its cache compressed
      -- the NAR again, and one inflated past the ceiling.
      runTest "downloadCapFor ignores FileSize" $
        let base = streamTestNarInfo streamTestNar
            narSize = NarInfo.niNarSize base
            ceilingCap = Subst.compressedBodyCeiling narSize
            capFor fileSize = Subst.downloadCapFor base {NarInfo.niFileSize = fileSize}
         in assertEqual
              "caps"
              (replicate 3 (Right (fromInteger ceilingCap)))
              (map capFor [Nothing, Just (narSize - 1), Just (ceilingCap * 1000)]),
      runTest "compressedBodyCeiling floors small and scales large" $
        let smallCeiling = Subst.compressedBodyCeiling 10
            largeSize = 64 * 1024 * 1024
            largeCeiling = Subst.compressedBodyCeiling largeSize
         in if smallCeiling == 10 + 64 * 1024 && largeCeiling == largeSize + largeSize `div` 64
              then Pass
              else Fail ("unexpected ceilings: " <> T.pack (show (smallCeiling, largeCeiling))),
      -- The zstd mirror of the xz hostile trio: over-bound output is
      -- deterministic, while a corrupt frame and a frame cut off
      -- mid-way both retry - the codec judges end of input by
      -- libzstd's own frame-boundary signal, so truncation refuses
      -- here instead of yielding a short output.
      runTestM "withDecompressedSource zstd over-bound is fatal" $ do
        source <- chunkReader (streamChunks 7 zstdFixtureCompressed)
        result <- Subst.withDecompressedSource (xzFixtureSize - 1) "zstd" source drainChunkSource
        pure $ case result of
          Left (Subst.FatalFailure err) | "NarSize" `T.isInfixOf` err -> Pass
          other -> Fail ("expected fatal over-bound, got: " <> T.pack (show (void other))),
      runTestM "withDecompressedSource zstd garbage is transient" $ do
        source <- chunkReader ["not a zstd frame"]
        result <- Subst.withDecompressedSource 64 "zstd" source drainChunkSource
        pure $ case result of
          Left (Subst.TransientFailure _) -> Pass
          other -> Fail ("expected transient stream error, got: " <> T.pack (show (void other))),
      runTestM "withDecompressedSource zstd truncated is transient" $ do
        source <- chunkReader (streamChunks 7 (BS.take (BS.length zstdFixtureCompressed - 8) zstdFixtureCompressed))
        result <- Subst.withDecompressedSource xzFixtureSize "zstd" source drainChunkSource
        pure $ case result of
          Left (Subst.TransientFailure _) -> Pass
          other -> Fail ("expected transient truncation, got: " <> T.pack (show (void other))),
      -- The codec refuses a truncated frame, and the whole pipeline
      -- must carry that refusal into the Left channel as retryable
      -- rather than let a short NAR reach the store.
      runTestM "zstd truncated body fails the pipeline as transient" $ do
        tmpBase <- getTemporaryDirectory
        let tmpDir = tmpBase </> "nova-nix-test-zstd-trunc"
            compressedNar = CZstd.compress CZstd.defaultCompressionLevel streamTestNar
        forceRemoveIfExists tmpDir
        createDirectoryIfMissing True tmpDir
        source <- chunkReader (streamChunks 7 (BS.take (BS.length compressedNar - 8) compressedNar))
        result <-
          Subst.withDecompressedSource (toInteger (BS.length streamTestNar)) "zstd" source $
            Subst.consumeNarStream tmpSensitivity (tmpDir </> "out") ((streamTestNarInfo streamTestNar) {NarInfo.niCompression = "zstd"}) streamTestDigest
        forceRemoveIfExists tmpDir
        pure $ case result of
          Left (Subst.TransientFailure _) -> Pass
          Left (Subst.FatalFailure err) -> Fail ("zstd truncation classified fatal: " <> err)
          Right _ -> Fail "truncated zstd body was accepted",
      -- The bzip2 mirror of the xz hostile trio, over a fixture the
      -- bzip2 CLI produced: over-bound output is deterministic, while
      -- garbage and a truncated stream retry.
      runTestM "withDecompressedSource bzip2 chunked roundtrip" $
        case B64.decode bzip2FixtureB64 of
          Left err -> pure (Fail ("fixture base64 does not decode: " <> T.pack err))
          Right compressed -> do
            source <- chunkReader (streamChunks 7 compressed)
            result <- Subst.withDecompressedSource xzFixtureSize "bzip2" source $ \pull ->
              let go acc = do
                    chunk <- pull
                    if BS.null chunk
                      then pure (Right (BS.concat (reverse acc)))
                      else go (chunk : acc)
               in go []
            pure $ case result of
              Right out | out == xzFixturePayload -> Pass
              other -> Fail ("bzip2 source roundtrip diverged: " <> T.pack (show (fmap BS.length other))),
      runTestM "withDecompressedSource bzip2 over-bound is fatal" $
        case B64.decode bzip2FixtureB64 of
          Left err -> pure (Fail ("fixture base64 does not decode: " <> T.pack err))
          Right compressed -> do
            source <- chunkReader (streamChunks 7 compressed)
            result <- Subst.withDecompressedSource (xzFixtureSize - 1) "bzip2" source drainChunkSource
            pure $ case result of
              Left (Subst.FatalFailure err) | "NarSize" `T.isInfixOf` err -> Pass
              other -> Fail ("expected fatal over-bound, got: " <> T.pack (show (void other))),
      runTestM "withDecompressedSource bzip2 garbage is transient" $ do
        source <- chunkReader ["not a bzip2 stream"]
        result <- Subst.withDecompressedSource 64 "bzip2" source drainChunkSource
        pure $ case result of
          Left (Subst.TransientFailure _) -> Pass
          other -> Fail ("expected transient stream error, got: " <> T.pack (show (void other))),
      runTestM "withDecompressedSource bzip2 truncated is transient" $
        case B64.decode bzip2FixtureB64 of
          Left err -> pure (Fail ("fixture base64 does not decode: " <> T.pack err))
          Right compressed -> do
            source <- chunkReader (streamChunks 7 (BS.take (BS.length compressed - 8) compressed))
            result <- Subst.withDecompressedSource xzFixtureSize "bzip2" source drainChunkSource
            pure $ case result of
              Left (Subst.TransientFailure _) -> Pass
              other -> Fail ("expected transient truncation, got: " <> T.pack (show (void other))),
      -- A bzip2-compressed NAR through the whole post-download
      -- pipeline: the format the historical caches serve decompresses,
      -- hashes, parses, and materializes in one bounded pass.
      runTestM "bzip2 NAR streams through the pipeline" $
        case B64.decode bzip2NarFixtureB64 of
          Left err -> pure (Fail ("fixture base64 does not decode: " <> T.pack err))
          Right compressed -> do
            tmpBase <- getTemporaryDirectory
            let tmpDir = tmpBase </> "nova-nix-test-bzip2-pipeline"
                destPath = tmpDir </> "out"
            forceRemoveIfExists tmpDir
            createDirectoryIfMissing True tmpDir
            source <- chunkReader (streamChunks 7 compressed)
            result <-
              Subst.withDecompressedSource (toInteger (BS.length bzip2NarFixture)) "bzip2" source $
                Subst.consumeNarStream
                  tmpSensitivity
                  destPath
                  ((streamTestNarInfo bzip2NarFixture) {NarInfo.niCompression = "bzip2"})
                  (CHash.hashBytes bzip2NarFixture)
            materialized <- Dir.doesFileExist (destPath </> "greeting")
            forceRemoveIfExists tmpDir
            pure $ case result of
              Left err -> Fail ("bzip2 pipeline failed: " <> Subst.attemptFailureMessage err)
              Right narBytes
                | narBytes /= BS.length bzip2NarFixture -> Fail "bzip2 pipeline counted the wrong NAR size"
                | not materialized -> Fail "bzip2 pipeline left no tree at the destination"
                | otherwise -> Pass,
      runTest "streaming compression support matches the strict set" $
        case (Subst.streamingDecompressionSupported "xz", Subst.streamingDecompressionSupported "zstd", Subst.streamingDecompressionSupported "bzip2", Subst.streamingDecompressionSupported "brotli") of
          (Right (), Right (), Right (), Left _) -> Pass
          other -> Fail ("unexpected support set: " <> T.pack (show other)),
      -- The support set is encoded twice - the strict decompressor and
      -- the streaming decision - so their agreement is pinned across
      -- the codecs either could plausibly grow, not left to comments.
      runTest "strict and streaming support sets agree" $
        let agrees compression =
              case (Subst.decompressorFor 1 compression, Subst.streamingDecompressionSupported compression) of
                (Right _, Right ()) -> True
                (Left _, Left _) -> True
                _ -> False
         in if all agrees ["none", "", "xz", "zstd", "bzip2", "brotli"]
              then Pass
              else Fail "strict and streaming compression support drifted",
      -- The strict path is the streaming path's differential oracle:
      -- the same NAR materialized by both must produce the same tree.
      runTestM "strict and streaming unpack agree" $ do
        tmpBase <- getTemporaryDirectory
        let tmpDir = tmpBase </> "nova-nix-test-stream-oracle"
        forceRemoveIfExists tmpDir
        createDirectoryIfMissing True tmpDir
        strictOutcome <- case NAR.deserialise streamTestNar of
          Left err -> pure (Left (T.pack err))
          Right entry -> unpackNarEntry tmpSensitivity (tmpDir </> "strict") entry
        source <- chunkReader (streamChunks 11 streamTestNar)
        streamOutcome <- Subst.consumeNarStream tmpSensitivity (tmpDir </> "streamed") (streamTestNarInfo streamTestNar) streamTestDigest source
        strictTree <- NAR.serialiseFromPath (tmpDir </> "strict")
        streamedTree <- NAR.serialiseFromPath (tmpDir </> "streamed")
        forceRemoveIfExists tmpDir
        pure $ case (strictOutcome, streamOutcome) of
          (Right (), Right _)
            | NAR.serialise strictTree == NAR.serialise streamedTree -> Pass
            | otherwise -> Fail "strict and streaming trees diverge"
          other -> Fail ("oracle setup failed: " <> T.pack (show other)),
      -- With no cache there is nothing to offer, so nothing is fetched.
      runTestM "a path asked of no cache is not offered, and nothing is fetched" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-subst"
        createDirectoryIfMissing True tmpStore
        store <- openStore (StoreDir tmpStore)
        result <- offeredThenFetched defaultFetchRetryPolicy ioRetryEffects store [] (StorePath "test" "hello")
        closeStore store
        removeDirectoryRecursive tmpStore
        pure (assertEqual "no-caches" Subst.SubstNotFound result),
      -- defaultCacheConfig has correct URL
      runTest "defaultCacheConfig url" $
        assertEqual "default-url" "https://cache.nixos.org" (Subst.ccUrl Subst.defaultCacheConfig),
      -- defaultCacheConfig has priority 40
      runTest "defaultCacheConfig priority" $
        assertEqual "default-prio" 40 (Subst.ccPriority Subst.defaultCacheConfig),
      -- verifyNarHash: matching NAR hash accepted (HIGH#2 integrity gate);
      -- the returned digest is the declared hash, decoded.
      runTest "verifyNarHash accepts matching hash" $
        case Subst.verifyNarHash (sampleNarInfo sampleNarHash) sampleNarBytes of
          Right declared
            | CHash.formatNixHash declared == sampleNarHash -> Pass
            | otherwise -> Fail "returned digest does not match the declared hash"
          Left err -> Fail ("expected acceptance, got: " <> err),
      -- verifyNarHash: mismatched NAR hash rejected
      runTest "verifyNarHash rejects mismatched hash" $
        case Subst.verifyNarHash (sampleNarInfo wrongNarHash) sampleNarBytes of
          Left _ -> Pass
          Right _ -> Fail "expected mismatch rejection",
      -- verifyNarHash: malformed NAR hash rejected
      runTest "verifyNarHash rejects malformed hash" $
        case Subst.verifyNarHash (sampleNarInfo "not-a-hash") sampleNarBytes of
          Left _ -> Pass
          Right _ -> Fail "expected malformed-hash rejection",
      -- verifyNar: the only constructor of VerifiedNar carries the bytes
      -- unchanged and the declared digest decoded, so the unpack sink
      -- and the registration read both from the witness.
      runTest "verifyNar carries the bytes and the declared digest" $
        case Subst.verifyNar (sampleNarInfo sampleNarHash) sampleNarBytes of
          Right verified
            | Subst.verifiedNarBytes verified /= sampleNarBytes -> Fail "verified bytes differ from the input"
            | CHash.formatNixHash (Subst.verifiedNarDigest verified) /= sampleNarHash -> Fail "carried digest is not the declared hash"
            | otherwise -> Pass
          Left err -> Fail ("expected acceptance, got: " <> err),
      -- verifyNar: either signed claim failing yields no witness at all.
      runTest "verifyNar rejects a hash or size mismatch" $
        let wrongHash = Subst.verifyNar (sampleNarInfo wrongNarHash) sampleNarBytes
            wrongSize = Subst.verifyNar ((sampleNarInfo sampleNarHash) {NarInfo.niNarSize = toInteger (BS.length sampleNarBytes) + 1}) sampleNarBytes
         in case (wrongHash, wrongSize) of
              (Left _, Left _) -> Pass
              other -> Fail ("expected both rejections, got: " <> T.pack (show other)),
      -- narInfoMatchesPath: identity match accepted
      runTest "narInfoMatchesPath accepts matching identity" $
        if Subst.narInfoMatchesPath (StorePath sampleHash "hello") (sampleNarInfo sampleNarHash)
          then Pass
          else Fail "expected identity match",
      -- narInfoMatchesPath: identity mismatch rejected
      runTest "narInfoMatchesPath rejects mismatched identity" $
        if Subst.narInfoMatchesPath (StorePath (T.replicate 32 "b") "hello") (sampleNarInfo sampleNarHash)
          then Fail "expected identity mismatch"
          else Pass,
      -- catchSync, which a fetch and a query wrap each attempt in: a
      -- synchronous exception becomes the handler's value, and so a failed
      -- substitute the realisation passes to the next cache.
      runTestM "catchSync folds a synchronous exception into the handler's value" $ do
        result <- Subst.catchSync (throwIO (ErrorCall "disk full")) (pure . Subst.SubstError . T.pack . show)
        pure $ case result of
          Subst.SubstError err
            | "disk full" `T.isInfixOf` err -> Pass
          other -> Fail ("expected SubstError from sync exception, got " <> T.pack (show other)),
      -- catchSync: an asynchronous exception passes through untouched -
      -- cancellation must abort substitution, never be spent as a failed
      -- substitute and from there a local build.
      runTestM "catchSync passes an asynchronous exception through" $ do
        outcome <- try (Subst.catchSync (throwIO (asyncExceptionToException (ErrorCall "cancelled"))) (pure . Subst.SubstError . T.pack . show))
        pure $ case (outcome :: Either SomeException Subst.SubstResult) of
          Left escaped -> case fromException escaped of
            Just (SomeAsyncException _) -> Pass
            Nothing -> Fail ("wrong exception escaped: " <> T.pack (show escaped))
          Right result -> Fail ("async exception folded into " <> T.pack (show result)),
      -- clearStaleDestination: removes a read-only leftover tree (the
      -- crash-between-unpack-and-register wedge)
      runTestM "clearStaleDestination removes read-only leftovers" $ do
        tmpBase <- getTemporaryDirectory
        let staleRoot = tmpBase </> "nova-nix-test-stale-dest"
            staleFile = staleRoot </> "bin" </> "tool"
        createDirectoryIfMissing True (staleRoot </> "bin")
        writeFile staleFile "leftover"
        perms <- getPermissions staleFile
        Dir.setPermissions staleFile (Dir.setOwnerWritable False perms)
        Subst.clearStaleDestination staleRoot
        gone <- Dir.doesPathExist staleRoot
        pure (if gone then Fail "stale tree survived" else Pass),
      -- clearStaleDestination: a missing destination is a no-op
      runTestM "clearStaleDestination missing path no-op" $ do
        tmpBase <- getTemporaryDirectory
        outcome <- try (Subst.clearStaleDestination (tmpBase </> "nova-nix-test-no-such-dir"))
        pure $ case (outcome :: Either SomeException ()) of
          Right () -> Pass
          Left e -> Fail ("threw: " <> T.pack (show e)),
      -- unpackNarEntry: what the archive grammar refuses (traversal
      -- shapes, a NUL) is refused on every host before anything is
      -- written
      runTestM "unpackNarEntry rejects unsafe entry names" $ do
        tmpBase <- getTemporaryDirectory
        let dest = tmpBase </> "nova-nix-test-unpack-unsafe"
            evil name = NAR.NarDirectory [(name, NAR.NarRegular False "x")]
            -- Cleared before each: the root is created exclusively, so
            -- a leftover from the previous name would refuse for a
            -- different reason than the one under test.
            unpackFresh name = do
              Subst.clearStaleDestination dest
              unpackNarEntry tmpSensitivity dest (evil name)
        results <- mapM unpackFresh ["..", ".", "", "a/b", "a\NULb"]
        Subst.clearStaleDestination dest
        pure $
          if all (\case Left err -> "NAR directory entry name" `T.isInfixOf` err; Right () -> False) results
            then Pass
            else Fail ("accepted an unsafe name: " <> T.pack (show results)),
      -- Names upstream's restore writes on POSIX as it finds them
      -- (posixOnlyTree, compared against nix-store --restore 2.33.2)
      -- materialize byte for byte through both unpack paths there, and
      -- the tree re-serialises to the NAR it came from.  Windows refuses
      -- the first such name before writing any entry.
      runTestM "names upstream restores on POSIX materialize as their bytes" $ do
        tmpBase <- getTemporaryDirectory
        let refusal = "NAR directory entry name 'COM1' is a Windows device name"
        outcomes <- unpackedBothWays tmpSensitivity (tmpBase </> "nova-nix-test-unpack-posix-names") posixOnlyTree
        pure $
          if SI.os == "mingw32"
            then assertEqual "Windows refusals (strict, streamed)" (Left refusal, Left refusal) outcomes
            else assertEqual "trees read back (strict, streamed)" (Right posixOnlyTree, Right posixOnlyTree) outcomes,
      -- The same tree substituted into a temp store: the strict pipeline
      -- unpacks and seals it, then re-serialises the store path from disk
      -- against the narinfo's hash before it registers anything, so a
      -- success means the path's bytes reproduce the NAR.  Windows
      -- refuses it before writing.
      runTestM "a store path holding names upstream restores on POSIX substitutes" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-unpack-posix-store"
            sp = StorePath (T.replicate 32 "c") "posix-names"
            rawNar = NAR.serialise posixOnlyTree
            info = (streamTestNarInfo rawNar) {NarInfo.niStorePath = storePathToText defaultStoreDir sp}
        forceRemoveIfExists tmpStore
        store <- openStore (StoreDir tmpStore)
        result <- Subst.unpackAndVerify store sp info rawNar
        onDisk <- case result of
          Subst.SubstSuccess _ lock -> do
            releasePathLock lock
            Right <$> NAR.serialiseFromPath (storePathToFilePath (stDir store) sp)
          other -> pure (Left (T.pack (show other)))
        closeStore store
        forceRemoveIfExists tmpStore
        pure $ case onDisk of
          Left shown
            | SI.os == "mingw32" && "is a Windows device name" `T.isInfixOf` shown -> Pass
          _ -> assertEqual "store path read back" (Right posixOnlyTree) onDisk,
      -- A name and a link target with no UTF-8 reading.  Linux holds
      -- both as bytes, as upstream restores them; a macOS volume cannot
      -- hold the name (APFS refuses it, HFS+ rewrites it) but keeps the
      -- target's bytes; Windows holds neither.
      runTestM "bytes with no UTF-8 reading materialize where the host holds them" $ do
        tmpBase <- getTemporaryDirectory
        let dest = tmpBase </> "nova-nix-test-unpack-raw-bytes"
            nameTree = NAR.NarDirectory [("caf\xE9", NAR.NarDirectory [("\xFF", NAR.NarRegular True "x")])]
            targetTree = NAR.NarDirectory [("link", NAR.NarSymlink "caf\xE9")]
            nameRefusal reason = "NAR directory entry name \"caf\\233\" is not valid UTF-8, " <> reason
            targetRefusal = "NAR symlink target \"caf\\233\" is not valid UTF-8, so it has no UTF-16 form for Windows"
            both outcome = (outcome, outcome)
        named <- unpackedBothWays tmpSensitivity dest nameTree
        targeted <- unpackedBothWays tmpSensitivity dest targetTree
        pure $ case SI.os of
          "mingw32" ->
            assertEqual
              "Windows refusals"
              (both (Left (nameRefusal "so it has no UTF-16 form for Windows")), both (Left targetRefusal))
              (named, targeted)
          "darwin" ->
            assertEqual
              "macOS refuses the name and keeps the target"
              (both (Left (nameRefusal "which a macOS filesystem cannot hold")), both (Right targetTree))
              (named, targeted)
          _ ->
            assertEqual
              "Linux keeps both"
              (both (Right nameTree), both (Right targetTree))
              (named, targeted),
      -- The bytes reach the filesystem without passing through the
      -- locale's codec: under an ASCII file-system encoding (a C locale)
      -- or a Latin-1 one a UTF-8 name still lands as its UTF-8 bytes.
      -- Spelled through 'FilePath', the first cannot encode the name and
      -- the second writes other bytes.
      runTestM "names land as their bytes whatever the file-system encoding" $ do
        tmpBase <- getTemporaryDirectory
        let dest = tmpBase </> "nova-nix-test-unpack-codec"
            tree =
              NAR.NarDirectory
                [ ("caf\xC3\xA9", NAR.NarRegular False "e"),
                  ("link", NAR.NarSymlink "\xE2\x9C\x93"),
                  ("\xE2\x9C\x93", NAR.NarRegular False "check")
                ]
            under encodingName = do
              encoding <- mkTextEncoding encodingName
              bracket getFileSystemEncoding setFileSystemEncoding $ \_ -> do
                setFileSystemEncoding encoding
                unpackedBothWays tmpSensitivity dest tree
        if SI.os == "mingw32"
          then Pass <$ putStrLn "  SKIP  a Windows path is UTF-16 and no locale codec spells it"
          else do
            outcomes <- mapM under ["ASCII//ROUNDTRIP", "ISO-8859-1//ROUNDTRIP"]
            pure (assertEqual "trees read back under ASCII and Latin-1 (strict, streamed)" (replicate 2 (Right tree, Right tree)) outcomes),
      -- unpackNarEntry: a NAR symlink materializes as a REAL link or fails
      -- loudly - never as a regular file holding the target text, which
      -- would silently diverge from the signed NAR hash
      runTestM "unpackNarEntry symlink is real or loud" $ do
        tmpBase <- getTemporaryDirectory
        let dest = tmpBase </> "nova-nix-test-unpack-symlink"
            tree =
              NAR.NarDirectory
                [ ("real", NAR.NarRegular False "contents"),
                  ("link", NAR.NarSymlink "real")
                ]
        Subst.clearStaleDestination dest
        result <- unpackNarEntry tmpSensitivity dest tree
        outcome <- case result of
          Right () -> do
            isLink <- Dir.pathIsSymbolicLink (dest </> "link")
            pure (if isLink then Pass else Fail "symlink materialized as a non-link")
          Left _ -> do
            leftBehind <- Dir.doesFileExist (dest </> "link")
            pure (if leftBehind then Fail "failed loudly but left a regular file behind" else Pass)
        Subst.clearStaleDestination dest
        pure outcome,
      -- unpackNarEntry: a directory-target symlink that sorts BEFORE its
      -- target still gets the directory link flavor (second-pass typing)
      runTestM "unpackNarEntry types forward dir symlink" $ do
        tmpBase <- getTemporaryDirectory
        let dest = tmpBase </> "nova-nix-test-unpack-dirlink"
            tree =
              NAR.NarDirectory
                [ ("alink", NAR.NarSymlink "zdir"),
                  ("zdir", NAR.NarDirectory [("f", NAR.NarRegular False "x")])
                ]
        Subst.clearStaleDestination dest
        result <- unpackNarEntry tmpSensitivity dest tree
        outcome <- case result of
          Left _ -> pure Pass -- symlinks unavailable here; the loud failure is the contract
          Right () -> do
            throughLink <- doesDirectoryExist (dest </> "alink")
            pure (if throughLink then Pass else Fail "dir symlink not traversable (wrong flavor)")
        Subst.clearStaleDestination dest
        pure outcome,
      -- Folding sibling names MATERIALIZE on every platform: true names
      -- via the NTFS per-directory flag on Windows and on any volume the
      -- probe reports sensitive (Linux, case-sensitive APFS), upstream's
      -- case-hack renaming on a folding volume.  The platform serialiser
      -- reverses whichever branch ran, so the tree re-serialises to its
      -- original NAR either way.
      runTestM "folding sibling names materialize and round-trip" $ do
        tmpBase <- getTemporaryDirectory
        let dest = tmpBase </> "nova-nix-test-unpack-fold"
            tree =
              NAR.NarDirectory
                [ ("Foo", NAR.NarRegular False "upper"),
                  ("foo", NAR.NarRegular False "lower")
                ]
        Subst.clearStaleDestination dest
        result <- unpackNarEntry tmpSensitivity dest tree
        outcome <- case result of
          Left err -> pure (Fail ("unpack failed: " <> err))
          Right () -> do
            onDisk <- NAR.serialiseFromPath dest
            if onDisk /= tree
              then pure (Fail "materialized tree does not re-serialise to its NAR")
              else
                if SI.os == "mingw32" || tmpSensitivity == CaseSensitive
                  then do
                    -- True names, not the hack: both spellings hold
                    -- distinct contents.
                    upper <- BS.readFile (dest </> "Foo")
                    lower <- BS.readFile (dest </> "foo")
                    pure (assertEqual "true names" ("upper", "lower") (upper, lower))
                  else do
                    -- The hack: the later variant carries the suffix.
                    lower <- BS.readFile (dest </> "foo~nix~case~hack~1")
                    pure (assertEqual "case-hacked name" "lower" lower)
        Subst.clearStaleDestination dest
        pure outcome,
      -- Every unpack creates exclusively, its root included, as
      -- upstream's restore does: a destination that already holds an
      -- entry is refused with upstream's wording and left as it was,
      -- not truncated or written into.  Callers clear a stale
      -- destination first.  Any volume answers this, so it is the
      -- exclusive create's check on every platform, Windows' CREATE_NEW
      -- included.
      runTestM "unpack refuses a destination that already exists" $ do
        tmpBase <- getTemporaryDirectory
        let dest = tmpBase </> "nova-nix-test-unpack-taken"
            fileTree = NAR.NarRegular False "new"
            dirTree = NAR.NarDirectory [("f", NAR.NarRegular False "new")]
            fileRefusal = "creating file '" <> quotedAsCxx dest <> "': File exists"
            dirOverFile = "creating directory '" <> T.pack dest <> "': File exists"
            dirOverDir = "path '" <> T.pack dest <> "' already exists"
            streamInto tree = do
              let raw = NAR.serialise tree
              source <- chunkReader [raw]
              outcome <- Subst.consumeNarStream tmpSensitivity dest (streamTestNarInfo raw) (CHash.hashBytes raw) source
              pure (either (Left . Subst.attemptFailureMessage) (const (Right ())) outcome)
            strictAndStreamed tree = (,) <$> unpackNarEntry tmpSensitivity dest tree <*> streamInto tree
        Subst.clearStaleDestination dest
        BS.writeFile dest "old"
        fileOnFile <- strictAndStreamed fileTree
        dirOnFile <- strictAndStreamed dirTree
        keptFile <- BS.readFile dest
        Subst.clearStaleDestination dest
        createDirectoryIfMissing True dest
        dirOnDir <- strictAndStreamed dirTree
        wroteInto <- Dir.doesPathExist (dest </> "f")
        Subst.clearStaleDestination dest
        let verdict
              | keptFile /= "old" = Fail "the existing file was written over"
              | wroteInto = Fail "the existing directory was written into"
              | otherwise =
                  assertEqual
                    "refusals, strict and streamed (a file over a file, a directory over a file, a directory over a directory)"
                    ((Left fileRefusal, Left fileRefusal), (Left dirOverFile, Left dirOverFile), (Left dirOverDir, Left dirOverDir))
                    (fileOnFile, dirOnFile, dirOnDir)
        pure verdict,
      -- std::quoted escapes a double quote and a backslash inside the
      -- path, so a name holding both pins upstream's exact text.  A
      -- Windows name can hold neither.
      runTestM "the file refusal quotes its path as std::quoted does" $ do
        tmpBase <- getTemporaryDirectory
        let plain = tmpBase </> "nova-nix-test-unpack-q"
            dest = plain <> "\"b\\s"
            expected = "creating file '\"" <> T.pack plain <> "\\\"b\\\\s\"': File exists"
            tree = NAR.NarRegular False "new"
            raw = NAR.serialise tree
        if SI.os == "mingw32"
          then Pass <$ putStrLn "  SKIP  a Windows name cannot hold a double quote"
          else do
            Subst.clearStaleDestination dest
            BS.writeFile dest "old"
            strict <- unpackNarEntry tmpSensitivity dest tree
            source <- chunkReader [raw]
            streamed <- Subst.consumeNarStream tmpSensitivity dest (streamTestNarInfo raw) (CHash.hashBytes raw) source
            Subst.clearStaleDestination dest
            pure (assertEqual "refusals (strict, streamed)" (Left expected, Left expected) (strict, either (Left . Subst.attemptFailureMessage) (const (Right ())) streamed)),
      -- The exclusive create reaches as far as base's own open on
      -- Windows, where a raw CreateFileW stops at MAX_PATH (260
      -- characters) unless the path is spelled in the file namespace.
      -- Every component here is short; only the whole path is long.
      runTestM "unpack writes a file whose path is past MAX_PATH" $ do
        tmpBase <- getTemporaryDirectory
        let dest = tmpBase </> "nova-nix-test-unpack-long"
            component = replicate 60 'd'
            depth = 5 :: Int
            nest inner = NAR.NarDirectory [(TE.encodeUtf8 (T.pack component), inner)]
            tree = foldr (const nest) (NAR.NarDirectory [("leaf", NAR.NarRegular False "deep")]) [1 .. depth]
            leafPath = foldl' (</>) dest (replicate depth component) </> "leaf"
            raw = NAR.serialise tree
            readLeaf = either (pure . Left) (const (Right <$> BS.readFile leafPath))
        Subst.clearStaleDestination dest
        strict <- unpackNarEntry tmpSensitivity dest tree >>= readLeaf
        Subst.clearStaleDestination dest
        source <- chunkReader [raw]
        streamed <-
          Subst.consumeNarStream tmpSensitivity dest (streamTestNarInfo raw) (CHash.hashBytes raw) source
            >>= readLeaf . either (Left . Subst.attemptFailureMessage) (const (Right ()))
        Subst.clearStaleDestination dest
        pure $
          if length leafPath <= 260
            then Fail "the fixture path is not past MAX_PATH"
            else assertEqual "leaf contents (strict, streamed)" (Right "deep", Right "deep") (strict, streamed),
      -- The pure case-hack naming is a function of the probed
      -- sensitivity, not of the platform: on a folding volume later
      -- case variants gain the reversible suffix with a per-name
      -- counter, on a sensitive one every name keeps its spelling.
      runTest "caseHackDiskNames renames later case variants on a folding volume" $
        assertEqual
          "hack naming"
          [("Foo", "Foo"), ("foo", "foo~nix~case~hack~1"), ("fOO", "fOO~nix~case~hack~2"), ("bar", "bar")]
          (caseHackDiskNames CaseInsensitive ["Foo", "foo", "fOO", "bar"]),
      runTest "caseHackDiskNames keeps every name on a sensitive volume" $
        assertEqual
          "identity naming"
          [("Foo", "Foo"), ("foo", "foo"), ("fOO", "fOO"), ("bar", "bar")]
          (caseHackDiskNames CaseSensitive ["Foo", "foo", "fOO", "bar"]),
      -- The key behind both: case variants share it exactly on a
      -- folding volume.
      runTest "onDiskNameKey folds case only on a folding volume" $
        if onDiskNameKey CaseSensitive "Makefile" /= onDiskNameKey CaseSensitive "makefile"
          && onDiskNameKey CaseInsensitive "Makefile" == onDiskNameKey CaseInsensitive "makefile"
          then Pass
          else Fail "the key does not follow the probed sensitivity",
      -- A name with no UTF-8 reading has no characters to fold, so it
      -- keys as its bytes even on a folding volume.
      runTest "onDiskNameKey keys a name with no UTF-8 reading as its bytes" $
        assertEqual
          "keys of a Latin-1 and a UTF-8 spelling"
          ("caf\xE9", "CAF\xC3\x89")
          (onDiskNameKey CaseInsensitive "caf\xE9", onDiskNameKey CaseInsensitive "caf\xC3\xA9"),
      -- The probe against the filesystem's own answer: two sibling
      -- names differing only by case are two entries on a sensitive
      -- volume and one on a folding one.
      runTestM "probeCaseSensitivity agrees with the filesystem" $ do
        tmpBase <- getTemporaryDirectory
        let dir = tmpBase </> "nova-nix-test-case-probe"
        Subst.clearStaleDestination dir
        createDirectoryIfMissing True dir
        BS.writeFile (dir </> "Probe") "upper"
        BS.writeFile (dir </> "probe") "lower"
        entries <- Dir.listDirectory dir
        probed <- probeCaseSensitivity dir
        Subst.clearStaleDestination dir
        let observed = if length entries == 2 then CaseSensitive else CaseInsensitive
        pure (assertEqual "probe against the filesystem" observed probed),
      -- Where the volume's serialiser strips the suffix (a folding
      -- volume), an incoming name carrying it must reject (it could
      -- not round-trip); on a volume that keeps names apart such a
      -- name is legitimate and materializes verbatim.
      runTestM "incoming case-hack suffix names reject where the serialiser strips" $ do
        tmpBase <- getTemporaryDirectory
        let dest = tmpBase </> "nova-nix-test-unpack-suffix"
            tree = NAR.NarDirectory [("x~nix~case~hack~1", NAR.NarRegular False "v")]
        Subst.clearStaleDestination dest
        result <- unpackNarEntry tmpSensitivity dest tree
        outcome <- case (tmpSensitivity, result) of
          (CaseInsensitive, Left _) -> pure Pass
          (CaseInsensitive, Right ()) -> pure (Fail "suffix-bearing name accepted where the serialiser strips")
          (CaseSensitive, Right ()) -> do
            kept <- BS.readFile (dest </> "x~nix~case~hack~1")
            pure (assertEqual "verbatim where names stay apart" "v" kept)
          (CaseSensitive, Left err) -> pure (Fail ("a volume keeping names apart rejected a legitimate name: " <> err))
        Subst.clearStaleDestination dest
        pure outcome,
      -- The capability probe itself: NTFS grants the per-directory flag
      -- (CI runners and the dev box run NTFS temp dirs); other
      -- platforms report unsupported.
      runTestM "trySetCaseSensitiveDir reflects platform support" $ do
        tmpBase <- getTemporaryDirectory
        let dir = tmpBase </> "nova-nix-test-csdir"
        Subst.clearStaleDestination dir
        createDirectoryIfMissing True dir
        flagged <- trySetCaseSensitiveDir dir
        Subst.clearStaleDestination dir
        pure (assertEqual "flag support" (SI.os == "mingw32") flagged),
      -- unpackAndVerify re-serialises the materialized tree and checks it
      -- against the declared hash before returning a registration: a
      -- faithful round-trip passes, and the registration carries the
      -- declared hash.
      runTestM "unpackAndVerify accepts a tree that reproduces its hash" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-unpack-verify"
        forceRemoveIfExists tmpStore
        store <- openStore (StoreDir tmpStore)
        let sp = StorePath (T.replicate 32 "b") "roundtrip"
            tree =
              NAR.NarDirectory
                [ ("data.txt", NAR.NarRegular False "verified bytes"),
                  ("sub", NAR.NarDirectory [("inner", NAR.NarRegular False "nested")])
                ]
            rawNar = NAR.serialise tree
            narHash = CHash.formatNixHash (CHash.hashBytes rawNar)
            info =
              NarInfo.NarInfo
                { NarInfo.niStorePath = storePathToText defaultStoreDir sp,
                  NarInfo.niUrl = "nar/roundtrip.nar",
                  NarInfo.niCompression = "none",
                  NarInfo.niFileHash = Nothing,
                  NarInfo.niFileSize = Nothing,
                  NarInfo.niNarHash = narHash,
                  NarInfo.niNarSize = fromIntegral (BS.length rawNar),
                  NarInfo.niReferences = [],
                  NarInfo.niDeriver = Nothing,
                  NarInfo.niSigs = [],
                  NarInfo.niCA = Nothing
                }
        result <- Subst.unpackAndVerify store sp info rawNar
        onDisk <- BS.readFile (storePathToFilePath (stDir store) sp </> "data.txt")
        -- A success carries the path lock still held; release it before
        -- the store teardown (Windows cannot delete a file a handle
        -- holds open).
        case result of
          Subst.SubstSuccess _ lock -> releasePathLock lock
          _ -> pure ()
        closeStore store
        forceRemoveIfExists tmpStore
        pure $ case result of
          Subst.SubstSuccess reg _ ->
            if prNarHash reg == narHash && onDisk == "verified bytes"
              then Pass
              else Fail "registration or on-disk bytes diverge from the declared hash"
          other -> Fail ("unpackAndVerify failed: " <> T.pack (show other)),
      runTestM "a substituted tree takes upstream's modes and the store's time" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-unpack-canonical"
        forceRemoveIfExists tmpStore
        store <- openStore (StoreDir tmpStore)
        let sp = StorePath (T.replicate 32 "t") "canonical"
            dest = storePathToFilePath (stDir store) sp
            rawNar =
              NAR.serialise
                ( NAR.NarDirectory
                    [ ("data", NAR.NarRegular False "data"),
                      ("run", NAR.NarRegular True "#!/bin/sh\n")
                    ]
                )
            info =
              NarInfo.NarInfo
                { NarInfo.niStorePath = storePathToText defaultStoreDir sp,
                  NarInfo.niUrl = "nar/canonical.nar",
                  NarInfo.niCompression = "none",
                  NarInfo.niFileHash = Nothing,
                  NarInfo.niFileSize = Nothing,
                  NarInfo.niNarHash = CHash.formatNixHash (CHash.hashBytes rawNar),
                  NarInfo.niNarSize = fromIntegral (BS.length rawNar),
                  NarInfo.niReferences = [],
                  NarInfo.niDeriver = Nothing,
                  NarInfo.niSigs = [],
                  NarInfo.niCA = Nothing
                }
        result <- Subst.unpackAndVerify store sp info rawNar
        outcome <- case result of
          Subst.SubstSuccess _ lock -> do
            releasePathLock lock
            foldr andThen Pass
              <$> traverse
                (uncurry canonicalAs)
                [(executablePermissions, dest), (readOnlyPermissions, dest </> "data"), (executablePermissions, dest </> "run")]
          other -> pure (Fail ("unpackAndVerify failed: " <> T.pack (show other)))
        closeStore store
        forceRemoveIfExists tmpStore
        pure outcome,
      -- The verify-fails branch of the strict pipeline: bytes that do
      -- not match the narinfo's signed NarHash or NarSize never reach
      -- the unpack sink, so nothing is written at the store path and
      -- the lock is free afterwards.
      runTestM "unpackAndVerify writes nothing for a NAR that fails verification" $ do
        tmpBase <- getTemporaryDirectory
        let tmpStore = tmpBase </> "nova-nix-test-unpack-unverified"
        forceRemoveIfExists tmpStore
        store <- openStore (StoreDir tmpStore)
        let sp = StorePath (T.replicate 32 "f") "unverified"
            rawNar = NAR.serialise (NAR.NarDirectory [("data.txt", NAR.NarRegular False "untrusted bytes")])
            consistent =
              NarInfo.NarInfo
                { NarInfo.niStorePath = storePathToText defaultStoreDir sp,
                  NarInfo.niUrl = "nar/unverified.nar",
                  NarInfo.niCompression = "none",
                  NarInfo.niFileHash = Nothing,
                  NarInfo.niFileSize = Nothing,
                  NarInfo.niNarHash = CHash.formatNixHash (CHash.hashBytes rawNar),
                  NarInfo.niNarSize = toInteger (BS.length rawNar),
                  NarInfo.niReferences = [],
                  NarInfo.niDeriver = Nothing,
                  NarInfo.niSigs = [],
                  NarInfo.niCA = Nothing
                }
            wrongHash = consistent {NarInfo.niNarHash = CHash.formatNixHash (CHash.hashBytes "not the nar")}
            wrongSize = consistent {NarInfo.niNarSize = toInteger (BS.length rawNar) + 1}
            destPath = storePathToFilePath (stDir store) sp
        hashResult <- Subst.unpackAndVerify store sp wrongHash rawNar
        sizeResult <- Subst.unpackAndVerify store sp wrongSize rawNar
        written <- Dir.doesPathExist destPath
        reLock <- tryAcquirePathLock (StoreDir tmpStore) sp
        mapM_ releasePathLock reLock
        closeStore store
        forceRemoveIfExists tmpStore
        pure $ case (hashResult, sizeResult, reLock) of
          (Subst.SubstError hashErr, Subst.SubstError sizeErr, Just _)
            | written -> Fail "an unverified NAR reached the store path"
            | not ("NAR hash mismatch" `T.isInfixOf` hashErr) -> Fail ("wrong hash error: " <> hashErr)
            | not ("NAR size mismatch" `T.isInfixOf` sizeErr) -> Fail ("wrong size error: " <> sizeErr)
            | otherwise -> Pass
          (_, _, Nothing) -> Fail "lock still held after a refused NAR"
          other -> Fail ("expected two SubstError results, got: " <> T.pack (show other))
    ]
  where
    -- A tree unpacked into a fresh destination through the strict and
    -- the streaming path, each read back from disk, or the unpack's
    -- refusal.  The read-back tree carries every name and link target
    -- as the bytes on disk, so equal trees serialise to the same NAR.
    unpackedBothWays sensitivity dest tree = do
      let raw = NAR.serialise tree
          streamed = do
            source <- chunkReader [raw]
            outcome <- Subst.consumeNarStream sensitivity dest (streamTestNarInfo raw) (CHash.hashBytes raw) source
            pure (either (Left . Subst.attemptFailureMessage) (const (Right ())) outcome)
          readBack unpack = do
            Subst.clearStaleDestination dest
            result <- unpack
            onDisk <- either (pure . Left) (const (Right <$> NAR.serialiseFromPath dest)) result
            Subst.clearStaleDestination dest
            pure onDisk
      strict <- readBack (unpackNarEntry sensitivity dest tree)
      viaStream <- readBack streamed
      pure (strict, viaStream)
    sampleNarBytes = "nova-nix nar sample bytes" :: BS.ByteString
    sampleNarHash = CHash.formatNixHash (CHash.hashBytes sampleNarBytes)
    wrongNarHash = CHash.formatNixHash (CHash.hashBytes ("different bytes" :: BS.ByteString))
    sampleHash = T.replicate 32 "a"
    sampleNarInfo narHash =
      NarInfo.NarInfo
        { NarInfo.niStorePath = "/nix/store/" <> sampleHash <> "-hello",
          NarInfo.niUrl = "nar/sample.nar",
          NarInfo.niCompression = "none",
          NarInfo.niFileHash = Just sampleNarHash,
          NarInfo.niFileSize = Just 0,
          NarInfo.niNarHash = narHash,
          NarInfo.niNarSize = fromIntegral (BS.length sampleNarBytes),
          NarInfo.niReferences = [],
          NarInfo.niDeriver = Nothing,
          NarInfo.niSigs = [],
          NarInfo.niCA = Nothing
        }
    -- A real xz stream: "nova-nix xz fixture payload\n" 64 times
    -- (1792 bytes) compressed with xz -9 to 112 bytes, embedded as
    -- base64 so the test source stays ASCII.
    xzFixturePayload = BS.concat (replicate 64 "nova-nix xz fixture payload\n")
    xzFixtureSize = toInteger (BS.length xzFixturePayload)
    xzFixtureB64 =
      "/Td6WFoAAATm1rRGAgAhARwAAAAQz1jM4Ab/AC1dADcbyzKSinKbUBue2bMHcUt1zJQMzAec5W6XwPbK3dKJtL3q9K3rfg1KQ/ZOAAAAAAAWUPRD8XwohgABSYAOAAAA2Eg7urHEZ/sCAAAAAARZWg==" :: Text
    -- cache.nixos.org narinfos recorded 2026-08-20 (nixos-25.05
    -- channel), verbatim: GNU hello (references including itself,
    -- deriver, production signature) and a doc output whose References
    -- line is empty.
    recordedHelloNarInfo =
      T.unlines
        [ "StorePath: /nix/store/jppkr0h8aap5z7m4xy4vg5yrwlly9h2v-hello-2.12.1",
          "URL: nar/1m1sbal63vqhlvbcxzdj8yr6fhhld7dsyhxbxwip54dgvg56rjnc.nar.xz",
          "Compression: xz",
          "FileHash: sha256:1m1sbal63vqhlvbcxzdj8yr6fhhld7dsyhxbxwip54dgvg56rjnc",
          "FileSize: 52056",
          "NarHash: sha256:0s8wi24d3bc4zxyxq3hffj102zi7cjp3wqnpj6nhqrwgjsgqff81",
          "NarSize: 249480",
          "References: dska0s3gxxd8azbsn85pwm6xcqhivldw-glibc-2.40-66 jppkr0h8aap5z7m4xy4vg5yrwlly9h2v-hello-2.12.1",
          "Deriver: a9y211d3lq3yf2mpfvb72f0qvsl0wi3s-hello-2.12.1.drv",
          "Sig: cache.nixos.org-1:xV9xUvtPGHTSo6eeFWYYhaNFiH8MYSXKZzmxe0cf+acdDkIYxFPLC/MNc1CjlbsVoaUwGhcigUdr9eW2+W1UAw=="
        ]
    recordedHelloDocNarInfo =
      T.unlines
        [ "StorePath: /nix/store/4xrqj3kcd89bsg5crbpnx1ppg7nxw9xp-hello-1.0.0.2-doc",
          "URL: nar/1r86zzj3g8jhzyvi17vchsj3frrqk5l329viq2i24iish53x922l.nar.xz",
          "Compression: xz",
          "FileHash: sha256:1r86zzj3g8jhzyvi17vchsj3frrqk5l329viq2i24iish53x922l",
          "FileSize: 892",
          "NarHash: sha256:0amizmj17fgj5sz1gvr743kncrzwvq747lml3zhd374f14hzgd7a",
          "NarSize: 2072",
          "References: ",
          "Deriver: 5knzybx32a3iq53jjw6xd6fn4awn2l3h-hello-1.0.0.2.drv",
          "Sig: cache.nixos.org-1:jUFHex7R7zPonZZqjVy/OOTguUZZE/sityC0zvu3sb35Az8zBTROeFhUC/J/UFx8Ui8UflA7uHyLJm5Z+Ca6Bw=="
        ]
    -- A NAR with the shapes streaming unpack must materialize:
    -- nesting, an executable, a symlink, a zero-byte file, an empty
    -- directory, and a sibling pair that collides on folding
    -- filesystems.  Entries in NAR name order.
    -- Both survive a round trip through disk on every platform.  The
    -- executable bit does on Windows because the store keeps it in an
    -- alternate data stream; the nested symlink target does because
    -- nova-cache 0.11.1.1 normalises a Windows reparse point's separators
    -- back to the POSIX spelling at the NAR boundary.
    streamTestLinkTarget = "bin/tool"
    streamTestNar =
      NAR.serialise
        ( NAR.NarDirectory
            [ ("Makefile", NAR.NarRegular False "all:\n"),
              ("bin", NAR.NarDirectory [("tool", NAR.NarRegular True "#!/bin/sh\n")]),
              ("empty", NAR.NarRegular False ""),
              ("emptydir", NAR.NarDirectory []),
              ("link", NAR.NarSymlink streamTestLinkTarget),
              ("makefile", NAR.NarRegular False "lower\n")
            ]
        )
    streamTestDigest = CHash.hashBytes streamTestNar
    streamTestNarInfo rawNar =
      NarInfo.NarInfo
        { NarInfo.niStorePath = "/nix/store/" <> sampleHash <> "-stream",
          NarInfo.niUrl = "nar/stream.nar",
          NarInfo.niCompression = "none",
          NarInfo.niFileHash = Nothing,
          NarInfo.niFileSize = Nothing,
          NarInfo.niNarHash = CHash.formatNixHash (CHash.hashBytes rawNar),
          NarInfo.niNarSize = fromIntegral (BS.length rawNar),
          NarInfo.niReferences = [],
          NarInfo.niDeriver = Nothing,
          NarInfo.niSigs = [],
          NarInfo.niCA = Nothing
        }
    streamChunks chunkLen bytes
      | BS.null bytes = []
      | otherwise = BS.take chunkLen bytes : streamChunks chunkLen (BS.drop chunkLen bytes)
    -- The xz fixture's payload, compressed by nova-cache:zstandard's
    -- own encoder - the exact pairing the push path ships.
    zstdFixtureCompressed = CZstd.compress CZstd.defaultCompressionLevel xzFixturePayload
    -- The same payload compressed by the bzip2 CLI (bzip2 -9),
    -- embedded base64 so the test source stays ASCII.  The reference
    -- encoder is deliberately the format's own tool and not the
    -- decoder's library: nova-cache:bzip2 decodes only, so a
    -- self-produced fixture could not catch the two disagreeing.
    bzip2FixtureB64 =
      "QlpoOTFBWSZTWTop7coAAf/RgAAQQAInJddwIACQKZMTIMjAqqBkDaJtTgCQKgZA5AyBAFwNwMAeAYAgCQLgXA+AoBUCQIAkCAJAUAgCgH4u5IpwoSB0U9uU" :: Text
    -- A NAR every platform materializes identically (no executable
    -- bit, no separator-bearing symlink target), and the bzip2 CLI's
    -- compression of exactly these bytes.
    bzip2NarFixture =
      NAR.serialise
        (NAR.NarDirectory [("greeting", NAR.NarRegular False "nova-nix bzip2 fixture\n")])
    bzip2NarFixtureB64 =
      "QlpoOTFBWSZTWa3VDM4AAFJ5gG7yAIBAYjAAP+ffcCAAlIaTJM1PUnqD1DJpoHo01BkkA9I0AAAAtARwcfWQAq1WLMjpmBpVKUAJM6EA8IDQEABOzcRgdDpvigJ6ZZa760fpxZtj+ts6GDiuqt/B6UuthWIrabWmZTkFlI46C8MmbMoVmMYwEYRUCYQW/F3JFOFCQrdUMzg=" :: Text

-- ---------------------------------------------------------------------------
-- Tests: Push (pure narinfo construction + closure computation)
-- ---------------------------------------------------------------------------

-- | A substituted tree is held to the content address its narinfo
-- states, as upstream's store holds a path it adds: hashed as the
-- address's method reads it, its NAR or the bytes of the one file a flat
-- or text address names, modulo the path's hash part, and refused in
-- upstream's words when that differs.
testStatedContentAddress :: IO [Bool]
testStatedContentAddress = do
  putStrLn "substituter/stated-content-address"
  tmpBase <- getTemporaryDirectory
  let root = tmpBase </> "nova-nix-test-stated-ca"
      dir = StoreDir root
      hashPart = T.replicate 32 "c"
      sp = StorePath hashPart "addressed"
      dest = storePathToFilePath dir sp
      digestOf algo bytes = fromMaybe BS.empty (rawHashWithAlgo algo bytes)
      spelled prefix algo digest = prefix <> algo <> ":" <> Nix32.encode digest
      directory = NAR.NarDirectory [("f", NAR.NarRegular False "contents")]
      holdingHashPart = "before " <> TE.encodeUtf8 hashPart <> " after"
      -- Each tree checked against each address, the tree written fresh.
      checked tree address = do
        forceRemoveIfExists root
        createDirectoryIfMissing True root
        sensitivity <- probeCaseSensitivity root
        written <- unpackNarEntry sensitivity dest tree
        result <- either (pure . Left . ("unpack: " <>)) (const (Subst.checkStatedAddress (volumeCaseHack sensitivity) dir sp dest address)) written
        forceRemoveIfExists root
        pure result
      holds label tree address = runTestM label $ assertEqual label (Right ()) <$> checked tree (Just address)
      mismatch label tree address = runTestM label $ do
        result <- checked tree (Just address)
        pure $ case result of
          Left err
            | ("ca hash mismatch importing path '" <> storePathToText dir sp <> "';\n  specified: ") `T.isPrefixOf` err -> Pass
          other -> Fail ("expected upstream's mismatch, got: " <> T.pack (show other))
      chunkings :: [Int] -> BS.ByteString -> [BS.ByteString]
      chunkings sizes bytes = case sizes of
        _ | BS.null bytes -> []
        size : rest -> BS.take size bytes : chunkings (rest <> [size]) (BS.drop size bytes)
        [] -> [bytes]
      moduloDigest modulus chunks = (\start -> moduloHashFinalize (foldl' moduloHashUpdate start chunks)) <$> moduloHashInit "sha256" modulus
  sequence
    [ runTestM "a narinfo with no content address holds the tree to its NarHash alone" $
        assertEqual "none" (Right ()) <$> checked directory Nothing,
      holds "a tree whose NAR has the recursive address stated is held" directory (spelled "fixed:r:" "sha256" (digestOf "sha256" (NAR.serialise directory))),
      holds "an address in another algorithm is read in that one" directory (spelled "fixed:r:" "sha1" (digestOf "sha1" (NAR.serialise directory))),
      mismatch "a tree whose NAR has another digest is refused in upstream's words" directory (spelled "fixed:r:" "sha256" (digestOf "sha256" "something else")),
      holds "a flat address is of the file's bytes" (NAR.NarRegular False "contents") (spelled "fixed:" "sha256" (digestOf "sha256" "contents")),
      holds "a text address is of the file's bytes" (NAR.NarRegular False "contents") (spelled "text:" "sha256" (digestOf "sha256" "contents")),
      runTestM "a flat address names a regular file, and a directory is refused" $ do
        result <- checked directory (Just (spelled "fixed:" "sha256" (digestOf "sha256" "contents")))
        pure (either (const Pass) (const (Fail "a directory was held to a flat address")) result),
      -- Each occurrence of the hash part is hashed as zeros, and its
      -- offset follows the bytes as |OFFSET.
      holds
        "the tree is hashed modulo the path's hash part"
        (NAR.NarRegular False holdingHashPart)
        (spelled "fixed:" "sha256" (digestOf "sha256" ("before " <> BS.replicate 32 0 <> " after" <> "|7"))),
      mismatch "the hash part is not hashed as itself" (NAR.NarRegular False holdingHashPart) (spelled "fixed:" "sha256" (digestOf "sha256" holdingHashPart)),
      runTest "a stream without the hash part hashes as it is" $
        assertEqual "plain" (Just (digestOf "sha256" "abc def")) (moduloDigest "xyz" ["abc ", "def"]),
      runTestM "a digest modulo a hash part is the same however the stream is split" $ do
        result <-
          QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} $
            QC.forAll ((,) <$> QC.listOf (QC.elements "abc") <*> QC.listOf1 (QC.choose (1, 5))) $ \(text, sizes) ->
              let bytes = TE.encodeUtf8 (T.pack text)
               in moduloDigest "abc" (chunkings sizes bytes) QC.=== moduloDigest "abc" [bytes]
        pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))
    ]

-- | The case sensitivity of the suite's temp directory, so a test
-- materializing a NAR there keys sibling names the way a store on that
-- volume would.
tempCaseSensitivity :: IO CaseSensitivity
tempCaseSensitivity = getTemporaryDirectory >>= probeCaseSensitivity

-- ---------------------------------------------------------------------------
-- Tests: NAR and hash known-answer vectors
-- ---------------------------------------------------------------------------

-- | An offer with the URL of the cache whose substitute it carries, the
-- part of a substitute a test can compare.
offerCache :: Realise.Offer Subst.Substitute -> Realise.Offer Text
offerCache offer = case offer of
  Realise.Offered substitute references -> Realise.Offered (Subst.ccUrl (Subst.substituteCache substitute)) references
  Realise.NotOffered -> Realise.NotOffered
  Realise.QueryFailed failure -> Realise.QueryFailed failure

-- | The substituter's narinfo and NAR requests under the transfer
-- layer's retry policy, against a loopback cache.  The policy's effects
-- are recorded rather than slept, so each case states the requests the
-- cache saw and the backoff the policy chose.
testSubstituterRetry :: IO [Bool]
testSubstituterRetry = do
  putStrLn "substituter/retry"
  case signedNarInfo of
    Left err -> (: []) <$> runTest "substituter retry setup" (Fail (T.pack err))
    Right (publicKey, narInfoText) ->
      let narInfoOk = Send (httpResponse "200 OK" (TE.encodeUtf8 narInfoText))
          narOk = Send (httpResponse "200 OK" nar)
          unavailable = Send (httpResponse "503 Service Unavailable" "")
          unsignedNarInfo = Send (httpResponse "200 OK" (TE.encodeUtf8 (NarInfo.renderNarInfo unsigned)))
          trustingCache base = Subst.CacheConfig base [publicKey]
          forbidding = Map.singleton "allowSubstitutes" ""
          -- Realise a derivation whose one output is the fixture's path,
          -- from the caches given, under the adjusted config, in a store
          -- kept under the temporary directory rather than at /nix/store.
          -- Returns the result and whether the output is the substitute's
          -- tree or the builder's file.
          realiseFrom label = realiseWithEnv label Map.empty
          -- 'realiseFrom' for a derivation with more in its environment.
          realiseWithEnv label extraEnv caches adjust = do
            tmpBase <- getTemporaryDirectory
            let root = tmpBase </> ("nova-nix-test-subst-fallback-" ++ label)
                storeDir = StoreDir (root </> "store")
                drvSP = StorePath (T.replicate 32 "d") "retry.drv"
                (builder, builderArgs)
                  | SI.os == "mingw32" = ("cmd.exe", ["/c", "echo built>%out%"])
                  | otherwise = ("/bin/sh", ["-c", "echo built > $out"])
                drv =
                  Derivation
                    { drvOutputs = [DerivationOutput "out" sp "" ""],
                      drvInputDrvs = Map.empty,
                      drvInputSrcs = [],
                      drvPlatform = currentPlatform,
                      drvBuilder = builder,
                      drvArgs = builderArgs,
                      drvEnv = Map.insert "name" "retry" extraEnv
                    }
                config =
                  adjust
                    (defaultBuildConfig storeDir)
                      { bcCaches = caches,
                        bcFetchRetry = defaultFetchRetryPolicy {frpBaseDelayMs = 1}
                      }
                outPath = storePathToFilePath storeDir sp
            bracket_ (forceRemoveIfExists root) (forceRemoveIfExists root) $
              bracket (openStore storeDir) closeStore $ \store -> do
                writeDrv store drv drvSP
                result <- buildWithDeps config store drv drvSP
                substituted <- Dir.doesFileExist (outPath </> "data")
                built <- Dir.doesFileExist outPath
                pure (result, substituted, built)
          substitute label narInfoReply narReply =
            withHttpServer (route narInfoReply narReply) $ \base requests _ -> do
              tmpBase <- getTemporaryDirectory
              sleeps <- newIORef []
              warnings <- newIORef []
              let root = tmpBase </> ("nova-nix-test-subst-retry-" ++ label)
                  cache = Subst.CacheConfig base [publicKey] 40
                  effects =
                    RetryEffects
                      { reSleepMs = \ms -> atomicModifyIORef' sleeps (\seen -> (seen ++ [ms], ())),
                        reJitter = pure 0,
                        reWarn = \msg -> atomicModifyIORef' warnings (\seen -> (seen ++ [msg], ()))
                      }
              bracket_ (forceRemoveIfExists root) (forceRemoveIfExists root) $
                bracket (openStore (StoreDir root)) closeStore $ \store -> do
                  result <- offeredThenFetched defaultFetchRetryPolicy effects store [cache] sp
                  outcome <- case result of
                    Subst.SubstSuccess _ lock -> do
                      releasePathLock lock
                      Right <$> BS.readFile (storePathToFilePath (stDir store) sp </> "data")
                    other -> pure (Left other)
                  (,,,,) base outcome <$> requests <*> readIORef sleeps <*> readIORef warnings
       in sequence $
            [ -- The order two processes meet the locks in decides whether they
              -- can deadlock, and each path's NAR is requested under its lock,
              -- so the requests show the order without a race to observe.
              runTestM "a substitution takes its output locks in the order a build does" $ do
                let low = StorePath (T.replicate 32 "a") "lock-order-lib"
                    high = StorePath (T.replicate 32 "z") "lock-order-bin"
                case servedPaths [(high, []), (low, [])] of
                  Left err -> pure (Fail (T.pack err))
                  Right (key, serve) ->
                    withHttpServer (\target _ -> serve target) $ \base requests _ -> do
                      tmpBase <- getTemporaryDirectory
                      let root = tmpBase </> "nova-nix-test-substitute-lock-order"
                          caches = [Subst.CacheConfig base [key] 40]
                          config = (defaultBuildConfig (StoreDir root)) {bcCaches = caches}
                      bracket_ (forceRemoveIfExists root) (forceRemoveIfExists root) $
                        bracket (openStore (StoreDir root)) closeStore $ \store -> do
                          offers <- evalStateT (traverse (Subst.queryReferencesWith defaultFetchRetryPolicy quietRetry Realise.NoFallback caches) (high :| [low])) Subst.emptyNarInfoMemo
                          case traverse offeredSubstitute offers of
                            Nothing -> pure (Fail ("not offered: " <> T.pack (show (fmap offerCache offers))))
                            Just batch -> do
                              _ <- substitutePaths config store Realise.RegisterAll batch
                              assertEqual "NAR requests" (map narRequest [low, high]) . filter (`elem` map narRequest [low, high]) <$> requests,
              runTestM "a reference query believes a signed narinfo for the path" $
                withHttpServer (route (const narInfoOk) (const narOk)) $ \base _ _ ->
                  assertEqual "references" (Realise.Offered base [])
                    <$> queryOffer Realise.NoFallback [trustingCache base 40] sp,
              runTestM "a reference query passes over a narinfo no trusted key signed" $
                withHttpServer (route (const unsignedNarInfo) (const narOk)) $ \base _ _ ->
                  assertEqual "references" Realise.NotOffered
                    <$> queryOffer Realise.NoFallback [trustingCache base 40] sp,
              runTestM "a reference query passes over a narinfo for another path" $
                withHttpServer (\_ _ -> narInfoOk) $ \base _ _ ->
                  assertEqual "references" Realise.NotOffered
                    <$> queryOffer Realise.NoFallback [trustingCache base 40] (StorePath (T.replicate 32 "c") "retry"),
              -- Upstream rethrows a failed narinfo query unless fallback is
              -- set, so a later cache holding the path is never asked.
              runTestM "a reference query stops at a failing cache" $
                withHttpServer (route (const unavailable) (const narOk)) $ \failing _ _ ->
                  withHttpServer (route (const narInfoOk) (const narOk)) $ \working workingRequests _ -> do
                    offer <- queryOffer Realise.NoFallback [trustingCache failing 10, trustingCache working 40] sp
                    seen <- workingRequests
                    pure (assertEqual "stopped" (Realise.QueryFailed (failing <> ": retry: HTTP 503 fetching " <> failing <> "/" <> hash <> ".narinfo"), []) (offer, seen)),
              runTestM "a reference query stops at the first cache that offers the path" $
                withHttpServer (route (const narInfoOk) (const narOk)) $ \first _ _ ->
                  withHttpServer (route (const narInfoOk) (const narOk)) $ \later laterRequests _ -> do
                    offer <- queryOffer Realise.NoFallback [trustingCache first 10, trustingCache later 40] sp
                    seen <- laterRequests
                    pure (assertEqual "offered by the first" (Realise.Offered first [], []) (offer, seen)),
              runTestM "a reference query that no cache answers offers nothing, asking each cache once" $
                withHttpServer (\_ _ -> Send (httpResponse "404 Not Found" "")) $ \first firstRequests _ ->
                  withHttpServer (\_ _ -> Send (httpResponse "404 Not Found" "")) $ \later laterRequests _ -> do
                    offer <- queryOffer Realise.NoFallback [trustingCache first 10, trustingCache later 40] sp
                    seen <- (,) <$> firstRequests <*> laterRequests
                    pure (assertEqual "not offered" (Realise.NotOffered, ([narInfoPath], [narInfoPath])) (offer, seen)),
              runTestM "under fallback a reference query falls through a failing cache to the next" $
                withHttpServer (route (const unavailable) (const narOk)) $ \failing _ _ ->
                  withHttpServer (route (const narInfoOk) (const narOk)) $ \working _ _ ->
                    assertEqual "references" (Realise.Offered working [])
                      <$> queryOffer Realise.BuildFromSource [trustingCache failing 10, trustingCache working 40] sp,
              -- The whole realisation, against upstream's outcomes: a cache
              -- that fails stops the build unless fallback is on, and one
              -- that lacks the path or offers an untrusted narinfo does not.
              -- The .drv is named under the logical store directory, as
              -- upstream's printStorePath names it, though the store's files
              -- are kept elsewhere.
              runTestM "a NAR that fails after its retries stops the build and names --fallback" $
                withHttpServer (route (const narInfoOk) (const unavailable)) $ \base _ _ -> do
                  (result, substituted, built) <- realiseFrom "nar-down" [trustingCache base 40] id
                  pure
                    ( assertEqual
                        "stopped"
                        ( BuildFailure
                            ( "some substitutes for the outputs of derivation '/nix/store/"
                                <> T.replicate 32 "d"
                                <> "-retry.drv' failed (usually happens due to networking issues); try '--fallback' to build derivation from source"
                            )
                            1,
                          False,
                          False
                        )
                        (result, substituted, built)
                    ),
              runTestM "under fallback a NAR that fails builds from source instead" $
                withHttpServer (route (const narInfoOk) (const unavailable)) $ \base _ _ -> do
                  (result, substituted, built) <- realiseFrom "nar-down-fallback" [trustingCache base 40] (\config -> config {bcFallback = Realise.BuildFromSource})
                  pure (assertEqual "built" (BuildSuccess sp, False, True) (result, substituted, built)),
              -- 'bcSubstitutes' reaches the decision: the cache serves the
              -- output whole, and is asked for nothing.
              runTestM "a derivation that forbids substitutes builds beside a cache that serves its output" $
                withHttpServer (route (const narInfoOk) (const narOk)) $ \base requests _ -> do
                  (result, substituted, built) <- realiseWithEnv "forbidden" forbidding [trustingCache base 40] id
                  seen <- requests
                  pure (assertEqual "built" (BuildSuccess sp, False, True, []) (result, substituted, built, seen)),
              runTestM "under always-allow-substitutes the same derivation is substituted" $
                withHttpServer (route (const narInfoOk) (const narOk)) $ \base _ _ -> do
                  (result, substituted, built) <-
                    realiseWithEnv "forbidden-always" forbidding [trustingCache base 40] (\config -> config {bcSubstitutes = Realise.AlwaysAllowSubstitutes})
                  pure (assertEqual "substituted" (BuildSuccess sp, True, False) (result, substituted, built)),
              runTestM "a narinfo the cache lacks builds, with no flag" $
                withHttpServer (\_ _ -> Send (httpResponse "404 Not Found" "")) $ \base requests _ -> do
                  (result, substituted, built) <- realiseFrom "narinfo-404" [trustingCache base 40] id
                  seen <- requests
                  pure (assertEqual "built" (BuildSuccess sp, False, True, [narInfoPath]) (result, substituted, built, seen)),
              runTestM "a narinfo that fails after its retries stops the build with the failure" $
                withHttpServer (route (const unavailable) (const narOk)) $ \base requests _ -> do
                  (result, substituted, built) <- realiseFrom "narinfo-down" [trustingCache base 40] id
                  seen <- requests
                  pure
                    ( assertEqual
                        "stopped"
                        (BuildFailure (base <> ": retry: HTTP 503 fetching " <> base <> "/" <> hash <> ".narinfo") 1, False, False, replicate 5 narInfoPath)
                        (result, substituted, built, seen)
                    ),
              runTestM "under fallback a narinfo that fails builds from source instead" $
                withHttpServer (route (const unavailable) (const narOk)) $ \base _ _ -> do
                  (result, substituted, built) <- realiseFrom "narinfo-down-fallback" [trustingCache base 40] (\config -> config {bcFallback = Realise.BuildFromSource})
                  pure (assertEqual "built" (BuildSuccess sp, False, True) (result, substituted, built)),
              runTestM "an unsigned narinfo is passed over for the next cache" $
                withHttpServer (route (const unsignedNarInfo) (const narOk)) $ \untrusted untrustedRequests _ ->
                  withHttpServer (route (const narInfoOk) (const narOk)) $ \trusted _ _ -> do
                    (result, substituted, built) <- realiseFrom "unsigned-then-signed" [trustingCache untrusted 10, trustingCache trusted 40] id
                    seen <- untrustedRequests
                    pure (assertEqual "substituted" (BuildSuccess sp, True, False, False) (result, substituted, built, narPath `elem` seen)),
              runTestM "an unsigned narinfo alone builds, with no flag" $
                withHttpServer (route (const unsignedNarInfo) (const narOk)) $ \base requests _ -> do
                  (result, substituted, built) <- realiseFrom "unsigned" [trustingCache base 40] id
                  seen <- requests
                  pure (assertEqual "built" (BuildSuccess sp, False, True, False) (result, substituted, built, narPath `elem` seen)),
              runTestM "a narinfo that fails once with a 503 is fetched on the retry" $ do
                (base, outcome, seen, sleeps, warnings) <-
                  substitute "narinfo-503" (\earlier -> if earlier == 0 then unavailable else narInfoOk) (const narOk)
                pure
                  ( assertEqual
                      "substituted after one retry"
                      (Right payload, [narInfoPath, narInfoPath, narPath], [250], ["HTTP 503 fetching " <> base <> "/" <> hash <> ".narinfo; retrying in 250 ms"])
                      (outcome, seen, sleeps, warnings)
                  ),
              runTestM "a narinfo that keeps failing spends upstream's five attempts and doubling delay" $ do
                (_, outcome, seen, sleeps, _) <- substitute "narinfo-down" (const unavailable) (const narOk)
                pure $ case outcome of
                  Left (Subst.SubstError _) -> assertEqual "attempts" (replicate 5 narInfoPath, [250, 500, 1000, 2000]) (seen, sleeps)
                  other -> Fail ("expected a substitution error, got: " <> T.pack (show other)),
              runTestM "a NAR download answering 501 stops after one request and names the cache" $ do
                (base, outcome, seen, sleeps, _) <-
                  substitute "nar-501" (const narInfoOk) (const (Send (httpResponse "501 Not Implemented" "")))
                pure $ case outcome of
                  Left (Subst.SubstError err)
                    | (base <> ": ") `T.isPrefixOf` err && "HTTP 501" `T.isInfixOf` err -> assertEqual "one request" ([narInfoPath, narPath], []) (seen, sleeps)
                  other -> Fail ("expected an HTTP 501 substitution error, got: " <> T.pack (show other)),
              -- One attempt follows the client's redirect limit, then stops.
              runTestM "a NAR redirect loop stops after one attempt" $ do
                (_, outcome, seen, sleeps, _) <-
                  substitute "nar-redirect" (const narInfoOk) (const (Send "HTTP/1.1 302 Found\r\nLocation: /nar/retry.nar\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"))
                pure $ case outcome of
                  Left (Subst.SubstError err)
                    | "too many redirects" `T.isInfixOf` err ->
                        assertEqual "one attempt" (narInfoPath : replicate (HTTP.redirectCount HTTP.defaultRequest + 1) narPath, []) (seen, sleeps)
                  other -> Fail ("expected a redirect-loop substitution error, got: " <> T.pack (show other)),
              runTestM "a torn NAR transfer is retried from its first byte" $ do
                let torn = "HTTP/1.1 200 OK\r\nContent-Length: " <> TE.encodeUtf8 (T.pack (show (BS.length nar))) <> "\r\nConnection: close\r\n\r\n" <> BS.take 10 nar
                (base, outcome, seen, sleeps, warnings) <-
                  substitute "nar-torn" (const narInfoOk) (\earlier -> if earlier == 0 then Send torn else narOk)
                let warnedTorn = case warnings of
                      [warning] -> ("download error fetching " <> base <> "/nar/retry.nar: ResponseBodyTooShort") `T.isPrefixOf` warning
                      _ -> False
                pure $
                  if warnedTorn
                    then assertEqual "substituted after one retry" (Right payload, [narInfoPath, narPath, narPath], [250]) (outcome, seen, sleeps)
                    else Fail ("unexpected warnings: " <> T.pack (show warnings)),
              -- A cache that names a NAR by its NarHash, as cache.nixos.org
              -- names its zstd NARs, serves one file for every path with
              -- that NAR, and an upload that compresses the NAR again
              -- replaces it, so an earlier narinfo's FileSize and FileHash
              -- describe bytes the cache no longer serves.  The signed
              -- NarHash and NarSize still describe the NAR in the file.
              runTestM "a NAR longer than its narinfo's FileSize, under a FileHash of other bytes, is substituted" $
                case signedNarInfoFor staleFileFields of
                  Left err -> pure (Fail (T.pack err))
                  Right (_, staleText) -> do
                    (_, outcome, seen, sleeps, _) <-
                      substitute "stale-file-fields" (const (Send (httpResponse "200 OK" (TE.encodeUtf8 staleText)))) (const narOk)
                    pure (assertEqual "substituted" (Right payload, [narInfoPath, narPath], []) (outcome, seen, sleeps)),
              -- The signed NarSize bounds the body.  This body is the start
              -- of a NAR whose one file runs past the ceiling, so the parser
              -- is still reading when the cap stops the transfer, and a body
              -- past its signed ceiling is never asked for again.
              runTestM "a NAR body one byte past its signed ceiling fails after one request" $ do
                let ceilingBytes = Subst.compressedBodyCeiling (NarInfo.niNarSize unsigned)
                    pastCeiling = BS.take (fromInteger ceilingBytes + 1) (NAR.serialise (NAR.NarRegular False (BS.replicate (fromInteger ceilingBytes) 0x78)))
                (base, outcome, seen, sleeps, _) <-
                  substitute "past-ceiling" (const narInfoOk) (const (Send (httpResponse "200 OK" pastCeiling)))
                pure $
                  assertEqual "body" (fromInteger ceilingBytes + 1) (BS.length pastCeiling)
                    `andThen` assertEqual
                      "stopped"
                      (Left (Subst.SubstError (base <> ": NAR body exceeds its download ceiling (" <> T.pack (show ceilingBytes) <> " bytes)")), [narInfoPath, narPath], [])
                      (outcome, seen, sleeps)
            ]
              -- Upstream's NotFound and Forbidden: a binary cache store
              -- reads both as the file not being in that cache.
              ++ [ runTestM ("a narinfo answered " <> status <> " is a miss, not retried") $ do
                     (_, outcome, seen, sleeps, _) <-
                       substitute ("narinfo-" ++ statusCode status) (const (Send (httpResponse (TE.encodeUtf8 status) ""))) (const narOk)
                     pure (assertEqual "miss" (Left Subst.SubstNotFound, [narInfoPath], []) (outcome, seen, sleeps))
                 | status <- notInCache
                 ]
              -- A NAR the narinfo named and the cache no longer holds is
              -- upstream's SubstituteGone: one request, never retried, and
              -- a miss rather than a failed substitute.
              ++ [ runTestM ("a NAR answered " <> status <> " is a miss after one request") $ do
                     (_, outcome, seen, sleeps, _) <-
                       substitute ("nar-" ++ statusCode status) (const narInfoOk) (const (Send (httpResponse (TE.encodeUtf8 status) "")))
                     pure (assertEqual "gone" (Left Subst.SubstNotFound, [narInfoPath, narPath], []) (outcome, seen, sleeps))
                 | status <- notInCache
                 ]
  where
    notInCache :: [Text]
    notInCache = ["404 Not Found", "410 Gone", "401 Unauthorized", "403 Forbidden", "407 Proxy Authentication Required"]
    statusCode = T.unpack . T.take 3
    quietRetry = RetryEffects {reSleepMs = const (pure ()), reJitter = pure 0, reWarn = const (pure ())}
    queryOffer fallback caches path =
      offerCache <$> evalStateT (Subst.queryReferencesWith defaultFetchRetryPolicy quietRetry fallback caches path) Subst.emptyNarInfoMemo
    hash = T.replicate 32 "b"
    sp = StorePath hash "retry"
    narInfoPath = "/" <> TE.encodeUtf8 hash <> ".narinfo"
    narPath = "/nar/retry.nar"
    payload = "retried payload\n"
    nar = NAR.serialise (NAR.NarDirectory [("data", NAR.NarRegular False payload)])
    unsigned =
      NarInfo.NarInfo
        { NarInfo.niStorePath = "/nix/store/" <> hash <> "-retry",
          NarInfo.niUrl = "nar/retry.nar",
          NarInfo.niCompression = "none",
          NarInfo.niFileHash = Nothing,
          NarInfo.niFileSize = Nothing,
          NarInfo.niNarHash = CHash.formatNixHash (CHash.hashBytes nar),
          NarInfo.niNarSize = fromIntegral (BS.length nar),
          NarInfo.niReferences = [],
          NarInfo.niDeriver = Nothing,
          NarInfo.niSigs = [],
          NarInfo.niCA = Nothing
        }
    -- The file fields another compression of the NAR gives: a FileSize
    -- 139 bytes short of the body served, and a FileHash of other bytes.
    staleFileFields =
      unsigned
        { NarInfo.niFileSize = Just (NarInfo.niNarSize unsigned - 139),
          NarInfo.niFileHash = Just (CHash.formatNixHash (CHash.hashBytes "another compression of the NAR"))
        }
    signedNarInfo = signedNarInfoFor unsigned
    -- A narinfo signed by 'sigTestKeyText' and rendered, with the public
    -- key that verifies it.
    signedNarInfoFor info = do
      secretKey <- Signing.parseSecretKey sigTestKeyText
      publicKey <- Signing.toPublicKey secretKey
      sig <- Signing.sign secretKey info
      pure (Signing.renderPublicKey publicKey, NarInfo.renderNarInfo info {NarInfo.niSigs = [sig]})
    route narInfoReply narReply path earlier
      | path == narInfoPath = narInfoReply earlier
      | path == narPath = narReply earlier
      | otherwise = Send (httpResponse "500 Internal Server Error" "unexpected request")

-- ---------------------------------------------------------------------------
-- Tests: store-path name validation at write sinks
-- ---------------------------------------------------------------------------

-- | 'posixOnlyEntries' as one directory.
posixOnlyTree :: NAR.NarEntry
posixOnlyTree = NAR.NarDirectory posixOnlyEntries

-- ---------------------------------------------------------------------------
-- Tests: substituter signature verification (the --trusted-key trust gate)
-- ---------------------------------------------------------------------------

-- | Minimal narinfo for signing tests; the fingerprint covers StorePath,
-- NarHash, NarSize, and References.
sigTestNarInfo :: NarInfo.NarInfo
sigTestNarInfo =
  NarInfo.NarInfo
    { NarInfo.niStorePath = "/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-hello-1.0",
      NarInfo.niUrl = "nar/aaaa.nar",
      NarInfo.niCompression = "none",
      NarInfo.niFileHash = Just ("sha256:" <> T.replicate 52 "0"),
      NarInfo.niFileSize = Just 1,
      NarInfo.niNarHash = "sha256:" <> T.replicate 52 "0",
      NarInfo.niNarSize = 1,
      NarInfo.niReferences = [],
      NarInfo.niDeriver = Nothing,
      NarInfo.niSigs = [],
      NarInfo.niCA = Nothing
    }

-- | A substituter cache trusting the given public key text.
sigTestCache :: Text -> Subst.CacheConfig
sigTestCache publicKeyText =
  Subst.CacheConfig
    { Subst.ccUrl = "http://localhost/test-cache",
      Subst.ccPublicKeys = [publicKeyText],
      Subst.ccPriority = 40
    }

-- | Derive (trusted cache, signature by the trusted key, signature by an
-- impostor key with the same name).  Left on any setup failure.
sigTestSetup :: Either String (Subst.CacheConfig, Text, Text)
sigTestSetup = do
  secretKey <- Signing.parseSecretKey sigTestKeyText
  impostorKey <- Signing.parseSecretKey sigTestWrongKeyText
  publicKey <- Signing.toPublicKey secretKey
  let publicKeyText = Signing.renderPublicKey publicKey
  validSig <- Signing.sign secretKey sigTestNarInfo
  impostorSig <- Signing.sign impostorKey sigTestNarInfo
  pure (sigTestCache publicKeyText, validSig, impostorSig)

-- | verifySigs is the security boundary behind @--trusted-key@: unsigned
-- narinfos, wrong-key signatures, and malformed trusted keys must all be
-- rejected; a valid signature must be accepted.
testVerifySigs :: IO [Bool]
testVerifySigs = do
  putStrLn "substituter/verify-sigs"
  case sigTestSetup of
    Left err -> (: []) <$> runTest "verifySigs test setup" (Fail (T.pack err))
    Right (cache, validSig, impostorSig) ->
      sequence
        [ runTest "valid signature accepted" $
            assertEqual "verify ok" (Right ()) (Subst.verifySigs cache sigTestNarInfo {NarInfo.niSigs = [validSig]}),
          runTest "unsigned narinfo rejected" $
            assertLeft "no sigs" (Subst.verifySigs cache sigTestNarInfo),
          runTest "same-name signature from a different key rejected" $
            assertLeft "impostor sig" (Subst.verifySigs cache sigTestNarInfo {NarInfo.niSigs = [impostorSig]}),
          runTest "valid signature among invalid ones accepted" $
            assertEqual "any valid" (Right ()) (Subst.verifySigs cache sigTestNarInfo {NarInfo.niSigs = [impostorSig, validSig]}),
          runTest "signature under a different key name rejected" $
            assertLeft "name mismatch" (Subst.verifySigs cache sigTestNarInfo {NarInfo.niSigs = ["other-key:" <> T.drop (T.length "test-key-1:") validSig]}),
          runTest "malformed trusted public key rejected" $
            assertLeft "bad pubkey" (Subst.verifySigs (sigTestCache "not-a-key") sigTestNarInfo {NarInfo.niSigs = [validSig]}),
          -- The flat trusted-key set: a signature valid under ANY of
          -- several trusted keys is accepted, even when the matching key
          -- is not first.
          runTest "a signature valid under any of several trusted keys is accepted" $
            let multiKey = cache {Subst.ccPublicKeys = Subst.ccPublicKeys Subst.defaultCacheConfig ++ Subst.ccPublicKeys cache}
             in assertEqual "any-of" (Right ()) (Subst.verifySigs multiKey sigTestNarInfo {NarInfo.niSigs = [validSig]}),
          runTest "a signature matching none of several trusted keys is rejected" $
            let multiKey = cache {Subst.ccPublicKeys = ["cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=", "other.cache-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="]}
             in assertLeft "none-of" (Subst.verifySigs multiKey sigTestNarInfo {NarInfo.niSigs = [validSig]}),
          -- A malformed key anywhere in the set is an error, not a
          -- silently skipped key: a typo must not quietly narrow the
          -- trusted set to the keys that happened to parse.
          runTest "a malformed key among valid ones is an error, not skipped" $
            let multiKey = cache {Subst.ccPublicKeys = "not-a-key" : Subst.ccPublicKeys cache}
             in assertLeft "typo not skipped" (Subst.verifySigs multiKey sigTestNarInfo {NarInfo.niSigs = [validSig]})
        ]

-- | Narinfo field validation gates the pipeline ahead of the signed
-- fingerprint: a malformed field must fail as a parse error before its
-- text can reach the fingerprint or anything downstream.
testNarInfoValidation :: IO [Bool]
testNarInfoValidation = do
  putStrLn "substituter/narinfo-field-validation"
  sequence
    [ runTest "well-formed narinfo passes" $
        assertEqual "valid" (Right ()) (Subst.validateNarInfoFields sigTestNarInfo),
      runTest "malformed StorePath rejected" $
        assertLeft "bad store path" (Subst.validateNarInfoFields sigTestNarInfo {NarInfo.niStorePath = "/nix/store/zzz"}),
      runTest "derivation StorePath rejected" $
        assertLeft "drv path" (Subst.validateNarInfoFields sigTestNarInfo {NarInfo.niStorePath = "/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-hello-1.0.drv"}),
      runTest "malformed reference rejected" $
        assertLeft "bad reference" (Subst.validateNarInfoFields sigTestNarInfo {NarInfo.niReferences = ["../escape"]}),
      runTest "malformed NarHash rejected" $
        assertLeft "bad narhash" (Subst.validateNarInfoFields sigTestNarInfo {NarInfo.niNarHash = "sha256:nope"}),
      runTest "negative NarSize rejected" $
        assertLeft "negative narsize" (Subst.validateNarInfoFields sigTestNarInfo {NarInfo.niNarSize = -1}),
      runTest "field validation reports ahead of signature verification" $
        case sigTestSetup of
          Left err -> Fail (T.pack err)
          Right (cache, validSig, _) ->
            case Subst.judgeNarInfo cache sigTestPath sigTestNarInfo {NarInfo.niStorePath = "/nix/store/zzz", NarInfo.niSigs = [validSig]} of
              Subst.NarInfoFailed err
                | "invalid narinfo" `T.isInfixOf` err -> Pass
                | otherwise -> Fail ("expected the field-validation error, got: " <> err)
              other -> Fail ("expected a failed query, got: " <> T.pack (show other)),
      -- Upstream skips a substitute for another path and one no trusted
      -- key signed, and fails on a trusted key it cannot parse.
      runTest "a narinfo for another path or unsigned is passed over, and a malformed key fails" $
        case sigTestSetup of
          Left err -> Fail (T.pack err)
          Right (cache, validSig, _) ->
            let signed = sigTestNarInfo {NarInfo.niSigs = [validSig]}
                malformedKey = case Subst.judgeNarInfo (sigTestCache "not-a-key") sigTestPath signed of
                  Subst.NarInfoFailed err | "invalid public key" `T.isPrefixOf` err -> Pass
                  other -> Fail ("expected a failed query, got: " <> T.pack (show other))
             in assertEqual "usable" (Subst.NarInfoUsable signed) (Subst.judgeNarInfo cache sigTestPath signed)
                  `andThen` assertEqual "unsigned" (Subst.NarInfoPassedOver "narinfo has no signatures") (Subst.judgeNarInfo cache sigTestPath sigTestNarInfo)
                  `andThen` assertEqual
                    "another path"
                    (Subst.NarInfoPassedOver ("narinfo names " <> NarInfo.niStorePath sigTestNarInfo))
                    (Subst.judgeNarInfo cache (StorePath (T.replicate 32 "c") "hello-1.0") signed)
                  `andThen` malformedKey
    ]
  where
    sigTestPath = StorePath (T.replicate 32 "a") "hello-1.0"
