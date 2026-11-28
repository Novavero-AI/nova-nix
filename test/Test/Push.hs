{-# LANGUAGE TupleSections #-}

-- | Tests of pushing to a binary cache.
module Test.Push
  ( testPushPure,
    testPushClosureIO,
    testPushMultipart,
    testPushDirectory,
  )
where

import CacheFixture (CacheSetup (..), Fault (..), Logged (..), Multipart (..), RequestKind (..), cacheSetup, requestLog, storedObject, withModelCache)
import Control.Exception (IOException, bracket, bracket_, try)
import Control.Monad (filterM, forM_, guard, (>=>))
import Control.Monad.Except (runExceptT)
import Control.Monad.State.Strict (evalStateT)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (isDigit)
import Data.Either (isLeft)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Int (Int32)
import Data.List (intercalate, nub, sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import HttpFixture (withDirectoryServer)
import Nix.Builder (BuildConfig (..), BuildResult (..), buildWithDeps, defaultBuildConfig)
import Nix.CacheDirectory (CacheInfo (..), CacheOpenFailure (..), cacheFilePath, cacheInfoFileName, cacheInfoText, directoryUrl, fileUrlDirectory, parsePriority, readCacheInfo)
import qualified Nix.Cli.Args as Cli
import Nix.Derivation (Derivation (..), DerivationOutput (..), currentPlatform)
import qualified Nix.Hash as Hash
import Nix.Http (FetchRetryPolicy (..), RetryEffects (..), defaultFetchRetryPolicy)
import Nix.Logger (Logger (..))
import Nix.Push (PushArtifact (..), PushCompression (..), PushConfig (..), PushSummary (..), checkRecordedNarHash, computeClosure, defaultPartSize, isDerivationPath, loadKeyFile, mkNarInfo, mkPartSize, mkPushArtifact, narFileName, narHashMatches, outputPathsOnly, parsePushCompression, partSizeBytes, planMissing, preparePath, pushPaths, splitParts, storePathBasename, stripHashPrefix)
import Nix.Push.Directory (DirectoryPush (..), PushTarget (..), ed25519SeedBytes, loadSecretKeyFile, pushTarget, pushToDirectory, signNarInfo)
import qualified Nix.Realise as Realise
import Nix.Store (Store (..), addToStore, closeStore, openStore, releasePathLock, writeDrv)
import Nix.Store.DB (PathInfo (..), PathRegistration (..), Ultimate (..), queryReferences, registerPaths)
import Nix.Store.Path (StoreDir (..), parseStorePath, storePathToFilePath)
import Nix.Store.Path.Internal (StorePath (..))
import qualified Nix.Substituter as Subst
import NodeStatus (NodeStatus (..), nodeStatus)
import qualified NovaCache.Base64 as B64
import qualified NovaCache.Hash as CHash
import qualified NovaCache.NAR as NAR
import qualified NovaCache.NarInfo as NarInfo
import qualified NovaCache.Signing as Signing
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getTemporaryDirectory)
import qualified System.Directory as Dir
import System.FilePath (equalFilePath, (</>))
import qualified System.Info as SI
import Test.Harness (TestResult (..), andThen, assertEqual, assertRight, findTestShell, forceRemoveIfExists, offeredThenFetched, runTest, runTestM, testLogger, throwawayKey, withTempStore)
import qualified Test.QuickCheck as QC

-- ---------------------------------------------------------------------------
-- Tests: Push (pure narinfo construction + closure computation)
-- ---------------------------------------------------------------------------

testPushPure :: IO [Bool]
testPushPure = do
  putStrLn "push/narinfo"
  let hashA = T.replicate 32 "a"
      hashB = T.replicate 32 "b"
      spA = StorePath hashA "hello-1.0"
      spB = StorePath hashB "dep-2.0"
      narHash = "sha256:0123abcdef"
      narBytes = BS.replicate 1234 0x6e
      artifactNone = mkPushArtifact PushNone narHash narBytes
      artifactZstd = mkPushArtifact PushZstd narHash narBytes
      -- References deliberately unsorted; deriver present.
      ni = mkNarInfo artifactNone spA [spB, spA] (Just spB)
      niZstd = mkNarInfo artifactZstd spA [spB, spA] (Just spB)
  sequence
    [ runTest "narinfo StorePath is canonical /nix/store" $
        assertEqual "StorePath" ("/nix/store/" <> hashA <> "-hello-1.0") (NarInfo.niStorePath ni),
      runTest "narinfo URL is nar/<digest>.nar" $
        assertEqual "URL" "nar/0123abcdef.nar" (NarInfo.niUrl ni),
      runTest "compression none mirrors NAR fields into file fields" $
        assertEqual
          "file fields"
          ("none", Just (NarInfo.niNarHash ni), Just (NarInfo.niNarSize ni))
          (NarInfo.niCompression ni, NarInfo.niFileHash ni, NarInfo.niFileSize ni),
      runTest "narinfo sizes carry through" $
        assertEqual "NarSize" 1234 (NarInfo.niNarSize ni),
      -- The zstd artifact: file fields describe the compressed object,
      -- named by its own file hash, while the NAR fields stay the
      -- archive's - and the substituter's strict decoder round-trips
      -- the bytes under the declared NarSize bound.
      runTest "zstd artifact declares the compressed object" $
        assertEqual
          "zstd fields"
          ("zstd", Just (paFileHash artifactZstd), Just (fromIntegral (paFileSize artifactZstd)), 1234)
          (NarInfo.niCompression niZstd, NarInfo.niFileHash niZstd, NarInfo.niFileSize niZstd, NarInfo.niNarSize niZstd),
      -- Ground truth, not field copying: the file fields must equal
      -- independent computation over the artifact's actual bytes, and
      -- must NOT collapse into the NAR fields (the copy-paste hazard
      -- for a compressible input like this one).
      runTest "zstd file fields are computed from the compressed bytes" $
        let independentHash = CHash.formatNixHash (CHash.hashBytes (paBytes artifactZstd))
         in if paFileHash artifactZstd == independentHash
              && paFileSize artifactZstd == BS.length (paBytes artifactZstd)
              && paFileHash artifactZstd /= narHash
              && paFileSize artifactZstd /= BS.length narBytes
              then Pass
              else Fail "zstd file fields do not match the compressed bytes",
      runTest "none artifact IS the NAR" $
        if paBytes artifactNone == narBytes
          && paFileHash artifactNone == narHash
          && paFileSize artifactNone == BS.length narBytes
          then Pass
          else Fail "none artifact diverges from its NAR",
      runTest "zstd object is named by its independently computed file hash" $
        assertEqual
          "zstd URL"
          ("nar/" <> stripHashPrefix (CHash.formatNixHash (CHash.hashBytes (paBytes artifactZstd))) <> ".nar.zst")
          (NarInfo.niUrl niZstd),
      -- mkNarInfo cannot be handed NAR fields that disagree with the
      -- artifact: they come from the artifact itself.
      runTest "narinfo NAR fields come from the artifact" $
        assertEqual
          "NAR fields"
          (narHash, toInteger (BS.length narBytes))
          (NarInfo.niNarHash niZstd, NarInfo.niNarSize niZstd),
      runTestM "zstd artifact round-trips through the substituter decoder" $ do
        out <- Subst.decompressNar 1234 "zstd" (paBytes artifactZstd)
        pure (assertEqual "push-substitute roundtrip" (Right narBytes) out),
      -- The CLI vocabulary is the wire vocabulary, parsed in one place.
      runTest "parsePushCompression accepts the register and rejects by name" $
        case (parsePushCompression "none", parsePushCompression "zstd", parsePushCompression "brotli") of
          (Right PushNone, Right PushZstd, Left err)
            | "none, zstd" `T.isInfixOf` err -> Pass
          other -> Fail ("unexpected parse outcomes: " <> T.pack (show other)),
      runTest "references are sorted basenames" $
        assertEqual
          "References"
          [hashA <> "-hello-1.0", hashB <> "-dep-2.0"]
          (NarInfo.niReferences ni),
      runTest "deriver renders as a basename" $
        assertEqual "Deriver" (Just (hashB <> "-dep-2.0")) (NarInfo.niDeriver ni),
      runTest "no client-side signatures" $
        assertEqual "Sigs" ([] :: [Text]) (NarInfo.niSigs ni),
      -- The first main run of the cache job pushed a store's first valid
      -- path alphabetically, a .drv, and the cache refused its narinfo:
      -- a store registers recipes as valid paths, a cache serves outputs.
      runTest "outputPathsOnly drops derivations and keeps everything else" $
        assertEqual
          "outputs"
          ["hello", "hello.drv.tar", "src"]
          (map spName (outputPathsOnly [StorePath (T.replicate 32 "a") "hello", StorePath (T.replicate 32 "b") "hello.drv", StorePath (T.replicate 32 "c") "hello.drv.tar", StorePath (T.replicate 32 "d") "src"])),
      runTest "isDerivationPath is the .drv suffix on the name, not the hash" $
        assertEqual "drv" [True, False, False] (map isDerivationPath [StorePath (T.replicate 32 "a") "x.drv", StorePath (T.replicate 32 "b") "x", StorePath (T.replicate 32 "c") "drv"]),
      runTest "planMissing keeps only uncached paths" $
        assertEqual
          "missing"
          [hashB]
          (map spHash (planMissing (Set.singleton hashA) [spA, spB])),
      runTest "stripHashPrefix drops the algo tag" $
        assertEqual "tagged" "deadbeef" (stripHashPrefix "sha256:deadbeef"),
      runTest "stripHashPrefix tolerates a bare digest" $
        assertEqual "bare" "deadbeef" (stripHashPrefix "deadbeef"),
      runTest "storePathBasename joins hash and name" $
        assertEqual "basename" (hashA <> "-hello-1.0") (storePathBasename spA),
      runTest "narFileName appends .nar" $
        assertEqual "file" "0123abcdef.nar" (narFileName narHash),
      -- checkRecordedNarHash: the pre-upload integrity gate
      runTest "push gate accepts a registered matching path" $
        assertEqual
          "gate ok"
          (Right ())
          (checkRecordedNarHash (Just (pathInfoFor narHash)) narHash spA),
      runTest "push gate refuses an unregistered path" $
        case checkRecordedNarHash Nothing narHash spA of
          Left err
            | "not registered" `T.isInfixOf` err -> Pass
            | otherwise -> Fail ("wrong error: " <> err)
          Right () -> Fail "unregistered path must not be publishable",
      runTest "push gate refuses a changed-on-disk path" $
        case checkRecordedNarHash (Just (pathInfoFor "sha256:other")) narHash spA of
          Left err
            | "DB recorded" `T.isInfixOf` err -> Pass
            | otherwise -> Fail ("wrong error: " <> err)
          Right () -> Fail "hash mismatch must not be publishable",
      -- narHashMatches decodes both spellings before comparing.
      -- parseNixHash reads only the sha256:nix-base32 spelling, so a
      -- base16-recorded digest (both literals spell the empty-string
      -- sha256) falls back to text equality and refuses - the safe
      -- direction.  When the parser learns more spellings (foreign
      -- caches), this pin flips and the gate widens with it.
      runTest "push gate pins the accepted hash spellings" $
        if narHashMatches
          "sha256:e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
          "sha256:0mdqa9w1p6cmli6976v4wi0sw9r4p5prkj7lzfd1877wk11c9c73"
          then Fail "base16 spelling unexpectedly matched - widen this pin"
          else Pass,
      runTest "push gate still refuses different digests" $
        if narHashMatches
          "sha256:0mdqa9w1p6cmli6976v4wi0sw9r4p5prkj7lzfd1877wk11c9c73"
          (Hash.formatNixHash (Hash.hashBytes "different"))
          then Fail "distinct digests must not match"
          else Pass
    ]
  where
    pathInfoFor recordedHash =
      PathInfo
        { piPath = "/nix/store/" <> T.replicate 32 "a" <> "-hello-1.0",
          piNarHash = recordedHash,
          piNarSize = 1234,
          piDeriver = Nothing,
          piRegTime = 0,
          piUltimate = NotUltimate,
          piSignatures = Set.empty,
          piContentAddress = Nothing
        }

-- | Closure computation against a real (temp) store database.
testPushClosureIO :: IO [Bool]
testPushClosureIO = do
  putStrLn "push/closure"
  withTempStore $ \store -> do
    let spTop = StorePath (T.replicate 32 "1") "top"
        spMid = StorePath (T.replicate 32 "2") "mid"
        spLeaf = StorePath (T.replicate 32 "3") "leaf"
        regFor sp refs =
          PathRegistration
            { prPath = sp,
              prNarHash = "sha256:" <> T.replicate 52 "0",
              prNarSize = 1,
              prDeriver = Nothing,
              prReferences = refs,
              prUltimate = NotUltimate,
              prSignatures = Set.empty,
              prContentAddress = Nothing
            }
    registerPaths (stDB store) [regFor spTop [spMid], regFor spMid [spLeaf], regFor spLeaf []]
    fromTop <- computeClosure store [spTop]
    fromBoth <- computeClosure store [spTop, spLeaf]
    sequence
      [ runTest "closure walks transitive references" $
          assertRight "fromTop" fromTop $ \closure ->
            assertEqual
              "hashes"
              (Set.fromList (map spHash [spTop, spMid, spLeaf]))
              (Set.fromList (map spHash closure)),
        runTest "closure deduplicates across roots" $
          assertRight "fromBoth" fromBoth $ \closure ->
            assertEqual "count" (3 :: Int) (length closure),
        -- loadKeyFile: keys copied through Windows editors arrive with
        -- a BOM and CRLF; skipping the normalization turns every push
        -- into an auth rejection with no visible cause.
        runTestM "loadKeyFile strips BOM and CRLF" $ do
          tmpBase <- getTemporaryDirectory
          let keyFile = tmpBase </> "nova-nix-test-api-key"
          BS.writeFile keyFile (BS.pack [0xEF, 0xBB, 0xBF] <> "the-secret-key\r\n")
          loaded <- loadKeyFile keyFile
          Dir.removeFile keyFile
          pure (assertEqual "normalized key" (Right "the-secret-key") loaded),
        runTestM "loadKeyFile rejects an effectively-empty key file" $ do
          tmpBase <- getTemporaryDirectory
          let keyFile = tmpBase </> "nova-nix-test-api-key-empty"
          BS.writeFile keyFile (BS.pack [0xEF, 0xBB, 0xBF] <> "  \r\n")
          loaded <- loadKeyFile keyFile
          Dir.removeFile keyFile
          pure $ case loaded of
            Left err | "empty" `T.isInfixOf` err -> Pass
            other -> Fail ("expected empty-key rejection, got: " <> T.pack (show other)),
        runTestM "loadKeyFile reports a missing file" $ do
          tmpBase <- getTemporaryDirectory
          loaded <- loadKeyFile (tmpBase </> "nova-nix-test-no-such-key-file")
          pure $ case loaded of
            Left err | "cannot read key file" `T.isInfixOf` err -> Pass
            other -> Fail ("expected read error, got: " <> T.pack (show other))
      ]

-- | push sends a NAR over its part size through nova-cache's multipart
-- upload.  Each case pushes one registered path to a fresh
-- 'withModelCache': nova-cache's write endpoints behind a proxy that
-- refuses a body over the part size, as Cloudflare refuses one over
-- 100 MB in front of the public cache.  The cache records each request,
-- and a case reads back the requests and the NAR the cache stores.
testPushMultipart :: IO [Bool]
testPushMultipart = do
  putStrLn "push/multipart"
  tmpBase <- getTemporaryDirectory
  let source = tmpBase </> "nova-nix-test-push-multipart-src"
      topSource = tmpBase </> "nova-nix-test-push-multipart-top"
      goneSource = tmpBase </> "nova-nix-test-push-multipart-gone"
      sp = StorePath (T.replicate 32 "m") "large-output"
      -- A small path that references the large one, for a closure of two.
      spTop = StorePath (T.replicate 32 "n") "small-top"
      -- A registered path whose tree is gone, which a push cannot read.
      spGone = StorePath (T.replicate 32 "p") "gone"
  forceRemoveIfExists source
  forceRemoveIfExists topSource
  forceRemoveIfExists goneSource
  createDirectoryIfMissing True source
  createDirectoryIfMissing True topSource
  createDirectoryIfMissing True goneSource
  BS.writeFile (goneSource </> "payload") "gone"
  BS.writeFile (source </> "payload") (BS.pack (take payloadBytes (cycle [0 .. 250])))
  BS.writeFile (topSource </> "payload") "small"
  results <- withTempStore $ \store -> do
    addToStore store source sp Nothing []
    addToStore store topSource spTop Nothing [sp]
    addToStore store goneSource spGone Nothing []
    forceRemoveIfExists (storePathToFilePath (stDir store) spGone)
    prepared <- runExceptT (preparePath PushNone store sp)
    case (prepared, mkPartSize testPartBytes) of
      (Right (artifact, narInfo), Just partSize) -> multipartCases store sp spTop spGone partSize (paBytes artifact) (TE.encodeUtf8 (NarInfo.niUrl narInfo))
      (Left err, _) -> sequence [runTest "push/multipart setup" (Fail err)]
      (Right _, Nothing) -> sequence [runTest "push/multipart setup" (Fail "the test part size is not one")]
  forceRemoveIfExists source
  forceRemoveIfExists topSource
  forceRemoveIfExists goneSource
  pure results
  where
    -- Large enough for a NAR of at least three parts, so a part has parts
    -- before and after it.
    payloadBytes = 3000
    testPartBytes = 1024
    multipartCases store sp spTop spGone partSize nar narObject =
      sequence
        [ runTest "the test NAR is at least three parts" $
            assertEqual "parts" True (count >= 3),
          -- The command that pushes words the failure; the push says only
          -- what went wrong.
          runTestM "a push that cannot read a path reports why, without the command's prefix" $ do
            (result, _) <- withModelCache behindProxy (\base -> pushPaths (config base partSize) store [spGone])
            pure $ case result of
              Left err | not ("push failed" `T.isInfixOf` err), not (T.null err) -> Pass
              other -> Fail ("expected the failure without the command's prefix, got " <> T.pack (show other)),
          runTestM "a NAR over the part size goes up in parts under the proxy's limit, each request with the write key, and is completed" $ do
            outcome <- pushTo behindProxy partSize
            pure $
              delivered testPartBytes outcome
                `andThen` assertEqual "requests" ([HashList] ++ inParts ++ published) (kinds outcome),
          runTestM "a NAR of one part's size goes in one PUT, and one byte more goes in two parts" $
            case (mkPartSize (BS.length nar), mkPartSize (BS.length nar - 1)) of
              (Just whole, Just split) -> do
                oneRequest <- pushTo (cacheSetup (BS.length nar) key) whole
                twoParts <- pushTo (cacheSetup (BS.length nar - 1) key) split
                pure $
                  delivered (BS.length nar) oneRequest
                    `andThen` assertEqual "one request" ([HashList, NarPut] ++ published) (kinds oneRequest)
                    `andThen` delivered (BS.length nar - 1) twoParts
                    `andThen` assertEqual "two parts" ([HashList, Begin, Part 1, Part 2, Complete] ++ published) (kinds twoParts)
              _ -> pure (Fail "the NAR's size is no part size"),
          runTestM "a part whose response is lost is sent again" $ do
            outcome <- pushTo (once (Part 2) DropResponse) partSize
            pure $
              delivered testPartBytes outcome
                `andThen` assertEqual "requests" ([HashList, Begin, Part 1] ++ map Part (2 : [2 .. count]) ++ [Complete] ++ published) (kinds outcome),
          runTestM "a part answered 409 begins a new upload" $ do
            outcome <- pushTo (once (Part 2) LoseParts) partSize
            pure $
              delivered testPartBytes outcome
                `andThen` assertEqual "requests" ([HashList, Begin, Part 1, Part 2] ++ inParts ++ published) (kinds outcome),
          runTestM "a part answered 404 begins a new upload" $ do
            outcome <- pushTo (once (Part 3) ForgetUpload) partSize
            pure $
              delivered testPartBytes outcome
                `andThen` assertEqual "requests" ([HashList, Begin, Part 1, Part 2, Part 3] ++ inParts ++ published) (kinds outcome),
          runTestM "a completion whose response is lost leaves the narinfo to confirm the NAR" $ do
            outcome <- pushTo (once Complete DropResponse) partSize
            pure $
              delivered testPartBytes outcome
                `andThen` assertEqual "requests" ([HashList] ++ inParts ++ published) (kinds outcome),
          runTestM "a narinfo refused after a completion that failed uploads the NAR again" $ do
            outcome <- pushTo (once Complete (Refuse "502 Bad Gateway")) partSize
            pure $
              delivered testPartBytes outcome
                `andThen` assertEqual "requests" ([HashList] ++ inParts ++ [NarInfoPut] ++ inParts ++ published) (kinds outcome),
          runTestM "a cache that keeps losing parts fails the push after three uploads, naming the request and the server's message" $ do
            outcome@(result, _) <- pushTo (always (Part 2) LoseParts) partSize
            pure $
              assertEqual "uploads begun" 3 (countOf Begin outcome)
                `andThen` failsWith ["/2 returned HTTP 409: ", "holds 0 parts, so the next is part 1", "(3 uploads begun)"] result,
          runTestM "a part whose response is always lost fails the push after the retry policy's attempts" $ do
            outcome@(result, _) <- pushTo (always (Part 1) DropResponse) partSize
            pure $
              assertEqual "sends of part 1" (frpAttempts retry) (countOf (Part 1) outcome)
                `andThen` failsWith ["/1 failed: "] result,
          runTestM "a narinfo refused after every completion fails the push after three uploads, naming both answers" $ do
            outcome@(result, _) <- pushTo (always Complete (Refuse "502 Bad Gateway")) partSize
            pure $
              assertEqual "uploads begun" 3 (countOf Begin outcome)
                `andThen` assertEqual "narinfos sent" 3 (countOf NarInfoPut outcome)
                `andThen` failsWith [".narinfo returned HTTP 400: NAR nar/", "is not stored", "the NAR's last completion: POST nar/", "returned HTTP 502: ", "(3 uploads begun)"] result,
          runTestM "a cache without multipart upload takes a NAR over the part size in one PUT" $ do
            outcome <- pushTo ((cacheSetup maxBound key) {csMultipart = WithoutMultipart}) partSize
            pure $
              delivered maxBound outcome
                `andThen` assertEqual "requests" ([HashList, Begin, NarPut] ++ published) (kinds outcome),
          -- The narinfo that decides an unconfirmed NAR goes with the
          -- other narinfos, after every NAR: checked as soon as the
          -- completion went unanswered, it would announce the large path
          -- before the small one's NAR, and announce a path whose
          -- references are not all in place in a longer closure.
          runTestM "the narinfo that confirms a NAR goes after every NAR of the closure, in the closure's order" $ do
            (result, state) <- withModelCache (once Complete DropResponse) (\base -> pushPaths (config base partSize) store [spTop])
            pure $
              assertEqual "push" (Right (PushSummary 2 0)) result
                `andThen` assertEqual "stored NAR" (Just nar) (storedObject narObject state)
                `andThen` assertEqual "requests" ([HashList] ++ inParts ++ [NarPut, NarInfoPut, NarInfoPut, NarInfoGet]) (map lgKind (requestLog state)),
          runTestM "a part answered with a status a retry could outlive is sent again" $ do
            outcome <- pushTo (once (Part 2) (Refuse "503 Service Unavailable")) partSize
            pure $
              delivered testPartBytes outcome
                `andThen` assertEqual "requests" ([HashList, Begin, Part 1] ++ map Part (2 : [2 .. count]) ++ [Complete] ++ published) (kinds outcome),
          runTestM "a part refused with a status no retry changes fails the push at once" $ do
            outcome@(result, _) <- pushTo (always (Part 1) (Refuse "413 Payload Too Large")) partSize
            pure $
              assertEqual "sends of part 1" 1 (countOf (Part 1) outcome)
                `andThen` failsWith ["/1 returned HTTP 413: refused by the fixture"] result,
          runTestM "an answer the upload does not define fails the push at once" $ do
            noId@(noIdResult, _) <- pushTo (once Begin (Refuse "201 Created")) partSize
            noError@(noErrorResult, _) <- pushTo (always (Part 1) (Refuse "204 No Content")) partSize
            pure $
              assertEqual "beginnings" 1 (countOf Begin noId)
                `andThen` failsWith ["/uploads answered with no upload id: refused by the fixture"] noIdResult
                `andThen` assertEqual "sends of part 1" 1 (countOf (Part 1) noError)
                `andThen` failsWith ["/1 returned HTTP 204"] noErrorResult,
          runTest "the default part size is 64 MiB, under the 100 MB a Cloudflare proxy allows" $
            assertEqual "default" (64 * 1024 * 1024, True) (partSizeBytes defaultPartSize, partSizeBytes defaultPartSize < cloudflareBodyLimit),
          runTest "a part size is at least one byte" $
            assertEqual "sizes" [Nothing, Nothing, Just 1] (map (fmap partSizeBytes . mkPartSize) [-1, 0, 1]),
          runTestM "splitting a NAR into parts keeps every byte in order, each part full but the last" $ do
            result <-
              QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} $
                QC.forAll ((,) <$> QC.choose (1, 40) <*> QC.resize 200 (QC.listOf QC.arbitrary)) $ \(size, bytes) ->
                  let input = BS.pack bytes
                      parts = maybe [] (`splitParts` input) (mkPartSize size)
                      (full, rest) = length bytes `divMod` size
                   in (BS.concat parts, map BS.length parts) QC.=== (input, replicate full size ++ [rest | rest > 0])
            pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))
        ]
      where
        key = TE.encodeUtf8 apiKey
        count = (BS.length nar + testPartBytes - 1) `div` testPartBytes
        inParts = [Begin] ++ map Part [1 .. count] ++ [Complete]
        published = [NarInfoPut, NarInfoGet]
        config base size =
          PushConfig
            { pcCacheUrl = base,
              pcApiKey = Just apiKey,
              pcCompression = PushNone,
              pcPartSize = size,
              pcRetry = retry
            }
        pushTo setup size = withModelCache setup (\base -> pushPaths (config base size) store [sp])
        behindProxy = cacheSetup testPartBytes key
        once kind fault = behindProxy {csFault = \seenKind seen -> fault <$ guard (seenKind == kind && seen == 0)}
        always kind fault = behindProxy {csFault = \seenKind _ -> fault <$ guard (seenKind == kind)}
        kinds = map lgKind . requestLog . snd
        countOf kind = length . filter (== kind) . kinds
        -- The push published the path, the cache holds the NAR it was
        -- given, and no request carried a body past the proxy.
        delivered limit (result, state) =
          assertEqual "push" (Right (PushSummary 1 0)) result
            `andThen` assertEqual "stored NAR" (Just nar) (storedObject narObject state)
            `andThen` assertEqual "bodies over the limit" [] (filter ((> limit) . lgBodyBytes) (requestLog state))
        failsWith fragments result = case result of
          Left err | all (`T.isInfixOf` err) fragments -> Pass
          other -> Fail ("expected a failure naming " <> T.pack (show fragments) <> ", got " <> T.pack (show other))
    apiKey = "test-write-key"
    retry = defaultFetchRetryPolicy {frpBaseDelayMs = 1}
    -- Cloudflare's 100 MB, read as the smaller 10^8 bytes.
    cloudflareBodyLimit = 100 * 1000 * 1000

-- | push to a directory: the binary cache it writes, read back file by
-- file and substituted from by nova-nix's own substituter, as a static web
-- server serves it and through its @file:\/\/@ URL; what a second push
-- skips; the refusal of a cache for another store directory and of a key
-- file that holds no secret key; how @--cache@ and the key files select
-- where a push writes; and how a substitution opens and reads a
-- @file:\/\/@ cache.  The key is generated for the run, and each case that
-- pushes pushes top, which references leaf, from a store of its own.
testPushDirectory :: IO [Bool]
testPushDirectory = do
  putStrLn "push/directory"
  tmpBase <- getTemporaryDirectory
  seed <- BS.pack <$> QC.generate (QC.vectorOf ed25519SeedBytes QC.arbitrary)
  let root = tmpBase </> "nova-nix-test-push-directory"
  bracket_ (forceRemoveIfExists root) (forceRemoveIfExists root) $ do
    createDirectoryIfMissing True root
    let keyFile = root </> "secret-key"
    key <- case throwawayKey seed of
      Left err -> pure (Left (T.pack err))
      Right (keyText, publicKey) -> do
        BS.writeFile keyFile (TE.encodeUtf8 keyText)
        fmap (,publicKey) <$> loadSecretKeyFile keyFile
    case key of
      Left err -> sequence [runTest "push/directory key" (Fail err)]
      Right (secretKey, publicKey) -> concat <$> sequence [directoryCases root secretKey publicKey, substitutionCases root secretKey publicKey, targetCases root, cacheInfoCases]
  where
    leaf = StorePath (T.replicate 32 "p") "leaf"
    top = StorePath (T.replicate 32 "q") "top"
    contentsOf sp = TE.encodeUtf8 (spName sp) <> "\n"
    narOf sp = NAR.serialise (NAR.NarDirectory [("data", NAR.NarRegular False (contentsOf sp))])
    narInfoFile cache sp = cache </> T.unpack (spHash sp) <> ".narinfo"
    -- The directories the newest upstream release's file:// store makes in
    -- a cache it opens, sorted, which a substitution makes too.
    openedCacheDirectories = ["build-trace-v2", "log", "nar"]
    -- Those a push makes, sorted: the realisations directory under the
    -- name older upstream releases read as well.
    pushedCacheDirectories = sort ("realisations" : openedCacheDirectories)
    readMaybeFile path = either (const Nothing) Just <$> (try (BS.readFile path) :: IO (Either IOException BS.ByteString))
    -- A store holding top and leaf under the root, and the cache
    -- directory beside it.
    withSourceStore root label action = do
      let base = root </> label
      createDirectoryIfMissing True base
      bracket (openStore testLogger (StoreDir (base </> "store"))) closeStore $ \store -> do
        forM_ [(leaf, []), (top, [leaf])] $ \(sp, references) -> do
          let source = base </> T.unpack (spName sp)
          createDirectoryIfMissing True source
          BS.writeFile (source </> "data") (contentsOf sp)
          addToStore store source sp Nothing references
        action store (base </> "cache")
    -- A cache directory as a substitution reads it: served over loopback
    -- HTTP, or through its file:// URL.
    overHttp = withDirectoryServer
    directly cache use = use (directoryUrl cache)
    -- Build a derivation whose one output is top, with a builder that
    -- fails, in a fresh store whose one cache is the directory, served as
    -- given: the result, what leaf and top hold, and top's references
    -- there.
    substituteFrom serving root label cache publicKey = serving cache $ \base -> do
      shell <- findTestShell
      let dest = StoreDir (root </> label </> "dest")
          drvSP = StorePath (T.replicate 32 "r") "top.drv"
          drv =
            Derivation
              { drvOutputs = [DerivationOutput "out" top "" ""],
                drvInputDrvs = Map.empty,
                drvInputSrcs = [],
                drvPlatform = currentPlatform,
                drvBuilder = TE.encodeUtf8 shell,
                drvArgs = ["-c", "exit 1"],
                drvEnv = Map.singleton "name" "top"
              }
          config = (defaultBuildConfig dest) {bcCaches = [Subst.CacheConfig base [Signing.renderPublicKey publicKey] 40]}
      bracket (openStore testLogger dest) closeStore $ \store -> do
        writeDrv store drv drvSP
        result <- buildWithDeps config store drv drvSP
        held <- mapM (\sp -> readMaybeFile (storePathToFilePath (stDir store) sp </> "data")) [leaf, top]
        references <- map (parseStorePath (stDir store)) <$> queryReferences (stDB store) top
        pure (result, held, references)
    directoryCases root secretKey publicKey = do
      let pushTo cache compression = pushToDirectory (DirectoryPush cache secretKey compression)
          nar = narOf top
          narHash = CHash.formatNixHash (CHash.hashBytes nar)
          narUrl = "nar/" <> stripHashPrefix narHash <> ".nar"
          size = T.pack (show (BS.length nar))
      (pushed, directories, narInfoBytes, cacheInfo, storedNar, substituted, direct, again, before, after, modes) <-
        withSourceStore root "main" $ \store cache -> do
          pushed <- pushTo cache PushNone store [top]
          directories <- filterM (Dir.doesDirectoryExist . (cache </>)) pushedCacheDirectories
          narInfoBytes <- readMaybeFile (narInfoFile cache top)
          cacheInfo <- readMaybeFile (cache </> cacheInfoFileName)
          storedNar <- readMaybeFile (cache </> T.unpack narUrl)
          substituted <- substituteFrom overHttp root "main" cache publicKey
          listed <- cacheListing cache
          substitutedDirectly <- substituteFrom directly root "main-direct" cache publicKey
          listedAfter <- cacheListing cache
          let direct = (substitutedDirectly, listed == listedAfter)
          before <- cacheListing cache
          again <- pushTo cache PushNone store [top]
          after <- cacheListing cache
          let plain = root </> "main" </> "plain"
          BS.writeFile plain "plain"
          modes <- mapM (fmap (fmap nsPermissions) . nodeStatus) [plain, narInfoFile cache top, cache </> T.unpack narUrl, cache </> cacheInfoFileName]
          pure (pushed, directories, narInfoBytes, cacheInfo, storedNar, substituted, direct, again, before, after, modes)
      sequence
        [ runTest "a push to a directory writes the closure, and nova-nix substitutes it from the directory served over HTTP, building nothing" $
            assertEqual "push" (Right (PushSummary 2 0)) pushed
              `andThen` assertEqual "substitution" (BuildSuccess top, [Just (contentsOf leaf), Just (contentsOf top)], [Just leaf]) substituted,
          runTest "nova-nix substitutes the closure from the directory's file:// URL directly, file:///C:/... on Windows, building nothing, and writes nothing to the cache" $
            assertEqual "substitution" ((BuildSuccess top, [Just (contentsOf leaf), Just (contentsOf top)], [Just leaf]), True) direct,
          -- Upstream's file:// store creates these in every cache it opens,
          -- and a reader that cannot create a missing one drops the cache.
          runTest "a push makes the nar, log and realisations directories upstream's file:// store makes, the last as realisations and as build-trace-v2, so a reader of any pinned version that cannot write opens the cache" $
            assertEqual "directories" pushedCacheDirectories directories,
          runTest "the narinfo holds upstream's fields in upstream's order, names its NAR by the file's hash, and carries one signature the public key verifies" $
            case TE.decodeUtf8' <$> narInfoBytes of
              Just (Right text) ->
                let (fields, sigs) = break ("Sig: " `T.isPrefixOf`) (T.lines text)
                 in assertEqual
                      "fields"
                      [ "StorePath: /nix/store/" <> storePathBasename top,
                        "URL: " <> narUrl,
                        "Compression: none",
                        "FileHash: " <> narHash,
                        "FileSize: " <> size,
                        "NarHash: " <> narHash,
                        "NarSize: " <> size,
                        "References: " <> storePathBasename leaf
                      ]
                      fields
                      `andThen` assertEqual "signatures" 1 (length sigs)
                      `andThen` assertRight "parse" (NarInfo.parseNarInfo text) (\narInfo -> assertEqual "verified" True (all (Signing.verify publicKey narInfo) (NarInfo.niSigs narInfo)))
              other -> Fail ("no UTF-8 narinfo: " <> T.pack (show other)),
          runTest "the NAR is the path's archive, uncompressed, under nar/" $
            assertEqual "NAR" (Just nar) storedNar,
          runTest "nix-cache-info names the store directory alone, /nix/store, as upstream writes it" $
            assertEqual "nix-cache-info" (Just "StoreDir: /nix/store\n") cacheInfo,
          runTest "a second push skips every path the directory holds and changes no file" $
            assertEqual "push" (Right (PushSummary 0 2)) again
              `andThen` assertEqual "files" before after,
          runTest "the cache's files have the permissions a plain write leaves, so another user can read them" $
            case modes of
              Just plainMode : written
                | all (== Just plainMode) written -> Pass
              -- Windows has no mode.
              _ | SI.os == "mingw32" -> Pass
              other -> Fail ("modes, the plain file's first: " <> T.pack (show other)),
          runTestM "a NAR whose file is in the directory is not written again" $
            withSourceStore root "kept-nar" $ \store cache -> do
              initial <- pushTo cache PushNone store [top]
              Dir.removeFile (narInfoFile cache top)
              BS.writeFile (cache </> T.unpack narUrl) "kept"
              repeated <- pushTo cache PushNone store [top]
              kept <- readMaybeFile (cache </> T.unpack narUrl)
              rewritten <- Dir.doesFileExist (narInfoFile cache top)
              pure $
                assertEqual "pushes" [Right (PushSummary 2 0), Right (PushSummary 1 1)] [initial, repeated]
                  `andThen` assertEqual "NAR" (Just "kept") kept
                  `andThen` assertEqual "narinfo" True rewritten,
          -- Leaf comes first in the closure, so had its narinfo followed
          -- its NAR, the narinfo would be there when top's NAR failed.
          runTestM "a NAR that cannot be written fails the push before any narinfo is written, and leaves no partial file" $
            withSourceStore root "unwritable-nar" $ \store cache -> do
              createDirectoryIfMissing True (cache </> T.unpack narUrl)
              refused <- pushTo cache PushNone store [top]
              listing <- cacheListing cache
              let leafNar = narOf leaf
                  leafFile = "nar" </> T.unpack (stripHashPrefix (CHash.formatNixHash (CHash.hashBytes leafNar))) <> ".nar"
              pure $
                assertEqual "push failed" True (isLeft refused)
                  `andThen` assertEqual "files" [(leafFile, leafNar), (cacheInfoFileName, cacheInfoText)] listing,
          runTestM "with zstd the NAR is written compressed under its file's hash, and substitutes, over HTTP and from the directory, by its Compression field" $
            withSourceStore root "zstd" $ \store cache -> do
              zstdPushed <- pushTo cache PushZstd store [top]
              narInfo <- maybe (Left "no narinfo") (either (Left . show) Right . TE.decodeUtf8' >=> NarInfo.parseNarInfo) <$> readMaybeFile (narInfoFile cache top)
              stored <- either (const (pure Nothing)) (\found -> readMaybeFile (cache </> T.unpack (NarInfo.niUrl found))) narInfo
              decompressed <- maybe (pure (Left "no NAR")) (Subst.decompressNar (toInteger (BS.length nar)) "zstd") stored
              zstdSubstituted <- substituteFrom overHttp root "zstd" cache publicKey
              zstdDirect <- substituteFrom directly root "zstd-direct" cache publicKey
              pure $
                assertEqual "push" (Right (PushSummary 2 0)) zstdPushed
                  `andThen` assertRight
                    "narinfo"
                    narInfo
                    ( \found ->
                        assertEqual "compression" "zstd" (NarInfo.niCompression found)
                          `andThen` assertEqual "URL" ((\bytes -> "nar/" <> stripHashPrefix (CHash.formatNixHash (CHash.hashBytes bytes)) <> ".nar.zst") <$> stored) (Just (NarInfo.niUrl found))
                          `andThen` assertEqual "FileHash" (CHash.formatNixHash . CHash.hashBytes <$> stored) (NarInfo.niFileHash found)
                    )
                  `andThen` assertEqual "decompressed" (Right nar) decompressed
                  `andThen` assertEqual "substitution" (BuildSuccess top, [Just (contentsOf leaf), Just (contentsOf top)], [Just leaf]) zstdSubstituted
                  `andThen` assertEqual "direct substitution" (BuildSuccess top, [Just (contentsOf leaf), Just (contentsOf top)], [Just leaf]) zstdDirect,
          runTestM "a nix-cache-info for another store directory is refused in upstream's words, and the push leaves no file but it, only the empty directories the newest upstream's store also makes before it refuses" $
            withSourceStore root "foreign" $ \store cache -> do
              createDirectoryIfMissing True cache
              BS.writeFile (cache </> cacheInfoFileName) "StoreDir: /gnu/store\n"
              refused <- pushTo cache PushNone store [top]
              entries <- sort <$> Dir.listDirectory cache
              listing <- cacheListing cache
              pure $
                assertEqual "push" (Left ("binary cache '" <> directoryUrl cache <> "' is for Nix stores with prefix '/gnu/store', not '/nix/store'")) refused
                  `andThen` assertEqual "entries" (sort (cacheInfoFileName : openedCacheDirectories)) entries
                  `andThen` assertEqual "files" [(cacheInfoFileName, "StoreDir: /gnu/store\n")] listing,
          runTestM "signing a narinfo adds the key's signature to the sorted set of those it carries, so signing again changes nothing" $ do
            let unsigned =
                  NarInfo.NarInfo
                    { NarInfo.niStorePath = "/nix/store/" <> storePathBasename top,
                      NarInfo.niUrl = narUrl,
                      NarInfo.niCompression = "none",
                      NarInfo.niFileHash = Just narHash,
                      NarInfo.niFileSize = Just (toInteger (BS.length nar)),
                      NarInfo.niNarHash = narHash,
                      NarInfo.niNarSize = toInteger (BS.length nar),
                      NarInfo.niReferences = [storePathBasename leaf],
                      NarInfo.niDeriver = Nothing,
                      NarInfo.niSigs = [],
                      NarInfo.niCA = Nothing
                    }
            result <-
              QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} $
                QC.forAll (QC.shuffle =<< QC.sublistOf ["a-1:sig", "test-cache-1:other", "z-1:sig"]) $ \existing ->
                  case (signNarInfo secretKey unsigned, signNarInfo secretKey unsigned {NarInfo.niSigs = existing}) of
                    (Right alone, Right signed) ->
                      NarInfo.niSigs signed QC.=== sort (nub (NarInfo.niSigs alone <> existing))
                        QC..&&. signNarInfo secretKey signed QC.=== Right signed
                    other -> QC.counterexample (show other) False
            pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result))),
          -- Upstream's signer takes a key's second half as its public key,
          -- so a key whose second half is not its seed's signs for no key.
          runTestM "a key file that holds no secret key, or one whose second half is not its seed's public key, is refused without its contents, and one with a byte-order mark and CRLF loads" $ do
            let refusedFile = root </> "refused-key"
                markedFile = root </> "marked-key"
                mismatchedKey = B64.encode (BS.take ed25519SeedBytes (Signing.skBytes secretKey) <> BS.replicate ed25519SeedBytes 0)
            BS.writeFile refusedFile "test-cache-1:not!base64"
            refused <- loadSecretKeyFile refusedFile
            BS.writeFile refusedFile (TE.encodeUtf8 (Signing.renderPublicKey publicKey))
            public <- loadSecretKeyFile refusedFile
            BS.writeFile refusedFile (TE.encodeUtf8 ("test-cache-1:" <> mismatchedKey))
            halves <- loadSecretKeyFile refusedFile
            keyText <- BS.readFile (root </> "secret-key")
            BS.writeFile markedFile (BS.pack [0xEF, 0xBB, 0xBF] <> keyText <> "\r\n")
            marked <- loadSecretKeyFile markedFile
            pure $ case (refused, public, halves, marked) of
              (Left refusedErr, Left publicErr, Left halvesErr, Right loaded)
                | not ("not!base64" `T.isInfixOf` refusedErr),
                  "expected 64-byte secret key, got 32" `T.isInfixOf` publicErr,
                  "not the public key of its seed" `T.isInfixOf` halvesErr,
                  not (mismatchedKey `T.isInfixOf` halvesErr) ->
                    assertEqual "loaded" secretKey loaded
              other -> Fail ("unexpected loads: " <> T.pack (show other))
        ]
    -- What one run's queries for the paths find among the caches under the
    -- fallback: each offer's cache and the priority it was asked at, or
    -- the query's failure, with the warnings the run gave its logger.
    queried fallback caches paths = do
      messages <- newIORef []
      let logger = Logger {logMessage = \message -> modifyIORef' messages (++ [message]), logBuildLine = const (pure ())}
      offers <- evalStateT (traverse (Subst.queryReferencesWith logger defaultFetchRetryPolicy quietEffects fallback caches) paths) Subst.emptyCacheMemo
      (,) (map offeredBy offers) . mapMaybe (T.stripPrefix "warning: ") <$> readIORef messages
    offeredBy offer = case offer of
      Realise.Offered substitute _ ->
        let cache = Subst.substituteCache substitute
         in Right (Just (Subst.ccUrl cache, Subst.ccPriority cache))
      Realise.NotOffered -> Right Nothing
      Realise.QueryFailed failure -> Left failure
    quietEffects = RetryEffects {reSleepMs = const (pure ()), reJitter = pure 0, reWarn = const (pure ())}
    -- A cache by its file:// URL, trusting the key, at the configured
    -- priority its nix-cache-info replaces.
    fileCache key cache = Subst.CacheConfig (directoryUrl cache) [Signing.renderPublicKey key] 50
    -- Fetch top as a build does, from the first of the caches to offer it,
    -- into a fresh store.
    fetchedInto root label caches =
      bracket (openStore testLogger (StoreDir (root </> label </> "dest"))) closeStore $ \store -> do
        result <- offeredThenFetched defaultFetchRetryPolicy quietEffects store caches top
        case result of
          Subst.SubstSuccess _ lock -> releasePathLock lock
          _ -> pure ()
        pure result
    substitutionCases root secretKey publicKey = do
      let pushTo cache = pushToDirectory (DirectoryPush cache secretKey PushNone)
          missing = StorePath (T.replicate 32 "m") "missing"
          offeredAt cache priority = Right (Just (directoryUrl cache, priority))
          otherKey = snd <$> throwawayKey (BS.replicate ed25519SeedBytes 7)
          withField name value narInfo = BS8.unlines [if (name <> ": ") `BS.isPrefixOf` line then name <> ": " <> value else line | line <- BS8.lines narInfo]
          unsigned = BS8.unlines . filter (not . BS.isPrefixOf "Sig: ") . BS8.lines
          rewrite path change = BS.readFile path >>= BS.writeFile path . change
          statePriority cache priority = BS.writeFile (cache </> cacheInfoFileName) (cacheInfoText <> "Priority: " <> priority <> "\n")
          narUrl = "nar/" <> stripHashPrefix (CHash.formatNixHash (CHash.hashBytes (narOf top))) <> ".nar"
          refusedWith ending result = case result of
            Subst.SubstError err | ending `T.isSuffixOf` err -> Pass
            other -> Fail ("expected a failure ending " <> ending <> ", got " <> T.pack (show other))
      sequence
        [ runTestM "a file:// cache offers what it holds at the priority it states, 0 when it states none, and a path it lacks is a miss, with no warning" $
            withSourceStore root "subst-miss" $ \store cache -> do
              _ <- pushTo cache store [top]
              found <- queried Realise.NoFallback [fileCache publicKey cache] [top, missing]
              pure (assertEqual "offers" ([offeredAt cache 0, Right Nothing], []) found),
          runTestM "a narinfo no trusted key signed is passed over, and so is one with no signature, as signatures are required" $
            withSourceStore root "subst-signatures" $ \store cache -> do
              _ <- pushTo cache store [top]
              signedOther <- either (const (pure ([], []))) (\key -> queried Realise.NoFallback [fileCache key cache] [top]) otherKey
              rewrite (narInfoFile cache top) unsigned
              withoutSignature <- queried Realise.NoFallback [fileCache publicKey cache] [top]
              pure $
                assertRight "other key" otherKey (const Pass)
                  `andThen` assertEqual "signed by another key" ([Right Nothing], []) signedOther
                  `andThen` assertEqual "unsigned" ([Right Nothing], []) withoutSignature,
          runTestM "a file:// cache whose nix-cache-info names another store directory is passed over for the run, after upstream's warning, given once" $
            withSourceStore root "subst-foreign" $ \store cache -> do
              _ <- pushTo cache store [top]
              BS.writeFile (cache </> cacheInfoFileName) "StoreDir: /gnu/store\n"
              found <- queried Realise.NoFallback [fileCache publicKey cache] [top, leaf]
              pure (assertEqual "offers" ([Right Nothing, Right Nothing], ["binary cache '" <> directoryUrl cache <> "' is for Nix stores with prefix '/gnu/store', not '/nix/store'"]) found),
          runTestM "a file:// cache is asked at the Priority its nix-cache-info states, among HTTP caches at theirs, and caches of one priority in the order given" $
            withSourceStore root "subst-priority" $ \store firstCache -> do
              let secondCache = root </> "subst-priority" </> "second"
              _ <- pushTo firstCache store [top]
              _ <- pushTo secondCache store [top]
              withDirectoryServer firstCache $ \base -> do
                let served = Subst.CacheConfig base [Signing.renderPublicKey publicKey] 40
                    both = [fileCache publicKey firstCache, fileCache publicKey secondCache]
                statePriority firstCache "30"
                statePriority secondCache "10"
                lower <- queried Realise.NoFallback both [top]
                statePriority firstCache "10"
                tied <- queried Realise.NoFallback both [top]
                BS.writeFile (secondCache </> cacheInfoFileName) cacheInfoText
                unstated <- queried Realise.NoFallback [served, fileCache publicKey secondCache] [top]
                statePriority secondCache "60"
                later <- queried Realise.NoFallback [fileCache publicKey secondCache, served] [top]
                pure $
                  assertEqual "the lower priority first" ([offeredAt secondCache 10], []) lower
                    `andThen` assertEqual "one priority in the order given" ([offeredAt firstCache 10], []) tied
                    `andThen` assertEqual "none stated, before HTTP's 40" ([offeredAt secondCache 0], []) unstated
                    `andThen` assertEqual "60, after HTTP's 40" ([Right (Just (base, 40))], []) later,
          runTestM "a Priority that is not a 32-bit integer fails the query whatever the fallback" $
            withSourceStore root "subst-bad-priority" $ \store cache -> do
              _ <- pushTo cache store [top]
              statePriority cache "high"
              refused <- traverse (\fallback -> queried fallback [fileCache publicKey cache] [top]) [Realise.NoFallback, Realise.BuildFromSource]
              let failure = Left ("binary cache '" <> directoryUrl cache <> "' gives the Priority 'high' in its nix-cache-info, which is not a 32-bit integer")
              pure (assertEqual "queries" [([failure], []), ([failure], [])] refused),
          runTestM "a file:// URL that names no directory, and a cache that cannot be opened, are passed over after a warning" $ do
            let base = root </> "subst-unopenable"
                plain = base </> "plain"
                underFile = plain </> "cache"
            createDirectoryIfMissing True base
            BS.writeFile plain "plain"
            (offers, warnings) <- queried Realise.NoFallback [Subst.CacheConfig (directoryUrl base <> "?priority=1") [Signing.renderPublicKey publicKey] 50, fileCache publicKey underFile] [top]
            pure $ case warnings of
              [urlWarning, openWarning]
                | "a file:// URL with a query" `T.isPrefixOf` urlWarning,
                  ("cannot open binary cache '" <> directoryUrl underFile <> "': ") `T.isPrefixOf` openWarning ->
                    assertEqual "offers" [Right Nothing] offers
              other -> Fail ("warnings: " <> T.pack (show other)),
          runTestM "opening a directory that holds no cache makes the directories and the nix-cache-info the newest upstream's store makes, and no realisations directory under its older name" $ do
            let cache = root </> "subst-empty" </> "cache"
            createDirectoryIfMissing True cache
            found <- queried Realise.NoFallback [fileCache publicKey cache] [top]
            entries <- sort <$> Dir.listDirectory cache
            cacheInfo <- readMaybeFile (cache </> cacheInfoFileName)
            pure $
              assertEqual "offers" ([Right Nothing], []) found
                `andThen` assertEqual "entries" (sort (cacheInfoFileName : openedCacheDirectories)) entries
                `andThen` assertEqual "nix-cache-info" (Just cacheInfoText) cacheInfo,
          runTestM "a cache without the realisations directory's older name, as the newest upstream writes one, opens where the substitution cannot write" $
            withSourceStore root "subst-read-only" $ \store cache -> do
              _ <- pushTo cache store [top]
              Dir.removeDirectory (cache </> "realisations")
              let writable allowed = Dir.getPermissions cache >>= Dir.setPermissions cache . Dir.setOwnerWritable allowed
              found <- bracket_ (writable False) (writable True) (queried Realise.NoFallback [fileCache publicKey cache] [top])
              pure (assertEqual "offers" ([offeredAt cache 0], []) found),
          runTestM "a NAR a narinfo names outside the cache is refused in upstream's words, and one that is gone is a miss" $
            withSourceStore root "subst-nar-files" $ \store cache -> do
              _ <- pushTo cache store [top]
              original <- BS.readFile (narInfoFile cache top)
              let caches = [fileCache publicKey cache]
              BS.writeFile (narInfoFile cache top) (withField "URL" "../escape.nar" original)
              escaping <- fetchedInto root "subst-nar-escaping" caches
              BS.writeFile (narInfoFile cache top) (withField "URL" "/escape.nar" original)
              absolute <- fetchedInto root "subst-nar-absolute" caches
              BS.writeFile (narInfoFile cache top) original
              Dir.removeFile (cache </> T.unpack narUrl)
              stillOffered <- queried Realise.NoFallback caches [top]
              gone <- fetchedInto root "subst-nar-gone" caches
              pure $
                refusedWith "local binary cache path '../escape.nar' must not contain '..' or '.' segments" escaping
                  `andThen` refusedWith "local binary cache path '/escape.nar' must not be absolute" absolute
                  `andThen` assertEqual "offered" ([offeredAt cache 0], []) stillOffered
                  `andThen` assertEqual "gone" Subst.SubstNotFound gone
        ]
    cacheInfoCases = do
      let windows = SI.os == "mingw32"
          foreignStore prefix = Left (CacheUnopenable ("binary cache 'file:///c' is for Nix stores with prefix '" <> prefix <> "', not '/nix/store'"))
          badPriority value = Left (CachePriorityInvalid ("binary cache 'file:///c' gives the Priority '" <> value <> "' in its nix-cache-info, which is not a 32-bit integer"))
          holds label prop = runTestM label $ do
            result <- QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} prop
            pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))
          segment = QC.listOf1 (QC.elements (['a' .. 'z'] <> ['0' .. '9'] <> "-_.")) `QC.suchThat` (`notElem` [".", ".."])
          directory
            | windows = ("C:\\" <>) . intercalate "\\" <$> QC.listOf1 segment
            | otherwise = ("/" <>) . intercalate "/" <$> QC.listOf1 segment
          beforeNumber = QC.listOf (QC.elements " \t\n\v\f\r")
          afterNumber = QC.listOf (QC.elements ['\32' .. '\126']) `QC.suchThat` (maybe True (not . isDigit) . listToMaybe)
      sequence
        [ runTest "nix-cache-info is read as upstream reads it: each StoreDir trimmed and held to /nix/store, the last Priority the cache's, the first field refused the refusal, and WantMassQuery and fields named with spaces let be" $
            assertEqual
              "read"
              [ Right (CacheInfo Nothing),
                Right (CacheInfo (Just 40)),
                Right (CacheInfo (Just 40)),
                Right (CacheInfo (Just (-3))),
                Right (CacheInfo Nothing),
                foreignStore "/gnu/store",
                foreignStore "/nix/store/",
                badPriority "high",
                foreignStore "/gnu/store",
                badPriority "2147483648"
              ]
              ( map
                  (readCacheInfo "file:///c")
                  [ cacheInfoText,
                    "WantMassQuery: 1\nStoreDir: \t/nix/store \r\nPriority: 40\n",
                    "Priority: 10\nPriority:40",
                    "Priority: -3 lowest\n",
                    " StoreDir: /gnu/store\nStoreDir : /gnu/store\n",
                    "StoreDir: /nix/store\nStoreDir: /gnu/store\n",
                    "StoreDir: /nix/store/\n",
                    "Priority: high\nStoreDir: /gnu/store\n",
                    "StoreDir: /gnu/store\nPriority: high\n",
                    "Priority: 2147483648\n"
                  ]
              ),
          holds "a Priority reads as C reads an int: white space, a sign and digits, what follows them ignored" $
            QC.forAll ((,,) <$> beforeNumber <*> QC.arbitrary <*> afterNumber) $ \(before, number, after) ->
              parsePriority (BS8.pack (before <> show (number :: Int32) <> after)) QC.=== Just (fromIntegral number),
          runTest "a Priority without digits, or past a 32-bit int, does not read, and one with a plus sign or after a vertical tab does" $
            assertEqual
              "read"
              [Nothing, Nothing, Nothing, Nothing, Just 7, Just 12, Just 0, Just 2147483647, Just (-2147483648)]
              (map parsePriority ["", "-", "x1", "2147483648", "+7", "\v12", "0x10", "2147483647", "-2147483648"]),
          holds "the file:// URL directoryUrl spells names the directory again" $
            QC.forAll directory $ \dir ->
              case fileUrlDirectory "" (T.unpack (directoryUrl dir)) of
                Just (Right named) -> QC.counterexample named (equalFilePath named dir)
                other -> QC.counterexample (show other) False,
          runTest "directoryUrl puts a slash before a Windows drive and spells each separator a slash, and a URL of another scheme is no file:// one" $
            assertEqual "URL" (if windows then "file:///C:/srv/cache" else "file:///srv/cache") (directoryUrl (if windows then "C:\\srv\\cache" else "/srv/cache"))
              `andThen` assertEqual "https" Nothing (fileUrlDirectory "" "https://cache.example.org"),
          runTest "a file a narinfo names is held below the cache, in upstream's words" $
            assertEqual
              "paths"
              [ Right ("cache" </> "nar/x.nar"),
                Left "local binary cache path must not be empty",
                Left "local binary cache path '/x.nar' must not be absolute",
                Left "local binary cache path 'nar/../x.nar' must not contain '..' or '.' segments",
                Left "local binary cache path './x.nar' must not contain '..' or '.' segments",
                if windows then Left "local binary cache path 'C:\\x.nar' must not be absolute" else Right ("cache" </> "C:\\x.nar"),
                if windows then Left "local binary cache path 'nar\\..\\x.nar' must not contain '..' or '.' segments" else Right ("cache" </> "nar\\..\\x.nar")
              ]
              (map (cacheFilePath "cache") ["nar/x.nar", "", "/x.nar", "nar/../x.nar", "./x.nar", "C:\\x.nar", "nar\\..\\x.nar"])
        ]
    targetCases root = do
      let windows = SI.os == "mingw32"
          (absoluteUrl, absoluteDir)
            | windows = ("file:///C:/srv/cache", "C:/srv/cache")
            | otherwise = ("file:///srv/cache", "/srv/cache")
          refusedWith fragment result = case result of
            Left err | fragment `T.isInfixOf` err -> Pass
            other -> Fail ("expected a refusal naming " <> fragment <> ", got " <> T.pack (show other))
      sequence
        [ runTest "an http:// or https:// --cache is a nova-cache server, a file:// one a directory" $
            assertEqual "server" (Right (ServerTarget "https://cache.example.org" (Just "api-key"))) (pushTarget "https://cache.example.org//" (Just "api-key") Nothing)
              `andThen` assertEqual "plain http" (Right (ServerTarget "http://127.0.0.1:8080" Nothing)) (pushTarget "http://127.0.0.1:8080" Nothing Nothing)
              `andThen` assertEqual "directory" (Right (DirectoryTarget absoluteDir "secret")) (pushTarget absoluteUrl Nothing (Just "secret")),
          runTest "a Windows drive after file:/// names the drive's path on Windows, and a directory named for it elsewhere, and a drive's root is refused on Windows" $
            assertEqual
              "drive"
              (Right (DirectoryTarget (if windows then "C:/cache" else "/C:/cache") "secret"))
              (pushTarget "file:///C:/cache" Nothing (Just "secret"))
              `andThen` ( if windows
                            then refusedWith "filesystem root" (pushTarget "file:///C:/" Nothing (Just "secret"))
                            else assertEqual "drive root" (Right (DirectoryTarget "/C:/" "secret")) (pushTarget "file:///C:/" Nothing (Just "secret"))
                        ),
          runTest "a file:// cache needs --secret-key-file and refuses --key-file, and a server refuses --secret-key-file" $
            refusedWith "needs --secret-key-file" (pushTarget absoluteUrl Nothing Nothing)
              `andThen` refusedWith "--key-file is a nova-cache server's API key" (pushTarget absoluteUrl (Just "api-key") (Just "secret"))
              `andThen` refusedWith "a nova-cache server signs what it receives" (pushTarget "https://cache.example.org" Nothing (Just "secret")),
          runTest "a --cache that is no http://, https:// or file:// URL is refused" $
            refusedWith "--cache takes" (pushTarget (root </> "cache") Nothing (Just "secret"))
              `andThen` refusedWith "--cache takes" (pushTarget "s3://bucket" Nothing Nothing),
          runTest "a file:// URL naming a host, a relative path or the root, or with a query, a fragment or an escape, is refused" $
            foldr
              (andThen . refusedWith "file://")
              Pass
              [ pushTarget url Nothing (Just "secret")
              | url <- ["file://", "file:///", "file:////", "file://host/srv/cache", "file://srv/cache", "file://C:/cache", absoluteUrl <> "?secret-key=k", absoluteUrl <> "#top", absoluteUrl <> "%20x"]
              ],
          runTest "push takes --secret-key-file's value" $
            case Cli.optCommand <$> Cli.parseArgs ["push", "--cache", absoluteUrl, "--secret-key-file", "secret", "hash-name"] of
              Right (Cli.CmdPush args) -> assertEqual "secret key file" (Just "secret") (Cli.paSecretKeyFile args)
              other -> Fail ("unexpected parse: " <> T.pack (show other))
        ]

-- | Every file in a cache directory and in its @nar@ directory, by its
-- path below the cache, with its bytes, in order.
cacheListing :: FilePath -> IO [(FilePath, BS.ByteString)]
cacheListing cache = do
  names <- sort <$> Dir.listDirectory cache
  concat <$> mapM entry names
  where
    entry name = do
      let path = cache </> name
      isDirectory <- doesDirectoryExist path
      if isDirectory
        then (\inner -> [(name </> file, bytes) | (file, bytes) <- inner]) <$> cacheListing path
        else (\bytes -> [(name, bytes)]) <$> BS.readFile path
