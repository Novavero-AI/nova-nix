-- | Tests of signing keys as @key generate-secret@ and
-- @key convert-secret-to-public@ make and read them ("Nix.Key").
module Test.Key
  ( testKeys,
  )
where

import Control.Exception (bracket_)
import Control.Monad (void)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Nix.Key (ColonBase64Refusal (..), KeyRefusal (..), generateSecretKey, keyRefusalMessage, parseColonBase64, parseSecretKey, renderColonBase64, renderPublicKey, renderSecretKey)
import Nix.Push.Directory (loadSecretKeyFile, signNarInfo)
import qualified NovaCache.NarInfo as NarInfo
import qualified NovaCache.Signing as Signing
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory)
import System.FilePath ((</>))
import Test.Harness (TestResult (..), andThen, assertEqual, assertRight, forceRemoveIfExists, runTest, runTestM)
import qualified Test.QuickCheck as QC

-- | Keys: upstream's own example of a secret key and the public key its
-- documentation prints for it, the keys a generation makes, which push
-- signs with and a substituter verifies under the converted key, the
-- spelling read back, and each refusal in upstream's words.
testKeys :: IO [Bool]
testKeys = do
  putStrLn "key"
  tmpBase <- getTemporaryDirectory
  let root = tmpBase </> "nova-nix-test-key"
  bracket_ (forceRemoveIfExists root >> createDirectoryIfMissing True root) (forceRemoveIfExists root) $
    sequence
      [ -- The pair in upstream's documentation of nix key
        -- convert-secret-to-public, read as its example reads it, from
        -- echo, with the line feed echo adds.
        runTest "upstream's example secret key gives the public key upstream's documentation prints for it, a line feed after it read" $
          assertRight "read" (parseSecretKey (upstreamSecret <> "\n")) $ \secretKey ->
            assertEqual "public key" "cache.example.org-0:tVeHVMaFaKi8lieGBErmScEOuxlSJLA7eO6IPrT1VGc=" (renderPublicKey secretKey)
              `andThen` assertEqual "secret key" upstreamSecret (renderSecretKey secretKey),
        runTestM "a generated key is its name, a colon and 88 characters of base64, and two are not the same" $ do
          first <- renderSecretKey <$> generateSecretKey "cache.example.org-1"
          second <- renderSecretKey <$> generateSecretKey "cache.example.org-1"
          pure $
            assertEqual "shape" (Just 88) (BS.length <$> BS.stripPrefix "cache.example.org-1:" first)
              `andThen` assertEqual "different" True (first /= second),
        -- Push refuses a key whose second half is not its seed's public
        -- key, so loading it checks the halves of the generated key.
        runTestM "a generated key loads as push's secret key file, signs a narinfo, and the signature verifies under its converted public key" $ do
          secretKey <- generateSecretKey "cache.example.org-1"
          let keyFile = root </> "secret-key"
          BS.writeFile keyFile (renderSecretKey secretKey)
          loaded <- loadSecretKeyFile keyFile
          pure $
            assertRight "loaded" loaded $ \pushKey ->
              assertRight "signed" (signNarInfo pushKey narInfo) $ \signed ->
                assertRight "public key" (Signing.parsePublicKey (TE.decodeLatin1 (renderPublicKey secretKey))) $ \publicKey ->
                  assertEqual "verified" [True] (map (Signing.verify publicKey signed) (NarInfo.niSigs signed)),
        holds "a key's spelling reads back as itself, its public key its name and its last 32 bytes" $
          QC.forAll ((,) <$> keyName <*> QC.vectorOf 64 QC.arbitrary) $ \(name, bytes) ->
            let spelled = renderColonBase64 name (BS.pack bytes)
             in case parseSecretKey spelled of
                  Right secretKey ->
                    renderSecretKey secretKey QC.=== spelled
                      QC..&&. renderPublicKey secretKey QC.=== renderColonBase64 name (BS.pack (drop 32 bytes))
                  Left refusal -> QC.counterexample (show refusal) False,
        runTest "a spelling with no colon, no name or no bytes is corrupt, and a byte outside base64 is named" $
          assertEqual
            "read"
            [ Left ColonBase64Corrupt,
              Left ColonBase64Corrupt,
              Left ColonBase64Corrupt,
              Left ColonBase64Corrupt,
              Left ColonBase64Corrupt,
              Left (ColonBase64BadCharacter 0x40),
              Right ("k", "\0\0"),
              Right ("k", "\0\0")
            ]
            (map parseColonBase64 ["key", ":AAAA", "k:", "k:=AAAA", "k:A", "k:AA@A", "k:AAA=@", "k:A\nAA\n"]),
        runTest "a refused key is reported as upstream's command reports it, with the trace it adds while decoding" $
          assertEqual
            "messages"
            [ traced "key is corrupt",
              traced "key is corrupt",
              traced "invalid character in Base64 string: '\r'",
              traced "invalid character in Base64 string: ' '",
              "error: secret key is not valid",
              "error: secret key is not valid",
              "error: secret key is not valid"
            ]
            ( map
                (either keyRefusalMessage (const "read") . parseSecretKey)
                [ "no-colon",
                  ":" <> upstreamBase64,
                  "k:" <> BS.take 86 upstreamBase64 <> "\r\n",
                  "k:" <> BS.take 40 upstreamBase64 <> " " <> BS.drop 40 upstreamBase64,
                  "k:AAAA",
                  renderColonBase64 "k" (BS.replicate 63 0),
                  renderColonBase64 "k" (BS.replicate 65 0)
                ]
            ),
        runTest "what follows a padded key's base64 is not read, so a carriage return or a space after it is let be" $
          assertEqual
            "public keys"
            (replicate 2 (Right "cache.example.org-0:tVeHVMaFaKi8lieGBErmScEOuxlSJLA7eO6IPrT1VGc="))
            (map (fmap renderPublicKey . parseSecretKey) [upstreamSecret <> "\r\n", upstreamSecret <> " x"]),
        runTest "a key of the wrong size is refused as such, not as an unreadable one" $
          assertEqual "refusal" (Left SecretKeyNotValid) (void (parseSecretKey "k:AAAA"))
      ]
  where
    upstreamBase64 = "E7lAO+MsPwTFfPXsdPtW8GKui/5ho4KQHVcAGnX+Tti1V4dUxoVoqLyWJ4YESuZJwQ67GVIksDt47og+tPVUZw=="
    upstreamSecret = "cache.example.org-0:" <> upstreamBase64
    traced message = "error:\n       " <> TE.encodeUtf8 (T.singleton '\x2026') <> " while decoding key named ''\n\n       error: " <> message
    keyName = BS8.pack <$> QC.listOf1 (QC.elements (['a' .. 'z'] <> ['0' .. '9'] <> ".-"))
    holds label prop = runTestM label $ do
      result <- QC.quickCheckWithResult QC.stdArgs {QC.chatty = False} prop
      pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result)))
    narInfo =
      NarInfo.NarInfo
        { NarInfo.niStorePath = "/nix/store/" <> T.replicate 32 "a" <> "-signed",
          NarInfo.niUrl = "nar/a.nar",
          NarInfo.niCompression = "none",
          NarInfo.niFileHash = Nothing,
          NarInfo.niFileSize = Nothing,
          NarInfo.niNarHash = "sha256:1b8m03r63zqhnjf7l5wnldhh7c134ap5vpj0850ymkq1iyzicy5s",
          NarInfo.niNarSize = 120,
          NarInfo.niReferences = [],
          NarInfo.niDeriver = Nothing,
          NarInfo.niSigs = [],
          NarInfo.niCA = Nothing
        }
