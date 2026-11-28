-- | Tests that an interrupt stops an operation rather than becoming its
-- fallback value.
module Test.Exceptions
  ( testInterruptsPropagate,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (asyncThreadId, waitCatch, withAsync)
import Control.Exception (AsyncException (..), SomeException, bracket, bracket_, fromException, throw, throwIO, throwTo, try)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import HttpFixture (Reply (..), withHttpServer)
import Nix.Builder (buildWithDeps, defaultBuildConfig)
import Nix.Builder.Unpack (builtinUnpackBuilder, defaultUnpackLimits, envSrcs, runBuiltinUnpack)
import Nix.Derivation (Derivation (..), currentPlatform)
import Nix.Eval (MonadEval (..))
import Nix.Eval.IO (runEvalIO)
import Nix.Hash (sha256Digest)
import qualified Nix.Hash as Hash
import Nix.Http (defaultFetchRetryPolicy)
import Nix.Push (PushCompression (..), PushConfig (..), defaultPartSize, loadKeyFile, pushPaths)
import Nix.Store (closeStore, openStore)
import Nix.Store.Path (StoreDir (..), defaultStoreDir, storePathToFilePath, storePathToText)
import Nix.Store.Path.Internal (StorePath (..))
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory)
import qualified System.Directory as Dir
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath (takeDirectory, (</>))
import System.IO (IOMode (..), withBinaryFile)
import qualified System.Info as SI
import qualified System.Process as Proc
import Test.Harness (TestResult (..), forceRemoveIfExists, runTestM, testEvalState, testLogger)

-- ---------------------------------------------------------------------------
-- Tests: an interrupt stops the operation it reaches
-- ---------------------------------------------------------------------------

-- | How long 'interruptedAfterStall' lets an action run before
-- interrupting it: far longer than any of these actions takes to reach
-- the read it stalls in.
stallMicros :: Int
stallMicros = 200000

-- | Run an action in a thread of its own, deliver a user interrupt to it
-- once it has had time to stall, and report how it ended: @Left@ the
-- exception that stopped it, or @Right@ the value it returned instead.
interruptedAfterStall :: IO a -> IO (Either SomeException a)
interruptedAfterStall action =
  withAsync action $ \running -> do
    threadDelay stallMicros
    throwTo (asyncThreadId running) UserInterrupt
    waitCatch running

-- | Pass when the interrupt is what stopped the action.
interruptPropagated :: (Show a) => Either SomeException a -> TestResult
interruptPropagated outcome = case outcome of
  Left err
    | fromException err == Just UserInterrupt -> Pass
    | otherwise -> Fail ("stopped by another exception: " <> T.pack (show err))
  Right value -> Fail ("the interrupt became a value: " <> T.pack (show value))

-- | Run an action while a FIFO sits at the path with a writer that never
-- writes, so a read of the path blocks until something interrupts the
-- reader.  A FIFO with no writer at all reads as empty at once, because
-- GHC opens a file non-blocking.
withStalledFifo :: FilePath -> IO a -> IO a
withStalledFifo path action =
  bracket_ (Proc.callProcess "mkfifo" [path]) (Dir.removePathForcibly path) $
    withBinaryFile path ReadWriteMode (const action)

-- | Each of these operations has a fallback value for a failure (an
-- unreadable file, a cache miss, a failed push), and an interrupt stops
-- it rather than becoming that value.  A file read is made to stall
-- on a FIFO ('withStalledFifo') and interrupted there from another
-- thread, so those cases need POSIX; the request a push makes stalls on
-- the loopback fixture on every platform.  A walk of a tree on disk
-- refuses a FIFO before it reads one and has no other read that blocks,
-- so no case interrupts a walk: 'ExecBit.serialiseIfReadable' and the
-- push's 'catchSync' catch only synchronous exceptions.  A fetch cache write never
-- blocks, so its interrupt is raised from inside the write.  The removal
-- behind a build's cleanup neither blocks nor forces anything the caller
-- supplies, so no case reaches it.
testInterruptsPropagate :: IO [Bool]
testInterruptsPropagate = do
  putStrLn "exceptions/interrupts-propagate"
  tmpBase <- getTemporaryDirectory
  let root = tmpBase </> "nova-nix-test-interrupts"
      storeDir = StoreDir (root </> "store")
      cacheHome = root </> "cache"
      inStore = storePathToFilePath storeDir
      pushTo base =
        PushConfig
          { pcCacheUrl = base,
            pcApiKey = Nothing,
            pcCompression = PushNone,
            pcPartSize = defaultPartSize,
            pcRetry = defaultFetchRetryPolicy
          }
      plainDrv =
        Derivation
          { drvOutputs = [],
            drvInputDrvs = Map.empty,
            drvInputSrcs = [],
            drvPlatform = currentPlatform,
            drvBuilder = builtinUnpackBuilder,
            drvArgs = [],
            drvEnv = Map.empty
          }
      unpacking archive = plainDrv {drvEnv = Map.singleton envSrcs (TE.encodeUtf8 (storePathToText defaultStoreDir archive))}
      withCacheHome =
        bracket
          (lookupEnv "XDG_CACHE_HOME" <* setEnv "XDG_CACHE_HOME" cacheHome)
          (maybe (unsetEnv "XDG_CACHE_HOME") (setEnv "XDG_CACHE_HOME"))
          . const
      fifoCase name test
        | SI.os == "mingw32" = True <$ putStrLn ("  SKIP  " ++ T.unpack name ++ ": needs a FIFO")
        | otherwise = runTestM name test
  forceRemoveIfExists root
  createDirectoryIfMissing True (unStoreDir storeDir)
  results <- bracket (openStore testLogger storeDir) closeStore $ \store ->
    sequence
      [ runTestM "an interrupt during a push request stops the push" $
          withHttpServer (\_ _ -> Trickle) $ \base _ requested -> do
            outcome <- withAsync (pushPaths (pushTo base) store [StorePath (T.replicate 32 "a") "pushed"]) $ \pushing -> do
              requested
              throwTo (asyncThreadId pushing) UserInterrupt
              waitCatch pushing
            pure (interruptPropagated outcome),
        fifoCase "an interrupt reading the API key file is not an unreadable key" $ do
          let keyFile = root </> "stalled-key"
          outcome <- withStalledFifo keyFile (interruptedAfterStall (loadKeyFile keyFile))
          pure (interruptPropagated outcome),
        fifoCase "an interrupt reading a store derivation is not a missing one" $ do
          st <- testEvalState storeDir root
          let sp = StorePath (T.replicate 32 "d") "stalled.drv"
          outcome <- withStalledFifo (inStore sp) (interruptedAfterStall (runEvalIO st (readStoreDerivation sp)))
          pure (interruptPropagated outcome),
        -- The entry's path is the cache's layout: one file per key, named
        -- by the key's SHA-256 (fetchCacheFile).
        fifoCase "an interrupt reading the fetch cache is not a cache miss" $
          withCacheHome $ do
            st <- testEvalState storeDir root
            let key = "stalled-entry"
                entry = cacheHome </> "nova-nix" </> "fetch" </> T.unpack (Hash.bytesToHexText (sha256Digest (TE.encodeUtf8 key)))
            createDirectoryIfMissing True (takeDirectory entry)
            outcome <- withStalledFifo entry (interruptedAfterStall (runEvalIO st (lookupFetchCache key)))
            pure (interruptPropagated outcome),
        -- The entry's text is the interrupt, raised when the write forces it.
        runTestM "an interrupt writing the fetch cache is not an unwritable cache" $
          withCacheHome $ do
            st <- testEvalState storeDir root
            outcome <- try (runEvalIO st (writeFetchCache "interrupted-entry" (throw UserInterrupt)))
            pure (interruptPropagated outcome),
        -- The suite's own harness has a fallback too: a test's FAIL line.
        runTestM "an interrupt in a test stops the suite, not just that test" $ do
          outcome <- try (runTestM "interrupted" (throwIO UserInterrupt))
          pure (interruptPropagated outcome),
        fifoCase "an interrupt unpacking an archive is not a failed unpack" $ do
          let archive = StorePath (T.replicate 32 "f") "stalled.tar"
          outcome <- withStalledFifo (inStore archive) (interruptedAfterStall (runBuiltinUnpack storeDir defaultUnpackLimits (unpacking archive) [("out", root </> "stalled-out")]))
          pure (interruptPropagated outcome),
        fifoCase "an interrupt reading an input derivation is not an unreadable one" $ do
          let input = StorePath (T.replicate 32 "g") "stalled-input.drv"
              rootDrv = plainDrv {drvInputDrvs = Map.singleton input ["out"]}
              config = defaultBuildConfig storeDir
          outcome <- withStalledFifo (inStore input) (interruptedAfterStall (buildWithDeps config store rootDrv (StorePath (T.replicate 32 "h") "root.drv")))
          pure (interruptPropagated outcome),
        -- The failures the unpacker does catch: the gzip decoder throws a
        -- DecompressError, and a missing archive is an IOException.  Each
        -- is the archive's failure, not an escape.
        runTestM "a corrupt gzip archive and a missing one fail the unpack" $ do
          let corruptGzip = StorePath (T.replicate 32 "k") "corrupt.tar.gz"
              missing = StorePath (T.replicate 32 "j") "missing.tar"
          BS.writeFile (inStore corruptGzip) "not a gzip stream"
          corruptGzipOutcome <- runBuiltinUnpack storeDir defaultUnpackLimits (unpacking corruptGzip) [("out", root </> "corrupt-gzip-out")]
          missingOutcome <- runBuiltinUnpack storeDir defaultUnpackLimits (unpacking missing) [("out", root </> "missing-out")]
          pure $ case (corruptGzipOutcome, missingOutcome) of
            (Left (1, corruptGzipMsg), Left (1, missingMsg))
              | "corrupt.tar.gz" `T.isInfixOf` corruptGzipMsg
                  && not ("unsupported archive format" `T.isInfixOf` corruptGzipMsg)
                  && "missing.tar" `T.isInfixOf` missingMsg ->
                  Pass
            other -> Fail ("expected both unpacks to fail: " <> T.pack (show other))
      ]
  forceRemoveIfExists root
  pure results
