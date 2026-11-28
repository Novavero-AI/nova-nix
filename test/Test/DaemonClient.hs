{-# LANGUAGE LambdaCase #-}

-- | Tests of the daemon's client ("Nix.Daemon.Client"): its requests read
-- back by the daemon's own reader at every version, a session run in
-- memory against the daemon's side of a connection
-- ("Nix.Daemon.Connection") over a store held in memory, the daemon's
-- messages given to a logger, and the choice of a command's store.
module Test.DaemonClient
  ( testDaemonClient,
    testDaemonClientIO,
  )
where

import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Exception (finally)
import Control.Monad.State.Strict (State, modify', runState)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Nix.Config (defaultNixConfig)
import qualified Nix.Daemon.Client as Client
import qualified Nix.Daemon.Connection as Conn
import qualified Nix.Daemon.Ops as Ops
import qualified Nix.Daemon.Protocol as Proto
import qualified Nix.Daemon.Wire as Wire
import Nix.Hash (sha256Digest)
import Nix.Logger (Logger (..))
import Nix.Store.Path (StoreDir, defaultStoreDir)
import Nix.Store.Path.Internal (StorePath (..))
import Test.Harness (TestResult (..), andThen, assertEqual, runTest, runTestM)
import qualified Test.QuickCheck as QC

-- ---------------------------------------------------------------------------
-- Requests
-- ---------------------------------------------------------------------------

-- | The requests, the daemon's messages and the choice of store, without
-- a connection.
testDaemonClient :: IO [Bool]
testDaemonClient = do
  putStrLn "daemon/client"
  withNarHash "daemon/client" $ \narHash ->
    sequence
      [ runTestM "every request the client sends is read back whole by the daemon's reader, at every version it sends it at" $ do
          result <-
            QC.quickCheckWithResult QC.stdArgs {QC.chatty = False, QC.maxSuccess = 500} $
              QC.forAll ((,) <$> QC.elements versions <*> genRequest narHash) $ \(version, request) ->
                case Client.putRequest storeDir version request of
                  Left _ -> QC.property True
                  Right written ->
                    let bytes = BL.toStrict (BB.toLazyByteString written)
                     in case Wire.runDecoder Wire.getWord32 bytes of
                          Left failure -> QC.counterexample (show failure) False
                          Right (code, rest) -> case Ops.getRequest storeDir version code of
                            Nothing -> QC.counterexample ("no request has code " <> show code) False
                            Just decoder -> Wire.runDecoder decoder rest QC.=== Right (request, BS.empty)
          pure (if QC.isSuccess result then Pass else Fail (T.pack (QC.output result))),
        runTest "a request a version does not carry is not sent, saying which version it needs" $
          let at minor = Proto.Version (Proto.VersionNumber 1 minor) Set.empty
              refusal minor request = either Just (const Nothing) (Client.putRequest storeDir (at minor) request)
           in assertEqual "results" (Just "BuildPathsWithResults needs protocol 1.34, which the daemon does not speak") (refusal 33 (Ops.BuildPathsWithResults [] Proto.BuildNormal))
                `andThen` assertEqual "substitute" (Just "substituting in QueryValidPaths needs protocol 1.27, which the daemon does not speak") (refusal 26 (Ops.QueryValidPaths Set.empty Ops.Substitute))
                `andThen` assertEqual "no substitute" Nothing (refusal 26 (Ops.QueryValidPaths Set.empty Ops.NoSubstitute))
                `andThen` assertEqual "unframed upload" (Just "AddToStore needs protocol 1.25, which the daemon does not speak") (refusal 24 (Ops.AddToStore (Ops.AddToStoreRequest "x" "text:sha256" Set.empty Ops.NoRepair)))
                `andThen` assertEqual "never sent" (Just "CollectGarbage is not sent by this client") (refusal 38 (Ops.CollectGarbage (Ops.GCRequest Proto.ReturnLive Proto.WholeStore 0) Ops.KeepLiveness)),
        runTestM "the daemon's messages reach a logger as upstream's simple logger reports them" $ do
          (logger, messages, buildLines) <- recordingLogger
          let report = Client.loggerReport (Proto.Verbosity 3) logger
              activity level text = Proto.Activity (Proto.ActivityId 1) (Proto.Verbosity level) (Proto.ActivityType 105) text [] (Proto.ActivityId 0)
          mapM_
            report
            [ Proto.StderrNext "a line\n",
              Proto.StderrStartActivity (activity 3 "building 'x'"),
              Proto.StderrStartActivity (activity 4 "too quiet"),
              Proto.StderrStartActivity (activity 3 ""),
              Proto.StderrResult (Proto.ActivityId 1) Client.resBuildLogLine [Proto.FieldString "built a line"],
              Proto.StderrResult (Proto.ActivityId 1) Client.resPostBuildLogLine [Proto.FieldString "hooked"],
              Proto.StderrResult (Proto.ActivityId 1) (Proto.ResultType 105) [Proto.FieldInt 1],
              Proto.StderrStopActivity (Proto.ActivityId 1)
            ]
          loggedMessages <- reverse <$> readIORef messages
          loggedLines <- reverse <$> readIORef buildLines
          pure $
            assertEqual "messages" ["a line", "building 'x'...", "post-build-hook: hooked"] loggedMessages
              `andThen` assertEqual "build lines" ["built a line"] loggedLines,
        runTest "--store names the daemon, a socket, the store, a directory, or by default the daemon when it runs" $
          assertEqual "none" Client.AutoStore (Client.parseStoreTarget Nothing)
            `andThen` assertEqual "auto" Client.AutoStore (Client.parseStoreTarget (Just "auto"))
            `andThen` assertEqual "daemon" (Client.DaemonSocket Nothing) (Client.parseStoreTarget (Just "daemon"))
            `andThen` assertEqual "socket" (Client.DaemonSocket (Just "/run/socket")) (Client.parseStoreTarget (Just "unix:///run/socket"))
            `andThen` assertEqual "local" (Client.LocalStore Nothing) (Client.parseStoreTarget (Just "local"))
            `andThen` assertEqual "directory" (Client.LocalStore (Just "/srv/store")) (Client.parseStoreTarget (Just "/srv/store")),
        runTest "a command goes through the daemon whenever its socket accepts a connection, and to the store otherwise" $
          let accepting, refusing :: FilePath -> Tried (Either Text Text)
              accepting socket = Right ("connected to " <> T.pack socket) <$ tried socket
              refusing socket = Left "Connection refused" <$ tried socket
              tried :: FilePath -> Tried ()
              tried socket = modify' (socket :)
              choose connect target = runState (Client.chooseStore connect defaultSocket target) []
           in assertEqual "auto, running" (Right (Client.ThroughDaemon "connected to /default"), ["/default"]) (choose accepting Client.AutoStore)
                `andThen` assertEqual "auto, not running" (Right (Client.DirectStore Nothing), ["/default"]) (choose refusing Client.AutoStore)
                `andThen` assertEqual "daemon, not running" (Left "cannot connect to socket at '/default': Connection refused", ["/default"]) (choose refusing (Client.DaemonSocket Nothing))
                `andThen` assertEqual "socket named" (Right (Client.ThroughDaemon "connected to /named"), ["/named"]) (choose accepting (Client.DaemonSocket (Just "/named")))
                `andThen` assertEqual "store named" (Right (Client.DirectStore (Just "/srv/store")), []) (choose accepting (Client.LocalStore (Just "/srv/store")))
      ]
  where
    defaultSocket = "/default"

-- | A connector over a record of the sockets tried, latest first.
type Tried = State [FilePath]

-- | The versions requests are written at: every gate a request has, and
-- the newest with and without its features.
versions :: [Proto.Version]
versions =
  [Proto.Version (Proto.VersionNumber 1 minor) Set.empty | minor <- [18, 19, 21, 22, 23, 24, 25, 26, 27, 29, 30, 33, 34, 37, 38]]
    ++ [Client.offeredVersion]

-- | Requests the client sends, over a small pool of paths.
genRequest :: Proto.NarHash -> QC.Gen Ops.Request
genRequest narHash =
  QC.oneof
    [ Ops.SetOptions <$> genSettings,
      Ops.IsValidPath <$> genPath,
      Ops.QueryValidPaths <$> genPaths <*> QC.elements [Ops.Substitute, Ops.NoSubstitute],
      Ops.QuerySubstitutablePaths <$> genPaths,
      Ops.QueryReferrers <$> genPath,
      Ops.QueryValidDerivers <$> genPath,
      Ops.QueryDerivationOutputMap <$> genDrv,
      pure Ops.QueryAllValidPaths,
      Ops.QueryPathInfo <$> genPath,
      Ops.QueryMissing <$> QC.listOf genDerived,
      Ops.AddToStore <$> (Ops.AddToStoreRequest <$> QC.elements ["dump", "a-b"] <*> QC.elements ["text:sha256", "fixed:r:sha256", "fixed:sha256"] <*> genPaths <*> genRepair),
      Ops.AddToStoreNar <$> (Proto.ValidPathInfo <$> genPath <*> genInfo) <*> genRepair <*> QC.elements [Ops.CheckSigs, Ops.NoCheckSigs],
      Ops.BuildPaths <$> QC.listOf genDerived <*> QC.elements [Proto.BuildNormal, Proto.BuildRepair, Proto.BuildCheck],
      Ops.BuildPathsWithResults <$> QC.listOf genDerived <*> QC.elements [Proto.BuildNormal, Proto.BuildCheck],
      Ops.EnsurePath <$> genPath,
      Ops.AddTempRoot <$> genPath,
      Ops.AddIndirectRoot <$> QC.elements ["/home/u/result", "/a/b/c"]
    ]
  where
    genRepair = QC.elements [Ops.Repair, Ops.NoRepair]
    genPaths = Set.fromList <$> QC.listOf genPath
    -- A derived path that reads back the same at every version: a path
    -- that is no derivation as it is, and a derivation's outputs named.
    genDerived =
      QC.oneof
        [ Proto.DerivedOpaque <$> genPath,
          (\drv names -> maybe (Proto.DerivedOpaque drv) (Proto.DerivedBuilt drv) (Proto.namedOutputs (Set.fromList names))) <$> genDrv <*> QC.sublistOf [Proto.OutputName "out", Proto.OutputName "dev"]
        ]
    genSettings =
      Ops.ClientSettings
        <$> QC.arbitrary
        <*> QC.arbitrary
        <*> QC.arbitrary
        <*> (Proto.Verbosity <$> QC.choose (0, 7))
        <*> QC.arbitrary
        <*> QC.arbitrary
        <*> QC.arbitrary
        <*> QC.arbitrary
        <*> QC.arbitrary
        <*> (Map.fromList <$> QC.listOf ((,) <$> QC.elements ["substituters", "max-jobs", "timeout"] <*> QC.elements ["", "1", "https://cache.example.org/"]))
    genInfo =
      (\deriver refs time size ultimate -> (sampleInfo narHash refs) {Proto.upiDeriver = deriver, Proto.upiRegistrationTime = time, Proto.upiNarSize = size, Proto.upiUltimate = ultimate})
        <$> QC.oneof [pure Nothing, Just <$> genDrv]
        <*> (Set.fromList <$> QC.listOf genPath)
        <*> QC.choose (0, 2000000000)
        <*> QC.arbitrary
        <*> QC.arbitrary

-- | A path from the pool, which is no derivation.
genPath :: QC.Gen StorePath
genPath = QC.elements [fooPath, barPath, newPath]

-- | A derivation from the pool.
genDrv :: QC.Gen StorePath
genDrv = pure drvPath

-- ---------------------------------------------------------------------------
-- Sessions
-- ---------------------------------------------------------------------------

-- | Sessions run in memory against the daemon's side of a connection.
testDaemonClientIO :: IO [Bool]
testDaemonClientIO = do
  putStrLn "daemon/client-sessions"
  withNarHash "daemon/client-sessions" $ \narHash -> sessionTests narHash

-- | The sessions, given the NAR hash the store's infos carry.
sessionTests :: Proto.NarHash -> IO [Bool]
sessionTests narHash =
  sequence
    [ runTestM "the handshake agrees the newest version and both features, and hears the daemon's version and trust" $ do
        (outcome, end) <- session narHash Proto.Trusted latest $ (,) <$> Client.agreedVersion <*> Client.daemonInfo
        pure $
          assertEqual "outcome" (Right (Client.offeredVersion, Just (Proto.HandshakeInfo (Just nixVersion) (Just Proto.Trusted)))) outcome
            `andThen` assertEqual "end" Conn.ClientClosed end,
      runTestM "an older daemon's version is agreed, with no features and none of what it tells from later versions, and a request it does not carry is not sent while the session goes on" $ do
        let older = Proto.Version (Proto.VersionNumber 1 30) Set.empty
        (outcome, _) <- session narHash Proto.NotTrusted older $ do
          version <- Client.agreedVersion
          refused <- Client.attempt (Client.buildPathsWithResults [] Proto.BuildNormal)
          valid <- Client.isValidPath fooPath
          info <- Client.daemonInfo
          pure (version, refused, valid, info)
        pure $
          assertEqual
            "outcome"
            (Right (older, Left (Client.NotSent "BuildPathsWithResults needs protocol 1.34, which the daemon does not speak"), True, Just (Proto.HandshakeInfo Nothing Nothing)))
            outcome,
      runTestM "the queries answer what the store holds" $ do
        (outcome, _) <- session narHash Proto.Trusted latest $ do
          valid <- Client.isValidPath fooPath
          invalid <- Client.isValidPath missingPath
          validAmong <- Client.queryValidPaths (Set.fromList [fooPath, missingPath]) Ops.NoSubstitute
          everything <- Client.queryAllValidPaths
          referrers <- Client.queryReferrers barPath
          info <- Client.queryPathInfo fooPath
          noInfo <- Client.queryPathInfo missingPath
          outputs <- Client.queryDerivationOutputMap drvPath
          missing <- Client.queryMissing [Proto.DerivedOpaque missingPath]
          pure (valid, invalid, validAmong, everything, referrers, info, noInfo, outputs, missing)
        pure $ case outcome of
          Right (valid, invalid, validAmong, everything, referrers, info, noInfo, outputs, missing) ->
            assertEqual "valid" True valid
              `andThen` assertEqual "invalid" False invalid
              `andThen` assertEqual "valid among" (Set.singleton fooPath) validAmong
              `andThen` assertEqual "everything" (Set.fromList [fooPath, barPath, drvPath]) everything
              `andThen` assertEqual "referrers" (Set.singleton fooPath) referrers
              `andThen` assertEqual "info" (Just (sampleInfo narHash (Set.singleton barPath))) info
              `andThen` assertEqual "no info" Nothing noInfo
              `andThen` assertEqual "outputs" (Map.fromList [(Proto.OutputName "out", Just fooPath)]) outputs
              `andThen` assertEqual "missing" (Ops.Missing Set.empty Set.empty (Set.singleton missingPath) 0 0) missing
          Left failure -> Fail (T.pack (show failure)),
      runTestM "settings sent reach the store's work, and roots and paths asked for reach the store" $ do
        (outcome, _, fake) <-
          sessionWith narHash Proto.Trusted latest $ do
            Client.setOptions sampleSettings
            Client.addTempRoot fooPath
            Client.addIndirectRoot "/home/u/result"
            Client.ensurePath barPath
        pure $
          assertEqual "outcome" (Right ()) outcome
            `andThen` assertEqual "settings" (Just sampleSettings) (fakeSettings fake)
            `andThen` assertEqual "temporary roots" (Set.singleton fooPath) (fakeTempRoots fake)
            `andThen` assertEqual "indirect roots" (Set.singleton "/home/u/result") (fakeIndirectRoots fake)
            `andThen` assertEqual "ensured" [barPath] (fakeEnsured fake),
      runTestM "a NAR uploaded in chunks reaches the store whole, and its path is valid after" $ do
        chunks <- newIORef ["nar of ", "new", ""]
        let source = atomicModifyIORef' chunks (\case (chunk : rest) -> (rest, chunk); [] -> ([], BS.empty))
            info = Proto.ValidPathInfo newPath (sampleInfo narHash Set.empty)
        (outcome, _, fake) <- sessionWith narHash Proto.Trusted latest $ do
          Client.addToStoreNar info Ops.NoRepair Ops.CheckSigs source
          Client.isValidPath newPath
        pure $
          assertEqual "outcome" (Right True) outcome
            `andThen` assertEqual "uploaded" ["nar of new"] (fakeUploads fake),
      runTestM "a dump uploaded makes a path, whose info the client is given" $ do
        chunks <- newIORef ["the ", "dump", ""]
        let source = atomicModifyIORef' chunks (\case (chunk : rest) -> (rest, chunk); [] -> ([], BS.empty))
        (outcome, _, fake) <- sessionWith narHash Proto.Trusted latest (Client.addToStore (Ops.AddToStoreRequest "dump" "text:sha256" Set.empty Ops.NoRepair) source)
        pure $
          assertEqual "outcome" (Right (Proto.ValidPathInfo dumpPath (sampleInfo narHash Set.empty))) outcome
            `andThen` assertEqual "uploaded" ["the dump"] (fakeUploads fake),
      runTestM "a build's messages reach the report as they come, then its results" $ do
        reported <- newIORef []
        (outcome, _, _) <- sessionReporting (\message -> modifyIORef' reported (message :)) narHash Proto.Trusted latest $ do
          results <- Client.buildPathsWithResults [Proto.DerivedOpaque fooPath] Proto.BuildNormal
          Client.buildPaths [Proto.DerivedOpaque fooPath] Proto.BuildNormal
          pure results
        messages <- reverse <$> readIORef reported
        pure $
          assertEqual "outcome" (Right [Proto.KeyedBuildResult (Proto.DerivedOpaque fooPath) sampleBuildResult]) outcome
            `andThen` assertEqual "messages" (concat (replicate 2 buildMessages)) messages,
      runTestM "a daemon's error fails the operation, and the session goes on" $ do
        (outcome, _) <- session narHash Proto.Trusted latest $ do
          failed <- Client.attempt (Client.queryReferrers refusedPath)
          valid <- Client.isValidPath fooPath
          pure (failed, valid)
        pure (assertEqual "outcome" (Right (Left (Client.DaemonFailed (Ops.daemonError refusalMessage)), True)) outcome),
      runTestM "a request the daemon refuses before its work fails, and the connection it closes is lost" $ do
        (outcome, end) <- session narHash Proto.NotTrusted latest $ do
          refused <- Client.attempt (Client.addToStoreNar (Proto.ValidPathInfo newPath (sampleInfo narHash Set.empty)) Ops.Repair Ops.CheckSigs (pure BS.empty))
          after <- Client.attempt (Client.isValidPath fooPath)
          final <- Client.attempt (Client.isValidPath fooPath)
          pure (refused, isLeft after, final)
        pure $
          assertEqual "outcome" (Right (Left (Client.DaemonFailed (Ops.daemonError "you are not privileged to repair store paths")), True, Left (Client.ConnectionLost Client.disconnectedMessage))) outcome
            `andThen` assertEqual "end" (Conn.ClosedOnError (Ops.daemonError "you are not privileged to repair store paths")) end,
      runTestM "a daemon that is gone fails the handshake in upstream's words" $ do
        let gone = Conn.Transport (pure Nothing) (\_ -> pure ())
        outcome <- Client.runClient storeDir Client.offeredVersion gone (\_ -> pure ()) (pure ())
        pure (assertEqual "outcome" (Left (Client.ConnectionLost Client.disconnectedMessage)) outcome)
    ]
  where
    latest = Client.offeredVersion
    isLeft = either (const True) (const False)

-- ---------------------------------------------------------------------------
-- The daemon's side, in memory
-- ---------------------------------------------------------------------------

-- | What the store in memory holds and was asked.
data Fake = Fake
  { fakeValid :: !(Map.Map StorePath Proto.UnkeyedValidPathInfo),
    fakeSettings :: !(Maybe Ops.ClientSettings),
    fakeTempRoots :: !(Set.Set StorePath),
    fakeIndirectRoots :: !(Set.Set BS.ByteString),
    fakeEnsured :: ![StorePath],
    fakeUploads :: ![BS.ByteString]
  }

-- | The store's starting state: @foo@ referring to @bar@, and a
-- derivation whose @out@ is @foo@.
startingFake :: Proto.NarHash -> Fake
startingFake narHash =
  Fake
    { fakeValid = Map.fromList [(fooPath, sampleInfo narHash (Set.singleton barPath)), (barPath, sampleInfo narHash Set.empty), (drvPath, sampleInfo narHash Set.empty)],
      fakeSettings = Nothing,
      fakeTempRoots = Set.empty,
      fakeIndirectRoots = Set.empty,
      fakeEnsured = [],
      fakeUploads = []
    }

-- | A session with the daemon, trusting the client or not and offering a
-- version, over the store in memory, and how the daemon's side ended.
session :: Proto.NarHash -> Proto.Trust -> Proto.Version -> Client.ClientT IO a -> IO (Either Client.ClientFailure a, Conn.ConnectionEnd)
session narHash trust version steps = (\(outcome, end, _) -> (outcome, end)) <$> sessionWith narHash trust version steps

-- | 'session', with the store's state after it.
sessionWith :: Proto.NarHash -> Proto.Trust -> Proto.Version -> Client.ClientT IO a -> IO (Either Client.ClientFailure a, Conn.ConnectionEnd, Fake)
sessionWith = sessionReporting (\_ -> pure ())

-- | 'sessionWith', the daemon's messages given to a report.
sessionReporting :: (Proto.StderrMessage -> IO ()) -> Proto.NarHash -> Proto.Trust -> Proto.Version -> Client.ClientT IO a -> IO (Either Client.ClientFailure a, Conn.ConnectionEnd, Fake)
sessionReporting report narHash trust version steps = do
  state <- newIORef (startingFake narHash)
  toDaemon <- newChan
  toClient <- newChan
  daemonTransport <- channelTransport toDaemon toClient
  clientTransport <- channelTransport toClient toDaemon
  let config =
        Conn.ServerConfig
          { Conn.scStoreDir = storeDir,
            Conn.scVersion = version,
            Conn.scNixVersion = nixVersion,
            Conn.scVerbosity = Proto.Verbosity 3,
            Conn.scConfig = defaultNixConfig
          }
      daemon = Conn.serveConnection config trust (fakeStore narHash state) daemonTransport `finally` writeChan toClient Nothing
  withAsync daemon $ \running -> do
    outcome <- Client.runClient storeDir Client.offeredVersion clientTransport report steps `finally` writeChan toDaemon Nothing
    end <- wait running
    held <- readIORef state
    pure (outcome, end, held)

-- | One side of a connection in memory: it reads what the other side
-- writes to the channel it reads, and writes to the other's.  A
-- 'Nothing' in its channel is the end of its input, and every read after
-- it is the end too.
channelTransport :: Chan (Maybe BS.ByteString) -> Chan (Maybe BS.ByteString) -> IO (Conn.Transport IO)
channelTransport incoming outgoing = do
  ended <- newIORef False
  let receive = do
        done <- readIORef ended
        if done
          then pure Nothing
          else
            readChan incoming >>= \case
              Nothing -> Nothing <$ writeIORef ended True
              Just bytes
                | BS.null bytes -> receive
                | otherwise -> pure (Just bytes)
      sendBytes builder = writeChan outgoing (Just (BL.toStrict (BB.toLazyByteString builder)))
  pure (Conn.Transport receive sendBytes)

-- | The store in memory, over its state.
fakeStore :: Proto.NarHash -> IORef Fake -> Ops.DaemonStore IO
fakeStore narHash state =
  Ops.DaemonStore
    { Ops.dsIsValidPath = \_ path -> Right . Map.member path <$> valid,
      Ops.dsQueryValidPaths = \_ _ paths -> Right . Set.intersection paths . Map.keysSet <$> valid,
      Ops.dsQuerySubstitutablePaths = \_ _ -> pure (Right Set.empty),
      Ops.dsQueryReferrers = \_ path ->
        if path == refusedPath
          then pure (Left (Ops.daemonError refusalMessage))
          else Right . Map.keysSet . Map.filter (Set.member path . Proto.upiReferences) <$> valid,
      Ops.dsQueryValidDerivers = \_ _ -> pure (Right Set.empty),
      Ops.dsQueryDerivationOutputMap = \_ _ -> pure (Right (Map.fromList [(Proto.OutputName "out", Just fooPath)])),
      Ops.dsQueryAllValidPaths = \work -> record work *> (Right . Map.keysSet <$> valid),
      Ops.dsQueryPathInfo = \_ path -> Right . Map.lookup path <$> valid,
      Ops.dsQueryMissing = \_ targets -> pure (Right (Ops.Missing Set.empty Set.empty (Set.fromList [path | Proto.DerivedOpaque path <- targets]) 0 0)),
      Ops.dsAddToStore = \_ _ -> pure (collecting (\dump -> Right (Proto.ValidPathInfo dumpPath (sampleInfo narHash Set.empty)) <$ upload dump)),
      Ops.dsAddToStoreNar = \_ (Proto.ValidPathInfo path info) _ _ ->
        pure (collecting (\nar -> Right () <$ (upload nar *> modifyIORef' state (\fake -> fake {fakeValid = Map.insert path info (fakeValid fake)})))),
      Ops.dsBuildPaths = \work _ _ -> Right () <$ mapM_ (Ops.workReport work) buildEvents,
      Ops.dsBuildPathsWithResults = \work targets _ -> Right [Proto.KeyedBuildResult target sampleBuildResult | target <- targets] <$ mapM_ (Ops.workReport work) buildEvents,
      Ops.dsEnsurePath = \work path -> Right () <$ (record work *> modifyIORef' state (\fake -> fake {fakeEnsured = fakeEnsured fake ++ [path]})),
      Ops.dsAddTempRoot = \work path -> Right () <$ (record work *> modifyIORef' state (\fake -> fake {fakeTempRoots = Set.insert path (fakeTempRoots fake)})),
      Ops.dsAddIndirectRoot = \_ link -> Right () <$ modifyIORef' state (\fake -> fake {fakeIndirectRoots = Set.insert link (fakeIndirectRoots fake)}),
      Ops.dsFindRoots = \_ _ -> pure (Right Map.empty),
      Ops.dsCollectGarbage = \_ _ -> pure (Left notHere),
      Ops.dsOptimiseStore = \_ -> pure (Left notHere),
      Ops.dsVerifyStore = \_ _ _ -> pure (Left notHere),
      Ops.dsAddSignatures = \_ _ _ -> pure (Left notHere),
      Ops.dsNarFromPath = \_ _ -> pure (Left notHere)
    }
  where
    valid = fakeValid <$> readIORef state
    notHere = Ops.daemonError "not in these tests"
    upload bytes = modifyIORef' state (\fake -> fake {fakeUploads = fakeUploads fake ++ [bytes]})
    -- The settings the client last sent, as an operation's work carries
    -- them.
    record work = modifyIORef' state (\fake -> fake {fakeSettings = Ops.connClient (Ops.workSettings work)})
    collecting finish = Ops.Taking (more BS.empty) (finish BS.empty) (pure ())
      where
        more held chunk = pure (Ops.Taking (more (held <> chunk)) (finish (held <> chunk)) (pure ()))

-- | What a build the store in memory runs reports, and what the client is
-- sent for it.
buildEvents :: [Ops.LogEvent]
buildEvents =
  [ Ops.LogLine (Proto.Verbosity 0) "a line",
    Ops.ActivityStarted buildActivity,
    Ops.ActivityResult (Proto.ActivityId 7) Client.resBuildLogLine [Proto.FieldString "built a line"],
    Ops.ActivityStopped (Proto.ActivityId 7)
  ]

buildMessages :: [Proto.StderrMessage]
buildMessages =
  [ Proto.StderrNext "a line\n",
    Proto.StderrStartActivity buildActivity,
    Proto.StderrResult (Proto.ActivityId 7) Client.resBuildLogLine [Proto.FieldString "built a line"],
    Proto.StderrStopActivity (Proto.ActivityId 7)
  ]

buildActivity :: Proto.Activity
buildActivity = Proto.Activity (Proto.ActivityId 7) (Proto.Verbosity 3) (Proto.ActivityType 105) "building 'foo'" [] (Proto.ActivityId 0)

-- | The result the store in memory gives each build.
sampleBuildResult :: Proto.BuildResult
sampleBuildResult = Proto.BuildResult (Proto.Succeeded (Proto.Success Proto.Built Map.empty)) 1 1700000000 1700000001 Nothing Nothing

-- | Settings a client sends.
sampleSettings :: Ops.ClientSettings
sampleSettings = Ops.ClientSettings False True False (Proto.Verbosity 4) 2 30 True 3 True (Map.fromList [("max-jobs", "2")])

-- | A path's info with the NAR hash and the references given.
sampleInfo :: Proto.NarHash -> Set.Set StorePath -> Proto.UnkeyedValidPathInfo
sampleInfo narHash references =
  Proto.UnkeyedValidPathInfo
    { Proto.upiDeriver = Nothing,
      Proto.upiNarHash = narHash,
      Proto.upiReferences = references,
      Proto.upiRegistrationTime = 1700000000,
      Proto.upiNarSize = 10,
      Proto.upiUltimate = False,
      Proto.upiSignatures = Set.empty,
      Proto.upiContentAddress = Nothing
    }

-- | A NAR hash: the SHA-256 of a NAR in these tests, which a SHA-256
-- digest always makes.
sampleNarHash :: Maybe Proto.NarHash
sampleNarHash = Proto.narHashFromDigest (sha256Digest "nar of new")

-- | Run tests given the NAR hash their infos carry, or fail them where
-- there is none.
withNarHash :: Text -> (Proto.NarHash -> IO [Bool]) -> IO [Bool]
withNarHash group tests = maybe (sequence [runTest (group <> ": a NAR hash from a SHA-256 digest") (Fail "a SHA-256 digest made no NAR hash")]) tests sampleNarHash

-- | The message the store in memory fails a query with.
refusalMessage :: Text
refusalMessage = "the store refuses this path"

-- | Paths the sessions name.
fooPath, barPath, drvPath, missingPath, newPath, refusedPath, dumpPath :: StorePath
fooPath = StorePath "00000000000000000000000000000001" "foo"
barPath = StorePath "00000000000000000000000000000002" "bar"
drvPath = StorePath "00000000000000000000000000000003" "foo.drv"
missingPath = StorePath "00000000000000000000000000000005" "missing"
newPath = StorePath "00000000000000000000000000000007" "new"
refusedPath = StorePath "00000000000000000000000000000008" "refused"
dumpPath = StorePath "00000000000000000000000000000009" "dump"

-- | The store directory paths cross the wire under.
storeDir :: StoreDir
storeDir = defaultStoreDir

-- | The Nix version the daemon in memory names itself by.
nixVersion :: BS.ByteString
nixVersion = "nova-nix (test)"

-- | A logger that keeps each message and each build line, latest first.
recordingLogger :: IO (Logger, IORef [Text], IORef [BS.ByteString])
recordingLogger = do
  messages <- newIORef []
  buildLines <- newIORef []
  pure (Logger (\message -> modifyIORef' messages (message :)) (\line -> modifyIORef' buildLines (line :)), messages, buildLines)
